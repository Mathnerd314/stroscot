Assembly
########

A lot of languages aim for "no runtime overhead". But this is unattainable, even C structs and arrays have to use memcpy occasionally. Stroscot merely aims to be as fast as C, which in some sense limits Stroscot to C's abstractions. But it would be nice to do assembly directly as well so Stroscot can be even faster in some cases.

Architectures
=============

The first step in dealing with assembly is to decide which instruction set architectures to support. I couldn't find a list of processor architectures by popularity, but from `this quora answer <https://www.quora.com/What-kind-of-instruction-set-architecture-do-modern-processors-use>`__ and checking it by googling numbers of units sold for other random ISAs, the two primary architectures are x86-64 AMD64 (desktops) and ARMv8-A A64 (mobile devices).

Others:
* ARMv9-A A64: It's released, devices expected in 2022. Very similar to v8 so should be able to share the code. Verdict: on the roadmap
* 32-bit ARM: Old phones, the Raspberry Pi Zero. The XML database is similar. Verdict: Contributor.
* RISC-V: There are $100-ish dev boards listed at https://riscv.org/exchange/boards/. No non-dev systems yet. It's a relatively simple ISA, similar to ARM. Verdict: Contributor
* POWER: `Raptor <https://secure.raptorcs.com/content/base/products.html>`__ sells expensive systems. Even more niche than RISC-V. Verdict: C backend.
* MIPS: the company that develops it went bankrupt and is now doing RISC-V. There are consumer systems available in China (Loongson), but the rumor is that they too are moving to RISC-V or else to their own architecture LoongArch. Verdict: C backend.
* z/Architecture: really expensive, weird OS. Verdict: C backend.
* SPARC: It's end-of-life but I guess you can still buy servers second-hand. Verdict: C backend.
* 32-bit x86: Old desktop PCs. From a time/effort perspective it seems cheaper to buy a new computer instead of writing support for these. Verdict: C backend or contributor.

From a design perspective supporting 2 architectures is not much different from supporting 10, it's just a larger set of cases. ARM support will be tested through QEMU, x86 natively. There are also CI services that could work (Drone). Code bloat is an issue but keeping each ISA in its own folder should avoid drift.

In addition to the basic ISAs, there are also extensions and `microarchitectures <https://en.wikipedia.org/wiki/Microarchitecture>`__ to consider. `PassMark <https://www.cpubenchmark.net/share30.html>`__ has a list of CPU shares, it's probably wildly skewed to gaming but it's better than nothing. The data on CPU cycles, ports, etc. is rather detailed so it will probably depend on user submissions; for now I'll use my own CPU (AMD A6-3650 APU).

Instruction database
====================

An instruction is a sequence of bytes. Beyond that it's hard to define exactly. `sandsifter <https://github.com/xoreaxeaxeax/sandsifter>`__ defines an instruction as a sequence ``seq`` for which ``seq|000`` does not trigger a page fault, but ``se|q00`` does (where ``|`` is a page boundary), and which does not trigger an undefined instruction (#UD) trap. `haruspex <https://blog.can.ac/2021/03/22/speculating-x86-64-isa-with-one-weird-trick/>`__ is even more tricky and defines it as a sequence that fills the microcode speculation buffer with a number of micro-ops not matching the undefined instruction.

There are usually at least a few undocumented instructions on a processor, and we won't know what they are. So we need a syntax for writing instructions directly, ``instr('f0 0f')``. It's basically a ``.db`` statement, but whereas ``.db`` is used for file headers or data in the ``.data`` section, this is meant specifically for executable data.

But for a random byte sequence there is nothing the compiler can do besides pass it through. Normally we want to run a lot of pipelining optimizations. So for an optimizing compiler we need instruction metadata.

Data sources
------------

In terms of data sources for ISAs, for x86 the official sources are `Intel's SDM <https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html>`__ / `AMD's Architecture Programmer's Manual <https://developer.amd.com/resources/developer-guides-manuals/>`__, which use English and pseudocode and have numerous typos (if the experiences of others hold true). Also they are only distributed as PDFs. Parsing the PDFs is a lot of work. `EXEgesis <https://github.com/google/EXEgesis>`__ uses a hacky Xpdf parser but has some amount of effort invested by Google. `x86doc <https://github.com/HJLebbink/x86doc/tree/master/Python>`__ uses pdfminer to generate HTML which seems like a more friendly starting point.

More structured are x86 instruction databases:
* `Intel XED <https://intelxed.github.io/>` (`file <https://github.com/intelxed/xed/blob/main/datafiles/xed-isa.txt>`__).
* LLVM `x86 tables <https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86.td>`__
* NASM `instruction table <https://github.com/netwide-assembler/nasm/blob/master/x86/insns.dat>`__
* `GNU Assembler (gas) <https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=opcodes/i386-opc.tbl;h=b0530e5fb82f4f4cd85d67f7ebf6ce6ebf9b45b5;hb=HEAD>`__
* `iced <https://github.com/icedland/iced/blob/65d1f49584247a09dcc2559727936a53014268f5/src/csharp/Intel/Generator/Tables/InstructionDefs.txt>`__
* The K Framework `formal X86 semantics <https://github.com/kframework/X86-64-semantics>`__ contains most of the Haswell instructions. It was manually written, but checked with fuzzing. It probably could generate good data, but it requires parsing the K syntax.
* `OSACA <https://github.com/RRZE-HPC/OSACA/tree/master/osaca/data/isa>`__ is AGPL licensed and very incomplete
* `Ghidra <https://github.com/NationalSecurityAgency/ghidra/blob/master/Ghidra/Processors/x86/data/languages/ia.sinc#L1594>`__, seems to have semantics
* emulators: https://github.com/colejohnson66/rbx86, https://bochs.sourceforge.io/
* https://github.com/asmjit/asmjit, https://github.com/bitdefender/bddisasm, https://github.com/dyninst/dyninst, https://github.com/herumi/xbyak, qemu/capstone, https://github.com/diegocarba99/bagheera, https://github.com/mongodb-labs/disasm, zydis, https://github.com/MahdiSafsafi/AMED, https://github.com/nidud/asmc
* Go assembler https://cs.opensource.google/go/go/+/master:src/cmd/internal/obj/x86/avx_optabs.go;l=1791?q=vfixupimmss&ss=go
* https://github.com/Barebit/x86reference/blob/master/x86reference.xml

For ARM we have XML `Machine Readable Architecture instruction tables <https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools>`__, which is nice-ish XML, and the code has been validated against ARM's conformance suite. There is a toy disassembler `hs-arm <https://github.com/nspin/hs-arm>`__ using the tables. EXEgesis also parses the XML. `asl-interpreter <https://github.com/alastairreid/asl-interpreter>`__ runs the descriptions.

Timing:
* https://github.com/e12005490/valgrind_timing/tree/117292a3a94f843c173bdb53e4933c6b79570240/variable_time_instructions
* ARM: ?


The basic goal is to have official data sources where possible and otherwise generate it automatically via measurement, that way new processors / ISAs can be added quickly.

Templates
---------

The most basic data is an instruction list. Listing them out exhaustively would be too much so instead we have a list of templates, each of which can turned into an instruction by filling in the holes. Following Xed we can call the data that is filled in "explicit operands". The explicit operands are themselves named templates of bitstrings/bytestrings and can refer to registers, addressing modes / addresses, and immediate values.

The templates should have names. For automatically generating them it could be a hash of the template string, or else the smallest unique opcode prefix or something. But really we want to use the mnemonics from the docs.

Intel has variable-length instructions and the docs seem to use byte-based templates, for example 64-bit ADCX is ``66 <REX.w> 0F 38 F6 <MODRM>``. The REX has 3 bits of operand data; the modrm is an operand and can be 1-6 bytes (register or memory with optional SIB/displacement). We could parse the Intel docs for this (EXEgesis + handling all the weird encoding stuff), but I think dumping Xed's `iform list <https://intelxed.github.io/ref-manual/xed-iform-enum_8h.html>`__ and using Xed directly for encoding is the way to go. It doesn't match the docs 1-1 but it saves on sanity - e.g. the separate memory / register templates.

ARM has fixed length instructions and uses a bit-based format, for example A64 ADDS is ``sf 0101011 shift* 0 Rm**** imm6***** Rn**** Rd****``. Here each name is an operand and the stars represent extra bits of the operand - the operand is a fixed-length bitstring. hs-arm `seems <https://github.com/nspin/hs-arm/blob/8f10870a4afbbba010e78bd98e452ba67adc34e0/nix-results/harm.harm-tables-src/gen/Harm/Tables/Gen/Insn.hs>`__ to pull out this information just fine, although its operand names are a little weird.

So the information for each template is:

* form name (string)
* explicit operands (list)

  * name (dest, src1, xmm1, etc.) - optional
  * type:

    * immediate (range/size b, z, etc.)
    * register class (class GPR8, GPRv, XMM, etc.)
    * memory (size b, v, etc.)
* encoding function ``[Operands] -> Bits``

We also want to store Xed's isa_set field, the condition on CPUID for this instruction to work, and the valid modes (32-bit, 64-bit, real, protected, etc.). There are lots of overlapping sets of instructions and maintaining one master set is easier than duplicating the data.

Affected state
--------------

Another important piece of data is the affected state, i.e. the list of read/written flags/registers/etc. This is used in instruction reordering/scheduling and register allocation to minimize `data hazards <https://en.wikipedia.org/wiki/Hazard_(computer_architecture)#Data_hazards>`__.

The affected things depend on the instruction (and the operands). Where can we get this info?

It seems possible to automatically determine by fuzzing (weighted towards special cases like 0 and 1). But it's probably really slow and the result is somewhat suspect - it can't determine that a flag/register becomes undefined, and it may miss reads/writes that happen in rare circumstances.

From the Intel docs there is a little ``(r,w)`` or ``(r)`` after the operands. But this doesn't include everything. The rest can be found by scanning the English text, but unless we use NLP this will only give a list of affected things and not read/write info.

Xed has info on read/written flags. But it abbreviates other flag registers - for example (per the Intel documentation) VFIXUPIMMSS reads MXCSR.DAZ and conditionally updates MXCSR.IE and MXCSR.ZE, but Xed just records a MXCSR attribute. LLVM similarly just has ``USES = [MXCSR]``. NASM and gas don't seem to have flag information at all. iced does have flag info but no MXCSR. The K semantics don't have MXCSR.

For ARM modifying asl-interpreter should give info on flags etc.

The schema:

* form name
* affected things (list)
  * type:
    * explicit operand (+ index)
    * fixed register
    * pseudo resource
    * flag bit
  * read: read / not read / conditionally read / unknown
  * written:
    * value: constant,  copied from input, read + constant, complex computation, undefined, ...
    * not written, conditionally written, unknown
* possible exceptions

Instructions with no data have all possible affected things present, with read/write unknown.

Pseudo-resource includes things like load-link/store-conditional. LDXR sets monitors (write) and STXR checks monitors (read). A second LL clears the monitor so LL is actually read/write. Anyway the monitor is a pseudo resource, because it's not a register.

Classification
--------------

There are a lot of instructions. We can classify them based on their affected state:
* data: reads and writes only flags/general-purpose registers/stack pointer/memory (does not read/write the program counter or other state). memory prefetch/barrier are also data instructions
* call: reads the program counter
* jump: sets the program counter to something other than the next instruction
* branch: jump that can go to multiple addresses depending on the state of various flags/registers
* interrupt: unconditionally throws an exception
* privileged: requires privileged processor state to execute successfully (e.g. ring 0)
* nop: does nothing

For code layout knowing the possible execution paths is important. branch, jump, call, return have to be handled specifically.

Performance
-----------

the data present in LLVM for instruction scheduling (such as uops, execution ports/units, and latencies),

If PSTATE.DIT is 1 the execution time is independent of the values.

Attributes / metadata
---------------------

* concurrency / memory model
