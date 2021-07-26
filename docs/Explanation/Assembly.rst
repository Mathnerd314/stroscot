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


Templates
---------

The most basic data is an instruction list. Listing them out exhaustively would be too much so instead we have a list of templates, each of which can turned into an instruction by filling in the holes. Following Xed we can call the data that is filled in "explicit operands". The explicit operands are themselves bitstrings/ bytestrings and can refer to registers, addressing modes / addresses, and immediate values.

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

Another important piece of data is the affected state, i.e. the list of read/written flags/registers/etc. This is used in scheduling/register allocation to minimize `data hazards <https://en.wikipedia.org/wiki/Hazard_(computer_architecture)#Data_hazards>`__.

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

  * So need a concurrency / memory model to define semantics
* how fast can you add new instructions and make an official release?
* instruction reordering / scheduling possible?

Operations
==========

To abstract the ISA we consider the instructions from a functional perspective - these functions are called "operations". Operations don't reference registers, the operations all take/return temporaries. Since all registers/flags/etc. can be stored/loaded to memory, temporaries are conceptually a bitstring in immutable memory of a fixed bitwidth. These bitwidths vary by the instruction: x86 uses 1, 8, 16, 32, 64, 80, 128, 256, 512, etc. (for flags, segment registers, general-purpose registers, FPU registers, MMX/SSE/AVX).

For example the operations corresponding to x86-64 "DIV—Unsigned Divide", ADD, and ADC with 64-bit operands look like:

::

  divide (src : B64) (high : B64) (low : B64) =
    divisor = src
    dividend = high : low
    if divisor == 0
      DE
    else
      quotient = src2 / src1
      if quotient >= 2^64
        DE
      else
        { quotient = quotient, remainder = src2 mod src1 }

  add (src1 : B64) (src2 : B64) =
    dest = src1 + src2
    ... flags ...
    { dest, OF, SF, ZF, AF, CF, PF }

  adc (src1 : B64) (src2 : B64) (cf : B1) =
    dest = src1 + src2 + cf
    ... flags ...
    { dest, OF, SF, ZF, AF, CF, PF }

Accessing memory is handled by a separate operation:

::

  read : Addr -> {B64 | B32 | B16 | B8}
  read a =
  if noncanonical a -- https://stackoverflow.com/questions/25852367/x86-64-canonical-address
    if referencesSSsegment a
      fault SS(0)
    else
      fault GP(0)
  else
    if unaligned a && enabled alignment_checking
      fault AC(0)
    else if not_in_physical_memory a
      fault PF(fault-code)
    else
      memory[a]

Register allocation
===================

After that we implement register allocation.

After register allocation, there are additional register-memory mov's and flag test/set, and all operations read/write the physical registers/flags.

There's some code in Python `here <https://ppci.readthedocs.io/en/latest/reference/codegen/index.html>`__

Control flow
============

The ADD instruction is not so simple

Blocks
======

A basic block (BB) is a sequence of instructions that is entered only from the top, and that contains no terminator instructions except for a single one at the end. The last instruction in a BB must be a terminator instruction, so execution cannot fall through the end of the BB but instead jumps to a new BB.

Terminator instructions are unconditional branches.

EBB parameter
    A formal parameter for an EBB is an SSA value that dominates everything
    in the EBB. For each parameter declared by an EBB, a corresponding
    argument value must be passed when branching to the EBB. The function's
    entry EBB has parameters that correspond to the function's parameters.

EBB argument
    Similar to function arguments, EBB arguments must be provided when
    branching to an EBB that declares formal parameters. When execution
    begins at the top of an EBB, the formal parameters have the values of
    the arguments passed in the branch.


A basic block is a mixture of jump and non-jump instructions that is complete, in the sense that any execution of the program will take one of the jumps. Any arbitrary sequence of instructions can be turned into a basic block by adding an unconditional jump at the end.

Although phi nodes were an interesting idea all the `cool kids <https://mlir.llvm.org/docs/Rationale/Rationale/#block-arguments-vs-phi-nodes>`__ are now using block arguments. Blocks arguments fit better into various analysis passes.

Symbols
=======

Jump/branch instructions take an address as a parameter. These addresses can be specified as an absolute memory location or relative to the program counter. Both of these require knowing the memory layout of the program. However, the addresses are stored in most object files as symbols and are not resolved until link time or load time. The actual value stored is a placeholder and it is fixed up by relocations.

When building a shared library the assembly is generated to minimize the number of relocations which must be applied, since they take time when starting the program. Position independent code will call non-static functions via the Procedure Linkage Table and reference global/static variables through Global Offset Tables. Local program counter-relative references do not need entries. The PLT and GOT tables are different for each process, but the actual code of the library is shared across all the processes. The indirection via the table `slowed down Python <https://bugs.python.org/issue38980>`__ by `27% <https://fedoraproject.org/wiki/Changes/PythonNoSemanticInterpositionSpeedup>`__ and is optimized away for non-extern functions in LLVM.

A symbol is a name and a value. In a C object file, there will be a symbol for each function and for each global and static variable, named similarly. These symbol's values will roughly be the address of the variable, i.e. the result of ``&my_global_var``.

Object files contain a table of references to all the symbols used by the code, as well as the locations in the code that the references are made, classified by type of reference (e.g. absolute vs relative references). The object file also contains a table of defined symbols, all the symbols which it exports. there can also be references to symbol names defined in a different object file, known as an undefined symbol.

Symbols also have versions, which are effectively part of the name of the symbol. But looking up an unversioned symbol resolves to the default versioned symbol.

Relocations
===========

During the linking process, the linker will assign an address to each defined symbol, and will resolve each undefined symbol by finding a defined symbol with the same name. Then it will perform relocations, modifications to the assembly code. A simple, and commonly used, relocation is “set this location in the contents to the value of this symbol plus this addend.” There are different kinds of relocations for different modes of addressing in the machine code.

The linker does not reorder sections of code, so relative jumps can be inserted fairly easily if you know the basic block sizes. The locations of absolute offsets need to be marked in the final executable so that the operating system loader can adjust them if it needs to load the executable somewhere other than its preferred address.

A relocation in an object file may refer to an undefined symbol. If the linker is unable to resolve that symbol, it will normally issue an error (but not always: for some symbol types or some relocation types an error may not be appropriate).

The linker also does some optimizations known as relaxation based on knowing final addresses. The most common type of relaxation is shrinking call instructions, e.g. replacing a 32-bit offset with a 16-bit offset. When the linker relaxes a relocation in the middle of the code, it may need to adjust any PC relative references which cross the point of the relaxation. Therefore, when relaxing, the assembler needs to generate relocation entries for all PC relative references. If the instruction size doesn't change these relocations are not required.

Linker
======

There are actually two linkers: the static linker, which creates a shared object or executable, and the dynamic linker or loader, which finalizes addresses and performs relocations. Ignoring relocations, the role of a static linker is essentially ``cat``, while the loader is more like ``unzip``. Although the loader also has to search through the filesystem for all the shared objects, again similar to ``cat``. The static linker mostly deals with sections while the dynamic linker uses segments; there are only a few types of segments but lots of section types.

The static linker can be replaced after a lot of work by a language-specific linker and object format. For example the Go project uses its own linker and object format (bastardized ELF). This allows more freedom in defining symbols and for additional metadata to be stored in the objects. We can also completely get rid of object files and store the information in a database. The database would allow using the same incremental build system that the compiler uses. But for a first pass it might be a bit much; GHC uses the system assembler and linker.

The loader is much harder to replace, in terms of inertia; although it can be changed to a non-standard path, distributing it would be difficult, and using a different shared object format would likely go the way of `FatELF <https://icculus.org/finger/icculus?date=2009-11-03&time=19-08-04>`__ (nowhere). Plus there are features like ASLR and lazy loading that would have to be reimplemented. So Stroscot should definitely produce outputs that the loader can understand.

A linker needs to:

    Find all symbol definitions that live in each object file and library.
    Assign each symbol a final, absolute, address.
    Find all symbol references in each object file and library.
    Replace all symbol references with the absolute address of that symbol.
    Write completed executable to memory (loader) or file (linker).

Blocks
======

From a user perspective there are two types of jumpable addresses:

memory - effective address computation
SIB addressing form, where the index register is not used in address calculation, Scale is ignored. Only the base and displacement are used in effective address calculation.
VSIB memory addressing



Memory and the program counter are virtualized as well, using labels. A label refers to a memory location with a specific block of code loaded. The blocks are not ordered, so unconditional jumps must be inserted between blocks if necessary. The block order can be determined using profiling, removing the unconditional jump that is taken most often.

Memory references should be virtualized as well, so we also have memory labels. The alignment and format of the memory address should be specified.

Instructions and blocks are marked by the virtual registers they consume and use (input / output registers). The call and jump instructions are special in that a mapping may be given between the virtual registers and physical registers. Instruction constraints:
* Output: the register must not contain a value used after the block
* Output early clobber: output and the register must not be used for any inputs of the block
* Input: the register is read but not written to. Multiple inputs may all be assigned to the same register, if they all contain the same value.
* Tied input: register that is read and written
* Tied input early clobber: register that is read and written and does not share a register with any other input
* alignstack, sideeffect

There are also constraints from the ABI calling convention: https://gitlab.com/x86-psABIs/x86-64-ABI

Output formats
==============

* native binary
* shared object / DLL (main difference is position-independent code)
* static object file

Debugging information
=====================

Debugging information is essentially a complete fiction. After optimization and transformation the output machine code bears no resemblance to the original program. But debuggers needs to know which machine code instruction corresponds to which source code location. So DWARF information should be generated as early as possible, ideally right after parsing, and then propagated through each transformation.

DWARF is oriented around traditional compilation units and thus it might not quite flexible enough for our purposes. But it's a standard and GHC does it so it should be reasonable.

the linker performs a global program analysis to find all reachable interface types and discard methods that don’t match any signatures in reachable interface types and cannot be called via reflection.

A key structural issue with the current linker is that it expects to do everything in memory. It deserializes all of the input objects into the heap and produces the output in memory as well. As a result, its peak memory footprint includes the entirety of the inputs (even if it eliminates most symbols as unreachable). Many of the linker’s issues revolve around this design choice.

Remove/cache work on the critical path (linker is critical). Use incremental build system with fingerprinting.

linker algorithm:
* goal: avoid deserializing relocs/metadata if possible
* (in parallel) mmap the inputs read-only

  * mmap manager to avoid mmap-ing too many files

* (mapreduce) build global symbol table

  *  while we can read the input symbol tables in any order, we must add them to the global symbol table in the order given on the command line.

* (in parallel) build bitmap of reachable symbol names by DFS through symbol table / inputs. Bias priority towards staying in package.
* don't compact reachable symbols, because keeping a simple mapping to the original symbol indexes is likely more valuable.
* mmap the output file read/write
* (in parallel) copy symbol data to output

  * store symbol data in temporary scratch with minimal lifetime

* apply relocations directly to the mmapped output.

new object format:
* symbol index
* fixed width
* pack byte data so mmap can skip over it
* int-indexed symbol table - global table built by the linker

  * native Go reference (import index, symbol index)

    * imported package table in referencing code unit
    * exported symbol definition table in each package

  * Linknamed symbols - symbols defined in assembly that can only be resolved via their names.

    * identified at their definition site, discard after object loading.

  * “Dupok” symbols - coalesced/deduplicated symbols. content-addressed and deduplicated via a separate table.

directly load a Go object file into its running image
link and execute a test in a single step, rather than producing a binary that will be discarded almost immediately


if all code is position-independent and we retain entire packages, then all regular symbol references can be done with nothing more than the base address of the package that contains the symbol, by statically baking in the offsets of all the symbols. This would make offset tables incredibly small, though would make cross-package symbol references more expensive.

processing relocations should be a fundamental part of any package for working with object files.

libc
====

libc is not really part of the system proper, but in practice a lot of programs end up depending on it anyway.

Reason one is that libc wraps all the syscalls. To avoid this, the pioneer here is Go with their own syscall implementation. OTOH the implementation exposes `bugs <https://marcan.st/2017/12/debugging-an-evil-go-runtime-bug>`__. This means that reimplementing syscalls will probably run into more bugs, but if the implementation follows Go's closely this might not be an issue. And it should be faster / less register pressure to do syscalls in assembly than to set up a C stack and call into libc. But on various systems (`OpenBSD <https://utcc.utoronto.ca/~cks/space/blog/programming/Go116OpenBSDUsesLibc>`__, Illumos, Solaris) avoiding libc isn't possible because system calls must be made through the system libc.

Reason two is compatibility; a lot of programs interface with C by calling C libraries. Facilities such as malloc and errno can be avoided / reimplemented but in general the only way to get a working program is to use the C runtime.

Overall, it seems to a first approximation that small executables on Linux are the only C-free possibility. In particular Go's net package depends on system C APIs everywhere except Linux. But these toy Linux programs are the kind of programs that people use for comparisons on system programming, so it still seems to be worth implementing. There's that "cool factor" of one less dependency. For example Zig uses direct syscalls, but also implements a link_libc flag that turns it off.

The syscalls themselves take / modify C structs. So regardless of whether we link with libc, we still need a C parser / ABI to get anywhere.

compile to C - you're only compiling to a subset, since C includes inline assembly. It's a design choice that saves some implementation complexity by adding a big/slow dependency. Better to make it optional.

Linux syscalls
--------------

Parsing all the syscalls requires either manually writing them out / copying them from `somewhere <https://filippo.io/linux-syscall-table/>`__ or doing a lot of kernel source spelunking. Go has some stuff `here <https://pkg.go.dev/golang.org/x/sys/unix?utm_source=godoc>`__ (`script <https://cs.opensource.google/go/x/sys/+/master:unix/linux/mkall.go>`__): it generates syscall numbers and constants / `struct definitions <https://utcc.utoronto.ca/~cks/space/blog/programming/GoCGoCompatibleStructs>`__ from the headers.

The only place the syscall arguments are defined is in individual files with macros from the family `SYSCALL_DEFINEx <https://lwn.net/Articles/604287/>`__ (e.g. `io_uring_setup <https://github.com/torvalds/linux/blob/141415d7379a02f0a75b1a7611d6b50928b3c46d/fs/io_uring.c#L9737>`__). We have to run the preprocessor for true correctness; the best option seems to be hooking the macro to print out the arguments with `diagnostic pragmas <https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html#Diagnostic-Pragmas>`__. Although scraping the files directly with grep + parentheses matching seems like it would work alright.

The actual convention is documented `here <https://stackoverflow.com/questions/2535989/what-are-the-calling-conventions-for-unix-linux-system-calls-and-user-space-f/2538212#2538212>`__ and `here <https://manpages.debian.org/unstable/manpages-dev/syscall.2.en.html>`__. The syscall number is expected in rax, return values in rax and rdx. otherwise all registers, segments and eflags are saved. Arguments left to right are rdi, rsi, rdx, r10, r8, r9.

Signed range of -4096 < eax < 0 is an error code, anything else may be an orderly return value




When the Linux kernel starts a program, it looks for a PT_GNU_STACK segment. If it does not find one, it sets the stack to be executable (if appropriate for the architecture). If it does find a PT_GNU_STACK segment, it marks the stack as executable if the segment flags call for it. (It’s possible to override this and force the kernel to never use an executable stack.) Similarly, the dynamic linker looks for a PT_GNU_STACK in any executable or shared library that it loads, and changes the stack to be executable if any of them require it.

When this all works smoothly, most programs wind up with a non-executable stack, which is what we want. The most common reason that this fails these days is that part of the program is written in assembler, and the assembler code does not create a .note.GNU_stack section. If you write assembler code for GNU/Linux, you must always be careful to add the appropriate line to your file. For most targets, the line you want is:

.section .note.GNU-stack,"",@progbits

There are some linker options to control this. The -z execstack option tells the linker to mark the program as requiring an executable stack, regardless of the input files. The -z noexecstack option marks it as not requiring an executable stack. The gold linker has a --warn-execstack option which will cause the linker to warn about any object which is missing a .note.GNU-stack option or which has an executable .note.GNU-stack option.

The execstack program may also be used to query whether a program requires an executable stack, and to change its setting.

These days we could probably change the default: we could probably say that if an object file does not have a .note.GNU-stack section, then it does not require an executable stack. That would avoid the problem of files written in assembler which do not create the section. It’s possible that this would cause some programs to incorrectly get a non-executable stack, but I think that would be quite unlikely in practice. An advantage of changing the default would be that the compiler would not have to create an empty .note.GNU-stack section in all object files.

By the way, there is one thing you can do with a normal function that you can not do with a nested function: if the nested function refers to any variables in the enclosing function, you can not return a pointer to the nested function to the caller. If you do, the variable will disappear, so the variable reference in the nested function will be dangling reference. It’s worth noting here that the Go language supports nested function literals which may refer to variables in the enclosing function, and when using Go this works correctly. The compiler creates variables on the heap if necessary, so they do not disappear until the garbage collector determines that nothing refers to them any more.

Finally, I’ll mention that there are some plans to implement a different scheme for nested functions in C, one which does not require any memory to be both writable and executable, but these plans have not yet been implemented. I’ll leave the implementation as an exercise for the reader.




Modern ELF systems can randomize the address at which shared libraries are loaded. This is generally referred to as Address Space Layout Randomization, or ASLR. Shared libraries are always position independent, which means that they can be loaded at any address. Randomizing the load address makes it slightly harder for attackers of a running program to exploit buffer overflows or similar problems, because they have no fixed addresses that they can rely on. ASLR is part of defense in depth: it does not by itself prevent any attacks, but it makes it slightly more difficult for attackers to exploit certain kinds of programming errors in a useful way beyond simply crashing the program.

Although it is straightforward to randomize the load address of a shared library, an ELF executable is normally linked to run at a fixed address that can not be changed. This means that attackers have a set of fixed addresses they can rely on. Permitting the kernel to randomize the address of the executable itself is done by generating a Position Independent Executable, or PIE.

It turns out to be quite simple to create a PIE: a PIE is simply an executable shared library. To make a shared library executable you just need to give it a PT_INTERP segment and appropriate startup code. The startup code can be the same as the usual executable startup code, though of course it must be compiled to be position independent.

When compiling code to go into a shared library, you use the -fpic option. When compiling code to go into a PIE, you use the -fpie option. Since a PIE is just a shared library, these options are almost exactly the same. The only difference is that since -fpie implies that you are building the main executable, there is no need to support symbol interposition for defined symbols. In a shared library, if function f1 calls f2, and f2 is globally visible, the code has to consider the possibility that f2 will be interposed. Thus, the call must go through the PLT. In a PIE, f2 can not be interposed, so the call may be made directly, though of course still in a position independent manner. Similarly, if the processor can do PC-relative loads and stores, all global variables can be accessed directly rather than going through the GOT.

Other than that ability to avoid the PLT and GOT in some cases, a PIE is really just a shared library. The dynamic linker will ask the kernel to map it at a random address and will then relocate it as usual.

This does imply that a PIE must be dynamically linked, in the sense of using the dynamic linker. Since the dynamic linker and the C library are closely intertwined, linking the PIE statically with the C library is unlikely to work in general. It is possible to design a statically linked PIE, in which the program relocates itself at startup time. The dynamic linker itself does this. However, there is no general mechanism for this at present.

* Dead code elimination
* Uncluttered syntax without string constants or backslashes - intrinsic functions. One function per instruction, no point in making sequences.

hardest problem: calls. The semantics of a call are inherently system/ABI dependent, to the point of not being captured in a target triple. The semantics thus have to be described at the call site. But the data format doesn't really matter as the call instruction will most likely be wrapped / generated.

Josh Haberman wrote:
> Josh Haberman  gmail.com> writes:
> > Mike Pall  mike.de> writes:
> > > The main loop of an interpreter is a tough job for compilers (and
> > > CPUs). Most interpreters have been written in C, so C compilers
> > > have been tuned for this case over the years. They still generate
> > > mediocre code, compared to what you can achieve in assembler.
> >
> > Could you possibly summarize the few biggest strategies that you
> > use to beat C compilers when it comes to interpreter loops?  I'm
> > really interested in this problem.  I can (and do) read the LuaJIT
> > code, but that doesn't tell me what was deficient about the original
> > compiler output.
>
> Hmm, this probably seems overly broad.  Maybe a better question
> to ask would be: what do compilers still suck at, in the context
> of interpreter main loops?  Common wisdom is that compilers these
> days are so good that a human can rarely ever beat them.  Clearly
> that's wrong in the case of LuaJIT, but how so?  What bad decisions
> do compilers make in interpreter loops?

LuaJIT's interpreter is fast, because:

    It uses indirect threading (aka labeled goto in C).
    It has a very small I-cache footprint (the core of the interpreter fits in 6K).
    The parser generates a register-based bytecode.
    The bytecode is really a word-code (32 bit/ins) and designed for fast decoding.
    Bytecode decode and dispatch is heavily optimized for superscalar CPUs.
    The bytecode is type-specialized and patched on-the-fly.
    The dispatch table is patched to allow for debug hooks and trace recording. No need to check for these cases in the fast paths.
    It uses NaN tagging for object references. This allows unboxed FP numbers with a minimal cache footprint for stacks/arrays. FP stores are auto-tagging.
    It inlines all fast paths.
    It uses special calling conventions for built-ins (fast functions).
    Tons more tuning in the VM ... and the JIT compiler has it's own bag of tricks.

The control-flow graph of an interpreter with C switch-based
dispatch looks like this:

      .------.
      V      |
      |      |
      L      |  L = instruction load
      D      |  D = instruction dispatch
   / /|\ \   |
  / / | \ \  |
  C C C C C  |  C = operand decode
  X X X X X  |  X = instruction execution
  \ \ | / /  |
   \ \|/ /   |
      |      |
      V      |
      `------'

Each individual instruction execution looks like this:

  ......V......
  :X    |     :
  :     |\ \  :
  :     F S S :  F = fast path
  :     |/ /  :  S = slow paths
  :     |     :
  :.....V.....:

We're talking here about dozens of instructions and hundreds of
slow paths. The compiler has to deal with the whole mess and gets
into trouble:

* Diamond-shaped control-flow is known to be the worst-case
  scenario for most optimizations and for register alloction.
  Nested diamond-shaped control-flow is even worse.

* The compiler doesn't have enough hints to see what the fast
  paths and what the slow paths are. Even if you'd be able to tell
  it, it still sees this as a single giant control-flow graph.

  Anything in this loop could potentially influence anything else,
  so almost nothing can be hoisted or eliminated. The slow paths
  kill the opportunities for the fast paths and the complex
  instructions kill the opportunities for the simpler instructions.

* The standard register allocation heuristics fail at this scale,
  so the compiler has trouble keeping important variables in
  registers.

We can use a direct or indirect-threaded interpreter even in C,
e.g. with the computed 'goto &' feature of GCC:

  * * * * *
  | | | | |
  C C C C C    C = operand decode
  X X X X X    X = instruction execution
  L L L L L    L = next instruction load
  D D D D D    D = next instruction dispatch
  | | | | |
  V V V V V

This effectively replicates the load and the dispatch, which helps
the CPU branch predictors. But it has its own share of problems:

* There's no loop the compiler could recognize. And all of those
  gotos can jump to pretty much anywhere in the code. Therefore
  the compiler is unable to hoist anything, because there _will_
  be a slow path where an aliasing store kills all opportunities.

* The register allocator can only treat each of these segments
  separately and will do a real bad job. There's just no way to
  give it a goal function like "I want the same register
  assignment before each goto".

* Tail-merging and CSE will happily join all these common tails of
  each instruction and generate a single dispatch point. Ick. You
  can try to disable some optimizations for this piece of code,
  but this will negatively impact all paths.

* There's a point where you start to fight the compiler and this
  is a game you cannot win.

If you write an interpreter loop in assembler, you can do much
better:

* Keep a fixed register assignment for all instructions.

* Keep all important state in registers for the fast paths. Spill/reload
  only in the slow paths. (No C compiler manages to do that on x86.)

* Move the slow paths elsewhere, to help with I-Cache density.

* Pre-load instructions and pre-decode operands.

Here's how this would look like:

  *  *  *  *  *
  |  |  |  |  |
  C  C  C  C  C    C = partial operand decode for this instruction
  F> F> F> F> F>   F = fast path, > = exit to slow path
  L  L  L  L  L    L = next instruction load
  C  C  C  C  C    C = partial operand decode for the next instruction
  D  D  D  D  D    D = next instruction dispatch
  |  |  |  |  |
  V  V  V  V  V

You can get this down to just a few machine code instructions.
E.g. x=x+1 is turned into the ADDVN bytecode. This means it's specialized for the 2nd operand to be a constant. Here's the x86 code (+ SSE2 enabled) for this instruction:

// Prologue for type ABC instructions (others have a zero prologue).
movzx  ebp, ah                  Decode RC (split of RD)
movzx  eax, al                  Decode RB (split of RD)

// The instruction itself.
cmp    [edx+ebp*8+0x4], -13     Type check of [RB]
ja     ->lj_vmeta_arith_vn
movsd  xmm0, [edx+ebp*8]        Load of [RB]
addsd  xmm0, [edi+eax*8]        Add to [RC]
movsd  [edx+ecx*8], xmm0        Store in [RA]

// Standard epilogue: decode + dispatch the next instruction.
mov    eax, [esi]               Load next bytecode
movzx  ecx, ah                  Decode RA
movzx  ebp, al                  Decode opcode
add    esi, 0x4                 Increment PC
shr    eax, 0x10                Decode RD
jmp    [ebx+ebp*4]              Dispatch to next instruction

Yes, that's all of it. I don't think you can do this with less instructions. This code reaches up to 2.5 ipc on a Core2 and takes 5-6 cycles (2 nanoseconds on a 3 GHz machine).

BTW: For the LuaJIT/ARM interpreter I had to add even more crazy stuff to make it fast. The assembler code for the LuaJIT/x86 interpreter is rather straightforward in comparison. I don't think you're going to see any compiler generate code like this, anytime soon (not even my own).

Here's a dump of the ARM dual-number/soft-float machine code for the ADDVN bytecode of LuaJIT (add of variable + number constant). It gives a good example of the kind of optimizations that are only possible with assembler:

and   r12, r4, lr, lsr #21     // Decode RB * 8
and   r11, r4, lr, lsr #13     // Decode RC * 8
ldrd  r0, [r9, r12]            // Load TValue from BASE[RB]
ldrd  r2, [r5, r11]            // Load TValue from KBASE[RC]
|ldrb r12, [r6]                // Load next opcode
cmn   r1, #14                  // 1st operand is integer?
cmneq r3, #14                  // And 2nd operand is integer?
bne   >2                       // No, try FP variant
adds  r0, r0, r2               // Yes, do integer add
bvs   ->lj_vmeta_arith_vn      // Fallback on overflow

1:
|ldr  lr, [r6], #4             // Load next instruction, increment PC
strd  r0, [r9, r10]            // Store TValue result in BASE[RA]
|ldr  r12, [r7, r12, lsl #2]   // Load code address for next opcode
|and  r10, r4, lr, lsr #5      // Pre-decode next RA * 8
|lsr  r11, lr, #16             // Pre-decode next RD
|bx   r12                      // Jump to code for next opcode

2:  // FP variant
cmn   r1, #14                  // 1st operand is number?
cmnlo r3, #14                  // And 2nd operand is number?
bhs   ->lj_vmeta_arith_vn      // Fallback if not
bl    extern __aeabi_dadd      // Soft-float add
|ldrb r12, [r6]                // Reload volatile opcode reg
b <1

    r4 is pre-initialized to 0x7f8 (255*8), which allows fast decoding and scaling of the 8 bit operands inside the 32 bit instruction word. The pre-scaling of operands is required for the subsequent 'ldrd' instruction, which only allows base+offset or base+index addressing.

    'ldrd' loads a 64 bit value into two consecutive registers. This conveniently allows loading a TValue from the stack or the constant table with a single instruction. The hi-word has the type code, which overlaps with the hi-word of doubles. Similarly, 'strd' allows storing a TValue in one go -- that's either a double or an integer + type code.

    The type codes are small negative numbers (NaN-tagged values), which allows for a fast type check with 'cmn' (compare negated). Integers are at -14, other types are at -1..-13, numbers occupy the remaining space (hiword of a double).

    The checks can be chained with predicated instructions, e.g. cmn + cmneq + bne (both are integers) or cmn + cmnlo + bhs (both are numbers). The fast paths are always the straight line fall-through paths, e.g. the integer add in this example.

    Some other operations, e.g. bit.* allow even more streamlined type checks, e.g. cmn + blne to a subroutine that handles the (uncommon) non-integer cases. It starts with a bhi to the fallback code (not a number) and continues with an inlined conversion from numbers to integers.

    If you carefully look at the load latencies (2 cy) and the early register constraints (for addresses and stored values), you'll see the above code doesn't have any stalls. All operations are carefully interleaved, based on the data dependencies. Even the next opcode dispatch (marked with '|') is interleaved with the current opcode execution.

    Also note that the pre-decoding of the operands for the next instruction is done in the delay slot of the load of the machine code address for the next opcode. The decoded RD is used for many instructions, but not for the ADDVN instruction shown here (OTOH not doing it would just waste a delay slot).

    Yes, this bytecode instruction could be split into two instructions. One for the integer and FP variant, each. And with dynamic bytecode patching to adapt to runtime behavior. But one needs a state machine and a dual-variant to prevent infinite re-patching due to type instability. That would add too much complexity and increase the I-cache footprint a lot, for little gain (and ARM has very small caches).

    The JIT compiler specializes to the runtime type, anyway. And it's able to turn that into an adds + bvs for the integer case. The overflow check can be eliminated in some cases, which leaves only an add instruction. It's a tad more complex in practice, than it sounds, though. :-)


On PPC/e500 I had to use a couple more tricks: e.g. merging the
operand decode and the index scaling. That crazy 'rlwinm'
instruction comes real handy here. Or hand-scheduling the
instruction load above the stores in the fast path. Or using
vector (!) instructions for type checks.

There's just no way you can reasonably expect even the most
advanced C compilers to do this on your behalf.

There are some more design goals for an interpreter, like having
only a single fast path in every bytecode instruction etc. ...
I won't go into that here, but remember: every percent counts!

Final words: A modern compiler contains a huge set of heuristics
that interact with each other in strange ways. They have been
tuned for 'average' code. But an interpreter is a very different
beast, so you'll inevitably get disappointing results.

Implemented

(This stuff may still be a work-in-progress, but significant parts are done)
Nanboxed value representation
Copying, quad-color incremental, generational garbage collector
Arena-based bump allocator for heap-allocated values
Miscellaneous type-safe efficient data structures
Memory allocator API
In-progress
SSA interpreter
Unboxed packed string representation for short ASCII strings
Unicode-correct String implementation
Unimplemented
Saving snapshots of the VM state (images)
Numeric tower
libffi-based cffi
Compiler from IROHA to low-level Phire bytecode
Tracing JIT compiler

Use libgccjit for code generation?
Far Future
Optimized assembly interpreter a la LuaJIT and JavaScriptCore
Concurrent garbage collection

Virtual memory API

API to abstract over VirtualAlloc/mmap.

    [X] POSIX implementation using mmap+posix_madvise
    [ ] Win32 implementation using VirtualAlloc

Should pages be context-managed?

Pros:

    Consistency
    Ease of integrating into existing arena implementation
    Efficiency?
        madvise on Linux can decommit many pages at once

Cons:

    Requires allocating memory to keep track of owned pages

Solution
Compile-time decision on which virtual memory backend to use

No runtime overhead and the APIs are similar enough that this is easy.
Requires platform abstraction layer of a sort

file:../src/platform.h and file:../src/platform/posix.h
Allocator subclasses should provide their own way of managing reserved pages

For example, using the object-level allocator to manage a linked list.
Forget mix_alloc et al

Such a deep hierarchy is something of a design smell, not to mention loses efficiency quickly. Allocating memory shouldn’t involve chasing 3 levels of function pointers.
Changes to existing APIs
yu_err alloc(allocator *ctx, void **out, size_t num, size_t elem_size, size_t alignment);

Behaves much like it does currently. However, it should keep track of allocated and usable size.
yu_err realloc(allocator *ctx, void *ptr, size_t num, size_t elem_size, size_t alignment);

Like the current realloc, but it must make an effort to resize the allocation in-place if possible. In particular, if usable_size() returns >= num * elem_size, realloc() must resize in-place.
TBD: “Sticky alignment”

Should alignment = 0 use the current alignment of `ptr` or the default alignment? (i.e. should alignment be “sticky”) Does it ever make sense to reallocate to a default alignment if previously allocated to a non-default?
void free(allocator *ctx, void *ptr);

No differences to current free contract.
New APIs
size_t allocated_size(allocator *ctx, void *ptr);

Rationale: the allocator must keep track of this information anyway (primarily for realloc). Providing an API to access it may reduce redundancy in some cases. However, this function need not be particularly fast, and may have undesirable effects on the CPU cache.
size_t usable_size(allocator *ctx, void *ptr);

Many allocators over-allocate slightly for a variety of reasons (having separate free lists for different sizes of object, being a power-of-2 allocator, etc). The application should have access to this information to reduce unnecessary allocations.
size_t reserve(allocator *ctx, void **out, size_t num, size_t elem_size);

Reserves a section of virtual memory. Callers may rely on this function not necessarily committing the address space to physical memory, which makes this somewhat difficult to shim with dmalloc() for debugging.

Since most or all OSes reserve memory at the page-level, this function returns the number of bytes actually reserved, which must be >= the requested size.
void release(allocator *ctx, void *ptr);

Unreserve all addresses starting at ptr. Portions of a reserved space cannot be released individually due to restrictions in VirtualAlloc. `ptr` must be the out pointer from reserve().
yu_err commit(allocator *ctx, void *ptr, size_t num, size_t elem_size);

Commit pages of virtual memory to physical memory. All pages containing addresses in the interval [ptr,ptr+num*elem_size) should be committed. `ptr` need not fall on a page boundary.
void decommit(allocator *ctx, void *ptr, size_t num, size_t elem_size);

Decommit all pages containing addresses in the interval [ptr,ptr+num*elem_size). `ptr` need not fall on a page boundary.
Removed APIs
array_alloc, array_realloc, array_free, array_len

While potentially useful, these haven’t actually been used.
Creating an allocator
Move to a pseudo-subclass system

Somewhat cleaner than the current system of polymorphism. Currently allocators must cast ctx->adata to their internal data structure. If they require multiple structures, then they have to define a special combination struct just to act as the adata member. This also makes stack allocating them somewhat awkward. With a subclass system, methods can take the allocator struct directly and be cast to the proper function signature in context init.

c.f. Linux kernel, SQLite
Example

struct my_allocator {
  yu_mem_funcs base;  // Must be first struct member
  data_structure bookkeeping;
  ...
};

yu_err my_allocator_init(struct my_allocator *ctx) {
  ctx->base.alloc = (yu_alloc_fn)my_alloc;
  ...
}

yu_err my_alloc(struct my_allocator *ctx, void **out, size_t num, size_t elem_size) {
  ...
}

...

Remaining problems
Should the origin of a pointer matter to free/allocated_size/usable_size?

This is one of the big reasons to prefer allocators even having to manage page-level allocations. Should free() on pointer allocated with reserve() be equivalent to doing decommit+release on that pointer? Additionally, should allocated_size() and usable_size() behave as expected? At the moment, I am inclined to say yes, though this complicates the implementation of an allocator. Such a dichotomy would, to an extent, defeat the point of including page-level allocation in the allocator API to begin with.

Seeing as reserved pages must be released when the context is freed, this does not necessarily introduce additional complexity into allocator implementations.

On top of that, this makes automatically placing large objects in their own space without metadata much easier.
Should reserve() be required to provide an address allocated on a page boundary?

Since it will have access to the object-level allocator for bookkeeping, I am inclined to say yes.
(size_t num, size_t elem_size) vs (size_t bytes)

The former is slightly more awkward since most allocations will be sized in bytes. However, it reduces the possibility for accidental overflow (by multiplying signed ints to determine size, for example). Additionally, it opens the possibility of using an elem_size of 0 to be a default, such as system page size (this does not make sense for commit() since the caller has no way to determine how much was actually committed, but reserve() provides that information). The utility of that is perhaps somewhat limited.
Sticky alignment

Pros:

    Removes possibility of accidentally reallocating to the default alignment.

Cons:

    Reallocating to a stricter alignment may be useful (for objects allocated to their size), so the alignment parameter must remain anyway.
    May require extra bookkeeping. The pointer cannot be trusted to contain the actual alignment, since it may be located stricter than necessary.

    The alloc-ng branch of the git repository contains the prototype for a next-generation allocator API. This new API is lower-level than the existing API, but the design is considerably more flexible and offers many potential performance improvements.
Goals
See file:../src/yu_alloc.h
Fast, 64-bit optimized, virtual-memory-aware allocator
Explicit control over virtual memory pages
Embrace over-reserving addresses

64-bit address space is almost unlimited. Reserving the entire object heap of addresses is feasible if we don’t have to commit it.
Minimize allocations

Have an explicit API to get the real usable size of an allocation. Care must be taken not to unnecessarily expose implementation details. However, this could avoid unnecessary calls to allocator functions in a number of circumstances
Progress
Spec out allocator API
See file:alloc-ng-specv2.org
[#B] Platform abstraction layer for virtual memory
See file:../src/platform.h

    [X] POSIX (mmap+posix_madvise) implementation
    [X] Linux (mmap+madvise) implementation Linux has a little more flexibility than the pure POSIX API.
    [X] Win32 (VirtualAlloc) implementation Untested

[#A] Test suite for new features

    [X] Allocated/usable size functions
    [X] Reserving/releasing virtual memory
    [X] Committing/decommitting virtual memory
    [X] Both at once
    [X] Reserving roughly at a fixed address
    [X] Reserving exactly at a fixed address
    [X] Context management of pages

[#A] Provide a wrapper for system malloc()
Can use system malloc() to implement bookkeeping for managing pages.
[#C] Provide a debug allocator

Probably based on dmalloc. Requires implementing ‘extras’, especially since dmalloc does not support aligned_alloc. The existing debug_alloc may be of use. However, a true debug_alloc will have to track page-level allocations as well. Shimming this on top of dmalloc may not work, since subsystems expect to be able to reserve large address spaces. Might be OK on Linux with its lazy committing policy.
[#C] Provide a higher-performing allocator wrapping jemalloc APIs directly

jemalloc provides more control like aligned realloc and getting usable size. This does require using experimental jemalloc APIs, but should be much more efficient than shimming the functionality.
[#A] Update existing subsystems to new allocator API
Changes in most cases should be relatively minimal.

    [X] Test framework (test/test.h)
    [X] yu_buf, yu_str
    [X] test_alloc
    [X] Object arenas
    [X] Generic data structures

[#B] Update existing subsystems to make efficient use of new features

Arenas in particular can make efficient use of reserving huge chunks of addresses without actually committing. Part of the reason for the whole redesign is to improve garbage collector performance.

Data hazards: RAW is unavoidable. WAR/WAW can be eliminated by renaming as in the `Tomasulo algorithm <https://en.wikipedia.org/wiki/Tomasulo_algorithm>`__. WAW can be also ignored if the value isn't used.


memory layout can affect program performance. see profile guide memory layout thesis

CPU model:

    Fetch / decode / cache / fuse instructions into micro-ops and place into queues
    Retrieve the next uop instruction from the head of the instruction queues.
    record physical register names of logical register inputs
    assign new physical registers to output logical registers
    stall the instruction until a station is free.
    assign the station to the instruction
    stall the instruction until all physical input registers become available.
    execute the instruction at the station.
      store/load interact with memory order buffer
        memory prefetching - Processor does lookahead and fetches early. Stall if not available/
      zeroing a register is a no-op because all physical registers are initialized to zero
      "retired" - finished executing
    buffer outputs in reorder buffer until earlier instructions have completed
    un-stall instructions at stations that now have their inputs available - can take some cycles for cross-station RAW dependencies

instruction scheduler
schedule the instructions intelligently such that they will arrive at the corresponding position in the pipeline at the exact cycle when the data will be available to them.

branch prediction: branch target buffer (BTB), indirect branch target array, loop detector and renamed return stack buffer. mispredicted branch clears cache and restarts.



timing of instructions - most are fixed. load operations depend on what's cached.

Register allocation. At each program point there is a map from variables to registers or memory. Registers are limited but fast. Send variables that do not fit to memory, spilling the least used variables and filling them back when needed (copy to/from memory). Because of register renaming / memory buffering, the actual register / address assignment doesn't matter, only the spill pattern. (TODO: check this with some benchmarks)

data: read/write of instructions. control flow graph.

Registers and memory have relatively similar APIs: read, and write.

File and network APIs are generally managed by user-level code. So the point of the memory system is to assign a storage location for every value, insert moves / frees where necessary, and overall minimize the amount of resources consumed.

For more advanced programming there is the need to avoid the use of slow storage mechanisms as much as possible by addressing the fast storage mechanisms directly. (Really?)

ELF
===

The kernel/loader only uses segments when loading executables into memory. So we don't need to bother with sections. Ignoring one-offs and notes (comments), there is only one segment `type <http://www.sco.com/developers/gabi/latest/ch5.pheader.html#p_type>`__, , a loadable segment PT_LOAD. The attributes are ``flags, offset, vaddr, filesz, memsz, align``. ``filesz <= memsz``. ``filesz`` bytes starting from offset from the file are mapped to memory starting at ``vaddr``. If ``memsz > filesz``, the extra bytes are defined to hold the value 0 and to follow the segment's initialized area. ``vaddr - offset mod align == 0``; ``align`` is usually the page size. ``flags`` defines the permissions that mmap uses and can be any combination of read/write/execute.

So ignoring the file format / alignment / special handling of ending with 0's, a loadable segments is ``Load {flags, vaddr, contents : [Byte]}``.

`PE <https://docs.microsoft.com/en-us/windows/win32/debug/pe-format>`__ is similar, the handling of alignment is different. Mach-O doesn't even have a page currently available from Apple so who cares.

Assembly
========

A segment may have executable pieces but `also <https://stackoverflow.com/questions/55607052/why-do-compilers-put-data-inside-textcode-section-of-the-pe-and-elf-files-and>`__ pieces of non-executed data: this is used with GHC's `tables-next-to-code layout <https://lists.llvm.org/pipermail/llvm-dev/2012-February/047555.html>`__ and also ARM's "constant islands" or `literal pools <https://en.wikipedia.org/wiki/Literal_pool>`__. Conceptually the pieces are just smaller segments, but an actual segment is sized to a multiple of the page size. So to convert pieces to segments we would start with single-page segments with permissions the union of the permissions of the contained pieces, zeroing the memory if no piece defines it, and then merge together adjacent segments with the same permissions.

We can write executable pieces using our instruction templates, ``Piece = [{flags | executable, vaddr, contents : [Instruction]}]``, where ``Instruction = (InstructionTemplate, Operands)`` (or actually an ADT because the number of operands is fixed by the template).

Labels: we split ``[Instruction]`` in each executable piece into blocks, ``(Label,Block) where Block = [Instruction]``. To form a piece the labels are erased and the blocks concatenated.

determine the size of all the assembled code and data
generate code using symbol addresses
code's size cannot depend on the value of a symbol declared after the code in question.

Code layout: a little 1D `constraint language <https://developer.android.com/reference/androidx/constraintlayout/widget/ConstraintLayout>`__:
* fixed address
* start/end of A is eq/leq/geq/lt/gt a constant plus start/end of B
* align x A, ensure start of A is a multiple of x.

Generally the smallest layout wins, but the layout is also optimized for cache coherence.

IR Style
--------

Goals:
* represent non-local control flow (faults)
* optimizations are localized (read small portion, write small portion)
* all known optimizations can be implemented
* fixes evaluation order only for stateful operations

https://cs.stackexchange.com/questions/74794/why-is-static-single-assignment-preferred-over-continuation-passing-style-in-man


Signals
=======

Signal types:
* commands: SIGKILL, SIGSTOP. No modifiable behavior.
* asynchronous, process-directed: ctrl-C, SIGINT. It can be delivered to any thread of the process and isn't necessarily delivered immediately. kernel op, kill(2) or sigqueue(2). If you have an event loop, my favorite is the "self-pipe trick". At process start time, create a pipe and set the O_NONBLOCK flag. Hold onto both ends. From the signal handler, write into the pipe (ignoring EAGAIN). In your event loop, read from the pipe in the poll/select/whatever. When there's something to read, a signal is pending. And you can dispatch the signal however you want. like signalfd but cross-platform.
* asynchronous, thread-directed. sent by tgkill(2) or pthread_kill(3). IDK.
* synchronous, thread-directed. hardware exception: SIGBUS, SIGFPE, SIGILL, SIGSEGV and traps SIGEMT, SIGTRAP. E.g. a null-pointer dereference signals that specific thread. It's delivered immediately; letting the thread continue for a while first doesn't make sense. Handled with try-catch / try-finally (`Windows SEH <https://docs.microsoft.com/en-us/cpp/cpp/structured-exception-handling-c-cpp?view=msvc-160>`__) - print stacktrace and dump `like zig  <https://github.com/ziglang/zig/blob/e2b954c2738c683a85b864eb33530f0e3dbbc480/lib/std/debug.zig#L1527>`__, or unwind to code that's unaffected. type of exception is in si_code in siginfo_t

Signal handlers can only call async-signal-safe functions, so it should fix things up and return control to the regular program. Hardcode handler into runtime and don't allow changing.

* realtime signals: queued multiple times, can carry word-sized datum. sigqueue or timer_create

The hardware exceptions are `handled <https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/kernel/traps.c>`__ in the kernel, so what's visible to the program are the signals, SIGFPE->FPE_INTDIV (DE), SIGSEGV (`PF <https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/mm/fault.c#L1487>`__, GP), SIGBUS (SS, AC), SIGILL (UD), etc.





In this email, I argue that LLVM IR is a poor system for building a
Platform, by which I mean any system where LLVM IR would be a
format in which programs are stored or transmitted for subsequent
use on multiple underlying architectures.

LLVM IR initially seems like it would work well here. I myself was
once attracted to this idea. I was even motivated to put a bunch of
my own personal time into making some of LLVM's optimization passes
more robust in the absence of TargetData a while ago, even with no
specific project in mind. There are several things still missing,
but one could easily imagine that this is just a matter of people
writing some more code.

However, there are several ways in which LLVM IR differs from actual
platforms, both high-level VMs like Java or .NET and actual low-level
ISAs like x86 or ARM.

First, the boundaries of what capabilities LLVM provides are nebulous.
LLVM IR contains:

 * Explicitly Target-specific features. These aren't secret;
   x86_fp80's reason for being is pretty clear.

 * Target-specific ABI code. In order to interoperate with native
   C ABIs, LLVM requires front-ends to emit target-specific IR.
   Pretty much everyone around here has run into this.

 * Implicitly Target-specific features. The most obvious examples of
   these are all the different Linkage kinds. These are all basically
   just gateways to features in real linkers, and real linkers vary
   quite a lot. LLVM has its own IR-level Linker, but it doesn't
   do all the stuff that native linkers do.

 * Target-specific limitations in seemingly portable features.
   How big can the alignment be on an alloca? Or a GlobalVariable?
   What's the widest supported integer type? LLVM's various backends
   all have different answers to questions like these.

Even ignoring the fact that the quality of the backends in the
LLVM source tree varies widely, the question of "What can LLVM IR do?"
has numerous backend-specific facets. This can be problematic for
producers as well as consumers.

Second, and more fundamentally, LLVM IR is a fundamentally
vague language. It has:

 * Undefined Behavior. LLVM is, at its heart, a C compiler, and
   Undefined Behavior is one of its cornerstones.

   High-level VMs typically raise predictable exceptions when they
   encounter program errors. Physical machines typically document
   their behavior very extensively. LLVM is fundamentally different
   from both: it presents a bunch of rules to follow and then offers
   no description of what happens if you break them.

   LLVM's optimizers are built on the assumption that the rules
   are never broken, so when rules do get broken, the code just
   goes off the rails and runs into whatever happens to be in
   the way. Sometimes it crashes loudly. Sometimes it silently
   corrupts data and keeps running.

   There are some tools that can help locate violations of the
   rules. Valgrind is a very useful tool. But they can't find
   everything. There are even some kinds of undefined behavior that
   I've never heard anyone even propose a method of detection for.

 * Intentional vagueness. There is a strong preference for defining
   LLVM IR semantics intuitively rather than formally. This is quite
   practical; formalizing a language is a lot of work, it reduces
   future flexibility, and it tends to draw attention to troublesome
   edge cases which could otherwise be largely ignored.

   I've done work to try to formalize parts of LLVM IR, and the
   results have been largely fruitless. I got bogged down in
   edge cases that no one is interested in fixing.

 * Floating-point arithmetic is not always consistent. Some backends
   don't fully implement IEEE-754 arithmetic rules even without
   -ffast-math and friends, to get better performance.

If you're familiar with "write once, debug everywhere" in Java,
consider the situation in LLVM IR, which is fundamentally opposed
to even trying to provide that level of consistency. And if you allow
the optimizer to do subtarget-specific optimizations, you increase
the chances that some bit of undefined behavior or vagueness will be
exposed.

Third, LLVM is a low level system that doesn't represent high-level
abstractions natively. It forces them to be chopped up into lots of
small low-level instructions.

 * It makes LLVM's Interpreter really slow. The amount of work
   performed by each instruction is relatively small, so the interpreter
   has to execute a relatively large number of instructions to do simple
   tasks, such as virtual method calls. Languages built for interpretation
   do more with fewer instructions, and have lower per-instruction
   overhead.

 * Similarly, it makes really-fast JITing hard. LLVM is fast compared
   to some other static C compilers, but it's not fast compared to
   real JIT compilers. Compiling one LLVM IR level instruction at a
   time can be relatively simple, ignoring the weird stuff, but this
   approach generates comically bad code. Fixing this requires
   recognizing patterns in groups of instructions, and then emitting
   code for the patterns. This works, but it's more involved.

 * Lowering high-level language features into low-level code locks
   in implementation details. This is less severe in native code,
   because a compiled blob is limited to a single hardware platform
   as well. But a platform which advertizes architecture independence
   which still has all the ABI lock-in of HLL implementation details
   presents a much more frightening backwards compatibility specter.

 * Apple has some LLVM IR transformations for Objective-C, however
   the transformations have to reverse-engineer the high-level semantics
   out of the lowered code, which is awkward. Further, they're
   reasoning about high-level semantics in a way that isn't guaranteed
   to be safe by LLVM IR rules alone. It works for the kinds of code
   clang generates for Objective C, but it wouldn't necessarily be
   correct if run on code produced by other front-ends. LLVM IR
   isn't capable of representing the necessary semantics for this
   unless we start embedding Objective C into it.


In conclusion, consider the task of writing an independent implementation
of an LLVM IR Platform. The set of capabilities it provides depends on who
you talk to. Semantic details are left to chance. There are features
which require a bunch of complicated infrastructure to implement which
are rarely used. And if you want light-weight execution, you'll
probably need to translate it into something else better suited for it
first. This all doesn't sound very appealing.

LLVM isn't actually a virtual machine. It's widely acknoledged that the
name "LLVM" is a historical artifact which doesn't reliably connote what
LLVM actually grew to be. LLVM IR is a compiler IR.
