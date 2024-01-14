Assembly
########

8D. The language shall not require the presence of an operating system. [Note that on many machines it will be necessary to provide run-time procedures to implement some features of the language.]

8E. There shall be a few low level operations to interrogate and control physical resources (e.g., memory or processors) that are managed (e.g., allocated or scheduled) by built-in features of the language.

11E. There shall be a machine independent interface to other programming languages including assembly languages. Any program element that is referenced in both the source language program and foreign code must be identified in the interface. The source language of the foreign code must also be identified.

Creating a high-level programming language with absolutely no runtime overhead is a challenging task. For example C has runtime overhead: the generated instruction sequences may not be optimal, memory management with malloc/free may be slower than a custom allocator, data type layouts may not be optimal, function calls follow the C calling convention which is not necessarily the fastest, and the standard library may be slower than hand-written routines.

So writing code in assembly can be advantageous. It is best to think of a high-level programming language as a macro assembler with particularly complex macros - this begs the question of why most languages make it so hard to use inline assembly. Even in a JIT-style execution model, some amount of code is compiled to assembly. The difference is there are also jumps into the interpreter, which sort of "glues together" regions of compiled code.

On the other hand, typical assembly syntaxes deviate from the rules and structures that people expect. By exposing assembly facilities as intrinsics, developers can solve performance and functionality issues without the need to learn a separate set of "low-level" assembly language conventions. Writing even the lowest level code in Stroscot style allows developers to "open up the hood" without switching to another language. Admittedly, using assembly to solve a performance problem is sometimes like using a sledgehammer to crack open an egg, but in situations such as SIMD optimization, the research is not yet at a level that allows 100% perfect code generation. Assembly intrinsics provide a viable solution to addressing this issue. Furthermore, this solution is robust, in that almost any issue can be solved by writing the desired program in assembly. Not every developer will need to use assembly to satisfy their requirements, and indeed it is generally better to use higher-level facilities, but as a fallback strategy, allowing direct assembly provides a level of control not otherwise possible. I say "direct" but in fact register allocation is not too hard to do optimally so Stroscot assembly mainly lives at the level of intrinsics rather than machine code instructions.

Architectures
=============

The first step in dealing with assembly is to decide which instruction set architectures to support. I couldn't find a list of processor architectures by popularity, but from `this quora answer <https://www.quora.com/What-kind-of-instruction-set-architecture-do-modern-processors-use>`__ and checking it by googling numbers of units sold for other random ISAs, the two primary architectures are x86-64 AMD64 (desktops) and ARM64 (mobile devices).

Others to consider as well:

* C: compilation to a self-contained C program makes porting much easier, and obviates the need for many of these architectures. Verdict: on the roadmap. Note though that this is only compiling to a subset of C - not every C program can be produced. For example, jumps (tail calls) are hard to encode in C - you either do one massive function with goto's, or `trampolining <https://en.wikipedia.org/wiki/Tail_call#Through_trampolining>`__, or non-portable TCO conventions like Manticore's JWA convention.
* WASM: it still doesn't support `tail calls <https://github.com/WebAssembly/proposals/issues/17>`__. Given the lack of progress it seems like a low priority. Verdict: Contributor.
* LLVM: The bitcode format may be worth targeting at some point. Per blog posts the API is much more unstable than the IR, and generating the IR in memory and parsing it is about as fast as using the API. Verdict: Contributor.
* RISC-V: There are $100-ish dev boards listed at https://riscv.org/exchange/boards/. No non-dev systems yet. It's a relatively simple ISA, similar to ARM. Verdict: Contributor
* 32-bit ARM: Old phones, the Raspberry Pi Zero. The XML database is similar. Verdict: Contributor.
* 32-bit x86: Old desktop PCs. From a time/effort perspective it seems cheaper to buy a new computer instead of writing support for these. Verdict: C backend or contributor.
* POWER: `Raptor <https://secure.raptorcs.com/content/base/products.html>`__ sells $5K-ish systems. Much more expensive and niche than RISC-V. Verdict: C backend.
* MIPS: the company that develops it went bankrupt and is now doing RISC-V. There are consumer systems available in China (Loongson), but the rumor is that they too are moving to RISC-V or else to their own architecture LoongArch. Verdict: C backend.
* z/Architecture: really expensive, weird OS. Verdict: C backend.
* SPARC: It's end-of-life but I guess you can still buy servers second-hand. Verdict: C backend.

From a design perspective supporting 2 architectures is not much different from supporting 10, it's just a larger set of cases, but 10 is 5x the work of 2. ARM support will be tested through QEMU, x86 natively. There are also CI services that could work (Drone). Code bloat is an issue but keeping each ISA in its own folder should avoid drift.

In addition to the basic ISAs, there are also extensions and `microarchitectures <https://en.wikipedia.org/wiki/Microarchitecture>`__ to consider. For example ARM64 is divided into v8-A, v9-A, and others. `PassMark <https://www.cpubenchmark.net/share30.html>`__ has a list of CPU shares, it's probably wildly skewed to gaming but it's better than nothing. The data on CPU cycles, ports, etc. is rather detailed and has to be generated by running benchmarking programs, so it will probably depend on user submissions; for now I'll use my own CPU (AMD A6-3650 APU).

Operating systems
=================

In planned order:

1. Linux for AMD64, because it's what I'm typing on now
2. Android for ARM, because it's my phone and it's easy to hook up
3. Windows for AMD64, which I can emulate with WINE and test fairly easily

We'll exclude Apple for now because their OS documentation sucks, they charge $100/year for a "developer license", and their anti-competitive practices mean that they would probably find some way to shut Stroscot down once Stroscot starts being a serious competitor with Swift. Of course there is nothing stopping someone else from jumping through all the hoops needed to placate Apple and making a port.

Instruction database
====================

x86 has a lot of instructions - somewhere around 1000 unique mnemonics, and many variants of those instructions. ARM too has at least a thousand instruction variants. With so many, it is clear that a structured database of instruction information is needed.

Goals
-----

* 99% Completeness - it is not too hard to cover all of the instructions mentioned in official sources, and all of the "undocumented" instructions discovered so far by tools such as sandsifter and haruspex. But outside of this, it is impossible to be complete - there are simply too many bit patterns. sandsifter/haruspex take days to run and do not even explore the full instruction space, making assumptions about the format of instructions. But these tools have confirmed that there are many undocumented instructions. Therefore, it must be assumed that the database is incomplete - more instructions may be discovered in the future. We should therefore allow raw bit patterns not present in the database, ``instr('f0 0f')`` or similar, throughout the pipeline.

* Accuracy - Generally, all data should either come directly from official sources or measurement, and be automatically generated. This allows adding new instructions, processors, and microarchitectures as quickly as they become available. Furthermore it is easy to verify the information by checking it against the sources or rerunning the tool.

* Consistency - the database should have a consistent format, structure, and representation, so that it can be easily used in the compiler. This format should be documented for accessibility.

Definition of an instruction
----------------------------

An instruction is a finite sequence of binary data (generally some number of bytes). The general idea is that instructions are a syntactic unit above bits, like words in a character string. Except unlike words, there's no instruction separator character; instructions are all run together like ``afewinstructions``. Segmenting ARM instructions is simple because they are all 32 or 64 bits. For x86, the length varies from 1 to 15 bytes and is affected by almost all parts of the instruction. `sandsifter <https://github.com/xoreaxeaxeax/sandsifter>`__ can determine the length of the first instruction in some bytes by finding an index for which ``seq|uence`` does not trigger a page fault, but ``se|quence`` does (where ``|`` is a page boundary). `haruspex <https://blog.can.ac/2021/03/22/speculating-x86-64-isa-with-one-weird-trick/>`__ is even more tricky and examines the microcode speculation buffer performance counters to see how many nops after the byte sequence were speculated. With these tools we can segment arbitrary data into x86 instructions, assuming access to the processor.

Templates
---------

Listing instructions out exhaustively one-per-line would be too much data due to exponential explosion, so instead we have a list of templates, each of which can turned into an instruction by filling in the holes. Following Xed we can call the data that is filled in "explicit operands". The explicit operands are distinguished bitstrings and can refer to registers, addresses, and immediate values. We choose each template so that it has similar behavior regardless of what is chosen for its explicit operands.

The templates should have names. For automatically generating them from sandsifter data, it could be a hash of the template string, or else the smallest unique opcode prefix, or something. But really we want to use the mnemonics from the official docs where possible, for standardization.

Intel in their docs seems to use byte-based templates, for example 64-bit ADCX is ``66 <REX.w> 0F 38 F6 <MODRM>``. The REX is 1 byte with 3 bits of varying operand data; the modrm is an operand and can be 1-6 bytes (register or memory with optional SIB/displacement).

ARM has fixed length instructions and uses a bit-based format, for example A64 ADDS is ``sf 0101011 shift* 0 Rm**** imm6***** Rn**** Rd****``. Here each name is an operand and the stars represent extra bits of the operand - the operand is a fixed-length bitstring.

A basic schema for each instruction template is:

* form name (string)
* explicit operands (list)

  * name (dest, src1, xmm1, etc.)
  * type:

    * immediate (range/size b, z, etc.)
    * register class (class GPR8, GPRv, XMM, etc.)
    * memory (size b, v, etc.)

* encoding function ``[Operands] -> Bits``

Metadata
--------

Although we should allow the possibility of having no data about an instruction besides its bit pattern, many compiler optimizations depend on having more data, such as pipelining, register allocation, instruction scheduling, and instruction selection. Basic list of data:

* conditions under which instruction is valid (processor, CPUID, valid modes such as 32-bit, real, protected, etc.)
* timing

  * latency - number of clock cycles required for execution
  * throughput - rate at which the instruction can be executed (cycles/instr)
  * whether execution time is data dependent (for cryptography)
  * micro-ops - if the instruction is broken down
  * functional units - ALU, multiplier, divider, and load/store units

* affected state - read/conditionally read/written/clobbered flags/general-purpose registers/stack pointer/memory/FP regs/SIMD regs/program counter - used in instruction reordering/scheduling and register allocation to minimize `data hazards <https://en.wikipedia.org/wiki/Hazard_(computer_architecture)#Data_hazards>`__. "undefined" in the context of Intel means "arbitrary bit-pattern may be returned". This is distinct from C's UB but matches LLVM's "undef".
* possible traps/exceptions
* pseudo-resources - to represent non-deterministic instructions, like random number generation, monitors, etc.
* semantics - the mathematical function of state + pseudo-resources -> state represented by the instruction
* Category: nop, movement, arithmetic, logic, floating point, string, cryptography, SSE, AVX, control flow, I/O, system call/privilege rings/virtualization, concurrency, (atomics, fences), cache control (prefetch/barrier), performance monitoring/debugging, virtual memory, interrupts/exceptions/traps

To handle novel instructions, there are generally sensible defaults in the absence of information (all registers set to arbitrary bit patterns, all traps possible, non-deterministic "anything goes" semantics, etc.).

Since we represent sets of instruction using templates, the metadata is conceptually specified for a given template as a function ``[Operands] -> Metadata``.

Data sources
------------

For x86, the official sources are `Intel's SDM <https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html>`__ / `AMD's Architecture Programmer's Manual <https://developer.amd.com/resources/developer-guides-manuals/>`__, which use English and pseudocode and have numerous typos (if the experiences of others hold true). Also they are only distributed as PDFs. Parsing the PDFs is a lot of work. `EXEgesis <https://github.com/google/EXEgesis>`__ uses a hacky Xpdf parser but had some amount of effort invested in it by Google before they stopped maintaining it. `x86doc <https://github.com/HJLebbink/x86doc/tree/master/Python>`__ uses pdfminer to generate HTML which seems like a more friendly starting point.

More structured but less official are x86 instruction databases:

* `Intel XED <https://intelxed.github.io/>`__ (`file <https://github.com/intelxed/xed/blob/main/datafiles/xed-isa.txt>`__). This might as well be official - although it is technically its own open-source project, I think Intel uses it internally.
* LLVM `x86 tables <https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86.td>`__
* NASM `instruction table <https://github.com/netwide-assembler/nasm/blob/master/x86/insns.dat>`__
* `GNU Assembler (gas) <https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=opcodes/i386-opc.tbl;h=b0530e5fb82f4f4cd85d67f7ebf6ce6ebf9b45b5;hb=HEAD>`__
* `iced <https://github.com/icedland/iced/blob/65d1f49584247a09dcc2559727936a53014268f5/src/csharp/Intel/Generator/Tables/InstructionDefs.txt>`__
* `OSACA <https://github.com/RRZE-HPC/OSACA/tree/master/osaca/data/isa>`__ is AGPL licensed and very incomplete
* `Ghidra <https://github.com/NationalSecurityAgency/ghidra/blob/master/Ghidra/Processors/x86/data/languages/ia.sinc#L1594>`__, seems to have semantics
* emulators: https://github.com/colejohnson66/rbx86, https://bochs.sourceforge.io/, QEMU
* https://github.com/asmjit/asmjit, https://github.com/bitdefender/bddisasm, https://github.com/dyninst/dyninst, https://github.com/herumi/xbyak, qemu/capstone, https://github.com/diegocarba99/bagheera, https://github.com/mongodb-labs/disasm, zydis, https://github.com/MahdiSafsafi/AMED, https://github.com/nidud/asmc
* Go assembler https://cs.opensource.google/go/go/+/master:src/cmd/internal/obj/x86/avx_optabs.go;l=1791?q=vfixupimmss&ss=go
* https://github.com/Barebit/x86reference/blob/master/x86reference.xml

Overall I think extracting Xed's `iform list <https://intelxed.github.io/ref-manual/xed-iform-enum_8h.html>`__ and using Xed for encoding is the way to go. It doesn't match the docs 1-1 but it saves on sanity - e.g. the separate memory / register templates abstract over the complications of MODRM.

We also need instruction semantics. Some academics have created a `formal X86-64 semantics <https://github.com/kframework/X86-64-semantics>`__ containing most of the userspace Haswell instructions. It was mostly manually written and has been checked with fuzzing. It is written in the K Framework syntax. It is missing concurrency, crypto (AES), supervisor/privileged, x87 floating-point, MMX, and also has a bug where it rounds too much with fused multiply-add floating point precision. I don't know how to manipulate K language stuff but the actual instruction semantics is pretty simple so maybe something could be hacked together.

For affected state, there are some choices. In the Intel docs, there is a little ``(r,w)`` or ``(r)`` after the operands, that EXEgesis picks up, but this doesn't include everything. For example (per the Intel documentation) VFIXUPIMMSS reads MXCSR.DAZ and conditionally updates MXCSR.IE and MXCSR.ZE, but these are not in the thing. Xed has info on read/written standard flags. But it abbreviates other flag registers - for VFIXUPIMMSS, Xed just records a MXCSR attribute. LLVM similarly just has ``USES = [MXCSR]``. NASM and gas don't seem to have flag information at all. iced does have flag info but no MXCSR. The K semantics don't have MXCSR. So I guess Xed is the best data source but we will have to use EXEgesis somehow to scrape the affected flags from the instruction description, and then manually mark them as read/write/conditional or just leave it at coarse reordering information. It might be also possible to automatically determine it by fuzzing (weighted towards special cases like 0 and 1). But it's probably really slow and the result is somewhat suspect - it can't determine that a flag/register becomes undefined, and it may miss reads/writes that happen in rare circumstances.

For ARM, we have official XML `Machine Readable Architecture instruction tables <https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools>`__. It includes both the encoding and the semantics, and the code has been validated against ARM's conformance suite. There is a toy disassembler `hs-arm <https://github.com/nspin/hs-arm>`__ using the tables. hs-arm `seems <https://github.com/nspin/hs-arm/blob/8f10870a4afbbba010e78bd98e452ba67adc34e0/nix-results/harm.harm-tables-src/gen/Harm/Tables/Gen/Insn.hs>`__ to pull out the template information just fine, although its operand names are a little weird. `asl-interpreter <https://github.com/alastairreid/asl-interpreter>`__ runs the descriptions, which are written in a special language - it should be possible to run this to get affected state.

Timings:

* https://www.agner.org/optimize/instruction_tables.ods
* https://uops.info/
* I think EXEgesis or llvm-exegesis generates timings
* There are some papers on using ML and measured timings to predict block performance, probably more accurate than instruction-level data

Foreign code
============

One specific set of assembly sequences we care about is calling code in other languages, particularly C. Many functions use the C calling convention, such as C memory allocation and Windows system calls. The most straightforward way to call these is to look up the symbol's in the object file, set up the stack and registers appropriately for the calling convention, and jump to it. It is a bit annoying for example as we must allocate space for the C stack; Go uses 4MB or so. Also the calling conventions are messy - we will have to extract them from libffi or `LLVM <https://github.com/llvm/llvm-project/blob/6243d7d28b923c9e4f881f2a7ac77c1d13486ab2/llvm/lib/Target/X86/X86CallingConv.td#L1136>`__.

Another solution is to create a stub C file with a method declared with a fixed, known calling convention containing the desired FFI code, This C file can then be compiled to assembly via clang or GCC. Then this assembly can then be processed by identifying the method in the output and converting the contents of that method and its dependencies to Stroscot's IR, essentially decompiling it into Stroscot. The IR can then be optimized to remove the overhead of the intermediary known calling convention, directly assigning registers and returning values. So for example if we wanted to compile the ceiling function, ceil, we'd create a stub method stub_ceil, something like:

.. code-block:: C

  fastcall double stub_ceil(double in) {
    return ceil(in);
  }

So then we would compile that to assembly, decompile it to Stroscot, and patch it into the IR. Compared to calling the symbol, this approach is much more flexible - it handles:

* all calling conventions, and pretty much all languages (as they have FFI's to be called from C)
* functions defined as macros
* inlined functions
* C++ template expansion
* writing arbitrary code, instead of just calling pre-defined functions - and it's all optimized with the language's native compiler *and* Stroscot's decompiler+assembler

So the stub function approach will handle pretty much everything - it is robust. The calling convention details are baked into the compiled stub, so with no optimization, we can simply include the stub as a blob and call the compiled stub using the known calling convention. And it is not too hard to analyze the assembly, remove intermediate register assignments, and ensure that calling a symbol with the C calling convention optimizes to a direct call just like the simple approach. The costs? We have to decompile assembly to IR, and also we still have to implement at least one C calling convention, albeit we can choose the simplest/easiest one.

A third approach is to use the stub method but compile to LLVM IR instead of assembly. LLVM has a more structured representation, representing calling conventions and so on explicitly, so we have to implement all the calling conventions again like with the symbol approach. We also have to translate LLVM IR to Stroscot IR, a bit easier than decompiling aassembly as there are only 67 instructions. It may also be easier to optimize, as LLVM's SSA form means we do not have to recover clobbered register information. It is also possible to use the Clang API to directly process C/C++ to LLVM in-memory, which should be faster than generating assembly via a separate process.

Eventually, it would be good to support all of the methods. For the initial implementation, the LLVM stub seems the priority, as compiling C/C++ robustly and efficiently is the main goal. For simple cases, the compiled stub will likely consist of a call instruction and nothing else, so we can work on supporting that instruction and gradually add more support. So the initial work consists of implementing the C default calling convention and calling Clang/LLVM, giving functionality as good as the direct-symbol approach with about as much work. Then we can explore assembly stubs and direct symbol calls later .

Now for importing a whole file, like a C header file, it's a bit more involved, and we do actually have to work at the source level. The stub method let us evaluate any snippet of code, but we have to determine the symbols, types, and so on to use in those snippets. So we have to read the header file, process each declaration, and generate a stub for each. Or multiple, in the case of a C++ template function. Again, some things that may look like functions to the C programmer may actually be declared using macros or other techniques, but in this context there is not enough information to determine the desired Stroscot <-> foreign language mapping. So this process can be semi automated at most; it will be able to bind the functions in the common cases but programmers will have to write the stub code and signature manually in complicated cases.

And then of course there's linking - you have to specify the object file that you're going to link with and so that's sort of are so linking and stress cut straws cut all the jit execution model so of course you can just specify the object that run time as a file path and then stress cut will load that file and link with it um and so that complicates things because it means that you need the file to be available when you're testing the program and when So yeah the whole model is dependent are having a running system And so for example if you want to test an embedded system you have to hook up the embedded system to the network with the first computer And so that the fast computer can do all the heavy lifting white compilation and optimization and stuff um So that's a bit of complexity

Operations
==========

To abstract the ISA we consider the instructions from a functional perspective - these functions are called "operations". Operations are exposed in Stroscot as intrinsic functions. This allows using a familiar syntax.

Operations don't reference registers, the operations all take/return temporaries. Since all registers/flags/etc. can be stored/loaded to memory, temporaries are conceptually an immutable bitstring of a fixed bitwidth. These bitwidths vary by the instruction: x86 uses 1, 8, 16, 32, 64, 80, 128, 256, 512, etc. (for flags, segment registers, general-purpose registers, FPU registers, MMX/SSE/AVX).

For example the operations corresponding to x86-64 DIV, ADD, and ADC with 64-bit operands look like:

::

  divide (src : B64) (high : B64) (low : B64) =
    divisor = src
    dividend = high ++ low
    if divisor == 0
      fault DE
    else
      quotient = src2 / src1
      if quotient >= 2^64
        fault DE
      else
        { quotient = quotient, remainder = src2 mod src1 }

  add (src1 : B64) (src2 : B64) =
    dest = src1 + src2
    ... compute flags ...
    { dest, OF, SF, ZF, AF, CF, PF }

  adc (src1 : B64) (src2 : B64) (cf : B1) =
    dest = src1 + src2 + cf
    ... compute flags ...
    { dest, OF, SF, ZF, AF, CF, PF }

Accessing memory is handled by a separate operation - but in the ISA x86 has combined read-add instructions:

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
