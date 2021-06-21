Assembly
########

Architectures
=============

The first step in dealing with assembly is to decide which instruction set architectures to support. I couldn't find a list of processor architectures by popularity, but from googling numbers of units sold it seems clear that the two primary architectures to support nowadays are x86-64 AMD64 (desktops) and ARMv8-A AArch64 (mobile devices).

Others:
* ARMv9-A: It's released, I don't think there are any devices using it yet. But it'll have to be supported soon.
* RISC-V: There are $100-ish dev boards listed at https://riscv.org/exchange/boards/. No consumer systems yet.
* POWER: They're expensive, but `Raptor <https://secure.raptorcs.com/content/base/products.html>`__ sells systems.
* MIPS: the company that develops it went bankrupt and is now doing RISC-V. There are consumer systems available in China (Loongson), but the rumor is that they too are moving to RISC-V or else to their own architecture LoongArch.
* z/Architecture: really expensive
* SPARC: It's end-of-life but I guess you can still buy servers second-hand.
* 32-bit x86: Old desktop PCs. From a time/effort perspective it seems cheaper to buy a new computer instead of writing support for these.
* 32-bit ARM: Old phones, the Raspberry Pi Zero.

For now these other architectures will be left to contributors because writing the support is a lot of work and from a design perspective supporting 2 architectures is not much different from supporting 10, it's just a larger set of cases.

ARM support will be tested through QEMU, x86 natively. There are also CI services that could work (Drone).

In addition to the basic ISAs, there are also extensions and `microarchitectures <https://en.wikipedia.org/wiki/Microarchitecture>`__ to consider. `PassMark <https://www.cpubenchmark.net/share30.html>`__ has a list of CPU shares, it's probably wildly skewed to gaming but it's better than nothing. Optimization runs an ensemble model where it has a large set of CPUs allocated according to the popularity measurements, all running the program, and it chooses what will minimize the total run time summed across all CPUs.

Instruction database
====================

We need the instruction data in a machine-readable form.

ARM has made available XML `instruction tables <https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools>`__. There is a toy disassembler `hs-arm <https://github.com/nspin/hs-arm>`__ using the tables. EXEgesis also parses the XML.

The only official x86 sources are the `Intel <https://software.intel.com/content/www/us/en/develop/articles/intel-sdm.html>`__ / `AMD <https://developer.amd.com/resources/developer-guides-manuals/>`__ PDFs. Parsing them is a lot of work but `EXEgesis <https://github.com/google/EXEgesis>`__ has made inroads. It uses a hacky Xpdf parse though so reimplementing a new parser based on Python's `Parsr <https://github.com/axa-group/Parsr>`__ might be better. Although, EXEgesis also has micro-op and timing information which seems to be generated out of thin air.

Non-official x86 sources:
* `Intel XED <https://intelxed.github.io/>`__ for x86, which has info on read/written flags (`file <https://github.com/intelxed/xed/blob/main/datafiles/xed-isa.txt>`__)
* NASM tables for x86, `instructions <https://github.com/netwide-assembler/nasm/blob/master/x86/insns.dat>`__ `registers <https://github.com/netwide-assembler/nasm/blob/master/x86/regs.dat>`__, good mnemonics but not much else
* K Framework `formal semantics <https://github.com/kframework/X86-64-semantics>`__
* LLVM's tables, e.g. `x86 <https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86.td>`__ (Keystone seems to use identical tables)
* `OSACA <https://github.com/RRZE-HPC/OSACA/tree/master/osaca/data/isa>`__ (AGPL licensed)
* MazeGen's X86 `XML reference <http://ref.x86asm.net/x86reference.xml>`__ (`Github <https://github.com/Barebit/x86reference>`__)
* `GNU Assembler <https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=opcodes/i386-opc.tbl;h=b0530e5fb82f4f4cd85d67f7ebf6ce6ebf9b45b5;hb=HEAD>`__
* `iced assembler/disassembler <https://github.com/icedland/iced>`__ (mostly in C#)

However it is generated, the instruction database should describe:
* types (sizes) of operands
* storage possibilities of operands: memory, register, constant, flag

  * constraints / storage requirements (pinned registers)

* which storage locations are read/written/clobbered
* other side effects:
  * hidden registers
  * Load-link/store-conditional:

    * LL sets monitors (state goes open -> exclusive)
    * Memory accesses from other processors, second LL from same processor, context switch, exceptions, spurious events all clear monitors (exclusive -> open)
    * SC checks monitors and sets memory if all exclusive
    * Software must avoid having any explicit memory accesses, system control register updates, or cache maintenance instructions between paired LDXR and STXR instructions.



  * So need a concurrency / memory model to define semantics
* how fast can you add new instructions and make an official release?
* instruction reordering / scheduling possible?


Functional instructions
=======================

The first step in representing an ISA is to consider it from a functional perspective. We use continuation-passing style, so enterable addresses are functions consuming the physical processor state that never return. For example the x86-64 "DIV—Unsigned Divide" instruction looks like:

::

  divide (Operandsize,SRC) RDX RAX faults={DE,GP,SS,PF,AC} next = ...

The faults can be simplified as for most of them operation stays in the kernel. Really on Linux the fault arguments are based on the signal handlers, SIGFPE->FPE_INTDIV, SIGSEGV, etc.

Flags are also arguments, or get passed to the ``next`` continuation. For example the 64-bit ADD instruction takes two 64-bit data and produces a 64-bit datum plus 6 1-bit datums for the flags OF, SF, ZF, AF, CF, and PF. Similarly ADC (add with carry) produces the same output and takes the same arguments with the addition of a 1-bit datum (the carry flag).

Register allocation
===================

After that we implement register allocation. What this means from a user perspective is that each instruction, instead of reading/writing various physical registers/flags, is a pure function taking and producing data in various bitwidths. These bitwidths vary by the instruction: x86 uses 1, 8, 16, 32, 64, 80, 128, 256, 512, etc. (for flags, segment registers, general-purpose registers, FPU registers, MMX/SSE/AVX).

There's some code in Python `here <https://ppci.readthedocs.io/en/latest/reference/codegen/index.html>`__

Control flow
============

The ADD instruction is not so simple

Blocks
======

A basic block (BB) is a maximal sequence of instructions that can only be entered from the top, and that contains no terminator instructions except for the last one. The last instruction in a BB must be a terminator instruction, so execution cannot fall through the end of the BB but instead jumps to an address.

The basic terminator instructions are unconditional branches. Conditional branches are turned into terminator instructions by adding the fall-through address as an argument.

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

Although phi nodes were an interesting idea all the cool kids are now using block arguments. Blocks arguments fit better into various analysis passes.

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

There are actually two linkers: the static linker, which creates a shared object or executable, and the dynamic linker or loader, which finalizes addresses and performs relocations. Ignoring relocations, the role of a static linker is essentially ``cat``, while the loader is more like ``unzip``. Although the loader also has to search through the filesystem for all the shared objects. The static linker uses sections while the dynamic linker uses segments; there are only a few types of segments but lots of section types.

The static linker can be replaced after a lot of work by a language-specific linker and object format. For example the Go project uses its own linker and object format (bastardized ELF). This allows more freedom in defining symbols and for additional metadata to be stored in the objects. In fact we don't really need object files at all and can store the information in a database. The database would allow using the same incremental build system that the compiler uses. But for a first pass it might be a bit much; GHC uses the system assembler and linker.

The loader is harder to replace; although it can be changed to a non-standard path, distributing it would be difficult, and using a different shared object format would likely go the way of `FatELF <https://icculus.org/finger/icculus?date=2009-11-03&time=19-08-04>`__ (i.e. nowhere). Plus there are features like ASLR and lazy loading that would have to be reimplemented. So Stroscot should definitely produce outputs that the loader can understand.

A linker needs to:

    Find all symbol definitions that live in each object file and library.
    Assign each symbol a final, absolute, address.
    Find all symbol references in each object file and library.
    Replace all symbol references with the absolute address of that symbol.
    Write completed executable.



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

Debugging information is essentially a complete fiction. After optimization and transformation the output machine code bears no resemblance to the original program. But creating the DWARF tables as accurately as possible does improve the debugging experience. Although, DWARF is oriented around traditional compilation units and thus is not quite flexible enough for our purposes. Similarly, providing stack traces helps.

DWARF information should be generated as early as possible, because less information will have been lost and also because it avoids recompilation.



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
