Code generation
###############

The back-end is complicated. There are several steps to be taken here.

#. Instruction selection
#. Register allocation
#. Peephole optimization
#. Instruction scheduling

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

Register allocation
===================

Definitions:

temporary
  A location storing a bitstring, mapped to a logical register or memory location.
virtual register
  A temporary that has write access. Not used because the codegen uses SSA.
logical register
  A register as used in an instruction
physical register
  A CPU register in the hardware register file. Logical registers are dynamically mapped to these, making many register assignment decisions immaterial.
program point
  location between consecutive instructions
live
  An output t is live at a program point if t holds a value that might be used later by another instruction j. The instruction j is said to be dependent on i.
interference point
  A program point where each of a set of output temporaries could be used later.
spilling
  Spilling is assigning a temporary into memory. It requires the generation of spill codem, store/load instructions to move the value to and from memory.
register class
  Most CPU’s contain registers grouped into classes. For example, there may be registers for floating point, and registers for integer operations.
register alias
  Some registers may alias to registers in another class. A good example are the x86 registers rax, eax, ax, al and ah.

Processor registers have shorter access times, but they are limited in number, forcing some temporaries to be spilled. The surefire approach spill-everywhere is to store every temporary to memory. Then every operation is wrapped with spill instructions for each input/output. But it's slow so the problem is modeled as applying optimizations to spill-everywhere.

Register assignment allows the use of equivalent instructions with different registers, e.g. add a,a vs add b,b
Live range splitting allocates temporaries to different locations in different parts of their live
ranges.
Multi-allocation allocates temporaries simultaneously to registers as well as memory, reduc-
ing the overhead of spilling in certain scenarios [31].
Load-store optimization avoids reloading spilled values by reusing values loaded in previous
parts of the spill code.
Rematerialization recomputes values at their use points rather than loading them from memory, when the
recomputation instructions are deemed less costly than the load.
Coalescing eliminates move instructions if the source and destination are the same.
Register packing assigns temporaries of small bit-widths to different parts of larger-width
registers (for processors supporting such assignments) to improve register utilization.


At each program point there is a map from variables to registers or memory. Registers are limited but fast. Send variables that do not fit to memory, spilling the least used variables and filling them back when needed (copy to/from memory). Because of register renaming / memory buffering, the actual register / address assignment doesn't matter, only the spill pattern. (TODO: check this with some benchmarks) Registers and memory have relatively similar APIs: read, and write.

Instruction selection
=====================

The IR is split into a series of instruction patterns, a forest of trees. Usually a tree rewrite system is used - bottom up rewrite generator (BURG). See pyburg.

One way is to write a lot of patterns and try all these patterns in turn. If a pattern matches a specific sequence of instructions, the pattern can be applied, and the instructions are substituted by the pattern substitute. Another way, is to define per instruction the effects of the instruction, and for each pair of instructions that are evaluated, combine the effects of these instructions. If there exist an instruction which has the same effect as the combined effect of the two original instructions, the substitution can be made. This is the combiner approach as described by [Davidson1980].

The advantage of having the combiner, is that only per instructions the effects of the instruction must be defined. After this, all instructions with effects can be potentially combined. This reduces the amount of work to define peephole optimization patterns from N * N to N. Namely, not all instruction combinations must be described, but only the effects per instruction.

* Instruction selection - replacing sequences of instructions with cheaper/shorter sequences of instructions.
* Peephole optimizations / strength reduction - like ``x*2`` by ``x << 1``/``x+x``, or setting a register to 0 using XOR instead of a mov, exploiting complex instructions such as decrement register and branch if not zero.
* Sparse conditional constant propagation - dead code / dead store elimination, constant folding/propagation
* Partial evaluation
* common subexpression elimination, global value numbering - tricky with blocks
* code factoring - CSE but for control flow
* Test reordering - do simpler tests first - treat control flow as data
* Removing conditional branch cases if can prove won't be taken
* Inlining

* Space optimizations - anti-inlining
* Trampolines allow placing code at low addresses
* Macro compression compresses common sequences of code

Instruction Scheduling
======================

Instruction scheduling assigns issue cycles to program instructions. Valid instruction schedules
must satisfy instruction dependencies and constraints imposed by limited processor resources.

Latency
  the minimum number of cycles that must elapse between the issue of the depending instructions. Variable latencies (such as those arising from cache memory accesses) are typically handled by assuming the best case and relying on the processor to stall the execution otherwise.

Resources
  resource model where each resource s has a capacity cap(s) and each instruction i consumes con(i, s) units of each resource s during dur(i, s) cycles. VLIW processors can be modeled by an additional resource with capacity equal to the processor’s issue width.

CPU model:

::

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

The instruction scheduler schedules the instructions intelligently such that they will arrive at the corresponding position in the pipeline at the exact cycle when the data will be available to them.

Data hazards: RAW is unavoidable. WAR/WAW are eliminated in modern processors by renaming as in the `Tomasulo algorithm <https://en.wikipedia.org/wiki/Tomasulo_algorithm>`__. WAW can be also ignored if the value isn't used.

timing of instructions - most are fixed. load operations depend on what's cached.

* Scheduling / reordering / pipelining
* minimize pipeline stalls, when an instruction in one stage of the pipeline depends on the result of another instruction ahead of it in the pipeline but not yet completed.
* ensure the various functional units are fully fed with instructions to execute.
* avoid cache misses by grouping accesses
* clear out unconditional jumps (inlining). Avoid inlining so much that it cannot fit in the cache.
* splitting/combining recursive calls / basic blocks
* Bias conditional jumps towards the common case

branch prediction: branch target buffer (BTB), indirect branch target array, loop detector and renamed return stack buffer. mispredicted branch clears cache and restarts.

Layout
======

For example getting rid of the jump here:

.. code-block:: asm

    jmp my_label
    my_label:

even if the jump can't be avoided, memory layout can affect program performance. see profile guided memory layout thesis

Control flow
============

The ADD instruction is not so simple

Control flow graph

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

FFI calls
=========

The semantics of a call are inherently system/ABI dependent, to the point of not being captured in a target triple. The semantics thus have to be described at the call site. But the data format doesn't really matter as the call instruction will most likely be wrapped / generated. Maybe libffi can help.

basic FFI types: ``()``, ``bool``, ``int8``, ``int16``, ``int32``, ``int64``, ``float``, ``double``, ``pointer``
Process C/C++ headers with clang, or inspect LLVM bitcode, to identify FFI types

symbols can be statically or dynamically linked

you can also just enclose foreign code in ``extern C { ... }``.
this goes through clang to identify its FFI signature

Linux syscalls
--------------

Parsing all the syscalls requires either manually writing them out / copying them from `somewhere <https://filippo.io/linux-syscall-table/>`__ or doing a lot of kernel source spelunking. Go has some stuff `here <https://pkg.go.dev/golang.org/x/sys/unix?utm_source=godoc>`__ (`script <https://cs.opensource.google/go/x/sys/+/master:unix/linux/mkall.go>`__): it generates syscall numbers and constants / `struct definitions <https://utcc.utoronto.ca/~cks/space/blog/programming/GoCGoCompatibleStructs>`__ from the headers.

The only place the syscall arguments are defined is in individual files with macros from the family `SYSCALL_DEFINEx <https://lwn.net/Articles/604287/>`__ (e.g. `io_uring_setup <https://github.com/torvalds/linux/blob/141415d7379a02f0a75b1a7611d6b50928b3c46d/fs/io_uring.c#L9737>`__). We have to run the preprocessor for true correctness; the best option seems to be hooking the macro to print out the arguments with `diagnostic pragmas <https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html#Diagnostic-Pragmas>`__. Although scraping the files directly with grep + parentheses matching seems like it would work alright.

The actual convention is documented `here <https://stackoverflow.com/questions/2535989/what-are-the-calling-conventions-for-unix-linux-system-calls-and-user-space-f/2538212#2538212>`__ and `here <https://manpages.debian.org/unstable/manpages-dev/syscall.2.en.html>`__. The syscall number is expected in rax, return values in rax and rdx. otherwise all registers, segments and eflags are saved. Arguments left to right are rdi, rsi, rdx, r10, r8, r9.

Signed range of -4096 < eax < 0 is an error code, anything else may be a normal return value. ("A.2 AMD64 Linux Kernel Conventions" of `System V Application Binary Interface AMD64 Architecture Processor Supplement <https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs>`__)

Non-executable stack
---------------------

When the Linux kernel starts a program, it looks for a PT_GNU_STACK segment. If it does not find one, it sets the stack to be executable (if appropriate for the architecture). If it does find a PT_GNU_STACK segment, it marks the stack as executable if the segment flags call for it. (It’s possible to override this and force the kernel to never use an executable stack.) Similarly, the dynamic linker looks for a PT_GNU_STACK in any executable or shared library that it loads, and changes the stack to be executable if any of them require it. When this all works smoothly, most programs wind up with a non-executable stack. The most common reason this fails is that part of the program is written in assembler, and the assembler code does not create a .note.GNU_stack section. If you write assembler code for GNU/Linux, you must always be careful to add the appropriate line to your file. For most targets, the line you want is ``.section .note.GNU-stack,"",@progbits`` There are some linker options to control this. The -z execstack option tells the linker to mark the program as requiring an executable stack, regardless of the input files. The -z noexecstack option marks it as not requiring an executable stack. The gold linker has a --warn-execstack option which will cause the linker to warn about any object which is missing a .note.GNU-stack option or which has an executable .note.GNU-stack option. The execstack program may also be used to query whether a program requires an executable stack, and to change its setting.

ASLR
----

Modern ELF systems can randomize the address at which shared libraries are loaded. This is generally referred to as Address Space Layout Randomization, or ASLR. Shared libraries are always position independent, which means that they can be loaded at any address. Randomizing the load address makes it slightly harder for attackers of a running program to exploit buffer overflows or similar problems, because they have no fixed addresses that they can rely on. ASLR is part of defense in depth: it does not by itself prevent any attacks, but it makes it slightly more difficult for attackers to exploit certain kinds of programming errors in a useful way beyond simply crashing the program.

Although it is straightforward to randomize the load address of a shared library, an ELF executable is normally linked to run at a fixed address that can not be changed. This means that attackers have a set of fixed addresses they can rely on. Permitting the kernel to randomize the address of the executable itself is done by generating a Position Independent Executable, or PIE.

It turns out to be quite simple to create a PIE: a PIE is simply an executable shared library. To make a shared library executable you just need to give it a PT_INTERP segment and appropriate startup code. The startup code can be the same as the usual executable startup code, though of course it must be compiled to be position independent.

When compiling code to go into a shared library, you use the -fpic option. When compiling code to go into a PIE, you use the -fpie option. Since a PIE is just a shared library, these options are almost exactly the same. The only difference is that since -fpie implies that you are building the main executable, there is no need to support symbol interposition for defined symbols. In a shared library, if function f1 calls f2, and f2 is globally visible, the code has to consider the possibility that f2 will be interposed. Thus, the call must go through the PLT. In a PIE, f2 can not be interposed, so the call may be made directly, though of course still in a position independent manner. Similarly, if the processor can do PC-relative loads and stores, all global variables can be accessed directly rather than going through the GOT.

Other than that ability to avoid the PLT and GOT in some cases, a PIE is really just a shared library. The dynamic linker will ask the kernel to map it at a random address and will then relocate it as usual.

This does imply that a PIE must be dynamically linked, in the sense of using the dynamic linker. Since the dynamic linker and the C library are closely intertwined, linking the PIE statically with the C library is unlikely to work in general. It is possible to design a statically linked PIE, in which the program relocates itself at startup time. The dynamic linker itself does this. However, there is no general mechanism for this at present.


ABI
---

Swift 5 has a stable ABI, which has been `praised <https://gankra.github.io/blah/swift-abi/>`__. This allows dynamic linking to system-wide libraries. Dynamic linking means that the ABI (method signatures) is provided at compile time but the actual methods are only available at runtime via the system dynamic linker.

An ABI consists of the names of some symbols together with their calling convention, which specifies the layout of types and return values. It is a property of the platform and toolchain. Linux C uses the Itanium ABI, Windows has MSVC (supported by LLVM) and also gcc can use Itanium. There are split conventions for 64-bit vs 32-bit.

C++ templated and Rust generic functions ``template <typename T> bool process(T value)`` generate symbols for each type (monomorphization) but have no direct ABI.

ABI should follow API, nothing can save API-breaking changes. Annotations optimize the ABI, at the cost of adding more ways to break compatibility. Swift made adding some annotations backwards-compatible. Example annotations are frozen (non-resilient) layout, exhaustively matchable, inlineable, non-subclassable, non-escaping.

Example: we change ``{ path : ptr char } -> Maybe {size : int64_t}`` to ``{ path : ptr char } -> Maybe {last_modified_time : int64_t, size : int64_t}``. In Swift this only breaks ABI if the ``frozen`` annotation is present. By default types are resilient, meaning they are passed by reference and the size, alignment, stride, and extra inhabitants of types are looked up from the type's witness table at runtime. But this is only outside the ABI boundary, inside the dynamic library it can assume the representation. And pointers have uniform layout hence don't need the witness table. Swift compiles polymorphic APIs to a generic ABI, rather than monomorphizing. Also fields of resilient types are only exposed as getters and setters, so can be computed instead of being stored fields.

Reabstraction thunks wrap closures with the wrong ABI.

ownership is part of the calling convention:

- function stores value and will release it
- functions borrows value and does not keep it

exceptions use a special calling convention with the error type boxed in a register. The caller initializes the “swift error” register to 0, and if there’s an exception the callee sets that register to hold the boxed error’s pointer. This makes error propagation really fast.

binary compatibility - changes will not break memory-safety or type-safety. Observable behavior may change, and preconditions, postconditions, and invariants may break. If a value is inlined, the old value will be used in existing compiled objects. Removing functionality has the expectation that the functionality is unused - if a client attempts to use the removed functionality it will get an error.

"fragile" or "frozen" describes C structs, which have very strict binary compatibility rules. Swift has "resilient" structs which store a witness table with metadata on their interpretation.

The following changes are binary compatible:

- Changing the body/value/initial value of a function, constant, or variable
- Adding, changing, or removing a default argument
- Changing a variable to a constant or vice versa
- Adding, reordering, or removing members of resilient structs.
- Adding, reordering, or removing cases of a resilient enum.
- Changing parsing rules

Interpreter
===========

LuaJIT's interpreter is fast, because:

* It uses indirect threading (aka labeled goto in C).
* It has a very small I-cache footprint (the core of the interpreter fits in 6K).
* The parser generates a register-based bytecode.
* The bytecode is really a word-code (32 bit/ins) and designed for fast decoding.
* Bytecode decode and dispatch is heavily optimized for superscalar CPUs.
* The bytecode is type-specialized and patched on-the-fly.
* The dispatch table is patched to allow for debug hooks and trace recording. No need to check for these cases in the fast paths.
* It uses NaN tagging for object references. This allows unboxed FP numbers with a minimal cache footprint for stacks/arrays. FP stores are auto-tagging.
* It inlines all fast paths.
* It uses special calling conventions for built-ins (fast functions).
* Tons more tuning in the VM ... and the JIT compiler has it's own bag of tricks.

The control-flow graph of an interpreter with C switch-based
dispatch looks like this:

::

  repeat {
    load instruction
    dispatch instruction
    switch(instruction_type) {
      case X:
        decode operations
        if good
          fast instruction execution
        else
          slow execution
    }
  }

There are dozens of instructions and hundreds of slow paths. The compiler doesn't know which paths are fast. Even if it did, it's still a single giant loop body. The standard register allocation heuristics fail at this scale, so the compiler has trouble keeping important variables in registers. There's just no way to give it a goal function like "I want the same register assignment before each goto". Diamond-shaped control-flow is known to be the worst-case scenario for most optimizations and for register alloction. Nested diamond-shaped control-flow is even worse. Tail-merging and CSE will happily join all these common tails of each instruction and generate a single dispatch point. Ick. You can try to disable some optimizations for this piece of code, but this will negatively impact all paths. Almost nothing can be hoisted or eliminated, because there will be a slow path where an aliasing store kills all opportunities.. The slow paths kill the opportunities for the fast paths and the complex instructions kill the opportunities for the simpler instructions.

We can use direct or indirect threading with computed goto. clang/LLVM optimizes the looped switch to indirect threading at ``-O``. (`ref <https://internals.rust-lang.org/t/computed-gotos-tco-threaded-interpreters-experiments-and-findings/4668/6>`__)

::

  static void* dispatch_table[] = { &&OP1, &&OP2, ... };

  // indirect
  #define DISPATCH(ip) goto *dispatch_table[memory[ip] >> 12]
  // direct
  #define DISPATCH(ip) jump *ip++

  DISPATCH();

  OP:
      decode operands
      execute instruction
      ip = reg[R_PC]++ // load next instruction
      DISPATCH(ip); // dispatch next instruction
  ...


This effectively replicates the load and the dispatch, which helps
the CPU branch predictors.

If you write an interpreter loop in assembler, you can do better:
* Keep a fixed register assignment for all instructions.
* Only a single fast path in every bytecode instruction
* Keep all important state in registers for the fast paths. Spill/reload only in the slow paths. (No C compiler manages to do that on x86.)
* The fast paths are always the straight line fall-through paths.
* Move the slow paths elsewhere, to help with I-Cache density.
* Pre-load instructions and pre-decode operands.
* Remove stalls. Interleave operations based on the data dependencies.

ELF
===

The kernel/loader only uses segments when loading executables into memory. So we don't need to bother with sections. Ignoring one-offs and notes (comments), there is only one segment `type <http://www.sco.com/developers/gabi/latest/ch5.pheader.html#p_type>`__, , a loadable segment PT_LOAD. The attributes are ``flags, offset, vaddr, filesz, memsz, align``. ``filesz <= memsz``. ``filesz`` bytes starting from offset from the file are mapped to memory starting at ``vaddr``. If ``memsz > filesz``, the extra bytes are defined to hold the value 0 and to follow the segment's initialized area. ``vaddr - offset mod align == 0``; ``align`` is usually the page size. ``flags`` defines the permissions that mmap uses and can be any combination of read/write/execute.

So ignoring the file format / alignment / special handling of ending with 0's, a loadable segments is ``Load {flags, vaddr, contents : [Byte]}``.

`PE <https://docs.microsoft.com/en-us/windows/win32/debug/pe-format>`__ is similar, the handling of alignment is different. Mach-O doesn't even have a man page currently available from Apple so who cares.

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
========

Goals:

* represent non-local control flow (faults)
* optimizations are localized (read small portion, write small portion)
* all known optimizations can be implemented
* fixes evaluation order only for stateful operations

https://cs.stackexchange.com/questions/74794/why-is-static-single-assignment-preferred-over-continuation-passing-style-in-man


LLVM IR sucks
-------------

LLVM IR contains:

 * Explicitly Target-specific features, like x86_fp80
 * Target-specific ABI code, to interoperate with native C ABIs
 * Implicitly Target-specific features, like Linkages.
 * Target-specific limitations: alignment on alloca, supported integer types
 * Undefined Behavior, with C "nasal demons" semantics and optimization passes that assume behavior
 * Intentional vagueness, and edge cases that no one is interested in fixing.
 * Inconsistent IEEE-754 arithmetic

Interpreting LLVM is slow. High level abstractions are chopped up into lots of small low-level instructions. The interpreter has to execute a relatively large number of instructions to do virtual method calls. Languages built for interpretation use fewer more expensive instructions, and have lower per-instruction  overhead.

JITing LLVM is faster than static C compilers, but it's not fast compared to real JIT compilers. It requires recognizing patterns in groups of instructions, and then emitting code for the patterns. This works, but it's more involved than a simple template generator.

LLVM IR isn't capable of representing necessary semantic information for high level languages such as Objective-C without embedding the information into it using hacky mechanisms. Existing transformations reverse-engineer Objective C out of the lowered code, which isn't guaranteed to be safe by LLVM IR rules alone and only works if the Objective C frontend generated the IR.
