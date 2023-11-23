Operational primitives
######################

As used in Stroscot, operational primitives refer to the stateful operations that form the building blocks of imperative programs. Examples include memory operations (read, write), foreign function calls, compiler intrinsics, and OS system calls. It would also be possible to call them "primitive operations", but this term is less precise and could be read as including elements of the runtime such as lambda reduction and term rewriting.

There is an operational interpretation of every expression in Stroscot. For example, the operational interpretation of a value is returning that value. The operational interpretation of addition on two machine integers consists of storing the integers to memory or fixed registers, executing the ``add`` assembly instruction, appropriately handling any error conditions or traps, packaging up the result as a value, and returning it. And so on. Generally, a program may be viewed as assembly instruction sequences interleaved together with higher-level "glue" code. During optimization, one goal is to convert and reduce as much of this "glue" code as possible into assembly. Each switch into "glue" code corresponds to a jump back into the interpreter, with associated overhead.

Assembly
========

Operational primitives are naturally represented as assembly instruction sequences or templates. After all, the CPU cannot execute anything else. So with a built-in assembler, we have a complete set of operational primitives. By the nature of the definition, these are hardware and platform specific.

Ideally we would model primitives as deterministic functions from input machine state to output machine state. We can use a `CRIU image <https://criu.org/Images>`__ to model the machine state (at least on Linux - Windows is left as an exercise). This allows more control than a traditional ELF image + exit code, as it captures the complete state of a process in the middle of its execution, like a debugger would. But the behavior of almost all assembly instructions can be affected by unpredictable external factors. Therefore we model primitives as functions that take an input CRIU state and an additional "recording" of external factors and produces an output CRIU state. Examples of recorded factors:

* signal interrupts (any interruptible instruction)
* memory order (for any instruction reading/writing shared memory)
* system call results (for syscalls)
* spurious failures (for load-linked/store-conditional)
* CPU cycle count (rdtsc)
* random numbers (rdrand)
* current CPU / core id (cpuid)
* FPU state (floating-point)
* chip ID (if the instruction outputs undefined or "reserved for future definition" registers)

Generally speaking, all assembly instructions are deterministic after recording and controlling these factors - otherwise, programs would not execute reliably. For most instructions there is a "default" recording that can be assumed (no signals, no memory interactions, etc.), so it would be possible to formulate these instructions without a recording, but as rdrand etc. are also instructions it is easier to formalize all instructions and instruction sequences as taking a recording.

Recordings are not just theoretical; there are programs that implement record/replay. They use various techniques, such as ptrace/breakpoints (rr - single threaded), intercepting DLL calls (Replay.io - mainly for JS), dynamic instrumentation (PinPlay, Undo.io - multithreaded), and machine virtualization (research-level; a bit problematic as it requires emulating a whole system).

When we are optimizing, we often want to replace one instruction sequence with another. For example, we may want to redo the register allocation, or replace an instruction sequence with a faster one. So we need a semantics for these instruction sequences that allows us to determine if two instruction sequences are equivalent, and then we can define operational primitives as equivalence classes of instruction sequences. In general, an instruction sequence may have arbitrary effects, and may be a complete program. So it is easier to think about comparing programs, and then we can define instruction sequences as equivalent if they have the same behavior when embedded in appropriate programs. Conceptually, comparing programs is simple: run the programs and see if they do the same thing. But programs on modern systems have a lot of parts.

Abstracted assembly
===================

The nature of assembly is that it is a bit messy; we have to deal with register allocation and recordings and so forth. It is more convenient if we assume a fixed calling convention, say that all data for the operation (including the recording or decision to record de novo) is stored and returned in memory. Since all registers/flags/etc. can be stored/loaded to memory, and record/replay can be implemented on an instruction level, this does not lose any expressiveness - it merely adds significant overhead to executing the instruction. But in return it means operations work on immutable bitstrings rather than machine states. Generally these bitstrings are of a fixed, known width, such as 1, 8, 16, 32, 64, 80, 128, 256, 512, etc. (for flags, segment registers, general-purpose registers, FPU registers, MMX/SSE/AVX).



Operations are exposed in Stroscot as intrinsic functions. This allows using Stroscot's typical syntax. For example the operations corresponding to x86-64 DIV, ADD, and ADC with 64-bit operands look like:

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

Runtime and OS calls
====================

The concept of a runtime depends on whether the program is compiled or interpreted. A compiler outputs native machine code that requires a specific library called a "runtime". The runtime is a library that's part of every program that can be either statically or dynamically linked. Meanwhile, an interpreter is an executable that includes a runtime. For example Java compiles to bytecode with javac, but the "runtime" or interpreter is the separate program "java" (JRE). The JRE implements concurrency and memory management.

For Stroscot the plan is for the compiled runtime to be minimal since many things can be implemented by linking in part of the standard library and it's always nice to have small executable sizes. For example Zig claims to have `"no runtime" <https://ziglang.org/documentation/master/#Memory>`__. Really this just means no default memory allocator - in practice, compared to assembly, Zig `still has <https://drewdevault.com/2020/01/04/Slow.html>`__ 2-3 KiB overhead (30%) for printing error messages.

libc is the C runtime for compiled programs. Go and Zig have a link_libc flag/no-libc mode that allows choosing to not link it, but in practice a lot of programs end up depending on libc anyway. Specifically, libc wraps all the syscalls, so on various systems (`OpenBSD <https://utcc.utoronto.ca/~cks/space/blog/programming/Go116OpenBSDUsesLibc>`__, Illumos, Solaris) avoiding libc isn't possible because system calls must be made through the system libc. And Windows/Mac require libc indirectly because you have to link to DLLs that depend on libc. Only on Linux is it possible to avoid libc by using direct syscalls. This can cause `evil bugs <https://marcan.st/2017/12/debugging-an-evil-go-runtime-bug>`__. If the implementation follows Go's or Zig's closely this probably isn't an issue because they've worked out all the bugs. And it should be faster / less register pressure to do syscalls in assembly than to set up a C stack and call into libc.

Even on Linux, many programs still need libc for compatibility. They interface with C by calling C libraries. Facilities such as malloc and errno can be avoided / reimplemented but in general the only way to get a working program is to use the C runtime. In particular Go's net package depends on system C APIs everywhere except Linux, where they went to some effort to implement a no-libc version.

So overall it seems that self-contained executables on Linux are the only libc-free possibility. But these kinds of programs are what people use for comparisons on system programming, so it still seems to be worth implementing. There's that "cool factor" of one less dependency.

The syscalls themselves take / modify C structs. So regardless of whether we link with libc, we still need a C parser / ABI to get anywhere.

FFI calls
=========

Stroscot should work well with existing code written in other languages, either through natively importing and using that code or through easy-to-use bridges or interfaces.

The semantics of a call are inherently system/ABI dependent, to the point of not being captured in a target triple. The semantics thus have to be described at the call site. But the data format doesn't really matter as the call instruction will most likely be wrapped / generated. Maybe libffi can help.

basic FFI types: ``()``, ``bool``, ``int8``, ``int16``, ``int32``, ``int64``, ``float``, ``double``, ``pointer``
Process C/C++ headers with clang, or inspect LLVM bitcode, to identify FFI types

symbols can be statically or dynamically linked

you can also just enclose foreign code in ``extern C { ... }``.
this goes through clang to identify its FFI signature

Use C/C++ in the same address space - requires bindings with LLVM or SWIG, or compiling to the LLVM / GCC backend. Linking C requires an understanding of the calling conventions for all languages concerned, as well as concern for stack limits when calling C or C++.

Linux syscalls
--------------

Parsing all the syscalls requires either manually writing them out / copying them from `somewhere <https://filippo.io/linux-syscall-table/>`__ or doing a lot of kernel source spelunking. Go has some stuff `here <https://pkg.go.dev/golang.org/x/sys/unix?utm_source=godoc>`__ (`script <https://cs.opensource.google/go/x/sys/+/master:unix/linux/mkall.go>`__): it generates syscall numbers and constants / `struct definitions <https://utcc.utoronto.ca/~cks/space/blog/programming/GoCGoCompatibleStructs>`__ from the headers.

The only place the syscall arguments are defined is in individual files with macros from the family `SYSCALL_DEFINEx <https://lwn.net/Articles/604287/>`__ (e.g. `io_uring_setup <https://github.com/torvalds/linux/blob/141415d7379a02f0a75b1a7611d6b50928b3c46d/fs/io_uring.c#L9737>`__). We have to run the preprocessor for true correctness; the best option seems to be hooking the macro to print out the arguments with `diagnostic pragmas <https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Pragmas.html#Diagnostic-Pragmas>`__. Although scraping the files directly with grep + parentheses matching seems like it would work alright.

The actual convention is documented `here <https://stackoverflow.com/questions/2535989/what-are-the-calling-conventions-for-unix-linux-system-calls-and-user-space-f/2538212#2538212>`__ and `here <https://manpages.debian.org/unstable/manpages-dev/syscall.2.en.html>`__. The syscall number is expected in rax, return values in rax and rdx. otherwise all registers, segments and eflags are saved. Arguments left to right are rdi, rsi, rdx, r10, r8, r9.

Signed range of -4096 < eax < 0 is an error code, anything else may be a normal return value. ("A.2 AMD64 Linux Kernel Conventions" of `System V Application Binary Interface AMD64 Architecture Processor Supplement <https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs>`__)

ABI
---

Swift 5 has a stable ABI, which has been `praised <https://gankra.github.io/blah/swift-abi/>`__. This allows dynamic linking to system-wide libraries. Dynamic linking means that the ABI (method signatures) is provided at compile time but the actual methods are only available at runtime via the system dynamic linker.

An ABI consists of the names of some symbols together with their calling convention, which specifies the layout of types and return values. It is a property of the platform and toolchain. Linux C uses the Itanium ABI, Windows has MSVC (supported by LLVM) and also gcc can use Itanium. There are split conventions for 64-bit vs 32-bit.

C++ templated and Rust generic functions ``template <typename T> bool process(T value)`` generate symbols for each type (monomorphization) but have no direct ABI.

ABI should follow API, nothing can save API-breaking changes. Annotations optimize the ABI, at the cost of adding more ways to break compatibility. Swift made adding some annotations backwards-compatible. Example annotations are frozen (non-resilient) layout, exhaustively matchable, inlineable, non-subclassable, non-escaping.

Example: we change ``{ path : ptr char } -> Maybe {size : int64_t}`` to ``{ path : ptr char } -> Maybe {last_modified_time : int64_t, size : int64_t}``. In Swift this only breaks ABI if the ``frozen`` annotation is present. By default types are resilient, meaning they are passed by reference and the size, alignment, stride, and extra inhabitants of types are looked up from the type's witness table at runtime. But this is only outside the ABI boundary, inside the dynamic library it can assume the representation. And pointers have uniform layout hence don't need the witness table. Swift compiles polymorphic APIs to a generic ABI, rather than monomorphizing. Also fields of resilient types are only exposed as getters and setters, so can be computed instead of being stored fields.

Re-abstraction thunks wrap closures with the wrong ABI.

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

C/C++
-----

Interop with C/C++ is a good target feature. There are varying approaches (in increasing order of ease of use):

* libffi just implements basic assembly stubs for setting registers. It doesn't handle function signatures, memory layout or anything else - calling is all manual.
* `rust-bindgen <https://github.com/rust-lang/rust-bindgen>`__ parses headers with clang and generates FFI struct descriptions and function prototypes in Rust. It requires a separate build step. It doesn't handle many features properly, such as macros, inline methods, templates, inheritance, destructors, exceptions and non-trivial calling conventions.
* `c2ffi <https://github.com/rpav/c2ffi>`__ parses headers with clang and generates JSON. There is hacked in support for some preprocessor macros and templates, but it is otherwise similar to rust-bindgen.
* `dragonffi <https://github.com/aguinet/dragonffi>`__ again uses clang but it works by compiling code snippets. This allows the full range of C/C++ to be used.

I think the dragonffi approach is the best, since it's the most powerful and least error prone. There is some effort to analyze the result of the compilation and integrate it with the rest of Stroscot, but deep integration with an existing C/C++ compiler seems better than trying to write one from scratch.


-------


Usually these are modeled using primitive operations, e.g. file descriptors are allocated with the open syscall rather than declaratively as ``{ fd1 = inode 1234 }``. But the more state we model as state, the more powerful our debugging tools get. A traditional debugger has no way to undo closing a file. However, a filestate-aware debugger can reopen the file. The less we view the program as an I/O machine the easier it is to use high-bandwidth interfaces such as io_uring to perform bulk state changes - describing what rather than how is the hallmark of a high-level language. Of course, in most cases the program will use state in a single-threaded manner and it will simply be compiled to the primitive operation API by the automatic destructive update optimization.



 operational primitive as a function from input machine state to output machine state. This is actually a function because we can always set up the machine to a given state and see what it does.


 Generally speaking the machine can be simulated deterministically as a function from machine state to machine state - otherwise programs would not execute reliably. We can examine emulator projects such as QEMU or a formal ISA semantics to get a good idea of what each instruction does. Due to out-of-order execution the execution time of each instruction is nondeterministic; this is not modeled.

Yes, there are projects and tools that focus on ensuring reproducible execution, particularly by controlling and managing different aspects of the execution process. Some of these projects include:

1. **rr (Record and Replay Debugger)**: rr is a lightweight tool that enables the recording and deterministic replaying of execution traces of multi-threaded programs. It allows for the precise replication of program execution, helping in the identification and debugging of complex issues.

2. **Pernosco**: Pernosco provides a cloud-based collaborative debugging platform that allows developers to record, replay, and analyze the execution of complex software systems. It enables teams to collaboratively investigate and debug issues in a reproducible manner.

3. **Pin Play**: Pin Play is an extension of the Pin dynamic binary instrumentation framework that enables the record and replay of the execution of parallel programs. It allows for the deterministic reproduction of thread schedules and memory accesses, aiding in debugging and analysis.

4. **Deterministic Parallel Java (DPJ)**: DPJ is a programming model and runtime system that emphasizes determinism in parallel and concurrent Java programs. It provides constructs and mechanisms for controlling the execution of parallel threads, ensuring predictable and reproducible outcomes.

5. **Chaos Engineering Tools**: While not specifically focused on reproducibility, Chaos Engineering tools such as Chaos Monkey, developed by Netflix, and similar tools aim to test the resiliency of systems by inducing controlled failures. These tools can help uncover non-deterministic behaviors in distributed systems, leading to improved reliability and predictability.

These projects contribute to ensuring reproducible execution by providing tools and mechanisms to control and manage the concurrent execution of threads, handle I/O operations, and manage random number generation, thereby enabling the deterministic and consistent behavior of programs across different runs and environments.


Store state
-----------

Most papers limit themselves to keeping the values of mutable variables in the store. But conceptually the state of a program could include the state of the computer, the stock market, quantum fluctuations, etc. - all information within the chronological past of a program. But practically we are limited to state that we can read and write deterministically. In particular the read operation must satisfy the associative array definition:

::

    read k (write j v D) = if k == j then v else read k D
    read k emptyStore = MissingValue

So one constraint to be a variable is that the state must be accessible. So for example the kernel limits us - we do not have full control over peripheral devices or processes not related to ours. We can represent this by shadowing access-controlled variables and returning ``WriteFailed`` for inaccessible variables.

