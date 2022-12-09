Intrinsics
##########

Hardware operations
===================

Hardware instructions are a specific series of assembly instructions on a specific platform. This fixes the behavior in all cases quite strongly - we can always set up the machine to a given state and see what it does. Generally speaking the machine can be simulated deterministically as a function from machine state to machine state - otherwise programs would not execute reliably. We can examine emulator projects such as QEMU to get a good idea of what each instruction does. Due to out-of-order execution the execution time of each instruction is nondeterministic; this is not modeled.

To abstract the ISA we consider each hardware instruction as a deterministic function from inputs to outputs - these functions are called "operations". Operations don't reference registers, the operations all take/return temporaries. Since all registers/flags/etc. can be stored/loaded to memory, temporaries are conceptually an immutable bitstring of a fixed bitwidth. These bitwidths vary by the instruction: x86 uses 1, 8, 16, 32, 64, 80, 128, 256, 512, etc. (for flags, segment registers, general-purpose registers, FPU registers, MMX/SSE/AVX). The precise definition of inputs and outputs requires some care. E.g. for floating-point the FPU state must be considered an input, and RDRAND must have the hardware RNG state as input. Some registers are left in a state that the Intel SDM calls "reserved for future definition" and "undefined" - these must be excluded from outputs, or the precise chip-specific behavior determined and the chip ID considered part of the platform ID. But with suitable munging, operations are well-defined.

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

Ideally we will expose every assembly instruction as a hardware instruction, for our supported platforms.

Portable operations
===================

On non-native platforms, the given assembly instructions for a hardware operation will likely not exist. Generally it is better to implement use a portable API written at a higher level, such as the limb-multiply routine in libGMP.

So the API structure is that we have "native" modules providing native hardware instructions, which give compile-time errors on attempts to use it on non-native platforms, and a portable library that provides a cross-platform interface using switch statements like ``case platform of A -> implA; B -> implB; ...``. Some hardware operations are ubiquitous, so it makes sense to expose them directly as portable operations. Addition wraps on all 64-bit hardware in the same way so only needs one portable operation. Other instructions like division have differing behavior, so we can provide 0-returning (ARM native) and ``DivideByZero`` exception-throwing (Intel native) division as portable operations. There is also the intersection of these functions with signature ``Int -> Int\{0} -> Int``, which is useful when we want zero-overhead on multiple platforms and can prove that the divisor is non-zero. But ideally the compiler will be able to optimize the conditionals out of the 0-returning/exception-throwing versions, giving the native version without requiring a separate function.

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

