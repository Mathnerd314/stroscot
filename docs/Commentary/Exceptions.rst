Exceptions
##########

Exceptions allow us to focus on the normal (non-exceptional) case when performing operations that might fail. The downside is that it is more tricky to reason about how the failure cases are handled, because the handling code may be many levels of function calls removed from the operation.

Exception menagerie
===================

The first question is what an exception is. Let us try to classify all the exceptions, everything that can use an exception-like semantic. The set of exceptions is unquestionably large so this list is probably incomplete.

Failure
-------

The most obvious exception is one that is thrown explicitly. Languages provide several ways to throw an exception:

* Haskell provides a TODO marker ``undefined`` that throws a predefined exception like ``ErrorCall "undefined"``.
* Haskell allows throwing a string, like ``error "something bad happened"``.
* VB allows raising a numbered message, like ``Error 1234``, with a more complex ``Err.Raise`` method with optional parameters ``{ source = "CurrentProject.CurrentFile", description = "Application-defined or object-defined error", helpfile = "C:/VBHelp/DefaultVBHelp.hlp", helpcontext = lookup number vbhelpfile  }``. The numbers are supposed to be unique declared constants so really they should be understood as module-qualified symbols.
* C++ allows throwing any value like ``throw x``
* Haskell/Java/C#/Python similarly allow ``throw x`` but limit throwable values to be of a type ``Throwable``, ``Exception``, or ``BaseException``.

A superclass ``Exception`` type seems more straightforward than C++'s "throw any value" - every value can be wrapped in a constructor such as ``ExceptionWrapper x``, and this avoids the semantic issues of having exceptional values which "aren't exceptions". Python switched from no common superclass required in v2 to requiring membership in ``BaseException`` in v3.

Domain exception
----------------

A domain exception is an "alternate result" returned from a function when its precondition for success is false, e.g. when the operation is called on an "invalid" argument outside of its domain. A domain exception is used when checking the domain via a separate function would be tedious - instead the caller can check the returned value to see if the precondition was satisfied.

In high-reliability scenarios, it's best to statically prove the nonexistence of domain exceptions or that the domain exception is limited to a small region. For example arithmetic overflows can represent security vulnerabilities and should generally be handled immediately. But otherwise there is a dynamic semantics for domain exceptions which lends itself to containment strategies.

Examples:

* Converting a (user-input) string to an integer may fail on invalid characters and return ``NotAnInt``.
* ``if "x" then ... else ...`` returns ``InvalidCondition``
* A case statement where the scrutinee doesn't match any of the patterns returns ``FailedMatch``
* Divide by zero returns ``DivideByZero``
* An arithmetic computation where the result doesn't fit in the specified representation, e.g. ``IntegerOverflow``
* A floating-point exception, when signaling is enabled
* A cast of a value to a type that it isn't
* An assertion or contract failure
* Looking up a nonexistent key in a dictionary returns ``KeyNotFound``
* Looking up an out-of-bounds index in an array returns ``IndexOutOfBounds``

System exception
----------------

These are used in C functions with the convention that a zero or positive return is success and a negative return is an exception. As such the exception codes are all negative. On Linux/POSIX there is EINTR, EINVAL, ENOENT, ETIMEDOUT, etc., on Windows there are the types NTSTATUS and HRESULT.

In many ways these are similar to domain exceptions, but the difference is that the failure is not predictable in advance with a predicate because it depends on external state that may change at any moment, in particular during the few nanoseconds between checking the predicate and executing the call.

The other difference is that the set of possible exception codes is not small or fixed. The kernel is complex and a system call may return almost any exception code, hence requiring hundreds of cases. Furthermore new kernels may add and use new exception codes. So in addition to exceptions for all known exception codes there also needs to be a family ``UnknownExceptionCode 123``.

Generally there is only one way a call can succeed. Hence exceptions are very useful to handle all the failure cases you don't care or know about in an aggregate manner.

Examples:

* Opening a file may fail with a ``FileNotFound`` exception. The file may have been deleted too recently to be detected by any previously performed checks.

* A query to a database might fail with ``DatabaseCorrupted``.

* A network operation might fail with an unreachable host exception, often split into many sub-exceptions for different levels of unreachability.

* Dereferencing a null or invalid pointer may return ``InvalidPointerException``

* Reading a file consisting of 10 blocks into a 1-block buffer may fill the buffer to capacity and return a ``PartialRead`` exception to alert that more can be read with another call. Arguably this is an "alternate success" but it still fits naturally into the system exception category.

Hardware exceptions
-------------------

Hardware exceptions are similar to system exceptions except that they are a small set of exceptions that can be triggered on almost every instruction. They take a convoluted path, first the thread triggers an interrupt, the interrupt is handled by the OS, then if the OS decides it's an exception it injects it into the thread that caused the exception. The exceptions are delivered immediately (synchronously); the thread cannot continue execution cannot resume where it left off.

On Linux hardware exceptions are transformed to signals SIGBUS, SIGFPE, SIGILL, SIGSEGV, SIGTRAP, SIGEMT (emulator trap, not used on x86) `in the kernel <https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/kernel/traps.c>`__. The type of exception is in si_code in siginfo_t, rather haphazardly: DE translates to SIGFPE with si_code FPE_INTDIV, `PF/GP <https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/mm/fault.c#L1487>`__ to SIGSEGV, SS/AC to SIGBUS, UD to SIGILL, etc.

On Windows / Visual C++ hardware exceptions are handled with `Structured Exception Handling <https://docs.microsoft.com/en-us/cpp/cpp/structured-exception-handling-c-cpp?view=msvc-160>`__. We can catch hardware exceptions inline in code with ``__try { } __except``. SEH can be thought of much like temporarily registered signal handlers, where the exceptional conditions are signals. The __try blocks define where the handler is in effect, and the __except and __finally blocks are the handlers if the "signal" is received. In 32-bit Windows the handlers are on a stack, in 64-bit Windows SEH uses instruction tables to do unwinding. GCC / LLVM haven't implemented SEH. (LLVM is `in progress <https://reviews.llvm.org/D102817>`__) There is also `Vectored Exception Handlers <https://docs.microsoft.com/en-us/windows/win32/debug/vectored-exception-handling>`__ which provides thread-wide hardware exception handling similar to Linux.

`Zig  <https://github.com/ziglang/zig/blob/e2b954c2738c683a85b864eb33530f0e3dbbc480/lib/std/debug.zig#L1527>`__ implements a signal handler for hardware exceptions that dumps the stacktrace and aborts. But we can do better and, like Windows SEH, allow unwinding to a handler in the code. In C signal handlers can only call async-signal-safe functions, but ``siglongjmp`` is async-signal-safe so in practice any code can be executed so long as ``sigsetjmp`` is called first. A further extension should be able to do DWARF unwinding and allow pretending that hardware exceptions are simply return values from instructions. Really what happens is that the handler jumps to a failure continuation, and this failure continuation is ``f DivByZero``, while the success continuation is ``f 123`` or whatever.

Resource exhaustion
-------------------

This covers running out of memory (OOM), stack (stack overflow), sockets, and file descriptors. Generally resource exhaustion exceptions are system exceptions from a failed allocation call.

Threads compete for resources. Any allocation attempt might fail, because the developer doesn't know the total resources available on the target system, and because other threads and other processes are simultaneously competing for that same unknown pool. But OOM locations are predictable to the compiler because it knows exactly where allocations occur and can throw an exception if the allocation fails. Hence OOMs can be localized to the source code that generates the allocation statement.



OOM isn't reliably reported by the OS because by default page allocation doesn't fail even in a low-memory condition. Instead the pages get swapped to disk and the system just gets really slow. On Linux even without swap the programs gets paused on trying to access the page and the OOM killer just selects a process to kill. Similarly ulimit just segfaults on OOM. To reliably enforce a limit it has to be checked by the allocator. But 32-bit address space exhaustion is reliably reported.

Stack overflow is more tractable than OOM, in the sense that there is no asynchronous competition for the resource, hence a static analysis can show that there is sufficient stack. It is also easy to handle stack overflow by switching to an alternate stack. It is also fairly predictable to determine whether an expression uses the C stack: it must call a C function.

Stack overflow can leave a Windows critical section in a corrupt state. Windows user routines likely have many stack overflow bugs, this isn't something it's hardened against. So maybe stack overflow isn't recoverable on Windows. On Linux the syscalls don't use a stack so should be fine.

If an application only uses a few pages of memory then the overhead for reserves is significant, so the amount of reserved space should be configurable or calculated to its minimum viable size.

Out of file descriptors is pretty easy to handle, since few operations allocate file descriptors and it is easy to avoid those.

Deadlock
--------

It can be detected that a thread is stuck when it is waiting on an MVar with no other references. Then the runtime can replace the takeMVar with throwing a BlockedIndefinitelyOnMVar exception. Similarly with Deadlock and some other Haskell concurrency exceptions.

These are synchronous exceptions in that they're directly attributable to the action the current thread is taking. But Haskell uses the asynchronous delivery mechanism for implementation convenience. IMO it's a bug, they should be delivered synchronously and not be maskable.

Nontermination
--------------

Infinite loops can be detected and replaced with a Nontermination or Loop exception. Dynamically, this can be implemented by decrementing a fuel counter on every reduction step and throwing an exception when it runs out. Whatever the starting fuel, an infinite loop is guaranteed to throw an exception. Statically the analyses are more general and can prove termination or nontermination without requiring the arbitrary choice of initial fuel. Most functions can be classified, but totality checkers are not omniscient.

Exception groups
----------------

Consider a parallel map, e.g. something like ``parallel-map arr $ \(i,v) -> f i v`` that can execute multiple ``f``'s concurrently. Now there may be 0, 1, or multiple failures of ``f``. If there are no failures everything is fine. But if there are 1 or multiple failures, we cannot return an array, and must throw an exception. If there is one exception we can just throw that exception. But if there are multiple, then what? In general all ``f`` may run in parallel, but if we execute some range on a thread then an earlier ``f`` exception will stop the thread and later ``f`` exceptions will not be reported. So reporting the complete set of (potential) exceptions is impossible, we can only report the exception encountered by each thread. And in fact the controller may kill all the worker threads after receiving the "first" exception, so the later threads will stop abruptly. This "first" exception may not be chronologically first due to scheduling vagaries, but it is logically the first as seen by the controller.

So, since all the other threads will be killed anyway after this first exception, the exceptions these other threads may or may not have encountered can be ignored, and we can just report the first exception to the caller. But this discards information.

Instead, the ``ThreadKilled`` exceptions can be reported along with the first exception and any other exceptions that manage to make it through. This is important enough that Joe Duffy `added <http://joeduffyblog.com/2009/06/23/concurrency-and-exceptions/>`__ an "AggregateException" and a Python PEP added `Exception Groups <https://www.python.org/dev/peps/pep-0654>`__. It does require a new catch mechanism ``try-except*``, to filter individual exceptions in the group, but it provides more control over exception handling in concurrent systems.

Of course true recovery still requires handling all exceptions inside the thread, before they are reported to the controller.

Aborts
------

An `abort <https://docs.microsoft.com/en-us/dotnet/api/system.threading.thread.abort?view=net-6.0>`__ is an exception that can't be suppressed unless you defuse it by calling ``ResetAbort`` with the correct token inside the catch handler. The abort is automatically re-raised at the end of any catch block that catches it without defusing it. A similar idea is an exception with a freshly defined type that can't be matched by anything but a corresponding handler.

Examples include aborting a UI computation before it finishes due to a redraw, and returning a solution directly from inside a search tree's call stack.

This got removed from .NET, so it's not clear that the rethrowing/defusing behavior is needed in practice. The control flow pattern can be implemented directly with continuations.

Runtime bugs
------------

* ExecutionEngineException
* An Access Violation inside mscorwks.dll or mscoree.dll
* A corrupt GC heap

These are thrown in the runtime or core standard libraries when safety invariants have been violated. Although it's generally a security risk to continue execution, there are cases where these exceptions can be handled, e.g. write barrier code that catches access violations and converts them into NullReferenceExceptions.

Cancellation
------------

SIGKILL/SIGSTOP cannot be blocked or handled by the program, so aren't exceptions. Similarly C's ``exit`` function and the Linux ``exit`` syscall always shut the program down and don't return. But Haskell provides a ``ProcessCancelled`` exception that propagates normally and does a hard process exit when it reaches the top level. Often processes are too coarse and one wishes to gracefully cancel a thread, so there is also a ``ThreadCancelled`` exception. Even finer is a ``TaskCancelled`` exception for a task runner library.

Generally with a cancellation exception you should only do cleanup. Cancellation is a message from outside of your current execution saying “you must die as soon as possible.” If you swallow the exception, you break the very nature of the cancellation mechanism. Similarly cleanup in response to cancellation should be minimal, avoiding long pauses, to ensure quick cancellation.

However catching the cancellation and containing it is possible, e.g. in the case of a sandbox or REPL that catches a ``ProcessCancelled`` exception and aborts the current evaluation instead of terminating the whole process.

Cancellation is not always needed. Usually one can get away with setting a flag, emptying a queue, etc. that gets checked in the processing loop and then the thread/process can finish gracefully by returning.

Haskell's asynchronous exceptions allow sending a cancellation exception to another thread. Uses: timeouts, aborting speculative computation, handling resource exhaustion.

Signals
-------

This mainly means `Linux signals <https://man7.org/linux/man-pages/man7/signal.7.html>`__, excluding process commands and hardware exceptions that also use the signal API. Signals can be process-directed (kernel op, ctrl-C in terminal SIGINT, kill(2), sigqueue(2), SIGEV_SIGNAL) or thread-directed (tgkill(2), pthread_kill(3), pthread_sigqueue(3), SIGEV_THREAD_ID). A process-directed signal can be delivered to any thread of the process that isn't masked. A thread-directed signal can only be delivered to the specified thread. The signal isn't necessarily delivered immediately but is queued if all targeted threads are masked. The limit is 1 pending signal of each type for standard signals (id<32), but real-time signals (33 to 63) can queue more up to some limit and also can carry an int-sized datum.

The most obvious signal is ``SIGINT``, sent by doing Ctrl-C in a terminal. On Windows console Ctrl-C handling starts a new thread in the process with whatever function is passed to ``SetConsoleCtrlHandler``. Also similar is Windows' graphical UI message queue, you can get a WM_CLOSE message when the window's X is clicked. Signals can be queued up similarly using DJB's self-pipe trick.

The general idea is to do unwinding/injection in a signal handler. Since most signals are out-of-band it is natural to make them resumable.

Non-categorizations
===================

The above provides a field guide to exceptions. But splitting exceptions into categories is rather loose. There are always special cases to the special cases - what is "rare" to one person might be another's bread and butter. Some people categorize exceptions with the idea of hard-coding how a category is handled. There are very clear drawbacks of hardcoding a handling pattern - it will be unclear to programmers whether they should opt-in to the handling, and for exceptions on the fence, programmers will have to constantly convert into and out of the category. But let's go through these proposals.

Unrecoverable errors
--------------------

Joe Duffy of Midori distinguishes "recoverable errors" from "bugs", and Herb Sutter claims "Programs bugs are not recoverable run-time errors and so should not be reported as exceptions or error codes" and "cannot be meaningfully handled". The idea is that recoverable errors use the typical exception mechanism, while bugs panic and fail-fast. These mechanisms have been adopted in Go and Rust. But, if you look at these so-called unrecoverable errors, they turn out to be easily recoverable with a handling or containment strategy. And `Linus <https://lkml.org/lkml/2022/9/19/1105>`__ says "in the kernel, 'panic and stop' is not an option". The kernel has a hard requirement that it limps along, no matter what happens, because there is no separate environment outside the kernel that can recover. There is no line in the sand where you can say "fundamental rule X has been violated, time to halt irrevocably". If it's not good enough for the kernel, it can't be a mandatory design in a general-purpose language like Stroscot.

For example Duffy considers null pointer exceptions unrecoverable. But a command ``dereference 0`` is just like a hash table lookup in terms of semantics. There is nobody saying that trying to look up a missing element in a hash table should crash the program. In fact many pointer issues cause no problems in C programs in practice and there is an ``-fno-delete-null-pointer-checks`` option to avoid the dumb "standard" behavior.

Herb Sutter wants to make allocation failures unrecoverable. But `Linus <https://lkml.org/lkml/2022/9/19/1250>`__ says "'allocation failures cannot panic' [...] is a major kernel requirement". Even in application programming, although it's generally fine to panic on allocation by default, as soon as your program starts running into resource limits you start wanting more control. Being able to cancel a specific task or method call instead of the whole process is really useful. Of course here you are dealing with "soft" limits as physically running out of memory is all but impossible to due to swap.

Duffy argues that assertion failure is pervasive, so it must be unrecoverable so that we can make asserting functions no-throw. This is like saying that if you have a headache you should solve it by cutting off your head. There are less drastic options - fundamentally this is just a type signature problem. Stroscot is designed so you don't need type signatures in the first place, the "lethargy" solution. Another option, "loosening", is to include these pervasive errors in the function arrow, so a signature ``a -> b`` is really ``a -> b|Fail``. A third option "verification"  is to actually check if the assertions can be thrown, so that ``assert false`` must have ``Fail`` in the signature but tricky type signatures like ``divide : Int -> (Int\{0}) -> Int`` do not. Similarly, since allocation failure is configuration-dependent, it can be written to check code as though it is using an ideal memory allocator that never OOMs, or to use an analysis that allows omitting ``OOM`` in the signature if the function does not allocate.

Soft errors
-----------

Google says in their C++ style guide: "Invalid user input should not cause exceptions to be thrown". This also is an overreach. Invalid input is a perfectly fine exception, because often you are in the middle of parsing a part of the input and it needs to bubble up a few levels to get an input chunk large enough to report. This style recommendation seems to be to avoid uncaught exceptions, although it's hard to say because this is just a 1-line note in a blanket "Don't use exceptions" prohibition and they end it with the cop out "We would need to make the style guide even longer to document these restrictions!" In Stroscot uncaught exceptions will show up as soon as you write a type signature.

Undefined behavior
------------------

Per `SO <https://stackoverflow.com/a/6793463>`__  undefined behavior (UB) was a term originally used by the C standard to allow language constructs to behave differently across hardware. For example dereferencing 0 gave 0 on the PDP-11 but was a useful address on Interdata. These behaviors were documented in platform-specific addenda to the C standard provided by computer manufacturers and compiler implementors. Over time, the standard has moved some of this vagueness to the term "implementation-defined". Also any discussion of "undefined behavior" has disappeared, e.g. the platform-specific addenda have disappeared, leaving compilers to define what UB means. This has made ISO C unusable for OS development. :cite:`yodaikenHowISOBecame2021` As of 2022, UB in LLVM refers to the following constructs:

* nasal demons - Examples are division by zero and null pointer read, i.e. various domain exceptions. Encountering these is supposed to be "impossible" for any conforming program, so any execution path leading to this is dead and and any transformation of this codepath that doesn't affect defined execution paths is fine. AFAICT only gcc compiler writers like the nasal demons interpretation. It got introduced because it allowed removing checks in inner loops for significant speedups. Nasal demons can lead to unwanted optimizations - for example gcc will delete a null pointer check because the pointer was previously dereferenced hence it being null is "impossible" (Linux kernel CVE). LLVM is relatively conservative on the nasal demons and `uses traps in many cases <https://blog.llvm.org/2011/05/what-every-c-programmer-should-know_21.html>`__
* poison - basically an exception. Example is the result of integer overflow on non-wrapping operations. It propagates like an exception through most operations. Certain operations such as phi, select, and freeze have recovery behavior. `Other operations <https://llvm.org/docs/LangRef.html#poisonvalues>`__ such as branches, address dereference, division, returns, and calls, trigger nasal demons on poison instead of propagating.
* undef - Examples are uninitialized variables, clobbered registers, and flags reserved by the CPU manufacturer. It's a register with indeterminate value (kind of). It represents the set of all possible bit patterns at the specified width, with an actual value chosen non-deterministically at each read. There are `some issues <https://web.archive.org/web/20180621011720/http://sunfishcode.github.io/blog/2014/07/14/undef-introduction.html>`__ with specifying when the read occurs. LLVM is resolving these issues by replacing the value ``undef`` with the operation ``freeze poison`` - because it is an operation, the non-determinism side effect is precisely located. So this form of UB is effectively deprecated in favor of ``poison``.

There is a simple transformation of replacing UB with the "safe" behavior of doing runtime checks and throwing an exception upon encountering what would otherwise be UB. In particular the CompCert C interpreter's `UB semantics <https://compcert.org/man/manual004.html>`__ are that a fatal exception is thrown once any UB is encountered.

Usability-wise getting rid of UB by default is great. Users like programs to either work or not work - debugging silent corruption due to a new compiler optimization is not fun. As further evidence, most new languages have decided to avoid UB and be "safe". The most notable is Java with its VM semantics. Rust similarly panics on division by zero and `panics or is well-defined for most other operations <https://github.com/rust-lang/rfcs/blob/master/text/0560-integer-overflow.md>`__.

Cost-wise safety is not usually that expensive. LLVM traps on UB in most cases anyways, and the general improvement in processors seems to have gotten runtime checking / exception throwing to be sufficiently fast that there is little benefit to nasal demons. Java's HotSpot optimizations like moving checks out of tight loops get code very nearly as fast as removing the checks entirely, and memory bandwidth dominates most code anyway. The checks are all inline code with cold paths so the only real cost is a few instruction and branch prediction cache misses.

Static verification can make safe code zero-cost over UB by proving that a block of code cannot throw any exceptions and then removing all the exception codepaths. There is the usual reject/defer/override choice if the analysis fails. For example Zig provides "disable safety checks" compilation modes / per-block annotations which say the undefined behavior is unreachable, enabling nasal demons behavior. Zig also has a force-enable runtime safety for the disable compilation modes, but IMO this is overcomplicating and it's simpler to drop the compilation modes and say the checks are always on unless disabled in the source.

Still though, there is some cost if an operation's semantics doesn't match the hardware - the checks cannot be eliminated by static verification. Providing "bare-metal" operations that do map 1-1 to hardware will solve this. Such operations are "safe" as well and have no undefined behavior because the hardware defines the behavior. So for example we would have x86 ``x / 0 = DivideError`` and ARM ``x / 0 = 0``, and these would compile to one instruction on their respective platforms (plus some unwinding code for hardware exception signal handling in the case of x86).

This palette of options should satisfy almost all use cases. The default of throwing exceptions is quite usable, and for speed one can code with the near-assembly bare-metal operations or fiddle around with static verification. In fact we can still get the nasal demons behavior by overriding the static verification analysis. But in this context we are clearly doing something unsafe so will expect the silent corruption if the override is incorrect.

Examples from `Chandler Carruth <https://www.youtube.com/watch?v=yG1OZ69H_-o>`__ and how Stroscot deals with them:

* null pointer dereference - standard operation throws exception, hardware-specific operation may produce something useful
* sink finding algorithm on cyclic graph - reduces to ``Meaningless`` exception if compiler notices it, otherwise infinite loop. Quoting Chandler: "You cannot actually do this [detect infinite loops]. You will run out of the ability to detect errors and the trade-offs you're making in performance are insane here." But AProve identified termination in 316/497 C programs in TERMCOMP 2022, so this is in fact possible to some extent. If the termination checker can solve most of the simple cases then giving a warning for hard cases will be useful to some.
* ``(1 : u32) << 33`` - standard operation throws exception, hardware-specific operation may produce something useful
* ``(1 : i32) << 31``, ``(0b11 : i32) << 30``, ``(0b111 : i32) << 30``, ``(-1 : i32) << 31``, ``(-2 : i32) << 30`` - standard operation multiplies by power of 2 like ``x << y = x * (2 ^ y)``, throws exception on overflow
* ``alloc(16 +(n-1)*8)`` - warn that exception may be thrown due to allocation size being negative if ``n<=-1``. Arbitrary-precision by default so no risk of wrapping.
* 32-bit unsigned integer index increment - follow Zig/Swift, have standard unsigned integer operations error on overflow and specialized operations that wrap
* ``(min_bound i32) >> 7`` - standard operation uses 2's complement.
* ``memcpy null null 0`` - whatever makes sense to newbies. probably throws an exception.

Sync/async
----------

The sync/async split seems fine because they are clearly distinguished. Most languages avoid C++'s "throw any value" design and require  ``throw : Exception -> a``, providing a wrapper constructor ``error : a -> Exception`` to inject values into the exception type. Similarly (per Snoyman) sync/async use different library calls, so we should require disjoint types, like ``throw : SyncException -> a`` and ``throwTo : ThreadId -> AsyncException -> a`` and use ``AsyncE / SyncE`` wrappers to convert other values.

Also they are distinguished based on semantics. Synchronous exceptions are thrown at a clearly defined point in the instruction stream. This means the compiler can omit exception handling code if it can prove no exceptions can occur. Asynchronous exceptions originate outside the thread or flow of computation. The runtime system requires specific support to inject asynchronous exceptions into the thread, ideally allowing an exception to be injected at an arbitrary location in the code. Purely functional programming which avoids mutation and side-effects naturally has very few injection points; propagating an exception simply abandons the computation. Once injected the async exceptions bubble up like synchronous exceptions. Proving the absence of asynchronous exceptions requires a global cross-thread analysis, as well as analysis of signal IPC if those are modeled as asynchronous exceptions.

The general feeling seems to be "async exceptions are terrible" and to not bother. But Stroscot follows Haskell in having async exceptions anyway. The timeout and cancel functions in Haskell's async package use async exceptions to great benefit. The Warp webserver bases all of its slowloris protection on async exceptions. The downside is that the programmer must be aware of asynchronous exceptions if they are in use by a library, in particular being aware of injection points, but this is facilitated by splitting sync and async types.

Patterns
========

When a function call throws an exception, a programmer must decide: handle or propagate.

Handle
------

Log: Set a flag or write to a log file and use another handling strategy

Recover: Swallow the exception and execute an alternate code path that does not produce an exception or produces an exception unrelated to the original. Generally you want to recover as close to the exception's source as possible, but sometimes there is not enough context and it has to propagate a few levels before recovering.

While recovering from an OOM exception you have to assume that you can't allocate more memory. The program can try to allocate memory, and this can succeed, e.g. if another thread freed memory since the OOM was thrown, but the handler should still be designed to expect this to fail. So one can't call any memory-allocating functions - hence memory allocation should be visible in a compiler query and it should be possible to assert that a block doesn't allocate memory. But with a little work you can restore invariants, e.g. release locks or gracefully close network connections. A high-level catch-and-dump works if you pre-allocate a buffer for the dump. If there is a reasonable boundary then a containment strategy that terminates the current task, frees up its memory, and moves on is possible. Specialized code trying to do fine-grained OOM recovery needs extensive fuzzing or real-world use; most such code is incorrect the first time around. Generally it needs to do explicit memory management and test for OOM at every allocation.

Presubstitution: Behave as if the exception was a specific non-exception value. Simplest form of recovery, used by IEEE floating point. For example ``1 / 0`` returns the exception ``Infinity``, but ``1 / Infinity`` is presubstituted to 0 instead of propagating the exception.

Resume: The exception value contains a continuation. The handler performs some work and then calls the continuation. A more complex version of recovery.

Retry: execute a recovery block and call the block again with modified arguments. The block is treated as a transaction, meaning that the application state is not modified by the failed block. Most complex version of recovery.

Containment: All exceptions are caught at a level boundary (pokemon exception handling). It's not recovery - it doesn't fix the exception at the source, but merely restricts the damage. The inner level cleans up its resources when the exception propagates. The outer level terminates the inner level and (often) does logging, filtering, and display. Usually the outer level is close to the base of the program. For example, an event loop or thread pool, and only an throwing task gets terminated. Or a thread terminates but not the process. Or an exception gets caught before an FFI boundary to avoid polluting the API. In a high-reliability context containment is dangerous because code may cause damage if it continues and the other threads might not be isolated from it. But it can prevent DOS attacks by allowing partial restarts, and poisoning locks ensures isolation. Another issue is that exceptions may be handled incorrectly in the middle of the call stack. Still, a common and useful pattern.

Terminate (abort, crash): Ask to OS to end the process. Similar to containment but the boundary is the OS. The program must be designed to be crash-only, able to handle SIGKILL without data loss. This requirement is pervasive, e.g. a network protocol cannot demand a goodbye message, file I/O must use shadow copies, etc. But in a large fraction of cases termination is the right design anyway. For example the JVM apparently has weird bugs when you catch OOM, like computing 2 + 3 = 7, so termination is the only real option. Crash-only makes people more productive at writing code, because the error case is just a single call to terminate and there are no exploitable corner cases. But termination can't be the only handling mechanism because it doesn't allow graceful communication to the user or containing the restart to a thread. Still, it is good practice to start with termination, find unwanted terminations during testing, and replace with a different strategy.

Dump core: Similar to termination but the contents of memory is written out.

Backtrack: Try another path of execution at a previously encountered nondeterministic choice

Trap: Suspend process and signal exception. Wait for another process (e.g. interactive debugger) to fix

Propagate
---------

Unwind: Perform cleanup such as freeing resources, unlocking mutexes, restoring invariants, or setting a connection to an error state, then return the exception. The cleanup part is hard to specify - Stroscot provides invariant checking and finalizers, but it is not clear if these are sufficient. It is certainly possible to write code that unwinds correctly; and pragmatically, most code will work without any cleanup, or at least not corrupt user data.

Serialize: Unwinding but across a process or thread boundary. Catch action, convert to value, pass value via IPC, convert back to exception and rethrow.

Wrap: As unwind, but change the exception returned. Often this loses fidelity by replacing a very specific exception with a more generic one, making it harder to perform recovery unless the original exception is chained in.

Frequency
---------

The most common behavior is unwinding, followed by containment or termination. Recovery also occurs for some interfaces that use exceptions for common cases.

Traces
======

A trace is built by keeping track of the exception as it propagates. The semantics are a little different with lazy evaluation because the propagation is demand-driven, but should be close enough. E.g. ``case {}.x of 1 -> ...`` produces ``MissingCaseException { trace = NoSuchAttributeException {...}, ...}``. With fancy formatting the nested exceptions will look like a stacktrace. Space considerations limit the depth and detail of stack traces.  For example if you accumulate over an infinite list, traces are theoretically infinite, but properly the trace display should compress this somehow. Similarly tail calls mean entries may be added or missing. So the trace is a best-effort guess subject to compiler whims - it has no formal contract. Traces are mainly useful as a light reminder to the programmer of where to look in the code in a large codebase.

Alas, building a trace is expensive. Throwing an exception should be cheap. What do?

The basic strategy is to not provide traces in the language. Code should not use traces - the exception value should contain all relevant information to handle the exception. And a trace is mostly useless for debugging as it does not contain memory values - the programmer is better off walking through a dump with a debugger. Dumping core at the time of throwing is an established practice


 And with reversible debugging the trace and any other information can be extracted after-the-fact in a debug environment. But how do we debug production crashes? We could run in deterministic tracing mode all the time by default. rr shows it's possible to get the overheads low, but so far only works on Linux. Another solution is to and should allow recovery of the trace.

Erlang's solution is to only provide the first trace entry (closest to raising the exception). This is not too costly, and at least provides the file, line number, and attempted operation. E.g. assertions record the failing predicate expression.

But the main solution IMO is to determine that the exception is caught by a handler that doesn't use the stack trace and optimize it away as an unused read-only operation.

Top level
=========

There is always a top-level catch-all exception handler, which is guaranteed to not throw during handling. So exceptions never pop off the whole stack.

For a stateful function, the top-level handler has to figure out what to do in the case of an exception. For the most part the exceptions are known in advance, so this simply means running the exception or failure continuation in the ``Task`` instead of the success continuation. The failure continuation will in turn most likely retrieve the exception from the state and return it to the program continuation as an exception value. But the failure continuation could also stop the program or do something completely separate from the main program.

Automatic propagation
=====================

Exception handling can be classified as explicit or implicit, or to use `the Swift error handling rationale's terms <https://github.com/apple/swift/blob/main/docs/ErrorHandlingRationale.rst#kinds-of-propagation>`__, manual and automatic. Manual handling requires visible operators or control structures (markers) when calling a function that can throw exceptions, while automatic handling does not. Manual handling marks that the called function can throw exceptions, so it is also marked and typed per the Swift definitions. Automatic handling is unmarked and may be typed or untyped depending on if there is a type signature listing the possibly thrown exceptions.

With manual handling in unsafe languages such as C one can forget the marker, or (if it is not a single operator) write it incorrectly. Safe languages have checks for the marker integrated into the compiler - for example, Go's errcheck linter finds unhandled exceptions and missing exception checks every time. But since the markers can be inserted automatically, why write them at all?

The answer I've come up with is that the visible marker encourages beginners to write robust code because it provides a starting point to write down all the cases and reason through their handling. It is easier to identify errors in code using manual propagation because the erroneous cases are often visibly missing or underhandled. However, specialized marker syntaxes such as Swift's ``try`` or Rust's ``?`` remove this advantage, making the marker just syntactic noise. It's really only a difference for beginners, so the tutorial can garner the same advantage so long as it introduces explicit error handling first and emphasizes that automatic error propagation is a shortcut syntax and exception safety should always be considered.

The most common syntax is unwinding, and manual unwinding code, even if a single character, is still repetitive, tedious, ugly, and annoying boilerplate, making programmers discouraged and code less readable and maintainable. In particular there is the case where you are in a call chain several levels deep and want to throw an exception that is handled higher up - with manual propagation you would have to add unwinding code to each intervening function. But since this boilerplate is visible and translates into basic language facilities it is easy for beginners to understand. The sequencing is explicit in the translation so there is no ambiguity.

Automatic propagation is a language feature that makes it very easy to do unwinding. Vaguely, it decorates every expression with an early return of the form ``case expr of (e : Exception) -> return e; x -> x``. But this definition doesn't specify which exception gets returned, e.g. from ``throw a + throw b``. The more correct (operational) semantics is that as soon as an exception is evaluated (thrown) it is immediately propagated to the nearest applicable exception-catching context and the remaining part of the expression is discarded. This exposes the evaluation strategy of the language implementation. Java says left-to-right, but this prevents many optimizations. Stroscot's answer is that the exception returned is a deterministic function of the expression and compiler version. However the compiler's evaluation strategy is not exposed to the static verification system, so type signatures must be written as if either exception could be returned.

:cite:`peytonjonesSemanticsImpreciseExceptions1999` says that because automatic propagation is "nondeterministic", ``catch`` should be an operation of the I/O monad - but in fact nothing in their semantics makes use of the I/O monad. ``getException`` is just ``return`` and pattern matching (section 4.4, page 9). Their approach merely uses the I/O monad as a "sin bin" for nondeterminism. Stroscot's choice to expose the nondeterminism allows more concise and flexible pure exception handling. But since the verification system models the set of exceptions and ``catch`` as randomly picking one, it robustly checks all evaluation strategies, including strange possibilities such as ``let x = throw 1 + throw 2 in try x == try x`` evaluating to false. (CBN expansion duplicates x, then try/catch picks different branches)


According to `Joel <https://www.joelonsoftware.com/2003/10/13/13/>`__ automatic propagation sucks because the early returns mean magic gotos are invisibly sprinkled throughout your code. It does take some training to learn to read code as if every line, expression, and subexpression could throw an exception and to use finalizers appropriately. But automatic propagation gives streamlined syntax. With automatic propagation it does not require any changes to a call chain to throw an exception and catch it several layers higher up. Generally, it is easy to quickly write code for the happy path using automatic propagation because you don't mark any error paths.

The correctness of code using automatic propagation is hard to judge. An exception code path may unwind too soon and not restore its state properly, but this may not be obvious. There are a few mutable state strategies that are easy to check:

* construct pure values, then commit all of them at once with an atomic operation
* use RAII so that every resource is disposed of properly
* use type signatures to verify exception safety invariants

But in general, these cannot guarantee that the returned state is correct. So to satisfy the Joels, Stroscot uses manual handling by default, allows opting into automatic propagation on a per-exception value basis, and out on a per-file/function basis via a warning. Stroscot also allows manual handling all the time, regardless of whether or not automatic propagation is enabled.



 With a variant type like ``a -> b|Exception`` a function returns either a value or an exception. So just use the normal ``return`` keyword to return exceptions. Then to respond to specific exceptions programmatically, returned exception-or-values can be pattern-matched like any other return value:

The case handling syntax seems easy and clear, and it's possible to locally reason about and decide how best to react to exceptions.
But a Quorum-style study should check on what's clearest to beginners. Limiting ``return`` to normal values and using ``throw`` for ``Exception`` values is also a possibility.

Just because there is shared syntax doesn't mean exceptions don't propagate, exceptions still unwind if they aren't caught by the case statement. They can be wrapped up in a Result type though to prevent propagation.


Syntax
======

``throw`` / ``catch`` are the common syntax, like in Java:

.. code-block:: java

  try {
    throw new BadInputError("xyz")
  } catch (BadInputError badInputErr) {
    printf("bad input error occurred: %s\n", badInputErr)
  }

However these duplicate ``return / case``. Exceptions aren't magic and don't need special syntax, so we just use ``return / case``:

::

  case (BadInputError "xyz") of
    BadInputError badInputErr -> printf "bad input error occurred: %s\n" badInputErr
    _ -> return ()

Go introduced panic-recover-defer to replace throw-catch-finally.

.. code-block:: go

  func main() {
    defer func() {
      err := recover().(error)
      var badInputErr *BadInputError
      if errors.As(err, &badInputErr) {
        fmt.Printf("bad input error occurred: %s\n", badInputErr)
      }
    }()

    panic(fmt.Errorf("validateInput: %w", &BadInputError{input: "xyz"}))
  }

Per `Rob Pike <https://groups.google.com/g/golang-nuts/c/HOXNBQu5c-Q/m/ltQ-QHBrw9gJ>`__ it is deliberately hard to discriminate exceptions with the recover mechanism because "fine-grained exception handling makes code unreadable in practice". try-catch makes the code "inside-out".

Swift:
try X else catch - wraps into Either type, an exception value (failure) or a normal value (success)
try X else Y - presubstitute Y on exception

no-throw
========

The standard C++ ecosystem uses exceptions. But the extra paths introduced by exceptions `add measurable overhead <https://grenouillebouillie.wordpress.com/2022/05/09/the-hidden-cost-of-exception-handling/>`__.  So people create forks of existing libraries that eradicate exceptions. The Windows kernel, for instance, has its own fork of the STL that doesn't use exceptions. This bifurcation of the ecosystem is neither pleasant nor practical to sustain.

Exception API
=============

Value
-----

Semantically an exception-throwing function returns a tagged union of the exception value or the returned value. The exception value can store a little or a lot of data. Zig uses a global u16 enum ``err`` (for now, possibly to be expanded to a tagged union), with compiler support that allows writing individual error subset types across files. In C an error is an integer constant. Java uses a Throwable class. Go and Swift use an Error protocol/interface. In C++ any value can be thrown. Rust uses a polymorphic Result type that can specialize to `most of the above <https://pcarleton.com/2021/04/28/rust-what-to-pick-for-the-type-when-writing-your-own-result-type/>`__:
* an enum containing various types of library-specific errors
* the enum ``std::io::Error``, which is an ADT ``Os i32|Simple ErrorKind|SimpleMessage ErrorKind str|Custom ErrorKind std::error::Error`` packed to fit into a pointer-sized word
* a boxed ``std:error::Error`` trait

In languages without a tagged union type a boolean flag and two pointers are used - because the flag variable is often re-used, it will often not trigger an unused variable warning, meaning exceptions are unsafe in these languages.

For example, C puts the return value in an out parameter, the error in the global variable ``errno``, and the boolean flag as the return, sometimes mixed with useful return info. We generally need a temporary for each return value and out parameter. So a nested function call ``f(g(x))`` looks like:

.. code-block:: c

    auto tmp, HRESULT hr;
    hr = g(x, &tmp)
    if (isError(hr))
         errorhandling(hr, errno);
    auto result;
    auto hr = f(tmp, &result)
    if (isError(hr))
         errorhandling(hr, errno);
    return result;

Objective C uses an out-parameter ``NSError** err`` for the flag and exception data. ``err`` is declared locally in the calling function and used as an out-parameter multiple times. This looks like:

.. code-block:: c

    NSError err;
    auto tmp = g(x, err);
    if (isError(err))
         errorhandling(err);
    auto result = f(tmp, err)
    if (isError(err))
         errorhandling(err);

Go uses multiple return values for exception codes. You reuse err for each call, so for ``f(g(x))`` you write:

.. code-block:: go

    v1, err := g(x)
    if err != nil {
        fmt.Println("error")
        return
    }
    v2, err := f(v1)
    if err != nil {
        fmt.Println("error")
        return
    }
    return v2

C++ generally aims to give exceptions a streamlined calling convention, so that the "happy path" has no overhead. But the extra conditionals can still give significant code bloat.

Languages with variant types can use a single value to represent the failure/success/flag trifecta. In Haskell there's the ``Either a b = Left a | Right b`` variant type or its less informative cousin ``Maybe a = Either () a = Nothing | Just a``. Furthermore there is the monad transfomer `ExceptT <https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html>`__ equal to ``ExceptT (m (Either e a))``. Scala has Either, ``Option a = Some a | None``, and ``Try a = Success a | Failure Throwable``. Rust has Option like Scala and ``Result T E = OK T | Err E``.

Variants force the caller to deal with the exception if they want to use the result.  This works well unless the call does not really have a meaningful result (e.g. ``write_line : (&mut self, s: &str) -> Result<(), IoError>`` in Rust); then it depends on whether there is a warning for ignoring results. Variant types also tends to create a lot of nesting, one level for every sequential computation that can fail.

.. code-block:: rust

  fn parse_two_ints_and_add_them() {
    match parse_int() {
      Err e => Err e
      Ok x => match parse_int() {
        Err e => Err e
        Ok y => Ok (x + y)
      }
    }
  }

A bind operator addresses the repetitive exception handling logic but still requires nesting:

.. code-block:: rust

  fn parse_two_ints_and_add_them() {
    parse_int().and_then(|x|
      parse_int().and_then(|y|
        x+y
      )
    )

Returning errors early from the function addresses nesting:

.. code-block:: rust

  fn parse_two_ints_and_add_them() {
    x = match parse_int() {
      Err e => return (Err e)
      Ok x => x
    }

    y = match parse_int() {
      Err e => return (Err e)
      Ok y => y
    }

    return Ok (x + y)
  }

To solve nesting and repetition simultaneously Rust has introduced the question mark syntax:

.. code-block:: rust

  fn parse_two_ints_and_add_them() {
    x = parse_int()?
    y = parse_int()?
    return OK (x+y)
  }

Defining errors
---------------

Per Snoyman ``error "something bad happened"`` is bad practice. String-based exception messages make proper exception handling difficult. Instead, it's best to define a custom exception type SomethingBad, which is trivial to catch, ``catch (\SomethingBad -> ...)``. Syntax for defining a custom exception type:

::

    symbol SomethingBad
    show SomethingBad = "something bad happened"
    isException SomethingBad = true

    foo = throw SomethingBad

It's still a bit wordy though, a macro is better:

::

  exception SomethingBad "something bad happened"

Exceptions in IO
----------------

In Haskell there are two primitives for throwing an exception, ``raise# : Exception -> a|Exception`` and ``raiseIO# : Exception -> Cmd``, wrapped as ``throw`` and ``throwIO``. ``throw`` creates an exception which will propagate as soon as it is evaluated. ``throwIO`` is a command which will propagate once it is executed.

Example: interacting with a file.
Let's consider the simplest program, suitable for scripting tasks and other things where you don't want to think too much:

::

  readFile fp =
    handle <- openFile fp ReadMode
    readAllBytes handle

Opening might fail and interacting with the file handle might fail. How do we handle this?

::

  readFileHandled fp =
    handle <- openFile fp ReadMode
    case handle of
      NoSuchThing -> return handle
      (_ : Handle) -> readAllBytes handle

  readFileSafe fp =
    eres <- try (openFile fp ReadMode)
    case eres of
      Error NoSuchThing -> return eres
      Result handle -> readAllBytes handle

In first, we represent failure via return values. E.g. if the file doesn't exist, ``openFile`` reduces to an action that returns ``NoSuchThing``, rather than a file handle, i.e. ``openFile "nonexistent"`` reduces to ``\x -> x NoSuchThing`` (``return NoSuchThing``), type ``((Handle|Error) -> Task) -> Task``. In second, the task instead reduces to an exception (a non-task value), i.e. ``openFile "nonexistent"`` reduces to ``NoSuchThing``, type ``(Handle -> Task) -> TaskE where TaskE = Exception | Task { Task = TaskE }``.

So the programs look like ``... >>= \cont -> readAllBytes NoSuchThing cont`` versus ``... >>= \cont -> NoSuchThing (\handle -> readAllBytes handle cont)``. With strict semantics both of these reduce to ``\cont -> NoSuchThing``.

With the second we need ``try`` to walk through the ``Task`` structure until it hits an exception or the end of the continuation. So the first is nicer. But the second might be useful for tasks that don't return a value and fail rarely.

The two approaches can be stacked; ``openFile`` could return a ``Symlink`` exceptional value or reduce to a ``NoSuchThing`` control-flow exception. Then ``try`` will return ``Result (Handle|Symlink) | Error NoSuchThing``. But this is really verbose to handle, we really want ``Result Handle | Error (Symlink|NoSuchThing)``.

Consider the following function:

::

  func =
    foo = lookup "foo" m
    bar = lookup "bar" m
    baz = lookup "baz" m
    f foo bar baz

We want composability and a unified interface across Maybe, Either, and IO. Say we need to know about why a lookup failed. ``lookup k`` could throw ``KeyNotFound k``, ``lookup :: (Eq k) => k -> [(k, v)] -> (KeyNotFound k|v)``. Exceptions should unwind like Either, so if any of the lookups fail then func returns the failure. We should be able to specify a default for lookup like with ``maybe``, ``lookup key m {KeyNotFound _ = Nothing}``.  The type of ``f`` should not contain the lookup exceptions, ``f :: SomeVal -> SomeVal -> SomeVal -> (F'sExceptionType|F'sResult)``.

Try
---

Swift/Rust define syntactic markers for local exception propagation points, a "try" or "?" keyword at the call site. ``try foo()`` examines the ValueOrError type that ``foo()`` returns. If it is an exception, ``try`` unwinds/propagates/throws/returns the exception from the function, otherwise the function continues with the value. The claim is that without ``try`` exceptions are silent or invisible.

But in practice this is very burdensome. Every call involves an annotation, either on the function (to say it cannot generate exceptions) or on the call site (to mark propagation). It's a lot of bookkeeping. Many languages have implemented exception handling just fine without this burden.

Precise type signatures offer a similar guarantee - the throwing function must be annotated to say it can throw, and the catching function must be annotated to say it can catch. But unlike ``try``, with precise signatures for a call chain ``A-B-C`` the intervening ``B`` does not need any modification when ``C`` starts throwing exceptions, and the modifications must still be made even if ``C`` already throws exceptions.

Even without ``try``, it is still possible to understand the control flow of a function - just assume every operation may throw an exception, and code accordingly. Indeed, async exceptions, OOM, and other "universal" exceptions can be thrown from almost anywhere, so ``try`` is just noise on every function call if these are included.

Still though, some people may like seeing where exceptions come from. So there is a warning ``-Wunmarked-exception`` that takes a set of exceptions as argument and warns for each callsite not marked with the identity function ``rethrowing`` that can propagate exceptions from the set. With this warning you can get Swift-like behavior for a subset of exceptions.

Exit points
-----------

non-local control problem: To know the resulting state by the time exception gets caught, need to know
- a program's state at the time of the throw
- the state changes that occur while that exception is propagated up the call stack - and possibly across threads in a concurrent program

exceptions create an abrupt jump from one point of code to another, like goto. They create too many possible exit points for a function. To write correct code, you really have to think about every possible code path through your function. Every time you call a function that can raise an exception and don’t catch it on the spot, you create opportunities for surprise bugs caused by functions that terminated abruptly, leaving data in an inconsistent state, or other code paths that you didn’t think about.

It is true that what should be a simple 3 line program often blossoms to 48 lines when you put in good exception checking, but that’s life, and papering it over with exceptions does not make your program more robust.

Signatures
==========

Callers have to code to handle the exceptions, so they need to know which exceptions are thrown. The exception set is part of the return type and function semantics.

There are several warnings that check exception lists:

* unused-exception - an exception or exception set is listed, but there is no way to throw it
* unlisted-exception - an exception may be thrown on a given input, but is not contained in the return type
* duplicate-exception - supposing the return type is ``E1|E2|R``, both ``E1|R`` and ``E2|R`` are valid signatures

Sample signature styles (enforced by the compiler where relevant):

1. ``precise`` - the set of thrown exceptions is listed in the signature. All possible exceptions given the types of the arguments are listed, and no unreachable exceptions are allowed in the list.
2. ``lower`` - a set of definitely thrown exceptions are listed, but other exceptions may be thrown
3. ``upper`` - like precise, all possible exceptions must be listed, but unreachable excpetions may also be listed

With ``lower`` it is not possible to say that a function doesn't throw, but with the other two it is.

Call chains
-----------

The `C# post <http://web.archive.org/web/20060101083304/http://discuss.develop.com/archives/wa.exe?A2=ind0011A&L=DOTNET&P=R32820>`__ complains that if you have a chain ``a = b catch ...; b = c; c = d; d = ...`` and ``d`` is changed from no-throw to throwing ``SomeException``, then ``b`` and ``c`` must have ``...|SomeException`` added to their type.

This is similar to Java's checked exceptions and Swift says they like this requirement because it adds extra static safety. When you add a new exception to ``d`` you get exception messages for all the call sites and can decide to handle or propagate to fix each site. Swift gets a similar but less precise safety check from marking call sites as throwing with ``try``.

But the C# posts says having to change all the type signatures just to throw an exception is a pain. It encourages "swallowing" exceptions by catching and ignoring them, instead of changing the signatures. Swallowing can result in an inconsistent state with no debugging traces. Handling exceptions at the appropriate place is better - e.g. in this case function ``a`` might have more knowledge of the state of the world.

With exception set synonyms the amount of work needed to add an exception can be minimized. Java only allows defining synonyms with superclasses, which isn't really composable if you have different libraries. But set union and difference are quite useful and mean that the program can adapt to exception behavior without advance planning.

The ``lower`` style of signature doesn't require any synonyms because exceptions can be omitted from the signatures, but uses synonyms for commonly occuring sets of exceptions. This is the most efficient in terms of productivity because the code requires no extra work for exception changes. If a user wants to document that some exceptions are thrown they can add them to the signature. But it isn't required, and it adds extra work later if you want to stop throwing the exception.

With ``upper`` a synonym style is to define one exception set ``LibraryException`` with all the common exceptions your library throws (overflow, divide by zero, out of memory, etc.) and use that in each signature. It is not too hard to maintain a single exception set for a library. It's a little better than Java's ``throws Exception`` because the exception set is finite, but requires almost as little maintenance as ``lower``. Exceptions that people should care about can be documented by adding them redundantly to the signature, ``DivideByZero|LibraryException``. And exceptions that aren't thrown can be asserted by removing them, e.g. ``LibraryException\DivideByZero``.
Application code can use set operations to build a combined set, ``AppException=(Library1Exception|Library2Exception)\(HandledException1|HandledException2)``.

With ``precise``, the style I came up with is to have a built-in compiler function ``exceptions _`` that computes the exception set of each function. Then for the actual signature you can write a self-referential signature ``a : ... -> Int | exceptions a``, if you don't want to make any guarantees about exception behavior, or ``Int | (exceptions a \ SomeException)``, to say that ``SomeException`` is not thrown, or ``Int | (exceptions a | SomeException)``, to say that ``SomeException`` is definitely thrown. ``exception x`` is somewhat magical is that it knows the rest of the signature and scopes the list of exceptions appropriately, e.g. for the signature ``x : Int -> Int | ExceptionA``, ``exceptions x = ExceptionA``, but for the signature ``x : Bool -> Bool | ExceptionB``, ``exceptions x = ExceptionB``, and similarly in the signature ``x : Int | Bool -> Int | Bool | exceptions x``, ``exceptions x = ExceptionA | ExceptionB``.

With ``precise`` you can also write a specification without referencing ``exceptions a``. doing a "full list" of all the component exceptions, or a "computed list" writing the set as a computation of child functions. So if ``a`` returns ``Int`` normally and calls ``b`` and ``c`` and catches ``SomeException`` from ``b``, then the computed list would be ``a : Int | (exceptions b \ SomeException) | exceptions c``. Both types of list cost some thought but ensure reliability as every exception is accounted for. A full list ensures that control flow is local because newly thrown exceptions must be caught or added to the list for every method in the chain. A computed list does not list exceptions that propagate through the function, so is less verbose. To newly throw an exception, it only needs to listed where it is thrown and where it is caught.

Lists are somewhat mindless in that the compiler knows the exceptions thrown better than the developer. The compiler should be able to compute ``exceptions x`` precisely and report it to the user, even if no annotations are used. In fact there should be two ways of reporting it, to follow the two styles of list: listing out all the thrown exceptions as a set (using predefined sets but not referencing any computed ``exceptions x``), or printing how to compute the thrown exceptions based on the thrown exceptions of the child functions (using ``exceptions x`` as closely as possible). Then the developer can read the spec, see that it looks alright, and copy it as a signature, and with an IDE fix signatures in just a few clicks.

So with ``lower`` or the self-referential ``precise`` style, no extra work is required to throw an exception, with ``upper`` one synonym has to be changed (the global list), with ``precise`` "computed list" style two signatures have to be changed (the thrower and the catcher), and with ``precise`` "full list" style all signatures between thrower and catcher have to be changed.

The full list style is attractive for small projects, but as Gunnerson says, for large projects this requires too much maintenance and thus decreases productivity and code quality. But there are various viable alternatives, with varying levels of precision.


Java checked exceptions
-----------------------

Java uses ``upper`` but with a set of unchecked exceptions (Error and RuntimeException) implicitly included as possibilities. For practical purposes this is basically the same as ``upper``. With the call chain-compatible style it's just defining ``LibraryException=...|RuntimeException`` - it doesn't really affect the style. RuntimeException is overly broad, for example division by zero should be checked.

A minimal set of common exceptions is those that pure functions can throw without using an explicit throw statement: async exceptions, OOM, stack overflow, and nontermination. But here it is still arguable that nontermination shouldn't be a common exception because most pure functions terminate and knowing that a function doesn't return is useful. With ``upper`` the problem is resolved definitively because there are no implicitly allowed exceptions.

A reduction in the cost of checked exceptions is to use a single "throws" keyword that allows all checked exceptions (similar to "throws Exception" in Java). The pain of versioning is reduced: either a function fails or it doesn't. The failure code is often irrelevant to handling. Swift, Midori approach. I like the synonym style of ``upper`` better, and using ``|Exception`` with ``upper`` to mimic this style is an option.

Default signature type
----------------------

For user types what exception style should signatures like ``foo : a -> b`` use by default?

The compiler will use model checking for coverage checking and code generation, so most spurious warnings that an impossible exception can escape won't appear - e.g. checking that a string contains only digits will ensure that the ``parseInteger`` function doesn't fail, so the compiler won't warn about an uncaught ``FailedParse`` exception. If the compiler can't prove it directly then an assertion will ensure an assertion failure  is thrown instead of the exception.

And the return type doesn't matter for overloading. But there is still the input validation situation where you know that a file is CSV but the compiler will warn that the parse can fail.

Going off of Haskell pure functions are common and should use minimal syntax. But pure functions can still fail with exceptions - assertions, OOM, etc. Similarly system calls may throw rare exception codes that don't need to be handled in practical programming. ``precise`` or ``upper`` would be verbose with maintaining the exception lists. ``lower`` gives succinct type signatures: write the exceptions that callers should care about and ignore the rest.  So for example one can write ``head : [a] -> a`` or ``head : [a] -> a|EmptyListException`` depending on what's important.

With proper set definitions as in the previous section ``precise`` and ``upper`` may not be so bad. These styles are definitely useful, but even so the signatures may be too complex for most users. In the end my preference is ``lower`` by default and ``precise`` for ``total`` declarations. But it still needs a survey to confirm this, as with most syntax decisions.

For ``lower`` there can be a warning that a listed exception has no handling code anywhere in the program (uncaught exception). Doing a similar thing with ``upper`` or ``precise`` would give false positives for uncommon exceptions like the unknown system call codes that don't need to be handled.

Fresh exceptions
----------------

A model where every function can randomly return exceptions is good `chaos engineering <https://en.wikipedia.org/wiki/Chaos_engineering>`__. A function that doesn't throw exceptions can still be typed as throwing exceptions, and with automatic exception propagation, there is often no actual handling code needed when adding a new exception. We can formalize this with the notion of "fresh" exceptions, exceptions using new (fresh) symbols that may or may not be in various sets. Propagating and catching such exceptions is dependent entirely on the sets the exception is in and are the same for all fresh exceptions in the same sets. If the function is written to handle one such "fresh" exception appropriately then it can be inferred to handle all fresh exceptions appropriately. And with async exceptions, all functions must be written to handle fresh exceptions or to handle async exceptions as a set.

Adding or removing a fresh exception to a function throwing a lot of fresh exceptions is not a semantic change - the function still throws fresh exceptions. The semantic behavior comes when there is a handler that matches on the exception. Removing said exception from the throw list results in dead code but does not break any contracts if the function's invariants still hold. So the only breaking API change is adding a non-fresh exception that client code unexpectedly matches. For example if you have ``{a; b} catch (\FooException -> ...)`` and ``a`` is throwing ``FooException``, then ``b`` also throwing ``FooException`` could lead to unexpected results if the handler for ``FooException`` is only designed to handle ``a``. This can be avoided by always using fresh symbols for new exceptions.

So the lifecycle looks like:

.. graphviz::

  digraph {

  nonexistent [label="not thrown, not caught"]
  fresh [label="thrown, not caught"]
  dead [label="not thrown, caught"]
  mature [label="thrown, caught"]

  nonexistent -> fresh [label="add failure"]
  fresh -> mature [label="add handler"]

  mature -> dead [label="remove failure"]
  fresh -> nonexistent [label="remove failure"]

  dead -> nonexistent [label="remove handler"]
  mature -> fresh [label="remove handler"]

  }

The key here is that removing an in-use failure from a function is not reversible; you can only add a fresh exception, not an in-use exception. This is an "open-world" style of exception handling.

There is also a "closed-world" style enforced by ``upper``, where the code is not designed to allow adding fresh exceptions and expects to deal with a fixed set of exceptions. Here it is not possible to add exceptions without breaking the API. But removal is fine. This can be worked around by wrapping new exceptions in old exception types and "escaping" the type checking, but it's better to break the API.


Composability
-------------

Function types which cannot generate exceptions are subtypes of function types which can. To use higher order functions like map, polymorphic types like ``forall a b. (a -> b) -> t a -> t b`` should allow ``a`` and ``b`` to contain exceptions. To reason about this properly ``upper`` or ``precise`` types are needed. There doesn't seem to be any downside to deferring exception propagation and treating exceptions as values when applying higher-order functions. Higher-order exception handling constructs are possible, but in practice most Haskell code seems to stick with catch or try. The only really complex construct is ``bracket`` but that's handled in Stroscot with finalizers.

It is quite useful to know the domain for which a function cannot generate exceptions. So usually a function will have two signatures, a "narrow" type for which the function doesn't throw exceptions and a "wide" type for which it does, e.g. ``(/) : Int -> (Int\{0}) -> Int`` and ``(/) : Int -> Int -> Int|DivisionByZero``. Ideally the compiler can prove that the narrow type is appropriate and specialize code to not use exceptions. This can be ensured by specifying a signature at the usage site that excludes the exceptions.

Stroscot's sets allow unions, e.g. you can express throwing ``MyException`` or ``HisException`` as ``x|MyException|HisException``. This makes combining libraries and their exception types fairly straightforward. This is impossible in many languages. Java's workaround is to instead use superclass catch-all types such as IOException and ReflectiveOperationException. It's not clear how useful these superclasses are - Swift claims reacting to an arbitrary IOException is difficult. IOExceptions can at least use an operation failure path that for example retries the operation a couple times, while Exceptions are so general that retrying may not make sense. But Storscot's subsets allow fine-grained definition so are much more expressive.

Snoyman `discusses <https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/>`__ using a ``Text`` type - it avoids the need for a real exception type, but means all exceptions are unstructured and can't be handled appropriately. His preferred approach is the constraint ``MonadThrow m``, but this throws ``Exception`` and isn't fine-grained. We could generalize by adding a type parameter to ``MonadThrow``, ``(MonadThrows m MyException, MonadThrows m HisException) => String -> m Int``, but now it's clear that this is the `existential typeclass antipattern <https://web.archive.org/web/20200510033212/https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/>`__ and ``String -> Int|MyException|HisException`` is much clearer.

Another note is that Stroscot's signatures are independent - they all are checked against the implementation, rather than a type interface. For example the following:

::

  a : Int -> Int|Exception
  a x = if x > 0 then x else NegativeException

  b : Int -> Int|NegativeException
  b x = a (x*2)

  c : Int
  c = b 3

``a`` can define a broad type for programmer convenience. But ``b`` can defined a precise type, e.g. for an exported interface. ``c`` declares that it throws no exceptions even though it calls exception-throwing functions, because the compiler can rule out those exceptions. With Java's checked exceptions, ``a``'s signature would require ``b`` and ``c`` to declare ``throws Exception`` or write a useless try-catch.

Implementation
==============

The implementation needs to transfer control from throw to catch, and run finalizers.

To illustrate take a simple example:

::

  foo = if p then throw Exception else return 42
  bar =
    x <- foo
    return (x + 1)
  baz = bar `catch` \e -> {print e; return 0}

Return codes
------------

With return codes this becomes:

::

  throw = return
  -- foo unchanged
  bar =
    tmp <- foo
    case tmp of
      e | isException e -> throw e
      x -> return (x+1)
  baz =
    tmp <- bar
    case tmp of
      e | isException e -> print e; return 0
      x -> return x

The tagged union for the value and the exception code costs extra registers/memory. It's setup/teardown on every call. Inlining helps, as does encoding the union as a machine word by returning exceptions via unusual return values such as negative numbers or zero. The values are often specific to the function and the values of the arguments.

Also checking the codes creates branches. Although the branches can usually be predicted they still pollute the cache and the duplicated exception-checking code takes up a lot of space. The branches add overhead to the non-exception path. But the exception path suffers no significant penalties compared to the non-exception path - it's a symmetric approach.

Continuations
-------------

The continuation-based approach depends on two things: callCC and dynamic scoping. Both of these are somewhat tricky to implement but end up with minimal overhead in compiled code. Basically, ``throw`` ends up being a continuation action passed via dynamic scoping. The interesting behavior is all in ``catch``:

::

  body catch handler =
    old = throw
    callCC (\ec ->
      return = return {throw=old}
      continue = continue {throw=old}
      break = break {throw=old}
      throw = \ex -> ec (handler ex) { throw=old }
      body
    )

The throw is unregistered if the function returns normally, otherwise ``throw`` restores the context and jumps to the handler.

setjmp / longjmp
~~~~~~~~~~~~~~~~

``setjmp`` / ``longjmp`` are an inefficient stack-based implementation of continuations. The registers must all be saved, costing a lot on both exception and non-exception paths. It's disliked.

::

  body catch handler =
    e = ref NoException
    ctx = setjmp()
    if read e == NoException
      body { throw ex = { e := ex; longjmp ctx } }
    else
      handler (read e)



Unwinding tables
~~~~~~~~~~~~~~~~

Unwinding tables are another stack-based implementation of continuations. The instruction pointer is used to find the loaded segment and its corresponding unwinding table. Every non-leaf function has an entry, and the return address on the stack is looked up to jump into handling code or code to pop the stack frame and restore register invariants. Since the tables can be cold and there's no branches on the non-exception path, the non-exception path is quite fast. But it's not zero cost because the exception path may keep registers alive that the non-exception path doesn't use, the code to read the tables takes up some space, and there are relocations at load time for the tables themselves.

Unwinding tables both help and harm performance. They require lots of data for the runtime support to unwind stacks, search for handlers, and so on. And compared to exception values these tables are slow to look up for the exception path. But if you have many levels of unwinding the normal path is faster because the values aren't wrapped and the handlers are cold and aren't dirtying your instruction cache or TLB.

The table-based system ended up being roughly 7% smaller and 4% faster (geomean) than the return code-based on some key benchmarks.

Another optimization is to cache exceptions as static data, so that throw doesn't allocate.

stack overflow in SEH filter clauses can be interpreted as not handling the exception.

Microsoft's implementation of C++ exceptions allocates on the stack, and delays deallocation until the end of a C++ catch clause. It is quite inefficient in terms of stack usage.

Zero overhead
-------------

Whatever marketing you have heard about zero-overhead C++ exceptions is misleading. Per `measurements <https://grenouillebouillie.wordpress.com/2022/05/09/the-hidden-cost-of-exception-handling/>`__ (also in Herb Sutter's thing IIRC), just turning on exception handling support in a C++ project previously compiled without exception support, not throwing any exceptions at all, gives a 15-52% binary size increase. The overhead arises from jump tables, additional stack space per thread (e.g., a 1K reservation, to save a dynamic allocation) and additional thread-local storage. For this reason many C++ projects disable exceptions.

With a proper design, a flag to turn off exceptions should not be needed. The compiler should be able to prove that code cannot throw exceptions and optimize away catch handlers accordingly. Only assertions that a function cannot throw are needed, to help the compiler with its proofs.

As far as strategy I'm thinking to switch between return codes and continuations depending on how hot the exception path is. Exception propagation for common exceptions (above 30%) should be handled by return codes. It would be good to bias the implementation somewhat towards the hot path (exception or not), e.g. by moving cleanup code on the cold path to the end of the function, out of the hot code path, but missing this isn't enough to seriously compromise performance. Continuation-based unwinding should be reserved for really rare exceptions, 1 in 100 or less, where cache misses predominate.

Interruptible cleanup
=====================

Interruptible cleanup actions - the interaction of async exceptions and cleanups. A cleanup function which may block and should be interruptible to avoid a long delay in execution.

When closing a file one often wants to flush buffers (fsync). So there are 3 variants of hClose:
* The flush marks a checkpoint, and should retry until complete regardless of interruptions
* The flush is unnecessary, just close the file
* The flush is productive but interruptible (EINTR), and should not be retried on interrupt. This avoids the situation where the flush takes a long time and the thread is unkillable. Note that it requires two async exceptions to kill the thread, one to enter the cleanup handler and another to interrupt the flush.

The Linux close syscall is interruptible, but it is guaranteed to close the file even if interrupted. Similarly hClose should close the file handle in all cases. Except when hClose is used outside the context of a cleanup, e.g. in the acquire part of bracket, interrupting should avoid visible side effects and interrupted hClose should not close the file.

sending a final "goodbye" message over a TCP connection.

putMVar/takeMVar: these should use tryPutMvar/tryTakeMVar

the default: with interruptibleMask or a separate bracketInterruptible this is a matter of taste. uninterruptibleMask provides behavior that is easier to reason about, but may block for an unbounded amount of time, possibly leading to deadlock.

"I don't want this action to throw exceptions. Sync exceptions are all caught, now I want uninterruptibleMask to disable async exceptions." or "I want this interruptible action for logging; I ensured the resource will be released if this action fails, so I disable async exceptions here only because the policy requires that".

an openFile might talk to a network mounted NFS drive and take 30s or so in the worst case. but this is where async exceptions interrupt the operation.

Injection
---------

Most languages use polling solutions to implement semi-asynchronous exceptions. These make various operations interruptible, i.e. before/after executing they check for async exceptions and if so inject the exception. But this isn't good enough since a lot of time may be spent between checks. A true async solution has no polling, the thread jumps directly to handling code.

On Linux we can use the ``pthread_kill`` API to inject true async exceptions as signals. This sends a signal to interrupt the thread, which if unhandled will simply terminate the process, so one must install a signal handler. Similar to hardware exceptions this handler does the equivalent of a ``siglongjmp`` to return from the signal handler into a normal execution context, and then does the unwinding magic (using DWARF or similar).

On Windows there is `no direct equivalent <https://stackoverflow.com/questions/37378035/how-to-signal-a-specific-thread-in-windows>`__ to ``pthread_kill``. But internally the functionality is there: a kernel-mode APC sets ``RequestInterrupt=TRUE`` so the scheduler will interrupt a running thread even if it's in a long computation. But we cannot directly create a kernel-mode APC from user code. There are some options:

* The QueueUserAPCEx Windows driver implements an API to create a kernel-mode APC. But signing drivers on recent versions of Windows is impossible.
* SuspendThread issues a kernel-mode APC (API intended for debuggers). So we pause the thread with SuspendThread, save its state via GetThreadContext, make a new context with the instruction pointer set to the handler, and resume the thread with SetThreadContext and ResumeThread. But it's low-level and requires several kernel roundtrips. Also GetThreadContext may `fail <https://stackoverflow.com/questions/3444190/windows-suspendthread-doesnt-getthreadcontext-fails>`__.
* Windows 10 RS5 adds "Special User APCs" (QUEUE_USER_APC_FLAGS_SPECIAL_USER_APC) which are delivered quickly via a kernel-mode APC, but then run as a user APC. The kernel-level API passes in a CONTEXT argument containing the registers from before the APC, like Linux's signal handler, but the documented API doesn't have this info. It may be possible to get it somehow with the documented API, or we can live dangerously and use the kernel-level API.

System calls on Windows are implemented with layers of C in between, so the handler has to ensure the C code completes to ensure proper cleanup. So it walks the stack and overwrites the first user-mode frame with an exception handling information frame, skipping internal Windows stack frames. This functionality is also useful on Linux, if we're using glibc.

throwTo the calling thread is an interesting academic question - the easiest is to define it as throw. GHC has some weird suspension behavior that interacts with unsafePerformIO.

Masking
-------

Asynchronous exceptions can be masked, then they get delayed to the unmask call. It allows writing "reliable" code that functions correctly even if an async exception is sent. But, if you mask async exceptions all the time, async cancellation will hang indefinitely. Use finalizers instead of masking wherever possible.

Most code should run unmasked, and if you do mask the scope of the mask should be minimized to ensure that asynchronous exceptions can be handled ASAP.

The mask function is ``mask io = if Masked then io {unmask = id} else io {Masked = True, unmask io = io {Masked = False} }``. To make masking composable, only the outermost mask takes effect - mask within a mask is a no-op and similarly the unmask it creates. This solves the "wormhole" `problem <https://mail.haskell.org/pipermail/libraries/2010-March/013310.html>`__.

How to implement masking? Linux provide a masking API to block signals, which queues them up. So we can just use that. On Windows the only way to mask kernel-mode interrupts is to use a critical region, which is again a kernel mode API and not something we can use. So the handler itself must check if the thread is masked and if so store the exception somewhere to be thrown on unmask.

::

  \cont ->
    Block
      TakeMVar m (\a ->
        catch (\c -> Unblock (compute a (Block c)))
          (\e _ -> PutMVar m a (throw e))
          (\b -> PutMVar m b (Unblock cont))
      )


Interruptible operations
------------------------

Some operations can block for a long time. Async exceptions must be able to interrupt this blocking for timely injection. So every operation which may block comes in two versions, interruptible and uninterruptible. An interruptible operation is conceptually similar to ``allowInterrupt; <op>``, but it only polls for asynchronous exceptions while blocked, and does not throw an asynchronous exception if it does not block.

Uninterruptible operations do not poll for async exceptions and hence can block indefinitely. They should only be used in exception cleanup code and only if there is no correct alternative. Sometimes there is no alternative, e.g. flushing a file cannot be avoided. Other times concurrency operations can be replaced with non-blocking versions, e.g. putMVar with tryPutMVar.

With uninterruptibleMask / interruptibleMask you can switch between the modes, and they only affect operations that can block. The mask itself is unaffected. So conceptually it is the operations that are uninterruptible.
``mask $ do { ...; uninterruptibleMask_ $ op; ... }``.

In C interruptible functions are specified to fail with errno set to [EINTR] if a signal handler interrupts the program. I.e., the system routine will return but fail to complete its action and one must call the system routine again. The SA_RESTART flag on the signal handler restarts the call, but the duration of the timeout is unspecified, so it is better to do it from user code, particularly when writing an asynchronous event loop that uses select or poll.

So to make calls reliably, when a program is using signals without SA_RESTART, it must check whether the return value of interruptible system calls is -1 EINTR, and restart the system call if it is the case. This is annoying to write; so, skalibs provides small wrappers around interruptible system calls, so that programmers can just call those safe wrappers and never bother with this again. The performance loss from having a wrapper layer is totally negligible compared to the cost of using a system call in the first place.

Unmask is not an inverse
------------------------

These pieces of code are not equivalent:

1. mask $ \restore -> (uninterruptiblePutMVar var x >>= foo) `catch` \e -> bar
2. mask $ \restore -> (putMVar var x >>= foo) `catch` \e -> bar
3. mask $ \restore -> (restore $ putMVar var x >>= foo) `catch` \e -> bar
4. (uninterruptiblePutMVar var x >>= foo) `catch` \e -> bar

The first snippet has two scenarios:
    putMVar succeeds, foo executes
    putMVar throws deadlock, bar executes
The second has an additional scenario:
    putMVar throws async exception, bar executes
The third adds another scenario:
    putMVar succeeds, the restore throws an async exception, bar executes

It's not a no-op to unmask an interruptible operation. There is a gap between putMvar and the end of the restore - an async exception can occur after the operation succeeds but before the transition from unmasked to masked.


It is wrong to wrap an ExceptT, EitherT, or ErrorT around an IO-based transformer stack. For example ``myFunction :: String -> ExceptT MyException IO Int``. The IO code can always throw exceptions, async exceptions if nothing else. You haven't limited the possibility of exceptions, you've only added one extra avenue by which an exception can be thrown.

 handling GHC's broken `asynchronous exception system <https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/>`__. The system is broken because it is so complicated that nobody can agree on the desired behavior / correct form of even simple examples. The prototypical example of using it is `bracket <https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Exception.html#v:bracket>`__:

::

  bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
  bracket before after thing = withRunInIO $ \run -> EUnsafe.mask $ \restore -> do
    x <- run before
    res1 <- EUnsafe.try $ restore $ run $ thing x
    case res1 of
      Left (e1 :: SomeException) -> do
        _ :: Either SomeException b <- EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ run $ after x
        EUnsafe.throwIO e1
      Right y -> do
        _ <- EUnsafe.uninterruptibleMask_ $ run $ after x
        return y

Here we use 4 operations: mask, try, ``uninterruptibleMask_``, throwIO. mask shields the cleanup action from being attacked by asynchronous exceptions, allowing exceptions inside restore. try catches exceptions and allows cleanup to occur. ``uninterruptibleMask_`` blocks interrupts from interrupting the after handler. Finally throwIO rethrows the exception, so that any exception inside the after handler will be swallowed.

Apparently, though, nobody can agree on whether the after handle should run with an uninterruptible mask.


Transformers

foo :: Int -> IO String
can always be generalized with a usage of liftIO to:
foo :: MonadIO m => Int -> m String

However,
bar :: FilePath -> (Handle -> IO a) -> IO a
needs lifted-base or exceptions, and gets:
bar :: MonadBaseControl IO m => FilePath -> (Handle -> m a) -> m a
bar :: (MonadIO m, MonadMask m) => FilePath -> (Handle -> m a) -> m a

This applies to exception handling and forking threads. use the Acquire type from resourcet.

Custom exception types

Some people prefer ``display`` for user-friendly display, and think ``show`` should always use literal syntax.

but exception code-style of checking done everywhere leads to huge amounts of untested/broken code-paths.

An exception in a pure computation does not immediately halt the program, but instead returns an exception value that propagates through the pure code and halts the program when it is used in an imperative instruction, close in spirit to what LLVM calls a `poison value <https://llvm.org/devmtg/2020-09/slides/Lee-UndefPoison.pdf>`__ . This enforces nonstrict execution of pure code, enabling speculation, discarding unused code, etc. Unlike LLVM which only has the exception value 'poison', Stroscot has numerous exception values. The exception values are contained in sets, which can form the exception value hierarchies found in Java or Haskell but can also express other relationships.

Under the hood an exception value is simply a normal value with ``isException t = true``. exception values can be loaded/stored into a variable or array. Pattern matching on an exception value will execute a matching or catch-all case if present, allowing exception recovery, but otherwise propagate a ``MissingCase`` exception. When an exception value reaches the top-level ``Task`` structure, the handler will detect that the exception is not allowed in the ``Task``, print the exception value, and exit the program. Tracking whether something can be an exception value requires the usual analysis to identify its type (possible set of values).

We can redefine exception values to be something else, e.g. add a definition ``NoSuchAttributeException {} "x" = 3``. Then ``{}.x == 3`` and the exception is silenced. Similarly we can do ``case {}.x of NoSuchAttributeException {} "x" -> 3``, or pass the exception to a function that does such exception-handling. We can also match on generic exceptions, ``case {}.x of e | isException e -> 3``. The alternative to ``isException`` is a single standard exception constructor ``Exception x``, IDK.

The exceptions also store the arguments to the exception, e.g. a ``MissingCaseException`` will store the value and the case itself, ``x, \x -> case x of ...``. These compose up the stack so that we can pass in a value at any point and resume computing.

I guess there is ABI stability to consider.  says the interface will be added to and not have things removed from it. This means I have to assume (for forward compatibility) that any positive integer could be added to the interface and returned as an exception code. So I need a catch-all anyway. But I can support up to some kernel version, and make the catch-all crash the program, instead of figuring out a reasonable behavior.


    Exceptions allow higher levels of an application to decide how to handle "can't happen" failures in deeply nested functions, without boilerplate in between. But you must still examine all of a function's transitive callers when adding a throw statement. Either the intermediate functions must support the basic exception safety guarantee or the handler must immediately terminate the program. For instance, if f() calls g() calls h(), and h throws an exception that f catches, g has to clean up properly or f has to terminate. Exception safety requires RAII and isolating writing to persistent state into a "commit" phase. This may force obfuscating code to isolate the commit, a cost that could be avoided by avoiding exceptions.


    writing no-throw code that uses an exception-throwing function is tedious - you have to handle all the exceptions and update the code whenever the exception list changes. But with precise checking it is pretty straightforward. In C++ integrating exception-throwing code into no-throw code doesn't work though.



Assertions
==========

An assertion expresses an expectation or requirement for the program state. Assertions function similarly to breakpoints in a debugger - they check a condition and trigger an exceptional situation. But because they appear inline in the source code, they communicate assumptions and constraints to other developers. They are more precise than a comment because they are executable. Depending on where they appear in the program flow and the condition, assertions can express simple defensive sanity checks, loop invariants, pre-conditions, post-conditions, or the presence or absence of side effects. Assertions ensure more reliable code.

At their core, assertions are still just a use of the exception-handling mechanism - ``assert cond`` is equivalent to ``when cond (throw AssertionFailure)``. Stroscot also allows generating values non-deterministically and constraining them using ``assume``, so that an assertion may check a property of a function over all inputs. This is more commonly referred to as a contract or signature, but there's no clear distinction between contracts, signatures, and assertions - the type assertion ``a : Int -> Int`` relies on non-determinism but can appear in the module (acting as a signature and showing up in the documentation) or in the body (checking a property of a value).

In typical languages, assertions have a verbose syntax that clutters the code. Stroscot introduces several specialized compact syntaxes, such as type assertions. This should allow using them more easily and make them suitable for more contexts.

Also, in C++, there are some pitfalls of assertions. To avoid unexpected behavior, Stroscot places several restrictions on assertion expressions. An assertion condition must evaluate to true or false. It must not require any continuation side-effects to evaluate. Ambient state can be read and written, but the state after evaluating the assertion expression is discarded and evaluation continues with the state from before evaluating the assertion expression.

assert - error if trace exists where expression is false, omitted if compiler can prove true, otherwise runtime check with error if expression evaluates to false,
assume expr - prunes traces where expression is false. backtracking implementation at runtime.

Regarding assertions that are only checked in debug builds, it is certainly possible with a statement of the form ``when DEBUG { assert X }``, and of course it is possible to define a function for this .


 in practice unconditionally enabled assertions are more useful/common, particularly if the compiler is good at checking assertions and optimizing them away. Imagine you're designing a car and put in air bags. You test the car and the air bags in all sorts of configurations and they work great and are much safer. But just as you're getting ready to go into production to send the car out to consumers, you take out all the airbags. That's what debug-only assertions are like. It's for this reason that GCC and clang do not deactivate asserts when compiling with optimizations.






