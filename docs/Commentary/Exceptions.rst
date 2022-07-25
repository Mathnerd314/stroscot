Exceptions
##########

Exception menagerie
===================

The first question is what an exception is. Let us try to list all the types of exceptions, or everything that can use an exception-like semantic. The number of exceptions is unquestionably large.

Non-example: Process commands
-----------------------------

SIGKILL/SIGSTOP cannot be blocked or handled by the program, so aren't exceptions. Similarly C's ``exit`` function doesn't return.

Synchronous exceptions
----------------------

Synchronous exceptions are thrown at a clearly defined point in the instruction stream. This means the compiler can omit exception handling code if it can prove no exceptions can occur.

Domain exception
~~~~~~~~~~~~~~~~

A domain exception is an "alternate result" returned when the precondition for success is false, i.e. the operation is called on an invalid argument outside of its domain. This is used when checking the domain via a separate function would be tedious.

Examples:

* Converting a string to an integer may fail on invalid characters. It is easier to return a ``NotAnInt`` value from the operation than to restrict the type to strings that represent integers.

* Looking up a nonexistent key in a dictionary may return ``KeyNotFound``

* ``if "x" then ... else ...`` returns ``InvalidCondition``

* A case statement returns ``FailedMatch`` if the argument doesn't match any of the patterns

* C++ allows ``throw x`` where ``x`` can be any value. Most other languages limit ``x`` to be of a type ``Exception``.

System exception
~~~~~~~~~~~~~~~~

These are used in C functions with the convention that a zero or positive return is success and a negative return is an exception. As such the exception codes are all negative. On Linux/POSIX there is EINTR, EINVAL, ENOENT, ETIMEDOUT, etc., on Windows there are the types NTSTATUS and HRESULT.

In many ways these are similar to domain exceptions, but the difference is that the failure is not predictable in advance with a predicate because state may have changed between the predicate and the call.

The other difference is that the set of possible exception codes is not small. The kernel is complex and a system call may return almost any exception code, hence requiring hundreds of cases. But typically there is only one way the call can succeed. Hence the argument for exceptions, to handle all the cases you don't care about in a uniform manner.

Also the set of possible exception codes is not fixed. New kernels may add and use new exception codes. So in addition to exceptions for all known exception codes there also needs to be a family ``UnknownExceptionCode 123``.

Examples:

* Reading into a buffer may return more data than the buffer so can fill it to to capacity and return a ``PartialRead`` exception so more can be read on the next call.

* Opening a file may fail with a ``FileNotFound`` exception. The file may have been deleted too recently to be detected by any previously performed checks.

* A query to a database might fail with ``DatabaseCorrupted``.

Hardware exceptions
~~~~~~~~~~~~~~~~~~~

Hardware exceptions on Linux are transformed to signals SIGBUS, SIGFPE, SIGILL, SIGSEGV, SIGTRAP, SIGEMT (emulator trap, not used on x86) `in the kernel <https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/kernel/traps.c>`__. The type of exception is in si_code in siginfo_t SIGFPE->FPE_INTDIV (DE), SIGSEGV (`PF <https://github.com/torvalds/linux/blob/a931dd33d370896a683236bba67c0d6f3d01144d/arch/x86/mm/fault.c#L1487>`__, GP), SIGBUS (SS, AC), SIGILL (UD), etc. The signals can only be handled by the thread that raises them and are delivered immediately (synchronously); queueing and letting the thread continue normally doesn't make sense. The exceptions must be handled: execution cannot resume where it left off.

On Windows the equivalent of signals for hardware exceptions is `Structured Exception Handling <https://docs.microsoft.com/en-us/cpp/cpp/structured-exception-handling-c-cpp?view=msvc-160>`__ or more specifically `Vectored Exception Handlers <https://docs.microsoft.com/en-us/windows/win32/debug/vectored-exception-handling>`__\ . In Visual C++ we can actually catch hardware exceptions inline with ``__try { } __except``. But GCC / LLVM haven't implemented SEH. (LLVM is `in progress <https://reviews.llvm.org/D102817>`__)

SEH can be thought of much like temporarily registered signal handlers, where the exceptional conditions are signals, the __try blocks define where the handler is in effect, and the __except and __finally blocks are the handlers if the "signal" is received. In 64-bit Windows there are instruction tables that do unwinding.

`Zig  <https://github.com/ziglang/zig/blob/e2b954c2738c683a85b864eb33530f0e3dbbc480/lib/std/debug.zig#L1527>`__ implements a signal handler for hardware exceptions that dumps the stacktrace and aborts. But we can do better and, like Windows SEH, allow unwinding to a handler in the code. Signal handlers can only call async-signal-safe functions. In practice we can get around this with ``siglongjmp``. A further extension should be able to do DWARF unwinding and allow pretending that hardware exceptions are simply return values from instructions. Really what happens is that the handler jumps to a failure continuation, and this failure continuation is ``f DivByZero``, while the success continuation is ``f 123`` or whatever.

Logic exceptions
~~~~~~~~~~~~~~~~

A logic exception is the result of an undefined operation or programming bug. Examples: incorrect cast, attempt to dereference null, array out-of-bounds access, pattern match failure, assertion failure, contract failure, TODO marker.

Assertions and contracts are statically checked by default, but when the compiler cannot prove them it may emit runtime checks. These checks throw logic exceptions if the assertion doesn't hold. It's of course better to have the compiler prove the assertions, but some people are lazy and only fix things when they actually break in production.

Furthermore this category is not particularly distinguished from domain exceptions. A hash table lookup with a nonexistent key returns ``NotFound``; why does an array lookup with an out-of-bounds index crash the program? The claim is that "the code should be fixed" but this same argument applies to adding a member check to the hash table lookup.

Arithmetic exceptions
~~~~~~~~~~~~~~~~~~~~~

These can thrown in floating point with certain flags in Ada, Fortran (F90 and later), C++ and C (C99, fenv.h, float.h on certain compilers).

Divide-by-zero also throws. You want NaN-style propagation for DbZ.

Representation exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~

These happen when the result doesn't fit in the specified representation, e.g. arithmetic over/underflow. These can represent a security vulnerability.

Resource exhaustion
~~~~~~~~~~~~~~~~~~~

This covers allocation failure due to running out of memory (OOM), stack overflow, out of file descriptors, etc. Resource exhaustion exceptions appear in the typical way, as an expression reducing to an exception rather than its expected value.

OOMs are unpredictable at runtime because threads compete for memory. Any allocation attempt might fail, because the developer doesn't know the total resources available on the target system, and because other threads and other processes are simultaneously competing for that same unknown pool. But OOM locations are predictable to the compiler because it knows exactly where allocations occur and can throw an exception if the allocation fails. Hence OOM is not "asynchronous" - it originates from the allocation statement.

But the programmer cannot generally predict whether evaluating an expression will allocate and hence potentially throw an OOM, because of implicit allocations. Here are some examples:
* Implicit boxing, causing value types to be instantiated on the heap.
* marshaling and unmarshaling for the FFI
* immutable array operations
* graph reduction
* JITing a method or basic block, generating VTables or trampolines

But programming OOM-free is consistent, in the sense that if the compiler is able to eliminate all allocations and hence eliminate the possibility of OOM, then these will most likely be consistently eliminated on every compile. So asserting that a function or block can't OOM is possible. .NET had Constrained Execution Regions which implemented this, with various hacks such JITing the region at load time rather than when the region was first executed. So there's precedent.

So then there are two ways to handle OOM: let it crash, or try to recover. Recovering from OOM is hard, since you can't allocate more memory. It is allowed to try to allocate memory, and this can succeed, e.g. if another thread freed memory since the OOM was thrown, but the handler should still be designed to expect this to fail. The JVM apparently has weird bugs when you catch OOM, like 2 + 3 = 7, so crashing is the only real option there. But you can restore invariants, e.g. release locks.

Stack overflow is more tractable than OOM, in the sense that there is no asynchronous competition for the resource, hence a static analysis can show that there is sufficient stack. It is also easy to handle stack overflow by switching to an alternate stack. It is also fairly predictable to determine whether an expression uses the C stack: it must call a C function.

Stack overflow can leave a Windows critical section in a corrupt state. Windows user routines likely have many stack overflow bugs, this isn't something it's hardened against. So maybe stack overflow isn't recoverable on Windows. On Linux the syscalls don't use a stack so should be fine.

Out of file descriptors is pretty easy to handle. Since few operations allocate file descriptors, it is easy to avoid allocating FDs in a handler.

Deadlock
~~~~~~~~

It can be detected that a thread is stuck when it is waiting on an MVar with no other references. Then the runtime can replace the takeMVar with throwing a BlockedIndefinitelyOnMVar exception. Similarly with Deadlock and some other Haskell concurrency exceptions.

These are synchronous exceptions in that they're directly attributable to the action the current thread is taking. But Haskell uses the asynchronous delivery mechanism for implementation convenience. IMO it's a bug, they should be delivered synchronously and not be maskable.

Nontermination
~~~~~~~~~~~~~~

Infinite loops can be detected and replaced with a Nontermination or Loop exception. Dynamically, this can be implemented by decrementing a fuel counter on every reduction step and throwing an exception when it runs out. Whatever the starting fuel, an infinite loop is guaranteed to throw an exception. Statically the analyses are more general and can prove termination or nontermination without requiring the arbitrary choice of initial fuel. Most functions can be classified, but totality checkers are not omniscient.

Exception groups
~~~~~~~~~~~~~~~~

Consider a parallel map, e.g. something like ``parallel-map arr $ \(i,v) -> f i v`` that can execute multiple ``f``'s concurrently. Now there may be 0, 1, or multiple failures of ``f``. If there are no failures everything is fine. But if there are 1 or multiple failures, we cannot return an array, and must throw an exception. If there is one exception we can just throw that exception. But if there are multiple, then what? In general all ``f`` may run in parallel, but if we execute some range on a thread then an earlier ``f`` exception will stop the thread and later ``f`` exceptions will not be reported. So reporting the complete set of (potential) exceptions is impossible, we can only report the exception encountered by each thread. And in fact the controller may kill all the worker threads after receiving the "first" exception, so the later threads will stop abruptly. This "first" exception may not be chronologically first due to scheduling vagaries, but it is logically the first as seen by the controller.

So, since all the other threads will be killed anyway after this first exception, the exceptions these other threads may or may not have encountered can be ignored, and we can just report the first exception to the caller. But this discards information.

Instead, the ``ThreadKilled`` exceptions can be reported along with the first exception and any other exceptions that manage to make it through. This is important enough that Joe Duffy `added <http://joeduffyblog.com/2009/06/23/concurrency-and-exceptions/>`__ an "AggregateException" and a Python PEP added `Exception Groups <https://www.python.org/dev/peps/pep-0654>`__. It does require a new catch mechanism ``try-except*``, but it provides more control over exception handling in concurrent systems.

Of course true recovery still requires handling all exceptions inside the thread, before they are reported to the controller.

Aborts
~~~~~~

An `abort <https://docs.microsoft.com/en-us/dotnet/api/system.threading.thread.abort?view=net-6.0>`__ is an exception that can't be suppressed unless you defuse it by calling ``ResetAbort`` with the correct token inside the catch handler. The abort is automatically re-raised at the end of any catch block that catches it without defusing it. A similar idea is an exception with a freshly defined type that can't be matched by anything but a corresponding handler.

Examples include aborting a UI computation before it finishes due to a redraw, and returning a solution directly from inside a search tree's call stack.

This got removed from .NET, so it's not clear that the rethrowing/defusing behavior is needed in practice. The control flow pattern can be implemented directly with continuations.

Process exit
~~~~~~~~~~~~

Using a ``ProcessExit`` exception for exiting ensures graceful cleanup and allows cancelling the exit via catching, e.g. in the case of a sandbox or interactive interpreter. But of course there is an underlying ``exit`` command which always shuts the program down and does not return.

Serious bugs
~~~~~~~~~~~~

* ExecutionEngineException
* An Access Violation inside mscorwks.dll or mscoree.dll
* A corrupt GC heap

These are thrown in the runtime or core standard libraries when safety invariants have been violated. Although it's generally a security risk to continue execution, there are cases where these exceptions can be handled, e.g. write barrier code that catches access violations and converts them into NullReferenceExceptions.

Asynchronous exceptions
-----------------------

Asynchronous exceptions originate outside the thread or flow of computation. The runtime system requires specific support to inject asynchronous exceptions into the thread, ideally allowing an exception to be injected at an arbitrary location in the code. Once injected they bubble up like synchronous exceptions. Proving the absence of asynchronous exceptions requires a global cross-thread analysis, as well as analysis of signal IPC if those are modeled as asynchronous exceptions. Hence the tagline "async exceptions are terrible." But Stroscot follows Haskell in having them anyway. The timeout and cancel functions in Haskell's async package use async exceptions to great benefit. The Warp webserver bases all of its slowloris protection on async exceptions.

The programmer must be aware of asynchronous exceptions and code so that unwinding restores any necessary invariants, or track the invariant violations down
when they inevitably forget.
Functional programming which avoids mutation and side-effects except in the outermost loops
naturally has very few invariants; propagating an exception simply abandons all the work done up to that point.
However, complex mutations
cannot be trivially reversed.  Packets cannot be unsent. It
is inherently dangerous to asynchronously unwind from an arbitrary point.
The language has some responsibility to help here. It is not clear if invariant checking and finalizers are sufficient.
It is certainly possible to write code that handles asynchronous
exceptions correctly; and pragmatically, unwinding through most
code will generally just work.

Wrappers
~~~~~~~~

We distinguish sync/async with disjoint types; to allow throwing sync exceptions as async and vice-versa there are special "AsyncToSync" and vice-versa wrapper exceptions.

Thread cancellation
~~~~~~~~~~~~~~~~~~~

Often processes are too coarse and one wishes to gracefully cancel a thread from another thread. Uses: timeouts, aborting speculative computation, handling resource exhaustion. The solution is an asynchronous exception ``ThreadCancelled``.

With cancellation you should only do cleanup. Thread cancellation is a message from outside of your current execution saying “you must die as soon as possible.” If you swallow the exception, you break the very nature of the cancellation mechanism. Similarly cleanup in response to cancellation should be minimal, avoiding long pauses, to ensure quick cancellation.

The full power of thread cancellation is not always needed. Usually one can get away with setting a flag, emptying a queue, etc. that gets checked in the processing loop and then the thread can exit itself gracefully.

Signals
~~~~~~~

This mainly means `Linux signals <https://man7.org/linux/man-pages/man7/signal.7.html>`__, excluding process commands and hardware exceptions that also use the signal API. Signals can be process-directed (kernel op, ctrl-C in terminal SIGINT, kill(2), sigqueue(2), SIGEV_SIGNAL) or thread-directed (tgkill(2), pthread_kill(3), pthread_sigqueue(3), SIGEV_THREAD_ID). A process-directed signal can be delivered to any thread of the process that isn't masked. A thread-directed signal can only be delivered to the specified thread. The signal isn't necessarily delivered immediately but is queued if all targeted threads are masked. The limit is 1 pending signal of each type for standard signals (id<32), but real-time signals (33 to 63) can queue more up to some limit and also can carry an int-sized datum.

The most obvious signal is ``SIGINT``, sent by doing Ctrl-C in a terminal. On Windows console Ctrl-C handling starts a new thread in the process with whatever function is passed to ``SetConsoleCtrlHandler``. Also similar is Windows' graphical UI message queue, you can get a WM_CLOSE message when the window's X is clicked. Signals can be queued up similarly using DJB's self-pipe trick.

The general idea is to do unwinding/injection in a signal handler. Since most signals are out-of-band it is natural to make them resumable.

Other categorizations
---------------------

Some languages try to create separate categories such as unrecoverable failures, catastrophic exceptions, programming mistakes, and so on. For example there is Rust's Result vs panic, and Herb Sutter claims logic exceptions are not "errors" or "exceptions" as they "cannot be meaningfully handled". But this is completely bogus as panics and logic exceptions can be handled with the containment strategy just fine. So we classify them here as exceptions.

In general, splitting exceptions into hard categories seems to be very subjective and doomed to failure because there are always special cases to the special cases - what is "rare" to one person might be another's bread and butter. There's a very clear drawback of a hard split for exceptions - it may be unclear to programmers which side to use.

The sync/async split seems fine because async is distinguished by originating outside the thread, and this is a clear definition.

Patterns
========

When a function call throws an exception a programmer must decide: handle or propagate.

Handle
------

Log: Set a flag or write to a log file and use another handling strategy

Recover: Execute an alternate code path that does not produce an exception or produces an exception unrelated to the original. Generally you want to recover as close to the exception's source as possible, but sometimes there is not enough context and it has to propagate a few levels before recovering.

Presubstitution: don't call function again (abandon attempt), return a default value. Often the function's range is expanded to accommodate this. For example ``1 / 0`` returns ``Infinity``.  Simplest form of recovery.

Resume: The exception value contains a continuation. The handler performs some work and then calls the continuation. A more complex version of recovery.

Retry: execute a recovery block and call the block again with modified arguments. The block is treated as a transaction, meaning that the application state is not modified by the failed block. Most complex version of recovery.

Containment: All exceptions are caught at a level boundary (pokemon exception handling). It's not recovery - it doesn't fix the exception at the source, but merely restricts the damage. The inner level cleans up its resources when the exception propagates. The outer level terminates the inner level and (often) does logging, filtering, and display. Usually the outer level is close to the base of the program. For example, an event loop or thread pool, and only an throwing task gets terminated. Or a thread terminates but not the process. Or an exception gets caught before an FFI boundary to avoid polluting the API. In a high-reliability context containment is dangerous because code may cause damage if it continues and the other threads might not be isolated from it. But it can prevent DOS attacks by allowing partial restarts, and poisoning locks ensures isolation. Another issue is that exceptions may be handled incorrectly in the middle of the call stack. Still, a common and useful pattern.

Terminate (abort, crash): Ask to OS to end the process. Similar to containment but the boundary is the OS. Termination makes people more productive at writing code, because exceptions are obvious during testing. But it doesn't allow graceful communication to the user. It makes the system very brittle. But it is safe if the program is crash-only, designed to handle SIGKILL without data loss. In such a case termination is one method call away. Crash-only affects design, e.g. a network protocol cannot demand a goodbye message, and file I/O must use shadow copies, etc., so it cannot be the only option.

Dump core: Similar to termination but the contents of memory is written out.

Backtrack: Try another path of execution at a previously encountered nondeterministic choice

Trap: Suspend process and signal exception. Wait for another process (e.g. interactive debugger) to fix

Propagate
---------

Unwind: Behave as if the block immediately returned the exception

Serialize: Unwinding but across a process or thread boundary. Catch action, convert to value, pass value via IPC, convert back to exception and rethrow.

Cleanup: Perform some actions such as freeing resources or unlocking mutexes, then continue unwinding

Wrap: As cleanup, but change the exception returned. Often this loses fidelity by replacing a very specific exception with a more generic one, making it harder to perform recovery.

Frequency
---------

The most common behavior is unwinding, followed by containment or termination. Recovery also occurs for some interfaces that use exceptions for common cases.
scate constant, 99, 123, 420,
460, 481

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

Cleanup
=======

A function can return early if an exception is thrown. Combined with return/goto/break/continue there can be complex non-linear control flow. It is easy to forget about exceptions when writing some code.

Exception safe code works correctly even when the functions it calls throw exceptions. To avoids an explosion of exception handling code, many languages have a built-in language pattern for performing "cleanups", actions that must be performed even if an exception is thrown. Often the only reasonable way to ensure exception safety is to use cleanups habitually.

Examples of cleanups are resource deallocation, releasing locks, and rolling back a transaction after failure. Recognizing that an exception can leave things in an unwanted state and that a cleanup pattern should be used may be tricky for inexperienced programmers. Generally, a clean-up action becomes necessary as the result of some "acquire" operation.

C's ``if (err) goto cleanup; ... ; cleanup: f`` pattern and ``finally`` place cleanup code after the code that can throw, allowing reading the code top-to-bottom. But then throws act as go-downs and you have to scan down almost the whole function to the finally block to see what cleanups are waiting and if an allocation has a matching clean-up. Also there is an indentation pyramid with nested try-finally. Java 6's nested ``finally`` patterns ``x <- newThing; try { ... } finally { cleanUp x }`` (`standard pattern <http://www.javapractices.com/topic/TopicAction.do?Id=25>`__) and ``x = var null; try { x := newThing; ... } finally { if (x != null) then cleanUp x }`` (`here <https://stackoverflow.com/questions/2699209/java-io-ugly-try-finally-block>`__) are both awkward and verbose and have been replaced with try-with-resources ``try (f = newThing) { }`` (which is still awkward with `chained resources <https://stackoverflow.com/questions/12552863/correct-idiom-for-managing-multiple-chained-resources-in-try-with-resources-bloc>`__). C# has a similar ``using (f = newThing) { ... }`` syntax. For all of these interleaving resource usages to get ``alloc a; alloc b; free a; free b`` is impossible.

Go/Zig's ``defer``, `D <http://ddili.org/ders/d.en/scope.html>`__\ 's ``scope(exit)``, ``scope(success)``, and ``scope(failure)`` (referring to exiting the scope unconditionally as with ``defer``, without exception, or by exception respectively) put the cleanup before the code that uses the resource, right after the code that's being cleaned up after. It's easy to verify that allocations match up with their nearby cleanups, but the clean-up is delayed to the scope's exit. The syntax is very compact, just specify a cleanup function, but is a statement rather than an expression, meaning that the acquire must also be a statement. The pattern nests in the sense that ``defer``-ed statements are run latest-defined to earliest-defined on exit, but again doesn't allow interleaving. ``defer`` and ``scope`` do not allow returning a resource from a function and skipping the cleanup. They also introduce memory allocation questions since the deferred expressions can capture local variables.

RAII is very similar to ``defer`` but puts the cleanup in a destructor in a class. The class constructor is the resource handle allocation function, and the destructor is automatically run at the end of the function scope, resulting in no visible syntax overhead - just acquire a resource. It ensures you cannot insert any failure points between the allocation and the start of the cleanup's scope. A resource can even be allocated with its corresponding cleanup in the middle of an expression. But defining a new class for every cleanup operation is tedious - fortunately in C++0x it is possible to define one "RAII lambda" class and be done. And C++'s semantics define lambda memory handling so there is no allocation issue. Also with C++17 it is apparently possible to copy/move RAII types and store them in a data structure or return them from a function, but it seems fragile.

Finalizers are inspired by RAII but free resources "promptly" as opposed to at the end of the function scope. Similarly to ``defer`` the cleanup is placed before the code but similarly to ``finally`` there is a marker at the end of the scope. The pattern is ``newFinalizer f; ... ; use f``, where the ``use`` is at the function or block's end. The finalizer will run after the ``use`` if code flows normally and soon after raising an exception otherwise. Finalizers allow nesting, in the natural way ``newFinalizer a; newFinalizer b; ...; use b; use a``, and interleaving, by reordering to ``use a; use b``. But the execution order on an exception is latest-first. Finalizers also allow returning the allocated resource. It is also possible to embed the finalizer in a resource handle and use RAII style programming - each operation calls ``use`` and extends the finalizer's life.

For control flow purposes (ignoring async exceptions) finally can be `easily implemented <https://hackage.haskell.org/package/base-4.16.0.0/docs/src/Control.Exception.Base.html#finally>`__ in terms of catch:

::

  a finally sequel = (a catch \e -> sequel >> throw e) >>= (\r -> sequel >> return r)

As an idiom this is verbose due to the repetition of ``sequel``, but as a library function this doesn't matter. Per `JS semantics <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch>`__ a try-catch-finally is ``(a catch b) finally c``.

This doesn't work for two catch clauses though; that translates as ``a catch (\case b -> ...; c -> ...)`` rather than ``(a catch b) catch c``.

Cleanup and exceptions
----------------------

Throwing an exception from a cleanup action is somewhat tricky. Say we have

::

  x =
    f = newFinalizer (throw Bar)
    throw Foo
    use f

  y = x catch \case
          Foo -> print "foo"
          Bar -> print "bar"

The finalizer runs as soon as it is known that ``use`` will not be called - i.e. before the ``throw Foo``. So it is equivalent to ``throw Bar; throw Foo`` - then ``throw Bar`` wins and hence ``y`` outputs ``bar``.

As far as I can tell this is the obvious choice and the choice C++ should have made, but they instead decided to terminate on throwing exceptions from destructors during exception handling. The only justification seems to be that it simplifies the implementation of unwinding.

Finalizers directly implement ``scope(exit)``, and with an extra flag variable they can implement ``scope(success)`` and ``scope(failure)``:

::

  scope_failure rollback =
    flag = mut true
    f = newFinalizer (if flag then { rollback } else {})
    ScopeFailure flag f

  use (ScopeFailure flag f) =
     flag := false
     use f


  s <- scope_failure rollback
  code
  use s

`Herb Sutter <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4152.pdf>`__ claims that the extra ``use`` is "tedious and fragile" and forces the programmer to think about the placement of failure determination.

Well, you can define a proper ``scope_failure rollback { ... }`` combinator, and even redefine return/continue/break to not run the rollback. But personally I think the finalizers are more powerful because they allow interleaving scopes. The combinators only allow nesting which isn't as expressive. In Haskell land Snoyman created `ResourceT <https://hackage.haskell.org/package/resourcet>`__ even though there was ``bracket``, exactly because of this.

The `D guy <https://vimeo.com/97329153>`__ claims you need an even larger combinator than bracket, which goes as follows:

::

  if action then
    when !next
      rollback
    cleanup

With masking this looks like:

::

  generalBracket action next rollback cleanup =
    mask $ \unmasked -> do
      resource <- action
      b <- unmasked (next resource) `catch` \e -> do
        _ <- rollback resource
        _ <- cleanup resource
        throw e
      c <- cleanup resource
      return (b, c)

action has to be masked because there could be an async exception between the action and running next.

Exception safety
----------------

This code in Rust or C++ is not exception safe: (based on `this code <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/1995/N0623.asc>`__ and `this code <https://github.com/rust-lang/rfcs/blob/master/text/1236-stabilize-catch-panic.md#background-what-is-exception-safety-in-rust>`__)

::

  push_ten_more : (v : Vec T) -> T -> Op { v : Vec (T|uninitialized) }
  push_ten_more (this@(readRef -> Vector arr)) t =
    new_arr = alloc (length arr + 10)
    for (i in indexes arr)
      copy arr[i] to new_arr[i]
      delete arr[i]
    this := Vector new_arr

    for i in 0..10 {
      (ptr v) offset (len + i) := t.clone()
    }
  }

The update to the Vector happens when the next 10 elements are uninitialized, and ``Vec`` has an internal invariant that its elements are safe to deallocate. So if `t.clone` throws then the initialization will not be called. Vec's destructor that assumes the invariant will then free uninitialized memory.

The basic issue is that Rust and C++ confuse values with resources. Values can be copied without side effects, while resources are expensive to copy. In this code the Rust/C++ semantics require calling a destructor ``delete`` on each element of a ``vec``, and copying values with ``copy_to_`` and ``clone`` operations that can fail - almost everything is a resource. In Stroscot almost everything is a value, inert data - copy/clone is built into the language and can't fail. Similarly we wouldn't necessarily call any finalizers (``delete``) - the finalizer is called after the last use, and likely there are other copies and this is not the last use. Even if the Stroscot code was written to call an operation ``clone`` that could throw exceptions, the rest of the elements will be deallocated if needed, but otherwise not. In all cases memory is safe due to the finalizer semantics.

A smaller issue is the uninitialized array. This means the array may be filled with ``uninitialized`` values (exceptions). The result type reflects this possibility. With careful rewriting, the code can provide the strong guarantee that the resulting vector only contains values of type T. This can be done by extending the array one element at a time or by saving the exception(s) thrown in a separate list and rethrowing at the end as an exception group.

Besides explicit memory management, broken logical invariants are rarely observed. Reasoning about invariants with pure values is straightforward, and fail-fast coding styles mean that the program doesn't live long. And when writing cleanups the programmer is already thinking about exception safety and restoring invariants, so will write an exception-safe cleanup.

To write an exception safe operation on a mutable data structure, there are two steps:
* identify invariants of data structures. These can be written as assertions using the pure read operations on stores. With this the static verification will identify the function and the exceptional control flow that breaks the invariant.
* place exception cleanup handlers to restore broken invariants

C++ has `levels of safety <https://en.wikipedia.org/wiki/Exception_safety>`__ for stateful functions based on what invariants are preserved.

* no-throw means forward progress is guaranteed and no exceptions will emerge. This can be enforced by never throwing exceptions, only calling other no-throw functions, and forbidding async exceptions. But the entire ecosystem uses exceptions. So people create forks of existing libraries that eradicate exceptions. The Windows kernel, for instance, has its own fork of the STL that doesn't use exceptions. This bifurcation of the ecosystem is neither pleasant nor practical to sustain.
* Strong safety means that state transitions happen atomically and a failure will return to the old state. To ensure this one needs basic safety and to copy the relevant data beforehand and write it back afterwards.  This is infeasible for even simple data structures in C++ due to overloaded assignment and copy operators being able to throw.
* Basic safety means that the final state will be valid, i.e. all invariants hold. You need to safeguard against each function call throwing. This requires adding handling code to each call and trusting the documentation for the list of thrown exceptions (or using no-throw).

These levels only work for stateful data structures that call a small and easily auditable set of other functions.



A simple example is ``TwoList``, which maintains two mutable linked lists with the invariant that they are the same. What does adding an element look like?

::

  add a (List l) =
    head = read l
    l := Cons a head

  add a (TwoLists l1 l2) =
    add a l1
    add a l2

But this is not exception safe for the invariant, because an async exception between the two adds may add a to l1 but not l2.

If add is no-throw we can fix this just by adding uninterruptibleMask. But add allocates so can throw out of memory. But remove is no-throw so we can use remove:

::

  add a (TwoLists l1 l2) =
    mask_ $
      case try (add a l1) of
        Err e -> throw e
        Ok ->
          case try (allowInterrupt >> add a l2) of
            Ok -> return
            Err e -> uninterruptibleMask (remove a l1) >> throw e

Here add should have strong safety, i.e. it restores the state if an exception is thrown during the add.

Poisoning
---------

Rust has "poisoning" for taking locks. Essentially this uses the scope_failure cleanup to poison the lock on failure:

::

  getGuard mutex =
    acquireLock mutex
    flag = mut true
    f = newFinalizer {
      if flag then
        poisonLock mutex
      releaseLock mutex
    }
    Guard flag f

  finishGuard (Guard flag f) =
    flag := false
    use f

Once the lock is poisoned then locking the mutex will throw an exception. This is a safeguard against trying to use data that was corrupted due to an exception, e.g. a concurrent heap that doesn't have the heap property anymore.

Although poisoning by default allows using multithreading without having to consider exception safety across threads (as poisoned data will tear down all connected threads), invariant checking also covers single-threaded exception safety. But a PoisoningMutex can at least be in the standard library for those who want the functionality.

Syntax
======

The Swift error handling rationale classifies unwinding by the syntax required. With manual propagation is done with visible control operators or structures, while with automatic propagation happens according to rules defined by the language.

Manual
------

Manual propagation has tedious repetitive boilerplate, making programmers discouraged and code less readable and maintainable. But since manually propagated exceptions can be implemented with basic language facilities (out parameters, conditionals) they don't need any special considerations and are always available. The boilerplate marks the call site and that the function can throw exceptions, so it is also marked propagation and typed propagation. Manual propagation is often ugly and annoying but according to `Joel <https://www.joelonsoftware.com/2003/10/13/13/>`__ it's better than getting magic unexpected gotos sprinkled throughout your code at unpredictable places.

Manual propagation has to juggle three pieces of data: the error, the returned value, and a boolean describing whether an error or value was returned. The main coding problem is forgetting to check the flag and assuming you always have a valid value. This possibility does not make manual exception propagation "untyped" as the Swift document claims. Unsafe, perhaps. Because the flag variable is often re-used, it will not trigger an unused variable warning. However, Go's errcheck linter finds the missing exception checks every time, and can be integrated into the compiler as a warning.

The exception can store a little or a lot of data. Zig uses a global tagged union ``err`` type (limited to u16 for now), with compiler support that allows writing individual error subset types. In C an error is an integer constant. Java uses a Throwable class. Go and Swift use an Error protocol/interface. In C++ any value can be thrown. Rust uses a polymorphic Result type that can specialize to `most of the above <https://pcarleton.com/2021/04/28/rust-what-to-pick-for-the-type-when-writing-your-own-result-type/>`__:
* an enum containing various types of library-specific errors
* the enum ``std::io::Error``, which is an ADT ``Os i32|Simple ErrorKind|SimpleMessage ErrorKind str|Custom ErrorKind std::error::Error`` packed to fit in a pointer
* a boxed ``std:error::Error`` trait

C puts the return value in an out parameter, the error in the global variable ``errno``, and the boolean flag as the return, sometimes mixed with useful return info. We generally need a temporary for each return value and out parameter. So a nested function call ``f(g(x))`` looks like:

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


Automatic
---------

Automatic propagation is more succinct and efficient, and besides complicating the language semantics there's not much reason to avoid it.

``throw`` / ``catch`` have become the common keywords after C++ and Java, but it's syntactically heavyweight.

In Haskell there are two ways of throwing an exception, ``throw`` and ``throwIO``. ``throw`` creates an exception which will propagate as soon as it is evaluated. ``throwIO`` is a command which will propagate once it reaches the top level. This is generalized to ``throwM = lift . throwIO``.

Swift:
try X else catch - wraps into Either type, an exception value (failure) or a normal value (success)
try X else Y - presubstitute Y on exception

NaN style propagation - ``a + b`` is either an exception or the sum. Problem: ``ExceptionA + ExceptionB``, which exception gets returned? Depends on evaluation strategy of compiler implementation.

:cite:`peytonjonesSemanticsImpreciseExceptions1999` says that ``catch`` should be an operation of the I/O monad - but in fact nothing in their semantics makes use of the I/O monad, ``getException`` is just ``return`` and pattern matching (section 4.4, page 9). Their approach is just using the I/O monad as a "sin bin" for nondeterminism. Stroscot's choice is to instead make exceptions first-class values of the language, allowing more concise and flexible exception handling. Exception nondeterminism is first-class as well, so an exceptional value's denotation is in fact a set of exceptions, and ``try`` randomly picks one. So ``let x = throw 1 + throw 2 in try x == try x`` can evaluate to false.

Idea
----

Exceptions aren't magic and don't need special syntax. With a variant type like ``a -> b|Exception`` a function returns either a value or an exception. So just use the normal ``return`` keyword to return exceptions. Then to respond to specific exceptions programmatically, returned exception-or-values can be pattern-matched like any other return value:

::

  foo = return AnException

  bar = case foo of
    AnException -> "ohno"
    r -> "success"

The case handling syntax seems easy and clear, and it's possible to locally reason about and decide how best to react to exceptions.
But a Quorum-style study should check on what's clearest to beginners. Limiting ``return`` to normal values and using ``throw`` for ``Exception`` values is also a possibility.

Just because there is shared syntax doesn't mean exceptions don't propagate, exceptions still unwind if they aren't caught by the case statement. They can be wrapped up in a Result type though to prevent propagation.

``error "something bad happened"`` is bad practice. String-based exception messages make proper exception handling difficult. Instead, it's best to define a custom exception type SomethingBad, which is trivial to catch, ``catch (\SomethingBad -> ...)``. Syntax for defining a custom exception type:

::

    symbol SomethingBad
    show SomethingBad = "something bad happened"
    isException SomethingBad = true
    foo = throw SomethingBad


Example: interacting with a file.
Let's consider the simplest form, suitable for scripting tasks and other things where you don't want to think too much:

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

In first, we represent failure via return values. E.g. if the file doesn't exist, ``openFile`` returns ``NoSuchThing`` rather than a file handle, i.e. ``openFile "nonexistent"`` reduces to ``\x -> x NoSuchThing`` (``return NoSuchThing``), type ``((Handle|Error) -> Task) -> Task``. In second, the task instead reduces to an exception (a non-task value), i.e. ``openFile "nonexistent"`` reduces to ``NoSuchThing``, type ``(Handle -> Task) -> TaskE where TaskE = Exception | Task { Task = TaskE }``.

So the programs look like ``... >>= \cont -> readAllBytes NoSuchThing cont`` versus ``... >>= \cont -> NoSuchThing (\handle -> readAllBytes handle cont)``. With strict semantics both of these reduce to ``\cont -> NoSuchThing``.

With the second we need ``try`` to walk through the ``Task`` structure until it hits an exception or the end of the continuation. So the first is nicer. But the second might be useful for tasks that don't return a value and fail rarely.

The two approaches can be stacked; ``openFile`` could return a ``Symlink`` value as well as reducing to a ``NoSuchThing`` exception. Then ``try`` will return ``Result Symlink`` but a ``Error NoSuchThing``. But this is an abomination.

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

Precise signatures (discussed in the next section) offer a similar guarantee - the throwing function must be annotated to say it can throw, and the catching function must be annotated to say it can catch. But unlike ``try``, with precise signatures for a call chain ``A-B-C`` the intervening ``B`` does not need any modification when ``C`` starts throwing exceptions, and the modifications must still be made even if ``C`` already throws exceptions.

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

The ``lower`` style of signature doesn't require any synonyms because exceptions can be omitted from the signatures. This is the most efficient in terms of productivity because the code requires no extra work for exception changes. If a user wants to document that some exceptions are thrown they can add them to the signature. But it isn't required, and it adds extra work later if you want to stop throwing the exception.

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

Function types which cannot generate exceptions are subtypes of function types which can. To use higher order functions like map, polymorphic types like ``forall a b. (a -> b) -> t a -> t b`` should allow ``a`` and ``b`` to contain exceptions. To reason about this properly ``upper`` or ``precise`` types are needed. There doesn't seem to be any downside to deferring exception propagation and treating exceptions as values when applying higher-order functions.

q: how do lower/upper/precise types work in functions

It is quite useful for optimization to know that functions cannot generate exceptions. This can be achieved manually via overloading or the compiler can specialize code.

Higher-order exception handling constructs are possible, but in practice most Haskell code seems to stick with catch or try. The only really complex construct is ``bracket`` but that's handled in Stroscot with finalizers.

Sets allow unions, e.g. you can express throwing ``MyException`` or ``HisException`` as ``MyException|HisException``. This makes combining libraries and their exception types fairly straightforward. But this is impossible in many languages. The common solution is to use an unstructured catch-all type such as Exception.

It's not clear how useful the ability to define subsets of exceptions is - Java only has a few superclasses such as IOException and ReflectiveOperationException. Swift claims reacting to an arbitrary IOException is difficult. But IOExceptions are exceptions from I/O operations, so they can use an operation failure path that for example retries the operation a couple times, while Exceptions are so general that retrying may not make sense.

A similar pattern uses Text instead of Exception. This avoids the need for a real exception type, but means all exceptions are unstructured and can't be handled appropriately.

It's better to use constraints, ``(Throws m MyException, Throws m HisException) => String -> m Int``. Then the existential quantification limits the exceptions to those listed, because it's not necessarily an IO monad.

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
----------------

Unwinding tables are another stack-based implementation of continuations. The instruction pointer is used to find the loaded segment and its corresponding unwinding table. Every non-leaf function has an entry, and the return address on the stack is looked up to jump into handling code or code to pop the stack frame and restore register invariants. Since the tables can be cold and there's no branches on the non-exception path, the non-exception path is quite fast. But it's not zero cost because the exception path may keep registers alive that the non-exception path doesn't use, the code to read the tables takes up some space, and there are relocations at load time for the tables themselves.

Unwinding tables both help and harm performance. They require lots of data for the runtime support to unwind stacks, search for handlers, and so on. And compared to exception values these tables are slow to look up for the exception path. But if you have many levels of unwinding the normal path is faster because the values aren't wrapped and the handlers are cold and aren't dirtying your instruction cache or TLB.

The table-based system ended up being roughly 7% smaller and 4% faster (geomean) than the return code-based on some key benchmarks.

Another optimization is to cache exceptions as static data, so that throw doesn't allocate.

stack overflow in SEH filter clauses can be interpreted as not handling the exception.

Microsoft's implementation of C++ exceptions allocates on the stack, and delays deallocation until the end of a C++ catch clause. It is quite inefficient in terms of stack usage.

Zero overhead
-------------

Just turning on exception handling support in a C++ project previously compiled without exception support, not throwing any exceptions at all, gives +15-52% bloat. The overhead arises from jump tables, additional
stack space per thread (e.g., a 1K reservation, to save a dynamic allocation) and additional thread-local storage. For this reason many C+ projects disable exceptions.

With a flexible implementation it should be possible to optimize exceptions so that it doesn't cost anything to not use exceptions.

exception propagation for common exceptions should be handled by exception codes.
It would be good to bias the implementation somewhat towards the
non-exception path, perhaps by moving exception paths to the ends of functions
and cleanups out of the hot code path, but not enough to seriously compromise performance.
It should not use table-based unwinding except for really rare exceptions.

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
* Windows 10 RS5 adds "Special User APCs" (QUEUE_USER_APC_FLAGS_SPECIAL_USER_APC) which are delivered quickly via a kernel-mode APC, but then run as a user APC. The kernel-level API passes in a CONTEXT argument containing the registers from before the APC, like Linux's signal handler, but the documented API doesn't have this info. It may be possible to get it somehow with the documented API, or can live dangerously and use the kernel-level API.

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
