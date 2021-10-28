Imperative programming
######################

Tasks
=====

Haskell uses a state monad ``s -> (# s, a #))`` for implementing I/O, where ``s = State# RealWorld`` is a special zero-sized token type. It seems awkward, and is not really a safe abstraction due ``unsafePerformIO`` that constructs state tokens out of thin air. We compile pure functions to imperative programs, not the reverse.

Tasks are a more direct approach to I/O - sequences of I/O operations are values of type ``Task``, similar to a `free monad <https://www.reddit.com/r/haskell/comments/swffy/why_do_we_not_define_io_as_a_free_monad/>`__. Statements that don't return are directly of the Task type, like ``Exit { code : Int}``. Statements that continue in a sequential fashion have a ``continuation`` argument, like ``Print { s : String, continuation : Task }``, so are of type ``Command = Task -> Task``. Statements that return a value use a continuation of type ``a -> Task``, e.g. ``ReadFile { path : Fd, continuation : String -> Task}``, so are of type ``Operation a = (a -> Task) -> Task``. And since tasks are values we can also use them as arguments, like the ``delayed_task`` in ``SetTimeout { delay : Int, delayed_task : Task, continuation : Task}``.

So conceptually the "Hello World" program is simply the value ``Print "Hello World" (Exit 0)``. Except print isn't a primitive operation, it's more like:

::

  Data "Hello, world!\n" (\msg ->
    Block "_start" [Sys_write stdout (addr msg) (length msg) (Sys_exit 0)])

with Stroscot's internal assembler language.

Task isn't really a monad, but we can compose operations that return values using the continuation monad's bind operation.

The datatype is similar to the "fudgets" mentioned in :cite:`erkokValueRecursionMonadic2002`, except we don't have a pure constructor. Or `this <http://comonad.com/reader/2011/free-monads-for-less-3/>`__ type ``FFI o i``, but with control flow represented explicitly instead of using ``o`` or ``i`` parameters.

Continuations
=============

Why does Stroscot use continuations for its I/O model?

First of all, continuations are simple. They're the supercharged typed equivalent of a goto. A continuation is a function that takes as argument "the rest of the program", or "its future". Executing a continuation fills in a skeleton program with this future - or it can discard the future if it is not relevant. The implementation can compile continuations to jumps under most circumstances and closures otherwise, so the execution model is also conceptually simple.

Second, they're universal. Continuations are the basis in formal denotational semantics for all control flow, from goto statements to exception handling, subsuming vanilla call flow, recursion, generators, coroutines,
backtracking, and even loops along the way. They are "the mother of all monads" as all other monads can be encoded in the continuation type. This allows a uniform and consistent interface.

Concurrency
===========

The general idea with concurrency is there are multiple threads of execution, each thread composed of (imperative) operations, and the combination of various operations may have various semantics.

The smallest examples runtimewise just have memory access. For example this program SB: :cite:`sewellX86TSORigorousUsable2010`

::

  A = mem 0
  B = mem 0
  t1 = fork {A := 1; return B}
  t2 = fork {B := 1; return A}
  (x,u) = join (t1, t2)

Here the threads are provided by fork/join. In this case, the tasks use load/store instructions, but there could be anything.

At an API level, ``fork`` takes a ``Task`` but allowing an extra "finish execution gracefully" value ``Die``. In the above example we've used an overloading of ``fork`` for ``Operation a`` using an IVar:

::

  fork t =
    i = emptyIVar
    fork { a = t; writeIVar i a; Die }
    i

  join (x:xs) = { v = readIVar x; vs = join xs; return (v :vs) }

IVar is blocking so with mfix it's possible to obtain a deadlock.

We also want forkOn, for specifying the affinity (CPU / processor / whatever).

This program has a race condition - on x86 the output may be any combination of 0's and 1's. On a low level, race conditions are fine and an expected part of concurrent programming. No undefined behavior here. But on a program level Stroscot simulates the program's (concurrent) execution, and will give a warning if it's not consistent.

Concurrency allows various synchronization mechanisms (discussed in the :ref:`standard library <concurrency-library>`) that block running a task until a given condition is met or communicate information between tasks. We only get the possibility of deadlock when we use blocking primitives. With wait-free data structures we never need to block.

Scheduler
=========

Somewhere we have to use OS threads. The behavior of the OS scheduler is complicated and hard to abstract.

What we want is an I/O model that allows writing web servers to handle as many clients as possible simultaneously.

Choices:
blocking I/O and one client per OS thread (read/write) - requires stack for each thread
blocking I/O with userspace fibers (M:N threading model) - complex. A M:N scheduler must have a userspace component. This doubles the icache/dcache footprint. Fairness, nice, SMP balancing, memory allocation, TLS, RT scheduling, preemption, tid, debugging, security in userspace is hard - little control and only indirect access to statistics. Kernel events must trigger upcalls to activate context switches in the userspace component, but e.g. for memory there is no way to do async and a kernel context is blocked. M:N makes userspace<->userspace context switching faster, but slows down kernel<->userspace interaction. And kernel interaction is by default cheap on modern systems. For RT scheduling you would have to either set the priority every context switch, eliminating any performance advantage, or fall back to 1:1 scheduling for those threads.

OTOH as Go shows (Go HTTP servers are reasonably fast), you can have cooperative coroutines with tiny userspace stacks. You start with one thread per processor (possibly bound so there's no CPU shuffling?) and have this pool of homogeneous threads run through a task queue. This does no context switching of any kind, the only overhead is that the queue is concurrent (Go has per-thread queues too to mitigate this).

nonblocking I/O and level-triggered readiness notification (select/poll/kqueue) - requires fd for each connection
nonblocking I/O and readiness change notification (kqueue, epoll, realtime signals)
asynchronous I/O and completion notification (AIO, io_uring, IOCP)
server code in kernel (kttpd, TUX Threaded linUX webserver)
Bring the TCP stack into userspace - netmap, Sandstorm






 Third is to depend on special features of the scheduler such as UMS to have one thread switch execution to another without stopping (discussed `here <https://www.youtube.com/watch?v=KXuZi9aeGTw>`__).


Stroscot uses a mixed cooperative/preemptive model. Context switching is only possible at specific yielding points, but every action visible to another thread is a yield point. So memory access is divided into shared and non-shared.

Of these the most relevant is the "current" or most recent state; most functions do not need the stream of history.

state is passed/returned in an implicit parameter / out parameter ``realWorld``. There is a special syntax inout for this.

 So we can start from the end of the program and compute a DAG of causality then run the program forward. Some things can be linearized because they are commutative, like allocating references, while others like I/O cannot and will cause errors if multiple states are used.


A mutex is a synchronization primitive that will make the thread go to sleep if it’s blocked. Mutexes are only useful if threads spend a significant amount of time sleeping. C++ std::mutex is a good cross-platform mutex.

Fairness guarantees progress for all threads. It excludes the situation where some threads keep on getting the lock and a loser thread is always just a bit too late and is left out for a very long time.

Adaptive mutexes use a spinlock for some number of iterations and fall back to a mutex. A spinlock should use "test and test-and-set" because the initial test avoids invalidating the cache line.

A ticket spinlock, FIFO fairness: when you enter lock() you read and increment "in" and you spin until the “out” variable has the same value. To unlock you increment "out".

FIFO/ticket mutex: no OS API to wake up the right sleeper. So every time that you increment you have to wake all sleepers and all but one will immediately go back to sleep. Lock Convoy, or realtime SCHED_FIFO.

The Linux scheduler might take an unreasonably long time to schedule you again even if every other thread is sleeping or calls yield().


FIO (FFI o i) o (i -> FIO a)

Stroscot's running model is based on an event loop with a task queue. Each loop iteration takes some arbitrary non-zero number of arbitrarily-chosen tasks off the queue and runs them in parallel. Tasks are requests to the scheduler,

The tasks operate on a shared state, so the semantics of satisfying the requests in parallel must be defined. We want to error when things clearly conflict. Samples:

* Variable: Two writes with different values conflict. But if only one task writes the variable or all writes are equal then no conflict.
* Mutex: Two acquires, mutex available, a winner is nondeterministically chosen to be scheduled. The loser is blocked on the mutex or scheduled in a failure branch if it was try_acquire. No mutex available, block.
* Append-style file writing: Conflicts if same file descriptor
* Exiting: conflicts with anything but an identical exit

Etc. It's a bit twisty to define, but it's easy to err on the side of erroring, so it should be maintainable.

The program is required to have the same result regardless of the order the tasks are run. This is checked by the verification system.

Consider a single-threaded Javascipt-esque event loop - it has a FIFO queue of tasks, and the loop pops off tasks and runs them one by one. By itself this is not really concurrent - it's `cooperative multitasking <https://en.wikipedia.org/wiki/Cooperative_multitasking>`__. We can use an asynchronous programming style or a monad to get a "thread" of execution, but this breaks down for tasks that finish and have no continuation.



Stroscot sees all programs as functional manipulations of immutable values. So a state or snapshot is a value. Conceptually a state could include a lot of things, including the state of the CPU, details of other running threads, the stock market, etc. - all as long as it is within the chronological past

But a program will only observe a portion of the entire universe, hence the state is localized to some world line where all the information has been collected.

Mutation creates a new snapshot from an old snapshot by adding, removing, or changing the values attached to various places. As the program runs it builds up a history of snapshots. A history from the beginning of the program to some point in time is an execution.

Stroscot is concurrent and parallel.


The implementation uses work stealing FIFO queues :cite:`leaJavaForkJoin2000`. In practice these queues combine pure and imperative/concurrent sparks, so there's one per processor and not much overhead besides cache pollution, the need to use memory locations instead of registers, and synchronization.

Then there are concurrent operations. These are just load/store and the various architecture-specific fence instructions.


For store buffering the outcome may be ``(1,1)``, ``(1,0)``, or ``(0,1)``. But under the relaxed memory model used by X86 (Total Store Order or TSO) ``(0,0)`` is also possible. But under any model values other than 0 or 1 are not possible.

Also possible is independent reads of independent writes (IRIW):

::

  {a = X; b = Y}
  {X := 1}
  {Y := 1}
  {c = Y; d = X}

Here the initial state is ``(X,Y)=(0,0)``, and the final state can be ``(a,b,c,d)=(1,0,1,0)``.

Conditionals are a little hard to schedule because you have to make sure both sides can be speculated or discard the untaken branch promptly.
The simplest and main model of parallelism is parallel reduction or dataflow. So one would write out the above graph as bindings like ``c11 = a11 * b11`` (one can always find an ordering, using topological sort) and then Stroscot would read it back into the DAG. More complicated is allowing functions, for example ``foldMap f g (x:xs) = g (f x) xs`` generates a DAG of f's and g's if the list layout is known. Even with general recursion it should still be possible to identify data dependencies and assign DAG cells to temporary values in some fashion.

Sparks run in separate threads, so they require synchronization. Also moving cores thrashes the cache. Hence we need a cost model: run long parallelizable computations in sparks, but keep short computations in the same spark. The GC has to be concurrent. But the values are all pure so mutation isn't a big issue, just avoid using in-place update that crosses spark boundaries.

join is a "block until task complete" operation. The synchronization is like a `barrier <https://hackage.haskell.org/package/extra-1.7.8/docs/Control-Concurrent-Extra.html#t:Barrier>`__ or `IVar <https://hackage.haskell.org/package/data-ivar-0.30/docs/Data-IVar.html>`__.




Before getting right into the gritty details about why I think we should think
about a path away from M:N scheduling, I'll go over the details of the
concurrency model we currently use.

Rust uses a user-mode scheduler to cooperatively schedule many tasks onto OS
threads. Due to the lack of preemption, tasks need to manually yield control
back to the scheduler. Performing I/O with the standard library will block the
*task*, but yield control back to the scheduler until the I/O is completed.

The scheduler manages a thread pool where the unit of work is a task rather
than a queue of closures to be executed or data to be pass to a function. A
task consists of a stack, register context and task-local storage much like an
OS thread.

In the world of high-performance computing, this is a proven model for
maximizing throughput for CPU-bound tasks. By abandoning preemption, there's
zero overhead from context switches. For socket servers with only negligible
server-side computations the avoidance of context switching is a boon for
scalability and predictable performance.

# Lightweight?

Rust's tasks are often called *lightweight* but at least on Linux the only
optimization is the lack of preemption. Since segmented stacks have been
dropped, the resident/virtual memory usage will be identical.

# Spawning performance

An OS thread can actually spawn nearly as fast as a Rust task on a system with
one CPU. On a multi-core system, there's a high chance of the new thread being
spawned on a different CPU resulting in a performance loss.

Sample C program, if you need to see it to believe it:

```
#include <pthread.h>
#include <err.h>

static const size_t n_thread = 100000;

static void *foo(void *arg) {
    return arg;
}

int main(void) {
    for (size_t i = 0; i < n_thread; i++) {
        pthread_attr_t attr;
        if (pthread_attr_init(&attr) < 0) {
            return 1;
        }
        if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) < 0) {
            return 1;
        }
        pthread_t thread;
        if (pthread_create(&thread, &attr, foo, NULL) < 0) {
            return 1;
        }
    }
    pthread_exit(NULL);
}
```

Sample Rust program:

```
fn main() {
    for _ in range(0, 100000) {
        do spawn {
        }
    }
}
```

For both programs, I get around 0.9s consistently when pinned to a core. The
Rust version drops to 1.1s when not pinned and the OS thread one to about 2s.
It drops further when asked to allocate 8MiB stacks like C is doing, and will
drop more when it has to do `mmap` and `mprotect` calls like the pthread API.

# Asynchronous I/O

Rust's requirements for asynchronous I/O would be filled well by direct usage
of IOCP on Windows. However, Linux only has solid support for non-blocking
sockets because file operations usually just retrieve a result from cache and
do not truly have to block. This results in libuv being significantly slower
than blocking I/O for most common cases for the sake of scalable socket
servers.

On modern systems with flash memory, including mobile, there is a *consistent*
and relatively small worst-case latency for accessing data on the disk so
blocking is essentially a non-issue. Memory mapped I/O is also an incredibly
important feature for I/O performance, and there's almost no reason to use
traditional I/O on 64-bit. However, it's a no-go with M:N scheduling because
the page faults block the thread.

# Overview

Advantages:

* lack of preemptive/fair scheduling, leading to higher throughput
* very fast context switches to other tasks on the same scheduler thread

Disadvantages:

* lack of preemptive/fair scheduling (lower-level model)
* poor profiler/debugger support
* async I/O stack is much slower for the common case; for example stat is 35x
  slower when run in a loop for an mlocate-like utility
* true blocking code will still block a scheduler thread
* most existing libraries use blocking I/O and OS threads
* cannot directly use fast and easy to use linker-supported thread-local data
* many existing libraries rely on thread-local storage, so there's a need to be
  wary of hidden yields in Rust function calls and it's very difficult to
  expose a safe interface to these libraries
* every level of a CPU architecture adding registers needs explicit support
  from Rust, and it must be selected at runtime when not targeting a specific
  CPU (this is currently not done correctly)

# User-mode scheduling

Windows 7 introduced user-mode scheduling[1] to replace fibers on 64-bit.
Google implemented the same thing for Linux (perhaps even before Windows 7 was
released), and plans on pushing for it upstream.[2] The linked video does a
better job of covering this than I can.

User-mode scheduling provides a 1:1 threading model including full support for
normal thread-local data and existing debuggers/profilers. It can yield to the
scheduler on system calls and page faults. The operating system is responsible
for details like context switching, so a large maintenance/portability burden
is dealt with. It narrows down the above disadvantage list to just the point
about not having preemptive/fair scheduling and doesn't introduce any new ones.

I hope this is where concurrency is headed, and I hope Rust doesn't miss this
boat by concentrating too much on libuv. I think it would allow us to simply
drop support for pseudo-blocking I/O in the Go style and ignore asynchronous
I/O and non-blocking sockets in the standard library. It may be useful to have
the scheduler use them, but it wouldn't be essential.

[1] http://msdn.microsoft.com/en-us/library/windows/desktop/dd627187(v=vs.85).aspx
[2] http://www.youtube.com/watch?v=KXuZi9aeGTw

Then you schedule this graph on the processors by breaking it into "sparks" :cite:`trinderAlgorithmStrategyParallelism1998` and running the sparks in a compatible order.
The compiler should be able to do a pretty good job of separating computations into sparks. Plus breaking recursive functions into sparking and non-sparking versions is tedious.

But, as the compile target, we also want explicit parallelism.

As far as implementation, the basic implementation choices are atomic instructions on shared memory and OS-provided mutexes. Spinlocks are hard to use (`1 <https://matklad.github.io/2020/01/02/spinlocks-considered-harmful.html>`__ `2 <https://mjtsai.com/blog/2020/01/06/beware-spinlocks-in-user-space/>`__), they will waste power and the scheduler will run the busy wait a lot instead of doing real work. But `WebKit <https://webkit.org/blog/6161/locking-in-webkit/>`__ implements locks and condition variables using a byte-size reference and some global queues. There's still a spinning loop, the number of times to spin before giving up and parking should be optimized for each lock operation.

Memory model
------------

The Linux memory `model <https://github.com/torvalds/linux/blob/3d5c70329b910ab583673a33e3a615873c5d4115/tools/memory-model/linux-kernel.def>`__

In practice the synchronization primitives one can use are a combination of those provided by the scheduler and the atomic operations provided by the OS.

Various synchronization primitives:

* Linux kernel internal operations:
* `atomic x86 operations <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/x86/include/asm/atomic64_64.h>`__ `lock types <https://www.infradead.org/~mchehab/kernel_docs/locking/locktypes.html>`__
* atomic operations
* memory barrier
* threads:
  * the kernel scheduler has fairness, SMP balancing, RT scheduling, preemption, execution statistics, credentials, virtual memory, etc.
  * Userspace scheduler will always lose in functionality. Only has a performance advantage - kernel call is expensive. But so is cache miss - small memory footprint. Requires changes to debuggers, tracers.
* wait-free data types

MVar = Full value | Empty (Queue Process)

* Haskell's MVar
* goroutines, channels
* global variables
* shared memory
* message passing (queues)

All of these generate happens-before relationships on the various operations. We could track this with vector clocks, IDK why - the posets are easier to reason about directly.

The single-threaded event loop is a deterministic enough scheduling pattern that there are no race conditions, it's not really concurrent. But more complex schedulers easily create race conditions with shared memory.
the relaxed-consistency model allows implementing private memory that is then mapped back to shared on synchronization

Synchronization operations impose constraints on execution order. For example, acquiring a lock blocks until the lock is released. They introduce the problems of deadlock and starvation, which can be detected as the absence of progressing execution orders.

Parallelism
===========

Parallelism - the root is "parallel" or "happening at the same time". But with `relativity <https://en.wikipedia.org/wiki/Relativity_of_simultaneity>`__, simultaneity is not absolute. We instead consider `causal structure <https://en.wikipedia.org/wiki/Causal_structure>`__ - event separation can be timelike or spacelike. Timelike separation communicates information from past to future, while no dependency is possible with spacelike separation. Hence we define an execution as a directed graph of information flow, where a node is a value and an edge is read "can casually influence" (we could also use the reverse "reads data from"). Since there is no time travel the graph is acyclic and its transitive closure forms a partial order or poset. Then things happen "in parallel" if neither causally influences the other.

For example, `multiplying <https://en.wikipedia.org/wiki/Matrix_multiplication_algorithm#Parallel_and_distributed_algorithms>`__ two 2x2 matrices:

.. image:: _static/matrix-multiply.svg

The multiplications all happen in parallel and the additions in parallel.

There's no explicit syntax for parallelism - pure computations have inherent parallelism. Writing it out looks like:

::

  multiply ([[a11 a12] [a21 a22]]) ([[b11 b12] [b21 b22]]) = [[t11 t12] [t21 t22]]
    where
        c11 = a11 * b11
        d11 = a12 * b21
        -- 6 more multiplications defining cNN and dNN
        t11 = c11 + d11
        -- 3 more additions defining tNN

Stroscot schedules the instructions to maximize instruction-level parallelism, where appropriate.

With large (>1000 width) matrices we might want to multiply sub-matrices on multiple threads. That requires concurrency, so is handled by writing the synchronization operations explicitly.  Stroscot doesn't parallelize on the thread level by default because automatically spawning threads would be surprising, and the choice of thread/scheduler/performance model (OS thread, green thread) influences what granularity to split up the computation at.

But still, for complex data science type computations we might want automatic parallelization. So a library can provide a DSL function ``parallelize`` to automatically rewrite pure computations to concurrent ones, implementing the "small on single thread, big splits into small" model. But the implementation won't necessarily support all of Stroscot, e.g. lambdas are hard to support in parallel.

OS Model
========

An application consists of one or more processes. A process, in the simplest terms, is an executing program.

A job object allows groups of processes to be managed as a unit. Job objects are namable, securable, sharable objects that control attributes of the processes associated with them. Operations performed on the job object affect all processes associated with the job object.

One or more threads run in the context of the process. A thread is the basic unit to which the operating system allocates processor time. A thread can execute any part of the process code, including parts currently being executed by another thread.

UMS threads are lightweight threads that applications schedule. An application can switch between UMS threads in user mode without involving the system scheduler and regain control of the processor if a UMS thread blocks in the kernel. Each UMS thread has its own thread context instead of sharing the thread context of a single thread. The ability to switch between threads in user mode makes UMS more efficient than thread pools for short-duration work items that require few system calls.

A fiber consists of a stack and a small storage space for registers. A fiber runs in the context of a thread and does not have its own thread context. Fiber switching is fewer OS calls than a full-on thread context switch, but in general, fibers do not provide advantages over a well-designed multithreaded application. However, using fibers can make it easier to port applications that were designed to schedule their own threads.

A task represents an asynchronous operation. A thread pool is a collection of worker threads that efficiently execute tasks on behalf of the application - each worker thread is locked to a core. Tasks are queued. They run in fibers which run in the thread pool, but are relatively lightweight compared to fibers. Tasks support waiting, cancellation, continuations, robust exception handling, detailed status, and custom scheduling.
