Concurrency
###########

The general idea with concurrency is there are multiple threads of execution, each thread composed of (imperative) operations, and the combination of various operations may have various semantics. Normally we run in an OS thread and use a combination of hardware and OS operations. Working in the cloud, we still run in an OS thread, but the operations use the networking stack. In an embedded environment each thread is bound to a core.
We only get the possibility of deadlock when we use blocking operations. With wait-free / atomic operations we never need to block.

The smallest examples runtimewise just have memory access. For example this program SB: :cite:`sewellX86TSORigorousUsable2010`

::

  x = mem 0
  u = mem 0
  A = mem 0
  B = mem 0
  t1 = fork {A := 1; x := !(read B) }
  t2 = fork {B := 1; u := !(read A) }
  join (t1, t2)
  print (!(read x), !(read u))

Here the threads are provided by the C stdlib's pthreads, and the operations are hardware load/store instructions.
This program has a race condition - the outcome may be ``(1,1)``, ``(1,0)``, or ``(0,1)`` under sequential consistency. But under the relaxed memory model used by X86 (Total Store Order or TSO) ``(0,0)`` is also possible. But under any model values other than 0 or 1 are not possible.

Another example is independent reads of independent writes (IRIW):

::

  {a = X; b = Y}
  {X := 1}
  {Y := 1}
  {c = Y; d = X}

Here the initial state is ``(X,Y)=(0,0)``, and the final state can be ``(a,b,c,d)=(1,0,1,0)`` under POWER. But both ARMv8 and x86 forbid this outcome.

Simulation
==========

On a low level, race conditions are fine and an expected part of concurrent programming. No undefined behavior here. But on a program level Stroscot simulates the program's (concurrent) execution, and will give a warning if it's not consistent.
The program is required to have the same result regardless of the order the tasks are run. This is checked by the verification system. Basically the simulation maintains a list of each thread and its top-level Task value. Each loop iteration takes some arbitrary non-zero number of arbitrarily-chosen tasks and runs their operations in parallel. The tasks operate on a shared state, so the semantics of satisfying the requests in parallel must be defined. We want to error when things clearly conflict.

Samples:

* Variable: Two writes with different values conflict. But if only one task writes the variable or all writes are equal then no conflict.
* Mutex: Two acquires, mutex available, a winner is nondeterministically chosen to be scheduled. The loser is blocked on the mutex or scheduled in a failure branch if it was try_acquire. No mutex available, block.
* Append-style file writing: Conflicts if same file descriptor
* Exiting: conflicts with anything but an identical exit (clean exit requirement), or else no conflicts

Etc. It's a bit lengthy to simulate the entire task interface, but operations change infrequently, so it should be maintainable.

Acquiring a lock blocks until the lock is released. This introduces the problems of deadlock and starvation, which can be detected as the absence of progressing execution orders.

All of these generate happens-before relationships on the various operations. We could track this with vector clocks, IDK why - the posets are easier to reason about directly.

The verification system handles the nondeterminism somehow, check out papers on concurrency verification. The behavior of the OS scheduler is complicated and hard to model except as an adversary. The Linux scheduler might take an unreasonably long time to schedule a particular thread even if every other thread is sleeping or calls yield. Or it might decide to run it immediately, or move it on another core, etc.

Parallelism
===========

Parallelism - the root is "parallel" or "happening at the same time". But with `relativity <https://en.wikipedia.org/wiki/Relativity_of_simultaneity>`__, simultaneity is not absolute. We instead consider `causal structure <https://en.wikipedia.org/wiki/Causal_structure>`__ - event separation can be timelike or spacelike. Timelike separation communicates information from past to future, while no dependency is possible with spacelike separation. Hence we define an execution as a directed graph of information flow, where a node is a value and an edge is read "can casually influence" (we could also use the reverse "reads data from"). Assuming no time travel the graph is acyclic and its transitive closure forms a partial order or poset. Then things happen "in parallel" if neither causally influences the other.

For example, `multiplying <https://en.wikipedia.org/wiki/Matrix_multiplication_algorithm#Parallel_and_distributed_algorithms>`__ two 2x2 matrices:

.. image:: /_static/matrix-multiply.svg

The multiplications all happen in parallel and the additions in parallel.

There's no explicit syntax for parallelism - pure computations have inherent parallelism. Writing it out looks like:

::

  multiply a b =
    (m,n) = dim a
    (n' | n == n',o) = dim b
    for [1..m] $ \i ->
      for [1..o] $ \j ->
        sum [ (a !! (i,k)) * (b !! (k,j)) | k <- [1 .. n] ]

``for`` and ``sum`` can evaluate arguments in parallel. More complicated is allowing functions, for example ``foldMap f g (x:xs) = g (f x) (foldMap f g xs)`` generates a DAG of f's and g's if the list spine is known. Even with general recursion it should still be possible to identify data dependencies and assign DAG cells to temporary values in some fashion. Conditionals are a little hard to schedule because you have to make sure both sides can be speculated or discard the untaken branch promptly.

Stroscot schedules the instructions to maximize instruction-level parallelism, where appropriate. This takes advantage of the design of modern CPUs, where there are multiple "ports" and each port can execute an instruction simultaneously.

With large (>1000 width) matrices we might want to multiply sub-matrices on multiple threads (cores). That requires concurrency, so is handled by writing the synchronization operations explicitly.  Stroscot doesn't parallelize on the thread level by default because automatically spawning threads would be surprising, and the choice of thread/scheduler/performance model (OpenMP, OS thread, green thread) influences what granularity to split up the computation at.

But still, for complex data science computations we might want automatic parallelization that takes advantage of multicore hardware. So we can provide a DSL function ``parallelize`` to automatically rewrite pure computations to concurrent ones, implementing the "small on single thread, big splits into small" operations on top of fork/join model and taking the thread / task queue implementation as a parameter. Doug Lea's work stealing task queues can be very efficient given the correct task granularity.

Haskell's "par" is interesting, but too fine-grained to be efficient. You have to manually add in a depth threshold and manually optimize it. It's just as clear to use explicit fork/join operations, and indeed the ``rpar/rpar/rseq/rseq`` pattern proposed in `the Parallel Haskell book <https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch02.html>`__ is just fork/join with different naming.

As far as the actual task granularity, Cliff Click says the break-even point is somewhere around the middle of the microsecond range, thousands of cycles / machine code instructions. Below that the overhead for forking the task exceeds the speedup from parallelism, but above you can make useful progress in another thread.

OS Model
========

An application consists of one or more processes. A process, in the simplest terms, is an executing program.

A job object allows groups of processes to be managed as a unit. Job objects are namable, securable, sharable objects that control attributes of the processes associated with them. Operations performed on the job object affect all processes associated with the job object.

One or more threads run in the context of the process. A thread is the basic unit to which the operating system allocates processor time. A thread can execute any part of the process code, including parts currently being executed by another thread.

Windows has a special thread type "UMS thread" which has more application control. An application can switch between UMS threads in user mode without involving the system scheduler and regain control of the processor if a UMS thread blocks in the kernel. Each UMS thread has its own thread context. The ability to switch between threads in user mode makes UMS more efficient than thread pools for short-duration work items that require few system calls.

A fiber (green thread, virtual thread, goroutine) consists of a stack, saved registers, and fiber local storage. A fiber runs in the context of a thread and shares the thread context with other fibers. Fiber switching is fewer OS calls than a full thread context switch. When fibers are integrated into the runtime they can be more memory efficient than threads. Per Microsoft, fibers in C do not provide many advantages over threads.

