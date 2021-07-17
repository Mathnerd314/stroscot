Imperative programming
######################

Tasks
=====

Haskell uses a state monad ``s -> (# s, a #))`` for implementing I/O, where ``s = State# RealWorld`` is a special zero-sized token type. It seems awkward, and is not really a safe abstraction due ``unsafePerformIO`` that constructs state tokens out of thin air. We compile pure functions to imperative programs, not the reverse.

Tasks are a more direct approach to I/O - sequences of I/O operations are values of type ``Task``, similar to a `free monad <https://www.reddit.com/r/haskell/comments/swffy/why_do_we_not_define_io_as_a_free_monad/>`__. Statements that don't return are directly of the Task type, like ``Exit { code : Int}``. Statements that continue in a sequential fashion have a ``continuation`` argument, like ``Print { s : String, continuation : Task }``, so are of type ``Op = Task -> Task``. Statements that return a value use a continuation of type ``a -> Task``, e.g. ``ReadFile { path : Fd, continuation : String -> Task}``, so are of type ``OpV a = (a -> Task) -> Task``. And since tasks are values we can also use them as arguments, like the ``delayed_task`` in ``SetTimeout { delay : Int, delayed_task : Task, continuation : Task}``.

So conceptually the "Hello World" program is simply the value ``Print "Hello World" (Exit 0)``. Except print isn't a primitive operation, it's more like:

::

  Data "Hello, world!\n" (\msg ->
    Block "_start" [Sys_write stdout (addr msg) (length msg) (Sys_exit 0)])

with Stroscot's internal assembler language.

Task isn't really a monad, but we can compose operations that return values using the continuation monad's bind operation.

The datatype is similar to the "fudgets" mentioned in :cite:`erkokValueRecursionMonadic2002`, except we don't have a pure constructor. Or `this <http://comonad.com/reader/2011/free-monads-for-less-3/>`__ type ``FFI o i``, but with control flow represented explicitly instead of using ``o`` or ``i`` parameters.

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

So in Stroscot concurrency is impure fork/join. In this case, the impurity is load/store instructions, but there could be anything.

This program has a race condition - on x86 the output may be any combination of 0's and 1's. On a low level race conditions are fine and an expected part of concurrent programming. No undefined behavior here. But on a program level Stroscot simulates the program's (concurrent) execution, and will give a warning if it's not consistent.

Concurrency allows various synchronization mechanisms (discussed in the :ref:`standard library <concurrency-library>`) that block running a task until a given condition is met or communicate information between tasks. We only get the possibility of deadlock when we use blocking primitives. With wait-free data structures we never need to block.

At an API level, ``fork`` takes a ``Task`` but allowing an extra "finish execution gracefully" value ``Die``. In the above example we've used an overloading of ``fork`` for ``OpV a`` using an IVar:

::

  fork t =
    i = emptyIVar
    fork { a = t; writeIVar i a; Die }
    i

  join (x:xs) = { v = readIVar x; vs = join xs; return (v :vs) }


IVar is blocking so with a cyclic program it's possible to obtain a deadlock.

We also want forkOn, for specifying the affinity (CPU / processor / whatever).

Scheduler
=========

Somewhere we have to use OS threads. The behavior of the OS scheduler is complicated and hard to abstract.

http://www.kegel.com/c10k.html#1:1 says to use 1:1 threading. OS threads have tid, TLS, nice, and works with existing code/tools. But threads also use relatively expensive stacks. And there is no way to have one thread switch execution to another without stopping (discussed `here <https://www.youtube.com/watch?v=KXuZi9aeGTw>`__), you have to depend on the scheduler. Whereas when we have one thread per processor and they're bound, there's no CPU shuffling, and running through a task queue does no context switching of any kind, the only overhead is that the queue is concurrent.


Stroscot uses a mixed cooperative/preemptive model. Context switching is only possible at specific yielding points, but every action visible to another thread is a yield point. So memory access is divided into shared and non-shared.

Of these the most relevant is the "current" or most recent state; most functions do not need the stream of history.

state is passed/returned in an implicit parameter / out parameter ``realWorld``. There is a special syntax inout for this.

 So we can start from the end of the program and compute a DAG of causality then run the program forward. Some things can be linearized because they are commutative, like allocating references, while others like I/O cannot and will cause errors if multiple states are used.


most mutex implementations are really good
most spinlock implementations are pretty bad
the Linux scheduler is OK but far from ideal. The most popular replacement, the MuQSS scheduler has other problems instead.
the Windows scheduler is pretty good

in practice the OS controls the scheduler, not us.

benchmark in which there is nothing else to do except fight over the lock show spinlocks winning, because if you go to sleep, all that will happen is that some other thread will wake up that will fight over the same lock. There really is no benefit of ever going to sleep, besides power consumption.


And finally, none of those benchmarks measure the problem that I ran into: A thread that took a crazy long time to acquire a lock. These benchmarks measure throughput, not latency. (to be fair I also wouldn’t have thought to measure latency until it became a problem)
Defining Mutexes and Spinlocks

But before we move on, I have already gone on for too long without describing the terms. When I say “mutex” I mean a synchronization primitive that will make the thread go to sleep if it’s blocked. Spinlocks do not go to sleep if they don’t acquire the lock, they just try again. There are also “adaptive mutexes” which will try to spin for a while and if they’re not successful, they’ll go to sleep to let other threads run. And finally all of these have “fair” versions which guarantee progress for all threads. Without fairness, if you have two threads, A and B, you could in theory get a situation where A keeps on getting the lock and B is always just a bit too late. This is never a problem with two threads, but the more threads you have, the more likely you are to get those cases where one thread is left out for a very long time.

If you need a mutex in C++, std::mutex is actually really good on all the platforms that I know of. If you need a spinlock, you might be tempted to use std::atomic_flag, which looks like it’s perfectly intended for writing a spinlock (and that’s what my spinlock was using when we encountered the problem that started this investigation) but there is a subtle problem with atomic_flag. I learned about the problem by reading the slides of an AMD presentation at this years Game Developer Conference. On page 46 of those slides we find that you should prefer “test and test-and-set” over just test-and-set. To illustrate what that means, let me first implement a terrible spinlock using std::atomic_flag:

struct terrible_spinlock
{
    void lock()
    {
        while (locked.test_and_set(std::memory_order_acquire))
        {
        }
    }
    void unlock()
    {
        locked.clear(std::memory_order_release);
    }

private:
    std::atomic_flag locked = ATOMIC_FLAG_INIT;
};

To lock we call test_and_set which will return the old state. If it was false, nobody else has called test_and_set yet. If it was true, somebody else has already called this and we’ll have to try again. In unlock we just clear the flag again. The memory orderings tell the compiler and the CPU about which re-orderings are allowed. They are very important to avoid memory barriers and to keep the fast path (when the lock is free to take) fast, but they’re a big topic in themselves and will not be explained in this blog post.

So why is this terrible? The first reason is that while we’re spinning like this, we appear to the CPU and to the OS like a very busy thread and we will never be moved out of the way. So if the thread that has the lock is not currently running, we could be blocking it from running, causing it to not give up the lock.

But even if we added a way to indicate that other threads should take priority (like _mm_pause() or std::this_thread::yield()) we have a second problem that has to do with the ownership of cache lines on multi-core processors. To ensure correctness when multiple processors are writing to the same memory, there are protocols for which processor is allowed to write. All current processor use some variant of the MESI protocol, in which you have to force the “invalid” state on all other cores when modifying a cache line. That means that if multiple cores are banging on the spinlock above, they keep on fighting over who gets ownership and keep on invalidating the cache line for others. This leads to lots of coordination work and transferring of memory between the CPU caches, slowing down the whole system. In the AMD slides above they have a benchmark where one thread is doing a bunch of matrix multiplications, and fifteen threads are banging on a spinlock. On a terrible spinlock like this the matrix multiplications became ten times slower, even though they’re not even touching the same cache line. Completely unrelated work slows down.

So here is a spinlock written according to the AMD recommendations:

struct spinlock_amd
{
    void lock()
    {
        for (;;)
        {
            bool was_locked = locked.load(std::memory_order_relaxed);
            if (!was_locked && locked.compare_exchange_weak(was_locked, true, std::memory_order_acquire))
                break;
            _mm_pause();
        }
    }
    void unlock()
    {
        locked.store(false, std::memory_order_release);
    }

private:
    std::atomic locked{false};
};

This fixes both of the issues above. (but we’ll still make one more improvement further down) It solves the first problem by calling _mm_pause() which the CPU uses as a hint that the other hyper-thread running on the same core should run instead of this thread, and it solves the second problem by loading the memory before attempting to make a change to it. In the MESI protocol this means that the cache line can be in the “shared” state on all cores which requires no communication between the CPU cores until the data actually changes.

So now that we have a spinlock that’s not terrible lets try benchmarking this.
Measuring Latency

It’s always difficult to try to reproduce one-off occurrences like the one that started my investigation. Just because we saw a thread taking milliseconds to acquire a spinlock doesn’t mean that the problem was actually with the spinlock. Maybe something else was wrong. How do you even measure rare things like that? First off lets start with the simplest possible thing. On multiple threads run this loop:

for (size_t i = num_loops; i != 0; --i)
{
    auto time_before = std::chrono::high_resolution_clock::now();
    mutex.lock();
    auto wait_time = std::chrono::high_resolution_clock::now() - time_before;
    longest_wait = std::max(wait_time, longest_wait);
    mutex.unlock();
}

So we take a timestamp before we call lock and a timestamp after we have succeeded in locking the mutex. (or a spinlock. It’s a template) Then we remember the longest time it took. In my benchmark num_loops is 16384 (no particular significance to that number, just something that doesn’t run too fast or too slow) and I repeat the entire test 100 times to get more than one measurement. Then I take the four runs out of the hundred that had the longest waits and print how long the longest wait in that run was. The results are shocking:
Type 	Average test duration 	Four longest waits
std::mutex 	62 ms 	2.9 ms, 2.8 ms, 1.5 ms, 1.4 ms
terrible_spinlock 	825 ms 	103.5 ms, 90.6 ms, 77.1 ms, 75.7 ms
spinlock_amd 	68 ms 	62.3 ms, 61.5 ms, 60.9 ms, 59.8 ms

These are measurements for running the above test code on sixteen threads on a AMD Ryzen 7 1700. (which has eight cores, sixteen hyperthreads) The “average test duration” is the average time that it took to finish running the above loop in 16384 iterations on all cores. Based on that column we can confirm that using the terrible spinlock really does slow things down by a factor of 10, just like in the benchmark in the AMD presentation.

In the “longest wait” column we see huge wait times for the spinlocks. One of the threads in the spinlock_amd test had to wait for 62.3 ms when the whole test only took 68 ms on average. Meaning most of the other threads probably got to finish their loops entirely before it even got to run once.

There is one improvement we can easily make to the spinlock to help with the really bad latency cases, we can call std::this_thread::yield:

struct spinlock
{
    void lock()
    {
        for (int spin_count = 0; !try_lock(); ++spin_count)
        {
            if (spin_count ❬ 16)
                _mm_pause();
            else
            {
                std::this_thread::yield();
                spin_count = 0;
            }
        }
    }
    bool try_lock()
    {
        return !locked.load(std::memory_order_relaxed) && !locked.exchange(true, std::memory_order_acquire);
    }
    void unlock()
    {
        locked.store(false, std::memory_order_release);
    }

private:
    std::atomic❬bool❭ locked{false};
};

It’s roughly the same code as before except I replaced compare_exchange with exchange and every once in a while I call std::this_thread::yield(). Why every 16 spins? I tried a lot of different options and this one did well. The difference between _mm_pause() and yield() is that _mm_pause() is a hint for the CPU while yield() is a hint for the OS. In theory I shouldn’t need to hint anything to the OS here since I’m running sixteen software threads on sixteen hardware threads, but in practice it helps a lot. The longest wait gets reduced to 11.4 ms.

One question you may have is that since yield() is a OS call, don’t we lose the benefits of spinlocks? Because if we’re going to call into the OS anyway, why not use a full mutex? The answer is that yield() is actually a very cheap call. On my machine it takes roughly 130 nanoseconds, in both Linux and Windows. (that is of course if nothing else needs to run. If something else needs to run then we’ll have to wait longer to come back) We can afford to lose 130 nanoseconds every once in a while to keep the simplicity of a spinlock.

In any case all of these are terrible results. The best thing we’ve seen so far, std::mutex, can still take 3ms to acquire the lock. I think it’s about time that we give these “fair” mutexes a look.

The simplest fair exclusion mechanism to write is a ticket spinlock. The idea is that when you enter lock() you take a ticket and you wait for your number to be called. To implement that you just need to increment numbers, so the implementation is pretty simple:

struct ticket_spinlock
{
    void lock()
    {
        unsigned my = in.fetch_add(1, std::memory_order_relaxed);
        for (int spin_count = 0; out.load(std::memory_order_acquire) != my; ++spin_count)
        {
            if (spin_count ❬ 16)
                _mm_pause();
            else
            {
                std::this_thread::yield();
                spin_count = 0;
            }
        }
    }
    void unlock()
    {
        out.store(out.load(std::memory_order_relaxed) + 1, std::memory_order_release);
    }

private:
    std::atomic❬unsigned❭ in{0};
    std::atomic❬unsigned❭ out{0};
};

When we enter we increment the “in” variable and then spin until the “out” variable has the same value. It looks more complicated than it is because of all the memory orderings and the spin count logic. Btw everyone gets the memory orderings on the unlock wrong. The load can be relaxed, and the increment does not have to be an atomic increment, but the store has to be a release. If you use an atomic fetch_add() here, the fast path (when the lock is free to take) will be twice as slow. Does the ticket_spinlock help? Here is the full table with all entries:
Type 	Average test duration 	Four longest waits
std::mutex 	62 ms 	2.9 ms, 2.8 ms, 1.5 ms, 1.4 ms
terrible_spinlock 	825 ms 	103.5 ms, 90.6 ms, 77.1 ms, 75.7 ms
spinlock_amd 	68 ms 	62.3 ms, 61.5 ms, 60.9 ms, 59.8 ms
spinlock 	69 ms 	11.4 ms, 10.8 ms, 10.4 ms, 9.9 ms
ticket_spinlock 	93 ms 	1.5 ms, 1.5 ms, 1.49 ms, 1.48 ms

The ticket spinlock is worse on throughput, taking almost 50% longer to finish the test, but the wait time is pretty consistent. Where does that 1.5ms come from? I think it’s related to how long a scheduler time slice is on Linux, because it makes sense that the biggest outlier would be the time that it takes to get scheduled again. (remember these are outliers, the average wait is much shorter) We’ll see below that Windows does much better, because with the ticket_spinlock, when it’s your time to get the lock, there is no way that it takes 1.5 ms for all the other threads to go through that tiny critical section, so we are mostly measuring scheduler overhead here.

Oh and if you want a fair mutex algorithm, (as opposed to a fair spinlock) they get much more complicated. You might be tempted to turn the ticket_spinlock into a ticket_mutex by using a futex, and this does work (it’s shown in this talk) but it’s not ideal since you have no way of waking up the right sleeper. So every time that you increment you have to wake all sleepers and all but one will immediately go back to sleep. So a good fair mutex will usually involve some kind of linked list built on the stack of the sleeping threads, but that is hard to do, especially since you don’t want to be 100% fair because that really slows you down. (it’s called a Lock Convoy) It’s too much for this blog post to cover.
Measuring Idle Time

You may have noticed that I’m not actually measuring the exact situation that we had at work. I said that a spinlock was free to take but it still took several milliseconds to be acquired. That’s not what I’m measuring above. When I have a large delay of acquiring the spinlock in the above, that is probably because some other thread keeps on winning and keeps on making progress. So depending on how you spread the work between the threads, this might not be a problem.

So how would we measure the situation I had at work? What I really want to measure is if nobody has the lock and somebody wants to take the lock, and it’s not being taken. It’s a bit harder to measure, but here is an attempt:

for (size_t i = num_loops; i != 0; --i)
{
    mutex.lock();
    auto wait_time = std::chrono::high_resolution_clock::now() - time_before;
    if (first)
        first = false;
    else if (wait_time ❭ longest_idle)
        longest_idle = wait_time;
    time_before = std::chrono::high_resolution_clock::now();
    mutex.unlock();
}

So I switched the order around. Instead of saving the timestamp before taking the lock, I save the timestamp just before releasing the lock. Then the next thread entering can measure how much time has passed since the last thread gave up the lock. The check for “if (first)” is necessary because in the first iteration of the loop the time wouldn’t have been set yet. Somebody has to give up the lock once for me to get a useful measurement. If there are long times where the lock is not being held even though a thread wants to take it, this would detect that.

Before showing the results I have to mention that finding long times here is more rare, so I ran the entire benchmark 1000 times instead of 100 times, as above. But then I found crazy outliers like the one I noticed at work right away:
Type 	Average test duration 	Four longest idle times
std::mutex 	86 ms 	0.8 ms, 0.28 ms, 0.26 ms, 0.25 ms
terrible_spinlock 	852 ms 	134.8 ms, 124.6 ms, 119.7 ms, 96.5 ms
spinlock_amd 	65 ms 	7.0 ms, 6.9 ms, 0.54 ms, 0.45 ms
spinlock 	66 ms 	1.4 ms, 1.2 ms, 0.33 ms, 0.32 ms
ticket_spinlock 	95 ms 	13.0 ms, 3.3 ms, 2.6 ms, 2.4 ms

So this table is different from the previous table in that it shows how long the mutex was not being held even though somebody was trying to enter. Meaning for the terrible spinlock there was literally a time where the spinlock wasn’t being held, somebody was trying to enter, and it took them 134 ms to do so. The other, better spinlock implementations do much better, but on all of them we see wait times of more than a millisecond. The spinlock that I had written at work was probably slightly worse than spinlock_amd in the table above, so no wonder we were seeing crazy long pauses. If I can reproduce these hitches in a couple of seconds in an artificial benchmark like this, of course you would see it if you profile a game for hitches for long enough. And we can also see why replacing the spinlock with a mutex solved the problem.

I have to say that I am really weirded out by the ticket_spinlock performing this badly. I can’t explain what’s happening there. Why would that one in particular spend several milliseconds leaving the lock idle? Does the OS keep on giving time to the wrong threads? Does it ignore my yield calls? I actually can’t explain what’s happening with any of the spinlock cases. What is going on that we are literally spending more than a millisecond not holding the lock even though there are threads that would want to enter? It’s time to finally look at the Windows scheduler, because it does much better here.
Windows Scheduler

Running the same benchmarks on the same machine in Windows, here are the results for the longest waits (the first table):
Type 	Average test duration 	Four longest waits
std::mutex 	60ms 	24.1 ms, 20.1 ms, 17.4 ms, 17.0 ms
terrible_spinlock 	168 ms 	32.2 ms, 30.0 ms, 27.3 ms, 26.1 ms
spinlock_amd 	67 ms 	61.1 ms, 57.8 ms, 57.0 ms, 56.5 ms
spinlock 	63 ms 	48.0 ms, 40.0 ms, 40.0 ms, 36.2 ms
ticket_spinlock 	95 ms 	0.78 ms, 0.34 ms, 0.27 ms, 0.2 ms

Immediately we get a different feel. std::mutex makes individual threads wait longer, but runs faster overall. terrible_spinlock does better for some reason, and ticket_spinlock performs very well: It only has one outlier that isn’t all that bad, otherwise it has the best wait times we’ve seen yet.

Where Windows really does better is in the idle times:
Type 	Average test duration 	Four longest idle times
std::mutex 	56 ms 	0.58 ms, 0.46 ms, 0.27 ms, 0.19 ms
terrible_spinlock 	188 ms 	21.1 ms, 20.9 ms, 18.2 ms, 18.0 ms
spinlock_amd 	68 ms 	17.3 ms, 6.6 ms, 5.0 ms, 4.9 ms
spinlock 	62 ms 	0.16 ms, 0.14 ms, 0.14 ms, 0.14 ms
ticket_spinlock 	94 ms 	0.38 ms, 0.35 ms, 0.26 ms, 0.25 ms

These numbers look much better. With std::mutex and with a properly written spinlock, the lock never sits idle for long. This is exactly what you want and what you’d expect. spinlock_amd still has long times where the lock is free to take but nobody takes it. This just shows that you do need to call yield() while you’re spinning, even if you have one software thread per hardware thread. Otherwise the OS just might not schedule the right thread for some reason.
Alternative Linux scheduler

Really the Windows results just shows us that the Linux scheduler might take an unreasonably long time to schedule you again even if every other thread is sleeping or calls yield(). The Linux scheduler has been known to be problematic for a long time. A popular alternative scheduler was BFS, which among other things had this in it’s FAQ:

    For years we’ve been doing our workloads on linux to have more work than we had CPUs because we thought that the “jobservers” were limited in their ability to utilise the CPUs effectively (so we did make -j6 or more on a quad core machine for example). This scheduler proves that the jobservers weren’t at fault at all, because make -j4 on a quad core machine with BFS is faster than *any* choice of job numbers on CFS

BFS has since evolved into MuQSS, which is maintained by Con Kolivas here. In order to run this, I had to compile my own Linux kernel. I tried watching a video on the side and for most of the build that worked, but then at a certain point the build used all 16 cores to compress something and my video stuttered and the audio went bad and it just became totally unwatchable. Worse than anything I had ever experienced on Windows. Trying the same thing again with MuQSS I had no problems. The video kept on playing fine. So while subjectively it’s immediately better, let’s try running our benchmarks on it to see how it performs. First, here are the results for the longest wait time again:
Type 	Average test duration 	Four longest waits
std::mutex 	60 ms 	0.20 ms, 0.18 ms, 0.18 ms, 0.17 ms
terrible_spinlock 	813 ms 	94.4 ms, 8.6 ms, 8.6 ms, 7.1 ms
spinlock_amd 	72 ms 	63.9 ms, 63.8 ms, 59.7 ms, 57.3 ms
spinlock 	21 ms 	7.3 ms, 6.6 ms, 4.2 ms, 4.0 ms
ticket_spinlock 	2538 ms 	23.1 ms, 16.3 ms, 16.2 ms, 15.4 ms

And once again they look very different. ticket_spinlock fell off a cliff and runs super slowly. std::mutex now performs great (slightly faster than before) and has very short wait times. spinlock somehow got crazy fast. Apparently this code can run three times faster. Who knew that the scheduler can have that much of an impact? (if I run this benchmark single-threaded, it finishes in just under 1 ms. So on sixteen threads it needs to take at least 16ms. 21ms is pretty close to ideal)

Let’s also look at how this scheduler performs when the mutex is free to take and somebody wants to take it. How long does it sit idle?
Type 	Average test duration 	Four longest idle times
std::mutex 	73 ms 	0.15 ms, 0.11 ms, 0.10 ms, 0.09 ms
terrible_spinlock 	1433 ms 	94.8 ms, 85.1 ms, 83.6 ms, 72.1 ms
spinlock_amd 	67 ms 	4.8 ms, 4.7 ms, 4.0 ms, 2.9 ms
spinlock 	22 ms 	4.4 ms, 4.3 ms, 3.8 ms, 3.7 ms
ticket_spinlock 	2518 ms 	18.6 ms, 2.1 ms, 1.9 ms, 1.4ms

MuQSS does not do as well here as Windows. ticket_spinlock performs terribly again, worse than terrible_spinlock. On all the spinlocks we see long times where the lock just sat idle even though somebody was trying to acquire it. Only by using std::mutex can we ensure that we don’t get random long stalls. But when we do use std::mutex, we get much better results than with the default Linux scheduler.

So what conclusions can we draw from this? MuQSS is promising but it clearly has problems. It’s very impressive that the spinlock is suddenly three times faster than anything we saw with the other schedulers. But it’s a real problem that ticket_spinlock suddenly performs terrible. I am pretty sure that people use ticket_spinlocks in production.
SCHED_RR and SCHED_FIFO

When you create a thread on Linux you can say that you want to use a different scheduler. So I switched back to the normal Linux scheduler and tried creating threads with SCHED_RR and SCHED_FIFO. The first result is that your system slows to a crawl because those threads take priority over everything else. Even mouse movement gets choppy. But the idle times were much better. The results for both options were similar, so I’ll just show the results for SCHED_RR. First lets get the wait times out of the way:
Type 	Average test duration 	Four longest waits
std::mutex 	62 ms 	4.4 ms, 4.3 ms, 1.5 ms, 1.5 ms
terrible_spinlock 	848 ms 	10.3 ms, 4.9 ms, 3.3 ms, 3.0 ms
spinlock_amd 	73 ms 	67.1 ms, 66.6 ms, 66.0 ms, 64.0 ms
spinlock 	66 ms 	5.2 ms, 4.4 ms, 4.1 ms, 3.8 ms
ticket_spinlock 	97 ms 	9.0 ms, 0.28 ms, 0.16 ms, 0.14 ms

The results are actually pretty similar to the normal scheduler. The biggest difference is that ticket_spinlock has much shorter waits. (except for one huge outlier) But looking at the times that the mutex sat idle we can see a bigger difference:
Type 	Average test duration 	Four longest idle times
std::mutex 	76 ms 	0.13 ms, 0.12 ms, 0.12 ms, 0.12 ms
terrible_spinlock 	2043 ms 	40.6 ms, 4.2 ms, 1.2 ms, 0.76 ms
spinlock_amd 	70 ms 	0.13 ms, 0.09 ms, 0.09ms, 0.09 ms
spinlock 	62 ms 	0.23 ms, 0.19 ms, 0.16 ms, 0.15 ms
ticket_spinlock 	97 ms 	26.8 ms, 10.1 ms, 1.9 ms, 0.15 ms

Here the results look very different. The biggest idle times for std::mutex look better than they looked on Windows. ticket_spinlock looks bad because it has three big outliers, but otherwise it was actually really stable. spinlock_amd has the shortest idle times yet. If you don’t yield with this scheduler, the OS will let you run so the lock never sits idle.

So why isn’t this the default? The big problem is that these threads essentially run at a crazy high priority and you’re starving everything else on the system. I’ve heard stories of game developers trying to use SCHED_FIFO or SCHED_RR on Linux and while it seemed great for the game at first, they actually ran into problems because they were locking up other parts of the system, causing weird unexpected deadlocks for the game. They also ran into problems because not all of their threads needed the high priority thread options and those threads now never got to run. In the end it wasn’t worth it.
Further Work and Benchmark Code

I think this is a good point to end the blog post. There are many open questions remaining (How do adaptive mutexes perform? What makes a good mutex? What is the right way to benchmark mutex throughput?) but this blog post is already quite long and already covers a lot of different topics. Those other questions will either have to wait for a future blog post, or you’ll have to investigate them yourself with my benchmarking code, which is uploaded here. (it requires google benchmark)
Guidance

So what should we do based on the above? I hope to have convinced you to avoid spinlocks, even for very short sections. Spinlocks are only a benefit if you really really care about the fast path and are certain that the lock will essentially always be free to take. On my computer the fast path on a spinlock is roughly 7ns and on a mutex it’s roughly 14ns, so both of those numbers are very fast. But when spinlocks go bad, the scheduler can be really confused, at least on Linux. Maybe future scheduler improvements will change the results on this. It was very interesting to see that the spinlock was three times faster using the MuQSS scheduler.

But even taking that into account you get the problem that nothing else can run on your core. If you manage to use all your cores well using spinlocks, I would be very impressed. Chances are that if you want to fully utilize all your cores, you will eventually add work that can run “in the gaps” at low priority, and that’s only possible if you use mutexes, because you need to let the scheduler know what your gaps are. (of course this doesn’t apply if your work is very uniform, like you’re using 16 cores to compress a file or something like that. In that case there are no gaps to fill)

The other conclusion to take so far is that schedulers are an open problem. The Windows scheduler certainly does best on not keeping the lock idle (as long as you use std::mutex or a good spinlock) but the Linux schedulers have problems. This was known for a while simply because audio can stutter on Linux when all cores are busy (which doesn’t happen on Windows) but it’s good to have benchmarks for this. It is really bad that a lock can sit idle for milliseconds even though somebody is trying to get it. Now that game developers are slowly moving onto Linux, I predict that hiccups like that will go away within a couple years, as more work will be done on the scheduler.
Edit: The BMQ Scheduler

In the comments below an anonymous commenter (or maybe their name is actually “Anon”) asked me if I had tried the BMQ scheduler. I hadn’t heard of it because apparently it’s fairly new. (it was announced here) But it performed well enough that I wanted to modify the blog post instead of just writing a comment. Here are the worst wait times:
Type 	Average test duration 	Four longest waits
std::mutex 	63 ms 	1.5 ms, 1.0 ms, 0.9 ms, 0.7 ms
terrible_spinlock 	949 ms 	34.9 ms, 33.0 ms, 29.4 ms, 17.9 ms
spinlock_amd 	72 ms 	70.4 ms, 69.5 ms, 69.5 ms, 67.5 ms
spinlock 	63 ms 	3.7 ms, 3.6 ms, 3.6 ms, 3.5 ms
ticket_spinlock 	127 ms 	1.3 ms, 1.1 ms, 0.95 ms, 0.70 ms

This looks pretty good, but not great. (ticket_spinlock is noticeably slower than with the normal scheduler, but the worst wait times are better) Where it really shines though is in the times that the lock sits idle:
Type 	Average test duration 	Four longest idle times
std::mutex 	77 ms 	0.06 ms, 0.03 ms, 0.03 ms, 0.02 ms
terrible_spinlock 	1075 ms 	35.1 ms, 32.1 ms 19.8 ms, 17.3 ms
spinlock_amd 	72 ms 	4.0 ms, 4.0 ms, 4.0 ms, 4.0 ms
spinlock 	62 ms 	0.39 ms, 0.37 ms, 0.25 ms, 0.24 ms
ticket_spinlock 	141 ms 	12.3 ms, 12.0 ms, 1.8 ms, 1.8 ms

The row for std::mutex looks better than anything we’ve seen so far. If you use a mutex with this scheduler, the lock will never sit idle. It will always correctly schedule a thread that wants to take the lock. In the spinlock_amd row we must have hit a case where there is some magic constant at 4.0ms. There were a lot more waits of 4.0 ms in the data. (not all of the values were this big, it’s just that when there was an outlier it was often 4.0 ms) Spinlock also does great in not keeping the lock idle. Only ticket_spinlock does poorly for some reason.

So among the Linux schedulers I tested, this looks to be the best one, since we mostly care about std::mutex and spinlock, and it does best there. The only downside is that ticket_spinlock runs a bit slow. (and I have to admit I was hoping for a repeat of MuQSS running the spinlock three times faster, but it’s not a downside to not see that)

Cont (Cont r i) (FFI o i, o)

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

With large (>1000 width) matrices we might want to multiply sub-matrices on multiple threads. That requires concurrency, so is handled by writing the synchronization operations explicitly. You can use a DSL function ``parallelize`` to automatically rewrite pure computations to concurrent ones, implementing the "small on single thread, big splits in small" model. Stroscot doesn't parallelize on the thread level by default because automatically spawning threads would be surprising, and the choice of thread/scheduler/performance model (OS thread, green thread) influences what granularity to split up the computation at.
