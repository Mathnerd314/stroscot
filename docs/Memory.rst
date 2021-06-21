Memory management
#################

Programming in Stroscot by default sees a mathematical view of data: all data is fixed once created, and there is infinite storage space. And with the availability of cloud storage this model is pretty much accurate: global storage capacity is measured in zettabytes, and there is always a backup / history mechanism in place.

But the path from cloud to CPU is long, so there is a lot of caching in between:

Physical registers (0.3 ns): managed by the CPU
Logical registers (0.3 ns): assembly
Memory Ordering Buffers (MOB), L1/L2/L3 Cache (0.5-7 ns): Managed by the CPU
Main Memory (0.1us-4us): assembly
SSD (16us-62us): file APIs
LAN (0.5-500ms): network stack
HDD (3 ms): file APIs
WAN (150ms): network stack

for more advanced programming there is the need to avoid the use of slow storage mechanisms as much as possible by addressing the fast storage mechanisms directly.

The point of the memory system is to assign a storage location for every value, insert moves / frees where necessary, and overall minimize the amount of resources consumed.

User-level primitives
=====================

Pointers
--------

memory cell
  An integer ``i`` with ``0 <= i < MAX``. Since ternary computers `might <https://www.extremetech.com/computing/295424-back-off-binary-samsung-backed-researchers-debut-ternary-semiconductor>`__ eventually become popular, you should not assume that ``MAX`` is a power of 2, but for all practical purposes this will be some fixed number of logical bits (0/1).

Pointers are indices into a shared global sparse array of memory cells. It is in fact possible to implement the typical `sparse array operations <https://developer.android.com/reference/android/util/SparseArray>`__. There are functions to directly allocate memory at an address, `mmap <https://man7.org/linux/man-pages/man2/mmap.2.html>`__ with MAP_FIXED_NOREPLACE on Linux and `VirtualAlloc <https://docs.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc>`__ on Windows. Reading and writing are done directly in assembly. The list of currently mapped pages can be had from ``/proc/self/maps`` and `VirtualQueryEx <https://reverseengineering.stackexchange.com/questions/8297/proc-self-maps-equivalent-on-windows/8299>`__, although this has to be filtered to remove pages reserved by the kernel and internal pages allocated by the runtime, and looks slow - it's easier to wrap the allocation functions and maintain a separate list of user-level allocations. Clearing mappings, hashing memory, and indexing by mapped pages all work when restricted to the list of user pages.

In practice direct allocation is never used and instead there are ``mmap NULL`` and ``malloc`` which allocate memory with system-chosen location. This means that the program behavior must be observationally equivalent no matter what addresses the system picks. The limitations on the system's choice are that the allocation must be suitably aligned and disjoint from all unrevoked allocations. (The system can also return an out of memory error, but this doesn't have to result in equivalent behavior so it can be ignored.)

We also want to support interfacing with other languages, so we need a pair ``record_foreign`` / ``erase_foreign`` that functions similarly to direct allocation but doesn't actually call into the OS.

Eliminating pointer reads amounts to tracking down the matching pointer write, which can be accomplished by tracing control flow. Eliminating pointer writes requires proving that the address is never read before deallocation, which requires a global analysis of pointer reads. The analysis is complex as it has to deal with symbolic intervals but should be possible. We also want ``volatile_read`` / ``visible_write`` to prevent optimizing these away, e.g. for multithreaded situations with shared memory. Or maybe these should be the default and pointer operations should never be optimized.

Eliminating pointers entirely is not possible as they have to be used for system calls and interfacing with C. But we can minimize the lifetime of pointers in the standard library to the duration of the call, and use values / references everywhere else.

Pointers introduce a class of memory errors: use-after-free, double free, and memory leaks. But at this low level of operation it seems reasonable to expect programmers to worry about such errors.

References
----------

A reference is essentially a pointer but allocated using a specific Stroscot function and with deallocation managed by Stroscot.

There is no need for a read-only reference, because Stroscot supports cyclic data so it is simply the value directly.

A mutable array is simply a reference containing a pure array.


Since we work with datatypes first and their representations only incidentally, we do not have to handle buffer overflows; pointer arithmetic is implicit in the pack/unpack functions and due to our correctness properties, unpacking fields of the datatype must read within the allocated buffer.

Representation
==============

Layout is usually defined by its size, alignment, padding/stride, and field offsets, but this only specifies the representation of simple flat records. With enumerations, there is the question of how to encode constants. It gets even more complicated with ADTs, like JS's `value type <https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations>`__, and the choices often impact performance significantly. Finally there is the use of pointers. For example, we can encode a list in a number of ways:

::

  ["a","b"]
  # flat list, stored like [2,"a","b"] or [1,"a",1,"b",0]
  # intrusive list, stored like x=[1,"a",&y], y=[1,"b",&0]
  # uniform list, stored like x=[1,&x1,&y],x1="a",y = [1,&y1,&0],y1="b"

So in Stroscot there is no fixed memory representation. Instead memory layout is defined by overloaded ``pack``/``unpack`` functions that write/read a memory buffer, similar to the `store library <https://github.com/mgsloan/store/blob/master/store-core/src/Data/Store/Core.hs>`__. The pack/unpack functions will end up getting passed around a lot, but implicit parameters scale pretty well, so it shouldn't be an issue. Unlike Narcissus :cite:`delawareNarcissusCorrectbyconstructionDerivation2019` we don't have a state parameter, also because of implicit parameters.

::

  unpack : ReadBuffer -> a
  pack : a -> WriteBuffer

  read : ReadBuffer -> Vector Word8
  write : WriteBuffer -> Vector Word8 -> WriteBuffer
  length : WriteBuffer -> Int

  shift : ReadBuffer -> (offset : Int) -> ReadBuffer
  shift : WriteBuffer -> (offset : Int) -> WriteBuffer

Write buffers have a length, so to compute the size of the type, we can do ``length . pack``. Read buffers don't have a length so to figure out the length of something in memory we do ``length . pack . unpack``. Some simple types:

::

  pack : Either a b -> WriteBuffer
  pack (Left a) =
    w = emptyWriteBuffer
    write w 0
    write w (pack a)
  pack (Right b) =
    w = emptyWriteBuffer
    write w 1
    write w (pack b)

  unpack r =
    case read r of
      0 -> Left (unpack (shift r 1))
      1 -> Right (unpack (shift r 1))

  pack : (a,b) -> WriteBuffer
  pack (a,b) =
    w = emptyWriteBuffer
    write w (pack a)
    write w (pack b)

  unpack r =
    a = unpack r
    l = length (pack a)
    b = unpack (shift r l)
    (a,b)

``unpack`` can fail on invalid byte sequences, but ``pack`` must always return a byte sequence. Also ``unpack`` can be more lenient and decode sequences that ``pack`` doesn't produce, e.g. nonzero padding bytes. So for correctness we only require ``unpack . pack = id : a -> a`` and not the reverse. Using this constraint we can derive ``unpack`` from ``pack``, or vice-versa, if the format isn't too complicated.

One tricky part is that the naive way to specify types interferes with overloading, subtyping and implicit conversions. ``pack (Int8 1)`` can give a byte as expected, but it can also implicitly convert to an ``Int32`` and give 4 bytes. Since we have dependent types this isn't a real issue, just make sure the code generated after representation specialization passes the type explicitly: ``pack Int32 (Int8 1)``.

A few things need to optimize away for reasonable performance.  ``length . pack`` should optimize to something like ``const 20`` for most values, or at least something that doesn't allocate, so that field accesses are independent and values can be allocated sanely. These functions might have to be hacked in, specializing to constant-sized values.

Since writing these serialization functions all the time would be tedious, we can make a format DSL that specifies the functions in a nicer way. Although one of these DSL's will be the standard / default, it'll be some kind of macro / constraint system, so defining new format DSLs for specific purposes shouldn't be hard.

Non-default representations
---------------------------

The translation to use pack/unpack is pretty simple: a pack is inserted around every constructor, and an unpack is inserted into the scrutinee of every match statement and field assignment. So ``Foo { x = .. }`` translates to ``pack (Foo { x = unpack (..) } )``.

But this translation uses whatever pack/unpack are in scope; they can be overridden like any other implicit parameters. To prevent mismatches the result of pack actually contains the matching unpack function.

Memory management
=================

Ownership a la Rust cannot even handle doubly-linked lists. Code frequently switches to the ``Rc`` type, which besides cycles has the semantics of GC. There is even a `library <https://github.com/Others/shredder>`__ for a ``Gc`` type that does intrusive scanning.

And the malloc/free model is also not correct;

Meanwhile, as far as tracing GC goes, moving and compaction have been optimized using clever algorithms, but there is not a lot of room for performance improvements at runtime. The interesting area of research is static analysis. To that end some work :cite:`proustASAPStaticPossible2017` :cite:`corbynPracticalStaticMemory2020` on "as static as possible" (ASAP) memory management is quite relevant.

To begin with we must model memory. In reality memory is simply a map from addresses to words. But this doesn't prevent any memory errors. So instead we have memory mapping (opaque) addresses to memory blocks, which are byte arrays of fixed size mapping to a single type. The type's fields then determine the data / unpacked fields (ignored for memory purposes) and the references. We can name the references by their dereferencing list, e.g. ``.a.b.c``. A given type may contain arbitrarily many references. A zone is the set of memory blocks reachable from a given value by following all the references. Function ``scan`` (figure 4.9) marks or frees the blocks in a zone. This is used in function ``clean`` which frees blocks from the zones of the antimatter set that aren't in the zones of the matter set, using `tri-color marking <https://en.wikipedia.org/wiki/Tracing_garbage_collection#Tri-color_marking>`__.

Roughly then, to implement memory management, we call ``clean`` during each state transition in the program, with the matter set containing all live blocks and the antimatter set containing all allocated blocks. The size of the state transitions is arbitrary, but smaller ones ensure freeing is timely, while larger ones (may) reduce scanning. So ASAP is essentially stop-the-world GC in design.

The main difference is that, while most GC's use reachability to approximate liveness, ASAP uses a finer approximation. In particular, the main analysis, ``Access(s,z)``, determines if any memory blocks of a zone ``z`` could be accessed during execution starting from state ``s``. With this information we can define the matter and antimatter sets for a state transition ``s -> t``:

* The matter set is all blocks that will be accessed, ``M = {z | Access(t,z) = yes}``
* The antimatter set is all blocks that are accessed before but not accessed later, ``A = {z | Access(s,z) = yes && Access(t,z) = no}``

We can prune all zones in A that are subsets of zones in M.

Using an aliasing analysis, ``Shape``, we can further refine the sets into multiple calls to ``clean``. ``Shape``, given two locations and a state, determines if the zones of those two locations overlap. We use it as follows:
* We partition the anti-matter zones into overlapping zone-sets :math:`A_i`.
* For each component, we filter the matter set to overlapping zones: ``M_i = { z in M | Shape(z, A_i) = yes }``

If the antimatter set is empty then no calls to ``clean`` are needed at all. Furthermore, calls to ``clean`` with an empty :math:`M_i` are unconditional frees and are quite efficient, although there is some overhead to avoid double frees with cyclic/shared data structures. This overhead can be eliminated if the sharing is statically known. Sometimes aliasing can be disallowed through analyzing the program structure and otherwise there are features that disallow aliasing, e.g. in C the strict aliasing rule and the ``restrict`` keyword.

.. note::

  Proust's presentation is a bit more complex and confusing.  It's a thesis, so it probably didn't get much proofreading. For ``clean`` he outlines a basic mark-and-sweep algorithm using a mark array, but this doesn't handle cycles, so he introduces an algorithm in Figure 6.8 that's kind of like tricolor marking but more complicated. He has a 'maybe' state for ``Access`` and ``Shape`` which is uniformly treated as yes. Furthermore he does not use the state-transition formalism so his definition of liveness is split for the constructs of his core language. And by definition the antimatter and matter sets cannot overlap, but he describes a specific pass to remove the matter from the antimatter.


Manual memory management
------------------------

There are cases where garbage collection can be faster than memory management :cite:`appelGarbageCollectionCan1987`. In particular, scratch buffers, as exemplified by GNU C's `obstack <https://www.gnu.org/software/libc/manual/html_node/Obstacks.html>`__. These can be filled full of miscellaneous data to compute a result, then the result can be copied out to a different area of memory and the scratch buffer can be freed in one go. As Appel points out, even if freeing an individual object is a single machine instruction, such as a stack pop, freeing a lot of objects still has significant overhead compared to copying out the useful data.

To do this automatically, we would have to determine the number of regions and their size, which is possible but would most likely require a lot of heuristic decision-making. It is better to allow the programmer to allocate chunks of memory directly, and store/read values within the chunks. The chunks can then be freed by the automatic memory management when they are no longer needed. This requires some modifications to the algorithm to track regions but should be fine.

Compiler memory management
--------------------------

For the compiler itself, a trivial bump or arena allocator is sufficient for most purposes, as it is invoked on a single file and lasts a few seconds. With multiple files and large projects the issue is more complicated, as some amount of information must be shared between files. Optimization passes are also quite traversal-intensive and it may be more efficient to do in-place updates with a tracing GC rather than duplicating the whole AST and de-allocating the old one. Two other sources of high memory usage are macros and generics, particularly in combination with optimizations that increase code size such as inlining.

Overall I don't see much of an opportunity, SSD and network speeds are sufficient to make virtual memory and compile farms usable, so the maximum memory is some large number of petabytes. The real issue is not total usage but locality, because compilers need to look up information about random methods, blocks, types etc. very often. But good caching/prefetching heuristics should not be too hard to develop. In practice the programs people compile are relatively small, and the bottleneck is the CPU because optimizations are similar to brute-force searching through the list of possible programs. Parallelization is still useful. Particularly when AMD has started selling 64-core desktop processors, it's clear that optimizing for some level of that, maybe 16 or 32 cores, is worthwhile.

Copy management
---------------

As well as handling allocation/deallocation, it would also be good to provide copy/move operations. The copy has a copy-on-write semantics where the copy isn't actually done unless/until the original is modified/deallocated. But the memory is managed under the new allocator.

Resource management
-------------------

There are also non-memory resources like thread-handles, file-handles, locks, and sockets. These can be passed around and stored in data structures. The same usage analysis should work to close these resources.




It is important to free memory when it is no longer needed, because otherwise you will run out.

garbage collection partially solves the memory management problem. Garbage collection decreases performance and increases memory usage.

The problem in parallel programming is race conditions. The standard solution is mutexes and condition variables, and variants thereof.



Languages that support exceptions need to support destructors or they need to support a try/finally construct. Otherwise using exceptions is too difficult, because if you have some local state to clean up in a function, you have to catch and rethrow every exception.

The goal of exceptions in C++ is that code which does not throw an exception should be just as efficient as code which is compiled without any support for exceptions. Unfortunately, this is impossible. When any function can throw an exception, and when there are destructors which must be run if an exception is thrown, the compiler is limited in its ability to move instructions across function calls. Of course it is not generally possible to move instructions which change global or heap memory across a function call, but in the absence of exceptions it is generally possible to move instructions which do not change memory or which change only stack memory. This means that exceptions limit what the compiler is able to do, and it follows that compiling with exception support generates code which is less efficient than compiling without exception support.

Of course exceptions still have their uses, but lets consider programming without them (this is easy for me to imagine–I didn’t use exceptions in the gold linker). If you program without exceptions, how useful are destructors and/or try/finally? What comes to mind is functions with multiple return points, loops with multiple exits, and RAII coding.

C has neither destructors nor try/finally. Does it miss them? I would say yes. A common workaround I’ve seen is to change all return points and loop exit points to use a goto to a label which does cleanups.

The gcc compiler has an extension to C to support, in effect, destructors. You can use __attribute__ ((__cleanup__ (function))) with any local variable. When the variable goes out of scope, the function will be called, passing it the address of the variable. This is an effective extension, but it is not widely used.



The Go language does not have destructors. Instead, it has two more dynamic mechanisms. A defer statement may be used to run a function on function exit or when processing a panic. A finalizer may be used to run a function when the garbage collector finds that a block of memory has nothing pointing to it and can be released. Both approaches are dynamic, in that you have to executed the defer statement or call the runtime.SetFinalizer function. They are have no lexical scoping; a single defer statement in a loop can cause its argument to be called many times on function exit.

These ideas are significantly different from destructors, which are associated with a type, and are executed when an object of that type goes out of lexical scope or is explicitly deleted. Destructors are primarily used to release resources acquired by an object of the type. This is a less important concept in a garbage collected language like Go.

The absence of destructors means that Go does not support the RAII pattern, in which an object is used to acquire a mutex or some other resource for the scope of a lexical block. Implementing this in Go requires two statements: one to acquire the mutex, and a defer statement to release the mutex on function exit. Because deferred functions are run on function exit, the mapping is not exact; you can not use this technique to acquire a lock in a loop. In fact, acquiring a mutex in a loop and correctly releasing it when a panic occurs is rather difficult in Go; fortunately it is easy to handle correctly by moving the body of the loop to a separate function. In any case, Go discourages this type of programming. Mutexes are available in Go, but channels are the preferred mechanism for synchronization.

Are defer statements and finalizers sufficient replacement for destructors in a garbage collected language? They are for me. When I write C++ my destructors are almost entirely concerned with releasing memory. In fact, in the gold linker I often deliberately omitted destructors, because many of the data structures live for the life the program; in such a case, destructors serve only to slow down program exit. I would be interested to hear of a pattern of programming which relies on destructors for cases other than releasing memory or RAII.


