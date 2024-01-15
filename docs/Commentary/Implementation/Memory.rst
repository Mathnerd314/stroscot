Memory model
############

For memory management we have to consider values, called objects. Pointers are manually freed and hence don't need to be managed.

An invalidate queue is more like a store buffer, but it's part of the memory system, not the CPU. Basically it is a queue that keeps track of invalidations and ensures that they complete properly so that a cache can take ownership of a cache line so it can then write that line. A load queue is a speculative structure that keeps track of in-flight loads in the out of order processor. For example, the following can occur

    CPU speculatively issue a load from X
    That load was in program order after a store to Y, but the address of Y is not resolved yet, so the store does not proceed.
    Y is resolved and it turns out to be equal to X. At the time that the store to Y is resolved, that store searches the load queue for speculative loads that have issued, but are present after the store to Y in program order. It will notice the load to X (which is equal to Y) and have to squash those instructions starting with load X and following.

A store buffer is a speculative structure that exists in the CPU, just like the load queue and is for allowing the CPU to speculate on stores. A write combining buffer is part of the memory system and essentially takes a bunch of small writes (think 8 byte writes) and packs them into a single larger transaction (a 64-byte cache line) before sending them to the memory system. These writes are not speculative and are part of the coherence protocol. The goal is to save bus bandwidth. Typically, a write combining buffer is used for uncached writes to I/O devices (often for graphics cards). It's typical in I/O devices to do a bunch of programming of device registers by doing 8 byte writes and the write combining buffer allows those writes to be combined into larger transactions when shipping them out past the cache.


Pieces
======

* Safe - no dangling pointers (freeing object from live set)
* Complete - no memory leaks (never freeing object from dead set). There is also excessive memory usage, where a program continually uses ever-growing arrays, e.g. an ever-growing Game of Life configuration. But this is not something the compiler can fix. The best the compiler can do is to optimize the program to remove large objects in cases where they aren't necessary.
* Promptness - time from object being dead to it being freed
* Throughput - time to execute program including memory management
* Pause time - time spent in memory manager with all other threads locked

Mutator
-------

* ``src  = New`` - an explicit API in the language, adding to the set of ever-allocated objects ``O`` and allocated objects ``A``
* ``val = Read src`` - reading the value of a cell
* ``Write src val`` - changing the value of a cell. The unpack function may also change but it's a constant-sized function pointer so can be stored easily.
* Roots - objects with easily accessible references
* Live objects will be accessed after the current state, ``z in A and Access(s,z) = yes``

Collector
---------

* Deallocation/reclamantion - removing an object ``o in O`` from ``A``
* A dead object is not live, ``z in O and (Access(s,z) = no or z notin A)``.
* A freed object is in ``O \ A``
* Dead reachable objects are called cruft.
* Unreachable but not freed objects are called floating garbage.
* Mark-sweep: mark all reachable objects as live, free all unreachable objects

.. graphviz::

  digraph G {
    black [fillcolor=black,fontcolor=white,style=filled,label="Presumed live"]
    grey [fillcolor=grey22,fontcolor=white,style=filled,label="grey"]
    white [label="Possibly dead"]

    initial -> white
    white -> grey [label="mark push"]
    grey -> black [label="mark pop"]
    white -> dead [label="sweep"]
    black -> white [label="sweep"]
  }

Allocator
---------

* allocate - reserves the underlying memory storage for an object
* free - returns that storage to the allocator for subsequent re-use

free list, buddy system, bump pointer, mmap/munmap

garbage collection

- pauses
- bandwidth for tracing
- design complexity
- simple user code

Root set
    Any object references in the local variables and stack of any stack frame and any object references in the global object.

RC: count for reference

The count is changed when:

    When object first created it has one reference count
    When any other variable is assigned a reference to that object, the object’s count is incremented.
    When object reference does exit the current scope or assigned to the new value its reference count is decreased by one
    when some object has zero reference count it is considered dead and object is instantly freed.
    When an object is garbage collected, any objects that it refers to have their reference counts decremented.

    does not detect cycles: two or more objects that refer to each other. An simple example of cycle in JS code:

o = ref {} // count of object is 1
f := unpack o; // count of object is 2
o = null; // reference count of object is 1

mark & sweep - reachable/unreachable objects

moving - move reachable object, updating all references to object

semi-space: objects are allocated in "to space" until it becomes full, then "to space" becomes the "from space", and vice versa. reachable objects moved from the "from space" to the "to space". new objects are once again allocated in the "to space" until it is once again full and the process is repeated.

requires 2x address space, lots of copying

Mark-compact: relocates reachable objects towards the beginning of the heap area. can be sliding, arbitrary, or optimize for locality

lazy sweep: when allocate memory and free list is empty, allocator
sweeps unsweeped chunk of memory.

generations: two or more sub-heaps, “generations” of objects. objects allocated to youngest, swept often. promoted to the next generation once sufficient sweep count. Each progressively older generation is swept less often than the next younger generation.

1. Write barrier: catch writes of new objects to already marked objects.

::

  function writeBarrier(object,field) {
        if (isMarked(object) && isNotMarked(field))
          gcMark(field); // mark new field

  }

2. Read barriers: Read barriers are used when collector is moving. They help to get correct reference to the object when collection is running:

::

  function readBarrier(object) {
        // if gc moved object we return new location of it
        if (moved(object)) return newLocationOf(object);
        return object;
  }

Concurrent/incremental GC:
interleave program and GC, GC on separate thread

write barriers or RC increases make every assigment with a heap object on the right hand side a bit more costly. In this case copying the live set can be faster. (related: Appel's Garbage Collection Can Be Faster Than Stack Allocation)
but this introduces memory churn from allocation, and the dominant portion of the execution time is waiting for the cache lines to be loaded or pre-loaded.
You can actually see this exact behavior when profiling Java applications with high allocation rates, for example. You get weird stats that show that allocation is taking no significant time and GC is taking no significant time, but throughput still sucks. By eliminating the in-the-hot-loop allocations, you can see the throughput go up by a significant factor, sometimes by over an order of magnitude, because it avoids stalling.

The issue is latency for a single request and that can be as little as 150 clocks. In managed GC every allocation manipulates some internal data structure. In JVM it's a bump of a top of “thread local allocation block” (TLAB) pointer. In cases of high allocation rates, it is very likely that this pointer will have been evicted thus forcing a round trip to main memory. The factor here is rate of allocations, as opposed to rate of bytes allocated which is reported by typical tools.

Measuring memory performance is tricky. The same workload on the same binary can give 2x changes in performance. It's very sensitive to load order and memory layout. E.g. showing slides during a talk caused the JVM to load in a different memory segment, which changed the results of a timing sensitive calculation which in turn prevented the JVM from doing a memory adaptation that would drop CPU from 100% to 20% and improve latency dramatically.

Generational GC with a tiny live set can win on microbenchmarks, but real programs have large live sets that don't fit in cache and the data will overflow the young generation before it dies. It's been tried repeatedly at Sun and failed miserably. You're better off with the largest young-gen you can get and sucking on the cache misses, OR doing the allocation "by hand" to force rapid (L1-cache-sized) reuse.

    Functions means making closures, new scopes, copy/pass parameters, do indirections on returns, memory allocations, etc

In Rust, this: let x = Vec::with_capacity(10000); for i in 0..10000 {c.push(x)}
avoids resizing/reallocating the vec x because with_capacity specifies the size.
for a_iter_source.iter().collect() there is an optional .size_hint function on the iter that tells how many items it has

in practice asymptotics are BS, and performance depends strongly on memory management. Modeling memory access as O(1) is not correct, due to cache hierarchies - :cite:`jurkiewiczCostAddressTranslation2014` ends up with a log(n) overhead for random access, and similarly `this thread <https://news.ycombinator.com/item?id=12388244>`__ says it's more like O(N^{1/3}) (3D memory architecture), until you near the Bekenstein bound at which point it's O(N^{1/2}) by the holographic principle. Which of these approximations is right? Who knows, power law fitting is `hard <http://bactra.org//weblog/491.html>`__, none of the articles does a convincing job with the empirical data. But generally, the point is that the effects of memory hierarchies can outweigh the asymptotics. Empirically trees are terrible for caches, indirect lookups hit memory hard.

For pointers, can optimize Maybe<T> to still fit into a pointer (null). Then converting T[] to Maybe<T>[] is a no-op.

The GC can use several pages of stack once it is triggered. It needs a separate stack. Similarly crawling the stack allocates on the stack. Again, use a separate stack, tighten up invariants, and add stack probes.


stack is reserved when the thread is created, and can be committed as well. It's inadequate to only reserve because Windows has the unfortunate behavior that committing a page of stack can fail even if plenty of memory is available. If the swap file needs to be extended on disk, the attempt to commit can actually time out during this period, giving you a spurious fault.  If you want a robust application, you should always commit your stack reservations eagerly.

The end of the stack reservation consists of an unmapped page that's a trap for runaway processes, a 1-page buffer for executing stack-overflow backout, and a normal page that generates stack overflow exception when it is allocated past, which will only have a few free bits in an SO condition. So you can really only rely on 1 page to handle SO, which is inadequate. Conclusion: reserve/commit an alternate stack at the beginning to handle SO conditions.

There is a guard bit set on all reserved but uncommitted stack pages.  The stack allocation/deallocation routines must touch/restore stack pages a page at a time, so that these uncommitted pages can be committed and de-committed in order.

getPointer is like C#'s ``fixed`` block but it allows interleaving. Pinned blocks remain where they are during GC, forcing GC generations to start at awkward locations and causing fragmentation. Pinned objects can be moved to a separate GC area before pinning but this could make fragmentation worse if the pin lifetimes are unpredictable. Efficient patterns are:
- pin for a time shorter than a GC cycle, then GC is unaffected
- pin an old object, then it can be stored statically or in a mature generation
- pin a bunch of objects as a group, then you can use an arena
- pin in a LIFO manner, then you can use a stack in an arena
- pin same-sized objects, then you can use a free list in an arena

A very inefficient pattern is to randomly allocate and pin a large number of randomly-sized objects.



I concluded after looking at it again that sharing parts of data structures should be pure, so my plan to use immutable references wasn't going to work because allocating a reference would be impure. So instead there is an allocation interface.

Copying, quad-color incremental, generational garbage collector
Arena-based bump allocator for heap-allocated values
Memory allocator API
Virtual memory API
* POSIX mmap+posix_madvise
* Windows VirtualAlloc

File and network APIs are generally managed by user-level code. So the point of the memory system is to assign a storage location for every value, insert moves / frees where necessary, and overall minimize the amount of resources consumed.

For more advanced programming there is the need to avoid the use of slow storage mechanisms as much as possible by addressing the fast storage mechanisms directly. (Really?)

Memory hierarchy - Place more commonly used items in faster locations - register/cache/memory/disk/recalculate. Items accessed closely together in time should be placed in related locations. Rematerialization recalculates a value instead of loading it from a slow location.

Higher order functions usually require some form of GC as the closures are allocated on the heap. But once you accept GC it is not too tricky, just perform closure conversion or lambda lifting (https://pp.ipd.kit.edu/uploads/publikationen/graf19sll.pdf). There is room for optimization as many algorithms work.

Polymorphism requires a uniform representation for types (pointer/box), or templates like C++. Functional languages use a uniform representation so pay an overhead for accessing via the indirection. Unboxing analysis reduces this cost for primitive types - it works pretty well. GHC doesn't have a particularly good data layout though, because it's all uniform.

* Alias analysis - changing memory references into values
* tail call optimization, Stack height reduction - stack optimizations
* deforestation - remove data structure

API for requesting memory in an async fashion - memory starvation is often the result of contention, so waiting could get you the memory.
* if you request more than is physically on the system, the request will fail immediately, because that much memory will never become available, barring hot-swapping RAM.
* the API would have to ignore swapping. Otherwise, excessive paging occurs before actual memory exhaustion. So the request succeeds but because it's backed by the disk performance completely tanks and the user ends up killing the process.
Unfortunately, no OS APIs allow requesting memory asynchronously. The closest you get is Linux's on-demand backing allocation.

Azul GC: :cite:`clickPauselessGCAlgorithm2005` :cite:`teneC4ContinuouslyConcurrent2011`
Shenandoah "low pause" is 10ms which is the same order of magnitude as NUMA memory map stuff (>10ms if misconfigured)

cache misses are the most important performance metric for memory management, but not usually measured

escape detection - even with 70% of allocations on stack, not good enough to beat Azul
escape analysis - all or nothing
separate allocator has to be as fast as main allocator

compile time garbage collection is detecting when an allocation becomes unused and freeing it
structure reuse is detecting when an allocation becomes unused and reusing the memory for a new allocation
destructive assignment is when a memory address is passed in and modified by the function

Goroutines have little overhead beyond the memory for the stack, which is just a few kilobytes. Go's run-time uses resizable, bounded stacks. A newly minted goroutine is given a few kilobytes, which is almost always enough. When it isn't, the run-time grows (and shrinks) the memory for storing the stack automatically, allowing many goroutines to live in a modest amount of memory. The CPU overhead averages about three cheap instructions per function call. It is practical to create hundreds of thousands of goroutines in the same address space. If goroutines were just threads, system resources would run out at a much smaller number.

The programmer cannot generally predict whether a given code sample will allocate and hence potentially throw an OOM, because of implicit allocations. Here are some examples:

* Implicit boxing, causing value types to be instantiated on the heap.
* marshaling and unmarshaling for the FFI
* immutable array operations
* graph reduction
* JITing a method or basic block, generating VTables or trampolines

But if the compiler is able to eliminate all allocations and hence eliminate the possibility of OOM, then these will most likely be consistently eliminated on every compile. So asserting that a function or block can't OOM is possible. .NET had Constrained Execution Regions which implemented this, with various hacks such JITing the region at load time rather than when the region was first executed. So there's precedent.


GCC (and later LLVM) added MemorySSA

A scratch buffer, as exemplified by GNU C's `obstack <https://www.gnu.org/software/libc/manual/html_node/Obstacks.html>`__ seems to be an array variable plus metadata. They don't require any special support AFAICT. But a stack regime is too restrictive.

The TelaMalloc paper seems like a good memory regime. Basically you have allocations (Start Clock Cycle, End Clock Cycle, Size in bytes) and the compiler has to be able to statically determine how to pack them into a memory space. TelaMalloc uses a limited space of size 𝑀, which is a good model for embedded. With a small limit like 8k, you can track the state of each byte and it's not too much overhead. With a larger limit like the 2GB on video cards you will have to use a tree-like structure to track allocations. On desktop, the allocation limit is generally total physical RAM plus swap minus other running processes. 32-bit also has a limit of 2-3 GB due to virtual addressing.


Allocation randomization such as that found in DieHard or OpenBSD does an isolated allocation for each object at a random address. This allows probabilistically detecting bad pointer arithmetic and buffer overflows because they will access a guard page or fence-post before or after the isolated allocation. It also improves security because malicious code will have to determine the addresses of important objects in order to proceed with an exploit; with careful pointer structure the addresses will not be directly accessible and a brute-force search will have to be used. A "trap page" can detect this brute-force search and abort the program.


Control Flow Guard (CFG) builds a bit map table at compile time of all the legitimate target locations the code can jump too. During the execution of the application, whenever there is a call to jump to a new target location, CFG verifies that the new location was in the bit map table of valid jump locations. If the requested jump location is not listed in the "Valid Jump Locations", Windows terminates the process thereby preventing the malware from executing.

Data Execution Prevention (DEP) marks various pages as non-executable, specifically the default heap, stack, and memory pools.

Structured Exception Handling Overwrite Protection (SEHOP): terminate process if SEH chain does not end with correct symbolic record. Also, the linker will produce a table of each image's safe exception handlers, and the OS will check each SEH entry against these tables.

Address Space Layout Randomization (ASLR) mainly refers to randomizing the locations of executable and library images in memory, but can also randomize allocations.


DieHard segregates metadata from allocations by putting them in different address ranges. Most allocators store metadata adjacent to the allocation, which improves memory locality but makes it easy to corrupt allocation information.

Many allocators attempt to increase spatial locality by placing objects that are allocated at the same time near each other in memory [11, 14, 25, 40]. DieHard’s random allocation algorithm instead makes it likely that such objects will be distant. This spreading out of objects has little impact on L1 locality because typical heap objects are near or larger than the L1 cache line size (32 bytes on the x86). However, randomized allocation leads to a large number of TLB misses in one application (see Section 7.2.1), and leads to higher resident set sizes because it can induce poor page-level lo- cality. To maintain performance, the in-use portions of the DieHard heap should fit into physical RAM

DieHard's complete randomization is key to provably avoiding a range of errors with high probability. It reduces the worst-case odds that a buffer overflow has any impact to 50%. The actual likelihood is even lower when the heap is not full. DieHard also avoids dangling pointer errors with very high probability (e.g., 99.999%), making it nearly impervious to such mistakes. You can read the PLDI paper for more details and formulae.

 On 64-bit the address space is so large that there is no need to worry about packing page allocations, you just allocate some number of pages at a random address and deal with packing things into that.


 But it is possible to allocate pages at fixed addresses, so conceptually the randomization is just an efficient strategy for when allocations are sparse. The randomization does allow detecting allocation errors via unmapped page access, but Valgrind detects that too.

Program memory usage can be segmented into several types:

* frequently accessed data, where reducing the size increases cache performance but they cannot be freed
* infrequently accessed data, where they can be paged to disk but space optimizations don't have much use. It may be better to free the pages and recreate them when needed.
* dead data, which will be determined to not be needed by a traversal and eventually freed
* leaks, data which has been allocated but will never be used

Memory usage affects various measurements: paging, cache misses, OOM aborts, and the physical and virtual memory consumption reported in Task Manager.

Basically you track at each program point what is allocated. Then for each allocation you try to find an unused space of sufficient size - this is where the solver comes in because there are constraints. For phi nodes you just combine allocations, so the state is whatever is allocated in either. And for recursion you require the starting and ending allocations to be identical, or warn about unbounded memory usage.
The UI is essentially the compiler says "ok, I know how to compile your program", or else it errors with an allocation that the compiler can't figure out how to fit.
there is also the end time... somehow you have to propagate the information of how long a block lives back to the allocation point, so that the allocator can make a good decision. I guess it's your standard backwards dataflow pass. The hard part is figuring out a good representation of "cycle time" that can handle loops and stuff



well you've very helpful so far with the references. let me summarize my position to wrap this up:
* Most programmers do not know or care about atomics at all. If they do use atomics, they will most likely use whatever guarantees sequential consistency. Per the volatile-by-default papers, with Hotspot's standard barrier coalescing and read caching optimizations, this has a geometric mean overhead of around 50%, with a maximum observed overhead of 167%. For many purposes this magnitude is not significant enough to matter; it's about the hit you get moving from C++ to Java. And the relaxed annotations significantly reduced the max overhead from 81% to 34%. The fences needed for SC are well-understood and don't have many optimizations.
* If someone needs performance, most likely they are already at the level where they have picked a platform to optimize for and are looking at the assembly. So the most natural memory model is the hardware's. These models are fully specified, well-tested to match actual hardware, and have even (in most cases) been signed off by the hardware manufacturers. Using the hardware memory model does mean the programmer has to learn about the different fences and decide which one to use, but I don't think this task can be avoided if it's really important to optimize a concurrent program. Implementing compiler optimizations based on a hardware MM seems doable.
* Lastly there are the "portable" C++ and Java memory models (and maybe others like LLVM). These allow execution behavior / optimizations not possible on certain platforms, and maybe even "thin air" behavior of execution not possible on any supported platform. In the thin air case it seems it's a bug, but apparently in the x86 vs ARM case it's intended behavior. To avoid confusion, my strategy is to have a literal assembly translation for each atomic operation, so that the behavior is maybe not the fastest but is predictable and consistent.


memory access optimizations: (unconditionally safe only for unshared RAM, but may be allowed depending on memory model and access patterns)
- alias analysis
- reorder loads and stores
- narrowing a store into a bitfield
- rematerializing a load
- Dead Store Elimination (per :cite:`yangDeadStoreElimination2017` need to have "secret variables" that can be cleared using "scrubbing stores" that don't get DSE'd)
- turning loads and stores into a memcpy call
- introducing loads and stores along a codepath where they would not otherwise exist

Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming
https://arxiv.org/abs/1908.05647
Perceus: Garbage Free Reference Counting with Reuse
https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf
Perceus was inspired by the Counting Immutable Beans paper.
Both system uses reference counting with static analysis to find reusable memory cells.
Perceus algorithm is used in the Koka language https://github.com/koka-lang/koka
ASAP (As Static As Possible) is a compile-time automatic memory management system using whole program analysis.
https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf
Micro-mitten: ASAP implementation for a simple Rust like language. Some promising results but his implementation was quite flawed so overall results were negative.
https://github.com/doctorn/micro-mitten
ASAP is not ready for production use, it needs more research.
But it fills a hole in the memory management design space.


There is no concise built-in syntax for dereferencing pointers, because there are many different flavours of memory accesses: aligned or unaligned, cached or uncached, secure or non-secure, etc. and it is critical that every memory access is explicit about its flavor. A side effect of putting more information in the access operation is that pointers are untyped, simply a wrapper around bitvectors.


The ‘load’ and ‘store’ instructions are specifically crafted to fully resolve to an element of a memref. These instructions take as arguments n+1 indices for an n-ranked tensor. This disallows the equivalent of pointer arithmetic or the ability to index into the same memref in other ways (something which C arrays allow for example). Furthermore, for the affine constructs, the compiler can follow use-def chains (e.g. through `affine.apply operations <../Dialects/Affine.md/#affineapply-affineapplyop>`__ or through the map attributes of `affine operations <../Dialects/Affine.md/#operations>`__) to precisely analyze references at compile-time using polyhedral techniques. This is possible because of the `restrictions on dimensions and symbols <../Dialects/Affine.md/#restrictions-on-dimensions-and-symbols>`__.

A scalar of element-type (a primitive type or a vector type) that is stored in memory is modeled as a 0-d memref. This is also necessary for scalars that are live out of for loops and if conditionals in a function, for which we don’t yet have an SSA representation – `an extension <#affineif-and-affinefor-extensions-for-escaping-scalars>`__ to allow that is described later in this doc.

local (“arena”) allocators speed up short-running programs, keep long–running ones from slowing down over time. All global allocators eventually exhibit diffusion–i.e., memory initially dispensed and therefore (coincidentally) accessed contiguously, over time, ceases to remain so, hence runtime performance invariably degrades. This form of degradation has little to do with the runtime performance of the allocator used, but rather is endemic to the program itself as well as the underlying computer platform, which invariably thrives on locality of reference."
diffusion should not be confused with fragmentation–an entirely different phenomenon pertaining solely to (“coalescing”) allocators (not covered in this paper) where initially large chunks of contiguous memory decay into many smaller (non-adjacent) ones, thereby precluding larger ones from subsequently being allocated –even though there is sufficient total memory available to accommodate the request. Substituting a pooling allocator, such as theone used in this benchmark (AS7), is a well-known solution to the fragmentationproblems that might otherwise threaten long-running mission-critical systems."


Arena-based bump allocator for objects
Cheap write barrier in the common case
Mark-and-compact collection for oldest generation
Copying generational collection for younger generations
Special space (in cache?) for nursery generation
State Transitions


I think it's better to write a faster GC than to try to special-case various types of allocation. The GC itself can special case things. Optimizing requires global information and only the GC has a global view.

Static immutable data should be interned.

Compress strings with shoco https://github.com/Ed-von-Schleck/shoco or  the sequitur algorithm http://www.sequitur.info/. Maybe can fit into a 64-bit word. Cleaning the dictionary periodically would probably have to happen to avoid resource leaks, which might have to recompress every relevant string. Fortunately, long strings tend to be fairly long-lived.


ultimate allocator - steal features from all other allocators. It's one of those well-researched areas where a few percent lives. Substitution isn't really an option but maybe some components could be pluggable. Thread safe but values are pure and references can be determined to be thread-local so lots of optimizations.

We want to automatically determine the number of allocation regions and their size to maximize locality.

locate memory leaks - places where allocated memory is never getting freed - memory usage profiling

Handling OOM gracefully - non-allocating subset of language. Should be enough to implement "Release some resources and try again" and "Save the user's work and exit" strategies. Dumping core is trivial so doesn't need to be considered.

A derived pointer is a reference plus an offset. When the address and layout of the object is known we can store the derived pointer as the sum of the value address and offset, allowing direct pointer dereferencing. But since the address is known we could also just store the derived pointer as the offset, so it's only useful if computing the sum is necessary and expensive.

An object can be treated as an array, N[i] and N.length.

The array part of shared memory is necessary because there is a double-word CAS operation on x86 (CMPXCHG16B), and also for efficiency.


With persistent memory only word-sized stores are atomic, hence the choice of shared memory as an array of words. https://stackoverflow.com/questions/46721075/can-modern-x86-hardware-not-store-a-single-byte-to-memory says that there are in fact atomic x86 load/store instructions on the byte level.

word
  An integer ``i`` with ``0 <= i < MAX``.


Ternary: in current computers all words are some number of bits. Most discussion of ternary uses pure ternary, but IMO words will be a mixture of trits and bits - the mixture allows approximating the magic radix e more effectively. IDK. Whatever the case, the bit/trit (digit) is the smallest unit of memory, and all other data is a string of digits.

Since no commercially available computers support ternary it is not worth supporting explicitly in the language. But for future-proofing, we must ensure that anytime there is a binary string, the APi can be extended to use a mixed binary/ternary string.


Eliminating pointers entirely is not possible. But we can minimize the lifetime of pointers in the standard library to the duration of the call, and use values / references everywhere else.
