Memory management
#################

The malloc/free model is not correct; what we need to keep track of is what will be accessed and where it will be stored. Memory leaks are a pervasive problem, in general there is no real solution besides profiling and buying more RAM. Stroscot can prove memory will not be accessed in the future and hence free it, with a more precise analysis than traditional GC. On the other hand use-after-free and double free can be statically checked for pointers, and aren't a problem at all for the rest of the language because Stroscot frees automatically.

Memory management is not about finding a place to store things. If it was, global storage capacity is measured in zettabytes, so we could just store everything in the cloud. Or hard drives would be sufficient for almost all purposes. The issue is storing and retrieving things in the most efficient way possible, with little overhead - in particular preserving cache locality. Examples:
* A loop that allocates and deallocates a scratch buffer is much more performant if the buffer is allocated to the same location every time - the allocation/deallocation code can even be pulled out of the loop.
* Grouping hot variables into a page
* Grouping things that will be freed together (pools)

Garbage collection scanning slows things down because it pulls in a lot of memory; generational GC reduces this somewhat, but the more interesting area of memory management research is static analysis. To that end some work :cite:`proustASAPStaticPossible2017` :cite:`corbynPracticalStaticMemory2020` on "as static as possible" (ASAP) memory management is quite relevant. Conceptually we are taking a tracing GC algorithm and partially evaluating it at compile time. It's a whole program undecidable analysis, but Stroscot already has 3 or 4 of those planned...

* The live set is all objects that are live (will be accessed), ``L(s) = {z | Access(s,z) = yes}``
* The dead set is the complement of the live set, ``D(t) = {z | Access(t,z) = no}``
* The newly-dead set is all objects that are accessed before but not accessed later, ``A = {z | Access(s,z) = yes && Access(t,z) = no} = L(s) intersect D(t)``, where the state transition is ``s -> t``.

To implement memory management, we deallocate the newly-dead set after each operation. This doesn't necessarily reclaim the memory, but ensures freeing is timely if needed. We also can compact the live set by removing dead fields.

a copying incremental collector

Quad-color incremental marking

The GC status of an object is set by two bits, the mark bit and the gray bit. The mark bit is stored in a bitmap, can be white or black. The gray bit is stored in a boxed_value object, determining whether an object has been fully marked. Only traversable objects have a gray bit and hence quad colors. Non-traversable (flat) objects have very simple state transitions (just white->black->white).

.. graphviz::

  digraph G {
    "Newly allocated traversable object" [fillcolor=lightgray,style=filled]
    s1 [label="Sweep"]
    s2 [label="Sweep"]
    wb1 [label="Write",fillcolor=lightgray,style=filled]
    wb2 [label="Write"]
    "Object Fully Traversed" [fillcolor=black,fontcolor=white,style=filled]
    "Gray Stack" [fillcolor=grey22,fontcolor=white,style=filled]

    "New Object" -> s1
    s1 -> "Object Is Freed"
    "Object Is Freed" -> s2
    s2 -> "Sweep Resets To white"
    "Sweep Resets To white" -> wb1
    wb1 -> s1 [label="Flat"]
    wb1 -> "Traversal Starts: Object Was Rooted Or Referenced"
    "New Object" -> "Traversal Starts: Object Was Rooted Or Referenced"
    "Traversal Starts: Object Was Rooted Or Referenced" -> "Push"
    wb2 -> "Push"
    "Object Fully Traversed" -> "Sweep Resets To white"
    "Object Fully Traversed" -> wb2
    "Push" -> "Gray Stack"
    "Gray Stack" -> "Pop"
    "Pop" -> "Add Referenced Objects To Gray Stack"
    "Add Referenced Objects To Gray Stack" -> "Object Fully Traversed"
  }

  10:50:15 From  Chris Engelbert  to  Everyone : That is kinda like the distinction that go has, errors are returned from functions and can be handled, panics are like real issues that normally cannot be handled anyways
  10:55:37 From  Chris Engelbert  to  Everyone : They are super handy. If you have a go lib which bubbles up an error multiple levels, you’ll have a really hard time to find the real issuer of the error.
  10:56:39 From  Yorick Peterse  to  Everyone : http://joeduffyblog.com/2016/02/07/the-error-model/
  10:58:25 From  Matt Dziubinski  to  Everyone : This is interesting: https://tuplex.cs.brown.edu/
  This part in particular: https://twitter.com/ms705/status/1410658222885773315
  "The key insight in Tuplex is that we can *specialize* the  code *to the data*, rather than emitting general code.

  E.g., if input data is mostly integers, Tuplex generates LLVM IR that assumes integers & splits the data into common case (compiled) and exceptions (interpret)."

  Error handling in Swift resembles exception handling in other languages, with the use of the try, catch and throw keywords. Unlike exception handling in many languages—including Objective-C—error handling in Swift doesn’t involve unwinding the call stack, a process that can be computationally expensive. As such, the performance characteristics of a throw statement are comparable to those of a return statement.”
  11:18:21 From  Aaron Goldman  to  Everyone : Proceedings of an International Conference on Algol 86 Implementation : Department of Computer Science, University of Manitoba, Winnipeg, June 18-20, 1974
  11:33:09 From  Matt Dziubinski  to  Everyone : Cf. this part (page 26): 8 Benchmark II: Variation in Locality (Long Running)
  11:33:20 From  Matt Dziubinski  to  Everyone : "Perhaps the most valuable aspects of local (“arena”) allocators is that, besides speeding up short-running programs, as demonstrated in the previous benchmark, they keep long–running ones from slowing down over time. All global allocators eventually exhibit diffusion–i.e., memory initially dispensed and therefore (coincidentally) accessed contiguously, over time, ceases to remain so, hence runtime performance invariably degrades. This form of degradation has little to do with the runtime performance of the allocator used, but rather is endemic to the program itself as well as the underlying computer platform, which invariably thrives on locality of reference."
  11:33:29 From  Matt Dziubinski  to  Everyone : "N.B., diffusionshouldnot be confused with fragmentation–an entirely different phenomenon pertaining solely to (“coalescing”) allocators (not covered in this paper) where initially large chunks of contiguous memory decay into many smaller (non-adjacent) ones, thereby precluding larger ones from subsequently being allocated –even though there is sufficient total memory available to accommodate the request. Substituting a pooling allocator, such as theone used in this benchmark (AS7), is a well-known solution to the fragmentationproblems that might otherwise threaten long-running mission-critical systems."


Newly allocated traversable objects are light-gray. Writing only changes the state of non-gray objects.

  When the object is marked during the mark phase, it's turned dark-gray (mark bit turned black) and pushed onto the gray stack. In case it's unreachable, the sweep phase can free a light-gray object like any other object marked white.

  Dark-gray objects are turned black after traversal (clearing the gray bit) and turned white after sweeping. The write barrier may trigger during this short period and move the barrier back by turning it dark-gray again.

  An object that survived one GC cycle is turned white like all other survivors. In case the object is written to after that, it's turned light-gray again. But this doesn't push the object onto the gray stack right away! In fact, only the gray bit needs to be flipped, which avoids further barriers as explained above.

  The main advantage of the quad-color algorithm is the ultra-cheap write barrier: just check the gray bit, which needs only 2 or 3 machine instructions. And due to the initial coloring and the specific color transitions, write barriers for e.g. tables are hardly ever triggered in practice. The fast path of the write barrier doesn't need to access the mark bitmap, which avoids polluting the cache with GC metadata while the mutator is running.

  The quad-color algorithm can easily fall back to the tri-color algorithm for some traversable objects by turning them white initially and using forward write barriers. And there's an obvious shortcut for non-traversable objects: marking turns a white object black right away, which touches the mark bitmap only. Since these kind of objects are in segregated arenas, they don't need to be traversed and their data never needs to be brought into the cache during the mark phase.


Arena-based bump allocator for objects
  Cheap write barrier in the common case
  Mark-and-compact collection for oldest generation
  Copying generational collection for younger generations
  Special space (in cache?) for nursery generation
  State Transitions


I think it's better to write a faster GC than to try to special-case various types of allocation. The GC itself can special case things. Optimizing requires global information and only the GC has a global view.

Static immutable data should be interned.

Compress strings with shoco https://github.com/Ed-von-Schleck/shoco or  the sequitur algorithm http://www.sequitur.info/. Maybe can fit into a 64-bit word. Cleaning the dictionary periodically would probably have to happen to avoid resource leaks, which might have to recompress every relevant string. Fortunately, long strings tend to be fairly long-lived.

Model
=====

For memory management we have to consider values, called objects. Pointers are manually freed and hence don't need to be managed.




An invalidate queue is more like a store buffer, but it's part of the memory system, not the CPU. Basically it is a queue that keeps track of invalidations and ensures that they complete properly so that a cache can take ownership of a cache line so it can then write that line. A load queue is a speculative structure that keeps track of in-flight loads in the out of order processor. For example, the following can occur

    CPU speculatively issue a load from X
    That load was in program order after a store to Y, but the address of Y is not resolved yet, so the store does not proceed.
    Y is resolved and it turns out to be equal to X. At the time that the store to Y is resolved, that store searches the load queue for speculative loads that have issued, but are present after the store to Y in program order. It will notice the load to X (which is equal to Y) and have to squash those instructions starting with load X and following.

A store buffer is a speculative structure that exists in the CPU, just like the load queue and is for allowing the CPU to speculate on stores. A write combining buffer is part of the memory system and essentially takes a bunch of small writes (think 8 byte writes) and packs them into a single larger transaction (a 64-byte cache line) before sending them to the memory system. These writes are not speculative and are part of the coherence protocol. The goal is to save bus bandwidth. Typically, a write combining buffer is used for uncached writes to I/O devices (often for graphics cards). It's typical in I/O devices to do a bunch of programming of device registers by doing 8 byte writes and the write combining buffer allows those writes to be combined into larger transactions when shipping them out past the cache.


Allocator
=========

ultimate allocator - steal features from all other allocators. It's one of those well-researched areas where a few percent lives. Substitution isn't really an option but maybe some components could be pluggable. Thread safe but values are pure and references can be determined to be thread-local so lots of optimizations.

We want to automatically determine the number of allocation regions and their size to maximize locality.

locate memory leaks - places where allocated memory is never getting freed - memory usage profiling

Handling OOM gracefully - non-allocating subset of language. Should be enough to implement "Release some resources and try again" and "Save the user's work and exit" strategies. Dumping core is trivial so doesn't need to be considered.

