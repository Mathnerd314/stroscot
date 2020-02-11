Memory management
#################

There are roughly 3 styles of memory management:

* Malloc/free calls to an allocator
* Automatic tracing garbage collection
* Mixtures of the above

Rust has improved the malloc/free model, but it has not significantly reduced the need to think about when memory is allocated or freed. There is still a lot of cloning and owning. Code frequently switches to the Rc type, which besides cycles has the semantics of GC.

As far as tracing GC goes, moving and compaction have been optimized using clever algorithms, but there is not a lot of room for performance improvements at runtime. Work has focused on static analysis and reducing what the garbage collector has to track.

For a compiler, a trivial bump or arena allocator is sufficient for most purposes, as it is invoked on a single file and lasts a few seconds. With multiple files and large projects the issue is more complicated, as some amount of information must be shared between files. Optimization passes are also quite traversal-intensive and it may be more efficient to do in-place updates with a tracing GC rather than duplicating the whole AST and de-allocating the old one. Two other sources of high memory usage are macros and generics, particularly in combination with optimizations that increase code size such as inlining.

Overall I don't see much of a challenge, SSD and network speeds are sufficient to make virtual memory and compile farms usable, so the maximum memory is some large number of petabytes. The real issue is not total usage but locality, because compilers need to look up information about random methods, blocks, types etc. very often. But good caching/prefetching heuristics should not be too hard to develop. In practice the programs people compile are relatively small, and the bottleneck is the CPU because optimizations are similar to brute-force searching through the list of possible programs. Parallelization is still useful. Particularly when AMD has started selling 64-core desktop processors, it's clear that optimizing for some level of that, maybe 16 or 32 cores, is worthwhile.
