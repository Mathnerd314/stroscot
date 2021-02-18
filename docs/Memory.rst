Memory management
#################

Every program has to use memory in order to store mutable values and interact with system calls.

Terms
-----

memory cell
  A circuit that can store some fixed number of logical bits (0/1). Since ternary computers might eventually become popular, a cell is probably best modeled as a register containing an integer ``i`` with ``0 <= i < MAX``, with no restriction that ``MAX`` is a power of 2.

RAM value
  an associative array going from memory cell names to memory cell values, together with a distinguished memory cell name.

Core expression
  Any instance of a Core net

Core value
  Core expressions that are not reducible (does not contain cut).

The point of the memory system is to ensure every Core value has a corresponding RAM value representation, and to rewrite the program to use RAM values instead of Core values.

Representation
--------------

Layout is usually defined by its size, alignment, padding/stride, and field offsets, but this only specifies the representation of simple records. With enumerations, there is the question of how to encode constants. It gets even more complicated with ADTs, like JS's `value type <https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations>`__, and the choices often impact performance significantly.

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

References
----------

::

  alloc : a -> Ref a
  deref : Ref a -> a

A pointer type ``Ptr`` is a wrapper around the ``UInt`` type of the machine's native address size. It deliberately does not support any arithmetic operations, to discourage careless pointer arithmetic. A further refinement is ``APtr n``, an ``n``-byte aligned pointer. A reference type ``Ref a`` is a pointer that is guaranteed to refer to an value of type ``a`` when its memory is dereferenced. References offer a lot of flexibility for memory layout. For example, we can encode a linked list in a number of ways:

::

  # flat list, stored like 1a1a0
  List a = Nil | Cons a (List a)
  # intrusive list, stored like 1a* -> 1a* -> 0
  List a = Nil | Cons a (Ref (List a))
  # uniform list, stored like 1** -> {a, 1** -> {a, 0}}
  List a = Nil | Cons (Ref a) (Ref $ List a)

Unrestricted, references introduce an entire class of memory errors; in particular, we cannot free the memory the reference refers to unless the reference will not be dereferenced anymore. Violating this condition is a use-after-free. Similarly freeing itself must happen exactly once, so we have double frees and memory leaks.

On a positive note, since we work with datatypes first and their representations only incidentally, we do not have to handle buffer overflows; pointer arithmetic is implicit in the pack/unpack functions and due to our correctness properties, unpacking fields of the datatype must read within the allocated buffer.

Since Stroscot is non-strict we have actually two reference types, evaluated and non-evaluated.

Automatic memory management
---------------------------

Ownership a la Rust cannot even handle doubly-linked lists. Code frequently switches to the ``Rc`` type, which besides cycles has the semantics of GC. There is even a `library <https://github.com/Others/shredder>`__ for a ``Gc`` type that does intrusive scanning. Meanwhile, as far as tracing GC goes, moving and compaction have been optimized using clever algorithms, but there is not a lot of room for performance improvements at runtime. The interesting area of research is static analysis. To that end some work :cite:`proustASAPStaticPossible2017` :cite:`corbynPracticalStaticMemory2020` on "as static as possible" (ASAP) memory management is quite relevant.

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
