Memory management
#################

The language should have automatic memory management. Manual memory management is slow, tedious, and error prone. Automatic memory management is better in all respects, but the implementation has to be flexible enough to be usable for all the things manual memory management is.

Memory models
=============

3-3I. It shall be possible to define types whose elements are indirectly accessed. Elements of such types may have components of their own type, may have substructure that can be altered during execution, and may be distinct while having identical component values. Such types shall be distinguishable from other composite types in their definitions. An element of an indirect type shall remain allocated as long as it can be referenced by the program. [Note that indirect types require pointers and sometimes heap storage in their implementation.]

3-3J. Each execution of the constructor operation for an indirect type shall create a distinct element of the type. An operation that distinguishes between different elements, an operation that replaces all of the component values of an element without altering the element's identity, and an operation that produces a new element having the same component values as its argument, shall be automatically defined for each indirect type.

Per :cite:`kangFormalMemoryModel2015` there are pretty much two models of memory, pointers and references. Pointers model memory as an integer-indexed array of 2^32 or 2^64 words, accessed by the OS/hardware APIs. References model memory as an associative array from symbolic "references" (potentially infinite in number) to "cells", values (stored in some unspecified format, but with lossless storage).

Kang describes how combinations of these can be made, for example the "quasi-concrete model" which uses a data type that starts out containing a reference, implements various arithmetic operations symbolically, but switches to a pointer once an integer address is requested. You can also imagine the other direction, a pointer that masquerades as a reference but errors when attempting to store a value larger than the allocation. But references and pointers are the fundamental ideas and serve to implement all other possibilities.

:cite:`brightProgrammingLanguageIdeas2022` brings up the old x86 16-bit pointer model. There were data, code, stack, and extra segment registers. A near pointer simply adds an offset to the appropriate segment register. Far and huge pointers set the segment register first, allowing access to other segments. Far pointers were unnormalized, while huge points were normalized to a canonical segment+offset pair. Nowadays, in x86-64, pointers are just represented as a uniform 64-bit absolute address. The only residue of segment addressing is there are some "load relative" instructions that take offsets instead of absolute pointers.

Bright suggests that the lesson is to only have one type of pointer. But I disagree. The lesson is really to ensure that a pointer is self-contained, in that it always points to the same location, and unique, in that no other pointer value refers to that location. In the 16-bit pointer model, only far and huge pointers were self-contained. And far and huge pointers had the issue of allowing multiple representations of the same address. The normalization solved this, but there were disagreements on how to normalize and it was often skipped for performance reasons. Comparatively, the 64-bit model has a unique pointer value for every address. Turning now to modern models, the concrete and symbolic models are both fine in this regard; integers and symbols are self-contained and unique.

Bright also raises the specter that "You will wind up with two versions of every function, one with manually managed pointers and one with garbage collected pointers (references). The two versions will require different implementations. You'll be sorry." How worrisome is this?

Well, first let's try to use a pointer as a reference. There are many issues to consider:

* Allocation size: Generally it is assumed the pointer points to some fixed-size buffer of bytes. But this means we can't store arbitrary-sized values; they just don't fit. Usually this is solved by restricting the possible values to a finite set, then the storage is fixed.
* Serialization: To mimic the ability of a reference to store heterogeneous types of data, strings, numbers, lists, functions, and so on, we need a universal serialization function, that e.g. stores a type tag. We can probably expose such a serialization function from the compiler, as the compiler needs such a function to implement references. Alternatively, for a restricted type, this is solved by writing a custom serialization function.
* Ownership - Pointers can just be calculated out of thin air, so some other function could overwrite our buffer. The format could be corrupted, or the memory could be deallocated altogether. Usually this is solved by making a private copy of the buffer at some isolated address that no other part of the program uses, and only writing back changes at the end in one atomic operation.

Is someone really going to work through these issues and write a second version of the function? When they could just make the pointer into a reference with ``newRef (deserialize (readBytes 10 ptr))`` and let the compiler do all the work? References should have decent performance so there will be no reason to try to shoehorn a pointer into a reference-like API. Pointers are really a low-level, byte-based abstraction whose only business is interfacing with C code. As evidence that they are needed I offer `C# <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/unsafe-code#pointer-types>`__ which has included them since 1.0.

As far as using a reference as a pointer, as long as we don't want to do pointer arithmetic, we can just store an array of bytes in the reference. Such a pattern is common in Java, e.g. the ArrayList class. But when we want to materialize some bytes at a given memory address, there is no way to do it with references. References just don't support interfacing with C code.

I guess it is possible that someone will have two versions of a function, one that implements it in pure Stroscot via references and one that calls out a C library with pointers. But externally, I think both of them should interact with the rest of the code using references. Using pointers with the C code might avoid a few conversion calls, but references are a lot cleaner to use, e.g. avoiding the use of callbacks, and there is the guaranteed optimization that you can use a reference as a pointer with zero-cost. So I don't think this poses an issue. Even if the C wrapper did use pointers because it was easier than converting to/from references all the time, that's a judgement call on the part of the library author and I don't think there is a solution that would let everyone standardize on one universal type. The best a "pointerOrRef" type can support, even restricted to a small type like ``int8``, is get/set like a regular reference.

Aliasing
--------

Steelman 7I. The language shall attempt to prevent aliasing (l.e., multiple access paths to the same variable or record component) that is not intended, but shall not prohibit all aliasing. [...] All aliasing of components of elements of an indirect type shall be considered intentional.

The language is convoluted and hard to understand, but the way I read this is that anyone who uses an indirection expects aliasing and the language should not do anything to prevent it. Certainly, if you don't need aliasing, you could just use a constant directly.

Pointers
========

Pointers are the low-level API, they can interface with the OS or other languages (mainly C). I did a study of Windows/Linux memory APIs and concluded that memory is best modeled as the global mutable array ``Memory = Map (Word,BitIdx) Status``. The status allows storing metadata, it's `a complex ADT <https://github.com/Mathnerd314/stroscot/blob/master/src/model/MemoryStatus.hs>`__ which has various states like unallocated, committed, etc. The array is indexed at the bit level because that's the granularity `Valgrind's Memcheck <https://valgrind.org/docs/manual/mc-manual.html#mc-manual.machine>`__ uses, but most of the status will be the same for a byte or page as the memory allocators / OS operations work at higher granularity.

It is simple enough to maintain "extra" status bits, and instrument memory functions to check the status of memory before operating. This is essentially what Valgrind does. With this it is possible to identify many common errors, like double free, use after free, access to undefined memory, and null pointer dereferencing. But there is still the possibility of overflowing a buffer into an adjacent allocation, or more generally `type punning <https://en.wikipedia.org/wiki/Type_punning>`__ by reading some memory as a format it was not written with. These sorts of possibilities are intrinsic to the "big array of bits" model, and many low-level hacks rely on such functionality, so I would say to use references if you want to avoid such things. But of course someone can easily add bounds-checking etc. on top of the basic pointer model as a library.

Most addresses will not be allocated (status Free), hence the array is sparse in some sense. It is in fact possible to implement the typical `sparse array operations <https://developer.android.com/reference/android/util/SparseArray>`__. There are functions to directly allocate memory at an address. Reading and writing are done directly in assembly. The list of currently mapped pages can be had from ``/proc/self/maps`` and `VirtualQueryEx <https://reverseengineering.stackexchange.com/questions/8297/proc-self-maps-equivalent-on-windows/8299>`__, although this has to be filtered to remove pages reserved by the kernel and internal pages allocated by the runtime, and looks slow - it's easier to wrap the allocation functions and maintain a separate list of user-level allocations. Clearing mappings, hashing memory, and indexing by mapped pages all work when restricted to the list of user pages. It's a little more complicated than simple sparsity because there are many different statuses and the operations overlap.

Storage vs. memory
-------------------

In practice, the path from cloud to CPU is long, and accessible storage is not just RAM. Some latency numbers and the programming API:

* Physical registers (0.3 ns): managed by the CPU
* Logical registers (0.3 ns): assembly read/write
* Memory Ordering Buffers (MOB), L1/L2/L3 Cache (0.5-7 ns): Managed by the CPU
* Main Memory (0.1us-4us): assembly read/write
* GPU memory (0.2us-0.5us): assembly read/write, driver ioctl's
* NVRAM (200us-250us): assembly read/write, special calls
* SSD (250-500us): kernel file APIs
* LAN (0.5-500ms): kernel network stack, driver bypass
* HDD (3 ms): kernel file APIs
* WAN (150ms): kernel network stack, driver bypass

Not all applications will use all of these, but all will use some and there is an application that uses each. So all of these have to be modeled in order to create a performant application. Ideally the memory management system would be a "storage management system" that combines all of these into a single pointer-like abstraction and allows copying data between locations as appropriate. But it's a leaky abstraction, I'm not sure it can be pulled off except as a library.

"You-choose" Allocation
-----------------------

In practice, fixed-address allocation / assignment is not commonly used. Instead, there are ``mmap NULL``, ``malloc``, and the C library API alloc/realloc, which allocate memory with system-chosen / allocator-chosen location. For verifying behavior, the right model for this is adversarial, i.e. the allocator chooses the worst possible location, subject to restrictions such as that the allocation must be suitably aligned and disjoint from all unrevoked allocations. More formally, the behavior of a correct program should not depend on what addresses the system picks, i.e. all choices should be observationally equivalent. (The system can also return an out of memory error, but this doesn't have to result in equivalent behavior.)

Of course, the actual allocation strategy should not be the worst, rather it should try to achieve the best performance. For the most part, people do not seem to pay much attention to allocator design, because it is pretty cheap. For example `in Doom 3 <https://www.forrestthewoods.com/blog/benchmarking-malloc-with-doom3/>`__ the median time for is 31 nanoseconds, ranging from 21 nanoseconds to 201 microseconds, and free is comparable.

But, speeding up allocation is actually fairly important. Combining operations into a single larger operation (allocate a larger buffer, call ``close_range`` to close several open FD's than to iterate over them individually) by pushing allocations forward and delaying frees, as long as there is sufficient memory or resource capacity available, can be a big win. In contrast, reads and writes are always real work, and besides SIMD there is not much way to optimize it.

There are also a lot of locality and cache effects from the address allocation algorithm. In the trivial case, the memory usage can be predicted in advance and allocations given fixed assignments, giving zero cost memory allocation. In more practical applications, variable allocations will need to be tracked, but there are still tricks for grouping allocations based on access patterns, avoiding fragmentation. Most research has been on runtime allocation optimization, but many of these optimizations can be precomputed at compile time. For example:

* A loop that allocates and deallocates a scratch buffer in the body is much more performant if the buffer is allocated to the same location every time - the allocation/deallocation code can even be pulled out of the loop.
* Grouping hot variables into a page, so the page is always loaded and ready
* Grouping things that will be freed together (pools/arenas)

Optimizing access
-----------------

Generally, optimizations are allowed to eliminate possibilities allowed by the memory model, but there could also be an option to strictly preserve the set of possibilities.

Eliminating a pointer read amounts to tracking down the matching pointer write and propagating the value directly, which can be accomplished by tracing control flow. There is the issue of data races with concurrent writes, but the memory model dictates which values a read may resolve to, and the verifier already handles nondeterminism, so it is not much harder than normal value propagation. There is also modeling foreign code, specifically determining whether the foreign code can write a pointer (i.e, whether the pointer is shared or not).

Eliminating a pointer write requires proving that the address is never read before deallocation or another pointer write. Again there are the issues of data races and foreign code.

CHERI
-----

CHERI pointers are 129-bit, consisting of a 1-bit validity tag, bounds, permissions, object type, and actual pointer. Valid pointers may only be materialized in a register or memory by transforming an initial unbounded pointer obtained from the OS. This means that the simple model of pointers as integers is no longer valid. Instead, a pointer is the combination of an integer address and a capability. The `CHERI C/C++ API <https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-947.pdf>`__ represents the address+capability value as ``void*`` and addresses as ``vaddr_t``; there doesn't seem to be a way to refer to a capability without an address.

I tried to read further, but the model is complicated, essentially implementing a GC to avoid dangling pointers, so I am not sure it will ever become mainstream.

Persistent memory
-----------------

The pointer API, assembly wrapping, and OS calls cover using persistent memory via standard file APIs or memory-mapped DAX. Memory is volatile while persistent memory is not, so persistent memory is faster storage, not weird RAM. And storage is complex enough that it seems best handled by libraries. Making the memory management system memkind-aware seems possible, like memory bound to NUMA nodes.

References
==========

5B. Each variable must be declared explicitly. Variables may be of any type. The type of each variable must be specified as part of its declaration and must be determinable during translation. [Note, "variable" throughout this document refers not only to simple variables but also to composite variables and to components of arrays and records.]
5E. There shall be no default initial-values for variables.
5F. Assignment and an implicit value access operation shall be automatically defined for each variable.
9C. It shall be.possible to mark variables that are shared among parallel processes. An unmarked variable that is assigned on one path and used on another shall cause a warning.

A reference is a symbolic index into a global associative array of objects, ``Map Reference Object``. The array allows allocating new references, deleting them, and reading/writing the reference. Reference symbols can be compared for equality, hashed to an integer, and packed/unpacked to/from an integer.

The packing and hashing requires a little explanation. Packing the same reference always returns the same value during a program execution, and the packed value is distinct from the packed value of any other reference. But the exact value is internal to the memory system - it is an "adversarial" model similar to pointers where if the program's behavior depends on the choice of packed value it is incorrect. The hashing is similar to packing, it is again the same value for the same reference, it is just that there is no distinctiveness constraint (so the program must have the same behavior even if all references hash to 0), and also no way to unhash the value, so there is no need to worry about resolving unpack invocations.

There are higher-level types like immutable references and reference wrappers, but those all translate away to normal references or pointer access and don't need involvement from the compiler. Per :cite:`ichbiahRationaleDesignADA1979` we should provide a "freeze" operation which recursively removes all reference indirections and turns a reference-containing value into a truly immutable/constant object, as this is "the most useful and should be retained as the unique meaning of constancy".

Pointer conversion
------------------

The location of the data of a reference is not fixed. If it's small enough it could just be in a register, or there could be multiple copies of the data in memory. Also GC can move/copy the reference. The data could be produced on-demand and be represented by a thunk. All that can really be said is that the compiler will respect the semantics of storing and retrieving data.

Foreign operations like OS calls require a pointer to a memory address, because references don't necessarily exist in memory. The canonical way of doing this is simply reading the reference value and storing it in a buffer represented by a pointer ("materializing" it in memory). Internally, when compiling away the reference, the compiler tries to find a good way to store the reference - if it's lucky, it can backpropagate the pointer request and store the data there from the beginning, so that the "read and store" operation is actually a no-op that makes zero copies.

But, in the fallback case of storing a few words, where a memory allocation is appropriate, the reference translates directly to a pointer allocation. The memory is configured to trap on stray user-level access, so that only the compiler-generated code has access. Even in this case, though, the reference's internal value is not the pointer itself, rather there is a more complex strategy of using a "handle" identifier that allows moving the data around after it is allocated.




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

https://github.com/ollef/sixten talks about being able to represent intrusive lists. I experimented with allowing the decision of pointer vs direct storage to be made in pack, but it really simplifies the code a lot to require all pack functions to produce flat blobs of data.

Destructors are inspired by C++ RAII destructors, hence the name. Admittedly the actual API doesn't bear much resemblance. `Finalizers <https://en.wikipedia.org/wiki/Finalizer>`__ can resurrect objects and don't have deterministic execution, hence would be a bad name. Go's defer statement and try-finally are related, but they only work locally and have imprecise execution semantics.

Portable mmap:
* Yu virtualalloc https://github.com/alpha123/yu/tree/master/src/platform
* Go: https://github.com/edsrzf/mmap-go
* C: mmap on windows https://github.com/alitrack/mman-win32
* C++: https://github.com/mandreyel/mio
* Rust: https://github.com/RazrFalcon/memmap2-rs

Representation
==============

11A. The language shall permit but not require programs to specify a single physical representation for the elements of a type. These specifications shall be separate from the logical descriptions. Physical representation shall include object representation of enumeration elements, order of fields, width of fields, presence of "don't care" fields, positions of word boundaries, and object machine addresses. In particular, the facility shall be sufficient to specify the physical representation of any record whose format is determined by considerations that are entirely external to the program, translator, and language. The language and its translators shall not guarantee any particular choice for those aspects of physical representation that are unspecified by the program. It shall be possible to specify the association of physical resources (e.g., interrupts) to program elements (e.g., exceptions or signals).

A lot of languages have a fixed or default memory representation for values, e.g. a C struct, a Haskell ADT, and a Python object are always laid out in pretty much the same way. The more systems-level languages allow controlling the layout with flags, for example Rust has `type layout <https://doc.rust-lang.org/reference/type-layout.html>`__ and also C compatibility. Layout is then defined by its size, alignment, padding/stride, and field offsets. Now it's great to have a compact representation of the memory layout - but only if you can actually write the memory layout you want using these features. But these flags are't really that powerful. Here's some examples of what can't generally be done with the current memory DSL's:

* specify the in-memory order of fields differently from their logical order
* specifying how to encode enumeration constants (per struct it appears in)
* turn array-of-structs into struct-of-arrays
* flattening a datatype, like ``Either Bool Int`` into ``(Bool,Int)``, or representing a linked list as a contiguous series of records.
* storing some parts via pointer indirections (non-contiguous memory layout)
* NaN-boxing and NuN-boxing (`ref <https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations>`__ `2 <https://searchfox.org/mozilla-central/source/js/public/Value.h#526>`__), representing the JS ``Any`` type as a single 64-bit word.
* parsing network packets into structured data

Maybe some of these could be addressed by flags, but from the last two, it is clear that we are really looking for a general-purpose memory serialization interface. I looked at `Data.Binary <https://hackage.haskell.org/package/binary-0.8.9.1/docs/src/Data.Binary.Get.Internal.html#Decoder>`__, `store <https://github.com/mgsloan/store/blob/master/store-core/src/Data/Store/Core.hs>`__, and :cite:`delawareNarcissusCorrectbyconstructionDerivation2019`. Narcissus is too complex IMO:

::

  Format = Set (S, St, T, St)
  Encode = S -> St -> Option (T, St)
  Decode = T -> St -> Option (S, St)

The state parameter can be gotten rid of by defining ``S = (S,St), T = (T,St)``:

::

  Format = Set (S, T)
  Encode = S -> Option T
  Decode = T -> Option S

And we can make encode/decode total by defining ``S = {s | exists t. (s,t) in Format}``, ``T = {t | exists s. (s,t) in Format}``.

I thought about letting ``pack`` narrow the range of values, e.g. rounding 1.23 to 1.2, but concluded that it would be surprising if storing a value to memory changed it. The rounding can be defined as a pre-pass over the data to convert it to a ``Measurement`` type that then has optimized storage.

One tricky part is that the naive way to specify types interferes with overloading, subtyping and implicit conversions. ``pack (Int8 1)`` can give a byte as expected, but it can also implicitly convert to an ``Int32`` and give 4 bytes. Since we have dependent types this isn't a real issue, just make sure the code generated after representation specialization passes the type explicitly: ``pack Int32 (Int8 1)``.

A few things need to optimize away for reasonable performance.  ``length . pack`` should optimize to something like ``const 20`` for most values, or at least something that doesn't allocate, so that field accesses are independent and values can be allocated sanely. These functions might have to be hacked in, specializing to constant-sized values.

Since writing these serialization functions all the time would be tedious, we can make a format DSL that specifies the functions in a nicer way. Although one of these DSL's will be the standard / default, it'll be some kind of macro / constraint system, so defining new format DSLs for specific purposes shouldn't be hard.

The translation to use pack is pretty simple: every value is wrapped in a call to pack, the result is stored as a tuple ``(cell,unpack)``, and every usage applies unpack to the cell. The translation uses whatever pack is in scope; pack can be overridden like any other implicit parameters. The unpack functions will end up getting passed around a lot, but function pointers are cheap constants, and constant propagation is a thing, so it shouldn't be an issue.

So finally the most general API is ``Write = Alloc (Size,Align) (Addr -> Write) | Store, Store = Map Addr MaskedWord`` and ``Unpack a = Maybe Addr -> Read -> a, Read = Map Addr Word``. This allows masked writes and multiple or fixed allocation addresses, but does not allow failing to read the value back. Also the ``pack`` function allows passing arbitrary side-band data to the ``unpack`` function. Maybe though, it is still not general enough, we should just have lens-like functions like ``write : Memory -> a -> Memory`` and ``read :: Memory -> a``. There still need to be constraints though, like that you get back what you wrote and non-interference of writes.

Now we also want to allow optimization of the memory representation. Consider some data points - if there is only one possible value, then the compiler should optimize this to a constant and not store it at all. If there are two possible values, the compiler should probably use a boolean flag and again hard-code the values as constants. If the potential values include all values of a given type (and nothing else), then the compiler should use the representation for that type. If the potential values include a given type, and also members of another type, then the compiler should use the most narrowly-defined representation that contains both of those types. And it should consider whether it can choose the representation of the union type so as to minimize the amount of conversion needed for the more commonly used type (as in NaN/NuN-boxing). If the potential values can be anything, then the compiler should use the universal representation.

The process of fixing the memory representation of a program can be modeled as follows. We start with a program that passes around values. Then we insert conversion operations: on every declaration, we insert a conversion to binary, and on every use, we insert a conversion from binary. As the binary representation is defined so that a read of a write is is the identity, this transformation does not change the meaning of the program. Then we additionally write this binary representation to memory on the declaration, and read this binary representation from memory on use. Again this does not change the semantics due to the non-interference of writes property. Although, in reality it could change the semantics: maybe a cosmic ray or something could change what we have written. But at this point, our program operates purely on memory and does not have any values floating around.

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

A derived pointer is a reference plus an offset. When the address and layout of the object is known we can store the derived pointer as the sum of the value address and offset, allowing direct pointer dereferencing. But since the address is known we could also just store the derived pointer as the offset, so it's only useful if computing the sum is necessary and expensive.

An object can be treated as an array, N[i] and N.length.

The array part of shared memory is necessary because there is a double-word CAS operation on x86 (CMPXCHG16B), and also for efficiency.


With persistent memory only word-sized stores are atomic, hence the choice of shared memory as an array of words. https://stackoverflow.com/questions/46721075/can-modern-x86-hardware-not-store-a-single-byte-to-memory says that there are in fact atomic x86 load/store instructions on the byte level.

word
  An integer ``i`` with ``0 <= i < MAX``.


Ternary: in current computers all words are some number of bits. Most discussion of ternary uses pure ternary, but IMO words will be a mixture of trits and bits - the mixture allows approximating the magic radix e more effectively. IDK. Whatever the case, the bit/trit (digit) is the smallest unit of memory, and all other data is a string of digits.

Since no commercially available computers support ternary it is not worth supporting explicitly in the language. But for future-proofing, we must ensure that anytime there is a binary string, the APi can be extended to use a mixed binary/ternary string.


Eliminating pointers entirely is not possible. But we can minimize the lifetime of pointers in the standard library to the duration of the call, and use values / references everywhere else.


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

Dumping ground
==============



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


Memory model
~~~~~~~~~~~~

The ‘load’ and ‘store’ instructions are specifically crafted to fully resolve to an element of a memref. These instructions take as arguments n+1 indices for an n-ranked tensor. This disallows the equivalent of pointer arithmetic or the ability to index into the same memref in other ways (something which C arrays allow for example). Furthermore, for the affine constructs, the compiler can follow use-def chains (e.g. through `affine.apply operations <../Dialects/Affine.md/#affineapply-affineapplyop>`__ or through the map attributes of `affine operations <../Dialects/Affine.md/#operations>`__) to precisely analyze references at compile-time using polyhedral techniques. This is possible because of the `restrictions on dimensions and symbols <../Dialects/Affine.md/#restrictions-on-dimensions-and-symbols>`__.

A scalar of element-type (a primitive type or a vector type) that is stored in memory is modeled as a 0-d memref. This is also necessary for scalars that are live out of for loops and if conditionals in a function, for which we don’t yet have an SSA representation – `an extension <#affineif-and-affinefor-extensions-for-escaping-scalars>`__ to allow that is described later in this doc.
