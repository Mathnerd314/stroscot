Memory
######

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

https://github.com/ollef/sixten talks about being able to represent intrusive lists. I experimented with allowing the decision of pointer vs direct storage to be made in pack, but it really simplifies the code a lot to require all pack functions to produce flat blobs of data.
