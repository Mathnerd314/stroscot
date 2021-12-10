Memory
######

There are two main models of memory. The concrete model models memory as a an integer-indexed array of 2^32 or 2^64 words. The symbolic model models memory as an associative array from symbols (potentially infinite in number) to "cells", arrays of words of various lengths. In Stroscot these models correspond to pointers and references respectively. Combinations of these can be made, for example the "quasi-concrete model" which uses a data type that starts out containing a reference, implements various arithmetic operations symbolically, but switches to a pointer once an integer address is requested. :cite:`kangFormalMemoryModel2015`

Value representation
====================

::

  Bit = 0 | 1
  MaskedBit = Bit | Masked
  MaskedByte = [0..7] -> MaskedBit
  Cell = [MaskedByte]
  Object = (cell : Cell, unpack : Cell -> a)
  pack : a -> Object

The memory representation of a value is defined by an overloaded ``pack`` function. The result of pack is an *object*, a tuple containing a cell and a matched unpack function to read the value back from the cell. Usually ``unpack`` will be a function pointer and Stroscot can use constant propagation to optimize it out of the object. In the worst case ``unpack`` contains a reference to the full value and Stroscot will use its default value representation.

Cells are contiguous arrays of bytes. They can be reallocated and copied freely by the runtime. Cells are garbage collected, deallocated automatically when the cell is no longer used/accessible. Cells have a mask of unused bits to allow making holes. For example ``010`` masked with ``101`` produces ``0*0``, moving 3 bits. The runtime is free to use the center bit for garbage collection purposes.

``pack`` must always succeed. ``unpack`` can fail or succeed on data not produced by ``pack``, the behavior is irrelevant. ``unpack`` should not depend on the value of any bits masked by ``pack``.

For correctness we require ``unpack (pack x) = x``. Using this constraint we can derive ``unpack`` from ``pack``, or vice-versa, if the format isn't too complicated.

Pointers
========

Pointers provide a low-level API for interfacing with the OS or other languages (mainly C), an unavoidable task in most programs. To use them you have to import the ``Pointer`` module.

Concept
-------

A pointer is a numeric index into a global mutable array, ``Map Word (BitIdx -> Status)``. The statuses allow storing metadata. The array is indexed at the bit level because that's the granularity `Valgrind's Memcheck <https://valgrind.org/docs/manual/mc-manual.html#mc-manual.machine>`__ uses, but most of the status will be the same for a byte or page. The status is `an ADT <https://github.com/Mathnerd314/stroscot/blob/master/src/model/MemoryStatus.hs`__ .

Operations
----------

Various functions record different statuses for chunks of memory. Memory functions check the status of memory before operating, hence preventing common errrors like double free, access to undefined memory, null pointer dereferencing, etc. Similarly inaccessible memory cannot be read/written, preventing use after free.

Most addresses will not be allocated (status Free/Unknown), hence the array is sparse in some sense. It is in fact possible to implement the typical `sparse array operations <https://developer.android.com/reference/android/util/SparseArray>`__. There are functions to directly allocate memory at an address. Reading and writing are done directly in assembly. The list of currently mapped pages can be had from ``/proc/self/maps`` and `VirtualQueryEx <https://reverseengineering.stackexchange.com/questions/8297/proc-self-maps-equivalent-on-windows/8299>`__, although this has to be filtered to remove pages reserved by the kernel and internal pages allocated by the runtime, and looks slow - it's easier to wrap the allocation functions and maintain a separate list of user-level allocations. Clearing mappings, hashing memory, and indexing by mapped pages all work when restricted to the list of user pages. It's a little more complicated than simple sparsity because there are many different statuses and the operations overlap.

In practice fixed-address allocation is never used and instead there are ``mmap NULL`` and ``malloc`` which allocate memory with system-chosen location. This means that the program behavior must be observationally equivalent no matter what addresses the system picks. The limitations on the system's choice are that the allocation must be suitably aligned and disjoint from all unrevoked allocations. (The system can also return an out of memory error, but this doesn't have to result in equivalent behavior so it can be ignored.)

There is also the C library API alloc/realloc/free for non-page-sized allocations.

The memory management system uses the pointer API internally, just with a special status tag to avoid overlapping with user data.

Optimizing access
-----------------

Eliminating pointer reads amounts to tracking down the matching pointer write, which can be accomplished by tracing control flow. Eliminating pointer writes requires proving that the address is never read before deallocation, which requires a global analysis of pointer reads. They're both a bit tricky as they have to make assumptions about what pointers foreign code will use and analyze the possible values a dereference may take. But, should be possible.

.. _destructors:

Destructors
===========

Destructors allow the prompt freeing of allocated memory and resources like thread handles, file handles, and sockets.  A destructor is a magic value created with the operation ``newDestructor : Op Destructor``. It supports equality, hashing, and an operation ``lastUse : Destructor -> Op Bool``. All calls to ``lastUse`` but the last in the program return false; the last ``lastUse`` returns true. There is also ``useForever : Destructor -> Command`` which ensures that ``lastUse`` always returns false.

Stroscot checks a no-leak property for each destructor ``x`` that exactly one of the following holds:
* ``lastUse x`` is called infinitely often, returning false each time
* ``lastUse x`` returns true and is never called thereafter
* ``useForever x`` is called

If the control flow does not allow this no-leak property to hold, Stroscot will error.

::

  reduce (NewDestructor c) =
    f = freshSymbol
    reduce (c f)
  reduce (Use f c) =
    if will_call (Use f) c
      reduce (c False)
    else if !(could_call (Use f) c)
      reduce (c True)
    else
      error

TODO: can it be shared. need some way to coordinate control flow analysis across threads

Finalizers
==========

Finalizers are a more relaxed approach to resource management. It is a magic value created with the one-argument function ``newFinalizer : (free : Command) -> Op Finalizer``. It supports equality, hashing, and a command ``use : Finalizer -> Command``.

The semantics is that ``free`` will be called as soon as it is known that ``use`` will no longer be called. The general transformation:

::

  reduce (NewFinalizer free c) =
    f = freshSymbol
    transform (c f) {free,f}

  transform : Task -> Task
  transform c =
    if could_call (Use f) c
      let c' = continuation c
      c { continuation = transform c' }
    else
      reduce (free {continuation = c})

Destructors are very similar to finalizers. In fact we can use destructors to implement *prompt* finalizers, that guarantee ``free`` is called immediately after some ``use``:

::

  newPromptFinalizer free =
    d = newDestructor
    let f = PromptFinalizer free d
    use f
    return f
  use (PromptFinalizer free d) =
    l = lastUse d
    if l
      free

However, a prompt finalizer would give an error on programs such as the following:

::

  free = print "Freed."
  f = newFinalizer free
  use f
  b = input Bool
  if b
    print "A"
    use f
  else
    print "B"

With a normal finalizer, instead of erroring, Stroscot will insert a call to ``free`` before the ``print "B"`` statement in the else branch.

Finalizers are as prompt as prompt finalizers, on the programs where prompt finalizers do not error. With this guarantee, finalizers subsume manual memory management. Taking a program written with standard ``malloc/free``, we can change it:
* ``malloc`` is wrapped to return a tuple with ``newPromptFinalizer``, ``free`` is replaced with ``use``
* every operation is modified to call ``use``
* the prompt finalizer is replaced with a finalizer

The finalizer program compiles identically to the original. Note that this transformation is a bit fragile though - if the ``use`` corresponding to the ``free`` is deleted, the lifetime of the finalizer is shortened and depending on the program structure the point at which ``free`` should be called may become hard to compute. But hopefully the analysis will be fairly robust and able to handle most cases.

If multiple finalizers simultaneously become able to call ``free``, the finalizers are run in the order of creation, first created first.

References
==========

An reference is a symbolic index into a global associative array of objects, ``Map Reference Object``. Operations on references are stateful and include allocation, reading, and perhaps writing. But references can be compared for equality and hashed to an integer. References can be packed to a 64-bit word and unpacked to the identical reference. The value of the word is internal to the memory system but can be assumed to be in pointer format.

Pointer conversion
------------------

A reference has at least one pointer associated with it. There can be multiple copies of the data hence multiple pointers. GC can move/copy the reference so the set of pointers varies over time.

Often operations are simpler with pointers, so you can access the pointer from a wrapped block, ``withPointer ref { \address -> doWhatever address }``, locking the object in place for the duration of the operation. The alignment of the pointer can be specified when the reference is constructed, ``var x { alignment = ... }``. The default is no alignment, to allow packing data compactly, although of course the location may be aligned anyway.

Types
-----

Immutable
_________

::

  x = imm [1,2]
  read x # [1,2]

  # if the value is a list
  x[0] # 1

The contents of an immutable reference are fixed once created, i.e. an immutable reference cannot be written. It can be freely shared across threads.

Reading generally uses the memory in-place. It is almost a pure operation, except that the read operation prolongs the lifetime of the reference, hence for GC purposes the read operation must have a definite timestamp.

Variable
________

::

  x = mut [0,0]
  x := [1,2]
  read x # [1,2]

  # if the value is a list
  x[1] := 3
  x[1] # 3

A variable is a thread-local reference that can store arbitrary packable values. Thread local means that reading/writing from a different thread than the owning thread returns an error. You can get/set the owner with ``getOwner/setOwner``. Initially the thread that allocates the variable owns it.

Reading elides the copy if the reference is dead after the read, otherwise copies.

Shared memory
_____________

::

  x = mem [1,2]

A reference to shared memory is a fixed-size array of contiguous bits. The size is restricted to a multiple of bytes or words depending on the ISA. The operations are defined by the CPU; you can use fences, atomics, whatever is in the ISA.

Reading always copies (into a register, usually).

::

  x[1] := 3
  x[1] # 3 or the value from some other thread

Each word is its own reference; this uses the word sized load-store operations of the ISA.

Thread-local storage
____________________

::

  x = tls 0

See https://docs.microsoft.com/en-us/windows/win32/procthread/using-thread-local-storage

Essentially it's a shared memory variable that stores ``Map ThreadId Word``, and each thread only sees/writes its own id. So in that sense it behaves similarly to a variable, but OTOH all threads can use it.

Symbol
______

A symbol can be thought of as a type of reference. It supports neither reading nor writing, but has an identity.

Wrapper
_______

A wrapper reference is a custom implementation of the basic allocate/get/set operations. There's no special support needed in the language for this beyond overloading.

::

  x := [3,4]
  read x # [3,4]

For example, there is a wrapper API around shared memory to allow load/store of non-word values; it packs and writes or reads and unpacks, erroring if the value doesn't fit in the array. Hence a 1-word shared memory reference can be used like a variable containing a word.