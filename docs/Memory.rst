Memory
######

word
  An integer ``i`` with ``0 <= i < MAX``. Since ternary computers `might <https://www.extremetech.com/computing/295424-back-off-binary-samsung-backed-researchers-debut-ternary-semiconductor>`__ eventually become popular, you should not assume that ``MAX`` is a power of 2, but for all practical purposes this will be. Hence a word corresponds to some fixed number of logical bits (0/1).

There are two main models of memory. The concrete model models memory as a an integer-indexed array of 2^32 or 2^64 words. The symbolic model models memory as an associative array from symbols (potentially infinite in number) to "cells", arrays of words of various lengths. In Stroscot these models correspond to pointers and references respectively. Combinations of these can be made, for example the "quasi-concrete model" which uses a data type that starts out containing a reference, implements various arithmetic operations symbolically, but switches to a pointer once an integer address is requested. :cite:`kangFormalMemoryModel2015`

Value representation
====================

Layout is usually defined by its size, alignment, padding/stride, and field offsets, but this only specifies the representation of simple flat records. With enumerations, there is the question of how to encode constants. It gets even more complicated with ADTs, like JS's `value type <https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations>`__, and the choices often impact performance significantly. Finally there is the use of pointers. For example, we can encode a list in a number of ways:

::

  ["a","b"]
  # flat list, stored like [2,"a","b"] or [1,"a",1,"b",0]
  # intrusive list, stored like x=[1,"a",&y], y=[1,"b",&0]
  # uniform list, stored like x=[1,&x1,&y],x1="a",y = [1,&y1,&0],y1="b"

So in Stroscot there is no fixed memory representation. Instead memory layout is defined by overloaded ``pack``/``unpack`` functions, similar to the `store library <https://github.com/mgsloan/store/blob/master/store-core/src/Data/Store/Core.hs>`__. Unlike Narcissus :cite:`delawareNarcissusCorrectbyconstructionDerivation2019` we don't have a state parameter, because the pack/unpack functions can take implicit parameters. To prevent mismatches unpack is not top-level and the result of pack is actually a tuple ``(buffer,unpack)`` containing the matching unpack function.

::

  pack : a -> (Write, unpack : Read -> a)

  newWrite : inout (w : Write) -> Write
  write : inout (w : Write) -> (data : Bits) -> (mask : Bits) -> ()
  shift : inout (w : Write) -> Write -> (offset : Int) -> ()

  read : ReadBuffer -> (mask : Bits) -> Vector Word8
  shift : ReadBuffer -> (offset : Int) -> ReadBuffer

The mask creates / filters out undefined bits. For example ``010`` masked with ``101`` produces ``0*0`` and then the runtime is free to use the center bit for garbage collection purposes.

Some simple types:

::

  pack : Either a b -> WriteBuffer
  pack (Left a) =
    w = emptyWriteBuffer
    write w 0
    write w (pack a)
    (w,unpack)
  pack (Right b) =
    w = emptyWriteBuffer
    write w 1
    write w (pack b)
    (w,unpack)

  unpack r =
    case read r of
      0 -> Left (unpack (shift r 1))
      1 -> Right (unpack (shift r 1))

  pack : (a,b) -> WriteBuffer
  pack (a,b) =
    w = emptyWriteBuffer
    write w (pack a)
    write w (pack b)
    (w,unpack)

  unpack r =
    a = unpack r
    l = length (pack a)
    b = unpack (shift r l)
    (a,b)

``unpack`` can fail on invalid byte sequences, but ``pack`` must always return a byte sequence. Also ``unpack`` can be more lenient and decode sequences that ``pack`` doesn't produce. ``pack`` may narrow the range of values, e.g. rounding 1.23 to 1.2.

So for correctness we require ``pack . unpack . pack = pack``. Using this constraint we can derive ``unpack`` from ``pack``, or vice-versa, if the format isn't too complicated.

One tricky part is that the naive way to specify types interferes with overloading, subtyping and implicit conversions. ``pack (Int8 1)`` can give a byte as expected, but it can also implicitly convert to an ``Int32`` and give 4 bytes. Since we have dependent types this isn't a real issue, just make sure the code generated after representation specialization passes the type explicitly: ``pack Int32 (Int8 1)``.

A few things need to optimize away for reasonable performance.  ``length . pack`` should optimize to something like ``const 20`` for most values, or at least something that doesn't allocate, so that field accesses are independent and values can be allocated sanely. These functions might have to be hacked in, specializing to constant-sized values.

Since writing these serialization functions all the time would be tedious, we can make a format DSL that specifies the functions in a nicer way. Although one of these DSL's will be the standard / default, it'll be some kind of macro / constraint system, so defining new format DSLs for specific purposes shouldn't be hard.

Usage
-----

The translation to use pack is pretty simple: every value is wrapped in a call to pack, the result is stored as a tuple ``(cell,unpack)``, and every usage applies unpack to the cell. The translation uses whatever pack is in scope; pack can be overridden like any other implicit parameters. The unpack functions will end up getting passed around a lot, but function pointers are cheap constants, and constant propagation is a thing, so it shouldn't be an issue.

Pointers
========

Pointers are numeric indices into a shared global array. This array is sparse, in that some (most) addresses will not be allocated. The memory array is an array of statuses, which allows detecting and erroring on use-after-free and double free. For example we can implement fenceposts that mark the bytes above / below an allocation as undefined and then we'll get bounds checking. The array is a bit array and the statuses are stored at the bit level because that's the granularity `Valgrind's Memcheck <https://valgrind.org/docs/manual/mc-manual.html#mc-manual.machine>`__ uses.

The status is an ADT:

* on Windows:
  * protection state: one of `Free, Reserved, <https://docs.microsoft.com/en-us/windows/win32/memory/page-state>`__ `NOACCESS, EXECUTE, EXECUTE_READ, EXECUTE_READWRITE, EXECUTE_WRITECOPY, READONLY, READWRITE <https://docs.microsoft.com/en-us/windows/win32/memory/memory-protection-constants>`__
  * additional flags: TARGETS_INVALID, TARGETS_NO_UPDATE, WRITECOPY, GUARD, NOCACHE, or WRITECOMBINE
* on Linux: based on `vm_area_struct <https://elixir.bootlin.com/linux/latest/C/ident/vm_area_struct>`__ / data from /proc/[pid]/maps, /proc/[pid]/map_files, and .
  * state: Free (no further information) or Reserved
  * flags listed in `VmFlags <https://elixir.bootlin.com/linux/latest/C/ident/VM_NONE>`__ in `/proc/[pid]/smaps <https://man7.org/linux/man-pages/man5/proc.5.html>`__ - readable, writeable, etc.
  * memory mapping: anonymous or a file/device mapping (offset, device, inode, pathname, deleted). anonymous is (0,0,0,"",false) and might have a pseudo path - stack, vdso, or heap. We can extend the pseudo path to the list of ELF segments - text, static data, global data.
  * extended status: currently resident in RAM, number of processes sharing it (`Pss <https://stackoverflow.com/questions/9922928/what-does-pss-mean-in-proc-pid-smaps>`__), clean/dirty, marked as `referenced/accessed <https://stackoverflow.com/questions/35391017/the-meaning-of-referenced-in-process-smaps>`__ (i.e. not currently reclaimable), swapped out, locked mapping, memory protection key (see pkeys(7))
* Allocator bits (based on Memcheck):
  * (un)allocated - It is an error to read or write an unallocated location. To properly match the malloc/free pair when using multiple allocators simultaneously, a freeable allocated status stores the set of bits belonging to the allocation and a stateful zero-argument function that frees the allocation.
  * (un)initialized - it is an error to have observable behavior dependent on uninitialized data. But it is not an error to copy uninitialized data around in memory.
  * MMS-internal - This page is managed by the memory management subsystem. Access to this memory must have the implicit argument ``inMMS`` set to true, to prevent accidental corruption.
* Unknown status - Programs using the FFI may have external allocators allocate pages. So besides the initial memory setup, all memory is set to unknown status to indicate that one needs to check if it is in use before using it for anything. Allocations at system-chosen addresses automatically skip already-allocated pages, so they can ignore the unknown status.
* Thread sharing: list of threads that may read/write this memory

when can memory access can be optimized away

TODO: These statuses can be overlapped and mixed. Complete and clean up the list.

 Various functions record different statuses for chunks of memory. Memory functions check the status of memory before operating (prevention of double free). Inaccessible memory cannot be read/written (prevention of use after free).

It is in fact possible to implement the typical `sparse array operations <https://developer.android.com/reference/android/util/SparseArray>`__. There are functions to directly allocate memory at an address, `mmap <https://man7.org/linux/man-pages/man2/mmap.2.html>`__ with MAP_FIXED_NOREPLACE on Linux and `VirtualAlloc <https://docs.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualalloc>`__ on Windows. Reading and writing are done directly in assembly. The list of currently mapped pages can be had from ``/proc/self/maps`` and `VirtualQueryEx <https://reverseengineering.stackexchange.com/questions/8297/proc-self-maps-equivalent-on-windows/8299>`__, although this has to be filtered to remove pages reserved by the kernel and internal pages allocated by the runtime, and looks slow - it's easier to wrap the allocation functions and maintain a separate list of user-level allocations. Clearing mappings, hashing memory, and indexing by mapped pages all work when restricted to the list of user pages.

It's a little more complicated than simple sparsity because there are actually two pairs of operations, reserve/release to manage virtual address space and commit/decommit for backing pages.

In practice direct allocation is never used and instead there are ``mmap NULL`` and ``malloc`` which allocate memory with system-chosen location. This means that the program behavior must be observationally equivalent no matter what addresses the system picks. The limitations on the system's choice are that the allocation must be suitably aligned and disjoint from all unrevoked allocations. (The system can also return an out of memory error, but this doesn't have to result in equivalent behavior so it can be ignored.)

There is also the C library API alloc/realloc/free for non-page-sized allocations.

Eliminating pointer reads amounts to tracking down the matching pointer write, which can be accomplished by tracing control flow. Eliminating pointer writes requires proving that the address is never read before deallocation, which requires a global analysis of pointer reads. The analysis is complex as it has to deal with symbolic intervals but should be possible.

Eliminating pointers entirely is not possible as they have to be used for system calls and interfacing with C. But we can minimize the lifetime of pointers in the standard library to the duration of the call, and use values / references everywhere else.

The memory management system uses the pointer API internally, just with a special tag to avoid overlapping with user data.

unsafe blocks for pointer manipulation - are they necessary?

References
==========

An reference is a symbolic index into an associative array. Operations include allocation, reading, and writing, all of which operate on arbitrary types of values.

Because cells can be reallocated and copied freely by the runtime, and may not be laid out contiguously in memory, there is no conversion from a reference to an address. But references can be compared for equality and hashed to an integer.

Deallocation is done automatically when the symbolic index is no longer used/accessible, similar to GC. Ownership a la Rust cannot even handle doubly-linked lists. Code frequently switches to the ``Rc`` type, which besides cycles has the semantics of GC. There is even a `library <https://github.com/Others/shredder>`__ for a ``Gc`` type that does intrusive scanning. GC is more composable and it can also be faster than manual memory management :cite:`appelGarbageCollectionCan1987`. As Appel points out, even if freeing an individual object is a single machine instruction, such as a stack pop, freeing a lot of objects still has significant overhead compared to copying out the useful data.

A scratch buffer, as exemplified by GNU C's `obstack <https://www.gnu.org/software/libc/manual/html_node/Obstacks.html>`__ seems to just be a reference to an array plus metadata. They don't require any special support AFAICT.

Types of references:

Variable
--------

A variable is a thread-local reference that can store anything.

Shared variable
---------------

These references are limited to storing things that pack to word size (atomically readable/writable), but can be used cross-thread.


.. _destructors:

Destructors
===========

Destructors allow the prompt freeing of allocated memory and resources like thread handles, file handles, and sockets.  A destructor is essentially a magic value, created with the stateful ``newDestructor``. It supports equality, hashing, and a stateful operation ``lastUse``. All calls to ``lastUse`` but the last in the program return false; the last ``lastUse`` returns true. There is also a ``useForever`` call which ensures that ``lastUse`` always returns false.

An example usage is based on `AutoCloseFD <https://android.googlesource.com/platform/system/vold/+/android-7.1.1_r11/AutoCloseFD.h>`__. We store the destructor and the file descriptor in a tuple for brevity - in a real implementation this would use an internal symbol so as to strengthen encapsulation.

::

  open path mode flags =
    fd = sys_open(path, flags | O_CLOEXEC, mode)
    (fd,newDestructor)

  maybeClose (fd,d) = if lastUse d then sys_close fd else {}

  write t@(fd,d) ... =
    sys_write fd ...
    maybeClose t

The determination of which lastUse call is actually the last use is somewhat complex, but for non-branching control flow it's easy to follow. With branches it is possible to get time-travel errors, for example

::

  d = newDestructor
  if lastUse d
    print "Contradiction!"
    lastUse d

In a lot of cases there is no observable difference in which lastUse returns true. E.g. just removing the print statement from the example we'd have nothing depending on lastUse and it can be eliminated. Similarly for ``maybeClose; if ... { maybeClose }`` the control flow is equivalent to having a lastUse at the end outside the conditional. So hopefully these sorts of errors will remain curiosities.

Destructors are inspired by C++ RAII destructors, hence the name. Admittedly the actual API doesn't bear much resemblance. `Finalizers <https://en.wikipedia.org/wiki/Finalizer>`__ can resurrect objects and don't have deterministic execution, hence would be a bad name. Go's defer statement and try-finally are related, but they only work locally and have imprecise execution semantics.

Stroscot checks a fairness property that one of the following holds:
* ``lastUse`` is called infinitely often
* ``lastUse`` returns true
* ``useForever`` is called

In particular (in the absence of ``useForever``) the program may not execute infinitely without calling ``lastUse`` and without ``lastUse`` having returned true.