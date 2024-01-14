Resource management
###################

Resources are things that can be acquired and released, and are available in limited quantities due to OS or physical limitations. Examples include memory allocations, file handles, internet sockets, mutexes/locks, process table entries, and process identifiers (PIDs). A resource leak happens if the program does not promptly release a resource it has acquired after the program is finished with the resource. A resource management technique prevents resource leaks by releasing resources promptly.

It is possible to manage resources by hand, but it is quite error-prone and tedious. Automatic resource management aims to provide a concise and safe replacement. Stroscot's solution is called "destructors" or "finalizers" or something like that.

Manual approach
===============

The manual approach is to release sequentially after acquiring, for example::

  f1 = open "file.txt"
  f2 = open "file2.txt"
  do_something f1 f2
  close f1
  do_something_2 f2
  close f2

There are several issues:

* Early exit leak: There is a resource leak if do_something contains a return, exception, goto, break, or continue out of the do_something block. `Raymond Chen <https://web.archive.org/web/20201017213150/https://devblogs.microsoft.com/oldnewthing/?p=36783>`__ complains about C++ macros hiding return and goto statements in the function body.
* Failed acquire: if open fails, do_something and close will be called on an invalid value. But with automatic exception propagation this will be fine since ``close InvalidHandle = InvalidHandle``.
* Adjacency: If do_something is long, the release is far from the acquisition, so manual inspection cannot easily identify if the corresponding release function is actually called.
* Encapsulation: the release requirement exposes a detail of the value, namely that it is a resource. This means it cannot be stored in a data structure or returned from a function without special care to call release later.
* Interleaving: Not a problem here, but many solutions impose the requirement that f2 is freed before f1, a LIFO nesting.

try-finally
===========

The try-finally modifies the basic approach by marking the do_something and release blocks explicitly. There are two patterns in Java 6, `standard pattern <http://www.javapractices.com/topic/TopicAction.do?Id=25>`__ and open-in-try (`here <https://stackoverflow.com/questions/2699209/java-io-ugly-try-finally-block>`__)::

  f = open "file.txt"
  try:
      do_something f
  finally:
      close f

  f = var InvalidHandle
  try:
      f := open "file.txt"
      do_something f
  finally:
    if(f != InvalidHandle)
      close f

The unwinding protection solves the early exit leak. The failed acquire again doesn't really matter. This pattern does not solve adjacency, encapsulation, or interleaving, meaning that it is still awkward and verbose. For adjacency, throws act as go-downs and you have to scan down almost the whole function to the finally block to see what cleanups are waiting and if an allocation has a matching clean-up. Also there is an indentation pyramid with nested try-finally.

try-with-resources
==================

The try-with-resources solves adjacency by making ``close`` a Java built-in interface, but it still can't do encapsulation or interleaving::

  try(f = open "file.txt")
      do_something f

In particular `chained resources <https://stackoverflow.com/questions/12552863/correct-idiom-for-managing-multiple-chained-resources-in-try-with-resources-bloc>`__ are awkward.

C# has a similar ``using (f = newThing) { ... }`` syntax, and Python has ``with``.

Bracket
=======

Haskell has a ``bracket acquire release action`` combinator which functions similarly to try-with-resources. It similarly doesn't support interleaving or encapsulation.

The `D guy <https://vimeo.com/97329153>`__ claims you need an even larger combinator than bracket, which goes as follows:

::

  if acquire then
    when !next
      rollback
    release

With masking this looks like:

::

  generalBracket acquire next rollback release =
    mask $ \unmasked -> do
      resource <- acquire
      b <- unmasked (next resource) `catch` \e -> do
        _ <- rollback resource
        _ <- release resource
        throw e
      c <- release resource
      return (b, c)

acquire has to be masked because there could be an async exception between the action and running next.

Scope guard
===========

This approach involves a "deferred release", which is called when the scope is exited. For example::

  f = open "file.txt"
  defer (close f)
  do_something f

It is available in C++ as the ScopeGuard class, in Go and Zig as the defer statement, and in D via the `scope <http://ddili.org/ders/d.en/scope.html>`__ keyword, where there are ``scope(exit)``, ``scope(success)``, and ``scope(failure)`` (referring to exiting the scope unconditionally as with ``defer``, without exception, or by exception respectively).

It does solve early exit.

The pattern nests in the sense that ``defer``-ed statements are run latest-defined to earliest-defined on exit, but again doesn't allow interleaving.

It solves adjacency by putting the cleanup before the code that uses the resource, right after the code that's being cleaned up after. It's easy to verify that allocations match up with their nearby cleanups, but the clean-up is delayed to the scope's exit. The syntax is very compact, just specify a cleanup function, but is a statement rather than an expression, meaning that the acquire must also be a statement.

``defer`` and ``scope`` do not allow returning a resource from a function and skipping the cleanup. They also introduce memory allocation questions since the deferred expressions can capture local variables. So no encapsulation.

Goto cleanup
============

C has this pattern::

  f = open "file"
  if (f == InvalidHandle)
    return
  if (isException (do_something f))
    goto cleanup
  if (isException (do_something_2 f))
    goto cleanup
  cleanup:
    close f

This suffers from early return but addresses adjacency in that the cleanup label is present and can handle interleaving with conditionals. It is not encapsulated though and it is easy to mess up handling a failed acquire.

Garbage collection
==================

Java's GC almost solves the problem by making closing automatic. It looks like::

  f = open "file.txt"
  do_something f

It solves all the issues of early exit, adjacency (implicit in the open), nesting, and encapsulation (the GC is based on global program flow). Unfortunately, Java's GC does not guarantee it will promptly call the finalizer, so in practice the semantics are not usable.

RAII
====

C++ uses RAII, which looks like GC, but uses deterministic memory management, such as stack allocation or reference counting. It ensures you cannot insert any failure points between the allocation and the start of the cleanup's scope. A resource can even be allocated with its corresponding cleanup in the middle of an expression. But defining a new class for every cleanup operation is tedious - fortunately in C++0x it is possible to define one "RAII lambda" class and be done. C++'s semantics define lambda memory handling so there is no allocation issue.

With C++17 it is apparently possible to copy/move RAII types and store them in a data structure or return them from a function, but it seems fragile. In case of a complicated object graph, such as multiple objects sharing a resource, RAII falls down because the available strategies for deterministic memory management are insufficient.

Destructors
===========

Design
------

Stroscot's destructors (or finalizers, or whatever name) are inspired by C++'s RAII and Java's GC. I am still waffling on the name because they behave a lot more like C++-style RAII, but they are also not quite as prompt, closer to Java's GC finalizers. Java's finalizers have inherent problems because they are associated with GC. In particular, because the GC may not run, Java's finalizers have no guarantee of timeliness or ordering, and hence cannot be used to free resources. In contrast, with the "automatic static" analysis of :cite:`proustASAPStaticPossible2017`, Stroscot's destructors free as soon as it is statically known that they are no longer used, so they are much closer conceptually to the stack or reference counting discipline of RAII, even though they have the syntactic cleanliness of a GC solution.

It is not really a big assumption that there is exactly one best place to release the resource, "immediately after the last use of the operation". Semantics-wise, identifying the best location for the release operation is a static, completely solvable problem. It is just of a high complexity :math:`\Sigma^0_1`. So if we are willing to accept potentially long compile times, we can eliminate resource management errors. Such an analysis is more precise than traditional GC, because GC looks at what references are "in scope" and cannot free an unused reference embedded in a structure whose other parts are in use.

Compared to Java's finalizers and C++'s RAII, I made destructors functions, not directly associated with any objects. This avoids issues with resurrection and so on - the destructor is just a lambda so it can do whatever and still be freed without issues.

Similarly to ``defer``, the cleanup is placed at the top, near or within the acquire call.

Destructors directly implement D's ``scope(exit)``, and with an extra flag variable they can implement ``scope(success)`` and ``scope(failure)``:

::

  scope_failure rollback =
    flag = mut true
    f = newDestructor (if flag then { rollback } else {})
    ScopeFailure flag f

  use (ScopeFailure flag f) =
     flag := false
     use f


  s <- scope_failure rollback
  code
  use s

Similarly to ``finally``, there is a marker for the end of the destructor, ``use``. `Herb Sutter <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4152.pdf>`__ claims that the extra ``use`` is "tedious and fragile" and forces the programmer to think about the placement of failure determination. One can define a proper ``scope_failure rollback { ... }`` block structure, and even redefine return/continue/break to not run the rollback. But personally I think the destructors are more powerful because they allow interleaving scopes. The combinators only allow nesting which isn't as expressive. In Haskell land Snoyman created `ResourceT <https://hackage.haskell.org/package/resourcet>`__ even though there was ``bracket``, exactly because of this.

Destructors allow nesting, in the natural way ``newDestructor a; newDestructor b; ...; use b; use a``, and interleaving, by reordering to ``use a; use b``. Destructors also support encapsulation such as returning the allocated resource. It is also possible to embed the destructor in a resource handle and use RAII style programming - each operation calls ``use`` and extends the destructor's life.

Formal definition
-----------------

More formally, a destructor is a magic value created with the one-argument function ``newDestructor : (free : Command) -> Op Destructor``. It supports equality, hashing, and command ``use : Destructor -> Command`` and ``useForever : Destructor -> Op Command``. The semantics is that ``free`` will be called as soon as it is known that ``use`` and ``useForever`` will not be called. Calling ``use`` delays finalization until after the ``use``, and ``useForever`` cancels the destructor and returns the free operation. The general transformation:

::

  reduce (NewDestructor free c) =
    f = freshSymbol
    transform (c f) {free,f}
  reduce (Use f c) = c
  reduce (UseForever f c) = c free

  transform : Task -> Task
  transform c =
    if !(could_call (Use f) c || could_call (UseForever f) c)
      reduce (free {continuation = c})
    else if will_definitely_call (UseForever f) c
      reduce c
    else
      if not (will_definitely_call (Use f) c)
        info("Delaying destructor due to conditional usage")
      let c' = continuation c
      reduce (c { continuation = transform c' })


Non-prompt finalization
-----------------------

Destructors do not really free memory "immediately after the last use", as the `info("Delaying destructor due to conditional usage")`` message points out. Rather they free "immediately before the first point of non-use". This distinction is clear when the location to free depends on further input:

::

  af = print "a"
  a = newDestructor af
  b = input Bool
  if b then
    print "b"
    exit
  else
    print "c"
    use a
    print "d"
    exit

Because ``a`` might be used in the else branch, it cannot be freed between the ``newDestructor af`` and ``input Bool`` statements, even though this would be the earliest place to free for a "true" input. Instead, ``a`` is freed as soon as it is known it will (unconditionally) not be used, hence this program is equivalent to:

::

  af = print "a"
  b = input Bool
  if b then
    af
    print "b"
    exit
  else
    print "c"
    af
    print "d"
    exit

Non-prompt finalization can be made into an error/warning if prompt memory management is desired.

I also started out with a different API, instead of a ``free`` operation that gets called at random times, destructors had this operation ``lastUse : Destructor -> Op Bool``, that returns false for every call except the last. The pattern for using this API is to make each use check if it's the ``lastUse``, like ``use (PromptDestructor free d) = { l = lastUse d; if l { free } }``. But eventually I proved that this API is equivalent to the current API: the use function works to implement the current API with the old (minus the non-prompt finalization), and you can implement the old API with the current one::

  isFinalized = mut false
  f = newDestructor { isFinalized := true }
  lastUse =
    use f
    read isFinalized

This implementation of destructors works just fine in programs where the warning is not triggered. But with non-prompt finalization, delaying until known, the contract is not valid because ``lastUse`` could return false even though it is the last use (delaying the destructor after the read).

Subsuming manual memory management
----------------------------------

By construction, destructors without the warning are as prompt as destructors with the warning, on the programs where the warning does not trigger. In particular, destructors subsume prompt destructors subsume manual memory management. Taking a program written with standard ``malloc/free``, we can gradually change it:

1. ``malloc`` is wrapped to return a tuple with ``newDestructor``, ``free`` is replaced with ``use``
2. every operation is modified to call ``use``
3. the destructor warning is turned off
4. The ``use`` calls corresponding to ``free`` are removed

Up until step 4, the destructor program compiles identically to the original. It's step 4 that's a bit fragile - the lifetime of the destructor could be shortened and, depending on the program structure, the point at which ``free`` should be called may become much harder to compute. But hopefully the analysis will be fairly robust and able to handle most cases. At worst, the programmer will have to provide additional help to the destructor analysis in the form of inserting the ``use`` statements corresponding to ``free``. Either way, since all operations call ``use``, the program behavior is not changed, only its resource management.

Destructor order
----------------

If multiple destructors simultaneously become able to call ``free``, then destructor instruction insertions are run in the order of creation, first created first. This means the free calls will execute most recent first, e.g. if there is an exception.

::

  a = newDestructor (print "a")
  b = newDestructor (print "b")

  if randomBool then
    print "c"
    exit
  else
    print "c"
    use a
    use b
    exit

  # when bool is false: cab
  # when bool is true: bac

Freed on exit
-------------

Many resources are automatically freed by the OS on exit: memory, file handles, etc. This automatic freeing is generally more efficient than releasing each resource one by one. So as an optimization, one would like to *not* free these resources earlier, but rather hold on to them until the program exits and the OS frees them itself. So what we need is an analysis that determines at what point in the program there are sufficient spare resources that any further allocation can be satisfied without deallocation. This measure "the remaining amount of additional memory the program might use" will not necessarily be compared against the remaining memory amount of free physical memory actually available, but more likely a configurable parameter like 2MB. Once this point is determined the compiler can insert ``useForever`` calls to mark all the in-use resources as not needing manual finalization.

Sloppy frees
------------

GC is more composable and it can also be faster than manual memory management :cite:`appelGarbageCollectionCan1987`. As Appel points out, even if freeing an individual object is a single machine instruction, such as a stack pop, freeing a lot of objects still has significant overhead compared to copying out a small amount of useful data and just marking a whole region of objects as free. In a similar vein, sometimes we do not actually want the destructor to run as promptly as possible, but rather batch it with other allocations and free it all in one go. The opportunities for batching are hard to detect and even harder to implement by hand. Setting some "slop factor" of memory that can be delayed-freed is quite useful - the only downside is that if the program is pushing the limits of memory maybe it will crash at 1.9GB instead of 2GB.

Really, we are distinguishing "unused" or "dead" memory from memory that is released back to the OS or the rest of the program.

Evaluation order
----------------

There are also "space leaks" where memory could be freed earlier by evaluating expressions in a specific order but some other order is chosen. Certainly there is some evaluation order that results in minimum RAM usage, but maybe a less compact order is more time-efficient. So there is some amount of time-space tradeoff for this category. Destructors kind of skirt this issue by being completely imperative, but with unsafePerformIO this becomes relevant again.

On borrowing
------------

Rust has gotten much attention with the borrow checker, documented in :cite:`weissOxideEssenceRust2019`. Similar to destructors, Rust also has a concept of the "lifetime" of each reference. But, whereas in Stroscot the lifetime is simply the set of program states during which the reference is not dead, in Rust a lifetime is a *region* consisting of annotating each program point with the set of *loans* of the reference, where each loan is either unique or shared. At each point, a reference may have no loans, one unique loan, or many shared loans - no other possibilities are allowed. This restrictive set of allowed access patterns means that Rust does not allow simple cyclic pointer patterns such as doubly-linked lists.

Similarly, Val's `mutable value semantics <https://www.jot.fm/issues/issue_2022_02/article2.pdf>`__ is even more restrictive than Rust, dropping references altogether and simply using the function parameter annotation ``inout``. But it once again cannot represent any sort of cyclic pointer structure. It is really just the trick for representing state as the type ``s -> (a,s)``, and doesn't handle memory management at all.

In practice, Rust developers have a variety of escapes from the borrow checker.  code frequently switches to the ``Rc`` reference counted type, which besides cycles has the semantics of GC. There is even a `library <https://github.com/Others/shredder>`__ for a ``Gc`` type that does intrusive scanning.

Per :cite:`proustASAPStaticPossible2017`, destructors and the "free after last use" criterion subsume both region-based memory management and reference counting. :cite:`corbynPracticalStaticMemory2020` implemented a buggy incomplete version and showed even that version is comparable to Rust.

Implementability
----------------

If doing automatic static memory management is so easy, why hasn't it been tried before? Well, it has. For example, :cite:`guyerFreeMeStaticAnalysis2006` has a similar notion of automatically inserting frees, and they report good results. But that paper focused on reachability, rather than lack of use, and their analysis was local to function blocks, rather than global. So it didn't see much adoption.

:cite:`proustASAPStaticPossible2017` presented the theory and the formulation of the problem fairly well, but he fell into the trap of thinking that since the complexity of determining "waste blocks" was :math:`\Sigma_0^1`, any analysis had to be approximate. There are techniques for solving such high-complexity problems precisely, as evidenced in the TERMCOMP termination analysis competition, but such techniques really only got started in 2007 or so. From his citations list, Proust didn't really get into this area of the literature.

So the answer is, it seems novel to try to apply techniques from formal verification to memory management, and that's the only technique that seems powerful enough to implement destructors in the way presented here, where the point of finalization is guaranteed. All previous approaches have focused on approximate analyses that aren't powerful enough to subsume manual memory management.

Certainly there is some risk involved in implementing a novel analysis. But it doesn't seem like a `"cursed problem" <https://www.youtube.com/watch?v=8uE6-vIi1rQ>`__ where even trying to solve it is a waste of time - :cite:`corbynPracticalStaticMemory2020` got decent results with just 8 months or so of part-time work. I'd rather be spending a lot of effort on solving the right problem, even if it's hard, than getting sidetracked solving the wrong easy problem.

Exceptions in destructors
-------------------------

What to do when throwing an exception from a destructor? Say we have

::

  x =
    f = newDestructor (throw Bar)
    throw Foo
    use f

  y = x catch \case
          Foo -> print "foo"
          Bar -> print "bar"

Per Stroscot's semantics, the destructor runs as soon as it is known that ``use`` will not be called - i.e. immediately after constructing the destructor, before the ``throw Foo`` (because ``use f`` is unreachable). So ``x`` is equivalent to ``throw Bar; throw Foo``. Then per normal exception semantics ``throw Bar`` wins since it is first and ``y`` outputs ``bar``.

If we had an I/O operation ``writeOrThrow`` instead of ``throw Foo``, then ``use f`` is reachable. So then we have two cases:

* if ``writeOrThrow`` throws, then it is known that ``use`` will not be called. The destructor will be delayed until just before the first I/O operation executed after the ``writeOrThrow``, in this case ``print "foo"``. So ``y`` will exit with a ``Bar`` error without printing anything.
* if ``writeOrThrow`` doesn't throw, then the destructor will execute after the last ``use``. Again ``y`` will exit with a ``Bar`` error without printing anything.

As far as I can tell this is a logical choice. C++ instead decided to terminate the program on the ``throw Bar`` in the destructor. The justification seems to be that it slightly simplified the implementation of unwinding, and that the C++ STL wanted to "arbitrarily require that [destructors] may not throw." (`Herb Sutter <https://ptgmedia.pearsoncmg.com/imprint_downloads/informit/aw/meyerscddemo/DEMO/MAGAZINE/SUTTER.HTM>`__, also Item 16 "Destructors That Throw and Why They're Evil" of his 1999 book Exceptional C++) `John Kalb and David Abraham <http://web.archive.org/web/20130728131646/cpp-next.com/archive/2012/08/evil-or-just-misunderstood/>`__ say "The reason we can’t have throwing destructors is that nobody worked out how to deal with multiple exceptions wanting to propagate through the same set of stack frames. [...] we think termination is a bit draconian. Frankly, we don’t think it’s so hard to nail down the final details of how this should work."

Kalb proposes to "drop the second [destructor] exception on the floor and propagate the original one", but this is a bad idea. Since (per Sutter's convention) destructors generally don't fail, an exception during a destructor is going to be fairly serious, such as an OOM. Ignoring this exception in favor of a trivial logic exception is the wrong approach.

Exception safety
================

Exception safe code is code that works correctly even when exceptions are thrown. The basic issue is in Rust/C++ almost everything is a resource. In Stroscot almost everything is a value, inert data - copy/clone is built into the language and can't fail. Taking away explicit memory management makes it much easier to ensure exception safety. Reasoning about invariants with pure values is straightforward, and fail-fast coding styles mean that the program doesn't live long. And when writing cleanups the programmer is already thinking about exception safety and restoring invariants, so will write an exception-safe cleanup.

Still, what about exception safety for a mutable data structure? C++ has `levels of safety <https://en.wikipedia.org/wiki/Exception_safety>`__ for stateful functions based on what invariants are preserved.

* no-throw means forward progress is guaranteed. It's nice when you can give this, but most functions can fail due to insufficient memory.
* Strong safety means that state transitions happen atomically and a failure will return to the old state. To ensure this one needs basic safety and to copy the relevant data beforehand and write it back afterwards.  This is infeasible for even simple data structures in C++ due to overloaded assignment and copy operators being able to throw.
* Basic safety means that the final state will be valid, i.e. all invariants hold. You need to safeguard against each function call throwing. This requires adding handling code to each call and trusting the documentation for the list of thrown exceptions (or using no-throw).

These levels only work for stateful data structures that call a small and easily auditable set of other functions.

In Stroscot there are two steps to making code exception-safe:

* identify invariants. These can be written as assertions using the pure read operations on stores. With this the static verification will identify the function and the exceptional control flow that breaks the invariant.
* place exception cleanup handlers to restore broken invariants

This code in Rust or C++ is not exception safe: (based on `this code <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/1995/N0623.asc>`__ and `this code <https://github.com/rust-lang/rfcs/blob/master/text/1236-stabilize-catch-panic.md#background-what-is-exception-safety-in-rust>`__)

::

  push_ten_more : (v : Vec T) -> T -> Op { v : Vec (T|uninitialized) }
  push_ten_more (this@(readRef -> Vector arr)) t =
    new_arr = alloc (length arr + 10)
    for (i in indexes arr)
      copy arr[i] to new_arr[i]
      delete arr[i]
    this := Vector new_arr

    for i in 0..10 {
      (ptr v) offset (len + i) := t.clone()
    }
  }

The update to the Vector happens when the next 10 elements are uninitialized, and ``Vec`` has an internal invariant that its elements are safe to deallocate. So if `t.clone` throws then the initialization will not be called. Vec's destructor that assumes the invariant will then free uninitialized memory.

In Stroscot, most values are passed directly, not by reference. So there is no need for copying, cloning, or explicitly calling destructors, and the whole operation can fail only with OOM or an async exception (which is common to most functions so it's left implicit). Similarly the array has no path where it is returned uninitialized. So here is the corresponding Stroscot code::

  push_ten_more : Vec T -> T -> Op (Vec T)
  push_ten_more arr t =
    new_arr = alloc (length arr + 10) uninitialized
    for (i in indexes arr)
      new_arr[i] := arr[i]
    for i in 0..10 {
      arr[len + i] := t
    }

Another example is ``TwoList``, which maintains two mutable linked lists with the invariant that they are the same. What does adding an element look like?

::

  add a (List l) =
    head = read l
    l := Cons a head

  -- not exception safe
  add a (TwoLists l1 l2) =
    add a l1
    add a l2

This is not exception safe for the invariant, because an async exception between the two adds may add a to l1 but not l2. So we fix it by (1) adding the assertion and (2) adding a cleanup handler (following the scope guard destructor pattern).

::

  -- (1) add the assertion
  add : T -> { TwoList l1 l2 | freeze l1 == freeze l2 } -> { TwoList l3 l4 | freeze l3 == freeze l4 }
  add a (TwoLists l1 l2) =
    -- (2) add a cleanup handler
    finishedFlag = mut false
    l1_old = freeze l1
    l2_old = freeze l2
    f = newDestructor {
      when not finishedFlag then
        l1 := unfreeze l1_old
        l2 := unfreeze l2_old
    }
    add a l1
    add a l2
    -- disarm cleanup handler
    finishedFlag := true
    use f

Here we have strong safety, i.e. it restores the state if an exception is thrown during the add. If we didn't use the cleanup handler, the assertion would fail because we wouldn't necessarily have strong safety.

Poisoning
---------

Rust has "poisoning" for taking locks, which is a safeguard against trying to use data that was corrupted due to an exception, e.g. a concurrent heap that doesn't have the heap property anymore. Poisoned data will tear down all connected threads.

Essentially it uses the scope_failure cleanup to poison the lock on failure:

::

  getGuard mutex =
    acquireLock mutex
    flag = mut true
    f = newDestructor {
      if flag then
        poisonLock mutex
      releaseLock mutex
    }
    Guard flag f

  finishGuard (Guard flag f) =
    flag := false
    use f

Once the lock is poisoned then locking the mutex will throw an exception.

However, the invariant checking approach to exception safety discussed above will throw an exception anyway when trying to use a data structure operation on a corrupted data structure. It also covers the single-threaded case where no mutex is used but the recovery from an exception is incomplete. So poisoning isn't really that useful. But a PoisoningMutex can at least be in the standard library for those who want the functionality.

