Resource management
###################

Resources are things that can be acquired and released, and are available in limited quantities due to OS or physical limitations. Examples include memory allocations, file handles, internet sockets, mutexes/locks, process table entries, and process identifiers (PIDs). A resource leak happens if the program does not promptly release a resource it has acquired after the program is finished with the resource. A resource management technique prevents resource leaks by releasing resources promptly.

It is possible to manage resources by hand, but it is quite error-prone and tedious. Automatic resource management aims to provide a concise and safe replacement.

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

GC almost solves the problem by making closing automatic. It looks like::

  f = open "file.txt"
  do_something f

It solves all the issues of early exit, adjacency (implicit in the open), and encapsulation (the GC is based on global program flow). Unfortunately GC does not guarantee it will promptly call the finalizer, so in practice the semantics are not usable.

RAII
====

C++ uses RAII, which looks like GC, but uses deterministic memory management, such as stack allocation or reference counting. It ensures you cannot insert any failure points between the allocation and the start of the cleanup's scope. A resource can even be allocated with its corresponding cleanup in the middle of an expression. But defining a new class for every cleanup operation is tedious - fortunately in C++0x it is possible to define one "RAII lambda" class and be done. C++'s semantics define lambda memory handling so there is no allocation issue.

With C++17 it is apparently possible to copy/move RAII types and store them in a data structure or return them from a function, but it seems fragile. In case of a complicated object graph, such as multiple objects sharing a resource, RAII falls down because the available strategies for deterministic memory management are insufficient.

Finalizers
==========

Stroscot's finalizers are inspired by RAII and GC but free resources "promptly" as opposed to the "eventually" of GC or the stack or reference counting discipline of RAII. Similarly to ``defer`` the cleanup is placed near the acquire but similarly to ``finally`` there can be a marker for the end of the scope. The pattern is ``newFinalizer f; ... ; use f; ...; use f``. The finalizer will run after the last ``use`` if code flows normally and soon after raising an exception otherwise. Finalizers allow nesting, in the natural way ``newFinalizer a; newFinalizer b; ...; use b; use a``, and interleaving, by reordering to ``use a; use b``. But the execution order on an exception is latest-defined-first-run. Finalizers also support encapsulation such as returning the allocated resource. It is also possible to embed the finalizer in a resource handle and use RAII style programming - each operation calls ``use`` and extends the finalizer's life.

Finalizers directly implement D's ``scope(exit)``, and with an extra flag variable they can implement ``scope(success)`` and ``scope(failure)``:

::

  scope_failure rollback =
    flag = mut true
    f = newFinalizer (if flag then { rollback } else {})
    ScopeFailure flag f

  use (ScopeFailure flag f) =
     flag := false
     use f


  s <- scope_failure rollback
  code
  use s

`Herb Sutter <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4152.pdf>`__ claims that the extra ``use`` is "tedious and fragile" and forces the programmer to think about the placement of failure determination. One can define a proper ``scope_failure rollback { ... }`` block structure, and even redefine return/continue/break to not run the rollback. But personally I think the finalizers are more powerful because they allow interleaving scopes. The combinators only allow nesting which isn't as expressive. In Haskell land Snoyman created `ResourceT <https://hackage.haskell.org/package/resourcet>`__ even though there was ``bracket``, exactly because of this.

Exceptions in finalizers
========================

What to do when throwing an exception from a finalizer? Say we have

::

  x =
    f = newFinalizer (throw Bar)
    throw Foo
    use f

  y = x catch \case
          Foo -> print "foo"
          Bar -> print "bar"

Per Stroscot's semantics, the finalizer runs as soon as it is known that ``use`` will not be called - i.e. immediately after constructing the finalizer, before the ``throw Foo`` (because ``use f`` is unreachable). So ``x`` is equivalent to ``throw Bar; throw Foo``. Then per normal exception semantics ``throw Bar`` wins since it is first and ``y`` outputs ``bar``.

If we had an I/O operation ``writeOrThrow`` instead of ``throw Foo``, then ``use f`` is reachable. So then we have two cases:

* if ``writeOrThrow`` throws, then it is known that ``use`` will not be called. The finalizer will be delayed until just before the first I/O operation executed after the ``writeOrThrow``, in this case ``print "foo"``. So ``y`` will exit with a ``Bar`` error without printing anything.
* if ``writeOrThrow`` doesn't throw, then the finalizer will execute after the last ``use``. Again ``y`` will exit with a ``Bar`` error without printing anything.

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

In this code the Rust/C++ semantics require calling a destructor ``delete`` on each element of a ``vec``, and copying values with ``copy_to_`` and ``clone`` operations that can fail -  Similarly we wouldn't necessarily call any finalizers (``delete``) - the finalizer is called after the last use, and likely there are other copies and this is not the last use. Even if the Stroscot code was written to call an operation ``clone`` that could throw exceptions, the rest of the elements will be deallocated if needed, but otherwise not. In all cases memory is safe due to the finalizer semantics.

Another issue is the uninitialized array. This means the array may be filled with ``uninitialized`` values (exceptions). The result type reflects this possibility. With careful rewriting, the code can provide the strong guarantee that the resulting vector only contains values of type T. This can be done by extending the array one element at a time or by saving the exception(s) thrown in a separate list and rethrowing at the end as an exception group.

A simple example is ``TwoList``, which maintains two mutable linked lists with the invariant that they are the same. What does adding an element look like?

::

  add a (List l) =
    head = read l
    l := Cons a head

  add a (TwoLists l1 l2) =
    add a l1
    add a l2

But this is not exception safe for the invariant, because an async exception between the two adds may add a to l1 but not l2.

If add is no-throw we can fix this just by adding uninterruptibleMask. But add allocates so can throw out of memory. But remove is no-throw so we can use remove:

::

  add a (TwoLists l1 l2) =
    mask_ $
      case try (add a l1) of
        Err e -> throw e
        Ok ->
          case try (allowInterrupt >> add a l2) of
            Ok -> return
            Err e -> uninterruptibleMask (remove a l1) >> throw e

Here add should have strong safety, i.e. it restores the state if an exception is thrown during the add.

Poisoning
---------

Rust has "poisoning" for taking locks, which is a safeguard against trying to use data that was corrupted due to an exception, e.g. a concurrent heap that doesn't have the heap property anymore. Poisoned data will tear down all connected threads.

Essentially it uses the scope_failure cleanup to poison the lock on failure:

::

  getGuard mutex =
    acquireLock mutex
    flag = mut true
    f = newFinalizer {
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

