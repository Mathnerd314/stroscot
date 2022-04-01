Library
#######

Minimal definition
==================

At a minimum, the standard library should provide:
* containers, such as hash maps and binary trees
* algorithms operating over those containers, such as sorting
* basic support for multithreading
* string tokenization
* compiler's API

Poison values
=============

This requires some support from the OS to implement. Pointer reads generate page faults, which if they are invalid will be returned to the program via the signal "Segmentation fault" (SIGSEGV). C/C++ `can't handle these easily <https://stackoverflow.com/questions/2350489/how-to-catch-segmentation-fault-in-linux>`__ because they are `synchronous signals <https://lwn.net/Articles/414618/>`__ and synchronous signal behavior is mostly left undefined, but in fact signals are `fairly well-behaved <https://hackaday.com/2018/11/21/creating-black-holes-division-by-zero-in-practice/>`__ (`OpenSSL <https://sources.debian.org/src/openssl/1.1.1k-1/crypto/s390xcap.c/?hl=48#L48>`__'s method of recovering from faults even seems standards-compliant). It definitely seems possible to implement this as an error value in a new language. Go `allows <https://stackoverflow.com/questions/43212593/handling-sigsegv-with-recover>`__ turning (synchronous) signals into "panics" that can be caught with recover.

UDIV by 0 on ARM simply produces 0. So on ARM producing the division by 0 error requires checking if the argument is zero beforehand and branching. The people that really can't afford this check will have to use the unchecked division instruction in the assembly module, or make sure that the check is compiled out. But on x86, DIV by 0 on produces a fault, which on Linux the kernel picks up and sends to the application as a SIGFPE. So on x86 we can decide between inserting a check and handling the SIGFPE. It'll require testing to see which is faster in typical programs - my guess is the handler, since division by zero is rare.

Relations
=========

There are various types of relations: https://en.wikipedia.org/wiki/Binary_relation#Special_types_of_binary_relations

The question is, what data types do we need for relations?

* Function: we need functions, obviously.
* Functional: This is a function too, just add a "no clause defined" element.
* One-to-one: a function with an assertion, ``assume(forall x y; if f x == f y { assert x == y}``
* Many-to-one: A function, no constraints
* Injective: This is the converse of a function, just use the function.
* One-to-many: the converse of a function, again just use the function.

So the only relation that can't be represented by a one-argument function is a many-to-many relation. Here we really do have a set of tuples. There are choices of how to implement this set.

We could use a function of two arguments returning a boolean, if the domain/codomain are infinite. Or if both domain and codomain are finite, a set data structure containing tuples. Or a boolean matrix, if there are lots of tuples. Or a map of sets if one of the elements is sparse. Or a directed simple graph if we have a graph library.

Then we have the reflexive, symmetric, transitive closures for many-to-many relations. With a finite relation these are straightforward to compute via matrix algorithms or their equivalent. For infinite sets we have to work harder and use some form of symbolic reasoning.

Posets
======

Q: Can ~ be preferred if there is ambiguity? E.g. 1 <~ 2 resolving to 1 ~ 2. Is it safe under extension?

Dictionaries
============

Wikipedia calls these "associative arrays" and C++ and Haskell calls them maps. But dictionary seems to be the accepted term in the data structure textbooks, and it's about the right length as a word.

Generics
========

Since Stroscot is dynamic the default is heterogeneous collections that can contain anything. So for example you have a clause ``cons : Any -> List -> List``. Nonetheless collections have "types", namely the set of elements of the collection. So the example in section of 9.1.1 of :cite:`dolanAlgebraicSubtyping2016` might be written as follows:

::

  contents list = { e | e elementOf list }
  TypedList t = { l : List | contents l subset t}

  singleton : (a : Any) -> TypedList {a}
  singleton a = [a]

  get : (m : TypedList x) -> int -> Op x
  put : (m : List) -> int -> Any -> Cmd

  Component = Button|Checkbox

  disable : Component -> Cmd
  disable Button = print "Button disabled"
  disable Checkbox = print "Checkbox disabled"

  disableAll : TypedList Component -> Cmd
  disableAll l = mapM disable l

  buttons = [Button] : List Button
  components = [Button,Checkbox] : TypedList Component
  assert (not (component isElemOf TypedList Button))

  disableAll buttons
  disableAll components

  insertCheckbox : List -> Cmd
  insertCheckbox l = append l Checkbox
  disableAllAndAddCheckbox : TypedList Component -> Cmd
  disableAllAndAddCheckbox l = { disableAll l; insertCheckbox l }

  disableAllAndAddCheckbox buttons
  assert (not (buttons isElemOf TypedList Button))

It seems quite sufficient, because Stroscot finds assertion failures at compile time. But maybe the static typing people want more. In particular we want to specify a write bound, limiting the types of elements that may be added. So we define restricted lists:

::

  singleton : (s : Set) -> (a : s) -> RestrictedList
  put : (l : RestrictedList) -> int -> elemType l -> Cmd

All it does is give errors earlier though - the errors show up regardless once you try to use an element of the wrong type. So maybe it's not needed.

Transactional memory
====================

STM is a very attractive abstraction for beginners or those who can sacrifice some performance to ensure correctness. But the performance in benchmarks is so-so and when it's really slow the implementation is somewhat complex to optimize. So STM hasn't seen much success in high-performance areas. The main primitives have to be the OS mutexes and atomic instructions. But still, providing STM as a library would be good. Haskell has STM, Fortress worked on STM. It automates the programming pattern of "read struct pointer, read members, allocate new structure, compare-and-swap struct pointer" which is really common for high-performance concurrency.

The syntax is a simple DSL, ``atomically { if x { retry }; y := z }``. Transactions nested inside another transaction are elided, so that one big transaction forms. The semantics is a transaction has a visible effect (commits its writes) only if all state read during the transaction is not modified by another thread. The ``retry`` command blocks the transaction until the read state has changed, then starts it over, in an endless loop until a path avoiding the ``retry`` is taken. The implementation should guarantee eventual fairness: A transaction will be committed eventually, provided it doesn't retry all the time. The latest research seems to be :cite:`ramalheteEfficientAlgorithmsPersistent2021`, it might be usable. Have to extend it to handle transaction retries though.

Transactions have sequentially consistent semantics by default. But mixing transactions with low-level code might work, IDK. There could be ``atomically {order=relaxed} { ... }`` to use the CPU's memory model instead of totally ordered. The transaction syntax is more expressive than atomic instructions, so providing an atomic DSL would be nice. I.e. transactions matching atomic instructions should compile to the atomic instructions, plus thread wakeups but only if there are waiting threads with ``retry`` involved.

Units
=====

Code with units will probably never be the default, but numeric types with dimensional units are useful for safety. The main issue is performance - checking/converting units on every operation is slow. But I tried using some Python unit libraries and they were OK for scripting purposes. Inlining should work for compiled code. Syntax is an issue, handling exponents and other dimensionless operations is an issue.

Iterators
=========

Haskell has ``Foldable``, the main function being ``foldr : (a -> b -> b) -> b -> t a -> b``, which is equivalently ``t a -> (a -> b -> b) -> b -> b``, the latter part being the `Boehm-Berarducci encoding <https://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html>`__ of ``[a]``. So really ``Foldable t`` is just a function ``toList : t a -> [a]``. ``foldMap`` has a more general type that would allow a parallel fold, but in Haskell it's is required to be right-associative. So Haskell ``Foldable`` is strictly a linked list with ``foldr`` applied. We might as well call the class ``ListLike``.

`Fortress <https://homes.luddy.indiana.edu/samth/fortress-spec.pdf#page=128>`__ has real parallel folds similar to ``foldMap``. They have "reductions" which are just monoids, and then a "generator" is ``generate : (Monoid r) => Generator e -> (e -> r) -> r``. The monoid does not have to be commutative - results are combined in the natural order of the generator. Empty elements may be inserted freely by ``generate``. The implementation is based on recursive subdivision to divide a blocked range into approximately equal-sized chunks of work.

They also have generator comprehensions and big operator syntax, but the description is confusing.

::

  impure_list (Item : Set) = Nil | Cons { data : Item, next : Op (impure_list Item) }
  getIterator : [a] -> Op (impure_list a)
  getIterator arr = go 0 arr where
    go i arr | i < length arr = return $ Cons (arr[i]) (go (i+1) arr)
             | otherwise = return $ Nil

The problem with this design is you can accidentally store the ``next`` operation and re-use it. With ``next : Iterator -> Op (Done | Yield a)`` the similar pattern ``let y = next iter in { y; y}`` just results in calling ``next`` twice and does not corrupt the iterator state.

Work stealing task queues
=========================

Java has them, C++ has OpenMPI and libuv. Many other languages have a library for them as well. So Stroscot should too.

Properties
==========

Partial orders are good, no reason not to have them. The orders defined with posets should be usable dynamically. Similarly they should be in a set ``TotalOrder`` if appropriate. Similarly ``Commutative``, ``Associative`` for binary operators.

Arrays
======

In Stroscot the only mutable thing is a reference. So mutable arrays could mean two things: a fixed-size immutable array containing mutable values, or a mutable variable storing an immutable array. The second seems more similar to Java's ArrayList or C++ std::vector so is probably what is meant.

The key here for efficient performance is in-place (destructive) update, so that the array re-uses its storage instead of copying on every operation. There is a paper :cite:`hudakAggregateUpdateProblem1985` on how to do it for lazy programming - basically you perform reads eagerly, and delay array update operations as long as possible, until it is clear if you can do in-place update or will have to copy.

Evolution
=========

If the standard library is missing something, different incompatible implementations may arise. Sharing code then becomes problematic, because code is tied to one of these implementations. The need then arises for a wrapper library that smooths over the differences and provides a portable interface.

Generally something should only be in the standard library once it's reached this "portable interface" level, or if it's been a while and only one implementation has emerged. But otherwise, there are often 2-3 good alternatives that people need to choose from. So there should also be a "non-standard libraries" wiki or something listing alternatives and even providing comparison tables with pros/cons if people feel like writing it.

Equality
========

Equality is an equivalence relation ``(==) : Any -> Any -> Bool`` built in to Stroscot. Or maybe it is ``: a -> a -> Bool`` and only defined on certain types. The built-in approach seems more attractive.

For ordering though, ``(<=) : a -> a -> {LT,GT,EQ}`` seems the way to go. Many types do not have a reasonable ordering.

value representation:
  Nanboxing / nunboxing


Data structures
===============

Copy Python's, they've been optimized and should be as efficient as anything I'll write.
