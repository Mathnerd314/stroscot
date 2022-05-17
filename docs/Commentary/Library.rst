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

Primitive values
================

ISO/IEC 11404 has a classification of values:

1. primitive - defined axiomatically or by enumeration
2. primitive - cannot be decomposed into other values without loss of all semantics
3. primitive - not constructed in any way from other values, has no reference to other values
4. non-primitive - wholly or partly defined in terms of other values
5. generated - defined by the application of a generator to one or more previously-defined values
6. generated - specified, and partly defined, in terms of other values
7. generated - syntactically and in some ways semantically dependent on other values used in the specification
8. atomic - a value which is intrinsically indivisible. All primitive values are atomic, and some generated values such as pointers, procedures, and classes are as well.
9. aggregate - generated value that is made up of component values or parametric values, in the sense that operations on all component values are meaningful
10. aggregate - value which can be seen as an organization of specific component values with specific functionalities
11. aggregate - organized collection of accessible component values

Even ignoring the fact that the multiple definitions are all slightly different, these distinctions are also a matter of definition: we can define a 32-bit integer as one of 2^32 symbols, hence primitive and atomic, or as a list of boolean values of length 32, hence generated and aggregate. It seems easiest to avoid going down this rabbit hole and simply make a big list of all the sets of values, without attempting to create such a broad classification of the sets.

Dictionaries
============

Wikipedia calls these "associative arrays" and C++ and Haskell calls them maps. There is also the ISO/IEC 11404 "record" which only allows identifiers as keys and has a fixed key set. But dictionary seems to be the accepted term in the data structure textbooks, and it's about the right length as a word.

Tables
======

Tables such as those found in SQL are bags of records that all have the same fields.

Typed collections
=================

Following section 9.1.1 of :cite:`dolanAlgebraicSubtyping2016`, there are two interesting sets: the read bound and the write bound.

A simple read bound of a collection is that the returned value must be one of the elements - this can just be computed from the value. For example, for a list, defining ``contents (l : List) = { e | e elementOf l }``, we have ``elemAt : (l : List) -> Int -> contents l``. We can make a refinement type, ``TypedList t = { l : List | contents l subset t}``. With modifiable arrays the restriction must be put on the state parameter, ``TypedVar t = { v : Var | read v : t}``. Then ``elemAt : (l : TypedList x) -> Int -> x``.

However, the write bound is external to the collection - straightforward implementations produce heterogeneous collections that can contain anything. So for example ``setElemAt : Any -> Int -> List -> List``. Errors will show up once you try to read and use an element of the wrong type, but maybe the error message will not be so clear on when the element was inserted, making it hard to debug.

One solution is to write ``(setElemAt ...) : TypedList x`` around every modification operation. This will verify at compile time that all values are members of the set ``x``. Honestly this solution seems quite sufficient - the only issue is that asserting the value every time can become tedious.

So a more invasive solution is to define a set of restricted collections ``RestrictedList wb`` with the write bound set ``wb`` stored in the value and enforced for every write operation. For example it would be an error to do ``setElemAt b 0 (l : RestrictedList {a})``. This has the benefit of enforcing a uniform representation of elements. The write operation can even be extended by calling ``convert wb`` instead of just asserting membership.

Transactional memory
====================

STM is a very attractive abstraction for beginners or those who can sacrifice some performance to ensure correctness. But the performance in benchmarks is so-so and when it's really slow the implementation is somewhat complex to optimize. So STM hasn't seen much success in high-performance areas. The main primitives have to be the OS mutexes and atomic instructions. But still, providing STM as a library would be good. Haskell has STM, Fortress worked on STM. It automates the programming pattern of "read struct pointer, read members, allocate new structure, compare-and-swap struct pointer" which is really common for high-performance concurrency.

The syntax is a simple DSL, ``atomically { if x { retry }; y := z }``. Transactions nested inside another transaction are elided, so that one big transaction forms. The semantics is a transaction has a visible effect (commits its writes) only if all state read during the transaction is not modified by another thread. The ``retry`` command blocks the transaction until the read state has changed, then starts it over, in an endless loop until a path avoiding the ``retry`` is taken. The implementation should guarantee eventual fairness: A transaction will be committed eventually, provided it doesn't retry all the time. The latest research seems to be :cite:`ramalheteEfficientAlgorithmsPersistent2021`, it might be usable. Have to extend it to handle transaction retries though.

Transactions have sequentially consistent semantics by default. But mixing transactions with low-level code might work, IDK. There could be ``atomically {order=relaxed} { ... }`` to use the CPU's memory model instead of totally ordered. The transaction syntax is more expressive than atomic instructions, so providing an atomic DSL for machine code instruction would be nice. I.e. transactions matching atomic machine code instructions should compile to the atomic machine code instructions, plus junk like thread wakeups etc. but only if there are waiting threads with ``retry`` involved.

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

Value representation
====================

Nanboxing / nunboxing

Terms
=====

The name "term" comes from term rewriting, where a term is recursively constructed from constant symbols, variables, and function symbols. Technically there are also "lambda terms", but in Stroscot aas in most programming languages we call them "lambda expressions", and use "expression" to refer to all syntax that evaluates to a value.

Data structures
===============

Copy Python's, they've been optimized and should be as efficient as anything I'll write.

List flattening
===============

Lists don't automatically flatten, e.g. ``[a,[b,c]] != [a,b,c]``. Instead you can use a flatten function in the standard library, ``flatten [a,[b,c]] = [a,b,c]``. MATLAB's justification for flattening is that ``[A B]`` is the concatenated matrix with ``A`` left of ``B`` and ``[A;B]`` the concatenation with ``A`` above ``B``. This seems hard to remember and infix operators ``A horcat B`` and ``A vertcat B`` are just as clear.

List homomorphisms
==================

List concatenation is an associative binary operation, as such we can represent repeatedly applying an associative operation (a semigroup) as applying an operation to a (nonempty) list.

::

  combine op list = foldl1 op list
  sum = combine (+)
  product = combine (*)

  sum [1,2,3]
  product [2,3,4]

If the empty list is a possibility we need a monoid, i.e. specifying an identity element for the operation

::

  combine monoid list = foldMap monoid.op monoid.identity list
  sum = combine { op = (+), identity = 0 }
  product = combine { op = (*), identity = 1 }

This all works because the set of lists/nonempty lists under concatenation is isomorphic to the free monoid / free semigroup.