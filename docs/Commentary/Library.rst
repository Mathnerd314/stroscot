Library
#######

Importing
=========

Stroscot should support the standard libraries of popular languages, so e.g. if you want the C functions with C semantics you would ``import Library.C``. Compatibility is a natural step to world domination, and this allows an intermediate step of C semantics with Stroscot syntax. For example a function of type ``C.int -> C.size_t`` is different from plain ``int -> int``, and if you really need the semantics of C++'s unstable sort then it has to be included.

It's only worth supporting the biggies, in particular:
* C, the standardized library. Quite lean so it's the one to start with.
* C++, the standardized library
* Java, OpenJDK libraries (GPL but with linking exception, should be OK)

Others such as the Python standard library, Glib, and Boost are probably not worth the effort of including directly, rather they can be supported via FFI or rewritten natively.

Versioning
==========

The library should be divided up into modules and the modules should be versioned so that there's a deprecation cycle in place.

Synthesizing
============

Building on the work of others isn't enough, we also have to improve and synthesize a new, universal standard library for new programs to use. The standard library should be well-designed and built up steadily - the goal is to eventually include everything. If a user needs a Y-fast trie, they should be able to find it in the standard library. Duplicating implementations is a waste of man‑hours that can be spent developing something new. So we have to synthesize and combine various standard libraries:

* `Rust <https://github.com/rust-lang/rust/tree/master/library>`__ (MIT + Apache 2.0)
* `Go <https://github.com/golang/go/tree/master/src>`__ (BSD-style)
* `Haskell <https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries>`__ (BSD-style)

  * The alternate prelude `Foundation <https://github.com/haskell-foundation/foundation>`__ (BSD)

* Julia `1 <https://github.com/JuliaLang/julia/tree/master/base>`__ `2 <https://github.com/JuliaLang/julia/tree/master/stdlib>`__ (MIT)
* C

  * `glibc <https://sourceware.org/git/?p=glibc.git;a=tree>`__ (LGPLv2.1, some files BSD/ISC/etc.)
  * `Musl <https://git.musl-libc.org/cgit/musl/tree/>`__ (MIT)

* C++

  * `NVIDIA <https://nvidia.github.io/libcudacxx/>`__, `LLVM <https://libcxx.llvm.org/>`__, `MSVC <https://github.com/microsoft/STL>`__ (Apache 2 with LLVM Exceptions)
  * `HPX <https://hpx.stellar-group.org/>`__ (Boost Software License 1.0)
  * `EASTL <https://github.com/electronicarts/EASTL>`__ (BSD 3-Clause)

* Python `1 <https://github.com/python/cpython/tree/master/Modules>`__ `2 <https://github.com/python/cpython/tree/master/Lib>`__ (PSFv2)
* `Zig <https://github.com/ziglang/zig/tree/master/lib/std>`__ (MIT)
* Slate `1 <https://github.com/briantrice/slate-language/tree/master/src/core>`__ `2 <https://github.com/briantrice/slate-language/tree/master/src/lib>`__ `3 <https://github.com/briantrice/slate-language/tree/master/src/i18n>`__ (MIT)

Also, the proposals of the various languages are really useful, as they encapsulate changes and include motivation as to why the change was made. An aspect might simply be a historical "accident" from the initial design, but a proposal is always a deliberate design choice. Even the rejected proposals are useful as they indicate language/library "smells", areas that could use improvement.

* `GHC <https://github.com/ghc-proposals/ghc-proposals/pulls>`__
* `Python <https://github.com/python/peps>`__. The repo includes almost all proposals, but there are a few stray PRs:
https://github.com/python/peps/pull/2066/files
https://github.com/python/peps/pull/609/files
https://github.com/python/peps/pull/641/files
https://github.com/python/peps/pull/671/files
https://github.com/python/peps/pull/686/files
https://github.com/python/peps/pull/690/files
https://github.com/python/peps/pull/2620/files (and other PEPs after Jun 1 2022)

* `Rust <https://github.com/rust-lang/rfcs/pulls>`__ (`accepted <https://rust-lang.github.io/rfcs/>`__)
* `Go <https://github.com/golang/go/labels/Proposal>`__
* `PHP <https://wiki.php.net/rfc>`__

TODO: go through these, unfortunately there’s a lot

Maybe once the language is more defined it will be worth standardizing the embedding of some application-specific libraries. Audio, graphics, networking, databases, servers, cryptography.

Minimal definition
==================

At a minimum, the standard library should provide:
* containers, such as arrays, hash maps, and binary trees
* algorithms operating over those containers, such as insertion, lookup, and sorting
* basic support for multithreading
* string tokenization
* compiler's API

Numbers
=======

Arbitrary precision is attractive for beginners but hard to optimize. Machine-precision integers and floating-point numbers should be easily accessible. Still, overflow, roundoff, and catastrophic cancellation all appear with the standard sized types. A high-level language can avoid these by using bignums and computational reals.

Matrix multiplication
=====================

Suppose we are multiplying three matrices A, B, C. Since matrix multiplication is associative, (AB)C = A(BC). But one order may be much better, depending on the sizes of A, B, C. Say A,B,C are m by n, n by p, p by q respectively. Then computing (AB)C requires mp(n + q) multiplications, and computing A(BC) requires (m + p)nq multiplications. So if m = p = kn = kq, then (AB)C costs 2k^3 n^3, while A(BC) costs 2 k n^3, which if k is large means A(BC) is going to be much faster than multiplying (AB)C. The matrix chain multiplication algorithm by Hu Shing finds the most efficient parenthesization in O(n log n) time, given the sizes of the matrices. In practice the sizes must be observed through profiling. But this data must be collected at the level of the matrix chain  multiplication, as re-association optimisations are hard to recognise when the multiplication is expanded into loops.

Strings
=======

Text types::

  Text = Text { bytes : ByteArray#, offset : Int, length : Int } -- sequence of bytes, integers are byte offsets
  ByteString = BS { payload : Addr#, finalizer : ref Finalizers, length : Int }
  Lazy = Empty | Chunk Text Lazy


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

Since functions can nondeterministically return multiple values and comparing them can give multiple results, we might want equality operations anyEqual and allEqual to control how values are merged.



Conversion
==========

There is a function ``convert : (T : Set) -> Any -> T|Exception`` in a module in the core library.

Conversion is intended for equivalent values, so it should satisfy the properties of an equivalence relation:
* Reflexive: ``convert T a = a``, if ``a : T``
* Symmetric: ``convert T (convert T2 a) = a``, if ``a : T`` (assuming ``convert T2 a`` succeeds)
* Transitive: ``convert T3 (convert T2 a) = convert T3 a`` (assuming both conversions succeed)

These rules avoid conversion "gotchas" where information is lost during conversion. For example all convertible numbers must be "exactly representable" in the target type because of transitivity and the existence of arbitrary-precision types (``convert Exact (convert Approx a) == convert Exact a``).

Conversion is only a partial function, hence these properties may not hold due to some of the conversions resulting in errors. For example ``convert Float32 (2^24+1 : Int32)`` fails because only ``2^24`` and ``2^24+2`` are exactly representable as floats. Generally one direction of the conversion should be total, or there should be subtypes like ``Float32_Int subset Float 32`` and ``Int32_Float subset Int32`` for which conversion to both ``Float32`` and ``Int32`` is total.

Conversion for unions is often undefined, because if ``convert T2 (a : T) = b``, and ``a != b``, then by reflexivity we have ``convert (T|T2) a = a``.  and by assumption and reflexivity we have ``convert (T|T2) (convert T2 a) = convert (T|T2) b = b``, violating transitivity. Hence ``convert (T|T2)`` must be undefined.

Also, it is generally too much work (quadratic) to define all conversions explicitly. Conversion thus relies on an A* search through the conversion graph for the minimum cost conversion. The conversion graph is specified via some functions:

::

  guess_starting_type : Any -> [Set]
  neighbors : Set -> [(Set,Cost)]
  est_distance : Set -> Set -> Cost

The cost can be an estimate of the CPU cycles needed to compute it, or the amount of precision lost during conversion, or both (combined with a lexicographic order). With precise numbers the lowest-cost conversion will be unambiguous, and probably fairly stable even if conversions are added or removed.

The conversion syntax overlaps somewhat with a constructor function, e.g. it is often the case that ``int32 x == convert Int32 x``. But constructors have fewer rules. Because convert is an equivalence relation it can be applied automatically, whereas constructors may lose information, be stateful, or lazily evaluate their argument.

Values could be made equivalent to their string representation. This would mainly be useful for converting values to strings, as multiple decimal literals parse to the same floating point number so that direction would be a partial function. So an explicit parse function is also needed.

Often we prefer conversions to be total; this is accomplished by overloading ``convert`` with a default flag argument to get the desired behavior. These flags are outside the scope of the equivalence relation. For example ``convert Byte 1099 { narrowing = true } = 75`` whereas without the narrowing flag it would error, as it is not exactly representable. This allows re-using the promotion mechanism so is preferred to defining a new function like ``lossyConvert``. Some conversions such as `int32 to float64 <https://stackoverflow.com/questions/13269523/can-all-32-bit-ints-be-exactly-represented-as-a-double>`__ do not need flags as they are already total.

Conversion is misleading when it privileges one out of multiple sensible mappings. For example, a Date July 30, 1989 might convert to an int with a decimal representation of the year, month, and day 19800730, or a Unix epoch date 617760000 / 86400 = 7150. Both these conversions have the desirable property that later dates correspond to larger integers, and so either might be useful. In such cases, it is better not to define the convert operator, and instead provide multiple ordinary functions to implement the various mappings.

Promotion
=========

Promotion is a catch-all dispatch rule for arithmetic operators on mixed types, based on `Julia's <https://docs.julialang.org/en/v1/manual/conversion-and-promotion/>`__. It works as follows:

1. Compute a common type using ``promote_rule``
2. Promote all operands to common type using ``convert``
3. Invoke the same-type implementation of the operator, if it exists

For example ``(a : Int32) + (b : Float32) = (convert Float32 a + convert Float32 b) { lossy = true }`` since ``promote_rule (a : Int32) (b : Float32) = out { lossy = true}; Float32``. The system is extensible by defining new conversions and new promotion rules.

Julia's promotion rules:
* Floating-point values are promoted to the largest of the floating-point argument types.
* Integer values are promoted to the larger of either the native machine word size or the largest integer argument type.
* Mixtures of integers and floating-point values are promoted to a floating-point type big enough to hold all the values.
* Integers mixed with rationals are promoted to rationals.
* Rationals mixed with floats are promoted to floats.
* Complex values mixed with real values are promoted to the appropriate kind of complex value.

The main issue is that promotion is implicit type conversion. Standard ML, OCaml, Elm, F#, Haskell, and Rust don't have any implicit type conversions and work fine. Doing ``5 == "5"`` by converting the number to a string is weird; equality should not promote. Scala has implicit conversions and static typing but its algorithm is a brute force search type thing. It's expensive and a powerful feature, that maybe overloading is sufficient to emulate.

Per :cite:`pradelGoodBadUgly2015` the acceptable JS coercions are:

* coercing to bool in ``if-else``, ``!x``, ``x && y``, and ``x || y``  as follows:

  * 0, -0, null, false, NaN, undefined, and the empty string ("") coerce to false.
  * Objects, including empty objects {}, empty array [], all nonempty strings (including "false"), all numbers except zero and NaN coerce to true.

* binary ``+`` can combine two numbers or a string and a defined value (not null or undefined).
* unary ``+, -`` and binary ``-, *, /, %, <<, >>, >>>`` only work on numbers
* relational operators ``<, >, <=, >=`` works on two numbers or two strings
* bitwise operators ``~, &, |`` work only on numbers
* equality is of type ``forall a. (a|undefined|null) -> (a|undefined|null) -> bool`` and does no coercions

The counter idiom ``x = (x | 0) + 1`` seems to be hardly used, probably not worth supporting.

The one confusing example is ``m & 8192 != 8192``, which parses as ``m & (8192 != 8192) = m & false``. So using a boolean in place of a number here should be an error.

Strict and non-strict equality are easily confused so strict equality should be the default.

Comparison
==========

Equality and ordering can be defined two ways:
* as a heterogeneous built-in for all values, ``het_eq : Any -> Any -> Bool`` and ``het_ord : Any -> Any -> {LT,GT,EQ}``
* homogeneously / strictly, only on arguments of the same type for certain types ``a``, ``(==) : a -> a -> Bool`` and ``(<=) : a -> a -> {LT,GT,EQ}``

Python and Ruby allow heterogeneous equality ``1 == 'a'`` but not heterogeneous comparison ``1 < 'a'``. However a Python library `safesort <https://github.com/wolever/safesort>`__ implements heterogeneous comparison as ``(type(objA).__mro__, objA) > (type(objB).__mro__, objB)``. Java ``.compareTo()`` and ``<=`` are homogeneous (``<=`` is limited to primitive types), but ``.equals()`` is heterogeneous, and ``==`` is as well if you cast to ``Object`` first. C++ and Haskell only provide homogeneous equality and comparison, although C++ allows overloading like ``operator<(Type1,Type2)``.

It seems from googling "TypeError: '<' not supported between instances of 'str' and 'int'" that forgetting to parse a string to an int is a common error - heterogeneous operators make this error invisible. Also many types do not have a reasonable intrinsic ordering, and there is no canonical ordering across different types, so any heterogeneous order is mostly arbitrary. Similarly syntactic comparison of lambdas is subject to break under optimization. So making ``<=`` and ``==`` the homogeneous, preferred operators will probably catch many errors, although it breaks the tradition to have heterogeneous equality.

But heterogeneous comparison allows non-linear patterns, sorting heterogeneous lists, ordering record fields, etc. So it should still be available in the standard library as ``het_ord`` and ``het_eq``. It's just a bit too powerful to make it the syntactic default - a little ugliness in generic code is a small price to pay for catching beginner errors. The default ``==`` can be extended when convenient to delegate to ``het_eq``, e.g. adding ``None == a = het_eq None a`` to allow comparison with ``None`` for all values and hence implementing comparison on ``(a|None)``.

Julia has separate heterogeneous equality ``===``, using the same syntax as Javascript's strict equality. I think the different types of equality are a bit too confusing and ``het_eq`` is clearer, but this should be verified.

Since functions can nondeterministically return multiple values and comparing them can give multiple results, we might want equality operations anyEqual and allEqual to resolve the nondeterminism.

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

Serialization
=============

The hard parts are that Storscot has more types of values: cyclic terms, lambdas. Ideally these would be deconstructible with term rewriting. References are a sticking point, the store needs special handling, probably just a reference <-> refid map.

As far as wire encoding, a custom binary format and JSON seem sufficient.