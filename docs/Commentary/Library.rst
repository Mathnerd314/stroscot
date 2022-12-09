Library
#######

Importing
=========

Stroscot should support the standard libraries of popular languages, so e.g. if you want the C functions with C semantics you would ``import Library.C``. Compatibility is a natural step to world domination, and this allows an intermediate step of C semantics with Stroscot syntax. For example a function of type ``C.int -> C.size_t`` is different from plain ``int -> int``, and if you really need the semantics of C++'s unstable sort then it has to be included.

It's only worth supporting the biggies, in particular:

* C, the standardized library. Quite lean so it's the one to start with.
* C++, the standardized library
* Java, OpenJDK libraries (GPL but with linking exception, should be OK to use for most projects. And Oracle v Google found that reimplementing the API via Apache-2 licensed Apache Harmony was fair use)

Others, such as the Python standard library, Glib, and Boost, are probably not worth the effort of copying directly, rather programs using these libraries can be linked via FFI or rewritten into Stroscot.

Synthesizing
============

Just copying wholesale isn't enough, we also have to create a new standard library for new programs to use. The stretch goal is to offer a common set of abstractions used by all programs that covers all use cases. Of course it won't be anywhere close initially - the near-term goal is to ensure that what the standard library is always improving, and to build it out steadily. But eventually, even if a user needs a weird data structure like a Y-fast trie, they should be able to find it in the standard library. Duplicating implementations is a waste of man‑hours that can be spent developing something new.

Quality is hard, but we can get most of the way there by synthesizing various standard libraries and implementing what they agree on (commonalities) and what is unique to one library (innovations). Sources:

* `Rust <https://github.com/rust-lang/rust/tree/master/library>`__ (MIT + Apache 2.0, `small <https://blog.nindalf.com/posts/rust-stdlib/>`__)
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
*
* Slate `1 <https://github.com/briantrice/slate-language/tree/master/src/core>`__ `2 <https://github.com/briantrice/slate-language/tree/master/src/lib>`__ `3 <https://github.com/briantrice/slate-language/tree/master/src/i18n>`__ (MIT)

Also, the proposals of the various languages are really useful, as they encapsulate changes and include motivation as to why the change was made. An aspect of the library might simply be a historical "accident" from the initial design, but a proposal is always a deliberate design choice. Even the rejected proposals are useful as they indicate language/library "smells", areas that could use improvement.

* `GHC <https://github.com/ghc-proposals/ghc-proposals/pulls>`__
* `Python <https://github.com/python/peps>`__. The repo includes almost all proposals, but there are a few stray PRs:

  * https://github.com/python/peps/pull/2066/files
  * https://github.com/python/peps/pull/609/files
  * https://github.com/python/peps/pull/641/files
  * https://github.com/python/peps/pull/671/files
  * https://github.com/python/peps/pull/686/files
  * https://github.com/python/peps/pull/690/files
  * https://github.com/python/peps/pull/2620/files (and other PEPs after Jun 1 2022)

* `Rust <https://github.com/rust-lang/rfcs/pulls>`__ (`accepted <https://rust-lang.github.io/rfcs/>`__)
* `Go <https://github.com/golang/go/labels/Proposal>`__
* `PHP <https://wiki.php.net/rfc>`__

TODO: go through these, unfortunately there’s a lot

Definition
==========

The C library is also considered somewhat small, so this defines a minimum:

* file system input/output
* data types and conversions
* memory allocation and manipulation
* concurrency / threading
* OS date and time
* math functions
* error handling and assertions.

For metacircularity Stroscot should also expose the compiler's API and an eval function.

C++ also provides:

* containers, such as arrays, hash maps, and binary trees
* algorithms operating over those containers, such as insertion, lookup, and sorting
* string tokenization
* regular expressions

Python has more:

* data compression
* cryptography
* networking
* CSV/XML parsing
* unit testing
* profiling

Maybe once the language is more defined it will be worth standardizing the embedding of some application-specific libraries. Audio, graphics (GUI), databases, servers.

Then there are the APIs that have caused endless bikeshedding:

* random number generation
* serialization (data persistence)

Evolution
=========

Principles:

* The library should be divided up into modules. The modules should be dated/versioned independently to allow specifying fine-grained dependencies. The modules should also have hashes in the name, to avoid name collisions. Neither dates nor hashes should appear in actual source code, and they should be centralized in a lock file, to avoid the "magic number" antipattern. If a package depends on packages with colliding names the lockfile should specify how to rename the packages. The modules should also be downloadable independently, so really the "standard library" is a software repository with high standards for inclusion.
* It should be easy to add code to the standard library. Taking more than a year to add a new API is just too slow; 6 months seems about right.

  * If there are multiple popular third-party libraries that do similar things but are incompatible, sharing code becomes problematic because code is tied to one of these implementations, and the community starts to split and newcomers get turned off by decision paralysis. Example: `scalaz vs cats <https://github.com/fosskers/scalaz-and-cats>`__ was an issue with Scala for a long time, before `it became clear <https://www.reddit.com/r/scala/comments/afor0h/scalaz_8_timeline/>`__ that Scalaz 8 would never be released and scalaz was effectively dead. There are several strategies for dealing with incompatibilities:

    * Create a wrapper interface that smooths over the differences and provides a portable interface
    * Analyze the pros and cons and vote for one library to make standard
    * Synthesize a new library that combines all the pros and none of the cons

    It doesn't really matter if the wrong decision is made because a robust evolution process means it can always be changed later, and in the short term 50% is better than 0% even if there is a 60% option.

  * If there is a single popular third-party library that has become the "go-to" library for some task, the process is even easier: it should just be incorporated after it has been proven to be sufficiently stable. The standard library provides discoverability and maintenance benefits over isolated libraries. For example incorporation solves the `left-pad <https://qz.com/646467/how-one-programmer-broke-the-internet-by-deleting-a-tiny-piece-of-code/>`__ issue where key libraries are maintained by solo developers with no oversight. Since it's all FLOSS, licensing should not be an issue, and presumably most developers will be happy to share maintainershup and join the team, or relinquish maintainance entirely.

* It should also be easy to remove code from the standard library. Some APIs inevitably become obsolete as others are added and become more popular. Similarly it should be easy to fix names, implementation details, and API design, as conventions change. This is accomplished as an add-remove pair. But people need time to migrate, so there should be a 2-year deprecation process. There should be some amount of forward stability so that if code compiles with an old SL, it will continue to do so with a new SL. This means deprecated API isn't actually removed, it instead goes to a "compatibility graveyard" and stays around for old projects while being invisible to new ones.
* Generally speaking, third party libraries should be either be in active development or designed as specialized replacements for something in the standard library. The active development is obvious: it's much easier to rapidly iterate when you don't have to maintain compatibility. The specialized is less obvious: most likely the third-party is better than the standard in some way, but there are trade-offs. There is a possibility that standardizing a solution will crowd out other solutions, but discussing trade-offs and alternatives in the standard library documentation is probably sufficient.

Blessed prelude
===============

The standard library is blessed in that its prelude module is imported by default into every module. Other than this there is no special support from the compiler for the standard library. Furthermore there is a compiler option to override the prelude import to import no prelude or a different prelude module.

Since the prelude is imported by default it should be small, so that no name conflicts arise. The definition of small varies but we'll just take the intersection among popular languages. A truly minimal prelude would just have the import statement, which would also have some advantages.

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

Discussed in the posets commentary.

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

A straightforward collection implementation produces a heterogeneous collection that can contain anything. So for example a linked list ``mkList [x,...y] = Cons x (mkList y); mkList [] = Nil``. We can type these lists by a set that contains all the elements, in particular defining ``List t = Cons t (List t) | Nil``. The type of all lists is ``List Any`` We can infer a good type for a list value with ``contents (l : List Any) = { e | e elementOf l }; type (l : UList) = List (contents l)`` - we have ``forall (l : List t). contents l subset t`` so this is the lower bound / principal type.

::

  uncons : List t -> Maybe (t, List t)
  cons : x -> List y -> List (x|y)

We see from looking at ``uncons`` that this type parameter is a read bound, i.e. the returned value must be one of the elements. Following section 9.1.1 of :cite:`dolanAlgebraicSubtyping2016` we might expect two parameters, a read bound and a write bound. But as far as I can tell, with first-class stores we don't need a second parameter - rather we write constraints, and if necessary two constraints:

::

  get : (s : Store) -> (r : Ref | read s r : a)  -> (a, Store)
  set : (s : Store) -> (r : Ref) -> a -> (s : Store | read s r : a)
  modify : (s : Store) -> (r : Ref) -> (a -> b | read s r : a) -> (s : Store | read s r : b)

With the formulation here, values are pure, so there is no polymorphic aliasing problem or whatever.

One other way to add a parameter that both I and Cliff Click came up with independently is a "restricted list", that for example makes ``(RList Int []) ++ ["a"]`` an error. Unrestricted lists construct heterogeneous lists and errors on unexpected elements will not show up until you try to read and use an element of the wrong type. Likely the error message will not be so clear on when the element was inserted, making it hard to debug. Instead of adding type assertions in random places, the restricted list will verify that all values are members of the write type when inserted.

::

  RList { write_type : Type, l : List Any | contents l subseteq rt, contents l subseteq write_type, wt subseteq write_type }
  uncons : RList wt rt -> Maybe (rt, RList wt rt)
  uncons l | Nil <- l.l = Nothing
  uncons l | Cons x l' <- l.l = Just (x, RList l.write_type l')
  cons : (v : wt) -> RList wt rt -> RList wt (rt|{v})
  cons x l = assert (x : write_type l); l { l = x : l.l }
  nil : (wt : Type) -> RList wt {}
  nil wt = RList wt Nil
  setWriteType : (wt' : Type) -> (RList wt rt | rt subseteq wt') -> RList wt' rt
  setWriteType wt' l = l { write_type = wt' }

The constraint ``contents l subseteq rt`` follows naturally from the list parameter discussion above. The constraint ``write_type subseteq rt`` in the constructor follows Dolan and can be derived from requiring that all lists are constructible from  ``cons`` and ``nil``. This constraint can be dropped if ``RList`` is taken as the primitive constructor, allowing mismatches between write type and contents.

The constraint ``wt subseteq write_type`` allows subtyping like ``RList (Int|String) [1] : RList Int Int``; the alternative would be ``wt == write_type`` which would make it an invariant parameter and then you would have to use type coercions. As far as subtyping, ``RList a b subseteq RList c d`` iff ``c subseteq a`` and ``b subseteq d``. The type of all restricted lists is ``RList {} Any``.

The write operation can be extended by calling ``x' = convert write_type x`` instead of just asserting membership, but the combination of loose and restricted typing seems unlikely to be desired.

Because the write type is part of the value, empty lists of different write types are distinct, e.g. the empty list ``RList Int []`` is not equal to the empty list ``RList String []``. Cliff suggested an alternate design where the empty list is special-cased as a symbol that is an element of all list types and the write bound is specified on the cons operation, like ``cons Int x l``. But this requires duplicating the type each time and has some bugs if the types mismatch; it seems more convenient to be have empty restricted lists know their type.

The ``setWriteType`` function is a bit weird. In fact we can always set the write bound to ``Any`` and have the program still work. The benefit of the restricted list is in invalidating programs. To get maximum invalidation we have to use a pattern like ``foo (l : RList Int Int) = { l = setWriteType Int l; ... }`` or a view pattern ``foo (coerceRList Int Int -> l) = ...``, so that the value-level write type is always as small as possible and matches the expected type-level write type.

Per Dolan we have 5 type synonyms that cover some common cases (unfortunately ``RList`` is still necessary for complex read-and-add situations):

::

  RListI t = RList t t // mutable list of some element type
  RListP_R t = RList Any t // List t (unrestricted writes)
  RListP_L t = RList {} t // unwriteable List t
  RListN_R t = RList t Any // any list with write type t
  RListN_L t = RList t {} // empty list with write type t

How to use ``RList``? Some playing around:

::

  IorS = Int|String
  a = mkRList IorS [1,2,3]
  assert (a : RList IorS Int)
  assert (a : RList IorS IorS)
  assert (a : RList {} Any) // type containing all RLists
  b = map (+1) a
  c = b ++ ["foo","bar"]
  assertNot (c : RList IorS Int)
  assert (c : RList IorS IorS)
  err = b ++ [[]]
  assert (err : RListWriteException)
  d = setWriteType (Int|String|List Int) c ++ [[]]
  assertNot (d : RListWriteException)

Transactional memory
====================

STM is a good abstraction for beginners or those who can sacrifice some performance to ensure correctness. Per studies it provides the ease of use of coarse locks with most of the performance of fine-grained locks. But livelock errors are hard to debug and when a program using STM is slow it is somewhat complex to profile and optimize. So STM hasn't seen broad success. Stroscot's base concurrency primitives still have to be OS mutexes and atomic instructions.

Still, providing STM as a library would be good. Haskell has STM, Fortress worked on STM, there's an Intel C++ STM library. The programming pattern of "read struct pointer, read members, allocate new structure, compare-and-swap struct pointer" is really common for high-performance concurrency and encapsulating this pattern in an STM library would be great.

STM syntax is a simple DSL, ``atomically { if x { retry }; y := z }``. Transactions nested inside another transaction are combined, so that one big transaction forms. The semantics is that a transaction has a visible effect (commits its writes) only if all state read during the transaction is not modified by another thread. The ``retry`` command blocks the transaction until the read state has changed, then starts it over, in an endless loop until a path avoiding the ``retry`` is taken. The implementation should guarantee eventual fairness: A transaction will be committed eventually, provided it doesn't retry all the time. The latest research STM implementation seems to be :cite:`ramalheteEfficientAlgorithmsPersistent2021`, it might be usable. It doesn't handle retries though. The most naive implementation just puts transactions on a FIFO queue, takes a global lock when entering a transaction, and adds retries to the back.

Transactions have sequentially consistent semantics by default, but mixing transactions with low-level relaxed-semantics code might work, IDK. There could be ``atomically {order=relaxed} { ... }`` to use the CPU's memory model instead of totally ordered. The transaction syntax is more expressive than atomic instructions, so providing an atomic DSL for machine code instructions would be nice. I.e. transactions matching atomic machine code instructions should compile to the atomic machine code instructions and nothing else. If there are waiting threads with ``retry`` involved, then we do need extra junk like thread wakeups etc., but it would be nice to avoid this in simple cases.

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

Goto/Break/continue
===================

`Core <https://github.com/core-lang/core/issues/44>`__ proposes to drop break and continue due to implementation complexity and mental complexity. He argues that it is clearer to use an extra boolean variable and only slightly clunkier. Per the `structured program theorem <https://en.wikipedia.org/wiki/Structured_program_theorem#Implications_and_refinements>`__ it is possible to compute any computable function with three control structures, semicolon, if, and while (and no break/continue). There are drawbacks in that the theorem usually must introduce additional local variables and duplicate code. For example consider `this program <https://ecommons.cornell.edu/bitstream/handle/1813/34898/bbang.pdf?sequence=2>`__::

  start = state0
  state0 | a0 = halt
         | a1 = p01; state1
         | a2 = p02; state2
  state1 | a1 = halt
         | a0 = p10; state0
         | a2 = p12; state2
  state2 | a2 = halt
         | a1 = p21; state1
         | a2 = p20;

A translation into structured programming loosely based on the paper::

  state = mut 0
  halt = mut false
  while !halt
    if state == 0 then
      if α1 then
         p01; state := 1
      else if α2 then
         p02; state := 2
      else halt := true
    else if state == 1 then
      if α2 then
        p12; state := 2
      else if α0 then
        p10; state := 0
      else halt := true
    else
      assert (state == 2) //must be state 2
      if α0 then
         p20; state := 0
      else if α1 then
         p21; state := 1
      else halt := true

Notice this is longer than the original description using recursion, mainly due to the extra variables. S. Rao Kosaraju proved that with arbitrary-depth, multi-level breaks from loops it's possible to avoid adding additional variables in structured programming, but known algorithms still duplicate code. In common cases the duplication can be avoided by clever structuring though.

Per https://hal.inria.fr/inria-00072269/document Table 5, the most common flow-affecting constructs in Java were (as a percentage of methods) return (65.5%), short-circuit operators (13.2%), single-level break (3.6%), single-level continue (0.3%), and labelled break/continue (0.13%). A `different comparison <https://sci-hub.se/10.1002/spe.2298>`__ (Table IV) finds that continue and goto are about equal in frequency in C, that synchronized and continue are about equal in frequency in Java, and break is about half as common as try/catch/throw in Java.

In Stroscot, it's not too hard to provide break/continue/goto within the continuation-based I/O model, and many C programmers will expect these operators, so they are planned to be implemented. They will be provided as functions, rather than as keywords, so will be imported and not steal syntax by default.

Work stealing task queues
=========================

Java has them, C++ has OpenMPI and libuv. Many other languages have a library for them as well. So Stroscot should too.

https://wingolog.org/archives/2022/10/03/on-correct-and-efficient-work-stealing-for-weak-memory-models
Chase-Lev work-stealing double-ended queue updated by "Correct and Efficient Work-Stealing for Weak Memory Models"

per comment in https://news.ycombinator.com/item?id=33065142 there is a patent


Properties
==========

Partial orders are good, no reason not to have them. The orders defined with posets should be usable dynamically. Similarly they should be in a set ``TotalOrder`` if appropriate. Similarly ``Commutative``, ``Associative`` for binary operators.

Arrays
======

In Stroscot the only mutable thing is a reference. So mutable arrays could mean two things: a fixed-size immutable array containing mutable values, or a mutable variable storing an immutable array. The second seems more similar to Java's ArrayList or C++ std::vector so is probably what is meant.

The key here for efficient performance is in-place (destructive) update, so that the array re-uses its storage instead of copying on every operation. There is a paper :cite:`hudakAggregateUpdateProblem1985` on how to do it for lazy programming - basically you perform reads eagerly, and delay array update operations as long as possible, until it is clear if you can do in-place update or will have to copy.

https://aplwiki.com/wiki/Leading_axis_theory

Conversion
==========

There is a function ``convert : (T : Set) -> Any -> T|Exception`` in a module in the core library. Conversion is intended to produce equivalent values, so these modified equivalence relation properties should hold:

* Reflexive: ``convert T a = a``, if ``a : T``
* Symmetric: ``convert T (convert T2 a) = a``, if ``a : T`` (assuming ``convert T2 a`` succeeds)
* Transitive: ``convert T3 (convert T2 a) = convert T3 a`` (assuming both conversions succeed)

These rules avoid conversion "gotchas" where information is lost during conversion. For example all convertible numbers must be exactly representable in the target type because of transitivity and the existence of arbitrary-precision types (``convert Exact (convert Approx a) == convert Exact a``).

Conversion is only a partial function, hence these properties may not hold due to some of the conversions resulting in errors. For example ``convert Float32 (2^24+1 : Int32)`` fails because only ``2^24`` and ``2^24+2`` are exactly representable as floats. Generally one direction of the conversion should be total, or there should be subtypes like ``Float32_Int subset Float 32`` and ``Int32_Float subset Int32`` for which conversion to both ``Float32`` and ``Int32`` is total.

Conversion for unions is often undefined, because if ``convert T2 (a : T) = b``, and ``a != b``, then by reflexivity we have ``convert (T|T2) a = a``.  and by assumption and reflexivity we have ``convert (T|T2) (convert T2 a) = convert (T|T2) b = b``, violating transitivity. Hence ``convert (T|T2)`` on at least one of ``a`` or ``b`` must be undefined.

Also, it is generally too much work (quadratic) to define all conversions explicitly. Conversion thus relies on an A* search through the conversion graph for the minimum cost conversion. The conversion graph is specified via some functions:

::

  guess_starting_type : Any -> [Set]
  neighbors : Set -> [(Set,Cost)]
  est_distance : Set -> Set -> Cost

The cost can be an estimate of the CPU cycles needed to compute it, or the amount of precision lost during conversion, or both (combined with a lexicographic order). With precise numbers the lowest-cost conversion will be unambiguous, and probably fairly stable even if conversions are added or removed.

The conversion syntax overlaps somewhat with a constructor function, e.g. it is often the case that ``int32 x == convert Int32 x``. But constructors have fewer rules. Because convert is an equivalence relation it can be applied semi-automatically, whereas constructors may lose information, be stateful, or lazily evaluate their argument.

Values could be made equivalent to their string representation. This would mainly be useful for converting values to strings, as multiple decimal literals parse to the same floating point number so that direction would be a partial function. So an explicit parse function is also needed.

Often we prefer conversions to be total; this is accomplished by overloading ``convert`` with a default flag argument to get the desired behavior. These flags are outside the scope of the equivalence relation. For example ``convert Byte 1099 { narrowing = true } = 75`` whereas without the narrowing flag it would error, as it is not exactly representable. This allows re-using the promotion mechanism so is preferred to defining a new function like ``lossyConvert``. Some conversions such as `int32 to float64 <https://stackoverflow.com/questions/13269523/can-all-32-bit-ints-be-exactly-represented-as-a-double>`__ do not need flags as they are already total.

Conversion is misleading when it privileges one out of multiple sensible mappings. For example, a date July 30, 1989 might convert to an int with a decimal representation of the year, month, and day 19800730, or a Unix epoch date 617760000 / 86400 = 7150. Both these conversions might be useful; e.g. they both have the desirable property that later dates correspond to larger integers. In such cases, it is better not to define the convert operator, and instead provide multiple named conversion functions ``toDateDecimal``, ``toUnixTime`` to implement the various mappings.

C++ has implicit conversion. This allows adding an appropriate function to the source or destination type, which is called when there is a type mismatch. The `Google C++ Guide <https://google.github.io/styleguide/cppguide.html>`__ recommends never using this feature and always making conversions explicit with a cast like ``(X) y``. But apparently there are waivers to this rule when the objects are in fact interchangable representations of the same value.

Promotion
=========

Promotion is a catch-all dispatch rule for arithmetic operators on mixed types, based on `Julia's <https://docs.julialang.org/en/v1/manual/conversion-and-promotion/>`__. It works as follows:

1. Compute a common type using ``promote_rule``
2. Promote all operands to common type using ``convert``
3. Invoke the same-type implementation of the operator, if it exists

For example if ``promote_rule (a : Int32) (b : Float32) = out { lossy = true}; Float32`` then ``(a : Int32) + (b : Float32) = (convert Float32 a + convert Float32 b) { lossy = true }``. The system is extensible by defining new conversions and new promotion rules.

Julia's promotion rules:
* Floating-point values are promoted to the largest of the floating-point argument types.
* Integer values are promoted to the larger of either the native machine word size or the largest integer argument type.
* Mixtures of integers and floating-point values are promoted to a floating-point type big enough to hold all the values.
* Integers mixed with rationals are promoted to rationals.
* Rationals mixed with floats are promoted to floats.
* Complex values mixed with real values are promoted to the appropriate kind of complex value.

Promotion is effectively implicit type conversion but scoped to certain functions. Standard ML, OCaml, Elm, F#, Haskell, and Rust don't have any implicit type conversions and work fine. Scala has full implicit conversions, a search invoked when types mismatch. The search is brute force, hence expensive to compile, and promotion seems sufficient. Also the semantics of promotion are simple (expanded function domain) vs implicit conversion which requires some kind of nondeterminism.

Per :cite:`pradelGoodBadUgly2015` the acceptable JS coercions are:

* coercing to bool in ``if-else``, ``!x``, ``x && y``, and ``x || y``  as follows:

  * 0, -0, null, false, NaN, undefined, and the empty string ("") coerce to false.
  * Objects, including empty objects {}, empty array [], all nonempty strings (including "false"), all numbers except zero and NaN coerce to true.

* binary ``+`` can combine two numbers or a string and a defined value (not null or undefined).
* unary ``+, -`` and binary ``-, *, /, %, <<, >>, >>>`` only work on numbers
* relational operators ``<, >, <=, >=`` works on two numbers or two strings
* bitwise operators ``~, &, |`` work only on numbers
* equality is of type ``forall a. (a|undefined|null) -> (a|undefined|null) -> bool`` and does no coercions. Having ``5 == "5"`` by converting the number to a string is counterintuitive.

The counter idiom ``x = (x | 0) + 1`` seems to be hardly used, probably not worth supporting.

A confusing example is ``m & 8192 != 8192``, which parses as ``m & (8192 != 8192) = m & false``. So using a boolean in place of a number here should be an error.

Equality and comparison
=======================

The comparison function itself is discussed in Posets. Basically it is a single function ``comp : Any -> Any -> {LessThan,Equal,GreaterThan,Incomparable}`` satisfying the requirements of a "partial comparison operation".

Loose comparison will perform a type conversion when comparing two things. In particular in JS it will convert objects to strings, booleans and strings to numbers, and numbers to bigints, and transitive chains of these. Loose comparison is considered a confusing mistake; equality should not do type conversion. Almost all JS programs do not use this feature, either via ``===`` or by avoiding cases that invoke conversion. :cite:`pradelGoodBadUgly2015`

The terms universal and multiversal come from `Scala <https://github.com/lampepfl/dotty/issues/1247>`__. For multiversal, it is `required <https://github.com/lampepfl/dotty/blob/language-reference-stable/docs/_docs/reference/contextual/multiversal-equality.md>`__ that a suitable comparison for the two values is defined. If no comparison is defined then the values are incomparable. Due to symmetry and transitivity, the scheme partitions the universe of values into a "multiverse" of sets, where values within a set can be compared but comparison of values from different sets errors. In contrast a "universal" comparison does no type conversion and assigns some arbitrary order to unrelated types

``<=`` and ``==`` are multiversal in Java, C++, Haskell, and Rust. ``<=`` is also multiversal in Python and Ruby. Java ``.compareTo()`` is multiversal as well. It seems from googling "TypeError: '<' not supported between instances of 'str' and 'int'" that forgetting to parse a string to an int is a common error - multiversal comparison make this error obvious. Also many types such as compiled lambdas do not have a portable intrinsic ordering, and there is no canonical ordering across different types. So making the default operators homogeneous should catch many errors, although it breaks from Python.

Python ``==`` is universal, and a Python library `safesort <https://github.com/wolever/safesort>`__ implements universal comparison. Also Java ``.equals()`` is universal. Julia provides universal equality as ``===``. Universal comparison allows non-linear patterns, sorting heterogeneous lists/containers, ordering record fields, assertions/unit tests, serialization and meta-programming, etc. Generally any place where contextual equivalence is desired, i.e. two values are considered equal if they are *functionally identical* in all contexts, i.e. substitutable as per the `Liskov substitution principle <https://en.wikipedia.org/wiki/Liskov_substitution_principle>`__. So this should be available somehow, e.g. in the standard library as ``univ_comp``. But it's too powerful to make it the syntactic default - a little ugliness in generic code is a small price to pay for catching beginner errors. The default ``==`` can be extended when convenient to delegate to ``univ_comp``, e.g. adding ``None == a = univ_comp None a === Equal`` to allow comparison with ``None`` for all values and hence implementing comparison on ``(a|None)``. As far as syntax I think a wordy expression is clearest, but this should be verified as maybe ``===`` is better.

IEEE 754 recommends that ``NaN`` should be incomparable with itself. But since "should" means "preferred but not necessarily required," deviating from the standard is allowed. The recommended behavior was decided circa 1985 so that ``x != x`` could be used to detect NaN values, in place of the ``isnan`` predicate which was not widely available (`1 <https://stackoverflow.com/questions/1565164/what-is-the-rationale-for-all-comparisons-returning-false-for-ieee754-nan-values>`__ `2 <https://grouper.ieee.org/groups/msc/ANSI_IEEE-Std-754-2019/background/predicates.txt>`__). Bertrand Meyer `says <https://bertrandmeyer.com/2010/02/06/reflexivity-and-other-pillars-of-civilization/>`__ that self-equal NaN makes more sense, but that the "proper" behavior is incomparable NaN but that ``==`` should error on incomparable values to preserve monotonicity. Few think about behavior on NaNs so this "proper" behavior of crashing immediately seems like a friendly approach. And handling the exception by pattern-matching on the ``==`` seems quite natural.

Of course universal comparison needs reflexive NaN, and also ``-0 != +0``. Furthermore, since inspecting the representation is possible, for full contextual equivalence all NaNs and encodings must be distinguished. Furthermore we need a total order; IEEE provides a guide that ``-0 < +0``, ``-NaN < x < NaN``, ``10e1 < 1e2``, ``abs signalingNaN < abs quietNaN``, but the rest needs to be defined. There should be a note that different encodings of the value will compare unequal. Printing out "f32 0x1234 (un-normalized representation of 1.0)" for these values should make the failures clear.

Deep comparison will compare the values of references rather than the reference identities. It's less common in Stroscot because more things are values, but still it can be useful for mutable structures. It basically is some logic to memoize comparisons of cyclic structures and then a call to a passed-in "value comparison" which should itself call back to the deep comparison for references.

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

Java has this by default, so we should too.

The hard parts are that Stroscot has more types of values: cyclic terms, lambdas. Ideally these would be deconstructible with term rewriting. References are a sticking point, the store needs special handling, probably just a reference <-> refid map.

As far as wire encodings, a custom binary format and JSON seem sufficient, the others can be 3rd-party libraries.

Function pipelines
==================

Haskell has function composition ``(.)`` and Julia has the "pipe" operator ``(|>)``.

According to `YSAGuide <https://github.com/jrevels/YASGuide#other-syntax-guidelines>`__ pipelines like ``a . b . c`` are bad style and one should instead use intermediate results, ``\x -> { a1 = a x; b1 = b a1; c1 = c b1; return b1 }``, except with better named variables than ``x,a1,b1,c1``. The reason given is that debugging composed functions in the REPL is hard and clutters stacktraces. This sounds like a debugger problem - function pipelines are shorter and easier to read.
