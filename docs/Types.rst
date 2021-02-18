Types
#####

Types are hard. Academics have spent decades in search of the perfect type system. The alternative is model checking, almost the opposite approach: take an existing program and try to prove properties about it. These meet in the middle with assertions. In particular a type signature ``a : T`` is just an assertion ``assert(isElemOfType(a, T))``.

Terminology
===========

At this point you might be getting confused about terminology. I certainly am. So here we are:

reference
   A subset of the associative array within a RAM value that decodes to a set of memory cell names.

variable
   A name/symbol/identifier representing a function argument or mathematical object

l-value
   A variable representing a memory cell name, for example ``arr[i+foo()]``

r-value
   A value. Except in C++11 (but not Stroscot) you can still bind its memory address, to get an x-value. Segfaults galore. ¯\\_(ツ)_/¯

Introduction
============

We follow the semantic definition: types are sets. In particular, we have typing as membership :math:`A : B = A \in B`. We define the set of sets as :math:`S =\{ \emptyset \} \cup \left\{ s \mid \exists x. x \in s\right\}`. Equality on sets is the usual extensional equality :math:`A \in S \land B \in S \rightarrow \left(\left(x \in A \leftrightarrow x\in B\right) \leftrightarrow A=B \right)`. ``A`` is a subtype of ``B`` if every element of ``A`` is also in ``B``. Said another way, there are no elements that are in ``A`` but not in ``B``. So subtyping is just set inclusion :math:`A \subseteq B` (and forms a distributive lattice).

Stroscot is dependently typed, so you can pass around types (sets). The semantics is that they're AST nodes. Only ensure is special, since it has to compute properties of types. The values (things that aren't types/sets) are as defined in the core: ADT elements (a list of elements with a tag) and lambda expressions and functions and so on. For simple types, checking whether an element is a member is as simple as checking the tag, but the set can describe an almost arbitrary computation.

We follow New Foundations (specifically NFU) in allowing any type (set) to be specified with a stratified formula. Our stratification condition is very loose. The usual formulation requires ``lvl(b)=lvl(a)+1`` for ``a in b``, but this can be relaxed to ``lvl(b) > lvl(a)``. Holmes had a paper proving this consistent, but his site has been reorganized; consistency also follows from the proof of the Statified Comprehension Theorem on page 45 of :cite:`holmesElementarySetTheory1998`, changing ``n-1`` to ``n-k,k>0``. In practice, this means levels must form a poset, or said another way, that there cannot be a cyclic chain of set membership tests (a in b, b in c, c in a, for example). Also note that if the left side is known to be a set, then the membership test can be replaced with a subset inclusion check (expandable to a logical formula) and there is no constraint generated at all. So it is only when we are dealing with mixed sets containing both sets and primitive elements that we can really run into trouble. Overall, since the stratification only applies to quantified variables and most types do not use quantification, this most likely will never come up as an issue in practice.

NFU has universal sets, so there is no issue of function types being a power set that is "too large" for the universe; the power set of the universe is actually a subset of the universal set.

Type declarations
=================

Types in Stroscot act as identity functions restricted to a certain domain. So you use an application, similar to assembly syntax such as ``dword 0``:

::

   a = s8 2

To match Haskell, there is also a standard operator ``(:)`` defined as ``x : y = y x``, with low precedence, so you can write

::

   a = 2 : s8

These two options seem more logical compared to other choices such as ``a : s8 = 2`` (Swift,Jai - hard to read with long types) or ``s8 a = 2`` (C,Rust - overlaps with function definition). The name is simply a syntactic handle to refer to the value; it doesn't have an innate type. In contrast the representation of the value must be specified to compile the program.

Type synthesis
==============

Type synthesis is tricky, but it's only tricky because it's trying to do the work of the termination checker. It's not actually necessary from a user-level perspective. The only useful case might be pretty-printed type signatures. But with really precise types, they end up looking like ``(Nil-->0) & (Cons a b-->1+(length b))``, which is just the original program, so having the developer specify type signatures seem like the best option.


There's the `sub <https://github.com/stedolan/fyp>`__\ `typing <https://github.com/stedolan/mlsub>`__ stuff which actually has some pretty powerful type synthesis, better (and slower) than Hindley-Milner. But `dependent <https://github.com/UlfNorell/insane/>`__
`circular <https://github.com/gelisam/circular-sig>`__ dependent types will presumably ruin all the fun and require type signatures. However, bidirectional type checking :cite:`dunfieldBidirectionalTyping2019` should be able to minimize the amount of signatures required.

Numbers
=======

In the mathematical world there are integers and real numbers, which have well-defined arithmetic operations (besides division by 0). In the computer world we do not have either of these luxurious spaces, but rather various formats for numbers which represent subsets of the space.

Integers
--------

The most common integer format is a signed/unsigned integer with the range :math:`[0,2^{k}-1]` or :math:`[-2^{k-1},2^{k-1}-1]`, taking :math:`k` bits. But it is not too tricky to implement the basic operations for arbitrarily-ranged integers :math:`[a,b]`, where :math:`b-a+1` is a power of 2. We can represent as :math:`a+k` or :math:`b-k` where :math:`k` is unsigned or :math:`(a+b+1)/2 + k` for signed :math:`k`. The operations use :math:`\log_2 (b-a+1)` bits and expand the constants out (:math:`(x+a)+(y+a)=(x+y)+2*a`, etc. - there's definitely clever ways to structure the computations for efficiency). When the range can be determined statically there is no overhead besides the extra operations (and there are no extra operations if the range fits into the machine-sized integer). If we use branching operations we can go even farther and use a tag bit to represent unions of ranges, :math:`[-23,2] \cup [56,100]`.

Floating point
--------------

Floating point numbers are an integer mantissa times an integer radix raised to an integer exponent. The radix is usually 2 but `IEEE-754 <https://en.wikipedia.org/wiki/IEEE_754>` has also defined decimal floating point (radix 10). The exponent itself is another integer, usually restricted to a quite small range. We can also include posits; these are mantissa * radix ^ exponent * useed ^ regime, where the first part is the floating point stuff, useed is 2 ^ 2 ^ maximum exponent size, and the regime is nonnegative.

Actual types
------------

We could try to define generic integer/float types, but only a few have efficient arithmetic operations. So in practice we have only ``sN`` / ``uN`` (for ``N`` restricted to 8/16/32/64), ``Float``, and ``Double``. Non-power-of-2 integers, fixed-point arithmetic, unums, and posits can all be defined in libraries. It would also be good to have arbitrary-precision types, like `GMP <https://gmplib.org/>`__'s integer/rational and `MFPR <https://www.mpfr.org/>`__'s float that uses an s32/s64 exponent and an arbitrary precision mantissa. The binding could be at the C level like `Haskell's integer-gmp <https://hackage.haskell.org/package/integer-gmp>`__ or it could use the assembly routines directly.

Operations
----------

Literals are parsed into records like ``NumberLiteral { digits = "123", exponent = "24" }``. We can define implicit conversions to the various the numeric types. Leadings 0's restrict the type, so ``010`` must be stored in a type that can contain 999.

For arithmetic we define implicit conversions, ``convert : s8 -> Arb`` and so on to an arbitrary precision type ``Arb`` with the usual arithmetic operations, ``(+) : Arb -> Arb -> Arb`` and so on. Then narrowing the result back into a restrictive format is represented explicitly with an operation, ``narrow s16 (2+30*x)`` and so on. The compiler then figures out how to compute the answer as efficiently as possible. For floating point the narrowing also takes a precision argument, or optimizes for the best precision like Herbie, depending on whether speed or accuracy is preferred.

For compatibility with other languages we can define narrowed arithmetic operations, like ``a plus b = assert(a is s16 && b is s16); x = narrow s16 (a+b); assert(x is s16)``. These give an out-of-range / overflow / unrepresentable error if the result doesn't fit. We can also support implicit conversions ``convert : s8 -> s16`` and so on; the compiler has to check that the narrowed arbitrary-precision computation matches the various fixed-width computations, but it should be resolvable.

Floating points numbers don't have implicit conversions between each other, besides the conversion from literals. The arithmetic operations are defined normally, ``(+) :: f32 -> f32 -> f32`` and so on.

Strings
=======

The standard, terrible null-terminated C string will always be needed, but most purposes should be satisfied by using an array / buffer together with a length. There can be different encodings: 8-bit UTF8, 16-bit UTF16, 32-bit UTF32, or some other encoding like Shift JIS or Big5. There are some optimizations that can be made for non-mutating views (substrings), e.g. storing an offset too to gives zero-copy slices (although ignoring allocators, a pointer is sufficient instead of start+offset). Iterating through strings is an interesting API design problem, particularly seeking for the :math:`n` th character, but isn't too hard overall. Dealing with invalid characters is a little trickier, but an implicit mode parameter should be sufficient. We also need datatypes for dealing with streaming I/O, but continuations work for that.

Records
=======

Structural subtyping of records allows you to pass ``{a: 1, b: 2}`` to a function expecting ``{b: Int}``. This is similar to inheritance in other languages.

Functions
=========

Function type declarations come in two forms. The first version simply checks compatibility, that the return type is as expected on the given input.

::

   A : S -> Int

   -- expands to

   s = arbElemOfType(S)
   assert(isElemOfType (A s) Int)

The second version restricts the definition of the function so it is only defined on the type. This is useful for overloading.

::

   restrict A : S -> Int
   A = ...

   -- expands to

   A$untyped = ...
   A = {
      assert (isElemOfType $args S)
      ret = A$untyped args
      assert (isElemOfType ret Int)
      ret
   }

Roles
=====

Roles are just an optimization for ``coerce``. I don't know why GHC polluted their type system with them, besides that it was a dirty hack to solve a pressing problem.
