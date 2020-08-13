Types
#####

Types are hard. Academics have spent decades in search of the perfect type system. The alternative is model checking, almost the opposite approach: take an existing program and try to prove properties about it. These meet in the middle with assertions. In particular a type signature ``a : T`` is just an assertion ``assert(isElemOfType(a, T))``. More specifically,

::

   data A = A (Int -> Int)
   # translates to
   A = gensym
   A = type { \x -> case x of
     s f -> ensure(isElemOfType(i,S)); isElemOfType(f i,Int)
     _ -> false
     }

Introduction
============

We follow the semantic definition: types are sets. In particular, we have typing is membership :math:`A : B = A \in B`. We define the set of sets as :math:`S =\{ \emptyset \} \cup \left\{ s \mid \exists x. x \in s\right\}`. Equality on sets is the usual extensional equality :math:`A \in S \land B \in S \rightarrow \left(\left(x \in A \leftrightarrow x\in B\right) \leftrightarrow A=B \right)`. Subtyping is set inclusion :math:`A \subseteq B` (and forms a distributive lattice).

The values (things that aren't types/sets) are as defined in the core: ADT elements (a list of elements with a tag) and lambda expressions and so on. For many types, checking whether an element is a member is as simple as checking the tag.

We follow New Foundations (specifically NFU) in allowing any type (set) to be specified with a stratified formula. The usual formulation requires ``lvl(b)=lvl(a)+1`` for ``a in b``, but this can be relaxed to ``lvl(b) > lvl(a)``. I can't find Holmes's paper with the exact result, but the proof of the Statified Comprehension Theorem on page 45 of :cite:`holmesElementarySetTheory1998` can easily be changed from ``n-1`` to ``n-k,k>0``. Since the stratification only applies to quantified variables and most types do not use quantification this won't come up often. Since NFU has universal sets there is no issue of function types being a power set that is "too large" for the universe.

Condition checking
==================

There's some interesting `work <http://mmjb.github.io/T2/>`__ I found on termination checking by Microsoft, called `TERMINATOR <https://web.archive.org/web/20131005142732/http://research.microsoft.com:80/en-us/um/cambridge/projects/terminator/papers.htm>`__. There's an interesting representation of terms as sets, which ends up mapping out all the paths through the program, and then identifying termination is fairly easy. But since you can check all these conditions it's a very powerful analysis that can also check buffer overflows and array bounds and resource use :cite:`albertResourceAnalysisDriven2019` and things of that nature.

Type synthesis
==============

Type synthesis is tricky, but it's only tricky because it's trying to do the work of the termination checker. It's not actually necessary from a user-level perspective. The only useful case might be pretty-printed type signatures. But with really precise types, they end up looking like ``(Nil-->0) & (Cons a b-->1+(length b))``, which is just the original program, so having the developer specify type signatures seem like the best option.


There's the `sub <https://github.com/stedolan/fyp>`__\ `typing <https://github.com/stedolan/mlsub>`__ stuff which actually has some pretty powerful type synthesis, better (and slower) than Hindley-Milner. But `dependent <https://github.com/UlfNorell/insane/>`__
`circular <https://github.com/gelisam/circular-sig>`__ dependent types will presumably ruin all the fun and require type signatures. However, bidirectional type checking :cite:`dunfieldBidirectionalTyping2019` should be able to minimize the amount of signatures required.


Terminology
===========

At this point you might be getting confused about terminology. I certainly am. So here we are:

memory cell
   A circuit that can store some fixed number of logical bits, 0 or 1. Since ternary computers might eventually become popular, a cell is probably best modeled as

value
   A group of memory cells, paired with some interpretation as a data type

data type
   A representational type, that can generally be narrowed down to a pair of functions doing pickle/unpickle on the value's bits

reference
   A value providing a way to access another value, usually a memory address. It's distinct from an API because at any given time there's only one unique value that can be accessed using the reference.

variable
   A name/symbol/identifier representing a function argument or mathematical object

l-value
   A variable representing a memory cell. The difference is some people don't consider ``arr[i+foo()]`` to be a variable, while they'll all agree it's an l-value.

r-value
   A value without a reference, basically an anonymous temporary value. Except in C++11 (but not Stroscot) you can still bind its memory address, to get an x-value. Segfaults galore. ¯\\_(ツ)_/¯

subtyping
    ``A`` is a subtype of ``B`` if every element of ``A`` is also in ``B``. Said another way, there are no elements that are in ``A`` but not in ``B``.

inheritance
    Record subtyping, where for example ``{a,b}`` is considered a subtype of ``{a}``.

Scoping and qualification
=========================

There is no kind of syntax or semantics for changing or redefining identifiers (besides :ref:`fexprs <fexprs>`); you can shadow, with warning, but once an identifier is declared in a scope, that's what that identifier refers to for the duration of the scope.

Numbers
=======

As a review, there are two main formats for numbers: integers and floating-point numbers. Integers are integers in the mathematical sense but limited to a certain range. Floating point numbers are an integer mantissa times an radix raised to an exponent. The radix is usually 2 but `IEEE-754 <https://en.wikipedia.org/wiki/IEEE_754>` has also defined decimal floating point (radix 10). The exponent itself is another integer, usually restricted to a quite small range.

We could try to define generic integer/float types, but only a few have efficient arithmetic operations. So in practice we have only ``IntN`` / ``UIntN`` (for ``N`` restricted to some powers of 2), ``Float``, and ``Double``. Non-power-of-2 ``N``, fixed-point arithmetic, unums, and posits can all be defined in libraries. It would also be good to have arbitrary-precision types, like `GMP <https://gmplib.org/>`__'s integer/rational and `MFPR <https://www.mpfr.org/>`__'s float that uses an Int32/Int64 exponent and an arbitrary precision mantissa. The binding could be at the C level like `Haskell's integer-gmp <https://hackage.haskell.org/package/integer-gmp>`__ or it could use the assembly routines directly.

Literals are parsed into records like ``Number { digits = "123", exponent = "24" }``. We can define implicit conversions to the various the numeric types. Leadings 0's restrict the type, so ``010`` must be stored in a type that can contain 999.

Subtyping between numeric types should work fine if there are a few type annotations. For example we can have an addition function ``(+) : Int8 -> Int8 -> Int8`` and similarly for ``Int16``. Then if we have a restriction on the output to ``Int8`` later it will flow back to ensure all the types are ``Int8``, and similarly using an ``Int16`` will force the output to ``Int16``. If there are 3 or more levels of precision then both an output and an input will need a type annotation, but this is 2 annotations for the whole program, which seems fine.

Strings
=======

The standard, terrible null-terminated C string will always be needed, but most purposes should be satisfied by using an array / buffer together with a length. There can be different encodings: 8-bit UTF8, 16-bit UTF16, 32-bit UTF32, or some other encoding like Shift JIS or Big5. There are some optimizations that can be made for non-mutating views (substrings), e.g. storing an offset too to gives zero-copy slices (although ignoring allocators, a pointer is sufficient instead of start+offset). Iterating through strings is an interesting API design problem, particularly seeking for the :math:`n` th character, but isn't too hard overall. Dealing with invalid characters is a little trickier, but an implicit mode parameter should be sufficient. We also need datatypes for dealing with streaming I/O, but continuations work for that.

Records
=======

Structural subtyping of records allows you to pass ``{a: 1, b: 2}`` to a function expecting ``{b: Int}``.

Roles
=====

Roles are just an optimization for ``coerce``. I don't know why GHC polluted their type system with them, besides that it was a dirty hack to solve a pressing problem.
