Compiler library
################

For the language to be useful it must have an implementation, and ideally a self-hosting implementation. In this framework the compiler, and hence the language, are really just another set of library modules. I will unimaginatively call this library the "compiler library". Per the standard library evolutionary design process, the compiler library will likely serve as the prototype for the standard library, so some investment is worthwhile. Writing a full-featured standard library is important for testing the compiler and getting the "feel" of the language.

It is particularly important to have regression tests from the beginning, to catch near-infinite loop bugs and so on.

Synthesizing
============

Designing a good, quality API is hard, but lots of people have tried. There are various existing libraries that seem worth inspecting:

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
* Slate `1 <https://github.com/briantrice/slate-language/tree/master/src/core>`__ `2 <https://github.com/briantrice/slate-language/tree/master/src/lib>`__ `3 <https://github.com/briantrice/slate-language/tree/master/src/i18n>`__ (MIT)

We don't just copy wholesale, rather we look for what libraries agree on (commonalities), what is unique to one library (innovations), and what libraries overlap but disagree on (competition). Commonalities and innovations are fairly straightforward, they can just be adopted. But if there are multiple competing solution to a problem, then we have to look at experience reports as to which solution is best, and create a new solution with the best parts of each.

The proposals of the various languages can be useful, as they include motivation as to why the library was designed that way. An aspect of the library might simply be a historical "accident" from the initial design, but a proposal is always a deliberate design choice. Even the rejected proposals are useful as they indicate language/library "smells", areas that could use improvement.

* `GHC <https://github.com/ghc-proposals/ghc-proposals/pulls>`__
* `Python <https://github.com/python/peps>`__. The repo includes almost all proposals, but there are a few stray PRs:

  * https://github.com/python/peps/pull/2066/files
  * https://github.com/python/peps/pull/609/files
  * https://github.com/python/peps/pull/641/files
  * https://github.com/python/peps/pull/671/files
  * https://github.com/python/peps/pull/686/files
  * https://github.com/python/peps/pull/690/files
  * https://github.com/python/peps/pull/2620/files (and other PRs after Jun 1 2022)

* `Rust <https://github.com/rust-lang/rfcs/pulls>`__ (`accepted <https://rust-lang.github.io/rfcs/>`__)
* `Go <https://github.com/golang/go/labels/Proposal>`__
* `PHP <https://wiki.php.net/rfc>`__

TODO: go through these, unfortunately there’s a lot

Importing
=========

Stroscot should support the standard libraries of popular languages, so e.g. if you want the C functions with C semantics you would ``import Library.C``. Compatibility is a natural step to world domination, and this allows an intermediate step of C semantics with Stroscot syntax. For example a function of type ``C.int -> C.size_t`` is different from plain ``int -> int``, and if you really need the semantics of C++'s unstable sort then it has to be included.

It's only worth supporting the biggies, in particular:

* C, the standardized library. Quite lean, and it's the basis of most OS's, so it's definitely the one to start with, and maybe the only one needed
* C++, the standardized library
* Java, OpenJDK libraries (GPL but with linking exception, should be OK to use for most projects. And Oracle v Google found that reimplementing the API via Apache-2 licensed Apache Harmony was fair use)

Others, such as the Python standard library, Glib, and Boost, are probably not worth the effort of copying directly, rather programs using these libraries can be linked via FFI or rewritten into Stroscot.

Scope
=====

The C library is considered somewhat small, so this defines a minimum:

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

Then there are the libraries suggested by ChatGPT, libraries for:

* creating visually appealing and user-friendly graphical user interfaces and interactive applications
* building games with high-performance 2D or 3D graphics and spatial audio
* reading and writing files
* making network requests
* parsing and emitting data formats, such as audio, video, images, text, speech, and financial data.
* high-performance numerical computation such as machine learning and data manipulation
* data visualization and analysis
* integrating with relational databases and NoSQL databases
* building server-side applications and client-side web applications
* building desktop applications
* working with the command-line interface and operating system APIs for a variety of platforms, including desktop operating systems, Android, iOS, and web browsers.
* working with robotics/control systems - sensors, actuators, and low-level hardware peripherals.
* scaling across multiple processors, cores, machines, and cloud platforms, communicating and coordinating between processes, and in general doing concurrent, distributed, and parallel computing without getting bogged down in low-level details of data storage and processing
* applications: automation, analytics, financial computing, data science, scientific computing, virtual and augmented reality
* real-time data processing and real-time communication

Booleans
========

four distinct 2-valued types, for true/false, yes/no, on/off, and 0/1

Per :cite:`pradelGoodBadUgly2015` the only acceptable coercions are coercing to bool in ``if-else``, ``!x``, ``x && y``, and ``x || y``.

Numbers
=======

Mathematically, the definition of `number <https://en.wikipedia.org/wiki/Number#Main_classification>`__ mainly refers to natural numbers, integers, rationals, real numbers, and complex numbers (the "numeric tower"), but other mathematical structures like p-adics or the surreal numbers are also considered numbers.

Representation
--------------

There are various ways to represent these numbers. Naturals are generally represented as a list of digits in some base (a decimal). Integers are naturals with a sign. Rationals may be written as a (possibly improper) fraction of integers, a terminating or infinitely repeating decimal, a "mixed number" an integer and a proper fraction, or a floating point of a decimal times an exponent 1/2^n. For the complete fields such as reals and p-adics there are even more representations:

* Cauchy sequence of rationals
* nondecreasing bounded sequence of rationals
* an infinite decimal
* predicate which determines if a rational is lower, equal to, or higher than the number
* "sign expansion", an ordinal and a function from the domain of that ordinal to {-1,+1}

Completion also brings with it the computability issue. For example, finding a rational approximation of Chaitin's Ω constant within a given precision has complexity at least :math:`\Sigma^0_1`, meaning that every Turing program attempting to compute Ω has a precision beyond which it will unconditionally fail to produce an answer. Practically, one mainly restricts attention to computable numbers, i.e. those numbers for which the predicate/sequence/function is representable as a terminating program, but although they are closed under the field operations, equality is still complexity at least :math:`\Sigma^0_1`. I'm not sure of a direct example, but for example it is an open question if :math:`e+\pi` is rational, algebraic, irrational or transcendental.

Complex numbers have two main representations, rectangular (1+2i) and polar (sqrt(5) e^(i arctan(2))). Each of these has two coordinates, so we might represent them as ``data Complex = Rectangular Real Real | Polar Real Real``. Most complex numbers have a 1-1 conversion between the two forms. There are the issues that 0 has only one rectangular form but many polar forms, and the polar angle can differ by any multiple of 360 degrees, but restricting the polar number set to the "small" polar set where theta in [0,360 degrees) and r=0 -> theta=0 fixes this.

So far we have only considered the variety of mathematical forms. The representation on a computer also involves a certain amount of differentiation based on practicalities. There are arbitrary-precision bignums and symbolic representations that can represent almost all values, subject to memory and computability limits, which are great for those who don't care much about performance. But for reasons of efficiency, and also for faithfulness to standards etc. which specify a representation, many programs will want to use fixed-size types that restrict values to a certain range, precision, and bit representation, such as int8, uint16, or the IEEE floating point formats.

So, how do we deal with this multitude of forms? Generally, programs are not representation-independent, and each algorithm or operation in a program will have a preferred representation that it works with for input and output, preferred for reasons of accuracy, speed, or convenience. We cannot reliably perform automatic conversion between formats, as they differ in ranges and so on; there will be unrepresentable value in one direction or the other, loss of precision in the case of floating-point, and the conversion itself adds nontrivial overhead. Thus, we must consider each representation of a mathematical value to be a distinct programmatic value. There are thus several sets relevant to, for example, the integers:

* Int8, Int16, UInt16, etc.: the sets of integers representable in various fixed representations
* GmpIntegers: the set of all integers as represented in arbitrary precision in libGMP (disjoint from the above)

  * GmpIntegers8, GmpIntegers16, GmpIntegersU16, etc.: the subsets of libGMP integers corresponding to the fixed representations

* Integers: the disjoint union (sum type) of all integer representations
* Any: the universal set containing the above and all other values

Syntax
------

Number syntax is mainly `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__. There is the integer literal ``4211``, extended to the decimal ``12.11``. Different bases are provided, indicated with a prefix - decimal ``1000``, hexadecimal ``0x3e8``, octal ``0o1750``, binary ``0b1111101000``. Exponential notation ``1.23e+3`` may be either integer or rational. Positive exponents with decimal (e) / hexadecimal (p) / binary (b) are allowed. Also there is a sign. Numbers can also have a suffix interpreted as the format. This expand to a term that specifies the format by applying it, e.g.  ``123i8`` expands to ``int8 123``. Formats include IEE 754 float/double, signed and unsigned fixed bit-width integers, and fixed-point rationals. So the full syntax is sign, base, mantissa, exponent, format.

Leadings 0's are significant - literals with leading zeros must be stored in a type that can hold the digits all replaced with their highest value, e.g. ``0001`` cannot be stored in a ``i8`` (type must be able to contain ``9999``). Parsing leading ``0`` as octal is widely acknowledged as a mistake and should not be done. On the other hand trailing 0's are not significant - the decimal point should never be the last character in numeric literals (e.g. 1. is invalid, and must be written as 1 or 1.0).

It seems worth allowing extension of bases / exponential formats to characters other than xob / epb.

Flix says binary and octal literals are rarely used in practice, and uses this as a reason to drop support for them. Despite this most languages include support. Clearly there is a conflict here, so let's dive deeper.

Per `Wikipedia <https://en.wikipedia.org/wiki/Octal>`__, octal is indeed rare these days because bytes do not divide evenly into octets whereas they do divide into 2 hex digits. But it can still be useful in certain cases like the ModRM byte which is divided into 2/3/3 just like how a byte divides unevenly into octets, or chmod's Unix file permission specifications which use 3-bit modes. Of course such usages are more likely to confuse than elucidate and using symbolic notation like ``modrm direct eax`` or ``u=rwx,g=rw,o=r`` is clearer. Nonetheless octal still crops up in legacy code as an omnipresent C feature, so should be included for compatibility. The main thing to avoid is the prefix 0 for octal, as leading zeros are useful for other purposes as well. ``0o`` has been introduced and widely adopted, with no obvious complaints.

For binary literals, Java 7 added binary literals in 2011, C++ in 2014, and C# 7 in 2017, suggesting significant demand. The `Java proposal <https://mail.openjdk.org/pipermail/coin-dev/2009-March/000929.html>`__ lists bitmasks, bit arrays, and matching protocol specifications as killer usages. Hexadecimal is just artifical for these usages and obscures the intent of the code. Key to the usage of binary literals is a digit separator, so you can break up a long sequence like ``0b1010_1011_1100_1101_1110_1111``. In theory ``0b1`` could be confused with ``0xB1``, but teaching programmers about the standardized ``0-letter`` pattern should mostly solve this.

The alternative to not including literal support is to use a function parsing a string, so one would write for example ``binary "001100"``. Since Stroscot does compile-time evaluation this would work with no runtime overhead and give compile-time exceptions. But it is a little more verbose than the ``0-letter`` literals. It is true that humans have 10 fingers but this isn't much reason to restrict literals to decimal, and once you have hex, binary and octal are just more cases to add.

Digit grouping
--------------

`Wikipedia <https://en.wikipedia.org/wiki/Decimal_separator#Digit_grouping>`__ lists the following commonly used digit grouping delimiters: comma ",", dot ".", thin space " ", space " ", underscore "_", apostrophe/single quote «'».Traditionally, English-speaking countries employ commas, and other European countries employ dots. This causes ambiguity as ``1.000`` could either be ``1`` or ``1000`` depending on country. To resolve this ambiguity, various standards organizations have advocated the thin space in groups of three since 1948, using a regular word space or no delimiter if not available. However, comma, dot, and space are already in use in programming languages as list separator, radix point, and token separator.

Hence underscore and apostrophe have been used in PLs instead. Simon of `Core <https://github.com/core-lang/core/issues/52>`__ says apostrophe is more readable. Underscore is also used in identifiers, which can confuse as to whether a symbol is an identifier or a numeric literal. But the underscore is the natural ASCII replacement for a space. 13+ languages have settled on underscore, `following <https://softwareengineering.stackexchange.com/questions/403931/which-was-the-first-language-to-allow-underscore-in-numeric-literals>`__ Ada that was released circa 1983. Only C++14, Rebol, and Red use the "Swiss" apostrophe instead.

C++14 chose quote to solve an ambiguity in whether the ``_db`` in ``0xdead_beef_db`` is a user-defined format or additional hexadecimal digits, by making it ``0xdead'beef_db``. This could have been solved by specifying that the last group parses as a format if defined and digits otherwise, or parses as digits and requires an extra underscore ``__db`` to specify a format.

Rebol uses comma/period for decimal point so quote was a logical choice. There doesn't seem to be any reason underscore couldn't have been used. Red is just a successor of Rebol and copied many choices.

Operations
----------

Considering the multitude of forms, and the fact that representations are often changed late in a project, it seems reasonable to expect that most code should be representation-agnostic. The library should support this by making the syntax "monotonous", in the sense of `Jef Raskin <https://en.wikipedia.org/wiki/The_Humane_Interface>`__, meaning that there should be only one common way to accomplish an operation. For example, addition should have one syntax, ``a+b``, but this syntax should work on numerous forms. This avoids a profusion of operators such as ``+.`` for addition of floating-point in OCaml which is just noisy and hard to remember. Messing with the basic PEMDAS operations is a recipe for errors.

Internally, each exposed operation is implemented as overloading the symbol for various more specific "primitive" operations, ``(+) = lub [add_int8, add_int16, ...]``. The compiler will be able to use profiling data to observe the forms of the numbers involved and select the appropriate primitive operation, so it should always be possible to replace a direct use of the primitive ``add`` with the normal ``+`` operation without significantly affecting performance. But for expressiveness purposes, it does seem worth exposing the primitives. Conceptually, since the primitives don't overlap, each primitive ``add`` operation is the restriction of the overloaded ``(+)`` to the domain of the specific primitive, so even if we didn't expose the primitives we could define them ourselves as ``add_int8 = (+) : Int8 -> Int8 -> Int8`` and so on. It makes sense to avoid this convolutedness and simply expose the primitives directly - in one stroke, we avoid any potential optimization problems, and we also ensure that the domains of the primitives are only defined in one place (DRY). Of course, such primitives are quite low-level and most likely will only be needed during optimization, as a sanity check that the representation expected is the representation in use.

For fixed-precision integers and floating point, the operations work in stages: first, the numbers are converted to arbitrary-precision, then the operation is performed in arbitrary precision, then the result is rounded. In the case of fixed-precision integers, there are choices such as truncating (clamping/saturating), wrapping, or erroring on overflow. In the case of floating point, there are numerous rounding modes and errors as well.

Commonly, the rounding is considered part of the operation, and the rounding mode is just fixed to some ambient default, but this is not optimal with respect to performance. Herbie provides a different approach. Given a real-valued expression and assumptions on the inputs, Herbie produces a list of equivalent computations, and computes their speed and accuracy for various choices of machine types and rounding. The programmer can then choose among these implementations, selecting the fastest, the most accurate, or some trade-off of speed and precision. The question is then how to expose this functionality in the language. The obvious choice is to make the rounding operation explicit. In interpreted mode arbitrary-precision is used, at least to the precision of the rounding, and in compiled mode Herbie is used. Or something like that.

floating-point variables

* register or memory.
* on machines such as 68881 and x86, the floating registers keep excess precision. For most programs, the excess precision does only good, but a few programs rely on the precise definition of IEEE floating point.
* fast: allow higher precision / formula transformations if that would result in faster code. it is unpredictable when rounding to the IEEE types takes place and NaNs, signed zero, and infinities are assumed to not occur.
* standard: follow the rules specified in ISO C99 or C++; both casts and assignments cause values to be rounded to their semantic types
* strict: rounding occurs after each operation, no transformations
* exception handling, mode handling

Matrix multiplication
=====================

Suppose we are multiplying three matrices A, B, C. Since matrix multiplication is associative, (AB)C = A(BC). But one order may be much better, depending on the sizes of A, B, C. Say A,B,C are m by n, n by p, p by q respectively. Then computing (AB)C requires mp(n + q) multiplications, and computing A(BC) requires (m + p)nq multiplications. So if m = p = kn = kq, then (AB)C costs 2k^3 n^3, while A(BC) costs 2 k n^3, which if k is large means A(BC) is going to be much faster than multiplying (AB)C. The matrix chain multiplication algorithm by Hu Shing finds the most efficient parenthesization in O(n log n) time, given the sizes of the matrices. In practice the sizes must be observed through profiling. But this data must be collected at the level of the matrix chain  multiplication, as re-association optimisations are hard to recognise when the multiplication is expanded into loops.

Strings
=======

Text types::

  Text = Text { bytes : ByteArray#, offset : Int, length : Int } -- sequence of bytes, integers are byte offsets
  ByteString = BS { payload : Addr#, finalizer : ref Finalizers, length : Int }
  Lazy = Empty | Chunk Text Lazy

Interpolation and internationalization are two things that have to work together, copy JS i18n and Python interpolation like ``i'{x} {y}'.format(locale_dict)``.

Poison values
=============

This requires some support from the OS to implement. Pointer reads generate page faults, which if they are invalid will be returned to the program via the signal "Segmentation fault" (SIGSEGV). C/C++ `can't handle these easily <https://stackoverflow.com/questions/2350489/how-to-catch-segmentation-fault-in-linux>`__ because they are `synchronous signals <https://lwn.net/Articles/414618/>`__ and synchronous signal behavior is mostly left undefined, but in fact signals are `fairly well-behaved <https://hackaday.com/2018/11/21/creating-black-holes-division-by-zero-in-practice/>`__ (`OpenSSL <https://sources.debian.org/src/openssl/1.1.1k-1/crypto/s390xcap.c/?hl=48#L48>`__'s method of recovering from faults even seems standards-compliant). It definitely seems possible to implement this as an error value in a new language. Go `allows <https://stackoverflow.com/questions/43212593/handling-sigsegv-with-recover>`__ turning (synchronous) signals into "panics" that can be caught with recover.

UDIV by 0 on ARM simply produces 0. So on ARM producing the division by 0 error requires checking if the argument is zero beforehand and branching. The people that really can't afford this check will have to use the unchecked division instruction in the assembly module, or make sure that the check is compiled out. But on x86, DIV by 0 on produces a fault, which on Linux the kernel picks up and sends to the application as a SIGFPE. So on x86 we can decide between inserting a check and handling the SIGFPE. It'll require testing to see which is faster in typical programs - my guess is the handler, since division by zero is rare.

Null
====

``null`` is just a symbol. The interesting part is the types. A type may either contain or not contain the null value. If the type does contain null, then the null value represents an absent or uninitialized element, and should be written with a question mark, like ``Pointer?``. If the type does not contain null, then the value is guaranteed to be non-null, and the type should not have a question mark. We can formalize this by making ``?`` a type operator, ``A? = assert (null notin A); A | {null}``.

Relations
=========

There are various types of relations: https://en.wikipedia.org/wiki/Binary_relation#Special_types_of_binary_relations

The question is, what data types do we need for relations?

* Function: a function, obviously.
* Functional: This is a function too, just add a ``NoClauseDefined`` element to the result type.
* One-to-one: a function with an assertion, ``assume(forall x y; if f x == f y { assert x == y})``
* Many-to-one: A function, no constraints
* Injective: This is the converse of a function, just use the function.
* One-to-many: the converse of a function, again just use the function.
* Many-to-many: the only relation that can't be represented by a one-argument function

So, a function represents most relations, and for a many-to-many relation, we need to represent a set of tuples. There are choices of how to implement this set.

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

Wait-free
---------

:cite:`ramalheteEfficientAlgorithmsPersistent2021` claims to be wait-free. But Cliff says it's impossible to get wait-free. CAS is lock-free, about a dozen clocks on X86 (L2 miss time). But 's' You can NOT have a wait-free under any circumstance, except single-thread simulation of a multi-core device.  i.e., under current hardware a wait-free is always slower than a single core.
Still playing catchup.  Parents made it home after a 36hr delay.  😛
Sure, OS schedules threads.  As an example, some linux kernel kept 8 runnables on a core-local runnable queue for fast rotation and time-slicing.  On my 10 real-core, 20 hyper-thread machine, that would be 8x20 or 160 runnables on local queues.  I launch 1000 runnables.  One of them attempts a wait-free program shutdown by writing a single boolean "true" to a global that the other 999 threads read.  Kernel launches 120 threads on 20 cores and rotates between them.  All those 999 threads are computing e.g. Mandelbrots in a loop (might as well be bit-miners).  Kernel rotates amongst the 160, which get equal billing at 1/8 a hyper-core.  Kernel will rotate in a different thread when one of the currently running stops - which is never.  So kernel never gives a single clock cycle to the other ~820 threads.  So the stop bit thread never runs, and the program never halts.  Seen in practice during our Azul days, on non-Azul hardware.


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

Control structures
==================

 https://www.ccs.neu.edu/home/shivers/papers/loop.pdf / https://www.youtube.com/watch?v=PCzNwWmQdb0


 see also Reference/Syntax, a lot of potential control structures

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
* Transitive: ``convert T3 (convert T2 a) = convert T3 a`` (assuming all conversions succeed)

These rules avoid conversion "gotchas" where information is lost during conversion. For example all convertible numbers must be exactly representable in the target type because of transitivity and the existence of arbitrary-precision types (``convert Exact (convert Approx a) == convert Exact a``).

Conversion is only a partial function, hence these properties may not hold due to some of the conversions resulting in errors. For example ``convert Float32 (2^24+1 : Int32)`` fails because only ``2^24`` and ``2^24+2`` are exactly representable as floats. Generally one direction of the conversion should be total, or there should be subtypes like ``Float32_Int subset Float 32`` for which conversion to both ``Float32`` and ``Int32`` is total.

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

JavaScript is notorious for its pervasive and byzantine coercion rules. They are as follows:

  * 0, -0, null, false, NaN, undefined, and the empty string ("") coerce to false.
  * Objects, including empty objects {}, empty array [], all nonempty strings (including "false"), all numbers except zero and NaN coerce to true.

* binary ``+`` can combine two numbers or a string and a defined value (not null or undefined).
* unary ``+, -`` and binary ``-, *, /, %, <<, >>, >>>`` only work on numbers
* relational operators ``<, >, <=, >=`` works on two numbers or two strings
* bitwise operators ``~, &, |`` work only on numbers. For example ``m & 8192 != 8192`` parses as ``m & (8192 != 8192) = m & false``, not as intended. Forbidding using a boolean in place of a number makes it an error.
* equality is of type ``forall a. (a|undefined|null) -> (a|undefined|null) -> bool`` and does no coercions, as e.g. having ``5 == "5"`` by converting the number to a string is counterintuitive.

The counter idiom ``x = (x | 0) + 1`` seems to be hardly used, probably not worth supporting.


Equality and comparison
=======================

The comparison function itself is discussed in Posets. Basically it is a single function ``comp : Any -> Any -> {LessThan,Equal,GreaterThan,Incomparable}`` satisfying the requirements of a "partial comparison operation".

Loose comparison will perform a type conversion when comparing two things. In particular in JS it will convert objects to strings, booleans and strings to numbers, and numbers to bigints, and transitive chains of these. Loose comparison is considered a confusing mistake; equality should not do type conversion. Almost all JS programs do not use this feature, either via ``===`` or by avoiding cases that invoke conversion. :cite:`pradelGoodBadUgly2015`

`Scala <https://github.com/lampepfl/dotty/issues/1247>`__ categorizes equality as universal and multiversal. A `multiversal equality <https://github.com/lampepfl/dotty/blob/language-reference-stable/docs/_docs/reference/contextual/multiversal-equality.md>`__ is allowed to error if no suitable comparison for the two values is defined; this is a fine-grained way of saying that the values are incomparable. Due to symmetry and transitivity, the scheme partitions the universe of values into a "multiverse" of sets, where values within a set can be compared but comparison of values from different sets errors. In contrast a "universal" comparison assigns some arbitrary order to unrelated types; it creates a total order and never says two values are incomparable.

As far as defaults, it seems from googling "TypeError: '<' not supported between instances of 'str' and 'int'" that forgetting to parse a string to an int is a common error in Python - multiversal comparison makes this error obvious. Also many types such as compiled lambdas do not have a portable intrinsic ordering, and there is no canonical ordering across different types. So making the default equality and comparison operators multiversal should catch many errors. This doesn't seem like it will be too controversial: ``<=`` and ``==`` are multiversal in Java, C++, Haskell, and Rust, ``<=`` is multiversal in Python and Ruby, and Java's ``.compareTo()`` is multiversal as well. The only default universal comparison operators are Python ``==`` and maybe Java ``.equals()``. It seems Python's universal equality has bitten at least `one guy <https://lists.archive.carbon60.com/python/dev/919516>`__. Quoting in case the link breaks, "It bit me when I tried to compare a byte string element with a single character byte string (of course they should have matched, but since the element was an int, the match was not longer True)". Universal equality is easier to implement than universal comparison because you just have to return not equal, but conceptually this "not equal" actually means "greater than or less than" using the universal comparison operator, so it is the same sort of footgun. Universal comparison is just too powerful to make it the syntactic default - a little ugliness in abstract code is a small price to pay for catching indavertent errors.

Now even though universal comparison cannot be the default, it is still quite useful. In particular, a universal comparison operation based on the language's built-in notion of "contextual equivalence" is really useful, i.e. two values are considered equal if their behavior in all contexts is *functionally identical*. This is the finest-grained notion of equality available and can be used for non-linear pattern matching and writing type assertions and unit tests. For example we can extend multiversal equality ``==`` to ``(a|None)`` by delegating, ``None == a = universal_equal None a`` and its symmetric variant. Because the equality is universal we are guaranteed a boolean false rather than an exception. This built-in universal equality can be extended to universal comparison, which then allows sorting heterogeneous lists/containers, ordering record fields, doing serialization and meta-programming. So this should be available somehow. In Python universal comparison is relegated to a third-party library `safesort <https://github.com/wolever/safesort>`__ - that is definitely too obscure. Julia provides universal equality as ``===`` in the default prelude, which seems too up-front (and also confuses Javascript programmers). For now I think they will live in the default prelude as wordy functions ``universal_equal`` and ``universal_compare``. The wordy expressions seem clear and the long names will discourage their careless use. But this should be verified as maybe Julia's ``===`` is better. But what would universal comparison be? ``<==``?

What is the result of ``NaN == NaN``? IEEE 754 2019 says "Language standards should map their notation for the symbols = and ≠ to the Quiet predicates ``compareQuietEqual`` and ``compareQuietNotEqual``". This recommended behavior was decided circa 1985 so that ``x != x`` could be used to detect NaN values, in place of the ``isnan`` predicate which was not yet widely available in languages (`1 <https://stackoverflow.com/questions/1565164/what-is-the-rationale-for-all-comparisons-returning-false-for-ieee754-nan-values>`__). Thus (traditionally) ordered comparison predicates (<, >, ≤, ≥) were signaling, but unordered comparison predicates (= and ≠) were quiet, and this small inconsistency in the behavior of NaN was deemed unlikely to cause a program malfunction. And Fortran's checks for uncaught signals were very conservative so most programmers wanted as few signals as possible. (`2 <https://grouper.ieee.org/groups/msc/ANSI_IEEE-Std-754-2019/background/predicates.txt>`__) But since "should" means "preferred and recommended as particularly suitable but not necessarily required," deviating from the standard is allowed, although discouraged.

In this case I think mapping ``==`` to ``compareSignalingEqual`` has become desirable, for several reasons:

* Stroscot's floating point exception handling is quite improved compared to Fortran: it generates language-level exceptions, rather than OS-level signals. This is relevant because the behavior of ``signalingEqual`` is that we will get an exception on a NaN comparison, compared to ``quietEqual`` where we get a boolean false (with =) or true (with ≠), . At the interpreter prompt, this exception will most likely escape to the top level and prompt the user to redo their computation so as to not produce the NaN, with a nice traceback. When compiling, it will most likely generate an ignorable warning, similar to very early Fortran compilers. But Stroscot checks ranges of floating point numbers so if a NaN is not possible it will not give the warning, hence this warning will be more useful. And the exception can be handled quite naturally by pattern-matching on the result of the ``==``, whereas Fortran requires writing a program-level SIGFPE signal handler with a lot of boilerplate and no information about the exception context.
* Few novices know what NaNs are or think about behavior on NaNs. The ``compareSignalingEqual`` predicate is "intended for use by programs not written to take into account the possibility of NaN operands", so it is more novice-friendly. Even for experts the behavior of ``compareQuietEqual(NaN,NaN) = false`` is considered "counter-intuitive" and "very difficult for a programmer to accept". And since you get an error message it is quite learnable since you just have to read the message and learn about the whole sordid NaN story.
* Bertrand Meyer `has an argument <https://bertrandmeyer.com/2010/02/06/reflexivity-and-other-pillars-of-civilization/>`__ that erroring on NaN preserves monotonicity.
* It is not too hard to modify Stroscot to follow the standard: hide ``(==)`` from the default prelude and import it from IEEE. This can be accomplished at the top level in a per-project fashion so should not be too burdensome.
* The ``compareSignalingEqual`` predicate was only created relatively recently, in the 2008 standard. AFAICT no other programming language has actually tried changing the behavior of ``==`` yet. Few recent language developers have actually read the IEEE 754 standard carefully enough to notice this issue, and discussions in the context of older languages such as `Python <https://mail.python.org/pipermail/python-ideas/2012-October/016627.html>`__ were hampered by compatibility constraints. So the time seems ripe to try it out and see if changing it annoys people, makes them happy, or doesn't really matter. It is easy to stop throwing an exception but hard to start.

So (unless there is a flood of complaints) in Stroscot the result of ``NaN == NaN`` is some exceptional value like ``InvalidOperation``.

Meanwhile there is the universal comparison and equality. This is of course reflexive, so``universal_equal NaN NaN = true``. For ``universal_compare`` IEEE provides its totalOrder predicate, ordering as follows from smallest to largest:

* negative quiet NaN
* negative signaling NaN
* negative infinity
* negative numbers
* negative subnormal numbers
* negative zero
* positive zero
* positive subnormal numbers
* positive numbers (``10e1 < 1e2``)
* positive infinity
* positive signaling NaN
* positive quiet NaN

There is a little more to define because (since inspecting the representation is possible by casting to a bytestring) all NaNs and encodings must be distinguished. There should be a note that different encodings of the value will compare unequal. Printing out "f32 0x1234 (un-normalized representation of 1.0)" for these values should make the failures clear.

Deep comparison compares the values of references rather than the reference identities. It's less common in Stroscot because more things are values, but it can still be useful for mutable structures. It basically is some logic to memoize comparisons of cyclic structures and then a call to a passed-in "value comparison" which should itself call back to the deep comparison for references.

Since functions can nondeterministically return multiple values and comparing them can give multiple results, we might want equality operations anyEqual and allEqual to resolve the nondeterminism.

Generally we want to define equality on records by comparing some subset of the fields, like ``eqPoint = (==) on [x,y]`` and ``eqColorPoint = (==) on {x,y,color}``. In fact :cite:`` proposes that all object equality in a language should be declared in terms of equality of a subset of fields. But this would exclude IEEE float equality so Stroscot still allows user-defined equality.


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

Serialization is the ability to convert an object graph into a stream of bytes, and more broadly the reverse as well (deserialization). In Java the OO model was defined first and serialization was added later as a "magic function". The design has various problems, as described in `Project Amber <https://openjdk.org/projects/amber/design-notes/towards-better-serialization>`__:

* serialization can access private classes and fields, an implicit public set of accessors
* deserialization bypasses defined constructors and directly creates objects via the runtime, an implicit public constructor
* serialization/deserialization uses magic private methods and fields to guide the process, such as readObject, writeObject, readObjectNoData, readResolve, writeReplace, serialVersionUID, and serialPersistentFields
* The Serializable marker interface doesn’t actually mean that instances are serializable. Objects may throw during serialization, as e.g. Java has no way to express the constraint that a TreeMap is serializable only if the Comparator passed to the constructor is serializable. Also there are objects such as lambdas, which are easily serializable but error due to lacking Serializable, requiring special type casts.
* Serialization uses a fixed encoding format that cannot be modified to JSON/XML/a more efficient/flexible format, or one with version markers. There are no checks that serialization/deserialization is a round trip.

In Stroscot the privacy is a non-issue because everything is exposed through the internal module. Magic methods are no issue either because they are just normal multimethods. The hard parts are that Stroscot has more types of values: cyclic terms, lambdas. Ideally these would be deconstructible with term rewriting. References are also a sticking point, the store needs special handling, probably just a reference <-> refid map. We would like a friendly, generic way to write a function that can serialize all types of values, so that implementing new serialization formats in 3rd-party libraries is possible.

Amber also says the format should be versioned, because unless you plan for versioning from the beginning, it can be very difficult to version the serialized form with the tools available without sacrificing compatibility. But JSON has no version numbers, and XML only barely. So this can be folded into general library-level compatibility and versioning.

Cycles and non-serializable data
--------------------------------

Cyclic data occurs in many places, e.g. a doubly linked list ``rec { a = {next: b, prev: None}; b = {next: None, prev: a} }``. We also have non-serializable data such as finalizers that does not live across program restarts. These cannot be serialized to JSON etc. as-is, because the format doesn't support it. The solution is a replacer, which transforms cyclic and non-serializable data to a form suitable for serialization. The replacer produces a bijection from bad values to good values, so that we can serialize the good values in place of the bad values and do the opposite transformation on deserialization. Then we serialize this bijection separately (out-of-band).

It is much easier to do replacement out of band because in-band replacement leads to DOS attacks such as "billion laughs". Basically the attacker defines a system such as ``a = "lol"; b = a+a; c=b+b; d=c+c;``, etc., constructing a string of a billion laughs, or similarly a large object that takes up too much memory. A simple solution is to cap memory usage, but this means some objects fail to serialize. Instead in-band entities must be treated lazily and not expanded unless necessary. Out-of-band avoids the issue by not allowing references in data.

Function pipelines
==================

Haskell has function composition ``(.)`` and Julia has the "pipe" operator ``(|>)``.

According to `YSAGuide <https://github.com/jrevels/YASGuide#other-syntax-guidelines>`__ pipelines like ``a . b . c`` are bad style and one should instead use intermediate results, ``\x -> { a1 = a x; b1 = b a1; c1 = c b1; return b1 }``, except with better named variables than ``x,a1,b1,c1``. The reason given is that debugging composed functions in the REPL is hard and clutters stacktraces. This sounds like a debugger problem - function pipelines are shorter and easier to read.

Values
======

These are derived values, so they don't belong on the Values page.

Lists
-----

A list is a function ``Nat -> Value|EndOfList``, with the proviso that if ``list i = EndOfList`` then ``list (i+k) = EndOfList`` for all ``k>=0``. Basic list syntax is the usual syntactic sugar for list values.

::

  [] // empty list
  arr = [a, b, c]

Heterogeneous lists are possible, ``[1,"x",int32 3]``.

In Stroscot tuple is synonymous with list.


Arrays
------

An indexing scheme is a function ``IndexValue -> [0..length]``. An array is a term ``array scheme list``. A typed array also specifies an element type, ``typed_array type scheme list``. Tensors are arrays that use compound values as indexes. Sparse arrays use indexing schemes that map many indices to one value.

Strings
-------

A string is a list of bytes of a given length. Subtypes include C null-terminated strings, UTF-8 encoded strings, and characters (UTF-8 encoded strings containing exactly one grapheme cluster). Also filenames and paths.

Bitvectors
----------

A bitvector is the symbol ``bits`` applied to a list of bits, ``bits [1,0,1]``.

Date/time
=========

Various types of complete and incomplete date/time values. C.f. ISO 8601, `Wikipedia <https://en.wikipedia.org/wiki/Time_standard>`__

* Timescale: TAI, TT, GPS, TCB, TCG, TDB, UT1, TST, LTST, UTC ``Z``, utc offset ``±hh:mm``, civil timezone, smeared UTC (3+ variants)
* Instants:

  * Named instants: Rata Die, Unix epoch, GPS epoch, ... (absolute or dependent on timescale)
  * Offset time: offset, unit, instant
  * complete date-time ``±YYYYY-MM-DDThh:mm:ss.ss`` + timescale
  * time record: a collection of chronological key-value information sufficient to specify an instant (includes timescale). A date may be specified based on a calendar such as proleptic Gregorian or the many other calendars.

* duration: a time difference as a collection of time fields (including a timescale)
* partial date: a collection of time fields (including timescale) that has the semantics of replacing or overriding the fields of an instant with those of the partial date's
* recurrence: RRule as documented in the iCalendar RFC. (https://dateutil.readthedocs.io/en/stable/rrule.html)

Records
=======

A record is an ordered list of key-value pairs, ``record {a = 1, b = 2, c = 3}``.

Maps
----

A map is an unordered set of key-value pairs, with the restriction that each key has one value. ``map {a = 1, b = 2, c = 3}``

A table is a map from tuples to values.

Multimap
--------

A multimap is an unordered bag of key-value pairs. ``multimap {a = 1, a = 1, b = 2, c = 2, c = 3}``

Bags
====

A bags is a function ``numInstances :  Any -> Nat``.

Pointers
========

Pointers are just a wrapper for particular bit patterns (integers), like ``pointer 0xdeadbeef``. You can do integer arithmetic and turn it into a pointer, but at least on x86-64 not all 64-bit integers are valid pointers.

References
==========

References are a term ``ref r123``, where ``r123`` is a symbol. Most symbols are autogenerated inside the reference creation operation ``ref``, but there is no technical restriction on which symbols are allowed.

Numbers
=======

A decimal number consists of a sign and a list of digits, a decimal marker, and more digits. A floating-point number is similar but adds a base and a separately specified exponent. A rational numbers is a ratio of two integers. A complex number is a real and an imaginary component, or a polar angle and magnitude, in both cases real numbers.

Mathematical structures
=======================

A polynomial is a list or array of terms, where each term is the product of a number (coefficient) and zero or more variables, each raised to a power. A truncated power series is a polynomial in a single variable, starting from a constant and with terms increasing up to some power. A generating function is an infinite (formal) power series.

An equation is a term where the top term is ``=`` and the left and right are mathematical expressions. A system of equations is a list of equations with ``and`` applied. A differential equation is an equation including partial derivatives or derivatives with respect to time.

A probability distribution is a function from a sigma-algebra to a real number [0,1].

A special function is a function over the real or complex numbers, usually taking one or two arguments and producing one output. Modular forms are special functions.

Fields/rings/ideals are generally represented as simple symbols or terms.

Piecewise/interpolated functions are terms that contain the data as a list.

An optimization problem is a set (the search space, usually R^n or Z^n or so on with some constraints) together with an objective function mapping each possibility to a real number, that is either minimized or maximized.

Intervals
=========

An interval has a start value, an end value, and two flags for whether each side is open or closed. There is also a "ball" representation as the middle and distance to each side. The values can be numbers but also other things like date/times.

Units
=====

Units are symbols or terms. Quantities are terms ``quantity number unit``. See the Units page.

Multimedia
========

I guess we use SVG as the main format for graphics, plots, and geometric objects. Raster images are just arrays of pixel colors. Similarly audio files and time-series data are arrays of samples.

Graphs
======

Directed graphs are a set of vertices and a set of (vertex,vertex) pairs. Undirected graphs are the same but with the proviso that if (a,b) is present then (b,a) must be as well.

Chemicals
=========

There are several ways of depicting these:

* IUPAC name / CAS number - string/symbol
* `SMILES <https://en.wikipedia.org/wiki/Simplified_molecular-input_line-entry_system>`__ - a string/term
* Chemical markup language - XML
* An annotated graph. Each bond (edge) has metadata, and the vertex stores 3D coordinates.

Databases
=========

A database connection is a term with file handle data and other stuff. A database query is a term too. A database result is a list or a term or whatever.

Regular expression
==================

A regular expression is a term with symbols for each formalism, like "or", concatenation, capture groups, quantifiers, the wildcard character, character classes, and so on.

Finite-state machine
====================

A FSM is represented as some rewrite rules, each state/input rewrites to some new state.

GIS
===

A geographic point has a geographic reference and 3 coordinates. Files may store vector and raster data. Raster is just an array with point information. Vector data may be points, lines, multi-lines (strings), or polygons. Topology information encodes topological connections between different shapes.

Quantum computing
=================

Similarly to how a classical computation maps a RAM of n bits to another RAM of n bits, a quantum computation (gate) over n qubits is a unitary matrix of size 2^n by 2^n. A more structured representation involves forming quantum circuits out of qubit wires and quantum gates. There are also measure and set operations to interact with classical computations.

Logical propositions
====================

These are just formulas, terms and binders built using the logical connectives (and, or, not, forall, exists, etc.) With sequent calculus we also allow the turnstile as a symbol.

Infinite values
===============

A potentially-infinite expression is a function ``f : Nat -> Value`` that is Cauchy (under the term distance metric). Such expressions are considered equivalent to any other Cauchy sequence that is distance 0 away in the limit. There is a canonicalization or evaluation operation on potentially-infinite expressions; it either resolves the expression to a finite term, or proves that the term is not finite and resolves the expression to a canonical "simplest form" Cauchy sequence, or else errors.

Generally such expressions are generated by evaluating with "fuel". For example, computing ``let fib x y = x:(fib y (x+y)) in fib 0 1`` with progressively more fuel generates the sequence ``0:*, 0:1:*, 0:1:1:*, 0:1:1:2:*``, etc. (where ``*`` is a long expression where evaluation ran out of fuel) This is Cauchy and not equivalent to any finite term, so it will be canonicalized as a "simplest form" Cauchy sequence, maybe as something like ``infiniteValue $ \n -> let fib x y = x:(fib y (x+y)) in take n (fib 0 1)``.

One well-behaved subset of infinite values is the rational infinite values, which can be specified as the solution of a system of equations, for example ``let x=1:x in x``. These can be verified to be in normal form by ensuring no reduction in any equation.

Programming with infinite data structures is simple, natural, and practical. But it is more complex than finite data structures. Computers do not have an infinite amount of memory, so these structures are not physically represented in memory, but rather symbolically. It is generally slower to access individual elements, and the execution engine may exhibit high resource usage if the computation is complex. Overall, performance is more unpredictable compared to a finite data structure. Some computations may not work at all. A simple rule of thumb is "demand" - the execution engine analyzes which parts of the structure are required, and generally a computation will work well if only a finite portion needs to be evaluated. But there are also tricks to evaluate computations with an infinite demand. If nothing works, the execution engine fails. But there are many tricks implemented - generally, if you can do it by hand the computer can do it too.

So for example infinite lists, like the natural numbers starting with 1. We can try some operations:

- when we print this infinite list, it is going to exceed our maximum output. So the right thing is to summarize the data - something like "[1,2,3,...]", where the ... indicates output was omitted. There are many heuristics to use for summarizing output.
- summing the infinite list, this computes the limit as the list goes to infinity, so is infinity.
- filtering the even numbers from an infinite list: this gives us the infinite list of all the even numbers.
- filtering all the numbers less than 100: this again requires computing a limit, should be simple.
- take - taking a certain number of elements of the list

timeouts - is it better to just churn through a computation and let the user do Ctrl-C, or to enforce some limit like 5s per command? If the user running a long computation they will be very annoyed at an arbitrary timeout. What is more useful is "progress monitoring" - detecting "hangs" where evaluation is definitely not advancing productively.

Infinite values separate control and data - first you construct an infinite value (data), then you decide what to do with it (control). In many languages, if you wanted to generate a list of 10 primes, and a list of 100 primes, you'd need to write a function to generate a finite list, and call it twice with a different amount of stuff to generate. But with more complicated usages of infinite data structures, it's not that simple - for example, if you want the first ten even numbers, with an infinite list it's just taking elements from the filtered list, but without, you would have to back-propagate the demand to generate 1 through 20. In general there is no clear way to do it up front by allocating a finite list.

Numbers
=======

Normal numbers are simply terms, but a real number is a function ``ω+1 -> {-1,+1}`` excluding certain trivial sequences (c.f. `surreal numbers <https://en.wikipedia.org/wiki/Surreal_number>`__ S_ω "the field of real numbers" and sign expansion representation), and similarly p-adic numbers need special handling as the different metric completion of the rational numbers.

Modules
=======

Modules are a set of rewriting rules together with exported and hidden symbols. These symbols are qualified to the module's value, in something of a recursive knot. The knot can be resolved by expanding the module out as an infinite term.

Qualified symbols
=================

A qualified symbol or identifier is a term consisting of a bare symbol together with a module. A literal looks like ``qsym <module> sym123``. Normally you use the period operator ``.`` and write ``module.sym``, but the period operator is a function so this is an expression not a value. Examples of qualified symbols include the predefined symbols of the prelude, like ``null``, ``true``, and ``false`` - the latter two forming the boolean type.

Portable operations
===================

Across platforms, hardware assembly instructions vary and many will likely not exist. So we need to abstract this. The API structure is that we have "native" modules providing direct hardware instructions, which give compile-time errors on attempts to use it on non-native platforms. Then on top we have a portable library that provides a cross-platform interface using switch statements like ``case platform of A -> implA; B -> implB; ...``. Most hardware operations are ubiquitous, so it makes sense to expose them directly as portable operations. Actually, it is hard to think of a reason not to expose a hardware primitive - probably every assembly instruction for our supported platforms should be exposed as a portable operation.

Addition wraps on all 64-bit hardware in the same way so only needs one portable operation. Other instructions like division have differing behavior, so we can provide 0-returning (ARM native) and ``DivideByZero`` exception-throwing (Intel native) division as portable operations. There is also the intersection of these functions with signature ``Int -> Int\{0} -> Int``, which is useful when we want zero-overhead on multiple platforms. Ideally the compiler will be able to prove that the divisor is non-zero and so both of the 0-returning/exception-throwing versions will compile to one instruction on both ARM and x86.

Then we also want portable APIs at a higher level, such as the limb-multiply routine in libGMP.

Supporting different platforms (Linux, macOS, Windows, etc) is hard:

* certain things aren't portable (e.g. the assembly used for switching thread stacks)
* differences between Linux distributions - llvm-sys doesn't compile on Alpine Linux, and so Inko doesn't support Alpine Linux for the time being.
* macOS ARM64 runners cost $0.16 per minute, and isn't available for forks / third-party contributors
* Some platforms aren't supported on CI runners at all, like FreeBSD, so you need to use qemu or similar software to run FreeBSD in a VM

Recommendation: err on the side of not supporting platforms, and document this, instead of sort-of-but-not-quite supporting it.