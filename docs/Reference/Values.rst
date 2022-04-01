Values
######

Values are `immutable <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg>`__ and have standard notions of equality, comparison, literal syntax, and deconstruction. Values can be copied freely and discarded if they are no longer needed.

TODO:  ISO/IEC 11404 General purpose datatypes

Values are either primitive, meaning no sub-values, or composite, containing substructures (which are values as well.)

Values as described here are assumed to be in normal form, meaning no reduction rules apply to them. But programs may use syntax that does not necessarily correspond to a normal form to describe values, and similarly may define reduction rules that reduce values to other expressions. Semantically a value that reduces to something else is not a value anymore, but nonetheless we still call it a value.

Atomic values
^^^^^^^^^^^^^

The values here are primitive, meaning they are defined without reference to other datatypes, and atomic, meaning the values are intrinsically indivisible.

Symbols
=======

Symbols are unique values defined in modules and addressed by strings. Bare symbols consist Of a sequence of letters (including the underscore) and digits, starting with a letter. Case is significant, thus ``foo``, ``Foo``, and ``FOO`` are distinct identifiers. Other strings can be turned into symbols with ``@``, for example ``@"null"``. In addition operator symbols can be defined with parentheses, ``(+)``. There is also the empty parentheses ``()``.

::

  symbol
  underscore_symbol
  unícσdє-symβol

Examples of predefined symbols include ``null``, ``true``, and ``false`` - the latter two forming the boolean type.

Namespacing
-----------

Identifiers can be qualified by periods: ``a.b.c``. ``.`` is an infix left-associative operator that binds tighter than juxtaposition.

Number literals
===============

In the mathematical world the definition of `number <https://en.wikipedia.org/wiki/Number#Main_classification>`__ variously refers to integers, rationals, real numbers, complex numbers, and/or other mathematical structures like p-adics or the surreal numbers. (the "numeric tower") But in the computer world the atomic notion of "number" is a literal that looks like a number.

Integers
--------

::

  4711
  4711L
  1.2e+3

Notations for integers (decimal ``1000``, hexadecimal ``0x3e8``, octal ``0o1750``)  are provided.
Integers can also be denoted in base 2 by using the ``0b``  prefix: ``0b1111101000``.
Positive exponents with decimal (e) / hexadecimal (p) / binary (b), and ``_`` as digit separator.
Leadings 0's are significant - literals with leading zeros must be stored in a type that can hold the digits all replaced with their highest value, e.g. ``0001`` cannot be stored in a ``i8`` (type must be able to contain ``9999``).

::

  base = 0[a-z]
  digit = [0-9a-fA-F_]
  pos_exponent = [a-zA-Z] +? [0-9_]+
  format = [a-zA-Z] identifier

  integer = base? digit+ pos_exponent? format?

Rationals
---------

::

  exponent = [a-zA-Z] [+-]? [0-9_]+
  float = base? digit+(\.digit+)? exponent? format?

  rational = float | float '/' float

Number syntax is `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__, but liberalized. We allow floats to be the numerator/denominator because writing out ``1e99 / 1e-99`` would be tedious.

The normal form of a rational is ``AeB / C`` where ``A`` and ``C`` are relatively prime and ``C`` is not  zero and not divisible by 10.

Strings
=======

::

  "Hello world!\n"
  ``Hello user ${id}``
  [Enclosed text]
  'string'
  """ multiline
  string"""

Double and single quotes are both supported, as well as a multi-line syntax.
There is no explicit syntax for characters, instead characters are strings of length 1.
Escape sequences are defined; the main ones are ``\"`` to escape a quote and ``\\`` to escape a backslash, the others aren't relevant to parsing the literal.

String concatenation is ``++``.

The string is raw bytes terminated with a null character, like in C.
Often strings encoded in UTF-8.

Time
====

The JSR-310 `ThreeTen <https://www.threeten.org/>`__ library in `Java <https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html>`__ seems to have undergone the most peer review. It relies heavily on ISO 8601. Another is http://time4j.net/. For some reason these are all Java. Rust has a basic thing in `std <https://doc.rust-lang.org/std/time/index.html>`__. There is a more complete Rust library but the author is `opinionated <https://github.com/time-rs/time/issues/406#issuecomment-989753838>`__.




%L - Millennium
%C - Century
%X - Decade
%Y - Year
%M - Month
%D - Day
%V - Week Year
%W - Week
%w - Week Day
%O - Ordinal Day

%h - Hour
%m - Minute
%s - Second
%u - Microsecond

%Z - Zone Hour including +/-
%z - Zone Minute

%[,.]3x - Value including fraction with given precision, using either comma or dot.
%−Z     - Use U+2212 for negative timezone hours (ISO recommended)



Generally values are represented using symbols applied to strings, e.g. ``instant "2011-12-03T10:15:30.999999999Z"``, ``localDate '2010-12-03'``, etc. This hides all internal representation details.

Internally there is a more compact form, e.g. a 128-bit number.

Fixed-point types are like ``x*2^-5 years`` where ``x`` is an integer and ``2^-5 years`` is the resolution. But you still use ISO 8601 syntax, it just gets rounded into the representation (earliest time such that the interval represented by the less precise contains the interval represented by the more precise).

Binary data
===========

Most data in a computer simply sits in storage and has no easily accessible interpretation. It is simply a sequence of bits. As such Stroscot provides binary data values. These are just a list of bits, ``bits [1,0,1]``, but as a separate datatype the bits can be stored compactly.

There is also a binary/hex literal syntax:

::

  base = 0[a-z]
  digit = [0-9a-fA-F_]

  data = base digit+

We allow various base prefixes ``0?`` - ``x`` (hexadecimal), ``o`` (octal), ``d`` (decimal) and ``b`` (binary), but extensible to other bases. The decimal base expands to the shortest binary string that can contain that decimal. So for example ``0b010 = bits [0,1,0]``.

Another way to write data is as a string ``bits "abcd\x0F"`` which makes use of UTF-8 characters and hexadecimal for invalid byte sequences, but this doesn't work for null characters.

Aggregate values
^^^^^^^^^^^^^^^^

Term
====

A term is a symbol applied to other values.

::

  tree (combine (leaf 1 2) (leaf 1 2))
  (++++) a b
  (*) (some (weird thing)) 12

Note that if there is an appropriate syntax rule the second example could also be written as ``a ++++ b``, but it would be an equivalent value.

Expressions subsume algebraic data types, since an ADT value is a symbol (constructor) applied to other values.


Number formats
--------------

Numbers can have a suffix interpreted as the format. This expand to a term that specifies the format by applying it, e.g.  ``123i8`` expands to ``int8 123``.

Reals
-----

The real numbers and numeric spaces containing it are uncomputably large. In practice only a subset of  (computable) values is accessible. Generally we take the integers and rationals and consider the closure under operations:
* constants (e, pi)
* arithmetic ops (addition, subtraction multiplication, division)
* other ops (sqrt, pow, sin, cos, abs, find root of polynomial in interval, definite integral, etc.)

These operations can be written out as terms, e.g. ``sin (7/2)``, so to support the real numbers only appropriate symbols and defined operations are needed.

p-adics and surreals
--------------------

Similarly to reals, p-adics and surreal numbers are represented using integers, rationals, constants, and operations on them.

Complex
-------

These are just a term ``complex a b`` representing ``a + b*i`` where ``a,b`` are real numbers. Maybe it is also worth having ``complex_polar r t = r*exp (i*t)``.

Lists
======

Lists can represent the result of binary operations that are associative. But lists don't automatically flatten, e.g. ``[a,[b,c]] != [a,b,c]`` (although there is a flatten function in the standard library)

::

  arr = a : [b, c]

  sum [1,2,3]
  product [2,3,4]

Basic list syntax is the same as in Haskell, thus ``[]`` is the empty list, ``x:xs`` denotes a list with head element ``x`` and tail list ``xs``, and the usual syntactic sugar for list values in brackets is also provided, thus ``[x,y,z]`` is exactly the same as ``x:y:z:[]``. But ``:`` isn't typed so you can write ``1:2`` for example, or have heterogeneous lists.

Immutable arrays are lists that have a statically-known length and constant element storage size, which allows efficient (packed) storage.

In Stroscot tuple is synonymous with lists. There's not a different type with different semantics like in Python or Pure.

Tensors
-------

Tensors are just nested arrays, e.g. here is a (3,2,5)-sized rank 3 tensor:

::

  [[[0, 1, 2, 3, 4],
    [5, 6, 7, 8, 9]],
   [[10, 11, 12, 13, 14],
    [15, 16, 17, 18, 19]],
   [[20, 21, 22, 23, 24],
    [25, 26, 27, 28, 29]]]

As with lists they can be heterogeneous.

If you want to save a bit of bracket typing you can use ``reshape`` on a flat array:

::

  reshape (3,2,5)
    [0, 1, 2, 3, 4,
     5, 6, 7, 8, 9,
     10, 11, 12, 13, 14,
     15, 16, 17, 18, 19,
     20, 21, 22, 23, 24,
     25, 26, 27, 28, 29]

There is also a ``matrix`` DSL which turns semicolons into rows.

::

  matrix [1,2;3,4]
  # [[1,2],[3,4]]

Records
=======

Records are like C structs or Python dictionaries. The order of the fields is remembered, so this data type is a list of key-value pairs.

::

  rec = {a = 1, b = 2, c = 3}
  rec.a # 1
  rec[a] # 1
  {a = x} = rec # x = 1
  {a,b} = rec # a = 1, b = 2
  # record update
  rec // {b=4, d = 4}
    # {a = 1, b = 4, c = 3, f = 5}

Maps
====

Maps are associative arrays, very similar to records except the fields are not ordered (commutative/sorted list).

Sets
====

Sets are like commutative/sorted lists with no repeated values or maps where the value is always the symbol ``present``.

The literal syntax is just ``set`` applied to a list.

::

  set [1,2,3]

Functions
=========

Functions are first-class and hence values. Equality is determined by alpha beta eta equality (i.e., beta reduce to normal form, eta reduce, and compare via alpha equivalence).

Cyclic values
=============

Sometimes it is useful to deal with solutions to a system of equations, like ``let x=cons 1 x in x``. These are also values - there is a way to compute the (unique) minimal graph representation.

Modules
=======

Modules are also first class, they are discussed in their own page.

References
==========

References are values, we'll go with ``ref 123`` for syntax. They are not persistent, meaning the value can change between program runs and the details of the value is an implementation detail. Also they are unforgeable, writing ``ref 123`` in a program will give an error. But the REPL allows writing ``ref 123`` for convenience in debugging.

Pointers
========

Pointers are values as well, they are just particular bit patterns. ``pointer 0xdeadbeef``.
