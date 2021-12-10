Values
######

Values are `immutable <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg>`__ and have standard notions of equality, comparison, literal syntax, and deconstruction. Values can be copied freely and discarded if they are no longer needed.

Values are either basic, meaning no sub-values, or composite, containing substructures (which are values as well.)

Doing logic in Stroscot is confusing because the reduction semantics itself uses logic. The proof tree in the reduction semantics is the program being executed, while the proof tree in type theory is automatically deduced from the type (formula) by a meta-program (theorem prover).

Symbols
=======

Symbols or atoms are unique values.

::

  atom
  underscore_atom
  unícσdє-αtσm

To export the symbol to other modules a special keyword ``symbol`` is used:

::

  symbol foo

It is good practice to use the ``symbol`` keyword even if the identifier is not exported.

Examples of predefined atoms include ``null``, ``true``, and ``false``.

Symbol tree
===========

So long as an expression has no reduction rules, it is treated as a symbol tree (expression tree, or simply tree) and can be freely composed with other values.

::

  tree (combine (leaf 1 2) (leaf 1 2))
  (++++) a b
  (*) (some (weird thing)) 12

Note that if there is an appropriate syntax rule the second example could also be written as ``a ++++ b``, but it would be an equivalent value.

Symbol trees subsume algebraic data types, since an ADT value is a symbol (constructor) applied to other values.

TODO: some way to ensure that no rules for a class of expressions are defined in the module.

Numbers
=======

In the mathematical world the definition of `number <https://en.wikipedia.org/wiki/Number#Main_classification>`__ variously refers to integers, rationals, real numbers, complex numbers, and/or other mathematical structures like p-adics or the surreal numbers.

Integers
--------

::

  base = 0[a-z]
  digit = [0-9a-fA-F_]
  pos_exponent = [a-zA-Z] +? [0-9_]+
  format = [a-zA-Z] identifier

  integer = base? digit+ pos_exponent? format?

Positive exponents with decimal (e) / hexadecimal (p) / binary (b), and ``_`` as digit separator.
Leadings 0's are significant - literals with leading zeros must be stored in a type that can hold the digits all replaced with their highest value, e.g. ``0001`` cannot be stored in a ``i8`` (type must be able to contain ``9999``).

Formats
~~~~~~~

There is an optional suffix interpreted as the format, e.g. ``100i32`` for an integer. But these simply expand to  symbol tree that specifies the format by applying a format conversion function to it, e.g. ``int8 123``.

Rationals
---------

::

  exponent = [a-zA-Z] [+-]? [0-9_]+
  float = base? digit+(\.digit+)? exponent? format?

  rational = float | float '/' float

Number syntax is `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__, but liberalized. We allow floats to be the numerator/denominator because writing out ``1e99 / 1e-99`` would be tedious.

Reals
-----

The real numbers and numeric spaces containing it are uncomputably large, in practice only a subset can be considered values. Generally we take the integers and rationals and consider the closure under operations:
* constants (e, pi)
* arithmetic ops (addition, subtraction multiplication, division)
* other ops (sqrt, pow, sin, cos, abs, find root of polynomial in interval, definite integral, etc.)

These operations can be written out as symbol trees, e.g. ``sin (7/2)``, so to support the value representation only a file with appropriate symbols is needed.

p-adics and surreals
--------------------

Similarly to reals, p-adics and surreal numbers are integers, constants, and operations on them.

Complex
-------

The value is just a standard symbol tree ``complex a b`` representing ``a + b*i`` where ``a,b`` are real numbers. Maybe it is also worth having ``complex_polar r t = r*exp (i*t)``.

Lists
======

Lists are also called tuples or immutable arrays. They can represent the result of binary operations that are associative.

::

  arr = [a, b, c]

  sum [1,2,3]
  product [2,3,4]

Strings
=======

::

  "Hello world!"
  ``Hello user ${id}``
  [Enclosed text]
  'string'
  """ multiline
  string"""

There is no explicit syntax for characters, instead characters are strings of length 1.

Escape sequences are defined; the main ones are ``\"`` to escape a quote and ``\\`` to escape a backslash, the others aren't relevant to parsing the literal.

String concatenation is ``++``.

Time
====

The JSR-310 `ThreeTen <https://www.threeten.org/>`__ library in `Java <https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html>`__ seems to have undergone the most peer review. It relies heavily on ISO 8601.

The values are represented using symbols applied to strings, e.g. ``instant "2011-12-03T10:15:30.999999999Z"``, ``localDate '2010-12-03'``, etc.

Binary data
===========

Most data in a computer simply sits in storage and has no easily accessible interpretation. It is simply a sequence of bits. As such Stroscot provides binary data values. These are just a list of bits, ``bits [1,0,1]``, but as a separate datatype the bits can be stored compactly.

But there is also a literal syntax:

::

  base = 0[a-z]
  digit = [0-9a-fA-F_]

  data = base digit+

We allow various base prefixes ``0?`` - ``x`` (hexadecimal), ``o`` (octal), ``d`` (decimal) and ``b`` (binary), but extensible to other bases. The decimal base expands to the shortest binary string that can contain that decimal.

Another way to write data is as a string ``bits "abcd\x0F"`` which makes use of UTF-8 characters and hexadecimal for invalid byte sequences.

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

Maps are associative arrays, very similar to records except the fields are not ordered. (Or are ordered by a fixed comparison operation)

Sets
====

Sets are like sorted lists with no repeated values or maps where the value is always the symbol ``present``.

The literal syntax is just ``set`` applied to a list.

::

  set [1,2,3]

Functions
=========

Functions are first-class and hence values. Equality is determined by higher-order unification and so is mostly syntactic.

Modules
=======

Modules are also first class, they are discussed in their own page.

Cyclic values
=============

Sometimes it is useful to deal with solutions to a system of equations, like ``x=cons 1 x``. These are also values.

References
==========

References are values, although they are not persistent, meaning no literal syntax. They can change between program runs and the details of the value are obscure implementation details.

Pointers
========

Pointers are values as well, they are just particular bit patterns. ``pointer 0xdeadbeef``.
