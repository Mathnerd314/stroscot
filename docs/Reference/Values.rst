Values
######

Values are `immutable <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg>`__ and have notions of equality, comparison, literal syntax, and deconstruction. Values can be copied freely and discarded if they are no longer needed.

Strictly, values are defined to be normal forms. Some confusion arises because of WHNF and weak reduction, but here we assume strong reduction. Expressions include values and terms containing reducible expressions.

Values are described here in their typical syntactic forms for convenience. These syntactic forms typically are reducible terms, rather than values, but they reduce to a value of the compiler's preferred data structure implementation. Programs may define the syntax here to other values.

Symbols
=======

Symbols are unique values defined in modules and addressed by strings. Bare symbols consist Of a sequence of letters (including the underscore) and digits, starting with a letter. Case is significant, thus ``foo``, ``Foo``, and ``FOO`` are distinct identifiers. Other strings can be turned into symbols with ``@``, for example ``@"null"``. In addition operator symbols can be defined with parentheses and a sequence of punctuation characters, e.g. ``(+)``. There is also the empty parentheses symbol ``()``.

::

  symbol
  underscore_symbol
  unícσdє-symβol
  (++)
  @"a long symbol"

Examples of predefined symbols include ``null``, ``true``, and ``false`` - the latter two forming the boolean type.

Namespacing
-----------

Identifiers can be qualified by periods: ``a.b.c``. ``.`` is an infix left-associative operator that binds tighter than juxtaposition.

Term
====

A term is a symbol applied to other values.

::

  tree (combine (leaf 1 2) (leaf 1 2))
  (++++) a b
  (*) (some (weird thing)) 12

Note that if there is an appropriate syntax rule the second example could also be written as ``a ++++ b``, but it would be an equivalent value.

Terms subsume algebraic data types, since an ADT value is a symbol applied to other values.

Numbers
=======

In the mathematical world the definition of `number <https://en.wikipedia.org/wiki/Number#Main_classification>`__ variously refers to integers, rationals, real numbers, complex numbers, and/or other mathematical structures like p-adics or the surreal numbers. (the "numeric tower") But most of these are composite structures. In the computer world the primitive notion of "number" is a literal that looks like a number.

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

Number formats
--------------

Numbers can have a suffix interpreted as the format. This expand to a term that specifies the format by applying it, e.g.  ``123i8`` expands to ``int8 123``. Formats include IEE 754 float/double, signed and unsigned fixed bit-width integers, and fixed-point rationals.

Complex
-------

These are just a term ``complex a b`` representing ``a + b*i`` where ``a,b`` are real numbers. Maybe it is also worth having ``complex_polar r t = r*exp (i*t)``.

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

The string is raw bytes terminated with a null character, like in C, or a length plus raw bytes.
Often strings are encoded in UTF-8.

Character
---------

A “character” is not just a single Unicode code point. For example, “G” + grave-accent is a character represented by two Unicode code points, and emojis similarly have lots of code points. Unicode calls characters "grapheme clusters" and provides an algorithm for identifying them in UAX #29. The main notable feature of the algorithm is that a grapheme cluster may be arbitrarily long due to the use of combining characters/accents and ZWJs, for example in Zalgo text, hence a character must be repesented as a variable-length sequence of codepoints. Hence it is simplest and most correct to define a character as a Unicode string of grapheme length 1.

Date/time
=========

Date/time values are written using symbols applied to strings, lists, or records using ISO 8601 style formats, e.g. ``instant "2011-12-03T10:15:30.999999999Z"``, ``gregorianDate [2010,12,03]``, or ``time { hour = 10, minute = 10, second = 12.3 }``. This hides all internal representation details. Internally there is a more compact form, e.g. a 128-bit number.

Binary data
===========

Most data in a computer simply sits in storage and has no easily accessible interpretation. It is simply a sequence of bits. As such Stroscot provides binary data values. These are just a list of bits, ``bits [1,0,1]``, but as a separate datatype the bits can be stored compactly.

There is also a binary/hex literal syntax:

::

  base = 0[a-z]
  digit = [0-9a-fA-F_]

  data = base digit+

We allow various base prefixes ``0?`` - ``x`` (hexadecimal), ``o`` (octal), ``d`` (decimal) and ``b`` (binary), but extensible to other bases. The decimal base expands to the shortest binary string that can contain that decimal. So for example ``0b010 = bits [0,1,0]``.

Another way to write data is as a string ``bits "abcd\x0F"`` which makes use of UTF-8 characters and hexadecimal for invalid byte sequences.

Lists
======

A list represents an ordered sequence of values; it may be empty, finite, or infinite.

::

  arr = a : [b, c]

Basic list syntax is the same as in Haskell, thus ``[]`` is the empty list, ``x:xs`` denotes a list with head element ``x`` and tail list ``xs``, and the usual syntactic sugar for list values in brackets is also provided, thus ``[x,y,z]`` is exactly the same as ``x:y:z:[]``. But ``:`` isn't typed so you can write ``1:2`` for example, or have heterogeneous lists. More advanced list operations are done with the efficient ``++`` operator.

A list has push and pop operations from head and tail so can be used as a stack, queue, or double-ended queue (dequeue).

Tuple
-----

In Stroscot tuple is synonymous with list. There's not a different type with different semantics like in Python or Pure. But you can use the tuple syntax ``(a,b)`` as is convenient.

Arrays
------

(Immutable) arrays are lists together with an indexing scheme. The indexing scheme specifies the length of the list and how index values map to integer indexes of the list. For example ``array (range_inclusive 1 3) [1,2,3]`` defines a 1-based array where ``arr[i] = i``. Maybe there is also an element type, ``typed_array int32 (range_inclusive 1 3) [1,2,3]``

Mutable arrays are a reference pointing to an immutable array. There is also an array of mutable cells but this is not used often as mutable array operations are optimized to in-place operations.

Tensors
-------

Tensors are just nested lists, e.g. here is a (3,2,5)-sized rank 3 tensor:

::

  [[[0, 1, 2, 3, 4],
    [5, 6, 7, 8, 9]],
   [[10, 11, 12, 13, 14],
    [15, 16, 17, 18, 19]],
   [[20, 21, 22, 23, 24],
    [25, 26, 27, 28, 29]]]

If you want to save a bit of bracket typing you can use ``reshape`` on a flat list:

::

  reshape (3,2,5)
    [0, 1, 2, 3, 4,
     5, 6, 7, 8, 9,
     10, 11, 12, 13, 14,
     15, 16, 17, 18, 19,
     20, 21, 22, 23, 24,
     25, 26, 27, 28, 29]

Or similarly use a 3D array:

::

  array (range 0 3, range 0 2, range 0 5) [0,1,2,...,29]

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
    # {a = 1, b = 4, c = 3, d = 5}

Maps
----

Maps are the same as records except the fields are not ordered (set of pairs).

Multimap
--------

A multimap is a map where the values are nonempty bags.

Sets
====

Sets are unordered lists with no repeated values, similar to a map whose values are all the symbol ``present`` or a function ``isElemOf : Any -> {Present|Absent}``.

::

  set [1,2,3]

Bags
====

Bags are unordered multisets, similar to a map whose values are nonnegative integers.

::

  bag [1,1,2,3]

Priority queue
--------------

This is a bag plus an ordering operation.

Functions
=========

Functions are first-class and hence values. Equality is determined by alpha beta eta equality (i.e., beta reduce to normal form, eta reduce, and compare via alpha equivalence).

Cyclic values
=============

Sometimes it is useful to deal with solutions to a system of equations, like ``let x=cons 1 x in x``. These are also values - there is a way to compute the (unique) minimal graph representation.

Modules
=======

Modules are also first class, they are discussed in their own page.

Pointers
========

Pointers are just a wrapper for particular bit patterns (integers), like ``pointer 0xdeadbeef``. You can do integer arithmetic and turn it into a pointer, but at least on x86-64 not all 64-bit integers are valid pointers.

References
==========

References are like pointers but use symbols instead of integers, we'll go with ``Ref r123`` for syntax where ``r123`` is a symbol. The main difference from a pointer is that you can't do arithmetic on symbols. Most symbols are autogenerated inside the reference creation operation ``ref``, but you can also write reference values directly. This is mainly for convenience in debugging at the REPL, since fixed symbols are tantamount to global variables and hence are bad programming practice.


Data Structures
===============

Arrays
    Array
    Bit array
    Bit field
    Bitboard
    Bitmap
    Circular buffer
    Control table
    Image
    Dope vector
    Dynamic array
    Gap buffer
    Hashed array tree
    Lookup table
    Matrix
    Parallel array
    Sorted array
    Sparse matrix
    Iliffe vector
    Variable-length array

Lists

    Singly/Circular/Doubly Linked list
    Array list
    Association list
    Self-organizing list
    Skip list
    Unrolled linked list
    VList
    Conc-tree list
    Xor linked list
    Zipper
    Doubly connected edge list also known as half-edge
    Difference list
    Free list

Trees
  Binary trees
    AA tree
    AVL tree
    Binary search tree
    Binary tree
    Cartesian tree
    Conc-tree list
    Left-child right-sibling binary tree
    Order statistic tree
    Pagoda
    Randomized binary search tree
    Red–black tree
    Rope
    Scapegoat tree
    Self-balancing binary search tree
    Splay tree
    T-tree
    Tango tree
    Threaded binary tree
    Top tree
    Treap
    WAVL tree
    Weight-balanced tree
  B-trees
    B-tree
    B+ tree
    B*-tree
    Dancing tree
    2–3 tree
    2–3–4 tree
    Queap
    Fusion tree
    Bx-tree
  Heaps
    Heap
    Binary heap
    B-heap
    Weak heap
    Binomial heap
    Fibonacci heap
    AF-heap
    Leonardo heap
    2–3 heap
    Soft heap
    Pairing heap
    Leftist heap
    Treap
    Beap
    Skew heap
    Ternary heap
    D-ary heap
    Brodal queue
  Bit-slice trees - each tree node compares a bit slice of key values.
    Radix tree (compressed trie), Patricia tree
    Bitwise trie with bitmap
    Suffix tree
    Suffix array
    Compressed suffix array
    FM-index
    Generalised suffix tree
    B-tree
    Judy array
    X-fast trie
    Y-fast trie
    Merkle tree
  Multi-way trees
    Ternary tree
    K-ary tree
    And–or tree
    (a,b)-tree
    Link/cut tree
    SPQR-tree
    Spaghetti stack
    Disjoint-set data structure (Union-find data structure)
    Fusion tree
    Enfilade
    Exponential tree
    Fenwick tree
    Van Emde Boas tree
    Rose tree
  Space-partitioning trees
    Segment tree
    Interval tree
    Range tree
    Bin
    K-d tree
    Implicit k-d tree
    Min/max k-d tree
    Relaxed k-d tree
    Adaptive k-d tree
    Quadtree
    Octree
    Linear octree
    Z-order
    UB-tree
    R-tree
    R+ tree
    R* tree
    Hilbert R-tree
    X-tree
    Metric tree
    Cover tree
    M-tree
    VP-tree
    BK-tree
    Bounding interval hierarchy
    Bounding volume hierarchy
    BSP tree
    Rapidly exploring random tree
  Application-specific trees
    Abstract syntax tree
    Parse tree
    Decision tree
    Alternating decision tree
    Minimax tree
    Expectiminimax tree
    Finger tree
    Expression tree
    Log-structured merge-tree

Hash-based structures

    Bloom filter
    Count–min sketch
    Distributed hash table
    Double hashing
    Dynamic perfect hash table
    Hash array mapped trie
    Hash list
    Hash table
    Hash tree
    Hash trie
    Koorde
    Prefix hash tree
    Rolling hash
    MinHash
    Quotient filter
    Ctrie

Graphs
    Graph
    Adjacency list
    Adjacency matrix
    Graph-structured stack
    Scene graph
    Decision tree
        Binary decision diagram
    Zero-suppressed decision diagram
    And-inverter graph
    Directed graph
    Directed acyclic graph
    Propositional directed acyclic graph
    Multigraph
    Hypergraph

Other

    Lightmap
    Winged edge
    Quad-edge
    Routing table
    Symbol table
    Piece table
