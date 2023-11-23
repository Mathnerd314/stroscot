Values
######

In Stroscot values are defined to be expressions that are in normal form (strongly reduced), i.e. they evaluate to themselves. WHNF is not sufficient to ensure a value, e.g. ``[1,undefined]`` reduces to ``undefined`` hence is not a value. Traditionally a function is only defined on values, but lazy evaluation allows functions to produce useful behavior for non-values as well.

Values are immutable (as `Rich Hickey says <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg>`__) and have notions of equality, hashing, literal syntax, and deconstruction. In terms of memory management values can be copied freely, and discarded if they are no longer needed.

For convenience, "value" really describes the equivalence class of expressions that reduce to a value. In particular here we describes values by their concise syntactic form. These syntactic forms typically are reducible terms, rather than normal forms - usually they reduce to a value of the compiler's preferred data structure implementation. Programs may define the syntax here to other values.

Symbols
=======

Symbols or identifiers are unique values. Bare symbols can be either ordinary or extended, vaguely like `Raku <https://docs.raku.org/language/syntax#Identifiers>`__. However symbols defined in modules are qualified to that module, hence more specific.

::

  symbol
  underscore_symbol
  unícσdє-symβol
  (++)
  @"a long symbol"

Ordinary
--------

An ordinary identifier matches the pattern ``alphanum (alphanum|((apostrophe|hyphen)alphanum))*``, i.e. a sequence of digits/letters/underscores and isolated/embedded apostrophes or hyphens.

"Alphabetic" means Unicode General Category value Letter (L), and the underscore _. "Numeric" includes characters with the Unicode General Categories value Number and Decimal Digit (Nd).

Case is significant, thus ``foo``, ``Foo``, and ``FOO`` are distinct identifiers.

Extended
--------

Extended symbols use an escape sequence to allow more freedom. They have the form ``@"sym str"`` (from Zig), which allows spaces etc. following the syntax for a string. An extended symbol matches a corresponding ordinary symbol if that form exists, i.e. ``null == @"null"``.

In addition parsing rules for operator symbols may be suppressed with parentheses. For example ``(+)`` is equivalent to ``@"+"``. There is also the empty parentheses symbol ``()``, called "unit".

Qualified
---------

A qualified symbol is a bare symbol together with a module reference. A literal looks like ``qsym <module> sym123``. Normally you use the period operator ``.`` and write ``module.sym``, but the period operator is a function not a constructor.

Examples of qualified symbols include the predefined symbols of the prelude, like ``null``, ``true``, and ``false`` - the latter two forming the boolean type.

It is key to support side-by-side execution of multiple versions of the same functionality. So the module part of a qualified name includes a version number and a cryptographic hash.

Namespacing
-----------

Identifiers can be qualified by periods: ``a.b.c``. ``.`` is an infix left-associative operator that binds tighter than juxtaposition. This gets resolved to a nondeterministic set of qualified names.

Term
====

A term is a symbol applied to other values.

::

  tree (combine (leaf 1 2) (leaf 1 2))
  (++++) a b
  (*) (some (weird thing)) 12

Note that if there is an appropriate syntax rule the second example could also be written as ``a ++++ b``, but it would be an equivalent value.

Terms subsume algebraic data types, since an ADT value is also a symbol applied to other values. An ADT is a "free" term that has no reduction rules defined, or conversely a term is an "untyped" ADT data constructor with no restrictions on its arguments.

Term application also includes the other function syntax - keyword arguments, implicit arguments, and so on. This argument syntax is not usually reduced away, so is part of the term's value.

Lists
======

A list represents an ordered sequence of values that is empty or finite, but not infinite (only terms can be infinite).

Basic list syntax is the usual syntactic sugar for list values.

::

  [] // empty list
  arr = [a, b, c]

``arr`` translates to a Lisp-like term ``list a b c``, making use of the term syntax's capability to take variadic arguments. Heterogeneous lists are possible, ``list 1 "x" (int32 3)``.

Haskell's cons operator is abandoned in favor of concatenation:

::

  xs ++ ys // concatenation
  [x] ++ xs // list with head element ``x`` and tail list ``xs``, like cons

``++`` is just a symbol, so you can write ``[1] ++ 2`` for example, i.e. the term ``(++) (list 1) 2``.

A list has push and pop operations from head and tail so can be used as a stack, queue, or double-ended queue (dequeue).

Tuple
-----

In Stroscot tuple is synonymous with list - they're both immutable. There's not a different type with different semantics like in Python or Pure. You can use the tuple syntax ``(a,b)`` in place of list syntax ``[a,b]`` whenever convenient.

Arrays
------

(Immutable) arrays are lists together with an indexing scheme. The indexing scheme specifies the length of the list and how index values map to integer indexes of the list. For example ``array (range_inclusive 1 3) [1,2,3]`` defines a 1-based array where ``arr[i] = i``. Maybe there is also an element type, ``typed_array int32 (range_inclusive 1 3) [1,2,3]``

Mutable arrays are a reference pointing to an immutable array. Operations are optimized by the memory system, so it does in-place operations where possible but can still resize the array. Conceptually you are doing ``(read arr)[0]`` to get the first element, i.e. taking an immutable snapshot and then reading/modifying it. This is hidden normally because ``arr[0]`` and ``arr[0] := 1`` are overloaded to read/write mutable arrays.

There is also an array of mutable cells (bytes), similar to C pointers / arrays. You can do something like ``readOffset Int 0 ptr``. You can read a different type than you wrote, and it doesn't have to be aligned (although aligned accesses may be faster depending on architecture). This type is useful for low-level munging but mutable arrays are probably safer.

:cite:`Tremblay` says that "allowing the size of arrays to be decided at run time [...] introduces considerable implementation problems and interferes with compile-time error checking. This feature may be of only limited value in certain applications areas." But Storscot is based on an interpeter model - so the only time the size of an array could be decided is at run-time.

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

Strings
=======

A string is a sequence of bytes of a given length. Subtypes include null-terminated strings like C and UTF-8 encoded strings.

.. code-block:: none

  "Hello world!\n"
  ``Hello user ${id}``
  [Enclosed text]
  'string'
  """ multiline
  string"""

Double and single quotes are both supported, as well as a multi-line syntax.
Escape sequences are defined:

.. code-block:: none

  \newline Backslash and newline ignored
  \\ Backslash (\)
  \' Single quote (')
  \" Double quote (")

  \a ASCII Bell (BEL)
  \b ASCII Backspace (BS)
  \f ASCII Formfeed (FF)
  \n ASCII Linefeed (LF)
  \r ASCII Carriage Return (CR)
  \t ASCII Horizontal Tab (TAB)
  \v ASCII Vertical Tab (VT)

  \0 null byte
  \ooo Byte with octal value ooo
  \xhh Byte with hex value hh
  \N{name} Codepoint with name, abbreviation or alias 'name' in the Unicode database
  \nnnn Codepoint with decimal value nnnn. The maximum value of a codepoint is 1114111.
  \uxxxx Codepoint with hex value xxxx. The maximum value is hexadecimal 10ffff.
  \& Backslash and ampersand ignored. The purpose of this escape sequence is to make it possible to write a numeric escape followed immediately by a regular ASCII digit.
  \^[@A-Z[\\]^_] caret control code notation (does anyone use?)

There is also a binary/hex literal syntax to abbreviate ``\xAA\xBB\xCC`` as ``0xAABBCC``: We allow various base prefixes - ``0x`` (hexadecimal), ``0o`` (octal), ``0d`` (decimal) and ``0b`` (binary). The decimal base expands to the shortest binary string that can contain that decimal. So for example ``0d6 = 0b110 = bits [1,1,0]``.

::

  base = 0[a-z]
  digit = [0-9a-fA-F_]

  data = base digit+

Characters
----------

There is no explicit syntax for characters, instead a character is a Unicode string containing exactly one grapheme cluster. Unicode provides an algorithm for identifying grapheme clusters in UAX #29. The main notable feature of the algorithm is that a grapheme cluster / character is not just a single Unicode code point and may be arbitrarily long due to the use of combining characters/accents and ZWJs. For example, “G” + grave-accent is a character represented by two Unicode code points, and emojis similarly have lots of code points, as does Zalgo text. Hence a character is in general an arbitrary length sequence of codepoints and it is simplest and most correct to define a character as a type of string.

Bitvectors
==========

Most data in a computer simply sits in storage and has no easily accessible interpretation. It is simply a sequence of bits. As such Stroscot provides bitvector values to represent binary data.

The normal form of a bitvector is just the symbol ``bits`` applied to a list of bits, ``bits [1,0,1]``. The symbol marks that the list should be stored compactly.

A more compact way to write a bitvector is via a string ``bits "abcd\x0F"``. This syntax uses UTF-8 characters and hexadecimal escapes, but is limited to expressing bitvectors whose length is a multiple of 8.

Date/time
=========

Date/time values are written using symbols applied to strings, lists, or records using ISO 8601 style formats, e.g. ``instant "2011-12-03T10:15:30.999999999Z"``, ``gregorianDate [2010,12,03]``, or ``time { hour = 10, minute = 10, second = 12.3 }``. This hides all internal representation details. Internally there is a more compact form, e.g. a 128-bit number.

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

Once you get to four values, it is best to make a record with named entries instead of using a tuple.

Maps
----

Maps are the same as records except the fields are not ordered (set of pairs).

::

  map {a = 1, b = 2, c = 3}

Multimap
--------

A multimap is a map where the values are nonempty bags.

::

  multimap {a = 1, a = 1, b = 2, c = 2, c = 3}
  -- same as
  map {a = bag [1,1], b = bag [2], c = bag [2,3]}

Sets
====

Sets are the mathematical definition, i.e. a function ``isElemOf : Any -> {Present|Absent}``. They may be specified by logical formulas. Finite sets may be specified as lists with no repeated values, similar to a map whose values are all the symbol ``Present``.

::

  universalSet = set (\_ -> Present)
  a = set [1,2,3]
  -- equivalent to
  b = map { 1 = Present, 2 = Present, 3 = Present }
  a = set (\x -> lookup {default=Absent} b x)

More notation for sets is discussed on the :ref:`Sets` page.

Bags
====

Bags are unordered multisets, similar to a map whose values are nonnegative integers.

::

  bag [1,1,2,3]

Priority queue
--------------

This is a bag plus an ordering operation.

Lambdas
=======

Lambdas are first-class and hence values. Equality is determined by alpha beta eta equality (i.e., beta reduce to normal form, eta reduce, and compare modulo alpha equivalence).

Modules
=======

Modules are also first class, they are discussed in their own page.

Infinite values
===============

Sometimes it is useful to deal with values that are solutions to a system of equations, like ``let x=cons 1 x in x``. These are also values. For terms with no reduction rules, there is a way to compute the (unique) minimal graph representation, where loops in the graph represent infinite cyclic repetitions. There are also infinitely reducible expressions, e.g. ``let fib x y = cons x (fib y (x+y)) in fib 0 1`` requires infinitely many reductions to reduce to an infinite normal form (infinite list value).

Rewriting system
================

A rewriting system consists of a set of rewrite rules. They are defined over a fixed abstract rewriting system called the "substitution calculus" consisting of the proofs from Stroscot's core logic, where reduction is cut elimination. Free variables etc. are incorporated by extending the ``Use`` rule. Terms are representatives of equivalence classes of proofs under ``<->*`` of the substitution calculus. Contexts are similarly representatives of precontexts.

A (conditional) rewrite rule has the form ``l -> r | C1, ..., Cn`` where ``l`` and ``r`` are both terms. The conditions take the form of predicates ``Pi(x1, ..., xm, ->)``, where the ``xi`` are the free variables of ``l`` and ``r``, and ``->`` is the rewrite relation of the system. An unconditional rewrite rule ``l -> r`` is one where the conditions ``Ci`` are always true. Example predicates are:

* type predicates, term must be of a certain form
* ``a`` joins with, rewrites to, or is convertible to ``b``

A term ``M`` rewrites to a term ``N`` by a rewrite rule ``l -> r | Ci`` if, for some context ``C`` with one hole, and substitution ``σ``, the propositions ``M <->* C[l /. σ]``, ``C[r /. σ] <->* N``, and ``Ci /. σ`` all hold, where ``C[l]`` means ``C`` with the hole substituted by ``l``, and ``<->*`` is the relation of the substitution calculus.

Pointers
========

Pointers are just a wrapper for particular bit patterns (integers), like ``pointer 0xdeadbeef``. You can do integer arithmetic and turn it into a pointer, but at least on x86-64 not all 64-bit integers are valid pointers.

References
==========

References are like pointers but use symbols instead of integers, we'll go with ``Ref r123`` for syntax where ``r123`` is a symbol. The main difference from a pointer is that you can't do arithmetic on symbols. Most symbols are autogenerated inside the reference creation operation ``ref``, but you can also write reference values directly. This is mainly for convenience in debugging at the REPL, since fixed symbols are tantamount to global variables and hence are bad programming practice.

Postfix ++ and -- are statements

Data Structures
===============

.. code-block:: none

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
      Ternary search tree
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
