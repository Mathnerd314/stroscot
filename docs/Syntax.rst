Syntax
######

Almost everything in Stroscot is an expression. The basics are numbers, booleans, and character strings of text. But there's also block statements and layout.

The stuff here is mostly a placeholder while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode) and picking out the nicest examples. Short and terse syntax is good, as is flexibility and generality. But in the end, syntax is decided by usage, so imitation is the best way to make new users of Stroscot feel comfortable.

Numbers
=======

::

  (0[box])?[0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([eEpP][+-]?[0-9_]+)?

Number syntax is `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__, slightly liberalized to allow using floating-point notation for integers and binary exponents for decimals.

Arithmetic and Parentheses
==========================

Stroscot supports your typical arithmetic operations and precedence levels:

::

   3+1/(7+1/(15+1/1))
   --> 355/113 = 3.14159292035...

For brevity, trailing parentheses can be omitted:

::

   3+1/(7+1/(15+1/1
   --> 355/113

If you don't like this, you can set Stroscot to warn or error on
unmatched parentheses, or run the code formatter which will add them.

Strings
=======

::

   "Hello world!"
   ``Hello user ${id}``
   [Enclosed text]
   'short'

Structured Data
===============

Stroscot supports records and arrays.

::

  {a = 1, b = 2, c = 3}
  [a, b, c]

Sequences and slices:

::

  [..]
  [minBound..]
  [minBound..maxBound]
  [minBound,minBound+1..maxBound]
  slice(list, 0, 2)
  slice(list, a, length list - b)

Atoms
=====

Any undefined identifier can be used as an atom, i.e. a component of a symbolic expression. For example this will produce ``4`` if ``foo`` is undefined:

::

  f (foo x) = x + 2

  f (foo 2)

Due to the fexpr semantics any expression can be used and pattern-matched, like ``javascript (1 + "abc" { 234 })``.

To export the semantics to other modules a special keyword ``atom`` is used:

::

  atom foo

This ensures that no rules for ``foo`` are defined and adds the symbol to the export list. It is good practice to use this even if the identifier is not exported.

Operators
=========

String concatenation is ``+``, but most operators are textual:

::

   true and false = false
   true or false = true
   true xor true = false
   5 mod 2 = 1
   raise x by 1

New operators can be declared with `mix <http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf>`__ `fix <http://www.bramvandersanden.com/publication/pdf/sanden2014thesis.pdf>`__ semantics, e.g.

::

   syntax _&&_ associate left above _and_ _or_ _not_ below _||_

Blocks
======

::

   if true then 1 else 2 = 1
   repeat while x > 0 { x -= 1 }
   repeat until x == 0 { x -= 1 }
   repeat 10 times { x -= 1 }
   repeat { x -= 1 } while x > 0

::

   procedure foo
     x = 0
     x += 1
     provide x

   obtain http_server
   procedure main
     parse_args
     build_folder
     http_server.serve(folder)

::

   check {
     risky_procedure
   } error {
     fix(error) or error("wtf")
   } regardless {
     save_logs
   }

::

   x = input number
   display x

::

   // comment
   /* multiline
   comment */
   {- nesting {- comment -} -}
   if(false) { code_comment }

Parsing
=======

I've got a basic Earley algorithm working for now. But eventually I'm extending it with BSRs and layout and other fun things. There's also `Yakker <https://github.com/attresearch/yakker>`__, which is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

  A new parsing engine, Yakker, capable of handling the requirements of modern applications including full scannerless context-free grammars with regular expressions as right-hand sides for defining nonterminals. Yakker also includes facilities for binding variables to intermediate parse results and using such bindings within arbitrary constraints to control parsing. Yakker supports both semantic actions and speculative parsing techniques such as backtracking and context-free lookahead and several parsing back ends (including Earley, GLR and backtracking).  In addition, nonterminals may be parameterized by arbitrary values, which gives the system good modularity and abstraction properties in the presence of data-dependent parsing. Finally, legacy parsing libraries, such as sophisticated libraries for dates and times, may be directly incorporated into parser specifications.

Operator precedence will be a DAG, rather than levels.::

  precedence _*_ higher than _+_
  precedence _/_ equals _*_

I've looked at various algorithms but I think the only way to handle it completely correctly and generically is to have a disambiguating pass on an ambiguous parse tree. The alternatives involve generating extra parser states or using PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

Treesitter implements incremental LR parsing with error recovery, but since it doesn't support ambiguity I don't think it's sufficient for a compiler.

Long-term, the goal is to use partial evaluation to generate the parser, by speeding up a naive brute-force algorithm applied to the grammar. There is already a paper on LR parsing by partial evaluation :cite:`sperberGenerationLRParsers2000`, so with sufficiently powerful compiler optimization handling general CFG grammars by specializing Earley etc. should be possible.

Layout
======

Blocks of sequential statements are a common occurrence in a program. The most obvious is the initial declaration list, but other constructs introduce clauses as well. For readability, clauses may span multiple lines, so some way of distingishing the start / end of clauses must be defined. Generally, this amounts to adding braces and semicolons so as to make it layout-insensitive. The braces are virtual braces; they don't match with explicit braces.

::

  a
   b
   c
  d

  # becomes
  { a b c; d}

Generally, behavior of a new line depends on its indentation level, relative to the indentation of the previous line:

*  if it is indented more, a continuation of the previous line, so nothing is inserted (`offside rule <https://en.wikipedia.org/wiki/Off-side_rule>`__)
* if it is at the same level, another item in the sequence, so a semicolon is inserted
* if there is a (nonempty) line at lower indentation, the sequence is ended (a close brace is inserted at the start of the line)

This is complicated by the presence of nested layouts and grammar rules without layout that allow free choice of indentation. To use the example above, if ``a`` started a layout we would have wanted ``{ a {b;c}; d}`` instead. Also, closed operators (e.g. parentheses) inhibit layout; this amounts to skipping whitespace layout when inside an explicit delimiter pair. But of course constructs inside the delimiter pair can start another layout. Finally we also want to parse 1-line things without braces:

::

  let a = b in c
  # let { a = b } in c

Type declarations
=================

Types in Stroscot act as identity functions restricted to a certain domain. So you use an application, similar to assembly syntax such as ``dword 0``:

::

   a = Int8 2

To match Haskell, there is also a standard operator ``(:)`` defined as ``x : y = y x``, with low precedence, so you can write

::

   a = 2 : Int8

These two options seem more logical compared to other choices such as ``a : Int8 = 2`` (Swift,Jai - hard to read with long types) or ``Int8 a = 2`` (C,Rust - overlaps with function definition). The name is simply a syntactic handle to refer to the value; it doesn't have an innate type. In contrast the representation of the value must be specified to compile the program.

Variables
=========

::

  a = mut 1
  a := 2

Mutable variables are completely distinct from name binding, so we have distinct notation for setting them.

DSL
===

Stroscot aims to be a "pluggable" language, where you can write syntax, type checking, etc. for a small DSL like SQL and then use it in a larger program with some embedding syntax.

::

  run_sql_statement { SELECT ... }

The idea extends further, embedding lower-level and incompatible languages like assembly and C++.

::

  result = asm { sumsq (toregister x), (toregister y) }
  my_func = load("foo.cpp").lookup("my_func")

Another useful one might be TeX / mathematical expressions:

::

   tex { result = ax^4+cx^2 }
   math { beta = phi lambda }

These are particularly useful with functions that fuse multiple operations such as expmod and accuracy optimizers that figure out the best way to stage a computation.

Namespacing
===========

Identifiers can be qualified by periods: ``a.b.c``. ``.`` is an infix left-associative operator that binds tighter than juxtaposition. ``.`` is preferred to ``::`` because it's shorter and because modules are first-class.