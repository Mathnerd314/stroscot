Syntax
######

Almost everything in Stroscot is an expression. The basics are numbers, booleans, and character strings of text. But there's also block statements and layout.

Design
======

The stuff here is mostly a placeholder while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and picking out the nicest examples. But in the end, syntax is decided by usage, so a lot of the syntax here will probably become final.

Quorum and its associated set of syntax studies provide useful datapoints on keywords and constructs. But Stroscot has a unique design so we can't use a lot of the research, and the research is limited to begin with.

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Legibility
----------

The main factor improving readability is consistency; reading is disrupted when unconventional layouts are used.

Spacing is important to identify word boundaries (intra-letter spacing significantly smaller than inter-word spacing) and sentence boundaries (two spaces after the period, although the period's whitespace itself is distinctive). Justified text is harder to read than ragged-right due to the inconsistent spacing arising from bad line-breaking. Left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line. The default line spacing seems fine.

Line length is a good question. Programming uses fixed-width characters so it's measured in characters. 80 characters is standard, but monitors are wider now, so 100 is plausible. Diff programs are often the limiting factor, but on my monitor I can fit 2 108-character texts side-by-side along with a space in the middle and the taskbar. 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt (24px) I can fit 97 characters, while 23px fits 102 characters.

Unicode
=======

Unicode has various guidelines relevant to writing a programming language parser:

* UTF-8 Decoding. Replace invalid bytes/characters with Unicode's REPLACEMENT CHARACTER U+FFFD.
* `NFC <http://unicode.org/reports/tr15/#Norm_Forms>`__, normalize the input / ensure the input is normalized  before doing anything with it. There is enough software that automatically normalizes to NFC (e.g. web browsers) that it seems safe to require NFC; bugs can be worked around by changing the input rather than modifying NFC.
* `line-breaking <https://www.unicode.org/reports/tr14/#BreakingRules>`__ (specifically, to determine hard / mandatory breaks)
* `word-breaking <http://www.unicode.org/reports/tr29/#Word_Boundary_Rules>`__ to split up lines into tokens - it needs to be extended to account for program identifiers / multicharacter symbols
* `identifier syntax <https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax>`__, which specifies sets of valid identifier start/continue characters
* `confusables <http://www.unicode.org/reports/tr39/#Confusable_Detection>`__, which seems useful for writing a similarly-named identifiers warning. There could also be a warning for weird scripts (listed in `TR31 <http://www.unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers>`__) or zero-width characters.

Practically, most programs will use ASCII, but the Unicode algorithms provide a robust way to implement a lexical analyser. `Lots of languages <https://rosettacode.org/wiki/Unicode_variable_names>`__ have support for Unicode, although the exact set of allowed characters varies.

Layout
======

Blocks of sequential statements are a common occurrence in a program. The most obvious is the initial declaration list, but other constructs introduce clauses as well. For readability, clauses may span multiple lines, so some way of distingishing the start / end of clauses must be defined. Generally, this amounts to adding braces and semicolons so as to make it layout-insensitive. The braces are virtual braces; they don't match with explicit braces.

::

  a
   b
   c
  d

  # becomes
  { a {b; c}; d}

Generally, behavior of a new line depends on its indentation level, relative to the indentation of the previous line:

* if it is indented more, it's a sequence given as an argument to the previous line, so a virtual open brace is inserted
* if it is at the same level, another item in the sequence, so a (virtual) semicolon is inserted
* if there is a (nonempty) line at lower indentation (or EOF), the sequence is ended as it's a new declaration (`offside rule <https://en.wikipedia.org/wiki/Off-side_rule>`__). A virtual close brace is inserted at the start of the line.

Indentation level is taken to be the sequence of whitespace characters, so "space, tab, space" is different from (incomparable to) "tab, space, space" but is less than "space, tab, space, em space" and more than "space, tab".

Layout handling is complicated by the presence of grammar rules without layout that allow free choice of indentation, e.g.

::

  a
    + b
    + c
  # equal to
  a {+ b; + c}
  # should be equivalent to
  a + (b + c)

It should be possible to handle these with a fixup phase.

Also, closed operators (e.g. parentheses) inhibit layout; this amounts to skipping whitespace layout when inside an explicit delimiter pair. But of course constructs inside the delimiter pair can start another layout. Finally for constructs that usually use layout we still want to parse 1-line things without braces:

::

  let a = b in c
  # equivalent to
  let { a = b } in c

Parsing
=======

I've got a basic Earley algorithm working for now. But eventually I'm extending it with BSRs and layout and other fun things. There's also `Yakker <https://github.com/attresearch/yakker>`__, which is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

  A new parsing engine, Yakker, capable of handling the requirements of modern applications including full scannerless context-free grammars with regular expressions as right-hand sides for defining nonterminals. Yakker also includes facilities for binding variables to intermediate parse results and using such bindings within arbitrary constraints to control parsing. Yakker supports both semantic actions and speculative parsing techniques such as backtracking and context-free lookahead and several parsing back ends (including Earley, GLR and backtracking).  In addition, nonterminals may be parameterized by arbitrary values, which gives the system good modularity and abstraction properties in the presence of data-dependent parsing. Finally, legacy parsing libraries, such as sophisticated libraries for dates and times, may be directly incorporated into parser specifications.

I've looked at various algorithms but I think the only way to handle it completely correctly and generically is to have a disambiguating pass on an ambiguous parse tree. The alternatives involve generating extra parser states or using PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

Treesitter implements incremental LR parsing with error recovery, but since it doesn't support ambiguity I don't think it's sufficient for a compiler.

Long-term, the goal is to use partial evaluation to generate the parser, by speeding up a naive brute-force algorithm applied to the grammar. There is already a paper on LR parsing by partial evaluation :cite:`sperberGenerationLRParsers2000` and also on specializing Earley, so with sufficiently powerful compiler optimization handling general grammars should be possible.

Numbers
=======

::

  (0[box])?[0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([eEpP][+-]?[0-9_]+)?

Number syntax is `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__, slightly liberalized to allow using floating-point notation for integers and binary exponents for decimals.

Strings
=======

::

  "Hello world!"
  ``Hello user ${id}``
  [Enclosed text]
  'string'
  """ multiline
  string"""

There is no explicit syntax for characters, instead characters are Unicode strings of length 1.

Arrays
======

::
  arr = [a, b, c]
  arr[0] # a

Sequences and slices:

::

  [..]
  [minBound..]
  [minBound..maxBound]
  [minBound,minBound+1..maxBound]
  slice(list, 0, 2)
  slice(list, a, length list - b)

Monad comprehensions

Records
=======

::

  rec = {a = 1, b = 2, c = 3}
  rec.a # 1
  rec[a] # 1
  {a = x} = rec # x = 1
  {a,b} = rec # a = 1, b = 2
  # record update
  rec // {b=4, d = 4}
    # {a = 1, b = 4, c = 3, f = 5}

Atoms
=====

Atoms are any identifiers that don't have a grammar rule defined.
::

  atom
  underscore_atom
  unícσdє-αtσm
  symbol ++++ tree

Patterns
========

::

   _ # matches anything
   a # matches anything and binds a
   ^a # matches the atom a
   [(1, "x"), {c: 'a'}] # literal matching itself
   [1, ...] # matches any list starting with 1
   {a: 1, ...: rest} # matches a and the rest of the record
   pat AND pat # matches both patterns simultaneously
   pat OR pat # matches either pattern
   ~pat # desugars to u_ = let pat = u_ in ..., where u_ is a unique name

Guards allow arbitrary functions:

::

   a with a > 0

View patterns

::

   (f -> a)

Functions patterns

::

   Int z = toInteger z

   Int a

Pattern synonyms

::

   pattern F a b = ["f",a,b]

Arbitrary patterns

::

   _f a # matches any function application


Operators
=========

Operator precedence will be a DAG, rather than levels.::

  precedence _*_ higher than _+_
  precedence _/_ equals _*_

Stroscot supports your typical PEMDAS:

::

  1 + 2 * 3^2
  --> 19
  3+1/(7+1/(15+1/1))
  --> 355/113 = 3.14159292035...

String concatenation is ``+``, but most other operators are textual:

::

   true and false = false
   true or false = true
   true xor true = false
   5 div 2 = 2
   5 mod 2 = 1
   raise x by 1

New operators can be declared with `mix <http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf>`__ `fix <http://www.bramvandersanden.com/publication/pdf/sanden2014thesis.pdf>`__ semantics, e.g.

::

   syntax _&&_ associate left above _and_ _or_ _not_ below _||_

Umatched Parentheses
--------------------

For brevity, trailing parentheses can be omitted:

::

   3+1/(7+1/(15+1/1
   --> 355/113

If you don't like this, you can set Stroscot to warn or error on
unmatched parentheses, or run the code formatter which will add them.

Chained Comparison
------------------

::

  1 <= 2 < 3
  9 > 2 < 3

Blocks
======

::

  if true then 1 else 2 = 1
  repeat while x > 0 { x -= 1 }
  repeat until x == 0 { x -= 1 }
  repeat 10 times { x -= 1 }
  repeat { x -= 1 } while x > 0
  repeat
    x = x * 2
    if (x % 2 == 0)
      break

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

Misc
====

::

   x = input number
   display x

::

   // comment
   /* multiline
   comment */
   {- nesting {- comment -} -}
   if(false) { code_comment }

Scoping and qualification
=========================

There is no kind of syntax or semantics for changing or redefining identifiers (besides :ref:`fexprs <fexprs>`); you can shadow, with warning, but once an identifier is declared in a scope, that's what that identifier refers to for the duration of the scope.

Variables
=========

::

  a = mut 1
  a := 2

Mutable variables are completely distinct from name binding, so we have distinct notation for setting them.

Type declarations
=================

::

  a = 2 : s8
  a = s8 2

These two options seem more logical compared to other choices such as ``a : s8 = 2`` (Swift,Jai - hard to find the = with long types) or ``s8 a = 2`` (C,Rust - overlaps with function definition). The name is simply a syntactic handle to refer to the value; it doesn't have an innate type. In contrast the representation of the value must be specified to compile the program.

The second syntax is similar to assembler syntax such as ``dword 0``.

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
