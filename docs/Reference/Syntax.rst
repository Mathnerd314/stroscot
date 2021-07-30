Syntax
######

Almost everything in Stroscot is an expression. Values are numbers, booleans, and character strings of text. But there's also block statements and layout.

Unicode
=======

Practically, most programs will use ASCII. But the Unicode algorithms are robust and supporting other languages isn't too hard. `Lots of languages <https://rosettacode.org/wiki/Unicode_variable_names>`__ have support for Unicode, although the exact set of allowed characters varies.

* Start with bytes. Decode using UTF-8, replacing invalid bytes/characters with Unicode's REPLACEMENT CHARACTER U+FFFD.
* `NFC <http://unicode.org/reports/tr15/#Norm_Forms>`__ normalize the input, warning if input isn't normalized. There is enough software that automatically normalizes to NFC (e.g. web browsers) that it seems safe to require NFC; bugs can be worked around by changing the input rather than modifying NFC.
* A warning for weird scripts (listed in `TR31 <http://www.unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers>`__) or zero-width characters.

Some combination of the following algorithms to do lexical analysis:
* `line-breaking <https://www.unicode.org/reports/tr14/#BreakingRules>`__ (specifically, to determine hard / mandatory breaks)
* `word-breaking <http://www.unicode.org/reports/tr29/#Word_Boundary_Rules>`__ to split up lines into tokens - it needs to be extended to account for program identifiers / multicharacter symbols
* `identifier syntax <https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax>`__, which specifies sets of valid identifier start/continue characters

Stroscot is case-sensitive.

Layout
======

The most obvious is the initial declaration list, but other constructs introduce clauses as well. For readability, clauses may span multiple lines, so some way of distingishing the start / end of clauses must be defined. Generally, this amounts to adding braces and semicolons so as to make it layout-insensitive. The braces are virtual braces; they don't match with explicit braces.

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

Revisiting this, the goal is to use partial evaluation to generate the parser, by speeding up a naive brute-force algorithm applied to the grammar. There is already a paper on LR parsing by partial evaluation :cite:`sperberGenerationLRParsers2000` and also on specializing Earley, so with sufficiently powerful compiler optimization handling general grammars should be possible.

In particular the parser should be written as a nondeterministic finite state transducer that builds up trees (outputs a list in the style of start-children-end or S-expressions or something).

Formally:

    Q is a finite set, the set of states;
    I is a subset of Q, the set of initial states;
    F is a subset of Q, the set of final states; and
    Σ is a finite set, called the input alphabet;
    Γ is a finite set, called the output alphabet;
    The transition function is of type :math:`Q \times (\Sigma \cup \{\epsilon \})\to P(Q \times (\Gamma \cup \{\epsilon \}))`, where ε is the empty string and P(Q) denotes the power set of Q.

TODO: match this up with Parsec, attoparsec, trifecta, etc. the syntax should be similar except with nondeterministic choice ``|``.

Operators
---------

Operator precedence will be a DAG, rather than levels.::

  precedence _*_ higher than _+_
  precedence _/_ equals _*_

Stroscot supports your typical PEMDAS:

::

  1 + 2 * 3^2
  --> 19
  3+1/(7+1/(15+1/1))
  --> 355/113 = 3.14159292035...

Most other operators are textual:

::

   true and false = false
   true or false = true
   true xor true = false
   5 div 2 = 2
   5 mod 2 = 1

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

Values
======

Values are immutable and have built-in notions of equality, comparison, literal syntax, and deconstruction. Values can be copied freely and discarded if they are no longer needed. Typical values are strings, numbers, lists, maps, ADTs, ... with the substructures values as well.

Values include all the core expressions, ADT elements and lambda expressions and so on. Doing logic in Stroscot is confusing because the reduction semantics itself uses logic. The proof tree in the reduction semantics is the program being executed, while the proof tree in type theory is automatically deduced from the type (formula) by a meta-program (theorem prover).

Numbers
-------

::

  (0[a-z])?[0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([a-zA-Z][+-]?[0-9_]+)?[A-Z]?

Number literals are parsed into records like ``NumberLiteral { string = "123e24" }``. Leadings 0's could be significant, e.g. ``010`` could be different from ``10``. Defined ``0x`` sequences allow ``x`` to be ``x`` (hexadecimal), ``o`` (octal), and ``b`` (binary). ``p10/P10`` is a binary exponent, ``e10`` is a decimal exponent.

Then there is an optional suffix (usually interpreted as a type).

Strings
-------

::

  "Hello world!"
  ``Hello user ${id}``
  [Enclosed text]
  'string'
  """ multiline
  string"""

There is no explicit syntax for characters, instead characters are Unicode strings of length 1.

Escape sequences are defined; the main ones are ``\"`` to escape a quote and ``\\`` to escape a backslash, the others aren't relevant to parsing the literal.

String concatenation is ``++``.

Arrays
------

Immutable arrays are also called tuples or lists.

::
  arr = [a, b, c]
  arr[0] # a
  length arr # 3

Mutable arrays (arrays stored in a variable) are what people usually call arrays

   arr = mut [1,2,3]
   arr[0] # 1
   arr[1] := 4
   length arr # 3

Sequences and slices:

::

  [..]
  [minBound..]
  [minBound..maxBound]
  [minBound,minBound+1..maxBound]
  slice(list, 0, 2)
  slice(list, a, length list - b)

Monad comprehensions

All arrays are immutable and can contain arbitrary types of data, so we could also call them tuples or lists.

Records
-------

Records are like convenient hash maps, or C structs.

::

  rec = {a = 1, b = 2, c = 3}
  rec.a # 1
  rec[a] # 1
  {a = x} = rec # x = 1
  {a,b} = rec # a = 1, b = 2
  # record update
  rec // {b=4, d = 4}
    # {a = 1, b = 4, c = 3, f = 5}

Symbols
-------

If an expression tree has no reduction rules, it is treated as a symbol tree. Symbols are essentially data constructors and can be freely applied to construct data.

::

  atom
  underscore_atom
  unícσdє-αtσm
  symbol ++++ tree
  some (weird thing) * 12

To export the symbol to other modules a special keyword ``symbol`` is used:

  ::

    symbol foo

This ensures that no rules for ``foo`` are defined in the module. It is good practice to use the ``symbol`` keyword even if the identifier is not exported.

Examples of predefined atoms include null, true, and false.

Variables
=========

There is no kind of syntax or semantics for changing or redefining identifiers (besides :ref:`fexprs <fexprs>`); you can shadow, with warning, but once an identifier is declared in a scope, that's what that identifier refers to for the duration of the scope. OTOH references behave pretty much like mutable variables.

::

  a = mut 1
  a := 2
  raise a by 1

Mutable assignment (``:=``) is completely distinct from name binding (``=``). They have distinct notation.


Functions
=========

::

  f 1 = 1
  f 2 = 2
  f y | y != 1 && y != 2 = 3

::

  f
  | 1 y = 1
  | x 2 = 2
  | x y = 3

Patterns
--------

Patterns all compile to guard conditions on ``$args``. They also check that the arity of ``$args`` is the number of patterns.

::

   _ --> True
   a --> True -- binds a
   [(1, "x"), {c: 'a'}] -> $args[i] == [(1, "x"), {c: 'a'}] -- literal match
   [1, ...] --> $args[i][0] == 1 -- matches any list starting with 1
   {a: 1, ...: rest} --> $args[a] == 1 # matches a and the rest of the record
   pat1 AND pat2 --> match $args pat1 and match $args pat2 # matches both patterns simultaneously
   pat1 OR pat2 --> match $args pat1 or match $args pat2 # matches either pattern
   ~pat --> True # desugars to f u_ ... = let pat = u_ in ..., where u_ is a unique name
   (a : b) --> a elemOf b # type tag
   a | f a --> f a # guard, arbitrary function
   (f -> a) --> match (f $args[i]) a # view pattern
   ^a --> $args[i] == a -- matches the atom a
   ^f a b c --> $args[0] == f # matches the symbol tree with atom f
   f a --> $args.length >= 2 # matches any symbol tree besides a single atom

Pattern synonyms

::

   pattern F a b = ["f",a,b]

Inline definitions
------------------

Patterns can be made inline; they are lifted to the closest scope that allows definitions.

::

   range = sqrt((dx=x1-x0)*dx + (dy=y1-y0)*dy)

   -- translates to
   dx=x1-x0
   dy=y1-y0
   range = sqrt(dx*dx + dy*dy)


Keyword arguments
-----------------

::

   foo w x y z = z - x / y * w

   v = foo (y:2) (x:4) (w:1) (z:0)
   # 0-4/2*1
   v == foo {x:4,y:2,w:1,z:0}
   # true

Positional arguments
--------------------

::

   v == foo 1 4 2 0
   # true

You can mix positional and keyword arguments freely; positions are
assigned to whatever is not a keyword argument.

::

   v == foo {z:0} {w:1} 4 2
   # true

Arguments are curried:

::

   c y = y+10
   b x = c

   b 2 1
   # 11

Implicit arguments
------------------

These behave similarly to arguments in languages with dynamical scoping.

::

   -- standard library
   log s = if (priority > loglevel) { logPrint s }

   -- components of an application
   foo = log "foo" { priority = DEBUG }
   bar = log "bar" { priority = WARNING }

   -- main file
   logPrint x = writeFile file x
   file = "a"
   loglevel = DEBUG

   main =
     foo
     bar
     foo {file="b"}

``loglevel`` is defined close to the top level, but each use
site is scattered in the code. The implicit argument replaces
the global variable that is often used.
Similarly ``logPrint`` is passed implicitly instead of being a member of a global Logger instance.

Because

Claim: Explicit argument passing cannot replace our implicit variable example

The file variable does not exist in the standard
library; it is part of the user's code. To use explicit argument passing
would require adding new arguments to log, or modifying main to store print partially-applied, but this would break anyone
else using the library. Not to mention that just one intervening
function is rare and we'd probably need to modify 20 or 30 functions in
a bad case.


Using positional arguments inhibits passing positional arguments
implicitly:

::

   bar = foo + 2
   baz a = bar {x:4,y:2} - a

   v + 2 == baz 0 {z:0,w:1}
   # true
   v + 2 == baz 1 _ _ 0
   # Error: too many arguments to baz

Similarly keyword arguments inhibit passing down that keyword
implicitly:

::

   a k = 1
   b k = k + a

   b {k:2}
   # Error: no definition for k given to a

A proper definition for b would simply omit k:

::

   a k = 1
   b = k + a

   b {k:2}
   # 3

For functions with no positional arguments, positions are assigned
implicitly left-to-right:

::

   a = x / y + y
   a 4 1
   # 5

Atoms that are in lexical scope are not assigned positions, hence (/)
and (+) are not implicit positional arguments for a in the example
above. But they are implicit keyword arguments:

::

   a = x / y + y
   a {(+):(-)} 4 1
   # 4/1-1=3

The namespace scoping mechanism protects against accidental use in large
projects.

Other types of arguments
------------------------

Default arguments, output arguments, and variadic arguments are
supported:

::

   a {k:1} = k + 1
   a # 2

   b = out {a:3}; 2
   b + a
   # 5

   c = sum $arguments
   c 1 2 3
   # 6
   c *([1 2])
   # 3

Modula-3 added keyword arguments and default arguments to Modula-2. But I think they also added a misfeature: positional arguments with default values. In particular this interacts very poorly with currying. If ``foo`` is a function with two positional arguments, the second of them having a default value, then ``foo a b`` is either passing ``b`` to the result of ``f a`` or overriding the default value of the second argument. So specifying/overriding default arguments always requires the use of keyword syntax.

Implicit arguments use keywords as well, so they override default arguments:

::

   a {k:1} = k
   b = a
   c = b {k:2}
   c # 2

Output arguments can chain into implicit arguments, so you get something like the state monad:

::

   inc {x} = out {x:x+1}

   x = 1
   inc
   x # 2

Concatenative arguments
-----------------------

Results not assigned to a variable are pushed to a stack:

::

   1
   2
   3

   %stack
   # 1 2 3

``%`` is the most recent result, with ``%2`` ``%3`` etc. referring to
less recent results:

::

   {a = 1}
   extend % {b=2}
   extend % {c=3}
   shuffle

These stack arguments are used for positional arguments when not
supplied.

Inheritance
-----------

Stroscot uses multimethods, so the standard vtable implementation of Java/C++ is out. But the general idea of inheritance is, for ``Foo`` a child of ``Bar`` to rewrite calls ``exec (Foo ...) a b`` to calls ``exec (Bar ...) a b``, and this can be automated with a macro:

::

  inherit foopat barpat barmethodlist = {
    for (m : barmethodlist) {
      m foopat = m barpat
    }
  }

Lambdas
=======

::

  \a b -> stuff
  \a b. stuff
  lambda {
    a 1 = stuff
    a 2 = other
  }

Pattern-matching
----------------

``match`` is an expression:

::

  f = match (2+2) (5+5) | x y = 2
                        | 1 y = 2

It desugars to a lambda applied to the arguments.

Blocks
======

::

   x = input number
   display x

   foo =
     x = 0
     x += 1
     provide x

   obtain http_server
   main =
     parse_args
     build_folder
     http_server.serve(folder)

The translation rules are based on the continuation monad:

::

  {e} = e
  {e;stmts} = \c -> e ({stmts} c) = e . {stmts}
  {p <- e; stmts} = \c -> e (\x -> (\p -> {stmts}) x c) = e >>= {stmts}

Bang notation
-------------

::

  apply! { f !(g !(print y) !x) }

Idris defines `!-notation <http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation>`__, "implicitly bound application".

TODO: define the exact translation rules

Control structures
==================

These are things that can show up in blocks.

::

  a = if true then 1 else 2 -- just a function if_then_else : Bool -> a -> a -> a
  x = emptyRef; if true { x := 1 } else { x := 2 }; print x -- if on blocks
  repeat while x > 0 { x -= 1 }
  repeat until x == 0 { x -= 1 }
  repeat 10 times { x -= 1 }
  repeat { x -= 1 } while x > 0
  repeat
    x = x * 2
    if (x % 2 == 0)
      break

::

   check {
     risky_procedure
   } error {
     fix(error) or error("wtf")
   } regardless {
     save_logs
   }

More here: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions

Programs
========

A program is a block, and every declaration is a macro or control structure.

So for example you can implement a conditional definition:

::

   if condition
      a = 1
   else
      a = 2


Comments
========

::

   // comment
   /* multiline
      comment */
   {- nesting {- comment -} -}
   if(false) { code_comment - lexed but not parsed except for start/end }
   #! shebang at beginning of file

Type declarations
=================

::

  a = 2 : s8
  a = s8 2

DSL
===

Stroscot aims to be a "pluggable" language, where you can write syntax, type checking, etc. for a DSL.
Due to the fexpr semantics any expression can be used and pattern-matched, like ``javascript (1 + "abc" { 234 })``.

E.g. we could write a small DSL like SQL and then use it in a larger program with some embedding syntax.

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

Identifiers can be qualified by periods: ``a.b.c``. ``.`` is an infix left-associative operator that binds tighter than juxtaposition.
