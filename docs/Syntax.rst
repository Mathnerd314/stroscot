Syntax
######

Almost everything in Stroscot is an expression. The basics are numbers, booleans, and character strings of text. But there's also block statements and layout.

Design
======

The stuff here is mostly a dumping ground of ideas while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and picking out the nicest examples. But in the end, syntax is decided by usage, so a lot of the syntax here will probably become final.

Quorum and its associated set of syntax studies provide useful datapoints on keywords and constructs. But Stroscot has a unique design so we can't use a lot of the research, and the research is limited to begin with.

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Haskell/Idris syntax is mostly awesome, use it.

Legibility
----------

The main factor improving readability is consistency; reading is disrupted when unconventional layouts are used.

Spacing is important to identify word boundaries (intra-letter spacing significantly smaller than inter-word spacing) and sentence boundaries (two spaces after the period, although the period's whitespace itself is distinctive). Justified text is harder to read than ragged-right due to the inconsistent spacing arising from bad line-breaking. Left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line. The default line spacing seems fine.

Line length is a good question. Programming uses fixed-width characters so it's measured in characters. 80 characters is standard, but monitors are wider now, so 100 is plausible. Diff programs are often the limiting factor, but on my monitor I can fit 2 108-character texts side-by-side along with a space in the middle and the taskbar. 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt (24px) I can fit 97 characters, while 23px fits 102 characters.

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

Later in the pipeline:
* A similarly-named identifiers warning based on `confusables <http://www.unicode.org/reports/tr39/#Confusable_Detection>`_

TODO: see if there are any more Unicode guidelines relevant to writing a programming language parser

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

  (0[box])?[0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([eEpP][+-]?[0-9_]+)?

Number syntax is `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__, slightly liberalized to allow using floating-point notation for integers and binary exponents for decimals.

Number literals are parsed into records like ``NumberLiteral { digits = "123", exponent = "24" }``. Leadings 0's could be significant, e.g. ``010`` could be different from ``10``.

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

Atoms / Symbols
---------------

If an expression tree has no reduction rules, it is treated as a symbol tree. Symbols are essentially data constructors and can be freely applied to construct data.

::

  atom
  underscore_atom
  unícσdє-αtσm
  symbol ++++ tree
  some (weird thing) * 12

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

Functions operate on values and produce the same outputs given the same inputs.

The semantics of functions are defined by pattern-matching rules a la `Pure <https://agraef.github.io/pure-docs/pure.html#definitions-and-expression-evaluation>`__.

Stroscot supports many types of arguments. Functions are extremely common, so the more styles supported,
the shorter the code will be.

Haskell has a rule that identifiers starting with uppercase letters are constructors and cannot be defined to be functions, but this rule reduces maintainability. If the representation is changed there is no way to replace the raw constructor with a smart constructor. So instead every library is forced to define functions like ``mkThing = Thing`` to get around this syntactic restriction.

Stroscot supports pattern matching as well as predicate dispatch - these cases are unordered:

::

   f 1 = 1
   f 2 = 2
   f y | y != 1 && y != 2 = 3

The pipe syntax matches cases from top to bottom:

   ::

      f
      | 1 y = 1
      | x 2 = 2
      | x y = 3

Patterns
--------

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

Implementation
--------------

The way Stroscot implements dispatch is:
* eliminate all the statically impossible cases (cases that fail)
* use profiling data to identify the hot paths
* build a hot-biased dispatch tree
* use conditionals for small numbers of branches, tables for large/uniform branches (like switch statements)

Lambdas
=======


::

  { a b = stuff }
  \a b -> stuff
  \a b. stuff

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


Blocks are inspired by Haskell's do notation, but have a twist, based on the observation that the continuation monad is `the mother of all monads <https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/the-mother-of-all-monads>`__. Since it's the mother, we don't lose anything by fixing the monadic operations in the do-notation to be the continuation monad operations. That link gives a generic way to implement monads via the continuation monad, but the direct implementation is pretty clean. For example the `StateT monad <https://github.com/Mathnerd314/stroscot/blob/master/tests/Continuations-State.hs>`__.

Using the continuation monad allows us to modify the translation:

::

  {e} = e
  {e;stmts} = \c -> e ({stmts} c) = e . {stmts}
  {p <- e; stmts} = \c -> e (\x -> (\p -> {stmts}) x c) = e >>= {stmts}

Haskell has the translation ``{e;stmts} = e >> stmts = \c -> e (\_ -> {stmts} c)``. But usually ``e`` returns ``()``, so ``(>>)`` is applied at the type ``f () -> f b -> f b`` and that ``\_`` is a ``\()``. With our translation, operations which don't return a value are functions ``r -> r``. Haskell's translation would require them to be ``Cont r () = (() -> r) -> r``, which is equivalent but has an extra ``()`` floating around. But in both translations operations whose value is used are of type ``Cont r a = (a -> r) -> r``. The non-uniform type for actions might make copying monads from Haskell a little harder, but on the other hand we get function composition as a built-in syntax. That's right, the most basic operation in category theory is available as syntactic sugar. Take that, monads. And also we can easily use indexed monads, just change ``r -> r`` to ``r -> s``.

It's worth noting that even though monads can be implemented easily, monads are overrated to begin with:

* ReaderT is handled by implicit parameters
* StateT is a mutable reference
* WriterT is a StateT that's not read
* Error/Except are handled by poison values

To see how I/O works, consider printing hello world: ``print "Hi"``. As a task this looks like ``Print "Hi" exit``, where ``exit`` is what happens after (the continuation). The operation is ``print a = \cont -> Print a cont``. With the continuation as the last argument we can just use the partially-applied function, ``print = Print``. ``print a >> print b = \cont -> Print a (Print b cont)``. Now consider ``read ref >>= print``. The operation is ``\cont -> Read ref (\v -> Print v cont)``, which is just ``Read ref >>= Print`` where ``>>=`` is the continuation monad's bind operation.

ApplicativeDo
-------------

ApplicativeDo :cite:`marlowDesugaringHaskellDonotation2016` has two functions. The first is to make some do-notation sequences be Applicative rather than Monad. In fact though these are exactly the sequences handled by idiom brackets, of the form ``{a <- ax; b <- bx; return (f a b)} = apply f a b``. Idiom brackets are shorter, so the value this provides is minimal.

The second function is to use applicative operations instead of monadic operations because in "some" monads the applicative operation is more efficient. Their example is the Haxl DSL:

::

   numCommonFriends :: Id → Id → Haxl Int
   numCommonFriends x y = do
      fx ← friendsOf x
      fy ← friendsOf y
      return (length (intersect fx fy))

Well, sorry to burst the bubble, but if you're writing a DSL then writing it as a macro is much more powerful than trying to shoehorn it into an applicative/monadic framework. They discuss in the paper that the translation to use applicative operations is ambiguous and the best one depends on details of the computation that are not accessible, because functions are opaque. It's exactly these kinds of details that *are* accessible in a DSL - you just write a pass that walks over the expression tree and estimates the costs. Similarly the `use/def analysis <https://en.wikipedia.org/wiki/Use-define_chain>`__ that they use for the rewriting is a standard compiler pass. The commutativity mentioned in the paper is another property one could know from the DSL and that changes the output significantly.

For regular do notation, the applicative notation translates to exactly the same continuation as the monadic notation.

Verdict: DSL in disguise. Just write a DSL. Stroscot does not benefit at all by adding ApplicativeDo.

RecursiveDo
-----------

RecursiveDo :cite:`erkokValueRecursionMonadic2002` is an older extension to do notation. The motivating example is a circuit DSL:

::

   toggle : Signal Bool
   toggle = out
      where
         inp = inv out
         out = delay False inp

   counter : Signal Bool -> Signal Int
   counter reset = out
      where
         next = delay 0 inc
         inc = out + 1
         out = mux reset zero next
         zero = 0

But wait, where's the do notation? In fact, this is really just a DSL. There are no monads and no sequencing to be found. All of these operations happen in parallel. The uses for these circuit descriptions all depend on the circuits being specified using a small set of operations specified in a typeclass.

Investigating Hackage, mdo is uncommon. "Many Haskell programmers will never use it in their careers." (`1 <https://ro-che.info/articles/2015-09-02-monadfix>`__) Uses fall into categories:
* DSLs, where variable assignments are interpreted as data
* Gratuitous (no/one binding, or bindings do not refer to bindings from later)
* Examples where it would be clearer to use mfix or the do-rec notation that is just ``(a,b,c) <- mfix (\(a,b,c) -> (_,_,_))``
* I/O monad, mfix is used to write the code in a recursive style instead of modifying a variable, e.g. forking two threads that kill each other:

::

   mdo
      a <- fork $ killThread b
      b <- fork $ killThread a

   -- vs
   bId <- newEmptyMVar
   a <- fork $ readMVar b >>= killThread
   b <- fork $ killThread a
   writeMVar bId b

The code for IO's mfix uses unsafeDupableInterleaveIO. This has been the subject of at least one `bug <https://gitlab.haskell.org/ghc/ghc/-/issues/5421>`__ (`two <https://gitlab.haskell.org/ghc/ghc/-/issues/15349>`__ counting fixST), and is why there is both fixIO and `unsafeFixIO <https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Unsafe.html#v:unsafeFixIO>`__. Reasoning about fixIO seems to `require <https://wiki.haskell.org/Evaluation_order_and_state_tokens>`__ laziness semantics and maybe also an understanding of Haskell's State-based I/O model.

Also, most monads fail to satisfy monadic right shrinking, which IMO makes the notation completely unintuitive:

::

   mdo
      z <- f z
      w <- g z
      return (z,w)

   -- is NOT equivalent to

   z <- mdo
            z <- f z
            return z
   w <- g z
   return (z,w)

The only price to pay for leaving mdo out is that value-recursive monadic computations have to be written with ``mfix``. We can still implement ``mfix`` for the monads that matter, like ``State``. According to all available knowledge, ``mfix`` can't be implemented for continuations, so nothing is lost from regular programs.

Verdict: Not only a DSL in disguise, but also a footgun. Provide mfix and the rec{} notation in an obscure library for those who care.

Arrows
------

You might be getting the pattern here. Arrows were inspired by a parsing DSL. Any arrow which supports the ArrowApply class is a monad. Arrows not supporting ArrowApply must write operations for every language element supported (variable, function, conditional, grammar production choice, and so on). Continuations require ArrowApply to even implement the basic arrow interface. Verdict: trash, a leaky "abstraction" that just wastes everyone's time.

Idiom brackets
--------------

While do notation is defined for monads, idiom brackets are defined for applicative functors, ``[[ f a b ]] = pure f <*> a <*> b``. But DSL notation works too: ``apply { a + b }``.

The issue with translating to ``<*>`` is that it assumes left-to-right evaluation. You can see this in the `translation <https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Applicative>`__ for Monads: ``m1 <*> m2`` binds ``m1`` before ``m2``. In Stroscot the program is required to be equivalent under all evaluation orders. So to enforce this we need a function ``parallel : [m a] -> m [a]`` that checks there is no issue with evaluating in parallel. Then using parallel the translation of ``apply { f a b x }`` looks like ``{ (av,bv,cv) = parallel (a,b,c); return (f av bv cv) }``

Idris defines `!-notation <http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation>`__, "implicitly bound application". The scoping is `unintuitive <https://github.com/idris-lang/Idris-dev/issues/4395>`__, but the notation itself is powerful. As a DSL ``apply! { f !(g !(print y) !x) }`` the scoping issue is resolved. TODO: define the exact translation rules

C-like reference access
-----------------------

For example we want to do:

::

  a = ref 1
  b = ref 2
  c = a + b
  a := c

Translated this looks like:

::

   ref 1 >>= \a ->
   ref 2 >>= \b ->
   parallel (read a, read b) >>= \(av,bv)  ->
   let c = av + bv in
   writeRef a c

I think the solution is another DSL. Inserting ``read a`` is not too complicated, just follow the C/C++ rules about converting lvalues to rvalues.

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

Comments
========

::

   // comment
   /* multiline
   comment */
   {- nesting {- comment -} -}
   if(false) { code_comment }

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
