Syntax
######

Almost everything in Stroscot is an expression. But there's also block statements and layout.

Unicode
=======

Practically, most programs will use ASCII. But the Unicode algorithms are robust and supporting other languages isn't too hard. `Lots of languages <https://rosettacode.org/wiki/Unicode_variable_names>`__ have support for Unicode, although the exact set of allowed characters varies.

* Start with bytes. Decode using UTF-8, replacing invalid bytes/characters with Unicode's REPLACEMENT CHARACTER U+FFFD.
* `NFC <http://unicode.org/reports/tr15/#Norm_Forms>`__ normalize the input, warning if input isn't normalized. There is enough software that automatically normalizes to NFC (e.g. web browsers) that it seems safe to require NFC; bugs can be worked around by changing the input (inserting joiners) rather than modifying NFC.
* A warning for weird scripts (listed in `TR31 <http://www.unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers>`__) or zero-width characters.

Some combination of the following algorithms to do lexical analysis:

* `line-breaking <https://www.unicode.org/reports/tr14/#BreakingRules>`__ (specifically, to determine hard / mandatory breaks)
* `word-breaking <http://www.unicode.org/reports/tr29/#Word_Boundary_Rules>`__ to split up lines into tokens - it needs to be extended to account for program identifiers / multicharacter symbols
* `identifier syntax <https://www.unicode.org/reports/tr31/#Default_Identifier_Syntax>`__, which specifies sets of valid identifier start/continue characters. Go's rule is that identifier characters must be letters or digits as defined by Unicode, and exported identifiers must start with an upper-case letter, but this excludes combining characters and Devanagari. Go's upper case restriction means 日本語 cannot be exported, and instead X日本語 must be used.

Stroscot is case-sensitive.

Layout
======

The most obvious is the initial declaration list, but other constructs introduce clauses as well. For readability, clauses may span multiple lines, so some way of distingishing the start / end of clauses must be defined. This amounts to adding braces and semicolons so as to make it layout-insensitive. The braces are virtual braces; they don't match with explicit braces.

::

  assertEqual
    {
      a
        b
        c
      d
    }
    { a {b; c}; d}

Behavior of a new line depends on its indentation level, relative to the indentation of the previous line. Indentation level is taken to be the sequence of whitespace characters related by "is prefix of", so "space, tab, space" is different from (incomparable to) "tab, space, space" but is less than "space, tab, space, em space" and more than "space, tab".

* If it is indented more, it's a sequence given as an argument to the previous line, so a virtual open brace is inserted
* If it is at the same level, another item in the sequence, so a (virtual) semicolon is inserted
* If there is a line at lower indentation (or EOF), the sequence is ended as it's a new declaration (`offside rule <https://en.wikipedia.org/wiki/Off-side_rule>`__). A virtual close brace is inserted at the start of the line.
* Incomparable levels are an error.

Layout handling is complicated by the presence of grammar rules without layout that allow free choice of indentation, e.g.

::

  assertEqual
    a
      + b
      + c
    a {+ b; + c}
    a + (b + c)

It should be possible to handle these with a fixup phase.

Also, closed operators (e.g. parentheses) inhibit layout; this amounts to skipping whitespace layout when inside an explicit delimiter pair. But of course constructs inside the delimiter pair can start another layout. Finally for constructs that usually use layout we still want to parse 1-line things without braces:

::

  assertEqual
    let a = b in c
    let { a = b } in c

Operators
---------

New operators can be declared with `mix <http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf>`__ `fix <http://www.bramvandersanden.com/publication/pdf/sanden2014thesis.pdf>`__ semantics, e.g.

::

  syntax _&&_ associate left above _and_ _or_ _not_ below _||_ equals _&_
  syntax [[_]]
  syntax if_then_else_
  syntax _postfix
  syntax prefix_

Operator precedence will be a poset, rather than levels. Infix symbols can be left or right associative.

Stroscot supports your typical PEMDAS:

::

  assertEqual
    1 + 2 * 3^2
    19
  assertEqual
    3+1/(7+1/(15+1/1))
    355/113
    3.14159292035...

Most operators are textual:

::

  assert
    true and false == false
    true or false == true
    true xor true == false
    5 div 2 == 2
    5 mod 2 == 1

Minus is both a unary prefix operator and a binary infix operator with special support to disambiguate the two. ``(-)`` denotes the binary minus operator and ``neg`` the unary minus operation.

Parentheses
-----------

Parentheses can be used to group expressions and override parsing as usual.

For brevity, trailing parentheses can be omitted:

::

  assertEqual
    3+1/(7+1/(15+1/1
    355/113

Although it parses, you can set Stroscot to warn or error on
unmatched parentheses, or run the code formatter which will add them.

Chained Comparison
------------------

::

  assert
    1 <= 2 < 3
    9 > 2 < 3

Variables
=========

Identifiers cannot be directly reassigned; you can shadow, which optionally generates a warning, but once an identifier is declared in a scope, that's what that identifier refers to for the duration of the scope. OTOH references behave like mutable variables.

::

  a = mut 1
  a := 2
  raise a by 1

Mutable assignment (``:=``) is completely distinct from name binding (``=``). They have distinct notation.

Functions
=========

Sequential matching:

::

  f 1 y = 1
  f x 2 = 2
  f x y = 3

Parallel matching:
::

  f 1 = 1
  ;
  f 2 = 2
  ;
  f y | y != 1 && y != 2 = 3

The extra ``;`` is an escape to avoid sequential matching of a sequence; if you alternate clauses of different functions or define clauses in different files they will also be combined with parallel matching.

Function application (juxtaposition) binds stronger than all operators and associates to the left, ``x y z --> (x y) z``.

Patterns
--------

Patterns all compile to guard conditions on ``$args``. They also check that the arity of ``$args`` is the number of patterns.

::

  _ --> True -- wildcard
  ^a --> $args[i] == a -- matches the atom a
  ^f a b c --> $args[0] == f && $args.length >= 4 # matches the symbol tree with atom f
  (f@_) a --> $args.length >= 2 # matches any symbol tree besides a single atom
  [(1, "x"), {c: 'a'}] -> $args[i] == [(1, "x"), {c: 'a'}] -- literal match
  [1, ..., 2] --> $args[i][0] == 1 && $args[i][-1] == 2 -- matches any list starting with 1 and ending with 2
  {a: 1, ...} --> $args[a] == 1 # matches a and the rest of the record
   pat1 AND pat2 --> match $args pat1 and match $args pat2 # matches both patterns simultaneously
   pat1 OR pat2 --> match $args pat1 or match $args pat2 # matches either pattern
  ~pat --> True # desugars to f u_ ... = let pat = u_ in ..., where u_ is a unique name
  (a : b) --> a elemOf b # type tag
  a | f a --> f a # guard, arbitrary function
  (f -> a) --> match (f $args[i]) a # view pattern

``_`` occuring by itself denotes an anonymous variable which matches any value without actually binding a name.

Destructuring and function bindings
------------------------------------

Generally identifiers ``f`` in head positions of a LHS ``f a b c`` are taken as literal function symbols. Identifiers in head position in a sub-term are taken to be constructors, and destructure the function argument. Identifiers in non-head positions are taken to be variables. This is Pure's "head = function rule".

::

  x = 2 # x is function of no arguments
  x a = a # x is function of one argument, binds variable "a"
  x (foo a b) # x is function of one argument, destructures term with head foo and binds a/b

Certain symbols such as tuple heads as head of the LHS are assumed not to be function definitions. Instead matching on them destructures the right hand side. For example you can define functions using destructuring:

::

  (x < y, x > y) = (<0) &&& (>0) $ s' (digitsToBits digits) where (CR s') = x-y

This translates to:

::

  x > y = case (z x y) of { (x < y, x > y) -> (x > y) }
  -- equivalent to
  x > y = case (z x y) of { (a,b) -> a }

  x < y = case (z x y) of { (x < y, x > y) -> (x < y) }

  z x y = (<0) &&& (>0) $ s' (digitsToBits digits) where (CR s') = x-y
  -- z a fresh symbol

To force a function definition you can use an as pattern, ``_@(,)``

To force interpretation as a variable you can use an anonymous as pattern, ``(f@_) a b c``. Then ``f`` is a variable and will match any symbol, rather just ``f``. Example converting a function application to a list::

  foo ((x@_) y) = (foo x) ++ [y]
  foo x = [x]
  > foo (a b c d);
  [a,b,c,d]

To force interpretation as a literal you can use ``^``. The symbol will be interpreted as a literal even in variable position::

  foo ^foo = "self-application"

  foo bar # does not reduce

You can also declare ``foo`` to be a symbol::

  symbol foo

However this is a module definition and means the symbol cannot be used as a variable in the module anymore.


Symbols
-------

 To say that it is actually a symbol a special keyword ``symbol`` is used:

::

  symbol foo

  foo x = 1

  bar 2 = 2
  # equivalent to _ 2 = 2 because bar is interpreted as a variable

Furthermore you can define a function symbol with an arity. This resolves applying the function symbol to arguments for which no clauses are defined to the exception ``undefined``, which often has better semantics than an unevaluated normal form.

::

  function symbol foo arity 2

  foo 1 2 = "fine"

  foo 1 2 # "fine"
  foo 3 4 # undefined
  foo 1 # not affected - normal form

This just creates a low priority definition ``foo _ _ = undefined``.

Non-linear patterns
-------------------

Non-left-linear patterns such as ``foo a a`` are allowed, this is interpreted as ``foo a b | a == b`` - rename variables and check for equality using ``==``. See :ref:`trs-equality-linearity` for a discussion.

Pattern synonyms
----------------

::

  toPolar (Point x y) = (sqrt (x^2 + y^2), atan2 x y)
  pattern Polar r t = (toPolar -> (r,t))

Pattern definitions are unidirectional in that they define matchers for syntax used only in patterns, not in expressions. To make a bidirectional pattern simply define the builder:

::

  Polar r t = Point (r * cos t) (r * sin t)

Variable bindings
-----------------

::

  a --> if a then $arga[0] == a else True -- binds a if a is not defined as a symbol
  _a --> True -- hole, binds a even if a is an existing symbol


This defines the variables as a zero-arity function symbol. So for example you can write

::

  a | True = 1
  a | False = 2

which means the same thing as

::

  a | True = 1
    | False = 2

i.e. ``a = 1``.

Inline definitions
------------------

Definitions can be made inline; they are lifted to the closest scope that allows definitions.

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

You can specify a subset of the arguments to generate a partially applied function:

::

  a = foo (y:2) (w:1)
  b = a (x:4) (z:0)
  b == v
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

Positional arguments support currying, for example:

::

  c x y = x-y
  b = c 3

  b 4
  # 1

Implicit arguments
------------------

These behave similarly to arguments in languages with dynamical scoping. Positional arguments can be passed implicitly, but only if the function is used without applying any positional arguments. If the LHS contains positional arguments only that number of positional arguments are consumed and they are not passed implicitly.

::

  foo w x y z = z - x / y * w
  bar = foo + 2
  baz a = bar {x:4,y:2} - a

  bar 1 2 3 4
  # (4 - 2/ 3 * 1) + 2

  ((0-4/2*1)+2)-5 == baz 5 {z:0,w:1}
  # true
   baz 1 2 3 4 5
  # Error: too many arguments to baz, expected [a]

Similarly keyword arguments inhibit passing down that keyword
implicitly:

::

  a k = 1
  b k = k + a

  b {k:2}
  # Error: no definition for k given to a

A proper definition for b would either omit k or pass it explicitly to a:

::

  a k = 1
  b = k + a
  b' k = k + a k

  b {k:2} == b' {k:2}
  # true

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
  assert
    a {(+):(-)} 4 1
    == 4/1-1
    == 3

The namespace scoping mechanism protects against accidental use in large
projects.

Infix operators can accept implicit arguments just like prefix functions:

::

  infix (**)
  x ** y {z} = x+y/z

Default arguments
-----------------

::

  a {k:1} = k + 1
  a # 2

Modula-3 added keyword arguments and default arguments to Modula-2. But I think they also added a misfeature: positional arguments with default values. In particular this interacts very poorly with currying. If ``foo`` is a function with two positional arguments, the second of them having a default value, then ``foo a b`` is ambiguous as to whether the second argument is overriden.

We can resolve this by requiring parentheses: ``(foo a) b`` passes ``b`` to the result of ``f a {_2=default}``, while ``foo a b`` is overriding the second argument. But it's somewhat fragile, a more robust method is to require specifying/overriding default arguments to use keyword syntax.

Implicit arguments use keyword syntax as well, so they override default arguments:

::

  a {k:1} = k
  b = a
  c = b {k:2}
  c # 2

Output arguments
----------------

::

  b = out {a:3}; 2
  b + a
  # 5

Output arguments can chain into implicit arguments, so you get something like the state monad:

::

  inc {x} = out {x:x+1}

  x = 1
  inc
  x # 2

It might be worth having a special keyword ``inout`` for this.

::

  inc {inout x} =
    x = x+1

Variadic arguments
------------------

Positional variadic arguments:

::

  c = sum $arguments
  c 1 2 3
  # 6
  c {$arguments=[1,2]}
  # 3

Only syntactically adjacent arguments are passed, e.g.

::

  (c 1 2) 3
  # error: 3 3 is not reducible

  a = c 1
  b = a 2
  # error: 1 2 is not reducible

There are also variadic keyword arguments:

::

  s = print $kwargs
  s {a:1,b:2}
  # {a:1,b:2}

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
less recent results. These stack arguments are used for positional arguments when not
supplied.

::

  {a = 1}
  extend % {b=2}
  extend % {c=3}
  shuffle
  # {b=2,a=1,c=3}

Inheritance
-----------

The general idea of inheritance is, for ``Foo`` a child of ``Bar`` to rewrite calls ``exec (Foo ...) a b`` to calls ``exec (Bar ...) a b``, and this can be automated with a macro:

::

  inherit foopat barpat barmethodlist = {
    for (m : barmethodlist) {
      m foopat = m barpat
    }
  }

Operators
---------

Operators are syntactic sugar for functions. Enclosing an operator
in parentheses turns it into an ordinary function symbol, thus ``x+y`` is
exactly the same as ``(+) x y``.

Lambdas
=======

::

  \a b -> stuff
  \a b. stuff
  lambda {
    a 1 = stuff
    a 2 = other
  }

A lambda raises an exception if no pattern matches (defined function), but otherwise is
a nameless local function. With the ``lambda{}`` syntax multiple clauses can be defined - they are matched sequentially. Multiple-argument lambdas are curried.

Because they're nameless lambdas aren't sufficient to define recursive function - use (named) local functions, or the function ``fix : (a -> a) -> a``.

Destructuring works in the arguments of lambdas as with named functions.

Matching
--------

``match`` is an expression:

::

  f = match (2+2) (5+5) | x y = 2
                        | 1 y = 2

It desugars to a lambda applied to the arguments.

``impossible`` is a special RHS used to help the verification analysis:

::

  f = match (2+2)
        | 5 = impossible

Reduce similarly reduces an expression to normal form using some rules:

::

  reduce x where
    x = y
    y = z
  # z

Operator sections
-----------------

Operator sections allow writing partial applications of infix operators.
A left section ``(x+)`` is equivalent to ``(+) x``. A right section ``(+y)`` is
equivalent to ``\x -> x + y``.

In contrast, ``(-x)`` denotes an application of unary minus; the
section ``(+-x)`` can be used to indicate a function which subtracts ``x``
from its argument.

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

  { f !(g !(print y) !x) }

  // desugars to
  {
    t1 <- print y
    t2 <- x
    t3 <- g t1 t2
    f t3
  }

The notation ``!expr`` within a block means that the expression ``expr`` should be bound in the block to a temporary before computing the surrounding expression. The expression is bound in the nearest enclosing block.
Expressions are lifted leftmost innermost.

Local definitions
-----------------

You can define variables and function locally to a block, clause, or clause group with let and where. These are in a new scope and only apply to the block where they are defined. This avoids cluttering up the program. All local definitions are substituted away before the block is evaluated in the ambient context.

::

  foo a y | true = {
    f x = g x + h y -- block definition
  }
    where
      g a = 2 * a -- clause definition
  foo b y | false = impossible
    where
      h y = a * 2 -- last clause, hence a clause group definition, but uses "a" which is only defined for first clause

A local definition shadows an ambient one, so for example you can write:

::

   f = f 4 where
     f 0 = 0
     f x = f (x-1)

``f 4`` and ``f (x-1)`` both refer to the local definition. But you will get a shadowing warning as it is bad style.

``let`` allows recursive definitions, bare definitions are not recursive:

::

  x = x + 1 # defines a new x shadowing the old x
  let x = (x : Int) * 2 # defines a fixed point with unique solution x=0

  fact x = if x==0 then 0 else x * fact (x-1) # fails due to fact being an unbound symbol
  let fact x = ... # proper definition


Monad comprehensions
--------------------

::

  [x,y | x=1..n; y=1..m; x<y]

A convenient means to construct lists and to write blocks compactly. There is a template expression, generator clauses which bind the result of continuation to a pattern, and filter clauses which allow skipping results.

::

  Expressions: e
  Declarations: d
  Lists of qualifiers: Q,R,S
  Qv is the tuple of variables bound by Q (and used subsequently)
  selQvi is a selector mapping Qv to the ith component of Qv

  -- Basic forms
  D[ e | ] = return e
  D[ e1,e2,e3 | ] = return [e1,e2,e3]
  D[ e | p <- e, Q ]  =
    p <- e
    D[ e | Q ]
  D[ e | e, Q ] =
    guard e
    D[ e | Q ]
  D[ e | let d, Q ] =
    let d
    D[ e | Q ]

  -- Parallel comprehensions (iterate for multiple parallel branches)
  D[ e | (Q | R), S ] =
    (Qv,Rv) <- mzip D[ Qv | Q ] D[ Rv | R ]
    D[ e | S ]

  -- Transform comprehensions
  D[ e | Q then f, R ] =
    Qv <- f D[ Qv | Q ]
    D[ e | R ]

  D[ e | Q then f by b, R ] =
    Qv <- f (\Qv -> b) D[ Qv | Q ]
    D[ e | R ]

  D[ e | Q then group using f, R ] =\
    ys <- f D[ Qv | Q ]
    let Qv = (fmap selQv1 ys, ..., fmap selQvn ys)
    D[ e | R ]

  D[ e | Q then group by b using f, R ] =
    ys <- f (\Qv -> b) D[ Qv | Q ]
    let Qv = (fmap selQv1 ys, ..., fmap selQvn ys)
    D[ e | R ]



Control structures
==================

These are things that can show up in blocks and have blocks as arguments.

::

  a = if true then 1 else 2 -- just a function if_then_else_ : Bool -> a -> a -> a
  {
    x = mut undefined
    if true { x := 1 } else { x := 2 } -- if on blocks
    print x
  }
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
  (* nesting (* comment *) *)
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

