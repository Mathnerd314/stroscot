Syntax
######

Design
======

The stuff here is mostly a dumping ground of ideas while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and picking out the nicest examples. But in the end, syntax is decided by usage, so a lot of the syntax here will probably become final.

Quorum and its associated set of syntax studies provide useful datapoints on keywords and constructs. But Stroscot has a unique design so we can't use a lot of the research, and the research is limited to begin with.

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Haskell/Idris syntax is mostly awesome, use it. (TODO: check this. The weird parentheses style may lose too many users)

TODO: see if there are any more Unicode guidelines relevant to writing a programming language parser

Natural language like Inform 7, while interesting, is quite wordy. It's also hard to scan through.

Fortress has "mathematical syntax", with an ASCII form and typeset form. They used LaTeX but HTML / MathML output should be possible too. And juxtaposition was overloaded. Probably worth emulating.

A language encourages certain expressions of thought. If the syntax is awkward then the feature will be used less and a bias will be introduced. But the styles of programming people come up with after a language is released are often completely different to what was intended by the language (e.g. Java and its design patterns). It's not clear that anything can be done about this, besides capturing as many existing patterns as cleanly as possible.

Textual
-------

There are some people who, when confronted with the complexity of syntax, think "It's better to use a binary format and store everything in a database." Now they have two problems. Math is textual, English is textual, the only stuff that isn't textual are flowcharts and tables. Flowcharts might be OK but graph layout is hard - graphviz barely works, and IDA Pro's graph layout when it decompiles stuff is quite lacking. Tables lead into spreadsheet programming which is generally not expressive as a language - and the formulas and cell values are textual. If you show me a way to write 123.6 that doesn't involve text maybe I'll think about this more.

There's also structural editing, `lamdu <http://www.lamdu.org/>`__ and so on, but they are designing an IDE alongside a programming language. I'm not too interested in IDEs and given that half the IDEs are for languages that also have a textual syntax, syntax doesn't seem to be a big factor in writing such an IDE.

Legibility
----------

The main factor improving readability is consistency; reading is disrupted when unconventional layouts are used.

Spacing is important to identify word boundaries (intra-letter spacing significantly smaller than inter-word spacing) and sentence boundaries (two spaces after the period, although the period's whitespace itself is distinctive). Justified text is harder to read than ragged-right due to the inconsistent spacing arising from bad line-breaking. Left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line. The default line spacing seems fine.

Line length is a good question. Programming uses fixed-width characters so it's measured in characters. 80 characters is standard, but monitors are wider now, so 100 is plausible. Diff programs are often the limiting factor, but on my monitor I can fit 2 108-character texts side-by-side along with a space in the middle and the taskbar. 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt (24px) I can fit 97 characters, while 23px fits 102 characters.

Layout improves legibility, Python syntax is often said to be "clean", hence why Stroscot has layout.

Reading code top-to-bottom, left-to-right makes sense. So definitions should be on the left, blocks indented in, and lines themselves should read left to right. So Ruby's statement modifiers ``X if Y`` are a bad idea because the ``if Y`` is easy to miss when scanning control flow.  But operators like ``a = b xor c`` are fine because at the point where the expression matters you're reading the whole line anyway and can parse it in your head.

Parsing
=======

I've got a basic Earley algorithm working for now. But eventually I'm extending it with BSRs and layout and other fun things. There's also `Yakker <https://github.com/attresearch/yakker>`__, which is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

  A new parsing engine, Yakker, capable of handling the requirements of modern applications including full scannerless context-free grammars with regular expressions as right-hand sides for defining nonterminals. Yakker also includes facilities for binding variables to intermediate parse results and using such bindings within arbitrary constraints to control parsing. Yakker supports both semantic actions and speculative parsing techniques such as backtracking and context-free lookahead and several parsing back ends (including Earley, GLR and backtracking).  In addition, nonterminals may be parameterized by arbitrary values, which gives the system good modularity and abstraction properties in the presence of data-dependent parsing. Finally, legacy parsing libraries, such as sophisticated libraries for dates and times, may be directly incorporated into parser specifications.

I've looked at various algorithms but I think the only way to handle it completely correctly and generically is to have a disambiguating pass on the set of parse tree generated by a nondeterministic automaton. The alternatives involve restricting parsers to be deterministic, for example PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

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



Blocks
======

Blocks are inspired by Haskell's do notation, but have a twist, based on the observation that the continuation monad is `the mother of all monads <https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/the-mother-of-all-monads>`__. Since it's the mother, we don't lose anything by fixing the monadic operations in the do-notation to be the continuation monad operations. That link gives a generic way to implement monads via the continuation monad, but the direct implementation is pretty clean. For example the `StateT monad <https://github.com/Mathnerd314/stroscot/blob/master/tests/Continuations-State.hs>`__.

It's worth noting that even though monads can be implemented easily, monads are overrated to begin with:

* ReaderT is handled by implicit parameters
* StateT is a mutable reference
* WriterT is a StateT that's not read
* Error/Except are handled by poison values

Using the continuation monad allows us to separate commands (not returning a value) and operations (returning a value). Haskell has the translation ``{e;stmts} = e >> stmts = \c -> e (\_ -> {stmts} c)``. But usually ``e`` returns ``()``, so ``(>>)`` is applied at the type ``f () -> f b -> f b`` and that ``\_`` is a ``\()``. With our translation, commands (which don't return a value) are functions ``r -> r``. Haskell's translation would require them to be ``Cont r () = (() -> r) -> r``, which is equivalent but has an extra ``()`` floating around. But in both translations operations (whose value is used) are of type ``Cont r a = (a -> r) -> r``. The non-uniform type for actions might make copying code from Haskell a little harder, but on the other hand we get function composition as a built-in syntax. That's right, the most basic operation in category theory is available as syntactic sugar in Stroscot. Take that, Haskell. And also we can easily use indexed monads, just change ``r) -> r`` to ``r) -> s``.

For an example of how natural this is you can look at :ref:`how I/O works <tasks>`.

ApplicativeDo
-------------

ApplicativeDo :cite:`marlowDesugaringHaskellDonotation2016` has two functions. The first is to make some do-notation sequences be Applicative rather than Monad. In fact though these are exactly the sequences handled by idiom brackets, of the form ``{a <- ax; b <- bx; return (f a b)} = return (f !a !b)``. Idiom brackets are shorter, so the value this provides is minimal.

The second function is to use applicative operations instead of monadic operations because in "some" monads the applicative operation is more efficient. Their example is the Haxl DSL:

::

   numCommonFriends :: Id -> Id -> Haxl Int
   numCommonFriends x y = do
      fx <- friendsOf x
      fy <- friendsOf y
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

Idris defines `!-notation <http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation>`__, "implicitly bound application". The scoping is `unintuitive <https://github.com/idris-lang/Idris-dev/issues/4395>`__, but the notation itself is powerful. Binding it to a syntactic block seems reasonable. And it can easily express idiom brackets, ``[[ f a b ]]`` becomes ``{ f !a !b }``. Idiom brackets save characters with more arguments, but bang notation looks natural if there are multiple bindings in the block.

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

Type declarations
=================

``2 : s8`` and ``s8 2`` seem more logical compared to other choices such as ``a : s8 = 2`` (Swift,Jai - hard to find the = with long types) or ``s8 a = 2`` (C,Rust - overlaps with function definition). The name is simply a syntactic handle to refer to the value; it doesn't have an innate type. In contrast the representation of the value must be specified to compile the program. The second syntax is similar to assembler syntax such as ``dword 0``.

Namespacing
===========

``.`` is preferred to ``::`` because it's shorter and because modules are first-class.

Partial loading
===============

The parser parses as much of the input as possible, but in general only a prefix of the input will be valid. Hence we can load a portion of the file by inserting junk / truncating the input buffer. The compiler will give a warning but the parser should handle it just fine.

Specificity
===========

This might seem overly complicated, but it's based on Zarf's `rule-based programming <https://eblong.com/zarf/rule-language.html>`__. When you're defining lots of rules for a IF game then specifying priorities by hand is tedious.

Comments
========

Analysis of languages showed C-style comments ``/* */`` and C++ style comments ``//`` are very common. In addition Pascal-style nesting multiline comments ``(* *)`` seem useful. Supporting shebang comments at the beginning of a file complicates the syntax a bit but makes the language easier to use for scripting. But the frequency of ``#`` is still less than ``//`` so it's not usable for normal comments.

Javadoc-style comments ``/** */`` are used in very clear stylized ways, so there seems to be no reason to forbid them.

People put comments in random places and they can be attached to whatever and indented strangely. The reformatter will mangle them, but hopefully this will become accepted behavior.

Whitespace
==========

Whitespace in identifiers... this doesn't work well with Haskell syntax. With whitespace ``do something = ...``` would define the identifier ``do something``, but in Haskell it's a clause ``do _`` that binds ``something``.

OTOH using a string works fine: ``"do something" = ...``

You could also make something an atom, then you can write ``do something`` in code but the clause definition is ``do ^something = ...``. The semantics are similar to a single identifier but different enough that I don't think it counts.

Indentation-sensitivty like Python seems great. It is readable and when copy-pasting code you only have to fix up the indentation (supported by all modern code editors) instead of messing with braces.

Function syntax
===============

Stroscot has first-class functions with lexically scoped name binding.

Lambdas are defined using whatever syntax. The ``\x.y`` style is closest to the mathematical notation (barring Unicode), Haskell uses ``\x -> y``, Cliff likes ``{x => y}``.

APL-style functions/operators ``(~R∊R∘.×R)/R←1↓⍳R`` are not preferred due to Unicode overuse, preferring operators written with words instead, but one could create them if desired.

Conceptually, term rewriting is the underlying model of computation.

Arguments
---------

Stroscot supports many types of arguments. Functions are extremely common, so the more styles supported,
the shorter the code will be.

Equations are tried in the order in which they are written; as soon as the left-hand side of an equation matches (and the condition part of the equation, if any, is satisfied), it can be applied to reduce the target term to the corresponding right-hand side. The term is rewritten until no more equations are applicable.

Currying
--------

Currying makes all functions symbols of order 0 and allows easy partial function application.
Partial function application allows reusing functions more easily, e.g. as the argument to map.
Function symbols also become first-class, because they can be passed around without being applied, ``f`` vs ``\x. f x``

Haskell style arguments ``f a`` are preferred over C style ``f(a)`` due to being shorter for arguments that are identifiers. The only place they lose in character count is complex arguments ``f (a+1) (b+2)`` vs ``f(a+1,b+2)``, but there you can use a tuple to match the syntax or the record ``f{x=a+1,y=b+2}`` which will most likely be clearer.

Constructor discipline
----------------------

Haskell has a division between constructors and functions:
* identifiers starting with lowercase letters are functions, and can only be used with rules of the form ``f x = ...``
* identifiers starting with uppercase letters are constructors and no rules of the form ``X a b = ...`` can be defined. But constructors are the only symbols allowed in sub-terms, e.g. ``f (X a b) = ...``

This rule reduces maintainability. If the representation is changed there is no way to replace the raw constructor with a smart constructor. So instead every library is forced to define functions like ``mkThing = Thing`` to get around this syntactic restriction. In fact in :cite:`kahrsNonOmegaOverlappingTRSsAre2016` there is a boilerplate trick to turn any TRS into a constructor TRS, by duplicating ``foo`` into a constructor ``Foo`` and a function ``foo``, converting subterms of the original rules to match on constructors, and adding rules that turn stuck patterns into constructors. For example ``k x y = x; s x y z = (x z) (y z)`` turns into:

::

  app (App K x) y = x
  app K x = App K x
  k = K

  app (App (App S x) y) z = app (app x z) (app y z)
  app S x = App S x
  app (App S x) y = App (App S x) y
  s = S

This is pretty verbose but the constructors besides app are nullary so it isn't as bad as it could be. For rules like associativity ``x*(y*z) = (x*y)*z`` and distributivity ``x*(y+z) = x*y+x*z`` handling all the stuck pattern rules for symbols ``+`` and ``*`` is a nightmare, and you also have to come up with alternative operator names for the constructors.

So Stroscot follows Pure in not having a constructor discipline. Any symbol can be used anywhere on the left-hand side of an equation. Any symbol may act as a constructor symbol if it happens to occur in head position in a normal form term.

There is a general convention for the standard library to use lowercase for potentially reducible expressions and uppercase for inert data. This is to vaguely follow Haskell.

Implicit arguments
------------------

Claim: Explicit argument passing cannot replace implicit arguments

See example: (copied from reference)

::

  -- standard library
   log s = if (priority > loglevel) { logPrint s }

  -- components of an application
   foo = log "foo" { priority = DEBUG }
   bar = log "bar" { priority = WARNING }
   baz =
    foo
    bar

  -- main file
   logPrint x = writeFile file x
   file = "a"
   loglevel = WARNING

   main =
     baz
     foo {loglevel=DEBUG}
     bar { file = "b"}

``loglevel`` is defined at the top level, but each use site is scattered in the code. The implicit argument replaces the global variable that is often used. Similarly ``logPrint`` is passed implicitly instead of being a member of a global Logger instance. The ``file`` variable does not exist in the standard library; it is part of the user's code.

To use explicit argument passing, we'd have to add explicit ``loglevel`` and ``logPrint`` arguments to ``log`` and all its callers. To minimize callers we could partially apply it in ``main`` and pass around just the ``log : String -> Cmd`` function. But still, we have to modify every caller of ``log`` and its callers and so on to pass around the ``log`` function.

Chained assignment
==================

This is mainly the expression ``a = b = 2`` but also the variant ``a := b := 2``. The `literature <http://www.cse.iitm.ac.in/~amannoug/imop/tr-3.pdf>`__ classifies this as "syntactic sugar", so handling it in the parser like Python seems the reasonable solution. C++'s "the assignment returns the lvalue" seems contrived. With ``=`` the assignments are all pure hence simultaneous; for ``:=`` doing assignments RTL to match C++ is probably better than `Python's LTR <https://docs.python.org/3/reference/simple_stmts.html#assignment-statements>`.

OTOH using ``=`` for equality comparison might break this.

Unless
======

Ruby's ``unless-else`` is unintuitive. Only support ``if-else``. Also ``if not`` is a possible replacement for ``unless``.

Tuples and records
==================

In `Maybe Not <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/MaybeNot.md>`__ Rich Hickey  says records/fields, and product types are "place oriented programming". Well, in `The Value of Values <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/ValueOfValuesLong.md>`__ he says place-oriented programming is when you use in-place update. But maps (his proposed alternative) also support in-place update and are place-oriented. The difference between maps and records seems to be that records have ordered fields.

So he seems have a different definition in mind, in particular that place-oriented means accessors are not first class - even when the fields are named, you cannot say ``object["name"]`` for an arbitrary object or an arbitrary name. But this is easily solved by adding such functionality. It also doesn't get into the mutable/immutable distinction that the values talk made.

His second point is that product types "complects" the meaning of things with their position in a list. "Complect" is from `Simple Made Easy <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/SimpleMadeEasy-mostly-text.md>`__ and is a pejorative version of "braid together".
Essentially he's saying that if you have ``(String, String)`` there is no way to know how the second string is different from the first string. Well, for commutative operations like addition the order literally doesn't matter. Adding any sort of information to ``(+) : (Int, Int) -> Int`` is complicating the picture. Similarly for Strings `coming up <https://gemma.msl.ubc.ca/resources/baseCode/apidocs/ubic/basecode/util/StringUtil.html#append-java.lang.String-java.lang.String-java.lang.String->`__ with names "appendee" and "appendant" for an  append operation is almost as bad as digging up "complect". Using numerical names ``s1`` and ``s2`` makes more sense. It still gives a record with named fields, but it makes sense to use positional arguments.

And if the types are different there's no ambiguity: ``(FirstName, LastName``, ``(Int,Bool)``, etc.