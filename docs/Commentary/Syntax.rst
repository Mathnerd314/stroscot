Syntax
######

Design
======

The stuff here is mostly a dumping ground of ideas while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and picking out the nicest examples. But in the end, syntax is decided by usage, so a lot of the syntax here will probably become final.

Quorum and its associated set of syntax studies provide useful datapoints on keywords and constructs. But Stroscot has a unique design so we can't use a lot of the research, and the research is limited to begin with.

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Haskell/Idris syntax is mostly awesome, use it. (TODO: check this. The weird parentheses style may lose too many users)

* Python - whitespace
* Elixir - improved Erlang
* TypeScript - JS with static typing
* PHP - no design at all
* Objective C - weirder than C++
* Haskell - poor library design

TODO: see if there are any more Unicode guidelines relevant to writing a programming language parser

Natural language like Inform 7, while interesting, is quite wordy. It's also hard to scan through.

Fortress has "mathematical syntax", with an ASCII form and typeset form. They used LaTeX but HTML / MathML output should be possible too. And juxtaposition was overloaded. Probably worth emulating.

A language encourages certain expressions of thought. If the syntax is awkward then the feature will be used less and a bias will be introduced. But the styles of programming people come up with after a language is released are often completely different to what was intended by the language (e.g. Java and its design patterns). It's not clear that anything can be done about this, besides capturing as many existing patterns as cleanly as possible.

Textual
-------

There are some people who, when confronted with the complexity of syntax, think "It's better to use a binary format and store everything in a database." Now they have two problems. Math is textual, English is textual, the only stuff that isn't textual are flowcharts and tables. Flowcharts might be OK but graph layout is hard - graphviz barely works, and IDA Pro's graph layout when it decompiles stuff is quite lacking. Tables lead into spreadsheet programming which is generally not expressive as a language - and the formulas and cell values are textual. If you show me a way to write 123.6 that doesn't involve text maybe I'll think about this more.

There's also structural editing, `lamdu <http://www.lamdu.org/>`__ and so on, but they are designing an IDE alongside a programming language. I'm not too interested in IDEs and given that half the IDEs are for languages that also have a textual syntax, syntax doesn't seem to be a big factor in writing such an IDE.

Legibility research
-------------------

The main factor improving readability is consistency; reading is disrupted when unconventional layouts are used.

Spacing helps identify boundaries:
* For words, intra-letter spacing should be significantly smaller than inter-word spacing.
* For sentences, there should be extra space after the period, although the period's whitespace itself is distinctive.
* Justified text is harder to read than ragged-right due to the inconsistent spacing arising from bad line-breaking.
* The default inter-line spacing (line height, leading) is fine for most people. Some people with disabilities need more. Longer lines can use a little bit more line height.
* To identify paragraphs, inter-paragraph spacing should be visibly larger than inter-line spacing, or paragraph indentation should be used.

Left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line.

Maximum line length is an open question. Diff programs seem like the limiting factor, but on my monitor I can fit 2 108-character texts at the default font size side-by-side along with a space in the middle and the taskbar. Rounding this down to 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt / 24px I can fit 97 characters, while a little less (17.25pt / 23px) fits 102 characters. The standard is 80 characters but monitors are wider now, so again 100 seems plausible. This can really only be tested by finding long lines of code and asking what line-breaking placement is most readable.

Code legibility
---------------

IMO layout improves code legibility. There haven't been any formal studies that I can find, but Python syntax is often said to be "clean". Also layout makes arguments over where to put braces moot. Hence Stroscot has layout.

Reading code top-to-bottom, left-to-right makes sense. So definitions should be on the left, blocks indented in, and lines themselves should read left to right. So Ruby's statement modifiers ``X if Y`` are a bad idea because the ``if Y`` is easy to miss when scanning control flow.  But operators like ``a = b xor c`` are fine because the assignment ``a =`` is clear and if the value of the expression matters you're reading the whole line anyway and can parse it in your head.

Unicode can improve legibility when the syntax is standard (e.g. θ for angles), but generally long names like ``Optimiser(learning_rate=...)`` are more readable than ``Optimiser(η=...)``. Programmers have neither the time nor the inclination to learn a new character set and accompanying set of conventions.

When the convention is established, short names are clearer than long names. Writing ``(+) { augend = 1, addend = 2 }`` is less clear than the simple ``1+2`` - the long names are not commonly used. But it is arguably still useful to include the long names, e.g. for currying.

A study :cite:`dossantosImpactsCodingPractices2018` found the following:

Putting opening braces in a line of their own (C# convention), as opposed to the same line of the statement, improved readability. The extra white space and matching vertical alignment of related curly braces makes blocks clearer. Closing curly braces terminating code blocks should be on their own line, except for secondary paths of execution, e.g.: closing brace of if statements followed by an else; closing braces of try statements followed by a catch.
Line lengths must be kept within the limit of 80 characters;
Each statement should be in a line of its own; do not separate multiple statements by a ‘‘;’’ in a single line.
Use import clauses instead of qualified names to reference names in code.
Frequent calls to sub-properties of class member properties should be made storing a reference to that sub-property, avoiding multiple statements containing long chains of objects and sub-properties;
Identifier names should use dictionary words for readability.
Grouping instructions using blank lines was inconclusive. Some thought the blanks broke the flow, others liked it.
On indenting 2 spaces vs 4 spaces, there was no consensus.
Nesting conditionals more than three levels deep was considered by some to be easy to read and clearer than using a complex condition. But the majority preferred refactoring to an ``else if`` chain.
Using variables to store intermediate parts of long logical expressions is only useful if that intermedate expression has a meaningful name and purpose or the expression is repeated. Otherwise it adds clutter, and you are better off just writing the complex expression.

:cite:`bauerIndentationSimplyMatter2019` studied indentation with eye tracking and found no statistically significant difference between 0,2,4,8 spaces. Looking at their graphs 4 spaces does seem to be a sweet spot though.

Another study :cite:`buseMetricSoftwareReadability2008` identified factors for readability, in decreasing order of significance:

* fewer identifiers per line
* shorter lines (characters)
* fewer '(' '{' '.' ','
* less indentation (preceding whitespace)
* fewer keywords
* more blank lines
* lower maximum occurrences of any single identifier
* shorter maximum length of identifier
* lower maximum occurrences of any single character
* more comments
* fewer '='  numbers spaces '==' '<' '>' 'if' 'for' 'while'
* higher number of '+' '*' '%' '/' '-'

They constructed several models using these factors, mainly a Bayesian classifier, all of which predicted average readability scores better than the human raters. But the model is not public.

Proportional fonts
------------------

For prose, a proportional fonts is more readable than monospace because it is denser and hence less eye movement is needed for reading. Spaces between words are easier to see. :cite:`arditiReadingFixedVariable1990` But proportional fonts have not caught on in programming. The main complaint is that identifiers do not line up nicely the way they do with a monospace font.

After reading about elastic tabstops I've come up with my own solution, "tablike spaces". The idea here is to use a proportional font for rendering, but to make the spaces jump to the pixel column they would use if the font was monospaced. So rendering "a bit of text" would render "a" at 0, "bit" at 2 ems, "of" at 6 ems, and "text" at 9 ems, where an em is the width of the widest character in the font.

A more complex algorithm treats the text as a giant table, so "a bit of text" gets split up into 4 cells "a ", "bit ", "of ", "text" which span 2,4,3,4 columns respectively. Then the column widths are calculated using the `auto table layout algorithm <https://www.w3.org/TR/CSS2/tables.html#auto-table-layout>`__ (simplified):

* Set the width of each column to 0.
* For each cell, calculate the width as rendered by the font, and increase the widths of the columns it spans so that together, they are at least as wide as the cell. Widen all spanned columns to be approximately the same.

Yet more complex is to treat it as a constraint problem. The constraints consist of minimum width constraints from the width of the tokens and order constraints that specify which chunks of text are before/after/line up with other chunks. The goal is to minimize the width of the table (sum of column widths), and as a secondary objective make the widths as uniform as possible (lowest standard deviation or absolute deviation). The Cassowary algorithm might work.

The constraint algorithm allows aligning the ends of text by justifying, so e.g. ``foo =`` and ``bar =`` have the identifiers stretched to the same width. But generally it is only the start of the text that needs to be aligned.

TODO: test it out by modifying https://github.com/isral/elastic_tabstops_mono.vsce

The advantage of tablike spaces over elastic tabstops is that the underlying text file is just indenting with spaces in a monospaced font. So it's only the visual presentation that changes, hence it can be used on a team.

Parsing
=======

I've got a basic Earley algorithm working for now. But eventually I'm extending it with BSRs and layout and other fun things. There's also `Yakker <https://github.com/attresearch/yakker>`__, which is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

  A new parsing engine, Yakker, capable of handling the requirements of modern applications including full scannerless context-free grammars with regular expressions as right-hand sides for defining nonterminals. Yakker also includes facilities for binding variables to intermediate parse results and using such bindings within arbitrary constraints to control parsing. Yakker supports both semantic actions and speculative parsing techniques such as backtracking and context-free lookahead and several parsing back ends (including Earley, GLR and backtracking).  In addition, nonterminals may be parameterized by arbitrary values, which gives the system good modularity and abstraction properties in the presence of data-dependent parsing. Finally, legacy parsing libraries, such as sophisticated libraries for dates and times, may be directly incorporated into parser specifications.

I've looked at various algorithms but I think the only way to handle it completely correctly and generically is to have a disambiguating pass on the set of parse tree generated by a nondeterministic automaton. The alternatives involve restricting parsers to be deterministic, for example PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

Treesitter implements incremental LR parsing with error recovery, but since it doesn't support ambiguity I don't think it's sufficient for a compiler.

Revisiting this, the goal is to use partial evaluation to generate the parser, by speeding up a naive brute-force algorithm applied to the grammar. There is already a paper on LR parsing by partial evaluation :cite:`sperberGenerationLRParsers2000` and also on specializing Earley, so with sufficiently powerful compiler optimization handling general grammars should be possible.

In particular the parser should be written as a nondeterministic finite state transducer that builds up trees (outputs a list in the style of start-children-end or S-expressions or something).

Formally:

* Q is a finite set, the set of states;
* I is a subset of Q, the set of initial states;
* F is a subset of Q, the set of final states; and
* Σ is a finite set, called the input alphabet;
* Γ is a finite set, called the output alphabet;
* The transition function is of type :math:`Q \times (\Sigma \cup \{\epsilon \})\to P(Q \times (\Gamma \cup \{\epsilon \}))`, where ε is the empty string and P(Q) denotes the power set of Q.

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

Assignment
==========

As a syntax ambiguity, there are two different interpretations of assignment, pattern binding and clause definition. The difference:

::

  pair = (1,2)
  (x,y) = pair # binding B

  # B as a pattern binding - defines two clauses
  x = case pair of (x,y) -> x
  y = case pair of (x,y) -> y
  --> x = 1
  # B as a clause definition
  (,) = \x y -> pair
  --> x not in scope, (3,4) reduces to (1,2) reduces cyclically to itself

Clearly the pattern binding is more useful here. So we have a basic convention for assignments: if the head of the LHS is a constructor symbol then it's a pattern binding. What is a constructor symbol? Well, it's up to the code, defined by the predicate ``isConstructor``. Most symbols are not constructors, so the ones that are constructors are declared with ``isConstructor sym = true``  or the macro declaration ``constructor sym``.

Assignment pattern bindings are irrefutable, meaning they never fail directly and instead define unevaluated variables that will raise pattern matching exceptions when evaluated. But there is an alternative syntax that allows failure as a control operation (from Idris / Inko):

::

  pat = val | <alternatives>
  p

is desugared to

::

  case val of
    pat -> p
    <alternatives>



If a clause does not match, the expression does not reduce - there is no error at all.

In the case of a simple variable ``x = ...`` the definitions coincide - the end result is a clause definition.

Another way to resolve the ambiguity is to use separate syntaxes, e.g. to use ``(x,y) <- pair`` for pattern bindings. But remembering to switch between pattern bindings and clause definitions is tedious.

The explicit syntax does allow defining new reduction rules for constructors. But if overriding basic syntax is desired, ``isConstructor`` can be locally overridden, e.g. if we want a sorted pair:

::

  (x,y) | x > y = (y,x)
    where
      isConstructor (,) = false

Usually it's more natural to use a new symbol, like ``sortedPair (x,y)``, so that the global definition of pairs is not affected.

Constructor discipline
----------------------

Haskell has a division between constructors and functions:
* identifiers starting with lowercase letters are functions, and can only be used with function bindings.
* identifiers starting with uppercase letters are constructors, and assignments of the form ``X a b = ...`` are pattern bindings.

This rule reduces maintainability. If the representation is changed there is no way to replace the dumb constructor with a smart constructor. So instead libraries are littered with boilerplate pseudo-constructors like ``mkThing = Thing`` to get around this syntactic restriction. In fact in :cite:`kahrsNonOmegaOverlappingTRSsAre2016` there is a boilerplate trick to turn any TRS into a constructor TRS, by duplicating ``foo`` into a constructor ``Foo`` and a function ``foo``, converting subterms of the original rules to match on constructors, and adding rules that turn stuck patterns into constructors. For example ``k x y = x; s x y z = (x z) (y z)`` turns into:

::

  app (App K x) y = x
  app K x = App K x
  k = K

  app (App (App S x) y) z = app (app x z) (app y z)
  app S x = App S x
  app (App S x) y = App (App S x) y
  s = S

This is pretty verbose but it's curried so it isn't as bad as it could be. For rules like associativity ``x*(y*z) = (x*y)*z`` and distributivity ``x*(y+z) = x*y+x*z`` handling all the stuck pattern rules for symbols ``+`` and ``*`` is a nightmare, and you also have to come up with alternative operator names for the constructors.

So Stroscot follows Pure in not having a constructor discipline. By appropriately setting ``isConstructor = true`` any symbol can be used as a constructor pattern on the left-hand side of an equation. Also any symbol may act as a constructor symbol in a value if it happens to occur in head position in a normal form term, regardless of ``isConstructor``.

There is a general convention for the standard library to use lowercase for potentially reducible expressions or "smart" constructors and uppercase for dumb data constructors. This is to vaguely follow Haskell.

Recursive definitions
---------------------

We want to support mutually recursive definitions, like so:

::

  a = 1 : b
  b = 1 : a

And also sequential execution, like so:

::

  a = openFile "a.txt"
  b = openFile "b.txt"

So the question is how ``b`` can be in scope in the body of ``a`` in the recursive version. Presumably it isn't in scope in the sequential version.

Type declarations
=================

``2 : s8`` and ``s8 2`` seem more logical compared to other choices such as ``a : s8 = 2`` (Swift,Jai - hard to find the = with long types) or ``s8 a = 2`` (C,Rust - overlaps with function definition). The name is simply a syntactic handle to refer to the value; it doesn't have an innate type. In contrast the representation of the value must be specified to compile the program. The second syntax is similar to assembler syntax such as ``dword 0``.

Namespacing
===========

``.`` is preferred to ``::`` because it's shorter and because modules are first-class. And as in Go, no ``->``, always ``.``.

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

Indentation-sensitivty like Python seems great. It is readable and when copy-pasting code you only have to fix up the indentation by moving the block left/right (supported by all modern code editors) instead of messing with braces.


Haskell's layout rules seem overly restrictive, for example this is not allowed:

::

  let bang_upper = Bang (Rule
    (Sequent newcut_bseq (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
    (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br))))

Although the parentheses make this unambiguous, Haskell requires indenting a lot more, past the ``=``:

::

  let bang_upper = Bang (Rule
                    (Sequent (fst bl_tseq, newcut_bseq) (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
                    (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br)))

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

Call syntax
-----------

There are a few ways to write function calls:
* Curried style: ``f (g a 1) (h b 2)``
* Lisp style: ``(f (g a 1) (h b 2))``
* C style: ``f(g(a,1),h(b,2))``
* C with spaces style: ``f(g(a 1) h(b 2))``
* Coffeescript: ``f (g a 1), (h b 2)``
* Postfix: ``2 b h 1 a g f``
* Postfix with argument counts: ``2 b 2 h 1 a 2 g 2 f``
* Explicit call, C: ``call(f,call(g,a,1),call(h,b,2))``
* Explicit call, curried: ``call f (call g a 1) (call h b 2)``

Comparing character counts, postfix is 13, C is 16, and curried is 17. For a simple function application ``f a b c`` curried is shorter by one character than C (more if you add a space after the comma like ``f(a, b)``, as is common) and the spaces are easier to type. Curried loses in character count only if you have a pattern like ``f (a) (b) (c)`` where all the expressions need parentheses. The curried style still allows passing a tuple and matching the C syntax.

Lisp is curried with extra parentheses. Coffeescript is curried with extra commas. Explicit call C is curried style with call inserted before parentheses and commas instead of spaces. Explicit call curried is curried with call inserted before functions. Postfix is pretty much unreadable so I'm ignoring it. So the two main contenders are curried and C.

The C style is incredibly common, whereas curried is only used by functional languages like Haskell and OCaml. But I'm still going with curried for now, because:

Arguments: (`Reddit thread <https://www.reddit.com/r/ProgrammingLanguages/comments/jde9xp/advantages_of_currying_in_a_programming_language/>`__)
* Curried is more readable - the spaces and parentheses have more vertical variation compared to commas
* Curried is pretty simple, only a bit more complex than S-expressions
* Curried is good for writing curried functions. In contrast the C style makes it inconvenient to use curried functions, you have to write lots of parentheses ``f(1)(2)(3)``. Also comparing ``any (== x) l`` and ``any(\y -> y == x,l)``, in the C style the comma is almost unnoticeable and the syntax is ambiguous as it could be grouped ``(x,l)``
* Curried style still allows tuples as arguments, pretty much matching the C style. In contrast the C style forces a tuple even if the combination of arguments doesn't represent a meaningful idea.

The main question is which style makes it easier to match parentheses - mismatching is a common novice programming error. :cite:`singerFunctionalBabyTalk2018` Also error messages for accidental partial application are important. TODO: test or survey some novice programmers later on

Implicit arguments
------------------

Claim: Explicit argument passing cannot replace implicit arguments

See example:

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

Default arguments and overloading
---------------------------------

`The Go FAQ <https://go.dev/doc/faq#overloading>`__ and `Rob Pike <https://talks.golang.org/2012/splash.article>`_ say Go deliberately does not support overloading or default arguments. Supposedly:
* overloading is confusing and fragile
* adding default arguments to a function results in interactions among arguments that are difficult to understand
* Naming separate functions leads to a clearer API
* Leaving these out simplifies the type system and method dispatch.

While separate functions can be clearer, supporting overloading and default arguments does not prevent creating separate functions. And when you do have a constructor-like method which supports several slightly different combinations of arguments, even the Go FAQ admits overloading is "useful". Similarly Pike admits there are cases where API design flaws can be patched by adding a default argument. And Stroscot is dynamically typed, so overloading doesn't complicate the type system at all. And although it complicates method dispatch, overloading enables generic functions and solves the expression problem.

Furthermore if you are trying to mimic a Java library that makes heavy use of this sort of overloading,name mangling using simple rules will give very long names. While using complicated rules will give shorter names, the names will be impossible to remember. Implementing overloading is strictly better than skipping it.

So the verdict here is that Go is excluding things people want to do, with unworkable alternatives.

n+k patterns
============

This is a feature removed from Haskell that simplifies writing recursive integer functions, like factorial. Basically ``case v of { x+k -> e; _ -> e' }`` translates to ``if v >= k then (\x -> e) (v-k) else e'``, where ``k`` is a literal.

Arguments:
* concise special notation, like for tuples and lists
* unfamiliar: the symbol + is being abused
* unnatural: not clear that residue must always be ``>= 0``, i.e. pattern matches a natural number
* easy to change to a guard clause ``case v of { x | x >= k -> let x = v-k in e; _ -> e' }`` or a view pattern ``case v of { (dec k -> Just x) -> e; _ -> e' } where dec k v = if v >= k then Just (v-k) else Nothing``

GHC-specific:
* Pattern still applies even if ``(+)`` is rebound away from ``(Prelude.+)``.
* only works for ``k >= 0``, as writing ``n+(-1)`` is forbidden.

Pattern synonyms should allow defining this like a view pattern, but without the ugly ``Just``. Then the pattern like ``x@(dec k) -> e`` solves the main issues: dec is its own symbol, and the user has imported it so knows its semantics. And ``k`` should be evaluated so can be a negative number or constant expression.

Chained assignment
==================

w = x = y = z
the value of z is assigned to multiple variables w, x, and y

the evaluation strategy differs between languages. For simple chained assignments, like initializing multiple variables, the evaluation strategy does not matter, but if the targets (l-values) in the assignment are connected in some way, the evaluation strategy affects the result.

In Python, assignment statements are not expressions and thus do not have a value. Instead, chained assignments are a series of statements with multiple targets for a single expression. The assignments are executed left-to-right so that i = arr[i] = f() evaluates the expression f(), then assigns the result to the leftmost target, i, and then assigns the same result to the next target, arr[i], using the new value of i.[9] This is essentially equivalent to tmp = f(); i = tmp; arr[i] = tmp though no actual variable is produced for the temporary value.


The `literature <http://www.cse.iitm.ac.in/~amannoug/imop/tr-3.pdf>`__ classifies this as "syntactic sugar", so handling it in the parser like Python seems the reasonable solution. C's "the assignment returns the lvalue" semantics is possible too but seems contrived. C's RTL semantics is probably better than `Python's LTR <https://docs.python.org/3/reference/simple_stmts.html#assignment-statements>`.  So a chain ``a = b = 2`` expands to ``b = 2; a = b``.

Using ``=`` for equality comparison conflicts with chaining ``a = b = 2``, because it can be interpreted as ``a = (b == 2)``. Really ``a = b = 2`` doesn't really seem that useful when you can just replace ``a`` with ``b`` in the rest of the expression. If you need multiple variables with the same value then you would write ``[a,b,c] = replicateM 3 (ref 0)`` rather than using a chain, because a chain would alias to the same variable. Python already has this problem with aliasing for ``a = b = []``, because ``[]`` is mutable.


Chained update with ``:=``, ``a := b := 2``, seems implementable. It doesn't conflict with equality and shortens some assignments.

Embedded assignment
===================

This embeds assignments in expressions, like

::

  a = (b = 1) + (c = 2)

Clearly it conflicts with equality comparison.

But for chained update it is unambiguous and returning the value would be possible:

::

  a = (b := 1) + (c := 2)

But then statements like

::

  b := 1

would have an unused return value. Maybe this value could be marked as optional somehow.

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