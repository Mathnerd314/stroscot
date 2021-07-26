Syntax
######

Design
======

The stuff here is mostly a dumping ground of ideas while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and picking out the nicest examples. But in the end, syntax is decided by usage, so a lot of the syntax here will probably become final.

Quorum and its associated set of syntax studies provide useful datapoints on keywords and constructs. But Stroscot has a unique design so we can't use a lot of the research, and the research is limited to begin with.

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Haskell/Idris syntax is mostly awesome, use it.

TODO: see if there are any more Unicode guidelines relevant to writing a programming language parser


Legibility
----------

The main factor improving readability is consistency; reading is disrupted when unconventional layouts are used.

Spacing is important to identify word boundaries (intra-letter spacing significantly smaller than inter-word spacing) and sentence boundaries (two spaces after the period, although the period's whitespace itself is distinctive). Justified text is harder to read than ragged-right due to the inconsistent spacing arising from bad line-breaking. Left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line. The default line spacing seems fine.

Line length is a good question. Programming uses fixed-width characters so it's measured in characters. 80 characters is standard, but monitors are wider now, so 100 is plausible. Diff programs are often the limiting factor, but on my monitor I can fit 2 108-character texts side-by-side along with a space in the middle and the taskbar. 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt (24px) I can fit 97 characters, while 23px fits 102 characters.

Values
------

Number syntax is `Swift's <https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html#grammar_numeric-literal>`__, but liberalized. Floating-point notation for integers. Binary exponents for decimals. Use of format characters besides xob.

Blocks
------

Blocks are inspired by Haskell's do notation, but have a twist, based on the observation that the continuation monad is `the mother of all monads <https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/the-mother-of-all-monads>`__. Since it's the mother, we don't lose anything by fixing the monadic operations in the do-notation to be the continuation monad operations. That link gives a generic way to implement monads via the continuation monad, but the direct implementation is pretty clean. For example the `StateT monad <https://github.com/Mathnerd314/stroscot/blob/master/tests/Continuations-State.hs>`__.

Using the continuation monad allows us to separate commands (not returning a value) and operations (returning a value).

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

This might seem overly complicated, but it's based on Zarf's rule-based programming. When you're defining lots of rules for a IF game then specifying priorities by hand is tedious.
