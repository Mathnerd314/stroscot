Stateful programming
####################

In Stroscot, like in denotational semantics, a program is conceptually a mathematical function. That is, for any given input, the program will always produce the same output, and this output is the entire scope of the program - there are no implicit side effects, like mutating some external state. Practically, this works well for using the programming language like a calculator: put in an expression, get an answer. And theory-wise the denotational semantics is useful for reasoning about the behavior of programs, such as asking whether two programs are equivalent.

Blocks
======

One issue with this denotational semantics view is that most programs these days are expected to operate in an interactive manner, receiving input, producing output, and manipulating a persistent state. How do we handle this? Well, we extend the semantics to include more values. In particular, the output of the program now includes values representing procedures or blocks of statements. So for example we might have a simple "check password" block that inputs a string, exits with failure if it is not the string "42", and otherwise outputs the string "You got it" and exits with success.

Following Haskell, what we need is do-notation. In Haskell, this is by default tied to monads, but with GHC's language extensions such as RebindableSyntax and QualifiedDo, there are really no restrictions on the types. The basic syntax is three productions::

  statement {e} = e
  command {e;stmts} = e >> {stmts}
  operation {pat <- e; stmts} = e >>= \x -> case x of { pat -> {stmts}; _ -> fail "" }

In Haskell, ``e >> m = e >>= \_ -> m``, and the type is ``(>>) : IO a -> IO b -> IO b``. I actually consider this a mistake. First, ``IO a`` makes it too easy to ignore return values - it should at least be a fixed type like ``IO ()``. But even ``()`` has some use, e.g. to allow returning exceptions, so really ``()`` should be replaced with a specialized type like ``FixedReturnValue``. At that point it seems worth defining a separate type synonym. So we can really consider statements to be two disjoint categories, ``Command = IO FixedReturnValue`` and ``Operation a = IO (a \ FixedReturnValue)``. If we hide ``FixedReturnValue`` then we just get two opaque types ``Command`` and ``Operation a`` and a combination ``IO a = Command | Operation a``. At that point there is essentially no linkage and we can just take ``Command`` and ``Operation a`` as two distinct types to begin with. So really ``(>>)`` should not be hardcoded to its monadic definition, for example it is useful to take ``(>>)`` to be function composition or category composition.

There is also ``return``, which in Haskell is one of the monad operations. There is definitely a difference between writing ``f prompt = getLine prompt`` and ``f prompt = { return (getLine prompt) }``, and it is not clear how one would phrase the second version "return a block that reads a line with this prompt" any other way.

Ensuring that adding or removing braces on a single statement is a no-op is a useful property; this property is clear based on the translation but may be confusing to some. So for example we would write ``f prompt = { getLine prompt }`` and ``f prompt = return (getLine prompt)``. The "last statement's value is returned" property is somewhat common in newer languages, the naked return less so.

There are many extensions to do-notation, we go over them here.

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

In Idris, the notation ``!expr`` within a block means that the expression ``expr`` should be bound in the block to a temporary before computing the surrounding expression. The expression is bound in the nearest enclosing block. Expressions are lifted leftmost innermost. It's a purely syntactic extension, introducing no new operations.

It's kind of noisy though since you have to write the bang on each subexpression. What we'd really like a natural way to enforce LTR call-by-value-style evaluation order on a compound expression. So for example if ``!(f (g (print y) x))`` could desugar to the same as above, disambiguating based on whether each subexpression was a simple value or a block. At that point the evaluation order might even be implicit as in Java, without even the need for a bang. But we also want to preserve non-strictness; it's kind of hard to determine whether an expression is a block without evaluating the expression, but possible.

Monad comprehensions
--------------------

Originally, there were list comprehensions, a convenient means to construct lists based loosely off set-builder notation. Then, people realized that they could be used for any monad, so now they support various operations. The full syntactic translation is as follows:

::

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
    (Qv,Rv) <- mzip D[ Qv | Q ] D[ Rv | R ] -- Qv is the tuple of variables bound by Q (and used subsequently)
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
    let Qv = (fmap selQv1 ys, ..., fmap selQvn ys) -- selQvi is a selector mapping Qv to the ith component of Qv
    D[ e | R ]

  D[ e | Q then group by b using f, R ] =
    ys <- f (\Qv -> b) D[ Qv | Q ]
    let Qv = (fmap selQv1 ys, ..., fmap selQvn ys)
    D[ e | R ]

We see that ``guard :: Bool -> f ()`` and ``mzip :: m a -> m b -> m (a, b)`` are new functions. ``guard`` actually has some non-trivial behavior - there's special integration of the blocks (monads) and exceptions so exceptions propagate between pure expressions and blocks. ``mzip`` is implementable using ``liftA2 (,)``. In fact, in the base library, lists are the only non-``liftA2`` instance, using the ``ZipList`` instance. Thus the parallel comprehensions really are only applicable to lists.

ApplicativeDo
-------------

ApplicativeDo :cite:`marlowDesugaringHaskellDonotation2016` has two functions. The first is to make some do-notation sequences be Applicative-only and not use Monad. In fact though, such Applicative-only sequences are limited; they are exactly the sequences handled by idiom brackets, the ``liftAn`` family of operations. If we assume a single variadic function ``liftA``, we can do the transformation ``{a <- ax; b <- bx; return (f a b)} = liftA f a b``. ``liftA`` is shorter and clearer, so the value the do-notation translation provides is minimal when the functor is only Applicative and not a Monad. Furthermore, for many monads, the applicative functions compile to exactly the same code as the monadic functions.

The second function of ApplicativeDo is performance: in "some" monads, the applicative operation is more efficient than the corresponding monadic operations. Marlow's example is the Haxl DSL:

::

  numCommonFriends :: Id -> Id -> Haxl Int
  numCommonFriends x y = do
    fx <- friendsOf x
    fy <- friendsOf y
    return (length (intersect fx fy))

Basically, with monadic ``liftM2``, the ``friendsOf`` operations cannot be easily performed in parallel, because the computation must first materialize an ``fx``, whereas with ``liftA2`` it is clear that the operations are independent and can be performed in parallel. My reaction is that, if you're writing a DSL, then writing it as a macro is much more powerful than trying to shoehorn it into an applicative framework, or in general any kind of "shallow" framework where you have to implement each operation piecemeal and can't examine the entire computation as an AST. The details in the paper confirm this: the translation to use applicative operations is ambiguous, reordering based on commutativity may be useful but is design-dependent, and overall the best translation still depends on a detailed estimate of the costs of each sub-computation. And of course their approach can't compute these costs because functions are opaque. It's exactly these kinds of details that *are* accessible in a DSL - you just write a pass that walks over the expression tree and estimates the costs. Similarly the `use/def analysis <https://en.wikipedia.org/wiki/Use-define_chain>`__ that they use for the rewriting is a standard compiler pass.

Verdict: Just use ``liftA`` or a DSL, this specific syntax is not needed.

RecursiveDo
-----------

:cite:`erkokValueRecursionMonadic2002` introduced the idea of "value recursion", in particular the operation ``mfix :: MonadFix m => (a -> m a) -> m a``. The GHC extension RecursiveDo introduces two syntaxes based on this operation, ``mdo`` and ``rec { }``. Erkok's motivating example was a circuit DSL:

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

Investigating Hackage, value recursion is uncommon. "Many Haskell programmers will never use it in their careers." (`1 <https://ro-che.info/articles/2015-09-02-monadfix>`__) Uses fall into categories:

* DSLs, where variable assignments are interpreted as data
* Gratuitous (no/one binding, or bindings do not refer to bindings from later)
* Rare use cases where using ``mfix`` directly is probably clearer than either ``mdo`` or the rec-notation

For example, in the I/O monad, ``mfix`` can be used to fork two threads that kill each other::

   mdo
      a <- fork $ killThread b
      b <- fork $ killThread a

   -- vs
   bId <- newEmptyMVar
   a <- fork $ readMVar b >>= killThread
   b <- fork $ killThread a
   writeMVar bId b

But the version with the variable is arguably clearer. The code for IO's mfix uses unsafeDupableInterleaveIO. This has been the subject of at least one `bug <https://gitlab.haskell.org/ghc/ghc/-/issues/5421>`__ (`two <https://gitlab.haskell.org/ghc/ghc/-/issues/15349>`__ counting fixST), and is why there is both fixIO and `unsafeFixIO <https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Unsafe.html#v:unsafeFixIO>`__. Reasoning about fixIO seems to `require <https://wiki.haskell.org/Evaluation_order_and_state_tokens>`__ laziness semantics and maybe also an understanding of Haskell's state-token-based I/O model.

For ``mfix`` vs ``mdo``, there are many implicit laws promoted by the ``mdo`` notation that are not satisfied. For example, right shrinking:

::

   lhs = mdo
      z <- f z
      w <- g z
      return (z,w)

   -- is NOT usually equivalent to

   rhs = mdo
      z <- mdo
               z <- f z
               return z
      w <- g z
      return (z,w)

Unfortunately, the only known monad satisfying right shrinking is the lazy state monad ``s -> (a,s)`` (and its restrictions, the output monad ``Monoid w => (a,w)``, reader monad ``p -> a``, identity monad ``a``, and trivial monad ``Void``). Erkok has a proof 3.1.6 that the trivial monad ``const Void`` is the only monad satisfying left-strictness ``undefined >>= f = undefined``, right shrinking, strictness 2.1.1, and various other properties. The setup is the above right shrinking rule where ``f xs = return (1 : xs); g xs = case xs of [x] -> return x; _ -> return 1``. He says "It is easy to see that [the LHS mdo block] must yield bottom by the strictness property". Expanding on this, if we start with ``(z,w) = undefined``, then after one loop we get ``z=1:undefined`` and ``g z = undefined``, so the overall function returns ``undefined`` by the left-strictness property, so therefore by strictness 2.1.1 the overall ``mfix`` is also undefined. But of course, if we start with the fixed point of the RHS, ``(z,w) = (repeat 1,1)``, we get that back even in the LHS. So Erkok's proof relies on strictness and ``mfix`` producing the least fixed point. Using similar arguments about bottom, there is a proof that Haskell `cannot have a State monad <https://smallbone.se/no-state-monad.html>`__. Really, the discussion should focus on the monad's behavior for defined values and total functions, and not discuss bottom or seq at all. I think it's best to suppose that the premises of the proof are incorrect, but the fact remains that the only known right-shrinkable monad is the lazy state monad. Absent other monads, it seems the lazy state monad is really the canonical implementation of ``MonadFix``, similar to how lists are the only non-trivial implementation of ``MonadZip`` and everything else is just lifting.

But Erkok had to write a thesis, so of course he can't just say "lazy state is MonadFix" and leave it there. Erkok proposes to leave out right shrinking and other properties to obtain a more general definition. The main issue with this is that ``mfix`` for the lazy state monad is no longer unique - there is a "shallow" mfix operations which simply apply the function to the bottom state. Erkok's definition of ``mfix`` for I/O is shallow in this sense. ``mfix`` cannot be implemented in general for ADT-style monads. (c.f. `GHC.Generics instances <https://hackage.haskell.org/package/base-4.18.0.0/docs/src/Control.Monad.Fix.html#line-140>`__). These operations are kind of useful for

data    U1        p = U1                  -- lifted version of ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper
newtype Par1   p = Par1 { unPar1 ::   p } -- gives access to parameter p
newtype Rec1 f p = Rec1 { unRec1 :: f p } -- a wrapper

Consider f ⊥. If it is ⊥, mfix f must be ⊥ by strictness. If f ⊥ = Left l, then f must factor through Left by monotonicity, i.e., there must be a function h such that f = Left · h, or equivalently, h = unLeft · f. Then mfix f = mfix (Left · h) = Left $ mfix h = Left $ mfix (unLeft . f). Similarly Right r must factor as f = Right . h, h = unRight . f, and mfix f = Right $ mfix (unRight . f). So to summarize, we have:

  mfix f = case f ⊥ of
   L1 _ -> L1 $ mfix (\x -> case f x of L1 y -> y)
   R1 _ -> R1 $ mfix (\x -> case f x of R1 y -> y)

 (4.3)
Just
 → return (fix (unJust · f ))
Note that we did not make any choices in constructing Equation 4.3; the behavior of
mfix is completely dictated by the properties that must be satisfied by all value recursion
operators. We leave it to the reader to show that Equations 4.2 and 4.3 are equivalent,
establishing uniqueness.


instance MonadFix Par1 where
    mfix f = Par1 (fix (unPar1 . f))

-- | @since 4.9.0.0
instance MonadFix f => MonadFix (Rec1 f) where
    mfix f = Rec1 (mfix (unRec1 . f))

-- | @since 4.9.0.0
instance MonadFix f => MonadFix (M1 i c f) where
    mfix f = M1 (mfix (unM1. f))

-- | @since 4.9.0.0
instance (MonadFix f, MonadFix g) => MonadFix (f :*: g) where
    mfix f = (mfix (fstP . f)) :*: (mfix (sndP . f))
      where
        fstP (a :*: _) = a
        sndP (_ :*: b) = b



 And then, for continuations, Erkok has a type-theoretic argument for why no implement of mfix exists. For codensity, there are `several potential implementations <https://github.com/ekmett/kan-extensions/issues/64>`__ of ``mfix``, based on I/O and state, but nobody has proven them correct.

or just a few operations (mfix state, fixIO, fix applied to a generic traversal of a strict sum-like monad) that happened to be collected in an ad-hoc manner. The main issue with the "relaxed" mfix definition is that, because the instances satisfy barely any properties, the ``mdo`` notation is completely unintuitive - simply adding a non-recursive statement at the end can break the program. For this reason, ``mdo`` is pretty much deprecated and most recommendations are to use ``mfix`` directly or ``rec {}`` which is just a lightweight macro for ``mfix`` with a tuple argument. At that point, ``mfix`` is just an ordinary function, not really part of the monad syntax.

``let`` allows recursive definitions, bare definitions are not recursive:

::

  x = x + 1 # defines a new x shadowing the old x
  let x = (x : Int) * 2 # defines a fixed point with unique solution x=0

  fact x = if x==0 then 0 else x * fact (x-1) # fails due to fact being an unbound symbol
  let fact x = ... # proper definition


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



I/O monad showdown
==================

One choice for operations
-------------------------

We might think that there are a lot of monads. After all, every library defines a few. But actually, there is a universal construction for monads. Specifically, ``Codensity m a`` in `kan-extensions <https://hackage.haskell.org/package/kan-extensions-0.5.0/docs/Control-Monad-Codensity.html>`__ is `the mother of all monads <http://blog.sigfpe.com/2008/12/mother-of-all-monads.html>`__ - it is a monad regardless of ``m`` (`see comment <http://blog.sigfpe.com/2008/12/mother-of-all-monads.html#c3279179532869319461>`__), and if ``m`` is a monad, then the monad values ``m a`` can be embedded and retrieved via ``lift :: m a -> Codensity m a`` and ``lowerCodensity :: Codensity m a -> m a``.

 ``(>>=)`` and retrieved via ``\f -> f return``. That blog post gives a generic way to implement monads via the continuation monad, but the direct implementation is pretty clean. For example the `StateT monad <https://github.com/Mathnerd314/stroscot/blob/master/tests/Continuations-State.hs>`__.



Monad constructions
-------------------

A monad transformer is a way of constructing monads from other monads - given a basic monad ``m a``, and a transformer ``T``, ``T m a`` is a new monad with a function ``lift :: m a -> T m a``. It is also generally possible to implement something like the inverse function ``lower :: T m a -> (m a | Fail)``, such that ``lower (lift x) == x``. Note though that ``T m a`` is generally larger, so ``lower`` erases some information and is partial. Monad transformers seem attractive - who doesn't want extra functionality in their I/O monad? But following this line of reasoning, applying a monad transformer once is not enough - we could apply the monad transformer a second time, and get even more functionality. To maximize functionality we would need an infinite monad transformer stack. But of course most type systems don't handle infinite types very well. The conclusion is that monad transformers are actually a clunky way to express functionality and we are better off implementing the functionality provided by monad transformers as dedicated features of the language. But to get rid of monad transformers completely, we need to ensure that these dedicated features provide functionality equivalent to arbitrary numbers of monad transformers, for every type of monad transformer.


Let's go through the list of existing monad transformers. I checked <https://hackage.haskell.org/package/transformers>`__, ChatGPT, and used various Google queries such as "monad transformer -MaybeT - StateT -...".

    IdentityT ``m a``
    MaybeT/OptionT ``m (Maybe a)``
    EitherT/ExceptT/ErrorT ``m (Either e a)``
    StateT/AccumT `` s -> m (a, s)``
    ReaderT ``r -> m a``
    WriterT ``m (a, w)``
    ContT ``(a -> m r) -> m r``
    ListT - ``m (Nil | Cons a (ListT m a))``
    pipes Proxy - ``Request (a', a  -> Proxy) | Response (b, b' -> Proxy) | M (m Proxy) | Pure r``
    Free codensity transformer ``(a -> m r) -> (f (m r) -> m r) -> m r``
    Free monad transformer ``mu T. m (Pure a | Free (f T))``
    ListZipperT: Transforms a base monad into a monad that supports efficient list manipulations.
    ParsecT: Enables building parser combinators using monadic style.

    LogicT
    SelectT

we see this is pretty much the case for Stroscot. The I/O store (discussed later in this document) allows implementing any number of StateT's (mutable variables). AccumT / WriterT is a mutable variable that's not read, and similarly ReaderT is a mutable variable that's not written. We can also use implicit parameters to get ambient values like ReaderT provides. Exceptions are a more general and powerful version of MaybeT/ErrorT/ExceptT/MonadFail. Logic-programming style nondeterminism covers ListT / SelectT.

The remaining monad transformer is ContT. ContT is not really a well-behaved monad transformer - although we can lift values ``m a -> ContT m a``, we cannot lift continuations, i.e. no function ``ContT Identity r a -> ContT m r a`` exists. Applying ``ContT`` twice, we find ``ContT r (ContT s m) a = ContT s m (Either (a, r -> m s) r)``

Continuations
-------------

Typing continuations is a little hard because they allow answer-type modification, e.g. the type of ``reset (3 + shift \k -> k)`` is ``int -> int``. Using prefix syntax ``reset (liftA (+) 3 (shift (\k -> k)))`` this ability to change type is a little more obvious. Since the operators are lambdas, the principal intersection types will be the most general, since intersection types can type all strongly normalizing programs. In this case it turns out we do not need the intersection operator and the Hindley-Milner type signature is sufficient. To express the types it is helpful to define the indexed continuation type ``ICont r s a = (a -> s) -> r``. Then the most general simple types are::

  return : a -> ICont b b a
  (>>=) : ICont i j a -> (a -> x -> j) -> x -> i

A general chain ``a >>= b >>= c >>= d`` has ``a`` of type ``ICont i j a1``, ``b/c`` of type ``a1/b1 -> ICont j/k k/l b1/c1 ``, ``d`` of type ``c1 -> x -> l``, and returns a function ``x -> i``. So the last callback in a chain can be represented using tokens or other weird things - it's only when we bind the continuation to another continuation that it has to use a function type. This freedom is useful when writing I/O simulators. Ignoring this the usual indexed monad signature for ``(>>=)`` is ``ICont i j a -> (a -> ICont j k b) -> ICont i k b``.

Using universal quantification and type constructors gives the `indexed codensity monad <https://www.reddit.com/r/haskell/comments/6vu2i4/fun_exploration_right_kan_extensions_swapped/>`__  or `right Kan extension <https://hackage.haskell.org/package/kan-extensions-5.2.5/docs/Data-Functor-Kan-Ran.html>`__ ``Ran m n a = forall r. (a -> n r) -> m r = forall r. ICont (m r) (n r) a``. ``ICont i j a = Ran (K i) (K j) a`` where ``K a b = a``.

Due to the quantification, the operations on ``Ran`` are restricted.  In particular ``callCC f = \c -> f (\x _ -> c x) c`` has type ``((a -> p -> j) -> ICont i j a) -> ICont i j a``, which does not unify with the desired type ``((a -> Ran n o b) -> Ran m n a) -> Ran m n a``. :cite:`wadlerEssenceFunctionalProgramming1992` section 3.4 says that the lack of callCC is a good thing because it means every continuation corresponds to an ``m-n`` operation. It's a semantic distinction: are your values "special" values with known types, hence in the type ``M m = forall r. m r`` and possible to use with callCC, or are they "return" values that have unknown structure?

We call the values in ``M m`` continuations, and the values in ``m r`` actions. A continuation represents "the future of the program". Executing a continuation plugs this future into a program description with a hole - usually there is one hole, but the continuation can discard the future or run it multiple times. The implementation can compile continuations to jumps under most circumstances and closures otherwise, so the execution model is also conceptually simple. Continuations are the basis in formal denotational semantics for all control flow, including vanilla call flow, loops, goto statements, recursion, generators, coroutines, exception handling, and backtracking. This allows a uniform and consistent interface. Continuations are more powerful than goto.

``Codensity`` is quite efficient compared to most ADT-style monads - the case analysis is pushed to the monad's operations, and there is no pile-up of binds - all uses of the underlying monad's bind are right-associated. It converts the computation to continuation-passing style. In particular free tree-like monads :cite:`voigtlanderAsymptoticImprovementComputations2008` and `MTL monad stacks <http://r6.ca/blog/20071028T162529Z.html>`__ are much cheaper when implemented via Codensity. As a contrary point, in the `case <https://www.mail-archive.com/haskell-cafe@haskell.org/msg66512.html>`__ of the Maybe monad an ADT version seemed to be faster than a Church encoding. Unfortunately hpaste is defunct so the code can't be analyzed further. It's not clear if the "CPS" version mentioned was actually Codensity. SPJ also suspects that deeply nested continuations will not optimize properly compared to the world tokens.

:cite:`meyerovichSocioPLTPrinciplesProgramming2012` mentions that generators and coroutines (one-shot continuations) have been preferred to multi-shot continuations, and if you read :cite:`elizarovKotlinCoroutinesDesign2021`, they say "The main reason for this is believed to be the
inherent complexity of the continuation-based code, and the difficulty of making it performant." But here we are simply implementing continuations as lambdas, so there is not really any more complexity added, and it seems safe to assume that an efficient lambda implementation (e.g. using optimal reduction) will also lead to efficient continuations, although perhaps it will need some tweaking.

Multi-prompt delimited continuations
------------------------------------

Multi-prompt delimited continuations are described in :cite:`dyvbigMonadicFrameworkDelimited2007` . These might appear more expressive than standard delimited continuations , but as the paper shows, multi-prompt continuations can be implemented as a monad and hence as a library to use with the standard continuations. So the simplicity of the standard continuations wins out. With the multi-prompt continuations you have to have a unique id supply and a stack. The unique id supply complicates multithreading, and the stack can overflow and requires care to handle tail recursion. Whereas standard continuations translate to pure lambdas, and tail recursion is dealt with by the host language's semantics.

Streams
-------

With the stream I/O model a program is of type ``[Response] -> [Request]``, where ``[]`` is the type constructor of destructively updateable lists. With an unsafe lazy read operation we can write an interpreter with constant overhead like so:

::

  RList a = Ref (Bottom | Nil | Cons a (Rlist a))

  c (prog : [Response] -> [Request])  =
    lst = ref Bottom : RList Response
    reqs = prog (unsafeLazyRead lst)
    loop reqs lst where
      loop [] _ = lst := Nil; return Done
      loop ((ReadRequest name) : reqs') lst =
        read name $ \contents ->
          tl = ref Bottom : RList Response
          lst := Cons (ReadResponse contents) tl
          loop reqs' tl

In a purely functional model, defining streams in terms of continuations requires linear space and quadratic time in terms of the number of requests issued. In particular, given ``prog [...xs,Bottom]) = [...as,newreq,Bottom]``, each request-response iteration has to evaluate ``head (drop (length as) (prog [...xs,newresp,bottom]))`` to get the new request, duplicating the evaluation of ``prog`` over the first ``xs`` elements. :cite:`hudakExpressivenessPurelyFunctional1989` Haskell 1.0 used streams as its I/O model due to this performance consideration. But given the destructive update implementation, I don't think this is an issue.

Per :cite:`hudakHistoryHaskellBeing2007`, continuations are easier to use than streams and preferred by most programmers. With continuations, responses are localized to each request, whereas streams require careful pattern-matching to ensure that requests and responses are matched up.

Free monad
----------

There are some definitions on Hackage of free monads:

.. code-block:: haskell

  -- free, control-monad-free, transformers-free
  data Free f a = Pure a | Free (f (Free f a))
  data FreeF f a b = Pure a | Free (f b)
  type FreeT = m (FreeF f a (FreeT f m a))

  -- indexed-free
  data IxFree f i j x where
      Pure :: a -> IxFree f i i a
      Free :: f i j (IxFree f j k a) -> IxFree f i k a

  -- free-operational
  type ProgramT instr m a = FreeT (Coyoneda instr) m a
  type Program instr = Free (Coyoneda instr) a

  -- operational
  data ProgramT instr m a where
    Lift :: m a -> ProgramT instr m a
    Bind :: ProgramT instr m b -> (b -> ProgramT instr m a) -> ProgramT instr m a
    Instr :: instr a -> ProgramT instr m a

  -- MonadPrompt, https://www.eyrie.org/%7Ezednenem/2013/06/prompt,
  -- https://www.reddit.com/r/haskell/comments/5a5frc/a_correct_free_monad_and_free_monad_fix/
  data Prompt p a = Done a | forall i. Prompt (p i) (i -> Prompt p a)
  type Prompt p a  =  forall b. (forall i. p i -> (i -> b) -> b) -> (a -> b) -> b

These are simple, but have drawbacks, per `Kmett <https://web.archive.org/web/20220124082435/http://comonad.com/reader/2011/free-monads-for-less/>`__. (>>=) used left-associatively has quadratic running time, as like (++) it must rescan the list of instructions with every bind. Every time you bind in a free monad, structure accumulates and this structure must be traversed past to deal with subsequent left-associated bind invocations. Free monads never shrink after a bind and the main body of the tree never changes.

Due to this, free monads are spine-strict - instructions must always be evaluated. Similarly MonadFix is not possible.

Yoneda
------

`Kmett <http://comonad.com/reader/2011/free-monads-for-less-2/>`__ says to use ``Yoneda (Rec f) a``, i.e. ``newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }``, instead of ``Codensity f a``. The claim is that this type is "smaller" than Codensity in the sense that the inhabitants of ``F`` are in a one-to-one correspondence with those of ``Free f a``. But what we are interested in is ``f a``; the recursive layering actually adds extra inhabitants as well, and there is also the ``Pure`` constructor that doesn't make much sense for I/O. For example ``F Identity ()`` is the type of Church numerals, ``(r -> r) -> (r -> r)`` while ``Codensity Identity () = forall r. r -> r = () = Identity ()``. So in this case it is actually ``F`` that is larger.

Just looking at the types, F has more arrows. Similarly compare the instances:

::

  -- F f
  return a = F (\kp _ -> kp a)
  F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)

  -- C f
  return x = C (\k -> k x)
  m >>= k = C (\c -> runC m (\a -> runC (k a) c))

The instance for ``C`` is fewer characters.

There is :cite:`rivasNotionsComputationMonoids2014` which derives the Codensity monad from the Yoneda lemma and the assumption that ``f`` is a small functor. Whereas the Yoneda-Rec seems to have no category theory behind it.

Generally it seems that Yoneda solves a different problem than an I/O monad.

Algebraic effects
-----------------

Codensity and algebraic effects are quite similar, both using a data type to represent operations. In fact the two are macro-expressively equivalent. :cite:`forsterExpressivePowerUserDefined2017` But Codensity doesn't require new syntax unlike the handler functionality. In the effect approach, computations are not first-class values.

OTOH effect types are quite useful, because you can define code that is polymorphic over the effect type, hence can be used as both pure and impure code. They use a monadic translation and then pure code is the identity monad. This can be shoehorned into continuations too by using a symbol marker with cases for pure and impure but maybe it is not as nice.

Call by push value
------------------

CBPV has "values" and "computations". The original presentation has these as separate categories, but :cite:`eggerEnrichedEffectCalculus2014` presents an alternative calculus EC+ where every computation is also a value. There is exactly one primitive that sequences computation, ``M to x. N``, which acts like the monadic bind ``M >>= \x -> N``, and similarly there is ``return``. And the evaluation is CBV. So stripping away the thunk stuff it seems to be a disguised version of monads. And the thunk stuff is a rather fragile way to implement CBN - it doesn't generalize to call by need. :cite:`mcdermottExtendedCallbyPushValueReasoning2019` And then there is jump-with-argument (JWA) which uses continuations and is equivalent to CBPV.

Applicative
-----------

All uses of Applicative can be rewritten using the laws to be of the form ``pure f <*> a <*> b ... <*> d`` (where ``<*>`` is left associative), hence all uses can be rewritten to the idiom bracket syntax. And the idiom bracket syntax ``([ f a b c ])`` can be replaced with variadic function syntax, ``apply_thing f a b c``. So variadic functions are sufficient.

Applicative can also be represented typeclass-free as functions using their Cayley representation and the Yoneda lemma, see :cite:`rivasNotionsComputationMonoids2014` and `this email <https://fa.haskell.narkive.com/hUgYjfKJ/haskell-cafe-the-mother-of-all-functors-monads-categories#post3>`__.

::

  Rep f v = forall a. f a -> f (b,a)
  Yoneda f a = forall b. (a -> b) -> f b
  Applicative f a = Rep (Yoneda f) a
  pure : a -> Applicative f a
  (<*>) : Applicative f (a -> b) -> Applicative f a -> Applicative f b

  lift : (pure : a -> f a) -> ((<*>) : forall b. f (a -> b) -> f a -> f b) -> f a -> Applicative f a
  lower : Applicative f a -> f a

So every function ``Applicative f => f a -> f b -> ...`` can be replaced with ``Applicative f a -> Applicative f b -> ...`` - the normalization enabled by Cayley and Yoneda means you don't have to worry about instance coherency.

Promises
--------

An example:

::

  function foo() {
    return f().then(v => { return g(v) })
  }

The ``then`` operation is basically monadic bind, so this is another form of monad syntax. There are `inconsistencies <https://buzzdecafe.github.io/2018/04/10/no-promises-are-not-monads>`__ with the Monad laws due to Promise flattening, which are enshrined in the spec and `unfixable <https://github.com/promises-aplus/promises-spec/issues/94>`__ without creating a wrapper API. But ignoring those, the Promise type is something like ``Promise err a = Fulfilled a | Rejected err | Pending ({ resolve : a -> IO (), reject : err -> IO ()} -> IO ())``, which focusing on ``Pending`` is a CPS monad ``(Either err a -> IO ()) -> IO () = EitherT err (Cont (IO ())) a``.

Some arguments against:

* Promises do not conform to functor or monad laws and thus are not safe for compositional refactoring.
* JS promises allow execution after the promise is resolved or rejected, resulting in untraceable behavior (fixed in C# by overriding return/throw instead of using resolve/reject)

Monad combined with identity monad
----------------------------------

With the lazy identity monad you can recover lazy pure code, as if there was no monad syntax. ``M m a = Either a (m a)`` is a monad (`SO implementation <https://stackoverflow.com/a/49703783>`__) so we can mix this in with other monads. For a dynamic language, we would like to split the universal type ``Any`` into actions and pure values, so that ``Any`` forms a monad and actions are just a special type of value that has more complex sequencing behavior. We calculate::

  Any = Either a (m a) = Either Pure Action
  Pure = a
  Action = m a = m Pure
  Pure = Any \ Action

``Int`` is not ``m _``, so it is pure. ``m Int`` is therefore an action. Therefore ``m (m Int)`` is not an action, because to be an action it would have to return a pure value. Hence ``m (m Int)`` is pure, a surprising conclusion. Similarly ``m (m (m Int))`` is an action. We can convert between these with ``join`` and ``return``. This weirdness somewhat explains why JS felt the need to collapse nested promises and break the monad laws - it avoids the need to unroll the promise chain to deduce whether a value is an action.

Async
-----

In JavaScript

::

  async function foo() {
    v = await f
    return g(v)
  }

Async/await notation requires marking core library calls with "await" and the whole call chain with "async", a tedious syntactic burden that Bob Nystrom calls `function coloring <http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/>`__\ .

It's better to make the async behavior automatic. Zig has done this but has `tons of bugs <https://gavinhoward.com/2022/04/i-believe-zig-has-function-colors/>`__\ . Monads in general and continuations in particular seem like a more principled approach, e.g. there is a `JS CPS library <https://github.com/dmitriz/cpsfy/blob/master/DOCUMENTATION.md>`__\ .

Futures
-------

According to `Erik Meijer <https://www.youtube.com/watch?v=QNpKYypLAO8>`__ (38:16), futures are kind of like comonads. A comonad has three operations: fmap, extract, and duplicate. Fmap make sense for a future, you can apply a function on the result. Duplicate is a little pointless but also possible, you can make a future that returns a future. There is the question of why not just the monadic ``return`` but it sort of makes sense, a future is delayed whereas a value is not. Finally you can ``extract`` the value from a future. This one is really pushing it though because the extract operation blocks, and can throw a deadlock exception, so it's not pure. We have to model extract more carefully as ``Future a -> M a`` for some monad. :cite:`uustaluComonadicNotionsComputation2008` called this kind of comonad-monad function a "BiKleisli category", i.e. the category ``BiKlesli Future M a b = Future a -> M b``. So rather than the comonad structure, we just have the identity and composition operations of the category, and arrow stuff. So really we aren't talking about comonads at all but rather arrows.

.. _tasks:

Tasks
-----

We can model I/O operations as members of a ``Task`` type, consisting of constructor terms plus callback(s) for what to do with the return value. Sequences of I/O operations are values of type ``Task``, similar to a `free monad <https://www.reddit.com/r/haskell/comments/swffy/why_do_we_not_define_io_as_a_free_monad/>`__. Statements that don't return are directly of the Task type, like ``Exit { code : Int}``. Statements that continue in a sequential fashion have a ``continuation`` argument, like ``Print { s : String, continuation : Task }``, so are of type ``Command = Task -> Task``. Statements that return a value use a continuation of type ``a -> Task``, e.g. ``ReadFile { path : Fd, continuation : String -> Task}``, so are of type ``Operation a = (a -> Task) -> Task``. And since tasks are values we can also use them as arguments, like the ``delayed_task`` in ``SetTimeout { delay : Int, delayed_task : Task, continuation : Task}``.

With this approach an I/O operation is data that can be pattern-matched over, allowing many metaprogramming techniques. It's a little harder for the compiler to optimize that readIORef has no observable side effects, as it's a reordering property (commutativity), but strict languages have been doing this for years.

To see how this works, consider the program ``print "Hi"``. As a task this is the value ``Print "Hi" (Exit 0)``, where ``Exit 0`` is what happens after printing (the continuation). The operation is ``print a = \cont -> Print a cont``. With the continuation as the last argument we can just use the partially-applied function, ``print = Print``. ``print a >> print b = \cont -> Print a (Print b cont)``. Now consider ``read ref >>= print``. The operation is ``Read ref >>= Print`` where ``>>=`` is the continuation monad's bind operation, which expands to ``\cont -> Read ref (\v -> Print v cont)``.

Actually print isn't a primitive operation, it's more like:

::

  Data "Hello, world!\n" (\msg ->
    Block "_start" [Sys_write stdout (addr msg) (length msg) (Sys_exit 0)])

with Stroscot's internal assembler language.

Task isn't really a monad, but we can compose operations that return values using the continuation monad's bind operation, as implemented with do-notation.

The datatype is similar to the "fudgets" mentioned in :cite:`erkokValueRecursionMonadic2002`, except we don't have a pure constructor. Or `this <http://comonad.com/reader/2011/free-monads-for-less-3/>`__ type ``FFI o i``, but with control flow represented explicitly instead of using ``o`` or ``i`` parameters.

World token
-----------

Haskell uses a state monad ``IO a = s -> (# s, a #))`` for implementing I/O, where ``s = World`` is a special zero-sized token type. Clean is similar but ``s = *World`` has the uniqueness type annotation so the tokens must be used linearly. Regardless, this approach is quite awkward:

* Programs like ``(a,_) = getChar s; (b,s') = getChar s; putChar (a,b) s'`` that reuse tokens are broken and have to be forbidden. Similarly programs like ``(a,s2) = getChar s; (b,s) = getChar s2`` that pass the token back also have to be forbidden.
* GHC requires many hacks to ensure that linearity holds during core-to-core transformations.
* Commands like ``exit 0`` have to be modeled as returning a world token, even though they don't return at all.
* It is not clear what the token represents: a thread? a core? a state? The semantics of an infinite program like ``x = write "x" >> x`` is tricky to specify - it is not really a function at all.
* An I/O operation is an abstract function which makes it quite difficult to inspect IO values or implement simulations of I/O such as `PureIO <https://hackage.haskell.org/package/pure-io-0.2.1/docs/PureIO.html>`__.

The one advantage it has (per SPJ) is speed - the world token is 0-bytes so has no calling convention overhead.

Logic programming
=================

To make a general-purpose relational programming language, we must find a method of embedding I/O that preserves the relational semantics. What I've come up with is to make programs produce a functional I/O term as output, so that the satisfying states contains bindings like ``main = readln (\x -> (print ("hello "++x) end)))``.

In general running a relational program may produce infinite satisfying states. Using the ``run`` function, the list of possible states of a term can be inspected, so it would limit expressiveness to disallow local nondeterminism. But nondeterminism in the I/O term is an error - there is no way to reconcile ``print "b"`` and ``print "c"``, because the user can only see one output. Arbitrarily choosing a state would just be confusing. So we require that the I/O be unique over all satisfying states. In standalone programs the state only contains the ``main`` term, so this means standalone programs must be deterministic overall and resolve to a single state. But ``run`` transforms a nondeterministic logic program to a deterministic stream of data, and spawning threads uses a fresh ``threadMain`` binding, so this shouldn't be too restrictive. Mercury `uses <https://www.mercurylang.org/information/doc-latest/mercury_trans_guide/IO.html>`__ the "unique world" state-passing model of I/O, and has a similar restriction that predicates that do I/O must be deterministic (may not fail or return multiple times).

Colored values
==============

Often mentioned during I/O discussions are Bob Nystrom's traits of `function coloring <http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/>`__\ . `tel on HN <https://news.ycombinator.com/item?id=8985436>`__ suggested using red = impure, and `Gavin <https://gavinhoward.com/2022/04/i-believe-zig-has-function-colors/#review-of-function-colors>`__ suggested replacing "call" with "use". Most of the traits are then about "impure functions", which Stroscot calls actions. Stroscot allows running actions in a pure environment using an I/O simulation. With these modifications the traits read:

1. Values include pure functions and actions.
2. The way you use a value depends on its type.
3. You can only use an action from within another action, or within an I/O simulator.
4. Actions are more painful to use (than pure functions).
5. Some core library members are actions.

The only trait here that might be disadvantageous is 4. Nystrom lists the following pain points for JS async actions:

* verbose to compose in expressions because of the callbacks / promise goop
* annoying hoops to use error-handling
* can’t be used with try/catch or a lot of other control flow statements.
* can't call a function that returns a future from synchronous code

C# async-await solves all but the first, but the await keyword is still painful. Nystrom says the real solution is "multiple independent callstacks that can be switched between." Stroscot goes further than switching and makes I/O callstacks first-class continuations. With continuations as the I/O abstraction, there is no distinction between sync and async, or rather it is all async. In particular all low-level operations are implemented in async style (taking callbacks), and combinators must be written using the callback/continuation model. But simple sequential code can be written in sync style and this interoperates seamlessly with the async code. Thus Stroscot's I/O continuation model solves the distinction pain Nystrom was complaining about.

There is still a pure/impure dichotomy though. Regardless of syntax, impurity cannot be hidden completely. Actions will always have some conceptual overhead compared to pure functions because they are sensitive to execution order. I don't know if this will make anyone "spit in your coffee and/or deposit some even less savory fluids in it" (Nystrom), but I/O is unfortunately awkward in a pure or mathematical world. A program that does no I/O must be an infinite loop (it cannot even exit, because that requires a syscall). :cite:`jonesTacklingAwkwardSquad2001` classifies I/O under the "awkward squad".

"Unsafe" I/O
============

Haskell has ``runST`` and ``unsafePerformIO`` that allow turning impure computation into pure computations. These can be implemented by throwing a resumable exception that's caught in a top-level handler that does the I/O. ``runST`` scrutinizes its computation for impure behavior such as printing or returning allocated references, while ``unsafePerformIO`` does not and exposes the internal evaluation order.

If one wants to understand the evaluation order or is dealing with commutative operations, these functions are quite useful, e.g. Debug.Trace.trace looks like a non-I/O function but actually outputs something on the console, and allocation can be done in any order.

The main things to avoid is global variables like ``var = unsafePerformIO (newIORef 1)`` pattern. Implicit parameters initialized in main compose much better. Similarly C's ``static`` variables inlined in functions should be forbidden. Although, optimal reduction should mean an unsafePerformIO is only evaluated once, hence reading a file or something should be fine.

Top-level I/O
-------------

In Python we can write a simple script like ``print "hello world"``. In Haskell we must have the boilerplate ``main =``, which is more verbose. We can address this by allowing modules to be actions that return the actual record. The main issue is we must have an instance of MonadFix in order to tie the recursive knot. But fortunately there are `several implementations <https://github.com/ekmett/kan-extensions/issues/64>`__ of MonadFix for continuations; the only question is which one is correct.

The other option is to restrict I/O outside of main, e.g. to only the main module, which means that e.g. mutable variables cannot exist between calls of a function. This seems too restrictive.



  * pure data, pure state, pure value - immutable data/state/value, cannot be modified and does not depend on any external factors
  * pure expression - Side-effect-free expression, evaluates to a value without any side effects. Also, deterministic expression, for an expression that has only one value. So instead of "impure expression" refer to an expression that has no value (unevaluatable expression) or multiple values (ambiguous expression) or executes side effects (imperative expression). Actually with the TRS formalism I use every expression is evaluatable so we don't worry about unevaluatable expressions.
  * pure programming language - a language that models the program as a mathematical function and enforces a clear distinction between immutable values and mutable or side-effectful expressions. Kind of a broad concept so doesn't need a term.


Stroscot is pure, because purity is great for equational reasoning. But it's also not pure, because pure programs can't do anything imperative. Confused yet? Obviously. The issue is that "pure" is an undefined term and different people mean it to use different things.

In Stroscot there are shared non-global states (stores), and a non-shared global state (I/O). But there is no shared global state, as this leads to initialization races and non-reentrant operations. With the store mechanism a programmer is free to declare some variables and assign some state, and the store will be passed around automatically. But the store only contains a map from variables to data and it is instead the I/O mechanism which interacts with the file system, the network, and other resources.



Limitations of purity
=====================

At present, destructive update is required to implement some algorithms efficiently. In particular consider some languages:

1. PURE: a "pure" CBV Lisp using a small set of primitive Lisp operations, ``ATOM EQ READ WRITE CONS CAR CDR`` assumed to be of constant cost, and "flow-chart style" control flow, assumed free
2. IMPURE: the Lisp extended with destructive mutation operators ``RPLACA RPLACD`` also of constant cost
3. HASK: a Haskell with lambdas, case, tuples, and lists
4. CYCLE: PURE but with an operation to construct cyclic data structures, CYCLE

It has been established that PURE ⊆ CYCLE ⊊ HASK ⊆ IMPURE as far as expressing efficient online programs:

* For the first relation, PURE programs can be run unmodified in CYCLE with equivalent reduction steps, showing inclusion. :cite:`ben-amramNotesPippengerComparison1996` says that it is an open problem to demonstrate an efficiency advantage of CYCLE over PURE.

* For the second relation, lazy languages allow cycles, hence showing inclusion. :cite:`ben-amramNotesPippengerComparison1996` says that :cite:`pippengerPureImpureLisp1997` shows that for a specific online problem "perm" any CYCLE solution will require at least O(n log n) time. The proof depends on the property of CYCLE that a cons cell can refer only to itself or previously-constructed values, which does not hold for LAZY as it allows naming future computations. :cite:`birdMoreHasteLess1997` demonstrate that HASK can solve "perm" in amortized O(n) time, via the use of lazy streams, hence HASK is strictly more efficient than CYCLE.

* For the third relation, the thunk mechanism of HASK can be emulated in IMPURE, showing inclusion. :cite:`ben-amramNotesPippengerComparison1996` theorizes that for IMPURE programs following a read-update-write structure, there is a correspondingly efficient HASK program. Since Haskell 1.0 programs use a lazy stream ``[Response] -> [Request]`` for I/O, this read-update-write model seems to encompass all programs, hence it seems likely that the two languages are of equal efficiency, although nobody has formally proved this (TODO).

The log(n) gap between CYCLE and HASK is calculated using the cost of updating a balanced binary tree. This is the cost of the predecessor problem in the `pointer machine <https://en.wikipedia.org/wiki/Pointer_machine>`__. In the more accurate RAM model the update cost is optimally O(log log m) time under some assumptions. (:cite:`strakaFunctionalDataStuctures2013`, chapter 5) Straka's implementation uses vEB trees which have a huge constant factor and space usage, but y-fast trees probably work too for a practical implementation.

Still though, a gap is a gap, so to get performance we must provide laziness or destructive update. And programming efficient amortized pure lazy data structures is quite complex, and not well-studied. It seems that any practical programming language will have to provide destructive update.

Automatic destructive update
============================

Although non-side-effectful programs do not have operators for destructive update, they can still express similar programs using a copying update operation that returns a new data structure with the relevant index modified. For `example <https://prog21.dadgum.com/41.html>`__ counting the frequency of byte values within a block of binary data:

::

  freq (b : Binary) = foldr (\arr x -> update x (+1) arr) (repeat 256 0) b

Maybe you are not familiar with ``scanr`` - in the core language, this would expand to a recursive function:

::

  freq (b : Binary) = go b (repeat 256 0)
    where
      go (x:rest) arr = go rest (update x (+1) arr)
      go [] arr = arr

This can be compared with a solution in Java - in Stroscot the function parameter's modifying  takes the place of the mutable array:

.. code-block:: Java

  int[] calculateFrequency(byte[] data) {
    int[] arry = new int[256];
    for(int i = 0; i < data.length; i++) {
      arry[data[i]]++;
    }
    return arry;
  }


The issue with the functional is that a naive implementation of "update" copies the entire array, using O(n) memory and time. :cite:`hudakAggregateUpdateProblem1985` shows that with a compiler analysis (hereafter called "automatic destructive update") a language can provide O(1) update-copy operations. The compiler searches through possible evaluation orders for an evaluation order that never accesses the old version of data after updating, and transforms such "single-threaded" programs to destructively update, giving the speedup. Programming with pure arrays in a "single-threaded" style is at least as expressive as imperative arrays - per Hudak, all the natural translations of imperative algorithms are single-threaded. Some of :cite:`okasakiPurelyFunctionalData1998`'s lazy data structures have a similar single-threaded use condition for amortized good performance, so the single-threaded condition seems reasonable. Also well-defined Ocaml programs that use side effects must be single-threaded, else there is a race condition.

Roc and Koka seem to be going down the automatic destructive update route via alias analysis and ref-counting optimizations. It seems like a great optimization and it does not seem too hard to allow marking array uses as single-threaded so the compiler warns if it has to copy.

Haskell avoided automatic destructive update because per SPJ it seemed too complicated, and instead relies on monads. Monadic style fixes an evaluation order, hence guarantees single threading because the old version is inaccessible. Monadic style is verbose, because simple function applications require the use of Applicative like ``liftA (+) 1 2``. It also is not very composable because ``runST`` is required to escape from the monad and the phantom state token type prevents mixing certain computations.

Clean has uniqueness types, which also enforce single threadedness. Uniqueness types disallow a simple example of implementing id in terms of const:

::

  id = const x x
  const x y = x

  a = array [1,2,3]
  b = id a
  b !! 10

Automatic destructive update may or may not work on this example depending on how smart the compiler is. But it definitely works on all uniqueness-typable programs, and is pretty much syntactically identical. So this is another case of tractable but incomplete vs difficult but complete - Stroscot aims for completeness.

Store
=====

We can formalize traditional imperative programming with mutable variables using the notion of a store. A store is a first-class value representing a subset of computer memory. It is basically a map from identifiers to values, a little "bundle of state", but it's more complicated than just a hash table to as to support implicit concurrency in expressions. In particular a store is a per-variable ordered list of reads and writes so that read-write and write-write conflicts may be detected.

A function that uses the store is a value of the State monad, ``s -> (s, a)``, where ``s = Store``. For example an assignment statement ``a := f b + g c`` translates to

::

  \s ->
    (b, s1a) = read s "b"
    (x, s1) = f b s1a
    (c, s2a) = read s "c"
    (y, s2) = g c s2a
    s' = merge s [s1,s2]
    a = x + y
    update s' "a" a

The store is passed into each function and returned as a result. The special ``merge`` operation combines concurrent stores and checks for conflicts by examining the list of operations - if there is a conflict, the variables involved are set to ``DataRace`` exceptions.

:cite:`warthWorldsControllingScope2011` has an asymmetric commit operation instead of a merge operation. This takes a parent and a child and propagates child writes to the parent. This is impure, still requiring a global state (the root). For example the behavior of this program seems really unintuitive:

::

  x = 1
  A = thisWorld.sprout()
  x = 2
  in A { print(x) }
  -- prints 2, not 1

IMO it is much more intuitive to have a "snapshot" model that merges immutable map-like structures, so that the program works like so:

::

  x = 1
  A = currentState
  x = 2
  with state=A { print(x) }
  -- prints 1

The "worlds" approach seems to have adopted the asymmetric model based on Javascript's property lookup semantics. Per :cite:`morrisettRefiningFirstclassStores1993` the rollback and undo examples they give can be implemented just as easily using the snapshot model.

The allowed "side effects" of stores are restricted to variable updates - I/O such as reading files and networking is not possible, because the program can't continue without external input, but it has already been given the full state of the store so there is no further place to insert this input. But per :cite:`johnsonStoresPartialContinuations1988` continuations and stores can coexist. The idea is that a continuation takes a result continuation and a store, operates on the store, then calls the result continuation with a final store and the result. So the type of a continuation returning ``a`` is ``(Store -> a -> r) -> Store -> r``. Written differently this is ``Store -> ((Store, a) -> r) -> r`` which is ``Store -> Cont r (Store, a)`` or ``StateT (Cont r) Store a``.

In fact monads and continuation-based IO can express mutable variable programming directly, e.g. Haskell has the ``readIORef`` primitive. So first-class stores aren't actually necessary. But per :cite:`johnsonStoresPartialContinuations1988` "the resulting increase in power of the environment appears to be well worth the cost" (in complexity and implementation overhead). The store has several advantages:

* The store is a first-class value similar to a dictionary, whereas a continuation is similar to a linked list. Thus variable values can be accessed in O(1) time from a store value, whereas a continuation value must be stepped through sequentially (simulated) to extract values, requiring O(n) time. Essentially, the store formalizes program data state, while continuations formalize program control state. Per :cite:`morrisettRefiningFirstclassStores1993`, continuations do not capture the state of mutable objects. For example, ``callCC (\c -> c; c); modify "a" (+1)`` increments by 2, rather than setting ``a`` twice.

* It is much simpler semantically to use a store for implicitly concurrent computations. In the above example, where ``f b`` and ``g c`` run in parallel, if we used a monad we would have to sequence the operations ``x <- f b; y <- g c`` or use explicit ``fork``/ ``merge`` operations ``x_t <- fork (f b); y_t <- fork (g c); x <- wait x_t; y <- wait y_t``. In either case, the operations are not fully commutative: in the first we cannot swap the order of ``x`` and ``y`` if there is a data race, and in the second we cannot move a wait before a fork. In contrast the store's ``merge`` operation is fully commutative because the result of a data race is well-defined to be an exception. The ``wait/fork`` machinery is not needed as it is simply passing around a value. Essentially stores provide a transactional view of memory.

* First-class stores allow manipulating the program data state in complex ways. Multiple stores may exist simultaneously, allowing isolated computations. In particular, the empty store value allows turning a stateful function into a pure function, without any type trickery like ``runST``. More generally the ability to apply a function to different explicitly-written store values allows program testing and debugging.

The efficient implementation of stores is somewhat of a research area. Automatic destructive update should allow linear or non-conflicting usage of the store to translate to direct memory reads and writes. With non-linear usage, efficiently making copies and allowing persistent access to old stores may require some cleverness, for example a persistent hash map backed by the persistent array found in :cite:`strakaFunctionalDataStructures2013`. Per :cite:`johnsonFirstclassStoresPartial1994` the cost of a stack-based implementation is about 10% overhead on an ancient machine. Per :cite:`johnsonStoresPartialContinuations1988` this can be reduced through caching optimizations, so that if a variable is looked up and we know it has not been written then it does not need to be looked up again, i.e. ``lookup x (update s y yv) | x != y = lookup s x``. We also want to coalesce updates and reads so that new versions do not have to be created all the time.

Store state
-----------

Most papers limit themselves to keeping the values of mutable variables in the store. But conceptually the state of a program could include the state of the computer, the stock market, quantum fluctuations, etc. - all information within the chronological past of a program. But practically we are limited to state that we can read and write deterministically. In particular the read operation must satisfy the associative array definition:

::

    read k (write j v D) = if k == j then v else read k D
    read k emptyStore = MissingValue

So one constraint to be a variable is that the state must be accessible. So for example the kernel limits us - we do not have full control over peripheral devices or processes not related to ours. We can represent this by shadowing access-controlled variables and returning ``WriteFailed`` for inaccessible variables.

Conveniently the CRIU project has a `list <https://criu.org/Images>`__ of what's in the state of a Linux user process. We reproduce it here:

* Core process info

  * name, sigmask, itimers, etc.
  * Task credentials: uids, gids, caps, etc.
  * Process tree linkage
  * arch-dependent information (registers, etc.)
  * Signal handling map
  * IDs of objects (mm, files, sihand, etc.) and namespaces

* Address space information (VMAs, segments, exe file, etc.)

  * Info about which virtual regions are populated with data (pagemap)
  * 4k page data dumps for each mapped page in the pagemap.

* Filesystem info

  * chroot and chdir information
  * Open file descriptors
  * Paths to files opened with open(2) syscall
  * File paths remaps (e.g. for invisible files)
  * Ghost invisible files
  * Mountpoints information
  *	Contents of a tmpfs filesystem

* Special fd's / sockets

  * Eventfd file information
  * Eventpoll file information
  * Target file descriptors of eventpoll fds
  * Inotify file information
  * Watch descriptors of inotify fds
  * signalfd info
  * Pipes information
  * Contents of pipes (data sitting in a pipe)
  * FIFO information
  * Contents of FIFOs
  * Unix sockets
  * PF_INET sockets, both IPv4 and IPv6
  * Contents of socket queues
  * Interval timers state
  * TCP connection state (including data in queues)
  * Uname nodename and domainname of a UTS namespace
  * Information about opened TTYs, including Termios and similar stuff
  * Info about PF_PACKET sockets
  * Info about network devices
  * IP addresses on network devices
  * Routing tables

Usually these are modeled using primitive operations, e.g. file descriptors are allocated with the open syscall rather than declaratively as ``{ fd1 = inode 1234 }``. But the more state we model as state, the more powerful our debugging tools get. A traditional debugger has no way to undo closing a file. However, a filestate-aware debugger can reopen the file. The less we view the program as an I/O machine the easier it is to use high-bandwidth interfaces such as io_uring to perform bulk state changes - describing what rather than how is the hallmark of a high-level language. Of course, in most cases the program will use state in a single-threaded manner and it will simply be compiled to the primitive operation API by the automatic destructive update optimization.

