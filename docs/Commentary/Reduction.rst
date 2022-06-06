Reduction
#########

Linear logic
============

Linear logic has boxes, The difference is not observable if we do not use duplication. e.g. ``(\x.print(x+1)) {print("x"); 2}`` can only print ``x3``. But if we change ``x+1`` to ``x+x`` then CBV/optimal/lazy is ``x4`` while CBN is ``xx4``.

So how do we specify the difference between the two, in linear logic?

::

  s x =
    (y,z) = dup x
    print(y+z)
  s (print("x"); 2)

Boxes do have some performance cost, so how can they be avoided? There are cases where boxes are not necessary:

1. When the term is linear or affine and does not need to duplicate anything.
2. When the duplication is duplication of a graph without any cuts, such as a boolean, integer, list of integers, etc. Even when there are cuts, the value can be forced and then copied directly, using a fold. (per :cite:`filinskiLinearContinuations1992`) Q: Does this change the evaluation semantics to be stricter?
3. Inlining, when the duplication is carried out, resulting in two terms.
4. More complex cases enforced by a typing system, such as Elementary Affine Logic.

Implementation
==============

Reduction is fairly simple to implement without duplication, as it is just pairs of constructors and destructors annihilating and joining their wires, or, for ``case``, joining some eraser nodes. But what about duplication?

Stroscot takes its general inspiration from the delimiter system found in Lambdascope. However, instead of having levels Stroscot keeps explicit track of "environments" or "scopes". In particular a delimiter has an inside scope and an outside scope. Initially, all delimiters look like opening/closing delimiters where the outside scope is the default/root scope ``0`` and the inside scope is the scope of the multiplexer involved. When two delimiters meet, the touching outer scopes are compared for equality (they should always be equal) and one inner scope remains the inner scope while the other inner scope become the new delimiter's outer scope.

To determine which scope becomes the outer scope, delimiters are also marked as "head", "full", or "empty" depending on whether they represent a reference to the result of a duplication, the target of a duplication, or a path that crosses the scope but doesn't duplicate. Interactions are allowed only between head delimiters and other delimiter; the head delimiter's scope stays on the inside.

For multiplexers the situation is a little more complicated. A multiplexer also has two scopes, an inner "label"/identity-like scope and an outer "ambient" scope. When a multiplexer crosses a delimiter, from outside to inside, its "ambient" scope is changed to the delimiter's inside scope. Meanwhile the delimiter's scope is split into a new set of scopes, and this is indexed by the label scope. In the Stroscot code these are referred to as "variant" scopes. In particular, multiplexers with the same label scope must split other scopes into the same set of variant scopes at each interaction. This is not too hard to keep track of, just give each scope a map ``other scope -> variant scope set`` that's lazily created.

For efficient graph reduction we want to reduce a term completely, if we are able to. The top-level instruction evaluation loop can be written strictly, using a code pointer for conditional nodes.

stack machines: waste of time. the stack manipulation takes up more resources than register allocation.

The implementation of overloading is similar to that used for checking equality of dependent types, i.e. it does a lot of normalization but isn't omniscient. The optimizer decides which case is dead code and will be dropped.
The ``lub`` operation is implemented as a primitive that desugars in the middle of compilation, in a deterministic manner based on termination checking.

Semantics
=========

The dispatch semantics is that all methods are run in parallel using the `lub operation <http://conal.net/blog/posts/merging-partial-values>`__. Predicate failure, failed assertions, exceptions, and nontermination are all treated as bottom. Furthermore ``lub`` is an oracle, analyzing the whole program - we want return type overloading, return values that are not accepted by the surrounding context are discarded.

This falls out naturally from doing the analysis on the CPS-transformed version of the program.

* Dead code elimination
* Recursion
  * induction variable analysis to replace multiplication by a loop index with addition
  * loop reversal - changing condition to zero comparison
  * loop unswitching - moving conditional outside loop
  * hoisting invariants, partial/total redundancy elimination
  * parallelization - multi-threaded or vectorized code

Return type overloading
=======================

In Haskell, typeclasses can cause ambiguity errors. For example ``show (read "1")`` gives "Ambiguous type variable ‘a0’ arising from ‘show’ and ‘read’ prevents the constraint ‘(Show a0, Read a0)’ from being solved." The ambiguity can be further attributed to ``read``. The function ``show :: Show a => a -> String`` takes a value of type ``a``, so dynamic dispatch can deduce the type ``a`` and there is no ambiguity. In contrast ``read "1"`` produces a type out of nowhere and could be of type ``Integer`` or ``Double``. Since ``read`` has a constraint ``Read a`` and does not take a value of type ``a`` as argument it is said to be return type overloaded (RTOed).

RTO, specifically overloaded constants such as ``zero :: Num a => a``, was mentioned in the original typeclasses paper as a desired feature. Haskell has several ways to resolve an RTOed expression:

* specify an inline signature ``read x :: Float``
* use type application ``read @Float``
* infer the type by applying a function expression or case statement of fixed argument type, ``let f = id :: Int -> Int in show (f $ read "1")`` or ``show (case read "1" of 1 -> 1)``
* a default declaration, for the Num, Show, Eq, Ord, Foldable and Traversable classes (all but Num require -XExtendedDefaultRules)

Rust traits are similar, the turbofish specifies the type explicitly, like ``iterator.collect::<Vec<i32>>``, and the type inference for defaulting is local rather than global. Ada similarly can disambiguate by return type if the return type is known.

Defaulting is considered by `Haskell Prime Proposal 4 <https://web.archive.org/web/20200107071106/https://prime.haskell.org/wiki/Defaulting>`__ to be a wart of the language. Its specific usage is numeric types for interactive calculator style things, where it can be replaced with using computational reals by default for literals. ExtendedDefaultRules is simply a hack for Curry-style type system oddities in GHCi - since the involved classes have no RTOed functions, it is unnecessary in an untyped setting. For instance, ``show []`` is unambiguous, despite having a polymorphic Haskell type of ``forall a. [a]`` and no ``Show`` instance because GHC does not allow polymorphic type class instances. GHCi defaulting to ``[Void]`` instead of ``[()]`` would make this clear, but ``Void`` was only recently added to the base library so GHCi uses ``()``.

Inline signatures and type application can be replaced in a dynamic language by passing the type explicitly as a parameter, ``read Double`` or ``read Float``, using normal overloading. Sometimes the type itself can be the function, ``Vector i32 iterator`` or ``Vector iterator`` instead of ``collect (Vec i32) iterator``. This actually standardizes and simplifies the observed syntax.

Function expression and case inference can be mimicked similar to `this C++ approach <https://artificial-mind.net/blog/2020/10/10/return-type-overloading>`__. We create a "blob" type that represents an RTO value of unknown type. Then we overload operations on the blob to return blobs, delaying resolution until an applied function or case makes it clear what a reasonable value would be. Furthermore the blob can store its type in a mutable reference and use ``unsafePerformIO`` to ensure that it resolves to the same type if it is used multiple times. Or it can be safe and evaluate at multiple types. This requires overloading every function that uses the blob, so can be some boilerplate.

For nullary values like ``top`` of a lattice or a ``default`` value, you don't need the blob machinery, you can just use a symbol. Then implement symbol conversion ``convert top Float = float Infinity`` and Julia's promotion machinery will take care of the rest for numeric operations, or add manual overloading and conversion otherwise. You could take this approach with ``read`` as well, so that ``read "x"`` is a term of type ``Read`` and you overload the function , but it might be more work than the blob approach.

List of class methods in GHC's libraries which are RTOed:

::

toEnum :: Enum a => Int -> a
fromInteger :: Num a => Integer -> a
fromRational :: Fractional a => Rational -> a
encodeFloat :: RealFloat a => Integer -> Int -> a
minBound :: Bounded a => a
maxBound :: Bounded a => a
mempty :: Monoid a => a
fromExportableBuffer   :: Exportable c => ExportableCharacterSequence -> c
outputCap :: OutputCap f => ([Int] -> String) -> [Int] -> f
indexByteArray# :: Prim a => ByteArray# -> Int# -> a
def :: Default a => a

unexpected :: (Parsing m) => String -> m a
eof :: (Parsing m) => m ()
getLine :: Interactive m => m String
getCurrentYear :: Interactive m => m Integer
pure :: Applicative f => a -> f a
fail :: MonadFail m => String -> m a
throwM :: (MonadThrow m, Exception e) => e -> m a
ask :: MonadReader r m => m r
parsec :: (Parsec a, CabalParsing m) => m a
qNewName :: Quasi m => String -> m Name

get :: Binary t => Get t
readsPrec :: (Read a) => Int -> ReadS a
buildInfo :: HasBuildInfo a => Lens' a BuildInfo
garbitrary :: GArbitrary f => Gen (f ())
iodataMode :: KnownIODataMode mode => IODataMode mode
hGetIODataContents :: KnownIODataMode mode => System.IO.Handle -> IO mode

unsafeArray :: (IArray a e, Ix i) => (i,i) -> [(Int, e)] -> a i e
unsafeAccumArray :: (IArray a e, Ix i) => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> a i e
newArray:: (MArray a e m , Ix i) => (i,i) -> e -> m (a i e)
basicUnsafeNew   :: PrimMonad m, MVector v a => Int -> m (v (PrimState m) a)

uniqueFieldAla :: (c b, Newtype a b, FieldGrammar c g) => FieldName -> (a -> b) -> ALens' s a -> g s a
tabulate :: Representable i f => (i -> a) -> f a
unmodel :: TestData a => Model a -> a -- Model is a type synonym family of TestData


mimicking RTO with simple overloading
implement a DSL source transformation that infers the types using HM and adds explicit types

