Dispatch
#########

Priorities
==========

Priority is novel AFAIK but powerful, and generalizes the CLOS dispatch system. I thought about making all clauses the same priority by default, but concluded that specificity was likely going to be confusing hence is better off opt-in. Specificity is found in predicate dispatch and Zarf's rule based programming, but both are far off from practical languages.

CLOS method dispatch
--------------------

CLOS has method qualifiers ``before``, ``after``, and ``around``. The basic idea of method combination can be seen here:

.. image:: /_static/Method-combination.png

But it turns out these can simply be implemented with priorities ``around_clause > before_clause > after_clause > prioHigh`` and next-method. ``before`` and ``after``:

::

   before f = around { f; next-method }
   after f = around { next-method; f }

Guards are handled by calling next-method on failure:

::

  (around f | c = d)  = if c then d else next-method

Overloading
===========

`The Go FAQ <https://go.dev/doc/faq#overloading>`__ says Go deliberately does not support overloading, but then it admits overloading is "occasionally useful". Actually, Go does support a limited form of overloading, in the form of interfaces - you can write ``a.area()`` and ``b.area()`` and these refer to different methods. But it's limited to dispatching on the self parameter's type.

The Go FAQ says overloading is confusing and fragile. Life is confusing and fragile, but this does not mean eliminating all life is the solution.

The Go FAQ says naming separate functions leads to a clearer API. While separate functions can be clearer, supporting overloading does not prevent creating separate functions. And suppose you are trying to wrap a Java library that makes heavy use of overloading. Name mangling using simple rules will give relatively long names like ``divide_int_int``. More complicated rules will give shorter name like ``div_ii``, but the names will be hard to remember. Either way, overloading means no mangling is needed at all, a strictly better alternative.

The Go FAQ says leaving overloading out simplifies the type system and method dispatch. Overloading doesn't complicate a dynamic type system at all. It does complicate method dispatch, but in exchange overloading enables generic functions and solves the expression problem, so it's worth it.

So the verdict here is that the Go developers were just too lazy to implement overloading.

Default arguments
=================

Default arguments are very similar in expressiveness to overloading, so as one might expect `Rob Pike <https://talks.golang.org/2012/splash.article>`_ says Go deliberately does not support default arguments. Supposedly adding default arguments to a function results in interactions among arguments that are difficult to understand.

Pike admits that it is really easy to patch API design flaws by adding a default argument. Expanding on this, it seems default arguments have a good place in the lifecycle of an API parameter:

1. A new parameter can be added to a function, just give it a default value such that the function behavior is unchanged.
2. The whole argument can be deprecated and removed on a major release, hard-coding the default value
3. The default value can be deprecated and then removed on a major release, forcing the value to be specified.
4. An existing parameter can be given a default value without breaking compatibility

.. graphviz::

  digraph {
  rankdir=LR
  Missing -> Defaulted [label="1"]
  Defaulted -> Missing [label="2", color="blue"]
  Defaulted -> Present [label="3", color="blue"]
  Present -> Defaulted [label="4"]
  }

One side effect of this lifecycle is it gravitates towards default parameters, because those don't break compatibility. So programs accumulate many default parameters that should be made into required parameters or removed. This is probably why developers say default parameters are a code smell (nonwithstanding the internet's main opinion, which comes from C#'s implementation breaking ABI compatibility in a way that can be fixed by using regular overloading). But regular pruning should be possible, just do occasional surveys as to remove/make mandatory/leave alone. And, without default parameters, adding or removing a parameter immediately breaks the API without a deprecation window, so it is effectively impossible - you have to make a new method name. This is why Linux has the unimaginatively named syscalls ``dup``, ``dup2`` and ``dup3``. IMO having deprecated parameters is better than trying to come up with a new name or having version numbers in names.

A `wartremover issue <https://github.com/wartremover/wartremover/issues/116>`__ provides 4 potentially problematic cases for default arguments. Going through them:

* Automatically allocated resource arg - this is deallocated by the finalizer system in Stroscot, hence no resource management problem. To avoid statefulness, the pattern is to default to a special value like ``AutoAllocate`` and do the allocation in the method body.
* Config - a big win convenience-wise for default arguments, only specify the parameters you care about.
* Delegated parameter: this is nicely handled in Stroscot by implicit parameters. Hence a pattern like ``ingest {compressor = None} = ...; doIngestion = { log; ingest }; executeIngest = { prepare; doIngestion }; parseAndExec cmdline = { compressor = parse cmdline; executeIngest }`` is possible. Once you realize the compressor is important you can remove the default and make it a keyword parameter - the default can still be provided as a definition in an importable module, and you get an unset argument error if no implicit parameter is set.
* Faux overloading like ``foo(i, name = "s")`` - useful just like overloading.

The verdict here is that defaulting is a power vs predictability tradeoff. The obvious choice given Stroscot's principles is power. There are no easy solutions for adding new behavior to a function besides adding a default argument flag to modifying an API in a backwards-compatible way, so default arguments are necessary. And default arguments have been used in creative ways to make fluent interfaces, giving programmers the enjoyment of complex interface design. The added implementation complexity is small. Security concerns are on the level of misreading the API docs, which is possible in any case. Adding a warning that a default argument has not been specified, forcing supplying all parameters to every method, seems sufficient.

Return type overloading
=======================

In Haskell, typeclasses can cause ambiguity errors. For example ``show (read "1")`` gives "Ambiguous type variable ‘a0’ arising from ‘show’ and ‘read’ prevents the constraint ‘(Show a0, Read a0)’ from being solved."

Following :cite:`oderskySecondLookOverloading1995` the ambiguity can be further attributed to ``read``. The function ``show :: Show a => a -> String`` takes a value of type ``a``, so dynamic dispatch can deduce the type ``a`` and there is no ambiguity. In contrast ``read "1"`` produces a type out of nowhere and could be of type ``Integer`` or ``Double``. Since ``read`` has a constraint ``Read a`` and does not take a value of type ``a`` as argument it is said to be return type overloaded (RTOed).

A brief categorization of some RTO functions in GHC's base libraries:

* Conversion functions, functions that extract a value: ``toEnum :: Enum a => Int -> a``, ``fromInteger :: Num a => Integer -> a``, ``fromRational :: Fractional a => Rational -> a``, ``encodeFloat :: RealFloat a => Integer -> Int -> a``, ``indexByteArray# :: Prim a => ByteArray# -> Int# -> a``
* Overloaded constants: ``maxBound :: Bounded a => a``, ``mempty :: Monoid a => a``, ``def :: Default a => a``
* Monadically-overloaded operations: ``pure :: Applicative f => a -> f a``, ``getLine :: Interactive m => m String``, ``fail :: MonadFail m => String -> m a``, ``ask :: MonadReader r m => m r`` ``parsec :: (Parsec a, CabalParsing m) => m a``
* Type-indexed constant: ``get :: Binary t => Get t``, ``readsPrec :: (Read a) => Int -> ReadS a``, ``buildInfo :: HasBuildInfo a => Lens' a BuildInfo``, ``garbitrary :: GArbitrary f => Gen (f ())``
* GADT faffing: ``iodataMode :: KnownIODataMode mode => IODataMode mode``, ``hGetIODataContents :: KnownIODataMode mode => System.IO.Handle -> IO mode``
* Creating arrays of type: ``newArray :: (MArray a e m , Ix i) => (i,i) -> e -> m (a i e)``, ``basicUnsafeNew :: PrimMonad m, MVector v a => Int -> m (v (PrimState m) a)``
* Representable functors: ``tabulate :: Representable f => (Rep f -> a) -> f a``, ``unmodel :: TestData a => Model a -> a``, where ``Rep`` and ``Model`` are type synonym families of their respective classes

GHC has several ways to resolve an RTOed expression:

Defaulting
----------

Defaulting is considered by `Haskell Prime Proposal 4 <https://web.archive.org/web/20200107071106/https://prime.haskell.org/wiki/Defaulting>`__ to be a wart of the language. `clinton84 <https://www.reddit.com/r/haskell/comments/mprk2e/generalized_named_and_exportable_default/gubpfbn/>`__ want a switch NoDefaulting to remove it entirely. But GHC plans to move in the opposite direction, expanding its use by allowing more and more classes to be defaulted, and recently allowing defaulting rules to be exported. `ref <https://ghc-proposals.readthedocs.io/en/latest/proposals/0409-exportable-named-default.html>`__

In Haskell 98 defaulting is limited to numeric types, where it allows numerical calculations such as ``1 ^ 2`` - ``^`` is generic in the 2 so must be defaulted. This usage can be replaced with using a single arbitrary-precision type for all literals that can accurately hold both ``Integer`` and ``Double``, and then Julia's conversion/promotion mechanism in operations.

With -XOverloadedStrings every string literal is wrapped in a call to ``fromString : IsString s => String -> s``. The usage is that Haskell has several text types, such as ``ByteString`` and ``Text``, and also some people define newtypes over them. The defaulting to ``IsString String`` seems to mainly be added for compatibility with existing source code. Probably Julia's conversion/promotion mechanism is sufficient for this as well. The corresponding ``IsList`` class for -XOverloadedLists has no defaulting rules, and nobody is complaining.

ExtendedDefaultRules for the Show, Eq, Ord, Foldable and Traversable classes is simply a hack for Curry-style type system oddities in GHCi - since the involved classes have no RTOed functions, it is unnecessary in an untyped setting. For instance, ``show []`` is unambiguous in a dynamically typed language - it matches a rule ``show [] = ...``. In Haskell it has a polymorphic type ``forall a. [a]`` and no principal ``Show`` instance because GHC does not allow polymorphic type class instances. GHCi defaulting to ``[Void]`` instead of ``[()]`` would make this clear, but ``Void`` was only recently added to the base library so GHCi uses ``()``.

GHC also supports defaulting plugins, `supposedly <https://github.com/hasktorch/hasktorch/issues/514>`__ to specify default device types and numeric types for tensors in haskell-torch. The defaulting can likely be solved by using default or implicit parameters. And Haskell-torch is a port of PyTorch so everything can be solved by using dynamic types. AFAICT there are no working defaulting plugins currently available.

Signatures
----------

The most direct way to resolve RTO in Haskell is to specify the type. There is an inline signature ``read x :: Float``, defining a helper ``readFloat :: String -> Float; readFloat = read``, or using type application ``read @Float``. Rust traits are similar, the turbofish specifies the type explicitly, like ``iterator.collect::<Vec<i32>>``, and the type inference for defaulting is local rather than global. Ada similarly can disambiguate by return type because the type of the LHS is specified.

Inline signatures and type application can be replaced in a dynamic language by passing the type explicitly as a parameter, ``read Double`` or ``read Float``, using normal overloading. Sometimes it can be shortened by making the type itself the function, ``Vec i32 iterator`` or ``Vec iterator`` instead of ``collect (Vec i32) iterator``. Either way, the resulting syntax is uniform, and more standard and simple than the observed Haskell / Rust syntax.

Multiple parameters can be handled in the obvious way, ``foo A B``, or we can pack them in a term ``foo (term A B)``.

Return types signatures
~~~~~~~~~~~~~~~~~~~~~~~

Julia essentially uses the same syntax I'm planning, ``zeros(Float64,0)``, with strict matching on the type ``Float64``. Contrariwise Martin Holters, a professor in Germany researching audio processing (i.e. not a language designer), filed a `Julia issue <https://github.com/JuliaLang/julia/issues/19206>`__ to introduce more complex syntax ``foo(x,y,z)::Type`` that specifies the return type. The issue generated no substantial discussion for 5 years so could be ignored, but let's go through it.

Martin says a dedicated syntax would be clearer than the "return type as first argument" convention because the type passed is used inconsistently. He gives a list of function calls using ``Float64``:

* ``map(Float64, 1) = 1.0``: this applies ``Float64`` to 1. IMO this should error because ``1 : Int`` is not a collection type.
* ``map(Float64, (1, 2)) = (1.0,2.0)``: good, so long as the overloading of types as conversions is remembered. It would be clearer to write ``map (convert Float64) (1, 2) = (1.0,2.0)``
* ``rand(Float64) = 0.16908130360440443``: ``Float64`` is the return type, good.
* ``rand(Float64, 1) = Vector{Float64} [0.1455494388391413]``: returning a vector is a bit inconsistent with the previous. It would be better to have a separate function ``randvec`` that takes a varargs list of dimensions, so ``randvec(Float64)`` would return a 0-dimensional array. With this using interpreting the parameter as the element type is fine.
* ``zeros(Float64) = Array{Float64, 0} [0.0]``: This always returns an array and interprets as the element type, like the proposed ``randvec``. Good.
* ``zeros(Float64, 1) = Vector{Float64} [0.0]``; Writing the full return type like ``zeros(1)::Vector{Float64}`` is verbose, and you would inconsistently write ``zeros(1,1)::Matrix{Float64}`` or ``zeros(1,1)::Array{Float64, 2}`` for a 2D array, compared to ``fill`` which has no types involved in calling it and is length invariant.

So the inconsistencies he points out are due to standard library oddities, rather than the syntax, and in the practical cases of ``zeros`` / ``randvec`` Martin's syntax would be worse IMO.

Martin also says ``foo()::T`` should invoke the method ``foo()::S`` such that ``S`` is the largest type with ``S<:T``. It's not clear why - he just says it "seems logical" but admits it doesn't "translate into any real benefits". Practically, one has to write the return type out, and writing the exact type used in the dispatch clause is simpler than picking out a supertype. For a trivial example, writing ``default {None}`` instead of ``default {None,1,2}`` makes it clearer that ``None`` will be used as the default. Furthermore it avoids conflicts like for ``default (Int|{None})`` if there were two rules ``default Int = 0; default {None} = None``

Inference
---------

The case where dynamic typing falls down is when the type is not specified directly on the function but rather is inferred. For example ``(fromInteger 1 + fromInteger 2) :: Int``, the type is pushed down so that ``fromInteger 1 :: Int``. If we went with giving the type as a parameter we would have to write ``fromInteger Int 1 + fromInteger Int 2``, duplicating the type. With a function expression of fixed type ``let f = id :: Int -> Int in show (f $ read "1")`` or case statement ``show (case read "1" of (1 :: Int) -> "x")`` the type is far removed from the ambiguous ``read``.

It is arguable whether inference is desirable. The programmer has to perform the same type inference in their head to follow the path the compiler is taken, which can make code tricky to understand. The meaning of an expression is context-dependent. But the original typeclasses paper :cite:`wadlerHowMakeAdhoc1989` mentions resolving overloaded constants based on the context as a feature. So this section discusses possible ways of implementing the context resolution.

One approach is similar to `this C++ thing <https://artificial-mind.net/blog/2020/10/10/return-type-overloading>`__. We create a "blob" type that represents an RTO value of unknown type as a function from type to value. Then we overload operations on the blob to return blobs, delaying resolution until the full type can be inferred. Furthermore the blob can store its type in a mutable reference and use ``unsafePerformIO`` to ensure that it resolves to the same type if it is used multiple times. Or it can be safe and evaluate at multiple types. This requires overloading every function to support the blob, so can be some boilerplate.

A little simpler approach is to use a term, so e.g. ``read "x"`` is a normal form. Then you overload functions to deal with these terms. This works well for nullary symbols like ``maxBound`` - you implement conversion ``convert maxBound Float = float Infinity`` and promotion will take care of the rest. The issue is that an expression like ``read "1" + read "2"`` will not resolve the return type, and returning ``read "3"`` is inefficient. I think the best solution is to return a blob. So this ends up being the blob solution but with readable literals for the first level of return value.

Another approach is to add type inference, but as a macro transformation. All it has to do is infer the types using Hindley Milner or similar and insert explicit type parameters for the RTOed functions. But this is really the opposite of what Stroscot aims for. Stroscot avoided static typing to begin with because there are no principal types if you have union types. For example a value of type ``A`` may take on the type ``(A|B)`` or ``(A|C)`` depending on context. The inserted types would have to be principal, negating the advantage of dynamic typing. Furthermore you'd have to use more macros to specify type instances and types of functions.

My original idea is to use nondeterminism: an overloaded function ``f : Something a => a`` is interpreted as ``(f A) amb (f B) amb ...``, combining all the typeclass instances. Disambiguating types using annotations is then replaced with disambiguating the result using assertion failures. This actually preserves the semantics of the static typeclass resolution AFAICT. In the discussion `on Reddit <https://www.reddit.com/r/ProgrammingLanguages/comments/uynw2i/return_type_overloading_for_dynamic_languages/>`__, it was brought up that if there are a lot of overloadings or the overloadings are recursive, it would be slow, exponential in the number of function calls. That's for a quite naive implementation; I think the compiler could do a type analysis and give good performance for most cases. HM also has exponential blowup on pathological cases. Overall I think this approach is the best, but it is not clear if it is actually helpful because features like automatic promotion will make many programs ambiguous.

Implementation
==============

The full dispatch mechanism is as follows:

::

   dispatch clauses args = do
     warnIfAnyPrioEqual clauses
     starting_clauses = find_no_predecessors clauses
     callParallel starting_clauses
      where
       callParallel clauses = lubAll (map call clauses)
       call clause = clause args { next-method = callParallel (find_covering methods clause) }
       lubAll = fold lub DispatchError

This depends on the ``lub`` primitive defined in `Conal's post <http://conal.net/blog/posts/merging-partial-values>`__. Since strict evaluation of ``(1,undefined)`` gives ``undefined``, ``lub`` instead works on expressions. Expressions with little information content are in the set Bottom or ⊥, such as predicate failure, failed assertions, exceptions, and nontermination. Values are normal forms excluding bottoms, ``NF \ Bottom``. Aggregate expressions can be partially evaluated and hence neither bottoms nor values, such as the aforementioned ``(1,undefined)``.


One of the clever ideas of the theory of semantic domains (“domain theory”) is to place a partial ordering on values (domain members), based on information content. Values can not only be equal and unequal, they can also have more or less information content than each other.

Structured types are not flat. For instance, the meaning of (i.e., the domain corresponding to) the Haskell type (Bool,Integer) contains five different kinds of values, as shown in the figure. Each arrow leads from a less-defined (less informative) value to a more-defined value.

Define an operation ``lub :: HasLub a => a -> a -> a``.

If the arguments are consistent, i.e. have a common upper bound, then that is the result.
Otherwise the context is used, ``f (a `lub` b) = f a `lub` f b``

The dispatch semantics is that all methods are run in parallel using the .  Furthermore ``lub`` is an oracle, analyzing the whole program - we want return type overloading, return values that are not accepted by the surrounding context are discarded.

This falls out naturally from doing the analysis on the CPS-transformed version of the program.


The way Stroscot optimizes dispatch is:
* eliminate all the statically impossible cases (cases that fail)
* use profiling data to identify the hot paths
* build a hot-biased dispatch tree
* use conditionals for small numbers of branches, tables for large/uniform branches (like switch statements)

The standard vtable implementation of Java/C++ is out. TODO: check out pattern dispatch paper

Karnaugh map with profiling data
