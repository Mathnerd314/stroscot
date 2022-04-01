Sets
####

Stroscot allows specifying properties about execution, which the compiler then attempts to prove or at least fuzz (see :ref:`Verification <Verification>`).

What are types?
===============

Are properties types? Is Stroscot typed? These get down to a fundamental disagreement in the literature.

In Church style, the style present in most academic papers on type systems, a term without a type is meaningless, there is only one principal type for a given expression, and the type system is sound and decidable. Haskell is Church-style - e.g. when the type inference fails for GADTs, the type signature must be given. Harper has `argued <https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/>`__ that dynamic languages are unityped static languages. Instead of "proper" types, one defines an agglomerated unitype, and throws away soundness. Most languages are already unsound so this is not much of a loss, and it increases expressiveness because terms such as the Y combinator become typeable. Because the type system is trivial, properties are not part of the type system.

Curry style types, called sorts in :cite:`pfenningChurchCurryCombining2012`, sorts define properties that can be checked or ignored, extrinsic to the terms themselves. A term may satisfy several sorts, or none at all. Since the sorts are optional there must necessarily be an operational semantics that does not refer to any sorts, and hence the language is dynamic. Statically verified properties are indeed sorts or "types" in the Curry sense.

So it comes down to definitions. Stroscot doesn't have types in Harper's asshole sense, but it does in the sense used by reasonable people. To avoid useless arguments we use the term "set" instead of "type" to refer to the set of values a variable may have. Constraining the set of values enables many useful optimizations to be performed, and seems to function similarly in practice to a type system. But since the "types" actually behave like sets "set" is IMO clearer.

Even so, there are type systems that are almost as expressive as Stroscot's sets. Distributive lattices as used in :cite:`dolanAlgebraicSubtyping2016` are isomorphic to collections of sets. Similarly :cite:`naikTypeSystemEquivalent2008` provides a method to interpret the model produced by a model checker as a type derivation using flow, intersection, and union types. So in some sense all the properties Stroscot verifies are bona-fide types. But the types are complex and precise, e.g. ``length : (Nil-->0) & (Cons a b-->1+(length b))``, and likely hard to interpret. Meanwhile errors produce a concrete program trace of a failing path, which should be easy to turn into a good error message or even allow interactive debugging.

Another notion of "type" is substructural typing. Stroscot's execution model is substructural (based on linear logic), but it's not clear how to expose this to the programmer.

Sets
====

Sets in Stroscots are defined by a predicate ``a isElementOf S : Value -> Set -> Bool``, where ``Set = { s : Any | forall a : Any. (a isElemof s : Bool) }``. The predicate can describe any side effect free computation, so a set can describe any boolean function.

Sets don't contain null by default, you have to add it to the predicate as an allowed value.

Function definition
-------------------

The usual method combination mechanisms apply. In particular you can define sets by creating a fresh symbol and overloading ``isElementOf``:

::

  symbol A
  1 isElementOf A = true
  2 isElementOf A = false
  a isElementOf A = (a : B) and not (a : C)

Often a set will be a fresh symbol ``Constr`` and a definition of ``isElemOf`` checking whether an element has the tag ``Constr`` as head and certain values as arguments. This is similar to a data type definition in other languages.

::

  symbol Foo
  a isElementOf Foo =
    match a | Foo (b : Int) (c : Bool) = true
            | otherwise = false

  # similar to data Foo = Foo Int Bool in Haskell

Set syntax definition
---------------------

You can also define sets with set-builder notation ``{s : SomeSet | predicate s }``, set literals ``{1,2,3}``, and set operations (union ``|``, ``intersect``, complement ``not``, difference ``-``, symmetric difference ``symdiff``). Although it is possible to override the definitions of these sets with a definition like ``1 isElementOf (Int|Bool) = false``, this is considered poor practice - it is better to define a fresh symbol for the set.

::

  {x : Int | x > 0}
  {x : List | 0 < size x < 10 }
  Int-{0}
  Int intersect (range 0 10)
  {a,b}
  {a} union {b} # same as {a,b}
  {a}|{b} # same as {a,b}
  {} # empty set, "Void"

Element definition
------------------

Because specifying a new element of a set is common, there is a special syntax using ``symbol``:

::

  symbol x : A

  # short for
  symbol x
  x : A = true

Poset definition
----------------

Sets form a poset under the subset relation (inclusion), which is called ``Set``. It is possible to use the poset constraint language with this to define sets. For example this defines a set ``x = {1}``:

::

   symbol x : Set
   elemR Set {1} = x

More generally we may compute either the minimal set or the maximal set satisfying the given constraints.

You can assert that two sets are disjoint (empty intersection), this is useful sometimes.

::

  assert (A disjoint B)

Ranges
------

The basic idea is that any numeric set of integers can be given via a lower and upper bound. In particular ``n isElementOf (range a b) = (n isElemOf Integer) and a <= n < b``. Many typical integral data types could be represented as ranges in this way:

::

  unsigned n = range 0 (2^n)
  signed n = range (-2^(n-1)) (2^(n-1))

  byte = unsigned 8
  sbyte = signed 8
  short = signed 16
  ushort = unsigned 16
  int = signed 32
  uint = unsigned 32
  // And so on ...

Dependent types are useful too with ranges. For example, say I have an array and want to pass an index whose range is guaranteed to be in-bounds. I can associate the upper bound of the number's range with the array length directly:

::

  get : forall T. (array : Array T) -> range 0 (length array) -> T
  get array index = array[index]

Enumeration
-----------

An enumeration is a set of symbols but the order of the symbols is defined.

::

   enum Doneness {Rare, MediumRare, Medium, WellDone}

   Rare < WellDone # true

ADTs
----

Abstract data types are sets containing trees of uninterpreted symbols. So a datatype declaration (from `here <https://github.com/UlfNorell/insane/blob/master/Context.agda>`__)

::

   data Cxt [ Ty : Cxt Ty -> Set ] : Set where
      nil  : Cxt Ty
      snoc : (G : Cxt Ty) -> Ty G -> Cxt Ty

is equivalent to

::

   symbol nil
   symbol snoc
   Cxt Ty = a = arbElem(); assume(a isOfType Cxt Ty); assert(Ty a isElemOf Set); Set { x where
      (x == nil
      or exists G y. x == (snoc G y) && G isElemOf (Cxt Ty) && y isElemOf (Ty G))
   }

:cite:`dolanPolymorphismSubtypingType2017` section 2.2 says the recursive appearance of ``Cxt Ty`` is interpreted using the least pre-fixed point and Bekić's theorem, but I think any fixed point will do.

Records
-------

Record specifications can be closed, listing all properties, or open, allowing other associations not mentioned. For example ``{a: "s", b: 2} isElementOf OpenRec [("b",Int)]``. The fields can be ordered or unordered. Some fields can be omitted, i.e. they are optional. This is different from a ``Maybe`` type because ``{a: Nothing}`` is distinct from ``{}``. This can be accomplished by writing ``Rec [("b",Int,Optional)]``.

Rich Hickey seems to think values like ``("b",Int)`` are important and should have names like ``:b`` and ``:c``, so you can write ``{b: 2, c: 3} isElementOf Rec [:b,opt :c]``. I guess? It could be useful if you use the same fields a lot, but it doesn't save too much typing. The real optimization is defining collections of fields, so you can write ``Rec ([:b,:c] ++ standardfields)``.

Clojure also has this thing about namespace qualification for records. XML had qualified names (namespaces), JSON doesn't. Everybody has switched from XML to JSON. So it seems like namespaces are overcomplicating the issue. Generally formats have a single domain of authority and don't need namespacing - the interpretation of a field is a property of the schema, not the value. This can be seen in the evolution of the ``<svg>`` element from

If you do have user-defined fields and need some simple thing to avoid key collisions you can use long key names like "org.mycompany.thing". This relies on the simple assumption that non-namespaced property names won’t have "." in them. But unlike a namespace mechanism this doesn't view unqualified names as incomplete qualified names, rather it sees qualified names as over-specialized names. "Over" is because you can't access it like ``obj.org.mycompany.thing``, you have to write ``obj["org.mycompany.thing"]``.

Tracing
-------

Often when checking if a value is in a set we want a detailed explanation why a value is not in a set, e.g. ``(1,"a") isElementOf (Int,Int)`` could output ``not element: second component "a" is not in set Int``. More generally the message looks like ``not element: $at $val is not in set $set`` and there is a stack of such messages from most specific to the overall expression.

Alternately we could provide the reduction history, something like:

::

  (1,"a") isElementOf (Int,Int)
  1 isElementOf Int and "a" isElementOf Int
  True and "a" isElementOf Int
  True and False
  False

But basically this is an error message and error messages are hard.

Annotations
===========

Programmers can use annotations to say that a value is in a specific set. This restricts the possible range of values an identifier may take, allowing the compiler to optimize for a specific runtime representation.

Set annotations are translated to assertions, and these assertions are statically checked, meaning values outside the set will give an assertion failure.

::

  a : T = { assert(a isElemOf T); a }

Don't override ``:``, it is intended as a no-op. For conversions use the explicit function ``convert Int64 2``.

Function annotations
====================

The main function type declaration restricts the definition of the function so it is only applied on the type, i.e. without other definitions the function is not defined outside its type. You can define multiple restricted functions to obtain overloaded behavior on different types. The restriction shows up in documentation and IDE tooltips.

::

   A : S -> T -> Int
   A = ...

   -- expands to

   A$untyped = ...
   A s t | $args isElemOf (S,T) = {
      ret = A$untyped $args
      assert (ret isElemOf Int)
      ret
   }

This behavior seems more similar to the type declarations found in other languages, hence why it is the default. E.g. in Rust ``i32 f(i32)`` cannot be applied to ``i64``, whereas with the ``check`` version ``f`` could be applied to ``i64``.

Dependent types
---------------

The types can bind the value, so Stroscot can express dependent types. And the values are in scope in the type, so even `insanely dependent types <https://github.com/UlfNorell/insane/>`__ can be defined:

::

  A : (s : S s) -> T s
  A = ...

  -- expands to
  A$untyped = ...
  A s | $args isElemOf (S s) = {
      ret = A$untyped $args
      assert (ret isElemOf (T s))
      ret

Check
-----

Another version of typing functions simply checks compatibility with a type, that the return value is in the expected set over the whole input range.

::

   A = ...

   check A : S -> Int

   -- expands to

   A = ...

   {
     s = arbElem()
     assume(s isElemOf S)
     assert(A s isElemOf Int)
   }

Total check
-----------

``check`` allows partial functions, i.e. nonterminating behavior or throwing exceptions. With a total check all behavior must be accounted for, similar to checked exceptions.

::

   total_check (/) : Int -> (Int | DivideByZero)

You can also specify a total type signature:

::

   total A : S -> T -> (Int | DivideByZero)
   A = ...

This is equivalent to specifying the regular type signature and also a ``total_check``.

Total check can be used for unit testing, just put the arguments and results in singleton sets:

::

   check square :  {2} -> {4}
   check square :  {3} -> {9}


Contracts
---------

Spec#, Eiffel, Ada SPARK, and Argus have "contracts", requires/ensures on methods. Many checks/throws in .NET and Java can be expressed as preconditions. In Stroscot preconditions can be represented using dependent types and a refinement type on the argument before the result. E.g. a "requires notnull" is written:

::

  total head : { xs : [a] | not (null xs) } -> a

And a multiple argument example, ``requires a >= b``:

::

  total (-) : (a : Nat) -> { b : Nat | a >= b } -> Nat

It looks a little weird, but IMO it's fine, and macros can implement the ``requires`` syntax if need be.

Postconditions ("ensures") can be expressed as restrictions on the return type:

::

  total square : Int -> {x : Int | x >= 0}

The curse of static typing
--------------------------

If a function ``foo`` does something unexpected, there are three possibilities:

1. Some unusual overloading of foo was defined. That is that clause's problem. You shouldn't override equals to return true only if the square root of one is the same as the other, and similarly you shouldn't have overloaded foo and done something unexpected. Static verification can help with this by documenting the expected properties. The solutions are to verify, change the behavior, or split the behavior into a different function name.

2. foo was defined with a reasonable clause but the clause relied on a contract that wasn't described. This is harder to catch as static verification usually only covers a subset of behavior, but the solution is to limit the clause with a signature / contract.

3. foo would work, but its signature has been defined too narrowly so is undefined

Usually functions are clearly written and work as long as the functions they call work on the arguments. So it is this third case that bites, because you can overload the called functions but you can't relax the signature. So restrictive signatures are a curse in this example.

Type synthesis
==============

Type synthesis is tricky, but with the termination checker we don't have any visible types. The optimizer does a form of type synthesis when it assigns formats to values, but the formats can be conditional on state, and the optimizer will use a catch-all format for hard cases, so the formats are complete but not sound. The only useful case for a complex type synthesis algorithm might be pretty-printed type signatures in documentation, but there having the developer specify type signatures is a viable option.

But `dependent <https://github.com/UlfNorell/insane/>`__
`circular <https://github.com/gelisam/circular-sig>`__ dependent types will presumably ruin all the fun and require type signatures.

We could do synthesis at run time, e.g. the type of a list of values is the list type applied to the set of values contained in the list. This might be useful for resolving type-overloaded methods.

Roles
=====

Roles are just an optimization for ``coerce``, but there are better ways to implement optimizations. It seems like a dirty hack to solve a pressing problem. I think Stroscot can get by without them.
