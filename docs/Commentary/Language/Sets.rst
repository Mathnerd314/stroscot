Sets
####

Stroscot allows specifying properties about execution, which the compiler then attempts to prove or at least fuzz (see :ref:`Commentary/Language/Verification:Verification`). The most common form of property is membership in a set of values, described here. Constraining the set of values enables many useful optimizations to be performed.

Steelman 3C "It shall be possible to define new data types in programs. A type may be defined as an enumeration, an array or record type, an indirect type, an existing type, or a subtype of an existing type. It shall be possible to process type definitions entirely during translation. An identifier may be associated with each type. No restriction shall be imposed on user defined types unless it is imposed on all types."

Definition
==========

Sets in Stroscots are defined by a predicate ``a isElementOf S : Value -> Set -> Bool``, where ``Set = { s : Any | forall a : Any. (a isElemof s : Bool) }``. The predicate can describe any side effect free computation, so a set can describe any boolean function. As described in :ref:`paradoxes` Stroscot avoids the set-theoretic paradoxes by requiring every set definition to be well-founded.

Sets don't contain null by default, you have to add it to the predicate as an allowed value.

Definition by overloading
-------------------------

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

We could have some shorthand syntax for ranges, like ``[1..n]``, but is the endpoint included? Swift says yes and has ``[1..<n]`` for right-exclusive ranges. Per `Ecstasy <https://github.com/xtclang/xvm/discussions/40>`__ this could be extended to "gremlin operators", left-exclusive ``[1>..n]`` and both-exclusive ``[1>..<n]``. There is also the `traditional interval notation <https://en.wikipedia.org/wiki/Interval_(mathematics)#Including_or_excluding_endpoints>`__ but this is hard to fit into the syntax.

Dependent types are useful too with ranges. For example, say I have an array and want to pass an index whose range is guaranteed to be in-bounds. I can associate the upper bound of the number's range with the array length directly:

::

  get : forall T. (array : Array T) -> range 0 (length array) -> T
  get array index = array[index]

Fixed-point rationals
---------------------

There are also fixed-point rational formats like ``x divided_by 2^5 : Scaled int (2^5)`` where ``x`` is an integer and ``2^5 `` is the denominator. The definition is just ``Scaled t d = { x : Rational | numerator x isElemOf t and denominator x == d }``.

Enumeration
-----------

Steelman 3-2A. "There shall be types that are definable in programs by enumeration of their elements. The elements of an enumeration type may be identifiers or character literals. Each variable of an enumeration type may be restricted to a contiguous subsequence of the enumeration.""

Steelman 3-2B. "Equality, inequality, and the ordering operations shall be automatically defined between elements of each enumeration type. Sufficient additional operations shall be automatically defined so that the successor, predecessor, the position of any element, and the first and last element of the type may be computed."

An enumeration is a set of symbols but various operations on the symbols are automatically defined.

::

   enum Doneness {Rare, MediumRare, Medium, WellDone}

   Rare < WellDone # true

It's a macro that defines the symbols, a comparison operator, conversion to/from an integer, and other operations.

ADTs
----

3-3G. It shall be possible to define types with alternative record structures (i.e., variants). The structure of each variant shall be determinable during translation.

3-3H. Each variant must have a nonassignable tag field (i.e., a component that can be used to discriminate among the variants during execution). It shall not be possible to alter a tag field without replacing the entire variant.

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

Refinements
-----------

Steelman 3D. "The constraints that characterize subtypes shall include range, precision, scale, index ranges, and user defined constraints. The value of a subtype constraint for a variable may be specified when the variable is declared. The language should encourage such specifications. [Note that such specifications can aid the clarity, efficiency, maintainability, and provability of programs.]"

::

  RefinementType = { x : SomeType | someProperty x }


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

4B. It shall be possible to specify the type of any expression explicitly. The use of such specifications shall be required only where the type of the expression cannot be uniquely determined during translation from the context of its use (as might be the case with a literal).

Programmers can use annotations to say that a value is in a specific set. This restricts the possible range of values an identifier may take, allowing the compiler to optimize for a specific runtime representation.

Set annotations are translated to assertions, and these assertions are statically checked, meaning values outside the set will give an assertion failure.

::

  a : T = { assert(a isElemOf T); a }

``:`` is a no-op to make analysis easy. There is also conversion ``convert Int64 2`` which might get an infix operator.

Function annotations
====================

7G. The type of each formal parameter must be explicitly specified in programs and shall be determinable during translation. Parameters may be of any type. The language shall not require user specification of subtype constraints for formal parameters. If such constraints are permitted they shall be interpreted as assertions and not as additional overloading. Corresponding formal and actual parameters must be of the same type.

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

Function types are powerful. For example formatted printing, buffered I/O, compression, and pipes can all use a single type ``Writer = ByteArray -> IO { bytes_written : int, 0 <= bytes_written < length p }``.

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

The curse of restrictive type signatures
----------------------------------------

If a function ``foo`` does something unexpected, there are three possibilities:

1. Some unusual implementation/overloading of foo was defined. That is that clause's problem. You shouldn't implement equals to return true only if the square root of one is the same as the other, and similarly you shouldn't have overloaded foo and done something unexpected. Static verification can help with this by documenting and checking the expected properties. Solutions for this case are to change the behavior or rename the function to make its semantics clearer.

2. foo was defined with a reasonable clause but the clause relied on a contract that wasn't described. This is harder to catch as static verification usually only covers a subset of behavior, but the solution is to limit the clause with a signature / contract. Type signatures are helpful here.

3. foo is undefined for the arguments you are calling with, because it has been defined with a restricted signature.

This last case is the "curse". Many functions are synonymous with their implementation and work as long as the functions they call work on the arguments. So it is better to use non-restrictive signatures, because the actual domain of the function stays as large as possible. When you write the non-restrictive signatures you are simply advertising that a particular domain is well-tested. In contrast a restrictive signature can't be extended except by duplicating the implementation of the function.
