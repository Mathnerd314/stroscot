Dispatch
#########

Stroscot supports pattern matching as well as predicate dispatch:

::

  f 1 = 1
  f 2 = 2
  f y | y != 1 && y != 2 = 3

Dispatch cases are not ordered. If multiple predicates match, it is required that all matching cases produce the same result. For example if we were to modify the last case:

::

  f 1 = 1
  f 2 = 2
  f y = 2

  f 1
  # Error: rule conflict for `f 1`


But if the first clause was ``f 1 = 2`` it would be allowed. This behavior is useful for creating optimized methods for specific types, and also for tests:

::

  fib (-1) = 0
  fib 0 = 1
  fib n = fib (n-1) + fib (n-2)

  # test
  fib 5 = 5

Although writing ``assert (fib 5 == 5)`` might be clearer.

Local functions are applied in the same way as global ones, i.e., the argument patterns of each rule are matched against the actual function arguments and optimal reduction is performed.

If none of the rules match then the function application remains unevaluated (it becomes a normal form) - by default no exception is raised.

Priorities
==========

The way sequential matching expands is that there is a chain of priorities.

::

   clause c1
   f 1 y = 1

   clause c2
   f x 2 = 2

   clause c3
   f x y = 3

   prioConstrain prioHigh > c1 > c2 > c3 > prioLow

Each clause has its own priority. Clauses with lower priority are ignored if there are clauses with higher priorities. Clauses with incomparable priorities are combined with lub. Clauses with equal priorities have their priorities disambiguated by adding specificity overriding (i.e., the actual priority is the tuple (priority, specificity) ordered by lexicographical order). Specificity is another poset relation defined by an SMT solver: ``a`` overrides ``b`` if ``UNSAT(a & not b)`` and ``SAT (not a & b)``. If the priorities + specificity are still equal then a warning is given and they are combined with lub.

If a priority is not specified it uses a fresh priority with ``prioLow < fresh < prioHigh``.

There is a macro to give all clauses in a region the same priority, which makes the specificity implementation more useful.

::

   prioAll p {
      isDoor _ = no
      isDoor "garden gate" = yes
   }
   -- the second clause is more specific than the first, hence overrides it

   prioAll p2 {
      isPerson _ = no
      isPerson m | isMan m = yes

      isMan _ = no
      isMan "Steve" = yes

      isDoor _ = no
      isDoor "Steve" = yes

      describe p | isPerson p = "Humanoid"
      describe m | isMan m = "Masculine"
      -- describe m | isDoor m = "Door" -- conflict with isMan for "Steve"
   } -- again each clause overrides the second


I thought about making all clauses the same priority by default, but concluded that specificity was too confusing.

You can get the priority of a clause with ``prioOf { clause | condition }``, so as to constrain a clause that has an anonymous priority. The AST must match exactly.

There is also ``prioBound {}``, similar in syntax to ``prioAll`` but puts a top/bottom constraint on fresh priorities.

You can also define sets of priorities, either by listing or with ``prioSet { }``, and use ``prioSetDef`` to define large-scale orderings. You can resolve conflicts in the priority ordering definitions by declaring the priorities (meta-priorities) of the definitions. See Posets.

Method qualifiers
=================

Stroscot also supports Common Lisp's method qualifiers ``before``, ``after``, and ``around``, as well as a custom qualifier ``list``. Although the dispatch algorithm is complex, the idea is that it is so generic that including it once in the language proper will save everyone's time by avoiding the need to reimplement it.

``before`` and ``after`` are simply ``around`` methods with the fixed priority ``around > before > after``:

::

   before f = around { f; next-method }
   after f = around { next-method; f }

Guards on around clauses are handled by unconditionally calling next-method:

::

  around {f | c = d}  = if c then d else next-method

The basic idea of method combination can be seen in this `illustration <https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/Method-combination.png>`__

``list`` simply produces the list of applicable methods, that can then be applied or manipulated as needed. It is an error to define a list method if there are any other types of methods (primary, before, after, or around).

Next method
===========

High priority methods shadow lower-priority methods, rather than entirely replacing them. ``next-method`` allows access to these shadowed methods.

You can run the methods with different parameters, ``next-method { silently=true }``.

You can also call a specific clause, ``callClause { clause | guard, module = ..., priority = ... }``, or its ``next-method``, ``callClauseNext``.

Implementation
==============

The full dispatch mechanism is as follows:

::

   dispatch methods args = do
     [arounds, befores, afters, primaries] = map topological_sort $ partition methods
     next-method = DispatchError
     f arounds where
        f (a:as) = call a { next-method = f as }
        f [] = g primaries

        g [] = DispatchError
        g _ =
            map call befores
            x = call (concat primaries)
            map call (reverse afters)
            return x

   call binds args = fold lub DispatchError (map ($ args) binds)

The way Stroscot optimizes dispatch is:
* eliminate all the statically impossible cases (cases that fail)
* use profiling data to identify the hot paths
* build a hot-biased dispatch tree
* use conditionals for small numbers of branches, tables for large/uniform branches (like switch statements)

Conversion
==========

There is a function ``convert : (T : Set) -> a -> T`` in a module in the core library.

Conversion is intended for equivalent values, so it should satisfy the properties of an equivalence relation (assuming all conversions succeed):
* Reflexive: ``convert T a = a``, if ``a : T``
* Symmetric: ``convert T (convert T2 a) = a``, if ``a : T``
* Transitive: ``convert T3 (convert T2 a) = convert T3 a``

Conversion is only a partial function, hence these properties may not hold due to some of the conversions resulting in errors. For example ``convert Float32 (2^24+1)`` fails because only ``2^24`` and ``2^24+2`` are exactly representable as floats. The "exactly representable" is a requirement because of transitivity and the existence of arbitrary-precision types (``convert Exact (convert Approx a) == convert Exact a``).

Conversion for unions is often undefined, because if ``a : T`` converts to ``b : T2``, ``T`` disjoint from ``T2``, then by reflexivity we have ``convert (T|T2) a = a`` and by assumption and reflexivity we have ``convert (T|T2) (convert T2 a) = convert (T|T2) b = b``, violating transitivity. Hence ``convert (T|T2)`` must be undefined.

The conversion syntax overlaps somewhat with a constructor function, e.g. it is often the case that ``int32 x == convert Int32 x``. But conversion works "deeply" on arrays and such, and because it is an equivalence relation can be applied automatically, whereas constructors may lose information, be stateful, or lazily evaluate their argument. A constructor can call convert, but not the reverse.

Values could be made equivalent to their string representation. This would mainly be useful for displaying values by calling ``convert String``, as multiple decimal literals parse to floating point numbers so the other direction is unpredictable. So an explicit parse function is also needed. It seems easier to hide this functionality away in a ``NumLiteral`` type.

Often we prefer conversions to be total; this is accomplished by adding flags to convert describing the desired behavior. These flags may break the equivalence relation. For example ``convert Byte 1099 { narrowing = true } = 75`` whereas without the narrowing flag it would error, as it is not exactly representable. This allows re-using the promotion mechanism from the next section so is preferred to defining a new function like ``lossyConvert``. Some conversions such as `int32 to float64 <https://stackoverflow.com/questions/13269523/can-all-32-bit-ints-be-exactly-represented-as-a-double>`__ do not need flags as they are already total.

Promotion
=========

Promotion is a catch-all dispatch rule for arithmetic operators on mixed types, similar to `Julia's <https://docs.julialang.org/en/v1/manual/conversion-and-promotion/>`__. It works as follows:

1. Compute a common type using ``promote_rule``
2. Promote all operands to common type using ``convert``
3. Invoke the same-type implementation of the operator, if it exists

For example ``(a : Int32) + (b : Float32) = (convert Float32 a + convert Float32 b) { lossy = true }`` since ``promote_rule (a : Int32) (b : Float32) = out { lossy = true}; Float32``. The system is extensible by defining new conversions and new promotion rules.

Julia's promotion rules:
* Floating-point values are promoted to the largest of the floating-point argument types.
* Integer values are promoted to the larger of either the native machine word size or the largest integer argument type.
* Mixtures of integers and floating-point values are promoted to a floating-point type big enough to hold all the values.
* Integers mixed with rationals are promoted to rationals.
* Rationals mixed with floats are promoted to floats.
* Complex values mixed with real values are promoted to the appropriate kind of complex value.

Equality
========

Since functions can return multiple values and comparing them can give multiple results, we might want equality operations anyEqual and allEqual to control how values are merged.

