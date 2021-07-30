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
  # Error: rule conflict


But if the first clause was ``f 1 = 2`` it would be allowed. This behavior is useful for creating optimized methods for specific types, and also for tests:

::

  fib (-1) = 0
  fib 0 = 1
  fib n = fib (n-1) + fib (n-2)

  # test
  fib 5 = 5

Although writing ``assert (fib 5 == 5)`` might be clearer.

Priorities
==========

The way sequential matching expands is that there is a chain of priorities.

::

   prio p1
   f 1 y = 1

   prio p2
   f x 2 = 2

   prio p3
   f x y = 3

   prioConstrain prioHigh > p1 > p2 > p3 > prioLow

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

Semantics
=========

The semantics is that all cases are run in parallel using the `lub operation <http://conal.net/blog/posts/merging-partial-values>`__. Predicate failure, failed assertions, and nontermination are all treated as bottom.

The semantics of ``lub`` is complicated, because it's the whole program that is analyzed - return values that are not accepted by the surrounding context are discarded. This falls out naturally from doing the analysis on the CPS-transformed version of the program.

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

Implicit conversion
===================

There is a function ``convert`` in a module in the core library. It includes as cases / requirements:

* ``convert a = convert (convert a))`` (transitivity)

Conversions are implicitly applied with this rule:

::

  f e | isError (f e) = f (convert e)

New cases to convert can be added; this is useful in various instances. For example we can create subtyping.

::

  convert e | e : S = T e

The default conversions are chosen follows:
* Conversions should be total, otherwise they are simply replacing one error with another error.
* Also they should be injective, e.g. int32 `can <https://stackoverflow.com/questions/13269523/can-all-32-bit-ints-be-exactly-represented-as-a-double>`__ be converted to float64, but int64 cannot.

Without these rules it is easy to get into trouble where the overloading is ambiguous.

Equality
========

Since functions can return multiple values and comparing them can give multiple results, we might want equality operations anyEqual and allEqual.