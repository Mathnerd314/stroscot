Overloading
###########

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


But ``f 2 = 2`` would be allowed. This behavior is useful for creating optimized methods for specific types, and also for tests:

::

   fib (-1) = 0
   fib 0 = 1
   fib n = fib (n-1) + fib (n-2)

   # test
   fib 5 = 5

Sequential matches
==================

The pipe syntax matches cases from top to bottom:

::

   f
   | 1 y = 1
   | x 2 = 2
   | x y = 3

Maybe you will also be able to use ``match`` within a function, if the syntax details work out:

   ::

      f = match (2+2) (5+5) | x y = 2
                            | 1 y = 2

Priorities
==========

The way this expands is that there is a poset of priorities.

::

   prioDef p1 > p2 > p3

   prio p1
   f 1 y = 1

   prio p2
   f x 2 = 2

   prio p3
   f x y = 3

Clauses with lower priority are ignored if there are clauses with higher priorities. Clauses with incomparable priorities are combined with lub. Clauses with equal priorities have their priorities disambiguated by adding specificity (lexicographical order). Specificity is another poset relation defined by an SMT solver: ``a`` is less specific than ``b`` if ``SAT(a & not b)`` and ``UNSAT (not a & b)``. If the priorities + specificity are still equal then a warning is given and they are combined with lub.

If a priority is not specified it uses a fresh priority incomparable with all others.

Method qualifiers
=================

Stroscot also supports Common Lisp's method qualifiers ``before``, ``after``, and ``around``, as well as a custom qualifier ``list``. Although the dispatch algorithm is complex, the idea is that it is so generic that including it once in the language proper will save everyone's time by avoiding the need to reimplement it.

The basic idea of method combination can be seen in this `illustration <https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/Method-combination.png>`__

``list`` simply produces the list of applicable methods, that can then be applied or manipulated as needed. It is an error to define a list method if there are any other types of methods (primary, before, after, or around).

Semantics
=========

The exact semantics is that all cases are run in parallel using the `lub operation <http://conal.net/blog/posts/merging-partial-values>`__ (or maybe its less powerful cousin ``unamb``). Predicate failure, failed assertions, and nontermination are all treated as bottom.

In fact the semantics is even more complicated, because return values that are not accepted by the surrounding context are also discarded. This falls out naturally from doing the analysis on the CPS-transformed version of the program.

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

Patterns
========

Patterns all compile to guard conditions on ``$args``. They also check that the arity of ``$args`` is the number of patterns.

::

   _ --> True
   a --> True
   ^a --> $args[i] == a
   [(1, "x"), {c: 'a'}] -> $args[i] == [(1, "x"), {c: 'a'}]
   [1, ...] --> $args[i][0] == 1
   {a: 1, ...: rest} --> $args[a] == 1
   pat1 AND pat2 --> match $args pat1 and match $args pat2
   pat1 OR pat2 --> match $args pat1 or match $args pat2
   ~pat --> True
   a with f a --> f a
   (f -> a) --> match (f $args[i]) a
   Int z --> $args[0] == Int
   _f a --> True

Overrides
=========

By default, methods are scoped to their module. Every definition ``foo = a`` binds the identifier ``Module.foo``, and each module creates a new identifier. The ``override`` statement prevents creating a new identifier, so that instead a base identifer can be extended.

.. code-block:: python3

  # module 1
  foo 1 = 1

  # module 2
  import 1
  override foo
  foo 2 = 3

  # module 3
  import 1, 2
  foo 1 # 1
  foo 2 # 3

If the override statement was not in module 2, then using ``foo`` in module 3 would result in an ambiguous name resolution error.

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