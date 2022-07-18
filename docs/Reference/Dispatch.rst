Dispatch
#########

Stroscot supports pattern matching as well as predicate dispatch:

::

  f 1 = 1
  f 2 = 2
  f y | y != 1 && y != 2 = 3

Dispatch cases are not ordered - they overlap and run in parallel. All cases are tried nondeterministically and run. If multiple predicates succeed, it is required that they produce the same result. For example if we were to modify the last case:

::

  f 1 = 1
  f 2 = 2
  f y = 2

  f 1
  # Error: rule conflict for `f 1`


But if the first clause was ``f 1 = 2`` it would be allowed. This behavior is useful for creating generic implementations that have optimized methods for specific types, and also for tests:

::

  fib (-1) = 0
  fib 0 = 1
  fib n = fib (n-1) + fib (n-2)

  # test
  fib 5 = 5

Although writing ``assert (fib 5 == 5)`` might be clearer.

Local functions are applied in the same way as global ones, i.e., the argument patterns of each rule are matched against the actual function arguments nondeterministically and optimal reduction is performed.

If none of the rules match then the term does not reduce (it becomes a normal form) - to raise an exception a catch-all clause must be defined.

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

Each clause has its own priority, which is the tuple (declared priority, specificity) ordered by lexicographical order. If a declared priority is not specified then it is an unnamed fresh priority with the relation ``prioLow < fresh < prioHigh``. Specificity is a poset relation defined by an SMT solver: ``a`` is more specific than (higher priority) ``b`` if ``UNSAT(a & not b)`` and ``SAT (not a & b)``.

Clauses with lower priority are shadowed by clauses with higher priorities. Clauses with incomparable priorities are run in parallel. Clauses with equal priorities give a warning and are run in parallel.

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



You can get the priority of a clause with ``prioOf { clause | condition }``, so as to constrain a clause that has an anonymous priority. The AST must match exactly.

There is also ``prioBound {}``, similar in syntax to ``prioAll`` but puts a top/bottom constraint on fresh priorities.

You can also define sets of priorities, either by listing or with ``prioSet { }``, and use ``prioSetDef`` to define large-scale orderings. You can resolve conflicts in the priority ordering definitions by declaring the priorities (meta-priorities) of the definitions. See Posets.

Next method
-----------

High priority methods shadow lower-priority methods, rather than entirely replacing them. ``next-method`` allows access to these shadowed methods. The shadowed methods are run in parallel.

You can run the methods with different parameters, ``next-method { silently=true }``.

You can also call a specific clause, ``callClause { clause | guard, module = ..., priority = ... }``, or its ``next-method``, ``callClauseNext``.

List methods
------------

Stroscot also supports Common Lisp's custom qualifier ``list``. ``list`` simply produces the list of applicable methods (ones whose guard is OK). This list can then be applied or manipulated as needed. It is an error to define a list method if there are any normal methods defined.
