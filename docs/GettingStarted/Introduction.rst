Introduction
############

This is a 10-minute (2000 word) introduction of the main concepts of Stroscot.

Term rewriting
==============

The execution of a Stroscot program is modeled as taking the main program as the starting term and applying rewrite rules until no more can be applied, resulting in the "value" or normal form of the program.

A rewrite rule or clause is applied by matching the left hand side to the current term or one of its subterms and applying the resulting substitution to the right hand side. Stroscot supports predicate dispatch or conditional rewriting, meaning that a rewrite rule may have a condition - the rewrite rule is applicable only if its condition holds.

In the debugger you can always see what the current term looks like and what rewrite rules are applied. For example, with the rules ``fact n | n > 0 = n * fact (n-1)`` and ``fact 0 = 1``, a possible reduction sequence is:

::

  fact 3
  3 * fact (3-1)
  3 * fact 2
  3 * (2 * fact (2-1))
  3 * (2 * fact 1)
  3 * (2 * (1 * fact (1-1)))
  3 * (2 * (1 * fact 0))
  3 * (2 * (1 * 1))
  3 * (2 * 1)
  3 * 2
  6

We say that ``fact 3`` reduces to the value ``6``, written more concisely as the rewrite rule ``fact 3 = 6``.

Parallel matching
=================

Stroscot's main reduction semantics is nondeterministic - all possible matches and rewrites are considered. It is an error if there is no unique answer. For example this is an error:

::

  f 1 = 1
  ;
  f y = 2

  f 1
  # Error: rule conflict for `f 1`

But if the first clause was ``f 1 = 2`` it would be allowed:

::

  f 1 = 2
  ;
  f y = 2

  f 1
  # 2

Since many programs are sequential there is a convenience syntax for making the cases non-overlapping:

::

  f 1 = 1
  f y = 2

  # equivalent to

  f 1 = 1
  ;
  f y | y != 1 = 2


But the parallel behavior is useful. For example you can write impromptu tests that Stroscot will verify give the same result:

::

  fib (-1) = 0
  fib 0 = 1
  fib n = fib (n-1) + fib (n-2)

  # test
  fib 5 = 5

  # would give an error on fib 5 = 6

Although writing ``assert (fib 5 == 5)`` might be clearer.

Another useful application is specializing generic methods to high-performance implementations for specific types, and in general giving more freedom to the compiler to do evaluation. For example consider the idea of "parallel and" from `this old Usenet post <https://groups.google.com/g/comp.lang.functional/c/sb76j3UE5Zg/m/h1ps0wEaTckJ>`__)::

  myand False x = False
  myand x False = False
  myand True True = True

With the sequential matching, matching ``myand True False`` tries the first clause and evaluates ``False == True``, which fails, so the second clause gives ``myand True False = False``. But this behavior depends on proving ``False != True``. In this case they are both values so it is trivial. But if instead of ``True`` we use a computation ``expensive_true`` that takes a long time to produce the value ``True``, then proving ``False != expensive_true`` takes a long time. Now consider parallel matching::

  myand False x = False
  ;
  myand x False = False
  ;
  myand True True = True

Because the system is parallel, Stroscot can choose which term to evaluate first. It may evaluate ``expensive_true`` for a little bit, but then it can switch to evaluating ``False`` and immediately match the second clause without finishing evaluation of ``expensive_true``.

Now in some cases the system can automatically be parallelized. In particular with this example, the one overlapping case is ``myand False False`` and it can be verified that both of the first two clauses give ``False``, hence the system is weakly orthogonal and can be parallelized. But in general identifying opportunities like this is hard, and also with sequential matching specializing generic methods would require careful priority management, so it is better to have an explicit syntax.

Logic programming
=================

Sometimes the requirement that the value is unique is burdensome. Stroscot also allow logic programming, so you can work with relations and nondeterministic functions.
