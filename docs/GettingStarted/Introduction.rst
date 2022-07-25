Introduction
############

This is a 10-minute introduction of the main concepts of Stroscot.

Secrets of matching
===================

Consider the program from `this old Usenet post <https://groups.google.com/g/comp.lang.functional/c/sb76j3UE5Zg/m/h1ps0wEaTckJ>`__)::

  myand False x = False
  myand x False = False
  myand True True = True

This syntax sequentially matches the clauses in order. Consider ``myand undefined False`` - matching the first clause evaluates ``False == undefined``. In Haskell ``undefined`` throws and evaluation results in an error. In Stroscot ``undefined`` is just a value, so this clause fails and the second clause gives ``myand undefined False = False``.

Now this depends on proving ``False != undefined``. In this case they are both values this is trivial, but what about an ``expensive_undefined`` which is an expensive computation that never terminates? Well, Stroscot also allows parallel matching::

  myand False x = False
  ;
  myand x False = False
  ;
  myand True True = True

The semantics is based on term rewriting - all possible matches and rewrites are considered. It is an error if there is no unique answer. For this example, the system is weakly orthogonal so it is easy to analyze. Stroscot proves that the answer is unique because  in the one ambiguous case ``myand False False`` both clauses give ``False``, and selects the parallel outermost reduction strategy which will always succeed if there is an answer. Hence evaluating ``myand expensive_undefined False`` reduces ``expensive_undefined`` for a little bit, then evaluates ``False`` to completion and matches the second clause.

Logic programming
=================

Sometimes the requirement that the return value is unique is burdensome. Stroscot also allow logic programming, so you can work with relations and nondeterministic functions.

Misquoting :cite:`iversonNotationToolThought1980`:

    Users of computers and programming languages are often concerned primarily with the efficiency of execution of algorithms, and might, therefore, summarily dismiss [logic programming]. Such dismissal would be short-sighted since a clear statement [...] can usually be used as a basis from which one may easily derive a more efficient algorithm.

    [...]

    The practice of first developing a clear and precise definition [...] without regard to efficiency, and then using it as a guide and a test in exploring equivalent processes possessing other characteristics, such as greater efficiency, is very common in mathematics. It is a very fruitful practice which should not be blighted by premature emphasis on efficiency in computer execution.

    [...]

    Finally, overemphasis of efficiency leads to an unfortunate circularity in design: for reasons of efficiency early programming languages reflected the characteristics of the early computers, and each generation of computers reflects the needs of the programming languages of the preceding generation.

Practically, logic programming is a great tool for naturally expressing computational tasks that use logical constraints. Large programs generally run into one or two of these tasks. Without logic programming these tasks must be solved in an ad-hoc and verbose way. Compare `Sudoku with Prolog <https://www.metalevel.at/sudoku/>`__ vs `Norvig's Sudoku solution <https://norvig.com/sudopy.shtml>`__. Other examples include parsers, typecheckers, and database queries.
