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

Now the semantics is based on term rewriting - all possible matches and rewrites are considered. It is an error if there is no unique answer. For this example, the system is weakly orthogonal so it is easy to analyze. Stroscot proves that the answer is unique because  in the one ambiguous case ``myand False False`` both clauses give ``False``, and selects the parallel outermost reduction strategy which will always succeed if there is an answer. Hence evaluating ``myand expensive_undefined False`` reduces ``expensive_undefined`` for a little bit, then evaluates ``False`` to completion and matches the second clause.

Logic programming
=================

Sometimes the requirement that the return value is unique is burdensome. Stroscot also allow logic programming, so you can work with relations and nondeterministic functions.
