Logic programming
#################

Benefits
========

Prolog's Definite Clause Grammars, and library `pio <https://www.swi-prolog.org/pldoc/doc/_SWI_/library/pio.pl>`__, are a great parsing DSL.

With typecheckers you can just directly translate the rules to Horn clauses and it runs. Similarly language interpreters are a direct translation of their operational semantics.

Incomplete data structures are great. ``date(2018, month, 14)`` describes every 14th day in this year, and the month can be constrained or the set of dates extracted.

Database queries are naturally expressed as logical operations on relations. For example pyDatalog and SQL can be called relational languages.

Syntax
======

Prolog uses Horn clauses of the form ``H :- A1, A2, A3``. This is read "The clause head ``H`` is implied by the body's goals ``A1``, ``A2`` and ``A3``." A fact is a clause with no goals, ``F.`` or ``F :- ``, equivalent to ``F :- true`` (since ``true && a = a``). The head and goals are predicates of various syntax. There are various goals; they can be predicate terms with variables and list patterns (Herbrand domain, original Prolog), linear logic formulas (`linear logic programming <https://www.youtube.com/watch?v=rICThUCtJ0k>`__), or constraints (constraint logic programming).

For example, reversing a list::

  nrev([],[]).
  nrev([H|T],L2) :- nrev(T,R), append(R,[H],L2).

  :- nrev([1,2], X), write(X), nl
  % [2,1]


In practice Prolog syntax is pretty bad; programs are heavy on meaningless intermediate variables such as ``R`` in the above. There is a `func <https://www.swi-prolog.org/pack/list?p=func>`__ package that allows writing ``nrev $ T`` instead of ``nrev(T,R), R``. A "function" is defined as follows:

* Any predicate ``p(...in, out)`` is a function from ``in`` to ``out``
* A dictionary is a function from keys to values
* An arithmetic expression ``2*_+3`` is a function on numbers
* A format string is a function from argument list to interpolated output
* A term with a ``~`` is a function which takes no input values and produces an output at the ~ position. For example::

  atom(atom_string(~,"hello world")).

  % is equivalent to

  atom_string(X,"hello world"),atom(X).

More generally we should be able to build compound expressions:
*  ``z = append a (append b c)`` instead of ``append(a,b,temp),append(temp,c,z)``
* ``t = f x + g x`` instead of ``f(x,y), g(x,z), t = y + z``
* in general, ``pred (f x)`` instead of ``f x y, pred(y)``

There is no issue with devising a consistent use of output variables for an expression syntax; Oz does this. In practice the "boring" deterministic code will take up 66%-95% of the program so special-casing familiar function syntax is important. Although it's not clear if Horn clauses are more readable, programmers have gotten used to skimming over assignment clauses and parsing parentheses and infix expressions.

For relations like ``precedes(x,y)``, Horn clauses are not necessarily the optimal syntax either. But the predicate syntax is traditional in logic and unlike functions there is not a well-developed alternative syntax to use. Powerful high-level logic syntax is still an unexplored area.

miniKanren :cite:`byrdRelationalProgrammingMinikanren2009` doesn't have a global clause database so clauses of the same predicate must be grouped, this gives a local "match" syntax::

  nrev l1 l2 :-
    matche l1 l2
      [] [] -> true
      (h . t) l2 -> fresh (\r -> conj
        [ nrev t r
        , append r [h] l2
        ])

  print (run (\x -> nrev [1,2] x)
  -- [[2,1]]


This seems helpful syntax-wise, but MiniKanren and Clojure core.logic are also quite tedious to use in practice.

``matche`` and other syntax desugars into a small set of primitives: :cite:`hemannMicroKanrenMinimalFunctional2013`

* ``fresh`` or ``exists (\x.<body>)``: true if ``body`` is true for some value of ``x``
* ``unify x y`` / ``x == y``: true if ``x`` unifies with ``y``
* ``disj [x,y,z]`` / ``conde [x,y,z]``: true if any of ``x,y,z`` are true (logical or / disjunction)
* ``conj [x,y,z]``: true if all of ``x,y,z`` are true (logical and / conjunction)
* ``run (\x y. <body>)`` delimits the boundary of the logic program. It returns the stream of substitutions of the given variables for which the body is true. Optionally the maximum length of the stream may be specified.

The expansion of ``nrev`` is given in :cite:`hemannFrameworkExtendingMicrokanren2017` page 137 (``define-relation`` is just DSL fluff around ``define`` per the appendix)::

  nrev l1 l2 =
    disj
      [ conj [ l1 == [], l2 == [] ]
      , fresh (\h -> fresh (\t ->
          conj
            [ (h,t) == l1
            ,fresh (\r -> conj [nrev t r, append r [h] l2])]
            ]
      ))]

  print (run (\x -> nrev [1,2] x)
  -- [[2,1]]

Relational programming
======================

Simple logic programs have what :cite:`byrdRelationalProgrammingMinikanren2009` terms "relational" semantics. A state is a map from some set of nominal variables to their substitution, a set of ground terms. A goal is a logical predicate over some variables - applying it to a state that defines the relevant variables gives true or false. The way sub-predicates works is complicated slightly by :cite:`clarkNegationFailure1978`, basically we use iff. Running a program consists of computing the set of satisifable states, which may be empty or infinite.

Practically the execution engine does not return a set, but rather a finite or infinite stream of satisfying meta-states. Meta-states are states that include unbound variables representing any term, and (in constraint logic programming) constraints for these unbound variables. Ideally the stream would be a minimal completely-covering set of meta-states in some arbitrary order, but in practice implementations can return identical or overlapping results.

miniKanren uses an "interleaving" search from :cite:`kiselyovBacktrackingInterleavingTerminating2005`, which is "complete" in the sense that it explores all branches fairly and will find all answers eventually. For relational programs the search strategy is irrelevant so long as it terminates, so there are many other choices; we can optimize the search, or dump the problem into an SMT solver and use its search strategy. CDCL with optimizations should be the fastest. This has been explored in the field of "answer set programming".

By default Prolog does not use the `occurs check <https://en.wikipedia.org/wiki/Occurs_check>`__ in unification. This means for ``x == f x`` the substitution ``x -> f x`` is obtained. Denotationally this can be accommodated by allowing states to contain infinite terms, :cite:`weijlandSemanticsLogicPrograms1990` ``x = f (f (f (...)))`` in this case. In most Prolog programs the occurs check does not make a difference and simply slows down unification. :cite:`aptWhyOccurcheckNot1992` Prolog defines a ``unify_with_occurs_check`` predicate for situations where logical soundness is desired, although the implicit unification when dispatching predicates is still unsound. miniKanren always uses the occurs check.

Non-relational programming
==========================

"Logic programming" a la Prolog has extended the execution engine with non-relational predicates that expose details of the underlying implementation:

* Cut (!) which commits to choices made since the parent goal was unified with the left-hand side of the clause containing the cut. miniKanren similarly includes operators conda (soft-cut) and condu (committed choice). Concurrent logic programming also has committed choice which prunes off all other branches once it is known that a clause's guard goals all succeed.
* ``var/1`` which checks if the variable is unbound
* ``copy_term/2`` which can duplicate unbound variables to fresh ones
* Side-effectful operations which execute even if the operation fails
* ``is`` which - as a side effect - computes an arithmetic expression and binds a variable
* unfair search so that ``ancestor_of(A, P) :- ancestor_of(A, Parent), parent_of(Parent, P). :- ancestor_of(x,y)`` diverges e switching the order of the goals does not
* Meta-programming which allows querying or modifying clauses at run time, such as nth_clause, assert, retract

These features expose the search strategy's order of trying clauses and mean the denotational semantics of programs must include the search strategy's implementation and any goal side effects. Programs that heavily use non-relational features are best understood as an imperative execution model with embedded backtracking.Backtracking can re-execute side-effectful operations, so Prolog uses a simple depth-first search strategy in an effort to make the imperative semantics comprehensible.

`Merritt <https://www.amzi.com/articles/prolog_under_the_hood.htm>`__ presents an execution model as follows. A goal is of type ``Goal = {call : Entry, redo : Entry }; Entry = {exit: Entry, fail : Entry} -> Exit; Exit = IO ()``. The composition ``A ; B`` of two goals is::

  comp A B = Goal { call = \exit fail -> a, redo = \exit fail -> d } where
    a = A.call b f
    b = B.call c e
    c = exit
    d = B.redo c e
    e = A.redo b f
    f = fail

Various examples of goals::

  write X = { call = {print X; exit} ; redo = fail }
  fail = { call = fail ; redo = fail }
  cut = { call = exit ; redo = abort_goal }
  unify X Y =
    r = newGoalId;
    tryNext =
      if (u = pop unifiers)
        pushChoicePoint r (tail unifiers)
        unify X u
        exit
      else
        fail
    return {
      call =
        (X, Y) = lookupOrAllocVars (X,Y)
        unifiers = unificationAlgo X Y
        pushChoicePoint r unifiers
        tryNext
      redo =
        unifiers = popChoicePoint r
        tryNext
    }

  predicate X =
    r = newGoalId
    tryNext =
      if (u = pop unifiers)
        pushChoicePoint r (tail unifiers)
        unify X u
        exit
      else
        fail
    return {
      call =
        unifiers = findClauses X >>= findUnifiers
        pushChoicePoint r unifiers
        tryNext
      redo =
        unifiers = popChoicePoint r
        tryNext
    }

The general advice is to use non-relational features `sparingly <http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html>`__ and only if you can justify the need based on performance. :cite:`byrdRelationalProgrammingMinikanren2009` shows that, for a sample program, non-relational features can be completely avoided. Cut can almost always be replaced with a tagging scheme that makes the matching clause unambiguous, or more expressive constraints. Byrd says there is no complete method for avoiding copy-term, but in his example it can be replaced by using templates with unique names and substituting these with logic variables.

Overall it seems that relational programming covers all the cases of logic programming that people care about. Relational programming has much clearer semantics. These non-relational features are antipatterns: implementation hacks for cases where the compiler is not sufficiently smart or the constraint language is not sufficiently expressive. Mercury has eliminated impure features. :cite:`hendersonDeterminismAnalysisMercury1996`

Modes
=====

Mercury has `modes <https://www.mercurylang.org/information/doc-latest/mercury_ref/Modes.html#Modes>`__. An instantiation state is either "free", a unbound variable "distinct" in that it does not appear anywhere else, or "bound", a mapping from possible function symbols to instantiation states of the symbols' arguments. A mode is a mapping from initial instantiation states to final instantiation states, with the constraint that no node is transformed from bound to free. The two standard modes are:

* ``in == ground >> ground.``
* ``out == free >> ground.``

There are other states, e.g. ``X`` in the term ``[X,X]`` is neither free nor bound, hence Mercury's mode system is incomplete.  I think this deficiency can be fixed by allowing constrained modes, at the expense of making the definition of modes even more complicated.

Mercury also categorises each mode of a predicate according to how many times it can succeed:

* deterministic: exactly one solution
* semideterministic: no solutions or one solution
* multisolution: at least one solution
* nondeterministic: zero, one, or more solutions
* failure/erroneous: no solution, always fails/errors

For example append can work in several modes:

* predicate (in, in, in), semideterministic: ``append [a,b] [c] [a,b,c] --> yes``
* function (in, in, out), deterministic: ``append [a,b] [c] Z --> Z = [a,b,c]``
* match left (out, in, in), semideterministic: ``append X [c] [a,b,c] --> X = [a,b]``
* match both (out, out, in), nondeterministic: ``append X Y [a,b,c] --> X=[],Y=[a,b,c];X=[a],Y=[b,c];X=[a,b],Y=[c],X=[a,b,c],Y=[]``
* match all (out, out, out), nondeterministic: ``append X Y Z --> X=[],Y=[],Z=[];...``

Each mode is a function from inputs to a set of outputs (or output / Maybe, in the deterministic/semideterministic case). So, characterizing all uses of predicates with mode declarations, predicates can be thought of as a collection of ad-hoc overloaded functions. Except it's not ad-hoc, because they all represent the same logical relation. Anyways, we can embed functional programming into logic programming, by a mode declaration ``(in, out), deterministic`` for each function. Similarly we can embed term rewriting, by a mode declaration ``(in, out), nondeterministic`` for the rewrite relation. The reverse is not possible - we cannot get from the behavior on a specific mode to the overall behavior of the predicate. To support logic programming in an integrated manner everything must be interpretable as a logic program.

Logic programming allows writing very concise code, although it can be unusably inefficient in some cases. For this, we can allow writing optimized imperative code, and asserting that this implements a specific mode of a predicate. Then the predicate becomes optimized. But with a smart compiler, the imperative code can be avoided most of the time, saving the need for duplication - just tune the hot cases. Similarly writing imperative code in the first place avoids the issue altogether, although it precludes most of the benefits of logic programming.

Unification
===========

Unification is the problem of finding all solutions to equations ``a1=b1, a2=b2, ...`` over tree terms and variables. This can be extended to the "dual unification" problem that also includes disequations ``c1 != d1`` in the list that must not be satisfied. The solution takes the form of a complete set of unifiers, where each unifier is a substitution that may have its free variables substituted to obtain a solution. A substitution is a unification problem where the left sides are all variables and those variables do not appear in the right sides.

Unification isn't actually a core concept of logic programing AFAICT, as e.g. constraint logic programming on reals doesn't use it (it uses systems of equalities of reals). But syntax trees require first-order unification to solve all the equalities that arise, so it's a standard technique for implementing logic programming.

The standard `unification algorithm <https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm>`__ :cite:`vukmirovicEfficientFullHigherOrder2021` works by applying reduction operations to various cases:

* delete: ``s=s`` is removed
* decompose: ``a s1 ... sm = a t1 ... tm`` to equations ``{s1 = t1, ..., sm = tm }``
* rigid/rigid conflict: ``a sm = b tn`` fails if a and b are different rigid heads
* dereference: ``F s1 ... sn = t`` to ``(F /. σ) ... = t``, if the substitution σ from another equation maps F
* empty equation list: trivially soluble
* alpha/eta normalization: ``λxm.s = λyn.t`` to ``λxm.s = λxm.t' xn+1 . . . xm``, where ``m ≥ n``, ``xi`` disjoint from ``yj``, and ``t' = t /. {y1 → x1 , ... , yn → xn }``
* beta normalization: reduce left/right to hnf
* under lambda: apply rule for ``a = b`` to ``λx. a = λx. b``

There are more complex reductions for hard cases:

* oracle fail: ``s=t`` fails if oracle determines to be insoluble
* oracle success: ``s=t`` has finite CSU, branch to each solution σ_i
* bind: try projections with the following binding substitutions:

  * flex-rigid ``P(λx. F s = λx. a t)``: try an imitation of a for F, if a is constant, and all Huet-style projections for F, if F is not an identification variable.
  * flex-flex with different heads ``P(λx. F s = λx. G t)``: all identifications and iterations for both F and G, and all JP-style projections for non-identification variables among F and G.
  * flex-flex with identical heads and the head is an elimination variable, ``P(λx. s = λx. t)``: no bindings.
  * flex-flex with identical heads, ``P(λx. F s = λx. F t)``: all iterations for F at arguments of functional
type and all eliminations for F.

Trying all the bindings is slow, but a good set of oracles makes the algorithm efficient in practice. Of course it would be better to find reduction rules that solve things generally rather than oracles which work on specific cases, but this is hard.

Perspectives
============

Based on:
* `Merritt <https://www.amzi.com/articles/prolog_under_the_hood.htm>`__, user of Prolog for "serious applications"
* `Byrd <https://stackoverflow.com/questions/28467011/what-are-the-main-technical-differences-between-prolog-and-minikanren-with-resp>`__, author of miniKanren
* `Reddit thread <https://www.reddit.com/r/ProgrammingLanguages/comments/9kb9z5/logic_programming_languages/>`__, particularly Paul Bone who did his PhD "Automatic Parallelism in Mercury")
* `HN thread <https://news.ycombinator.com/item?id=14439137>`__

Answer-set programming (ASP) rebases the solving process onto SMT/SAT-style propositional solvers. The semantics of ASP give a conventional first-order logical semantics to Prolog. Prolog's semantics are a bit obscure, because it's "whatever SLDNF gives you", which includes things like queries not terminating.

ASP is based on the "stable-model semantics", which competed with the "well-founded semantics". Existing practical tools only implement propositional solvers, not first-order logic - they work by first "grounding" the first-order formulae to a propositional representation, and then solving them. Compared to SLDNF this can cause blow-up or speed-up but under a finite domain assumption it gives the same results.

