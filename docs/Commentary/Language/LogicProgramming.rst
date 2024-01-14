Logic programming
#################

logic programming: express goals in a declarative form and solve problems using automated reasoning.

Motivation
==========

Misquoting :cite:`iversonNotationToolThought1980`:

    Users of computers and programming languages are often concerned primarily with the efficiency of execution of algorithms, and might, therefore, summarily dismiss [logic programming]. Such dismissal would be short-sighted since a clear statement [...] can usually be used as a basis from which one may easily derive a more efficient algorithm.

    [...]

    The practice of first developing a clear and precise definition [...] without regard to efficiency, and then using it as a guide and a test in exploring equivalent processes possessing other characteristics, such as greater efficiency, is very common in mathematics. It is a very fruitful practice which should not be blighted by premature emphasis on efficiency in computer execution.

    [...]

    Finally, overemphasis of efficiency leads to an unfortunate circularity in design: for reasons of efficiency early programming languages reflected the characteristics of the early computers, and each generation of computers reflects the needs of the programming languages of the preceding generation.

Practically, logic programming is a great tool for naturally expressing computational tasks that use logical constraints. Large programs generally run into one or two of these tasks. Without logic programming these tasks must be solved in an ad-hoc and verbose way. Compare `Sudoku with Prolog <https://www.metalevel.at/sudoku/>`__ vs `Norvig's Sudoku solution <https://norvig.com/sudopy.shtml>`__. Other examples include parsers, typecheckers, and database queries. Specifically:

* Prolog's Definite Clause Grammars, and library `pio <https://www.swi-prolog.org/pldoc/doc/_SWI_/library/pio.pl>`__, are a great parsing DSL. Furthermore the relation ``X parses_to Y`` can be flipped to get ``Y prints_to X``, automatically generating serializers from deserializers.

* With typecheckers you can just directly translate the rules to Horn clauses and it runs. Similarly language interpreters are a direct translation of their operational semantics.

* Incomplete data structures are great. ``date(2018, month, 14)`` describes every 14th day in this year, and the month can be constrained or the set of dates extracted. Furthermore with constraint logic programming sets of dates can be manipulated.

* Database queries are naturally expressed as logical operations on relations. For example pyDatalog and SQL can be called relational languages.

Semantics
=========

Relational
----------

Logic programs over classical true, false, not, ∧, ∨, ⊃, ∀, and ∃ have what :cite:`byrdRelationalProgrammingMinikanren2009` terms "relational" semantics (to be confused with relational databases). A state is a map from some set of nominal variables to their substitution, a set of ground terms. A goal is a logical predicate over some variables - applying it to a state that defines the relevant variables gives true or false. Running a program consists of computing the set of satisifable states, which may be empty or infinite.

Practically the execution engine does not return a set, but rather a finite or infinite stream of satisfying meta-states. Meta-states are states that include unbound variables representing any term, and (in constraint logic programming) constraints for these unbound variables. Ideally the stream would be a minimal completely-covering set of meta-states in some arbitrary order, but in practice implementations can return identical or overlapping results.

miniKanren uses an "interleaving" search from :cite:`kiselyovBacktrackingInterleavingTerminating2005`, which is "complete" in the sense that it explores all branches fairly and will find all answers eventually. Generally the search strategy is irrelevant so long as it is fair, so there are many other choices; we can optimize the search, or dump the problem into an SMT solver and use its search strategy. CDCL with optimizations should be the fastest.

Imperative
----------

Prolog has extended the execution engine with predicates that expose details of the underlying implementation:

* Cut (!) which commits to choices made since the parent goal was unified with the left-hand side of the clause containing the cut. miniKanren similarly includes operators conda (soft-cut) and condu (committed choice). Concurrent logic programming also has committed choice which prunes off all other branches once it is known that a clause's guard goals all succeed.
* ``var/1`` which checks if the variable is unbound
* ``copy_term/2`` which can duplicate unbound variables to fresh ones
* Side-effectful operations which execute even if the operation fails
* ``is`` which - as a side effect - computes an arithmetic expression and binds a variable
* unfair search so that ``ancestor_of(A, P) :- ancestor_of(A, Parent), parent_of(Parent, P). :- ancestor_of(x,y)`` diverges e switching the order of the goals does not
* Meta-programming which allows querying or modifying clauses at run time, such as nth_clause, assert, retract

In particular these features expose Prolog's search strategy. Prolog uses a simple depth-first search strategy, "SLD resolution", to explore clauses. This means the denotational semantics of programs must include the search strategy's implementation and any goal side effects. SLD resolution is inefficient and biased compared to more modern logic search strategies such as DPLL or CDCL. But SLD's simplicity is the main reason imperative Prolog execution is comprehensible.

Programs that heavily use imperative features and SLD resolution are best understood using an imperative execution model with embedded backtracking commands that can re-execute side-effectful operations. The imperative "Byrd Box" execution model was first described in "Understanding the control flow of Prolog programs" by Lawrence Byrd. This paper is not available online but the idea is described in many other places, e.g. `Merritt <https://www.amzi.com/articles/prolog_under_the_hood.htm>`__, and is visible in Prolog debuggers. It goes as follows. A goal is of type ``Goal = {call : Entry, redo : Entry }; Entry = {exit: Entry, fail : Entry} -> Exit; Exit = IO ()``. The composition ``A , B`` of two goals is::

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

The general advice is to use imperative features `sparingly <http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html>`__ and only if you can justify the need based on performance. :cite:`byrdRelationalProgrammingMinikanren2009` shows that, for a sample program, these features can be completely avoided. Cut can almost always be replaced with a tagging scheme that makes the matching clause unambiguous, or more expressive constraints. Byrd says there is no complete method for avoiding copy-term, but in his example it can be replaced by using templates with unique names and substituting these with logic variables.

Overall it seems that imperative features are antipatterns: band-aid implementation hacks that can be avoided by making the compiler smarter or the constraint language more expressive. Mercury has eliminated these features in favor of a state-token I/O system. :cite:`hendersonDeterminismAnalysisMercury1996`

* XSB: http://xsb.sourceforge.net/manual1/manual1.pdf

Proof-search
------------

A more general paradigm is sequent proof search, which allows all the connectives of linear logic and extensions such as infinite proof trees. A logic program consists of some list of program clauses (proof sequents) ∆ which can be seen as assumptions or axioms. There is then a goal sequent !∆, C −→ G representing a search state in which formulas ∆ are assumed, resources C are provided, and the goal is G. The logic engine then searches for proof trees which prove this sequent. Via the Curry-Howard correspondence, these proof trees correspond to programs of the type described by the sequent. As with relational programming a finite or infinite stream of satisfying programs is returned.

Since programs such as ``undefined`` trivially satisfy all goals, restrictions must be made to the space of proofs to obtain useful results. The standard restriction is to finite "cut free" proofs, which by the cut elimination theorem can prove all sequents with finite proofs. Uniform proofs are cut-free sequent proofs P such that every subproof of P is uniform and also for every non-atomic formula occurrence B in the right-hand side of the end-sequent of P, there is a proof P0 equal to P up to permutation of inference rules such that P0's last inference rule is the right introduction rule for the top-level logical connective occurring in B. The existence of uniform proofs allow a goal-directed search which starts by logically decomposing goals. It is only when the goal formula is atomic that other proof methods are used ("backchaining"). An abstract logic programming language is a system of goals, formulas, and rules such that a sequent has a proof if and only if it has a uniform proof.

Uniformity seems mainly useful for classical logic. In linear logic the dynamics of cut-free proof search can be described via Andreoli's focused proofs which alternate between "unfocused"/goal-reduction decomposition of all asynchronous formulas and "focused"/backchaining decomposition of some synchronous formula by using introduction rules for its top-level connective and all synchronous subformulas that might arise.

Answer set
----------

Answer-set programming (ASP) rebases the solving process onto SMT/SAT-style propositional solvers. ASP is based on "stable-model semantics", which competes with "program completion" and the "well-founded semantics" to define the meaning of negation. Program completion :cite:`clarkNegationFailure1978` interprets as a clause ``p :- q, not r`` as "p if and only if q and not r". A stable model is a mapping ``Prop -> {T,F}`` such that for each clause ``A :- B1, …, Bm , not C1, …, not Cn`` either some proposition ``Ci`` is true or the negation-free sequent ``B1, …, Bm |- A`` holds.

Although the semantics of ASP is conventional first-order logic, existing practical tools for ASP only implement propositional solvers, not first-order logic - they work by first "grounding" the first-order formulae to a propositional representation, and then solving them. Compared to SLDNF this can cause blow-up or speed-up but under a finite domain assumption it gives the same results.

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

Unification is the problem of finding all solutions to a system of equations. First-order unification solves a set of equalities ``a1=b1, a2=b2, ...`` over tree terms and variables. This can be extended to the "dual unification" problem that also includes disequations ``c1 != d1`` in the list that must not be satisfied. Constraint logic programming requires solving systems of equations over reals or other sets. The solution takes the form of a complete set of unifiers, where each unifier is a substitution that may have its free variables substituted to obtain a solution, together with constraints over those free variables. A substitution is a set of assignments from variables to expressions.

Unification isn't really part of the semantics of logic programming, as the semantics is phrased in terms of satisfiability. But it is a standard technique used in implementing logic programming, and in practice the implementation defines the semantics. Prolog only implements first-order unification. Teyjus / λProlog limit to higher-order "pattern lambdas". With ZipperPosition :cite:`vukmirovicEfficientFullHigherOrder2021` there is outlined a full higher-order unification algorithm extending Huet's semi-algorithm - the need to support multiple unifiers for a complete set complicates things a bit.

The outline of every unification algorithm is that it randomly applies simplifying reduction operations to an equation until it results in a substitution, then applies the substitution to the remaining equations (dereferencing). Here we show :cite:`vukmirovicEfficientFullHigherOrder2021`'s, adapted to match the presentation on `Wikipedia <https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm>`__:

* delete: ``s=s`` is removed
* decompose: ``a s1 ... sm = a t1 ... tm`` to equations ``{s1 = t1, ..., sm = tm }``
* rigid/rigid conflict: ``a sm = b tn`` fails if a and b are different rigid heads
* dereference: ``F s1 ... sn = t`` to ``(F /. σ) ... = t``, if the substitution σ from another equation maps F
* empty equation list: trivially soluble
* alpha/eta normalization: ``λxm.s = λyn.t`` to ``λxm.s = λxm.t' xn+1 . . . xm``, where ``m ≥ n``, ``xi`` disjoint from ``yj``, and ``t' = t /. {y1 → x1 , ... , yn → xn }``
* beta normalization: reduce left/right to hnf
* under lambda: apply rule for ``a = b`` to ``λx. a = λx. b``

ZipperPosition has more complex reductions for hard cases:

* oracle fail: ``s=t`` fails if oracle determines to be insoluble
* oracle success: ``s=t`` has finite CSU, branch to each solution σ_i
* bind: try projections with the following binding substitutions:

  * flex-rigid ``P(λx. F s = λx. a t)``: try an imitation of a for F, if a is constant, and all Huet-style projections for F, if F is not an identification variable.
  * flex-flex with different heads ``P(λx. F s = λx. G t)``: all identifications and iterations for both F and G, and all JP-style projections for non-identification variables among F and G.
  * flex-flex with identical heads and the head is an elimination variable, ``P(λx. s = λx. t)``: no bindings.
  * flex-flex with identical heads, ``P(λx. F s = λx. F t)``: all iterations for F at arguments of functional type and all eliminations for F.

The flex-binding step is slow, but a good set of oracles makes the algorithm efficient for most practical cases. Of course it would be better to find reduction rules that solve things generally rather than oracles which work on specific cases, but this is hard.

The unifier search can be integrated with the overall logical search for satisfiable formulas.

By default Prolog does not use the `occurs check <https://en.wikipedia.org/wiki/Occurs_check>`__ in unification. This means for ``x == f x`` the substitution ``x -> f x`` is obtained. Denotationally this can be accommodated by allowing states to contain infinite rational terms, :cite:`weijlandSemanticsLogicPrograms1990` ``x = f (f (f (...)))`` in this case. In most Prolog programs the occurs check does not make a difference and simply slows down unification. :cite:`aptWhyOccurcheckNot1992` Prolog defines a ``unify_with_occurs_check`` predicate, and has an option for doing the occurs check in the implicit unification when dispatching predicates. Meanwhile miniKanren always uses the occurs check. The occurs check is needed in first order logic theorem-proving, where skolemization turns quantifiers into variables and is sound only if the occurs check is used.


Sources
=======

Based on:
* `Byrd <https://stackoverflow.com/questions/28467011/what-are-the-main-technical-differences-between-prolog-and-minikanren-with-resp>`__, author of miniKanren
* `Reddit thread <https://www.reddit.com/r/ProgrammingLanguages/comments/9kb9z5/logic_programming_languages/>`__, particularly Paul Bone who did his PhD "Automatic Parallelism in Mercury")
* `HN thread <https://news.ycombinator.com/item?id=14439137>`__
