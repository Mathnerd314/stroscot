Dispatch
#########

Overview
========

Stroscot is based on a term rewriting formalism and supports higher order terms, pattern matching, and predicate dispatch (conditional rewriting). In general rules are not ordered - they overlap and run in parallel. If multiple rules match, all cases are tried nondeterministically and run. It is required that for each possibility the program either throws an exception or produces the same output.

Local functions are applied in the same way as global ones, i.e., the argument patterns of each rule are matched against the actual function arguments nondeterministically and optimal reduction is performed.

If none of the rules match then the term does not reduce (it becomes a normal form) - to raise an exception a catch-all clause must be defined.

Formal definition
=================

The substitution calculus consists of:

* A set A of preterms, defined to be the sequent trees of Stroscot's core logic.
* The binary relation ``R1 : A times A`` of cut elimination (actually a function), forming an abstract rewriting system on the preterms.
* A countably infinite subset ``V`` of preterms called variables, in particular applications of the Use rule.
* A substitution operation ``sθ`` taking a preterm ``s : A`` and a mapping ``θ : V -> A`` and producing another preterm. A finite mapping which maps variables ``xi`` to preterms ``ti`` and leaves all other variables unchanged is written as ``[x1 := t1, ..., xn := tn]``. The substitution should be a homomorphism on ``A``, i.e. for each syntactic construct of ``A`` such as ``s = f a`` (application) or ``s = \x -> a`` (abstraction) we should have ``sθ = (fθ) (aθ) `` or ``sθ = \x -> aθ``.

Finite terms ``T_f`` are representatives of equivalence classes of preterms under equivalence ``E = (R1 union R1 inverse)*`` of the substitution calculus. A variable ``x`` is free in a term ``t`` if ``t[x:=s] != t`` for some preterm ``s``. A term with no free variables is called closed, otherwise open. A context is a term with a distinguished free variable ``□`` called the hole. The notation ``C[x]`` where ``C`` is a context means ``C[□ := x]``.

The set of terms ``T`` includes both finite and infinite terms, and is defined as the `metric completion <https://en.wikipedia.org/wiki/Complete_metric_space#Completion>`__ of finite terms with a distance function :math:`2^{-n}` if the n-th level of the terms is the first level where a difference appears and 0 if the terms are equal. As a convention the top level is level zero.

A rewriting system consists of a substitution calculus and a set of conditional rewrite rules ``RL : T times P times T``, where ``P`` are predicates ``(T times T) times T times (V -> A)`` depending on the rewrite relation, left hand side, and match substitution.

The single-step rewrite relation ``R2 : (T times T) -> T times T`` is defined as ``\R -> { (t,C[rθ]) | C : context, θ : V -> A, t E C[lθ], (R,t,θ) in p, (l,p,r) in RL }``. The reflexive transitive closure of reduction ``R2*`` is defined as, given ``R``, the smallest set containing ``R2`` that is reflexively, transitively, and topologically closed.

We then condense the relation: an SCC is a set S of terms such that for all s in S and terms t, ``s R2* t`` and ``t R2* s`` implies t in S. We define a new condensed reduction relation ``R3`` on terms, such that ``x R3 y`` given ``R`` iff, letting A be the SCC of x and B the SCC of y, exists a in A, b in B such that a R2* b, A neq B. ``R3`` is acyclic, since the condensed terms are maximal, and transitively but not reflexively closed.

The set of meaningless terms ``U subset T`` is defined w.r.t. ``R3`` given ``R`` to be the smallest set with the following properties:

* Contains all root-active terms. A term t is root-active if every reduct of t can be reduced to a term with a top-level redex.
* Closure under reduction. If ``M ∈ U``, ``M → N`` then ``N ∈ U``.
* Closure under substitution. For all ``M ∈ U``, ``M /. σ ∈ U``
* Overlap. If a redex t overlaps a subterm, and this subterm is in U, then t in U.
* Indiscernibility - the meaningfullness of a term does not depend on its meaningless subterms. For all M, N, if N can be obtained from M by replacing a set of pairwise disjoint subterms in U with other terms of U, then M ∈ U if and only if N ∈ U.
* Topological closure

We extend ``R3`` with reductions from meaningless terms to the ``Meaningless`` exception to obtain ``R4`` given ``R``. This is our final relation ``Rf``.

So far ``R`` been a parameter, so ``Rf`` is a function ``(T times T) -> T times T``. We take the "optimal prefixedpoint", the intersection of the maximal prefixedpoints. I.e. we find the sets ``Pre = { R : R subseteq Rf(R) }, PreMax = { R in Pre : forall R' in Pre, R subseteq R' implies R= R' }, R = intersection PreMax``. We warn on all the reductions in ``Rf(R) \ R`` that are missing from ``R`` and make ``R`` not a fixed point of ``Rf``.

Evaluation an expression ``e`` first computes its set of normal forms ``NF(e) = { x : e R x, not(exists y. x R y) }``. If there are any non-exception values in ``NF(e)``, then the result is ``{x in NF(e) | not(x : Exception) }``. Otherwise if there are one or more exceptions the compiler arbitrarily chooses an exception value. If the result of evaluation is a set with one element, that element is said to be the value of the expression.

It is an error if the top-level expression does not evaluate to a value.

Conditions
==========

For the TRS we have defined a condition as a predicate, but syntactically it is more limited. I'm thinking these forms:

* a boolean guard ``| condexpr`` - the predicate holds if the expression reduces to true and does not reduce to false.

* a pattern guard ``| pat <- exp`` - the predicate holds if ``exp`` reduces to a form that is matched by ``pat`` and does not evaluate to a form not matching ``pat``. A boolean guard is almost a pattern guard ``True <- g`` but will match values like ``1 amb true``.

* a non-linear pattern like ``[x,x]`` - this is converted to a linear pattern like ``[x,y]``, and the predicate holds if ``x`` and ``y`` are in normal form and syntactically identical.

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
