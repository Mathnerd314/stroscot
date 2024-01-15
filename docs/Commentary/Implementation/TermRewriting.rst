Term rewriting
##############

Higher-order matching
=====================

As a consequence of confluence of the substitution calculus, each rewrite step is composed of an expansion in the substitution calculus, a replacement by applying some rule, and a reduction in the substitution calculus, so it is M <<- C[l] and C[r] ->> N. Handling lambdas in RHSs is fairly straightforward, just treat beta-reduction as a normal reduction step and handle it with the evaluation machinery. But for the lambdas on the left hand side, in the pattern, it is more complex.

Finding the contexts ``C`` is fairly straightforward, just enumerate all the subterms of ``t``. But solving the equation ``s = lθ`` is an instance of higher-order unification (specifically higher-order matching). The complexity of higher order matching is somewhere around :math:`{\mathcal {E}}^{4}`, "the minimal solution is of size at most 2^2^2^2..., the number of 2's proportional to the size of the problem". :cite:`stirlingDecidabilityHigherorderMatching2009` That proof is for the simply typed lambda calculus but the only properties of the STLC used are strong normalization and that terms have a defined eta long form (canonical form), so it is most likely also applicable to all lambda terms with unique normal forms. Naturally determining the normal form / lack of a normal form is of complexity :math:`\Sigma_0^1`, but most lambdas in programs are in normal form already.

There are two main possibilities for implementing higher-order matching. One is to read Stirling's paper and extract an algorithm. He says "implicit in the analysis are positive sensible algorithms for dual interpolation problems", so there is definitely an algorithm to extract. Another is to implement a more general algorithm for solving higher-order unification as opposed to matching, along the lines of Gérard Huet's preunification. There is a semi-algorithm with good performance, :cite:`vukmirovicEfficientFullHigherorder2021`. The implementation is `open source <https://github.com/sneeuwballen/zipperposition/blob/2889c1f0831f01e8e2f8ffabd5fd12b758ba6a30/src/core/JPFull.ml>`__ and only a few hundred lines.

Stirling's method, involving dual interpolation. 3 transformations, and reduction to a tiling game, is conceptually complex. Extracting a usable algorithm from the paper seems like it will be challenging and time-consuming. It is also not clear if the resulting algorithm will be any better than Huet's; it is at least guaranteed to terminate, but perhaps Huet's algorithm terminates for matching problems as well.

In contrast, Huet's algorithm is basically off-the-shelf. I will need unification anyways, for purposes such as determining if rules can overlap. Also the approach in Zipperposition allows plugging in "oracles" - I think Stirling's method can be plugged in as such an oracle. There is the issue of unification being :math:`\Sigma_0^1` and needing to deal with failed unifications but I think Huet's approach makes more sense to start with.

Unification
-----------

Unification is the problem of finding all solutions to a system of equations. First-order unification solves a set of equalities ``a1=b1, a2=b2, ...`` over tree terms and variables. This can be extended to the "dual unification" problem that also includes disequations ``c1 != d1`` in the list that must not be satisfied. Constraint logic programming requires solving systems of equations over reals or other sets. The solution takes the form of a complete set of unifiers, where each unifier is a substitution that may have its free variables substituted to obtain a solution, together with constraints over those free variables. A substitution is a set of assignments from variables to expressions.

Unification isn't really part of the semantics of logic programming, as the semantics is phrased in terms of satisfiability. But it is a standard technique used in implementing logic programming, and in practice the implementation defines the semantics. Prolog only implements first-order unification. Teyjus / λProlog limit to higher-order "pattern lambdas". With ZipperPosition :cite:`vukmirovicEfficientFullHigherorder2021` there is outlined a full higher-order unification algorithm extending Huet's semi-algorithm - the need to support multiple unifiers for a complete set complicates things a bit.

The outline of every unification algorithm is that it randomly applies simplifying reduction operations to an equation until it results in a substitution, then applies the substitution to the remaining equations (dereferencing). Here we show :cite:`vukmirovicEfficientFullHigherorder2021`'s, adapted to match the presentation on `Wikipedia <https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm>`__:

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


Cycle detection
===============

A first strategy for handling cycles is to prove the system is acyclic. Per :cite:`ketemaViciousCirclesRewriting2005` this includes orthogonal weakly head normalizing higher-order TRSs.

For general-purpose detection there are SCC computation algorithms; Wikipedia has a `list <https://en.wikipedia.org/wiki/Strongly_connected_component#Algorithms>`__. The DFS algorithms seem most appropriate as they can naturally be maintained during the reduction graph search; finding the normal forms of a term essentially already is a DFS. Kosaraju's algorithm is not appropriate as computing the transpose / converse of the reduction relation is not easy. Comparing Tarjan and the path-based algorithms, Tarjan uses a second index (pointer) while the path-based uses a stack. The stack manipulation of the path-based algorithm is simpler to understand than the invariants of Tarjan; the Wikipedia page for Tarjan is constantly vandalized with people who do not understand it. So I would say the path-based algorithm is better.

For associativity and commutativity there are special unification algorithms, where we represent terms as lists or bags rather than trees. There are some PhD theses and so on for this, Maude has references. I would say these are optimizations and for now acyclic detection plus general-purpose cycle handling is sufficient.

Nondeterminism
==============

This sounds a bit tricky to implement but it is not too bad. We can split into two steps:

* Find a value: Evaluate the expression to a value, any value including exceptional values. Because of the meaningless term reduction, every expression will evaluate to some kind of value. The search should be biased towards finding non-exception values but it does not need to be perfect, for example there are reduction strategies such as parallel outermost that are guaranteed to be normalizing for some classes of TRS. This is where cycle detection and infinite value handling come in.
* Non-determinism check: We can analyze confluence and the reduction sequence of the value to see if the expression can evaluate to anything else. If there are no other values or all other values are exceptions, we are done; otherwise, we handle the non-determinism appropriately, such as replacing an exception with a non-exception, erroring on multiple non-exception values at the top-level, collecting and returning the multiple values if inside a logical combinator such as ``allOf``, or skipping the non-determinism check entirely for ``oneOf``.

Infinitary rewriting
====================

The common notions of an ARS carry over to infinitary reductions as follows: :cite:`endrullisInfinitaryTermRewriting2014`

* transitive reduction: irreflexive kernel of reduction closure
* normal form: irreducible term
* strongly normalizing (terminating): every infinite reduction sequence has a limit
* nonterminating reduction: infinite reduction sequence with no limit or that does not reduce to its limit
* weakly normalizing (normalizing): every term has a reduction to a normal form
* confluence: if t reduces to t1 and t2, then there is a common term s such that t1 and t2 reduce to s.
* Church-Rosser: if t1 is equivalent via reductions and un-reductions to t2, then there is a common term s such that t1 and t2 reduce to s.
* normal form property w.r.t. reduction:: if u reduces to t and s, and s is a normal form, then t reduces to s
* normal form property: if t is equivalent to s and s is a normal form, then t reduces to s
* unique normalization w.r.t. reduction: if t reduces to t1 and t2, and t1, t2 are normal forms, then t1=t2
* unique normalization: if t1 is equivalent to t2, and t1, t2 are normal forms, then t1=t2

However, common theorems such as Newman's lemma do not carry over for the traditional "strong convergence" definition of closure. The examples of non-confluence do have confluent rewrites under T*, so I am not aware of any actual counterexamples, but I would like a proof that Newman's lemma holds under T* to feel assured.


Confluence
----------

Confluence has gotten a lot of attention as well and has automated provers. Confluence implies UN→; it is equivalent if the TRS is weakly normalizing. And there is an extension theorem:  Similarly a system can be shown to be UN= by presenting an extension of it that is confluent. :cite:`klopExtendedTermRewriting1991` So a UN= program is just a partially specified system. UN→ is a little more complex though. And the equivalence classes of terms are uncomputable in general so the extension is as well.

Confluence avoids situations where a system may branch into two distinct diverging states. It makes finding a normalizing strategy much easier as the strategy only has to avoid getting stuck evaluating a term infinitely (using the same rule infinitely often), as opposed to UN→ where the strategy must avoid using the wrong reduction rule at every step.

The Knuth-Bendix algorithm produces a confluent system from a set of non-oriented equations, but the rules in programs are oriented, so using this would be confusing. Not to mention that the algorithm fails often. So that's out.

A necessary condition for confluence is weak/local confluence, i.e. each critical pair is convergent. But this is not sufficient. Newman's lemma is that a terminating locally confluent TRS is confluent. But termination is quite strong. A generalization is a critical pair system :cite:`hirokawaDecreasingDiagramsRelative2009` (also called decreasingly confluent): the system must be left-linear, locally confluent, and its critical pair steps must be *relatively terminating*, i.e. the relation 'arbitrary steps followed by a critical pair step followed by arbitrary steps' is terminating. Trivial critical pair steps can be excluded, hence this includes weakly orthogonal TRSs. For a terminating TRS the TRS syntactic equality notion is equivalent to strict equality, hence the system is left linear in the CTRS sense, hence why this includes Newman's lemma.

We say → has random descent (RD), if for each R:a ↔∗b with b in normal form, all maximal reductions from a have length d(R) and end in b. Systems with random descent are confluent.

Normalization
-------------


A hypernormalizing strategy is a strategy that is normalizing even if arbitrary reduction steps are taken before and after steps of the strategy. This allows the compiler to make optimizations without changing the behavior of the program. A hypernormalizing strategy allows aggressive optimizations and program transforms.

There are also stronger properties than normalization. A Church-Rosser strategy is one with common reducts, i.e. there exist m and n, such that :math:`F^m(t)=F^n(u)` for every t and u equal via forward/backward evaluation. A normalizing strategy is Church-Rosser if the system is confluent and weakly normalizing (i.e. all objects have a normal form). In general a many-step CR strategy exists for effective ARS's, i.e. countable (in a computable fashion) and with a computable reduction relation. But the strategy is quite hard to compute, as it has to synchronize reducing subterms so that all components are reduced the same amount. And it's not clear that this synchronization offers anything to the programmer.

Cofinal strategies are weaker than Church-Rosser but stronger than normalizing: for every term a, if a reduces in a finite number of steps to b, then there is an object c obtained by applying the strategy some number of times to a such that b reduces to c. For critical pair TRSs any "fair" strategy that ensures every redex is eventually contracted is cofinal. The cofinal property provides slick proofs - it ensures every redex not part of a cycle is contracted. But at runtime non-normalizing terms have indistinguishable behavior (infinite loop), hence this means the cofinal strategy is doing unnecessary work.

There are also termination properties like strong convergence that ensure that for every term, there exists some number of reduction steps after which the head cannot be rewritten.
To ensure that term rewriting halts we probably also want a property like strong convergence, but this is a property of the rewriting strategy, not the TRS proper.

Evaluation strategy
===================

For convergent (confluent and strongly normalizing) programs, such as the simply typed lambda calculus, all strategies are normalizing and the result is the same no matter how they are reduced. So the focus is on inferring convergence and doing reduction efficiently. "In the small" leftmost innermost ensures "complete development", i.e. a subterm is reduced completely before the outer term, hence we can compute the subterm fully and only store an optimized representation of the normal form. So we can compile to fast assembly like a state machine. "In the large" optimal reduction ensures the smallest number of steps so we can avoid duplicating work and performing unneeded work.

But strongly normalizing implies not Turing complete, hence the termination verification will cause problems for complex programs. We need a fallback for these complex programs. Leftmost outermost reduction is the basis of lazy evaluation and is hypernormalizing for the lambda calculus. But for TRSs LO is only normalizing for left-normal TRSs, where variables do not precede function symbols in the left-hand sides of the rewrite rule. A better strategy is outermost fair (ensuring each outermost redex will eventually be evaluated - the simplest example is parallel outermost) - it's hypernormalizing for critical pair TRSs (decreasingly confluent TRSs), in particular weakly orthogonal TRSs. :cite:`hirokawaStrategiesDecreasinglyConfluent2011` So outermost fair seems a reasonable default, but there are non-orthogonal systems where it fails. The optimal reduction stuff is defined for match sequential TRSs but is a normalizing strategy that computes a result in the smallest number of reduction steps.

We could do user-specified strategies like Stratego, but then how would we know that they're normalizing.

There are is also lenient evaluation which evaluates all redexes in parallel except inside the arms of conditionals and inside lambdas, but it adds extra memory overhead for parameter passing.

Now, one can argue about which computational strategy is better (time, space, parallelism, ...)
Stroscot: be accepting of programs, ensure a normalizing strategy. But after that aim for most efficient in time/space for strict programs.

Q: can normalizing be as efficient as strict
profiling, other optimization tricks

So The way we handle cycles in the rewrite engine is something like:

* detect cyclic term via rule cycle detection or presence of AC operator
* use specialized matching (eg AC matching or Tarjan SCC + memo hash table) to identify all reductions out of SCC
* end with condensed normal form if no reduction out of SCC
* otherwise, pick a reduction out of the SCC

Then this infinite term is computed in chunks and fed to the surrounding context on demand (laziness), ensuring that a finite normal form is reached if possible and otherwise implementing an infinite stream of commands.

If the substitution calculus is convergent, then terms can be represented by preterms in normal form.

If reduction does not end in a condensed normal form, then the sequence of terms must be infinitely expanding in the sense that for every size s there is a point in the reduction where terms are always at least size s. Otherwise, assuming a finite number of term symbols, there are only finitely many terms of size < s, so there would be a cycle in the reduction and reduction would end in a condensed normal form.

A context is linear if every hole occurs exactly once.

Verifying confluence
====================

We often want to prove confluence. There are some key algorithms:

* The decreasing diagrams technique is a complete method for confluence on countable abstract rewrite systems.

* Computing critical pairs. A non-joinable critical pair means the system is not confluent. If all critical pairs are joinable the system is said to be locally confluent. An orthogonal system is one with no critical pairs, while a weakly orthogonal system is one with critical pairs that are trivially joinable. For an HORS there are more constraints to be orthogonal in addition to no critical pairs ("every set of redexes is pairwise simultaneous"). The substitution calculus must be complete, only needed for gluing, a descendant rewriting system, parametric, have head-defined rules, and be naturally closed under substitution. Parallel rewrite steps must be serializable and left-hand sides of rules must be linear.

  V. van Oostrom. Developing developments. TCS, 175(1):159–181, 1997.
  V. van Oostrom and F. van Raamsdonk. Weak orthogonality implies confluence: The higher order case. In Proc. 3rd LFCS, volume 813 of LNCS, pages 379–392, 1994.

* Proving termination. The Knuth Bendix Criterion (Newmann's lemma) says a terminating system is confluent iff it is locally confluent. Termination can be shown by exhibiting a well-ordering, such as recursive path ordering, dependency graph decomposition, and the subterm criterion.

  WANDA has more advanced techniques. Cynthia Kop. Higher Order Termination. PhD thesis, Vrije Universiteit, Amsterdam, 2012

  TTT2 also has some good techniques.

  Gramlich–Ohlebusch’s criterion says for innermost-terminating TRSs R with no innermost critical pairs, R is confluent if and only if all critical pairs are joinable by innermost reduction. There are innermost terminating systems that aren't terminating so this criterion can prove some systems that Knuth-Bendix can't.

* Decomposition: Several properties allow dividing the system into smaller, more tractable systems. First is modularity, that the disjoint union of two systems with the property has the property. We also usually have the converse, the disjoint union has the property only if the subsystems have the property.

  * Weak normalization and consistency (w.r.t. equivalence) are modular for first-order systems.
  * Left linearity, confluence, and unique normal forms (w.r.t. equivalence) are modular for semi-equational CTRSs.
  * Confluence is modular for join and semi-equational CTRSs. In fact if the disjoint union is confluent then the component systems must be confluent.
  * Confluence plus left linearity is modular for higher-order TRSs.
  * Weak termination, weak innermost termination, and strong innermost termination are modular for CTRSs in combination with confluence or the property that there are no extra variables in the conditions.
  * NF, unique normal forms with respect to reduction, and consistency with respect to reduction are modular in combination with left linearity. Consistency w.r.t. reduction means that there is no term reducing to two distinct variables; it is implied by the unique normal form property w.r.t. reduction as variables are normal forms.
  * Strong normalization plus consistency w.r.t. reduction plus left linearity is modular. This likely holds for CTRSs without extra variables as well.

  Order-sorted decomposition uses persistence of confluence. If sorts can be assigned to all terms and rule variables such that all rules don't increase the sort, then confluence can be separately considered for each sort and confluence as a whole follows from confluence on well-sorted terms.

  Decreasing diagrams allows decomposing a left-linear TRS into duplicating and non-duplicating rules. The TRS is confluent if all critical peaks are decreasing with respect to a rule labeling and the duplicating rules are terminating relative to the non-terminating rules.

  Layer-preserving decomposition decomposes TRSs into minimal pieces such that taking pieces pairwise they form layer-preserving combinations, i.e. rules in one piece operate only on terms of that piece. It is used in CSI.


* J. Nagele, B. Felgenhauer, and A. Middeldorp. Improving automatic confluence analysis of rewrite systems by redundant rules. In Proc. 26th RTA, volume 36 of LIPIcs, pages 257–268, 2015.

