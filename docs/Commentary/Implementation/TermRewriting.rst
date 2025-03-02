Term rewriting
##############

Logic programming
=================

Now in some sense using logic programming is a "two problems" approach. "Some people, when confronted with term rewriting, think 'I know, I'll use logic programming.' Now they have two problems." Phrasing term rewriting as a logical relation doesn't necessarily make it any easier to implement term rewriting - you still have to compute the transitive closure, do proper indexing and caching, figure out an evaluation strategy, think about optimizations, and think about parallel/concurrent execution of "sparks". It is just that now you are implementing these as part of the logical engine, rather than the term rewriting engine, so everything is slightly munged and harder to recognize.

That being said, ChatGPT says logic programming is a "natural fit for rule-based systems", particularly with the built-in backtracking, so it does seem that using logic programming techniques like CDCL could be beneficial. Optimizing the computation of reflexive transitive closures is a general problem that shows up in logic programming all the time, not just in term rewriting. And the pattern matching stuff of term rewriting is mostly syntax - pawning it off to a library that translates the patterns to logical constraints makes a lot of sense. Specifically for constraints that quantify over variables not bound in the left hand pattern, it pretty much is logic programming and there is not really any method of execution other than "feed this constraint into a logic analyzer and identify the values for which it is satisfiable".

Higher-order matching
=====================

As a consequence of confluence of the substitution calculus, each rewrite step is composed of an expansion in the substitution calculus, a replacement by applying some rule, and a reduction in the substitution calculus, so it is the predicate M <<- C[l] and C[r] ->> N. Handling lambdas in RHSs is fairly straightforward, just treat beta-reduction as a normal reduction step and handle it with the evaluation machinery. But for the lambdas on the left hand side, in the pattern, it is more complex.

Finding the contexts ``C`` is fairly straightforward, just enumerate all the subterms of ``t``. But solving the equation ``s = lθ`` is an instance of higher-order unification (specifically higher-order matching). The complexity of higher order matching is somewhere around :math:`{\mathcal {E}}^{4}`, "the minimal solution is of size at most 2^2^2^2..., the number of 2's proportional to the size of the problem". :cite:`stirlingDecidabilityHigherorderMatching2009` That proof is for the simply typed lambda calculus but the only properties of the STLC used are strong normalization and that terms have a defined eta long form (canonical form), so it is most likely also applicable to all lambda terms with unique normal forms. Naturally determining the normal form / lack of a normal form is of complexity :math:`\Sigma_0^1`, but most lambdas in programs are in normal form already or close to it.

There are two main possibilities for implementing higher-order matching. One is to read Stirling's paper and extract an algorithm. He says "implicit in the analysis are positive sensible algorithms for dual interpolation problems", so there is definitely an algorithm to extract. Another is to implement a more general algorithm for solving higher-order unification as opposed to matching, along the lines of Gérard Huet's preunification. There is a semi-algorithm with good performance, :cite:`vukmirovicEfficientFullHigherorder2021`. The implementation is `open source <https://github.com/sneeuwballen/zipperposition/blob/2889c1f0831f01e8e2f8ffabd5fd12b758ba6a30/src/core/JPFull.ml>`__ and only a few hundred lines.

Stirling's method, involving dual interpolation, 3 transformations, and reduction to a tiling game, is conceptually complex. Extracting a usable algorithm from the paper seems like it will be challenging and time-consuming. It is also not clear if the resulting algorithm will be any better than Huet's; it is at least guaranteed to terminate, but perhaps Huet's algorithm terminates for matching problems as well.

In contrast, Huet's algorithm is basically off-the-shelf. I will need unification anyways, for purposes such as logic programming and determining if rules can overlap. Also the approach in Zipperposition allows plugging in "oracles" - I think Stirling's method can be plugged in as such an oracle. There is the issue of unification being :math:`\Sigma_0^1` and needing to deal with failed unifications but I think starting with Huet's approach makes more sense.

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

By default Prolog does not use the `occurs check <https://en.wikipedia.org/wiki/Occurs_check>`__ in unification. This means for ``x == f x`` the substitution ``x -> f x`` is obtained. Denotationally this can be accommodated by allowing states to contain infinite rational terms, :cite:`weijlandSemanticsLogicPrograms1990` ``x = f (f (f (...)))`` in this case. In most Prolog programs the occurs check does not make a difference and simply slows down unification. :cite:`aptWhyOccurcheckNot1992` Prolog defines a ``unify_with_occurs_check`` predicate, and has an option for doing the occurs check in the implicit unification when dispatching predicates. Meanwhile miniKanren always uses the occurs check. The occurs check is needed in first order logic theorem-proving, where skolemization turns quantifiers into variables and is sound only if the occurs check is used. For Stroscot, we have infinite terms so skipping the occurs check makes perfect sense. It might be worth having it as a flag though in case the ``unify_with_occurs_check`` is needed for the skolemization thing. I don't think we're going to do much with quantifiers in Stroscot though? And there are other techniques for handling quantifiers besides skolemization.

Cycle detection
===============

A first strategy for handling cycles is to prove the system is acyclic. Per :cite:`ketemaViciousCirclesRewriting2005` this includes orthogonal weakly head normalizing higher-order TRSs. Also in general any terminating system is acyclic, so we can use all the techniques from the `TERMCOMP <https://termination-portal.org/wiki/Termination_Competition_2023>`__ term-rewriting category. There is probably some room for adapting termination techniques, acylic-ness is a weaker property than termination (local rather than global) so it should be easier to prove.

For general-purpose detection there are SCC computation algorithms; Wikipedia has a `list <https://en.wikipedia.org/wiki/Strongly_connected_component#Algorithms>`__. The DFS algorithms seem most appropriate as they can naturally be maintained during the reduction graph search; finding the normal forms of a term essentially already is a DFS. Kosaraju's algorithm is not appropriate as computing the transpose / converse of the reduction relation is not easy. Comparing Tarjan and the path-based algorithms, Tarjan uses a second index (pointer) while the path-based uses a stack. The stack manipulation of the path-based algorithm is simpler to understand than the invariants of Tarjan; the Wikipedia page for Tarjan is constantly vandalized with people who do not understand it. So I would say the path-based algorithm is better.

For associativity and commutativity there are special unification algorithms, where we represent terms as lists or bags rather than trees. There are some PhD theses and so on for this, Maude has references. I would say these are optimizations and for now acyclic detection plus general-purpose cycle handling is sufficient.

Nondeterminism
==============

This sounds a bit tricky to implement but it is not too bad. An analysis of nondeterminism starts with analyzing confluence. There are various equivalent ways of specifying confluence:

* (Standard definition) :math:`a \in S`` is deemed confluent if for all pairs :math:`b,c\in S` such that :math:`a \overset{*}{\to} b` and :math:`a \overset{*}{\to} c`, there exists :math:`d \in S` with :math:`b \overset{*}{\to} d` and :math:`c \overset{*}{\to} d`. If every :math:`a \in S` is confluent, we say that :math:`\to` is confluent.
* (Church-Rosser) :math:`\to` is confluent if :math:`x \overset{*}{\leftrightarrow} y` implies that there exists a :math:`z \in S` with :math:`x\overset{*}{\to}z` and :math:`y\overset{*}{\to}z`.
* (Semi-confluence) :math:`a \in S`` is deemed confluent if for all pairs :math:`b,c\in S` such that :math:`a \to b` and :math:`a \overset{*}{\to} c`, there exists :math:`d \in S` with :math:`b \overset{*}{\to} d` and :math:`c \overset{*}{\to} d`. If every :math:`a \in S` is confluent, we say that :math:`\to` is confluent.

You might not see that these are equivalent at first glance but check out :cite:`baaderTermRewritingAll1998` Theorem 2.1.5, basically you prove semi-confluence implies Church-Rosser by inducting on the length of the chain of :math:`x \overset{*}{\leftrightarrow} y`. I think in the case of our infinitary T* relation there is a similar proof which goes over whether the relation holds by the reflexive, transitive, one-step, or topological closure properties.

Another way of putting confluence is that a confluent element reduces to at most one normal form. Now due to our handling of meaningless terms, the TRS is strongly normalizing and every element has at least one normal form. Therefore, a confluent element corresponds directly with a value, it is just a matter of computing that value. Similarly a non-confluent element is nondeterministic and evaluates to multiples values.

There are some other properties in the literature like NF, UN, and UN→ (c.f. `Wikipedia <https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)>`__). Since the TRS is strongly normalizing these are equivalent to confluence; confluence is the most well-studied so that is what I am focusing on, but maybe there are some papers on the other properties that are applicable.

The Knuth-Bendix algorithm produces a confluent system from a set of non-oriented equations, but the rules in programs are oriented, so using this would be confusing. Not to mention that the algorithm fails often. So that's out.

Local confluence
----------------

A necessary condition for confluence is weak/local confluence, i.e. that each critical pair is convergent. But this is not sufficient in general, due to nonterminating systems like ``b = c; c = b; b = a; c = d``. Newman's lemma states that a finite locally confluent strongly normalizing TRS is confluent. There is also :cite:`hirokawaDecreasingDiagramsRelative2009` which shows that left-linear locally confluent "decreasing critical pair" systems, i.e. one where its critical pair steps are *relatively terminating*, i.e. the relation 'arbitrary steps followed by a critical pair step followed by arbitrary steps' is terminating, is convergent. Also trivial critical pair steps can be excluded, hence this includes weakly orthogonal TRSs.

In the infinitary case, Newman's lemma does not apply for the traditional "strong convergence" definition, but with Stroscot's advanced normalization I am not sure. The examples of non-confluence given in :cite:`klopInfinitaryNormalization2005` do have confluent rewrites under T*. Let's try to sketch a proof that Newman's lemma holds under T*. Suppose we have a T*-closed rewriting system that is locally confluent and terminating. Then for contradiction suppose we have a non-confluent element, i.e. there are :math:`a,b,c` such that :math:`a \overset{*}{\to} b` and :math:`a \overset{*}{\to} c` but there does exist any element :math:`d \in S` with :math:`b \overset{*}{\to} d` and :math:`c \overset{*}{\to} d`. Consider the set of such non-confluent elements and order them by the ordinal length of the reduction :math:`a \overset{*}{\to} c`; consider the tuple with minimal length. Then by definition of T*, if :math:`a \overset{*}{\to} c`, we either have:

* :math:`a=c`. Then :math:`c\to b` and we are done.
* :math:`a\overset{*}{\to}c'`, :math:`c'\overset{*}{\to}c`. First ask if :math:`a,b,c'` is a non-confluent triple. If it is non-confluent, then we have a smaller counterexample. Therefore, we must have an element :math:`v` such that :math:`b\overset{*}{\to}v` and :math:`c'\overset{*}{\to}v`. Then consider :math:`c',v,c`; again if it is non-confluent then we have a smaller counterexample. Therefore, there is an element :math:`w` with :math:`v\overset{*}{\to}w` and :math:`c\overset{*}{\to}w`. But then :math:`a\overset{*}{\to}c'\overset{*}{\to}c\overset{*}{\to}w` and :math:`a\overset{*}{\to}b\overset{*}{\to}v\overset{*}{\to}w` and the original example is confluent.
* :math:`a \to b` and :math:`a \to c`: this contradicts local confluence
* :math:`a \to b` and there is a sequence of relations :math:`a_i \overset{*}{\to} c_i` such that :math:`\lim d(a_i,a)^2 + d(c_i,c)^2 = 0`. Consider the relaxed relation :math:`\overset{*}{\to}_\epsilon`. Then for every :math:`\epsilon` there is an :math:`i` such that :math:`a \overset{*}{\to}_\epsilon \c_j` for :math:`j \geq i`. By some sort of induction hypothesis :math:`\overset{*}{\to}_\epsilon` is confluent so for every :math:`c_j` there is a :math:`d_j` with :math:`b \overset{*}{\to}_\epsilon d_j` and :math:`c_j \overset{*}{\to}_\epsilon d_j`. Then there is a point of closure :math:`d` of :math:`d_j` and :math:`\{b,c\} \overset{*}{\to} d`.
* two sequences of relations :math:`a_1_i \overset{*}{\to} b_i` and :math:`a_2_i \overset{*}{\to} c_i`

Now for non-confluent elements, I don't see much choice except to enumerate all the values. It would be nice to optimize exceptions somehow and avoid fully evaluating expressions when they evaluate to an exception and we know there is a non-exception value, but in general we can't predict the results of evaluation so we will have to examine all normal forms / evaluation paths exhaustively. From analyzing local confluence and critical pairs, we can at least identify a small set of backtracking points, choices in which rule is applied, namely the non-convergent critical pairs. This analysis is a bit conservative in that a non-convergent critical pair could still converge on a specific expression, but at that point we're well into the weeds of fine-grained optimization.



 Newman's lemma is that a terminating locally confluent TRS is confluent. But termination is quite strong.  For a terminating TRS the TRS syntactic equality notion is equivalent to strict equality, hence the system is left linear in the CTRS sense, hence why this includes Newman's lemma.

We say → has random descent (RD), if for each R:a ↔∗b with b in normal form, all maximal reductions from a have length d(R) and end in b. Systems with random descent are confluent.

Verifying confluence
--------------------

As confluence is such a useful property, we want to be able to automatically determine confluence. Fortunately, confluence has gotten significant attention and has automated provers; there is the yearly-ish `Confluence competition <http://project-coco.uibk.ac.at/2023/>`__. There are some key algorithms:

* The decreasing diagrams technique is a complete "pen-and-paper" method for confluence on countable abstract rewrite systems.

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


Infinitary rewriting
====================

The common notions of an ARS carry over to infinitary reductions as follows: :cite:`endrullisInfinitaryTermRewriting2014`

* transitive reduction: irreflexive kernel of reduction closure
* normal form: irreducible term
* strongly normalizing (terminating): every infinite reduction sequence has a limit
* nonterminating reduction: infinite reduction sequence with no limit or that does not reduce to its limit
* weakly normalizing (normalizing): every term has a reduction to a normal form



We can split into two steps:



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

Normalization
-------------

A hypernormalizing strategy is a strategy that is normalizing even if arbitrary reduction steps are taken before and after steps of the strategy. This allows the compiler to make optimizations without changing the behavior of the program. A hypernormalizing strategy allows aggressive optimizations and program transforms.

There are also stronger properties than normalization. A Church-Rosser strategy is one with common reducts, i.e. there exist m and n, such that :math:`F^m(t)=F^n(u)` for every t and u equal via forward/backward evaluation. A normalizing strategy is Church-Rosser if the system is confluent and weakly normalizing (i.e. all objects have a normal form). In general a many-step CR strategy exists for effective ARS's, i.e. countable (in a computable fashion) and with a computable reduction relation. But the strategy is quite hard to compute, as it has to synchronize reducing subterms so that all components are reduced the same amount. And it's not clear that this synchronization offers anything to the programmer.

Cofinal strategies are weaker than Church-Rosser but stronger than normalizing: for every term a, if a reduces in a finite number of steps to b, then there is an object c obtained by applying the strategy some number of times to a such that b reduces to c. For critical pair TRSs any "fair" strategy that ensures every redex is eventually contracted is cofinal. The cofinal property provides slick proofs - it ensures every redex not part of a cycle is contracted. But at runtime non-normalizing terms have indistinguishable behavior (infinite loop), hence this means the cofinal strategy is doing unnecessary work.

There are also termination properties like strong convergence that ensure that for every term, there exists some number of reduction steps after which the head cannot be rewritten.
To ensure that term rewriting halts we probably also want a property like strong convergence, but this is a property of the rewriting strategy, not the TRS proper.
