Term rewriting
##############

Rewrite rules
=============

A rewrite rule or clause is applied by matching the left hand side to the term or one of its subterms and applying the resulting substitution to the right hand side. As in predicate dispatch, we consider conditional TRSs so rewrite rules also have conditions - the rewrite rule can only be applied if its condition holds. The execution of a Stroscot program is modeled as applying rewrite rules until no more can be applied. A "value" in a TRS refers to a normal form, a term that cannot be reduced further.

For example, with the rules ``fact n | n > 0 = n * fact (n-1)`` and ``fact 0 = 1``, we get the reduction sequence

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

Higher-order rewriting
======================

A higher order rewriting system (HORS) consists of a substitution calculus and a set of rewrite rules.

A substitution calculus is an abstract rewriting system on a set of preterms. We assume an infinite number of atomic preterms, including variables, holes, symbols, and constants, indexed by integers, with holes being considered a subset of variables. We assume a substitution operation that renames bound variables as needed. A preterm is closed if it contains no free variables. A m-ary precontext is a preterm with holes 1 through m. It is linear if every hole occurs exactly once.

Terms are representatives of equivalence classes of preterms under ``<->*`` of the substitution calculus. If the substitution calculus is convergent, then terms are normal forms. Contexts are similarly representatives of precontexts.

A rewrite rule is of the form ``l -> r`` where ``l`` and ``r`` are both closed terms. The rewriting relation on terms is defined by ``M -> N`` if ``M <->* C[l]`` and ``C[r] <->* N`` for some rewrite rule ``l -> r`` and context ``C`` containing a hole; ``C[l]`` means ``C`` with the hole substituted by ``l``. Closed terms suffice because binders are in the substitution calculus, e.g. ``f x y = plus x y`` can be written as ``\x y. f x y -> \x y. plus x y``.

An example is the lambda calculus. The set of preterms is built from nullary symbols (variables, holes, symbols, and constants), applications of two preterms, and abstractions of a variable and a preterm. The substitution calculus rewriting rules are beta reduction and alpha renaming. Eta reduction can be added but makes the system only weakly orthogonal. :cite:`endrullisHighlightsInfinitaryRewriting2012`

Conditional rewriting
=====================

A conditional rewrite rule has the form ``l -> r | C1, ..., Cn``. It consists of an unconditional rewrite rule ``l -> r`` and conditions ``Ci``. The conditions take the form of predicates ``Pi(x1, ..., xm, ->)``, where the ``xi`` are the free variables of ``l`` and ``r``, and ``->`` is the rewrite relation of the system. Example predicates are:

* type predicates, term must be of a certain form
* ``a`` joins with, rewrites to, or is convertible to ``b``

The rewrite relation is defined as the fixed point of the union of the rewrite rules. Terese presents the least fixed point, but we could presumably use the optimal fixed point.

We can add a form of logic programming by allowing conditions to mention variables not in the LHS or RHS, e.g. ``precedes x z | precedes x y && precedes y z = true`` quantifies over all ``y``. This further extends to allowing variables on the RHS not present on the LHS,

Cycles
======

The untyped lambda calculus has cycles, e.g. ``Omega = let w=\x.x x in w w`` reduces to itself and :cite:`venturinizilliReductionGraphsLambda1984` shows a 6-cycle ``M M I``. Similarly commutativity ``a + b = b + a`` generates cycles.

:cite:`dershowitzRewriteSystems1991`'s notion of a congruence-class rewriting system is helpful - the rewrite rules are split into rules R and reversible equations S, and we consider the system R/S (R mod S). A term where only S rules apply (no R rules apply) is considered a normal form. So similarly we consider the `condensation <https://en.wikipedia.org/wiki/Strongly_connected_component#Definitions>`__ of the rewrite graph, condensing each SCC to a single term. A term is a "condensed normal form" if it has no reduction out of its SCC. Hence ``Omega``,  ``M M I`` and ``a + b`` would be condensed normal forms since their SCC contains themselves and they have no further reductions. We could further specify the normal form to be a canonical representative of the SCC, e.g. taking the smallest and lexicographically first element of the SCC, but leaving input unchanged seems better.

Nondeterminism
==============

A reduction sequence is not necessarily unique, e.g. in reducing ``3 * (2 * fact 1)`` to ``6`` we could compute ``fact 1 = 1`` or we could first use an associative law ``3 * (2 * fact 1) = (3 * 2) * fact 1 = 6 * (fact 1)``. Different reduction sequences can be more efficient in terms of memory usage; the compiler should use heuristics and hints to choose the best strategy.

For maximum expressiveness, we also want to allow local nondeterminism. Even if a term has two or more applicable reduction rules and reduces to two normal forms, the context might give the same behavior on the different values. E.g. this should be allowed:

::

  a = b
  a = c
  # a has two normal forms, nondeterministic

  f b = d
  f c = d

  print (f a) # deterministically prints d

However, top-level method dispatch nondeterminism is unresolvable. E.g. ``print a`` with this example is an error  - there is no way to reconcile ``print b`` and ``print c``, because the user can only see one output.

Exceptions complicate the semantics. We want our reduction strategy to be normalizing, so that if there is any reduction sequence that produces a non-exception value, Stroscot evaluates to that non-exception value, rather than an exception. So ``1 amb (throw b)`` should reduce to 1. This provides the benefits of lazy evaluation.

Also, exception propagation is nondeterministic. For example ``e = throw b + throw c`` will throw either ``b`` or ``c`` depending on which is evaluated first, and the choice is observable in a program with ``e catch print``. Exception nondeterminism is a different category from method dispatch nondeterminism and by default is considered benign, i.e. the compiler will not output a diagnostic and will resolve the ``catch`` using the exception that is most efficient to dispatch. But you can enable an error or warning that ensures thrown exceptions are unique. Regardless, the verification system will verify properties for all choices of exception, i.e. ``(case e of Exc b -> 1; Exc c -> "a") : Int`` will fail but ``(case (throw b) of Exc b -> 1; Exc c -> "a") : Int`` will not because ``c`` is unreachable.

Ordinal reduction
=================

Ordinal reduction is useful because it is "smoother" than finite reduction - normal forms exist more often, and there are fewer non-converging reduction sequences. E.g. ``x = 1 :: x`` has a proper denotation, instead of having to work with head normal forms and partially evaluated terms. Also I/O can be modeled as an infinite value with sub-terms for each outcome of the I/O operation.

The idea is to extend our set of terms to include infinite terms, defined as the `metric completion <https://en.wikipedia.org/wiki/Complete_metric_space#Completion>`__ of finite terms with a distance function :math:`2^{-n}` if the n-th level of the terms is the first level where a difference appears and 0 if the terms are equal.

The limit is extended to ordinal sequences, :math:`\lim_b s_b = l` if for every real number :math:`\epsilon > 0` there exists an ordinal :math:`N` such that :math:`d(s_b, l) < \epsilon` for :math:`N < b`. :math:`s` is said to (weakly) converge to :math:`l`.

We define the transitive closure of reduction :math:`\overset{*}{\to}` to contain the following:

* the identity relation :math:`\{x\|(x,x)\}`
* the successor relation :math:`\overset{*}{\to} \circ \to`
* the limit relation: if there exist elements :math:`s_i, i \geq 0` such that :math:`s_i \overset{*}{\to} s_j` for :math:`i < j`, and :math:`\lim_b s_b = t`, then :math:`s_0 \overset{*}{\to} t`

There is also strong convergence, which requires that the depth of the redexes contracted in the successive steps tends to infinity when approaching a limit ordinal from below. :cite:`simonsenWeakConvergenceUniform2010` proved that if there exists a weakly convergent reduction :math:`s \overset{*}{\to} t` that is not also strongly convergent, there will be some intermediate reduct :math:`u`` with the property that :math:`u`` reduces to itself in one step. When we use condensed reduction, the loops are removed (a loop does not reduce out of the SCC), so the strong and weak definitions coincide.

Generally reduction sequences only need to be computed to the first transfinite ordinal :math:`\omega`, because that suffices to obtain all reducts. For example ``x = 1 :: x`` reduces to ``x = 1 :: 1 :: 1 :: ...``, ``fib = 1 :: 2 :: zipWith (+) fib (head fib)`` reduces to ``fib = 1 :: 2 :: 3 :: ...``, and ``foo = let t = \x. x x x in t t`` reduces to ``foo = ... t t t t``. But for some systems this is not sufficient, e.g. ``a = b; f x a = f (g x) a`` has ``f c a -ω> f (g (g (g ...))) a -> f (g (g (g ...))) b`` but there is no derivation of length ω. :cite:`dershowitzRewriteRewriteRewrite1991` To collect all possible sequences I have instead used sequences indexed by all ordinals (a proper class). Using the `first uncountable ordinal <https://en.wikipedia.org/wiki/First_uncountable_ordinal>`__  :math:`\omega_{1}}` might also work.

Meaningless terms
=================

If a term infinitely reduces and never reaches a normal form, then there's not much semantic meaning in it.  We could compute equivalence classes of these terms but it is easier to define them all away. :cite:`kennawayMeaninglessTermsRewriting1999` defines the set of "mute" terms, the smallest set with the following properties:

* Contains all root-active terms. A term t is root-active if every reduct of t can be reduced to a term with a top-level redex.
* Closure under reduction. If ``M ∈ U``, ``M → N`` then ``N ∈ U``.
* Closure under substitution. For all ``M ∈ U``, ``M /. σ ∈ U``
* Overlap. For all ``(\x.M) ∈ U``, ``(\x.M) N ∈ U``. More generally if M nontrivially matches a subterm of the LHS of some rule, i.e. for some position ``u`` and substitution ``σ``, ``M = subterm (l /. σ) u`` and ``subterm l u`` is not a variable, then the overall LHS is in U, ``l /. σ ∈ U``.
* Indiscernibility - the meaningfullness of a term does not depend on its meaningless subterms. For all M, N, if N can be obtained from M by replacing a set of pairwise disjoint subterms in U with other terms of U, then M ∈ U if and only if N ∈ U.

There is also closure under expansion introduced in :cite:`severiDecomposingLatticeMeaningless2011`: if ``N ∈ U``, ``M → N`` then ``M ∈ U``. This makes the set easier to reason about, but we want ``t = 1 amb meaningless`` to reduce to 1, so ``t`` can't be meaningless itself, hence we don't want this property.

Mute terms form an easy set, :cite:`bucciarelliGraphEasySets2016` meaning we can safely equate all mute terms to an exception term without changing the semantics of normal terms. In particular we can equate them to a ``NonTermination`` or ``Meaningless`` exception.

Equating mute terms solves the issue of divergence of ordinal reduction. The negation of weak convergence is that there exists a depth where the subterm keeps changing, i.e. a divergent term must contain a mute term. With the mute term reduced to an exception the overall term converges.

.. _trs-equality-linearity:

Equality and left-linearity
===========================

There are several notions of equality, here presented in the order of earlier implies later:

* strict equality ``eq_s x y | x == y`` is syntactic equality of normal forms (both arguments must be fully reduced terms).
* syntactic equality is that used in non-linear TRS, ``eq_t x x -> True``. It matches unreduced terms, hence can match even if the term doesn't have a normal form.
* join equality ``a ↓ b`` means that a common reduct exists, i.e. there is a term ``c`` such that ``a -> c`` and ``b -> c``.
* semi-equational equality ``a ≈ b`` means that ``a`` can be rewritten to ``b`` via rewrites and inverse rewrites.

Computing any of these equalities is of complexity :math:`\Sigma^0_1` because it is a nontrivial property of the reduction relation.

Consider ``a = c a; c x = d x (c x); d x x = e``. ``a = mu x. c x = mu x. d x x``. If we allow ``mu x. d x x = e``, then we get both ``e`` and ``mu x. d e x`` as reducts. ``e`` is clearly a normal form hence for the system to be confluent we must have ``mu x. d e x == e`` so that ``mu x. d e x = e``. So for the different equalities:

* strict equality denies ``mu x. d x x``, because it might be reducible
*

 With syntactic equality and join equality this doesn't hold. With semi-equational equality it holds because they are reducts of the same term.


Syntactic equality and join equality are not stable, i.e. if the terms involved are reduced the terms may not be equal anymore. This instability means that the CTRS may not be confluent even if the unconditional TRS is.

 = e`` and this is the only result. Admittedly you have to define equality of infinite structures carefully; otherwise ``mu x. d x x`` doesn't even reduce. However, with TRS equality, ``a = c a = c (c a) = c (d a (c a)) = c (d (c a) (c a)) = c e = `` which definitely doesn't reduce, hence is distinct from ``e``, hence the system becomes nondeterministic.

 Strict equality and semi-equational equality are stable.








For example, ``c x | x ↓ c x = e; b = c b`` is confluent ignoring the conditions, but ``e ↓ c e`` does not hold because a CTRS is constructed as the least fixed point hence ``b`` reduces to normal forms ``e`` and ``c e``. Similarly with ``f x x = X, a = b, a = c, d = c, d = e``, ``f a d`` reduces to both ``f c c`` and ``f b e``. In the unconditional system both of these reduce to ``X``, but when the non-linearity is interpreted as TRS equality, ``f c c`` reduces but ``f b e`` does not.

``f x x = a, f x (g x) = b, c = g c`` ``f c c`` reduces to both ``a`` and ``b``

Semi-equational equality is a global property of the rewrite relation, meaning if you define a new nondeterministic term ``a = b; a = c`` then suddenly ``b == c`` when it didn't before. But for a confluent rewriting system semi-equational equality is equivalent to join equality, it just allows more reductions.

The main issue with strict equality can't simplify ``x==x`` to true but TRS equality and above can.



Consider some systems:

* The system  where the first and second rules use equality. With strict equality ``c`` has no normal form, hence ``f c c`` does not reduce with an ``f``-rule (it gets stuck evaluating a reduction of the form ``c -> g c -> g (g c) -> ...``).


 There are no critical pairs, so the system is locally confluent, but  hence the system is not confluent. With equational equality ``f c c`` reduces to both ``a`` and ``b`` in one step hence the system is not locally confluent.

* In the system , the term , hence the system does not have unique normal forms. With strict equality ``f a d`` does not reduce to ``X`` and with equational equality ``f b e`` reduces to ``X``.


* convergent (confluent and terminating) - These include typed systems such as the simply typed lambda calculus. For these, the result is the same no matter how they are reduced. So the focus is on do the reduction efficiently, compiling to fast assembly via a state machine and data format analysis and/or doing optimal reduction to reduce in the smallest number of steps.

Confluence
----------

Confluence has gotten a lot of attention as well and has automated provers. Confluence implies UN→; it is equivalent if the TRS is weakly normalizing. And there is an extension theorem: every TRS with unique normal forms (UN=) can be extended to a confluent TRS with the same set of normal forms by adding bottom terms and reductions to normal forms and bottoms that preserve the equivalence classes of terms. :cite:`middeldorpModularAspectsProperties1989` Similarly a system can be shown to be UN= by presenting an extension of it that is confluent. :cite:`klopExtendedTermRewriting1991` So a UN= program is just a partially specified system. UN→ is a little more complex though. And the equivalence classes of terms are uncomputable in general so the extension is as well.

Confluence avoids situations where a system may branch into two distinct diverging states. It makes finding a normalizing strategy much easier as the strategy only has to avoid getting stuck evaluating a term infinitely (using the same rule infinitely often), as opposed to UN→ where the strategy must avoid using the wrong reduction rule at every step.

The Knuth-Bendix algorithm produces a confluent system from a set of non-oriented equations, but the rules in programs are oriented, so using this would be confusing. Not to mention that the algorithm fails often. So that's out.

A necessary condition for confluence is weak/local confluence, i.e. each critical pair is convergent. But this is not sufficient. Newman's lemma is that a terminating locally confluent TRS is confluent. But termination is quite strong. A generalization is a critical pair system :cite:`hirokawaDecreasingDiagramsRelative2009` (also called decreasingly confluent): the system must be left-linear, locally confluent, and its critical pair steps must be *relatively terminating*, i.e. the relation 'arbitrary steps followed by a critical pair step followed by arbitrary steps' is terminating. Trivial critical pair steps can be excluded, hence this includes weakly orthogonal TRSs. For a terminating TRS the TRS syntactic equality notion is equivalent to strict equality, hence the system is left linear in the CTRS sense, hence why this includes Newman's lemma.

We say → has random descent (RD), if for each R:a ↔∗b with b in normal form, all maximal reductions from a have length d(R) and end in b. Systems with random descent are confluent.



Normalization
-------------


A hypernormalizing strategy is a strategy that is normalizing even if arbitrary reduction steps are taken before and after steps of the strategy. This allows the compiler to make optimizations without changing the behavior of the program. A hypernormalizing strategy allows aggressive optimizations and program transforms.

Leftmost outermost reduction is the basis of lazy evaluation and is hypernormalizing for the lambda calculus. But for TRSs LO is only normalizing for left-normal TRSs, where variables do not precede function symbols in the left-hand sides of the rewrite rule. A better strategy is outermost fair (ensuring each outermost redex will eventually be evaluated - the simplest example is parallel outermost) - it's hypernormalizing for critical pair TRSs (decreasingly confluent TRSs), in particular weakly orthogonal TRSs. :cite:`hirokawaStrategiesDecreasinglyConfluent2011`

There are also stronger properties than normalization. A Church-Rosser strategy is one with common reducts, i.e. there exist m and n, such that :math:`F^m(t)=F^n(u)` for every t and u equal via forward/backward evaluation. A normalizing strategy is Church-Rosser if the system is confluent and weakly normalizing (i.e. all objects have a normal form). In general a many-step CR strategy exists for effective ARS's, i.e. countable (in a computable fashion) and with a computable reduction relation. But the strategy is quite hard to compute, as it has to synchronize reducing subterms so that all components are reduced the same amount. And it's not clear that this synchronization offers anything to the programmer.

Cofinal strategies are weaker than Church-Rosser but stronger than normalizing: for every term a, if a reduces in a finite number of steps to b, then there is an object c obtained by applying the strategy some number of times to a such that b reduces to c. For critical pair TRSs any "fair" strategy that ensures every redex is eventually contracted is cofinal. The cofinal property provides slick proofs - it ensures every redex not part of a cycle is contracted. But at runtime non-normalizing terms have indistinguishable behavior (infinite loop), hence this means the cofinal strategy is doing unnecessary work.

There are also termination properties like strong convergence that ensure that for every term, there exists some number of reduction steps after which the head cannot be rewritten.
To ensure that term rewriting halts we probably also want a property like strong convergence, but this is a property of the rewriting strategy, not the TRS proper.

A perpetual strategy is the opposite of normalizing - if any strategy diverges, then perpetual strategy diverges. Leftmost-innermost is close to the strategies commonly used in strict languages and is perpetual. With a perpetual strategy inlining etc. hold only if reduction of the expression terminates, i.e. one must keep track of termination properties. A perpetual strategy gives the wrong behavior for if-then-else and short-circuit functions, so strict languages special-case these to ensure they don't cause nontermination. Perpetual strategies are antagonistic, "I'll crash your program if I can".



Modularity
==========

A property is modular if the disjoint union of two systems with the property has the property.

Left linearity, confluence, weak normalization, unique normal forms (w.r.t. equivalence), and consistency (w.r.t. equivalence) are modular for first-order systems. Modularity of left linearity, confluence, and unique normal forms extend to semi-equational CTRSs. Confluence also extends to join CTRSs. In fact if the disjoint union is confluent then the component systems must be confluent. Confluence is not modular for higher-order TRSs but confluence plus left linearity is.

Weak termination, weak innermost termination, and strong innermost termination are modular for CTRSs in combination with confluence or the property that there are no extra variables in the conditions.

NF, unique normal forms with respect to reduction, and consistency with respect to reduction are modular in combination with left linearity. Consistency w.r.t. reduction means that there is no term reducing to two distinct variables; it is implied by the unique normal form property w.r.t. reduction as variables are normal forms.

Strong normalization plus consistency w.r.t. reduction plus left linearity is modular. This likely holds for CTRSs without extra variables as well.


HORS
====

A HORS is orthogonal if:

A1 the substitution calculus is complete
A2 the substitution calculus is only needed for gluing
A4 the substitution calculus is a descendant rewriting system
A5 the substitution calculus is parametric and rules are head-defined
A7 the substitution calculus is naturally closed under substitution

A3 parallel rewrite steps can be serialised
A6 left-hand sides of rules are linear
A8 every set of redexes is pairwise simultaneous


Concrete strategies
===================

So: strategy must normalizing. Now, which strategy?

For terminating programs, all strategies are normalizing. Hence we want to infer termination and use this to optimize the strategy - leftmost innermost ensures "complete development", i.e. a subterm is reduced completely before the outer term, hence we can store the subterm using an optimized representation of the normal form.
But strongly normalizing implies not Turing complete, hence the typechecker that ensures termination will cause problems for complex programs. We need a fallback for non-terminating programs.

The simplest fallback is outermost-fair, it's a reasonable default and terminates on critical pair TRSs. But there are hand-written examples where it fails.

We could do user-specified strategies like Stratego, but then how would we know that they're normalizing.

The optimal reduction stuff is defined for match sequential TRSs.

non-strict strategies:
* Lenient evaluation - computation rule [Traub, FPCA 89], where all redexes are evaluated in parallel except inside the arms of conditionals and inside lambdas.
* extra memory overhead for parameter passing (inefficient)
* strictness analysis to optimize to eager (which has identical semantics to lazy 99% of the time)

Now, one can argue about which computational strategy is better (time, space, parallelism, ...)
Stroscot: be accepting of programs, ensure a normalizing strategy. But after that aim for most efficient in time/space for strict programs.

Q: can normalizing be as efficient as strict
profiling, other optimization tricks

A list List[Nat]. In a strict language ADTs are finite. In lazy, we might accept infinite lists (generators). We want precise types: the finite data structure and its infinite counterpart ARE DIFFERENT DATATYPES. Only discardable (weakenable) boxes can contain infinite structures, so uList. (Nat + !w List) is an infinite list, while uList. (Nat + List) is a strict list. Extends to more complicated data structures. With subtyping you can use a finite list with an infinite list transformer.

UNIX pipes. "yes fred | less" works fine, but "yes fred | sort | less" is an infinite loop, because yes fred is infinite and sort is strict. For finite streams the simple semantics of pipes, namely
1) First program generates output
2) This output is sent to next program
....
n) This output is sent to next program
n+1) This output is sent to terminal
suffices.
Most programs have finite output on finite input and block gracefully. Thus for MOST programs you need not worry about whether the execution of pipes is interleaved or not. The interleaving matters for long outputs because it saves memory (=time w/gc) and improves performance dramatically.
That interleaving works with certain infinite streams is just a natural generalization. The slow behavior of sort is also visible with long lists.
Laziness means you can implement interleaving once in the language (as the evaluation strategy) as opposed to piecemeal for each program.


Tree structure of terms (n⋅(n+1))/2 and n⋅((n+1)/2)

Given a set V of variable symbols, a set C of constant symbols and sets Fn of n-ary function symbols, also called operator symbols, for each natural number n ≥ 1, the set of (unsorted first-order) terms T is recursively defined to be the smallest set with the following properties:[1]

    every variable symbol is a term: V ⊆ T,
    every constant symbol is a term: C ⊆ T,
    from every n terms t1,...,tn, and every n-ary function symbol f ∈ Fn, a larger term f(t1, ..., tn) can be built.

Using an intuitive, pseudo-grammatical notation, this is sometimes written as: t ::= x | c | f(t1, ..., tn). Usually, only the first few function symbol sets Fn are inhabited. Well-known examples are the unary function symbols sin, cos ∈ F1, and the binary function symbols +, −, ⋅, / ∈ F2, while ternary operations are less known, let alone higher-arity functions. Many authors consider constant symbols as 0-ary function symbols F0, thus needing no special syntactic class for them.

A term denotes a mathematical object from the domain of discourse. A constant c denotes a named object from that domain, a variable x ranges over the objects in that domain, and an n-ary function f maps n-tuples of objects to objects. For example, if n ∈ V is a variable symbol, 1 ∈ C is a constant symbol, and add ∈ F2 is a binary function symbol, then n ∈ T, 1 ∈ T, and (hence) add(n, 1) ∈ T by the first, second, and third term building rule, respectively. The latter term is usually written as n+1, using infix notation and the more common operator symbol + for convenience.

Condensing acyclic rewriting systems gives back the same system. Orthogonal higher-order TRSs that are weakly head normalizing are acyclic, per :cite:`ketemaViciousCirclesRewriting2005`.

So The way we handle cycles in the rewrite engine is something like:

* detect cyclic term via rule cycle detection or presence of AC operator
* use specialized matching (eg AC matching or Tarjan SCC + memo hash table) to identify all reductions out of SCC
* end with condensed normal form if no reduction out of SCC
* otherwise, pick a reduction out of the SCC

Then this infinite term is computed in chunks and fed to the surrounding context on demand (laziness), ensuring that a finite normal form is reached if possible and otherwise implementing an infinite stream of commands.

Higher-order matching
---------------------

Handling lambdas in RHSs is fairly straightforward, just beta-reduce as much as possible when they are encountered. But in higher-order term rewriting systems the lambdas can show up on the left hand side, in the pattern. The rewriting system is then defined modulo lambda reduction. Executing a rule ``l -> r`` on a term ``t`` solves the equation ``t = C[lθ]`` and replaces it with ``C[rθ]``.

Finding the contexts ``C`` is fairly straightforward, just enumerate all the subterms of ``t``. But solving the equation ``s = lθ`` is an instance of higher-order unification (specifically higher-order matching).  The λ-superposition calculus relies on complete sets of unifiers (CSUs). The
CSU for s and t, with respect to a set of variables V , denoted by CSUV (s, t), is a
set of unifiers such that for any unifier % of s and t, there exists a σ ∈ CSUV (s, t)
and θ such that %(X) = (σ◦θ)(X) for all X ∈ V . The set X is used to distinguish
between important and auxiliary variables. We can normally leave it implicit

Higher order matching is decidable for the simply typed lambda calculus. But the proof is of the form "the minimal solution is of size at most 2^2^2^2..., the number of 2's proportional to the size of the problem". There are 3 transformations presented in the proof which reduce a larger solution to a smaller solution. These might be usable to prune the search tree. But at the end of the day it's mostly brute-force.

The proof relies on some properties of the STLC, namely normalization and that terms have a defined eta long form (canonical form).

It is not clear if there is a way to do untyped higher order matching for general lambda patterns.

As a consequence of confluence each rewrite step is composed of an expansion in the substitution calculus, a replacement by applying some rule, and a reduction in the substitution calculus, so it is M <<- C[l] and C[r] ->> N


If reduction does not end in a condensed normal form, then the sequence of terms must be infinitely expanding in the sense that for every size s there is a point in the reduction where terms are always at least size s. Otherwise, assuming a finite number of term symbols, there are only finitely many terms of size < s, so there would be a cycle in the reduction and reduction would end in a condensed normal form.

