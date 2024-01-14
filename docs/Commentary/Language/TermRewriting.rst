Term rewriting
##############

Higher-order rewriting
======================

The definition is vaguely based on :cite:`vanoostromConfluenceAbstractHigherOrder1994`. But Ooostrom's HORS definition, like Terese, uses closed terms and no separate substitution, avoiding free variables by adding additional binders, e.g. he writes the rule ``f x = 1`` as ``\x. f x -> \x. 1``. I don't like this because it's unclear how it interacts with currying, eta reduction, and conditional rewriting, so I added the substitution back.

There is some question of whether context substitution is capture-avoiding, i.e. does ``(\x. □)[□ := x]`` not resolve to ``(\x. x)``. Terese says it captures. With Oostrom this substitution is forbidden since ``x`` is not a closed term. In our more liberal definition this resolves to ``(\y. x)`` by alpha-renaming the variable.

An example substitution calculus is the lambda calculus. The set of preterms is built from nullary symbols (variables, holes, symbols, and constants), applications of two preterms, and abstractions of a variable and a preterm. The substitution calculus rewriting rules are beta reduction and alpha renaming. Eta reduction can be added too and makes the system only weakly orthogonal. :cite:`endrullisHighlightsInfinitaryRewriting2012`

Currying
========

Per :cite:`kennawayComparingCurriedUncurried1995` there are three related TRSs:

* The standard uncurried system where a function application ``f(t1, ... , tn)`` is formed from an n-ary function symbol ``f`` and n terms t1,...,tn.
* A curried system where a function application ``f t1 ... tn`` is formed from a nullary constant ``f`` and many application of the binary "application" operator, written as left-associative juxtaposition or more verbosely ``App(f,x)``.
* A "partially parameterized" (PP) system containing ``App`` and, for each n-ary function symbol in the uncurried system, n-1 partial application symbols and "uncurrying" rules ``App(f_i(...), x) = f_i+1 (..., x)``. GHC does something similar with a ``PAP`` constructor, so it is ``PAP 1 f_2 a`` instead of ``f_1(a)``. This allows combining all the partial-application rules into one handler, but forces a uniform representation for partial applications.

The uncurried system embeds in the curried system, i.e. for every uncurried term t there is a unique corresponding curried term t', and every reduct of t corresponds to a reduct of t' and vice-versa. Hence we lose no expressiveness by currying, and gain some expressiveness because only the curried system can express partially applied terms.

The PP system is an extension of the uncurried system, and is isomorphic to the curried system. The normal forms of PP and curried are in a bijection, and each curried reduction step corresponds to one or more PP steps. The curried system is simpler than the PP system, so we use the curried definition for the language syntax and semantics. But the PP system may be useful for compilation. It's easy to break the isomorphism though, e.g. ``printf "%i" 1`` can be defined in the curried system but not in the PP system because the arity is variable.

Conditional rewriting
=====================

In a general CTRS the reduction rules are of the form ``t | P -> s``, where ``P`` is a predicate. The allowed rewrites of a conditional rule are the unconditional rewrites ``t -> s`` that satisfy the condition ``P``. We can add a form of logic programming by allowing conditions to use variables not in the LHS, e.g. ``precedes x z | exists y. precedes x y && precedes y z = true``. This further extends to allowing variables on the RHS not present on the LHS. A further extension allows LHSs that are a single variable but have a condition - some authors disallow LHSs like this. Some syntax like ``l | exists x. C = r`` and ``l = exists x. r`` might make this easier to follow.

The definition of a CTRS is complicated by allowing predicates to refer to the rewrite relation ``->``. Naively, the rewrite relation would be defined as a fixed point of the rewrite rules. I.e. letting ``S`` be the system as a function of the rewrite relation ``->``, we would define ``->`` to be a relation ``R`` such that ``R = S(R)``. Terese presents the least fixed point. However, with badly-behaved conditions, no fixed point may exist, so instead we use the "optimal prefixedpoint", the intersection of the maximal prefixedpoints. I.e. we find the sets ``Pre = { R : R subseteq S(R) }, PreMax = { R in Pre : forall R' in Pre, R subseteq R' implies R= R' }, R = intersection PreMax``. We warn on all the reductions in ``S(R) \ R`` that make it not a fixed point.

For example, with a system with the single rule ``a -> a | a is a normal form``, ``S({}) = {(a,a)}`` and ``S({a,a}) = {}``. There is no fixed point, so the naive definition doesn't work. The optimal prefixedpoint is ``{}`` so ``R = {}``. But we warn that the reduction ``a -> a`` is not included. As another example, take ``a -> b if not(c -> d); c -> d if not(a -> b)``. The maximal (pre)fixedpoints are ``{(a,b)}`` and ``{(c,d)}``, but their intersection is ``{}`` so the optimal prefixedpoint is ``R = {}``. We warn that ``a -> b`` and ``c -> d`` are not included (extra diagnostics could show they are mutually exclusive). Lastly, ``a -> b if a -> b`` allows the reduction ``a -> b`` as it is the maximal (pre)fixedpoint.

The optimal prefixedpoint is correct in that reductions must satisfy conditions, conservative in that systems which have multiple differing interpretations do not reduce, but also a bit generous since it is maximal instead of least, and robust because independent rules are processed independently.

As far as terminology, the literature uses "term rewriting" to refer to unconditional term rewriting and "conditional term rewriting" otherwise. But many popular programming languages such as Haskell have conditional dispatch (guards, patterns, etc.), so we instead use "term rewriting" to refer to conditional and unconditional systems, and we refer to "unconditional TRSs" when necessary.

Cycles
======

The untyped lambda calculus has cycles, e.g. ``Omega = let w=\x.x x in w w`` reduces to itself and :cite:`venturinizilliReductionGraphsLambda1984` shows a 6-cycle ``M M I``. Similarly commutativity ``a + b = b + a`` generates cycles. Maude in fact has associative and commutative operators, declared with special syntax like ``op _;_ [assoc]``. But this is a bit specific; likely there are other equations we would want to have, like the Jacobi identity :math:`x\times (y\times z)\ =\ (x\times y)\times z\ +\ y\times (x\times z)` and so on. In general, we have a list of rewrite rules and it is not necessarily known a priori whether these may result in cycles, but we want to resolve any cycles that come up in a natural manner.

In a lot of cases, the cycle may be due to reversible equations ``pat1 = pat2; pat2 = pat1``.:cite:`dershowitzRewriteSystems1991`'s notion of a congruence-class rewriting system is formulated along these lines - the rewrite rules are split into rules R and reversible equations S, and we consider the system R/S (R mod S). A term where only S rules apply (no R rules apply) is considered a normal form. But this "rewriting modulo equations" formalism is not sufficient - we see with Omega that even beta reduction, a single rule, and one not obviously intended to be cyclic, can result in cycles.

So instead we consider the `condensation <https://en.wikipedia.org/wiki/Strongly_connected_component#Definitions>`__ of the rewrite graph, condensing each SCC to a single term. This condensation step is similar to the quotient R/S but acts individually on each term rather than on the system. A term is a "condensed normal form" if it has no reduction out of its SCC. Hence ``Omega``,  ``M M I`` and ``a + b`` would be condensed normal forms since their SCC contains themselves and they have no further reductions. We could further specify the normal form to be a canonical representative of the SCC, e.g. taking the smallest and lexicographically first element of the SCC, but leaving input unchanged and returning the first element of the SCC that is encountered seems better for debugging purposes.

Orthogonal higher-order TRSs that are weakly head normalizing are acyclic, per :cite:`ketemaViciousCirclesRewriting2005`, so the cycle condensation doesn't affect standard functional programming - condensing acyclic rewriting systems gives back the same system. Thus cycle detection shouldn't have much of an impact on performance.

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

Exceptions complicate the semantics. The literature speaks of "normalizing" strategies that will eventually find a normal form if one exists, but otherwise are allowed to loop forever. In Stroscot non-termination is an exception, so the corresponding property is that if there is any reduction sequence that produces a non-exception value, Stroscot evaluates to that non-exception value, rather than an exception. So ``1 amb (throw b)`` should reduce to 1. This provides the benefits of lazy evaluation.

The alternative "strict" evaluation strategy would be what the literature calls a "perpetual" strategy - if any strategy diverges, then a perpetual strategy diverges. With a perpetual strategy inlining etc. hold only if reduction of the expression terminates, i.e. one must keep track of termination properties. A perpetual strategy gives the wrong behavior for if-then-else and short-circuit functions, so strict languages special-case these to ensure they don't cause nontermination. Perpetual strategies are antagonistic, "I'll crash your program if I can". The evaluation strategies article discusses strict vs lazy more - overall lazy seems better.

Also, exception propagation is nondeterministic. For example ``e = throw b + throw c`` will throw either ``b`` or ``c`` depending on which is evaluated first, and the choice is observable in a program with ``e catch print``. Exception nondeterminism is a different category from method dispatch nondeterminism and generally seems benign. So the compiler will not output a diagnostic and will resolve the ``catch`` using the exception that is most efficient to dispatch. But you can enable an error or warning that ensures caught exceptions are unique. Regardless, the verification system will verify properties for all choices of exception, i.e. ``(case e of Exc b -> 1; Exc c -> "a") : Int`` will fail but ``(case (throw b) of Exc b -> 1; Exc c -> "a") : Int`` will not because ``c`` is unreachable.

Infinite reduction
==================

Infinite reduction is useful because it is "smoother" than finite reduction - normal forms exist more often. For example ``x = 1 :: x`` reduces to ``x = 1 :: 1 :: 1 :: ...``, ``fib = 1 :: 2 :: zipWith (+) fib (head fib)`` reduces to ``fib = 1 :: 2 :: 3 :: ...``, and ``foo = let t = \x. x x x in t t`` reduces to ``foo = ... t t t t``. With finite reduction we would have to use head normal forms and partially evaluated terms. With infinite reduction all of these terms have a proper denotation. Also I/O can be modeled as an infinite value with sub-terms for each outcome of the I/O operation.

The idea is to extend our set of terms to include infinite terms, defined as the `metric completion <https://en.wikipedia.org/wiki/Complete_metric_space#Completion>`__ of finite terms with a distance function :math:`2^{-n}` if the n-th level of the terms is the first level where a difference appears and 0 if the terms are equal. By convention the top level is level zero. This definition is equivalent to a co-inductive definition of terms, i.e. the largest set consisting of term-parts whose subterms are co-inductive terms.

This set, like the real numbers, is uncountably large and includes terms with no finite description. Actually we should be able to map the set of finite and infinite terms to the real interval [0,1]: do a breadth-first traversal of the term tree and encode the sequence of symbols using arithmetic encoding. Properly defined, the mapping should be a computable, continuous bijection that imposes a total order on terms. TODO: verify. There is then a canonical representation of each real number, such as `this continued fraction representation <https://oscarcunningham.com/494/a-better-representation-for-real-numbers/>`__, so also a canonical representation of each infinite term. Although, again like the reals, it is computationally more tractable to use an unevaluated-expression-tree symbolic representation and special-case interesting classes of terms, than it is to use a generic representation as an infinite sequence.

There are various extensions of the transitive closure to infinitary reduction, so the question arises as to which one to use. :cite:`kahrsInfinitaryRewritingClosure2013` discusses several and provides an ordering so that each is mostly a proper subset of the next (not sure about P* subset bi-infinite). Many of these use the monotonic closure operator \*. Specifically X* is the least fixedpoint of the function G defined as G(R) = X(R) union R, which by the (transfinite) Kleene fixed-point theorem exists and is the limit/union of the sequence :math:`X^0 = \emptyset, X^{n+1} = G(X^n), X^\delta = \bigcup_{\alpha < \delta} X^\alpha`.

* S*, the monotonic closure of strongly converging reduction sequences, "strong" being a requirement that the depth of the redexes contracted in successive steps tends to infinity. S=S* for "compatible" TRSs, ones where t R u imply C[t] R C[u] for any context C, which all iTRSs satisfy.
* W*=A=A*, the monotonic closure of weakly converging reduction sequences, and also the `adherent points <https://en.wikipedia.org/wiki/Adherent_point>`__ of reduction sequences in the metric space. Weak convergence by itself is not transitively closed, e.g. ``a = b; f x a = f (g x) a`` has ``f c a -ω> f (g (g (g ...))) a -> f (g (g (g ...))) b`` :cite:`dershowitzRwriteRewriteRewrite1991` :cite:`simonsenWeakConvergenceUniform2010`, hence the need for closure. By definition of adherent point, each w-reduct is either an accumulation point, i.e. a appears arbitrarily close infinitely often in a reduction sequence, or an isolated point which can be reached in a finite number of reductions.
* P*: the monotonic closure of the pointwise closure of the reflexive transitive closure (finite multi-step relation).
* bi-infinite rewriting, defined in :cite:`endrullisCoinductiveFoundationsInfinitary2018` Section 6.2 as the greatest relation R such that R = the reflexive transitive closure of single-step rewriting union R lifted to apply to subterms.
* T*: the monotonic closure of T, the topological closure of the reflexive transitive closure. T itself is not transitively closed, e.g. ``leq 0 x = true; leq (s x) (s y) = leq x y; inf = s inf`` has ``leq inf inf T leq (mu x. s x) (mu y. s y) T true`` (by topological closure of finite approximations of the S towers) but not ``leq inf inf T true`` (because the terms are of finite depth). Alternatively I have defined T* as the smallest relation M such that M is reflexively, transitively, and topologically closed and contains the single-step relation, which I think is equivalent.

S* is the standard in the literature but doesn't have much going for it besides that. If there is a reduction that switches heads, ``a X = b (c X); b X = a (c X)``, then S* says there are no w-reductions. W* has ``a e -w> a (mu x. c x)`` and ``a e -w> b (mu x. c x)``. TRSs are in general nondeterministic, so the "strongly converging" definition that requires a single limit to exist is too strong.

For cycle condensation we would like to equate as many terms as possible to get large SCCs, and similarly a large reduction relation means there will be an escape from infinite regresses. As an example, with bi-infinite rewriting or T*, the hypercollapsing term ``mu x. C x`` with rule ``C x = x`` will reduce to every term (limit of approximations ``C^n f = f``), making it ambiguous, while with W* and P* the hypercollapsing term only reduces to itself hence is a condensed normal form. Similarly with ``C A = A`` where ``A`` is a constant, ``mu x. C x = A`` with bi-infinite/T* but W*/P* don't reduce at all. Bi-infinite and T* seem equally simple to formalize since they are both single fixed points, so it seems T* wins because it's larger.

Also conditional rewriting can interact with infinite reduction and cause unwanted behavior with a weak closure. For example consider the system ``ds x y | x == y = e`` and reducing the infinite term ``G = ds G G`` (in :cite:`klopCombinatoryReductionSystems1980` this is achieved by the system ``G = a = c a; c x = ds x (c x)``). Since ``e`` is a normal form hence equal to itself, all finite terms defined by ``T = { x : x == e or x in ds T T }`` reduce to ``e``. So using a bi-infinite closure, ``G`` uniquely reduces to ``e``. But with a weak closure ``X = ds e X`` is a normal form and the system becomes nondeterministic. Similarly with ``dk x y | x == y = e x`` and ``G = dk G G``, we should get ``e (e (e ...))`` as the unique result, but with a weak closure we don't. Another tricky system is ``c x | x == c x = e; b = c b`` - the obvious reduction is ``b = mu x. c x = e``, but this system has a hidden circularity of the form ``mu x. c x = e`` if ``mu x. c x = e``. So again based on this we would like a bi-infinite or T* closure.

Overall I think T* is the simplest and easiest to understand, so that's what I picked.

Meaningless terms
=================

If a term never reaches a normal form, then there's not much semantic meaning in it.  We could compute equivalence classes of these terms but it is easier to define them all away.:cite:`kennawayMeaninglessTermsRewriting1999` defines criteria for a set of meaningless terms:

* Contains all root-active terms. A term t is root-active if every reduct of t can be reduced to a term with a top-level redex.
* Closure under reduction. If ``M ∈ U``, ``M → N`` then ``N ∈ U``.
* Closure under substitution. For all ``M ∈ U``, ``M /. σ ∈ U``
* Overlap. If a redex t overlaps a subterm, and this subterm is in U, then t in U. More specifically, if M nontrivially matches a subterm of the LHS of some rule, i.e. for some position ``u`` and substitution ``σ``, ``M = subterm (l /. σ) u`` and ``subterm l u`` is not a variable, then the overall LHS is in U, ``l /. σ ∈ U``. Specifically for the lambda calculus, if ``(\x.M) ∈ U`` then ``(\x.M) N ∈ U``. Another way of looking at it is that we want to ensure adding rules ``t = Meaningless`` preserves confluence.
* Indiscernibility - the meaningfullness of a term does not depend on its meaningless subterms. For all M, N, if N can be obtained from M by replacing a set of pairwise disjoint subterms in U with other terms of U, then M ∈ U if and only if N ∈ U.

:cite:`severiDecomposingLatticeMeaningless2011` adds closure under expansion: if ``N ∈ U``, ``M → N`` then ``M ∈ U``. This makes the set easier to reason about, but we want ``t = 1 amb Meaningless`` to evaluate to 1, so ``t`` can't be meaningless itself, hence we don't want this property.

We do add topological closure as a property of the mute terms, to preserve the property that the infinitary rewriting relation is closed. Essentially we are constructing a relation ``M = { (u,Meaningless) : u in U }``; it is easy to to see from our metric definition that this is closed iff U is closed. Then our new relation is ``R' = R union M`` which is closed because the union of two closed sets is closed.

There are various sets of meaningless terms, going roughly in decreasing size as follows:

* not head normalizing - head active or infinite left spine form or infinite abstraction
* head active or infinite left spine form \x1 ... xn -> (...P2)P1.
* head active or infinite abstraction \x1 -> \x2 -> ...
* head active - \x1 ... xn -> R P1 ... Pk where R is root-active
* not weak head normalizing - strong active or strong infinite left spine form (...P2) P1
* strong active - R P1 ... Pk where R is root-active
* mute / root-active = not top normalizing

Root-active or the set of "mute" terms is the smallest set (included by definition), and seems fine. It satisfies all the other properties, meaning we just have to check root-activeness.

A meaningless term set forms an easy set, :cite:`bucciarelliGraphEasySets2016` meaning we can safely equate all meaningless terms to an exception term without changing the semantics of normal terms. In particular we can equate them to a ``Meaningless`` exception.

With these reductions every term has a normal form. Proof :cite:`kennawayInfinitaryLambdaCalculus1997`: A term t is either meaningless or not (ignoring reductions to ``Meaningless``). If it is meaningless, it reduces to the normal form ``Meaningless``. If it is not, then it can be reduced to a root-stable term ``s``. Repeating the construction recursively on the subterms of s at depth 1 constructs a reduction of t to a term which is stable at every depth, i.e. a normal form.


Every TRS with unique normal forms (UN=) can be extended to a confluent TRS with the same set of normal forms by adding bottom terms and reductions to normal forms and bottoms that preserve the equivalence classes of terms. :cite:`middeldorpModularAspectsProperties1989` Meaningless terms don't accomplish this extension because a term ``1 amb meaningless`` can reduce to ``Meaningless`` instead of ``1`` hence breaking even UNR.

.. _trs-equality-linearity:

Left-nonlinearity
=================

There are several notions of equality that could be used for non-linear patterns, here presented in the order of decreasing strength (earlier implies later):

* strict equality ``a == b`` - true if both sides reduce to same unique normal form, false if reduce to different unique normal forms, indeterminate if could reduce to same or different normal forms.
* syntactic equality ``syn_eq a b`` matches terms (reduced or unreduced) that are syntactically identical. It can match even if the term doesn't have a normal form. It is the notion commonly used for non-left-linear TRSs in the literature.
* oriented equality ``a ->* b`` holds if ``a`` reduces to ``b``.
* join equality ``a ↓ b`` means that a common reduct exists, i.e. there is a term ``c`` such that ``a -> c`` and ``b -> c``.
* semi-equational equality ``a ≈ b`` means that ``a`` can be rewritten to ``b`` via rewrites and inverse rewrites.

Computing any of these equalities is of complexity :math:`\Sigma^0_1` - at least :math:`\Sigma^0_1` because it is a nontrivial property of the reduction relation, but at most :math:`\Sigma^0_1` because for equal terms there is a finite rewrite sequence as proof. If reduction is convergent, then for strict equality this reduction sequence can be computed straightforwardly by reducing to normal form, whereas the others involve a brute force search.

Semi-equational equality has "spooky action at a distance" when non-deterministic terms are involved. Consider the system ``a = b; a = c; f x (not x) = d`` and the terms ``f {a,b,c} {a,b,c}``.

* For semi-equational equality, ``f a a`` reduces to the 4 combinations ``f {b,c} {b,c}``, but the "spooky" equality ``b ≈ c`` holds, so the ``f`` rule does not apply. Hence these 4 combinations are the normal forms.
* For strict, syntactic, oriented, and join equalities, ``b != c`` so the two heterogeneous combinations ``f b c`` and ``f c b`` reduce to ``d``. The ``f`` rule does not apply to ``f a a``, ``f b b``, or ``f c c``.

To ensure convergence we have to have stable conditions, meaning if the terms involved are reduced then they are still equal (Terese 4.11.1, page 145 / PDF page 165). For example consider the system ``a = b; f x | x == a = c`` and the term ``f a``.

* For strict, join, and semi-equational equality, we have that ``(a == a) = (a == b) = (b == b) = true`` so ``f a = f b = c`` and also ``f a = c`` directly.
* For syntactic and oriented equality, we do not have ``b == a``, so ``f a`` reduces to both ``f b`` and ``c`` and the system is nondeterministic.

Terese's example 4.11.5 that join equality is not confluent does not work because with the optimal prefixedpoint we have ``c e = e``. Still, join equality is unstable in a non-confluent system. For example ``f x | x == b = x; a = b; a = c`` and the term ``f a``:

* With strict, syntactic, and oriented equality, there are only 2 NFs: ``f a = f b = b``, and ``f a = f c``.
* With join and semi-equational equality, there is a third reduction pattern ``f a = a = b/c``, giving the additional normal form ``c``.

Overall strict equality is the most conservative (least accepting), and the one whose behavior seems easiest to understand. It does reduce the laziness of the language a bit but even Haskell's ``==`` function is strict. So we'll go with strict equality.

There is some question about reducible expressions as patterns, e.g. ``a = b; f a@x x = x``. I think this can be handled separately from non-linear patterns.

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

Higher-order matching
---------------------

If the substitution calculus is convergent, then terms can be represented by preterms in normal form.

Handling lambdas in RHSs is fairly straightforward, just beta-reduce as much as possible when they are encountered. But in higher-order term rewriting systems the lambdas can show up on the left hand side, in the pattern. The rewriting system is then defined modulo lambda reduction.

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

