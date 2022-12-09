Term rewriting
##############

Higher-order rewriting
======================

The definition is vaguely based on :cite:`vanoostromConfluenceAbstractHigherOrder1994`. But Ooostrom's HORS definition, like Terese, uses closed terms and no separate substitution, avoiding free variables by adding additional binders, e.g. he writes the rule ``f x = 1`` as ``\x. f x -> \x. 1``. I don't like this because it's unclear how it interacts with currying, eta reduction, and conditional rewriting, so I added the substitution back.

There is some question of whether context substitution is capture-avoiding, i.e. does ``(\x. □)[□ := x]`` not resolve to ``(\x. x)``. Terese says it captures. With Oostrom this substitution is forbidden since ``x`` is not a closed term. In our more liberal definition this resolves to ``(\y. x)`` by alpha-renaming the variable.

An example substitution calculus is the lambda calculus. The set of preterms is built from nullary symbols (variables, holes, symbols, and constants), applications of two preterms, and abstractions of a variable and a preterm. The substitution calculus rewriting rules are beta reduction and alpha renaming. Eta reduction can be added but makes the system only weakly orthogonal. :cite:`endrullisHighlightsInfinitaryRewriting2012`

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

In a general CTRS the reduction rules are of the form ``t | P -> s``, where ``P`` is a predicate. The allowed rewrites of a conditional rule are the unconditional rewrites ``t -> s`` that satisfy the condition ``P``. We can add a form of logic programming by allowing conditions to use variables not in the LHS, e.g. ``precedes x z | exists y. precedes x y && precedes y z = true``. This further extends to allowing variables on the RHS not present on the LHS. A further extension allows LHSs that are a single variable but have a condition - some authors disallows variables LHSs. Some syntax like ``l | exists x. C = r`` and ``l = exists x. r`` might make this easier to follow.

The definition of a CTRS is complicated by allowing predicates to refer to the rewrite relation ``->``. Ideally the rewrite relation would be defined as a fixed point of the rewrite rules. I.e. letting ``S`` be the system as a function of the rewrite relation ``->``, we would define ``->`` to be a relation ``R`` such that ``R = S(R)``. Terese presents the least fixed point. However, with badly-behaved conditions, no fixed point may exist, so instead we use the "optimal prefixedpoint", the intersection of the maximal prefixedpoints. I.e. we find the sets ``Pre = { R : R subseteq S(R) }, PreMax = { R in Pre : forall R' in Pre, R subseteq R' implies R= R' }, R = intersection PreMax``. We warn on all the reductions in ``S(R) \ R`` that make it not a fixed point.

For example, with a system with the single rule ``a -> a | a is a normal form``, ``S({}) = {(a,a)}`` and ``S({a,a}) = {}``. There is no fixed point, so the naive definition doesn't work. The optimal prefixedpoint is ``{}`` so ``R = {}``. But we warn that the reduction ``a -> a`` is not included. As another example, take ``a -> b if not(c -> d); c -> d if not(a -> b)``. The maximal (pre)fixedpoints are ``{(a,b)}`` and ``{(c,d)}``, but their intersection is ``{}`` so the optimal prefixedpoint is ``R = {}``. We warn that ``a -> b`` and ``c -> d`` are not included (extra diagnostics could show they are mutually exclusive). Lastly, ``a -> b if a -> b`` allows the reduction ``a -> b`` as it is the maximal (pre)fixedpoint.

The optimal prefixedpoint is correct in that reductions must satisfy conditions, conservative in that systems which have multiple differing interpretations do not reduce, but also a bit generous since it is maximal instead of least, and robust because independent rules are processed independently.

As far as terminology, the literature uses "term rewriting" to refer to unconditional term rewriting and "conditional term rewriting" otherwise. But many popular programming languages such as Haskell have conditional dispatch (guards, patterns, etc.), so we instead use "term rewriting" to refer to conditional and unconditional systems, and we refer to "unconditional TRSs" when necessary.

Cycles
======

The untyped lambda calculus has cycles, e.g. ``Omega = let w=\x.x x in w w`` reduces to itself and :cite:`venturinizilliReductionGraphsLambda1984` shows a 6-cycle ``M M I``. Similarly commutativity ``a + b = b + a`` generates cycles.

:cite:`dershowitzRewriteSystems1991`'s notion of a congruence-class rewriting system is helpful - the rewrite rules are split into rules R and reversible equations S, and we consider the system R/S (R mod S). A term where only S rules apply (no R rules apply) is considered a normal form. So similarly we consider the `condensation <https://en.wikipedia.org/wiki/Strongly_connected_component#Definitions>`__ of the rewrite graph, condensing each SCC to a single term. This does away with the "rewriting modulo equations" formalism while still maintaining its power.

A term is a "condensed normal form" if it has no reduction out of its SCC. Hence ``Omega``,  ``M M I`` and ``a + b`` would be condensed normal forms since their SCC contains themselves and they have no further reductions. We could further specify the normal form to be a canonical representative of the SCC, e.g. taking the smallest and lexicographically first element of the SCC, but leaving input unchanged seems better.

Orthogonal higher-order TRSs that are weakly head normalizing are acyclic, per :cite:`ketemaViciousCirclesRewriting2005`, so the cycle condensation doesn't affect standard functional programming because condensing acyclic rewriting systems gives back the same system. So the cycle detector doesn't have to be that great, even supporting associativity/commutativity is going into PhD territory.

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

This set, like the real numbers, is uncountably large and includes terms with no finite description. A more tractable subset is the rational terms, finite cyclic structures (called rational because rational numbers have the similar property that their decimal expansion repeats when written out), but these can't represent non-repeating structures like the list of fibonacci numbers. The implementation will have to use some computable approximation. Practically most programs will only deal with finite or rational terms so performance on other types of terms is not critical.

There are various extensions of the transitive closure to infinitary reduction, so the question arises as to which one to use. :cite:`kahrsInfinitaryRewritingClosure2013` discusses several and provides an ordering so that each is mostly a proper subset of the next (not sure about P* subset bi-infinite). Many of these use the monotonic closure X*, which is the least fixedpoint of the function G defined as G(R) = X(R) union R, which by the (transfinite) Kleene fixed-point theorem exists and is the limit/union of the sequence :math:`X^0 = \emptyset, X^{n+1} = G(X^n), X^\delta = \bigcup_{\alpha < \delta} X^\alpha`.

* S*, the monotonic closure of strongly converging reduction sequences, "strong" being a requirement that the depth of the redexes contracted in successive steps tends to infinity. S=S* for "compatible" TRSs, ones where t R u imply C[t] R C[u] for any context C, which all iTRSs satisfy.
* W*=A=A*, the monotonic closure of weakly converging reduction sequences, and also the `adherent points <https://en.wikipedia.org/wiki/Adherent_point>`__ of reduction sequences in the metric space. Weak convergence by itself is not transitively closed, e.g. ``a = b; f x a = f (g x) a`` has ``f c a -ω> f (g (g (g ...))) a -> f (g (g (g ...))) b`` :cite:`dershowitzRwriteRewriteRewrite1991` :cite:`simonsenWeakConvergenceUniform2010`, hence the need for closure. By definition of adherent point, each w-reduct is either an accumulation point, i.e. a appears arbitrarily close infinitely often in a reduction sequence, or an isolated point which can be reached in a finite number of reductions.
* P*: the monotonic closure of the pointwise closure of the reflexive transitive closure (finite multi-step relation).
* bi-infinite rewriting, defined in :cite:`endrullisCoinductiveFoundationsInfinitary2018` Section 6.2 as the greatest relation R such that R = the reflexive transitive closure of single-step rewriting union R lifted to apply to subterms.
* T*: the monotonic closure of T, the topological closure of the reflexive transitive closure. T itself is not transitively closed, e.g. ``leq 0 x = true; leq (s x) (s y) = leq x y; inf = s inf`` has ``leq inf inf T leq (mu x. s x) (mu y. s y) T true`` (by topological closure of finite approximations of the S towers) but not ``leq inf inf T true`` (because the terms are of finite depth). Alternatively I have defined T* as the smallest relation M such that M is reflexively, transitively, and topologically closed and contains the single-step relation, which I think is equivalent.

S* is the standard in the literature but doesn't have much going for it besides that. If there is a reduction that switches heads, ``a X = b (c X); b X = a (c X)``, then S* says there are no w-reductions. W* has ``a e -w> a (mu x. c x)`` and ``a e -w> b (mu x. c x)``. TRSs are in general nondeterministic, so the "strong" definition that requires a single limit to exist is too strong.

For cycle condensation we would like to equate as many terms as possible to get large SCCs, and similarly a large reduction relation means there will be an escape from infinite regresses. As an example, with bi-infinite rewriting or T*, the hypercollapsing term ``mu x. C x`` with rule ``C x = x`` will reduce to every term (limit of approximations ``C^n f = f``), making it ambiguous, while with W* and P* the hypercollapsing term only reduces to itself hence is a condensed normal form. Similarly with ``C A = A`` where ``A`` is a constant, ``mu x. C x = A`` with bi-infinite/T* but W*/P* don't reduce at all. Bi-infinite and T* seem equally simple to formalize since they are both single fixed points, so it seems T* wins because it's larger.

Also conditional rewriting can interact with infinite reduction and cause unwanted behavior with a weak closure. For example consider the system ``ds x y | x == y = e`` and reducing the infinite term ``G = ds G G`` (in :cite:`klopCombinatoryReductionSystems1980` this is achieved by the system ``G = a = c a; c x = ds x (c x)``). Since ``e`` is a normal form hence equal to itself, all finite terms defined by ``T = { x : x == e or x in ds T T }`` reduce to ``e``. So using a bi-infinite closure, ``G`` uniquely reduces to ``e``. But with a weak closure ``X = ds e X`` is a normal form and the system becomes nondeterministic. Similarly with ``dk x y | x == y = e x`` and ``G = dk G G``, we should get ``e (e (e ...))`` as the unique result, but with a weak closure we don't. Another tricky system is ``c x | x == c x = e; b = c b`` - the obvious reduction is ``b = mu x. c x = e``, but this system has a hidden circularity of the form ``mu x. c x = e`` if ``mu x. c x = e``.

The common notions of an ARS carry over to infinitary reductions: :cite:`endrullisInfinitaryTermRewriting2014`

* transitive reduction: irreflexive kernel of reduction closure
* normal form: irreducible term
* strongly normalizing (terminating): every infinite reduction sequence has a limit
* nonterminating reduction: infinite reduction sequence with no limit or that does not reduce to its limit
* weakly normalizing (normalizing): every term has a reduction to a normal form
* confluence: if t reduces to t1 and t2, then there is a common term s such that t1 and t2 reduce to s.
* Church-Rosser: if t1 is equivalent to t2, then there is a common term s such that t1 and t2 reduce to s.
* normal form property w.r.t. reduction:: if u reduces to t and s, and s is a normal form, then t reduces to s
* normal form property: if t is equivalent to s and s is a normal form, then t reduces to s
* unique normalization w.r.t. reduction: if t reduces to t1 and t2, and t1, t2 are normal forms, then t1=t2
* unique normalization: if t1 is equivalent to t2, and t1, t2 are normal forms, then t1=t2

However common theorems such as Newman's lemma do not, so it is not clear how useful these are.

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


References
[1] C. Appel, V. van Oostrom, and J. G. Simonsen. Higher-order (non- )modularity. In Proc. 21st RTA,
volume 6 of LIPIcs, pages 17–32, 2010.
[3] R. Mayr and T. Nipkow. Higher-order rewrite systems and their confluence. TCS, 192(1):3–29, 1998.
[4]
[5] T. Nipkow. Higher-order critical pairs. In Proc. 6th LICS, pages 342–349, 1991.
[6] Tobias Nipkow. Functional unification of higher-order patterns. In Proc. 8th LICS, pages 64–74,
1993.

CoLL
FORT-h, FORTify
CSI+CeTA
ACP+CeTA
CONFident
CO3
AGCP
infChecker
NaTT

The confluence tool CoLL uses a version of Newman's lemma adapted for rewriting modulo, i.e. requires proving termination and local confluence.

a minimal complete set of A-unifiers need not to be finite, and thus enumeration may not terminate. In
fact there is a one-rule TRS that admits infinitely many A-critical pairs. However every left-linear TRS has a finite set.

Note that our tool uses the algorithm in [16] for AC unification and flattened
term representation for overcoming the coherence problem of AC-rewriting. Since

http://cops.uibk.ac.at/

 http://www.jaist.ac.jp/project/saigawa/

20. Durán, F., Meseguer, J.: A Church-Rosser checker tool for conditional order-sorted
equational maude specifications. In: Rewriting Logic and Its Applications. (2010)
69–85

CRC is a powerful
Church-Rosser checker for Maude and supports the Church-Rosser modulo the-
orem for any combination of associativity, commutativity, and/or identity the-
ories, except associativity theory.

 Zankl, H., Felgenhauer, B., Middeldorp, A.: CSI — a confluence tool. In: Proc.
23rd CADE. Springer (2011) 499–505

ACP and CSI [18]
employ layer-preserving decomposition [17] to split a TRS into subsystems.

Ohlebusch, E.: Modular Properties of Composable Term Rewriting Systems. PhD
thesis, Universität Bielefeld (1994)

When handling TRSs that contain reversible
rules, ACP [9] employs reduction-preserving completion [21]. This method effec-
tively works for C and AC rules, but not for associativity rules.

Peterson, G., Stickel, M.: Complete sets of reductions for some equational theories.
Journal of the ACM 28(2) (1981) 233–264

Aoto, T., Toyama, Y.: A reduction-preserving completion for proving confluence
of non-terminating term rewriting systems. LMCS 8(1) (2012) 1–29

CoLL was designed to complement Saigawa. The two tools will be
merged in the next version.


* Huet–Toyama–van Oostrom’s criterion [20]
* Huet’s strong-closedness criterion[7]

* Non-confluence: A term s is non-confluent if it fails unique normal forms with respect to reductions (UNR), i.e. a term s reduces to terms t and t', and one of the following holds: :cite:`aotoProvingConfluenceTerm2009`

  * t,t' in normal form and not equal
  * t,t' in head normal form and heads not equal. HNF is approximated by replacing redexes with variables and ensuring no rules unify.

  Maude has two commands "reduce" and "search". Reduce assumes confluence and just applies rules randomly. Search actually explores the full reduction tree and finds all final states.


References
1. Hindley, J.R.: The Church-Rosser Property and a Result in Combinatory Logic.
PhD thesis, University of Newcastle-upon-Tyne (1964)
2. Baader, F., Nipkow, T.: Term rewriting and all that. Cambridge University Press
(1998)
3. Jouannaud, J.P., Kirchner, H.: Completion of a set of rules modulo a set of equa-
tions. SIAM Journal on Computing 15(4) (1986) 1155–1194
4. Rubio, A.: A fully syntactic AC-RPO. Information and Computation 178(2)
(2002) 515–533
5. Plotkin, G.: Building in equational theories. Machine Intelligence 7 (1972) 73–90
6. Schulz, K.: Word unification and transformation of generalized equations. In: Word
Equations and Related Topics. Volume 677 of LNCS. (1993) 150–176
7. Schmidt, R.: E-unification for subsystems of s4. In: Proc. 9th RTA. Volume 1379
of LNCS. (1998) 106–120
8.
9. Aoto, T., Yoshida, J., Toyama, Y.: Proving confluence of term rewriting systems
automatically. In: RTA 2009. Volume 5595 of LNCS. (2009) 93–102
10. Huet, G.: Confluent reductions: Abstract properties and applications to term
rewriting systems. Journal of the ACM 27(4) (1980) 797–821
11. Toyama, Y.: Commutativity of term rewriting systems. In Fuchi, K., Kott, L., eds.:
Programming of Future Generation Computers II. North-Holland (1988) 393–407
12. van Oostrom, V.: Developing developments. Theoretical Computer Science 175(1)
(1997) 159–181
13. van Oostrom, V.: Confluence by decreasing diagrams converted. In Voronkov, A.,
ed.: Proc. 19th RTA. Volume 5117 of LNCS. (2008) 306–320
14. Aoto, T.: Automated confluence proof by decreasing diagrams based on rule-
labelling. In: Proc. 21st RTA. Volume 6 of LNCS. (2010) 7–16
15. Baader, F., Nipkow, T.: Term Rewriting and All That. Cambridge University
Press (1998)
16. Pottier, L.: Minimal solutions of linear diophantine systems: Bounds and algo-
rithms. In: Proc. 4th RTA. Volume 488 of LNCS. (1991) 162–173
17.
18.
19. Hirokawa, N., Middeldorp, A.: Decreasing diagrams and relative termination. Jour-
nal of Automated Reasoning 47(4) (2011) 481–501
21.
22. Toyama, Y.: On the Church-Rosser property for the direct sum of term rewriting
systems. Journal of the ACM 34(1) (1987) 128–143


 For this example, the system is weakly orthogonal so it is easy to analyze. Stroscot proves that the answer is unique because in the one ambiguous case ``myand False False`` both clauses give ``False``. Stroscot then selects the parallel outermost reduction strategy which will always succeed if there is an answer. Hence evaluating ``myand expensive_undefined False`` reduces ``expensive_undefined`` for a little bit, then evaluates ``False`` to completion and matches the second clause.

the parallel outermost strategy assumes an oracle finding the redexes of a term. With a CTRS determining whether a term is a redex is of complexity Sigma_0^1.

 In the case of UNR for ground TRSs, R. Verma. Complexity of normal form properties and reductions for term rewriting problems. Fundamenta
Informaticae, 92(1–2):145–168, 2009. doi: 10.3233/FI-2009-0070. and G. Godoy and F. Jacquemard. Unique normalization for shallow TRS. In Proc. 20th International
Conference on Rewriting Techniques and Applications, volume 5595 of Lecture Notes in Computer Science,
pages 63–77, 2009. doi: 10.1007/978-3-642-02348-4_5.
have established that polynomial time algorithms exist, using tree automata techniques. No
precise bound is given by these authors. In Section 5 we present an O(R^3 log R) time
algorithm for deciding UNR, where R denotes the sum of the sizes of the rules of R, and the size of a rule is the sum of the sizes of its left-hand side and
right-hand side.

In the complexity analysis we make use of the fact that systems of Horn clauses can
be solved in linear time, see Dowling and Gallier [5]. Their procedure finds the smallest
solution of a set of Horn clauses, in the sense that as few atoms as possible become true,
in time linear in the total size of the clauses. In our implementation, we do not generate these Horn clauses explicitly.
Instead, whenever we make a new inference p → q ∈ F, we check all possible rules that
involve p → q ∈ F as a premise. The result is an incremental algorithm (see Listing 6).
From an abstract point of view, however, this is essentially the same as solving the Horn
clauses as stated above. This remark also applies to inference rules presented later.


Some of the algorithms presented in this article use maximally shared terms for efficiency;
this idea is also known as hash consing. In a maximally shared representation, each ground
term f (t1 , . . . , tn ) is represented by a unique identifier (e.g., a natural number, or a pointer
into memory), which can be mapped to f and the identifiers of t1, . . . , tn . In order to
maintain maximal sharing, a lookup table mapping f and identifiers of t1, . . . , tn to the
identifier of f (t1, . . . , tn) is required. If the arity of f is bounded, constructing maximally
shared terms incurs a logarithmic overhead compared to a direct construction. Crucially
though, comparing two maximally shared terms takes constant time.


The set of normal forms of a ground system is a regular
language; in fact this is true for left-linear systems [2]. The idea is that the state [*] accepts those R-normal forms s that are not subterms of R, while normal form subterms are accepted in state [s].


curry TS to bound arity
compute the automaton N that accepts the R-normal forms (Section 3.3),
compute the rewrite closure (F, E) that allows checking reachability. The point of the rewrite closure is that reachability can be decomposed into a decreasing sequence of steps in E and F, followed by an increasing sequence of steps in E− and F.

A ground term is flat if it is either a constant or a function symbol applied to constants. A rule is flat if its LHS and RHS are flat. The idea of flattening goes back to Plaisted [17]. It makes all rules flat and sets up a bijection E between terms and flat terms. Subterms become constants in Σ[].

flatten(R):
  E ← ∅, R' ← ∅
  for all l → r ∈ R◦ do
    add mk(l) → mk(r) to R'

mk (App a b) =
  c1 = mk a
  c2 = mk b
  return $ lookupOrInsert (App c1 c2) (fresh constant)

If UNR does not hold, then distinct normal forms s and t. Assume WLOG minimum total size. If the peak has no root step then we can project it to the arguments, and obtain a smaller counterexample, hence the peak has a root step in its left part. Using the rewrite closure (cf. Lemma 3.19), the peak can be decomposed as
s →E∪F− · ←E∪F · →E∪F · ←E∪F− t

The left part has a root step. This means that there is a constant p such that s →E∪F− p and p ←E∪F · →E∪F · ←E∪F− t. First we consider the special case p ←E∪F− t. We define the first UNR-condition as that if for all R◦ -normal forms s, t and p ∈ Σ[], s →E∪F- p E∪F← t implies s = t. If the first UNR-condition is violated, then UNR clearly does not hold. If the first UNR-condition is satisfied, then we can define a partial function w(p) = t ⇐⇒ t is a normal form and t →∗E∪F− p.

Now considering ←E∪F · →E∪F, every forward E step is preceded by an inverse E step at the same position, inducing a peak between two constants from Σ[]. Define that p, q ∈ Σ[] are meetable if p E∪F← · →E∪F q.
Using the parallel closure of the meetable relation, q ←E∪F · →E∪F · ←E∪F− t iff there is a multi-hole context C and constants q1 , . . . , qn, p1, . . . , pn such that q ←E∪F C[q1, . . . , qn ], C[q1, . . . , qn ] ↑∥ C[p1 , . . . , pn ], and C[p1 , . . . , pn ] ←E∪F− t. Note that because s and t are normal forms, we have  s →E∪F− p , s = w(q) and t = C[w(p1), . . . , w(pn )].

Define the second UNR-condition as if (5.2) for R◦-normal forms s, t ∈ T (Σ◦ )
implies s = t. A curried ground TRS is UNR if and only if the first and second UNR-
conditions are satisfied.


Theorem:

DECIDING CR, NFP, UNC, AND UNR FOR GROUND TRSs
 17
p ∈ Σ[]
 p1 ◦ p2 → p ∈ E
 p1 ↑ q1 p2 ↑ q2
 q1 ◦ q2 → q ∈ E
refl
 cong
p ↑ p
 p ↑ q
q → p ∈F
 q ↑ r
 r ∈ Σ[]
 p ∈ Σ[]
 p ↑ q q → r ∈F
step l
 step r
p ↑ r
 p ↑ r

Lemma 5.4.
5.2. Computing Meetable Constants. The meetable constant relation ↑ can be com-
puted in a way similar to the rewrite closure from Section 3.5, using the inference rules in
Figure 9. So for this subsection, let ↑ be defined by those inference rules. The following
lemma shows that ↑ coincides with the meetable constants relation, justifying the symbol.
Lemma 5.5. For p, q ∈ Σ[], p ↑ q if and only if p E∪F ∗ ← · →∗E∪F q.
Proof. The proof follows the same principles as that of Lemma 3.20, so let us be brief.
First note that all rules in Figure 9 are consistent with the requirement that p ↑ q implies
p E∪F ∗← · →∗E∪F q.
On the other hand, assume that there is peak p E∪F ∗← · →∗E∪F q such that p ↑ q
does not hold. Choose such a peak of minimal length. Then either p = q, and (refl )
applies, or p F ← p0 ↑ q and (stepl ) applies, or p ↑ q0 →F q and (stepr ) applies, or
p E ← p1 ◦ p2 ↑∥ q1 ◦ q2 →E q, in which case (cong) applies. In each case, p ↑ q follows,
contradicting the assumption.
Note that as in the case of the rewrite closure, there is no (cong) rule for constants
c ∈ Σ◦ , because they would be instances of (refl ). There are O(kRk) instances of (refl ),
There are O(kRk2 ) instances of (cong) (because p and q determine p1 , p2 , q1 , and q2), and
O(kRk3 ) instances each of (step l ) and (step r ). Using Horn inference with p ↑ q as atoms,
the computation of ↑ takes O(kRk3) time.
Example 5.6 (continued from Example 3.21). In the same spirit as Example 3.21, we
present the relations ↑U and ↑V as tables, with the entries indicating rules and stage numbers.
f
 a
 fa
 b
 fb ffb fffb
f r 0
f a fa b
0
 a
 r0
 s1 l
 s1 l
 s1 l
 s1 l
 s1 l
f r 0
fa
 s1 r
 r s1 s1 l
 s1 s1↑U = a
 r0 s1 l s2 r
 ↑V =
 l
 l
 l
0 b
 s1 r
 s1 r
 r0
 s1 l
 s1 l
 s1 l
fa
 s1 r r s1 r
fb
 s1 r
 s1 r
 s1 r
 r0
 s1 l
 s1 l
b
 s2 s1 r0
l l ffb
 s1 r
 s1 r
 s1 r
 s1 r
 r0
 s1 l
fffb
 s1 r
 s1 r
 s1 r
 s1 r
 s1 r
 r 0
18
 BERTRAM FELGENHAUER
c E R◦ c normal form
 s ∈ W (p, q) p0 → p ∈ F
base
 stepF
c ∈ W ([c], [c])
 s ∈ W (p0 , q)
s1 ∈ W (p1 , q1 )
 s2 ∈ W (p2 , q2) p1 ◦ p2 → p ∈ E
 q1 ◦ q2 → q ∈ N
stepE
s1 ◦ s2 ∈ W (p, q)
Figure 10: Inference rules for s ∈ W (p, q)
1: compute rewrite closure (F, E) and a representation of N
2: let w(p) and n(p) be undefined for all p ∈ Σ[] (to be updated below)
3: for all constants c E R◦ that are normal forms do
4:
 push ([c], [c], c) to worklist
 — (base)
5: while worklist not empty do
6:
 (p, q, s) ← pop worklist
 — s ∈ W (p, q)
7:
 if w(p) = t is defined then
 — t ∈ W (p, n(p))
8:
 if s 6= t then
9:
10:
11:
12:
13:
14:
15:
16:
17:
return UNR(R) is false
 — first UNR-condition violated by s and t
continue at 5
w(p) ← s, n(p) ← q
for all rules p1 ◦ p2 → pr ∈ E with p ∈ {p1, p2 } do
if w(p1 ) = s1 and w(p2 ) = s2 are defined then
if there is a transition n(p1 ) ◦ n(p2 ) → qr ∈ N then
push (pr , qr , s1 ◦ s2) to worklist
 — (stepE )
for all rules p0 → p ∈ F do
push (p0, q, s) to worklist
 — (stepF )
Listing 11: Checking the first UNR-condition and computing w and n.
5.3. Checking UNR. We start by checking the first UNR-condition. To perform this
computation efficiently, we make use of the automaton N that recognizes normal forms,
cf. Section 3.3. The fact that s is a R◦ -normal form is witnessed by a run s →∗N q. Let
W (p, q) = {s | s ∈ T (Σ◦), s →∗E∪F − p ∈ Σ[] and s →∗N q ∈ ΣN }
Lemma 5.7. The predicate s ∈ W (p, q) is characterized by the inference rules in Figure 10.
Proof. The inference rules follow by an inductive analysis of the last step of the s →∗E∪F − p
reduction, where s →∗N q. Recall that N is deterministic, so q is determined by s.
(base) If there is a single step, then it must be using a rule s = c → [c] = p from E, where
c ∈ Σ, and c is a R◦ -normal form, which ensures that c → [c] = q ∈ N as well.
(stepF ) If the last step is an F − step, then s →∗E∪F − p0 →F − p, and there is a q with
s ∈ W (p, q).
(stepE ) If the last step is an E step but s is not a constant, then s = s1 ◦s2 →∗E∪F − p1 ◦p2 →E p,
and there are q1 , q2 with s1 ∈ W (p1 , q1 ) and s2 ∈ W (p2, q2 ).
Conversely, each derivation of s ∈ W (p, q) by these three inference rules gives rise to rewrite
sequences s →∗E∪F − p and s →∗N q.
The corresponding code is given in Listing 11. In addition to w(q) (which we introduced
immediately after Definition 5.1) we also compute a partial function n(q) which returns the
DECIDING CR, NFP, UNC, AND UNR FOR GROUND TRSs
 19
state of N that accepts w(q) if the latter is defined. The computed witnesses may have
exponential size (see Example 5.9), so in order to make the check on line 8 efficient, it is
crucial to use maximal sharing.
Example 5.8 (continued from Example 3.11). Let us check the first UNR-condition for
U ◦ = {fa → a, fa → b, a → a} according to Listing 11. The constant normal forms are f and
b, so we add ([f], [f], f) and ([b], [b], b) to the worklist. Then we enter the main loop. We may
ignore duplicate entries on the worklist, because they are skipped on line 10. Taking this
into account, the main loop is executed 3 times.
(1) ([f], [f], f) is taken from the worklist, and we assign w([f]) = f and n([f]) = f. The
conditions on lines 12 and 13 are never satisfied. Because [f] → [f] ∈ FU , ([f], [f], f) is
added to the worklist again by the final loop (lines 16 and 17).
(10
 ) ([f], [f], f) (which is a duplicate) is taken from the worklist, but now w([f]) = f is defined
and we reach line 10.
(2) ([b], [b], b) is taken from the worklist, and we assign w([b]) = b and n([b]) = [b]. Line 15
is not reached. Line 17 is reached for [b] → [b] ∈ FU and [fa] → [b] ∈ FU and we add
([b], [b], b) (a duplicate) and ([fa], [b], b) to the worklist.
(3) ([fa], [b], b) is taken from the worklist, and we assign w([fa]) = b and n([fa]) = [b].
([fa], [b], b) (a duplicate) is added to the worklist on line 17, because [fa] → [fa] ∈ FU .
The loop terminates without reaching line 9, so the first UNR-condition is satisfied for U.
We have derived the following exhaustive list of instances of s ∈ W (p, q) derivable by the
rules in Figure 10, corresponding to the normal forms f and b.
f ∈ W ([f], [f])
 b ∈ W ([b], [b])
 b ∈ W ([fa], [b])
For V, there is only one constant normal form, namely f. Hence, initially, we add
([f]R , [f], f) to the worklist. Then we enter the main loop (lines 5–17). On line 11, w([f]) is
set to f and n([f]) is assigned [f]. The only way that the check on line 10 could succeed
would be having a rule with left-hand side [f] ◦ [f] in EV , which is not the case. On line 17,
we add ([f]R , [f], f) to the worklist again, but in the next loop iteration, line 10 is reached.
The loop terminates, having recorded the normal form f with f ∈ W ([f], [f]).
Example 5.9. We exhibit a class of TRSs with exponential witness size. To this end, fix
k > 0 and consider the rules
ak → b
 ai → ai−1 ◦ ai−1
where 0 < i ≤ k. The check of the first UNR-condition will find the two normal forms b and
tk of ak , where t0 = a0 and ti+1 = ti ◦ ti for 0 < i ≤ k. The term tk has size 2k+1 − 1, but
only k − 1 distinct subterms.
Remark 5.10. The check of the first UNR-condition (Listing 11) is similar to the check of
UNC (Listing 8), with a few crucial differences:
• First, we use the automaton A = (Σ[], Σ[] , E ∪ F −) instead of C.
• Because A is not deterministic (the rules of F − are -transitions), different runs may
result in the same term. Hence the check on line 8 is needed, and witnesses need to be
stored, using maximal sharing for efficient equality tests.
• Furthermore, in addition to lines 10–13 in Listing 8, which correspond to lines 12–15 in
Listing 11, we need a similar loop processing the -transitions from F − , cf. lines 16–17 in
Listing 11. The latter change increases the complexity from O(kRk log kRk) to O(kRk2 ).
20
 BERTRAM FELGENHAUER
s ∈ W (p0 , q) p ↑ p0
 s ∈ W 0(p, q) p → p0 ∈ F
0 base0
 0 step0F
s ∈ W (p, q)
 s ∈ W (p0 , q)
s1 ∈ W 0(p1 , q1 )
 s2 ∈ W 0 (p2 , q2) p1 ◦ p2 → p ∈ E
 q1 ◦ q2 → q ∈ N
step0E
s1 ◦ s2 ∈ W 0(p, q)
Figure 12: Inference rules for s ∈ W 0 (p, q)
1: check first UNR-condition (obtaining w(·) and n(·)), and compute ↑
2: let w 0 (p, q) be undefined for all p ∈ Σ[] , q ∈ Σ[] ∪ {[?]} (to be updated below)
3: for all p, q with p ↑ q and w(q) defined do
4:
 push (p, n(q), w(q)) to worklist
 — (base0 )
5: while worklist not empty do
6:
 (p, q, s) ← pop worklist
 — s ∈ W 0(p, q)
7:
 if w 0
 (p, q) = t is defined then
 — t ∈ W 0(p, q)
8:
 if s = t or t = ∞ then
9:
 continue at 5
10:
 w 0 (p, q) ← ∞, s ← ∞
 — |W 0 (p, q)| ≥ 2
11:
 else
12:
 w 0 (p, q) ← s
13:
 if w(p) = t is defined and t 6= s then
14:
 return UNR(R) is false
 — second UNR-condition violated by t and s
15:
 for all p1 ◦ p2 → pr ∈ E and states q1 , q2 of N with (p, q) ∈ {(p1, q1 ), (p2 , q2)} do
16:
 if w0 (p1 , q1) = s0 1 and w0 (p2 , q2) = s0 2 are defined then
17:
 if there is a transition q1 ◦ q2 → qr ∈ N then
18:
 push (pr , qr , s0 1 ◦ s0 2) to worklist
 — (step0E )
19:
 for all rules p → p0
 ∈ F do
20:
 push (p0, q, s) to worklist
 — (step0F )
21: return UNR(R) is true
Listing 13: Checking the second UNR-condition.
For the second UNR-condition, let W 0 (p, q) be the set of R◦ -normal forms t ∈ T (Σ◦)
that are accepted by N in state q and satisfy the right part of (5.2), i.e.,
∗
 ∗
 ∗
p ←−−− C[p1 , . . . , pn ] ↑∥ C[q1, . . . , qn ] ←−−−− t −→ q
 (5.3)
E∪F
 E∪F −
 N
Lemma 5.11. The predicate s ∈ W 0 (p, q) is characterized by the inference rules in Figure 12.
Proof. The inference rules follow by an inductive analysis on the left-most step of the
p E∪F ∗← C[p1 , . . . , pn ] subreduction of (5.3):
(base0 ) If the sequence is empty, we have p = p1 , p1 ↑ q1, and t ∈ W (q1 , q). Conversely, we
have t ∈ W 0(p, q) whenever t ∈ W (p0, q) and p ↑ p0.
(step0F ) If the leftmost step is an F step, we have p F ← p0 E∪F ∗← C[q1 , . . . , qn ], and
t ∈ W 0 (p, q) for some q; in that case, t ∈ W 0(p0 , q) follows.
(step0E ) If the leftmost step is an E step, then either p E ← t ∈ Σ, but that case is already
covered by (base0 ), or p E ← p1 ◦p2, t = t1 ◦t2 , and there are states q1 , q2 with t1 ∈ W 0 (p1 , q1 ),
t2 ∈ W 0(p2 , q2), and a transition q1 ◦ q2 → qr ∈ N .
DECIDING CR, NFP, UNC, AND UNR FOR GROUND TRSs
 21
Because W 0 (p, q) may be an infinite set, we instead compute the partial function w0 (p, q) that
returns s if W 0(p, q) = {s} is a singleton set, or the special value ∞ if W 0 (p, q) has at least
two elements, where ∞ is distinct from any term and satisfies ∞ ◦ t = t ◦ ∞ = ∞ ◦ ∞ = ∞
for all terms t. The system has the UNR property if w0 (p, q) = w(p) whenever w0(p, q) and
w(p) are both defined.
The procedure is given in Listing 13. It maintains a worklist of tuples (p, q, s) where
s is either a term with s ∈ W 0 (p, q), or has the special value s = ∞. Note that each value
w 0 (p, q) may be updated up to two times: it starts out as undefined, may be updated to an
element of W 0 (p, q), and later ∞ 0to if W (p, q) has at least two elements. These updates are
performed by lines 7–12.

In order to achieve the desired complexity, care must be taken with the enumeration
on line 15: instead of iterating over all elements of E, an index mapping p to the rules
p1 ◦ p2 → pr ∈ E with p ∈ {p1 , p2} should be used.
The following two lemmas establish key invariants for showing that the procedures in
Listings 11 and 13 faithfully implement the inference rules in Figures 10 and 12, respectively.
Lemma 5.13. Whenever line 5 is reached in Listing 11, we have
(1) If (p, q, s) ∈ worklist or s = w(p) is defined and q = n(p), then s ∈ W (p, q) holds.
(2) Assume that ŝ ∈ W (p̂, q̂) can be inferred using an inference rule from Figure 10 with
premises P1 , . . . , Pn . Then either (p̂, q̂, ŝ) ∈ worklist, or ŝ = w(p̂) and q̂ = n(p̂), or there
is a premise Pi = s0 ∈ W (p0 , q0) such that w(p0 ) is undefined or s0 6= w(p0 ).
Proof. For the first invariant, first note that w(p) and n(p) are updated simultaneously
on line 11 with values that are taken from the worklist on line 6, so we may focus on the
addition of items to the worklist, which happens on lines 4, 15 and 17. On line 4, c is a
22
 BERTRAM FELGENHAUER
normal form, and c ∈ W ([c], [c]) holds by rule (base). On line 15, we have s1 ∈ W (p1 , q1)
and s2 ∈ W (p2 , q2 ), and rules p1 ◦ p2 → pr ∈ E and q1 ◦ q2 → qr ∈ N , allowing us to infer
s1 ◦ s2 ∈ W (pr , qr ) by (stepE ). Similarly, on line 17, we have s ∈ W (p, q) and p0 → p ∈ F,
and s ∈ W (p0, q) follows by (stepF ).
Consider the second invariant immediately after the loop on lines 1–4. If ŝ ∈ W (p̂, q̂)
can be derived by (base), then it is put on the worklist by that loop. All other inferences
have a premise s0 ∈ W (p0 , q0 ) for which w(p0 ) is undefined, since w(·) is nowhere defined.
So initially, the invariant holds. Noting that once w(p̂) is set, it will never be changed, the
invariant can be invalidated in only two ways.
(1) (p̂, q̂, ŝ) ∈ worklist is the item taken from the worklist on line 6. In this case, either
the algorithm aborts early on line 9, or we reach line 10, which ensures w(p̂) = ŝ and
n(p̂) = q̂ since ŝ determines q̂, or we reach line 11, which assigns w(p̂) = ŝ and n(p̂) = q̂.
So the invariant is maintained.
(2) There is a premise s0 ∈ W (p0 , q0) and w(p0) is assigned s0 on line 11; in that case, (p0 , q 0, s0 )
must be the most recent item taken from the worklist on line 6. This can only happen if
ŝ ∈ W (p̂, q̂) is derived by one of the rules (stepE ) or (stepF ) in the last step.
If the (stepE ) rule is used, let us assume that s0 = s1 , p0 = p1 and q 0 = q1 (the case
that s0 = s2, p0 = p2 and q 0 = q2 is completely analogous). So s1 ∈ W (p1, q1 ) holds.
If the other premise s2 ∈ W (p2 , q2) does not satisfy w(p2) = s2 , then the invariant
remains true. If both w(p1 ) = s1 and w(p2 ) = s2, then n(p1) = q1 and n(p2 ) = q2 follow
(because N is deterministic); since ŝ ∈ W (p̂, q̂) is derivable by (stepE ), there must also
be rules p1 ◦ p2 → p̂ ∈ E (where (p0, q 0 ) is one of (p1, q1 ) or (p2 , q2)) and q1 ◦ q2 → q̂ ∈ N .
Consequently, (p̂ = pr , q̂ = qr , ŝ = s1 ◦ s2 ) will be added to the worklist on line 15.
If the (stepF ) is used, q̂ = q0 holds and there must be a step p̂ → p0 ∈ F; hence (p̂, q̂, ŝ)
will be put on the worklist on line 17.
Lemma 5.14. Whenever line 5 is reached in Listing 13, we have
(1) If (p, q, s) ∈ worklist or s = w0(p, q) is defined, then s ∈ W 0(p, q) or s = ∞ and
|W 0 (p, q)| > 1.
(2) Assume that ŝ ∈ W 0 (p̂, q̂) can be inferred using an inference rule from Figure 12 with
premises P1, . . . , Pn . Then (p̂, q̂, ŝ) ∈ worklist, w0 (p̂, q̂) = ŝ, w 0 (p̂, q̂) = ∞, or there is a
premise Pi = s0 ∈ W 0 (p0 , q0) such that w 0(p0, q 0 ) is not equal to s0 or ∞.
Proof. Consider the first invariant. First note that w0 (p, q) is only updated on lines 10
and 12. In this case, (p, q, s) ∈ worklist was true at the beginning of the loop, so s ∈ W 0 (p, q)
or s = ∞ and |W 0(p, q)| > 1. This justifies setting w0 (p, q) = s on line 12. On line 10, we
additionally have t ∈ W 0 (p, q); we conclude that |W 0 (p, q)| > 1 (justifying w 0 (p, q) = ∞)
because either s = ∞, or s 6= t and s, t ∈ W 0 (p, q).
Hence we may focus on the items put on the worklist. On line 4, since w(q) ∈ W (q, n(q))
and p ↑ q, w(q) ∈ W 0(p, n(q)) follows. On line 18, we have p1 ◦ p2 → pr ∈ E and
q1 ◦ q2 → qr ∈ E. Moreover, we have either s0 1 ∈ W 0(p1 , q1 ) or s0 1 = ∞ and |W 0(p1 , q1 )| > 1;
and either s0 2 ∈ W 0 (p2, q2 ) or s0 2 = ∞ and |W 0(p2 , q2 )| > 1. If neither s0 1 = ∞ nor s0 2 = ∞,
then we have s0 1 ◦ s0 2 ∈ W 0 (p, q) by (stepE ). Otherwise, since we can derive t1 ◦ t2 ∈ W 0(p, q)
by (stepE ) for any t1 ∈ W 0(p1 , q1) and t2 ∈ W 0(p2 , q2 ), |W 0(p, q)| > 1 follows, and s0 1 ◦s0 2 = ∞.
So the invariant holds. Finally, on line 20, we have p → p0 ∈ F, and either s ∈ W 0(p, q) or
s = ∞ and |W 0 (p, q)| > 1. In the former case, s ∈ W 0(p0, q) by (stepF ), while in the latter
case, t ∈ W 0 (p0 , q) for any t ∈ W 0 (p, q); either way, the invariant holds.
DECIDING CR, NFP, UNC, AND UNR FOR GROUND TRSs
 23
Next we consider the second invariant. Immediately after the loop on lines 3–4, if
ŝ ∈ W 0(p̂, q̂) follows by (base0 ) then (p̂, q̂, ŝ) will be on the worklist. For all other inferences of
some ŝ ∈ W 0 (p̂, q̂), there is a premise s0 ∈ W 0 (p0, q 0 ) such that w 0(p0, q 0 ) is undefined, because
initially, w 0 is nowhere defined. The invariant for ŝ ∈ W 0 (p̂, q̂) may be invalidated in three
ways.
(1) (p̂, q̂, ŝ) is the item taken from the worklist on line 6. In this case, lines 7–12 ensure that
w 0 (p̂, q̂) = ŝ or w 0 (p̂, q̂) = ∞ at the next loop iteration.
(2) w 0 (p̂, q̂) = ŝ holds and the value of w 0 (p̂, q̂) is updated; this may only happen on line 10,
and the invariant still holds with w0(p̂, q̂) = ∞.
(3) There is a premise s0 ∈ W 0 (p0, q 0 ), and w0 (p0 , q0) is set to s0 or ∞ on line 10 or 12. This
means that the item taken from the worklist on line 6 satisfies p = p0 , q = q 0 , and s = s0
or s = ∞. Note that s = w 0 (p, q) holds at line 13.
If (step0E ) is used to infer ŝ ∈ W 0 (p̂, q̂), let us assume that s0 = s1, p0 = p1 and q 0 = q1 ;
(the case that s0 = s2 , p0 = p2 and q 0 = q2 is analogous). If w 0 (p2, q2 ) is not equal
to s2 nor ∞, then the invariant is maintained. Note that we have p1 ◦ p2 → p̂ ∈ E
and q1 ◦ q2 → q̂ ∈ N , with (p0, q 0 ) ∈ {(p1 , q1 ), (p2, q2 )}. Hence line 18 is reached with
pr = p̂, qr = q̂. At that point, s1 = w0(p1 , q1 ), s2 = w0 (p2 , q2). So either s1 ◦ s2 = ŝ, or
s1 ◦ s2 = ∞ so putting the item (pr , qr , s1 ◦ s2) on the worklist restores the invariant.
If (step0F ) is used to infer ŝ ∈ W 0 (p̂, q̂), then q 0 = q̂ and there must be a rule
p0
 → p̂ ∈ F. Hence line 20 will put (p̂, q̂, s) with s = ŝ or s = ∞ on the worklist,
restoring the invariant.
Theorem 5.15. The procedure in Figures 11 and 13 is correct and takes O(kRk3 log kRk)
time.
Proof. Consider the check of the first UNR-condition (Listing 11). Note that by the first
invariant of Lemma 5.13, the check on line 8 succeeds only if the first UNR-condition is
violated, since at that point s ∈ W (p, q), t ∈ W (p, n(q)) and s 6= t, so s →∗E∪F − p E∪F − ∗← t
and s and t are R◦ -normal forms. When the main loop exits, the worklist is empty, making
the case that (p, q, s) ∈ worklist in the second invariant impossible. Therefore, we can show
by induction on the derivation that for any derivation of s ∈ W (p, q) by the inference rules
in Figure 10, w(p) = s holds, using the second invariant. Consequently, the resulting partial
function w witnesses the fact that the first UNR-condition holds. Therefore, the check of
the first UNR-condition is correct.
Now look at the check of the second UNR-condition (Listing 13). Using the first invariant
of Lemma 5.14, we see that the check on line 13 succeeds only if the second UNR-condition
is violated, since at that point, either s ∈ W 0 (p, q) and s 6= w(p), or |W 0(p, q)| > 1, ensuring
that W 0 (p, q) contains an element distinct from w(p). On the other hand, if we reach
line 21, the worklist is empty, and by induction on the derivation we can show that for all
derivations of s ∈ W 0(p, q) using the rules in Figure 12, either w 0 (p, q) = s or w0 (p, q) = ∞,
using the second invariant. Furthermore, the check on line 13 has failed for all defined
values of w0 (p, q), which means that whenever both w 0 (p, q) and w(p) are defined, then
they are equal; in particular, w 0 (p, q) 6= ∞. Therefore, in (5.3), if w(p) is defined, we must
have t = w0(p, q) = w(p) = s, and the second UNR-condition follows, establishing UNR by
Lemma 5.4.
Next we establish the complexity bound. Let n = kRk. We claim that the check of
the first UNR-condition (Listing 11) takes O(n3 ) time. First note that the precomputation
24
 BERTRAM FELGENHAUER
(line 1) can be performed cubic time. Moreover, the bottom part of the main loop (lines 11–
17) is executed at most O(n) times, once for each possible value of p. So even without
indexing the rules of E, the bottom part takes at most O(n2 log n) time, where the log n
factor stems from the query of N and the maintenance of maximal sharing when constructing
s1 ◦ s2 . Furthermore, only O(n2 ) items are ever added to the worklist, so the top part of the
loop (lines 5–10) also takes O(n2 ) time. Overall, the check of the first UNR-condition is
dominated by the cubic time precomputation.
For the complexity second UNR-condition (Listing 13), note that computation of ↑
takes O(n3) time. We focus on the main loop (lines 4–20). Because w0 (p, q) is updated at
most twice for each combination (p, q), lines 13–20 are executed at most O(n) times for each
possible value of p, for a total of O(n2 ) times. By indexing the rules of E we can perform the
enumeration on line 16 in a total O(n3 ) time, accounting for O(n2) selected rules of E (each
of which is used for at most two values of p), and O(n) possible values for q1 or q2 , depending
on whether (p, q) = (p1 , q1 ) or (p, q) = (p2 , q2 ). By the same analysis, lines 17–18 are also
executed O(n3 ) time in total, for a total runtime of O(n3 log n) (as for the first check, the
log n factor stems from the query of N and the maximal sharing of s1 ◦ s2). Lines 19–20 are
also executed O(n3) times. Overall at most O(n3 ) items are added to worklist, so lines 6–14,
which are executed once per worklist item, take O(n3 ) time. In summary, the complexity is
O(n3 log n) as claimed.


finding normal forms requires knowing if a term is a normal form. As :cite:`lucasNormalFormsNormal2016` points out this is in general complexity :cite:`\Sigma^0_1`, because the conditions are programs. But the unconditional system is a good approximation, and checking the conditions can be done with a time limit for a further improved approximation. The distinction Lucas et al. draw between irreducible terms (no b such that a -> b) and normal forms (irreducible and operationally 1-terminating) seems contrived - it is true that the operationally terminating systems they define are easier to analyze, but as they admit, if "the interpreter implements some reachability checking" you can see some results on non-operationally-terminating systems. Hence their categorization of non-operationally-terminating systems as "useless" and "abnormal" is quite insulting.

For the unconditional system we can construct an automaton,

To each non-variable subterm v of t we associate a state qv . In addition
we have a state q⊤. The only accepting state is qt. The transition rules are:
• f (q⊤, . . . , q⊤) → q⊤ for all function symbols.
• f (qt1 , . . . , qtn ) → qf (t1,...,tn ) if f (t1, . . . , tn) is a subterm of t and qti is
actually q⊤ if ti is a variable.
• f (q⊤ . . . , q⊤, qt, q⊤, . . . , q⊤) → qt for all function symbols f whose arity is
at least 1.
The proof that this automaton indeed recognizes the set of terms that encompass
t is left to the reader.

Per :cite:`vittekCompilerNondeterministicTerm1996` programs using nondeterminism still spend a large part of reduction steps in deterministic evaluations. For example, constraint solvers do a nondeterministic choice followed by lots of deterministic propagation. So efficient implementation of deterministic computations is important regardless, and the efficiency of nondeterminism is less important.

The compiler creates an evaluation function that takes one argument (a term) and returns the resulting normal forms. This works particularly well for convergent systems where the result is a unique normal form, as subsystems are simply more evaluation functions.

In the nondeterministic case several results may be returned and the situation is more complicated. Backtracking works well.

A TRS is strongly descending if there is an ordering > such that t1 > t2 for every reduction t1 → t2 that belongs to a critical pair. Theorem: strongly descending and locally confluent implies confluent.

The proof that SN implies WN goes as follows: Assume term t SN and not WN. Then for every term s in the reduction closure of t, exists f(s) s.t. s -> f(s). Take smallest set containing t, closed under f, and topologically closed. By SN in reduction closure of t.

Then infinite reduction. Then limit exists by SN. Cannot be normal form. So continue reduction.

Proving a single term has SN and CR is Σ^0_1 and Π^0_2 respectively. Proving SN/CR for a system is Π^0_2.
WCR is Σ^0_1. The special halting problem "halts on the blank tape" is Σ^0_1. The general halting problem "halts on all inputs" is Π^0_2.

SN is in Σ01 because we can enumerate all reductions in order of length and if we halt then the term is SN.

for infinitary rewriting, SN is in Π11. A term is a function from positions to term-parts and an infinite reduction is a function from natural numbers to terms satisfying a Π11 predicate that each term rewrites to the next term.

(i) σ(β) rewrites to σ(β + 1) for all β < α, and
(ii) for all limit ordinals β < α, and γ approaching β from below, we have:
– σ(γ) converges to σ(β), and
– the depth of the γ-th rewrite steps tends to infinity.
If condition (ii) holds for all limit ordinals β ≤ α then the rewrite sequence σ is
called strongly convergent. An ordinal α can be viewed as a well-founded relation
α ⊆ N × N. The property of a relation to be well-founded can be expressed by
a  -formula, and the above properties on rewrite sequences are arithmetic.
By [4] the property SN∞
 R (s) holds if and only if all reductions admitted by s are
strongly convergent. Hence SN∞
 R and SNR ∞
 (s) can be expressed by a Π1 1
 -formula
since the above conditions (i) and (ii) are arithmetic.


we have two reductions starting from a term t. By Lemma
46(iii) they can be put into the form t -!f-c_t so and t ->!f-'f s1.
We then construct Fig. 10. The top left square exists by Theorem 57. The top right
and bottom left are given by Lemma 45. The remaining squares follow from Lemma 60

Since October 2005 we have started our three year project sponsored by Dutch NWO in the FOCUS/BRICKS framework with the title Infinite objects, computation, modeling, and reasoning (BRICKS project page). Our informal name for the project is INFINITY. There are three sites involved:

As to our subjects and methods, we focus on term (graph) rewriting (including infinitary lambda calculus, infinitary and higher-order rewriting) for the computational aspect, on coalgebraic techniques for the modeling aspect, and on proof theory for the reasoning aspect. Our aim is to study infinite objects, such as streams, infinite trees and terms, infinite term graphs or infinite processes, along the lines of these methods and develop relations between the three methods. Often these exhibit a regular or cyclic nature, but we are also interested in general infinite objects. A paradigm example is the coalgebra of infinite bitstreams. Several operators can be defined on these objects, such as the operator zip that zips two streams alternatingly into one. Defining ones = 1:ones, zeros = 0:zeros, and alt = 0:1:alt, we have equations such as zip(zeros,ones) = alt. The equations holding in this coalgebra between these objects and their operators can be proved in three different ways: by coalgebraic techniques, by proof theory, and by infinitary term rewriting. Up to now, these three methods have been developed in isolation. This project investigates the relations between these approaches, and aims to design methods such as new proof systems to deal with the infinite objects and their equations. Issues are well-definedness criteria, completeness of the methods, decidability and implementation.

for matchbox, the constraints are really
QF_NIA (polynomial integer arithmetic, standard matrices) and QF_LRA (arctic numbers)
but the Boolean aspect seems to dominate, so
instead I am using an (eager) bit-blasting translation to SAT,
with the Ersatz library https://hackage.haskell.org/package/ersatz-0.4.12
which neatly solves the memoization problem with hidden unsafePerformIO
so I can write the constraints in a functional way
(e.g., unknown numbers, and matrices, are instances of Num,
so `sum` automatically works, etc.)

For actual SMT writing, a nicely typed embedded DSL is
https://hackage.haskell.org/package/smtlib2-1.0/docs/Language-SMTLib2.html
but it seems unmaintained, and I found it hard to add patches
(necessary because of changes in SMTLIB format specs)
I recently looked into https://hackage.haskell.org/package/simple-smt
but it requires some ugly monadic code, cf. evaluate_rule(_arc)
https://gitlab.imn.htwk-leipzig.de/waldmann/pure-matchbox/-/blob/457-find-ar...

in MU-TERM, we generate first-order formulae that are translated into
constraints following this article (interpretations are given for
function symbols and relations):

https://link.springer.com/article/10.1007/s10817-017-9419-3

all this process from the generation of the fist-order formulae to the
application of the Farkas' Lemma is implemented using MU-TERM internal
libraries.

We translate these constraints from our internal format to SMTLIB2
format using the logic QF_NIA and setting the variables into the Int domain.

The result is a ByteString that is sent to a temporary file and an
external call is executed. The result is stored in a new temporary file
that is parsed by MU-TERM.

When we want to use finite rational domains (for example, if we want
only 1, 1/2 and 2 as coefficients), we still use nlsol (in this case,
relations are interpreted in the classical way):

https://www.cs.upc.edu/%7Ealbert/nonlinear.html

LoAT has a custom, minimalistic interface for SMT solvers [1].
Currently, there are two implementations, based on the C(++) APIs of Z3
and Yices (there was a third one for CVC4 in the past, and I plan to
integrate CVC5 in the future).

It uses the logics QF_LIA, QF_NIA, and QF_NIA + exponentials (which is
not yet standardized, as far as I know, but Z3 can at least handle the
trivial cases). Yices is used for QF_LIA and Z3 is used for QF_NIA.

For managing resources, I use Z3's timeout parameter, and Yices is
called in its own thread and stopped via the corresponding API-call
(yices_stop_search) when the timeout expires. For QF_NIA, that does not
work reliably, which is one of the reasons why LoAT uses Z3 instead of
Yices for QF_NIA.

The majority of the SMT calls are needed to check satisfiability of the
guards of transitions (to remove inapplicable transitions which arise
due to LoAT's underapproximations), and to check the preconditions of
LoAT's loop acceleration and non-termination techniques [2,3].

[1] https://github.com/LoAT-developers/LoAT/blob/master/src/smt/smt.hpp
[2] https://link.springer.com/chapter/10.1007/978-3-030-45190-5_4
[3] https://arxiv.org/abs/2111.13952

In AProVE, arithmetic constraints are created programmatically and represented using recursive data structures, without an explicit intermediate language. These data structures are then either exported to SMT-LIB2 strings or bit-blasted to propositional formulas.

For RPO, a dedicated propositional encoding is used:

Michael Codish, Jürgen Giesl, Peter Schneider-Kamp, René Thiemann:
SAT Solving for Termination Proofs with Recursive Path Orders and Dependency Pairs. J. Autom. Reason. 49(1): 53-93 (2012)

http://verify.rwth-aachen.de/giesl/papers/RPO-Journal-distribute.pdf

The representation of propositional formulas allows for arbitrary nesting of connectives. Formula construction uses structural hashing (aka sharing of subformulas), implemented via suitable instances of the Factory pattern. Formulas are converted to CNF via the implementation of Tseitin's equisatisfiable encoding in SAT4J.
...

Beyond propositional logic:

-- QF_NIA is used for arithmetic interpretations that are at least as expressive as linear polynomial interpretations (max-polynomials, matrix interpretations, non-linear polynomials, ...); carriers of these interpretations are the naturals, the integers, and the non-negative rationals.

Solved via bit-blasting with structural hashing/sharing of subformulas and several simplifications during formula construction, using the encoding in

Carsten Fuhs, Jürgen Giesl, Aart Middeldorp, Peter Schneider-Kamp, René Thiemann, Harald Zankl:
SAT Solving for Termination Analysis with Polynomial Interpretations. SAT 2007: 340-354

https://www.dcs.bbk.ac.uk/%7Ecarsten/papers/SAT07-satpolo.pdf

Experiments with Z3 v3.2 in 2011 (for a talk at a Z3 workshop) showed that solving QF_NIA formulas from the search for linear polynomial interpretations with /unbounded/ arithmetic variables was very slow.

However, adding constraints a_i <= N for variables a_i and small values for a constant bound N led to a performance that was only slightly worse than that of AProVE's dedicated SAT encoding for the same bound N. QF_NIA has the benefit over QF_BV that no overflow-related issues can occur, and internally SMT solvers can make use of the bound N.

(Rerunning such experiments with /current/ versions of Z3, Yices, CVC5, ... might be interesting.)

-- QF_LIA is used for linear polynomials with hard-coded coefficient 1, where only the additive weight is needed, and for KBO following the encoding in

Harald Zankl, Nao Hirokawa, Aart Middeldorp:
KBO Orientability. J. Autom. Reason. 43(2): 173-201 (2009)

http://cl-informatik.uibk.ac.at/users/hzankl/new/publications/ZA09_01.pdf

-- Quantifier-free arctic naturals/integers (as pointed out earlier, closely related to QF_LIA/QF_LRA, but with an additional -\infty value) are used in the search for arctic matrix interpretations. AProVE uses a variant of the unary order encoding described in Section 5.3 of

Carsten Fuhs:
SAT encodings: from constraint-based termination analysis to circuit synthesis. RWTH Aachen University, 2012, pp. 1-188

http://sunsite.informatik.rwth-aachen.de/Publications/AIB/2011/2011-17.pdf
...

-- External solvers called via Java API: SAT4J, SMTInterpol.

For quick queries on very small inputs (e.g., checking validity of conditional polynomial constraints from concrete max-polynomial interpretations or checking effectiveness of a semantic labelling), SAT4J is used - the overhead of spawning an external process is avoided.

This matters in particular for JVM-based termination tools: creating a new process first makes a copy of the parent process - with all its used memory - and only then runs the called program, with undesired effects (even if the process copy is actually created lazily by the OS):

https://www.tapatalk.com/groups/starexec/strange-memouts-t106.html

In this sense, for "heavy-duty" constraint solving in a competitive setting, also systems-level considerations matter.

-- External solvers used as processes: MiniSat, Yices, Z3.

MiniSat 2.2 is arguably not the most recent solver (last update in 2008?), so improvements via (suitably configured) more modern SAT solvers may be possible. According to Section 4 of

Emre Yolcu, Scott Aaronson, Marijn J. H. Heule:
An Automated Approach to the Collatz Conjecture. CADE 2021: 468-484

https://link.springer.com/content/pdf/10.1007/978-3-030-79876-5_27.pdf

a suitable configuration for modern solvers may be to change the solver's branching behaviour from phase-saving (default in many recent solvers) to negative branching (default in MiniSat - always try 'false' before 'true').

AProVE kills external processes on OS level if either their timer (configured in the strategy language) has elapsed or separate progress in the termination proof makes the remaining computation by the process unnecessary: for example, when AProVE searches for an RPO and a polynomial interpretation with two running MiniSat instances in parallel, and an RPO that simplifies the termination problem is found, then the search for a polynomial interpretation, along with its MiniSat process, are aborted immediately. Being able to abort computations immediately, also in a parallel setting, is a motivation to use external processes rather than a solver API.

An overview of AProVE's constraint solving automation via SAT and SMT solving with further pointers is available in Section 4 of

Jürgen Giesl, Cornelius Aschermann, Marc Brockschmidt, Fabian Emmes, Florian Frohn, Carsten Fuhs, Jera Hensel, Carsten Otto, Martin Plücker, Peter Schneider-Kamp, Thomas Ströder, Stephanie Swiderski, René Thiemann:
Analyzing Program Termination and Complexity Automatically with AProVE. J. Autom. Reason. 58(1): 3-31 (2017)

https://www.dcs.bbk.ac.uk/%7Ecarsten/papers/JAR-AProVE.pdf

NaTT requires an SMT solver that can read SMT-LIB 2.0 queries via stdin,
e.g. and by default, "z3 -smt2 -in". The communication doesn't go via
filesystem and I don't feel need for making API calls.

The theory is usually QF-LR(I)A. Coefficients are usually (ite b 0 1) or
(ite b 1 2), linearized by distribution. Probably more importantly, NaTT
uses incremental feature (push/pop).

Above are described in a publication:
https://doi.org/10.1007/978-3-319-08918-8_32, and later presented at
WST: https://akihisayamada.github.io/NaTT2016.pptx

It also does a partly lazy encoding, so that x in (x && false) etc. will
not be fully generated.

 improvements via (suitably configured) more modern SAT solvers
may be possible.

Yes. E.g., Kissat has lots of parameters,
and provides a uniform interface (command line and API)
My optimizer computed this
https://gitlab.imn.htwk-leipzig.de/waldmann/pure-matchbox/-/blob/master/strat/std.strat#L7
but the search space is huge, and my optimizer is naive
See also https://github.com/arminbiere/kissat/issues/25
There's room for at least one Bachelor's thesis here.

* AProVE kills external processes on OS level ...

I struggle with this (cf. https://github.com/yav/simple-smt/issues/21)
but this may be a Haskell/libraries problem (lazy IO).
Stopping Kissat via API just works. (I think it sets some flag
that the solver's main loop checks often enough.)

(Akihisa, on NaTT)

* Coefficients are usually (ite b 0 1) or (ite b 1 2),

Interesting! So it's actually a finite-domain constraint.
There are classical (non-SMT) solvers for these,
e.g. https://www.gecode.org/ Did any-one try this?
Possibly via the mini/flatzinc language?
https://www.minizinc.org/doc-2.6.4/en/part_4_reference.html

no, I mean constant parts are left unbounded. Interpretations look like
(ite b 0 1) * x + (ite b 0 1) * y + w
