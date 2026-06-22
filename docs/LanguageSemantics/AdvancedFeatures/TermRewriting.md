# Term rewriting

## Higher-order rewriting

The definition is vaguely based on {cite}`vanoostromConfluenceAbstractHigherorder1994`. But Ooostrom's HORS definition, like Terese, uses closed terms and no separate substitution, avoiding free variables by adding additional binders, e.g. he writes the rule `f x = 1` as `\x. f x -> \x. 1`. I don't like this because it's unclear how it interacts with currying, eta reduction, and conditional rewriting, so I added the substitution back.

There is some question of whether context substitution is capture-avoiding, i.e. does `(\x. □)[□ := x]` not resolve to `(\x. x)`. Terese says it captures. With Oostrom this substitution is forbidden since `x` is not a closed term. In our more liberal definition this resolves to `(\y. x)` by alpha-renaming the variable.

An example substitution calculus is the lambda calculus. The set of preterms is built from nullary symbols (variables, holes, symbols, and constants), applications of two preterms, and abstractions of a variable and a preterm. The substitution calculus rewriting rules are beta reduction and alpha renaming. Eta reduction can be added too and makes the system only weakly orthogonal. {cite}`endrullisHighlightsInfinitaryRewriting2012`

## Currying

Per {cite}`kennawayComparingCurriedUncurried1995` there are three related TRSs:

- The standard uncurried system where a function application `f(t1, ... , tn)` is formed from an n-ary function symbol `f` and n terms t1,...,tn.
- A curried system where a function application is written `f t1 ... tn`, written using left-associative juxtaposition. This can be represented in a standard TRS as `App(App(App(f,t1),t2),tn)` where `f` is a nullary constant and `App` is a fixed binary "application" operator.
- A "partially parameterized" (PP) system containing `App` and, for each n-ary function symbol in the uncurried system, n-1 partial application symbols and "uncurrying" rules `App(f_i(...), x) = f_i+1 (..., x)`. GHC does something similar with a `PAP` constructor, so it is `PAP 1 f_2 a` instead of `f_1(a)`. This allows combining all the partial-application rules into one handler, but forces a uniform representation for partial applications.

The uncurried system embeds in the curried system, i.e. for every uncurried term t there is a unique corresponding curried term t', and every reduct of t corresponds to a reduct of t' and vice-versa. Hence we lose no expressiveness by making the surface syntax curried, and gain some expressiveness because only the curried system can express partially applied terms.

The PP system is an extension of the uncurried system, and is isomorphic to the curried system. The normal forms of PP and curried are in a bijection, and each curried reduction step corresponds to one or more PP steps. The curried system is simpler than the PP system, so we use the curried definition for the language syntax and semantics. But the PP system may be useful for compilation - we can optimize arities and so on. It's easy to break the isomorphism though, e.g. `printf "%i" 1` can be defined in the curried system but not in the PP system because the arity is variable.

## Conditional rewriting

In a general CTRS the reduction rules are of the form `t | P -> s`, where `P` is a predicate. The allowed rewrites of a conditional rule are the unconditional rewrites `t -> s` that satisfy the condition `P`. We can add a form of logic programming by allowing conditions to use variables not in the LHS, e.g. `precedes x z | exists y. precedes x y && precedes y z = true`. This further extends to allowing variables on the RHS not present on the LHS. A further extension allows LHSs that are a single variable but have a condition - some authors disallow LHSs like this. Some syntax like `l | exists x. C = r` and `l = exists x. r` might make this easier to follow.

The definition of a CTRS is complicated by allowing predicates to refer to the rewrite relation `->`. Naively, the rewrite relation would be defined as a fixed point of the rewrite rules. I.e. letting `S` be the system as a function of the rewrite relation `->`, we would define `->` to be a relation `R` such that `R = S(R)`. Terese presents the least fixed point. However, with badly-behaved conditions, no fixed point may exist, so instead we use the "optimal prefixedpoint", the intersection of the maximal prefixedpoints. I.e. we find the sets `Pre = { R : R subseteq S(R) }, PreMax = { R in Pre : forall R' in Pre, R subseteq R' implies R= R' }, R = intersection PreMax`. We warn on all the reductions in `S(R) \ R` that make it not a fixed point.

For example, with a system with the single rule `a -> a | a is a normal form`, `S({}) = {(a,a)}` and `S({a,a}) = {}`. There is no fixed point, so the naive definition doesn't work. The optimal prefixedpoint is `{}` so `R = {}`. But we warn that the reduction `a -> a` is not included. As another example, take `a -> b if not(c -> d); c -> d if not(a -> b)`. The maximal (pre)fixedpoints are `{(a,b)}` and `{(c,d)}`, but their intersection is `{}` so the optimal prefixedpoint is `R = {}`. We warn that `a -> b` and `c -> d` are not included (extra diagnostics could show they are mutually exclusive). Lastly, `a -> b if a -> b` allows the reduction `a -> b` as it is the maximal (pre)fixedpoint.

The optimal prefixedpoint is correct in that reductions must satisfy conditions, conservative in that systems which have multiple differing interpretations do not reduce, but also a bit generous since it is maximal instead of least, and robust because independent rules are processed independently.

As far as terminology, the literature uses "term rewriting" to refer to unconditional term rewriting and "conditional term rewriting" otherwise. But many popular programming languages such as Haskell have conditional dispatch (guards, patterns, etc.), so we instead use "term rewriting" to refer to conditional and unconditional systems, and we refer to "unconditional TRSs" when necessary.

## Cycles

The untyped lambda calculus has cycles, e.g. `Omega = let w=\x.x x in w w` reduces to itself and {cite}`venturinizilliReductionGraphsLambda1984` shows a 6-cycle `M M I`. Similarly commutativity `a + b = b + a` generates cycles. Maude in fact has associative and commutative operators, declared with special syntax like `op _;_ [assoc]`. But this is a bit specific; likely there are other equations we would want to have, like the Jacobi identity $x\times (y\times z)\ =\ (x\times y)\times z\ +\ y\times (x\times z)$ and so on. In general, we have a list of rewrite rules and it is not necessarily known a priori whether these may result in cycles, but we want to resolve any cycles that come up in a natural manner.

In a lot of cases, the cycle may be due to reversible equations `pat1 = pat2; pat2 = pat1`.:cite:`dershowitzRewriteSystems1991`'s notion of a congruence-class rewriting system is formulated along these lines - the rewrite rules are split into rules R and reversible equations S, and we consider the system R/S (R mod S). A term where only S rules apply (no R rules apply) is considered a normal form. But this "rewriting modulo equations" formalism is not sufficient - we see with Omega that even beta reduction, a single rule, and one not obviously intended to be cyclic, can result in cycles.

So instead we consider the [condensation](https://en.wikipedia.org/wiki/Strongly_connected_component#Definitions) of the rewrite graph, condensing each SCC to a single term. This condensation step is similar to the quotient R/S but acts individually on each term rather than on the system. A term is a "condensed normal form" if it has no reduction out of its SCC. Hence `Omega`, `M M I` and `a + b` would be condensed normal forms since their SCC contains themselves and they have no further reductions. We could further specify the normal form to be a canonical representative of the SCC, e.g. taking the smallest and lexicographically first element of the SCC, but leaving input unchanged and returning the first element of the SCC that is encountered seems better for debugging purposes.

Orthogonal higher-order TRSs that are weakly head normalizing are acyclic, per {cite}`ketemaViciousCirclesRewriting2005`, so the cycle condensation doesn't affect standard functional programming - condensing acyclic rewriting systems gives back the same system. Thus cycle detection shouldn't have much of an impact on performance.

## Nondeterminism

A reduction sequence is not necessarily unique, e.g. in reducing `3 * (2 * fact 1)` to `6` we could compute `fact 1 = 1` or we could first use an associative law `3 * (2 * fact 1) = (3 * 2) * fact 1 = 6 * (fact 1)`. Different reduction sequences can be more efficient in terms of memory usage; the compiler should use heuristics and hints to choose the best strategy.

For maximum expressiveness, we also want to allow local nondeterminism. Even if a term has two or more applicable reduction rules and reduces to two normal forms, the context might give the same behavior on the different values. E.g. this should be allowed:

```
a = b
a = c
# a has two normal forms, nondeterministic

f b = d
f c = d

print (f a) # deterministically prints d
```

However, top-level method dispatch nondeterminism is unresolvable. E.g. `print a` with this example is an error - there is no way to reconcile `print b` and `print c`, because the user can only see one output.

Exceptions complicate the semantics. The literature speaks of "normalizing" strategies that will eventually find a normal form if one exists, but otherwise are allowed to loop forever. In Stroscot non-termination is an exception, so the corresponding property is that if there is any reduction sequence that produces a non-exception value, Stroscot evaluates to that non-exception value, rather than an exception. So `1 amb (throw b)` should reduce to 1. This provides the benefits of lazy evaluation.

The alternative "strict" evaluation strategy would be what the literature calls a "perpetual" strategy - if any strategy diverges, then a perpetual strategy diverges. With a perpetual strategy inlining etc. hold only if reduction of the expression terminates, i.e. one must keep track of termination properties. A perpetual strategy gives the wrong behavior for if-then-else and short-circuit functions, so strict languages special-case these to ensure they don't cause nontermination. Perpetual strategies are antagonistic, "I'll crash your program if I can". The evaluation strategies article discusses strict vs lazy more - overall lazy seems better.

Also, exception propagation is nondeterministic. For example `e = throw b + throw c` will throw either `b` or `c` depending on which is evaluated first, and the choice is observable in a program with `e catch print`. Exception nondeterminism is a different category from method dispatch nondeterminism and generally seems benign. So the compiler will not output a diagnostic and will resolve the `catch` using the exception that is most efficient to dispatch. But you can enable an error or warning that ensures caught exceptions are unique. Regardless, the verification system will verify properties for all choices of exception, i.e. `(case e of Exc b -> 1; Exc c -> "a") : Int` will fail but `(case (throw b) of Exc b -> 1; Exc c -> "a") : Int` will not because `c` is unreachable.

## Infinite reduction

Infinite reduction is useful because it is "smoother" than finite reduction - normal forms exist more often. For example `x = 1 :: x` reduces to `x = 1 :: 1 :: 1 :: ...`, `fib = 1 :: 2 :: zipWith (+) fib (head fib)` reduces to `fib = 1 :: 2 :: 3 :: ...`, and `foo = let t = \x. x x x in t t` reduces to `foo = ... t t t t`. With finite reduction we would have to use head normal forms and partially evaluated terms. With infinite reduction all of these terms have a proper denotation. Also I/O can be modeled as an infinite value with sub-terms for each outcome of the I/O operation.

The idea is to extend our set of terms to include infinite terms, defined as the [metric completion](https://en.wikipedia.org/wiki/Complete_metric_space#Completion) of finite terms with a distance function $2^{-n}$ if the n-th level of the terms is the first level where a difference appears and 0 if the terms are equal. By convention the top level is level zero. This definition is equivalent to a co-inductive definition of terms, i.e. the largest set consisting of term-parts whose subterms are co-inductive terms. The distance function is actually an [ultrametric](https://en.wikipedia.org/wiki/Ultrametric_space).

This set, like the real numbers, is uncountably large and includes terms with no finite description. Actually we should be able to map the set of finite and infinite terms to the real interval \[0,1\]: do a breadth-first traversal of the term tree and encode the sequence of symbols using arithmetic encoding. Properly defined, the mapping should be a computable, continuous bijection that imposes a total order on terms. TODO: verify. There is then a canonical representation of each real number, such as [this continued fraction representation](https://oscarcunningham.com/494/a-better-representation-for-real-numbers/), so also a canonical representation of each infinite term. Although, again like the reals, it is computationally more tractable to use an unevaluated-expression-tree symbolic representation and special-case interesting classes of terms, than it is to use a generic representation as an infinite sequence.

There are various extensions of the transitive closure to infinitary reduction, so the question arises as to which one to use. {cite}`kahrsInfinitaryRewritingClosure2013` discusses several and provides an ordering so that each is mostly a proper subset of the next (not sure about P\* subset bi-infinite). Many of these use the monotonic closure operator \*. Specifically X\* is the least fixedpoint of the function G defined as G(R) = X(R) union R, which by the (transfinite) Kleene fixed-point theorem exists and is the limit/union of the sequence $X^0 = \emptyset, X^{n+1} = G(X^n), X^\delta = \bigcup_{\alpha < \delta} X^\alpha$.

- S\*, the monotonic closure of strongly converging reduction sequences, "strong" being a requirement that the depth of the redexes contracted in successive steps tends to infinity. S=S\* for "compatible" TRSs, ones where t R u imply C[t] R C[u] for any context C, which all iTRSs satisfy.
- W\*=A=A\*, the monotonic closure of weakly converging reduction sequences, and also the [adherent points](https://en.wikipedia.org/wiki/Adherent_point) of reduction sequences in the metric space. Weak convergence by itself is not transitively closed, e.g. `a = b; f x a = f (g x) a` has `f c a -ω> f (g (g (g ...))) a -> f (g (g (g ...))) b` {cite}`dershowitzRewriteRewriteRewrite1991` {cite}`simonsenWeakConvergenceUniform2010`, hence the need for closure. By definition of adherent point, each w-reduct is either an accumulation point, i.e. a appears arbitrarily close infinitely often in a reduction sequence, or an isolated point which can be reached in a finite number of reductions.
- P\*: the monotonic closure of the pointwise closure of the reflexive transitive closure (finite multi-step relation).
- bi-infinite rewriting, defined in {cite}`endrullisCoinductiveFoundationsInfinitary2018` Definition 6.3 as the greatest relation R such that R = the reflexive transitive closure of (single-step rewriting union (R lifted to apply to subterms)). Per the remark under Corollary 7.5 it is a proper subset of T\*.
- T\*: the monotonic closure of T, the topological closure of the reflexive transitive closure. T itself is not transitively closed, e.g. `leq 0 x = true; leq (s x) (s y) = leq x y; inf = s inf` has `leq inf inf T leq (mu x. s x) (mu y. s y) T true` (by topological closure of finite approximations of the S towers) but not `leq inf inf T true` (because the terms are of finite depth). Alternatively I have defined T\* as the smallest relation M such that M is reflexively, transitively, and topologically closed and contains the single-step relation, which I think is equivalent.

S\* is the standard in the literature but doesn't have much going for it besides that. If there is a reduction that switches heads, `a X = b (c X); b X = a (c X)`, then S\* says there are no w-reductions. W\* has `a e -w> a (mu x. c x)` and `a e -w> b (mu x. c x)`. TRSs are in general nondeterministic, so the "strongly converging" definition that requires a single limit to exist is too strong.

Hypercollapsing terms like `mu x. C x` with rule `C x = x` (R1) or `C A = A` (R2) are a good question. They are sort of like trying to evaluate $f(x)=\sin(x)$ at $x=\infty$ - the rule amplifies small variations in the term, but because the term is infinite the variations do not shrink. With T\*, like how the toplogical closure of $f(x)$ at $\infty$ is the interval $[-1,1]$, the topological closure of the hypercollapsing term is the accumulation point of approximating reductions `C^n f = f` and so is every term (under R1) or `A` (under R2). Similarly with bi-infinite rewriting for `R={(fix C,A)}` we have `fix C -lifted R> C A -R2> A` so again it is every term or `A`. More generally, if we have a predicate ``` P`, then ``mu x. C x ``` with `C x | P x = x` reduces to all terms where `P` holds under bi-infinite / T\*. In contrast, with W\* and P\* the hypercollapsing terms only reduces to themselves hence are condensed normal forms. This behavior is what led to defining meaningless terms, so that hypercollapsing terms do reduce. I think the amplifying behavior of `T*` / bi-infinite is the most useful - most likely it will give an error due to ambiguity, but there are cases where it can produce usable values. In contrast W\* / P\* sort of give up on the reduction of hypercollapsing terms and do not get involved with the structure of the reduction.

Also conditional rewriting can interact with infinite reduction and cause unwanted behavior with a weak closure. For example consider the system `ds x y | x == y = e` and reducing the infinite term `G = ds G G` (in {cite}`klopCombinatoryReductionSystems1980` this is achieved by the system `G = a = c a; c x = ds x (c x)`). Since `e` is a normal form hence equal to itself, all finite terms defined by `T = { x : x == e or x in ds T T }` reduce to `e`. So using a bi-infinite closure, `G` uniquely reduces to `e`. But with a weak closure `X = ds e X` is a normal form and the system becomes nondeterministic. Similarly with `dk x y | x == y = e x` and `G = dk G G`, we should get `e (e (e ...))` as the unique result, but with a weak closure we don't. Another tricky system is `c x | x == c x = e; b = c b` - the obvious reduction is `b = mu x. c x = e`, but this system has a hidden circularity of the form `mu x. c x = e` if `mu x. c x = e`. So again based on this we would like a bi-infinite or T\* closure.

Overall, from these examples, it seems clear that allowing a reduction is better than forbidding it. Cycle condensation means that we would like to equate as many terms as possible to get large SCCs, and similarly a large reduction relation means there will be an escape from infinite regresses. Bi-infinite and T\* seem equally simple to formalize since they are both single fixed points, so T\* wins because it's larger.

Actually there is a larger relation. We can define a family of relation $\overset{*}{\to}_\epsilon$ which contains the single-step relation, is transitively closed, and in addition to containing the reflexive relation, contains all pairs $(x,x')$ with $d(x,x')<\epsilon$. We see that each $\overset{*}{\to}_\epsilon$ is topologically closed since if we have a sequence $(x_n, y_n) \to (x, y)$ then we can just go far enough to have $d(x,x_n)<\epsilon$ and $d(y,y_n)<\epsilon$ and then we conclude $x \overset{*}{\to}_\epsilon y$ from $x_n \overset{*}{\to}_\epsilon y_n$.

Since (due to our distance metric) all distances are of the form $2^{-n}$, we can limit our considerations to the sequence of relations $R_n = \overset{*}{\to}_{2^{-n}}$. $R_n$ is monotonically decreasing in the sense that $R_n \supseteq R_{n'}$ for $n<n'$, so there is a pointwise convergence property: either $(x,y) \in R_n$ for all $n$, or there is an $n$ such that $(x,y) \notin R_{n'}$ for $n' > n$. Therefore, we can define $E^*(\to) = \lim_{\epsilon\to 0} \overset{*}{\to}_\epsilon = \lim_{n\to \infty} R_n = \bigcap_n R_n$ (the limit here being something like the [Kuratowski limit](https://en.wikipedia.org/wiki/Kuratowski_convergence)).

Does E\* = T\*? Well, $T^*(\to) \subseteq R_n$, because they are closed under the properties of T\* and also the epsilon-distance. Therefore, $T^*(\to) \subseteq E^*(\to)$. What about $E^*(\to) \subseteq T^*(\to)$? Well... it is tricky. For each $n$, we have a rewrite sequence of alternating epsilons and single-steps, connected by transitivity. We don't need to worry about more than one epsilon between each single-step, as our metric is an ultrametric. Similarly we can assume an epsilon around each single-step because it's simply distance zero. So for each $\epsilon$ we have a (finite) rewrite sequence like epsilon-step-epsilon-step-epsilon. But it is not clear that we can take these disconnected segments and get a relation in T\* using topological closure and transitivity. Like say we have binary numbers `0 1 0 1` and the single-step rewrite relation takes a term like `0 1 0 1 +` to `0 1 1 0`, incrementing by 1. With E\*, starting at `0 0 0 0 ...`, we can go within epsilon to a finite term ending in `+`, and single-step plus epsilon up to `1 1 1 ... 1`, so there is the rewrite `0 0 0 ... -> 1 1 1 ...`. But with T\* it seems `0 0 0 0 ...` does not rewrite to anything besides itself, because the only rewrite relations within epsilon of `0 0 ...` are `0 ... 0 + = 0 ... 1` and the RHS also converges to `0 0 ...`. It would be nice if I have misinterpreted something and they are the same but for now I am going with E\* as the definition.

As more justification for lax rewrite relations, there is an example in {cite}`endrullisInfinitaryTermRewriting2014` of the system `P (S x) = x; S (P x) = x` and the term `t = P SS PPP SSSS PPPPP SSSSSS ...`. Under E\*, we have `fix S =_e fix P =_e t`, because we can always replace the term at some (finite) depth and cancel out the front terms. In fact all infinite sequences of S's and P's are in the same SCC in E\*. Now here for weak closure operators there is a discontinuity and `fix S` and `fix P` become normal forms with `t` reducing to each. For T\* the behavior is more like E\*, we get that `fix S` reduces to `fix P` as it is the limit of the reductions `S (fix P) -> fix P`, `S (S (fix P)) -> fix P`, etc., so it again equates infinities. Now you could say that having `fix S` and `fix P` is useful, it is sort of like positive/negative infinity, but the issue is that it is unstable. If I were defining a system like this I would have an outer function like `freeze (S x | x != P) = s (freeze x)` with no reduction for `s (p x)` defined, set up so that `freeze` produces a term of all s's or all p's. Because no reductions are defined this "frozen" term is stable. Since freeze has to inspect the whole term, we see that freezing pretty much any infinite term is going to be problematic - it really could be the case that there is a long string of S's but then it's all P's and the term is actually negative infinity, or that it alternates in the triangle pattern and there is no right answer. In some sense we would have to know "too much" about the term to say that it reduces to positive/negative infinity and not the other.

We also have that $R_n(x)$ converges pointwise to $E^*(x)$ using the Hausdorff distance because, considering $\sup_{b\in R_n(x)}d(E^*(x),b)$, there is an $n$ that is sufficiently large that no points more than $\epsilon$ away from $\overset{*}{\to}_0(x)$ disappear. Specifically, for each point $b \notin E^*(x)$ there is an $m$ such that for $m' > m$, $b \notin R_{m'}(x)$. Now suppose for contradiction that no such $n$ exists, i.e. for every $n$ there is a point $b_n \in R_n(x)$ with $d(b_n,E^*(x))>\epsilon$. Then since the set of terms is compact we have that $b_n$ has an accumulation point, say $b$. But since $b \in R_n(x)$ we must have $b \in E^*(x)$ so then $d(b_n,E^*(x))<d(b_n,b)+d(b,E^*(x)) < \epsilon$. Therefore, such an $n$ exists such that $d(R_n(x),E^*(x))<\epsilon$.

There is also the notion of the length of a rewrite sequence, for example as follows:

- The length of a reflexive rewrite `a -> a` or epsilon rewrite $d(x,x') < \epsilon$ is 1
- The length of a single-step rewrite is 1
- The length of a transitive rewrite `a -> b -> c` is the length of `a -> b` plus the length of `b -> c`
- The length of a topological rewrite `a -> b` where there is a sequence of rewrites $a_i \to b_i$ and $\lim a_i = a$ and $\lim b_i = b$ is the supremum of the lengths of $a_i \to b_i$, plus 1.

As the topological rewrites can get infinite, but the transitive rewrites must strictly increase, we need ordinals to measure the length. I am not sure exactly how large the supremum-of-sequence-of-ordinals construction can get but at least it is still bounded by some large ordinal. Of course we want commutativity for addition, so we want surreal addition not the asymmetric standard ordinal addition.

For proving things like confluence, we need a strong property similar to uniform convergence. Dini's theorem seems relevant. But first we need to define what it means for $\overset{*}{\to}$ to be "continuous". There was the suggestion in [MathOverflow](https://mathoverflow.net/questions/179123/continuous-relations) to consider whether the function $R(x) = \{ y : x \overset{*}{\to} y\}$ is continuous. As far as the distance metric in $P(S)$, ChatGPT suggested to use the [Hausdorff distance](https://en.wikipedia.org/wiki/Hausdorff_distance). Unfortunately this doesn't work: we may have the rewrite rules `A B = B; A C = C` and then `x = A (A (A (...)))` and `x_n = A (A (A (... (B))))` (n initial A's). We have `R(x) = {B,C,A B,A C,A (A B),...}` and `R(x_n) = {B,A B,A (A B), ..., A (... (A B))}`. Clearly `C` is distance 1 from every element of `R(x_n)`, so the Hausdorff distance of `R(x_n)` and `R(x)` is 1 even though the distance between `x` and `x_n` goes to zero, so `R` isn't continuous under this definition.

The next suggestion is that a relation is continuous if it preserves limits. This is analogous to how one can define continuity for functions between topological spaces using nets; one requires that for any net $(x_a)_{a\in A}$ with $\lim x_a = c$, the corresponding net $\left(y_a = f(x_a)\right)_{a\in A}$ has $\lim_{a\in A} y_a = f(c)$. But this definition for functions is not directly applicable to relations because a relation may have multiple values and multiple limits. Therefore, the author's suggestion is to consider accumulation points: one requires that for any net $(x_a)_{a\in A}$ with an accumulation point $x$, and a corresponding net $\left(y_a \in R(x_a)\right)_{a\in A}$, then there exists a point $y \in R(x)$ that is an accumulation point of $y_a$. The author discusses a bit about how restricting to ultranets is an equivalent definition and allows using limits instead of accumulation points, but constructing non-constant ultranets requires the axiom of choice, so I will avoid that. Now this definition is a bit too general for our purposes. Since we are dealing with a metric space, and it is first-countable, every net corresponds to a sequence (a net with countable basis). So we can assume $A=\mathbb{N}$. So what is required to prove is that for any sequence $(x_n)$ with an accumulation point $x$, and a corresponding sequence $\left(y_n \in E^*(\to)(x_n)\right)$, then there exists a point $y \in E^*(\to)(x)$ that is an accumulation point of $y_n$.

> That is, suppose $\lim x_n \to x$ (where $\lim a_n \to b$ means $b$ is an accumulation point of the set $a_n$) and there exists a sequence $y_n$, with $y_n \in R(x_n)$. Then "preserving the limit" means there is a point $y$ such that $\lim y_n \to y$ and $y \in R(x)$ . For E\* this is true because the space of terms is compact, so there is always a convergent subsequence of $y_n$

A function is continuous as a relation iff it is continuous in the usual sense and a composition of continuous relations is continuous. A partial function that is continuous on its domain is continuous as a relation iff its domain is closed. However, this definition is not symmetric in X and Y (as Joonas Ilmavirta observed, this is a necessary consequence of agreeing with the usual definition on functions). It also does not coincide with subobjects of X×Y in the category of topological spaces (which include not only all subspaces of X×Y but also all subsets equipped with any finer topology).

However, if we restrict to compact Hausdorff spaces, the disadvantages disappear. Limits of universal nets or ultrafilters are well-defined single-valued operations on compact Hausdorff space, so there is a clear choice for what it means for a relation to be "homomorphic with respect to limits". A relation between compact Hausdorff spaces is continuous iff it is closed as a subset of X×Y, and thus continuity is symmetric in X and Y. In addition, these continuous relations are also exactly those subsets of X×Y that are themselves compact Hausdorff spaces, just as in the case of homomorphic relations between algebraic structures.

As a final note, there is a simultaneous generalization of the algebraic case and compact Hausdorff spaces, which is algebras over a monad (compact Hausdorff spaces are the same as algebras over the monad that takes a set to the set of ultrafilters on it, with the structure map of an algebra telling you how to take limits of ultrafilters). Let T:Set→Set be a monad and let A and B be sets. Given a relation R⊆A×B, we can consider the two projections A←R→B and apply T to get a diagram TA←TR→TB. Let ˜TR be the image of TR in the product TA×TB. In this way, T naturally extends to a functor ˜T:Rel→Rel.

We can now define a "homomorphic relation" between T-algebras. Let A and B be T-algebras with structure maps μA:TA→A and μB:TB→B. We say a relation R⊆A×B is homomorphic if for any x∈TA, if y is a value of ˜TR(x), then μB(y) is a value of R(μA(x)). But this is just saying that μA×μB:TA×TB→A×B restricts to a map ˜TR→R, and this restriction will then make R itself a T-algebra via the composition TR→˜TR→R and a subalgebra of A×B. Conversely, if R is a subalgebra of A×B, then the structure map TR→R must factor through ˜TR as a restriction of μA×μB. Thus homomorphic relations between algebras over a monad always coincide with subalgebras of the product.

We could also just consider the Hausdorff distance on $S \times S$ but the function-to-power-set construction is more natural IMO.

So going back to Dini's theorem: the set S of terms is a compact metric space. Our relations $R_n$ are also monotonically decreasing in that $R_n(x) \supseteq R_{n+1}(x)$. Each $R_n$ is continuous in that for any $\epsilon > 0$, we can choose $\delta = 2^{-n}$ and then if $d(x,x')<\delta$ then $d(R_n(x),R_n(x'))=0$.

Finally we must show that $R$ is continuous. Take $x$ and $\epsilon > 0$, and suppose for contradiction that for every $\delta > 0$ there is an $x'$ with $d(x,x')<\delta$ and $d(R(x),R(x'))\geq\epsilon$. Then either there is a point $y \in R(x)$ with $d(y,R(x')) > \epsilon/2$ or a point $y' \in R(x')$ with $d(R(x),y') > \epsilon/2$. Now consider a sequence $x_n$ of such $x$'s, for $\delta = 2^{-n}$. We must either have infinitely many $y_n \in R(x)$ or infinitely many $y_n \in R(x_n)$. Suppose for the moment there are infinitely many $y_n \in R(x)$ and the subsequence of such $n$. Then as $R(x)$ is closed we must have $y = \lim y_n \in R(x)$. So we have $x \overset{*}\to y$, $\lim x_n = x$, but $d(y,R(x_n)) > \epsilon/4$. Observe that $x$ must be an infinite term because if $x$ is finite there are no other points within the distance bounded by the depth of the term. So for example,

> but no `x_n` rewrites to anything starting with `C`. So then we conclude that $R$ is not continuous

Now considering the alternative, infinitely many $y_n' \in R(x_n')$,

Consider or $y_n \in R(x')$ and $d(R(x),y_n) > \epsilon/2$

> Take the subsequence of such $(x'_n,y_n$

is the greater control implied by the monotonicity. The limit function must be continuous, since a uniform limit of continuous functions is necessarily continuous. The continuity of the limit function cannot be inferred from the other hypothesis (consider x n x^\{n} in [ 0 , 1 ] [0,1].)
Proof
Let ε > 0 varepsilon >0 be given. For each n ∈ N nin mathbb \{N} , let g n = f − f n {displaystyle g\_\{n}=f-f\_\{n}}, and let E n E\_\{n} be the set of those x ∈ X xin X such that g n ( x ) < ε {displaystyle g\_\{n}(x)\<varepsilon }. Each g n g\_\{n} is continuous, and so each E n E\_\{n} is open (because each E n E\_\{n} is the preimage of the open set ( − ∞ , ε ) {displaystyle (-infty ,varepsilon )} under g n g\_\{n}, a continuous function). Since ( f n ) n ∈ N {displaystyle (f\_\{n})\_{nin mathbb \{N} }} is monotonically increasing, ( g n ) n ∈ N {displaystyle (g\_\{n})\_{nin mathbb \{N} }} is monotonically decreasing, it follows that the sequence E n E\_\{n} is ascending (i.e. E n ⊂ E n + 1 {displaystyle E\_\{n}subset E\_\{n+1}} for all n ∈ N nin mathbb \{N} ). Since ( f n ) n ∈ N {displaystyle (f\_\{n})\_{nin mathbb \{N} }} converges pointwise to f f, it follows that the collection ( E n ) n ∈ N {displaystyle (E\_\{n})\_{nin mathbb \{N} }} is an open cover of X X. By compactness, there is a finite subcover, and since E n E\_\{n} are ascending the largest of these is a cover too. Thus we obtain that there is some positive integer N N such that E N = X {displaystyle E\_\{N}=X}. That is, if n > N n>N and x x is a point in X X, then | f ( x ) − f n ( x ) | < ε {displaystyle f(x)-f\_\{n}(x)|\<varepsilon }, as desired.

and that $f : S -> P(S), f(x) = \{y : x \overset{*}{\to} y\}$ is a continuous mapping. It seems intuitively true that these hold for finite rule systems, as they can only match on a finite prefix of the term, but with conditionals the conditions may not be continuous.

> P SS PPP SSSS PPPPP SSSSSS

## Meaningless terms

If a term never reaches a normal form, then there's not much semantic meaning in it. We could compute equivalence classes of these terms but it is easier to define them all away. {cite}`kennawayMeaninglessTermsRewriting1999` defines criteria for a set of meaningless terms:

- Contains all root-active terms. A term t is root-active if every reduct of t can be reduced to a term with a top-level redex.
- Closure under reduction. If `M ∈ U`, `M → N` then `N ∈ U`.
- Closure under substitution. For all `M ∈ U`, `M /. σ ∈ U`
- Overlap. If a redex t overlaps a subterm, and this subterm is in U, then t in U. More specifically, if M nontrivially matches a subterm of the LHS of some rule, i.e. for some position `u` and substitution `σ`, `M = subterm (l /. σ) u` and `subterm l u` is not a variable, then the overall LHS is in U, `l /. σ ∈ U`. Specifically for the lambda calculus, if `(\x.M) ∈ U` then `(\x.M) N ∈ U`. Another way of looking at it is that we want to ensure adding rules `t = Meaningless` preserves confluence.
- Indiscernibility - the meaningfullness of a term does not depend on its meaningless subterms. For all M, N, if N can be obtained from M by replacing a set of pairwise disjoint subterms in U with other terms of U, then M ∈ U if and only if N ∈ U.

{cite}`severiDecomposingLatticeMeaningless2011` adds closure under expansion: if `N ∈ U`, `M → N` then `M ∈ U`. This makes the set easier to reason about, but we want `t = 1 amb Meaningless` to evaluate to 1, so `t` can't be meaningless itself, hence we don't want this property.

We do add topological closure as a property of the mute terms, to preserve the property that the infinitary rewriting relation is closed. Essentially we are constructing a relation `M = { (u,Meaningless) : u in U }`; it is easy to to see from our metric definition that this is closed iff U is closed. Then our new relation is `R' = R union M` which is closed because the union of two closed sets is closed.

There are various sets of meaningless terms, going roughly in decreasing size as follows:

- not head normalizing - head active or infinite left spine form or infinite abstraction
- head active or infinite left spine form x1 ... xn -> (...P2)P1.
- head active or infinite abstraction x1 -> x2 -> ...
- head active - x1 ... xn -> R P1 ... Pk where R is root-active
- not weak head normalizing - strong active or strong infinite left spine form (...P2) P1
- strong active - R P1 ... Pk where R is root-active
- mute / root-active = not top normalizing

Root-active or the set of "mute" terms is the smallest set (included by definition), and seems fine. It satisfies all the other properties, meaning we just have to check root-activeness.

A meaningless term set forms an easy set, {cite}`bucciarelliGraphEasySets2016` meaning we can safely equate all meaningless terms to an exception term without changing the semantics of normal terms. In particular we can equate them to a `Meaningless` exception.

With these reductions every term has a normal form. Proof {cite}`kennawayInfinitaryLambdaCalculus1997`: A term t is either meaningless or not (ignoring reductions to `Meaningless`). If it is meaningless, it reduces to the normal form `Meaningless`. If it is not, then it can be reduced to a root-stable term `s`. Repeating the construction recursively on the subterms of s at depth 1 constructs a reduction of t to a term which is stable at every depth, i.e. a normal form.

It is a bit tricky to come up with an example of a meaningless term, as the cycle condensation and infinitary rewriting make a lot of examples meaningful. For example, {cite}`klopInfinitaryNormalization2005` gives `A(1)` with the reduction rule `A(x) = A(B(x)`. Without infinitary rewriting, the limit would not be in the reduction closure, therefore the reduction closure would consist only of partial reducts, each having a top-level redex, and `A(1)` would be root-active and meaningless. Similarly, without cycle condensation, there would be the reduction `A(B(B(...))) -> A(B(B(...)))`, so the limit would be root-active and meaningless. But in our semantics, the limit `A(B(B(...)))` exists, and the cycle is condensed, therefore it is a normal form and `A(1)` is not meaningless. Similarly in {cite}`kennawayMeaninglessTermsRewriting1999` there are some examples:

- `Last(Ones)` with the rules `Last(Cons(x,y)) = Last(y)` and `Ones=Cons(1,Ones)`. This rewrites to `Last([1,1,...])` which then rewrites to every term following the reduction `Last([1,1,...,x])=x`. So it is ambiguous, but not meaningless.
- `A` with `A = B A; B x = x`. Again this rewrites to `B (B (...))` which rewrites to every term and is not meaningless.
- `fix identity` (`Y I`), where `fix f = f (fix f); identity x = x` - This rewrites to terms of the form `I I ... (fix I)`, which again rewrites to every term.

To be meaningless in our system, a term cannot be root-stable, and to avoid cycle condensation, it must cycle through an infinite non-repeating set of terms. Also, we assume compactness of the term space, so there is a finite set of term symbols. So what is an example of a meaningless term? Maybe something like counting in binary with the least significant digit first, `0  -> 1 -> 0 1 -> 1 1 -> ...`. Even when we get to an infinite digit sequence, there still are no repeats and it is not root-stable. Whatever weird topological closure we use does not result in a cycle either.

Every TRS with unique normal forms (UN=) can be extended to a confluent TRS with the same set of normal forms by adding bottom terms and reductions to normal forms and bottoms that preserve the equivalence classes of terms. {cite}`middeldorpModularAspectsProperties1989` Meaningless terms don't accomplish this extension because a term `1 amb meaningless` can reduce to `Meaningless` instead of `1` hence breaking even UNR.

(trs-equality-linearity)=

## Left-nonlinearity

There are several notions of equality that could be used for non-linear patterns, here presented in the order of decreasing strength (earlier implies later):

- strict equality `a == b` - true if both sides reduce to same unique normal form, false if reduce to different unique normal forms, indeterminate if could reduce to same or different normal forms.
- syntactic equality `syn_eq a b` matches terms (reduced or unreduced) that are syntactically identical. It can match even if the term doesn't have a normal form. It is the notion commonly used for non-left-linear TRSs in the literature.
- oriented equality `a ->* b` holds if `a` reduces to `b`.
- join equality `a ↓ b` means that a common reduct exists, i.e. there is a term `c` such that `a -> c` and `b -> c`.
- semi-equational equality `a ≈ b` means that `a` can be rewritten to `b` via rewrites and inverse rewrites.

Computing any of these equalities is of complexity $\Sigma^0_1$ - at least $\Sigma^0_1$ because it is a nontrivial property of the reduction relation, but at most $\Sigma^0_1$ because for equal terms there is a finite rewrite sequence as proof. If reduction is convergent, then for strict equality this reduction sequence can be computed straightforwardly by reducing to normal form, whereas the others involve a brute force search.

Semi-equational equality has "spooky action at a distance" when non-deterministic terms are involved. Consider the system `a = b; a = c; f x y | x != y = d` and the terms `f {a,b,c} {a,b,c}`.

- For semi-equational equality, `f a a` reduces to the 4 combinations `f {b,c} {b,c}`, but the "spooky" equality `b ≈ c` holds, so the `f` rule does not apply. Hence these 4 combinations are the normal forms.
- For strict, syntactic, oriented, and join equalities, `b != c` so the two heterogeneous combinations `f b c` and `f c b` reduce to `d`. The `f` rule does not apply to `f a a`, `f b b`, or `f c c`.

To ensure convergence we have to have stable conditions, meaning if the terms involved are reduced then they are still equal (Terese 4.11.1, page 145 / PDF page 165). For example consider the system `a = b; f x | x == a = c` and the term `f a`.

- For strict, join, and semi-equational equality, we have that `(a == a) = (a == b) = (b == b) = true` so `f a = f b = c` and also `f a = c` directly.
- For syntactic and oriented equality, we do not have `b == a`, so `f a` reduces to both `f b` and `c` and the system is nondeterministic.

Terese's example 4.11.5 that join equality is not confluent does not work because with the optimal prefixedpoint we have `c e = e`. Still, join equality is unstable in a non-confluent system. For example `f x | x == b = x; a = b; a = c` and the term `f a`:

- With strict, syntactic, and oriented equality, there are only 2 NFs: `f a = f b = b`, and `f a = f c`.
- With join and semi-equational equality, there is a third reduction pattern `f a = a = b/c`, giving the additional normal form `c`.

Overall strict equality is the most conservative (least accepting), and the one whose behavior seems easiest to understand. It does reduce the laziness of the language a bit but even Haskell's `==` function is strict. So we'll go with strict equality.

There is some question about reducible expressions as patterns, e.g. `a = b; f a@x x = x`. I think this can be handled separately from non-linear patterns. Generally, if the pattern is reducible it will result in nondeterminism. E.g. continuing with the example, we will get `f b b` and `b` as distinct normal forms.

## Why

After going through term rewriting I find myself wondering if it is really useful. On the one hand, many of the concepts of term rewriting translate naturally to the setting of a programming language. Modeling expressions as terms and computation as localized rewrites of patterns is an elegant paradigm that extends traditional functional programming languages in a nice way. Particularly with evaluation strategies, the analysis of rewriting strategies was informative: call-by-value is efficient not because it is a good strategy but because it is a "dumb" perpetual strategy. I think going through the literature and formulating a term rewriting system was quite a useful exercise. Also, term rewriting as a field seems under-advertised and there are some great languages like Pure and Maude - it is definitely worth implement term rewriting in Stroscot.

On the other hand, even with the most expressive formulation of term rewriting that I could make, it still is less powerful than logic programming. There is no way to implement a rewrite of `solve[x+2=3,x]` to `` 1` ``, for example, without essentially reimplementing the evaluation logic of `+`. Whereas in logic programming, we can define a set like `{x | x+2=3}` and iterate through its elements. In that sense, term rewriting is trivial - it just defines a specific logical relation `-->` of "rewrites to normal form". Admittedly, it is much nicer to write out a list of term rewriting rules than to explicitly define the logical relation `-->` as a set. Particularly, a set of term rewriting rules is extensible and modular in a way a logical formula is not. But conceptually at least, term rewriting is implementable as a library using logic programming. We could use some macros to make the translation from patterns to formulas more concise.

# Term rewriting

## Logic programming

Now in some sense using logic programming is a "two problems" approach. "Some people, when confronted with term rewriting, think 'I know, I'll use logic programming.' Now they have two problems." Phrasing term rewriting as a logical relation doesn't necessarily make it any easier to implement term rewriting - you still have to compute the transitive closure, do proper indexing and caching, figure out an evaluation strategy, think about optimizations, and think about parallel/concurrent execution of "sparks". It is just that now you are implementing these as part of the logical engine, rather than the term rewriting engine, so everything is slightly munged and harder to recognize.

That being said, ChatGPT says logic programming is a "natural fit for rule-based systems", particularly with the built-in backtracking, so it does seem that using logic programming techniques like CDCL could be beneficial. Optimizing the computation of reflexive transitive closures is a general problem that shows up in logic programming all the time, not just in term rewriting. And the pattern matching stuff of term rewriting is mostly syntax - pawning it off to a library that translates the patterns to logical constraints makes a lot of sense. Specifically for constraints that quantify over variables not bound in the left hand pattern, it pretty much is logic programming and there is not really any method of execution other than "feed this constraint into a logic analyzer and identify the values for which it is satisfiable".

## Higher-order matching

As a consequence of confluence of the substitution calculus, each rewrite step is composed of an expansion in the substitution calculus, a replacement by applying some rule, and a reduction in the substitution calculus, so it is the predicate M \<<- C[l] and C[r] ->> N. Handling lambdas in RHSs is fairly straightforward, just treat beta-reduction as a normal reduction step and handle it with the evaluation machinery. But for the lambdas on the left hand side, in the pattern, it is more complex.

Finding the contexts `C` is fairly straightforward, just enumerate all the subterms of `t`. But solving the equation `s = lθ` is an instance of higher-order unification (specifically higher-order matching). The complexity of higher order matching is somewhere around ${\mathcal {E}}^{4}$, "the minimal solution is of size at most 2^2^2^2..., the number of 2's proportional to the size of the problem". {cite}`stirlingDecidabilityHigherorderMatching2009` That proof is for the simply typed lambda calculus but the only properties of the STLC used are strong normalization and that terms have a defined eta long form (canonical form), so it is most likely also applicable to all lambda terms with unique normal forms. Naturally determining the normal form / lack of a normal form is of complexity $\Sigma_0^1$, but most lambdas in programs are in normal form already or close to it.

There are two main possibilities for implementing higher-order matching. One is to read Stirling's paper and extract an algorithm. He says "implicit in the analysis are positive sensible algorithms for dual interpolation problems", so there is definitely an algorithm to extract. Another is to implement a more general algorithm for solving higher-order unification as opposed to matching, along the lines of Gérard Huet's preunification. There is a semi-algorithm with good performance, {cite}`vukmirovicEfficientFullHigherorder2021`. The implementation is [open source](https://github.com/sneeuwballen/zipperposition/blob/2889c1f0831f01e8e2f8ffabd5fd12b758ba6a30/src/core/JPFull.ml) and only a few hundred lines.

Stirling's method, involving dual interpolation, 3 transformations, and reduction to a tiling game, is conceptually complex. Extracting a usable algorithm from the paper seems like it will be challenging and time-consuming. It is also not clear if the resulting algorithm will be any better than Huet's; it is at least guaranteed to terminate, but perhaps Huet's algorithm terminates for matching problems as well.

In contrast, Huet's algorithm is basically off-the-shelf. I will need unification anyways, for purposes such as logic programming and determining if rules can overlap. Also the approach in Zipperposition allows plugging in "oracles" - I think Stirling's method can be plugged in as such an oracle. There is the issue of unification being $\Sigma_0^1$ and needing to deal with failed unifications but I think starting with Huet's approach makes more sense.

### Unification

Unification is the problem of finding all solutions to a system of equations. First-order unification solves a set of equalities `a1=b1, a2=b2, ...` over tree terms and variables. This can be extended to the "dual unification" problem that also includes disequations `c1 != d1` in the list that must not be satisfied. Constraint logic programming requires solving systems of equations over reals or other sets. The solution takes the form of a complete set of unifiers, where each unifier is a substitution that may have its free variables substituted to obtain a solution, together with constraints over those free variables. A substitution is a set of assignments from variables to expressions.

Unification isn't really part of the semantics of logic programming, as the semantics is phrased in terms of satisfiability. But it is a standard technique used in implementing logic programming, and in practice the implementation defines the semantics. Prolog only implements first-order unification. Teyjus / λProlog limit to higher-order "pattern lambdas". With ZipperPosition {cite}`vukmirovicEfficientFullHigherorder2021` there is outlined a full higher-order unification algorithm extending Huet's semi-algorithm - the need to support multiple unifiers for a complete set complicates things a bit.

The outline of every unification algorithm is that it randomly applies simplifying reduction operations to an equation until it results in a substitution, then applies the substitution to the remaining equations (dereferencing). Here we show {cite}`vukmirovicEfficientFullHigherorder2021`'s, adapted to match the presentation on [Wikipedia](<https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm>):

- delete: `s=s` is removed
- decompose: `a s1 ... sm = a t1 ... tm` to equations `{s1 = t1, ..., sm = tm }`
- rigid/rigid conflict: `a sm = b tn` fails if a and b are different rigid heads
- dereference: `F s1 ... sn = t` to `(F /. σ) ... = t`, if the substitution σ from another equation maps F
- empty equation list: trivially soluble
- alpha/eta normalization: `λxm.s = λyn.t` to `λxm.s = λxm.t' xn+1 . . . xm`, where `m ≥ n`, `xi` disjoint from `yj`, and `t' = t /. {y1 → x1 , ... , yn → xn }`
- beta normalization: reduce left/right to hnf
- under lambda: apply rule for `a = b` to `λx. a = λx. b`

ZipperPosition has more complex reductions for hard cases:

- oracle fail: `s=t` fails if oracle determines to be insoluble

- oracle success: `s=t` has finite CSU, branch to each solution σ_i

- bind: try projections with the following binding substitutions:

  - flex-rigid `P(λx. F s = λx. a t)`: try an imitation of a for F, if a is constant, and all Huet-style projections for F, if F is not an identification variable.
  - flex-flex with different heads `P(λx. F s = λx. G t)`: all identifications and iterations for both F and G, and all JP-style projections for non-identification variables among F and G.
  - flex-flex with identical heads and the head is an elimination variable, `P(λx. s = λx. t)`: no bindings.
  - flex-flex with identical heads, `P(λx. F s = λx. F t)`: all iterations for F at arguments of functional type and all eliminations for F.

The flex-binding step is slow, but a good set of oracles makes the algorithm efficient for most practical cases. Of course it would be better to find reduction rules that solve things generally rather than oracles which work on specific cases, but this is hard.

The unifier search can be integrated with the overall logical search for satisfiable formulas.

By default Prolog does not use the [occurs check](https://en.wikipedia.org/wiki/Occurs_check) in unification. This means for `x == f x` the substitution `x -> f x` is obtained. Denotationally this can be accommodated by allowing states to contain infinite rational terms, {cite}`weijlandSemanticsLogicPrograms1990` `x = f (f (f (...)))` in this case. In most Prolog programs the occurs check does not make a difference and simply slows down unification. {cite}`aptWhyOccurcheckNot1992` Prolog defines a `unify_with_occurs_check` predicate, and has an option for doing the occurs check in the implicit unification when dispatching predicates. Meanwhile miniKanren always uses the occurs check. The occurs check is needed in first order logic theorem-proving, where skolemization turns quantifiers into variables and is sound only if the occurs check is used. For Stroscot, we have infinite terms so skipping the occurs check makes perfect sense. It might be worth having it as a flag though in case the `unify_with_occurs_check` is needed for the skolemization thing. I don't think we're going to do much with quantifiers in Stroscot though? And there are other techniques for handling quantifiers besides skolemization.

## Cycle detection

A first strategy for handling cycles is to prove the system is acyclic. Per {cite}`ketemaViciousCirclesRewriting2005` this includes orthogonal weakly head normalizing higher-order TRSs. Also in general any terminating system is acyclic, so we can use all the techniques from the [TERMCOMP](https://termination-portal.org/wiki/Termination_Competition_2023) term-rewriting category. There is probably some room for adapting termination techniques, acylic-ness is a weaker property than termination (local rather than global) so it should be easier to prove.

For general-purpose detection there are SCC computation algorithms; Wikipedia has a [list](https://en.wikipedia.org/wiki/Strongly_connected_component#Algorithms). The DFS algorithms seem most appropriate as they can naturally be maintained during the reduction graph search; finding the normal forms of a term essentially already is a DFS. Kosaraju's algorithm is not appropriate as computing the transpose / converse of the reduction relation is not easy. Comparing Tarjan and the path-based algorithms, Tarjan uses a second index (pointer) while the path-based uses a stack. The stack manipulation of the path-based algorithm is simpler to understand than the invariants of Tarjan; the Wikipedia page for Tarjan is constantly vandalized with people who do not understand it. So I would say the path-based algorithm is better.

For associativity and commutativity there are special unification algorithms, where we represent terms as lists or bags rather than trees. There are some PhD theses and so on for this, Maude has references. I would say these are optimizations and for now acyclic detection plus general-purpose cycle handling is sufficient.

## Nondeterminism

This sounds a bit tricky to implement but it is not too bad. An analysis of nondeterminism starts with analyzing confluence. There are various equivalent ways of specifying confluence:

- (Standard definition) $a \in S`$ is deemed confluent if for all pairs $b,c\in S$ such that $a \overset{*}{\to} b$ and $a \overset{*}{\to} c$, there exists $d \in S$ with $b \overset{*}{\to} d$ and $c \overset{*}{\to} d$. If every $a \in S$ is confluent, we say that $\to$ is confluent.
- (Church-Rosser) $\to$ is confluent if $x \overset{*}{\leftrightarrow} y$ implies that there exists a $z \in S$ with $x\overset{*}{\to}z$ and $y\overset{*}{\to}z$.
- (Semi-confluence) $a \in S`$ is deemed confluent if for all pairs $b,c\in S$ such that $a \to b$ and $a \overset{*}{\to} c$, there exists $d \in S$ with $b \overset{*}{\to} d$ and $c \overset{*}{\to} d$. If every $a \in S$ is confluent, we say that $\to$ is confluent.

You might not see that these are equivalent at first glance but check out {cite}`baaderTermRewritingAll1998` Theorem 2.1.5, basically you prove semi-confluence implies Church-Rosser by inducting on the length of the chain of $x \overset{*}{\leftrightarrow} y$. I think in the case of our infinitary T\* relation there is a similar proof which goes over whether the relation holds by the reflexive, transitive, one-step, or topological closure properties.

Another way of putting confluence is that a confluent element reduces to at most one normal form. Now due to our handling of meaningless terms, the TRS is strongly normalizing and every element has at least one normal form. Therefore, a confluent element corresponds directly with a value, it is just a matter of computing that value. Similarly a non-confluent element is nondeterministic and evaluates to multiples values.

There are some other properties in the literature like NF, UN, and UN→ (c.f. [Wikipedia](<https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)>)). Since the TRS is strongly normalizing these are equivalent to confluence; confluence is the most well-studied so that is what I am focusing on, but maybe there are some papers on the other properties that are applicable.

The Knuth-Bendix algorithm produces a confluent system from a set of non-oriented equations, but the rules in programs are oriented, so using this would be confusing. Not to mention that the algorithm fails often. So that's out.

### Local confluence

A necessary condition for confluence is weak/local confluence, i.e. that each critical pair is convergent. But this is not sufficient in general, due to nonterminating systems like `b = c; c = b; b = a; c = d`. Newman's lemma states that a finite locally confluent strongly normalizing TRS is confluent. There is also {cite}`hirokawaDecreasingDiagramsRelative2009` which shows that left-linear locally confluent "decreasing critical pair" systems, i.e. one where its critical pair steps are *relatively terminating*, i.e. the relation 'arbitrary steps followed by a critical pair step followed by arbitrary steps' is terminating, is convergent. Also trivial critical pair steps can be excluded, hence this includes weakly orthogonal TRSs.

In the infinitary case, Newman's lemma does not apply for the traditional "strong convergence" definition, but with Stroscot's advanced normalization I am not sure. The examples of non-confluence given in {cite}`klopInfinitaryNormalization2005` do have confluent rewrites under T\*. Let's try to sketch a proof that Newman's lemma holds under T\*. Suppose we have a T\*-closed rewriting system that is locally confluent and terminating. Then for contradiction suppose we have a non-confluent element, i.e. there are $a,b,c$ such that $a \overset{*}{\to} b$ and $a \overset{*}{\to} c$ but there does exist any element $d \in S$ with $b \overset{*}{\to} d$ and $c \overset{*}{\to} d$. Consider the set of such non-confluent elements and order them by the ordinal length of the reduction $a \overset{*}{\to} c$; consider the tuple with minimal length. Then by definition of T\*, if $a \overset{*}{\to} c$, we either have:

- $a=c$. Then $c\to b$ and we are done.
- $a\overset{*}{\to}c'$, $c'\overset{*}{\to}c$. First ask if $a,b,c'$ is a non-confluent triple. If it is non-confluent, then we have a smaller counterexample. Therefore, we must have an element $v$ such that $b\overset{*}{\to}v$ and $c'\overset{*}{\to}v$. Then consider $c',v,c$; again if it is non-confluent then we have a smaller counterexample. Therefore, there is an element $w$ with $v\overset{*}{\to}w$ and $c\overset{*}{\to}w$. But then $a\overset{*}{\to}c'\overset{*}{\to}c\overset{*}{\to}w$ and $a\overset{*}{\to}b\overset{*}{\to}v\overset{*}{\to}w$ and the original example is confluent.
- $a \to b$ and $a \to c$: this contradicts local confluence
- $a \to b$ and there is a sequence of relations $a_i \overset{*}{\to} c_i$ such that $\lim d(a_i,a)^2 + d(c_i,c)^2 = 0$. Consider the relaxed relation $\overset{*}{\to}_\epsilon$. Then for every $\epsilon$ there is an $i$ such that $a \overset{*}{\to}_\epsilon c_j$ for $j \geq i$. By some sort of induction hypothesis $\overset{*}{\to}_\epsilon$ is confluent so for every $c_j$ there is a $d_j$ with $b \overset{*}{\to}_\epsilon d_j$ and $c_j \overset{*}{\to}_\epsilon d_j$. Then there is a point of closure $d$ of $d_j$ and $\{b,c\} \overset{*}{\to} d$.
- two sequences of relations ${a_1}_i \overset{*}{\to} b_i$ and ${a_2}_i \overset{*}{\to} c_i$

Now for non-confluent elements, I don't see much choice except to enumerate all the values. It would be nice to optimize exceptions somehow and avoid fully evaluating expressions when they evaluate to an exception and we know there is a non-exception value, but in general we can't predict the results of evaluation so we will have to examine all normal forms / evaluation paths exhaustively. From analyzing local confluence and critical pairs, we can at least identify a small set of backtracking points, choices in which rule is applied, namely the non-convergent critical pairs. This analysis is a bit conservative in that a non-convergent critical pair could still converge on a specific expression, but at that point we're well into the weeds of fine-grained optimization.

> Newman's lemma is that a terminating locally confluent TRS is confluent. But termination is quite strong. For a terminating TRS the TRS syntactic equality notion is equivalent to strict equality, hence the system is left linear in the CTRS sense, hence why this includes Newman's lemma.

We say → has random descent (RD), if for each R:a ↔∗b with b in normal form, all maximal reductions from a have length d(R) and end in b. Systems with random descent are confluent.

### Verifying confluence

As confluence is such a useful property, we want to be able to automatically determine confluence. Fortunately, confluence has gotten significant attention and has automated provers; there is the yearly-ish [Confluence competition](http://project-coco.uibk.ac.at/2023/). There are some key algorithms:

- The decreasing diagrams technique is a complete "pen-and-paper" method for confluence on countable abstract rewrite systems.

- Computing critical pairs. A non-joinable critical pair means the system is not confluent. If all critical pairs are joinable the system is said to be locally confluent. An orthogonal system is one with no critical pairs, while a weakly orthogonal system is one with critical pairs that are trivially joinable. For an HORS there are more constraints to be orthogonal in addition to no critical pairs ("every set of redexes is pairwise simultaneous"). The substitution calculus must be complete, only needed for gluing, a descendant rewriting system, parametric, have head-defined rules, and be naturally closed under substitution. Parallel rewrite steps must be serializable and left-hand sides of rules must be linear.

  V. van Oostrom. Developing developments. TCS, 175(1):159–181, 1997.
  V. van Oostrom and F. van Raamsdonk. Weak orthogonality implies confluence: The higher order case. In Proc. 3rd LFCS, volume 813 of LNCS, pages 379–392, 1994.

- Proving termination. The Knuth Bendix Criterion (Newmann's lemma) says a terminating system is confluent iff it is locally confluent. Termination can be shown by exhibiting a well-ordering, such as recursive path ordering, dependency graph decomposition, and the subterm criterion.

  WANDA has more advanced techniques. Cynthia Kop. Higher Order Termination. PhD thesis, Vrije Universiteit, Amsterdam, 2012

  TTT2 also has some good techniques.

  Gramlich–Ohlebusch’s criterion says for innermost-terminating TRSs R with no innermost critical pairs, R is confluent if and only if all critical pairs are joinable by innermost reduction. There are innermost terminating systems that aren't terminating so this criterion can prove some systems that Knuth-Bendix can't.

- Decomposition: Several properties allow dividing the system into smaller, more tractable systems. First is modularity, that the disjoint union of two systems with the property has the property. We also usually have the converse, the disjoint union has the property only if the subsystems have the property.

  - Weak normalization and consistency (w.r.t. equivalence) are modular for first-order systems.
  - Left linearity, confluence, and unique normal forms (w.r.t. equivalence) are modular for semi-equational CTRSs.
  - Confluence is modular for join and semi-equational CTRSs. In fact if the disjoint union is confluent then the component systems must be confluent.
  - Confluence plus left linearity is modular for higher-order TRSs.
  - Weak termination, weak innermost termination, and strong innermost termination are modular for CTRSs in combination with confluence or the property that there are no extra variables in the conditions.
  - NF, unique normal forms with respect to reduction, and consistency with respect to reduction are modular in combination with left linearity. Consistency w.r.t. reduction means that there is no term reducing to two distinct variables; it is implied by the unique normal form property w.r.t. reduction as variables are normal forms.
  - Strong normalization plus consistency w.r.t. reduction plus left linearity is modular. This likely holds for CTRSs without extra variables as well.

  Order-sorted decomposition uses persistence of confluence. If sorts can be assigned to all terms and rule variables such that all rules don't increase the sort, then confluence can be separately considered for each sort and confluence as a whole follows from confluence on well-sorted terms.

  Decreasing diagrams allows decomposing a left-linear TRS into duplicating and non-duplicating rules. The TRS is confluent if all critical peaks are decreasing with respect to a rule labeling and the duplicating rules are terminating relative to the non-terminating rules.

  Layer-preserving decomposition decomposes TRSs into minimal pieces such that taking pieces pairwise they form layer-preserving combinations, i.e. rules in one piece operate only on terms of that piece. It is used in CSI.

- 10. Nagele, B. Felgenhauer, and A. Middeldorp. Improving automatic confluence analysis of rewrite systems by redundant rules. In Proc. 26th RTA, volume 36 of LIPIcs, pages 257–268, 2015.

## Infinitary rewriting

The common notions of an ARS carry over to infinitary reductions as follows: {cite}`endrullisInfinitaryTermRewriting2014`

- transitive reduction: irreflexive kernel of reduction closure
- normal form: irreducible term
- strongly normalizing (terminating): every infinite reduction sequence has a limit
- nonterminating reduction: infinite reduction sequence with no limit or that does not reduce to its limit
- weakly normalizing (normalizing): every term has a reduction to a normal form

We can split into two steps:

## Evaluation strategy

For convergent (confluent and strongly normalizing) programs, such as the simply typed lambda calculus, all strategies are normalizing and the result is the same no matter how they are reduced. So the focus is on inferring convergence and doing reduction efficiently. "In the small" leftmost innermost ensures "complete development", i.e. a subterm is reduced completely before the outer term, hence we can compute the subterm fully and only store an optimized representation of the normal form. So we can compile to fast assembly like a state machine. "In the large" optimal reduction ensures the smallest number of steps so we can avoid duplicating work and performing unneeded work.

But strongly normalizing implies not Turing complete, hence the termination verification will cause problems for complex programs. We need a fallback for these complex programs. Leftmost outermost reduction is the basis of lazy evaluation and is hypernormalizing for the lambda calculus. But for TRSs LO is only normalizing for left-normal TRSs, where variables do not precede function symbols in the left-hand sides of the rewrite rule. A better strategy is outermost fair (ensuring each outermost redex will eventually be evaluated - the simplest example is parallel outermost) - it's hypernormalizing for critical pair TRSs (decreasingly confluent TRSs), in particular weakly orthogonal TRSs. {cite}`hirokawaStrategiesDecreasinglyConfluent2011` So outermost fair seems a reasonable default, but there are non-orthogonal systems where it fails. The optimal reduction stuff is defined for match sequential TRSs but is a normalizing strategy that computes a result in the smallest number of reduction steps.

We could do user-specified strategies like Stratego, but then how would we know that they're normalizing.

There are is also lenient evaluation which evaluates all redexes in parallel except inside the arms of conditionals and inside lambdas, but it adds extra memory overhead for parameter passing.

Now, one can argue about which computational strategy is better (time, space, parallelism, ...)
Stroscot: be accepting of programs, ensure a normalizing strategy. But after that aim for most efficient in time/space for strict programs.

Q: can normalizing be as efficient as strict
profiling, other optimization tricks

So The way we handle cycles in the rewrite engine is something like:

- detect cyclic term via rule cycle detection or presence of AC operator
- use specialized matching (eg AC matching or Tarjan SCC + memo hash table) to identify all reductions out of SCC
- end with condensed normal form if no reduction out of SCC
- otherwise, pick a reduction out of the SCC

Then this infinite term is computed in chunks and fed to the surrounding context on demand (laziness), ensuring that a finite normal form is reached if possible and otherwise implementing an infinite stream of commands.

If the substitution calculus is convergent, then terms can be represented by preterms in normal form.

If reduction does not end in a condensed normal form, then the sequence of terms must be infinitely expanding in the sense that for every size s there is a point in the reduction where terms are always at least size s. Otherwise, assuming a finite number of term symbols, there are only finitely many terms of size < s, so there would be a cycle in the reduction and reduction would end in a condensed normal form.

A context is linear if every hole occurs exactly once.

### Normalization

A hypernormalizing strategy is a strategy that is normalizing even if arbitrary reduction steps are taken before and after steps of the strategy. This allows the compiler to make optimizations without changing the behavior of the program. A hypernormalizing strategy allows aggressive optimizations and program transforms.

There are also stronger properties than normalization. A Church-Rosser strategy is one with common reducts, i.e. there exist m and n, such that $F^m(t)=F^n(u)$ for every t and u equal via forward/backward evaluation. A normalizing strategy is Church-Rosser if the system is confluent and weakly normalizing (i.e. all objects have a normal form). In general a many-step CR strategy exists for effective ARS's, i.e. countable (in a computable fashion) and with a computable reduction relation. But the strategy is quite hard to compute, as it has to synchronize reducing subterms so that all components are reduced the same amount. And it's not clear that this synchronization offers anything to the programmer.

Cofinal strategies are weaker than Church-Rosser but stronger than normalizing: for every term a, if a reduces in a finite number of steps to b, then there is an object c obtained by applying the strategy some number of times to a such that b reduces to c. For critical pair TRSs any "fair" strategy that ensures every redex is eventually contracted is cofinal. The cofinal property provides slick proofs - it ensures every redex not part of a cycle is contracted. But at runtime non-normalizing terms have indistinguishable behavior (infinite loop), hence this means the cofinal strategy is doing unnecessary work.

There are also termination properties like strong convergence that ensure that for every term, there exists some number of reduction steps after which the head cannot be rewritten.
To ensure that term rewriting halts we probably also want a property like strong convergence, but this is a property of the rewriting strategy, not the TRS proper.

