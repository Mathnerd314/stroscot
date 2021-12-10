Term rewriting
##############

Down with functions
===================

Haskell and other functional programming languages have had great success. But there's a dirty secret: the programs you can write with these languages are restricted. In particular the evaluation strategy doesn't have flexible semantics.

For example see the program from `this old Usenet post <https://groups.google.com/g/comp.lang.functional/c/sb76j3UE5Zg/m/h1ps0wEaTckJ>`):

::

  myand False x = False
  myand x False = False
  myand True True = True

Ideally ``myand`` should be defined with parallel semantics, so that ``myand undefined False = False``. But Haskell defines evaluation as trying the rules in order, so the first equation evaluates ``undefined`` and evaluation results in an error.

In Stroscot we have both parallel and sequential matching. The sequential matching is straightforward to implement as it is simply copying the implementation from Haskell and friends. The parallel matching is more difficult - for a proper explanation of the semantics we have to shift the paradigm from functions to term rewrites.

Menagerie
=========

There are various types of behavior which show up in a TRS, which require different handling.

* convergent (confluent and terminating) - These include typed systems such as the simply typed lambda calculus. For these, the result is the same no matter how they are reduced. So the focus is on do the reduction efficiently, compiling to fast assembly and/or doing optimal reduction to reduce in the smallest number of steps.
* cycles - The untyped lambda calculus has cycles, e.g. ``Omega = let w=\x.x x in w w`` reduces to itself and :cite:`venturiniReductionGraphsLambda1983` shows a 6-cycle ``M M I``. Similarly commutativity ``a + b = b + a`` generates cycles. If we are able to detect these then the solution is to collapse the SCC into a single term. The literature for commutativity uses a (congruence-)class-rewriting system - it is split into rules R and (reversible) equations S, and computes R/S (R mod S). The term at the end is considered a normal form because only S rules apply. So ``<Omega>`` and ``<a + b>`` would be normal forms.
* infinitely expanding terms - for example ``x = 1 :: x`` or ``let t = \x. x x x in t t``. If reduction does not end in a normal form SCC, then the sequence of terms must be expanding in the sense that for every size s there is a point in the reduction where terms are always at least size s (otherwise since there are only finitely many terms of size < s, there would be a cycle and it would be a normal form SCC). Here the idea is to first extend the computation to the limit, if it exists - an infinite normal form defined as a solution to a recurrence equation. So ``x = 1 :: x`` is already in a normal form, the second might resolve to ``x = x t``. Then this infinite term is computed in chunks and fed to the surrounding context (laziness).

   focus is on trying all reductions of terms and finding a normal form if it exists.

Normal forms
============

In a general TRS the notion of "value" is not well-defined: random evaluation strategies will give different results. We want there to be no privileged evaluation strategy - we always reach the same result. The weakest property that guarantees this is unique normal forms with respect to reduction (UN→), which is that if a term reduces to two normal forms then the normal forms are identical.

But a program naturally falls into error terms - these can be modeled by making errors reduce to themselves (hence they are not normal forms but omega normal forms - the class of terms including normal forms and terms which reduce to themselves). However UN→ is not sufficient for this, because error terms are not normal forms.

Since we model the program as generating an infinite data structure for I/O we extend this to uniqueness of infinite normal forms. Bohm reductions reduce all terms without head normal forms (i.e. those that perpetually have a redex in the head position, i.e. are root-active) to a bottom. :cite:`kennawayTransfiniteReductionsOrthogonal1991` Really we want uniqueness of the Bohm tree and not just uniqueness of normal forms.

Normalizing strategies find the normal form if it exists, i.e. if any strategy succeeds, a normalizing strategy succeeds. A normalizing strategy avoids getting stuck evaluating nonterminating arguments (time efficient in the large). With a normalizing strategy one can reason unconditionally about program fragments and the semantics are cleaner, allowing aggressive optimization and program transforms (substituting expression for value, removing unused expressions). A normalizing strategy handles if-then-else and short-circuit functions gracefully. It also allows infinite data structures.

So for correctness and expressiveness, the TRS part of the program should have unique normal forms and a normalizing strategy.

Other properties
================

Unfortunately these properties are not well studied in the literature. In general the properties are undecidable, so there is no simple and precise condition. Researchers have mostly focused on stronger properties, i.e. conditions sufficient for the properties to hold, as opposed to equivalent or weaker conditions.

Confluence
----------

Confluence implies UN→; it is equivalent if the TRS is weakly normalizing. And there is an extension theorem: every TRS with unique normal forms (UN=) can be extended to a confluent TRS with the same set of normal forms by adding bottom terms and reductions to normal forms and bottoms that preserve the equivalence classes of terms. :cite:`middeldorpModularAspectsProperties1989` Similarly a system can be shown to be UN= by presenting an extension of it that is confluent. :cite:`klopExtendedTermRewriting1991` So a UN= program is just a partially specified system. UN→ is a little more complex though. And the equivalence classes of terms are uncomputable in general so the extension is as well.

Confluence avoids situations where a system may branch into two distinct diverging states. It makes finding a normalizing strategy much easier as the strategy only has to avoid getting stuck evaluating a term infinitely (using the same rule infinitely often), as opposed to UN→ where the strategy must avoid using the wrong reduction rule at every step.

The Knuth-Bendix algorithm produces a confluent system from a set of non-oriented equations, but the rules in programs are oriented, so using this would be confusing. Not to mention that the algorithm fails often. So that's out.

A necessary condition for confluence is weak/local confluence, i.e. each critical pair is convergent. But this is not sufficient. Newman's lemma is that a terminating locally confluent TRS is confluent. But termination is quite strong. A generalization is a critical pair system :cite:`hirokawaDecreasingDiagramsRelative2009` (also called decreasingly confluent): the system must be left-linear, locally confluent, and its critical pair steps must be *relatively terminating*, i.e. the relation 'arbitrary steps followed by a critical pair step followed by arbitrary steps' is terminating. Trivial critical pair steps can be excluded, hence this includes weakly orthogonal TRSs. For a terminating TRS the TRS syntactic equality notion is equivalent to strict equality, hence the system is left linear in the CTRS sense, hence why this includes Newman's lemma.

We say → has random descent (RD), if for each R:a ↔∗b with b in normal form, all maximal reductions from a have length d(R) and end in b. Systems with random descent are confluent.

Termination
-----------

There are also stronger properties than normalization. A Church-Rosser strategy is one with common reducts, i.e. there exist m and n, such that :math:`F^m(t)=F^n(u)` for every t and u equal via forward/backward evaluation. A normalizing strategy is Church-Rosser if the system is confluent and weakly normalizing (i.e. all objects have a normal form). In general a many-step CR strategy exists for effective ARS's, i.e. countable (in a computable fashion) and with a computable reduction relation. But the strategy is quite hard to compute, as it has to synchronize reducing subterms so that all components are reduced the same amount. And it's not clear that this synchronization offers anything to the programmer.

Cofinal strategies are weaker than Church-Rosser but stronger than normalizing: for every term a, if a reduces in a finite number of steps to b, then there is an object c obtained by applying the strategy some number of times to a such that b reduces to c. For critical pair TRSs any "fair" strategy that ensures every redex is eventually contracted is cofinal. The cofinal property provides slick proofs - it ensures every redex not part of a cycle is contracted. But at runtime non-normalizing terms have indistinguishable behavior (infinite loop), hence this means the cofinal strategy is doing unnecessary work.

There are also termination properties like strong convergence that ensure that for every term, there exists some number of reduction steps after which the head cannot be rewritten.
To ensure that term rewriting halts we probably also want a property like strong convergence, but this is a property of the rewriting strategy, not the TRS proper.

A perpetual strategy is the opposite of normalizing - if any strategy diverges, then perpetual strategy diverges. Leftmost-innermost is close to the strategies commonly used in strict languages and is perpetual. With a perpetual strategy inlining etc. hold only if reduction of the expression terminates, i.e. one must keep track of termination properties. A perpetual strategy gives the wrong behavior for if-then-else and short-circuit functions, so strict languages special-case these to ensure they don't cause nontermination. Perpetual strategies are antagonistic, "I'll crash your program if I can".

A hypernormalizing strategy is a strategy that is normalizing even if arbitrary reduction steps are taken before and after steps of the strategy. Leftmost outermost for lambda calculus (the basis of lazy evaluation) is hypernormalizing. But for TRSs LO is only normalizing for left-normal TRSs, where variables do not precede function symbols in the left-hand sides of the rewrite rule. A better strategy is outermost fair (ensuring each outermost redex will eventually be evaluated- the simplest example is parallel outermost) - it's (hyper)normalizing for critical pair TRSs,, which include weakly orthogonal TRSs. :cite:`hirokawaStrategiesDecreasinglyConfluent`

Equality and left-linearity
===========================

The TRS notion of equality ``eq_t x x -> True`` is different from strict equality ``eq_s x y | x == y`` in a CTRS (conditional term rewriting system). Strict equality compares equality of normal forms (fully reduced terms). But ``eq_t c c`` matches even if ``c`` doesn't have a normal form. A broader CTRS equality is semi-equational equality which equates all terms that can be rewritten to each other via rewrites and inverse rewrites. In general strict equality is weaker than TRS equality (``x==x`` can't be simplified to true), TRS equality is weaker than semi-equational equality (because of the inverse rewrites). In general all 3 may be uncomputable, but strict equality is computable if there is a computable normalizing strategy, semi-equational equality is computable depending on the complexity of the system, while normalizing reduction in a system with TRS equality is undecidable.

For the CTRS to be confluent if the unconditional TRS is, the conditions in the rules have to be stable, i.e. if the terms involved are reduced the truth value of the condition doesn't change. Non-left-linear rules aren't stable, in general, while strict equality and equational equality are. So left-linearity essentially fixes a specification problem. There are non-left-linear systems that have unique normal forms or are confluent but it's arguable if they're useful or if they're just CTRSs with complex conditions in disguise.

If you aren't convinced of the bad behavior of left non-linearity consider some systems:

* In the system ``f x x = a, f x (g x) = b, c = g c`` the first rule is non-linear. There are no critical pairs, so the system is locally confluent, but ``f c c`` reduces to both ``a`` and ``b`` hence the system is not confluent. With strict equality ``c`` has no normal form, hence ``f c c`` does not reduce with an ``f``-rule (it gets stuck evaluating a reduction of the form ``c -> g c -> g (g c) -> ...``). With equational equality ``f c c`` reduces to both ``a`` and ``b`` in one step hence the system is not locally confluent.

* In the system ``f x x = X, a = b, a = c, c = c, d = c, d = e``, the term ``f a d`` reduces to both ``X`` and ``f b e``, hence the system does not have unique normal forms. With strict equality ``f a d`` does not reduce to ``X`` and with equational equality ``f b e`` reduces to ``X``.

Modularity
==========

A property is modular if the disjoint union of two systems with the property has the property.

Left linearity, confluence, weak normalization, unique normal forms (w.r.t. equivalence), and consistency (w.r.t. equivalence) are modular for first-order systems. Modularity of left linearity, confluence, and unique normal forms extend to semi-equational CTRSs. Confluence also extends to join CTRSs. In fact if the disjoint union is confluent then the component systems must be confluent. Confluence is not modular for higher-order TRSs but confluence plus left linearity is.

Weak termination, weak innermost termination, and strong innermost termination are modular for CTRSs in combination with confluence or the property that there are no extra variables in the conditions.

NF, unique normal forms with respect to reduction, and consistency with respect to reduction are modular in combination with left linearity. Consistency w.r.t. reduction means that there is no term reducing to two distinct variables; it is implied by the unique normal form property w.r.t. reduction as variables are normal forms.

Strong normalization plus consistency w.r.t. reduction plus left linearity is modular. This likely holds for CTRSs without extra variables as well.

Higher-order rewriting system
=============================

A HORS consists of a substitution calculus, an alphabet, and a set of rewrite rules.

A substitution calculus is an ARS on a set of prestructures. A structure is a prestructure that is a normal form with respect to the substitution calculus ARS.

Types are sets of prestructures. We assume every type is inhabited by an infinite number of atomic prestructures called variables. Among the variables a countable set called the alphabet is distinguished whose elements are called symbols. Holes are distinguished variables indexed by an integer.

A rewrite rule is a LHS and RHS, both closed structures of the same type, closed meaning containing no free variables that are not symbols. The TRS on structures is defined by M -> N if M <->* C[l] and C[r] <->* N for some rewrite rule l -> r and context C containing a hole of type matching the rewrite rule.

As a consequence of confluence each rewrite step is composed of an expansion in the substitution calculus, a replacement by applying some rule, and a reduction in the substitution calculus, so it is M <<- C[l] and C[r] ->> N

A m-ary precontext is a preterm with holes 1 through m. It is linear if every hole occurs exactly once.
The set of term is the set of representatives of preterms when considering equivalence classes under the substitution calculus.

 and a signature of operator symbols or constants.

An example is the lambda calculus. The set of raw preterms on a set of bound variables is built in the following way: A bound variable is a raw preterm iff it is in the set of bound variables. All other nullary symbols are raw preterms regardless. The application of two raw preterms is a raw preterm. Abstraction is a raw preterm where the first raw preterm is a bound variable symbol and the second is a raw preterm over the set of bound variables extended with the newly bound variable. The rewrite alphabet consists of operators and term variables (a.k.a. free variables), also nullary.

A preterm is a raw preterm over the empty set, i.e. all bound variables are bound. If it contains free variables it is called open, otherwise closed.


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
- Lenient evaluation - computation rule [Traub, FPCA 89], where all redexes are evaluated in parallel except inside the arms of conditionals and inside lambdas.
- extra memory overhead for parameter passing (inefficient)
  - strictness analysis to optimize to eager (which has identical semantics to lazy 99% of the time)

Now, one can argue about which computational strategy is ``better'' (time, space, parallelism, ...)
IMO: aim for most efficient normalizing strategy.


Stroscot aims to be accepting of programs so it uses a normalizing strategy.



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

Higher-order matching
=====================

Handling lambdas in RHSs is fairly straightforward, just reduce them away when they are encountered. But in higher-order term rewriting systems the lambdas can show up on the left hand side, in the pattern. The rewriting system is then defined modulo lambda reduction. Executing a rule ``l -> r`` on a term ``t`` solves the equation ``t = C[lθ]`` and replaces it with ``C[rθ]``.

Finding the contexts ``C`` is fairly straightforward, just enumerate all the subterms of ``t``. But solving the equation ``s = lθ`` is an instance of higher-order unification (specifically higher-order matching).

Higher order matching is decidable for the simply typed lambda calculus. But the proof is of the form "the minimal solution is of size at most 2^2^2^2..., the number of 2's proportional to the size of the problem". There are 3 transformations presented in the proof which reduce a larger solution to a smaller solution. These might be usable to prune the search tree. But at the end of the day it's mostly brute-force.

The proof relies on some properties of the STLC, namely normalization and that terms have a defined eta long form (canonical form).

Dispatch
========

The standard vtable implementation of Java/C++ is out. TODO: check out pattern dispatch paper


