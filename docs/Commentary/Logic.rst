Logic
#####

.. math::
  :nowrap:

  \newcommand{\rule}[3]{ \dfrac{\displaystyle ~~#1~~ }{\displaystyle ~~#2~~ } \  (#3)}
  \newcommand{\defeq}{\overset{\text{def}}{=}}
  \newcommand{\with}{\mathbin{\mathrm{\&}}}
  \newcommand{\par}{\mathbin{\mathrm{⅋}}}
  \newcommand{\multimapboth}{\mathbin{\mathrm{⧟}}}
  \newcommand{\bang}{{\mathrm{!}}}
  \newcommand{\whim}{{\mathrm{?}}}
  \newcommand{\ocin}{\mathrel{\raise{-1pt}{\mathrm{!}}\mathord{\in}}}
  \definecolor{mygray}{RGB}{156,156,156}
  \newcommand{\sk}[1]{{\color{mygray} #1}}

The logic proper
================

The sequent calculus idea is inspired by :cite:`downenSequentCalculusCompiler2016`. But they use a weird nonstandard logic (Figures 4/5) to match GHC Core and its syntax. In a new language it seems better to use the standard logical derivation rules without messing with them, and worry about the syntax second. Stroscot's infinite tree representation with use-def is better than the MultiCut / Rec rules because it allows the graph reduction method used in GHC to work.

For the base logic I went with linear logic because optimal reduction :cite:`guerriniTheoreticalPracticalIssues1996` is based on using the boxes of linear logic as markers. Without linear logic every sequent would be its own box, IDK if it could be made to work reasonably.

The logic is two-sided because it's more expressive to have the duals as part of the language. The one-sided logic with the duals and non-duals just seems like a way of encoding the left/right sides as a single list.

Jumbo connectives
=================

Based on :cite:`levyJumboLcalculus2006`, Stroscot aims for the largest allowable set of operators. In particular we generalize into two jumbo operators, :math:`\Sigma` (sigma) and :math:`\Pi` (pi). The generalized :math:`\Pi` rule is similar to Levy's rule except it allows multiple conclusion propositions. The generalized :math:`\Sigma` rule is the dual of :math:`\Pi`.

We have indexed variables :math:`A_{ij}` and :math:`B_{ik}` where :math:`0 \leq i < N, 0 \leq j < m_i, 0 \leq k < n_i`. We call :math:`N` the length of the jumbo type and the list :math:`[(m_i,n_i)]` the jumbo-arity.

The dual of implication is called "subtraction" or "difference" and is denoted :math:`-`. For an ADTs, the RHS of the difference is empty, i.e. ``a A | b B1 B2 | C`` looks like :math:`\Sigma [(a, [A]-[]),(b, [B_1, B_2]-[]), (c,[]-[])]`. This follows :cite:`wadlerCallbyvalueDualCallbyname2003` :cite:`crolardFormulaeastypesInterpretationSubtractive2004`.

When the RHS of :math:`\Sigma` is nonempty we get terms with holes, that can be pattern-matched by filling the holes, e.g. `difference lists <https://en.wikipedia.org/wiki/Difference_list>`__. (TODO: check that this actually gives efficient concatenation)

The jumbo connectives have the nice "unpacking" property that any combination of :math:`\Sigma` connectives is equivalent to a single :math:`\Sigma` connective, and likewise for :math:`\Pi`.

Common connectives
==================

The idea behind :math:`\bot` as contradiction is as follows: if we have a sequent :math:`\Gamma \vdash A\otimes \neg A`, we can decompose this into sequents :math:`\Gamma_1 \vdash A` and :math:`\Gamma_2, A \vdash` where :math:`\Gamma = \Gamma_1, \Gamma_2`. Then we can cut to derive the sequent :math:`\Gamma \vdash`, and hence derive :math:`\Gamma \vdash \bot`.

The notation :math:`\land,\lor` is chosen because the structure-preserving translation from intuitionistic logic preserves the logical operators :cite:`dicosmoIntroductionLinearLogic2015`, hence some intuition arises from using it. The notation for times and par is trickier; times and par are both `tensor/monoidal products <https://en.wikipedia.org/wiki/Monoidal_category>`__ (identities :math:`1,\bot`), and do not appear in classical logic. The fact that tuples are typically positive data leads us to privilege :math:`\otimes` as the default product, agreeing with Girard.

:math:`\par` is the dual of :math:`\otimes` in the sense that :math:`A \par B \equiv \neg (\neg A \otimes \neg B)`; unfortunately for deciding a notation, this seems to be its only useful property. :math:`\oplus, \odot ,\Box,\sharp, \bullet` and :math:`*` have meanings (direct sum/coproduct, Hadamard product/XNOR gate/symmetric product, modal operator, music, multiplication/logical and, convolution) dissimilar from the function of :math:`\par`. :math:`\mathbin{{\scriptstyle+}\mkern-0.522em\raise{-0.077em}{\diamond}},\mathbin{{\vee}\mkern-0.815em\raise{0.09em}{\bigcirc}}` don't have Unicode symbols so are hard to use. In the end none of the operators seems particularly evocative. :math:`\par` on the other hand redirects to linear logic on Wikipedia. So we follow Girard.

Programming types
-----------------

With the programming types we see the justification for the jumbo types: they can represent abstract data types (ADTs). Even though we can encode :math:`\Pi,\Sigma` using the common connectives:

.. math::

  \Pi [(\#t_1,[A_{1,1},A_{1,2},\ldots] \multimap [B_{1,1},B_{1,2},\ldots]),\ldots] \equiv (\smash{\stackrel{-}{\neg}} A_{1,1} \par \smash{\stackrel{-}{\neg}} A_{1,2} \par \ldots \par B_{1,1} \par \ldots) \land \ldots

  \Sigma [(\#t_1,[A_{1,1},A_{1,2},\ldots] \multimap [B_{1,1},B_{1,2},\ldots]),\ldots] \equiv (A_{1,1} \otimes A_{1,2} \otimes \ldots \otimes \smash{\stackrel{+}{\neg}} B_{1,1} \otimes \ldots) \lor \ldots

With the encoding, we lose the free-form tags and have to use strings like "RRRRRL". This leads to unbalanced proof trees and a general lack of expressiveness of the proof language.

Lambdas are in :cite:`maraistCallbynameCallbyvalueCallbyneed1995`: Call by name lambdas are :math:`\Omega_N = \bang \Omega_N \to \Omega_N`, Call by value or optimal lambdas are :math:`\Omega_V = \bang (\Omega_V \to \Omega_V)`. But honestly I'm not sure about the definition, I found another paper that says these all expand to the same thing. So for now the definitions are here rather than in the reference.

Exponentials
============

There are two S4 modalities !/bang/"of course" (positive/affirmative) and the dual ?/whim/whimper/"why not" (negative).

Contraction
-----------

Instead of binary contraction we allow :math:`n`-ary contraction for :math:`n\geq 2`. This is equivalent to binary contraction but makes the proof trees a little more compact.

Subexponentials
---------------

In standard linear logic there are two S4 modalities !/bang/"of course" (positive) and the dual ?/whim/whimper/"why not" (negative). But if we introduce two modalities :math:`\bang_1, \bang_2` with separate rules we cannot prove :math:`\bang_1 A \equiv \bang_2 A`. So in keeping with the maximalist approach we present the logic with subexponentials. The subexponentials are like type annotations, in that we can erase all the subexponentials to a single standard exponential, and we can infer subexponentials, computing the minimal subexponential structure necessary for the program to work. Subexponentials whose only operations are promotion/dereliction can be deleted from the program.

For notation, subexponentials look like :math:`\bang^x_m,\whim^x_m` where :math:`m` is in an index set :math:`M \supseteq \{\cdot\}` and :math:`x \in X, X = P(\{c, w, d\})`. :math:`m=\cdot` is written :math:`\bang^x,\whim^x`, and similarly :math:`x=\{\}` is written as :math:`\bang_m,\whim_m`, so that we recover the standard notation :math:`\bang,\whim` for :math:`m=\cdot,x=\{\}`. We can also write :math:`\bang_{(m,x)},\whim_{(m,x)}`, or more simply :math:`\bang_{m}` if the available operations are clear.

To use these we must define a relation :math:`\leq` on :math:`(M,X)` such that :math:`((M,X),\leq)` is a poset. :math:`\leq` must have that :math:`(m,x) \leq (n,y)` only if :math:`x\subseteq y`. Reflexivity ensures the identity theorem. Transitivity and the subset relation on :math:`X` ensure cut elimination. Antisymmetry ensures that if :math:`\bang^x_m A \equiv \bang^y_n A` then :math:`m=n` and :math:`x=y`, so that we do not have duplicate notation for a particular modality. We require :math:`(m,x) \leq (m,y)` for :math:`x \subseteq y`, but the relation between different modalities may not be so simple.

The rule for promotion requires that :math:`(z,o)\leq (x_i,m_i)` and :math:`(z,o)\leq (y_i,n_i)` for the elements of the context.

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } \vdash A, \overrightarrow{\whim^{y_i}_{n_i}\Delta_i} }{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } \vdash \bang^z_o A, \overrightarrow{\whim^{y_i}_{n_i}\Delta_i}}{\bang}
    &
    \rule{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } , A\vdash \overrightarrow{\whim^{y_i}_{n_i}\Delta_i} }{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i }, \whim^z_o A \vdash \overrightarrow{\whim^{y_i}_{n_i}\Delta_i}}{\whim}

  \end{array}

Dereliction requires :math:`d \in x`.

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\sk{\Gamma}, A \vdash \sk{\Delta} }{\sk{\Gamma}, \bang^x_m A \vdash \sk{\Delta}}{\bang d}
  & \rule{\sk{\Gamma} \vdash A, \sk{\Delta} }{\sk{\Gamma} \vdash \whim^x_m A, \sk{\Delta}}{\whim d}
  \end{array}

Weakening requires :math:`w \in x`.

.. math::
  :nowrap:

  \begin{array}{cc}
      \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang^x_m A \vdash \sk{\Delta}}{\bang w}
    & \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma} \vdash \whim^x_m A, \sk{\Delta}}{\whim w}
    \end{array}

Contraction requires :math:`c \in x`

.. math::
  :nowrap:

  \begin{array}{cc}
      \rule{\sk{\Gamma}, \overrightarrow{\bang^x_m A, \bang^x_m A, \cdots} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang^x_m A \vdash \sk{\Delta}}{\bang c_n}
    & \rule{\sk{\Gamma} \vdash \overrightarrow{\whim^x_m A, \whim^x_m A, \cdots}, \sk{\Delta} }{\sk{\Gamma} \vdash \whim^x_m A, \sk{\Delta}}{\whim c_n}
  \end{array}

We also allow quantification over subexponentials, as in :cite:`nigamAlgorithmicSpecificationsLinear2009`.

Modalities
----------

Because of the equivalences :math:`\bang \bang A \equiv \bang A, \bang \whim \bang \whim A \equiv \bang \whim A`, there are only 7 modalities created from combining exponentials. They have the relationships as follows, where an arrow :math:`A \to B` means :math:`\vdash A \to B` is provable:  :cite:`coniglioEqualityLinearLogic2002`

.. graphviz::

  digraph G {
    rankdir=LR
    "!A" -> "A"
    "A" ->"?A"
    "!A" -> "!?!A" -> {"!?A","?!A"} -> "?!?A" -> "?A"
    subgraph C {
      rank=same
      "A","!?A","?!A"
    }
  }

More generally with subexponentials:

* For :math:`(x,m)\geq(y,n)`, :math:`\bang^x_m \bang^y_n A \equiv \bang^x_m A \equiv \bang^y_n \bang^x_m A`, and similarly for :math:`\whim`.
* For :math:`(x,n)\leq(z,p)` and :math:`(y,o)\leq(w,m)` we can prove :math:`\bang^w_m \whim^x_n \bang^y_o \whim^z_p A \equiv \bang^w_m \whim^z_p A`.

With subexponentials the possible combinations become infinite, for example alternating patterns like :math:`\bang_1 \bang_2 \bang_1 \bang_2` cannot be simplified unless there is a relation in the poset between 1 and 2.

Polarized logic
===============

Following :cite:`lafontLinearLogicPages` we say a proposition :math:`A` is positive if :math:`A \leftrightarrow \bang A` and negative if :math:`A \leftrightarrow \whim A`, and that is is polarized if it is either positive or negative. We can forget negative polarity by forming :math:`A\otimes 1`, and positive polarity by :math:`A \par \bot`, and ensure a proposition has no polarity by using both.

:math:`\Sigma` forms positive propositions and :math:`\Pi` forms negative propositions, where in each case, the clauses :math:`\vec A - \vec B` or :math:`\vec A \multimap \vec B` are formed from positive subformulas :math:`A_j` and negative subformulas :math:`B_k`. Formulas :math:`\bang A` are always positive while formulas :math:`\whim A` are always negative.

:math:`BB = \text{Bool} \to \text{Bool}` is positive and doesn't conform to the above rules.

The polarized negations and shifts show up in polarized / focused linear logic. We use the convention that an up shift raises a negative to positive (increases the value, hence points up). This is the original definition of `Girard's <https://www.seas.upenn.edu/~sweirich/types/archive/1991/msg00123.html>`__, ``P = up N``. More recent papers such as :cite:`girardLocusSolumRules2001` and :cite:`zeilbergerLogicalBasisEvaluation2009` seem to have lost this convention and use definitions of up-shift and down-shift reversed from ours. :cite:`nigamAlgorithmicSpecificationsLinear2009` uses an alternate notation of delay operators :math:`\delta^\pm(\cdot)` instead of shifts.

Cartesian types
---------------

Certain "cartesian" types, like booleans, integers, lists, and in general ADTs of cartesian types using :math:`\Sigma`, have a "natural" proof of positivity that preserves the value. This is an extension of :cite:`filinskiLinearContinuations1992`'s observation in section 3.1 - we destruct the value, then use bang, then construct the same value. But :math:`BB` from above can't be cartesian because we cannot evaluate a function twice.

There's similarly negative types with a "natural" proof using :math:`\Pi`, e.g. for :math:`D=\Pi[(#l,[]\multimap []),(#r,[]\multimap [])]`. We could call these co-cartesian types.

The conclusion is to be generous with exponentials and use them whenever you have a cartesian / co-cartesian type, so that the proof structure identifies those operations.

Tangent: Reversible computing
-----------------------------

Another approach to duplication is "superstructural reversible logic" :cite:`sparksSuperstructuralReversibleLogic2014`. In their calculus, duplicating any type (cartesian or not) is impossible, without using non-reversible rules. The non-reversible structural rules could be restricted to a modality similarly to how linear logic restricts contraction/weakening. But in Theseus :cite:`jamesTheseusHighLevel2014` and even in the latest publication :cite:`chenComputationalInterpretationCompact2021` they use standard higher-order functions to do most of the programming. So really a reversible program is a data type ``Rev``, similar to how a normal program is a data type ``Task``. The difference is that ``Rev`` contains isomorphisms rather than continuations.

In terms of support, I don't think there's much needed at present. With the DSL support Stroscot should be able to write isomorphisms similar to Theseus, and run them forwards/backwards like the `Agda code <https://github.com/DreamLinuxer/popl21-artifact>`__. Once there is hardware/an ISA/an OS to target it should be straightforward to extend this basic support to a compiler.

Structural rules
================

As is usual for linear logic there are no structural rules for weakening or contraction (they are restricted to the exponentials above). And in Core we use a graph representation that internalizes the exchange rule, so there is not really an exchange rule either.

Restricting the exchange rule would result in an ordered type system / noncommutative logic, similar to a stack machine. But :cite:`shiVirtualMachineShowdown2005` shows that a register model is much better for an implementation - the extra stack swapping instructions give no benefit. Similarly restricting associativity would turn sequent lists into a binary tree - but this also has no benefit, it would just be a lot of shuffling operations. The number of operators would explode because every tree structure / stack index would create a new operator. Overall messing with the exchange rule seems like a nothing burger - some theoretical papers, but no real meat.

The cut rule is technically a theorem; we can prove that any proof using cut can be reformulated to be cut-free. But the expansion may result in exponentially more rule applications.

Similarly the identity rule is a theorem for propositional logic: we can produce a proof tree for :math:`A \vdash A` for any finite proposition :math:`A` via expansion of all the cases. Using the identity rule speeds up reduction because it skips iterating through the structure, and it also allows manipulating (prefixes of) :ref:`infinite <infinite>` trees.

Quantifiers
===========

To move from propositional to first-order logic we must extend the identity rule to include axioms for terms. Some presentations therefore call the identity rule "ax", for axiom, but in general the identity rule is a theorem so this is foolish IMO.

`nLab <https://ncatlab.org/nlab/show/sequent+calculus>`__ defines a substitution rule/theorem. There is a theorem that substitution rules can be eliminated from the proof tree, proven by taking the proof tree for :math:`\Gamma \vdash \Delta` and replacing all its identities :math:`x \vdash x` with identities :math:`t\ vdash t`.

Unlike with sets, quantifiers have no problem with identity expansion because the substitution is always for a variable and hence the number of quantifiers decreases.

Logic translations
==================

First we must define classical and intuitionistic logic. To define classical logic we simply add standard structural weakening and contraction rules to our linear logic. Then :math:`A\otimes B \equiv A \land B`, :math:`A\par B \equiv A \lor B`, and we obtain the usual classical logic with modalities :cite:`lafontLinearLogicPages`; all the connectives decompose into or are equivalent to the standard ones. To define intuitionistic logic we take classical logic and restrict the right hand side of all sequents to have at most one consequent; various pi/sigma connectives cannot be used as they would create multiple consequents, and similarly right contraction cannot be used. We allow disallow right weakening to make the translation easier.

The translation from intuitionistic logic to linear logic decorates every proposition and subproposition with !. :cite:`dicosmoIntroductionLinearLogic2015`

.. math::

  \left[\prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right)\right]_I &= \prod \limits_{i} \left(\overrightarrow{\bang\left[A_i\right]_I} \multimap \overrightarrow{\bang\left[B_i\right]_I}\right)

  \left[\sum \limits_{i} \left(\overrightarrow{A_i} - \overrightarrow{B_i}\right)\right]_I &= \sum \limits_{i} \left(\overrightarrow{\bang\left[A_i\right]_I} - \overrightarrow{\bang\left[B_i\right]_I}\right)

We can translate classical logic into intuitionistic logic by decorating every proposition and subproposition with :math:`\neg\neg` and moving the right to the left with another negation, i.e. :math:`\Gamma \vdash \Delta \Rightarrow \Gamma', \neg \Delta' \vdash`. Thus the translation of classical logic into linear logic decorates like :math:`\neg \bang (\neg \bang A) \equiv \whim \bang A`.

These two decoration translations preserve proof structure, in the sense that every intuitionistic/classical proof tree can be converted to a linear logic proof tree, and the reverse as well if the linear logic proof tree's sequent is the result of the proposition translation.

Patterns
========

We call sequents of the form :math:`\vdash A` proofs of :math:`A`. Similarly sequents :math:`A \vdash` are refutations of :math:`A`. :math:`\Sigma_R` constructs a proof from a collection of proofs and refutations, while :math:`\Pi_L` constructs a refutation from a collection of proofs and refutations. We can similarly consider proof patterns :math:`x, \ldots, z \vdash A` / refutation patterns :math:`x,\ldots,z, A \vdash` where :math:`x,\ldots,z` are free variables.

If we have a proof of :math:`A` then :math:`A` is a theorem (also called a tautology). If we prove a sequent :math:`\Gamma \vdash` then :math:`\Gamma` is a contradiction. We define equivalence :math:`A\equiv B` as the theorem :math:`\vdash A \leftrightarrow B`.

Definitions
===========

I didn't find any relevant papers on defining new notation for expressions in the sequent calculus. So we have to prove consistency ourselves. But I think the cut elimination theorem poses no problem, the key and commutative cases are trivial.

The identity theorem fails to complete if there is an infinite chain of definitions :math:`A_1 \defeq \ldots A_2 \ldots, A_2 \defeq \ldots A_3 \ldots, \ldots`. Hence we exclude that from the syntax, by requiring the identity theorem to complete for all propositions (i.e. the proposition has a "non-circular definition"). All non-definition identity steps decrease the size of the formula so it is only definitions that can make a formula circular. Technically there are more complex behaviors ruled out than the simple infinite definition expansion "circular" implies, but I figure the term is good enough.

For the substitution theorem we must also limit our substitution to non-circular definitions. The proof works by replacing variable identities :math:`x \vdash x` with more complex identities :math:`A \vdash A`, and works fine so long as the proposition is non-circular.

Non-circularity is a pretty loose restriction. If we know a definition is size-decreasing, we can induct as usual to prove the identity theorem: use the basic identity theorem on non-definition subtrees, use the definition rule on both sides for each definition, and continue switching between the two until it's built up.

Hence we only have to be careful for definitions like sets that can increase size when expanded. In general it is undecidable if a particular proposition is circular (see :ref:`discussion of set paradoxes <paradoxes>`). But most definitions don't have a definition on the RHS hence are easy to check for circularity.

Set theory
==========

The naive set theory definition comes from :cite:`shirahataLinearSetTheory1994` (page 10). The rest of the definitions are similar to the ones in :cite:`shulmanLinearLogicConstructive2018` except :math:`\in` is not affirmative.

.. _paradoxes:

Paradoxes
---------

It seems from playing with some examples that forbidding circular definitions is sufficient to prevent Russell's paradox and Curry's paradox. For example with :math:`R = \{x\mid \whim \neg(x \in x)\}`, :math:`\{x\mid F \} \in R` is defined (and provable) but :math:`R \in R` is circular hence not defined. So we cannot write the premise of Russell's paradox. We could try to work around this with a proposition like :math:`t\in R \land t= R`. This is not circular, but it is not sufficient to derive a paradox, as in order to apply contraction we have to use a substitution that produces :math:`R \in R`. Curry's paradox :math:`X=\{x\mid x\in x \to Y\}` similarly contains :math:`x\in x` and hence the notation :math:`X \in X` is circular and therefore forbidden as well.

More formally, suppose the logic is inconsistent, i.e. there is a derivation :math:`\vdash \bot`. This must be derived from the empty sequent. Then what does cut elimination do? Either it completes, in which case we get a contradiction because no rule derives the empty sequent, or there's some infinite chain of cut elimination. I claim the infinite chain only happens if there is a circular definition involved and the identity or substitution rules are on the sides. Hence, forbidding circular definitions in the identity and cut rules solves the issue.

The question of whether a given set comprehension is defined is undecidable, as we can encode the lambda calculus and hence the halting problem - the beta rule :math:`(\lambda x. A) t` does the same substitution as :math:`t\in\{x\mid A\}`. We can approximate definedness with a termination checking algorithm, type system, or syntactic check:

* Strict comprehension, i.e. the bound variable can only appear once in the formula :cite:`shirahataLinearSetTheory1998`
* New Foundations's stratified formulas :cite:`forsterQuineNewFoundations2019` :cite:`holmesElementarySetTheory1998`
* Hindley-Milner type inference (since the simply typed lambda calculus terminates)
* A size-checking algorithm like in :cite:`jonesCallbyvalueTerminationUntyped2008`
* Brute-force expansion

There is also :cite:`shirahataLinearConservativeExtension1996` which allows sets built from ZF's axioms.

Equality
--------

The axioms of reflexivity, substitution, etc. can take a variety of modalities as in :cite:`coniglioEqualityLinearLogic2002`, some of them corresponding with intuitionistic and classical notions of equality. For sets we use linear weak extensional equality. Alternatively we could use intuitionistic equality :math:`A\overset{!}{=}B \defeq !(A=B)`, then substitution is :math:`A\overset{!}{=}B, \phi \vdash \phi[A/B]`. But the linear equality seems more useful.

Proof of the substitution property: For :math:`\Pi` we use the right rule to split into cases for each tag, then we use contraction/weakening on :math:`\bang(A=B)` to match the number of A's/B's in the case, then the left rule to split into each A and B, giving each branch a copy of the hypothesis. :math:`\Sigma` is similar but with the left first. For exponentials, quantifiers, and set comprehension we simply do left/right in the correct order. Then at the end we use the hypothesis to change :math:`A[x/a]` on the left or right to :math:`B[x/b]`, or else weakening to remove the hypothesis followed by the identity.

Infinite structures
===================

These are used to support infinite types like the lambda calculus or lists, and similarly infinite expressions like ``x = 1 : x``. We construct "infinite" as a terminal coalgebra - our proof trees turn into fixed points of systems of formal equations :cite:`karazerisFinalCoalgebrasAccessible2011`.

Infinite structures can be paradoxical, e.g. we can prove :math:`\vdash\bot` using cut on the proposition :math:`A=\neg A`. Cut elimination will often fail to complete, but there is a progress property in the sense that the cut can always be pushed down and eliminate an identity rule or two matching logical rules.

Hashing
=======

To hash the graphs we can use the tree structure of the sequent derivations. Each upward slot in a node is hashed with a fixed value and each downward slot is hashed with a value corresponding to the path through the derivation tree followed by the label of the upward slot. It is written as a single DFS traversal with the leaves as base case that stores the hashed subtree and a map from edge name to partial path.

Hashing infinite graphs is harder, we have to hash each SCC as a unit. See :cite:`mauborgneIncrementalUniqueRepresentation2000`.

Recursion
=========

Sequent Core :cite:`downenSequentCalculusCompiler2016` also introduces two more rules "multicut" and "rec" that are illogical but computationally useful:

.. math::

    \begin{array}{cc}
      \rule
        {\Gamma, \Theta \vdash \Delta, \Lambda \quad \Gamma', \Lambda \vdash \Theta, \Delta' }
        {\Gamma, \Gamma' \vdash \Delta, \Delta' }{\text{multicut}}
      &
      \rule
        { \overrightarrow{\Gamma, \vec \Lambda, \Theta_i \vdash \Lambda_i, \vec \Theta, \Delta }}
        {\Gamma, \overrightarrow{\Theta_i} \vdash \overrightarrow{\Lambda_i}, \Delta }{\text{rec}}
    \end{array}

These probably aren't needed, the use-def and infinite structures and types encode recursion better and we can use GHC's graph reduction model (below).

Alternately let can be encoded as a record and recursion via a fixed-point combinator or a cycle in the graph. In particular :cite:`kiselyovManyFacesFixedpoint2013` outline a polyvariadic combinator:

::

  fix_poly fl = fix (\self -> map ($ self) fl)

To implement ``fix`` we can use the variant of the Y combinator :math:`\lambda f.(\lambda x.x x) (\lambda x.f (x x))`. To type it we need the cyclic/recursive type :math:`Wr = \Pi[(^w, Wr, r)]` (in the sense of an infinite, regular tree).

BOHM uses a fan/duplication node combined with a loop.

Graph reduction
---------------

Following :cite:`jonesImplementationFunctionalProgramming1987` chapter 12 we give each definition node a static integer. Then the root is a distinguished definition. Assuming the static data is stored on disk and paged in/out as needed, we can minimize runtime memory use in a compiler pass by introducing as many use-def indirections as possible, one for every sequent in the derivation. This also makes the connections between rules uniform. But having lots of indirections is inefficient so a later pass would remove indirections that will be immediately used (chunkification).

The optimal fixedpoint algorithm outlined in :cite:`shamirFixedpointsRecursiveDefinitions1976` (10.18, PDF pages 240-242) is a variation of Tarjan's strongly connected component algorithm. Cuts between two definitions ``f x`` are memoized in a list, and if the SCC algorithm finds a component ``f x -> let g = ... in g (f x)`` then this component is solved. If it has a unique solution then that's the answer, otherwise ``f x`` diverges and is replaced with a ``RecursionError`` or ``AmbiguousError``. We assume the solver allows uninterpreted "holes", so that the SCC can be solved before its sub-computations.

For comparison, to compute the least fixed point we would maintain a "working graph" and incrementally unfold the definition when encountered. But with the optimal fixed point we first reduce the definition to a value while copying other definitions in.

The solver is an SMT solver on the predicate ``SAT(y == g y)``, and for uniqueness ``UNSAT(y == g y && y != y0)`` where ``y0`` is the first solution found. We exclude error values as possible solutions since the recursion error will be more informative.

The posets the paper uses appear to be pointed directed-complete partial orders `(cppo's) <https://en.wikipedia.org/wiki/Complete_partial_order>`__.

Primitives
==========

Primitives (integers) can be handled by hacking special cases into Cut; we add primitive functions of type PiR that use the arguments provided by PiL during a cut, and also literals, special values of type SigmaR. Alternately we can use compressed graphs.

Compressed graphs
-----------------

64-bit integers are represented as a sigma type with 2^64 possibilities. So addition is represented as a case expression, where each case contains another case expression, and then each case constructs the integer corresponding to the addition. There is a lot of fan-out at each step, which would require 2^128 values to represent, clearly infeasible. So although this is the conceptual representation, the actual representation has no fan-out for the cases - instead the case nodes create symbolic variables ``a`` and ``b``, and the constructed value has the tag ``a+b``.

Confluent reduction
===================

Reduction of our linear logic trees is not confluent, but only because of commuting cuts. If we drop the black edges and only consider proof nets, then the system is confluent. A cut only interacts with other cuts at identity rules, but with a cut-identity-cut pattern it doesn't matter which cut reduces with the identity. (TODO: prove this formally)

Since reduction is confluent, it does not change anything to reduce in non-normal order for a time. The reduction will still terminate when going back to normal order. So terminating reductions can always be performed and even non-terminating reductions can be reduced somewhat. Hence during compilation we want to reduce the program as much as possible - ideally the compiled core should be cut-free. We can detect diverging terms and replace them with error terms. But we can't eliminate cuts involving complex recursion, so have to create a heap or a stack allocation. For example the Fibonacci list ``let fibs = 0 :: 1 :: zipWith (+) fibs (tail fibs) in { repeat forever { n <- readInt; print (fibs !! n) } }``, this needs some kind of reduction graph or memo stack involved.

Levels
======

For the implementation of optimal reduction we can add level indices to the terms in the promotion and dereliction rules of :math:`\bangc/\whimc`, as in :cite:`martiniFineStructureExponential1995` and :cite:`guerriniTheoreticalPracticalIssues1996`. Conceptually all terms have indices, but we can recover the indices in a proof tree by propagating the indices from the promotion/dereliction rules up/down according to the criteria that the indices involved in all non-:math:`\bangc/\whimc` promotion/dereliction rules must be the same.

To handle level indices in infinite trees, we store the difference function ``\a -> a + (j-i)`` and recover the levels by tracing from the root of the derivation tree (which is always level 0) and applying the difference function when encountered.

The level of a context is the maximum of the levels of its terms, 0 if it is empty.

.. math::

    \begin{array}{ccc}
      \rule{\bangc\Gamma^i \vdash A^j, \whimc\Delta^i }{\bangc\Gamma^i \vdash \bangc A^i, \whimc\Delta^i}{\bangc}_{j = i+1}
      & \rule{\sk{\Gamma^i}, A^i \vdash \sk{\Delta^i} }{\sk{\Gamma^i}, \bangc A^j \vdash \sk{\Delta^i}}{\bangc d}_{j\leq i}
      & \rule{\sk{\Gamma}, \overrightarrow{\bangc A, \bangc A, \cdots} \vdash \sk{\Delta} }{\sk{\Gamma}, \bangc A \vdash \sk{\Delta}}{\bangc c_n}
    \end{array}

.. math::

    \begin{array}{ccc}
      \rule{\bangc\Gamma^i, A^j \vdash \whimc\Delta^i }{\bangc\Gamma^i, \whimc A^i \vdash \whimc\Delta^i}{\whimc}_{j = i+1}
      & \rule{\sk{\Gamma^i} \vdash A^i, \sk{\Delta^i} }{\sk{\Gamma^i} \vdash \whimc A^j, \sk{\Delta^i}}{\whimc d}_{j \leq i}
      & \rule{\sk{\Gamma} \vdash \overrightarrow{\whimc A, \whimc A, \cdots}, \sk{\Delta} }{\sk{\Gamma} \vdash \whimc A, \sk{\Delta}}{\whimc c_n}
    \end{array}


To handle level mismatches we might also need lifting operators. The conditions are unclear.

.. math::

    \begin{array}{cc}
      \rule{\Gamma^i \vdash A^j, \Delta^i }{\Gamma^i \vdash A^i, \Delta^i}{\text{lift}_R}_{j > i}
      &
      \rule{\Gamma^i, A^j \vdash \Delta^i }{\Gamma^i, A^i \vdash \Delta^i}{\text{lift}_L}_{j > i}
    \end{array}

In practice I went with a different approach that generates matching ``Dup`` nodes, so the levels aren't needed.