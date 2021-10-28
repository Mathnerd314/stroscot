Logic
#####


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

Exponentials
============

There are two S4 modalities !/bang/"of course" (positive) and the dual ?/whim/whimper/"why not" (negative).

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

The polarized negations and shifts show up in polarized linear logic. :cite:`zeilbergerLogicalBasisEvaluation2009` We use the opposite direction for shifts from :cite:`zeilbergerLogicalBasisEvaluation2009` with the mnemonic that an up shift converts from negative to positive, hence increases the value. :cite:`nigamAlgorithmicSpecificationsLinear2009` uses delay operators :math:`\delta^\pm(\cdot)` instead of shifts.

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

The identity theorem fails to complete if there is an infinite chain of definitions :math:`A_1 \defeq \ldots A_2 \ldots, A_2 \defeq \ldots A_3 \ldots, \ldots`. Hence we exclude that from the syntax. Then we can induct as usual: use the normal identity theorem on non-definition subtrees, then use the definition rule on both sides for each definition, and continue alternating until it's built up. For the substitution theorem we must also limit our substitution to non-circular definitions.

For the parts of the proof tree not using the identity or substitution theorems the definition chain is always finite, hence it's not a huge restriction. In general it is undecidable if a particular definition usage is circular (see :ref:`discussion of set paradoxes <paradoxes>`). But most definitions don't have a definition on the RHS hence are easy to check for circularity.

Set theory
==========

The naive set theory definition comes from :cite:`shirahataLinearSetTheory1994` (page 10). The rest of the definitions are similar to the ones in :cite:`shulmanLinearLogicConstructive2018` except :math:`\in` is not affirmative.

.. _paradoxes:

Paradoxes
---------

It seems from playing with some examples that forbidding circular definitions is sufficient to prevent Russell's paradox and Curry's paradox. For example with :math:`R = \{x\mid \whim \neg(x \in x)\}`, :math:`\{x\mid F \} \in R` is defined (and provable) but :math:`R \in R` is circular hence not defined. So we cannot write the premise of Russell's paradox. We could try to work around this with a proposition like :math:`t\in R \land t= R`. This is not circular, but it is not sufficient to derive a paradox, as in order to apply contraction we have to use a substitution that produces :math:`R \in R`. Curry's paradox :math:`X=\{x\mid x\in x \to Y\}` similarly contains :math:`x\in x` and hence the notation :math:`X \in X` is circular and therefore forbidden as well.

More formally, suppose the logic is inconsistent, i.e. there is a derivation :math:`\vdash \bot`. This must be derived from the empty sequent. Then what does cut elimination do? Either it completes, in which case we get a contradiction because no rule derives the empty sequent, or there's some infinite chain of cut elimination. I claim the infinite chain only happens if there is a circular definition involved and the identity or substitution rules are on the sides. Hence, forbidding infinitely expanding definitions in these solves the issue.

The question of whether a given set comprehension is defined is undecidable, as we can encode the lambda calculus and hence the halting problem - the beta rule :math:`(\lambda x. A) t` does the same substitution as :math:`t\in\{x\mid A\}`. We can approximate definedness with a termination checking algorithm or syntactic check:

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

To hash the graphs we can use the tree structure of the sequent derivations. Each upward slot in a node is hashed with a fixed value and each downward slot is hashed with a value corresponding to the path through the derivation tree followed by the label of the upward slot. It is written as a single DFS traversal from the leaves upwards that stores the hashed subtree and a map from edge name to partial path.

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

The lambdas are from :cite:`maraistCallbynameCallbyvalueCallbyneed1995`, but honestly I'm not sure about the definition.
