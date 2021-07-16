Logic
#####

The logic for Stroscot is based on a two-sided linear logic sequent calculus.

Jumbo connectives
=================

Based on :cite:`levyJumboLcalculus2006`, we aim for the largest allowable set of operators. In particular we generalize into two jumbo operators, :math:`\Sigma` (sigma) and :math:`\Pi` (pi).

We start with the generalized :math:`\Pi` rule. This is similar to Levy's rule except it allows multiple conclusion propositions. We have indexed variables :math:`A_{ij}` and :math:`B_{ik}` where :math:`0 \leq i < N, 0 \leq j < m_i, 0 \leq k < n_i`. We call :math:`N` the length of the jumbo type and the list :math:`[(m_i,n_i)]` the jumbo-arity.

.. math::
    :nowrap:

    \begin{array}{cc}
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
    \rule{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta }}
      {\Gamma \vdash \prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right), \Delta }
      {\Pi_R}
    &
    \rule{\overrightarrow{ \sk{\Gamma_j} \vdash A_{i j}, \sk{\Delta_j} } \quad \overrightarrow{ \sk{\Theta_k}, B_{i k} \vdash \sk{\Lambda_k} }}
      {\sk{\overrightarrow{\Gamma}}, \sk{\vec \Theta}, \prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right) \vdash \sk{\overrightarrow{\Delta}}, \sk{\vec\Lambda}}
      {\Pi_{i} {}_{L}}
    \end{array}

Next we have the generalized :math:`\Sigma` rule. This is the dual of :math:`\Pi`. Following :cite:`wadlerCallbyvalueDualCallbyname2003` :cite:`crolardFormulaeastypesInterpretationSubtractive2004` the dual of implication is called "subtraction" or "difference" and is denoted :math:`-`. For normal ADTs, the RHS of the difference is empty, i.e. it looks like :math:`\Sigma (a. A - \cdot \mid b. B_1,B_2 - \cdot \mid c. \cdot - \cdot)`. The syntax :math:`\Sigma [(a, [A], []),(b, [B_1, B_2], []), (c,[],[])]` might be clearer.

.. math::
    :nowrap:

    \begin{array}{cc}
    \rule{\overrightarrow{ \sk{\Gamma_k}, B_{i k} \vdash \sk{\Delta_k} } \quad \overrightarrow{ \sk{\Theta_j} \vdash A_{i j}, \sk{\Lambda_j} } }
      {\sk{\overrightarrow{\Gamma}}, \sk{\overrightarrow{\Theta}} \vdash \sum \limits_{i} \left( \overrightarrow{A_i} - \overrightarrow{B_i} \right), \sk{\overrightarrow{\Delta}}, \sk{\overrightarrow{\Lambda}}}
      {\Sigma_{i} {}_{R}}
    &
    \rule{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta } }
      {\Gamma, \sum \limits_{i} \left ( \overrightarrow{A_i} - \overrightarrow{B_i} \right ) \vdash \Delta }
      {\Sigma_L}
    \end{array}

When the RHS is nonempty we get terms with holes, that can be pattern-matched by filling the holes, e.g. `difference lists <https://en.wikipedia.org/wiki/Difference_list>`__. (TODO: check that this actually gives efficient concatenation)

.. _connectives:

Common connectives
------------------

All of the standard operators 01⊕⊗⊤⊥&⅋⊸⧟ in linear logic can be expressed using :math:`\Sigma` and :math:`\Pi`. We use our notation for them.

.. list-table::
  :header-rows: 1
  :widths: 1,1,2,4
  :width: 100%

  * - Operator
    - Girard Notation
    - Name
    - Type
  * - :math:`F`
    - :math:`0`
    - Zero (False)
    - :math:`\Sigma []`
  * - :math:`1`
    - :math:`1`
    - One
    - :math:`\Sigma [(\#s,[] - [])]`
  * - :math:`A \lor B`
    - :math:`A \oplus B`
    - Plus (coproduct, or)
    - :math:`\Sigma [(\#l,[A] - []),(\#r,[B] - [])]`
  * - :math:`A \otimes B`
    - :math:`A \otimes B`
    - Times (tensor product)
    - :math:`\Sigma [(\#s,[A,B] - [])]`
  * - :math:`A^{\otimes n}`
    -
    - `Tensor power <https://en.wikipedia.org/wiki/Tensor_product#Tensor_powers_and_braiding>`__
    - :math:`\Sigma [(\#s,\overbrace{[A,\ldots,A]}^n - [])]`
  * - :math:`\smash{\stackrel{+}{\neg}} A`
    - :math:`A^{\bot}`
    - Positive Negation
    - :math:`\Sigma [(\#s,[] - [A])]`
  * - :math:`{↑}A`
    - :math:`A`
    - Up shift
    - :math:`\Sigma [(\#s,[A] - [])]`
  * - :math:`T`
    - :math:`\top`
    - Top (True)
    - :math:`\Pi []`
  * - :math:`\bot`
    - :math:`\bot`
    - Bottom (contradiction)
    - :math:`\Pi [(\#s,[] \multimap [])]`
  * - :math:`A \land B`
    - :math:`A \with B`
    - With (product, and)
    - :math:`\Pi [(\#l,[] \multimap [A]),(\#r,[] \multimap [B])]`
  * - :math:`A \par B`
    - :math:`A \par B`
    - Par ("unless", classical or, parallel product, dual of tensor)
    - :math:`\Pi [(\#s,[] \multimap [A,B])]`
  * - :math:`A^{\par n}`
    -
    - Par power
    - :math:`\Pi [(\#s,[] \multimap \overbrace{[A,\ldots,A]}^n)]`
  * - :math:`A \to B`
    - :math:`A \multimap B`
    - Lollipop (implication, internal hom)
    - :math:`\Pi [(\#f,[A] \multimap [B])]`
  * - :math:`A \leftrightarrow B`
    - :math:`A \multimapboth B`
    - Equivalence
    - :math:`\Pi [(\#l,[A] \multimap [B]),(\#r,[B] \multimap [A])]`
  * - :math:`\smash{\stackrel{-}{\neg}} A`
    - :math:`A^{\bot}`
    - Negative Negation
    - :math:`\Pi [(\#s,[A] \multimap [])]`
  * - :math:`{↓}A`
    - :math:`A`
    - Down shift
    - :math:`\Pi [(\#s,[] \multimap [A])]`

The idea behind :math:`\bot` as contradiction is as follows: if we have a sequent :math:`\Gamma \vdash A\otimes \neg A`, we can decompose this into sequents :math:`\Gamma_1 \vdash A` and :math:`\Gamma_2, A \vdash` where :math:`\Gamma = \Gamma_1, \Gamma_2`. Then we can cut to derive the sequent :math:`\Gamma \vdash`, and hence derive :math:`\Gamma \vdash \bot`.

The notation :math:`\land,\lor` is chosen because the structure-preserving translation from intuitionistic logic preserves the logical operators :cite:`dicosmoIntroductionLinearLogic2015`, hence some intuition arises from using it. The notation for times and par is trickier; times and par are both `tensor/monoidal products <https://en.wikipedia.org/wiki/Monoidal_category>`__ (identities :math:`1,\bot`), and do not appear in classical logic. The fact that tuples are typically positive data leads us to privilege :math:`\otimes` as the default product, agreeing with Girard.

:math:`\par` is the dual of :math:`\otimes` in the sense that :math:`A \par B \equiv \neg (\neg A \otimes \neg B)`; unfortunately for deciding a notation, this seems to be its only useful property. :math:`\oplus, \odot ,\Box,\sharp, \bullet` and :math:`*` have meanings (direct sum/coproduct, Hadamard product/XNOR gate/symmetric product, modal operator, music, multiplication/logical and, convolution) dissimilar from the function of :math:`\par`. :math:`\mathbin{{\scriptstyle+}\mkern-0.522em\raise{-0.077em}{\diamond}},\mathbin{{\vee}\mkern-0.815em\raise{0.09em}{\bigcirc}}` don't have Unicode symbols so are hard to use. In the end none of the operators seems particularly evocative. :math:`\par` on the other hand redirects to linear logic on Wikipedia. It can be said to be Girard's contribution.

Programming types
-----------------

We can also write some types common from programming:

.. list-table::
   :header-rows: 1
   :widths: auto

   * - Operator
     - Name
     - Type
   * - :math:`\text{Bool}`
     - Booleans
     - :math:`\Sigma [(\#F,[]-[]),(\#T,[]-[])]`
   * - :math:`\text{Int}`
     - Integers
     - :math:`\Sigma [(\#{-2}^{31},[]-[]),\ldots,(\#0,[]-[]),\ldots,(\#2^{31}-1,[]-[])]`
   * - :math:`L_A`
     - Linked list of A
     - :math:`\Sigma\{(\text{#nil},[]-[]),(\text{#cons},[A,L_A]-[])`

With these we see the justification for the jumbo types: they can represent abstract data types (ADTs). Even though we can encode :math:`\Pi,\Sigma` using the common connectives:

.. math::

  \Pi [(\#t_1,[A_{1,1},A_{1,2},\ldots] \multimap [B_{1,1},B_{1,2},\ldots]),\ldots] \equiv (\smash{\stackrel{-}{\neg}} A_{1,1} \par \smash{\stackrel{-}{\neg}} A_{1,2} \par \ldots \par B_{1,1} \par \ldots) \land \ldots

  \Sigma [(\#t_1,[A_{1,1},A_{1,2},\ldots] \multimap [B_{1,1},B_{1,2},\ldots]),\ldots] \equiv (A_{1,1} \otimes A_{1,2} \otimes \ldots \otimes \smash{\stackrel{+}{\neg}} B_{1,1} \otimes \ldots) \lor \ldots

With the encoding, we lose the free-form tags and have to use strings like "RRRRRL". This leads to unbalanced proof trees and a general lack of expressiveness of the proof language.

Polarized logic
---------------

The polarized negations and shifts show up in polarized linear logic. :cite:`zeilbergerLogicalBasisEvaluation2009` In particular :math:`\Sigma` is positive while :math:`\Pi` is negative. We use the opposite direction for shifts from :cite:`zeilbergerLogicalBasisEvaluation2009` with the mnemonic that an up shift converts from negative to positive, hence increases the value. For most purposes the polarity does not matter (they have identical derivation rules) so we write :math:`\neg A` and :math:`\smash{\updownarrow}A`. :cite:`nigamAlgorithmicSpecificationsLinear2009` uses delay operators :math:`\delta^\pm(\cdot)` instead of shifts.

The jumbo connectives have the nice property that any combination of purely-positive or purely-negative connectives is equivalent to a single jumbo connective, "unpacking" the formulas.

Exponentials
============

Normally there are two S4 modalities !/bang/"of course" (positive) and the dual ?/whim/whimper/"why not" (negative). But if we introduce two modalities :math:`\bang_1, \bang_2` with separate rules we cannot prove :math:`\bang_1 A \equiv \bang_2 A`. So in keeping with the maximalist approach we use subexponentials :math:`\bang^x_m,\whim^x_m` where :math:`m` is in an index set :math:`M \supseteq \{\cdot\}` and :math:`x \in X, X = \{\cdot, c, w, \emptyset\}`. :math:`m=\cdot` is written :math:`\bang^x,\whim^x`, and similarly :math:`x=\cdot` is written as :math:`\bang_m,\whim_m`, so that we recover the usual notation :math:`\bang,\whim` for :math:`m=x=\cdot`. We can also write :math:`\bang_{(m,x)},\whim_{(m,x)}` or more simply :math:`\bang_{m}` if the context is clear.

To use these we must define a relation :math:`\leq` on :math:`(M,X)` such that :math:`((M,X),\leq)` is a poset and :math:`(m,x) \leq (n,y)` implies :math:`x\leq y`, where the relation on :math:`X` is :math:`\emptyset \leq \{c, w\} \leq \cdot`. Reflexivity ensures the identity theorem. Transitivity and the compatibility with the poset :math:`X` ensure cut elimination. Antisymmetry ensures that if :math:`\bang^x_m A \equiv \bang^y_n A` then :math:`m=n` and :math:`x=y`, so that we do not have duplicate notation for a particular modality. For the standard modality :math:`m=\cdot` we require :math:`(\cdot,\emptyset) \leq \{(\cdot,c),(\cdot,w)\} \leq (\cdot,\cdot)`, but for other values of :math:`m` these relations may not hold.

The rule for promotion requires that :math:`(z,o)\leq (x_i,m_i)` and :math:`(z,o)\leq (y_i,n_i)` for the elements of the context.

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } \vdash A, \overrightarrow{\whim^{y_i}_{n_i}\Delta_i} }{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } \vdash \bang^z_o A, \overrightarrow{\whim^{y_i}_{n_i}\Delta_i}}{\bang}
    &
    \rule{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } , A\vdash \overrightarrow{\whim^{y_i}_{n_i}\Delta_i} }{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i }, \whim^z_o A \vdash \overrightarrow{\whim^{y_i}_{n_i}\Delta_i}}{\whim}

  \end{array}

For dereliction there are no restrictions.

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\sk{\Gamma}, A \vdash \sk{\Delta} }{\sk{\Gamma}, \bang^x_m A \vdash \sk{\Delta}}{\bang d}
  & \rule{\sk{\Gamma} \vdash A, \sk{\Delta} }{\sk{\Gamma} \vdash \whim^x_m A, \sk{\Delta}}{\whim d}
  \end{array}

Weakening requires :math:`x \geq w`.

.. math::
  :nowrap:

  \begin{array}{cc}
      \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang^x_m A \vdash \sk{\Delta}}{\bang w}
    & \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma} \vdash \whim^x_m A, \sk{\Delta}}{\whim w}
    \end{array}

Contraction requires :math:`x \geq c`. Instead of binary contraction we allow :math:`n`-ary contraction for :math:`n\geq 2`. This is equivalent to binary contraction but makes the proof trees a little more compact.

.. math::
  :nowrap:

  \begin{array}{cc}
      \rule{\sk{\Gamma}, \overrightarrow{\bang^x_m A, \bang^x_m A, \cdots} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang^x_m A \vdash \sk{\Delta}}{\bang c_n}
    & \rule{\sk{\Gamma} \vdash \overrightarrow{\whim^x_m A, \whim^x_m A, \cdots}, \sk{\Delta} }{\sk{\Gamma} \vdash \whim^x_m A, \sk{\Delta}}{\whim c_n}
  \end{array}

We define call-by-name lambdas as :math:`\Omega = \bang \Omega \multimap \Omega` and call-by-value or optimal lambdas :math:`\Omega = \bang (\Omega \multimap \Omega)`. :cite:`maraistCallbynameCallbyvalueCallbyneed1995`

Equivalences
------------

For :math:`(x,m)\geq(y,n)`, :math:`\bang^x_m \bang^y_n A \equiv \bang^x_m A \equiv \bang^y_n \bang^x_m A`, and similarly for :math:`\whim`.

For :math:`(x,n)\leq(z,p)` and :math:`(y,o)\leq(w,m)` we can prove :math:`\bang^w_m \whim^x_n \bang^y_o \whim^z_p A \equiv \bang^w_m \whim^z_p A`.

So for a single modality there are 7 derived modalities, with the relationships :math:`\bang A \to A \to \whim A`, :math:`\bang A \to \bang \whim \bang A \to \{\bang \whim A, \whim \bang A\} \to \whim \bang \whim A \to \whim A`. :cite:`coniglioEqualityLinearLogic2002` But with multiple modalities the possibilities become infinite, for example elements of the alternating sequence :math:`\bang_1 \bang_2 \bang_1 \ldots`. On the other hand, from an operational perspective, we can always erase the subexponential information and use the normal !/?.

We can prove the equivalences :math:`\whim A \leftrightarrow A \leftrightarrow \bang A` for certain "cartesian" types, like booleans, integers, and ADTs of cartesian types, with a natural proof structure that preserves information. This is an extension of :cite:`filinskiLinearContinuations1992`'s observation in section 3.1 - we destruct the value, then use bang, then construct the same value.

Does this mean there is a deeper structure to proofs? We can also write proofs for non-cartesian types like :math:`\text{Bool} \to \text{Bool}`, they just cannot preserve information (because we would have to use the function twice). I think the conclusion is to be generous with exponentials and use them whenever you want weakening/contraction, so that you don't have to examine the proof structure to identify those operations.

Structural rules
================

Finally we have the structural rules. As is usual for linear logic there are no structural rules for weakening or contraction (they are restricted to the exponentials above).

First is the exchange rule, given for permutations :math:`\sigma_L, \sigma_R`. In practice we use a graph or multiset representation that internalizes the exchange rule. Restricting the exchange rule would result in an ordered type system / noncommutative logic, similar to a stack machine. But :cite:`shiVirtualMachineShowdown2005` shows that a register model is much better for an implementation - the extra stack swapping instructions give no benefit. Similarly restricting associativity would turn sequent lists into a binary tree - but this also has no benefit, it would just be a lot of shuffling operations. The number of operators would explode because every tree structure / stack index would create a new operator. Overall messing with the exchange rule seems like a nothing burger - some theoretical papers, but no real meat.

.. math::
  :nowrap:

  \begin{array}{c}
    \rule{\sk{\Gamma} \vdash \sk{\Delta}}{\sk{\sigma_L(\Gamma)} \vdash \sk{\sigma_R(\Delta)}}{\text{x}}
  \end{array}

The cut rule is technically a theorem; we can prove that any proof using cut can be reformulated to be cut-free. But the expansion may result in exponentially more rule applications.

.. math::
  :nowrap:

    \rule{\sk{\Gamma} \vdash A, \sk{\Delta} \quad \sk{\Theta}, A \vdash \sk{\Lambda} }{\sk{\Gamma}, \sk{\Theta} \vdash \sk{\Delta}, \sk{\Lambda} }{\text{cut}}

Similarly the identity rule is a theorem for the logic we have formulated so far (propositional logic): we can produce a proof tree for :math:`A \vdash A` for any finite proposition :math:`A` via expansion of all the cases. Using the identity rule speeds up reduction because it skips iterating through the structure, and it also allows manipulating (prefixes of) :ref:`infinite <infinite>` trees.

.. math::
  :nowrap:

  \begin{array}{ccc}
    \rule{}{A \vdash A}{\text{id}}
  \end{array}

Quantifiers
===========

To move from propositional to first-order logic we must introduce identity axioms for terms. The set of terms consists of variables, name constants, and uninterpreted predicates :math:`f(t_1,\ldots,t_n)` where :math:`f` is a function symbol and :math:`t_i` are terms.

.. math::
  :nowrap:

  \begin{array}{ccc}
    \rule{}{x \vdash x}{\text{id}}
    &
    \rule{}{P(x_1,\ldots,x_n) \vdash P(x_1,\ldots,x_n)}{\text{id}}
  \end{array}

`nLab <https://ncatlab.org/nlab/show/sequent+calculus>`__ defines a substitution rule/theorem. :math:`A[\overrightarrow{x \mapsto t}]` stands for the proposition :math:`A` where all free occurrences of the variables :math:`\overrightarrow{x}` have been replaced by the propositions/terms :math:`\overrightarrow{t}` (and bound variables have been renamed to fresh ones when necessary). There is a theorem that substitution rules can be eliminated from the proof tree, proven by taking the proof tree for :math:`\Gamma \vdash \Delta` and replacing all its identities :math:`x \vdash x` with identities :math:`t\ vdash t`.

  .. math::
    :nowrap:

    \begin{array}{c}
      \rule{\Gamma \vdash \Delta}{\Gamma[\overrightarrow{x \mapsto t}] \vdash \Delta[\overrightarrow{x \mapsto t}]}{\text{sub}}
    \end{array}

Then we add quantifiers. For these :math:`x` must have no free occurrence in :math:`\Gamma` or :math:`\Delta`. Unlike with sets, there is no problem with identity expansion because the substitution is always for a variable and hence the number of quantifiers decreases.

.. math::
  :nowrap:

   \begin{array}{cccc}
      \rule{\Gamma \vdash A, \Delta}{\Gamma \vdash \forall x. A, \Delta}{\forall_R}
      &
      \rule{t\vdash t\quad\Gamma, A[x\mapsto t] \vdash \Delta}{\Gamma, \forall x. A \vdash \Delta}{\forall_L}
      &
      \rule{t\vdash t\quad\Gamma \vdash A[x\mapsto t], \Delta}{\Gamma \vdash \exists x. A, \Delta}{\exists_R}
      &
      \rule{\Gamma, A \vdash \Delta}{\Gamma, \exists x. A \vdash \Delta}{\exists_L}
    \end{array}

We also allow quantification over modalities, as in :cite:`nigamAlgorithmicSpecificationsLinear2009`.

Logic translations
==================

First we must define classical and intuitionistic logic. To define classical logic we simply add standard structural weakening and contraction rules to our linear logic. Then :math:`A\otimes B \equiv A \land B`, :math:`A\par B \equiv A \lor B`, and we obtain the usual classical logic with modalities :cite:`lafontLinearLogicPages`; all the connectives decompose into or are equivalent to the standard ones. To define intuitionistic logic we take classical logic and restrict the right hand side of all sequents to have at most one consequent; various pi/sigma connectives cannot be used as they would create multiple consequents, and similarly right contraction cannot be used. We allow disallow right weakening to make the translation easier.

The translation from intuitionistic logic to linear logic decorates every proposition and subproposition with !. :cite:`dicosmoIntroductionLinearLogic2015`

.. math::

  \left[\prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right)\right]_I &= \prod \limits_{i} \left(\overrightarrow{\bang\left[A_i\right]_I} \multimap \overrightarrow{\bang\left[B_i\right]_I}\right)

  \left[\sum \limits_{i} \left(\overrightarrow{A_i} - \overrightarrow{B_i}\right)\right]_I &= \sum \limits_{i} \left(\overrightarrow{\bang\left[A_i\right]_I} - \overrightarrow{\bang\left[B_i\right]_I}\right)

We can translate classical logic into intuitionistic logic by decorating every proposition and subproposition with :math:`\neg\neg` and moving the right to the left with another negation, i.e. :math:`\Gamma \vdash \Delta \Rightarrow \Gamma', \neg \Delta' \vdash`. Thus the translation of classical logic into linear logic decorates like :math:`\neg \bang (\neg \bang A) \equiv \whim \bang A`.

These two decoration translations preserve proof structure, in the sense that every intuitionistic/classical proof tree can be converted to a linear logic proof tree, and the reverse as well if the linear logic proof tree's sequent is the result of the proposition translation.

Definitions
===========

We call sequents of the form :math:`\vdash A` proofs of :math:`A`. Similarly sequents :math:`A \vdash` are refutations of :math:`A`. :math:`\Sigma_R` constructs a proof from a collection of proofs and refutations, while :math:`\Pi_L` constructs a refutation from a collection of proofs and refutations. We can similarly consider proof patterns :math:`x, \ldots, z \vdash A` / refutation patterns :math:`x,\ldots,z, A \vdash` where :math:`x,\ldots,z` are free variables.

If we have a proof of :math:`A` then :math:`A` is a theorem (also called a tautology). If we prove a sequent :math:`\Gamma \vdash` then :math:`\Gamma` is a contradiction. We define equivalence :math:`A\equiv B` as the theorem :math:`\vdash A \leftrightarrow B`.

We define the notation :math:`A\defeq B` as a pair of rules:

.. math::
  :nowrap:

    \begin{array}{cc}
      \rule{\Gamma \vdash B, \Delta}{\Gamma \vdash A, \Delta}{\text{def}_R}
      &
      \rule{\Gamma, B \vdash \Delta}{\Gamma, A \vdash \Delta}{\text{def}_L}
    \end{array}

The cut elimination theorem poses no problem, but the identity theorem fails to complete if there is an infinite chain of definitions :math:`A_1 \defeq \ldots A_2 \ldots, A_2 \defeq \ldots A_3 \ldots, \ldots`. So we categorize the definitions where such an infinite chain exists as undefined (circular) and exclude them from the syntax. Once we have our restriction, we can derive the identity theorem on it, via deriving the identity theorem on the definition instances and using the normal identity theorem on the sequent with all definition instances replaced with fresh variables. For the substitution theorem we must limit our substitution to propositions that are defined. For the parts of the proof tree not using the identity or substitution theorems the notation is always defined.

In general it is undecidable if a particular definition instance is defined (see :ref:`discussion of set paradoxes <paradoxes>`). But most definitions don't have a definition on the RHS hence are easy to check for circularity.

Set theory
==========

We define :math:`t\in \{x\mid A\} \defeq A[x\mapsto t]`. Here the elements :math:`t` of the sets are propositions; e.g. we can prove :math:`\bot \in \{x\mid x \leftrightarrow \bot \}`. Usually the variables in set theory range over sets. Hence we introduce new variables :math:`x^S` which range over sets. Since all sets are of the form :math:`\{x\mid X\}` these can be translated as follows:

* :math:`x^S \defeq \{x\mid X\}`
* :math:`\{x^S\mid X\} \defeq \{X\mid X\}`
* :math:`\{x\mid X\} \in Y \defeq X \in Y`
* :math:`\forall x^S. X \defeq \forall X. X`
* :math:`\{x^S\mid X\} \subseteq \{x^S\mid Y\} \defeq X \to Y`

So for example :math:`t^S\in \{x^S\mid x^S \in x^S \}` expands to :math:`T \in \{X \mid X \in \{x\mid X\} \}`

We can also define set-builder notation :math:`\{a_1,\ldots,a_n\} = \{x\mid x = a_1 \lor \ldots \lor x=a_n\}`.

We define :math:`a \ocin B = \bang(a \in B)`, :math:`\left[ P(x) \vdash_{x\in A} Q(x) \right] = \left[ \bang(x\in A), P(x) \vdash Q(x) \right]`, :math:`\exists x\in A. P(x) = \exists x. \bang(x \in A) \otimes P(x)`, :math:`\forall x\in A. P(x) = \forall x. (\bang(x\in A) \multimap P(x)`. This is similar to :cite:`shulmanLinearLogicConstructive2018` except :math:`\in` is not affirmative.

.. _paradoxes:

Paradoxes
---------

With regards to definedness, some statements may be defined but not others. For example :math:`\{x\mid T \} \in \{x\mid x \in x \}` is defined (and provable) but :math:`\{x\mid x \in x \} \in \{x\mid x \in x \}` is circular hence not defined.

It seems from playing with some examples that forbidding circular definitions is sufficient to prevent Russell's paradox and Curry's paradox. :math:`t\in R \land t= R, R = \{x\mid \neg(\bang x \in x)\}` is not circular, but it is not sufficient to derive a paradox, as in order to apply contraction we have to use substitution to obtain the forbidden proposition :math:`R \in R`. Since cut elimination holds, only the identity or substitution rules could produce paradoxes, hence closing the loopholes in these solves the issue.

The question of whether a given set comprehension is defined is undecidable, as we can encode the lambda calculus and hence the halting problem - the beta rule :math:`(\lambda x. A) t` does the same substitution as :math:`t\in\{x\mid A\}`. We can approximate definedness with a termination checking algorithm or syntactic check:

* Strict comprehension, i.e. the bound variable can only appear once in the formula :cite:`shirahataLinearSetTheory1998`
* New Foundations's stratified formulas :cite:`forsterQuineNewFoundations2019` :cite:`holmesElementarySetTheory1998`
* Hindley-Milner type inference (since the simply typed lambda calculus terminates)
* A size-checking algorithm like in :cite:`jonesCallbyvalueTerminationUntyped2008`
* Brute-force expansion

There is also :cite:`shirahataLinearConservativeExtension1996` which allows sets built from ZF's axioms.

Comprehension
-------------

We can prove the axiom schema of comprehension for (defined) formulas :math:`\phi`:

1. :math:`\vdash \phi \leftrightarrow \phi)`
2. :math:`\vdash x\in \{x\mid\phi\} \leftrightarrow \phi)`
3. :math:`\vdash \forall x. x\in \{x\mid\phi\} \leftrightarrow \phi)`
4. :math:`\vdash \exists y. \forall x. x\in y \leftrightarrow \phi)`

Equality
--------

The axioms of reflexivity, substitution, etc. can take a variety of modalities as in :cite:`coniglioEqualityLinearLogic2002`, some of them corresponding with intuitionistic and classical notions of equality. For sets we use linear weak extensional equality :math:`A=B \defeq \forall x. (x \in A \leftrightarrow x \in B)`.
We can easily prove

.. math::

  \vdash A=A

  A=B\vdash B=A

  A=B,B=C\vdash A=C

For substitution, we can prove :math:`!(A=B), \phi \vdash \phi[A/B]` for any specific proposition :math:`\phi`. Proof: For :math:`\Pi` we use the right rule to split into cases for each tag, then we use contraction/weakening on :math:`\bang(A=B)` to match the number of A's/B's in the case, then the left rule to split into each A and B, giving each branch a copy of the hypothesis. :math:`\Sigma` is similar but with the left first. For exponentials, quantifiers, and set comprehension we simply do left/right in the correct order. Then at the end we use the hypothesis to change :math:`A[x/a]` on the left or right to :math:`B[x/b]`, or else weakening to remove the hypothesis followed by the identity.

Alternatively we could use intuitionistic equality :math:`A\overset{!}{=}B \defeq !(A=B)`, then substitution is :math:`A\overset{!}{=}B, \phi \vdash \phi[A/B]`. But the linear equality seems more useful.

.. _infinite:

Infinite structures
===================

We want to support infinite types like the lambda calculus or lists, and similarly infinite expressions like ``x = 1 : x``. We construct "infinite" as a terminal coalgebra - our proof trees turn into fixed points of systems of formal equations :cite:`karazerisFinalCoalgebrasAccessible2011`. We represent these using variable and assignment rules. The semantics is that the variable usage is a "hole" that plugs in a copy of the derivation tree from the variable assignment. The type of the use rule can performs a substitution on the free variables of the type of the assignment.

.. math::

    \begin{array}{cc}
      \rule{X }{ \Gamma[\vec x/\vec t] \vdash \Delta[\vec x/\vec t] }{\text{Use}}
      &
      \rule{\Gamma \vdash \Delta}{ X = }{\text{Assign}}
    \end{array}

Infinite structures can be paradoxical, e.g. we can prove :math:`\vdash\bot` using cut on the proposition :math:`A=\neg A`. Cut elimination will often fail to complete, but there is a progress property in the sense that the cut can always be pushed down and eliminate an identity rule or two matching logical rules.

Hashing
=======

To hash the graphs we can use the tree structure of the sequent derivations. Each upward slot in a node is hashed with a fixed value and each downward slot is hashed with a value corresponding to the path through the derivation tree followed by the label of the upward slot. It is written as a single DFS traversal from the leaves upwards that stores the hashed subtree and a map from edge name to partial path.