Logic
#####

The logic for Stroscot is based on a two-sided linear logic sequent calculus.

Jumbo connectives
=================

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

The negations and shifts have identical derivation rules for each polarity, so we write :math:`\neg A` and :math:`\smash{\updownarrow}A` unless there is a need for the distinction.

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
     - 32-bit integers
     - :math:`\Sigma [(\#{-2}^{31},[]-[]),\ldots,(\#0,[]-[]),\ldots,(\#2^{31}-1,[]-[])]`
   * - :math:`L_A`
     - Linked list of A
     - :math:`\Sigma [(\text{#nil},[]-[]),(\text{#cons},[A,L_A]-[])]`
   * - :math:`Arr_A`
     - Arbitrarily-sized tuple of A
     - :math:`\Sigma [(\text{#0},[]-[]),(\text{#1},[A]-[]),(\text{#2},[A,A]-[]),\ldots]`

Exponentials
============

Promotion
---------

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\overrightarrow{\bang \Gamma_i } \vdash A, \overrightarrow{\whim\Delta_i} }{\overrightarrow{\bang \Gamma_i } \vdash \bang A, \overrightarrow{\whim\Delta_i}}{\bang}
    &
    \rule{\overrightarrow{\bang \Gamma_i } , A\vdash \overrightarrow{\whim\Delta_i} }{\overrightarrow{\bang \Gamma_i }, \whim A \vdash \overrightarrow{\whim\Delta_i}}{\whim}

  \end{array}

Dereliction
-----------

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\sk{\Gamma}, A \vdash \sk{\Delta} }{\sk{\Gamma}, \bang A \vdash \sk{\Delta}}{\bang d}
  & \rule{\sk{\Gamma} \vdash A, \sk{\Delta} }{\sk{\Gamma} \vdash \whim A, \sk{\Delta}}{\whim d}
  \end{array}

Weakening
---------

.. math::
  :nowrap:

  \begin{array}{cc}
      \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang A \vdash \sk{\Delta}}{\bang w}
    & \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma} \vdash \whim A, \sk{\Delta}}{\whim w}
    \end{array}

Contraction
-----------

.. math::
  :nowrap:

  \begin{array}{cc}
      \rule{\sk{\Gamma}, \overrightarrow{\bang A, \bang A, \cdots} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang A \vdash \sk{\Delta}}{\bang c_n}
    & \rule{\sk{\Gamma} \vdash \overrightarrow{\whim A, \whim A, \cdots}, \sk{\Delta} }{\sk{\Gamma} \vdash \whim A, \sk{\Delta}}{\whim c_n}
  \end{array}

Structural rules
================

Exchange
--------

.. math::

  \rule{\sk{\Gamma} \vdash \sk{\Delta}}{\sk{\sigma_L(\Gamma)} \vdash \sk{\sigma_R(\Delta)}}{\text{x}}

Cut
---

.. math::

  \rule{\sk{\Gamma} \vdash A, \sk{\Delta} \quad \sk{\Theta}, A \vdash \sk{\Lambda} }{\sk{\Gamma}, \sk{\Theta} \vdash \sk{\Delta}, \sk{\Lambda} }{\text{cut}}

Identity
--------

.. math::

  \rule{}{A \vdash A}{\text{id}}

Higher-order logic
==================

Predicates
----------

Terms consist of variables :math:`x`, literal values :math:`v`, and applications of terms to terms. Variables quantify over the universal set, smaller domains can be defined by the membership notation in :ref:`set-theory`.

If the term is a predicate (set) then it may be used as an atomic formula. Such an atomic formula :math:`t` must satisfy the identity rule:

.. math::

  \rule{}{t \vdash t}{\text{id}}

Interpreted predicates may be defined using other rules so long as the identity rule is derivable and the rules are consistent.

Substitution
------------

:math:`A[\overrightarrow{x \mapsto t}]` stands for the proposition :math:`A` where all free occurrences of the variables :math:`\overrightarrow{x}` have been replaced by terms :math:`\overrightarrow{t}` in the appropriate domains (and bound variables have been renamed to fresh ones when necessary).

  .. math::
    :nowrap:

    \begin{array}{c}
      \rule{t\vdash t\quad\Gamma \vdash \Delta}{\Gamma[\overrightarrow{x \mapsto t}] \vdash \Delta[\overrightarrow{x \mapsto t}]}{\text{sub}}
    \end{array}

Quantifiers
-----------

For these the variable :math:`x` must have no free occurrence in :math:`\Gamma` or :math:`\Delta`.

.. math::
  :nowrap:

   \begin{array}{cc}
      \rule{\Gamma \vdash A, \Delta}{\Gamma \vdash \forall x. A, \Delta}{\forall_R}
      &
      \rule{t\vdash t\quad\Gamma, A[x\mapsto t] \vdash \Delta}{\Gamma, \forall x. A \vdash \Delta}{\forall_L}
      \\
      \rule{t\vdash t\quad\Gamma \vdash A[x\mapsto t], \Delta}{\Gamma \vdash \exists x. A, \Delta}{\exists_R}
      &
      \rule{\Gamma, A \vdash \Delta}{\Gamma, \exists x. A \vdash \Delta}{\exists_L}
    \end{array}

Definitions
===========

If we define some notation :math:`A\defeq B`, then this means adding a pair of rules to our logic:

.. math::
  :nowrap:

    \begin{array}{cc}
      \rule{\Gamma \vdash B, \Delta}{\Gamma \vdash A, \Delta}{\text{def}_R}
      &
      \rule{\Gamma, B \vdash \Delta}{\Gamma, A \vdash \Delta}{\text{def}_L}
    \end{array}

The notation and rules are valid only if there is a finite proof of the identity sequent :math:`B \vdash B`.

.. _infinite:

Infinite proof structures
=========================

These have "use" and "def" rules ("def" is short for definition). The use is a "hole" that plugs in the derivation tree from the definition. The type of the use rule can performs a substitution on the free variables of the type of the assignment.

.. math::

    \begin{array}{cc}
      \rule{X }{ \Gamma[\overrightarrow{x \mapsto t}] \vdash \Delta[\overrightarrow{x \mapsto t}] }{\text{Use}}
      &
      \rule{\Gamma \vdash \Delta}{ X = }{\text{Def}}
    \end{array}

.. _set-theory:

Set theory
==========

We define :math:`t\in \{x\mid A\} \defeq A[x\mapsto t]`. Here the elements :math:`t` of the sets are propositions; e.g. we can prove :math:`\bot \in \{x\mid x \leftrightarrow \bot \}`. Usually the variables in set theory range over sets. Hence we introduce new variables :math:`x^S` which range over sets. Since all sets :math:`S` are of the form :math:`\{x\mid X_S \}` these can be translated as follows:

.. math::

  x^S \defeq \{x\mid X_S \}

  \{x^S\mid X\} \defeq \{X_S \mid X\}

  \{x\mid X\} \in Y \defeq X \in Y

  \forall x^S. X \defeq \forall X. X

  \{x^S\mid X\} \subseteq \{x^S\mid Y\} \defeq X \to Y

So for example :math:`t^S\in \{x^S\mid x^S \in x^S \}` expands to :math:`T \in \{X \mid X \in \{x\mid X\} \}`

We can also define set-builder notation :math:`\{a_1,\ldots,a_n\} = \{x\mid x = a_1 \lor \ldots \lor x=a_n\}`.

We define

.. math::

  a \ocin B = \bang(a \in B)

  \left[ P(x) \vdash_{x\in A} Q(x) \right] = \left[ \bang(x\in A), P(x) \vdash Q(x) \right]

  \exists x\in A. P(x) = \exists x. \bang(x \in A) \otimes P(x)

  \forall x\in A. P(x) = \forall x. (\bang(x\in A) \multimap P(x)

Comprehension
-------------

We can prove the axiom schema of comprehension for formulas :math:`\phi` with free variable :math:`x`:

1. :math:`\vdash \phi \leftrightarrow \phi`
2. :math:`\vdash x\in \{x\mid\phi\} \leftrightarrow \phi`
3. :math:`\vdash \forall x. x\in \{x\mid\phi\} \leftrightarrow \phi`
4. :math:`\vdash \exists y. \forall x. x\in y \leftrightarrow \phi`

Equality
--------

Equality on sets is defined as follows:

:math:`A=B \defeq \forall x. (x \in A \leftrightarrow x \in B)`.

We can easily prove that this equality is an equivalence relation:

.. math::

  \vdash A=A

  A=B\vdash B=A

  A=B,B=C\vdash A=C

For substitution, we can prove :math:`!(A=B), \phi \vdash \phi[A/B]` for any specific proposition :math:`\phi`.

