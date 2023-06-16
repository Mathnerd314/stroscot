Logic
#####

.. math::

  \newcommand{\rule}[3]{ \dfrac{\displaystyle ~~#1~~ }{\displaystyle ~~#2~~ } \  (#3)}
  \newcommand{\defeq}{\overset{\text{def}}{=}}
  \newcommand{\with}{\mathbin{\mathrm{\&}}}
  \newcommand{\par}{\mathbin{\mathrm{⅋}}}
  \newcommand{\multimapboth}{\mathbin{\mathrm{⧟}}}
  \newcommand{\bang}{{\mathrm{!}}}
  \newcommand{\whim}{{\mathrm{?}}}
  \newcommand{\bangc}{{\mathrm{!}}}
  \newcommand{\whimc}{{\mathrm{?}}}
  \newcommand{\ocin}{\mathrel{\raise{-1pt}{\mathrm{!}}\mathord{\in}}}
  \definecolor{mygray}{RGB}{156,156,156}
  \newcommand{\sk}[1]{{\color{mygray} #1}}

The Curry-Howard correspondence maps logic to programming. A logical system specifies well-formed proofs of propositions. These correspond to well-formed programs in a simple type system. By proving the logic sound and complete, we get an expressive programming language.

Logic style
===========

The use of sequent calculus is inspired by :cite:`downenSequentCalculusCompiler2016`. Whereas a natural deduction logic results in reduction patterns similar to the lambda-calculus, sequent calculus divides the program into values and continuations. Reduction always takes place at a cut point with a value and a continuation. Continuations are exposed as first-class manipulable variables, similar to CPS, but as discussed in IR, CPS-based IRs have drawbacks that sequent calculus-style IRs do not.

Between classical, intuitionistic, and linear logic, I went with linear logic. It's the most expressive, in that intuitionistic and classical logic can be encoded fairly naturally, but linear logic has more operators. Intuitionistic logic is traditional and has a direct machine implementation, but there is an operational semantics for linear logic :cite:`mackieGeometryInteractionMachine1995` and linear logic makes the expensive "copy term" operation more explicit. In intuitionistic logic, copying can happen anywhere in the reduction, which is harder to handle. The "boxes" of linear logic which demarcate copying boundaries are supposedly the basis of optimal reduction :cite:`guerriniTheoreticalPracticalIssues1996`.

:cite:`downenSequentCalculusCompiler2016`  say "Sequent Core lies in between [intuitionistic and classical logic]: sometimes there can only be one conclusion, and sometimes there can be many." Specifically, terms and types are intuitionistic (single conclusion), but commands, continuations and bindings are written to allow multiple propositions. Ret/Ax, WR/Name, and Jump/Label all introduce right-weakening. Linear logic seems a lot cleaner than this mess, but also lies between intuitionistic and classical logic, in that (like intuitionistic) there is no excluded middle, but also (like classical) there is duality and no limitation on the number of conclusions.

Linear logic can be formulated as two-sided, one-sided, or intuitionistic. I chose two-sided because again, it's the most expressive. Intuitionistic logic doesn't typically have negation, but it can be encoded as :math:`A \to \bot`, pretty bad for trying to simplify connectives. One-sided might seem attractive, as on first glance it makes formulas more uniform, but it actually doesn't really improve complexity because the one-sided logic has the same number of connectives, and the duals require special handling - e.g. an application of identity looks like :math:`\vdash \Sigma X^\bot, \Pi X` instead of :math:`\Pi X \vdash \Pi X`. So we end up with a representation of a connective as having a boolean for polarity, whereupon there is not much issue with adding a boolean for side as well to obtain the two-sided sequent. The two-sided sequent formulation preserves intent, e.g. the definition of refutations as sequents :math:`A \vdash` is not distinct from that of a proof in the one-sided calculus. Taking the two-sided logic as basic, the one sided logic can be formulated as a transformation that moves everything to the right side with negation and pushes down negations to atomic statements by applying duality. Such a transformation is of course useful, but forcing it to be applied from the start does not seem to serve much purpose.

There are also other logics similar to Girard's linear logic, like deep inference. Most papers on deep inference seem to add the `mix rule <https://ncatlab.org/nlab/show/mix+rule>`__ which corresponds to assuming :math:`1 \leftrightarrow \bot`. This doesn't seem attractive compared to plain linear logic - it proves contradiction as a theorem, hence loses the embedding of classical logic. `This page <https://www.pls-lab.org/en/Mix_rule>`__ mentions that mix holds in various models of linear logic such as coherent spaces and the game-theoretic semantics, but models are usually a simplification and there are models such as the syntactic model where mix doesn't hold. :cite:`strassburgerDeepInferenceExpansion2019` presents a deep inference system for non-mix MLL but says extending it to full LL is future work.

There is also the question of if, having removed weakening and contraction, the remaining structural rules, exchange and associativity, should be dropped. Linear logic gives the idea of the propositions being in named slots, as e.g. :math:`\vdash A^a, B^b`` is for almost all purposes the same as :math:`\vdash B^b, A^a``, and only in some narrow cases would we want to differentiate them. This associative array semantics corresponds well to the RAM model. In contrast, dropping exchange gives non-commutative or ordered logic, leading to a stack or list on each side. But :cite:`shiVirtualMachineShowdown2005` shows that a register model is much better for an implementation - the extra stack swapping instructions are more overhead than the additional register names. Stack-like access is just too restrictive. Similarly, dropping associativity gives a tree-like semantics, and trees are not iterable in constant time. The number of operators would explode because every tree structure / stack index would create a new operator. Hence linear logic is the clear winner. But, there seem to be reasonable ways of embedding linear logic in non-associative / non-commutative logic by adding associative / commutative modalities. :cite:`blaisdellNonassociativeNoncommutativeMultimodal2022` If there was a popular logic with such an embedding, then we could switch from linear logic to that. But per :cite:`millerOverviewLinearLogic2004` "no single [non-commutative] proposal seems to be canonical at this point."

Type annotations
================

In :cite:`downenSequentCalculusCompiler2016`, TyApp is written in natural deduction style. Actually, it seeems type annotations just sort of float around, they should not be part of the sequents at all. In my presentation of logic, terms are the proof trees, and propositions the types. So when it is written that ∀ and ∃ consume terms with types, like ∀R :math:`(\Gamma, a : \kappa \to v : \tau) \vdash (\Gamma \to \Lambda a : \kappa. v : \forall a : \kappa . \tau)`, :math:`a : \kappa` is just an assertion, and I would instead just write :math:`(\Gamma \to \tau) \vdash (\Gamma \to \forall a : \kappa . \tau)`, where :math:`a` has no free occurrence in :math:`\Gamma` and is ambiently assumed to be of some type. Stroscot is a unityped language, so the types can just be ignored for now.

Jumbo connectives
=================

Based on :cite:`levyJumboLcalculus2006`, Stroscot aims for the largest allowable set of operators. In particular we generalize into two jumbo operators, :math:`\Sigma` (sigma) and :math:`\Pi` (pi). The generalized :math:`\Pi` rule is similar to Levy's rule except it allows multiple conclusion propositions. The generalized :math:`\Sigma` rule is the dual of :math:`\Pi`. We have indexed variables :math:`A_{ij}` and :math:`B_{ik}` where :math:`0 \leq i < N, 0 \leq j < m_i, 0 \leq k < n_i`. We call :math:`N` the length of the jumbo type and the list :math:`[(m_i,n_i)]` the jumbo-arity.

In :cite:`downenSequentCalculusCompiler2016`, TLK/TRK correspond to the intuitionistic jumbo-style Sigma operator. But they have no corresponding Pi operator, but rather Case/Deflt, which have no effect on the sequent type, hence no logical meaning. They say they considered adding "general existential types" which seems to be a Pi operator in disguise, but mapping to GHC Core was too difficult and required a "heavy-handed encoding". Since  Stroscot is a new language, we can have a full set of jumbo operators to do all sorts of case-matching.

When the RHS of :math:`\Sigma` is nonempty we get terms with holes, that can be pattern-matched by filling the holes, e.g. `difference lists <https://en.wikipedia.org/wiki/Difference_list>`__. (TODO: check that this actually gives efficient concatenation)

The dual of implication is called "subtraction" or "difference" and is denoted :math:`-`. For an ADT, the RHS of the difference is empty, i.e. ``a A | b B1 B2 | C`` looks like :math:`\Sigma [(a, [A]-[]),(b, [B_1, B_2]-[]), (c,[]-[])]`. This follows :cite:`wadlerCallbyvalueDualCallbyname2003` and :cite:`crolardFormulaeastypesInterpretationSubtractive2004` but is flipped compared to Pi's implication.

The jumbo connectives have the nice "unpacking" property that any combination of :math:`\Sigma` connectives is equivalent to a single :math:`\Sigma` connective, and likewise for :math:`\Pi`.

The index :math:`i` in Levy's presentation is a tag drawn from a finite subset of a countable set of labels. But we can draw it from a subset of the universal set and hence get dependent types that depend on terms. In particular :math:`\Pi` gives a dependent function type and :math:`\Sigma` gives a dependent pair type.

Common connectives
==================

Girard chose his notation mainly for convenience, in particular he avoided the common symbols of logical connectives so that he didn't have to mark classical logical connectives vs. linear logic connectives. Since Stroscot only uses one logic style, we aren't limited by such considerations.

Per the structure-preserving "plethoric" translation in :cite:`dicosmoIntroductionLinearLogic2015`, section 2.1, PDF page 30, the linear logic operators that correspond to intuitionistic logic's conjunction/disjunction are the additive binary connectives, Girard's :math:`\with` and :math:`\oplus`. Girard's notation doesn't express their duality at all so it has to be replaced. Per Wikipedia, the customary symbols are :math:`\land / \lor`, programming uses :math:`\with / \vert`, and there is also the electronics notation :math:`\cdot / +`. ChatGPT says :math:`\land / \lor` is 80% of usage so that's what I went with.

For :math:`\otimes`, it is a `tensor/monoidal product <https://en.wikipedia.org/wiki/Monoidal_category>`__ so the symbol is natural. Technically :math:`\par` is a tensor product too, but the fact that tuples are typically positive data leads us to privilege one over the other. And it agrees with Girard, although I think he chose :math:`\otimes` to go with :math:`\oplus`.

:math:`\par` is the dual of :math:`\otimes` in the sense that :math:`A \par B \equiv \neg (\neg A \otimes \neg B)`; unfortunately for deciding a notation, this seems to be its only useful property. Looking at the literature, :math:`\oplus, \odot ,\Box,\sharp, \bullet` and :math:`*` have meanings (direct sum/coproduct, Hadamard product/XNOR gate/symmetric product, modal operator, music, multiplication/logical and, convolution) dissimilar from the function of :math:`\par`. :math:`\mathbin{{\scriptstyle+}\mkern-0.522em\raise{-0.077em}{\diamond}},\mathbin{{\vee}\mkern-0.815em\raise{0.09em}{\bigcirc}}` don't have Unicode symbols so are hard to use. In the end none of the operators seems particularly evocative. :math:`\par` on the other hand redirects to linear logic on Wikipedia. So again we follow Girard.

Then we have the units for each of these connectives. I'm most confident in my choice of :math:`\bot` as the unit for :math:`\par`. It is usually used for denoting the absurdity derived from a contradiction, and indeed we can prove :math:`A, \neg A \vdash \bot` and :math:`\neg A \equiv A \to \bot` starting from the identity rule. It also agrees with Girard.

For the unit of the tensor product, it's the standard unit type in programming, so ``()`` is a possibility, but in the math-style semantics the unit type is traditionally represented as :math:`\mathbb{1}`, matching Girard. I went with an unstyled :math:`1` for ease of input. Another choice would be the tee symbol :math:`\top` to be the dual of :math:`\bot`, but this conflicts with Girard's usage of that symbol.

The units of :math:`\lor / \land` are logical false / logical true. Looking at choices, :math:`\bot / \top` is out because :math:`\bot` is already taken. IMO it's pretty confusing that in Girard's notation :math:`\bot` is not the dual of :math:`\top`. :math:`0/1` is pretty common, but conflicts with using :math:`1` for the unit of :math:`\otimes`. :math:`T/F` is clear enough and doesn't conflict, so that's what I went with.

The negations and shifts show up in polarized / focused linear logic. We use the convention that an up shift raises a negative to positive (increases the value, hence points up). This is the original definition of `Girard's <https://www.seas.upenn.edu/~sweirich/types/archive/1991/msg00123.html>`__, ``P = up N``. More recent papers such as :cite:`girardLocusSolumRules2001` and :cite:`zeilbergerLogicalBasisEvaluation2009` seem to have lost this convention and use definitions of up-shift and down-shift reversed from ours. :cite:`nigamAlgorithmicSpecificationsLinear2009` uses an alternate notation of delay operators :math:`\delta^\pm(\cdot)` instead of shifts.

Programming types
-----------------

With the programming types we see the justification for the jumbo types: they can represent abstract data types (ADTs). Even though we can encode :math:`\Pi,\Sigma` using the common connectives:

.. math::

  \Pi [(\#t_1,[A_{1,1},A_{1,2},\ldots] \multimap [B_{1,1},B_{1,2},\ldots]),\ldots] \equiv (\smash{\stackrel{-}{\neg}} A_{1,1} \par \smash{\stackrel{-}{\neg}} A_{1,2} \par \ldots \par B_{1,1} \par \ldots) \land \ldots

  \Sigma [(\#t_1,[A_{1,1},A_{1,2},\ldots] \multimap [B_{1,1},B_{1,2},\ldots]),\ldots] \equiv (A_{1,1} \otimes A_{1,2} \otimes \ldots \otimes \smash{\stackrel{+}{\neg}} B_{1,1} \otimes \ldots) \lor \ldots

With the encoding, we lose the free-form tags and have to use strings like "RRRRRL". This leads to unbalanced proof trees and a general lack of expressiveness of the proof language.

Lambdas have several conflicting definitions, so I have left them out. :cite:`maraistCallbynameCallbyvalueCallbyneed1995` says call by name lambdas are :math:`\Omega_N = \bang \Omega_N \to \Omega_N`, and call by value lambdas are :math:`\Omega_V = \bang (\Omega_V \to \Omega_V)`. :cite:`filinskiLinearContinuations1992` agrees on call by name lambdas, but says call by value lambdas are :math:`\Omega_V = \bang (\Omega_V \to \whim \Omega_V)`. A paper I forgot to cite says that these all expand to the same infinite type so are equivalent, making me wonder if linear logic actually represents reduction strategy at all.

Exponentials
============

There are two S4 modalities !/bang/"of course" (positive/affirmative) and the dual ?/whim/whimper/"why not" (negative).

Contraction
-----------

Instead of binary contraction we allow :math:`n`-ary contraction for :math:`n\geq 2`. This is equivalent to binary contraction but makes the proof trees a little more compact.

Subexponentials
---------------

Standard linear logic only contains bang and whim. But per :cite:`nigamAlgorithmicSpecificationsLinear2009` these are not "canonical" - if we introduce two modalities :math:`\bang_1, \bang_2` with distinct but identical rules, we cannot prove :math:`\bang_1 A \equiv \bang_2 A`. So in keeping with the maximalist approach we present here the logic with subexponentials. The subexponentials function as annotations on exponentials, in that we can erase all the subexponential labels to a single standard exponential, and we can infer subexponential labels, computing the minimal subexponential structure necessary for the program to work. Subexponentials whose only operations are promotion/dereliction can be deleted from the program. :cite:`danosStructureExponentialsUncovering1993`

For notation, there's a lot of choices. I propose the following:

* :math:`\bang^x_m,\whim^x_m`, :math:`\bang_{(m,x)},\whim_{(m,x)}`, or more simply :math:`\bang_S,\whim_S` (with :math:`S = (m,x) \in (M,X)`) is the full notation for a subexponential, where :math:`m` is in an index set :math:`M` and :math:`x \in X, X = P(\{c, w\})`.
* The "well-known" subexponentials are :math:`\bang^x,\whim^x`, where the index :math:`m=\cdot \in M` is omitted. These allow easily omitting various operations.
* The "full" subexponentials are :math:`\bang_m,\whim_m`, and have :math:`x=\{c,w,d\}` - this allows easily writing distinguished subexponentials.
* Combining these conventions, :math:`\bang,\whim` are the well-known full exponentials :math:`m=\cdot,x=\{c,w,d\}`, as is standard.

To use these we must define a relation :math:`\leq` on :math:`(M,X)` such that :math:`((M,X),\leq)` is a poset abd satisfies two more requirements:

#. :math:`(m,x) \leq (n,y)` implies :math:`x\subseteq y`.
#. if :math:`x \subseteq y`, then :math:`(m,x) \leq (m,y)` for all :math:`m`.

Justifying these:

* Reflexivity ensures the identity theorem.
* Transitivity and the first requirement ensure cut elimination.
* Antisymmetry ensures that if :math:`\bang^x_m A \equiv \bang^y_n A` then :math:`m=n` and :math:`x=y`, so that we do not have duplicate notation for a particular modality.
* The second requirement is not strictly necessary, but makes the notation more coherent. If it is not satisfied we can simply split the index :math:`m` into two or more indexes :math:`m_i`.

The rule for promotion requires that :math:`(z,o)\leq (x_i,m_i)` and :math:`(z,o)\leq (y_i,n_i)` for the elements of the context.

.. math::
  :nowrap:

  \begin{array}{cc}
    \rule{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } \vdash A, \overrightarrow{\whim^{y_i}_{n_i}\Delta_i} }{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } \vdash \bang^z_o A, \overrightarrow{\whim^{y_i}_{n_i}\Delta_i}}{\bang}
    &
    \rule{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i } , A\vdash \overrightarrow{\whim^{y_i}_{n_i}\Delta_i} }{\overrightarrow{\bang^{x_i}_{m_i} \Gamma_i }, \whim^z_o A \vdash \overrightarrow{\whim^{y_i}_{n_i}\Delta_i}}{\whim}

  \end{array}

Dereliction must be allowed, otherwise the identity rule does not hold.

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

Because of the equivalences :math:`\bang \bang A \equiv \bang A, \whim\whim A \equiv \whim A, \bang \whim \bang \whim A \equiv \bang \whim A, \whim \bang \whim \bang A \equiv \whim \bang A`, there are only 7 modalities created from combining exponentials. They have the relationships as follows, where an arrow :math:`A \to B` means :math:`\vdash A \to B` is provable:  :cite:`coniglioEqualityLinearLogic2002`

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

* For :math:`(x,m)\geq(y,n)`, :math:`\bang^x_m \bang^y_n A \equiv \bang^x_m A \equiv \bang^y_n \bang^x_m A`, and identically with :math:`\whim`.
* For :math:`(x,n)\leq(z,p)` and :math:`(y,o)\leq(w,m)` we can prove :math:`\bang^w_m \whim^x_n \bang^y_o \whim^z_p A \equiv \bang^w_m \whim^z_p A`.

With distinguished subexponentials the possible modalities become infinite, for example alternating patterns like :math:`\bang_1 \bang_2 \bang_1 \bang_2` cannot be simplified unless there is a relation in the poset between 1 and 2. But of course if we erase the index labels then :math:`\bang \bang \bang \bang A \equiv \bang A`. Due to this, I think keeping the "well-known" subexponentials in the IR is fine and informative, but distinguished subexponentials should be avoided except during a pass that reduces exponential strength and eliminates unnecessary exponentials.

Classification of sequents
==========================

:cite:`downenSequentCalculusCompiler2016` splits the space of sequents into four sets: commands :math:`\Gamma \vdash \Delta`, terms :math:`\Gamma \vdash \tau`, continuations :math:`\Gamma, \tau \vdash \Delta`, and bindings :math:`\Gamma, \Delta' \vdash \Gamma', \Delta` (noting that :math:`\mid` is synonymous with ","). These clearly overlap, and the restrictions on what can be used where seem arbitrary.

It seems easier to follow :cite:`lafontLinearLogicPages` and classify proofs by properties of their sequent:

* :math:`\vdash A` is a proof of :math:`A`. :math:`A` is said to be a theorem or tautology. The theorem :math:`\vdash A \leftrightarrow B` shows equivalence :math:`A\equiv B`.
* :math:`A \vdash` or :math:`A \vdash \bot` is a refutation of :math:`A`. :math:`A` is said to be an absurdity.
* :math:`\Gamma \vdash` or :math:`\Gamma \vdash \bot` derives a contradiction from :math:`\Gamma`
* :math:`x, \ldots, z \vdash A`, where :math:`x,\ldots,z` are free variables representing propositions (most likely appearing in :math:`A`), is a proof pattern of :math:`A`. Similarly :math:`x, \ldots, z, A \vdash` is a refutation pattern.
* :math:`\Gamma \vdash A` is a term with result type :math:`A`

:math:`\Sigma_R` constructs a proof from a collection of proofs and refutations, while :math:`\Pi_L` constructs a refutation from a collection of proofs and refutations.

Polarized logic
===============

Following :cite:`lafontLinearLogicPages` we say a proposition :math:`A` is positive if :math:`A \equiv \bang A`. Of course the direction :math:`\bang A \vdash A` is trivial by dereliction.

Going through the rules on :math:`A \vdash \bang A`:

* :math:`F,1` are positive
* :math:`\bang A` is positive for any :math:`A`
* :math:`A \lor B,A \otimes B` are positive if :math:`A,B` are positive
* :math:`T,\bot` are not positive
* :math:`A \land B` is positive if :math:`A` is positive and :math:`\bang A \vdash B`, e.g. if :math:`B` is a theorem
* :math:`A \par \bot` is positive if :math:`A` is positive. More generally, :math:`A \par B` is positive if :math:`A` is positive, :math:`B \vdash \bot` (i.e., :math:`B` is a refutation), and :math:`A\vdash A,B`.

Dually we say :math:`A` is negative if :math:`A \equiv \whim A`. :math:`A` is positive iff :math:`\neg A` is negative, and vice-versa. In general, :math:`\Sigma [ \vec A_i - \vec B_i]` is positive and :math:`\Pi [ \vec A_i \multimap \vec B_i ]` is negative if :math:`A_i` are all positive and :math:`B_i` are all negative.

We say that a proposition is polarized if it is either positive or negative. Unpolarized propositions such as :math:`T \otimes \bot` exist, although every tautology is positive and every absurdity is negative. By strong consistency, no proposition can be both negative and positive.

Girard defined a syntactic rather than semantic notion of polarity, based on classifying connectives. By his definitions, :math:`A \par \bot` and :math:`A\otimes 1` remove polarity from a positive resp. negative proposition. But the semantic definition seems more useful.

Lafont also defines regular types :math:`A \equiv \whim \bang A`, but considering that there are 6 non-trivial modalities including a dual modality :math:`\bang \whim` this seems too limited. There is not a clear justification for this definition; maybe some paper in the literature explains it, but a simple search "regular linear logic" didn't uncover it.

Cartesian types
---------------

:cite:`filinskiLinearContinuations1992` observes in section 3.1 that, even in the absence of exponentials, we can actually copy and discard data of certain "cartesian" types in a linear fashion. This is quite similar to positivity. However, positivity is not necessary or sufficient to be cartesian - :math:`BB = \text{Bool} \to \text{Bool}` is positive, but the proof does not actually make a copy of the function, because it can only evaluate the function at one value. Also, as exponentials are non-canonical, per :cite:`lafontLinearLogicPages` a formula :math:`C = \alpha \otimes \bang(\alpha \ to \alpha \otimes \alpha) \otimes \bang(\alpha \to 1)` can be copied and discarded but does not satisfy :math:`C \vdash \bang \alpha` hence is not positive.

Formally, cartesian types are defined as commutative co-monoids. In terms of sequent proofs, this means we have a
proposition :math:`A`, with theorems :math:`w : A \vdash 1` and :math:`c : A \vdash A \otimes A`. There are then 3 laws that these theorems must satisfy, equivalences under cut elimination:

* unit:

.. math::

     \rule{c : A \vdash A \otimes A \quad (w \otimes \text{id}) : A \otimes A \vdash A \otimes 1}{A \vdash A \otimes 1}{\text{cut}} = \rule{\text{id} : A \vdash A \quad \vdash 1}{A \vdash A\otimes 1}{\otimes_R}

* commutativity:

.. math::

     \rule{c : A \vdash A \otimes A \quad swap : A_1 \otimes A_2 \vdash A_2 \otimes A_1}{A \vdash A \otimes A}{\text{cut}} = \text{c} : A \vdash A \otimes A

* associativity:

.. math::

     \rule{c : A \vdash A \otimes A \quad (id \otimes c) : A \otimes A \vdash A \otimes (A \otimes A) \quad A\otimes (B \otimes C) \vdash (A \otimes B) \otimes C}{A \vdash (A\otimes A) \otimes A}{\text{cut x2}} = \rule{c : A \vdash A \otimes A \quad (c \otimes id) : A \otimes A \vdash (A \otimes A) \otimes A}{A \vdash (A\otimes A) \otimes A}{\text{cut}}

In addition, Filinski adds uniqueness: :math:`w : A \vdash 1` and :math:`c : A \vdash A \otimes A` must each have only one unique cut-free proof.

We can similarly define co-cartesian types :math:`A` that can be copied and discarded on the right with theorems :math:`w : 1 \vdash A` and :math:`c : A \otimes A \vdash A` and are a commutative monoid.

I'm not sure if there is a simple description of all cartesian types, but of course :math:`\bang/\whim` are cartesian/co-cartesian, and :math:`Sigma [ \vec A_i - \vec B_i]` and and :math:`\Pi [ \vec A_i \multimap \vec B_i ]` are cartesian/co-cartesian if :math:`A_i` are all cartesian and :math:`B_i` are all co-cartesian. So ADTs in general - booleans, integers, lists, trees - are all cartesian. Our earlier example :math:`C` is in general not cartesian because, although copy and discard can be proven, it uses the included functions and those won't necessarily satisfy the laws.

Filinski says in 3.2 that all cartesian types are positive. Hence, for cartesian / co-cartesian types, because the proof of polarity is natural, we can be generous with exponentials and use them whenever we have such a type, without fear of changing the program semantics. This allows a more faithful representation of copy operations in the proof structure, avoiding implicit copying such as ``\x -> case x of True -> (True,True); False -> (False,False)``.

But, if we start with a plethoric translation to begin with and only remove unnecessary exponentials, probably cartesian types don't matter, because all the copy operations are explicitly represented using exponentials to begin with.

Tangent: Reversible computing
-----------------------------

Another approach to duplication is "superstructural reversible logic" :cite:`sparksSuperstructuralReversibleLogic2014`. In their calculus, duplicating any type (cartesian or not) is impossible, without using non-reversible rules. The non-reversible structural rules could be restricted to a modality similarly to how linear logic restricts contraction/weakening. But in Theseus :cite:`jamesTheseusHighLevel2014` and even in the latest publication :cite:`chenComputationalInterpretationCompact2021` they use standard higher-order functions to do most of the programming. So really a reversible program is a data type ``Rev``, similar to how a normal program is a data type ``Task``. The difference is that ``Rev`` contains isomorphisms rather than continuations.

In terms of support, I don't think there's much needed at present. With the DSL support Stroscot should be able to write isomorphisms similar to Theseus, and run them forwards/backwards like the `Agda code <https://github.com/DreamLinuxer/popl21-artifact>`__. Once there is hardware/an ISA/an OS to target it should be straightforward to extend this basic support to a compiler.

Structural rules
================

As is usual for linear logic there are no structural rules for weakening or contraction (they are restricted to the exponentials above). And in Core we use a graph representation that internalizes the exchange rule, so there is not really an exchange rule either.

The cut rule is technically a theorem; we can prove that any proof using cut can be reformulated to be cut-free. But the expansion may result in exponentially more rule applications.

Similarly the identity rule is a theorem for propositional logic: we can produce a proof tree for :math:`A \vdash A` for any finite proposition :math:`A` via expansion of all the cases. Using the identity rule speeds up reduction because it skips iterating through the structure, and it also allows manipulating (prefixes of) :ref:`infinite <infinite>` trees.

Quantifiers
===========

To move from propositional to first-order logic we must extend the identity rule to allow terms. Some presentations call the identity rule "ax", for identity axiom, but in general the identity rule is a theorem so this seems foolish. Instead we call it "id".

We explicitly list the identity theorem where it is needed:

* `nLab <https://ncatlab.org/nlab/show/sequent+calculus>`__ defines a substitution rule/theorem. There is a theorem that substitution rules can be eliminated from the proof tree, proven by taking the proof tree for :math:`\Gamma \vdash \Delta` and replacing all its identities :math:`x \vdash x` with identities :math:`t \vdash t`. This requires :math:`t \vdash t` to hold, hence we include it. If the identity rule is not used with ``x`` in the proof tree, then the identity rule is not needed for the substitution, but such a situation is unlikely.
* Quantifiers also require the identity rule, because cut elimination applies substitution of ``x`` for ``t`` in the proof tree where ``x`` is a variable.

Cut elimination for quantifiers is sound because the number of quantifiers in the sequent decreases.

Logic translations
==================

First we must define classical and intuitionistic logic. To define classical logic we simply add standard structural weakening and contraction rules to our linear logic. Then :math:`A\otimes B \equiv A \land B`, :math:`A\par B \equiv A \lor B`, and we obtain the usual classical logic with modalities :cite:`lafontLinearLogicPages`; all the connectives decompose into or are equivalent to the standard ones. To define intuitionistic logic we take classical logic and restrict the right hand side of all sequents to have at most one consequent; various pi/sigma connectives cannot be used as they would create multiple consequents, and similarly right contraction cannot be used. We allow disallow right weakening to make the translation easier.

The translation from intuitionistic logic to linear logic decorates every proposition and subproposition with !. :cite:`dicosmoIntroductionLinearLogic2015`

.. math::

  \left[\prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right)\right]_I &= \prod \limits_{i} \left(\overrightarrow{\bang\left[A_i\right]_I} \multimap \overrightarrow{\bang\left[B_i\right]_I}\right)

  \left[\sum \limits_{i} \left(\overrightarrow{A_i} - \overrightarrow{B_i}\right)\right]_I &= \sum \limits_{i} \left(\overrightarrow{\bang\left[A_i\right]_I} - \overrightarrow{\bang\left[B_i\right]_I}\right)

We can translate classical logic into intuitionistic logic by decorating every proposition and subproposition with :math:`\neg\neg` and moving the right to the left with another negation, i.e. :math:`\Gamma \vdash \Delta \Rightarrow \Gamma', \neg \Delta' \vdash`. Thus the translation of classical logic into linear logic decorates like :math:`\neg \bang (\neg \bang A) \equiv \whim \bang A`.

These two decoration translations preserve proof structure, in the sense that every intuitionistic/classical proof tree can be converted to a linear logic proof tree, and the reverse as well if the linear logic proof tree's sequent is the result of the proposition translation. There are other "uniform" translations, like in :cite:`danosStructureExponentialsUncovering1993`, but they aren't as simple.

Definitions
===========

I didn't find any relevant papers on defining new notation for expressions in the sequent calculus. So we have to prove consistency ourselves. But I think the cut elimination theorem poses no problem, the key and commutative cases are trivial.

The identity rule fails if the notation expands via an infinite chain of definitions :math:`A_1 = \ldots A_2 \ldots, A_2 = \ldots A_3 \ldots = \ldots`. For example for Russell's paradox and the related :math:`\{x : x \in x\}  \in \{x : x \in x\} = \{x : x \in x\}  \in \{x : x \in x\} = \ldots`. Hence we exclude such circular definitions by requiring the identity theorem to complete for all notations.

For the substitution theorem goes through with the same restriction on notation. The proof works by replacing variable identities :math:`x \vdash x` with more complex identities :math:`A \vdash A`.

Non-circularity is a pretty loose restriction. If we know a definition is size-decreasing, we can induct as usual to prove the identity theorem: use the basic identity theorem on non-definition subtrees, use the definition rule on both sides for each definition, and continue switching between the two until it's built up. Hence we only have to be careful for definitions like sets that can increase size when expanded.

Set theory
==========

The naive set theory definition comes from :cite:`shirahataLinearSetTheory1994` (page 10). The rest of the definitions are similar to the ones in :cite:`shulmanLinearLogicConstructive2018` except :math:`\in` is not affirmative.

.. _paradoxes:

Paradoxes
---------

It seems from playing with some examples that forbidding circular definitions is sufficient to prevent Russell's paradox and Curry's paradox. For example with :math:`R = \{x\mid \whim \neg(x \in x)\}`, :math:`\{x\mid F \} \in R` is defined (and provable) but :math:`R \in R` is circular hence not defined. So we cannot write the premise of Russell's paradox. We could try to work around this with a proposition like :math:`t\in R \land t= R`. This is not circular, but it is not sufficient to derive a paradox, as in order to apply contraction we have to use a substitution that produces :math:`R \in R`. Curry's paradox :math:`X=\{x\mid x\in x \to Y\}` similarly contains :math:`x\in x` and hence the notation :math:`X \in X` is circular and therefore forbidden as well.

More formally, suppose the logic is inconsistent, i.e. there is a derivation :math:`\vdash \bot`. This must be derived from the empty sequent. Then what does cut elimination do? Either it completes, in which case we get a contradiction because no rule derives the empty sequent, or there's some infinite chain of cut elimination. I claim the infinite chain only happens if there is a circular definition involved and the identity or substitution rules are on the sides. Hence, forbidding circular definitions in the identity and cut rules solves the issue.

The question of whether a given set comprehension is defined is undecidable, as we can encode the lambda calculus and hence the halting problem - the beta rule :math:`(\lambda x. A) t` does the same substitution as :math:`t\in\{x\mid A\}`. We can approximate definedness with a termination checking algorithm, type system, or syntactic check:

* Strict comprehension, i.e. the bound variable can only appear once in the formula :cite:`shirahataLinearSetTheory1998` Very restrictive.
* New Foundations's stratified formulas :cite:`forsterQuineNewFoundations2019` :cite:`holmesElementarySetTheory1998` NFU + Infinity + Choice is known to be consistent with the theory of types with the Axiom of Infinity, and is a subtheory of ZFC + "there is an n-Mahlo cardinal for each concrete natural number n". But the stratification is restrictive, e.g. we cannot define a set of sets that contain themselves even though this definition is well-founded.
* Hindley-Milner type inference (since the simply typed lambda calculus terminates). Seems like a reasonable check.
* A size-checking algorithm like in :cite:`jonesCallbyvalueTerminationUntyped2008`. Seems to have unpredictable behavior, but may solve some things HM can't.
* Brute-force expansion. Also somewhat unpredictable.
* Intersection type system. Typeable iff terminating, but undecidable in general and tricky to approximate. But should be strictly better than HM.

There is also :cite:`shirahataLinearConservativeExtension1996` which allows sets built from ZF's axioms.

Equality
--------

The axioms of reflexivity, substitution, etc. can take a variety of modalities as in :cite:`coniglioEqualityLinearLogic2002`, some of them corresponding with intuitionistic and classical notions of equality. For sets we use linear weak extensional equality. Alternatively we could use intuitionistic equality :math:`A\overset{!}{=}B \defeq !(A=B)`, then substitution is :math:`A\overset{!}{=}B, \phi \vdash \phi[A/B]`. But the linear equality seems more useful.

Proof of the substitution property: For :math:`\Pi` we use the right rule to split into cases for each tag, then we use contraction/weakening on :math:`\bang(A=B)` to match the number of A's/B's in the case, then the left rule to split into each A and B, giving each branch a copy of the hypothesis. :math:`\Sigma` is similar but with the left first. For exponentials, quantifiers, and set comprehension we simply do left/right in the correct order. Then at the end we use the hypothesis to change :math:`A[x/a]` on the left or right to :math:`B[x/b]`, or else weakening to remove the hypothesis followed by the identity.

Recursion and infinite structures
=================================

There is the question of representing recursion, as standard derivations only allow finite (well-founded) proofs.Sequent Core :cite:`downenSequentCalculusCompiler2016` introduces Let/MultiCut and Rec, which "serve two purposes: to give a shared name to the result of some computation, and to express (mutual) recursion."

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

But these rules are illogical and inadmissible. For example with Let/MultiCut, one can conclude from :math:`A,B \vdash B,C` and :math:`A,B \vdash B,C` (provable via Ax) that :math:`A \vdash C`. Similarly Rec concludes from :math:`A,B,C,D \vdash B, D, E, F` and :math:`A,B,C,E \vdash C, D, E, F` (again provable via Ax) that :math:`A,D,E \vdash B,C,F`.

Alternately let can be encoded as a record and recursion via a fixed-point combinator or a cycle in the graph. In particular :cite:`kiselyovManyFacesFixedpoint2013` outline a polyvariadic combinator:

::

  fix_poly fl = fix (\self -> map ($ self) fl)

To implement ``fix`` we can use the variant of the Y combinator :math:`\lambda f.(\lambda x.x x) (\lambda x.f (x x))`. To type it we need the cyclic/recursive type :math:`Wr = \Pi[(^w, Wr, r)]` (in the sense of an infinite, regular tree). BOHM uses a fan/duplication node combined with a loop.

Graph reduction
---------------

A better method, following :cite:`jonesImplementationFunctionalProgramming1987` chapter 12, is to
In Stroscot, we instead simply allow (regular) infinite proof trees. We construct "infinite" as a terminal coalgebra - our proof trees turn into fixed points of systems of formal equations :cite:`karazerisFinalCoalgebrasAccessible2011`, as opposed to actually being infinite. We represent the system of equations explicitly with the use-def rules, which also allow naming computations. Although the regular restriction means we can't represent some values directly such as the Fibonacci sequence, most of the types we care about are regular, like the lambda calculus or lists, and similarly some infinite values like ``x = 1 : x``.

Infinite structures can be paradoxical, e.g. we can prove :math:`\vdash\bot` using cut on the proposition :math:`A=\neg A`. Cut elimination will often fail to complete, but there is a progress property in the sense that the cut can always be pushed down and eliminate an identity rule or two matching logical rules.

 is better than  because it allows the graph reduction method used in GHC to work.


These probably aren't needed, the use-def and infinite structures and types encode recursion better and we can use GHC's graph reduction model (below).


 give each definition node a static integer. Then the root is a distinguished definition. Assuming the static data is stored on disk and paged in/out as needed, we can minimize runtime memory use in a compiler pass by introducing as many use-def indirections as possible, one for every sequent in the derivation. This also makes the connections between rules uniform. But having lots of indirections is inefficient so a later pass would remove indirections that will be immediately used (chunkification).

The optimal fixedpoint algorithm outlined in :cite:`shamirFixedpointsRecursiveDefinitions1976` (10.18, PDF pages 240-242) is a variation of Tarjan's strongly connected component algorithm. Cuts between two definitions ``f x`` are memoized in a list, and if the SCC algorithm finds a component ``f x -> let g = ... in g (f x)`` then this component is solved. If it has a unique solution then that's the answer, otherwise ``f x`` diverges and is replaced with a ``RecursionError`` or ``AmbiguousError``. We assume the solver allows uninterpreted "holes", so that the SCC can be solved before its sub-computations.

For comparison, to compute the least fixed point we would maintain a "working graph" and incrementally unfold the definition when encountered. But with the optimal fixed point we first reduce the definition to a value while copying other definitions in.

The solver is an SMT solver on the predicate ``SAT(y == g y)``, and for uniqueness ``UNSAT(y == g y && y != y0)`` where ``y0`` is the first solution found. We exclude error values as possible solutions since the recursion error will be more informative.

The posets the paper uses appear to be pointed directed-complete partial orders `(cppo's) <https://en.wikipedia.org/wiki/Complete_partial_order>`__.

Hashing
=======

To hash the graphs we can use the tree structure of the sequent derivations. Each upward slot in a node is hashed with a fixed value and each downward slot is hashed with a value corresponding to the path through the derivation tree followed by the label of the upward slot. It is written as a single DFS traversal with the leaves as base case that stores the hashed subtree and a map from edge name to partial path.

Hashing infinite graphs is harder, we have to hash each SCC as a unit. See :cite:`mauborgneIncrementalUniqueRepresentation2000`.

Primitives
==========

Primitives (integers) can be handled by hacking special cases into Cut; we add primitive functions of type PiR that use the arguments provided by PiL during a cut, and also literals, special values of type SigmaR. Alternately we can use a specialized proof trees: 64-bit integers are represented as a sigma type with 2^64 possibilities. So addition is represented as a case expression, where each case contains another case expression, and then each case constructs the integer corresponding to the addition. There is a lot of fan-out at each step, which would require 2^128 values to represent, clearly infeasible. So although this is the conceptual representation, the actual representation has no fan-out for the cases - instead the case nodes create symbolic variables ``a`` and ``b``, and the constructed value has the tag ``a+b``.

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