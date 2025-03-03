---
math:
  '\rule': '{ \dfrac{\displaystyle ~~#1~~ }{\displaystyle ~~#2~~ } \  (#3)}'
  '\defeq': '{\overset{\text{def}}{=}}'
  '\with': '{\mathbin{\mathrm{\&}}}'
  '\par': '{\mathbin{\mathrm{⅋}}}'
  '\multimapboth': '{\mathbin{\mathrm{⧟}}}'
  '\bang': '{{\mathrm{!}^+}}'
  '\whim': '{{\mathrm{!}^-}}'
  '\ocin': '{\mathrel{\raise{-1pt}{\mathrm{!}}\mathord{\in}}}'
  '\sk': '{{\color{mygray} #1}}'
---

mygray}{RGB}{156,156,156}
# Logic

The logic for Stroscot is based on a two-sided linear logic sequent calculus.

## Jumbo connectives

```{math}

\begin{array}{cc}
\rule{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta }}
  {\Gamma \vdash \mathop{𝕁^-} \limits_{i} \left(\overrightarrow{A_i} \vdash \overrightarrow{B_i}\right), \Delta }
  {𝕁^-_R}
&
\rule{\overrightarrow{ \sk{\Gamma_j} \vdash A_{i j}, \sk{\Delta_j} } \quad \overrightarrow{ \sk{\Theta_k}, B_{i k} \vdash \sk{\Lambda_k} }}
  {\sk{\overrightarrow{\Gamma}}, \sk{\vec \Theta}, \mathop{𝕁^-} \limits_{i} \left(\overrightarrow{A_i} \vdash \overrightarrow{B_i}\right) \vdash \sk{\overrightarrow{\Delta}}, \sk{\vec\Lambda}}
  {𝕁^-_{L} {}_{i}}
\end{array}
```

```{math}

\begin{array}{cc}
\rule{\overrightarrow{ \sk{\Theta_j} \vdash A_{i j}, \sk{\Lambda_j} } \quad \overrightarrow{ \sk{\Gamma_k}, B_{i k} \vdash \sk{\Delta_k} }}
  {\sk{\overrightarrow{\Gamma}}, \sk{\overrightarrow{\Theta}} \vdash \mathop{𝕁^+} \limits_{i} \left( \overrightarrow{A_i} \vdash \overrightarrow{B_i} \right), \sk{\overrightarrow{\Delta}}, \sk{\overrightarrow{\Lambda}}}
  {𝕁^+_{R} {}_{i}}
&
\rule{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta } }
  {\Gamma, \mathop{𝕁^+} \limits_{i} \left ( \overrightarrow{A_i} \vdash \overrightarrow{B_i} \right ) \vdash \Delta }
  {𝕁^+_L}
\end{array}
```

### Common connectives

All of the standard operators 01⊕⊗⊤⊥&⅋⊸⧟ in linear logic can be expressed using $𝕁^+$ and $𝕁^-$. We use our notation for them.

```{list-table}
  :header-rows: 1
  :widths: 1,1,2,4
  :width: 100%

  * - Operator
    - Girard Notation
    - Name
    - Type
  * - :math:`F`
    - :math:`0`
    - False (Zero)
    - :math:`𝕁^+ []`
  * - :math:`1`
    - :math:`1`
    - One
    - :math:`𝕁^+ [(\#s,[] - [])]`
  * - :math:`A \lor B`
    - :math:`A \oplus B`
    - Plus (coproduct, or)
    - :math:`𝕁^+ [(\#l,[A] - []),(\#r,[B] - [])]`
  * - :math:`A \otimes B`
    - :math:`A \otimes B`
    - Times (tensor product)
    - :math:`𝕁^+ [(\#s,[A,B] - [])]`
  * - :math:`A^{\otimes n}`
    -
    - `Tensor power <https://en.wikipedia.org/wiki/Tensor_algebra#Construction>`__
    - :math:`𝕁^+ [(\#s,\overbrace{[A,\ldots,A]}^n - [])]`
  * - :math:`\smash{\stackrel{+}{\neg}} A`
    - :math:`A^{\bot}`
    - Positive Negation
    - :math:`𝕁^+ [(\#s,[] - [A])]`
  * - :math:`{↑}A`
    - :math:`A`
    - Up shift
    - :math:`𝕁^+ [(\#s,[A] - [])]`
  * - :math:`T`
    - :math:`\top`
    - True (Top)
    - :math:`𝕁^- []`
  * - :math:`\bot`
    - :math:`\bot`
    - Bottom (contradiction)
    - :math:`𝕁^- [(\#s,[] \multimap [])]`
  * - :math:`A \land B`
    - :math:`A \with B`
    - With (product, and)
    - :math:`𝕁^- [(\#l,[] \multimap [A]),(\#r,[] \multimap [B])]`
  * - :math:`A \par B`
    - :math:`A \par B`
    - Par ("unless", classical or, parallel product, dual of tensor)
    - :math:`𝕁^- [(\#s,[] \multimap [A,B])]`
  * - :math:`A^{\par n}`
    -
    - Par power
    - :math:`𝕁^- [(\#s,[] \multimap \overbrace{[A,\ldots,A]}^n)]`
  * - :math:`A \to B`
    - :math:`A \multimap B`
    - Lollipop (implication, internal hom)
    - :math:`𝕁^- [(\#f,[A] \multimap [B])]`
  * - :math:`A \leftrightarrow B`
    - :math:`A \multimapboth B`
    - Equivalence
    - :math:`𝕁^- [(\#l,[A] \multimap [B]),(\#r,[B] \multimap [A])]`
  * - :math:`\smash{\stackrel{-}{\neg}} A`
    - :math:`A^{\bot}`
    - Negative Negation
    - :math:`𝕁^- [(\#s,[A] \multimap [])]`
  * - :math:`{↓}A`
    - :math:`A`
    - Down shift
    - :math:`𝕁^- [(\#s,[] \multimap [A])]`
```

The negations and shifts have identical derivation rules for each polarity, so we write $\neg A$ and $\smash{\updownarrow}A$ unless there is a need for the distinction.

The specific derivation rules can be derived from the jumbo connective rules and the above definitions. For ease of reference, here are the derivation rules for the common connectives:

```{math}

\begin{array}{rr}
 { \text{N/A} \  (F_R)} & \rule{}{\Gamma, F \vdash \Delta }{F_L} \\
 \rule{}{\Gamma \vdash T, \Delta }{T_R} & { \text{N/A} \  (T_L)} \\

 \rule{}{\vdash 1}{1_R} & \rule{\Gamma \vdash \Delta}{\Gamma, 1 \vdash \Delta }{1_L} \\
 \rule{\Gamma \vdash \Delta}{\Gamma \vdash \bot, \Delta }{\bot_R} & \rule{}{\bot \vdash}{\bot_L} \\

 \rule{\Gamma \vdash A_i, \Delta}{\Gamma \vdash A_1 \lor A_2, \Delta }{\lor_{iR}} &
  \rule{\Gamma, A \vdash \Delta \quad \Gamma, B \vdash \Delta}{\Gamma, A \lor B \vdash \Delta}{\lor_L} \\
 \rule{\Gamma \vdash A, \Delta \quad \Gamma \vdash B, \Delta}{\Gamma \vdash A \land B, \Delta}{\land_R} &
  \rule{\Gamma, A_i \vdash \Delta}{\Gamma, A_1 \land A_2 \vdash \Delta }{\land_{iL}} \\

 \rule{\Gamma \vdash A, \Delta \quad \Theta \vdash B, \Lambda}{\Gamma, \Theta \vdash A \otimes B, \Lambda, \Delta}{\otimes_R} &
  \rule{\Gamma, A, B \vdash \Delta}{\Gamma, A \otimes B \vdash \Delta }{\otimes_L} \\
 \rule{\Gamma \vdash A, B, \Delta}{\Gamma \vdash A \par B, \Delta }{\par_R} &
  \rule{\Gamma, A \vdash \Delta \quad \Theta, B \vdash \Lambda}{\Gamma, \Theta, A \par B \vdash \Lambda, \Delta}{\par_L} \\

 \rule{\Gamma_1 \vdash A, \Delta_1 \quad \ldots \quad \Gamma_n \vdash A, \Delta_n}{\Gamma_1, \ldots, \Gamma_n \vdash A^{\otimes n}, \Delta_1, \ldots, \Delta_n}{{\otimes n}_R} &
  \rule{\Gamma, \overbrace{A,\ldots,A}^n \vdash \Delta}{\Gamma, A^{\otimes n} \vdash \Delta }{{\otimes n}_L} \\
 \rule{\Gamma \vdash \overbrace{A,\ldots,A}^n, \Delta}{\Gamma \vdash A^{\par n}, \Delta }{{\par n}_R} &
 \rule{\Gamma_1, A \vdash \Delta_1 \quad \ldots \quad \Gamma_n, A \vdash \Delta_n}{\Gamma_1, \ldots, \Gamma_n, A^{\par n} \vdash \Delta_1, \ldots, \Delta_n}{{\par n}_L} \\

 \rule{\Gamma, A \vdash \Delta}{\Gamma \vdash \neg A, \Delta }{\neg_R} &
 \rule{\Gamma \vdash A, \Delta}{\Gamma, \neg A \vdash \Delta }{\neg_L} \\
 \rule{\Gamma \vdash A, \Delta}{\Gamma \vdash \smash{\updownarrow}A, \Delta }{\smash{\updownarrow}_R} &
 \rule{\Gamma, A \vdash \Delta}{\Gamma, \smash{\updownarrow}A \vdash \Delta }{\smash{\updownarrow}_L} \\

 \rule{\Gamma, A \vdash B, \Delta}{\Gamma \vdash A \to B, \Delta }{\to_R} &
 \rule{\Gamma \vdash A, \Delta \quad \Theta, B \vdash \Lambda}{\Gamma, \Theta, A \to B \vdash \Lambda, \Delta }{\to_L} \\
 \rule{\Gamma, A \vdash B, \Delta \quad \Gamma, B \vdash A, \Delta}{\Gamma \vdash A \leftrightarrow B, \Delta }{\leftrightarrow_R} &
 \rule{\Gamma \vdash A, \Delta \quad \Theta, B \vdash \Lambda}{\Gamma, A \leftrightarrow B \vdash \Delta }{\leftrightarrow_{\to L}} \\
  & \rule{\Gamma \vdash B, \Delta \quad \Theta, A \vdash \Lambda}{\Gamma, A \leftrightarrow B \vdash \Delta }{\leftrightarrow_{\leftarrow L}}
\end{array}
```

### Programming types

We can also write some types common from programming:

```{eval-rst}
.. list-table::
   :header-rows: 1
   :widths: auto

   * - Operator
     - Name
     - Type
   * - :math:`\text{Bool}`
     - Booleans
     - :math:`𝕁^+ [(\#F,[]-[]),(\#T,[]-[])]`
   * - :math:`\text{Int}`
     - 32-bit integers
     - :math:`𝕁^+ [(\#{-2}^{31},[]-[]),\ldots,(\#0,[]-[]),\ldots,(\#2^{31}-1,[]-[])]`
   * - :math:`L_A`
     - Linked list of A
     - :math:`𝕁^+ [(\text{#nil},[]-[]),(\text{#cons},[A,L_A]-[])]`
   * - :math:`Arr_A`
     - Arbitrarily-sized tuple of A
     - :math:`𝕁^+ [(\text{#0},[]-[]),(\text{#1},[A]-[]),(\text{#2},[A,A]-[]),\ldots]`
```

In general $𝕁^+$ can represent any algebraic data type.

## Exponentials

### Promotion

```{math}

\begin{array}{cc}
  \rule{\overrightarrow{\bang \Gamma_i } \vdash A, \overrightarrow{\whim\Delta_i} }{\overrightarrow{\bang \Gamma_i } \vdash \bang A, \overrightarrow{\whim\Delta_i}}{\bang_R}
  &
  \rule{\overrightarrow{\bang \Gamma_i } , A\vdash \overrightarrow{\whim\Delta_i} }{\overrightarrow{\bang \Gamma_i }, \whim A \vdash \overrightarrow{\whim\Delta_i}}{\whim_L}

\end{array}
```

### Dereliction

```{math}

\begin{array}{cc}
  \rule{\sk{\Gamma}, A \vdash \sk{\Delta} }{\sk{\Gamma}, \bang A \vdash \sk{\Delta}}{\bang d}
& \rule{\sk{\Gamma} \vdash A, \sk{\Delta} }{\sk{\Gamma} \vdash \whim A, \sk{\Delta}}{\whim d}
\end{array}
```

### Weakening

```{math}

\begin{array}{cc}
    \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang A \vdash \sk{\Delta}}{\bang w}
  & \rule{\sk{\Gamma} \vdash \sk{\Delta} }{\sk{\Gamma} \vdash \whim A, \sk{\Delta}}{\whim w}
  \end{array}
```

### Contraction

```{math}

\begin{array}{cc}
    \rule{\sk{\Gamma}, \overrightarrow{\bang A, \bang A, \cdots} \vdash \sk{\Delta} }{\sk{\Gamma}, \bang A \vdash \sk{\Delta}}{\bang c_n}
  & \rule{\sk{\Gamma} \vdash \overrightarrow{\whim A, \whim A, \cdots}, \sk{\Delta} }{\sk{\Gamma} \vdash \whim A, \sk{\Delta}}{\whim c_n}
\end{array}
```

### Admissible rules

The following rules are derivable from the four rules above.

Weak promotion, implied by promotion and dereliction:

```{math}

\begin{array}{cc}
    \rule{\Gamma \vdash A, \Delta }{\bang \Gamma \vdash \bang A, \whim \Delta}{\bang_\text{weak}}
    & \rule{\Gamma, A \vdash \Delta }{\bang \Gamma, \whim A \vdash \whim \Delta}{\whim_\text{weak}}
\end{array}
```

Digging is simply the theorems $\bang \bang A \equiv \bang a$ and $\whim \whim A \equiv \whim a$, but we present the sequent forms for completeness. It is implied by promotion and dereliction.

```{math}

\begin{array}{cc}
    \rule{\Gamma, \bang \bang A \vdash \Delta }{\Gamma, \bang A \vdash \Delta}{\bang_\text{dig}}
    & \rule{\Gamma \vdash \whim \whim A, \Delta }{\Gamma \vdash \whim A, \Delta}{\whim_\text{dig}}
\end{array}
```

Weak promotion and digging together imply promotion.

Absorption is implied by contraction and dereliction:

```{math}

\begin{array}{cc}
    \rule{\Gamma, A, \bang A \vdash \Delta }{\Gamma, \bang A \vdash \Delta}{\bang_\text{absorb}}
    & \rule{\Gamma \vdash A, \whim A, \Delta }{\Gamma \vdash \whim A, \Delta}{\whim_\text{absorb}}
\end{array}
```

Multiplexing is implied by absorption and dereliction:

```{math}

\begin{array}{cc}
    \rule{\Gamma, A, \ldots, A \vdash \Delta }{\Gamma, \bang A \vdash \Delta}{\bang_\text{multiplex}}
    & \rule{\Gamma \vdash A, \ldots, A, \Delta }{\Gamma \vdash \whim A, \Delta}{\whim_\text{multiplex}}
\end{array}
```

## Structural rules

### Exchange

$$
\rule{\sk{\Gamma} \vdash \sk{\Delta}}{\sk{\sigma_L(\Gamma)} \vdash \sk{\sigma_R(\Delta)}}{\text{x}}
$$

### Cut

$$
\rule{\sk{\Gamma} \vdash A, \sk{\Delta} \quad \sk{\Theta}, A \vdash \sk{\Lambda} }{\sk{\Gamma}, \sk{\Theta} \vdash \sk{\Delta}, \sk{\Lambda} }{\text{cut}}
$$

### Identity

$$
\rule{}{A \vdash A}{\text{id}}
$$

## Higher-order logic

### Predicates

Terms consist of variables $x$, literal values $v$, and applications of terms to terms. Variables quantify over the universal set, smaller domains can be defined by the membership notation in {ref}`set-theory`.

If the term is a predicate (set) then it may be used as an atomic formula. Such an atomic formula $t$ must satisfy the identity rule:

$$
\rule{}{t \vdash t}{\text{id}}
$$

Interpreted predicates may be defined using other rules so long as the identity rule is derivable and the rules are consistent.

### Substitution

$A[\overrightarrow{x \mapsto t}]$ stands for the proposition $A$ where all free occurrences of the variables $\overrightarrow{x}$ have been replaced by terms $\overrightarrow{t}$ in the appropriate domains (and bound variables have been renamed to fresh ones when necessary).

> ```{math}
> >
> \begin{array}{c}
>   \rule{t\vdash t\quad\Gamma \vdash \Delta}{\Gamma[\overrightarrow{x \mapsto t}] \vdash \Delta[\overrightarrow{x \mapsto t}]}{\text{sub}}
> \end{array}
> ```

### Quantifiers

For these the variable $x$ must have no free occurrence in $\Gamma$ or $\Delta$. In code we simply refer to quantifiers $Q$ and write $Q^+ = \forall, Q^- = \exists$.

```{math}

 \begin{array}{cc}
    \rule{\Gamma \vdash A, \Delta}{\Gamma \vdash \forall x. A, \Delta}{\forall_R}
    &
    \rule{t\vdash t\quad\Gamma, A[x\mapsto t] \vdash \Delta}{\Gamma, \forall x. A \vdash \Delta}{\forall_L}
    \\
    \rule{t\vdash t\quad\Gamma \vdash A[x\mapsto t], \Delta}{\Gamma \vdash \exists x. A, \Delta}{\exists_R}
    &
    \rule{\Gamma, A \vdash \Delta}{\Gamma, \exists x. A \vdash \Delta}{\exists_L}
  \end{array}
```

## Definitions

If we define some notation $A\defeq B$, then this means adding a pair of rules to our logic:

```{math}

  \begin{array}{cc}
    \rule{\Gamma \vdash B, \Delta}{\Gamma \vdash A, \Delta}{\text{def}_R}
    &
    \rule{\Gamma, B \vdash \Delta}{\Gamma, A \vdash \Delta}{\text{def}_L}
  \end{array}
```

The notation and rules are valid only if there is a finite proof of the identity sequent $B \vdash B$.

(infinite)=

## Infinite proof structures

These have "use" and "def" rules ("def" is short for definition). The use is a "hole" that plugs in the derivation tree from the definition. The type of the use rule can performs a substitution on the free variables of the type of the assignment.

$$
\begin{array}{cc}
  \rule{X }{ \Gamma[\overrightarrow{x \mapsto t}] \vdash \Delta[\overrightarrow{x \mapsto t}] }{\text{Use}}
  &
  \rule{\Gamma \vdash \Delta}{ X = }{\text{Def}}
\end{array}
$$

(set-theory)=

## Set theory

We define $t\in \{x\mid A\} \defeq A[x\mapsto t]$. Here the elements $t$ of the sets are propositions; e.g. we can prove $\bot \in \{x\mid x \leftrightarrow \bot \}$. Usually the variables in set theory range over sets. Hence we introduce new variables $x^S$ which range over sets. Since all sets $S$ are of the form $\{x\mid X_S \}$ these can be translated as follows:

$$
x^S \defeq \{x\mid X_S \}

\{x^S\mid X\} \defeq \{X_S \mid X\}

\{x\mid X\} \in Y \defeq X \in Y

\forall x^S. X \defeq \forall X. X

\{x^S\mid X\} \subseteq \{x^S\mid Y\} \defeq X \to Y
$$

So for example $t^S\in \{x^S\mid x^S \in x^S \}$ expands to $T \in \{X \mid X \in \{x\mid X\} \}$

We can also define set-builder notation $\{a_1,\ldots,a_n\} = \{x\mid x = a_1 \lor \ldots \lor x=a_n\}$.

We define

$$
a \ocin B = \bang(a \in B)

\left[ P(x) \vdash_{x\in A} Q(x) \right] = \left[ \bang(x\in A), P(x) \vdash Q(x) \right]

\exists x\in A. P(x) = \exists x.(\bang(x \in A) \otimes P(x))

\forall x\in A. P(x) = \forall x. (\bang(x\in A) \to P(x))
$$

We can prove the axiom schema of comprehension $\vdash \exists y. \forall x. x\in y \leftrightarrow \phi$ for all formulas $\phi$ with free variable $x$.

### Equality

Equality on sets is defined as follows:

$A=B \defeq \forall x. (x \in A \leftrightarrow x \in B)$.

We can easily prove that this equality is an equivalence relation:

$$
\vdash A=A

A=B\vdash B=A

A=B,B=C\vdash A=C
$$

For substitution, we can prove $!(A=B), \phi \vdash \phi[A/B]$ for any specific proposition $\phi$.
