Compiler design
###############

Core
====

Stroscot takes after Haskell in that all of the language is compiled to a small "core" language. Considerations:

* Based on :cite:`downenSequentCalculusCompiler2016a`, we use the full two-sided sequent calculus with cuts instead of an intuitionistic or one-sided calculus.
* Based on optimal reduction, mostly :cite:`guerriniTheoreticalPracticalIssues1996`, we use linear sequents, with operators for contraction (duplication) and weakening (erasing).
* Based on :cite:`levyJumboLCalculus2006`, we combine all of the different linear logic operators into two jumbo operators: plus-of-times :math:`\Sigma` and with-of-lollipop :math:`\Pi` (the extended lollipop includes par as well).

.. math::

    \newcommand{\rule}[3]{ \dfrac{\displaystyle ~~#2~~ }{\displaystyle ~~#3~~ } & (#1)}
    \begin{array}{clcl}
    \rule{\Sigma_{i} {}_{R}}{
    \overrightarrow{ \Gamma_j, A_{i j} \vdash \Delta_j } }{
    \overrightarrow{\Gamma} \vdash \sum \limits_{i} \left( \overrightarrow{A_i} \right), \overrightarrow{\Delta}}
    &
    \rule{\Sigma_L}{
    \overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \Delta } }{
    \Gamma, \sum \limits_{i} \left ( \overrightarrow{A_i}\right ) \vdash \Delta }
    \\
    \rule{\Pi_R}{
    \overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta } }{
    \Gamma \vdash \prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right), \Delta }
    &
    \rule{\Pi_{i} {}_{L}}{
    \overrightarrow{ \Gamma_j \vdash A_{i j}, \Delta_j } \quad \overrightarrow{ \Theta_k, B_{i k} \vdash \Lambda_k } }{
    \overrightarrow{\Gamma}, \vec \Theta, \prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right) \vdash \overrightarrow{\Delta}, \vec\Lambda}
    \end{array}

These have all the power of ADTs and normal functional programming, for example some Peano arithmetic:

::

  data Nat = Z | S *
  module {
    2 = S (S Z)
    3 = S 2
    plus a b = cut a | case
      Z -> ax b
      S s -> S (cut s b | plus)
  }

Pipeline
========

The start is a parser - this will be written later once partial evaluation is sufficient to specialize naive parsers efficiently. For now the code is input using ADTs and parentheses. The parser will also add token start/end and other debugging information.

Next is the fexpr interpreter loop. This starts with the ADT tree and produces evaluated code. Parts of the evaluator include turning name-strings into direct evaluation graph references and compiling pattern matching to tag scrutinization.

Currying is handled by a pass that creates partially-applied functions using the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`. Initially all user code starts out using one-argument functions.

Currently there are no code targets implemented - the main interactive element is an interpreter. There are some papers on partial evaluation and supercompilation that will probably get used for a C backend or a JIT or something.
