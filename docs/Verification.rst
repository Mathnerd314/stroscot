Verification
############

SMT solvers have become very powerful. but termination checking is still in its infancy. It is a difficult (undecidable) task. The state-of-the-art seems to be the ULTIMATE framework that does abstract interpretation of the program via Buchi automata. CPAChecker has also done well in SV-COMP using an extension of dataflow analysis.

SAT solving
===========

For SAT, conflict driven clause learning (CDCL) seems to be the most powerful algorithm for solving systems of complex clauses. It is based on assuming specific states for each variable based on each requirement and then, when a conflict is encountered, creating a new requirement from the clause and backtracking. There are extensions of it to nonlinear real systems :cite:`brausseCDCLstyleCalculusSolving2019`, and one paper/PhD on using CDCL for termination checking :cite:`dsilvaConflictDrivenConditionalTermination2015`.

TODO: conversion to CNF

Software verification
=====================

Verification is the process of verifying that, indeed, a system satisfies its specification. Verification techniques include model checking, deductive machine-checked proof, and randomized testing (fuzzing).

Model-checking does not refer to a model of the system but to the formal logic sense: an interpretation 𝜑 in some semantic domain that satisfies the formula F (𝜑 ⊨ F) is a model for F. A model checker *checks* if a given 𝜑 is a *model* for a given F.

The problem is this: *all* verification methods that verify arbitrary “deep” properties suffer from *extreme* scalability limitations (there are deep theoretical reasons for that). In practice, we can affordably verify such properties on ~2000 lines of formal specification. We can't check deep properties on an actual program of 200KLOC.
Correct/Cheap/Fast - pick 2. You can give up speed and only verify small programs. Or give up correctness and verify shallow properties. Or give up cheapness and verify high-level abstractions of the code.

model checkers:
[CPAChecker](https://cpachecker.sosy-lab.org/)
[NASA’s Java Pathfinder](https://github.com/javapathfinder)
[JBMC](http://www.cprover.org/jbmc/)
[CBMC](https://www.cprover.org/cbmc/)
[SCADE](https://www.ansys.com/en-gb/products/embedded-software/ansys-scade-suite)





Configurable Program Analysis
=============================

.. raw:: html

  <div style="display: none">
  \[
  \newcommand{\true}{\mathit{true}}
  \newcommand{\false}{\mathit{false}}
  \newcommand{\seq}[1]{{\langle #1 \rangle}}
  \newcommand{\sem}[1]{[\![ #1 ]\!]}
  \newcommand{\setsem}[1]{\bigcup_{e \in #1} \sem{e}}
  \newcommand{\locs}{\mathit{L}}
  \newcommand{\op}{\mathit{op}}
  \newcommand{\pc}{\mathit{pc}}
  \newcommand{\pcvar}{\mathit{pc}}
  \newcommand{\pco}{\mathit{pc_0}}
  \newcommand{\pce}{\mathit{pc_{err}}}
  \newcommand{\meet}{\sqcap}
  \newcommand{\cpa}{\mathbb{D}}
  \newcommand{\Nats}{\mathbb{N}}
  \newcommand{\Bools}{\mathbb{B}}
  \newcommand{\Ints}{\mathbb{Z}}
  \newcommand{\strengthen}{\mathord{\downarrow}}
  \newcommand{\transconc}[1]{\smash{\stackrel{#1}{\rightarrow}}}
  \newcommand{\transabs}[2]{\smash{\stackrel[#2]{#1}{\rightsquigarrow}}}
  \newcommand{\merge}{\mathsf{merge}}
  \newcommand{\stopop}{\mathsf{stop}}
  \newcommand{\wait}{\mathsf{waitlist}}
  \newcommand{\reached}{\mathsf{reached}}
  \newcommand{\result}{\mathsf{result}}
  \newcommand{\compare}{\preceq}
  \renewcommand{\implies}{\Rightarrow}
  \newcommand{\BUG}{{\sc fa}}
  \newcommand{\flag}{\mathit{flag}}
  \newcommand{\Itp}[3]{\smash{\mbox{\sc Itp}{(#2,#3)(#1)}}}
  \]
  </div>

A *configurable program analysis* (CPA) specifies --- independently of the analysis algorithm ---
the abstract domain and a set of operations that control the program analysis.
Such a CPA can be plugged in as a component into the software-verification framework
without the need to work on program parsers, exploration algorithms, and
their general data structures.

A *program* is represented by a *control-flow automaton* (CFA) / Kripke structure :math:`(\locs, l_o, G)`,
which consists of

* a set :math:`Ops` of program operations. Operations include:

  * Assignment of the form ``x := e``. The set of program variables that occur in operations from :math:`Ops` is denoted by :math:`X`, so :math:`x \in X`. :math:`e` is a (side-effect free) expression over variables from :math:`X`.
  * I/O operations
  * A test :math:`[p]` with a predicate :math:`p` over variables from :math:`X`. This is used for branching.
* A concrete state :math:`(c, l) \in C = (X \to V) \times \locs` is a pair of:

  * A *concrete data state*, a variable assignment :math:`c \in X \to V` that assigns to each variable a value :math:`V`; a simple model takes :math:`V = \Ints`.
  * A program location :math:`l \in \locs`. There is an initial program location :math:`l_I` (models the program entry)

The concrete transition relation :math:`\mathord{\transconc{}} \subseteq C \times Ops \times C` defines a (labeled) transition relation of how concrete states evolve into other concrete states. We define the notation :math:`\mathord{\transconc{o}} = \{ (c,o,c') \in \mathord{\transconc{}} \}`. We write :math:`c \transconc{o} c'` if :math:`(c, o, c') \in \mathord{\transconc{}}` and :math:`c \transconc{} c'` if there exists an :math:`o` with :math:`c \transconc{o} c'`. There is typically only one concrete state succeeding a given concrete state, but outside input and multi-threaded programs make the next state non-deterministic.

A path :math:`\sigma = \langle (c_1, o_1 , c_2 ), (c_2 , o_2 , c_3 ), \ldots , (c_{n-1} , o_{n-1} , c_n ) \rangle` is a sequence of consecutive concrete states. A path is called a program path if it starts in the initial location :math:`l_I`. A path is called feasible if the transitions are concrete transitions, :math:`c_i \transconc{o_i} c_{i+1}`. A location :math:`l` is called reachable if there exists a feasible program path from :math:`l_I` to :math:`l`.

A verification task consists of a CFA and an error location, with the goal to show that the error location is unreachable, or otherwise to find a feasible program path to the error location.

Dealing with concrete states is infeasible so the semantics can instead be defined by the `strongest-postcondition operator <https://en.wikipedia.org/wiki/Predicate_transformer_semantics#Strongest_postcondition>`__. After an assignment operation the variable must contain the value of the expression evaluated on the old value, and after an assume operation the assertion must be true. A set of concrete states is called a *region*. A first-order formula :math:`\psi` over variables from :math:`X` and possible locations in :math:`\locs` defines the region :math:`\sem{\psi} = \{ c \mid c \models \psi \}`.

A CPA :math:`\mathbb{C} = (D, \leadsto, \merge, \stopop)` consists of 4 elements:

* an abstract domain :math:`D = (C, {\cal E}, \sem{\cdot})`, consisting of

  * a set :math:`C` of concrete states,
  * a bounded join `semi-lattice <https://en.wikipedia.org/wiki/Semilattice>`__ :math:`({\cal E}, \sqsubseteq, \sqcup, \top, \bot)` over abstract-domain elements, and
  * a concretization function :math:`\sem{\cdot} : E \to 2^C` that maps each abstract-domain element to its represented set of concrete states. It should satisfy :math:`\sem{\top} = C`, :math:`\sem{\bot} = \emptyset`, :math:`e \sqsubseteq e' \to \sem{e} \subseteq \sem{e'}`, :math:`\sem{e \sqcup e'} \supseteq \sem{e} \cup \sem{e'}`

* a transfer relation :math:`T \subseteq E × G × E` computes abstract successor states. It assigns to each abstract state :math:`e` possible new abstract states :math:`e'` that are abstract successors of :math:`e`. Similarly to the CFA each transfer is labeled with a control-flow edge :math:`g`, so we have :math:`\overset{g}{\leadsto}` as well as :math:`\leadsto` derived from :math:`T`.

* a merge operator :math:`\merge :  E × E → E` specifies if and how to merge abstract states when control flow meets. The operator weakens the abstract state (also called widening) that is given as second parameter depending on the first parameter. Note that the operator :math:`\merge` is not commutative, and is not necessarily the same as the join operator of the lattice. The result of :math:`\merge(e, e')` can be anything between :math:`e'` and :math:`\top`. Two simple ones are :math:`\merge_{sep}(e,e')=e'` and :math:`\merge_{join}(e,e')=e \sqcup e'`
* a termination check :math:`\stopop : E × 2^E \to B` checks whether the abstract state :math:`e` that is given as first parameter is covered by the set :math:`R` of abstract states given as second parameter, i.e., every concrete state that :math:`e` represents is represented by some abstract state from :math:`R`. Two simple termination checks are :math:`\stopop_{sep}(e, R) = \exists e' ∈ R : e \sqsubseteq e'` and :math:`\stopop_{join}(e, R) = e \sqsubseteq \bigsqcup R`. The second requires :math:`D` to be a power-set domain.


Incremental program analysis
----------------------------

Another issue is incremental analysis. Solving the halting problem is slow so we would like to re-use most of the analysis when recompiling a file. Looking at a 2019 presentation :cite:`jakobsDifferentialModularSoftware` there doesn't seem to be any major breakthrough. Marking the analyzer's computation steps in the general incremental build framework is probably sufficient.

Condition checking
------------------

There's some interesting `work <http://mmjb.github.io/T2/>`__ on termination checking by Microsoft, called `TERMINATOR <https://web.archive.org/web/20131005142732/http://research.microsoft.com:80/en-us/um/cambridge/projects/terminator/papers.htm>`__. There's a representation of terms as sets, which ends up mapping out all the paths through the program, and then identifying termination is fairly easy. But since you can check all these conditions it's a very powerful analysis that can also check buffer overflows and array bounds and resource use :cite:`albertResourceAnalysisDriven2019` and things of that nature.

Optimizations
=============

A `talk <http://venge.net/graydon/talks/CompilerTalk-2019.pdf>`__ by Graydon Hoare on compilers mentions the paper :cite:`allenCatalogueOptimizingTransformations1971`. He says we need 8 optimization passes to get 80% of the performance:

* Common subexpression elimination - This starts from atomic expressions / closed connected components and then works up to identify opportunities for sharing. Because of unsharing fans it can share parents regardless of their other children; this doesn't increase the graph size and may decrease code size/computation. Since the graph may be cyclic we need a partitioning algorithm like in :cite:`mauborgneRepresentationSetsTrees1999`.
* Inlining - Going through :cite:`peytonjonesSecretsGlasgowHaskell2002`, a lot of the cases are handled by partial evaluation / optimal reduction that moves cuts down and exposes/eliminates case statements. But we also want to do it inside recursive functions etc., which means we need a strictness/termination analysis.
* Constant Folding - partial evaluation of the code includes this
* Loop unrolling, code motion - These are optimizations on mutable variables, so will have to wait until a mutability story is worked out. But unrolling recursive functions could prove useful, as part of inlining.
* Dead code elimination - Unused expressions aren't connected to the main graph and so are trivially eliminated. But we also want to eliminate conditional branches that will never be taken; this requires a reachability analysis.
* Peephole - this is instruction selection for the backend. LLVM might help, or find a JIT library.
