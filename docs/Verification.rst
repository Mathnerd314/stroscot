Verification
############

SMT solvers have become very powerful. but termination checking is still in its infancy. It is a difficult (undecidable) task. The state-of-the-art seems to be the ULTIMATE framework that does abstract interpretation of the program via Buchi automata. CPAChecker has also done well in SV-COMP using an extension of dataflow analysis.

SAT solving
===========

For SAT, conflict driven clause learning (CDCL) seems to be the most powerful algorithm for solving systems of complex clauses. It is based on assuming specific states for each variable based on each requirement and then, when a conflict is encountered, creating a new requirement from the clause and backtracking. There are extensions of it to nonlinear real systems :cite:`brausseCDCLstyleCalculusSolving2019`, and one paper/PhD on using CDCL for termination checking :cite:`dsilvaConflictDrivenConditionalTermination2015`.

TT-Open-WBO-Inc

Open-WBO-Inc-complete
Loandra
Open-WBO-Inc-satlike


UWrMaxSat	436	148.97
MaxHS	434	184.69
RC2-B	417	205.88
RC2-A	411	185.92
maxino	393	203.21

Pacose	385	197.83
QMaxSAT	377	281.47

maxino-pref	375	162.1
smax_minisat	339	182.94
smax_mergesat	282	293.93


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

A *program* is represented by a *control-flow automaton* or CFA :math:`(\locs, l_o, G)`,
which consists of

* a set :math:`\locs` of program locations/states (models the program counter :math:`\pc`)
* an initial program location/state :math:`l_0` (models the program entry)
* a set :math:`Ops` of program operations. In a simple model, :math:`Ops` are either assignment operations or assume operations. The set of program variables that occur in operations from :math:`Ops` is denoted by :math:`X`.
* a set :math:`G \subseteq \locs \times Ops \times \locs` of control-flow edges (models the operation that is executed when control flows from one program location to another).

A *concrete state* of a program is
a variable assignment :math:`c: X \cup \{\pc\} \to V`
that assigns to each variable a value :math:`V`; a simple model takes :math:`V = \Ints`.
The set of all concrete states of a program is denoted by :math:`C`.
A set :math:`r \subseteq C` of concrete states is called a *region*.
Each edge :math:`g \in G` defines a (labeled) transition relation
:math:`\mathord{\transconc{g}} \subseteq C \times \{g\} \times C`, which defines how concrete states of one program location (source) are transformed into concrete states of another program location (target).
The complete transition relation :math:`\transconc{}` is the union over
all control-flow edges:
:math:`\mathord{\transconc{}} = \bigcup_{g \in G} \transconc{g}`.
We write :math:`c \transconc{g} c'` if :math:`(c, g, c') \in \mathord{\transconc{}}`,
and :math:`c \transconc{} c'` if there exists a :math:`g` with :math:`c \transconc{g} c'`.
A concrete state :math:`c_n` is *reachable* from a region :math:`r`, denoted by :math:`c_n \in Reach(r)`, if
there exists a sequence of concrete states :math:`\seq{c_0, c_1, \ldots, c_n}`
such that :math:`c_0 \in r` and for all :math:`1 \leq i \leq n`,
we have :math:`c_{i-1} \transconc{} c_{i}`.

A CPA :math:`\mathbb{C} = (D, \leadsto, \merge, \stopop)` consists of 4 elements:

* an abstract domain :math:`D = (C, {\cal E}, \sem{\cdot})`, consisting of

  * a set :math:`C` of concrete states,
  * a bounded join `semi-lattice <https://en.wikipedia.org/wiki/Semilattice>`__ :math:`({\cal E}, \sqsubseteq, \sqcup, \top)` over abstract-domain elements, and
  * a concretization function :math:`\sem{\cdot} : E \to 2^C` that maps each abstract-domain element to its represented set of concrete states.

* a transfer relation :math:`T \subseteq E × G × E` computes abstract successor states. It assigns to each abstract state :math:`e` possible new abstract states :math:`e'` that are abstract successors of :math:`e`. Similarly to the CFA each transfer is labeled with a control-flow edge :math:`g`, so we have :math:`\overset{g}{\leadsto}` as well as :math:`\leadsto` derived from :math:`T`.

* a merge operator :math:`\merge :  E × E → E` specifies if and how to merge abstract states when control flow meets. The operator weakens the abstract state (also called widening) that is given as second parameter depending on the first parameter. Note that the operator :math:`\merge` is not commutative, and is not necessarily the same as the join operator of the lattice. The result of :math:`\merge(e, e')` can be anything between :math:`e'` and :math:`\top`. Two simple ones are :math:`\merge_{sep}(e,e')=e'` and :math:`\merge_{join}(e,e')=e \sqcup e'`
* a termination check :math:`\stopop : E × 2^E \to B` checks whether the abstract state :math:`e` that is given as first parameter is covered by the set :math:`R` of abstract states given as second parameter, i.e., every concrete state that :math:`e` represents is represented by some abstract state from :math:`R`. Two simple termination checks are :math:`\stopop_{sep}(e, R) = \exists e' ∈ R : e \sqsubseteq e'` and :math:`\stopop_{join}(e, R) = e \sqsubseteq \bigsqcup R`. The second requires :math:`D` to be a power-set domain, i.e. .

We run a CPA analysis with the following algorithm:::

  CPA(reached, wait)
  INPUT
     a CPA cpa = (D, T, merge, stopop)
     a set reached of abstract states in E (usually a single state e0)
     a set wait of frontier abstract states, a subset of reached (also e0)
  OUTPUT
     a set reached of reachable abstract states
     a set wait of frontier abstract states (empty if the algorithm terminated correctly)

  WHILE not wait.empty
    choose e from wait; remove e from wait;
    FOR each e' with T(e, e')
      FOR each e'' in reached
        // Combine with existing abstract state.
        e_new := merge(e', e'');
        IF e_new != e''
          wait    := (wait    union {e_new}) setminus e'';
          reached := (reached union {e_new}) setminus e'';
        ENDIF
      ENDFOR
      // Add new abstract state?
      IF not stop(e', reached)
        wait := wait union e';
        reached := reached union e';
      ENDIF
    ENDFOR
  ENDWHILE
  // wait is empty
  return reached

Incremental program analysis
----------------------------

Another issue is incremental analysis. Solving the halting problem is slow so we would like to re-use most of the analysis when recompiling a file. Looking at a 2019 presentation :cite:`jakobsDifferentialModularSoftware` there doesn't seem to be any major breakthrough. Marking the analyzer's computation steps in the general incremental build framework is probably sufficient.

Condition checking
------------------

There's some interesting `work <http://mmjb.github.io/T2/>`__ on termination checking by Microsoft, called `TERMINATOR <https://web.archive.org/web/20131005142732/http://research.microsoft.com:80/en-us/um/cambridge/projects/terminator/papers.htm>`__. There's a representation of terms as sets, which ends up mapping out all the paths through the program, and then identifying termination is fairly easy. But since you can check all these conditions it's a very powerful analysis that can also check buffer overflows and array bounds and resource use :cite:`albertResourceAnalysisDriven2019` and things of that nature.
