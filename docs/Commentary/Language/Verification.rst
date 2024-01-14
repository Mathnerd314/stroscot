Verification
############

.. epigraph::

   1B. The language shall be designed to [...] maximize automatic detection of programming errors.
   1E. There shall be no language restrictions that are not enforceable by translators.

   -- Steelman

.. epigraph::

   The attitude today is that you can write any sloppy piece of code and the compiler will run diagnostics. If it doesn’t spit out an error message, it must be done correctly.

   -- `Peter G. Neumann <https://www.technologyreview.com/2002/07/01/40875/why-software-is-so-bad/>`__

.. epigraph::

   Compile-time static checking is wonderful, and as much as possible should be done 100% statically so that people cannot write incorrect programs.

   -- `Linus Torvalds <https://lkml.org/lkml/2022/9/19/1250>`__

.. epigraph::

   If you are willing to settle for anything less than full [static] verification of all properties then you might as well give up and use a dynamic type system.

   -- Stroscot

Stroscot aims to be a practical programming language, but it also aims to provide strong guarantees about program behavior, for example that array accesses are not out of bounds. In most cases these can be ensured statically by the verification system.

Verification is the process of verifying that a system satisfies a property. Static verification and symbolic execution is a natural extension of unit testing, and much more powerful. Building it into the language with a standardized API and UX will allow many amazingly robust programs to emerge.

Scalability
===========

Verification suffers from *extreme* scalability limitations. "State space explosion" is a thing, i.e. the combinations of program states increases exponentially. Midori says: "The theorem proving techniques simply did not scale for our needs; our core system module took over a day to analyze using the best in breed theorem proving analysis framework!" Verification is always a time and memory hog.

To get around this there are various tricks:

* coarsen: combine "equivalent" states into abstract states, where equivalence is defined relative to the properties we are checking
* approximate: check a property that implies or is implied by the property we interested in. Can give positive / negative result but failure provides no information.
* optimization: check individual states really fast, so larger state spaces can be checked in a given amount of time
* smart fuzzing: change the order states are explored. Speeds up verification of some properties, but verifying the negation still requires exploring the full state space.

TLA+ can't check deep properties on 200KLOC, but can affordably verify them on 2KLOC. And meanwhile we can check "shallow" properties on arbitrarily large codebases without a state space explosion. There is a lot of room for optimization and clever design. Per :cite:`bergeronSystemsProgrammingLanguages1972` a 50% slowdown for additional error checking (relative to e.g. C) is completely manageable.

Rice's theorem and `halting problem approximation results <https://en.wikipedia.org/wiki/Halting_problem#Approximations>`__ show that no algorithm can evaluate a nontrivial property with perfect accuracy; there is an upper limit to accuracy (below 1) that no algorithm can surpass consistently. But these results do not prevent evaluating a nontrivial property with possible outputs Yes/No/IDK to a reasonable level of accuracy on practical programs, which have structure not found in random programs. Still, the verifier is developed on a "commercially reasonable effort" basis, where it is a bug in the compiler if the analysis returns IDK on a program, but the bug will only be fixed given sufficient funding and evidence of the usefulness of the program (e.g. that it works in other languages). So verification will likely be an area of development forever. There is no panacea, but we can provide options for the programmer if the analysis fails (returns IDK):

* reject: reject programs for which the algorithm fails as wrong (like ``-Werror``)
* defer: insert runtime checks if needed and throw an error when the property fails (like Haskell's ``-fdefer-type-errors``)
* override: behave as if the algorithm gave answer A (could crash at runtime, but allows bypassing bugs).
* scripting: allow specifying tactics and proof sketches so that the verifier has hints for how to search through the state space.

For definite bugs the options are similar, although the override option implies a somewhat foolish promise that "poison" inputs will be avoided, in exchange for a minimal speedup and the loss of safety.

The seL4 microkernel (8700 lines of C) has been successfully statically modeled and verified. Linux is 7.9 million lines of C (>50% drivers) and has not been. But simple type-checking-like things are probably not the blocker; most effort in the seL4 verification went towards concurrency things like interrupts, data races, and IPC.

Implementation
==============

The implementation in Stroscot will be "from scratch" - custom SAT/SMT solver, custom state space explorer, etc. The main reasons are to avoid the overhead present in existing tools of translating to/from the constraint specification languages such as SMTLIB or specialized XML formats, and to allow Stroscot to be self-hosted. But the implementation will use the techniques from various existing implementations such as Z3.

The field of verification is quite large but is centralized around various subjects/competitions:

* model checkers: `CBMC <https://www.cprover.org/cbmc/>`__, Verdi, Ironfleet, JSCert, Cosette, FSCQ, Chapar, CertiKOS, Linksem, miTLS and HACL*, Versat and IsaSAT, CakeML
* Concolic (Concrete-Symbolic) testing or dynamic symbolic execution: DART, CUTE, KLEE, `NASA’s Java Pathfinder <https://github.com/javapathfinder>`__, jCUTE, SAGE
* SV-COMP:

    * Overall: `2LS <https://github.com/diffblue/2ls>`__, `CBMC/CProver-witness2test <https://www.cprover.org/cbmc/>`__, `CoVeriTeam-Verifier (AlgoSelection, ParallelPortfolio) <https://gitlab.com/sosy-lab/software/coveriteam>`__, `CPAchecker, CPALockator, CPA-BAM-BnB, CPA-BAM-SMG, CPA-witness2test <https://cpachecker.sosy-lab.org>`__, `DIVINE <https://divine.fi.muni.cz/>`__, `ESBMC-kind, ESBMC-incr <https://esbmc.org/>`__, `Goblint <https://goblint.in.tum.de/>`__, `Symbiotic, Symbiotic-Witch <https://github.com/staticafi/symbiotic>`__, `UAutomizer, UGemCutter, UKojak, UTaipan <https://ultimate.informatik.uni-freiburg.de>`__, `VeriFuzz/VeriAbs <https://www.tcs.com/designing-complex-intelligent-systems>`__
    * Concurrency: `CSeq <https://gitlab.com/emersonwds/cseq>`__, `Lazy-CSeq <https://github.com/omainv/cseq/releases>`__, `Dartagnan <https://github.com/hernanponcedeleon/Dat3M>`__, `Deagle <https://github.com/thufv/Deagle>`__, `EBF <https://github.com/fatimahkj/EBF>`__, `Locksmith <http://www.cs.umd.edu/projects/PL/locksmith/>`__
    * Java: `COASTAL <https://www.cs.sun.ac.za/coastal>`__, `GDart <https://github.com/tudo-aqua/gdart-svcomp>`__, `Java-Ranger <https://github.com/vaibhavbsharma/java-ranger>`__, `JayHorn <https://github.com/jayhorn/jayhorn>`__, `JBMC <https://github.com/diffblue/cbmc>`__, `JDart <https://github.com/tudo-aqua/jdart>`__, `SPF <https://github.com/SymbolicPathFinder/jpf-symbc>`__
    * Reachability: `BRICK <https://github.com/brick-tool-dev/BRICK-2.0>`__, `Crux <https://crux.galois.com/>`__, `Theta <https://github.com/ftsrg/theta>`__, `Gazer-Theta <https://github.com/ftsrg/gazer>`__, `Graves-CPA <https://github.com/will-leeson/cpachecker>`__, `LART <https://github.com/xlauko/lart>`__, `Pinaka <https://github.com/sbjoshi/Pinaka>`__, `PeSCo <https://github.com/cedricrupb/cpachecker>`__
    * Overflow checking: `Frama-C-SV <https://gitlab.com/sosy-lab/software/frama-c-sv>`__, `Infer <https://fbinfer.com/>`__
    * Loop invariants: `Korn <https://github.com/gernst/korn>`__
    * Memory safety: `PredatorHP <https://www.fit.vutbr.cz/research/groups/verifit/tools/predator-hp/>`__, `SESL <https://spencerl-y.github.io/SESL/>`__
    * Software systems: `SMACK <https://smackers.github.io/>`__

* `TermComp <https://termcomp.herokuapp.com/Y2022/>`__: TRS: `AProVE <https://aprove.informatik.rwth-aachen.de/references>`__, matchbox, MnM, `MU-TERM <http://zenon.dsic.upv.es/muterm/index.php/documentation/>`__, NaTT, NTI+cTI, TTT2 (Tyrolean Termination Tool 2); Higher-order: SOL, Wanda; C: irankfinder, LoAT; Complexity: tct_trs;
* `Rewrite engines competitions <https://web.archive.org/web/20200516055926/http://rec.gforge.inria.fr/>`__


VU - Vrije Universiteit Amsterdam

    Prof.dr. Jan Willem Klop (VU)
    Dr. Roel de Vrijer (VU)
    Drs. Jörg Endrullis (VU) - Jambox termination tool
    Dr. Clemens Grabmayer (VU)
    Drs. Helle Hvid Hansen (VU)
    Dr. Dimitri Hendriks (VU)
    Drs. Ariya Isihara (VU)
    Femke van Raamsdonk (VU)

UU - Universiteit Utrecht

    Vincent van Oostrom (UU)
    Albert Visser (UU)
    Clemens Grabmayer (UU)
    Jeroen Ketema

CWI Amsterdam

    Dr. Frank de Boer (CWI)
    Drs. Clemens Kupke (CWI)
    Prof.dr. Jan Rutten (VU/CWI)



Astrée
B. Blanchet, P. Cousot, R. Cousot, J. Feret, L. Mauborgne, A. Miné, D. Monniaux, and
X. Rival. A Static Analyzer for Large Safety-Critical Software. In Proc. of PLDI’03, San
Diego, California, USA, June 2003. ACM Press.

P. Baudin, A. Pacalet, J. Raguideau, D. Schoen, and N. Williams. CAVEAT: a Tool for
Software Validation. In DSN. IEEE Computer Society, 2002.

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

A *program* is represented by a *control-flow automaton* (CFA) / Kripke structure :math:`(C, Ops, \transconc{})`,
which consists of

* A set :math:`C` of concrete states. Many papers use a simple state model consisting of a program counter/location and a data store mapping variable names to integers.
* A set :math:`Ops` of program operations (alphabet). Typical operations include:

  * Computation, where the state evolves with no input
  * External interactions of the system; e.g. IO operations ``Read 1`` for a read that returned 1 or ``Write`` for a write.
  * Havoc operations, similar to external operations

* A concrete transition function :math:`\mathord{\transconc{}} \subseteq C \times Ops \times C` defining a (labeled) transition relation of how concrete states evolve into other concrete states. Papers usually allow at most one concrete state succeeding a given concrete state and program operation, but it is possible to be nondeterministic, and we allow halting states with no available operations and a state to evolve differently with different operations. We define the notation :math:`\mathord{\transconc{o}} = \{ (c,o,c') \in \mathord{\transconc{}} \}`. We write :math:`c \transconc{o} c'` if :math:`(c, o, c') \in \mathord{\transconc{}}` and :math:`c \transconc{} c'` if there exists an :math:`o` with :math:`c \transconc{o} c'`.

A concrete path :math:`\sigma = \langle (c_1, o_1 , c_2 ), (c_2 , o_2 , c_3 ), \ldots , (c_{n-1} , o_{n-1} , c_n ) \rangle` is a sequence of consecutive concrete states. A concrete path is called a program path if it starts with the initial state :math:`c_I`. A path is called feasible if the transitions are concrete transitions, :math:`c_i \transconc{o_i} c_{i+1}`; paths are assumed to be feasible unless declared infeasible. A state :math:`c` is called reachable if there exists a feasible program path from :math:`c_I` to :math:`c`.

Dealing with concrete states will immediately lead to state explosion. So we introduce abstract states, that are sets of concrete states, and abstract operations, that are sets of concrete operations. An abstract domain :math:`D = ({\cal E}, G, \leadsto)` consists of

* a set :math:`{\cal E} \subseteq 2^C` of abstract states
* a set :math:`G \subseteq 2^{Ops}` of abstract operations.
* a transfer relation :math:`\leadsto \subseteq E × G × E`  of (labeled) abstract state transitions. We define :math:`\overset{g}{\leadsto}`, :math:`s \leadsto s'`, and abstract paths and reachability, in a manner similar to concrete states.

We have to tie this to our program. The domain *covers* the program if each reachable concrete state is contained in some abstract state in :math:`{\cal E}` and each operation encountered during a feasible path is contained in some abstract operation in :math:`G`. The domain is *compatible* with the program if :math:`(e,g,e')\in\leadsto \iff \exists c\ in e, c' \in e', o \in g. c,o,c' \in \mathord{\transconc{}}`.

To support loop acceleration we could extend our notion of compatibility to allow mapping multiple concrete state transitions to one abstract transition. But which abstract operation would the intermediate concrete operations map to? It seems better to model loop acceleration as a transformation on the concrete state transition graph that is reflected into a transformation on the abstract state graph.

The simplest covering domain is :math:`({C},{Ops})`. Slightly more complicated is the domain containing an abstract state for each program location. But the real meat lies in creating an abstract domain with complicated predicates on concrete states.

CPAChecker algorithm
--------------------

* A transfer operator that identifies successor abstract states to a given abstract state as well as their abstract operations, :math:`t : E → 2^{(E,G)}`.

* a merge operator :math:`\merge :  E × E → E` specifies if and how to merge abstract states when control flow meets. The operator weakens/widens the abstract state that is given as second parameter depending on the first parameter. Note that the operator :math:`\merge` is not commutative, and is not necessarily the same as the join operator of the lattice. :math:`e' \subseteq \merge(e, e') \subseteq \top`. Two simple ones are :math:`\merge_{sep}(e,e')=e'` and :math:`\merge_{join}(e,e')=e \cup e'`.

* The termination check :math:`\stopop : E × 2^E \to \{Stop,Continue\}` checks whether the abstract state :math:`e` that is given as first parameter is covered by the set :math:`R` of abstract states given as second parameter. Usually this is :math:`\stopop_{join}(e, R) = e \subseteq \bigcup R` but we can also use :math:`\stopop_{sep}(e, R) = \exists e' \in R . e \subseteq e'`.

Properties
==========

The most common property is membership in a set (bound checks, type safety, etc.). But there are "temporal" properties which cannot be described as sets - liveness, termination etc.

Reachability
------------

A reachability (safety) task consists of a program annotated with a set of error states, with the goal to show that the error states are unreachable, or otherwise to find a feasible program path to an error state. This can be used to verify assertions and check for type errors.

To prove unreachability we exhibit a covering domain with no concrete error states in any of the abstract states. To prove reachability we produce a concrete feasible path ending in an error state. The counterexample can then be fed into a debugger to determine what changes to make to the program.

Exceptions
~~~~~~~~~~

The main reachability analysis figures out which exceptions a piece of code may throw. Top-level unhandled exceptions are reported as warnings.

Assertions
~~~~~~~~~~

10F. It shall be possible to include assertions in programs. If an assertion is false when encountered during execution, it shall raise an exception. [Note that assertions can be used to aid optimization and maintenance.]

Assertions have a simple form ``assert expr`` that throws ``AssertionFailed``, equivalent to ``when expr (throw AssertionFailed)``. Java's complex form ``assert expr : exception`` that throws a specific ``exception`` on failure seems pointless - it's only a little less verbose than ``when expr (throw exception)``. Could be worth it though, throw it to the standard library to decide.

!0F. It shall also be possible to include assertions, such as the expected frequency for selection of a conditional path, that cannot be verified.

This part we can ignore, Stroscot verifies everything. The example given is more like a pragma for optimization.

Dead code
~~~~~~~~~

Reachability can also find dead (unreachable) code, like unused declarations, unused variables, or unsatisfiable conditions. Code is only dead if it is unreachable on all compilation configurations, so the build configurations must be interfaced. Assertions can exercise code too.

Many exceptions are unwanted, e.g. "no patterns matched in case". Reachability can verify these are dead code.

Termination
-----------

Termination checking verifies properties like "A function call must eventually return" or "A program execution that calls malloc() must eventually call free()". An infinite state transition sequence that doesn't call free is a counterexample. Termination is a liveness property - it's different from a safety property "A call to free must be preceded by a call to malloc". It's also different from "If the program ends gracefully then all memory has been freed". A lot of programs look like ``repeat { handleCommand{} }`` and for those we can prove termination of ``handleCommand`` but not the loop. But we can prove graceful exit.

Proving termination is of undecidable complexity, but in practice we can prove termination and nontermination in many cases. We can reduce liveness to fair termination constraints ``<A, B>``, in each trace either ``A`` is true for only finitely many states or ``B`` is true for infinitely many states.

To prove termination we construct an abstract state graph of reachable states and a ranking function mapping states to some well-ordered set such that every cycle in the state graph has a transition that decreases the rank.

To prove nontermination we need an infinite path of concrete states. If the abstract state graph is finite this can be simplified to an initial path of concrete states leading to a strongly connected component of abstract states with no exits.

There's also some interesting `work <http://mmjb.github.io/T2/>`__ on termination checking by Microsoft. There's a representation of terms as sets, which ends up mapping out all the paths through the program, and then identifying termination is fairly easy.

Logic
-----

Both reachability and termination can be expressed in CTL*. There is an even more expressive language, the modal μ-calculus.

Equivalence
-----------

Since the semantics of method dispatch and concurrency are non-deterministic, we would like to verify that the program is well-defined. This takes the form of checking that all execution paths of a program produce equivalent results. It's similar to confluence but a little weaker.

Equivalence of pure programs is based on comparing the return value, and discarding exceptions.

Equivalence of I/O programs is based on comparing events: we represent all I/O actions in a datatype and then compare as for pure programs.

In the literature there is a notion of bisimulation. But here our state transition graph includes computation transitions, while the amount of computation is not relevant for equivalence. But of course bisimulation implies equivalence.

Equivalence gives a stronger notion of dead or redundant code. For example, if the program is equivalent when commenting out an I/O statement, or if all the paths of a conditional statement are the same.

Supercompilation
----------------

Supercompilation produces an output program with observable behavior equivalent to an input program but faster.  Essentially we are transforming abstract states into pieces of code, creating a term in the output for every intermediate state.

The algorithm in :cite:`bolingbrokeSupercompilationEvaluation2010` is similar to that of CPAChecker. There is a termination check that takes a list of states and a state and either stops or continues - in particular it stops if any previously examined states are less than the current state by a well-quasi-order. Reduction produces successor states as with the transfer operator; as an optimization they skip merging/termination checking "intermediate" states. Another difference is that they are compiling pure programs so there is a "splitting" operation that transforms a state into a composition of substates. They are evaluating to full normal form rather than WHNF, so there is some nondeterminism in the evaluation order.

Incremental program analysis
----------------------------

Another issue is incremental analysis. Checking is slow so we would like to re-use most of the analysis when recompiling a file. Looking at a 2019 presentation :cite:`jakobsDifferentialModularSoftware` there doesn't seem to be any major breakthrough. Marking the analyzer's computation steps in the general incremental build framework is probably sufficient.

since you can check all these conditions it's a very powerful analysis that can also check buffer overflows and array bounds and resource use :cite:`albertResourceAnalysisDriven2019` and things of that nature.

SAT solving
===========

For SAT, conflict driven clause learning (CDCL) seems to be the most powerful algorithm for solving systems of complex clauses. It is based on assuming specific states for each variable based on each requirement and then, when a conflict is encountered, creating a new requirement from the clause and backtracking. There are extensions of it to nonlinear real systems :cite:`brausseCDCLstyleCalculusSolving2019`, and one paper/PhD on using CDCL for termination checking :cite:`dsilvaConflictdrivenConditionalTermination2015`.

SAT solving can be recast as proving a sequent :math:`C_1, \ldots, C_n \vdash \bot` with clauses :math:`C_i = (a_1 \land \ldots \land a_n \to b_1 \lor \ldots \lor b_m)`. Resolution is just the cut rule (although resolution-based solving are different from CDCL).

The conversion to CNF uses properties of classical reasoning. In the intuitionistic case, every formula can be transformed into an equiprovable sequent :math:`\Gamma_i, \Gamma_f \vdash d` with :math:`d` an atom, :math:`\Gamma_f` made of flat clauses as in the :math:`C_i` above, and implication clauses :math:`(a \to b) \to c`.

There are definitions of resolution for fragments of linear logic, and linear logic theorem provers.

Thread safety
=============

Thread safety means avoiding race conditions and deadlocks. The basic model is to repeatedly execute some amount of steps of each thread in a loop. Executing this model some amount of loops, we get a tree of executions. Going deeper in the tree extends the execution, and the tree branching is due to the nondeterministic choices at the beginning of each iteration of the loop. We can turn this tree into a graph by grouping nodes using an `equivalence relation <https://en.wikipedia.org/wiki/Equivalence_relation>`__ that determines if the behavior is the same for two executions. This forms the control flow graph that we need for verification. In particular we want to verify a lack of race conditions, i.e. that observable behavior of the program is not affected by the choices of the scheduler.

Observable behavior is defined by an I/O model that interprets the actions. For example, equivalent executions must write the same files and the same contents to the files, but not necessarily in the same order. But really it is up to the user to decide, maybe writing files in a different order is bad.

Deadlock is when there is no runnable thread and the program has not exited.

