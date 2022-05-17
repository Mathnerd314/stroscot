.. _Verification:

Verification
############

Stroscot aims to be a practical programming language, but it also aims to provide strong guarantees about program behavior. Verification is the process of verifying that a system satisfies a property. Static verification and symbolic execution is a fairly natural extension of unit testing, and much more powerful. Building it into the language with a standardized API and UX will allow many amazing programs to emerge.

Still, static analysis is incomplete - some programs will be too complex to categorize as wrong or right. Being conservative means to reject these programs as wrong, with an error. Stroscot is not conservative by default - it will simply output a warning, even if it discovers a definite bug in the program. But bugs and incomplete analyses can be treated as errors with ``-Werror``, or conversely completely suppressed.

Scalability and undecidability
==============================

Verification suffers from *extreme* scalability limitations. The combinations of program states increase exponentially, state space explosion. Midori says: "The theorem proving techniques simply did not scale for our needs; our core system module took over a day to analyze using the best in breed theorem proving analysis framework!" In general there is no fix: the properties we are interested in are non-trivial, so by Rice's theorem they are undecidable. Because of the undecidability no sound algorithm is complete - there are always programs which an algorithm will be unable to analyze.

To get around this there are various tricks:

* coarsen: combine "equivalent" program states into abstract program states, where equivalence is defined relative to the properties we are checking
* brute force: check individual states really fast, so larger state spaces can be checked in a given amount of time
* smart fuzzing: change the order states are explored, so that the failure mode is found earlier, before we run out of time or memory. But here we are not exploring the full state space
* approximate: check a property that implies the property we interested in

In practice, we can't check deep properties on 200KLOC, but we can affordably verify them on 2KLOC. And meanwhile we can check "shallow" properties on arbitrarily large codebases without a state space explosion. There is a lot of room for optimization and this will likely be an area of development after Stroscot gets popular.

Implementation
==============

The implementation in Stroscot will be "from scratch" - custom SAT/SMT solver, custom state space explorer, etc. The main reason is to avoid the overhead present in existing tools of translating to/from the constraint specification languages such as SMTLIB or specialized XML formats. But it will use the techniques from various existing implementations.

model checkers:
[CBMC](https://www.cprover.org/cbmc/)

The state-of-the-art seems to be the ULTIMATE framework that does abstract interpretation of the program via Buchi automata. CPAChecker has also done well in SV-COMP using an extension of dataflow analysis.


Verdi, Ironfleet, JSCert, Cosette, FSCQ, Chapar, CertiKOS, Linksem, miTLS and HACL*, Versat and IsaSAT
CakeML

Concolic (Concrete-Symbolic) testing or dynamic symbolic execution: DART, CUTE, KLEE, [NASA’s Java Pathfinder](https://github.com/javapathfinder), jCUTE, SAGE


Another goal: allow creating custom optimizations with formal proofs of their correctness

errors produce a concrete program trace of a failing path, which should be easy to turn into a good error message or even allow interactive debugging.

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
  * Unmodeled parts of the system; e.g. IO operations ``Read 1`` for a read that returned 1 or ``Write`` for a write.
  * Havoc operations, similar to unmodeled operations
* A concrete transition function :math:`\mathord{\transconc{}} \subseteq C \times Ops \times C` defining a (labeled) transition relation of how concrete states evolve into other concrete states. There is at most one concrete state succeeding a given concrete state and program operation, but we allow halting states with no available operations and a state to evolve differently with different operations. We define the notation :math:`\mathord{\transconc{o}} = \{ (c,o,c') \in \mathord{\transconc{}} \}`. We write :math:`c \transconc{o} c'` if :math:`(c, o, c') \in \mathord{\transconc{}}` and :math:`c \transconc{} c'` if there exists an :math:`o` with :math:`c \transconc{o} c'`.

A concrete path :math:`\sigma = \langle (c_1, o_1 , c_2 ), (c_2 , o_2 , c_3 ), \ldots , (c_{n-1} , o_{n-1} , c_n ) \rangle` is a sequence of consecutive concrete states. A concrete path is called a program path if it starts with the initial state :math:`c_I`. A path is called feasible if the transitions are concrete transitions, :math:`c_i \transconc{o_i} c_{i+1}`; paths are assumed to be feasible unless declared infeasible. A state :math:`c` is called reachable if there exists a feasible program path from :math:`c_I` to :math:`c`.

Dealing with concrete states will immediately lead to state explosion. So we introduce abstract states, that are sets of concrete states, and abstract operations, that are sets of concrete operations. An abstract domain :math:`D = ({\cal E}, G, \leadsto)` consists of

* a set :math:`{\cal E} \subseteq 2^C` of abstract states
* a set :math:`G \subseteq 2^{Ops}` of abstract operations.
* a transfer relation :math:`\leadsto \subseteq E × G × E`  of (labeled) abstract state transitions. We define :math:`\overset{g}{\leadsto}`, :math:`s \leadsto s'`, and abstract paths and reachability, in a manner similar to concrete states.

We have to tie this to our program. The domain *covers* the program if each reachable concrete state is contained in some abstract state in :math:`{\cal E}` and each operation encountered during a feasible path is contained in some abstract operation in :math:`G`. The domain is *compatible* with the program if :math:`(e,g,e')\in\leadsto \iff \exists c\ in e, c' \in e', o \in g. c,o,c' \in \mathord{\transconc{}}`.

To support loop acceleration we could extend our notion of compatibility to allow mapping multiple concrete state transitions to one abstract transition. But which abstract state would the intermediate concrete states map to? It seems better to model loop acceleration as a transformation on the concrete state transition graph that is reflected into a transformation on the abstract state graph.

The simplest covering domain is :math:`({C},{Ops})`. Slightly more complicated is the domain containing an abstract state for each program location. But the real meat lies in creating an abstract domain with complicated predicates on concrete states.

CPAChecker algorithm
--------------------

* A transfer operator that identifies successor abstract states to a given abstract state as well as their abstract operations, :math:`t : E → 2^{(E,G)}`.

* a merge operator :math:`\merge :  E × E → E` specifies if and how to merge abstract states when control flow meets. The operator weakens/widens the abstract state that is given as second parameter depending on the first parameter. Note that the operator :math:`\merge` is not commutative, and is not necessarily the same as the join operator of the lattice. :math:`e' \subseteq \merge(e, e') \subseteq \top`. Two simple ones are :math:`\merge_{sep}(e,e')=e'` and :math:`\merge_{join}(e,e')=e \cup e'`.

* The termination check :math:`\stopop : E × 2^E \to \{Stop,Continue\}` checks whether the abstract state :math:`e` that is given as first parameter is covered by the set :math:`R` of abstract states given as second parameter. Usually this is :math:`\stopop_{join}(e, R) = e \subseteq \bigcup R` but we can also use :math:`\stopop_{sep}(e, R) = \exists e' \in R . e \subseteq e'`.

Properties
==========

Rice's theorem shows that a program cannot evaluate a nontrivial property of another program with perfect accuracy. But it does not prevent a program from evaluating a nontrivial property with possible outputs Yes/No/IDK to a reasonable level of accuracy. So all of these properties are done on a "best-effort" basis, where it is a bug in the compiler if the analysis returns IDK, but such bugs are unlikely to be fixed on a reasonable timeframe.

The most common property is membership in a set (bound checks, type safety, etc.). But there are "temporal" properties which cannot be described as sets - liveness, termination etc.

Reachability
------------

A reachability (safety) task consists of a program annotated with a set of error states, with the goal to show that the error states are unreachable, or otherwise to find a feasible program path to an error state. This can be used to verify assertions and check for type errors.

To prove unreachability we exhibit a covering domain with no concrete error states in any of the abstract states. To prove reachability we produce a concrete feasible path ending in an error state. The counterexample can then be fed into a debugger to determine what changes to make to the program.

Assertions
~~~~~~~~~~

Side-effect free.

Assertions written inline, for sanity-checking. Not documented.

assert - error if trace exists where expression is false, omitted if compiler can prove true, otherwise runtime check with error if expression evaluates to false,
assume expr - prunes traces where expression is false. backtracking implementation at runtime.

Regarding debug-only assertions that are only checked in debug builds, in practice the unconditional "release" assertions are more useful/common than debug. Policies like “checked in debug” versus “checked in release” don't belong in a programming language. If you really want a debug-only check, you can say ``if(DEBUG) { assert X }``.

Imagine you're designing a car and put in air bags. You test the car and the air bags in all sorts of configurations and they work great and are much safer. But just as you're getting ready to go into production to send the car out to consumers, you take out all the airbags. That's what debug-only assertions are like.

``assert`` is deeply special, since it has to work with descriptions of executable properties, so unfortunately not all programs/properties will be statically resolvable.

Assertions are strongly recommended for reliability and defensive coding.

Invariants are just assertions in loop bodies.

Assertions have a simple form ``assert expr`` that throws ``AssertionFailed``.

Java's complex form ``assert expr : exception`` that throws ``exception`` on failure seems pointless.

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

Equivalence of pure programs is based on comparing results over all possible inputs.

Equivalence of I/O programs is based on comparing events: we represent all I/O actions in a datatype and then compare as for pure programs.

In the literature there is a notion of bisimulation. But here our state transition graph includes computation transitions, while the amount of computation is not relevant for equivalence. But of course bisimulation implies equivalence.

One use for equivalence is finding dead or redundant code. For example, if the program is equivalent when commenting out a line of code, or if a boolean expression is reversed.

Supercompilation
----------------

Supercompilation produces an output program with observable behavior equivalent to an input program but faster.  Essentially we are transforming abstract states into pieces of code, creating a term in the output for every intermediate state.

The algorithm in :cite:`bolingbrokeSupercompilationEvaluation2010` is similar to that of CPAChecker. There is a termination check that takes a list of states and a state and either stops or continues - in particular it stops if any previously examined states are less than the current state by a well-quasi-order. Reduction produces successor states as with the transfer operator; as an optimization they skip merging/termination checking "intermediate" states. Another difference is that they are compiling pure programs so there is a "splitting" operation that transforms a state into a composition of substates. They are evaluating to full normal form rather than WHNF, so there is some nondeterminism in the evaluation order.

Incremental program analysis
----------------------------

Another issue is incremental analysis. Checking is slow so we would like to re-use most of the analysis when recompiling a file. Looking at a 2019 presentation :cite:`jakobsDifferentialModularSoftware` there doesn't seem to be any major breakthrough. Marking the analyzer's computation steps in the general incremental build framework is probably sufficient.

since you can check all these conditions it's a very powerful analysis that can also check buffer overflows and array bounds and resource use :cite:`albertResourceAnalysisDriven2019` and things of that nature.

Optimizations
=============

A `talk <http://venge.net/graydon/talks/CompilerTalk-2019.pdf>`__ by Graydon Hoare on compilers mentions the paper :cite:`allenCatalogueOptimizingTransformations1971`. He says we need 8 optimization passes to get 80% of the performance:

* Common subexpression elimination - This starts from atomic expressions / closed connected components and then works up to identify opportunities for sharing. Because of unsharing fans it can share parents regardless of their other children; this doesn't increase the graph size and may decrease code size/computation. Since the graph may be cyclic we need a partitioning algorithm like in :cite:`mauborgneRepresentationSetsTrees1999`.
* Inlining - Going through :cite:`peytonjonesSecretsGlasgowHaskell2002`, a lot of the cases are handled by partial evaluation / optimal reduction that moves cuts down and exposes/eliminates case statements. But we also want to do it inside recursive functions etc., which means we need a strictness/termination analysis.
* Constant Folding - partial evaluation of the code includes this
* Loop unrolling, code motion - These are optimizations on mutable variables, so will have to wait until a mutability story is worked out. But unrolling recursive functions could prove useful, as part of inlining.
* Dead code elimination - Unused expressions aren't connected to the main graph and so are trivially eliminated. But we also want to eliminate conditional branches that will never be taken; this requires a reachability analysis.
* Peephole - this is instruction selection for the backend. LLVM might help, or find a JIT library.

SAT solving
===========

For SAT, conflict driven clause learning (CDCL) seems to be the most powerful algorithm for solving systems of complex clauses. It is based on assuming specific states for each variable based on each requirement and then, when a conflict is encountered, creating a new requirement from the clause and backtracking. There are extensions of it to nonlinear real systems :cite:`brausseCDCLstyleCalculusSolving2019`, and one paper/PhD on using CDCL for termination checking :cite:`dsilvaConflictdrivenConditionalTermination2015`.

SAT solving can be recast as proving a sequent :math:`C_1, \ldots, C_n \vdash \bot` with clauses :math:`C_i = (a_1 \land \ldots \land a_n \to b_1 \lor \ldots \lor b_m)`. Resolution is just the cut rule (although resolution-based solving are different from CDCL).

The conversion to CNF uses properties of classical reasoning. In the intuitionistic case, every formula can be transformed into an equiprovable sequent :math:`\Gamma_i, \Gamma_f \vdash d` with :math:`d` an atom, :math:`\Gamma_f` made of flat clauses as in the :math:`C_i` above, and implication clauses :math:`(a \to b) \to c`.

There are definitions of resolution for fragments of linear logic, and linear logic theorem provers.

CFG
===

So, executing this model some amount of loops, we get a tree of executions. Going deeper in the tree extends the execution, and the tree branching is due to the nondeterministic choices at the beginning of each iteration of the loop. We can turn this tree into a graph by grouping nodes using an `equivalence relation <https://en.wikipedia.org/wiki/Equivalence_relation>`__ that determines if the behavior is the same for two executions. This forms the control flow graph that we need for verification. In particular we want to verify that observable behavior of the program is not affected by the choices of the scheduler, as defined by I/O. For example, equivalent executions must write the same files and the same contents to the files, but not necessarily in the same order. But really it is up to the user to decide, maybe writing in a different order is bad.

