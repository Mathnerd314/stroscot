Verification
############

SMT solvers have become very powerful. but termination checking is still in its infancy. It is a difficult (undecidable) task. The state-of-the-art seems to be the ULTIMATE framework that does abstract interpretation of the program via Buchi automata. CPAChecker has also done well in SV-COMP using an extension of dataflow analysis.

For SAT, conflict driven clause learning (CDCL) seems to be the most powerful algorithm for solving systems of complex clauses. It is based on assuming specific states for each variable based on each requirement and then, when a conflict is encountered, creating a new requirement from the clause and backtracking. There are extensions of it to nonlinear real systems, and one recent paper/PhD on using it for termination checking.

Another issue is incremental analysis. Solving the halting problem is slow so we would like to re-use most of the analysis when recompiling a file. Marking the solver's computation steps in the general incremental computation framework is probably sufficient.

