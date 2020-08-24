Compiler design
###############

Pipeline
========

The start is a parser - this will be written later once partial evaluation is sufficient to specialize naive parsers efficiently. For now the code is input using ADTs and parentheses. The parser will also add token start/end and other debugging information.

Next is the fexpr interpreter loop. This starts with the ADT tree and produces evaluated code. Parts of the evaluator include turning name-strings into direct evaluation graph references and compiling pattern matching to tag scrutinization.

Currying is handled by a pass that creates partially-applied functions using the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`. Initially all user code starts out using one-argument functions.

Currently there are no code targets implemented - the main interactive element is an interpreter. There are some papers on partial evaluation and supercompilation that will probably get used for a C backend or a JIT or something.

Optimizations
=============

A `talk <http://venge.net/graydon/talks/CompilerTalk-2019.pdf>`__ by Graydon Hoare on compilers mentions the paper :cite:`allenCatalogueOptimizingTransformations1971`. He says we need 8 optimization passes to get 80% of the performance:

* Common subexpression elimination - This starts from atomic expressions / closed connected components and then works up to identify sharing. Because of unsharing fans it can share parents regardless of their other children; this doesn't increase the graph size and may decrease code size/computation. Since the graph may be cyclic we need a partitioning algorithm like in :cite:`mauborgneRepresentationSetsTrees1999`.
* Inlining - Going through :cite:`peytonjonesSecretsGlasgowHaskell2002`, a lot of the cases are handled, because of the graph structure and because partial evaluation / optimal reduction will move cuts down and expose/eliminate case statements. But we also want to do it inside recursive functions etc., which means we probably need a strictness/termination analysis.
* Loop unrolling, code motion - These are optimizations on mutable variables, probably unnecessary. But unrolling recursive functions could prove useful, as part of inlining.
* Constant Folding - partial evaluation of the code includes this
* Dead code elimination - Unused expressions aren't connected to the main graph and so are trivially eliminated. But we also want to eliminate conditional branches that will never be taken; this requires a reachability analysis.
* Peephole - this is instruction selection for the backend. LLVM might help, or find a JIT library.

Flags
=====

In general flags can take 4 levels: ignore, warn, error, and fix. Ignore ignores the issue as much as possible. Warn issues a warning but otherwise ignores the issue. Error stops the compiler from continuing. Fix automatically constructs a fix for the issue and modifies the source file(s) in-place.

Error messages
==============

Since Stroscot uses model checking, most failures will end up producing a counterexample. The counterexample may not be minimal, but it is much easier to debug a concrete instance than to try to figure one out from contextual information.

For source locations we produce the start/end span of two (filename, line number, column number) tuples. Go uses an efficient memory-map-like model from these tuples to integers, to avoid passing around strings. We could just use pointers, but Go's integers are ordered so comparing within files is faster. But it isn't clear how to make this incremental.

Output
======

The simplest output is to write out a copy of the interpreter with a comment "This is generated code - see Interpreter.hs for source" at the top. But we want to specialize this.

versioning of time/date
identifier minimization/translation
unit test
random input testing
quasiquotation
typechecking