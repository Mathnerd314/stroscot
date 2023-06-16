Intermediate representation
###########################

The intermediate representation (IR) is the main workhorse of Stroscot's execution engine. In most cases, source code is loaded once and transformed into IR, and never transformed back. IR allows many things that are not possible or efficient using a source code representation, as it stores more information and avoids irrelevant syntactic details.

Goals
=====

An IR represents the initial and final states of a transformation phase in the compilation pipeline. Considering the entire compiler as a transformation, we see that the source language should be valid IR, as well as machine code. All the source constructs of the source language should be expressible 1-1 and print out exactly, and similarly there should be IR constructs to output any sequence of bytes into the resulting executable. Of course the fewer constructs the better, but considering the wide range of variation in hardware, some amount of redundancy is fine.

But compilation is insufficient, we would also like to use the IR for the interpreter. Thus every IR node should have an execution semantics. This avoids most instances of vagueness or edge cases in the specification. It also ensures that expanding high level abstractions into lots of small low-level instructions is only necessary for compilation, whereas the interpreter can use fewer, more expensive high-level instructions to get lower instruction dispatch overhead. In particular, type checks, method dispatch, and mathematical values like unbounded integers and computable reals can all benefit from having specialized operations.

In the case of interpreting machine code, we get a "literal machine" like Zed Shaw's EaRing. Unlike traditional "virtual" machines where interpreting even the lowest-level IR instruction may still take several machine code instructions, the interpreter executes each IR machine instruction as a single machine code instruction, allowing pretty efficient "pre-compiled methods". The interpreter or "literal machine" might alternatively be termed an "assembler" because it takes an "assembly language", but LLVM and Java started using the term VM a long time ago, so "machine" is more fun. Also, traditionally assemblers are compilers rather than interpreters - Stroscot allows dynamic loading of new code, running code at a REPL, and introspection such as listing constants and functions from imports. It makes learning assembly pretty fun.

Like MLIR, the IR comes in three different forms, all describing the same semantic content:

* an in-memory form. This should have a low memory footprint, although not compressed. It should be low overhead to generate from AST, analyze, and interpret directly. Transformations should be localized, meaning they only affect a small portion of the IR and only need a few memory accesses rather than a full traversal of the graph.
* a textual form. This should have all IR information so as to be suitable for debugging (lossless). It should be easily parsable so that scripting tasks do not require building the full in-memory representation (easy to read for machines). Within those constraints, it should be as human-readable as possible (easy to read for humans), and easily diff'able (avoiding redundantly repeating information). This enables a workflow where IR can be read from a file, sent through a few passes, dumped to another file, and the passes can be messed with until the output is correct.
* a serialized "bytecode" form. The main use is build system caching. It should be compact so that disk/network usage is minimized. Gzipping the textual format seems the obvious possibility, but maybe a true binary format will be more efficient. It is also important to be able to serialize snippets of IR in isolation.

Also like MLIR, the IR is a `wide-spectrum language <https://en.wikipedia.org/wiki/Wide-spectrum_language>`__, coming in various dialects, roughly ordered linearly on a spectrum of abstraction level from source-like to assembly-like. The compilation process gradually down-translates and eliminates dialects, but any set of dialects may be in use for a particular phase. The use of a common IR representation across dialects allows sharing infrastructure for certain transformations, such as constant folding, dead code elimination, and graph rewriting. It also avoids unnecessarily translating data by allowing it to be pre-specified in a usable format. The transformations are designed to be extensible, passing through unknown operations uninterpreted. Codegen can treat unknown ops as clobbering all registers, although of course producing machine code requires knowing how to emit the operation. The IR also supports metadata, so that analysis passes can add necessary annotations.

Unlike Java bytecode, the IR is target-specific. Specific instances of the IR will assume processor features and other configuration, such as microarchitecture, calling conventions, alignment requirements, etc. Practically, code is "portable" only because of the use of conditional compilation for specific platforms that papers over the differences. Java files, networking, GUI, threading, and native methods are targer-specific, LLVM IR is target-specific, even WASM has different incompatible system interfaces. Maintaining these conditionals as opaque operations at the IR level would prevent many useful optimizations, although of course the opaque format does allow certain optimization rules to only be written once.

For debugging and diagnostic purposes, it is important to define a reverse transformation back from IR to AST, so that error messages can display the relevant code. For example, inlining may expand a macro, and the error in the macro may require showing bits of the IR for context. The AST generated from the IR does not have to be the original AST, it should instead accurately depict the IR. But the IR should capture source locations and other information necessary for good error messages.

Transformations
===============

As mentioned in the goals section, the in-memory form of IR should be suitable for all of the various transformations analyses. A `talk <http://venge.net/graydon/talks/CompilerTalk-2019.pdf>`__ by Graydon Hoare on compilers mentions the paper :cite:`allenCatalogueOptimizingTransformations1971`. He says we need 8 optimization passes to get 80% of GCC/LLVM performance: Inline, Unroll (& Vectorize), CSE, DCE, Code Motion, Constant Fold, Peephole. An ideal IR makes these optimizations as cheap as possible.

Constant folding, inlining, and unrolling all fall under compile-time reduction. Specifically:

* Constant folding reduces closed expressions, like ``1+2`` to ``3``.
* Inlining replaces a term with its expansion, like a rewrite step in term rewriting. Per :cite:`peytonjonesSecretsGlasgowHaskell2002`, inlining subsumes copy propagation and jump elimination.
* Loop-unrolling is typically phrased in an iterative setting, e.g. ``for (x = 0; x < 100; x++) delete(x)`` to ``for (x = 0; x < 100; x += 2) { delete(x); delete(x+1); }``. Phrased as recursion, we are transforming ``let loop x | x >= 100 = {}; loop x = { delete x; loop (x + 1) } in loop 0`` to ``let loop2 x | x >= 100 = {}; loop2 x = { delete x; delete (x+1); loop (x + 2) } in loop2 0``. This is clearly just inlining the definition of ``loop`` inside the body of ``loop`` and then performing some simplifying reductions.


Strong reduction can reduce inside lambdas and in any order of evaluation. It needs a careful definition of the interpreter's evaluation semantics to avoid changing behavior. Strong reduction can be extended to supercompilation / partial evaluation, so that a state graph is constructed based on global information flow. There are several issues with reduction:

* duplication - reduction can duplicate expressions or contexts. Optimal sharing graphs avoid this duplication.
* name capture - the no-shadowing strategy: maintain the set of in-scope variables, and rename any bound variable for which there is an enclosing binding. Main advantage is idempotency. Another strategy is a graph representation, no names in the first place.
* termination - Cut elimination on finite typed terms is terminating, but other forms of reduction such as TRS reduction are not, so in general reduction is Turing-complete. Reduction consumes compile time and may speed up runtime by avoiding work or slow it down by bloating code. It's not useful on on cold expressions. Bounding the number of reduction steps to normal form, via an ordering metric, might give a good estimate of reduction cost. Bounds like a maximum term depth and number of reduction steps avoid bloating, but are somewhat arbitrary and have to be stored in the IR to be idempotent. GHC uses loop-breakers for definition cycles, but again is somewhat arbitrary. It's possible to prove non-termination or divergence of expansion, then it's clear that no further work is useful. Another technique is to record the set of all observed states in an E-graph, then loops are obvious.

Vectorization is an instruction selection task, but means that the interaction between instruction selection and reduction is non-trivial.

Peephole is an instruction selection task as well, when it does not correspond to reduction.

Common subexpression elimination is intimately related to graph reduction. Per :cite:`balabonskiUnifiedApproachFully2011`, graph reduction can be characterized as giving each term in the unshared IR a label, and using an implementation such that all terms with the same label are represented as a single object (node) and reduced as a unit. Common subexpression "elimination" is then identifying identical expressions in the IR and giving them the same label in the initial labelling. The specific technique to identify duplicate expressions is "hash-consing". Hash-consing can be applied incrementally, so that CSE can be applied continuosly as other transformations are applied. One issue is merging alpha-equivalent expressions, :cite:`maziarzHashingModuloAlphaEquivalence2021`, which can be dealt with by encoding variable backedges as paths through a spanning tree. :cite:`mauborgneRepresentationSetsTrees1999` gives an algorithm identifying sharable components in cyclic graphs, useful for recursive structures.

As optimal reduction is also a term labelling, there should be an "optimal hash-consing" technique that identifies the Levy-labelling of terms with maximal sharing. The reduction ``(\x. E[x]) e --> E[e]`` shows that it will share all identical expressions, just as CSE with graph reduction. But it will also share an expression and its reduction, hence computing the labelling is at least of complexity :math:`\Sigma^0_1` - but if we restrict to family reductions, e.g. by pre-reducing to normal form, then this is not an issue. And it is fine if the analysis is conservative and does not necessarily identify maximal sharing, just some sharing. But it should at least merging obvious shared contexts, like the function call context ``g (h [])`` in ``g (h x)`` and ``g (h y)``. Ideally, this labelling should be the result of some actual initial expression and reduction history. To show that a maximal labelling exists, we need a join property of the form "for a term+history a, and another term+history b, there is a term+history c with all equivalences from a and also those from b". Then, because the set of possible labelings is finite (or in the infinite case appealing to the term depth being a well-ordering hence infinite joins existing), the greatest element must exist as the join of all labellings. But we would also like a more efficient way to compute the labelling than brute force. Noting that the labelled beta-reduction operation only concatenates labels, we can safely replace a set of labels where no label is a prefix of another with a set of fresh distinct labels, preserving some sharing.

"Dead code elimination" is an umbrella term per ChatGPT. In GHC it refers to eliminating unused bindings. Wikipedia also lists conditional branch elimination and unreachable code elimination, which require a more involved reachability analysis.

Last is code motion.

  * induction variable analysis to replace multiplication by a loop index with addition
  * loop reversal - changing condition to zero comparison
  * loop unswitching - moving conditional outside loop
  * hoisting invariants, partial/total redundancy elimination
  * parallelization - multi-threaded or vectorized code


, loop-invariant code motion (hoisting)

* storing arrays on the heap in the most efficient of a few straightforward ways

Because of unsharing fans it can share parents regardless of their other children; this doesn't increase the graph size and may decrease code size/computation.


* Purely functional: Fixes evaluation order only for stateful operations, passes states explicitly. It is difficult to reason about imperative state mutation efficiently.
* CPS: At the lowest level, an operation is "save all processor state to memory and jump".

* Like Thorin: SSA (explicit non-local control flow)
* Like Sea of nodes: Cliff says it's fast
* Like GNU lightning: IDK, need some basic starting point for design and features of assembly opcodes

Expanding machine code instructions into unpack, mathematical operations, round/repack means that there is a lot of overhead in code generation recognizing patterns of operations as instructions. On the other hand it allows writing a fewer number of more generic and powerful optimizations, instead of many processor-specific instruction patterns. So this choice favors ahead-of-time compilation at the expense of interpretation and JITing.

Sequent Core
============

CPS does expose control flow as continuation values, but it has problems. First, per :cite:`downenSequentCalculusCompiler2016`, there is not one CPS transform, but rather a family, each CPS transform fixing an evaluation order. One must choose among call-by-value, call-by-name, or call-by-need. As a benefit, the evaluation order of the translation target doesn't matter, and strong beta-eta reduction of the CPS'd term is sound. In fact, per :cite:`okasakiCallbyneedContinuationpassingStyle1994`, all CPS translations are based on CBV, and call-by-name/call-by-need CPS translations can be decomposed as a conversion to CBV pass followed by a CBV CPS translation. IOdeally, the compiler should be able to freely choose the evaluation order, to trade-off the locality of innermost vs. the hypernormalization of outermost. Being unable to safely perform out-of-order reductions is a deal-breaker.

The CBV CPS encoding is quite annoying, e.g. :cite:`downenSequentCalculusCompiler2016` it inverts nested function calls ``map f (map g xs)`` as ``λk.map g (λh.h xs (λys.map f (λh'.h' ys k)))``. Per :cite:`maurerCompilingContinuations2016` this makes CSE harder, e.g. ``f (g x) (g x)`` vs ``g (\xv. g (\yv. f k xv yv) x) x``. Also rewrite rules are harder to apply. Even CBV has an encoding - :cite:`flanaganEssenceCompilingContinuations1993` point out that "realistic" CBV CPS compilers mark source function calls as using special continuation closures to allow efficient calls. The call-by-need transform is worse - :cite:`okasakiCallbyneedContinuationpassingStyle1994` describes how the thunk graph itself must be represented in the CPS term. It does have the benefit that the term graph is built incrementally, by gluing together subgraphs generated on demand by reduction, but the graph is still obfuscated as imperative code. :cite:`kennedyCompilingContinuationsContinued2007` states assigning names to continuations is really a benefit, but doesn't discuss the other drawbacks of the encoding.

:cite:`sabryReasoningProgramsContinuationpassing1992` demonstrated that CBV CPS was reversible, and proved that beta-eta-reduction in CPS corresponded to the A-reductions plus call-by-value reduction on the original term. Hence, per :cite:`flanaganEssenceCompilingContinuations1993`, many compilers adopted reducing the expression to A normal form between other transformations as a replacement for CPS. However, per :cite:`kennedyCompilingContinuationsContinued2007`, ANF is not closed under beta-reduction - inlining can create nested lets, which then have to be "renormalized", floated out or rearranged via "commuting conversions". Similarly, the A-reduction ``E (if x then a else b) = if x then E a else E b`` duplicates the evaluation context, and as such is commonly left out. The workaround is to introduce a "join point", the ``e`` in ``let e z = ... in if x then e a else e b``. But join points are essentially continuations, second-class in that they are represented by function bindings. Even if specifically marked, they are fragile, in that per :cite:`maurerCompilingContinuations2016` the case-of-case transformation must handle join points specially, and similarly other transformations must preserve join points (e.g. not inlining the join point. Furthermore, they are not really functions, requiring a special calling convention to compile efficiently. As Kennedy says, "Better to start off with a language that makes continuations explicit."

So both CPS and ANF suck. Fortunately, :cite:`downenSequentCalculusCompiler2016` presents Sequent Core, which retains the advantages of first-class continuations while avoiding the drawbacks. Sequent Core does not force choosing an evaluation order. A nested function application is munged a little, but the order is not inverted and CSE still works. ``Cut`` glues together graph pieces, but is itself part of the graph, hence does not need to encode its sub-graphs. Functions reduce to join points and other types of sequent, rather than the reverse. Reduction is uniformly cut-elimination, and does not require renormalization.

SSA represents code as procedures containing imperative blocks, and can't express higher-order features. But, per :cite:`appelSSAFunctionalProgramming1998`, SSA code blocks map directly to mutually recursive tail-called functions, with the procedure as a distinguished function. Indeed, the Swift Intermediate Language adopted block arguments instead of φ-nodes. SSA's basic blocks identify "small" functions that can be compiled easily, but this pass can be replicated in any IR. The other aspect of SSA, single static variable assignment, is reflected in a pass that remove mutable variables by replacing them with additional input and output arguments.

Thorin [23] is a graph-based representation aiming to support both
imperative and functional code by combining a flat structure for ease
of code transformation and first-class closures for implementing
higher-order languages. However, Thorin is still intended for use
in strict languages with pervasive side effects; it remains to be
seen whether such a representation could be adapted for high-level
optimizations in a non-strict regime such as Haskell.


Operations
==========

At the top level, an IR is a list of operation instances. An operation is identified by a name (string), an optional JSON-ish dictionary of "inherent" operation attributes, and an optional source location. Operation instances also have an optional instance-specific "discardable" attribute dictionary, used for storing type analysis and similar semantic analysis. An instance produces (return) a list zero or more result values. They receive (take) a list of zero or more operand values. An operation instance also has zero or more successor blocks, and zero or more enclosed regions, enabling terminator instructions and hierarchical structures to be represented.


Control flow
============

The ADD instruction is not so simple

Control flow graph

Blocks
======

A basic block (BB) is a sequence of instructions that is entered only from the top, and that contains no terminator instructions except for a single one at the end. The last instruction in a BB must be a terminator instruction, so execution cannot fall through the end of the BB but instead jumps to a new BB.

Terminator instructions are unconditional branches.

Per cranelift:

EBB parameter
    A formal parameter for an EBB is an SSA value that dominates everything
    in the EBB. For each parameter declared by an EBB, a corresponding
    argument value must be passed when branching to the EBB. The function's
    entry EBB has parameters that correspond to the function's parameters.

EBB argument
    Similar to function arguments, EBB arguments must be provided when
    branching to an EBB that declares formal parameters. When execution
    begins at the top of an EBB, the formal parameters have the values of
    the arguments passed in the branch.


A basic block is a mixture of jump and non-jump instructions that is complete, in the sense that any execution of the program will take one of the jumps. Any arbitrary sequence of instructions can be turned into a basic block by adding an unconditional jump at the end.

Although phi nodes were an interesting idea all the `cool kids <https://mlir.llvm.org/docs/Rationale/Rationale/#block-arguments-vs-phi-nodes>`__ are now using block arguments. Blocks arguments fit better into various analysis passes.

Blocks
======

From a user perspective there are two types of jumpable addresses:

memory - effective address computation
SIB addressing form, where the index register is not used in address calculation, Scale is ignored. Only the base and displacement are used in effective address calculation.
VSIB memory addressing



Memory and the program counter are virtualized as well, using labels. A label refers to a memory location with a specific block of code loaded. The blocks are not ordered, so unconditional jumps must be inserted between blocks if necessary. The block order can be determined using profiling, removing the unconditional jump that is taken most often.

Memory references should be virtualized as well, so we also have memory labels. The alignment and format of the memory address should be specified.

Instructions and blocks are marked by the virtual registers they consume and use (input / output registers). The call and jump instructions are special in that a mapping may be given between the virtual registers and physical registers. Instruction constraints:
* Output: the register must not contain a value used after the block
* Output early clobber: output and the register must not be used for any inputs of the block
* Input: the register is read but not written to. Multiple inputs may all be assigned to the same register, if they all contain the same value.
* Tied input: register that is read and written
* Tied input early clobber: register that is read and written and does not share a register with any other input
* alignstack, sideeffect

There are also constraints from the ABI calling convention: https://gitlab.com/x86-psABIs/x86-64-ABI

Values
======

Since all values are representable in memory, we could use bytes in the IR for values. But this would lose the type information. So instead we must support all the value types listed in :ref:`Values`.
