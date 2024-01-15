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

Optimizations
=============

As mentioned in the goals section, the in-memory form of IR should be suitable for all of the various transformations and optimizations. An ideal IR makes these optimizations very cheap - not free (otherwise the initial IR wouldn't be a faithful representation of the source) but not more than a few operations either. In particular having to do a scan of the entire IR for a transformation is a no-no.



* removal of unused parameters
* replacement of parameters passed by reference by parameters passed by value.
* replace standard functions with faster alternatives when possible
* inlining
* deduplication of constants, functions, code sequences (tail merging / cross jumping)
* common subexpression elimination (CSE)
* dead code/store eliminate (DCE/DSE)
* conditional dead code elimination (DCE) for calls to built-in functions that may set errno but are otherwise free of side effects
* global constant and copy propagation
* constant propagation - which values/bits of values passed to functions are constants, function cloning
* value range propagation - like constant propagation but value ranges
* sparse conditional constant propagation (CCP), including bit-level
* elimination of always true/false conditions
* move loads/stores outside loops
* loop unrolling/peeling
* loop exit test
* cross-jumping transformation
* constant folding
* specializing call dispatch (possible targets, likely targets, test/branch)
* Code hoisting - evaluate guaranteed-evaluated expressions as early as possible
* copy propagation - eliminate unnecessary copy operations
* Discover which variables escape
* partial/full redundancy elimination (PRE/FRE)
* modified/referenced memory analysis, points-to analysis, aliasing
* strips sign operations if the sign of a value never matters
* convert initialization in switch to initialization from a scalar array
* termination checking
* loop nest optimizer based on the Pluto optimization algorithms. It calculates a loop structure optimized for data-locality and parallelism.
* graphite - loop distribution, loop interchange, unroll, jam, peel, split, unswitch, parallelize, copy variables, inline to use first iteration values, predictive commoning, prefetch
* final value replacement - loop to calculation using initial value and number of loop iterations
* explode structures to scalars in registers
* vectorization - loop vectorization, basic block vectorization, cost free (for debugging), likely faster, or code size
* reorder blocks, duplicate blocks, partition into hot/cold to improve paging and cache locality
* specialization of division operations using knowledge of the denominator

Magic numbers:

* search space sizes - Increasing values mean more aggressive optimization, making the compilation time increase, but with diminishing improvement in runtime execution time. Generally a formula producing a boolean "try optimization" answer or an integer "maximum number of possibilities to consider".
* memory limit - If more memory than specified is required, the optimization is not done.
* analysis skipping - ignore objects larger than some size
* ratios - if inlining grows code by more than this, cancel inlining. tends to be overly conservative on small functions which can increase by 300%.




 A `talk <http://venge.net/graydon/talks/CompilerTalk-2019.pdf>`__ by Graydon Hoare on compilers mentions the paper :cite:`allenCatalogueOptimizingTransformations1971`. He says we need 8 optimization passes to get 80% of GCC/LLVM performance: Inline, Unroll (& Vectorize), CSE, DCE, Code Motion, Constant Fold, Peephole.

Reduction
---------

Reduction covers constant folding, inlining, and unrolling. Specifically:

* Constant folding reduces closed expressions, like ``1+2`` to ``3``.
* Inlining replaces a term with its expansion, like a rewrite step in term rewriting.  It differs from constant folding in that unevaluated expressions may be substituted, like ``2*x`` to ``x+x``. Per :cite:`peytonjonesSecretsGlasgowHaskell2002`, inlining subsumes copy propagation and jump elimination.
* Loop-unrolling is typically phrased in an iterative setting, e.g. ``for (x = 0; x < 100; x++) delete(x)`` to ``for (x = 0; x < 100; x += 2) { delete(x); delete(x+1); }``. Phrased as recursion, we are transforming ``let loop x | x >= 100 = {}; loop x = { delete x; loop (x + 1) } in loop 0`` to ``let loop2 x | x >= 100 = {}; loop2 x = { delete x; delete (x+1); loop (x + 2) } in loop2 0``. This is clearly just inlining the definition of ``loop`` inside the body of ``loop`` and then performing some simplifying reductions.

There are also user-specified rewrite rules, which make everything more complicated.

Strong reduction can reduce inside lambdas and in any order of evaluation. It needs a careful definition of the interpreter's evaluation semantics to avoid changing behavior. Strong reduction can be extended to supercompilation / partial evaluation, so that a state graph is constructed based on global information flow. There are several issues with reduction:

* duplication - reduction can duplicate expressions or contexts. Optimal sharing graphs avoid this duplication.
* name capture - the no-shadowing strategy: maintain the set of in-scope variables, and rename any bound variable for which there is an enclosing binding. Main advantage is idempotency. Another strategy is a graph representation, no names in the first place.
* termination - Cut elimination on finite typed terms is terminating, but other forms of reduction such as TRS reduction are not, so in general reduction is Turing-complete. Reduction consumes compile time and may speed up runtime by avoiding work or slow it down by bloating code. It's not useful on on cold expressions. Bounding the number of reduction steps to normal form, via an ordering metric, might give a good estimate of reduction cost. Bounds like a maximum term depth and number of reduction steps avoid bloating, but are somewhat arbitrary and have to be stored in the IR to be idempotent. GHC uses loop-breakers for definition cycles, but again is somewhat arbitrary. It's possible to prove non-termination or divergence of expansion, then it's clear that no further work is useful. Another technique is to record the set of all observed states in an E-graph, then loops are obvious.

Instruction selection
---------------------

Once the IR has been reduced as far as possible, it must be converted to machine code. Vectorization and peephole optimization are essentially instruction selection features. They do interact a bit with reduction though - some peephole optimizations can also be cast as reductions, and some reductions may make it harder to recognize opportunities for vectorization.

CSE
---

Common subexpression "elimination" is actually identifying identical expressions in the IR and giving them a shared representation in an IR graph. It is related to graph reduction, which per :cite:`balabonskiUnifiedApproachFully2011`, can be characterized as giving each term in the unshared IR a label, and using an implementation such that all terms with the same label are represented as a single object (node) and reduced as a unit.  The specific technique to identify duplicate expressions is "hash-consing". Hash-consing can be applied incrementally, so that CSE can be applied continuosly as other transformations are applied. One issue is merging alpha-equivalent expressions, :cite:`maziarzHashingModuloAlphaEquivalence2021`, which can be dealt with by encoding variable backedges as paths through a spanning tree. :cite:`mauborgneRepresentationSetsTrees1999` gives an algorithm identifying sharable components in cyclic graphs, useful for recursive structures.

Optimal CSE
-----------

As optimal reduction is also a term labelling, there should be an "optimal hash-consing" technique that identifies maximal sharing according to optimal reduction. It is a bit tricky to define this precisely because the Levy-labelling used in optimal reduction is defined with respect to an initial term. This makes it easy to compute equivalence given the reduction history, but also limits the potential equivalences to sharing inherent to the term's reduction semantics. For compile-time usage, we would like the maximal equivalence - a "hash consing" algorithm which takes an unlabelled term and produces the labelling with maximal sharing.

Following Asperti, there are three ways to define the term equivalence relation of optimal reduction:

* Levy labelling - take an initial term with unique atomic labels for every subterm, perform reductions according to a special label-generation rule. Then terms with the same labels are Levy-equivalent. The Levy labelling is the only method that labels all parts of the term, rather than just redexes. The reduction ``(\x. E[x]) e --> E[e]`` shows that optimal CSE will share all identical expressions, just as CSE with graph reduction.
* Extraction - this summarizes each redex's history as a shorter reduction sequence related to its origin
* Zig-zig relation - the smallest equivalence relation on redexes-with-history containing the "is copy of" relation (5.1.7, 5.1.8). The set of equivalent redexes is called the "family" of the redex.

The zig-zag definition is perhaps the simplest definition, we would simply like to generalize the "is copy of" relation from redexes-with-history to redexes-without-history. We can define this is-copy-of as follows:

A redex S is a copy of a redex R, written R <= S, if and only if
there is a history sigma, a history rho, and a derivation t such that rho t is permutation equivalent to sigma (rho t equiv sigma) and S is a residual of R with respect to t (S in R/t)



A redex S with history sigma is a copy of a redex R with history rho, written
rho R <= sigma S, if and only if

there is a derivation t such that rho t is permutation equivalent to sigma (rho t equiv sigma)
and S is a residual of R with respect to t (S in R/t)



  Consider each case of lambda term:

* Bound variable with bound variable: a bound variable may equate with all of its other occurrences. But since the term is reduced, it cannot unify with anything else - each unique variable must have a unique label in the initial history. For example, for ``λy.y (λx.xx)``, the two bound appearances of ``x`` may be equated from an initial term ``λx.(λz.zz)x``. But we know that ``x`` and ``y`` cannot be equated, and similarly in ``λy.λy.x y``.

* Lambda abstraction: A lambda abstraction cannot equate Suppose we have ``λx.M`` and ``λy.N``.


 To show that a maximal labelling exists, we need a join property of the form "for a term+history a, and another term+history b, there is a term+history c with all equivalences from a and also those from b".




I am not sure how to prove this but let's look at `some examples <https://cs.stackexchange.com/questions/99492/confluence-of-beta-expansion>`__ of non-confluence.

First we have ``(λx.bx(bc))c`` and ``(λx.xx)(bc)``. The first results in no sharing. The second results in ``(b^1 c^2) (b^1 c^2)``. This seems to be the maximal sharing.



(Plotkin).
(λx.a(bx))(cd)
and a((λy.b(cy))d) (Van Oostrom).


 then this is not an issue. And it is fine if the analysis is conservative and does not necessarily identify maximal sharing, just some sharing. But it should at least merging obvious shared contexts, like the function call context ``g (h [])`` in ``g (h x)`` and ``g (h y)``. Ideally, this labelling should be the result of some actual initial expression and reduction history.  Then, because the set of possible labelings is finite (or in the infinite case appealing to the term depth being a well-ordering hence infinite joins existing), the greatest element must exist as the join of all labellings. But we would also like a more efficient way to compute the labelling than brute force. Noting that the labelled beta-reduction operation only concatenates labels, we can safely replace a set of labels where no label is a prefix of another with a set of fresh distinct labels, preserving some sharing.

DCE
---

"Dead code elimination" is an umbrella term per ChatGPT. In GHC it refers to eliminating unused bindings. Wikipedia also lists conditional branch elimination and unreachable code elimination, which require a more involved reachability analysis.

Code motion
-----------

  * induction variable analysis to replace multiplication by a loop index with addition
  * loop reversal - changing condition to zero comparison
  * loop unswitching - moving conditional outside loop
  * hoisting invariants
  * partial/total redundancy elimination
  * parallelization - multi-threaded or vectorized code

* storing arrays on the heap in the most efficient of a few straightforward ways

Because of unsharing fans it can share parents regardless of their other children; this doesn't increase the graph size and may decrease code size/computation.

More on IR
==========


* Purely functional: Fixes evaluation order only for stateful operations, passes states explicitly. It is difficult to reason about imperative state mutation efficiently.
* CPS: At the lowest level, an operation is "save all processor state to memory and jump".

* Like Thorin: SSA (explicit non-local control flow)
* Like Sea of nodes: Cliff says it's fast
* Like GNU lightning: IDK, need some basic starting point for design and features of assembly opcodes

Expanding machine code instructions into unpack, mathematical operations, round/repack means that there is a lot of overhead in code generation recognizing patterns of operations as instructions. On the other hand it allows writing a fewer number of more generic and powerful optimizations, instead of many processor-specific instruction patterns. So this choice favors ahead-of-time compilation at the expense of interpretation and JITing.

Lambda lifting/dropping
-----------------------

Turner writes https://www.cs.kent.ac.uk/people/staff/dat/miranda/manual/30.html: If your script consists of six functions, one of which solves a problem and the other five of which are auxiliary to it, it is probably not a good style to put the five subsidiary functions inside a where clause of the main one. It is usually better to make all six top level definitions, with the important one written first, say. There are several reasons for this:

1. easier to read - six separate chunks of information rather than one big one
2. easier to debug - each of its functions can be exercised separately, on appropriate test data, within a Miranda session
3. more robust for future development - for example if we later wish to add a second ``main`` function that solves a different problem by using the same five auxiliary functions in another way, we can do so without having to restructure any existing code.
4. in the current implementation, functions defined inside a "where" clause cannot have their types explicitly specified

In practice, programmers tend to use fewer than ten parameters, but little nesting, with a mixture of parameter lifting/dropping and block floating/sinking.

Turner also writes that nested blocks are “slower to compile”, but provides no evidence. Empirically danvy observed the opposite. Across a wide variety of functional languages (Miranda, Standard ML, Moscow ML, Chez Scheme GHC, SCM), block-structured programs were faster to compile and run than the corresponding recursive equations. The number of parameters is a bottleneck both in the compiler's processing and at runtime as procedure-calling conventions have a limited number of registers. This is especially noticeable for automatically produced recursive equations, which can have dozens of parameters.

Lambda lifting transforms a block-structured (let/lambda) program into a program consisting only of global (potentially recursive) combinator definitions / rewrite rules. This global definition style maps more closely to practical machine code than the Y combinator. The main complications are handling free variables, i.e. nested variable scope, and preserving function applications with known (constant) functions.

Algorithm:
Use unique fresh identifiers to avoid name capture
Assign a name to each lambda expression, like \x y. e --> let f = \x y. e in f
For each local definition f, let Ef denote the set of free identifiers that has to be abstracted out of the body of f. The function names themselves are treated as constants, and need not be abstracted out.
Set up a system of equations involving Ef, Eg etc. like Ef = {a} union Eg, Eg = {b} union Ef.
Find the least solution of this (transitive closure / repeated substitution, O(n^3)), which produces the values Ef for each f
In the definition of f: add the free variables Ef as arguments to the definition of f
For each occurrence of f: change the use f to f applied to its arguments Ef
Then there are no free variables in any definition body and they can all be lifted (floated) out to global scope, producing a set of recursive equations

Lambda-dropping transforms a set of recursive equations into one with block structure and lexical scope. This can be divided into block sinking, placing each block at its lowest point of use, and parameter dropping, using lexical scope to avoid passing parameters explicitly.

What really matters is the parameter passing and def-use paths. In a fully parameter-lifted program, all def-use paths are made explicit - recursive equations must carry around all the parameters that any of their callees might possibly need. In a fully parameter-dropped program, all def-use paths are made implicit - any variable that can be made a free variable is not passed explicitly.

To summarize, there are 3 intermediate forms:
- block-structured: uses lexical scope and block nesting (let/letrec/module scope) freely
- scope-insensitive program: all blocks have no free variables, but may be nested
- recursive equations: no nested blocks, all live variables explicitly passed as actual parameters.

The general pattern is parameter lift -> block float -> block sink -> parameter drop. This is a proper source-to-source transformation and corresponds to transforming a program into the functional representation of its optimal SSA form.

Parameter lifting makes a block-structured program "scope-insensitive" or "parameter-lifted", so that no function has any free variables. With first-order programs with is done by passing extra variables to each function to account for variables occurring free further down the call path. With higher-order functions there is a little currying trick.
Block floating eliminates block structure by moving local functions to the enclosing scope level whenever they do not use locally defined functions and do not refer to local free variables. After block floating, each function either is global, contains free variables (defined by an enclosing function), or free functions (defined in the same block). If the source program is parameter-lifted, then a block-floated program degenerates to recursive equations; otherwise, there will be SCCs that cannot float above their enclosing abstractions.
Block sinking restores block structure by localizing (strongly connected) groups of equations in the call graph. Any set of functions that is referenced by a single function only is made local to this function, by moving the set inside the definition of the function. More precisely, localization stops at the closest common context of all the uses. Going any further would entail code duplication.
Parameter dropping exploits scope. If every invocation of a function with a formal parameter uses the same argument value, and the argument value is lexically visible at the definition site of the function, that function parameter can be dropped.

Both stages can be computed formally using the call graph. A function f that calls some other function g depends on g. A function g that is dominated by some other function f in the call graph can only be called from the definition of f. The dominator tree of the call graph is thus a tree that induces a new block struc-
ture into the program. In the new program, each function is declared locally to its predecessor.

For higher-order programs, it requires complex analysis in the general case
P. Sestoft, Replacing function parameters by global variables
O. Shivers, Control-flow analysis of higher-order languages or taming lambda

In an imperative program in SSA form, each variable is assigned only at a single point in the program.
Values from different branches in the ow graph of program are merged using “phi-nodes”.
A naive algorithm for transforming an imperative program into SSA form creates phi-nodes for all variables, at all merge points in the program.
Minimizing the number of phi-nodes yields a program in optimal SSA form.

In his textbook on compiler construction [3], Appel connects SSA and functional programming
an imperative SSA flow graph corresponds to a block-structured functional program, via block-sinking
optimizing SSA corresponds to parameter-dropping

Together, lambda-lifting, lambda-dropping and control-flow analysis allow one to convert Curry’s fixpoint operator into Turing’s fixpoint operator.

instead of lifting only free variables, one lifts maximally free expressions

In both closure-converted and lambda-lifted programs, lambda abstractions are
named. Closure conversion differs from lambda-lifting for the following two rea-
sons:
•  In a closure-converted program, free variables are passed only when the name is defined. In a lambda-lifted program, free variables are passed each time the
name is used.
• Closure conversion only considers the free variables of a lambda-abstraction. Lambda-lifting also considers those of the callees of this lambda-abstraction.
In the latter sense, lambda-lifting can be seen as the transitive closure of closure conversion.


With its inverse translation defined, lambda-lifting acts like CPS. The CPS transformation reveals control-flow information, while lambda-dropping reveals scope information. Similarly to A transformations / A normal form and Sequent IR, is there is a happy medium between lambda-lifted and lambda dropped programs?


Sequent Core
============

So, the Curry-Howard correspondence maps logic to programming. A logical system specifies well-formed proofs of propositions. These correspond to well-formed programs in a simple type system. By proving the logic sound and complete, we get an expressive core programming language. Whereas a natural deduction logic results in reduction patterns similar to the lambda-calculus (lambda-apply), sequent calculus divides the program into values and continuations. Reduction always takes place at a cut point with a value and a continuation, and can produce multiple or zero values/continuations. Continuations are exposed as first-class manipulable variables, similar to CPS, but CPS-based IRs have drawbacks (fixing the evaluation order to CBV) that sequent calculus-style IRs do not.
Then there is the join point stuff, which also promises to be an alternative to CPS; I have read it but it just seems overly complex compared to sequents. Because now you have functions, and join points, and continuation parameters. There is a definition of join point in the paper but it is semantic (no obvious distinction between join points and functions), whereas the definition of continuations in sequent calculus is syntactic (minus rule as opposed to plus). As Kennedy says, "Better to start off with a language that makes continuations explicit."
So that is the summary, sequents are nice from a logical perspective, they seem to be a nice alternative to CPS, and I like them better than join points.

CPS does expose control flow as continuation values, but it has problems. First, per :cite:`downenSequentCalculusCompiler2016`, there is not one CPS transform, but rather a family, each CPS transform fixing an evaluation order. One must choose among call-by-value, call-by-name, or call-by-need. As a benefit, the evaluation order of the translation target doesn't matter, and strong beta-eta reduction of the CPS'd term is sound. In fact, per :cite:`okasakiCallbyneedContinuationpassingStyle1994`, all CPS translations are based on CBV, and call-by-name/call-by-need CPS translations can be decomposed as a conversion to CBV pass followed by a CBV CPS translation. IOdeally, the compiler should be able to freely choose the evaluation order, to trade-off the locality of innermost vs. the hypernormalization of outermost. Being unable to safely perform out-of-order reductions is a deal-breaker.

The CBV CPS encoding is quite annoying, e.g. :cite:`downenSequentCalculusCompiler2016` it inverts nested function calls ``map f (map g xs)`` as ``λk.map g (λh.h xs (λys.map f (λh'.h' ys k)))``. Per :cite:`maurerCompilingContinuations2016` this makes CSE harder, e.g. ``f (g x) (g x)`` vs ``g (\xv. g (\yv. f k xv yv) x) x``. Also rewrite rules are harder to apply. Even CBV has an encoding - :cite:`flanaganEssenceCompilingContinuations1993` point out that "realistic" CBV CPS compilers mark source function calls as using special continuation closures to allow efficient calls. The call-by-need transform is worse - :cite:`okasakiCallbyneedContinuationpassingStyle1994` describes how the thunk graph itself must be represented in the CPS term. It does have the benefit that the term graph is built incrementally, by gluing together subgraphs generated on demand by reduction, but the graph is still obfuscated as imperative code. :cite:`kennedyCompilingContinuationsContinued2007` states assigning names to continuations is really a benefit, but doesn't discuss the other drawbacks of the encoding.

:cite:`sabryReasoningProgramsContinuationpassing1992` demonstrated that CBV CPS was reversible, and proved that beta-eta-reduction in CPS corresponded to the A-reductions plus call-by-value reduction on the original term. Hence, per :cite:`flanaganEssenceCompilingContinuations1993`, many compilers adopted reducing the expression to A normal form between other transformations as a replacement for CPS. However, per :cite:`kennedyCompilingContinuationsContinued2007`, ANF is not closed under beta-reduction - inlining can create nested lets, which then have to be "renormalized", floated out or rearranged via "commuting conversions". Similarly, the A-reduction ``E (if x then a else b) = if x then E a else E b`` duplicates the evaluation context, and as such is commonly left out. The workaround is to introduce a "join point", the ``e`` in ``let e z = ... in if x then e a else e b``. But join points are essentially continuations, second-class in that they are represented by function bindings. Even if specifically marked, they are fragile, in that per :cite:`maurerCompilingContinuations2016` the case-of-case transformation must handle join points specially, and similarly other transformations must preserve join points (e.g. not inlining the join point). Furthermore, they are not really functions, requiring a special calling convention to compile efficiently. As Kennedy says, "Better to start off with a language that makes continuations explicit."

So all of CPS, ANF, and joint points suck. Fortunately, :cite:`downenSequentCalculusCompiler2016` presents Sequent Core, which retains the advantages of first-class continuations while avoiding the drawbacks. Sequent Core does not force choosing an evaluation order. A nested function application is munged a little, but the order is not inverted and CSE still works. ``Cut`` glues together graph pieces, but is itself part of the graph, hence does not need to encode its sub-graphs. Functions reduce to join points and other types of sequent, rather than the reverse. Reduction is uniformly cut-elimination, and does not require renormalization. Downen at al. implemented Sequent Core, but the implementation was complex. I think though that this complexity was mainly due to the need to interface with GHC and closely follow the design of GHC's original System F-like Core. A novel implementation focusing on a "clean" sequent logic, and emphasizing duality and symmetries, like Stroscot's, should be able to avoid this implementation complexity. I asked SPJ and he was like "go for it."

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

From a user perspective there are two types of jumpable addresses:

* memory - effective address computation
* SIB addressing form, where the index register is not used in address calculation, Scale is ignored. Only the base and displacement are used in effective address calculation.
* VSIB memory addressing

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

Since all values are representable in memory, we could use bytes in the IR for values. But this would lose the type information. So instead we must support all the value types listed in `Values`_.

Thorin
======

higher-order functions - lambda mangling transformation
non-explicit scope nesting via a dependency graph - control-flow restructuring program transformations are cumbersome to implement otherwise

closure: A record that contains the pointer to a function and value bindings of its free variables
functions and closures need to be first-class citizens

In Thorin, each definition is a node. Every use is an edge to this node. Each value is a node in a graph and every reference to a value is an edge to this node.

Names are solely present for readability. They have no semantic meaning. Thorin is graph-based with no named variables, therefore does not require explicit scope nesting.

Partial inlining is the crucial step in closure elimination.

control flow form (CFF) Thorin programs can be translated to control flow graphs (CFGs) without dynamically-allocated closures, and CFF-convertible programs are optimizable to CFF using lambda mangling. This superset includes typical higher-order programming idioms like map, fold, range, and generators.

Nameless: The β-reduction λa.(λx.(λa.x)) a ⇒ λa.λb.a requires renaming one of the a's. Without this capture-avoiding substitution, naı̈ve reduction λa.(λx.(λa.x)) a ⇒ λa.λa.a incorrectly does "name capture" of a. The inliner of GHC uses "the rapier" strategy of renaming as little as necessary, but it still adds significant complexity and overhead. Thorin's solution is to use a graph instead of names.

Blockless: Consider adding a conditional branch,
a (x: int, ret: fn(int)) = ret x
a (x: int, ret: fn(int)) = let {c = ret x} in branch (x = 0, c, c)
Similarly consider inlining:
let f a b = (let g x = x+1 in a + (g b)) in f 1 2
let {g x = x+1; f a b = a + (g b)} in f 1 2
let {g x = x+1; f a b = a + (g b)} in 1 + (g 2)
Languages with block/let nesting require applying "let floating" / "block sinking" normalization to repair the nesting. Although it is called normalization, there is generally not a canonical or minimal IR under these operations - c.f. :cite:`kennedyCompilingContinuationsContinued2007`'s discussion of commuting conversions and conditional expressions. Thorin represents nesting implicitly - if a function f uses a variable defined in another function g, f is implicitly nested in g. Therefore it does not need any floating or sinking normalization - we say Thorin is blockless.

Memory operations or side-effects are explicitly tracked by a functional store [27, 28].

Liveness: We need to know all direct and indirect dependencies from the view of a function. We call all functions which are live from the view of an another function f the scope of f. We call f the entry function of its scope.

Lambda Mangling: Lambda mangling is a combination of lambda lifting and dropping. We take a map M, then each parameter is cloned to a fresh identifier and rewritten to its looked up form.

for constant parameters we drop the parameter
then we do local optimizations (constant propagation, common subexpression elimination)
to eliminate function dependencies we parameter-lift the return address

Recursion

In a CPS program, all calls are tail-call form, like ``foo (...)``, but some occurrences of functions are not, e.g. they capture state in closures.
Thorin defines as follow:
1. Create a node for each Thorin function
2. For each function f, draw an edge to all functions which occur in f's body.
A strongly connected component (SCC) in this graph forms a recursion.
We classify:
recursive call: Any call within an SCC
simple recursive call: A recursive call within the scope of the callee
mutual recursive call: All other recursive calls
static parameter: A parameter which does not change its value within an SCC
first-order recursive call: A recursive call which only uses static parameters as higher-order arguments
loop: An SCC formed by recursive calls that are only of first order

we classify Thorin functions in the following way:
basic block (BB)-like function: A first-order function.
returning function: A second-order function with exactly one first-order parameter.
top-level function: A function whose scope does not contain free variables.
bad function: A function that is not BB-like, top-level, or returning.
control flow form: A scope that does not use bad functions.
