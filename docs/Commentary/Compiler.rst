Compiler design
###############

Dynamically typed languages are tricky to compile efficiently. There’s been lots of research on efficient JIT compilers for dynamic languages - SELF, Javascript, PyPy, Java - but these are quite involved, and still slower than C. Ahead-of-time compilation for dynamic languages is possible as well but not explored, and needs profile data to work properly. Currently Stroscot is aiming for AOT and JIT compilers with profiling.

Structure
=========

The interpreter:

* A parser - this is written using nondeterminism. Likely the full syntax will not be fast enough for practical purposes until late in the project, so for now the parser uses a deterministic Lisp-like syntax. The parser records file and line number information, token start/end, call stack, and other debugging information.
* Fexpr interpreter loop - this starts with the AST and produces a value. The main part is dispatching pattern matches. Uses the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`.
* Logic prover - a CDCL satisfiability search algorithm, handles nondeterminism such as dispatch, checking if a value is a member of a type (checking functions etc. is nondeterministic), explicit lub, etc.
* Memory management
* A dynamic assembler / JIT code generator

The specializer:

* Supercompiler / partial evaluator: computes possible states of the program
* Figures out how to represent space of program states efficiently (to avoid state explosion)
* Code generation: converts state transition relation to assembly instructions of the code target
* Static verification: Warns if error states are reachable, checks other specified properties to generate warnings

The JIT:

* Interleaves specialized generated code and the interpreter
* Counts state repetitions and specializes hot loops

Optimization
============

For a lot of compilation decisions we have several choices and want to pick the best one based on some measure of "performance". E.g. overloading/dispatch can be implemented in a variety of ways, specialized for call site - generally it boils down to branching on some condition (binary search), or doing a table lookup. The fastest solution depends on which clauses are relatively hot, but in general we don't know which clauses are hot.

Profile-guided optimization is an effective solution to this lack of information: we instrument a binary with counters for the various questions we might ask, and generate a profile with the answers. We might need to run a binary several different times to get good coverage so we also need a way to combine profiles together, i.e. profiles form a commutative monoid. Profiles themselves introduce a "Heisenbug" problem: we cannot measure the detailed performance of an unprofiled program, and turning profiling off may change the performance significantly. The solution is to build with profiling support for almost all of the compilation pipeline. We should only omit profiling instructions for non-profiled builds at the assembly level. And if we use hardware-assisted sampling profiling then we don't even need profiling instructions, in many cases, so profiling can simply be always enabled.

When trying to do a quick compile-run cycle, we still want to streamline hot paths so that the binary is not unusably slow, but cold spots can use a straightforward boilerplate translation that doesn't require much CPU. More generally, there are various optimization criteria to minimize during compilation. Generally anything that can be measured is fair game:

* Compile/runtime total elapsed time, power usage, memory usage
* Runtime executable size
* Throughput at runtime
* Request-response latency at runtime  - can be conditional, e.g. latency for a stop loss order on trading platform but latency for other types is not as important

These are generally not hard numbers but probabilistic variables, because computer performance depends on many uncontrollable factors hence is best treated is nondeterministic. A simple mean or median estimator is generally sufficient, but doing statistical hypothesis testing is more interesting. Worst case execution time is of interest in real-time systems. Execution time may be modeled by a Gumbel distribution (`ref <http://www.lasid.ufba.br/publicacoes/artigos/Estimating+Execution+Time+Probability+Distributions+in+Component-based+Real-Time+Systems.pdf>`__) or odd log-logistic generalized gamma (OLL-GG) or exponentiated Weibull (`ref <https://arxiv.org/pdf/2006.09864.pdf>`__), although these experiments should probably be redone as we are measuring different programs. The testbench is `here <https://mjsaldanha.com/sci-projects/3-prob-exec-times-1/>`__ and `here <https://github.com/matheushjs/ElfProbTET>`__ and could be extended with `gev <https://www.rdocumentation.org/packages/evd/versions/2.3-6/topics/gev>`__.

Obviously these have tradeoffs, so we need an overall objective function. For a focused objective like running static verification, all we want to see the error messages so total elapsed compile time is the only measurement. For production binaries, there will likely be a complex function for various runtime measurements based on actual costs and requirements, but compile time will be excluded from the criteria or minimally penalized. For debugging, running in a REPL, an edit-compile-test cycle, etc., both compile and runtime factors are important so the objective function becomes even more complex. gcc, clang, etc. have various optimization profiles like O0, O1, O2, O3, Og, On, Os, Oz, etc., which we can include presets for, but it's not clear these are sufficient.

We use branch-and-bound to explore the possibilities. With good heuristics even the truncated search algorithm should give good results. The goal is to quickly find bottleneck code regions that have significant effects on performance and compute good optimizations quickly. Then another profiling build to test that the proposed changes were correct.

There is also ISA selection and tuning for specific machines and CPUs. ISA, timing, cache, and memory characteristics are available for specific CPUs, but compiling specifically for a single CPU is not done often. Usually for x86 the code is compiled to work on SSE2 (since it's part of AMD64) and tuned for a "generic" CPU. The definition of this is vague - for `GCC <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81616>`__ and `LLVM <https://reviews.llvm.org/D118534>`__ it seems to be Haswell with a few slow cases on other architectures patched. It is supposed to be "an average of popular targets", so using a weighted sum of processors according to sales is most appropriate, but per-CPU-model sales data doesn't seem to be available easily. `PassMark <https://www.cpubenchmark.net/share30.html>`__, `3DMark <https://benchmarks.ul.com/compare/best-cpus?amount=0&sortBy=POPULARITY&reverseOrder=true&types=MOBILE,DESKTOP&minRating=0>`__, and `UserBenchmark <https://cpu.userbenchmark.com/>`__ publish their list of most benchmarked processors, which is probably good enough.

Formally proving optimizations correct is a good idea, as they are often buggy.

Need optimizations for:
* avoiding intermediate structures and dead or redundantly duplicated computation
* storing arrays on the heap in the most efficient of a few straightforward ways
* boiling away higher-order functions into tedious boilerplate (inlining)
* custom optimizations

A `talk <http://venge.net/graydon/talks/CompilerTalk-2019.pdf>`__ by Graydon Hoare on compilers mentions the paper :cite:`allenCatalogueOptimizingTransformations1971`. He says we need 8 optimization passes to get 80% of GCC/LLVM performance:

* Common subexpression elimination - This starts from atomic expressions / closed connected components and then works up to identify opportunities for sharing. Because of unsharing fans it can share parents regardless of their other children; this doesn't increase the graph size and may decrease code size/computation. Since the graph may be cyclic we need a partitioning algorithm like in :cite:`mauborgneRepresentationSetsTrees1999`.
* Inlining - Going through :cite:`peytonjonesSecretsGlasgowHaskell2002`, this is basically just reducing reducible expressions. The reason it's hard is doing reduction across statement boundaries, inside recursive functions, etc., in combination with a strictness/termination analysis.
* Constant Folding - more reduction of reducible expressions
* Loop unrolling/vectorization - mutable variables can be normalized to SSA, so really this is about unrolling recursive functions. It's a code size vs. code quality optimization, heavily dependent on scheduling.
* Loop-invariant code motion (hoisting) - this is just reducing in a certain order, i.e. scheduling again.
* Dead code elimination - Unused pure expressions aren't connected to the main graph and so are trivially eliminated. But we also want to eliminate conditional branches that will never be taken; this requires a reachability analysis.
* Peephole - this is instruction selection for the backend. We're going the Unison integrated constraint-satisfaction approach.

Cross compilation
=================

In cross compilation we have not one system, but two systems. To use the newer `Clang <https://clang.llvm.org/docs/CrossCompilation.html>`__ terminology, there is the **host** system where the program is being built, and the **target** system where the program will run. When the host and target systems are the same, it's a native build; otherwise it's a cross build.

The older `GNU terminology <https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html>`__ uses a triple, build/host/target; but the "target" there is really a configuration option, namely the supported target of the compiler that will run on the host. Only gcc need to specify the supported target, as Clang is generally built to support all supported targets. Since remembering whether the build system builds the host or vice-versa is tricky, overall the Clang terminology host/target/supported targets seems clearer than build/host/target.

the toolchain (gcc, llvm, as, ld, ar, strip, etc.) should be target-dependent, information stored in a YAML file or similar
the package set is also target-dependent. some packages that are pure data are target-independent

Bootstrapping
=============

Bootstrapping is a 2-stage process. We start with the source ``s`` and bootstrap compiler ``cB``, an old compiler using the old ABI. Then we build:

* stage 1: New compiler on old ABI ``c1=run(cB,s)``
* stage 2: New compiler on new ABI ``c2=run(c1,s)``

We can test stage 2 (the "compiler bootstrap test") by building a new compiler ``c3=run(c2,s)``. If the build is deterministic, ``c3`` should be bit-identical to ``c2``. We can also run the test suite to compare outputs of ``c1`` and ``c2``. But we cannot compare performance of ``c1`` and ``c2``, because they use different ABIs, and also ``cB`` may be buggy so ``c1`` and ``c2`` may not behave exactly the same. We can also use diverse double-compiling :cite:`wheelerFullyCounteringTrusting2010`, compiling with multiple bootstrap compilers ``cB``, to increase our confidence in the correctness of the stage 2 compiler.

For cross-compiling, we build stage 1 for the host and stage 2 for the target.

The compiler depends on libraries. The bootstrap compiler does not provide updated libraries, so we must build the libraries for the Stage 1 compiler.

build stage 2 compiler with the stage 1 compiler using the stage 1 package database ship with the stage 2 compiler). As such, the compiler is built with the identical libraries that it ships with. When running / interpreting byte code, we need to dynamically link packages and this way we can guarantee that the packages we link are identical to the ones the compiler was built with. This it is also the reason why we don’t have GHCi or Template Haskell support in the stage 1 compiler.

Complex bootstrap
=================

Actually bootstrapping is more complex. The compiler is really two components, an interpreter and a specializer. The input program can take arguments. The interpreter can take arguments (dialects, libraries). The specializer can take arguments (bytecode, optimization instructions, plugins). The output program can take arguments (compiled objects, runtime components such as libc or a garbage collector). All of these arguments and options aren't handled easily.

We can think about this using the Futamura projections. We assume a primitive

.. math::

  \newcommand{\run}[1]{⟦#1⟧}
  \run{\cdot} : \text{program} \to \text{data} \to \text{result}

that can run programs written in any language, given input data, and produce an output result. We use a denotational notion of result where erroring / not halting is itself a result. Two programs are equal if :math:`\run{p} d = \run{q} d` for all :math:`d`; equivalence of results depends on context and ranges from literal comparison to more advanced semantics.

Definitions:

* An interpreter :math:`i` has :math:`\run{i} (p,d) = \run{p} d`.
* A compiler :math:`c` has :math:`\run{\run{c} p} d = \run{p} d`.
* A specializer :math:`s` has :math:`\run{\run{s} (p,x)} y = \run{p} (x,y)`.
* A residual program is a program :math:`p_x` such that :math:`\run{p_x} y = \run{p} (x,y)`.
* A generating extension :math:`g_p` of a program :math:`p` has :math:`\run{g_p} x = p_x`, i.e. it produces residual programs of :math:`p`.
* A compiler generator :math:`c` has :math:`\run{\run{\run{c} p} x} y = \run{p} (x,y)`.
* A runner :math:`r` has :math:`\run{\run{r} c} (p,x) = \run{\run{c} p} x`

1 specializer generates residual programs, :math:`p_x = \run{s} (p,x)`.
2 specializers produces generating extensions, :math:`g_p = \run{s_1} (s_2,p)`.
3 specializers produces a compiler generator, :math:`c_{123} = \run{s_1} (s_2,s_3)`.
Similarly we can use a compiler generator: :math:`\run{\run{c} p} x` for residual programs, :math:`\run{c} p` for generating extensions, :math:`c_{123} = \run{\run{\run{c} s_1} s_2} s_3` to obtain the same compiler generator as formed by applying the specializers.

A generating extension of an interpreter is a compiler; similarly passing an interpreter :math:`i` to a compiler generator :math:`c` produces a compiler :math:`\run{c} i`. A generating extension of a string matcher is a matcher generator and a generating extension of a universal parser is a parser generator. Hence we should call a compiler generator a "generating extension generator".

A generating extension of a specializer is a compiler generator. :math:`\run{\run{\run{g_s}p}x}y = \run{\run{s}(p,x)} y = \run{p}(x,y)`

In particular, assuming :math:`c` is a compiler generator, :math:`c' = \run{c} s` is a compiler generator iff :math:`s` is a specializer. Proof: :math:`run (\run{s} (p,x)) y = \run{\run{\run{\run{c} s} p} x} y = \run{\run{\run{c}' p} x} y = \run{p} (x,y)` to show :math:`s` is a specializer, :math:`\run{\run{\run{c'} p} x} y = run (\run{s} (p,x)) y = \run{p} (x,y)` to show :math:`c'` is a compiler generator.

If :math:`\run{c} s = c`, :math:`c` is termed a self-generating compiler generator. :math:`\run{s} (s,s) = \run{\run{\run{c} s} s} s = c`. Furthermore :math:`s` is a specializer. OTOH if :math:`s` is a specializer then :math:`\run{s} (s,s)` is a compiler generator self-generating with :math:`s`.

With a runner :math:`r` we can turn a compiler generator :math:`c` into a specializer :math:`\run{r}c`. Self-applying this specializer gives a compiler generator with equivalent output to :math:`c` after two arguments have been applied:

.. math::

  \run{\run{\run{\run{r}c}(\run{r}c,\run{r}c)}p}x & = \run{\run{\run{\run{c}(\run{r}c)}(\run{r}c)}p}x \\
  & = \run{\run{\run{r}c}(\run{r}c,p)}x \\
  & = \run{\run{\run{c}\run{r}c}p}x \\
  & = \run{\run{r}c}(p,x) \\
  & = \run{\run{c}p}x

Compile-time code execution
===========================

We want to execute code that runs at compile time, e.g. reading a blob of data to be included as a literal. Clearly this code executes on the host, with the same filesystem as the rest of the source code.

We also want to read configuration, e.g. the target platform properties (word size, endianness, etc.).

Also we want to do computations with no runtime inputs, like 1+2.

Compiler ways
=============

GHC calls some options "compiler ways". They can be combined (e.g. threaded + debugging). The main issue is they affect the ABI, so ways need be stored into ABI hashes in installed libraries to avoid mismatching incompatible code objects.

- use the multi-threaded runtime system or not
- support profiling or not
- use additional debug assertions or not
- use different heap object representation (e.g. ``tables_next_to_code``)
- support dynamic linking or not

Depending on the selected way, the compiler produces and links appropriate objects together. These objects are identified by a suffix: e.g. ``*.p_o`` for an object built with profiling enabled; ``*.thr_debug_p.a`` for an archive built with multi-threading, debugging, and profiling enabled. See the gory details on the `wiki <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/compiler-ways>`__.

Installed packages usually don't provide objects for all the possible ways as it would make compilation times and disk space explode for features rarely used. The compiler itself and its boot libraries must be built for the target way.

Compiler memory management
==========================

For the compiler itself, a trivial bump or arena allocator is sufficient for most purposes, as it is invoked on a single file and lasts a few seconds. With multiple files and large projects the issue is more complicated, as some amount of information must be shared between files. Optimization passes are also quite traversal-intensive and it may be more efficient to do in-place updates with a tracing GC rather than duplicating the whole AST and de-allocating the old one. Two other sources of high memory usage are macros and generics, particularly in combination with optimizations that increase code size such as inlining.

Overall I don't see much of an opportunity, SSD and network speeds are sufficient to make virtual memory and compile farms usable, so the maximum memory is some large number of petabytes. The real issue is not total usage but locality, because compilers need to look up information about random methods, blocks, types etc. very often. But good caching/prefetching heuristics should not be too hard to develop. In practice the programs people compile are relatively small, and the bottleneck is the CPU because optimizations are similar to brute-force searching through the list of possible programs. Parallelization is still useful. Particularly when AMD has started selling 64-core desktop processors, it's clear that optimizing for some level of that, maybe 16 or 32 cores, is worthwhile.

Dynamic execution
=================

benefit: erases distinction between compile time and execution time. Hence optimizes for compile+execute time.


loading code at runtime
- typecheck, JIT compile, return function pointer
the function pointer doesn't have to be machine code, it can be bytecode, so the function runs through an interpreter
Compiler from IR to bytecode
Saving snapshots of the VM state (images)
Tracing JIT compiler
Use libgccjit for code generation?
Optimized assembly interpreter a la LuaJIT and JavaScriptCore


everyone had two entry points.
if you came from the
interpreter you had to call the
interpreter entry point and you
came from JITed code you entered the
JITed code favorite entry point

the goal here was JITed calling JITed had minimal overhead
so an x86 call instruction with the JITed entry point's address

so if a JITed calls interpreted there's a
JITed entry point that shuffles the
arguments and jumps to the interpreter

and if the interpreter makes
a call, it's a slow procedure that looks
up the interpreter endpoint or else
jumps to a trampoline that jumped to the JITed code

then there's deoptimization
it's tricky to stop running processors
from running code
if you try to
edit the method call buffers processors have
them cached
you
can't actually stop it
so first you change the vtable to the interpreter
then you change the head of the method to jump to the interpreter

there's also speculative optimization and escape analysis

Creating the compiled file consumes extra CPU time and storage vs the interpreter. The compiled version runs more efficiently. Some errors are only detected during compilation.

Julia - faster than Python, but JIT uses many slow trampolines

Javascript - V8 is a fast modern JIT


In a sea of nodes program dependence graph (PDG), nodes correspond to arithmetic/logic operations but also to control operations such as conditional jumps and loops. edges correspond to dependencies among operations.

graphs corresponding to relatively small programs turn quickly into a tangle that is quite difficult to grasp. PDGs cannot be read directly without assistance; this affects debugging speed. PDGs remain an obscure topic in advanced compiler courses.

In a CFG, nodes correspond to basic blocks, ordered sequences of operations that are always executed together. every operation belongs to a single basic block. edges correspond to control jumps across basic blocks. A CFG yields a structured, sequential view of the program that is easier to understand and debug, and is familiar for many systems engineers.

To turn a PDG into a CFG, compute an assignment of operations to basic blocks (global schedule) and an ordering of operations within each basic block (local schedule).

clustering basic blocks into (nested) loops, if-then-else structures, etc.
coloring the basic blocks that are executed most often

the value representation is optimized for the platform, and redundant checks are optimized out

The Implementation of Functional Programming Languages
Implementing functional languages: a tutorial
Implementing Lazy Functional Languages on Stock Hardware: The Spineless Tagless G-Machine
How to make a fast curry: push/enter vs eval/apply
GHC also does strictness analysis and optimistic evaluation.

a program is a dependency graph which is evaluated through a series of local reductions
the graph itself can be represented as code. In particular, we can represent a node as a function that when invoked, returns the desired value. The first time it is invoked, it asks the subnodes for their values and then operates on them, and then it overwrites itself with a new instruction that just says "return the result."


JIT cache: need >90% hit rate to pay off vs just doing normal JIT path of interpeting bytecode and optimizing. need profile data, otherwise optimizations will be different. The profile is a few megabytes but the compiled code may be 100s of megabytes since it has a lot of metadata.

rare methods don't show up in the profile, but may still need to be fast.

the c2 strategy is a counter with an absolute threshold. so eventually, as long it is not dead code, it will be JITed. it guarantees enough samples so that you have a good profile. trying to do an exponential decay so only hot methods

L1 cache is cheaper than memory, so clean up bytecode as soon as it is generated

Debugger
========

The debugger's view of the program's state is as a large expression or term. This state evolves in steps, where each step applies a rule to a redex or calls into the OS to perform a primitive operation.

We allow reversible/omniscient debugging, meaning that one can step both forward from a state (the usual) and backward from a state (query on where a value came from etc.).

One debugging technique useful in combination with reversible debugging is to use a step counter that starts at 0 at the beginning of the program and increments every time a reduction step is performed. The exact step that triggers a behavior can be determined by binary search. When we are debugging a phase of the compiler, we can use "fuel" for each phase - this specifies how many transformations can be performed during the phase of interest before moving on to the next phase.

Let's assume we have symbols, then there are lots of operations available from a debugger:

* breakpoints: set/clear/list, essentially a breakpoint is a true/false query on a state. can be syscall, call, return, signal injection, etc.
* queries: print backtrace / call stack, evaluate pure expression in context of state, dump state, dump memory, disassemble memory
* stepping: single step, step out, continue thread / all threads until breakpoint, run ignoring breakpoints until stopped with interactive commnad
* patching: replace definition, jump to address, return early from function, evaluate code in current context (e.g. set memory to value). The debugger can only run forward from the patched state because it has no history.
* IPC: send signal, modify files

Profiler
========

Measure

* time and memory usage.
* throughput (calls/second)
* A/B testing of multiple implementations

for functions, expressions, programs, etc.

Use statistical sampling and hardware performance counters to avoid overhead. Checkout criterion, papers on LLVM hardware sampling.

IR dump
=======

A good compiler can get 80% of the code to a fast-enough state. But nontrivial hot spots will still need hand-optimizing and tuning. At first it can be good to tweak the original code to get it to generate IR differently, but eventually the algorithm is set and the micro-optimizations matter, so you want to bake in the low-level implementation.

With a wide-spectrum language the IR is the same language as the original, just using lower-level operations. So you can compile source-to-source or directly write in the IR. For example SQL is declarative but being able to write a functional program using the underlying sort, filter, merge anti-join, etc. operations would be useful.

There are many levels to the pipeline, and each one is useful. For an interpreted program the only step that can't be represented is actually running the program, e.g. converting ``print "Hi" exit`` to output.

Evolution
=========

Try as we might, no language design is perfect. Languages inevitably change or extend their semantics over time, resulting in ecosystem fragmentation where programs end up being written in different "dialects" of the language. The evolution process aims to minimize the disruption to existing code by evolving the language in a controlled manner, in particular in discrete units of "features". The process guarantees a "compatibility promise" that the source code of existing programs written for an old language version can be automatically migrated to a new language version. Because the language evolves towards a standardized set of features, the langauge should avoid fragmentation.

A feature is a distinct chunk of compiler functionality, such as a change to the semantics of the language, a compiler plugin, or an external tool integration. A feature can be alpha, beta, or stable.

Alpha features are experimental features with little formal testing, released to get feedback. They may be documented informally or on an "alpha features" page. Alpha features have no compatibility guarantee and may be changed freely. Alpha features are kept behind feature toggles, which allow conditioning code on a feature. This allows testing features and integrating them on the main branch while isolating them from other tests and software releases. Alpha features will be removed from the compiler if they have not made any progress towards beta over the course of a year.

Beta features are implemented features that may change further. They must have a reasonable test suite and be documented in the commentary / reference in full detail, describing edge cases. They must also have a how-to if the feature's usage is not obvious. Fundamental new features may affect the tutorial as well, although generally new features are too advanced. Beta features cannot be toggled off but have automigration functionality for old code that is enabled by specifying the language version. Automigration is distinct from a toggle because it is a source-to-source rewrite of the code. Beta features may still have significant bugs, such as the inability to migrate old code correctly, but these bugs should generate readable error messages mentioning the feature name rather than crashing the compiler or silently failing.

Stable features are frozen features - further changes will be done as new features. They are considered to have reached a level of stability sufficient for long-term use. There is no visible difference in the implementation code between beta features and stable features and the distinction is mainly for marketing purposes.

The list of features is centralized in the code to `this specific file <https://github.com/Mathnerd314/stroscot/blob/master/src/features.txt>`__, to make finding them easier and to standardize handling. The scope of a feature may be identified by grep'ing the code for its identifier.

Moving a feature from alpha to beta should have a PR with documentation links and test case links. The PR should:

* change the feature list to set the feature's status to beta released on the current date. This enables old code warnings, automigration, and compiler bootstrap workarounds.
* implement automigration code if not already present
* remove all uses of the feature toggle in the code by modifying to the case where the feature is present (avoiding toggle debt).

Incremental compilation
=======================

Incremental compilation reduces rebuild time. With a good incremental build system, optimizations can be rechecked rather than rediscovered, so that the program

Hot reloading
=============

Hot reloading or "edit and continue" is the ability to change code and resources of a live application without restarting it. It speeds up the edit-test cycle because you can stay on a certain state of the program without needing to spend time to recreate it. It can be useful for games, UI design, or data analysis.

Edit and continue is really a debugger feature, because usually you edit the code while paused on a breakpoint, rather than while the program is actually running. Integrating with omniscient debugging is probably best, so you can manually select an old state and then evolve it using the new transition rules. For example when editing the jump height for a jump'n'run game, you probably don't want to continue from the game's start, or even the first jump input, but rather to just before the one tricky jump in the middle of the level. There is no indication of this magic location in the code or program state besides the player's x-coordinate being a certain value.

Erlang has hot code swapping, Smalltalks and Lisps have "live programming." Assisting System Evolution: A Smalltalk Retrospective is a recommended read.

The most basic implementation is to patch functions calls so they call a new function instead of an old one. A JIT already does this kind of patching when switching from interpreted to optimized code, so can do it easily. With ahead-of-time you can compile a new DLL, duplicate it to avoid locking, load it, and swap out the function pointer, but it requires specially marking the hot-reloadable methods.

Functions generally assume a fixed set of types and a fixed memory representation for all types. Changing the types or their representation can break program invariants and cause memory corruption. But it is possible - there are some projects for live kernel patching that can patch in-memory data structures to the correct format.

State is also an issue because the memory manager must be aware of the local state of a piece that reloaded and avoid leaking memory. In the case of handles such as an OpenGL context the desirable behavior is to transfer them over to the new code, but if the initialization code is changed then the handle should instead be closed and re-initialized. So we see some sort of incremental program execution going on.

Literal VM
==========

A virtual machine (VM) is a big while-switch-loop that reading opcodes and executes them. VMs are slow, because they’re interpreted - executing an opcode takes many machine code instructions. A literal VM is a VM where each opcode represents one machine code instruction. Because of this 1-1 mapping, functions can be easily JITed to machine code with little overhead. You might alternately call it an "assembler" because it takes an "assembly" language and generates machine code, but LLVM started using the term VM a long time ago. Also, unlike most traditional assemblers, a literal VM allows dynamic loading of new code, interpreting code and running it at a REPL, and introspection such as listing constants and functions from imports. It makes learning assembly pretty fun.

Testing
=======

The main strategy is "golden tests". To start you run some part of the compiler and serialize the output to some human-readable format. Then you store that data as the "expected" output, and run the test again every compile to make sure it still produces the expected output, comparing with diff or similar. Then you check a few changed outputs to find any bugs, fix what needs fixing, and for any non-bug changes, there should be a way to automatically update all the expected outputs to the current outputs.

For a parser, the main test is "integration testing" by giving it examples of source code, full modules, and verifying that it parses to the right AST / fails with the expected output. There is also "unit testing" where you test each grammar production rule individually, e.g. parse an expression or a statement or a block.

Another parser test is "stress testing", generating random valid code samples. The generator doesn't need to exercise every parser path, just the common ones. It pays off in that you understand the grammar better and you can test the performance of your parser.

For the IR golden tests are fine (compare after each optimization pass).
