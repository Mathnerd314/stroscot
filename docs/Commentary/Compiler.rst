Compiler design
###############

Definitions
===========

We assume a mathematical function

.. math::

  \newcommand{\run}[1]{⟦#1⟧}
  \run{\cdot} : \text{program} \to \text{data} \to \text{result}

that can run programs written in any language, given input data, and produce an output result. We use a denotational notion of result where erroring / not halting is itself a result. Two programs are equal if :math:`\run{p} d = \run{q} d` for all :math:`d`; the definition of equivalence of results depends on context, and ranges from literal comparison to more advanced semantics.

Definitions:

* An interpreter :math:`i` has :math:`\run{i} (p,d) = \run{p} d`.
* A compiler :math:`c` has :math:`\run{\run{c} p} d = \run{p} d`.
* A specializer :math:`s` has :math:`\run{\run{s} (p,x)} y = \run{p} (x,y)`.
* A residual program is a program :math:`p_x` such that :math:`\run{p_x} y = \run{p} (x,y)`.
* A generating extension :math:`g_p` of a program :math:`p` has :math:`\run{g_p} x = p_x`, i.e. it produces residual programs of :math:`p`.
* A generating extension generator (GNG) :math:`n` has :math:`\run{\run{\run{n} p} x} y = \run{p} (x,y)`.
* A runner :math:`r` has :math:`\run{\run{r} c} (p,x) = \run{\run{c} p} x`
* A bundler :math:`b` has :math:`\run{\run{r} (c,p)} x = \run{\run{c} p} x`

We can think about specializers using the Futamura projections.

* 1 specializer on a program and argument produces a residual program, :math:`p_x = \run{s} (p,x)`.
* 2 specializers on a program produces a generating extension, :math:`g_p = \run{s_1} (s_2,p)`.
* 3 specializers together produces a generating extension generator, :math:`n_{123} = \run{s_1} (s_2,s_3)`.

Applying a GNG to various things is useful:

* The generating extension of a string matcher is a matcher generator
* The generating extension of a universal parser is a parser generator.
* The generating extension of an interpreter :math:`i`` is a compiler :math:`c = \run{n} i`, :math:`\run{\run{c} p} d = \run{\run{\run{n} i} p} d = \run{i} (p,d) = \run{p} d`. Hence GNGs are often called compiler generators.
* The generating extension of a specializer is a GNG. :math:`\run{\run{\run{g_s}p}x}y = \run{\run{s}(p,x)} y = \run{p}(x,y)`

GNGs can do pretty much everything specializers do, except that we need specializers in source form to generate a GNG from another GNG:

* :math:`\run{\run{n} p} x` for residual programs
* :math:`\run{n} p` for generating extensions
* :math:`n_{123} = \run{\run{\run{n} s_1} s_2} s_3` to obtain the same GNG as formed by applying the specializers, :math:`\run{s_1} (s_2,s_3)`. The result is independent of the GNG used.

Assuming :math:`n` is a GNG, :math:`n' = \run{n} s` is a GNG iff :math:`s` is a specializer. Proof: :math:`run (\run{s} (p,x)) y = \run{\run{\run{\run{n} s} p} x} y = \run{\run{\run{n'} p} x} y = \run{p} (x,y)` to show :math:`s` is a specializer, :math:`\run{\run{\run{n'} p} x} y = run (\run{s} (p,x)) y = \run{p} (x,y)` to show :math:`n'` is a GNG.

If :math:`\run{n} s = n`, :math:`n` is termed a self-generating GNG. :math:`\run{s} (s,s) = \run{\run{\run{n} s} s} s = n`. Furthermore :math:`s` is a specializer. OTOH if :math:`s` is a specializer then :math:`\run{s} (s,s)` is a GNG self-generating with :math:`s`.

With a runner :math:`r` we can turn a GNG :math:`n` into a specializer :math:`\run{r}n`. Self-applying this specializer gives a GNG with equivalent output to :math:`n` after two arguments have been applied:

.. math::

  \run{\run{\run{\run{r}c}(\run{r}c,\run{r}c)}p}x & = \run{\run{\run{\run{c}(\run{r}c)}(\run{r}c)}p}x \\
  & = \run{\run{\run{r}c}(\run{r}c,p)}x \\
  & = \run{\run{\run{c}\run{r}c}p}x \\
  & = \run{\run{r}c}(p,x) \\
  & = \run{\run{c}p}x

Design
======


I don't really like how implicit building an executable is in most compiled languages.
There's no obvious entry point where you can say "this is me invoking the compiler", so it forces a 2-level system of shell and program.

I'd rather write something like:
writeFile (compileToExecutable main)

where it's clear that compileToExecutable is doing the work.


Model
=====

Stroscot's main compiler uses a hybrid JIT model. Full list of execution engine features:

Precompiled code loading: The model loads precompiled code from a database when available.
Fast start interpretation: If precompiled code is not available, the model interprets the source code for a fast start.
On-the-fly compilation: The model compiles source code to machine code while the program is running, as needed to improve performance.
Compilation caching: The compiled machine code is saved to a database for future use.
Distributed compilation: Compilation can be offloaded and fetched from machines on the network, allowing sharing of compiles and using beefier machines.
Incremental compilation: Even if code is modified, unchanged functions can still be used.
Tiered optimization: The model can optimize instructions, basic blocks, functions, and interprocedural (tracing), depending on requirements.
Profile-guided optimization: Records fine-grained profiles and optimizes based on current and previous profiling runs.
Speculative optimizations: Inlining, specializing, prefetching, predictive evaluation, keeping cold code out of cache.
Reoptimization: The model may recompile code if observed runtime behavior is not as expected.
Image: An executable may be generated with reduced capabilities, such as no dynamic code execution, no profiling, and all code pre-loaded into the executable. Not clear which exact configurations should be supported, or what subset of the language will work.

How does it stack up?

Steady state throughput - AOT has to generate code for all possible executions, and cannot dynamically adapt to the current program's execution pattern, hence is (in Mark Stoodley's opinion, no real benchmarks) 50-90% as fast as JIT. Stroscot does on-the-fly compilation so can make assumptions based on observed execution data. On-the-fly compilation can implement all AOT optimizations, although traditionally JIT compilers have stopped at the 80/20 boundary so AOT compilers have had more effort invested into ensuring optimal code for some cases, e.g. vectorization, hence do better on microbenchmarks.
Adapting to changes - AOT cannot adapt at all. Per `talk <https://youtu.be/gx8DVVFPkcQ?t=2171>`__, cached/AOT code is usually within 5-20% of peak JIT performance - not clear how to close gap besides reoptimization. Reoptimization should allow reaching peak performance regardless of starting state.
Ease of use - AOT is more complicated, two commands instead of one. Also requires specifying target platform, vs. autodetecting.
Start up time - AOT has minimal startup time, 20-50% than JIT without cache. With the compilation cache, Stroscot should be able offer startup as good as AOT for most programs, although maybe the disk access patterns will not be as optimized as AOT. The fast start interpreter means Stroscot has no compilation stall on a never-before-seen program, whereas AOT would have to stall while compiling.
Warm up time - AOT has minimal warm-up, but doesn't get as high as a JIT. The cache allows Stroscot to have minimal warm-up time to reach performance similar to AOT, but reoptimization is enabled so there is still a warm-up time to reoptimize and reach peak performance.
Runtime footprint - The image has minimal footprint so is the most suitable for embedded / real-time cases. Dropping runtime code generation, profile collection, and network capabilities produces the smallest CPU / memory footprint, at the cost of some language capabilities. If runtime code generation is needed, profiling and distributed compilation can be enabled in the image. Compilation memory/CPU usage is spiky and transient. Doing it on a beefy server makes the client machine's memory footprint not much more than the application load, hence much more predictable. CPU usage for sending data over the network interface may still be significant, but the client's profiling data is forwarded so there is no loss of optimization capability. A split debug/release model does however introduce the issue of heisenbugs, e.g. profile collection influencing performance and making the compiled profile for the image inaccurate.
Debugging - Stroscot should be easy to debug in-process because all the metadata is in memory and close to hand. Images require separate debug data, not clear if DWARF is sufficient.
Cross-compiling - the distributed and image models both allow offloading optimization to the host and profiling on the target.

SELF, Javascript, PyPy, Java, luajit

Image formats - Per numerous benchmarks of shared vs static, shared libraries are essentially a stupid legacy format; anything embedded should use a statically linked self-contained image. But it still makes sense to support them as an image target for compatibility.
The equivalent of "object files" in the JIT model are the profiling data and compile cache. Rather than ld, there is the JIT or the image generator.

I'm going to skip having a bytecode format like Java - the user provides textual source code files, and the compile cache includes processed AST checksums. If disk bandwidth is an issue, gzip compression is fine. Java bytecode is barely optimized, and it's easily decompiled. Javascript has shown that source-based distribution works fine, and obfuscators have been written for closed-source applications. The image capability is probably what closed-source applications will gravitate towards though.

The interpreter:

* A parser - this is written using nondeterminism. Likely the full syntax will not be fast enough for practical purposes until late in the project, so for now the parser uses a deterministic Lisp-like syntax. The parser records file and line number information, token start/end, call stack, and other debugging information. Produces IR.
* Fexpr interpreter loop - this starts with the AST in the IR and produces a value. The main part is dispatching pattern matches. Uses the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`.
* Logic prover - a CDCL satisfiability search algorithm, handles nondeterminism such as dispatch, checking if a value is a member of a type (checking functions etc. is nondeterministic), explicit lub, checking property of program, etc.
* Memory management - uses logic prover
* A dynamic assembler / JIT code generator

The specializer:

* Supercompiler / partial evaluator: computes possible states of the program
* Figures out how to represent space of program states efficiently (to avoid state explosion)
* Optimizer: inlining method calls, eliminating redundant code, and pipelining instructions
* Code generation: converts state transition relation to assembly instructions of the code target
* Static verification: Warns if error states are reachable, checks other specified properties to generate warnings

The JIT:

* Maintains tiered caches of IR: instruction, block, method, trace. Either empty, profiling, or compiled.
* Interleaves specialized generated machine code and the interpreter
* Profiler: gathering runtime statistics (branches, calls) to identify hotspots and make better optimization decisions.
* Specializes hot loops. To improve overall execution speed, assuming full CPU utilization, the speedup (in ms) times the number of executions must be higher than the time spent compiling. Generally this means the code must be executed at least 1000+ more times. Fortunately most real word apps (and benchmarks) are like that (run more than a second with high code reuse factor). With an old profile we can guess that the total number of runs will be the same, but without data a good predictor is the observed number of executions so far. And with on-stack replacement back-branches are also useful to measure re-executed basic blacks. For estimation purposes it would be good to know the probability distribution for the number of time a function is executed. ChatGPT says that the distribution is heavily application-dependent - some applications follow the Pareto distribution, but others are more like a log-normal distribution (0 at 0, right-biased hump), and some applications have several humps.

Methods can be prioritized in a list by (rate + 1) * (i + 1) * (b + 1), rate = d(i + b) / dt. d is deoptimizations, giving those methods an advantage. There is a cutoff at i + b >= 1500 so low-execution methods are not compiled. The highest N interpreted methods go to C1 with detailed profiling for eventual C2, the rest go to C1 with only counters so not too many methods are profiling simultaneously. A compiler thread running concurrently with execution threads processes compilation requests. While compilation is in progress, interpreted execution continues, including for methods in the process of being JIT'ed. Once the compiled code is available, the interpreter branches off to it. Methods may be pre-empted from detailed profiling by hotter methods. C2 similarly compiles from the top of the queue. Trivial methods or methods that C2 fails to analyze go into a perma-C1 state without profiling. Methods that fail or de-opt in C1 may go directly from interpreter to C2, if the profile is sufficient. Both C1 and C2 optimizations rely on speculative assumptions, so "de-optimizations" where a function's optimized code is discarded after hitting a trap can (and will) happen as the code learns which speculations stick. But after a while, deopts will be rare.

Methods are compiled so deoptimization is only possible at locations known as safepoints. Indeed, on deoptimization, the virtual machine has to be able to reconstruct the state of execution so the interpreter can resume the thread at the point in the method where compiled execution stopped. At a safepoint, a mapping exists between elements of the interpreter state (locals, locked monitors, and so on) and their location in compiled code—such as a register, stack, etc.

In the case of a synchronous deoptimization (or uncommon trap), a safepoint is inserted at the point of the trap and captures the state needed for the deoptimization. In the case of an asynchronous deoptimization, the thread in compiled code has to reach one of the safepoints that were compiled in the code in order to deoptimize.

Re-ordering operations across a safepoint would cause the state at the safepoint to differ from the original state. As a consequence, a compiled method only includes a few safepoints (on return, at calls, and in loops), rather than for every bytecode of a method.

Profile data consists of several collection of info:
* per-method counters:

  * invocation_counter - Incremented before each activation of the method - used to trigger frequency-based optimizations
  * backedge_counter - Incremented before each backedge taken - used to trigger frequency-based optimizations
  * Previous time the rate was acquired
  * Events (invocation and backedge counter increments) per millisecond
  * invoke_mask per-method
  * backedge_mask per-method
  * Total number of events saved at previous callback
  * Count of times method was exited via exception while interpreting
  * number_of_breakpoints, for fullspeed debugging support
  * Highest compile/OSR level this method has ever seen.

* detailed: instruction-level counts, several invocation/backends counts with timestamp, data on branches, call receiver types, typechecks (checkcast, instanceof, aastore). but collecting it adds 35% overhead over just per-method counters


Whole-Program Compilation - all code must be available at compile-time. This allows several optimizations
• Enables monomorphization which increases inlining opportunities and avoids the need to box primitives.
• Enables aggressive dead code elimination and tree shaking which significantly reduces code size.
• Enables cross namespace/module optimizations.

In the past, requiring access to the entire source code of a program may been impractical. Today, systems are sufficiently performant that JavaScript, Python, PHP, and Rust have ecosystems where there is no separate compilation, and arguably Java pioneered this model with JIT compilation not paying any attention to module boundaries. Similarly Google and Facebook use monolithic repositories of source code, but have caching optimizations so that developers may use the cloud.

Optimization
============

For a lot of compilation decisions we have several choices and want to pick the best one based on some criterion. Generally, there are various measurements to try to minimize. E.g. at compile time, there are various relevant metrics: execution time, memory usage, power usage. Similarly at runtime, there are more metrics: execution time, power usage, memory usage, executable size, throughput (work/time), latency (time from request to response). The runtime stuff is pretty loose - pretty much anything that can be estimated is fair game.

Complicating optimization, these criteria are not hard numbers but probabilistic variables, because computer performance depends on many uncontrollable factors hence is best treated is nondeterministic. We can consider simple statistics such as worst-case, best-case, average/mean, percentiles/quartiles, median, and mode, and differences such as range (worst-best). We can also consider moment-based values such as variance, standard deviation, coefficient of variation, skewness, and kurtosis. Going further, we can fit a probability distribution. According to the literature, execution time may be modeled by a Gumbel distribution (`ref <http://www.lasid.ufba.br/publicacoes/artigos/Estimating+Execution+Time+Probability+Distributions+in+Component-based+Real-Time+Systems.pdf>`__) or odd log-logistic generalized gamma (OLL-GG) or exponentiated Weibull (`ref <https://arxiv.org/pdf/2006.09864.pdf>`__), although these experiments should probably be redone as we are measuring different programs. The testbench is `here <https://mjsaldanha.com/sci-projects/3-prob-exec-times-1/>`__ and `here <https://github.com/matheushjs/ElfProbTET>`__ and could be extended with `gev <https://www.rdocumentation.org/packages/evd/versions/2.3-6/topics/gev>`__.

It would be great to support optimizing the code for any objective function based on some combination of these criteria. But that's hard. So let's look at some use cases:

* For a focused objective like running static verification, all we want to see the error messages so total elapsed compile time is the only measurement. Maybe we even want to disable outputting a binary, and all associated tasks.
* For a compile-run cycle run locally, e.g. a REPL or debugging session, we most likely just care about compile time plus run time execution time.
* For release builds, the main optimization criteria is some runtime criterion, like latency, execution time, etc. As a second constraint there is probably a compile time budget - although the binary will be used for some time, a 3 week compile time is probably not feasible. Thirdly maybe some "cost to compile" calculation.
* For CI builds on PRs, done in a cloud environment with 1000s of builds a day, "total cost to test" (compile+run) is most important. The main contributor to cost is power usage, but there could also be some  "machine rent / hour" cost.
* For compiling on Raspberry Pi, we mainly just want to get a build at all, but also it would good if it was fast. Maximum amount of memory, minimize some linear combination of compile time and runtime.
* For embedded, we want a small executable size (not the smallest possible though, there is probably a known budget like 64K), and to minimize runtime and compile time.

It seems the main objective function is always a weighted linear combination, and then we may want to add hard limit constraints (inequalities). So that's what we'll support initially, it's already better than GCC / Clang because you can tune the weights explicitly.

We use branch-and-bound to explore the possibilities. With good heuristics even the truncated search algorithm should give good results. The goal is to quickly find bottleneck code regions that have significant effects on performance and compute good optimizations quickly. Then another profiling build to test that the proposed changes were correct.

There is also ISA selection and tuning for specific machines and CPUs. ISA, timing, cache, and memory characteristics are available for specific CPUs, but compiling specifically for a single CPU is not done often. Usually for x86 the code is compiled to work on SSE2 (since it's part of AMD64) and tuned for a "generic" CPU. The definition of this is vague - for `GCC <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81616>`__ and `LLVM <https://reviews.llvm.org/D118534>`__ it seems to be Haswell with a few slow cases on other architectures patched. It is supposed to be "an average of popular targets", so using a weighted sum of processors according to sales is most appropriate, but per-CPU-model sales data doesn't seem to be available easily. `PassMark <https://www.cpubenchmark.net/share30.html>`__, `3DMark <https://benchmarks.ul.com/compare/best-cpus?amount=0&sortBy=POPULARITY&reverseOrder=true&types=MOBILE,DESKTOP&minRating=0>`__, and `UserBenchmark <https://cpu.userbenchmark.com/>`__ publish their list of most benchmarked processors, which is probably good enough.

Formally proving optimizations correct is a good idea, as they are often buggy.

E.g. overloading/dispatch can be implemented in a variety of ways, specialized for call site - generally it boils down to branching on some condition (binary search), or doing a table lookup. The fastest solution depends on which clauses are relatively hot, but in general we don't know which clauses are hot.

Profile-guided optimization is an effective solution to this lack of information: we instrument a binary with counters for the various questions we might ask, and generate a profile with the answers. We might need to run a binary several different times to get good coverage so we also need a way to combine profiles together, i.e. profiles form a commutative monoid. Profiles themselves introduce a "Heisenbug" problem: we cannot measure the detailed performance of an unprofiled program, and turning profiling off may change the performance significantly. The solution is to build with profiling support for almost all of the compilation pipeline. We should only omit profiling instructions for non-profiled builds at the assembly level. And if we use hardware-assisted sampling profiling then we don't even need profiling instructions, in many cases, so profiling can simply be always enabled. Still, if we are using profile information all the time and making major decisions based on it, it is important to be mostly accurate even on the initial run, so a good approximation is also key. (TODO: approximation of profiles is probably a whole research area, explore)

Optimization variables
======================

The variables controlled by the optimization criteria include the standard optimization flags and more. Speculative inlining possibilities, register allocation, instruction scheduling, instruction selection, lifetimes of various compile-time caches,

Build model
===========

We have several complicating features:

* Cross compilation: In general, we have not one system, but two systems. To use the newer `Clang <https://clang.llvm.org/docs/CrossCompilation.html>`__ terminology, there is the **host** system where the program is being built, and the **target** system where the program will run. When the host and target systems are the same, it's a native build; otherwise it's a cross build.

  The older `GNU terminology <https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html>`__ uses a triple, build/host/target; but the "target" there is really a configuration option, namely the supported target of the compiler that will run on the host. It is a gcc-ism to specify the supported target, as Clang is generally built to support all supported targets. Since remembering whether the build system builds the host or vice-versa is tricky, overall the Clang terminology host/target/supported targets seems clearer than build/host/target.

* Bootstrapping: We start with the source ``s`` and bootstrap compiler ``cB``, an old compiler using the old ABI. Then we build stage 1 ``c1=run(cB,s)``, new compiler on old ABI (targeting the host), and stage 2 ``c2=run(c1,s)``, new compiler on new ABI (targeting the target). We can test stage 2 (the "compiler bootstrap test") by building a new compiler ``c3=run(c2,s)``. If the build is deterministic, ``c3`` should be bit-identical to ``c2``. With multiple bootstrap compilers ``cB``, we can use diverse double-compiling :cite:`wheelerFullyCounteringTrusting2010` to increase our confidence in the correctness of the stage 2 compiler.

The toolchain (gcc, llvm, as, ld, ar, strip, etc.) should be target-dependent, information stored in a YAML file or similar
the package set is also target-dependent. some packages that are pure data are target-independent

 We can also run the test suite to compare outputs of ``c1`` and ``c2``. But we cannot compare performance of ``c1`` and ``c2``, because they use different ABIs, and also ``cB`` may be buggy so ``c1`` and ``c2`` may not behave exactly the same.

The compiler depends on libraries. The bootstrap compiler does not provide updated libraries, so we must build the libraries for the Stage 1 compiler.

build stage 2 compiler with the stage 1 compiler using the stage 1 package database ship with the stage 2 compiler). As such, the compiler is built with the identical libraries that it ships with. When running / interpreting byte code, we need to dynamically link packages and this way we can guarantee that the packages we link are identical to the ones the compiler was built with. This it is also the reason why we don’t have GHCi or Template Haskell support in the stage 1 compiler.

Complex bootstrap
=================

Actually bootstrapping is more complex. The compiler is really two components, an interpreter and a specializer. The input program can take arguments. The interpreter can take arguments (dialects, libraries). The specializer can take arguments (bytecode, optimization instructions, plugins). The output program can take arguments (compiled objects, runtime components such as libc or a garbage collector). All of these arguments and options aren't handled easily.


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

IR dump
=======

A good compiler can get 80% of the code to a fast-enough state. But nontrivial hot spots will still need hand-optimizing and tuning. At first it can be good to tweak the original code to get it to generate IR differently, but eventually the algorithm is set and the micro-optimizations matter, so you want to bake in the low-level implementation.

With a wide-spectrum language the IR is the same language as the original, just using lower-level operations. So you can compile source-to-source or directly write in the IR. For example SQL is declarative but being able to write a functional program using the underlying sort, filter, merge anti-join, etc. operations would be useful.

There are many levels to the pipeline, and each one is useful. For an interpreted program the only step that can't be represented is actually running the program, e.g. converting ``print "Hi" exit`` to output.

Incremental compilation
=======================

Incremental compilation reduces rebuild time. With a good incremental build system, optimizations can be rechecked rather than rediscovered, so that the program doesn't actually spend much time optimizing even though it has expensive optimizations.

Hot reloading
=============

Hot reloading or "edit and continue" is the ability to change code and resources of a live application without restarting it. It speeds up the edit-test cycle because you can stay on a certain state of the program without needing to spend time to recreate it. It can be useful for games, UI design, or data analysis.

Edit and continue is really a debugger feature, because usually you edit the code while paused on a breakpoint, rather than while the program is actually running. Integrating with omniscient debugging is probably best, so you can manually select an old state and then evolve it using the new transition rules. For example when editing the jump height for a jump'n'run game, you probably don't want to continue from the game's start, or even the first jump input, but rather to just before the one tricky jump in the middle of the level. There is no indication of this magic location in the code or program state besides the player's x-coordinate being a certain value.

Erlang has hot code swapping, Smalltalks and Lisps have "live programming." Assisting System Evolution: A Smalltalk Retrospective is a recommended read.

The most basic implementation is to patch functions calls so they call a new function instead of an old one. A JIT already does this kind of patching when switching from interpreted to optimized code, so can do it easily. With ahead-of-time you can compile a new DLL, duplicate it to avoid locking, load it, and swap out the function pointer, but it requires specially marking the hot-reloadable methods.

Functions generally assume a fixed set of types and a fixed memory representation for all types. Changing the types or their representation can break program invariants and cause memory corruption. But it is possible - there are some projects for live kernel patching that can patch in-memory data structures to the correct format.

State is also an issue because the memory manager must be aware of the local state of a piece that reloaded and avoid leaking memory. In the case of handles such as an OpenGL context the desirable behavior is to transfer them over to the new code, but if the initialization code is changed then the handle should instead be closed and re-initialized. So we see some sort of incremental program execution going on.

Testing
=======

The main strategy is "golden tests". To start you run some part of the compiler and serialize the output to some human-readable format. Then you store that data as the "expected" output, and run the test again every compile to make sure it still produces the expected output, comparing with diff or similar. Then you check a few changed outputs to find any bugs, fix what needs fixing, and for any non-bug changes, there should be a way to automatically update all the expected outputs to the current outputs.

For a parser, the main test is "integration testing" by giving it examples of source code, full modules, and verifying that it parses to the right AST / fails with the expected output. There is also "unit testing" where you test each grammar production rule individually, e.g. parse an expression or a statement or a block.

Another parser test is "stress testing", generating random valid code samples. The generator doesn't need to exercise every parser path, just the common ones. It pays off in that you understand the grammar better and you can test the performance of your parser.

For the IR golden tests are fine (compare after each optimization pass).
