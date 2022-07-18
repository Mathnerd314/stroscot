Compiler design
###############

Dynamically typed languages are tricky to compile efficiently. There’s been lots of research on efficient JIT compilers for dynamic languages - SELF, Javascript, PyPy, Java - but these are quite involved, and still slower than C. Ahead-of-time compilation is possible as well but not explored, and needs profile data to work properly. Currently Stroscot is aiming for AOT with profiling.

Scale
=====

As of 2016, The Google repo has 1 billion files, of which 9 million are code

Pipeline
========

The start is a parser - this will be written later once partial evaluation is sufficient to specialize naive parsers efficiently. For now the code is input using ADTs and parentheses. The parser will also add file and line number information, token start/end, call stack, and other debugging information.

Next is the fexpr interpreter loop. This starts with the ADT tree and produces evaluated code. Parts of the evaluator include turning name-strings into direct evaluation graph references and compiling pattern matching to tag scrutinization.

Currying is handled by a pass that creates partially-applied functions using the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`. Initially all user code starts out using one-argument functions.

Currently there are no code targets implemented - the main interactive element is an interpreter. There are some papers on partial evaluation and supercompilation that will probably get used for a C backend or a JIT or something.

Re LLVM IR vs API: the API is much more unstable than the IR. Also a blog post I read suggested that the IR and the API were about the same as far as performance.

Flags
=====

In general flags can take 4 levels: ignore, warn, error, and fix. Ignore ignores the issue as much as possible. Warn issues a warning but otherwise ignores the issue. Error stops the compiler from continuing. Fix automatically constructs a fix for the issue and modifies the source file(s) in-place.

There is also the value 'default' to set it to the default.

Error messages
==============

Since Stroscot uses model checking, most failures will end up producing a counterexample. The counterexample may not be minimal, but it is much easier to debug a concrete instance than to try to figure one out from contextual information.

For source locations we produce the start/end span of two (filename, line number, column number) tuples. Go uses an efficient memory-map-like model from these tuples to integers, to avoid passing around strings. But it isn't clear how to make this incremental, as removing a file causes all the integers to change. One idea is to store (filename hash, byte offset) as a 64-bit code, since then we can compare before/after within files and quickly check if two locations are equal.

Werror
======

Werror is an option, as usual. If you want a hard dependency on the compiler version, then feel free to use it, otherwise it's best to leave it unset so that users can use different compiler versions that emit different warnings.

Fuel
====

A technique for testing the compiler and systems in general is to use a "fuel" counter that decrements every time a certain operation is performed, and do something interesting when the counter reaches 0 such as finishing the optimizations or throwing an exception.

For example instead of testing for stack overflow we can test for running out of fuel. Stroscot's execution context doesn't involve a stack.

Optimization
============

For a lot of compilation decisions we don't have enough information - e.g. for overloading, how should we code the dispatch table? Profile-guided optimization is an effective solution to this: we instrument a binary with counters for the various questions we might ask, and generate a profile with the answers. We might need to run a binary several different ways to get good coverage so we also need a way to combine profiles together. If the profile shows that we don't use a code path very often, then we can de-optimize it and use a slow version. But if it's a hot path then we want to streamline that path as much as possible.

With respect to optimization we have various criteria to minimize.
* O0 - Compile time - when we are just running static verification
* On - Run time - faster programs are more useful
* Og - Execute time (compile + run) - the edit-compile-test cycle for debug builds, and similarly REPL loops
* Os/Oz - Output size - as binaries are often transferred across a network
* O? - Other statistics for Compile/Run/Execute - memory usage, power usage

Profiles themselves introduce a "Heisenbug" problem: we cannot measure the detailed performance of an unprofiled program. The solution is to build with profiling support for almost all of the compilation pipeline. We should only omit profiling instructions for non-profiled builds at the assembly level. And if we use hardware-assisted sampling profiling then we don't even need profiling instructions, in many cases.

General purpose use
Prepackaged software is very often expected to be executed on a variety of machines and CPUs that may share the same instruction set, but have different timing, cache or memory characteristics. As a result, the code may not be tuned to any particular CPU, or may be tuned to work best on the most popular CPU and yet still work acceptably well on other CPUs.
Special-purpose use
If the software is compiled to be used on one or a few very similar machines, with known characteristics, then the compiler can heavily tune the generated code to those specific machines, provided such options are available. Important special cases include code designed for parallel and vector processors, for which special parallelizing compilers are employed.
Embedded systems
These are a common case of special-purpose use. Embedded software can be tightly tuned to an exact CPU and memory size. Also, system cost or reliability may be more important than the code's speed. For example, compilers for embedded software usually offer options that reduce code size at the expense of speed, because memory is the main cost of an embedded computer. The code's timing may need to be predictable, rather than as fast as possible, so code caching might be disabled, along with compiler optimizations that require it.

Need optimizations for:
* avoiding intermediate structures and dead or redundantly duplicated computation
* storing arrays on the heap in the most efficient of a few straightforward ways
* boiling away higher-order functions into tedious boilerplate

Output
======

The simplest compiler writes out a file like:

::

  -- This is generated code - see <file> for source
  interpret = <boilerplate code for interpreter>
  data = "<contents of source file>"
  main = interpret data

This amounts to using a no-op specializer. But we can use a more intelligent specializer to produce more efficient code.

versioning of time/date
identifier minimization/translation
unit test
random input testing
quasiquotation
typechecking

RTS flags should be stored into ABI hashes in installed libraries to avoid mismatching incompatible code objects.

Compilation models
==================

Separate compilation is really incremental compilation - avoiding re-doing work that doesn't depend on other files. The ``.o`` files are not useful by themselves, so the compile-link process can be replaced with an incremental compilation database and a command that directly produces an executable or DLL (assembly). If memory is a concern then results can be unloaded/loaded from the database.

Executables and DLLs are defined by a stable ABI / set of entry points. Inlining depends on the content of the code, so we cannot inline, or in general do any optimizations across the ABI boundary.

Cross compilation
=================

In cross compilation we have not one system, but two systems. To use the newer `Clang <https://clang.llvm.org/docs/CrossCompilation.html>`__ terminology, there is the **host** system where the program is being built, and the **target** system where the program will run. When the host and target systems are the same, it's a native build; otherwise it's a cross build.

The older `GNU terminology <https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html>`__ uses a triple, build/host/target; but the "target" there is really a configuration option, namely the supported target of the compiler that will run on the host. Only compilers need to specify supported targets. Since remembering whether the build system builds the host or vice-versa is tricky, overall the Clang terminology host/target/supported targets seems clearer than build/host/target.

the toolchain (gcc, llvm, as, ld, ar, strip, etc.) should be target-dependent, information stored in a YAML file or similar
the package set is also target-dependent

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

We may also want to read configuration, e.g. the target platform properties (word size, endianness, etc.).


Also we want to do computations with no runtime inputs, like 1+2.


Compiler ways
=============

Some options are called "compiler ways". They can be combined (e.g.
threaded + debugging). The main issue is they affect the ABI.

- use the multi-threaded runtime system or not
- support profiling or not
- use additional debug assertions or not
- use different heap object representation (e.g. ``tables_next_to_code``)
- support dynamic linking or not

Depending on the selected way, the compiler produces and links appropriate
objects together. These objects are identified by a suffix: e.g. ``*.p_o`` for an
object built with profiling enabled; ``*.thr_debug_p.a`` for an archive built with
multi-threading, debugging, and profiling enabled. See the gory details on the
`wiki <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/compiler-ways>`__.

Installed packages usually don't provide objects for all the possible ways as it
would make compilation times and disk space explode for features rarely used.
The compiler itself and its boot libraries must be built for the target way.


Compiler memory management
==========================

For the compiler itself, a trivial bump or arena allocator is sufficient for most purposes, as it is invoked on a single file and lasts a few seconds. With multiple files and large projects the issue is more complicated, as some amount of information must be shared between files. Optimization passes are also quite traversal-intensive and it may be more efficient to do in-place updates with a tracing GC rather than duplicating the whole AST and de-allocating the old one. Two other sources of high memory usage are macros and generics, particularly in combination with optimizations that increase code size such as inlining.

Overall I don't see much of an opportunity, SSD and network speeds are sufficient to make virtual memory and compile farms usable, so the maximum memory is some large number of petabytes. The real issue is not total usage but locality, because compilers need to look up information about random methods, blocks, types etc. very often. But good caching/prefetching heuristics should not be too hard to develop. In practice the programs people compile are relatively small, and the bottleneck is the CPU because optimizations are similar to brute-force searching through the list of possible programs. Parallelization is still useful. Particularly when AMD has started selling 64-core desktop processors, it's clear that optimizing for some level of that, maybe 16 or 32 cores, is worthwhile.

Documentation generator
=======================

The documentation generator provides a nice way to browse through a large codebase. The documentation comments, type annotations, and argument names are pulled out for each function, and the code is accessible though an expando. The code has hyperlinks for all terms to the place where they are defined, or opens a menu if the term is overloaded. Code is prettified to use Unicode or MathML formulas where appropriate. There's regex-based search, and special searches for identifiers. Also useful is the call graph, in particular showing what functions call a given function. This can just be a link.

As far as targets, only HTML seems particularly important.

Refactorer / reformatter
========================

The refactoring tool reduces the amount of effort it takes to maintain code. It reads a program from a source file, rewrites the code according to specified rules, and writes the program back to the file. It automates easy, repetitive, tedious changes. When the rewrite cannot be done automatically the rule can insert ``TODO: check rule XXX`` comment markers. It provides a way to rename or inline functions, eliminate dead code, and transform old idioms to new idioms.

Theoretically we should provide both forward and backward compatibility for language versions via the refactoring tool, but practically I think only backwards compatibility (v1 -> v2) is relevant.

With no rules, the refactoring tool functions as a reformatter. Python's Black started out as opinionated but eventually grew lots of options - probably the reformatter should be very flexible, but have a preset default that's used for the compiler.

Inspired by gofix / `gofmt <https://go.dev/blog/gofmt>`__ .

Language server
===============

For integration with VSCode and other IDEs.

Interactive shell
=================

A REPL loop based on eval. Available from command line as bare ``stroscot`` or ``stroscot -i files``, and from API as a library function ``replLoop env`` or similar. Supports expressions and block syntax from the main language, and commands. Commands are built-in functions to the interpreter, like ``shell clear`` which runs ``clear`` in the shell. Or maybe the syntax should be ``:shell clear`` to avoid clashing with whatever is loaded. But namespaces are a thing, ``repl.shell clear``. The syntax will have to be worked out.

Full command list:
* shell, run shell thing

  * change/print current directory
  * list files

* show information about symbol
* push/pop level of interactive environment (source files are level 0, IE starts at level 1, and more can be added)
* clear definitions for specified symbols or current level of interactive environment
* load file
* dump/load interactive environment to/from text file
* reset - clear IE, load sources file from disk
* reload - dump IE, load sources file from disk, load IE dump
* quit process
* debugger commands
* profiler commands

Notebooks
=========

Ideally, notebooks would be incremental. Running (shift-enter) would act as if it reran the notebook from the start up to the selected cell. For speed the computation would be cached incrementally, so long-running computations would be skipped if possible. This model also allows putting interactive sliders in and quickly updating graphs.

But, jupyter's kernel `protocol <https://jupyter-client.readthedocs.io/en/latest/messaging.html>`__ is just a dumb "execute this string of code" REPL, no information on what cell it's from.
So we would have to hack jupyter to get this to work.

The simplest hack is concatenate all the cells to be executed into a string, and then each code execution is independent. Another idea is to add a "soft_reset" message. Then the frontend sends a soft reset followed by each executed code cell. More advanced is sending the execution number in the code execute message and omitting the code if it's the same as the previous execution - I don't know if sending all the code is much of a bottleneck.

For now living with REPL behavior seems fine.

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

Logic
=====

Doing logic in Stroscot is confusing because the reduction semantics itself uses logic. The proof tree in the reduction semantics is the program being executed, while the proof tree in type theory is automatically deduced from the type (formula) by a meta-program (theorem prover).

Debugger
========

Need this. Reversible debugging.

* breakpoints: set/clear/list
* backtrace of exception
* stepping: single step, step out, continue until breakpoint, run ignoring breakpoints
* evaluate pure expression


Profiler
========

Measure
* time and memory usage.
* throughput (calls/second)
* A/B testing of multiple implementations
for functions, expressions, programs, etc.

Use statistical sampling and hardware performance counters to avoid overhead.


IR dump
=======

A good compiler can get 80% of the code to a fast-enough state. But nontrivial hot spots will still need hand-optimizing and tuning. At first it can be good to tweak the original code to get it to generate IR differently, but eventually the algorithm is set and the micro-optimizations matter, so you want to bake in the low-level implementation.

With a wide-spectrum language the IR is the same language as the original, just using lower-level operations. So you can compile source-to-source or directly write in the IR. For example SQL is declarative but being able to write a functional program using the underlying sort, filter, merge anti-join, etc. operations would be useful.

There are many levels to the pipeline, and each one is useful. For an interpreted program the only step that can't be represented is actually running the program, e.g. converting ``print "Hi" exit`` to output.

Error messages
==============

From `Elm <https://elm-lang.org/news/compiler-errors-for-humans>`__:
* prefer "runtime" errors of dynamically-typed languages
* code location: show the line number, full line of input code, and mark range with ``^^^```
* give every message a hint / suggested fix
* color: red to draw attention to location, blue to separate messages
* layout: general message above code, hints below code
* VSCode integration


Features
========

A feature is a distinct chunk of compiler functionality. A feature toggle allows turning on a feature conditionally in the code. This allows testing features and integrating them on the main branch while isolating them from other tests and software releases. Identifying the scope of a feature is simply searching for uses of the feature flag. The feature toggles are stored in a big record of booleans in one place, to make finding them easier.

A feature can be alpha, beta, or stable.

* alpha: untested, may be changed or removed without automigration
* beta: well tested, details may change, automigration will be provided
* stable: beta but perceived as unlikely to change further

Generally only alpha features need toggles, beta features have automigration so can be turned on all the time.

All features should be documented in the commentary / reference in full detail, describing edge cases. Alpha features should be marked as alpha. Write a how-to if the feature is not obvious. Generally new features are advanced enough that they do not affect the tutorial.

Moving a feature from alpha to beta should have a PR with documentation links and test case links. The actual process is to change the feature list to set the feature's beta release date as the current date, so the compiler knows to warn that the feature declaration is no longer needed and to declare the feature when bootstrapping, and to modify all uses of the feature toggle in the code to the case where the feature is present (avoiding toggle debt).

