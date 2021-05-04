Compiler design
###############

Scale
=====

As of 2016, The Google repo has 1 billion files, of which 9 million are code

Pipeline
========

The start is a parser - this will be written later once partial evaluation is sufficient to specialize naive parsers efficiently. For now the code is input using ADTs and parentheses. The parser will also add token start/end and other debugging information.

Next is the fexpr interpreter loop. This starts with the ADT tree and produces evaluated code. Parts of the evaluator include turning name-strings into direct evaluation graph references and compiling pattern matching to tag scrutinization.

Currying is handled by a pass that creates partially-applied functions using the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`. Initially all user code starts out using one-argument functions.

Currently there are no code targets implemented - the main interactive element is an interpreter. There are some papers on partial evaluation and supercompilation that will probably get used for a C backend or a JIT or something.

Flags
=====

In general flags can take 4 levels: ignore, warn, error, and fix. Ignore ignores the issue as much as possible. Warn issues a warning but otherwise ignores the issue. Error stops the compiler from continuing. Fix automatically constructs a fix for the issue and modifies the source file(s) in-place.

Error messages
==============

Since Stroscot uses model checking, most failures will end up producing a counterexample. The counterexample may not be minimal, but it is much easier to debug a concrete instance than to try to figure one out from contextual information.

For source locations we produce the start/end span of two (filename, line number, column number) tuples. Go uses an efficient memory-map-like model from these tuples to integers, to avoid passing around strings. But it isn't clear how to make this incremental, as removing a file causes all the integers to change. One idea is to store (filename hash, byte offset) as a 64-bit code, since then we can compare before/after within files and quickly check if two locations are equal.

Fuel
====

A technique for testing the compiler and systems in general is to use a "fuel" counter that decrements every time a certain operation is performed, and do something interesting when the counter reaches 0 such as finishing the optimizations or throwing an exception.

Optimization
============

For a lot of compilation decisions we don't have enough information - e.g. for overloading, how should we code the dispatch table? Profile-guide optimization is an effective solution to this: we instrument a binary with counters for the various questions we might ask, and generate a profile with the answers. We might need to run a binary several different ways to get good coverage so we also need a way to combine profiles together. If the profile shows that we don't use a code path very often, then we can de-optimize it and use a slow version. But if it's a hot path then we want to streamline that path as much as possible.

With respect to optimization we have various criteria to minimize.
* Compile time - when we are just running static verification
* Run time - faster programs are more useful
* Execute time (compile + run) - the edit-compile-test cycle for debug builds, and similarly REPL loops
* Output size - as binaries are often transferred across a network
* Other statistics for Compile/Run/Execute - memory usage, power usage

The options O0, On, Og, Os/Oz correspond to using compile, run, execute, and output size as the primary objective.

Profiles themselves introduce a "Heisenbug" problem: we cannot measure the detailed performance of an unprofiled program. The solution is to build with profiling support for almost all of the compilation pipeline. We should only omit profiling instructions for non-profiled builds at the assembly level. And if we use hardware-assisted sampling profiling then we don't even need profiling instructions, in many cases.

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
