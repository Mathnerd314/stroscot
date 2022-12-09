Compiler design
###############

Dynamically typed languages are tricky to compile efficiently. There’s been lots of research on efficient JIT compilers for dynamic languages - SELF, Javascript, PyPy, Java - but these are quite involved, and still slower than C. Ahead-of-time compilation is possible as well but not explored, and needs profile data to work properly. Currently Stroscot is aiming for AOT with profiling.

Pipeline
========

The start is a parser - this will be written later once partial evaluation is sufficient to specialize naive parsers efficiently. For now the code is input using ADTs and parentheses. The parser will also add file and line number information, token start/end, call stack, and other debugging information.

Next is the fexpr interpreter loop. This starts with the ADT tree and produces evaluated code. Parts of the evaluator include turning name-strings into direct evaluation graph references and compiling pattern matching to tag scrutinization.

Currying is handled by a pass that creates partially-applied functions using the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`. Initially all user code starts out using one-argument functions.

Currently there are no code targets implemented - the main interactive element is an interpreter. There are some papers on partial evaluation and supercompilation that will probably get used for a C backend or a JIT or something.

Error levels
============

In general flags can take 4 levels: ignore, warn, error, and fix. Ignore ignores the issue as much as possible. Warn issues a warning but otherwise ignores the issue. Error stops the compiler from continuing. Fix automatically constructs a fix for the issue and either modifies the source file(s) in-place or outputs a patch. There is also the value 'default' to set it to the default of these 4 levels.

Stroscot is designed so that as little as possible is a hard error. Warnings allow continuing with compilation and finding all the errors in a file, hence produce more information than errors that simply stop at the first one. This choice is inspired by the ``--keep-going`` option in many build systems and the ``-fdefer-type-errors`` flag in GHC. In both cases a hard error is turned into a diagnostic; this is clearly a trend.

Onw reason to stop at the first error is to avoid wasted CPU cycles, but CPU is cheap these days and incremental building means going as far as possible is probably cheaper than rerunning from scratch each time. So Stroscot always processes as far as possible. Another reason is to avoid console scroll; for this there is a flag ``-ferror-limit=123`` to limit output.

Werror
------

The traditional Werror that turns warnings into errors is still an option, for those who like rerunning builds by hand at the command line and don't want to scroll through pages of output. But it is not recommended for use in build scripts. The main issue is that it introduces a hard dependency on the compiler's set of warnings. For example, suppose:

* Compiler version A has W1=ignore, W2=warning
* Compiler version B has W1=warning, W2=ignore, W3=warning

W2 is fine and will not cause any problems upgrading. But W1 and W3 will break the build when upgrading.

To solve this Stroscot has warning presets so you can use a list of options like ``--warning-preset=A -W3=error -Werror`` and selectively enable new warnings, upgrading incrementally. Of course this doesn't help with downgrading to a compiler version where the preset is not available, and there is still the issue of individual compiler warnings becoming smarter and finding errors where they didn't before.

Error messages
==============

Error messages are the UI of the compiler. Languages such as `Elm <https://elm-lang.org/news/compiler-errors-for-humans>`__ and Rust claim to have invested significant effort into improving their error messages, so Stroscot probably should too. Fortunately this has been researched since 1965, with a recent literature survey in :cite:`beckerCompilerErrorMessages2019`, so we aren't going in blind.

A distinction that seems useful is malformed program vs violated contract.

* A malformed program is outside the specifications of the language, e.g. an unbound identifier or syntactic error. With non-textual editing, it becomes impossible to insert malformed constructs, so the only malformed programs are incomplete programs. Programs are typically malformed due to "trivial" parse errors, so typically we would like to find a minimal correction to apply to make the program syntactically correct, so that we can find more interesting errors.

* A program with a violated contract is more interesting: it has a defined runtime semantics per the language semantics, but a type signature or assertion is violated, meaning that the programmer's intentions are not satisfied. Typically we want to output a "crippled" program that fails when it encounters the error, rather than attempting a fix.

It is probably worth splitting the ID numbers into two separate sets, Mnnn vs Cnnn, for malformed vs contract.

One issue is whether an error is reported (completeness) and if so when, either compile-time or runtime. Generally runtime errors have the execution trace available and produce more precise information, but are incomplete, while compile-time errors are reported earlier and are complete for some criteria. In Stroscot this distinction is muddied because we use model checking. Model checking essentially simulates all runs of a program at compile time, so is complete and reports back early. But a model checking failure will end up producing a counterexample, basically a failing runtime execution trace, so we get the precise information. This can be a bad example so we need to apply minimization. But generally, So Stroscot gets the best of both worlds. Of course the model checking itself is tricky to implement efficiently. But a small price to pay for avoiding confusing type inference errors.

Two more issues are locality and source mapping, ensuring the error message is reported at the location where the fix should be directed. A missing close brace may lead to an error only at EOF. Indentation sensitivity mitigates this particular error. Another issue is something like ``a = <expr>; ...; assert (a != 0)``, where the assertion is much later than the creation of the value that caused the error. We need a summarizer that tries to guess the important variables and outputs the callstack or other traditional details. Macros have similar problems - is the error in the macro use site or definition site?

Richer error handling such as a location system also introduces a performance concern, requiring more compiler engineering. For example we need an efficient mechanism for storing the start/end source location spans, consisting of two (filename, line number, column number) tuples, as passing around fully formatted strings would be slow. Go uses a map between locations and integers where file A maps to 1-100 and file B maps to 101-200, so that e.g. 150 maps to file B byte offset 50. But it isn't clear how to make this incremental, as removing a file causes all the integers to change. One idea is to store (filename hash : U32, byte offset : U32), since files are unlikely to be larger than 4 gigabytes. Whatever the solution, we should be able to compare same file, before/after within files, and if two locations are equal.

The wording may be important. A Java editor called Decaf intercepted and re-worded 30 of the most frequent Java error messages, and was found to significantly reduce error frequency and indications of struggling students. However a different study did not, suggesting the effects are weak. Still, some basic attempt at clear and friendly language is appropriate. Specific guidelines from :cite:`beckerCompilerErrorMessages2019`:

* Aim for readability and ensure comprehension by using plain/simple language, familiar vocabulary, and clear/concise/brief messages. Avoid cryptic jargon. There are multiple formal measures of readability for ‘normal’ prose, such as the Fry Readability Graph, Flesch formula, Dale-Chall formula, Farr-Jenkins-Paterson formula, Kincaid formula, Gunning Fog Index, and Linsear Write Index, but nobody has applied these to programming errors or devised a formal readability metric.

* Reduce cognitive load: Include all relevant information and reduce redundancy so the user does not process the same information twice. Use multiple modalities to provide feedback. The error message should use the minimal amount of boilerplate so that a developer can process the information quickly. But there should also be enough that someone who has never seen the message before can understand it.

* Provide context: Provide information about the relevant program code, such as the location of the error (explicitly or as an IDE annotation) and relevant symbols, identifiers, literals, and types involved in the error, as well as the program state such as variable values and stack traces. If an error message can appear in different contexts or could be sourced to multiple locations then disambiguate.

* Use a positive tone, and generally aim for a consumer UX: Novices are shaken, confused, dismayed, and discouraged by violent, vague, or obscure phrasing. Messages should be polite, restrained, friendly, and encouraging, making the computer seem subservient. Negative words like incorrect, illegal, and invalid should be avoided. Also `general UX guidelines <https://www.oreilly.com/library/view/designed-for-use/9781680501902/f_0298.xhtml>`__ advise to not place fault or blame, scold, or condemn the user (programmer). Sarcastic humor also seems counter-productive, although minor 'fun' humor may be OK but runs against briefness. Another `study <https://faculty.washington.edu/ajko/papers/Lee2011Gidget.pdf>`__ found personified I-messages such as "I don’t know what this is, so I’ll just go on to the next step" improved novice's knowledge acquisition rates and thus amount of levels completed in a set time. Of course `others <https://www.codewithjason.com/whos-blame-bad-code-coders/>`__ argue the coders are objectively the ones at fault, but this seems to be an impossible to win argument, like arguing that your girlfriend is fat. Even if it's true winning the argument doesn't make anyone better off. Psychology is weird. For children, the computer should not appear as if it is a sentient human, so as to develop the correct mental model.

* Provide a catalog of similar error examples (`Elm <https://github.com/elm/error-message-catalog>`__, `Rust <https://doc.rust-lang.org/error-index.html>`__): Providing handpicked, worked examples of how each error message is triggered can improve novices' understanding and also function as a compiler test suite. Particularly a side-by-side incorrect/correct layout with the differences highlighted has been studied and found helpful. However, brevity offers many advantages, and a study showed novice programmers can be confused as to whether the example code in the message is their code. There is also the issue of overdependence on programming by example. As such relegating the examples to a separate webpage, so there is a clear separation of example from actual, seems the best approach. For example, Rust and Microsoft give each error message a unique ID, and then has a page of all the IDs and their description. This catalog and ID mechanism has not been studied in the literature and poses a discoverability hazard, but a hyperlink in the error message seems sufficient - showing the catalog entry in the error message would be documentation overkill unless it is really short. The quintessential error catalog is Stack Overflow, which indexes both standard error messages and obscure library codes or memory addresses. Popular responses are upvoted and can be quite useful to both novices and experts. Compared to formal reference documentation, the catalog can provide briefer and more concrete and specific assistance. With a feedback loop between catalog and compiler, error message codes can be refined to cover common issues more precisely. However it should be noted that there is little point in trying to organize the catalog with categorization - agreement among category raters was only 60% in :cite:`mccallNewLookNovice2019`. It is better to use a flat list and focus effort on specific tricky error codes rather than attempting to find patterns among errors.

* Show solutions: The actual intent of the programmer may not be clear, but the compiler can analogize from the error catalog or other sources to guess what the programmer likely intended, and either provide a literal solution or sketch the requirements a solution must satisfy. Although debatable, my definition of the difference between an example and a solution is that the solution is phrased using specific information from the actual code, whereas the example is generic to the error ID. Also, the solution is produced only when there is a high degree of certainty for its applicability, avoiding leading the user down the wrong path. When guided appropriately by solutions, novices can repair errors approximately as fast as experts. With IDE integration, solutions may be interactively accepted and applied automatically instead of being transcribed by the user, allowing even experts to benefit from faster fixing. Elm says that every error should have a solution - this is probably overkill. Solutions are doable for trivial errors like unbound identifiers or uncaught exceptions, but many semantic errors have no obvious solution and can take weeks to work out.

* Allow dynamic interaction: A simple example is Rust's ``--explain`` flag that gives more context for some errors and for others reproduces the explanation from the catalog. This is a "tell-me-more" mechanism that allows requesting more error details. In Stroscot's case, where many contract errors take the form of failing program traces, another useful tool would be interactive omniscient debugging of these failing traces, so that the programmer can take a failure of ``assert (a != 0)`` and say "where did ``a`` come from?". Both of these cannot be the main interface, because the catalog is verbose and debugging is too time-consuming, but as options they are quite helpful.

* Provide cognitive scaffolding: A user may form the wrong conceptual model and/or move too quickly through writing the program. They then have a false sense of accomplishment. It is then the error messages's job to dislodge incorrect conceptual models and point out hasty errors. The user may also have misread the problem, but solving the wrong problem is a general issue in cognition, including startups launching and failing due to market fit, so the compiler generally can't tell that the wrong problem is being solved. Anyways, the goal is to use sufficient verbiage that the user can notice their conceptual model is wrong and search out documentation to repair it. To this end, the message should mention the key constructs and relationships that must be understood, e.g. syntactic construct names, compiler terminology, and library functions.

* Use logical argumentation (maybe): :cite:`barikHowShouldCompilers2018` analyzes error messages using Toulmin's argument model, which allows 6 components (extended to 7 by Barik):

  * The claim is the main assertion to be proven.
  * The grounds are evidence and facts that support the claim.
  * The warrant links the grounds to the claim.
  * The backing supports the warrant, usually by an example.
  * The qualifier limits the claim, explaining words such as "presumably".
  * The rebuttal acknowledges other valid views but explains why they are not appropriate.
  * A resolution is a claim that a defect will be removed with a specific change. (Added by Barik)

  StackOverflow and compiler error messages used 3 argument layouts: claim alone, a simple argument consisting of claim, grounds, and warrant, and an extended argument which is a simple argument plus backing. These layouts are multiplied times 2 depending on whether there was a resolution in the claim; my notation is that "claim" means a claim without resolution. The tested results were claim < {simple,extended}, extended < claim+resolution (claim+resolution being dubbed a non-logical "quick fix" instruction).

  Per the thesis :cite:`barikErrorMessagesRational` extended arguments are mainly useful for novices and unfamiliar code. Theorizing, if the developer knows what's going on, they likely want brief messages and their preference is claim+resolution > simple > extended > others. But with an ``--explain`` flag their preference is more like extended+resolution > simple+resolution > claim+resolution > extended > simple > others. It's probably worth a survey comparing error messages of varying verbosities to confirm.

* Report errors at the right time: Generally one wants to see as many errors as possible, because rerunning the compiler every time you fix an error is slow, and as soon as possible, using static analysis tools.

Per Elm / `Tidyverse <https://style.tidyverse.org/error-messages.html>`__ the message should have a layout like "general summary, program code fragment (location),error details / hints / suggested fix". The general summary is shown on hover in VSCode, and can be expanded downwards to see the full message. The tooltip seems to be around 120 monospaced characters wide and 5 ish lines tall. The size differs based on popup type so recheck when developing for LSP; it used to be 50 characters wide for everything. There is `an old VSCode bug <https://github.com/microsoft/vscode/issues/14165>`__ open for expandable popups, and a `CSS hack <https://stackoverflow.com/questions/44638328/vs-code-size-of-description-popup>`__ that makes them larger, but probably Stroscot has to be designed to accommodate small popups.

The code fragment shows the full line of input code with file/line number, and marks the failing expression with ``^^^```. The error and location marks should be colored red so they are easy to spot. Similarly Elm uses a blue separator line ``----`` to separate messages. With the LSP integration this is already taken care of because VSCode underlines the error location in the editor and has its own UI for browsing through errors.

Fuel
====

A technique for testing the compiler and systems in general is to use a "fuel" counter that decrements every time a certain operation is performed, and do something interesting when the counter reaches 0 such as finishing the optimizations or throwing an exception.

For example instead of testing for stack overflow we can test for running out of fuel. Stroscot's execution context doesn't involve a stack.

Optimization
============

For a lot of compilation decisions we have several choices and want to pick the best one based on some measure of "performance". E.g. overloading/dispatch can be implemented in a variety of ways, specialized for call site - generally it boils down to branching on some condition (binary search), or doing a table lookup. The fastest solution depends on which clauses are relatively hot, but in general we don't know which clauses are hot.

Profile-guided optimization is an effective solution to this lack of information: we instrument a binary with counters for the various questions we might ask, and generate a profile with the answers. We might need to run a binary several different times to get good coverage so we also need a way to combine profiles together, i.e. profiles form a commutative monoid. Profiles themselves introduce a "Heisenbug" problem: we cannot measure the detailed performance of an unprofiled program, and turning profiling off may change the performance significantly. The solution is to build with profiling support for almost all of the compilation pipeline. We should only omit profiling instructions for non-profiled builds at the assembly level. And if we use hardware-assisted sampling profiling then we don't even need profiling instructions, in many cases, so profiling can simply be always enabled.

When trying to do a quick compile-run cycle, we still want to streamline hot paths so that the binary is not unusably slow, but cold spots can use a straightforward boilerplate translation that doesn't require much CPU. More generally, there are various optimization criteria to minimize during compilation. Generally anything that can be measured is fair game:

* Compile total elapsed time
* Compile power usage
* Compile memory usage
* Runtime total time
* Runtime memory usage
* Runtime power usage
* Runtime executable size
* Runtime throughput
* Runtime request latency
* Other runtime service metrics

These are generally not hard numbers but probabilistic variables, because computer performance depends on many uncontrollable factors hence is best treated is nondeterministic. A simple mean or median estimator is generally sufficient, but doing statistical hypothesis testing is more interesting. Worst case execution time is of interest in real-time systems. Execution time may be modeled by a Gumbel distribution (`ref <http://www.lasid.ufba.br/publicacoes/artigos/Estimating+Execution+Time+Probability+Distributions+in+Component-based+Real-Time+Systems.pdf>`__) or odd log-logistic generalized gamma (OLL-GG) or exponentiated Weibull (`ref <https://arxiv.org/pdf/2006.09864.pdf>`__), although these experiments should probably be redone as we are measuring different programs. The testbench is `here <https://mjsaldanha.com/sci-projects/3-prob-exec-times-1/>`__ and `here <https://github.com/matheushjs/ElfProbTET>`__ and could be extended with `gev <https://www.rdocumentation.org/packages/evd/versions/2.3-6/topics/gev>`__.

Obviously these have tradeoffs, so we need an overall objective function. For a focused objective like running static verification, all we want to see the error messages so total elapsed compile time is the only measurement. For production binaries, there will likely be a complex function for various runtime measurements based on actual costs and requirements, but compile costs will be minimal or excluded. For debugging, running in a REPL, an edit-compile-test cycle, etc., both compile and runtime factors are important so the objective function becomes even more complex. gcc, clang, etc. have various optimization profiles like O0, O1, O2, O3, Og, On, Os, Oz, etc., which we can include presets for, but it's not clear these are sufficient.

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

Documentation generator
=======================

The documentation generator provides a nice way to browse through a large codebase, ensuring that code is easy-to-read and searchable. The documentation comments, type annotations, and argument names are pulled out for each function, and the code is accessible though an expando. The code has hyperlinks for all terms to the place where they are defined, or opens a menu if the term is overloaded. Code is prettified to use Unicode or MathML formulas where appropriate. There's regex-based search, and special searches for identifiers. Also useful is the call graph, in particular showing what functions call a given function. This can just be a link.

As far as targets, only HTML seems particularly important.

The notion of "well-documented" is hard to define. For one person the source code and function names may be sufficient, while for another a tutorial that has no relation to the codebase may be more useful.

Refactorer / reformatter
========================

The refactoring tool makes it easy to analyze source code, enabling tooling such as automatic code formatting and codebase maintenance. It reads a program from source file(s), rewrites the code according to specified rules, and writes the program back to the file(s). It automates easy, repetitive, tedious changes. When the rewrite cannot be done automatically the rule can insert ``TODO: check rule XXX`` comment markers. It provides a way to rename or inline functions, eliminate dead code, and transform old idioms to new idioms. The automated migration of code from old to new versions uses the refactoring API.

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

The debugger's view of the program's state is as a large expression or term. This state evolves in steps, where each step applies a rule to a redex or calls into the OS to perform a primitive operation. We allow reversible/omniscient debugging, meaning that one can step both forward from a state (the usual) and backward from a state (query on where a value came from etc.).

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

Try as we might, no language design is perfect. Langauges inevitably change or extend their semantics over time, resulting in ecosystem fragmentation where programs end up being written in different "dialects" of the language. The evolution process aims to minimize the disruption to existing code by evolving the language in a controlled manner, in particular in discrete units of "features". The process guarantees a "compatibility promise" that the source code of existing programs written for an old language version can be automatically migrated to a new language version. Because the language evolves towards a standardized set of features, the langauge should avoid fragmentation.

A feature is a distinct chunk of compiler functionality, such as a change to the semantics of the language, a compiler plugin, or an external tool integration. A feature can be alpha, beta, or stable.

Alpha features are experimental features with little formal testing, released to get feedback. They may be documented informally or on an "alpha features" page. Alpha features have no compatibility guarantee and may be changed freely. Alpha features are kept behind feature toggles, which allow conditioning code on a feature. This allows testing features and integrating them on the main branch while isolating them from other tests and software releases. Alpha features will be removed from the compiler if they have not made any progress towards beta over the course of a year.

Beta features are implemented features that may change further. They must have a reasonable test suite and be documented in the commentary / reference in full detail, describing edge cases. They must also have a how-to if the feature's usage is not obvious. Fundamental new features may affect the tutorial as well, although generally new features are too advanced. Beta features cannot be toggled off but have automigration functionality for old code that is enabled by specifying the language version. Automigration is distinct from a toggle because it is a source-to-source rewrite of the code. Beta features may still have significant bugs, such as the inability to migrate old code correctly, but these bugs should generate readable error messages mentioning the feature name rather than crashing the compiler or silently failing.

Stable features are frozen features - further changes will be done as new features. They are considered to have reached a level of stability sufficient for long-term use. There is no visible difference in the implementation code between beta features and stable features and the distinction is mainly for marketing purposes.

The list of features is centralized in the code to `this specific file <https://github.com/Mathnerd314/stroscot/blob/master/src/features.txt>`__, to make finding them easier and to standardize handling. The scope of a feature may be identified by grep'ing the code for its identifier.

Moving a feature from alpha to beta should have a PR with documentation links and test case links. The PR should:

* change the feature list to set the feature's status to beta released on the current date. This enables old code warnings, automigration, and compiler bootstrap workarounds.
* implement automigration code if not already present
* remove all uses of the feature toggle in the code by modifying to the case where the feature is present (avoiding toggle debt).
