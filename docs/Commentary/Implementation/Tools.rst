Tools
#####

13G. The language should be designed to work in conjunction with a variety of useful software tools and application support packages. These will be developed as early as possible and will include editors, interpreters, diagnostic aids, program analyzers, documentation aids, testing aids, software maintenance tools, optimizers, and application libraries. There will be a consistent user interface for these tools. Where practical software tools and aids will be written in the language. Support for the design, implementation, distribution, and maintenance of translators, software tools and aids, and application libraries will be provided independently of the individual projects that use them.

Nowadays every language comes with a tool suite, because a compiler alone is not enough. As Walter Bright points out, if it's a separate download, what happens is people never use it. A third-party tool is never installed, or it's the wrong version. Users do not want to read the manual for a third party tool and will never consider using it. But when it's built into the language, they magically start using it. And there's synergies, like you can integrate the documentation with the style checker and stuff like that. The tools can all be integrated with the compiler and use the same parser, ABI, etc. so that they are always compatible.

Bright says this is transformative - it's like a night and day difference. And the other advantage is, there's only one. If you leave it to third parties, you get an endless number of tools, all different. Two teams each pick a different documentation generator and now they can't be merged or they can't work together and then you'll have a big fight and what a waste of time. The built-in tool may not be the best, but it's always there. By building it in you sort of browbeat people into using it.

* Software design tools
* Interface design tools
* Database design tools
* Reusability-support tools
* Reverse-engineering tools
* Reengineering tools
* Code restructuring tools
* Change management tools
* Optimization tools
* Code editors
* Static analysis tools
* Standards-checking tools
* Complexity-analysis tools
* Bounds checkers
* Execution simulation tools
* Record/playback tools
* Debugging tools
* Defect-repairing tools
* Defect-tracking tools
* Troublesome code identification
* Code removal tools (dead or otherwise)
* Inspection-support tools
* Testing-support tools
* Groupware/communication/customer support tools
* Viral protection tools
* Migration or conversion tools
* Translation /internationalization tools
* Mass update tools
* Refactoring tools
* Dependency update tools

Style checker
=============

This is really just compiler warnings - keep adding warnings until no program is safe. But commercial style checking tools have this cool search and filter interface. It would also be cool to prioritize warnings based on machine learning.

Testing harness
===============

I don't just have unit testing and integration testing, I have model checking and formal verification. But there needs to be some interface for specifying how the model checker is run, like turning off tests and viewing summaries of results.

Documentation generator
=======================

The documentation generator provides a nice way to browse through a large codebase, ensuring that code is easy-to-read and searchable. The documentation comments, type annotations, and argument names are pulled out for each function, and the implementation code is accessible though an expando. The code has hyperlinks for all terms to the place where they are defined, or opens a menu if the term is overloaded. Code is prettified to use Unicode or MathML formulas where appropriate. There's regex-based search, and special searches for identifiers. Also useful is the call graph, in particular showing what functions call a given function. This can just be a link.

As far as targeted output formats, only HTML seems particularly important.

The notion of "well-documented" is hard to define. For one person the source code and function names may be sufficient, while for another a tutorial that has no relation to the codebase may be more useful.

If we have good IDE integration that exposes doc comments then maybe a documentation generator is not too important.

Refactorer/reformatter
======================

The refactoring tool reads a program from source file(s), rewrites the code according to specified rules, and writes the program back to the file(s). It automates easy, repetitive, tedious changes. When the need for a rewrite can be detected but it cannot be done automatically, the rule can insert ``TODO: check rule XXX`` markers. Refactoring provides a way to rename or inline functions, eliminate dead code, and transform old idioms to new idioms. Integrating refactoring with the language allows automated migration of code from old to new versions of libraries, as in gofix. Unlike gofix, the refactoring tool should be more like a library than a program, because writing gofix rules is just too hard. Gofix mainly implements a template style - find this AST pattern, replace with this template - but its pattern language is not very powerful.

With no rewrite rules, the refactoring tool simply reads the code and writes it back out, functioning as a reformatter. Regarding style configurability, there is a continuum. `gofmt <https://go.dev/blog/gofmt>`__ is probably the most extreme - it has 0 style-related configuration options. Slightly higher in the continuum is Python's Black, an "opinionated" and "uncompromising" formatter which has a "purposefully limited" selection of options - really just line length and "magic trailing commas" that avoid collapsing lists onto one line. At the other end are tools like clang-format and GNU indent which have 154 and 82 options respectively. The gofmt page claims that its output is "uncontroversial" but there actually have been many discussions on `its lack of configurability <https://github.com/golang/go/issues/40028>`__, `tabs vs spaces <https://www.reddit.com/r/golang/comments/ee4dqn/how_to_make_gofmt_or_go_fmt_use_whitespaces/>`__, and lack of detailed prettification like `removing whitespace at the head of functions <https://www.reddit.com/r/golang/comments/rrce2e/is_there_a_better_alternative_to_gofmt/hqflh4i/>`__ and `aligning assignments <https://www.reddit.com/r/golang/comments/rt0hra/can_i_tell_gofmt_to_make_some_exceptions_when/>`__. It is clear that, contrary to Ian Taylor's assertion, gofmt is not adequate for everybody. To me, the statement by Ian Taylor "It is an intentional design choice" sounds suspiciously like "I can't be arsed to implement that", a sort of concealed indifference as to what formatting is actually aesthetically pleasing. In contrast, with clang-format's ever-increasing set of options, `a single space <https://github.com/llvm/llvm-project/issues/59729>`__ is a significant issue and actually gets addressed. The configurability of clang-format ensures an attention to detail and resulting improvement in formatting quality that the mono-style of gofmt does not. So I think, similarly to Stroscot at large, functionality must be prioritized above minimalism: the reformatter must be able to generate all code styles, or at least the ones that people care enough to implement. "We are too lazy to maintain this" is not an acceptable reason to reject a style, when it could be necessary for someone's screen reader or to address other readability or accessibility concerns.

Ian does have a point about eliminating bike shed discussions, but that is IMO separate from the implementation of functionality. One can have a default "standard" or "core" coding style, just like having the default standard library and core API supported by the compiler. Standardization has the same benefits of raising the bikeshedding bar: if your format is really so much better, you should be able to convince the standards committee to switch it in the language-wide config file. Letting the language handle it rather than just the people at your organization is a step up in centralization and really has no downsides. But just because one style is standard doesn't mean that other styles shouldn't be accessible - it should be easy to override the language default style if an individual has other preferences. But, if an individual does want to deviate, it should be encouraged to run the reformatter as a checkout/checkin filter, so that repositories store code formatted in the standard style and code on the web still uses the standard format.

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

IDE support
===========

A GUI interface is higher-bandwidth than the REPL. Typically an IDE allows editing text files but more recently there is the notebook interface which integrates the REPL format with multiline code and allows outputting images, graphs, and interactive components (which would require separate windows when running a text file and command line).

Gilad Bracha says existing workspaces are missing many important features. He recommends writing a new workspace environment from scratch - he thinks 5 full-time people for 2-3 years could do it. But of course he's a bit biased since he led a team to do such a thing that got their funding pulled 1 year before completion. I think his experience and the deaths of other projects such as e.g. Light Table suggests that writing a new workspace environment is risky. You need a good cross-platform GUI library to even think about it - there is the Dart library, but maybe our port of that will suck. We could take a shortcut by using HTML and a browser engine, but IMO the massive web rendering stack introduces just a bit too much latency. Also, it seems impossible to implement the "tablike spaces" idea I had for an IDE with proportional fonts - I don't think table layout in HTML is performant enough to do one character per cell. (TODO: actually benchmark this with a hardcoded prototype) It's kind of a chicken-egg problem - you need an IDE to write code, but writing a new IDE requires writing code.

According to the `StackOverflow 2022 survey <https://survey.stackoverflow.co/2022/#section-most-popular-technologies-integrated-development-environment>`__, VSCode was the most popular editor and 2x as popular as either of the next two, Visual Studio and IntelliJ. I think VSCode is a sufficient environment for hosting a new language, while the existing tools for writing IDEs are insufficient, so implementing the language comes first. So, at least until the language is stable, I only aim to support VSCode / LSP and nothing else.

Language server
---------------

https://petevilter.me/post/datalog-typechecking/

https://www.youtube.com/watch?v=WxyyJyB_Ssc


VSCode comes with an extensive protocol for language integration, LSP. Other IDEs do support LSP to some extent, but generally not fully and you have to install plugins. There's a `specification <https://microsoft.github.io/language-server-protocol/specification>`__. As an overview, an LSP server can provide:

* syntax highlighting (the docs say you have to use TextMate grammars too, but from some examples it looks like LSP alone can be performant enough)
* tooltips
* autocomplete suggestions
* navigation outline / object browser
* debugger integration
* "navigate to definition" command
* find all references
* quick information / signature help
* compiler errors/warnings/fixes
* rename symbol
* refactoring actions - extract method
* edit and continue
* execute code snippets (REPL)

Per `this post <https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html>`__ a language server serves two goals: quickly processing new edits to source files, while also answering queries quickly. There are several potential designs:

* Map Reduce - split analysis into a relatively simple indexing phase (per file, in parallel), and a separate full analysis phase (global). Example: Java, the indexer.
* Precompiled Headers - declaration before use, all declaration in headers or equivalent interface files, snapshot the compiler's state immediately after imports for each compilation unit.
* Query-based - all function calls inside the compiler are instrumented to record which other functions were called during their execution. The recorded traces are used to implement fine-grained incrementality. If after modification the results of all of the dependencies are the same, the old result is reused. If a function is re-executed due to a change in dependency, the new result is compared with the old one. If despite a different input they are the same, the propagation of invalidation stops.

As Stroscot is dynamic, only a query-based approach is sufficiently general to work. The main drawback is extra complexity and slower performance (fine-grained tracking of dependencies takes time and memory). The performance can be alleviated by fine-tuning cache invalidation details and omitting some items from the cache, while the complexity is here to stay.

Notebooks
---------

Ideally, IMO, notebooks would be incremental. Running (shift-enter) would act as if it reran the notebook from the start up to the selected cell. For speed the computation would be cached incrementally, so long-running computations would be skipped if possible. This model allows putting interactive sliders in and quickly updating graphs. Also, like Smalltalk the workspaces should have memory and persist across close-open. Everything in the notebook state should be serialized, down to the cursor position. Ideally this should be a text-based format, JSON or maybe even a subset of Stroscot. Also it should be possible to export a notebook to a text file, once you've decided it's in a good state and don't need the interactivity anymore.

The modern workspace environment that's most popular is the Jupyter notebook interface. But jupyter's kernel `protocol <https://jupyter-client.readthedocs.io/en/latest/messaging.html>`__ is just a dumb "execute this string of code" REPL, no information on what cell it's from. So we would have to hack jupyter to get this to work. OTOH the LSP protocol does support incremental update and it looks like you can use this incremental update protocol with notebooks. So another win for supporting VSCode exclusively.

Debugger
========

The debugger is design for a specific debugging workflow and supports that workflow by providing necessary information and context at each step. Debugging procedure:

* Collect error messages, logs, and any available context, such as a stack trace.
* Identify sufficient and necessary conditions that trigger the bug, so that it can be reproduced in a controlled manner.
* Use error message documentation, code minimization, and bisection techniques to find the specific lines of code causing the issue. Debugging tools can help with navigation.
* Review the relevant code. Look for syntax errors, typos, or missing semicolons. Reading the code backwards or writing comments above every line can help. If your code relies on external libraries or APIs, review the documentation and usage to ensure you're using them correctly. Static analysis tools and linters may also catch potential coding errors, style violations, and other issues, pointing out patterns that are difficult to spot by eye.
* If no mistakes jumped out, trace the flow of data and logic through the code. Check if variables are being modified as expected and if conditional statements are behaving correctly.
* If you still haven't found the bug, collaborate with a colleague to review the code together. A fresh pair of eyes can often spot issues that you might have missed.
* At this point the bug may be unsolvable, but try taking a break and stepping away from the code. Returning with a fresh perspective can help you see the issue differently.
* Once you have found (or not found) the bug, document the bug, your findings, and the steps you've taken to troubleshoot it. This documentation can be helpful for future reference. Implement the necessary changes or workarounds and thoroughly test and add test cases to ensure the bug is resolved without introducing new problems.

The debugger's view of the program's state is as a large expression or term. This state evolves in steps, where each step applies a rule to a redex or calls into the OS to perform a primitive operation.

One debugging technique useful in combination with reversible debugging is to use a step counter that starts at 0 at the beginning of the program and increments every time a reduction step is performed. The exact step that triggers a behavior can be determined by binary search. Similarly when we are debugging a phase of the compiler, we can use "fuel" - this specifies how many transformations can be performed during the phase of interest before moving on to the next phase.

Let's assume we have symbols, then there are lots of operations available from a debugger:

* recording: record the whole program execution. Reversible debugging allows running a program backwards. Omniscient debugging allows queries over the entire execution of the program, as though all states were stored in a database and indexed. Recording can be implemented by instruction-level recording, but more efficient is to record only non-deterministic events, with occasional whole-program snapshots to allow seeking. Supporting concurrent execution requires recording inter-thread sequencing.
* breakpoints: set/clear/list. essentially a breakpoint is a true/false predicate on a transition. Can inspect state and the previous state - common conditions include at program line/column, transition calls syscall, transition enters function, expression not yet evaluated, transition invokes signal handler, variable has value in state, transition modifies variable, transition in certain thread.
* tracepoints: record message / data / statistics at a set of program transitions, like a breakpoint but without stopping
* queries: print backtrace / call stack, print state (threads, variables), dump memory, disassemble memory, blocked thread dependencies, pretty-printing, GUI visualizations
* stepping: single step, step out, continue thread / all threads until breakpoint, run ignoring breakpoints until stopped with interactive commnad
* REPL / patching: evaluate pure expression in context of state, evaluate arbitrary code in current state (e.g. set variable to value), replace definition, hot-reload code changes, jump to address, return early from function. Pedantically, the patched state has no real history so the debugger should only be able to run forward from the state, but we can graft the patched state onto the old state to avoid losing context.
* IPC: send signal, modify files


searching for watchpoints/breakpoints
build database of everything that happened
replaying different segments of the execution in parallel
applying different kinds of instrumentation to the same segments

instruction-accurate recording of your software - syscalls, shared memory, signal timing
this can be played forward and will always behave the same way
several approaches - Just-In-Time instrumentation of machine code (Undo.io), ptrace (rr, https://pernos.co), https://replay.io

to run backwards, we need more information - like if memory is overwritten, what was the value before? Unfortunately, if we recorded all memory changes explicitly, it would be slow and use up a lot of storage. Therefore debuggers use "Finnegan search" ("poor old Finnegan had to begin again...") - they start one or more forks of the process, and run these forks forward up until the desired target is reached. Usually there is a recent snapshot available so only that slice has to be re-executed during reverse stepping. The parallelism is mainly useful for larger breakpoints/watchpoints where the event of interest can be further back and multiple snapshots may have to be examined.


we only have snapshots and the minimal information needed to run forward deterministically. So for most tasks, like breakpoints, we need to recompute intermediate states, like memory contents.

JIT instrumentation
hardware performance counters - not available in containers/virtual machines


Debugging by querying a database of all program state by Kyle Huey
The State Of Debugging in 2022 by Robert O’Callahan
Debugging Backwards in Time (2003) by Bil Lewis
undo.io UDB
rr, WinDBG, Pernosco.
