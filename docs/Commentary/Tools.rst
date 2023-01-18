Tools
#####

Nowadays every language comes with a tool suite, because a compiler alone is not enough. These tools have to be integrated with the compiler and use the same parser, ABI, etc. so that they are compatible. This page discusses those.

Documentation generator
=======================

The documentation generator provides a nice way to browse through a large codebase, ensuring that code is easy-to-read and searchable. The documentation comments, type annotations, and argument names are pulled out for each function, and the implementation code is accessible though an expando. The code has hyperlinks for all terms to the place where they are defined, or opens a menu if the term is overloaded. Code is prettified to use Unicode or MathML formulas where appropriate. There's regex-based search, and special searches for identifiers. Also useful is the call graph, in particular showing what functions call a given function. This can just be a link.

As far as targets, only HTML seems particularly important.

The notion of "well-documented" is hard to define. For one person the source code and function names may be sufficient, while for another a tutorial that has no relation to the codebase may be more useful.

If we have good IDE integration that exposes doc comments then maybe a documentation generator is not too important.

Refactorer
==========

The refactoring tool makes it easy to analyze source code, enabling tooling such as automatic code formatting and codebase maintenance. It reads a program from source file(s), rewrites the code according to specified rules, and writes the program back to the file(s). It automates easy, repetitive, tedious changes. When the rewrite cannot be done automatically the rule can insert ``TODO: check rule XXX`` markers. It provides a way to rename or inline functions, eliminate dead code, and transform old idioms to new idioms. The automated migration of code from old to new versions uses the refactoring API.

With no rules, the refactoring tool functions as a reformatter. Python's Black started out as opinionated but eventually grew lots of options - probably the reformatter should be very flexible, but have a preset default that's used for the compiler.

Inspired by gofix / `gofmt <https://go.dev/blog/gofmt>`__ .

Language server
===============

For integration with VSCode and other IDEs. There's a `specification <https://microsoft.github.io/language-server-protocol/specification>`__. As an overview, we provide:

* syntax highlighting
* tooltips
* autocomplete suggestions
* navigation outline
* debugger integration
* navigate to definition
* compiler errors/warnings/fixes
* IDE-assisted renaming or refactoring

According to the `StackOverflow 2022 survey <https://survey.stackoverflow.co/2022/#section-most-popular-technologies-integrated-development-environment>`__, VSCode was the most popular editor and 2x as popular as either of the next two, Visual Studio and IntelliJ, so probably is the only one that needs to be supported.

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

Notebooks/workspaces are a UI interface, higher-bandwidth than the REPL or even text files. You can input multiline code (like text files), and output images, graphs, and interactive components (which would require separate windows when running a text file from the command line).

Ideally, IMO, notebooks would be incremental. Running (shift-enter) would act as if it reran the notebook from the start up to the selected cell. For speed the computation would be cached incrementally, so long-running computations would be skipped if possible. This model also allows putting interactive sliders in and quickly updating graphs. Also, like Smalltalk the workspaces should have memory and persist across close-open. Everything in the notebook state should be serialized, down to the cursor position. Ideally this should be a text-based format, JSON or maybe even a subset of Stroscot. Also it should be possible to export a notebook to a text file, once you've decided it's in a good state and don't need the interactivity anymore.

The modern workspace environment that's most popular is the Jupyter notebook interface. But jupyter's kernel `protocol <https://jupyter-client.readthedocs.io/en/latest/messaging.html>`__ is just a dumb "execute this string of code" REPL, no information on what cell it's from. So we would have to hack jupyter to get this to work. OTOH the LSP protocol does support incremental update and it looks like you can use this incremental update protocol with notebooks.

Gilad Bracha says Jupyter is missing many important features. He recommends writing a new workspace environment from scratch - he thinks 5 full-time people for 2-3 years could do it. But of course he's a bit biased since he led a team to do such a thing that got their funding pulled 1 year before completion. I think his experience and the deaths of other projects such as e.g. Light Table suggests that writing a new workspace environment is risky. You need a good cross-platform GUI library to even think about it. Gilad's new project and VSCode cheat by using HTML, but IMO the massive web rendering stack introduces just a bit too much latency. Also, it seems impossible to implement the "tablike spaces" idea I had for an IDE with proportional fonts - I don't think table layout in HTML is performant enough to do one character per cell. (TODO: actually benchmark this with a hardcoded prototype) It's kind of a chicken-egg problem - I think VSCode is a sufficient environment for hosting a new language, while the existing tools for writing IDEs are insufficient, so the language comes first.

So for now it seems like aiming to support notebooks via VSCode / LSP is the best choice.
