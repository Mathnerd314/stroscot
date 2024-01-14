Other programming languages
===========================

There are many existing programming languages to learn from. All of them have had effort put into their design so their features should be considered. Unfortunately there is not enough time to learn every language in depth and use it for 10 years to get an idea of its strengths and weaknesses, so we must rely on reports of other users on the web. Also even searching on the web for detailed reports of complexities encountered in large software projects is quite time-consuming, so we mainly examine popular languages.

Since we aim to be a popular language, we list the languages in order of popularity, so that more attention is focused on the earlier languages. There are several indexes with different measures of popularity (as of January 2023):

* `TIOBE <https://www.tiobe.com/tiobe-index/>`__ measures the quantity of search engine hits for "X programming"
* `PYPL <http://pypl.github.io/PYPL.html>`__ measures how often language tutorials are Googled.
* `Github Collection <https://github.com/collections/programming-languages>`__ measures stars and forks of implementation
* `GitHut 2.0 <https://madnight.github.io/githut/>` measures GitHub PRs, stars, forks, issues of language-detected repos
* `Languish <https://tjpalmer.github.io/languish/>`__ measures Github and Stackoverflow metrics
* `Github Octoverse <https://octoverse.github.com/2022/top-programming-languages>`__ measures amount of Github code written
* `RedMonk <https://redmonk.com/sogrady/2022/10/20/language-rankings-6-22/>`__ measures Github repositories and StackOverflow questions
* StackOverflow survey measures
* `IEEE Spectrum <https://spectrum.ieee.org/top-programming-languages-2022>`__ ranks 57 languages by 9 different measurements
* Google Trends

Out of these the PYPL index is probably most suited for designing a new language, because the tutorial measurement approximates the new PLs with features that programmers want. But it only has 28 languages - the TIOBE index is more complete with 50 languages ranked and 50 more languages listed. We also want to check that the top 20 PLs from the other rankings are also included.

PyPL index (top 28)
-------------------

1. Python

* Most popular on TIOBE index, said to be "easy to learn for beginners", "easy to write", "simple and elegant syntax" "similar to English".
* brevity, readability, developer-friendliness make it 5-10x more productive than Java
* "Batteries included" standard libraries, such as lists and dictionaries, numpy (BLAS wrapper) and scipy
* Twisted web framework, TensorFlow machine learning framework
* Mixed reference counting / tracing GC memory management
* Significant indentation - still a point of contention, e.g. whether it makes copy pasting code harder
* C++ interpreter CPython, slow performance. PyPy exists but has't been widely adopted due to incompatibilities.
* unintuitive "double underscore"/"dunder" method names like ``__getitem__`` and ``__str__``, C++'s ``operator[]`` and Java's ``toString`` are clearer
* paradigm: impure functional

2. Java

* Baroque type system, many types of class-like thing (interfaces, enumerations, anonymous adapters), with generics on top. Many compromises/holes such as covariant arrays
* Verbose. But there is a book on design patterns, which can be used to identify areas needing explicit syntax. The class-based syntax for the patterns is not worth emulating.
* try-finally and checked exceptions have wasted the time of many programmers.
* Keyword soup for declarations, such as "public static void main".
* Lack of operator overloading such as ``+`` for ``BigInteger``
* Every object has a 4-byte header and identity using ``==``. No value types besides primitives.
* Requirement that the class name must match the directory name.  When moving functionality around this implies a lot of changes inside source files. Led to IDEs with extensive support for refactoring.
* Static methods. Scoped to a class, but not related to objects. Can be very confusing.
* fulfils "compile once, run anywhere" promise at the expense of a huge runtime
* JIT is probably best in the world for throughput. Startup is slow but throughput matches C performance in many cases.
* Garbage collector takes big chunks of CPU time at irregular intervals. Low-pause GCs trade this for continuous overhead. Still not solved, around 15% overhead on wall clock time. :cite:`caiDistillingRealCost2022`
* paradigm: OO

3. JavaScript

* second-best JIT after Java, optimized for startup time - fast bytecode interpreters
* many strange features such as implicit type conversion, ``with`` statement, and ``eval``
* paradigm: impure functional

4. C#

* best designed C-style syntax - e.g. introduced async/await
* wide usage - desktop software (Windows), games (MonoGame, Unity), web development (ASP.NET Core), mobile (Xamarin)
* paradigm: OO

5. C

* most portable/widespread language. runs on just about every piece of silicon (although some require specialized compilers)
* language of most OS's, hence used for FFI stuff
* statically compiled, compilers are very efficient.
* difficult to work with -  unsafe pointers, common to see memory corruption and security vulnerabilities. valgrind, smart fuzzing, and static analysis have allowed catching these with great difficulty. Also there is the Boehm GC, used by many people who don't want to deal with memory management.
* header files slow down compilation as they have to be read many times during compilation
* paradigm: imperative

5. C++

* many features, which interact in messy/complex ways making C++ take a long time to learn
* fast, efficient standard libraries similar to hand-tuned code (but missing many features, see also Boost)
* templates, efficient at runtime but slow at compile time
* memory unsafe like C, although smart pointers and RAII make this a little better.
* Hard to debug, there is GDB, valgrind but really rr is the only way to track some errors down
* paradigm: imperative

6. PHP

* Initial design was hacked together quickly, inconsistent API design. Could be fixed but backwards compatibility was held to be more important.
* Like JS, several features with huge security or performance impact: eval, weak typing
* paradigm: imperative

7. R

* numerous libraries for statistics and data analysis
* lazy evaluation
* paradigm: functional

8. TypeScript

* `near superset <https://stackoverflow.com/questions/29918324/is-typescript-really-a-superset-of-javascript>`__ of JavaScript with an unsound type system
* doesn't really add anything besides the types, so only useful for ideas on gradual typing. Also the type inference is not too good.
* paradigm: OO

9. Swift

* Automatic reference counting, interesting but not something I want to copy
* syntax for exception handling, if let/guard let
* `exponentially slow <https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html>`__ type inference for numeric expressions, with bad heuristics
* paradigm: OO

10. Objective C

* deprecated by Apple in favor of Swift, but good to compare against C++
* paradigm: OO

11. Go

* opinionated design, touts meaningless features such as "strong typing"
* goroutines, killer feature - but stackless continuations are better
* finally added generics after a long time
* supposedly a Python replacement, but TensorFlow is mainly in Python and the Go binding `isn't officially supported <https://github.com/tensorflow/build/tree/master/golang_install_guide>`__
* paradigm: actor model

12. Rust

* good standard library design and documentation, probably worth copying
* voted "most loved" by StackOverflow
* ownership model/borrow checker has been found difficult to use by several studies (`1 <https://arxiv.org/pdf/1901.01001.pdf>`__, `2 <https://arxiv.org/pdf/2011.06171.pdf>`__, `https://dl.acm.org/doi/pdf/10.1145/3510003.3510107`__). Also it is incomplete - can't even write linked lists without `endless pain <https://rcoh.me/posts/rust-linked-list-basically-impossible/>`__. In practice Rust programmers `end up <https://rust-unofficial.github.io/too-many-lists/third-layout.html>`__  using reference counting or GC to ensure memory safety in complex cases
* concurrency safe, but async suffers from "borrow checker"-itis and uses atomic reference counting
* learning experience circa 2015 was "rough"
* compatibility pendulum has swung towards "too stable" - many changes that "should" be made for a better language that can't be
* paradigm: imperative

13. Kotlin

* JVM language with features tastefully copied from Groovy and Scala
* val keyword instead of final, null safety, extension methods, first-class type parameters
* coroutines
* mainly getting traction due to Google pushing it for Android
* paradigm: OO

14. MATLAB

* extensive numerical libraries
* array syntax confuses people, ``[1 [2 3]]`` is a flat array because ``[A B]`` means concatenate A&B. there is `no literal syntax <https://www.mathworks.com/help/matlab/math/multidimensional-arrays.html>`__ for 3D or higher dimension arrays.
* paradigm: imperative

15. Ruby

* weird syntax, e.g. expression by itself is return value - causes mistakes. Per Matsumoto `interview <https://www.artima.com/articles/the-philosophy-of-ruby>`__, Ruby was designed for *his* least surprise, and maybe for least surprise after memorizing the language, not for novice programmers or programmers familiar with other languages, so has many idiosyncrasies.
* complex library, e.g. both find_all and select methods that do the exact same thing
* Rails is `(still) <https://www.jetbrains.com/lp/devecosystem-2021/ruby/#Ruby_what-web-development-tools-and-or-frameworks-do-you-regularly-use-if-any>`__ the most popular framework. Requires reading the Rails guide to learn things like models having singular class names with capitals and no underscores but db tables with plurals, lower case and underscores. Or how in controllers you just reference params without anything suggesting if params is a variable, method, how its populated, where its scoped, etc. As compared to Django where novices can figure out the basics easily without needing a guide.
* slow, `YJIT <https://github.com/ruby/ruby/blob/master/doc/yjit/yjit.md>`__ added in 3.1
* paradigm: OO

16. (also 21) VBA / Visual Basic

* "mentally mutilates" programmers (according to Dijkstra)
* runs on .NET, so very similar to C# in semantics. There is also "Classic Visual Basic" but the differences are small.
* paradigm: imperative

17. Ada

* Still in use in aviation and DoD applications
* Considered somewhat legacy, but has many useful features ("C++98 with a design review")
* SPARK language is a dialect which extends contract support
* interesting design process (`Wikipedia <https://en.wikipedia.org/wiki/Ada_(programming_language)#History>`__):  committee gathered requirements and revised them several times (resulting in the Steelman report). 4 contractors put forward proposals - after two rounds, one was selected. The reference manual was written, and comments and corrections were received. The specification was then frozen and implementations were designed and validated. So far I have gone through

  * `Steelman report <https://www.adahome.com/History/Steelman/steelman.htm>`__
  * `Tinman report <http://iment.com/maida/computer/requirements/tinman.htm>`__
  * GREEN rationale
  * :cite:`fisherCommonProgrammingLanguage1976`

  Eventually I would also like to go through:

  * Full set of earlier requirements such as Strawman, Woodenman, Ironman, and Revised Ironman, linked `here <https://dwheeler.com/steelman/>`__ under "History"
  * Stoneman support requirements
  * RED rationale
  * 83, 95, 05, 2012 rationales
  * 83/95 style guides
  * Dijkstra's comments on the requirements and designs
  * Annotated 2012 reference manual

* paradigm: imperative

18. Dart

* targets JS, WASM (in progress), and native ARM/x86 with AOT and JIT, a pretty reasonable set of targets
* tied to Flutter UI framework, which is mostly for creating mobile apps but also supports desktop and web
* main advantage is sharing code between client and server
* sentiment seems to be that Kotlin is about the same language-wise and the JVM is better for enterprise work
* they have a package manager, but it doesn't support automatic vendoring so there are many version solving conflicts
* concurrency model is an async-await event loop similar to node plus actor-style "isolates"
* Google has been funding it, FUD about whether Google will kill it. It is an Ecma `standard <https://dart.dev/guides/language/spec>`__ though, probably will stick around for a decade regardless.
* paradigm: OO

19. Scala

* Type inference, allows avoiding repetition of Java such as ``SomeModule.MyClass v = new SomeModule.MyClass();``
* complex type system: implicit conversions, subtyping.
* complex syntax: scares off newbies, steep learning curve, not recommended. Scala 3 has 3 ways to end blocks (end, braces, indentation) and everyone is confused as to which one they should use.
* paradigm: impure functional

20. Lua

* Use of "tables" for everything is interesting
* LuaJIT was fast but the main developer left due to lack of income. Stroscot needs to avoid the same fate.
* paradigm: impure functional


22. ABAP (Advanced Business Application Programming)

* proprietary PL developed by SAP in 1983, only available as part of NetWeaver ERP suite.
* "German COBOL", popular in Eastern Europe / Germany.
* odd niche language, but with even bigger footprint than COBOL, gradually being phased out with Java/JS/etc.
* weird combination of BASIC and SQL. Built-in SQL syntax.
* all code is stored in databases, thousands of tables
* OOP extensions that make everything terrible to maintain
* good debugger
* paradigm: procedural/imperative

23. Julia

* good support for concurrency/parallelism
* C+Fortran+Python FFIs and syntax
* JIT design goes through LLVM and requires trampolines between functions, performance barrier
* paradigm: multiple dispatch

24. Groovy

* most "batteries included" JVM language... even has YAML support in the standard library.
* "kitchen sink" approach to language design, e.g. can declare variable bare, with var, or typed
* scripting language feel, more dynamic than Kotlin, also older
* good at DSLs, e.g. SQL DSL, used as Grails/Gradle configuration language
* starting to decline in popularity, even though it's still active
* paradigm: OO

25. Haskell

* "finest imperative programming language"
* small community, few core/maintenance developers (mainly SPJ) compared to size of codebase
* good in benchmarks and scripting but GC is still not usable in production
* poor library design, e.g. verbose naming conventions
* paradigm: pure functional

26. Perl

* A mess with the Raku split
* Various libraries on CPAN are good
* Contexts and sigils, terrible syntax for beginners
* paradigm: impure functional

27. COBOL

* most hated programming language per StackOverflow survey
* C2 wiki list: `why we hate COBOL <https://wiki.c2.com/?WhyWeHateCobol>`__
* paradigm: imperative

28. Delphi / Object Pascal

* still kicking, but proprietary
* paradigm: OO

TIOBE Next 22
-------------

8. SQL

* DSL used for databases, common enough that finding a good way to embed it is necessary
* query compilers do a lot of interesting data structure traversal optimizations
* PL/SQL is Oracle's extension which allows procedures, functions, and triggers; other databases have similar things
* paradigm: declarative

9. Assembly language

* Generally the last stage before a binary in any compilation pipeline
* full access to ISA and thus machine resources
* Intel and AT&T syntaxes for x86. `Several <https://blog.reverberate.org/2009/07/giving-up-on-at-style-assembler-syntax.html>`__ `posts <https://outerproduct.net/2021-02-13_att-asm.html>`__ say to use Intel.
* repetitive and tedious, too many side effects, not at all intuitive, full of many inconsistencies
* paradigm: imperative

18. Scratch

* Block-black visual programming language for children
* Essentially procedural, it has conditionals, loops, and functions
* paradigm: block-based

21. SAS

* Another statistics language, less popular than R and proprietary
* paradigm: data-driven

22. (Visual) FoxPro

* commercial language, don't know much about it
* paradigm: imperative

24. Fortran

* call-by-reference calling convention, avoids copying arrays but hard to program with
* still used for some numerical code
* handles floating point exceptions via signals
* paradigm: imperative

25. Lisp

* Easily parsable syntax, originator of macros
* Error messages involving macros are probably more confusing than the macros themselves
* Racket is probably the most popular Lisp now. Uses Chez Scheme's work on the nanopass framework.
* paradigm: functional

33. Prolog

* old language, dead/resting
* Few industrial-sized (>100k LOC) applications. Tends to be used as a DSL, e.g. IBM Watson uses it for pattern matching over natural language parse trees, but the rest of the application is written in C++ and Java.
* Blamed for contributing to the failure of Japan's `Fifth Generation Computer Systems <https://en.wikipedia.org/wiki/Fifth_Generation_Computer_Systems>`__ (FGCS) effort.
* great for querying relational databases, actually sort of a superset of SQL. You just load the relations and go.
* not well standardized - many implementations with different module systems/standard libraries
* What is standardized/hardcoded is the DFS search order, which for naive programs is often slow. Simply reordering the goals or clauses can give significant speedups. miniKanren uses a "fair" search strategy that avoids non-termination due to clause order, and it should be possible for a compiler to optimize the search even better with heuristics and profiling. Datalog restricts some features and has more efficient strategies.
* Prolog does not really have a way to examine or manipulate mid-level IR, e.g. something comparable to SQL EXPLAIN.
* Implementing speedups generally requires non-declarative workarounds that break the logical semantics. And e.g. cut is really hard to reason about. Mercury has "modes", not well-explored.
* Constraints allow avoiding cut in some cases, very powerful but not well explained in beginner courses
* paradigm: logic. generally agreed to be the highest-level paradigm.

34. D

* C/C++ style but different.
* Never really took off AFAICT - rift caused by the differences between D version 1 and 2, a general lack of learning resources and packages
* garbage collected
* many features that have been incorporated into C++, others that haven't like scope guards
* paradigm: imperative

35. PL/SQL is a dialect of of SQL

36. Bash shell

* Common on Unix systems, but I think Python is more useful when you're doing anything complex
* paradigm: imperative

37. Powershell

* Equivalent of Bash on Windows - I don't think there's much inspiring
* paradigm: imperative

39. Logo

* There is no official Logo implementation, but UCBLogo is popular.
* Simple procedural commands, functions, and Lisp-like linked lists
* Turtle graphics
* Personally I like the game `RoboWar <https://en.wikipedia.org/wiki/RoboWar>`__ better, which is based on Forth
* paradigm: block-based

40. Transact-SQL is a dialect of SQL

43. F#

* Mixture of C# and Haskell, not really as interesting as either
* paradigm: impure functional

44. OCaml

* paradigm: impure functional

45. CFML (ColdFusion Markup Language)

* paradigm: imperative

46. Scheme - discussed under Lisp

47. Erlang

* has a well-tested distributed, fault-tolerant, reliable, soft real-time, concurrent database
* designed to be crash-only, restart tolerant
* not used much outside Ericsson
* Per roastedby.ai, written by drunk masochistic Swedes. Just watch `Erlang: the movie <https://www.youtube.com/watch?v=xrIjfIjssLE>`__ and tell me they aren't drunk.
* paradigm: actor model

48. LabVIEW

* Pretty nice GUI approach to programming
* Lots of wires, and if you get a complicated enough program the auto wire layout command gives up
* paradigm: dataflow

50. ML

* paradigm: impure functional

Next 50 in alphabetical order
-----------------------------

ABC

ActionScript

* basically JS but with a different standard library

Alice

Apex

APL

AutoLISP

Awk

B4X

C shell

CL (OS/400)

CLIPS

Clojure

* one of few languages to use software transactional memory, custom implementation "MVCC"
* `interesting talks <https://github.com/matthiasn/talk-transcripts/tree/master/Hickey_Rich>`__ on functional programming and language design
* runs well on JVM
* slow
* never seen it used for anything performance-critical or that substantially affects a business

Common Lisp - discussed under Lisp

Crystal

* Ruby-like syntax - attracts Ruby programmers, but too complex for anyone else
* statically compiled with LLVM - the usual story of fast binaries but slow compile times
* HM-style type inference, the usual caveats

Elixir

* based on Erlang, new and supposedly great syntax

Emacs Lisp - discussed under Lisp

Forth

* Minimal design, almost as portable as C
* stack-based paradigm leads to annoying shuffling (dup, swap, etc.) to get anything done
* extended by Factor, Joy, and other "concatenative languages"

Hack

Icon

Io

J#

JScript

Korn shell

Ladder Logic

LPC

Modula-2

MQL5

MUMPS

NATURAL

Occam

OpenCL

OpenEdge ABL

PL/I

* Ye olde IBM language
* Weird exception handling style - "ON <EXCEPTION> GOTO <LABEL>", very similar to hardware implementation

Q

Racket - discussed under Lisp

Raku

* Perl 6, yay

Ring

RPG

S

Smalltalk

Solidity

SPARK - discussed under Ada

Stata

Tcl

VBScript

Verilog

* Two languages: a general purpose specification language, and a poorly-defined hardware synthesis language. Basically you have to run the synthesizer and see if it complains or generates a circuit.

VHDL

* also a hardware synthesis language

WebAssembly

X++

Xojo

Languages not in TIOBE
----------------------

Austral

* Has a `spec <https://austral-lang.org/spec/spec.html>`__ with rationales for some of its design decisions

Elm

* small ecosystem
* derivative of OCaml
* no substantial commits in main repo since 2019
* BDFL doing "exploratory work" closed-repo, most recently described in a 2021 `status update <https://discourse.elm-lang.org/t/status-update-3-nov-2021/7870>`__

Flix

* Long list of principles, most of which I disagree with. The corresponding Stroscot opinions are expressed on their respective pages.

Oz

* multi-paradigm, I don't like the way they integrated logic programming though

-  `Lever <https://github.com/cheery/lever/>`__
-  `Jai <https://github.com/BSVino/JaiPrimer/blob/4a2d14f3e1c8e82a4ba68b81d3fd7d8d438e955c/JaiPrimer.md>`__
-  `Pinafore <https://pinafore.info/>`__
-  `Macro Lambda Calculus <http://github.com/codedot/lambda>`__
-  `Wat <https://github.com/manuel/wat-js>`__
-  `Atomo <https://github.com/vito/atomo>`__ / `Atomy <https://github.com/vito/atomy>`__

REXX

* old, somewhat interesting
* `blog post <https://smartbear.com/blog/7-reasons-that-rexx-still-matters/>`__: peaked around 1995, major scripting language before Python came along
