Checklist
#########

This page goes through the `programming language checklist <https://www.mcmillen.dev/language_checklist.html>`__ and discusses things not discussed elsewhere.

Paradigms
=========

The checklist has a few paradigms: functional, imperative, object-oriented, procedural, stack-based, "multi-paradigm". In linguistics, a paradigm is "a set of linguistic items that form mutually exclusive choices in particular syntactic roles," specifically "a table of all the inflected forms of a particular verb, noun, or adjective." This seems to be a usable definition of a PL paradigm - you have all related versions of a semantic entity.

Unfortunately people seem to use paradigms as labels of entire languages, rather than classifications of their features. Stroscot, like every other language, is "multi-paradigm" - even assembly is multi-paradigm since it is imperative (syscalls) and structured (conditional jump). So the adjectives "object-oriented", "functional", etc. are best avoided in favor of discussing whether a language has specific semantic constructs, since "functional object-oriented language" sounds weird. Still, it's good to have a map of which paradigms embed into which other paradigms. This list is based on Wikipedia's list of paradigms:

* Action: `action descriptions <https://en.wikipedia.org/wiki/Action_language>`__ are given by the state trajectory relation
* Array-oriented functions are still functions
* Automata-based:

  * Nondeterministic automata are given by the transition relation
  * Deterministic automata are given by the transition function

* concurrency operations decompose into OS calls which are I/O operations

  * agents/actors/flow-based processes are threads with a dispatch loop

* data-driven programming is a main loop over condition-action pairs
* declarative is a logical relation or a function

  * functional

    * lambas are anonymous functions
    * functions are total functional binary relations

  * logic

    * a relation is a set of tuples
    * boolean operations are logical constraints

  * constraint: constraints are 0-1 loss functions in an optimization problem
  * dataflow is a block in single static assignment form
  * a reactive or incremental program is a state value plus a state update function or command
  * a query is a function that takes a database and produces a list of results

* differentiable: the `derivative <https://en.wikipedia.org/wiki/Fr%C3%A9chet_derivative>`__ is a function mapping a function :math:`f` to a linear operator :math:`A` such that :math:`\lim _{\|h\|\to 0}{\frac {\|f(x+h)-f(x)-Ah\|}{\|h\|}}=0`.
* dynamic: eval is a function from strings to values (and optionally with an environment)

* event driven: an ED program is some event handler functions, data binding event handlers to events, and a main loop function (provided by a library) that repeatedly checks for events and calls the matching event handler
* generic functions are just functions
* imperative programing:

  * commands can be represented as a tag (payload) plus a callback function returning another command
  * mutable variables are using read and write functions on an implicitly passed/returned store.
  * procedures are functions from arguments to commands

* Metaprogramming:

  * Attribute-oriented: attributes are a function from symbols to metadata
  * Macros: macros are functions that take an AST and a lexical environment

* Nondeterministic: a nondeterministic function is a relation
* Parallel: a block in single static assignment form can be easily parallelized using a concurrent worker pool
* Process-oriented programs can be represented using concurrent operations
* probabilistic programs are functions from parameters to a log probability
* Quantum:

  * quantum logic gates are functions, in particular unitary operators on states of qubits
  * a quantum program is a block, consisting of gate applications and discarding information (Qunity)

* Set-theoretic: set membership is a boolean predicate function
* Stack-based: a stack-oriented program is a function on stacks, a.k.a. lists
* structured:

  * loops are recursive functions
  * conditionals are lazy functions
  * Block-structured: block sequencing is Kleisli arrow composition, a function
  * Object-oriented: objects are mutable variables containing records of mutable variables and functions
  * Class-based: classes are types
  * recursion is syntax for applying a fixpoint function

* Symbolic: an AST is a value
* Value-level: types are sets

In addition there are some other paradigms:

* term rewriting systems are given by the rewriting relation
* optimization problems are relations based on on objective functions
* optimization solvers are functions from objective functions to a list of solutions

Graph of paradigms
------------------

.. graphviz::

  digraph paradigms {
    action -> relation
    array -> function
    "nondet automata" -> relation
    "det automata" -> function
    concurrency -> command
    actor -> concurrency
    agent -> concurrency
    flow -> concurrency
    actor -> loop
    agent -> loop
    flow -> loop
    "data-driven" -> loop
    "data-driven" -> condition
    "data-driven" -> function
    "data-driven" -> command
    declarative -> relation
    declarative -> function
    lambda -> function
    function -> relation
    relation -> set
    boolean -> constraint
    constraint -> optimization
    dataflow -> block
    reactive -> function
    reactive -> command
    query -> function
    differentiable -> function
    dynamic -> function
    event -> function
    event -> loop
    generic -> function
    command -> function
    "mutable variable" -> function
    procedure -> function
    attribute -> function
    macro -> function
    nondeterministic -> relation
    parallel -> block
    parallel -> concurrency
    process -> concurrency
    probabilistic -> function
    quantum -> function
    quantum -> block
    set -> boolean
    stack -> function
    loop -> function
    loop -> recursion
    conditional -> function
    block -> function
    object -> "mutable variable"
    class -> type
    recursion -> function
    type -> set
    "term rewriting" -> relation
    optimization -> relation
    optimization -> function
  }

Graphviz has chosen "function" as the root paradigm. This agrees well with experience. Quoting `Spivak <https://www.google.com/books/edition/Calculus/7JKVu_9InRUC?hl=en&gbpv=1&bsq=central%20objects>`__, "the most important concept in all of mathematics is that of a function - in almost every branch of modern mathematics functions turn out to be the central objects of investigation." Looking closer, function is part of an SCC ``function, relation, set, boolean, constraint, optimization``. The mathematical notion of function is broad; a functional relation cannot be expressed naturally using lambdas, but lambdas naturally express functions. So we need constraint logic programming as well to get full expressiveness of our functions. Hence the ultimate paradigm is functional logic programming.

Principles
==========

Paradigms are vague and only express common patterns; they cannot be used to design a programming language. So instead we have principles.

* Immature poets imitate; mature poets steal; bad poets deface what they take, and good poets make it into something better, or at least something different. The good poet welds his theft into a whole of feeling which is unique, utterly different than that from which it is torn. (T. S. Eliot)
* Make the irreducible basic elements as simple and as few as possible without having to surrender the adequate representation of a single datum of experience. (Albert Einstein)
* Write a prototype implementation. Conduct an A* search through the possible solutions, stopping early if the potential solution is clearly worse than the prototype. Periodically take the best solution out of all discovered so far and implement it as the new prototype. (`Branch and bound <https://en.wikipedia.org/wiki/Branch_and_bound>`__)
* Never finished, never complete, but tracking the progress of technology (Lennart Poettering)
* Code can be used as data, data can be used as code (Lisp)
* Productivity is being able to do things that you were never able to do before. (attributed to Franz Kafka, maybe Jim Manzi)
* As size and complexity increase, architectural design dominates materials. Create durable, non-leaky, beautiful interfaces. (`VPRI <http://www.vpri.org/pdf/tr2011004_steps11.pdf>`__, `John Regehr <https://blog.regehr.org/archives/666>`__)
* If it isn't documented, it doesn't exist (Coding Horror) Corollary: There is no undefined behavior, only undocumented behavior.
* Take a list of items. Imagine a specific walk through a familiar place. List distinctive features of the route. Combine each feature with an item to form new outrageous/memorable images. (Memory palace)
* People prefer a middle level of complexity: too simple and we are bored, too complex and we are confused. Moreover, the ideal level of complexity is a moving target, because the more expert we become at any subject, the more complexity we prefer. (Donald Norman)
* Better depends on your goodness metric (`Jim Waldo <http://web.archive.org/web/20210325222034/https://www.artima.com/weblogs/viewpost.jsp?thread=24807>`__)
* The shorter the [edit-test] cycle, the happier the programmer. (`Yue Yao <https://tripack45.github.io/2018/11/03/edit-compile-run/>`__)
* Do all things without grumbling or complaining (Philippians 2:14)
* Secure by default: The default level of access should have the least privilege and the most number of checks. (OpenBSD)
* Organize functions by functionality into expressive components. (`Uli Weltersbach <https://reasoncodeexample.com/2016/03/06/a-place-for-everything-and-everything-in-its-place-thoughts-on-organizing-source-code-by-type/>`__)
* When two elements of an interface conflict, or are ambiguous, the behavior should be that which will least surprise the user; in particular a programmer should try to think of the behavior that will least surprise someone who uses the program, rather than that behavior that is natural from knowing the inner workings of the program. (`POLA <https://en.wikipedia.org/wiki/Principle_of_least_astonishment>`__)

What are these principles useful for? Mainly wasting time. Practical decisions are are made by judging pros and cons.

Non-principles
--------------

Readability
~~~~~~~~~~~

The edit-test cycle time is an included design principle, and part of the cycle time is the time needed to understand the program. In fact per `here <https://web.archive.org/web/20060213015737/http://blogs.msdn.com/peterhal/archive/2006/01/04/509302.aspx>`__, the split is 70% Understanding Code, 25% Modifying Existing Code, 5% Writing New Code. So in its literal definition "ease of understanding code", readability is an important factor to optimize for.

But in practical studies, the typical "comments, naming, and formatting" advice associated with readability is not an important factor in cycle time. For example in a Scala vs Java parallel programming test :cite:`pankratiusCombiningFunctionalImperative2012`, the Scala programs ended up taking a lot longer to write - 72 hours vs 43 hours, with no working Scala programs submitted until the third week, versus a Java program working the first week. The main reason given in the paper was Scala's complex type system, although programmers listed other complaints such as poor documentation, poor IDE/debugging support, complex memory model, and complex parallel programming abstractions.

Documentation and IDE support are important factors in understanding code, but they are not usually mentioned as part of readability. For example, nobody would say Brainfuck is a readable language, even though liberally commented Brainfuck program (e.g. `Wikipedia examples <https://en.wikipedia.org/wiki/Brainfuck#Examples>`__) can be quite readable - one reads the comments, rather than the code. Similarly extensive commentary or an accompanying academic paper can make otherwise unreadable code suddenly "click" and make sense. Readability is a function of the reader and the writer, and the programming language has only a marginal impact.

Besides robust commenting syntax, readability doesn't really place any hard constraints on a programming language. So readability isn't a design principle.

Terseness
~~~~~~~~~

APL is terse mainly due to its use of symbols, and :cite:`holmesAPLProgrammingLanguage1978` mentions that some consider terseness an advantage. But is it really? Again the prtinciple for Stroscot is the edit-test cycle time, in particular the 70% of time needed to understand a program. An APL program may be short but if the APL program requires looking up symbols in a vocabulary while a normal word-based program is more verbose but self-contained, then the word-based program wins on cycle time.

Iverson argues the human mind has a limit on how many symbols it can cope with at one time. A terser notation allows larger problems to be comprehended and worked with. But this ignores the role of chunking: a novice chess player works with individual pieces, while an expert player works with configurations of the entire board. Similarly a programming expert will work on the level of program fragments, for example CRUD or the design patterns of Java, and the amount of verbiage involved in writing such patterns is immaterial to mental manipulation but rather only becomes relevant in the time necessary to read unfamiliar codebases and comprehend their patterns and the time needed to write out such verbose patterns when moving to implementation. Rather than terseness, this consideration argues to make programming patterns distinctive and easy to remember (the "memory palace" principle). APL's overloading of monadic and dyadic function symbols seems to conflate distinct functions.

There is some advantage to terseness in that code can be published inline in books or blog posts without detracting from the flow of the text. Documentation works better when the commentary and the code are visible on the same medium. But readability of the code is more important - a barcode is terse but provides no help without a complicated scanning procedure. UX design provides many techniques for creating navigable code listings, e.g. a 1000-line listings to be discussed in a short note with a hyperlink. accordion folds can be used for 100-line listings, and 10-line listings can be in a two-column format or with a collapsed accordion fold. So this advantage of terseness seems minimal compared to the benefits of general UX.


APL programmers tend to give up reading an APL program quickly and start programming it themselves. This is due to the fact that one can program so quickly in APL.
Turtles all the way down
~~~~~~~~~~~~~~~~~~~~~~~~

"Turtles all the way down" only makes sense in the context of a directional layout, such as inheritance or composition. Stroscot has :ref:`no inheritance <no inheritance>`, so it must be composition. But an infinite object tree would require infinite space - there has to be a trick to allow compressing it, e.g. that the objects at some point refer back to themselves. This pointer trick is the useful part, rather than the idea that all objects look like turtles, and is covered by the productivity principle.

Remember the Vasa
~~~~~~~~~~~~~~~~~

Bjarne Stroustrup `seems fond <https://www.stroustrup.com/P0977-remember-the-vasa.pdf>`__ of the phrase "Remember the Vasa" to warn against large last-minute changes. According to `Wikipedia <https://en.wikipedia.org/wiki/Vasa_(ship)>`__, the Vasa was a ship that sunk because the center of gravity was too high. Despite rumors that it was redesigned, there is no evidence that any alterations were performed during construction. It appears to have been built exactly as its designer Henrik Hybertsson envisioned it. And the design was obviously incorrect - a survey of shipwrights at the inquest after the sinking said the ship design "didn't have enough belly". So the only lesson I get is to learn from experienced designers to avoid making mistakes. But this is just T.S. Eliot's principle to steal from great poets.

Hungarian notation
~~~~~~~~~~~~~~~~~~

Hungarian notation puts abbreviated type annotations in variable names, so humans can check that the types are correct. But the compiler already checks declared types, automatically and much more thoroughly. So in the end it is noise. Mathematicians do use single-letter variables with subscripts, but these do not encode types, they are just abbreviations - e.g. ``x`` stands for "first coordinate". Per `Stroustrup <https://www.stroustrup.com/bs_faq2.html#Hungarian>`__ it is "a maintenance hazard and a serious detriment to good code. Avoid it as the plague."

Goals
=====

The ultimate
------------

Stroscot aims to be the ultimate programming language, rather than something just alright. The goal is to win the `ultimate showdown of ultimate destiny <https://www.youtube.com/watch?v=HDXYfulsRBA>`__ w.r.t. programming languages. This has been called "silly" by Dennis Ritchie (author of C) and "the dream of immature programmers" by Bjarne Stroustrup (author of C++), :cite:`sutterFamilyLanguages2000` but I think it can be made to work. To bring in an analogy with weapons, the question of which firearm is strongest is quite subjective and a matter of debate, due to loading and capacity questions. But the Tsar Bomba is without question the strongest weapon in history. In this analogy Stroscot would be an early nuke prototype.

Stroustrup claims there are "genuine design choices and tradeoffs" to consider, which I agree with to a point. Many queries in a compiler are too expensive to compute exactly and the method used to approximate the answer can be refined or optimized. There are competing approaches to answering these questions and methods of combining solvers to obtain more precise answers. The time/precision tradeoff here is real. But these are implementation tradeoffs, and don't affect the overall design of the language. While there may not be a best solver, there is a best set of syntax and features.

Global maximum
--------------

Stroscot aims to be a global maximum of features and syntax, based on the following optimization criteria in order:

Functionality
~~~~~~~~~~~~~

Stroscot is a `wide-spectrum language <https://en.wikipedia.org/wiki/Wide-spectrum_language>`__. If the language can't do X, then people will choose to use another language that can do X. Many languages suffer from "idea envy", where they try to retrofit new ideas from other languages. For example C++ and Java have recently added lambdas. This retrofitting is due to a shallow intellectual base. No idea is original, and lambdas are quite old. With sufficient research these ideas can be uncovered and incorporated.

Stroscot is based on a survey of the academic literature and uses ideas and techniques mainly from decades ago but also a few published in the past few years. It is actually really hard to come up with better ideas than these papers. I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much.

By preferring coverage of all functionality, we ensure a future-proof design, as new ideas are generally small tweaks on old ideas. When a programming language changes significantly, it loses its identity - for example, Python 2 and Python 3 are effectively separate programming languages, as are Perl 5 and Raku (Perl 6). A new language needs new tools and new libraries, so minimizing the number of new languages (breaking changes due to added features) is best.

You may point to INTERCAL's COMEFROM as something best avoided, but it's not hard to implement. The trickier parts are actually at the low level, interfacing memory management and calling conventions, and the value proposition there for a powerful interface should be clear. Another theory is that, even if Stroscot fails as a language, implementing lots of features will make people copy Stroscot's list of features.

Minimum set of built-in features
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Programming features overlap and solve particular needs, so we need to select a consistent set of features to implement. Fortunately there are only so many ways to implement a given feature. The compiler will provide the smallest set of features that can satisfy all functionality needs in a straightforward manner, trivializing them. E.g. because COMEFROM can be implemented with continuations and macros, we implement continuations and macros, rather than COMEFROM. By selecting the minimum, we ensure the built-in features are a "basis" in the sense that none are redundant. Fewer concepts simplifies the whole language, and approximates Python's goal of "There should be one-- and preferably only one --obvious way to do it." Also it ensures stability - write the compiler once and then go do something else.

Learnability
~~~~~~~~~~~~

It's often not that easy to learn a language. Google searches will often yield irrelevant results. Official documentation can be useful, but is often filled with terse wording, links to lengthy discussions containing irrelevant detail, and TODOs. The truth can be found in the compiler source code, but this often has one-letter variable names, very few comments, and an assumption that you know the coding style and design of the compiler.

Learnability means making things easier for generations of beginners by making the language "intuitive" so that language choices can be guessed rather than looked up. There is some amount of English discrimination involved, as the learnability studies' "beginners" are limited to English speakers in Western colleges, but English is the most popular language, and there is the functionality to translate Stroscot to other languages.

Learnability does not necessarily mean making the language similar to existing languages. Such a language might be easier for experts to learn in the short run, but in the long run (assuming Stroscot is successful) there will be many more novices than experts that need to learn the language, so the novices should be prioritized.

Concision
~~~~~~~~~

If there is a verbose syntax and a terse syntax (as measured by characters or screen space usage), both equally learnable, then the terse syntax is better, because the program can be more cheaply printed out and literate documentation is mainly made up of the prose/code comments rather than code.

APL is sometimes criticized for being too concise, but the actual (learnability) issue with APL is that, like Chinese, it has a lot of symbols and hence novices and experts alike suffer from `character amnesia <https://en.wikipedia.org/wiki/Character_amnesia>`__. J uses ASCII symbols hence mitigates the issue and is `praised for its terseness <https://procyonic.org/blog/a-critique-of-the-programming-language-j/>`__. But it still is difficult for novices to learn (basically you have to memorize `this page <https://code.jsoftware.com/wiki/NuVoc>`__) so an syntax based on English words may be better.

Performance
~~~~~~~~~~~

Stroscot aims for C-like performance on C-like programs, and similarly to match or exceed the performance of other styles of programming on their compilers. Beyond that, it is hard to make guarantees about the performance of any of the more expressive features. Since the algorithms used are best-in-class, Stroscot will likely give acceptable performance, but some problems are undecidable and the heuristics used may not be sufficient to prevent a combinatorial explosion; such explosions are of course bugs and patches fixing them will be accepted.

In the near term, since there is no compiler or interpreter fully implemented, performance is not measurable and hence is not a consideration. Once the interpreter can pass the tower of interpreters test, that will be the main performance criterion for it. For compilation, besides optimizing the generated code, the main performance-focused feature will be fine-grained incremental compilation to reduce compile times.

"Slow" is relative - if you can do 100x speedups then slow becomes fast and it's a qualitative difference. Features can't be gated on performance - implement first, speed up later. A lot of the time programs aren't written for speed. Most programmers can’t even measure performance correctly - compiled for debug instead of release, etc. Programmers want a convenient language however slow, and for the better programmers a way to speed up their programs when they're slow (profiling, performance-optimized code). Researchers prefer an inefficient language for which it is easy to devise optimizations and improvements. Similarly programmers don't want reliable code - they prefer convenience and don't want to learn new concepts. They will ship when their management says “ship!”

World domination
----------------

Stroscot aims to replace all the programming languages in use today. Mainly this involves improving FFI support and interoperability with C and C++. In particular we need to be able to parse headers and use data from them with Stroscot. Since headers include code we need to be able to fully compile C/C++, so that Stroscot is the sole compiler and all of its global optimizations can be used (`zig cc <https://andrewkelley.me/post/zig-cc-powerful-drop-in-replacement-gcc-clang.html>`__ is an example of how this works). The linkage is asymmetric - you can export specific C-style constructs back to C, but C can't use functions that depend on more advanced features.

Once the C/C++ implementation is stable enough for production use, focus will shift to developing automated conversion tools for other languages like Python and Java, so that the surface syntax can be changed to Stroscot's. And yes, this is the `E-E-E strategy <https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish>`__, but Stroscot is open source so it's all OK.

Standardization doesn't seem necessary, a popular language builds its own standard and Python, the world's most popular language as of `July 2022 <https://www.tiobe.com/tiobe-index/>`__, has `never been <https://stackoverflow.com/questions/1535702/python-not-a-standardized-language>`__ formally standardized. But there needs to be an open-source cross-platform implementation, with a committee process for changes to build consensus and ensure stability. Another alternative is to freeze Stroscot after release and design a new best language every 3-5 years.

Criticisms
==========

These criticisms from the checklist seem valid.

* Stroscot lacks reflection.
* Stroscot relies on an optimization which has never been shown possible
* Stroscot requires the compiler to be present at runtime
* Stroscot requires the language runtime to be present at compile-time
* Dangerous behavior is only a warning

Other programming languages
===========================

There are many existing programming languages to learn from. All of them have had effort put into their design so their features should be considered. But the disadvantages to a feature are not obvious and generally can only be found by examining complexities in large software projects in the language. The trick is to isolate the use case and cut the Gordian knot in a surgical manner.

.. _inspiring-projects:

Inspiring projects:

-  `Lever <https://github.com/cheery/lever/>`__
-  `Jai <https://github.com/BSVino/JaiPrimer/blob/4a2d14f3e1c8e82a4ba68b81d3fd7d8d438e955c/JaiPrimer.md>`__
-  `Pinafore <https://pinafore.info/>`__
-  `Macro Lambda Calculus <http://github.com/codedot/lambda>`__
-  `Wat <https://github.com/manuel/wat-js>`__
-  `Atomo <https://github.com/vito/atomo>`__ / `Atomy <https://github.com/vito/atomy>`__

Languages in TIOBE index order:

Python

* Most popular on TIOBE index, said to be "easy to learn for beginners", "simple and elegant syntax" "similar to English".
* brevity, readability, developer-friendliness make it 5-10x more productive than Java
* "Batteries included" standard libraries, such as lists and dictionaries, numpy (BLAS wrapper) and scipy
* Twisted web framework, TensorFlow machine learning framework
* Mixed reference counting / tracing GC memory management
* Significant indentation - still a point of contention, e.g. whether it makes copy pasting code harder
* C++ interpreter CPython, slow performance. PyPy exists but has't been widely adopted due to incompatibility.

C

* old and widespread language. Language of most OS's, hence runs just about everywhere (portable).
* statically compiled, compilers are very efficient.
* unsafe pointers, common to see memory corruption and security vulnerabilities. valgrind, smart fuzzing, and static analysis have allowed catching these. Also there is the Boehm GC, used by many people who don't want to deal with memory management.
* header files slow down compilation as they have to be read many times during compilation

Java

* Baroque type system, many types of class-like thing (interfaces, enumerations, anonymous adapters), with generics on top
* Compromises between performance and expressiveness such as covariant arrays
* The OO mantra has led to design patterns, which are a reference point for features support with explicit syntax. The class-based syntax for the patterns is not worth emulating.
* try-finally and checked exceptions have wasted the time of many programmers.
* Keyword soup for declarations, such as "public static void main".
* Lack of operator overloading such as ``+`` for ``BigInteger``
* Every object has a 4-byte header and identity using ``==``. No value types besides primitives.
* Requirement that the class name must match the directory name.  When moving functionality around this implies a lot of changes inside source files. Led to IDEs with extensive support for refactoring.
* Static methods. Scoped to a class, but not related to objects. Can be very confusing.
* JIT is probably best in the world for throughput. Startup is slow but throughput matches C performance in many cases.
* Garbage collector takes big chunks of CPU time at irregular intervals. Low-pause GCs trade this for continuous overhead. Still not solved, around 15% overhead on wall clock time . :cite:`caiDistillingRealCost2022`

C++

* many features, which interact in messy/complex ways making C++ take a long time to learn
* fast, efficient standard libraries similar to hand-tuned code (but missing many features, see also Boost)
* templates, efficient at runtime but slow at compile time
* memory unsafe like C, although smart pointers make this a little better.

C#

* best designed C-style syntax - e.g. introduced async/await
* wide usage - desktop software (Windows), games (MonoGame, Unity), web development (ASP.NET Core), mobile (Xamarin)

Visual Basic

* "mentally mutilates" programmers (according to Dijkstra)
* runs on .NET, so very similar to C# in semantics

JavaScript

* second-best JIT, optimized for startup time - fast bytecode interpreters
* many strange features such as implicit type conversion, ``with`` statement, and ``eval``

Swift

* Automatic reference counting, interesting but not something I want to copy
* syntax for exception handling, if let/guard let
* `exponentially slow <https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html>`__ type inference for numeric expressions, with bad heuristics

Delphi / Object Pascal

* still kicking
* proprietary, so not worth looking at too closely

PHP

* Initial design was hacked together quickly, inconsistent API design. Could be fixed but backwards compatibility was held to be more important.
* Several features with huge security or performance impact: eval, weak typing

Objective C

* deprecated by Apple in favor of Swift, but a good comparison against C++

Go

* opinionated design, touts meaningless features such as "strong typing"
* goroutines, killer feature - but stackless continuations are better
* finally added generics after a long time
* supposedly a Python replacement, but TensorFlow is mainly in Python and the Go binding `isn't officially supported <https://github.com/tensorflow/build/tree/master/golang_install_guide>`__

R

* numerous libraries for statistics and data analysis
* lazy evaluation

Perl

* A mess with the Raku split
* Various libraries on CPAN are good
* Contexts and sigils, terrible syntax for beginners

Lua

* Use of "tables" for everything is interesting
* LuaJIT was fast but the main developer left due to lack of income. Stroscot needs to avoid the same fate.

Ruby

* weird syntax, e.g. expression by itself is return value - causes mistakes.
* Rails is `(still) <https://www.jetbrains.com/lp/devecosystem-2021/ruby/#Ruby_what-web-development-tools-and-or-frameworks-do-you-regularly-use-if-any>`__ the most popular framework
* slow, `YJIT <https://github.com/ruby/ruby/blob/master/doc/yjit/yjit.md>`__ added in 3.1

Prolog

* old language, the family is discussed in the Logic Programming article

Rust

* good standard library design and documentation, probably worth copying
* voted "most loved" by StackOverflow
* borrow checker, can't even write linked lists without `endless pain <https://rcoh.me/posts/rust-linked-list-basically-impossible/>`__. They `end up <https://rust-unofficial.github.io/too-many-lists/third-layout.html>`__  using reference counting as a substitute for GC to ensure memory safety
* concurrency safe, but async suffers from "borrow checker"-itis and uses atomic reference counting

Julia

* good support for concurrency/parallelism
* C+Fortran+Python FFIs and syntax
* JIT design assumes trampolines, performance barrier

Kotlin

* JVM languages with improved features compared to Java
* val keyword instead of final, null safety, extension methods, first-class type parameters
* coroutines

D

* C/C++ style but different. Never really took off AFAICT.
* many features that have been incorporated into C++, others that haven't like scope guards

Scala

* Type inference, allows avoiding repetition of Java such as ``SomeModule.MyClass v = new SomeModule.MyClass();``
* complex type system: implicit conversions, subtyping

TypeScript

* `near superset <https://stackoverflow.com/questions/29918324/is-typescript-really-a-superset-of-javascript>`__ of JavaScript with an unsound type system
* doesn't really add anything besides the types, so only useful for ideas on gradual typing

Haskell

* "finest imperative programming language"
* small community, few core/maintenance developers (mainly SPJ) compared to size of codebase
* good in benchmarks and scripting but GC is still not usable in production
* poor library design, e.g. verbose naming conventions

Clojure

* one of few languages to use software transactional memory, custom implementation "MVCC"
* `interesting talks <https://github.com/matthiasn/talk-transcripts/tree/master/Hickey_Rich>`__ on functional programming and language design
* runs well on JVM

Elm

* small ecosystem
* derivative of OCaml
* no substantial commits in main repo since 2019
* BDFL doing "exploratory work" closed-repo, most recently described in a 2021 `status update <https://discourse.elm-lang.org/t/status-update-3-nov-2021/7870>`__

Erlang

* has a well-tested distributed, fault-tolerant, reliable, soft real-time, concurrent database
* designed to be crash-only, restart tolerant
* not used much outside Ericsson

Elixir

* based on Erlang, new and supposedly great syntax

Flix

* Long list of principles, most of which I disagree with. The corresponding Stroscot opinions are expressed on their respective pages.
