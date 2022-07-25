Comparison
##########

This page has two sections:

* a graph of paradigms, showing which language embed into which others
* a brief summary of interesting features of popular programming languages

Paradigms
=========

In linguistics, a paradigm is "a set of linguistic items that form mutually exclusive choices in particular syntactic roles," specifically "a table of all the inflected forms of a particular verb, noun, or adjective." This seems to be a usable definition of a PL paradigm - you have all related versions of a semantic entity.

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

Graphviz has chosen "function" as the root paradigm. This agrees well with experience. Quoting `Spivak <https://www.google.com/books/edition/Calculus/7JKVu_9InRUC?hl=en&gbpv=1&bsq=central%20objects>`__, "the most important concept in all of mathematics is that of a function - in almost every branch of modern mathematics functions turn out to be the central objects of investigation." Looking closer, function is part of an SCC ``function, relation, set, boolean, constraint, optimization``. The mathematical notion of function is broad; a functional relation cannot be expressed naturally using lambdas, but lambdas naturally express functions. So we need constraint logic programming as well to get full expressiveness of our functions.

Other programming languages
===========================

There are many existing programming languages to learn from. All of them have had effort put into their design so their features should be considered. But the disadvantages to a feature are not obvious and generally can only be found by examining complexities in large software projects in the language. The trick is to isolate the use case and cut the Gordian knot in a surgical manner.

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
* Twisted web framework
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

* Initial design was hacked together quickly, inconsistent API design. Could be fixed but backwards compatibility is more important.
* Several features with huge security or performance impact: eval, weak typing

Objective C

* deprecated by Apple in favor of Swift, but a good comparison against C++

Go

* opinionated design, touts meaningless features such as "strong typing"
* goroutines, killer feature
* finally added generics after a long time
* supposedly a Python replacement, but TensorFlow is mainly in Python and the Go binding `isn't officially supported <https://github.com/tensorflow/build/tree/master/golang_install_guide>`__

R

* numerous libraries for statistics and data analysis
* lazy evaluation

Perl

* A mess with the Raku split
* Various libraries on CPAN are good
* Contexts and sigils, terrible syntax IMO

Lua

* Use of "tables" for everything is interesting
* LuaJIT was fast but the main developer left. Storscot needs to avoid the same fate.

Ruby

* weird syntax, e.g. expression by itself is return value - causes mistakes.
* Rails is `(still) <https://www.jetbrains.com/lp/devecosystem-2021/ruby/#Ruby_what-web-development-tools-and-or-frameworks-do-you-regularly-use-if-any>`__ the most popular framework
* slow, `YJIT <https://github.com/ruby/ruby/blob/master/doc/yjit/yjit.md>`__ added in 3.1

Prolog

* The inference algorithm (SLD resolution) is inefficient and should be replaced with DPLL or CDCL. But SLD's simplicity is the main reason Prolog execution is comprehensible.
* Teyjus / λProlog rely on higher order pattern unification. It is possible to use Huet's semi-algorithm for higher order unification, though the lack of most general unifiers complicates things.

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

