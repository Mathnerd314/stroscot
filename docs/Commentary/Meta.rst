About
#####

Motivation
==========

Why another programming language, you may ask?  Why does a painter paint? To paraphrase Chapter 1 of The Mythical Man Month, there is the joy of: [#tractable]_
* designing and making complex and intricate things
* making things that create real results and are useful to other people
* constantly learning due to the nonrepeating nature of the task

It is the duty of a programming language to spread this joy far and wide.

Another line of motivation comes from `XKCD <https://xkcd.com/927/>`__ (paraphrased):

.. code-block:: RST

  How programming languages proliferate
  -------------------------------------
  SITUATION: There are 14 competing programming languages.

  Geek: 14?! Ridiculous! We need to develop one universal programming language
    that covers everyone's use cases.

  Soon: There are 15 competing programming languages.

Stroscot aims to be that universal language. The shackles of languages past are no more, and
programs are freed to be idealized to their purest form. Abstractions reach their highest level and coding is automated as far as possible.

.. [#tractable] Notably omitted here is Brooks's assertion that programming is a "tractable medium". It is not tractable. Programming is hard. :cite:`beckerWhatDoesSaying2021` questions this, but IMO fails quite badly - although incomplete, all the evidence available shows that programming is hard. The only positive contribution of the article is a reminder to focus on computer education and usability.

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
* If it isn't documented, it doesn't exist (Coding Horror)
* Take a list of items. Imagine a specific walk through a familiar place. List distinctive features of the route. Combine each feature with an item to form new outrageous/memorable images. (Memory palace)
* People prefer a middle level of complexity: too simple and we are bored, too complex and we are confused. Moreover, the ideal level of complexity is a moving target, because the more expert we become at any subject, the more complexity we prefer. (Donald Norman)
* Better depends on your goodness metric (`Jim Waldo <http://web.archive.org/web/20210325222034/https://www.artima.com/weblogs/viewpost.jsp?thread=24807>`__)
* The shorter the [edit-test] cycle, the happier the programmer. (`Yue Yao <https://tripack45.github.io/2018/11/03/edit-compile-run/>`__)
* Do all things without grumbling or complaining (Philippians 2:14)
* Secure by default: The default level of access should have the least privilege and the most number of checks. (OpenBSD)
* Organize functions by functionality into expressive components. (`Uli Weltersbach <https://reasoncodeexample.com/2016/03/06/a-place-for-everything-and-everything-in-its-place-thoughts-on-organizing-source-code-by-type/>`__)
* When two elements of an interface conflict, or are ambiguous, the behavior should be that which will least surprise the user; in particular a programmer should try to think of the behavior that will least surprise someone who uses the program, rather than that behavior that is natural from knowing the inner workings of the program. (`POLA <https://en.wikipedia.org/wiki/Principle_of_least_astonishment>`__)

Non-principles
--------------

Readability
~~~~~~~~~~~

Readability is a function of the program, not so much the programming language. Liberal comments can make even Brainfuck programs quite readable. So readability is only a consideration for the compiler - but even there, the extensive documentation means that few people will actually read the compiler code. So readability isn't a principle for Stroscot.

 But cutting the time needed to understand code results in a shorter edit-test cycle so is often a win (the split is 5% New Code, 25% Modifying Existing Code, 70% Understanding Code from `here <https://web.archive.org/web/20060213015737/http://blogs.msdn.com/peterhal/archive/2006/01/04/509302.aspx>`__).

Turtles all the way down
~~~~~~~~~~~~~~~~~~~~~~~~

"Turtles all the way down" only makes sense in the context of a directional layout, such as inheritance or composition. Stroscot has :ref:`no inheritance`, so it must be composition. But an infinite object tree would require infinite space - there has to be a trick, e.g. that the objects at some point repeat in a cycle. This trick is itself a useful programming tool, much more useful than the idea that all objects "look the same".

The Vasa
~~~~~~~~

Bjarne Stroustrup seems fond of the phrase "Remember the Vasa" to warn against large last-minute changes. According to `Wikipedia <https://en.wikipedia.org/wiki/Vasa_(ship)>`, the Vasa was a ship that sunk because the center of gravity was too high. Despite rumors that it was redesigned, there is no evidence that any alterations were performed during construction. It appears to have been built exactly as its designer Henrik Hybertsson envisioned it. And the design was obviously incorrect - a survey of shipwrights at the inquest after the sinking said the ship "didn't have enough belly". So the only lesson I get is to learn from experienced designers to avoid making mistakes. But this is just T.S. Eliot's principle to steal from great poets.

Goals
=====

The ultimate
------------

Stroscot aims to be the ultimate programming language, rather than something just alright. The goal is to win the `ultimate showdown of ultimate destiny <https://www.youtube.com/watch?v=HDXYfulsRBA>`__ w.r.t. programming languages. This has been called "silly" by Dennis Ritchie (author of C) and "the dream of immature programmers" by Bjarne Stroustrup, :cite:`sutterFamilyLanguages2000` but I think it can be made to work. The question of which firearm is strongest is quite subjective and a matter of debate, due to loading and capacity questions. But the Tsar Bomba is without question the strongest weapon in history. In this analogy Stroscot would be an early nuke prototype.

Stroustrup claims there are "genuine design choices and tradeoffs" to consider, which I agree with to a point. Many queries in a compiler are undecidable and the method used to approximate the answer can be refined or optimized. There are competing approaches to answering these questions and methods of combining solvers to obtain more precise answers. The time/precision tradeoff here is real. But these are implementation tradeoffs, and mainly affect the design of the compiler. Syntax and features all have global optima.

The main reason is that a lot of programming features overlap. By picking the best and most expressive version, there is only one syntax and feature to consider. This applies to anything from low-level systems programming to high-level computer algebra system manipulations. You may point to INTERCAL's COMEFROM as something best left unimplemented, but it's not hard to implement with continuations and macros. The trickier parts are actually at the low level, interfacing memory management and calling conventions, and the value proposition there for a consistent, powerful interace should be clear.

Many languages suffer from "idea envy", where new ideas in other languages seem better than the old ones implemented in the traditional language. For example C++ and Java have added lambdas. This is due to a shallow intellectual base. No idea is original, and lambdas are quite old. With sufficient research these old ideas can be uncovered and incorporated.

My theory is that, even if Stroscot fails as a language, if I implement complicated but generic algorithms for the compiler then people will refer to Stroscot just for the algorithms. I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much.

Another focus is learnability, particularly for novices. It's often not that easy to learn a language. Google searches will often yield irrelevant results. Official documentation can be useful, but is often filled with terse wording, links to lengthy discussions containing irrelevant detail, and TODOs. The truth can be found in the source code, but this often has one-letter variable names, very few comments, and an assumption that you know the coding style and meaning of the language constructs used.

Performance
-----------

Stroscot aims for C-like performance on C-like programs, and similarly to match or exceed the performance of other styles of programming on their compilers. Beyond that, it is hard to make guarantees about the performance of any of the more expressive features. Since the algorithms used are best-in-class, Stroscot will likely give acceptable performance, but some problems are undecidable and the heuristics used may not be sufficient to prevent a combinatorial explosion; such explosions are of course bugs and patches fixing them will be accepted.

In the near term, since there is no compiler or interpreter fully implemented, performance is not measurable and hence is not a consideration. Once the interpreter can pass the tower of interpreters test, that will be the main performance criterion for it. For compilation, besides optimizing the generated code, the main performance-focused feature will be fine-grained incremental compilation to reduce compile times.

"Slow" is relative - if you can do 100x speedups then slow becomes fast and it's a qualitative difference. Features can't be gated on performance - implement first, speed up later. A lot of the time programs aren't written for speed. Most programmers can’t even measure performance correctly - compiled for debug instead of release, etc. Programmers want a convenient language however slow, and for the better programmers a way to speed up their programs when they're slow (profiling, performance-optimized code). Researchers prefer an inefficient language for which it is easy to devise optimizations and improvements. Similarly programmers don't want reliable code - they prefer convenience and don't want to learn new concepts. They will ship when their management says “ship!”

World domination
----------------

Long term, Stroscot aims to replace all the programming languages in use today. Mainly this involves improving FFI support and interoperability with C and C++. In particular we need to be able to parse headers and use data from them with Stroscot. Since headers include code we need to be able to fully compile C/C++, so that Stroscot is the sole compiler and all of its global optimizations can be used (`zig cc <https://andrewkelley.me/post/zig-cc-powerful-drop-in-replacement-gcc-clang.html>`__ is an example of how this works). No language I know of has developed decent two-way linkage - you can export specific C-style constructs back to C, but C can't use any of the more advanced features.

Once the C/C++ implementation is stable enough for production use, focus will shift to developing automated conversion tools for other languages like Python and Java, so that the surface syntax can be changed to Stroscot's. And yes, this is the `E-E-E strategy <https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish>`__, but Stroscot is open source so it's all OK.

Standardization doesn't seem necessary, a popular language builds its own standard. But there needs to be an open-source cross-platform implementation, with a committee process for changes to build consensus and ensure stability. Another alternative is to freeze Stroscot after release and design a new best language every 3-5 years

.. _inspiring-projects:

Tagline
=======

The tagline for Stroscot is "an imperative programming language for modern processors". The breakdown:

Stros
  This is a vague reference to Charles Stross, author of the sci-fi book "Accelerando". In particular Stroscot aims to speed up the pace of technological development.

cot
  Similar to how the "trek" in "Star Trek" expresses a journey to find new worlds, the cot here expresses that Stroscot provides comfortable support while still being flexible, lightweight, portable, and compact.

imperative programming language
  This is a riff of the assertion "Haskell is the world's finest imperative programming language", first said in  the awkward squad paper :cite:`jonesTacklingAwkwardSquad2001` because "actions are first class values" in Haskell.

modern processors
  This is mostly because I don't want to have to write code generators for numerous archaic architectures. The plan for now is to only target 64-bit x86 / ARM and then later add a mode to generate LLVM IR.

Logo
====

The logo for Stroscot is inspired by the color scheme of the cover of Accelerando (the red rise of the machines), the `cot icon <https://thenounproject.com/term/cot/154357/>`__ by P Thanga Vignesh from the Noun Project, and a design I made a while back of "the infinite stack". The Paint picture I made is lost in time, but the general idea is you had a (potentially infinite) stack of reusable/composable components (the white/black blocks in the current icon) going left-to-right, and underneath it a processor (white) and various glue bits (red/blue).

The current logo is made mainly to solve the issue of finding the browser tabs with Stroscot documentation open (the default icon is unhelpful), so it is an instance of "programmer art". Interested parties can submit alternate designs and once there are a few submissions there will be a vote.

Choices
=======

Documentation first
-------------------

It is tempting to just start coding - a prototype might attract contributors and let the project gain momentum. But as the principle goes, "if it isn't documented, it doesn't exist". Looking at HN submissions of programming languages, the best docs win - it's only "famous" languages that can submit a Github repo full of files but without a README and still get discussion. To do well we need at least a README. But I'm going with a wiki style so I can write down every last detail. And there are code snippets for the places where writing code is clearer than explaining in English.

Sphinx
------

GH Pages/Jekyll can't do forward/back links. Checking out various options, Sphinx is used by Clang, GHC, Futhark, etc., although not Rust or Java. And it has a lot of features like automatic TOC generation, syntax highlighting, Graphviz, Bibtex integration, ... so far it's proving its worth. It's run via a Travis CI script and the generated docs are stored in the gh-pages branch.

Organization
------------

The documentation is organized according to `this system <https://diataxis.fr/>`_, because it shows up when you google "documentation system" and I couldn't find anything better.

The four functions:

* Getting started  (system calls these "tutorials") - overview information for newcomers, learning oriented (aim for a 1-week course)
* how-to guides - specific tasks / goals, e.g. solve specific error messages
* technical reference - describe the machinery, with as little fluff as possible
* commentary/explanation - understanding, explain the possible alternatives and why a choice was made

The categorization procedure:
* Does it describe specific actions the reader should take (1), or is it theoretical (2)?
* Is it an exploratory piece of art (A), or is it a descriptive quick-reference (B)?
* 1A: getting started
* 1B: how-to guide
* 2A: commentary
* 2B: reference

Quotes before commas
--------------------

The `MLA style guide <https://style.mla.org/the-placement-of-a-comma-or-period-after-a-quotation/>`__ doesn't explicitly forbid it, mentioning that it's similar to British style, and it matches the logical structure. Proper nesting is important in programming and it seems strange to ignore this. And it's the `official style on Wikipedia <https://en.wikipedia.org/wiki/MOS:LQUOTE>`__.

Forbidden words
---------------

A fair number of words in programming seem to be meaningless gibberish. So don't use them:

* structured
* paradigm
* impure
* object oriented
* static

Open source
-----------

The license is still undecided, so set to WTFPL.

Real "open source" goes beyond a LICENSE file:

* open development process
* appreciate comments or ideas from the community, benefit from other people's expertise without flatly contradicting them
* clearly documented process for contributing in CONTRIBUTING.md file, not "Old Boy's network"
* pull requests by community members should be merged or closed within a year
* records of decision making, decisions should be explained with reasoning
* development discussions should stay open for longer than 10 days
* mailing lists, forum, chat
* deleting posts, blocking, and locking should be reserved for spam, not civil criticism
* communication style should be civil, friendly, and helpful, and not aggressive or controlling.
* leadership should not be a corrupt cabal that gives special treatment to itself. They need to think of themselves as stewards and not owners. The difficulty goes up as more people are affected by decisions and more contributions received from people.
* possible to fork or patch without being called a "hostile attack"

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

Java
* Baroque type system, many types of class-like thing (interfaces, enumerations, anonymous adapters), with generics on top
* Compromises between performance and expressiveness such as covariant arrays
* The OO mantra has led to design patterns, which are a reference point for features support with explicit syntax. The class-based syntax for the patterns is not worth emulating.
* try-finally and checked exceptions have wasted the time of many programmers.
* Keyword soup for declarations, such as "public static void main".
* Lack of operators such as ``+`` for ``BigInteger``
* Every object has a 4-byte header and identity using ``==``. No value types besides primitives.
* Requirement that the class name must match the directory name.  When moving functionality around this implies a lot of changes inside source files. Led to IDEs with extensive support for refactoring.
* Static methods. Scoped to a class, but not related to objects. Can be very confusing.
* JIT is probably best in the world. Startup is slow but throughput matches C performance in many cases.
* Garbage collector takes big chunks of CPU time at irregular intervals. Low-pause GCs trade this for continuous overhead. Still not solved, something like 15% wall clock overhead. :cite:`caiDistillingRealCost2022`

Python
* Most popular besides C/C++, said to be "easy to learn for beginners", "simple and elegant syntax" "similar to English"
* brevity, readability, developer-friendliness make it 5-10x more productive than Java
* "Batteries included" standard library, such as lists and dictionaries
* Mixed reference counting / tracing GC memory management
* Significant indentation - still a point of contention, e.g. whether it makes copy pasting code harder
* C++ interpreter CPython, low performance. Projects such as PyPy exist but haven't been widely adopted.

C
* old and widespread language. Language of most OS's, hence runs just about everywhere (portable).
* statically compiled, compilers are very efficient.
* unsafe pointers, common to see memory corruption and security vulnerabilities. valgrind, smart fuzzing, and static analysis have allowed catching these. Also there is the Boehm GC, used by many people who don't want to deal with memory management.
* header files slow down compilation as they have to be read many times during compilation

Scala
* Type inference, allows avoiding repetition of Java such as ``SomeModule.MyClass v = new SomeModule.MyClass();``

C++
* many features, which interact in complex ways making C++ take a long time to learn
* fast, efficient standard libraries similar to hand-tuned code (but missing many features, see also Boost)
* templates, efficient at runtime but slow at compile time
* memory unsafe like C, although smart pointers make this a little better.

D
* C/C++ style but different. never really took off.
* many features that have been incorporated in C++, others that haven't been

Ruby
* crazy scripting language. weird syntax.
* expression by itself is return value, causes mistakes.

Stroscot aims to be a global maximum. If the language can't do X, then people will choose to use another language that can do X. Macros make it easy to rapidly add new syntax so often "do X" is as simple as writing another library. Underneath the syntax there are multiple ways to implement X - Stroscot has to pick primitives that combine well together. There’s a combinatorial explosion in feature interactions.

Another advantage of being a maximum is stability. When a programming language changes significantly, it loses its identity - for example, Python 2 and Python 3 are effectively separate programming languages, as are Perl 5 and Perl 6 (Raku). A new language needs new tools and new libraries, so minimizing the number of new languages (breaking changes) is best. Write the compiler once and then go do something else. Stroscot is based on a survey of the academic literature and uses ideas and techniques that are decades old and have been recognized to be effective for their purpose. It is actually really hard to come up with better ideas than these papers.

A low-priority goal is simplicity at the syntactic and semantic level. In a lot of cases this is overridden by other goals, e.g. with learnability using ``=`` for both assignment and equality comparison complicates the syntax quite a bit. But avoiding weird syntax features such as Rust's turbofish ``::<>`` seems reasonable. And if the programming language has fewer concepts, there’s less to learn, and novices will become proficient faster. Particularly, redundant features should be avoided - although macros allow defining redundant syntax, consistency is better.

erase distinction between commands and expressions
erase distinction between compile time and execution time
avoid implicit conversion

Language design is hard because the space of possible programming languages is infinite, and so compromises have to be made. It’s hard to provide hard numbers to quantify what makes one design better than another. Some of the things that can be quantified to some degree are the complexity of the implementation of a language and also the way that a particular language implementation performs.

My PhD thesis involved the implementation of a JIT compiler for JavaScript ES5. As such, I got to become intimately familiar with the semantics of the language and everything that has to go on behind the scenes to make JavaScript code run fast. At times, that was a frustrating experience. I’ve become convinced that a lot of the complexity and the hidden behaviors in JS and in many other languages are essentially bad for everyone.

Unnecessary complexity in a language is bad for those learning the language, because it makes the language less intuitive and harder to learn. It’s bad for the programmers working with the language everyday, because it increases their cognitive load and makes it harder to communicate about code. It’s bad for language implementers and tool maintainers, because it makes their job harder, but at the end of the day, it’s also bad for end users, because it leads to software with more bugs and poorer performance.

many object-oriented languages have this idea, borrowed from Smalltalk, that everything should be an object, including booleans and integer values. At the same time, languages implementation for these languages have to do a lot of work behind the scenes to try and represent integers efficiently (as machine integers) while presenting an interface to the user that resembles that of an object. However, the abstraction presented to the user for an integer object is typically not really the same as that of a normal OOP object, it’s a leaky abstraction, because being able to redefine integer values makes no sense, because integer values have to be singletons, and because being able to store properties/attributes on integers is both dumb and terrible for performance and so typically isn’t allowed.

Ultimately, integers are not objects in the object oriented sense. They’re a distinct type of atomic value with a special meaning, and that’s okay. The mistaken idea that “everything should be an object” doesn’t actually simplify anything in practice. We’re lying to ourselves, and in doing so, we actually makes the life of both language implementers and programmers more complicated.
Actionable Advice

This blog post has turned into more of a rant than I expected it to be. It’s easy to critique the status quo, but I’ll also try to conclude with some actionable advice. My first piece of advice for aspiring language designers is that you should start small. Your language is a user interface, and an API which people use to interface with machines. The smaller the API surface, the less you risk introducing accidental complexity and subtle design mistakes.

My second piece of advice is that if you can, you should try to keep your language small. Limiting yourself to a smaller feature set likely means you will want to choose features that don’t overlap and that provide the most expressiveness, the most value to programmers. If you do want to grow your language, do it slowly. Take some time to write code in your language and work through the potential implications of the design changes that you are making.

It’s easy to add new features later on, but if you add new features and people begin using them, it’s going to be hard or even impossible to take these features back, so choose wisely. Remember that you don’t have to please everyone and say yes to every feature request. No language or tool can possibly satisfy every use case, and in my opinion, trying to do so is a mistake.

Lastly, remember that language design is an art. It’s a delicate balance of many different constraints, just like user interface design. Brainfuck is a language that is very small and has very few concepts, but nobody would call it expressive or elegant. Lisp is regarded by many as one of the most beautiful and elegant languages in existence, but my PhD advisor, a Scheme fanatic, had the habit of writing code with single-letter variable names and very few comments. An elegant language doesn’t automatically make for elegant code, but you can encourage good coding practices if you lead by example.

learnability
tool support
performance

