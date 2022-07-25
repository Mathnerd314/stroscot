Meta
####

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

What are these principles useful for? Mainly wasting time. Practical decisions are are made by judging pros and cons.

Non-principles
--------------

Readability
~~~~~~~~~~~

Readability is a function of the program, not so much the programming language. Liberal comments can make even Brainfuck programs quite readable. So readability is only a consideration for the compiler - but even there, the extensive documentation means that few people will actually read the compiler code. So readability isn't a principle for Stroscot.

But cutting the time needed to understand code results in a shorter edit-test cycle so is often a win for that principle (the split is 5% New Code, 25% Modifying Existing Code, 70% Understanding Code from `here <https://web.archive.org/web/20060213015737/http://blogs.msdn.com/peterhal/archive/2006/01/04/509302.aspx>`__).

Turtles all the way down
~~~~~~~~~~~~~~~~~~~~~~~~

"Turtles all the way down" only makes sense in the context of a directional layout, such as inheritance or composition. Stroscot has :ref:`no inheritance <no inheritance>`, so it must be composition. But an infinite object tree would require infinite space - there has to be a trick to allow compressing it, e.g. that the objects at some point refer back to themselves. This pointer trick is the principle, much more useful than the idea that all objects "look the same", and is covered by the productivity principle.

The Vasa
~~~~~~~~

Bjarne Stroustrup `seems fond <https://www.stroustrup.com/P0977-remember-the-vasa.pdf>`__ of the phrase "Remember the Vasa" to warn against large last-minute changes. According to `Wikipedia <https://en.wikipedia.org/wiki/Vasa_(ship)>`__, the Vasa was a ship that sunk because the center of gravity was too high. Despite rumors that it was redesigned, there is no evidence that any alterations were performed during construction. It appears to have been built exactly as its designer Henrik Hybertsson envisioned it. And the design was obviously incorrect - a survey of shipwrights at the inquest after the sinking said the ship design "didn't have enough belly". So the only lesson I get is to learn from experienced designers to avoid making mistakes. But this is just T.S. Eliot's principle to steal from great poets.

Hungarian notation
~~~~~~~~~~~~~~~~~~

Hungarian notation puts types in variable names, so humans can check that the types are correct. But the compiler already checks types, and much more precisely. So in the end it is noise. Mathematicians do use single-letter variables with subscripts, but these do not encode types, they are just abbreviations - ``x`` stands for "first coordinate", etc.

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

Programming features overlap and solve particular needs, so we need to select a consistent set of features to implement. Fortunately there are only so many ways to implement a given feature. The compiler will provide the smallest set of features that can satisfy all functionality needs in a straightforward manner, trivializing them. E.g. because COMEFROM can be implemented with continuations and macros, we implement continuations and macros, rather than COMEFROM. By selecting the minimum, we ensure the built-in features are a "basis" in the sense that none are redundant. Fewer concepts simplifies the whole language. Also it ensures stability - write the compiler once and then go do something else.

Learnability
~~~~~~~~~~~~

It's often not that easy to learn a language. Google searches will often yield irrelevant results. Official documentation can be useful, but is often filled with terse wording, links to lengthy discussions containing irrelevant detail, and TODOs. The truth can be found in the source code, but this often has one-letter variable names, very few comments, and an assumption that you know the coding style and meaning of the language constructs used. Prioritizing learnability will make it easier for generations of beginners. There is some amount of English discrimination involved, as the learnability studies' "beginners" are limited to English speakers in Western colleges, but English is the most popular language, and there is the functionality to translate Stroscot to other languages.

Concision
~~~~~~~~~

If there is a verbose syntax and a terse syntax (as measured by characters or screen space usage), both equally learnable, then the terse syntax is better, because the program can be more cheaply printed out and literate documentation ends up primarily being English prose comments rather than code. The only issue with APL is that, like Chinese, it has a lot of symbols and `character amnesia <https://en.wikipedia.org/wiki/Character_amnesia>`__ is a (learnability) issue, whereas English has a small alphabet hence its input method is easier to remember.

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

The logo for Stroscot is inspired by the color scheme of the cover of Accelerando by Charles Stross (the red rise of the machines), the `cot icon <https://thenounproject.com/term/cot/154357/>`__ by P Thanga Vignesh from the Noun Project, and a design I made a while back of "the infinite stack". The Paint picture I made is lost in time, but the general idea is you had a (potentially infinite) stack of reusable/composable components (the white/black blocks in the current icon) going left-to-right, and underneath it a processor (white) and various glue bits (red/blue).

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

* Tutorial  ("getting started") - overview information for newcomers, learning oriented (aim for a 1-week course)
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

A fair amount of terminology in programming seem to be meaningless or ambiguous. So don't use it:

* dynamic - As `Harper <https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/>`__ points out, this is a marketing term.

  * dynamically typed - "unityped" (short for "has a universal type").
  * dynamic library - shared library
  * dynamic linking - linking at program startup
  * dynamic loading - run-time loading

* static - similar to dynamic, too many overloaded meanings to be usable.

  * statically typed - every language
  * static lifetime - program lifetime, bound when program starts and freed by OS when program terminates
  * static method - utility method, not bound to any object instance
  * static linkage - internal linkage, symbol only available in translation unit
  * static imports - scoped import, import members of modules
  * static library - precompiled file archive
  * static linking - compile time binding, resolving memory addresses at compile time

* pure - prefer the proposition that all expressions have values
* strongly typed - `8 definitions <https://perl.plover.com/yak/12views/samples/slide045.html>`__, all different. It's the semantic equivalent of "amazing", i.e. "My language is strongly typed" == "My language is amazing".
* undecidable - people use this word to imply that it's unimplementable, when there are working solvers like the ones in `termCOMP <https://termination-portal.org/wiki/Termination_Competition>`__ that solve many useful cases. Prefer "complexity at least :math:`\Sigma^0_1`", where :math:`\Sigma^0_1` is in the `arithmetic hierarchy <https://en.wikipedia.org/wiki/Arithmetical_hierarchy>`__, or a more precise class if known. Note that decidable problems / computable sets are in :math:`\Delta_{1}^{0} \subset \Sigma^0_1`.
* primitive - as per `Wikipedia <https://en.wikipedia.org/wiki/Primitive_data_type>`__, primitive is ambiguous and can mean "the base cases of an inductive definition", in which case use "base", or "whatever is provided by a particular processor or compiler", in which case use "built-in". Note that built-in does not mean base, e.g. integers can be defined in terms of booleans hence are not base cases.

Open source
-----------

The license is still undecided, so set to WTFPL to annoy people.

Real "open source" goes beyond a LICENSE file: (per `Luke Plant <https://lukeplant.me.uk/blog/posts/why-im-leaving-elm/>`__)

* open development process, permanent records of decision making, decisions should be explained with reasoning
* appreciate comments or ideas from the community, benefit from other people's expertise without flatly contradicting them
* clearly documented process for contributing in CONTRIBUTING.md file, not "Old Boy's network"
* pull requests by community members should be merged or closed within a year
* deleting posts, blocking, and locking should be reserved for spam, not civil criticism
* communication style should be civil, friendly, and helpful, and not aggressive or controlling.
* leadership should not be a corrupt cabal that gives special treatment to itself. They need to think of themselves as stewards and not owners. The difficulty goes up as more people are affected by decisions and more contributions received from people.
* possible to fork or patch without being called a "hostile attack"


Communication methods
---------------------

Stroscot's documentation first approach should help a lot with open development. As far as information, the main avenue for Stroscot is the Git repo. This has the documentation and the code all-in-one. Secondary sources are:
* real-time chat, for quick questions and discussion. Discord suffices for now (0 people anyway). Alternatives are Gitter, Element, and Matrix which are somewhat more open-source friendly.
* issues, for anything more important. Github issues seems fine, even Swift is using it. If open-source is a concern then `migrating to Gitlab <https://docs.gitlab.com/ee/user/project/import/github.html>`__ is possible.
* in the future, a forum for long-form discussions, where the problem needs more consideration than just the random sample in chat but it's not really an issue with the project. Github discussions is a possibility but Discourse is the standard. There are `free instances <https://free.discourse.group/>`__ for open-source projects, but needs 10+ contributors. Anything relevant to language/standard library development should have an issue filed.

As far as the "ping bot" that closes issues if they are not active, it seems like a good idea since if there is no reporter to discuss with then making progress is hard. IMO the bot should request a little discussion summary if there have been more than a few comments. Something like:

* Goal: Summary of what conditions need to be satisfied to close the issue
* Deliverable: What can be delivered in a few weeks to further the progress of this issue?
* Motivation: What advantages does this goal have?
* Risks: What concerns have been raised about this goal?
* Blockers: What resources or leadership decisions are needed, besides someone implementing it?

The summary doesn't need to be long, it can just link to the relevant comments. If the summary is inaccurate then someone who cares will correct it. And of course if the ping bot activates multiple times but nobody has worked on the issue then "The previous summary is accurate" is fine as the summary.
