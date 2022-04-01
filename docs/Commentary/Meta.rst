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

Stroscot aims to be the ultimate programming language, rather than something just alright. The goal is to win the `ultimate showdown of ultimate destiny <https://www.youtube.com/watch?v=HDXYfulsRBA>`__ w.r.t. programming languages. This has been called "silly" by Dennis Ritchie (author of C) and "the dream of immature programmers" by Bjarne Stroustrup, :cite:`sutterFamilyLanguages2000` but I think it can be made to work. The question of which gun is strongest is quite subjective and a matter of debate. But the Tsar Bomba is without question the strongest weapon in history. In this analogy Stroscot would be an early nuke prototype.

Stroustrup claims there are "genuine design choices and tradeoffs" to consider, which I agree with to a point. Many queries in a compiler are undecidable and the method used to approximate the answer can be refined or optimized. There are competing approaches to answering these questions and methods of combining solvers to obtain more precise answers. The time/precision tradeoff here is real. But these are implementation tradeoffs, and mainly affect the design of the compiler. Syntax and features all have global optima.

The main reason is that a lot of programming features overlap. By picking the best and most expressive version, there is only one syntax and feature to consider. This applies to anything from low-level systems programming to high-level computer algebra system manipulations. You may point to INTERCAL's COMEFROM as something best left unimplemented, but it's not hard to implement with continuations and macros. The trickier parts are actually at the low level, interfacing memory management and calling conventions, and the value proposition there for a consistent, powerful interace should be clear.

Many languages suffer from "idea envy", where new ideas in other languages seem better than the old ones implemented in the traditional language. For example C++ and Java have added lambdas. This is due to a shallow intellectual base. No idea is original, and lambdas are quite old. With sufficient research these old ideas can be uncovered and incorporated.

My theory is that, even if Stroscot fails as a language, if I implement complicated but generic algorithms for the compiler then people will refer to Stroscot just for the algorithms. I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much.

Performance
-----------

Stroscot aims for C-like performance on C-like programs, and similarly to match or exceed the performance of other styles of programming on their compilers. Beyond that, it is hard to make guarantees about the performance of any of the more expressive features. Since the algorithms used are best-in-class, Stroscot will likely give acceptable performance, but some problems are undecidable and the heuristics used may not be sufficient to prevent a combinatorial explosion; such explosions are of course bugs and patches fixing them will be accepted.

In the near term, since there is no compiler or interpreter fully implemented, performance is not measurable and hence is not a consideration. Once the interpreter can pass the tower of interpreters test, that will be the main performance criterion for it. For compilation, besides optimizing the generated code, the main performance-focused feature will be fine-grained incremental compilation to reduce compile times.

"Slow" is relative - if you can do 100x speedups then slow becomes fast and it's a qualitative difference. Features can't be gated on performance - implement first, speed up later. A lot of the time programs aren't written for speed. Most programmers can’t even measure performance correctly - compiled for debug instead of release, etc. Programmers want a convenient language however slow, and for the better programmers a way to speed up their programs when they're slow (profiling, performance-optimized code). Researchers prefer an inefficient language for which it is easy to devise optimizations and improvements. Similarly programmers don't want reliable code - they prefer convenience and don't want to learn new concepts. They will ship when their management says “ship!”

World domination
----------------

Long term, Stroscot aims to replace all the programming languages in use today. Initially this involves improving FFI support and interoperability with C and C++. In particular we need to be able to parse headers and use data from them with Stroscot. Next we want to fully compile other languages, so that Stroscot is the sole compiler and all of its global optimizations can be used (`zig cc <https://andrewkelley.me/post/zig-cc-powerful-drop-in-replacement-gcc-clang.html>`__ is an example of how this works). Once the implementation is stable enough for production use, focus will shift to developing automated conversion tools, so that the surface syntax can be changed to Stroscot's. And yes, this is the `E-E-E strategy <https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish>`__, but Stroscot is open source so it's all OK. No language I know of has developed decent two-way linkage - you can export specific C-style constructs back to C, but C can't use any of the more advanced features.

Standardization doesn't seem necessary, a popular language builds its own standard. But there needs to be an open-source cross-platform implementation, with a committee process for changes to build consensus and ensure stability. Another alternative is to freeze Stroscot after release and design a new best language every 3-5 years

.. _inspiring-projects:

Inspiring projects
==================

-  `Lever <https://github.com/cheery/lever/>`__
-  `Jai <https://github.com/BSVino/JaiPrimer/blob/4a2d14f3e1c8e82a4ba68b81d3fd7d8d438e955c/JaiPrimer.md>`__
-  `Pinafore <https://pinafore.info/>`__
-  `Macro Lambda Calculus <http://github.com/codedot/lambda>`__
-  `Wat <https://github.com/manuel/wat-js>`__
-  `Atomo <https://github.com/vito/atomo>`__ / `Atomy <https://github.com/vito/atomy>`__

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


It's often not that easy to learn a language. Google searches will often yield irrelevant results. Official documentation can be useful, but is often filled with terse wording, links to lengthy discussions containing irrelevant detail, and TODOs. The truth can be found in the source code, but this often has one-letter variable names, very few comments, and an assumption that you know the coding style and meaning of the language constructs used.

Dynamically typed languages are tricky to compile efficiently. There’s been lots of research on efficient JIT compilers for dynamic languages - SELF, Javascript, PyPy, Java - but these are quite involved, and still slower than C. Ahead-of-time compilation is possible as well but not explored, and needs profile data to work properly.

Arbitrary precision is attractive but hard to optimize. Machine-precision integers and floating-point numbers should be easily accessible.

convenient to use
good first language to teach a new programmer
intuitive
