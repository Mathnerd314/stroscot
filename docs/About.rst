About
#####

Executive summary
=================

..
  An executive summary is "half a slide using large print" (128 x's) and gets across how people should use the language.

Stroscot targets unoccupied programming enthusiasts. Feel free to improve the design WIP and maybe send a pull request.

Motivation
==========

Why another programming language, you may ask? Why does a painter paint?
When there is beauty to be found in programming languages, it is our duty to discover it and spread it far and wide. To paraphrase `XKCD <https://xkcd.com/927/>`__:

.. code-block:: RST

  How programming languages proliferate
  -------------------------------------
  SITUATION: There are 14 competing programming languages.

  Geek: 14?! Ridiculous! We need to develop one universal programming language
    that covers everyone's use cases.

  Soon: There are 15 competing programming languages.

Stroscot aims to be that universal language. The shackles of languages past are no more, and
programs are freed to be idealized to their purest form. Abstractions reach their highest level and coding is automated as far as possible.

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
----

The logo for Stroscot is inspired by the color scheme of the cover of Accelerando (the red rise of the machines), the `cot icon <https://thenounproject.com/term/cot/154357/>`__ by P Thanga Vignesh from the Noun Project, and a design I made a while back of "the infinite stack". The Paint picture I made is lost in time, but the general idea is you had a (potentially infinite) stack of reusable/composable components (the white/black blocks in the current icon) going left-to-right, and underneath it a processor (white) and various glue bits (red/blue).

The current logo is made mainly to solve the issue of finding the browser tabs with Stroscot documentation open (the default icon is unhelpful), so it is an instance of "programmer art". Interested parties can submit alternate designs and once there are a few submissions there will be a vote.

Goals
=====

The ultimate
------------

If you are wondering why a feature seems unduly powerful or complex, it is because I aim for Stroscot to be the ultimate programming language rather than something just alright. A lot of programming features overlap, so I'll pick the best and most expressive version I can find, but generally the idea is things go in rather than stay out. Anything from low-level systems programming to high-level CAS manipulations. You may point to INTERCAL's COMEFROM as something best left unimplemented, but it's not hard to implement with continuations and macros. The trickier parts are actually at the low level, interfacing memory management and calling conventions, and the value proposition there should be clear.

My theory is that, even if Stroscot fails as a language, if I implement complicated but generic algorithms for the compiler then people will refer to Stroscot just for the algorithms. I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much.

Performance
-----------

Stroscot aims for C-like performance on C-like programs, and similarly to match or exceed the performance of other styles of programming on their compilers. Beyond that, it is hard to make guarantees about the performance of any of the more expressive features. Since the algorithms used are best-in-class, Stroscot will likely give acceptable performance, but some problems are undecidable and the heuristics used may not be sufficient to prevent a combinatorial explosion; such explosions are of course bugs and patches fixing them will be accepted.

In the near term, since there is no compiler or interpreter fully implemented, performance is not measurable and hence is not a consideration. Once the interpreter can pass the tower of interpreters test, that will be the main performance criterion for it. For compilation, besides optimizing the generated code, the main performance-focused feature will be fine-grained incremental compilation to reduce compile times.

"Slow" is relative - if you can do 100x speedups then slow becomes fast and it's a qualitative difference. Features can't be gated on performance - implement first, speed up later. A lot of the time programs aren't written for speed. Most programmers can’t even measure performance correctly - compiled for debug instead of release, etc. Programmers want a convenient language however slow, and for the better programmers a way to speed up their programs when they're slow (profiling, performance-optimized code). Researchers prefer an inefficient language for which it is easy to devise optimizations and improvements. Similarly programmers don't want reliable code - they prefer convenience and don't want to learn new concepts. They will ship when their management says “ship!”

World domination
----------------

Long term, Stroscot aims to replace all the programming languages in use today. Initially this involves improving FFI support and interoperability with other languages. In particular we need to be able to parse files from other languages and use data from them with Stroscot. Next we want to fully compile other languages, so that Stroscot is the sole compiler and all of its global optimizations can be used (`zig cc <https://andrewkelley.me/post/zig-cc-powerful-drop-in-replacement-gcc-clang.html>`__ is an example of how this works). Once the implementation is stable enough for production use, focus will shift to developing automated conversion tools, so that the surface syntax can be changed to Stroscot's. And yes, this is the `E-E-E strategy <https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish>`__, but Stroscot is open source so it's all OK. No language I know of has developed decent two-way linkage - you can export specific C-style constructs back to C, but C can't use any of the more advanced features.

Standardization doesn't seem necessary, a popular language builds its own standard. But there needs to be an open-source cross-platform implementation, with a committee process for changes to build consensus and ensure stability. Another alternative is to freeze Stroscot after release and design a new best language every 3-5 years


Roadmap
=======

World domination is of course very far off, the roadmap at the moment is, in rough order:

* build system
* static analysis/optimization (includes memory management)
* finish up core IR
* pass tower of interpreters test
* cool parser with fexprs
* x86-64 compiler backend
* "One thing that I should have foreseen and prepared for is that a large number of people would make assumptions about the language based on sample code and complain without checking to see if those complaints were valid."

Principles
==========

Paradigms are vague and only express common patterns; they cannot be used to design a programming language. So instead we have principles.

* Immature poets imitate; mature poets steal; bad poets deface what they take, and good poets make it into something better, or at least something different. The good poet welds his theft into a whole of feeling which is unique, utterly different than that from which it is torn. (T. S. Eliot)
* Make the irreducible basic elements as simple and as few as possible without [surrendering] the adequate representation of a single datum of experience. (Albert Einstein)
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

Statistics
==========

* `Bus factor <https://en.wikipedia.org/wiki/Bus_factor>`__: 1
* 10k lines of code
* ~1 commit/month

Choices
=======

Documentation first
  It is tempting to just start coding - a prototype might attract contributors and let the project gain momentum. But as the principle goes, "if it isn't documented, it doesn't exist". Looking at HN submissions of programming languages, the best docs win - it's only "famous" languages that can submit a Github repo full of files. To do well we need at least a README. But I'm going with a wiki style so I can write down every last detail. And there are code scraps for the places where writing code is simpler than explaining, but none of them really work yet.

Sphinx
  GH Pages/Jekyll can't do forward/back links. Checking out various options, Sphinx is used by Clang, GHC, Futhark, etc., although not Rust or Java. And it has a lot of features like automatic TOC generation, syntax highlighting, Graphviz, Bibtex integration, ... so far it's proving its worth. It's run via a Travis CI script and the generated docs are stored in the gh-pages branch.

