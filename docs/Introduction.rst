Introduction
############

Motivation
==========

Why another programming language, you may ask? Why does a painter paint?
When there is beauty to be found in programming
languages, it is our duty to discover it and spread it far and wide. To paraphrase `XKCD <https://xkcd.com/927/>`__:

::

  How programming languages proliferate
  -------------------------------------
  SITUATION: There are 14 competing programming languages.
  Geek: 14?! Ridiculous! We need to develop one universal programming language
    that covers everyone's use cases.

  Soon: There are 15 competing programming languages.

Stroscot aims to be that universal language. The shackles of languages past are no more, and
programs are freed to be idealized to their purest form.

.. _inspiring-projects:

Inspiring projects
==================

-  `Lever <https://github.com/cheery/lever/>`__
-  `GHC <https://github.com/ghc/ghc/>`__
-  `Zig <https://github.com/ziglang/zig/>`__
-  `Jai <https://github.com/BSVino/JaiPrimer/blob/4a2d14f3e1c8e82a4ba68b81d3fd7d8d438e955c/JaiPrimer.md>`__
-  `Pinafore <https://pinafore.info/>`__
-  `Macro Lambda Calculus <http://github.com/codedot/lambda>`__
-  `A Pint-sized Earley Parser <https://github.com/JoshuaGrams/pep>`__
-  `Wat <https://github.com/manuel/wat-js>`__
-  `Atomo <https://github.com/vito/atomo>`__ / `Atomy <https://github.com/vito/atomy>`__
-  `Slate <https://github.com/briantrice/slate-language>`__

Tagline
=======

The tagline for Stroscot is "an imperative programming language for modern processors". The breakdown:

Stros
  This is a vague reference to Charles Stross, author of the sci-fi book "Accelerando". In particular Stroscot aims to speed up the pace of technological development.

cot
  Similar to how "Star Trek" expresses a journey to find new worlds, the cot here expresses that Stroscot provides a comfortable, lighweight yet flexible, portable, and compact support. In particular the build system uses the command ``cot``.

imperative programming language
  This is a riff of the assertion "Haskell is the world's finest imperative programming language", first said in  the awkward squad paper :cite:`jonesTacklingAwkwardSquad2001` because "actions are first class values" in Haskell.

modern processors
  This is mostly because I don't want to have to write code generators for numerous archaic architectures. The plan for now is to only target 64-bit x86 and then later add a mode to generate LLVM bytecode.

Goals
=====

The ultimate
------------

If you are wondering why a feature seems unduly powerful or complex, it is because I aim for Stroscot to be the ultimate programming language rather than something just alright. A lot of programming features overlap, so I'll pick the best and most expressive version I can find, but generally the idea is things go in rather than stay out. Anything from low-level systems programming to high-level CAS manipulations. You may point to INTERCAL's COMEFROM as something best left unimplemented, but it's not hard to implement with continuations and macros. The trickier parts are actually at the low level, interfacing garbage collectors and calling conventions, and the value proposition there should be clear.

My theory is that, even if Stroscot fails as a language, if I implement complicated but generic algorithms for the compiler then people will refer to Stroscot just for the algorithms. I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much.

Performance
-----------

Stroscot aims for C-like performance on C-like programs, and similarly to match or exceed the performance of other styles of programming on their compilers. Beyond that, it is hard to make guarantees about the performance of any of the more expressive features. Since the algorithms used are best-in-class, Stroscot will likely give acceptable performance, but some problems are undecidable and the heuristics used may not be sufficient to prevent a combinatorial explosion; such explosions are of course bugs and patches fixing them will be accepted.

In the near term, since there is no compiler or interpreter fully implemented, performance is not measurable and hence is not a consideration. Once the interpreter can pass the tower of interpreters test, that will be the main performance criterion for it. For compilation, besides optimizing the generated code, the main performance-focused feature will be fine-grained incremental compilation to reduce compile times.

World domination
----------------

Long term, Stroscot aims to replace all the programming languages in use today. Initially this involves improving FFI support and interoperability with other languages. In particular we need to be able to parse files from other languages and use data from them with Stroscot. Next we want to fully compile other languages, so that Stroscot is the sole compiler and all of its global optimizations can be used (`zig cc <https://andrewkelley.me/post/zig-cc-powerful-drop-in-replacement-gcc-clang.html>`__ is an example of how this works). Once the implementation is stable enough for production use, focus will shift to developing automated conversion tools, so that the surface syntax can be changed to Stroscot's. And yes, this is the `E-E-E strategy <https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish>`__, but Stroscot is open source so it's all OK.

Roadmap
=======

World domination is of course very far off, the roadmap at the moment is, in rough order:

* build system
* static analysis/optimization (includes memory management)
* finish up core IR
* pass tower of interpreters test
* cool parser with fexprs
* x86-64 compiler backend

Principles
==========

* Immature poets imitate; mature poets steal; bad poets deface what they take, and good poets make it into something better, or at least something different. The good poet welds his theft into a whole of feeling which is unique, utterly different than that from which it is torn. (T. S. Eliot)
* Make the irreducible basic elements as simple and as few as possible, but don't ignore a single datum of experience. (Albert Einstein)
* Choose a random modification. Accept with a probability decreasing over time if it worsens the system and a positive probability otherwise. Repeat until the system reaches a state that is good enough for the application. (Simulated annealing)
* Never finished, never complete, but tracking the progress of technology (Lennart Poettering)
* Code can be used as data, data can be used as code (Lisp)
* Productivity is being able to do things that you were never able to do before.
