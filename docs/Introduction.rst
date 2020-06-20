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

Goals
=====

Stroscot aims to be a "pluggable" language, where you can write syntax, type checking, etc. for a small DSL like SQL and then use it in a larger program with some embedding syntax.

::

  run_sql_statement { SELECT ... }

The idea extends further, embedding lower-level and incompatible languages like assembly and C++.

::

  result = asm { sumsq (toregister x), (toregister y) }
  my_func = load("foo.cpp").lookup("my_func")

The ultimate
------------

Eventually I would like Stroscot to be the ultimate programming language, supporting all known language constructs to some degree and providing a testbench for research into new ones. Anything from low-level systems programming to high-level CAS manipulations. You may point to INTERCAL's COMEFROM as something best left unimplemented, but it's not hard to implement with continuations. The trickier parts are actually at the low level, interfacing garbage collectors and calling conventions, and the value proposition there should be clear.

Roadmap
-------

World domination is of course very far off, the roadmap at the moment is basically "cool parser, optimal evaluation, Thorin-like IR, static typing/analysis, x86-64 / LLVM / WASM backend (pick 1), some kind of GC or memory management". But if you are wondering why a feature seems unduly powerful or complex, it is because I aim for the ultimate rather than something just alright.  A lot of programming features overlap, so I'll pick the best and most expressive version I can find, but generally the idea is things go in rather than stay out. I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much.

My theory is that, even if Stroscot fails as a language, if I implement complicated but generic algorithms in the compiler then people will refer to Stroscot just for the algorithms.

Performance
-----------

Stroscot aims for C-like performance on C-like programs, and similarly to match or exceed the performance of other styles of programming on their compilers. Beyond that, it is hard to make guarantees about the performance of any of the more expressive features. Since the algorithms used are best-in-class, Stroscot will likely give acceptable performance, but some problems are undecidable and the heuristics used may not be sufficient.

In the near term, since there is no compiler or interpreter fully implemented, performance is not measurable and hence is not a consideration. Once the interpreter can pass the tower of interpreters test, that will be the main performance criterion for it. For compilation, the main performance-focused feature will be incremental compilation with function-level hashing; additional optimizations may be made if this is insufficient.

