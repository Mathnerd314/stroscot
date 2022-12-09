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
  * dynamic linking - shared library linking
  * dynamic loading - run-time library loading

* static - similar to dynamic, too many overloaded meanings to be usable.

  * statically typed - every language
  * static lifetime - program lifetime, bound when program starts and freed by OS when program terminates
  * static method - utility method, not bound to any object instance
  * static linkage - internal linkage, symbol only available in translation unit
  * static imports - scoped import, import members of modules
  * static library - precompiled file archive
  * static linking - compile time binding, resolving memory addresses at compile time

* pure - prefer the proposition that all expressions have a unique value. So instead of "impure expression" refer to an expression that has no value or multiple values.
* strongly typed - `8 definitions <https://perl.plover.com/yak/12views/samples/slide045.html>`__, all different. It's the semantic equivalent of "amazing", i.e. "My language is strongly typed" == "My language is amazing".
* undecidable - people use this word to imply that it's unimplementable, when there are working solvers like the ones in `termCOMP <https://termination-portal.org/wiki/Termination_Competition>`__ that solve many useful cases. Godel's theorem only means that pathological examples exist for each specific implementation, which is true even with Hindley-Milner (linear for real-world programs, worst-case exponential). Prefer "complexity at least :math:`\Sigma^0_1`", where :math:`\Sigma^0_1` is in the `arithmetic hierarchy <https://en.wikipedia.org/wiki/Arithmetical_hierarchy>`__, or a more precise class if known. Note that decidable problems / computable sets are in :math:`\Delta_{1}^{0} \subsetneq \Sigma^0_1`.
* primitive - as per `Wikipedia <https://en.wikipedia.org/wiki/Primitive_data_type>`__, primitive is ambiguous and can mean "the base cases of an inductive definition", in which case use "base", or "whatever is provided by a particular processor or compiler", in which case use "built-in". Note that built-in does not mean base, e.g. integers can be defined in terms of booleans hence are not base cases.
* :math:`\subset` - per `Wikipedia <https://en.wikipedia.org/wiki/Subset#%E2%8A%82_and_%E2%8A%83_symbols>`__ this is ambiguous, use :math:`\subsetneq` and :math:`\subseteq`
* abomination - a fun word, but basically meaningless

Avoiding this terminology is easy to forget so is enforced by periodic grep's of the code.

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
