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
programs are freed to be written in their most pure and ideal form. Abstractions reach their highest level and coding is automated as far as possible.

To quote `Kennedy <https://en.wikipedia.org/wiki/We_choose_to_go_to_the_Moon>`__, "Man, in his quest for knowledge and progress, is determined and cannot be deterred. [Programming language development] will go ahead, whether we join in it or not, and it is one of the great adventures of all time. [...] We set sail on this new sea because there is new knowledge to be gained, and new rights to be won, and they must be won and used for the progress of all people. [...] But why, some say, [a programming language]? Why choose this as a goal? And they may well ask, why climb the highest mountain? Why, 35 years ago, fly the Atlantic? Why does Rice play Texas?  [...] We choose to [develop a universal programming language], not because it is easy, but because it is hard, because that goal will serve to organize and measure the best of our energies and skills, because that challenge is one that we are willing to accept, one we are unwilling to postpone, and one which we intend to win. [...] I'm the one who is doing all the work, so we just want you to stay cool for a minute."

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

Branding
========

The name "Stroscot" was developed by taking the names of programming languages from `Wikipedia <https://en.wikipedia.org/wiki/List_of_programming_languages>`__ and the `Esolangs wiki <https://esolangs.org/wiki/Language_list>`__, using those as a corpus for a `Markov generator <http://max.marrone.nyc/Markov-Word-Generator/>`__, and selecting one that seemed reasonable. I still get odd looks when I tell people the name though, and nobody can spell it.

The logo for Stroscot is inspired by the color scheme of the cover of Accelerando by Charles Stross (the red rise of the machines), the `cot icon <https://thenounproject.com/term/cot/154357/>`__ by P Thanga Vignesh from the Noun Project, and a design I made a while back of "the infinite stack". The Paint picture I made is lost in time, but the general idea is you had a (potentially infinite) stack of reusable/composable components (the white/black blocks in the current icon) going left-to-right, and underneath it a processor (white) and various glue bits (red/blue).

The current name and logo are made respectively to solve the issue of giving the project a URL and making it easier to find the browser tabs with Stroscot documentation open (the default icon is unhelpful). But they are not intended to be permanent; they are instances of "programmer art". Once the language gains some popularity, there will be a name and logo contest to select permanent branding.

For the name, first a list of around 200 names needs to be developed. The main criteria are:

* different from other existing programming language names
* pleasant in tonality and appearance
* pronounceable and spellable
* avoid the letters Y, H, K, J, and W because certain languages that use the Roman alphabet don't have them
* representative of the language in some way - abstract ideas, imagery or association, the flavor of the sound
* no existing trademarks
* no inappropriate meanings in any language

Then these will be narrowed down by a community survey, and I'll pick from like the top 5 or something.

Similarly for the logo, interested parties will submit designs and once there's a decent amount of submissions there will be a vote and final choice. It used to be that logos were harder to come up with than names, because they required drawing skill, so 20 might have been a reasonable cutoff. But now that AI can generate logos and it's just writing a prompt and seeing what comes out, the cutoff should probably also be around 200.

Generally a logo comes in many variations:

* Icon logomark
* favicon (16/32 pixel raster)
* Horizontal logo + stylized name
* Vertical logo + stylized name
* Stylized name by itself
* Black and white variations
* Formats: source files, PNG, SVG, PDF, EPS

Maybe the contest will just be for the icon logo and someone artistic will create the other variations.

There's also the need for a mascot. Go has a gopher, Python has snakes, Ocaml has a camel, Rust has a crab, Zig has two iguana variations. I'm thinking alligator, inspired by a 2023 trip to Florida.

Go went even further and made a `brand book <https://go.dev/assets/go-brand-book-v1.9.5.pdf>`__. I'll just throw down some free association:

* Stroscot is an open source programming language that enables the production of complete, optimal, and verified software in non-zero quantities
* Stroscot enables the development of massive systems with minimal errors.
* Stroscot has reasonable build times, great tools, and is suitable for many use cases.
* Stroscot can optimize for build time, power consumption, or any metric you choose.
* Stroscot combines the expressiveness of a dynamic language with the tooling of a static language.
* Tenets / core values:

  * Systematic - Eliminate guesswork
  * Concise - Clear and direct
  * Optimal - Achieve your best results
  * Leading - Stay ahead of the curve
  * Intuitive - Natural and effortless
  * Seamless - Integrate with ease

* Tone of voice: Stroscot values collaborative relationships. Stroscot's communications should be constructive, transparent, inclusive, responsive, perceptive, and dedicated. They should not be condemning, secretive, elitist, unhelpful, ignorant, or defeatist.
* Audience: Stroscot aims to be all things to all people. We can list some of the most common groups: professionals, hobbyists, students, academics, sysadmins, entrepreneurs. We can also list some of the larger uncommon groups: females, non-technical people, older adults, non-native speakers, people with learning disabilities, people with no internet access. Just keep all of them in mind and introduce options to specifically support a group when necessary.
* Messages: The language for you.
* Logo use: seems like standard boilerplate.
* Color: I like the blue/red/orange of the current logo, but with a new logo comes a new color scheme.
* Typography: The RTD theme uses Roboto Slab for headers, Lato for bodies, and Consolas for monospace. I guess it could be changed but the fonts seem fine, and it requires patching the theme which would add an extra build step.
* Mascot: Clearly the Go mascot artist had a lot of fun with poses and mediums and backstory, and from the YT video there's a bit of history in that it was similar to an avatar of bobf developed for Plan 9. I think Stroscot's mascot designs will develop naturally once someone makes a mascot, no need to force it.

Choices
=======

Documentation first
-------------------

It is tempting to just start coding - a prototype might attract contributors and let the project gain momentum. But as the principle goes, "if it isn't documented, it doesn't exist". Looking at HN submissions of programming languages, the best docs win - it's only "famous" languages that can submit a Github repo full of files but without a README and still get discussion. To do well, we definitely need at least a README. But I'm going with a wiki style, so I can write down every last relevant detail that affected how the language was put together or how it was designed. And there are code snippets for the places where writing code is clearer than explaining in English. Several successful languages such as Simula, REXX, and Ada have been designed documentation-first.

Erlang is a contrary example - per `Robert Virding <https://youtu.be/f3rP3JRq7Mw?t=1083>`__, until about 2004, there was no documentation of the rationale. But finally, Armstrong wrote a history of Erlang, and Virding got so tired of repeating himself in his consulting that he wrote a `paper <https://drive.google.com/file/d/1zKsOgwZJ_YZ1bY3b3gNRjAxpn6VneR8b/view>`__ about the design of Erlang. For example, all the error handling primitives are asynchronous, because the design was that all process communication was asynchronous. Per Virding, it's worth writing down the reasoning even if it seems self-evident. When you're making changes to a language, this rationale is key to knowing what is easily changed vs. what is a "load-bearing" design constraint. Otherwise, people will not see the line of thinking, push through naive changes, and break key guarantees of the language. Also, documenting the rationale makes it easier for people to learn how to use the language in the way it was intended to be used.

Sphinx
------

GH Pages/Jekyll can't do forward/back links. Checking out various projects, Sphinx is used by Clang, GHC, and Futhark. It has a lot of features like automatic TOC generation, syntax highlighting, Graphviz, Bibtex integration, ... so far it's proving its worth. It's run via a Github actions script and the generated docs are stored in the gh-pages branch.

Alternatives include Rust's self-written mdBook. But their `documentation <https://rust-lang.github.io/mdBook/format/markdown.html>`__ is itself ill-formatted, with the first line of each Markdown example indented for some random reason, which does not inspire confidence. There is also Java's javadoc, but it's not used much outside Java.

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

Currently Stroscot is still in the design stage so the majority of content is commentary.

Quotes before commas
--------------------

The `MLA style guide <https://style.mla.org/the-placement-of-a-comma-or-period-after-a-quotation/>`__ doesn't explicitly forbid it, mentioning that it's similar to British style, and it matches the logical structure (hence is called "logical quotation"). Proper nesting is important in programming and it seems strange to ignore this. And it's the `official style on Wikipedia <https://en.wikipedia.org/wiki/MOS:LQUOTE>`__.

Forbidden words
---------------

A fair amount of terminology seems to be meaningless or ambiguous. So don't use it:

* types - 5 definitions, an ambiguous term. Use "set" as it unambiguously refers to Stroscot's usage of a type as a value space (set of values). Discussed solely in the "Types" page.
* dynamic/static - As `Harper <https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/>`__ points out, these are marketing terms. Again discussed solely in the "Types" page. Some translations of related terms to avoid grep occurrences:

  * static lifetime - program lifetime, bound when program starts and freed by OS when program terminates
  * static method - utility method, not bound to any object instance
  * static/dynamic library - cached compiled code, compiled with/without position independent code. There are also format differences, use shared library ``.so`` / object archive ``.a`` for clarity.
  * static linkage - self-contained image generation
  * static linking - direct binding, resolving jumps to fixed memory addresses
  * static imports - scoped import, import members of modules
  * dynamic linking - shared library linking
  * dynamic loading - loading during execution

* strongly typed - `8 definitions <https://perl.plover.com/yak/12views/samples/slide045.html>`__, all different. It's the semantic equivalent of "amazing", i.e. "My language is strongly typed" == "My language is amazing". Again discussed solely in the "Types" page.
* pure - this is just an umbrella term, we can use more specific terminology

  * pure function - a mathematical function, a function that always produces the same output for the same input and has no implicit side effects.
  * pure data, pure state, pure value - immutable data/state/value, cannot be modified and does not depend on any external factors
  * pure expression - Side-effect-free expression, evaluates to a value without any side effects. Also, deterministic expression, for an expression that has only one value. Instead of "impure expression" refer to an expression that has no value (unevaluatable expression) or multiple values (ambiguous expression) or executes side effects (imperative expression). Actually with the TRS formalism I use every expression is evaluatable so we don't worry about unevaluatable expressions.
  * pure programming language - a language that models the program as a mathematical function and enforces a clear distinction between immutable values and mutable or side-effectful expressions. Kind of a broad concept so doesn't need a term.

* undecidable - people use this word to imply that it's unimplementable, when there are working solvers like the ones in `termCOMP <https://termination-portal.org/wiki/Termination_Competition>`__ that solve many useful cases. Godel's theorem only means that pathological examples exist for each specific implementation, which is true even with Hindley-Milner (linear for real-world programs, worst-case exponential). Prefer "complexity at least :math:`\Sigma^0_1`", where :math:`\Sigma^0_1` is in the `arithmetic hierarchy <https://en.wikipedia.org/wiki/Arithmetical_hierarchy>`__, or a more precise class if known. Note that decidable problems / computable sets are in :math:`\Delta_{1}^{0} \subsetneq \Sigma^0_1`.
* primitive - as per `Wikipedia <https://en.wikipedia.org/wiki/Primitive_data_type>`__, primitive is ambiguous and can mean "the base cases of an inductive definition", in which case use "base", or "whatever is provided by a particular processor or compiler", in which case use "built-in". Note that built-in does not mean base, e.g. integers can be defined in terms of booleans hence are not base cases.
* :math:`\subset` - per `Wikipedia <https://en.wikipedia.org/wiki/Subset#%E2%8A%82_and_%E2%8A%83_symbols>`__ this is ambiguous, use :math:`\subsetneq` and :math:`\subseteq`
* abomination - a fun word, but basically meaningless in a programming context where one person's "abomination" is another person's "cool hack"
* "etc" and "..." - they're just too imprecise. Usually if it's a list, these can just be omitted. If there is an intentional omission it can be replaced with an angle bracket construction like ``<more numbers>``, or the ambiguity erased with set-builder notation.
* homoiconic - per `Michael Arntzenius <https://futureofcoding.org/episodes/040>`__ it just means the language has a data structure that represents an AST. So as soon as you talk about an "AST value" you're talking about a homoiconic language. The Lisp folks make a big deal out of it, but even Python has an `AST node datatype <https://docs.python.org/3/library/ast.html>`__ in the standard library. Of course people don't seem to think of Python as homoiconic. The Julia folks apparently `stopped <https://stackoverflow.com/questions/31733766/in-what-sense-are-languages-like-elixir-and-julia-homoiconic>`__ calling themselves "homoiconic" because they were getting pushback. According to ChatGPT and Stefan Karpinski of Julia, a true homoiconic language is one where the syntax for writing code is the same as the syntax for writing the data structure representing the AST of the code. This pretty much means Lisp. Personally I think having a low-level Lisp representation like Stroscot does is sufficient to qualify, and arguably a good quasiquoting implementation would work too, but it's easier to just avoid the term and the endless debates.
* African American - genetically this term is `all over the map <https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-2680-1/figures/5>`__. Prefer "American of African culture". And try to avoid any personal labels in general, we're here to discuss code not politics.

Avoiding this terminology is easy to forget so is enforced by periodic grep's of the code.

Open source
-----------

Per the LICENSE file Stroscot is under an open-source license. I haven't seen many closed-source language that are really successful these days. It really seems that closed-source is a dying breed and FLOSS won.

But real "open source" goes beyond a LICENSE file: (per `Luke Plant <https://lukeplant.me.uk/blog/posts/why-im-leaving-elm/>`__)

* open development process, permanent records of decision making, decisions should be explained with reasoning
* appreciate comments or ideas from the community, benefit from other people's expertise without flatly contradicting them (although they may be wrong, don't flame them like Linus Torvalds)
* clearly documented process for contributing in CONTRIBUTING.md file, not "Old Boy's network"
* pull requests by community members should be merged or closed within a year
* deleting posts, blocking, and locking should be reserved for spam, not civil criticism
* communication style should be civil, friendly, and helpful, and not aggressive or controlling.
* leadership should not be a corrupt cabal that gives special treatment to itself. They need to think of themselves as stewards and not owners. The difficulty goes up as more people are affected by decisions and more contributions received from people.
* forks and patches are not called "hostile attacks"

Communication methods
---------------------

Stroscot's documentation first approach should help a lot with open development. As far as information, the main avenue for Stroscot is the Git repo. This has the documentation and the code all-in-one. Secondary sources are:

* real-time chat, for quick questions and discussion. Discord suffices for now (0 people anyway). Alternatives are Gitter, Element, and Matrix which are somewhat more open-source friendly.
* issues, for anything more important. Github issues seems fine, even Swift is using it. If open-source is a concern then `migrating to Gitlab <https://docs.gitlab.com/ee/user/project/import/github.html>`__ is possible.
* in the future, a forum for long-form discussions, where the problem needs more consideration than just the random sample in chat but it's not really an issue with the project. Github discussions is a possibility but Discourse is the standard. There are `free instances <https://free.discourse.group/>`__ for open-source projects, but the project first needs 10+ contributors. A Discourse would not replace issue tracking; anything relevant to language/standard library development should have an issue filed.

Issue workflow
--------------

As far as the "ping bot" that closes issues if they are not active, on first impression it seems like a good idea since if there is nobody around to discuss an issue with, then making progress on that issue is hard. So a basic "do you still care about this" if nobody has looked at it. Arguably though, a bug reporting process where a report is only looked at by someone with commit access months or years after the initial report is quite broken.

 With more than a few comments/participants, the bot should request a little discussion summary. Something like:

* Goal: Summary of what conditions need to be satisfied to close the issue
* Deliverable: What can be delivered in a few weeks to further the progress of this issue?
* Motivation: What advantages does this goal have?
* Risks: What concerns have been raised about this goal?
* Blockers: What resources or leadership decisions are needed, besides someone implementing it?

The summary doesn't need to be long, it can just link to the relevant comments. If the summary is inaccurate then someone who cares will correct it. And of course if the ping bot activates multiple times but nobody has worked on the issue then "The previous summary is accurate" is fine as the summary. There should be an exponential backoff on pings if the issue is still active but nothing has not changed since the last ping.

Releases and deadlines
----------------------

Software development is notoriously unpredictable, missing deadlines left and right. There is also the fact that Stroscot is an open-source project, with no funding. I like the `SuperTux FAQ <https://github.com/SuperTux/supertux/wiki/SuperTux_FAQ>`__ answer - "When will [it] be released? This is by far the most frequently asked question of all, and the answer is simple: When it's done. Honestly, we don't have a release date yet. Just like many of you, we'd like to see [Stroscot] finished and released to the public as soon as possible, but since we all are busy with other, probably less interesting things, the amount of time we are able to put into this is limited. Please be patient." I'm not in any hurry and I think getting things right is more important that rushing for time. I would also say that Stroscot, like SuperTux and most software projects, is already released in a "continuous release" sense - the docs build, there's a website, and at any time you can checkout and build the mainline and play around with it.

But, this would be categorized as a "nightly" or unstable release. What about declaring an alpha/beta/stable release? Stroscot is mostly on paper at the moment. In the future, there may be questions like "Why isn't this fully out yet? It feels finished and it works great. Just get it out there.", and that's where the objective criteria come in. I would say, the alpha state is a self-hosting language that's mostly usable, maybe with a uniform Lisp-like syntax, an incomplete standard library, and so on. The beta state is when the final syntax has been pretty much decided on. The stable release is when the standard library has been fleshed out and reached some level of completeness.
