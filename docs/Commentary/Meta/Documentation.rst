Documentation
#############

`Dijkstra <https://www.cs.utexas.edu/users/EWD/transcriptions/EWD05xx/EWD514.html>`__ has found that the level of care of the use of the (English) language is one of the most revealing criteria of the quality of a project. As he says, "no project can result in a product better than the language in which the product has been discussed and carried out. For anything non-trivial, careful use of language is not a luxury, but a must. [...] It has nothing to do with grammatical pedantry or anything of that sort."

Dijkstra also says a clear separation of concerns in a helpful manner is "a (technical) conditio sine qua non" [an indispensable condition] for any complicated undertaking. Its absence/presence is one of the more telling indications for judging the expected (in)competence for the task at hand.

Steelman 1F says "The language shall be composed from features that are understood. The semantics of each feature should be sufficiently well specified and understandable that it will be possible to predict its interaction with other features." Steelman 1H says "The language shall be completely and unambiguously defined. To the extent that a formal definition assists in achieving the above goals (i.e., all of section 1), the language shall be formally defined."

13A. The language shall have a complete and unambiguous defining document. It should be possible to predict the possible actions of any syntactically correct program from the language definition. The language documentation shall include the syntax, semantics, and appropriate examples of each built-in and predefined feature. A recommended set of translation diagnostic and warning messages shall be included in the language definition.


Format
======

I chose Sphinx for several reasons. GH Pages/Jekyll can't do forward/back links. Checking out various projects, Sphinx is used by Clang, GHC, and Futhark. It has a lot of features like automatic TOC generation, syntax highlighting, Graphviz, Bibtex integration, ... so far it's proving its worth. It's run via a Github actions script and the generated docs are stored in the gh-pages branch.

Sphinx uses reStructuredText, there are some plugins for Markdown but they seemed unmaintained. Also Markdown is more ambiguous and less flexible than rST. It is a bit annoying to have to convert imported text with pandoc now and then, but it's not the end of the world.

Alternatives include Rust's self-written mdBook. But their `documentation <https://rust-lang.github.io/mdBook/format/markdown.html>`__ is itself ill-formatted, with the first line of each Markdown example indented for some random reason, which does not inspire confidence. There is also Java's javadoc, but it's not used much outside Java.

There is no standard for rST heading characters that I could find; I have settled on ``#=-~^`` as top-level, first-level, through fourth-level.

It would be good to have an automatic formatter but I looked and the only obvious one was `docstrfmt <https://github.com/LilSpazJoekp/docstrfmt>`__ which doesn't support `directives <https://github.com/LilSpazJoekp/docstrfmt/issues/45>`__. Maybe passing it through Pandoc would work.

Organization
============

The documentation is organized according to `the Diataxis framework <https://diataxis.fr/>`_, because it shows up when you google "documentation system" and I couldn't find anything better.

The four functions:

* Tutorial  ("getting started") - overview information for newcomers, learning oriented (aim for a 1-week course)
* How-to guides - specific tasks / goals, e.g. solve specific error messages
* Technical reference - describe the machinery, with as little fluff as possible
* Commentary/explanation - understanding, explain the possible alternatives and why a choice was made

Currently Stroscot is still in the design stage so the majority of content is commentary.

The categorization procedure:

* Does it describe specific actions the reader should take (1), or is it theoretical (2)?
* Is it an exploratory piece of art (A), or is it a descriptive quick-reference (B)?
* 1A: getting started
* 1B: how-to guide
* 2A: commentary
* 2B: reference

Logical quotation
=================

The `MLA style guide <https://style.mla.org/the-placement-of-a-comma-or-period-after-a-quotation/>`__ doesn't explicitly forbid it, mentioning that it's similar to British style, and it matches the logical structure (hence is called "logical quotation"). Proper nesting is important in programming and it seems strange to ignore this. And it's the `official style on Wikipedia <https://en.wikipedia.org/wiki/MOS:LQUOTE>`__.

Forbidden words
===============

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

Avoiding this terminology is easy to forget so is enforced by periodic grep's of the docs.


