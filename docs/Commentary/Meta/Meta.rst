Meta
####

Motivation
==========

Why another programming language, you may ask?  Why does a painter paint? To paraphrase Chapter 1 of The Mythical Man Month, there is the joy of: [#tractable]_

* designing and making complex and intricate things
* making things that create real results and are useful to other people
* constantly learning due to the nonrepeating nature of the task

It is the duty of a programming language to spread this joy far and wide.

To quote `Kennedy <https://en.wikipedia.org/wiki/We_choose_to_go_to_the_Moon>`__, "Man, in his quest for knowledge and progress, is determined and cannot be deterred. [Programming language development] will go ahead, whether we join in it or not, and it is one of the great adventures of all time. [...] We set sail on this new sea because there is new knowledge to be gained, and new rights to be won, and they must be won and used for the progress of all people. [...] But why, some say, [a programming language]? Why choose this as a goal? And they may well ask, why climb the highest mountain? Why, 35 years ago, fly the Atlantic? Why does Rice play Texas?  [...] We choose to [develop a universal programming language], not because it is easy, but because it is hard, because that goal will serve to organize and measure the best of our energies and skills, because that challenge is one that we are willing to accept, one we are unwilling to postpone, and one which we intend to win. [...] I'm the one who is doing all the work, so we just want you to stay cool for a minute."

Another line of motivation comes from `XKCD <https://xkcd.com/927/>`__ (paraphrased):

.. code-block:: RST

  How programming languages proliferate
  -------------------------------------
  SITUATION: There are 14 competing programming languages.

  Geek: 14?! Ridiculous! We need to develop one universal programming language
    that covers everyone's use cases.

  Soon: There are 15 competing programming languages.

Stroscot aims to be that universal language. The shackles of languages past are no more, and
programs are freed to be written in their most pure and ideal form. Abstractions reach their highest level and coding is automated as far as possible. Probably the language with the most similar goals is Ada - the DoD looked around and they were spending tons of money porting from one language to another. It is much more economical to have one language. Ada has since fallen out of fashion, failing to adapt to new computer science principles and research, but many people acknowledge that Ada was one of the best programming languages of the time. :cite:`sammetWhyAdaNot1986`

.. [#tractable] Notably omitted here is Brooks's assertion that programming is a "tractable medium". It is not tractable. Programming is hard. :cite:`beckerWhatDoesSaying2021` questions this, but IMO fails quite badly - although incomplete, all the evidence available shows that programming is hard. The only positive contribution of the article is a reminder to focus on computer education and usability.

Tagline
=======

The tagline for Stroscot is "an imperative programming language for modern processors". The breakdown:

Stros
  This is a vague reference to Charles Stross, author of the sci-fi book "Accelerando". In particular Stroscot aims to speed up the pace of technological development.

cot
  Similar to how the "trek" in "Star Trek" expresses a journey to find new worlds, the "cot" here expresses that Stroscot provides comfortable support while still being flexible, lightweight, portable, and compact.

imperative programming language
  This is a riff of the assertion "Haskell is the world's finest imperative programming language", first said in  the awkward squad paper :cite:`jonesTacklingAwkwardSquad2001` because "actions are first class values" in Haskell.

modern processors
  This is mostly because I don't want to have to write code generators for numerous archaic architectures. The plan for now is to only target 64-bit x86 / ARM and then later add a mode to generate LLVM IR or C.

Executive summary
=================

..
  An executive summary is "half a slide using large print" (128 x's) and gets across how people should use the language.

Stroscot targets unoccupied programming enthusiasts. Feel free to improve the design WIP and maybe send a pull request.

Statistics
==========

* `First commit <https://github.com/Mathnerd314/stroscot/tree/a5264d6697f3e4a4034c4acc87c2a2022070a2bc>`__: December 2018
* `Bus factor <https://en.wikipedia.org/wiki/Bus_factor>`__: 1
* |activity|
* |loc| (includes documentation)

.. |activity| image:: https://img.shields.io/github/commit-activity/m/Mathnerd314/stroscot
.. |loc| image:: https://img.shields.io/tokei/lines/github/Mathnerd314/stroscot

Roadmap
=======

It is tempting to just start coding - a prototype might attract contributors and let the project gain momentum. But as the principle goes, "if it isn't documented, it doesn't exist". Looking at HN submissions of programming languages, the best docs win - it's only "famous" languages that can submit a Github repo full of files but without a README and still get discussion. To do well, we definitely need at least a README. But I'm going with a wiki style, so I can write down every last relevant detail that affected how the language was put together or how it was designed. And there are code snippets for the places where writing code is clearer than explaining in English. Several successful languages such as Simula, REXX, and Ada have been designed documentation-first.

Erlang is a contrary example - per `Robert Virding <https://youtu.be/f3rP3JRq7Mw?t=1083>`__, until about 2004, there was no documentation of the rationale. But finally, Armstrong wrote a history of Erlang, and Virding got so tired of repeating himself in his consulting that he wrote a `paper <https://drive.google.com/file/d/1zKsOgwZJ_YZ1bY3b3gNRjAxpn6VneR8b/view>`__ about the design of Erlang. For example, all the error handling primitives are asynchronous, because the design was that all process communication was asynchronous. Per Virding, it's worth writing down the reasoning even if it seems self-evident. When you're making changes to a language, this rationale is key to knowing what is easily changed vs. what is a "load-bearing" design constraint. Otherwise, people will not see the line of thinking, push through naive changes, and break key guarantees of the language. Also, documenting the rationale makes it easier for people to learn how to use the language in the way it was intended to be used.

The roadmap at the moment is, in vaguely the expected order of finishing:

* build system
* static analysis/optimization (includes memory management)
* finish up core IR
* pass tower of interpreters test
* cool parser with fexprs
* x86-64 compiler backend
* "lots of people make assumptions about the language based on sample code and complain without checking to see if those complaints were valid."
* World domination

Releases and deadlines
----------------------

Software development is notoriously unpredictable, missing deadlines left and right. There is also the fact that Stroscot is an open-source project, with no funding. I like the `SuperTux FAQ <https://github.com/SuperTux/supertux/wiki/SuperTux_FAQ>`__ answer - "When will [it] be released? This is by far the most frequently asked question of all, and the answer is simple: When it's done. Honestly, we don't have a release date yet. Just like many of you, we'd like to see [Stroscot] finished and released to the public as soon as possible, but since we all are busy with other, probably less interesting things, the amount of time we are able to put into this is limited. Please be patient." I'm not in any hurry and I think getting things right is more important that rushing for time. I would also say that Stroscot, like SuperTux and most software projects, is already released in a "continuous release" sense - the docs build, there's a website, and at any time you can checkout and build the mainline and play around with it.

But, this would be categorized as a "nightly" or unstable release. What about declaring an alpha/beta/stable release? Stroscot is mostly on paper at the moment. In the future, there may be questions like "Why isn't this fully out yet? It feels finished and it works great. Just get it out there.", and that's where the objective criteria come in. I would say, the alpha state is a self-hosting language that's mostly usable, maybe with a uniform Lisp-like syntax, an incomplete standard library, and so on. The beta state is when the final syntax of the core language has been pretty much decided on. The stable release is when the standard library has been fleshed out and reached some level of completeness.

Criticisms
==========

These criticisms from the `programming language checklist <https://www.mcmillen.dev/language_checklist.html>`__ seem valid.

* Stroscot lacks reflection.
* Stroscot relies on an optimization which has never been shown possible
* Stroscot requires the compiler to be present at runtime
* Stroscot requires the language runtime to be present at compile-time
* Dangerous behavior is only a warning
