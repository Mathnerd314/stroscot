Community
#########

Carbon says promoting a "healthy and vibrant community" is their overarching goal. For Stroscot, per the checklist, adoption is an outcome of the design process, not an explicit goal. It is worth tracking adoption closely to identify possible defects in the design process, but if it turns out that some strongly-held principle of Stroscot is affecting community growth, then it is likely the community growth that will have to suffer in the end. But it is not like I have deliberately chosen principles and values that will conflict with community adoption. The guidelines are vague and there is significant leeway for conforming to social pressure. And of course there is the bus-factor and other such considerations which mean that some minimal community is necessary regardless - the question is really about feel, "cozy community" vs. "huge community". And even with just me developing it, Stroscot has already gotten complex enough that communications about it have become tricky. So Stroscot should maintain a large and active community of users providing guidance and support.

This page is a collection of thoughts about communication and social expectations regarding Stroscot.

Open source
===========

Per the LICENSE file Stroscot is under an open-source license. I haven't seen many closed-source language that are really successful these days. It really seems that closed-source is a dying breed and FLOSS won.

But real "open source" goes beyond a LICENSE file: (per `Luke Plant <https://lukeplant.me.uk/blog/posts/why-im-leaving-elm/>`__)

* open development process, permanent records of decision making, decisions should be explained with reasoning
* appreciate comments or ideas from the community, benefit from other people's expertise without flatly contradicting them (although they may be wrong, don't flame them like Linus Torvalds)
* clearly documented process for contributing in CONTRIBUTING.md file, not "Old Boy's network"
* pull requests by community members should be merged or closed within a year, with good explanations for the choice (as opposed to "this issue is stale and has been closed and locked")
* deleting posts, blocking, and locking should be reserved for spam, not civil criticism
* communication style should be civil, friendly, and helpful, and not aggressive or controlling.
* leadership should not be a corrupt cabal that gives special treatment to itself. They need to think of themselves as stewards and not owners. The difficulty goes up as more people are affected by decisions and more contributions received from people.
* forks and patches are not called "hostile attacks", rather they should be encouraged

More on open source in the open source way book.

Code of Conduct
===============

This is filled in now. But it needs at least 10 people to fill in the different roles, so for now people will just have to do with less justice. Or maybe I could fill in the roles with ChatGPT.

Communication methods
=====================

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

Identity
========

Name
----

The name "Stroscot" was developed by taking the names of programming languages from `Wikipedia <https://en.wikipedia.org/wiki/List_of_programming_languages>`__ and the `Esolangs wiki <https://esolangs.org/wiki/Language_list>`__, using those as a corpus for a `Markov generator <http://max.marrone.nyc/Markov-Word-Generator/>`__, and selecting one that seemed reasonable. I still get odd looks when I tell people the name though, and nobody can spell it.

Logo
----

The logo for Stroscot is inspired by the color scheme of the cover of Accelerando by Charles Stross (the red rise of the machines), the `cot icon <https://thenounproject.com/term/cot/154357/>`__ by P Thanga Vignesh from the Noun Project, and a design I made a while back of "the infinite stack". The Paint picture I made is lost in time, but the general idea is you had a (potentially infinite) stack of reusable/composable components (the white/black blocks in the current icon) going left-to-right, and underneath it a processor (white) and various glue bits (red/blue).

Contests
--------

The current name and logo are made respectively to solve the issue of giving the project a URL and making it easier to find the browser tabs with Stroscot documentation open (the default icon is unhelpful). But they are not intended to be permanent; they are instances of "programmer art". There is an ongoing name and logo contest to select permanent branding.

For the name, first a list of around 200 names needs to be developed. The main criteria are:

* different from other existing programming language names
* pleasant in tonality and appearance
* pronounceable and spellable
* avoid the letters Y, H, K, J, and W because certain languages that use the Roman alphabet don't have them
* representative of the language in some way - abstract ideas, imagery or association, the flavor of the sound
* no existing trademarks
* no inappropriate meanings in any language

Then these will be narrowed down by a community survey, and I'll pick from like the top 5 or something. If you want to submit a name just file an issue or PR.

Similarly for the logo, interested parties will submit designs and once there's a decent amount of submissions there will be a vote and final choice. It used to be that logos were harder to come up with than names, because they required drawing skill, so 20 might have been a reasonable cutoff. But now that AI can generate logos and it's just writing a prompt and seeing what comes out, the cutoff should probably also be around 200 for the community to vote on.

Generally a logo comes in many variations:

* Icon logomark
* favicon (16/32 pixel raster)
* Horizontal logo + stylized name
* Vertical logo + stylized name
* Stylized name by itself
* Black and white variations
* Formats: source files, PNG, SVG, PDF, EPS

Maybe the contest will just be for the icon logo and someone artistic will create the other variations.

Mascot
------

There's also the need for a mascot. Go has a gopher, Python has snakes, Ocaml has a camel, Rust has a crab, Zig has two iguana variations. I'm thinking alligator, inspired by a 2023 trip to Florida. Clearly the Go mascot artist had a lot of fun with poses and mediums and backstory, and from the YT video there's a bit of history in that it was similar to an avatar of bobf developed for Plan 9. I think Stroscot's mascot designs will develop naturally once someone makes a mascot, no need to force it.

Theme
-----

The current theme is just the default RTD theme. I chose the blue/red/orange of the current logo to go OK with the RTD blue. For typography, the RTD theme uses Roboto Slab for headers, Lato for bodies, and Consolas for monospace.

With a new name and logo would naturally come a new theme, probably having no relation to the RTD theme. There are many guides on how to choose color schemes and so on that mesh well with a logo. For now, the RTD theme is generic enough, being used in countless Python projects and so on, that at least for me it evokes no particular associations other than a young project. Patching the RTD theme to customize fonts or colors would add an extra build step, which is not the end of the world, but I would rather convey the absence of a theme identity rather than the wrong theme identity. Let's just say there is an ongoing theme contest running parallel with the name and logo contests.

Brand
-----

Go made a `brand book <https://go.dev/assets/go-brand-book-v1.9.5.pdf>`__. Going through it and free associating with ChatGPT:

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

Culture
=======

Stroscot has its identity, values, and tone of voice as a project, and when speaking on behalf of Stroscot it is important to follow these. The community around Stroscot is more diverse and naturally some off-brand messages will also be introduced. On the one hand it is important to be inclusive and transparent and allow these messages in communication channels, on the other it is also important to be constructive and responsive and to point out the deviation in these messages from the culture of Stroscot.

Carbon has an overarching goal of promoting a healthy and vibrant community with an inclusive, welcoming, and pragmatic culture. They say `"culture eats strategy for breakfast" <https://techcrunch.com/2014/04/12/culture-eats-strategy-for-breakfast/>`__. That article describes a "Get Stuff Done" attitude where nobody can complain - if something is wrong, the complainer must come up with a solution and fix it. Anything can be changed and nobody is a victim.

Stroscot naturally has different goals and culture but it is still instructive to think about vision, mission, values, and purpose. It is important to "own the culture" with training, monthly communications, performance-appraisal, and role models.

* Board decisions are typically made through a consensus-building process, with a majority vote and the president having the final say in the event of a tie.
* Use collaborative tools for virtual meetings and decision tracking, like Docs, Zoom, forums, Discord, etc.
* Have regular board meetings and maintain transparent communication channels - publish meeting minutes, project updates, and financial reports regularly.
* Finances - regular audits or just publish the detailed ledger
* Channels for community feedback and suggestions.
* Surveys or open forums to gather input on significant project decisions.
* System for recognizing and rewarding outstanding contributions to the project.
* Community-voted awards or acknowledgments during project milestones.

Safe space
==========

One idea is to have a designated safe space channel, prohibiting inappropriate language or harmful language, including:

#. Behavior that is rude, disrespectful, or inconsiderate
#. Comments or jokes of a sexual nature
#. Sexually explicit or violent material
#. Language or behavior that belittles or diminishes others
#. Language that is explicitly racist or sexist
#. Using language or making jokes that denigrate or discriminate against individuals with disabilities
#. Using language or making jokes that discriminate based on Protected Attributes
#. Promoting or endorsing behavior that violates the established code of conduct
#. Personal internet or social media activities unrelated to project goals
#. Gossip or rumors about contributors or the project
#. False or harmful information (misinformation)

I can certainly make such a channel and even enforce its rules with a little 13b language model Discord bot. If it is successful, maybe the majority of beginner channels can be made safe spaces.

Culture policing
================

Something different is culture policing, ensuring communication stays on-tone in project channels. This too can be automated but it is a little more tricky because the bot should not censor speech, only fight speech with more speech. So somehow we have to get the AI to understand a concept like `Graham's hierarchy of disagreement <https://themindcollection.com/revisiting-grahams-hierarchy-of-disagreement/>`__ and make productive comments that encourage discussions to go up the hierarchy rather than down. Just calling out name-calling as name-calling is unlikely to dissuade a contributor from their path of unintelligent argument. Indeed calling their argument unintelligent is itself a form of name-calling, although when an emotionless language model says your argument is unintelligent it does carry a bit more weight. But if the bot can make level-6 arguments like "you said X is an idiot but actually X has won the Nobel prize", even if the information is hallucinated, it will still be funny enough to derail the conversation onto a more productive track. And if instead the bot starts an argument with a troll and the troll spends all their time arguing with the bot, that is in some sense a win too.
