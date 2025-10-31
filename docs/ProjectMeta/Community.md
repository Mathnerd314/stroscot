# Community

Carbon says promoting a "healthy and vibrant community" is their overarching goal. For Stroscot, per the checklist, adoption is an outcome of the design process, not an explicit goal. It is worth tracking adoption closely to identify possible defects in the design process, but if it turns out that some strongly-held principle of Stroscot is affecting community growth, then it is likely the community growth that will have to suffer in the end. But it is not like I have deliberately chosen principles and values that will conflict with community adoption. The guidelines are vague and there is significant leeway for conforming to social pressure. And of course there is the bus-factor and other such considerations which mean that some minimal community is necessary regardless - the question is really about feel, "cozy community" vs. "huge community". And even with just me developing it, Stroscot has already gotten complex enough that communications about it have become tricky. So Stroscot should maintain a large and active community of users providing guidance and support.

This page is a collection of thoughts about communication and social expectations regarding Stroscot.

## Open source

Per the LICENSE file Stroscot is under an open-source license. I haven't seen many closed-source language that are really successful these days. It really seems that closed-source is a dying breed and FLOSS won.

But real "open source" goes beyond a LICENSE file: (per [Luke Plant](https://lukeplant.me.uk/blog/posts/why-im-leaving-elm/))

- open development process, permanent records of decision making, decisions should be explained with reasoning
- appreciate comments or ideas from the community, benefit from other people's expertise without flatly contradicting them (although they may be wrong, don't flame them like Linus Torvalds)
- clearly documented process for contributing in CONTRIBUTING.md file, not "Old Boy's network"
- pull requests by community members should be merged or closed within a year, with good explanations for the choice (as opposed to "this issue is stale and has been closed and locked")
- deleting posts, blocking, and locking should be reserved for spam, not civil criticism
- communication style should be civil, friendly, and helpful, and not aggressive or controlling.
- leadership should not be a corrupt cabal that gives special treatment to itself. They need to think of themselves as stewards and not owners. The difficulty goes up as more people are affected by decisions and more contributions received from people.
- forks and patches are not called "hostile attacks", rather they should be encouraged

More on open source in the open source way book.

## Code of Conduct

This is filled in now. But it needs at least 10 people to fill in the different roles, so for now people will just have to do with less justice. Or maybe I could fill in the roles with ChatGPT.

## Communication methods

Stroscot's documentation first approach should help a lot with open development. As far as information, the main avenue for Stroscot is the Git repo. This has the documentation and the code all-in-one. Secondary sources are:

- real-time chat, for quick questions and discussion. Discord suffices for now (0 people anyway). Alternatives are Gitter, Element, and Matrix which are somewhat more open-source friendly.
- issues, for anything more important. Github issues seems fine, even Swift is using it. If open-source is a concern then [migrating to Gitlab](https://docs.gitlab.com/ee/user/project/import/github.html) is possible.
- in the future, a forum for long-form discussions, where the problem needs more consideration than just the random sample in chat but it's not really an issue with the project. Github discussions is a possibility but Discourse is the standard. There are [free instances](https://free.discourse.group/) for open-source projects, but the project first needs 10+ contributors. A Discourse would not replace issue tracking; anything relevant to language/standard library development should have an issue filed.

### Issue workflow

As far as the "ping bot" that closes issues if they are not active, on first impression it seems like a good idea since if there is nobody around to discuss an issue with, then making progress on that issue is hard. So a basic "do you still care about this" if nobody has looked at it. Arguably though, a bug reporting process where a report is only looked at by someone with commit access months or years after the initial report is quite broken.

With more than a few comments/participants, the bot should request a little discussion summary. Something like:

- Goal: Summary of what conditions need to be satisfied to close the issue
- Deliverable: What can be delivered in a few weeks to further the progress of this issue?
- Motivation: What advantages does this goal have?
- Risks: What concerns have been raised about this goal?
- Blockers: What resources or leadership decisions are needed, besides someone implementing it?

The summary doesn't need to be long, it can just link to the relevant comments. If the summary is inaccurate then someone who cares will correct it. And of course if the ping bot activates multiple times but nobody has worked on the issue then "The previous summary is accurate" is fine as the summary. There should be an exponential backoff on pings if the issue is still active but nothing has not changed since the last ping.

## Identity

### Name

The name "Stroscot" was developed by taking the names of programming languages from [Wikipedia](https://en.wikipedia.org/wiki/List_of_programming_languages) and the [Esolangs wiki](https://esolangs.org/wiki/Language_list), using those as a corpus for a [Markov generator](http://max.marrone.nyc/Markov-Word-Generator/), and selecting one that seemed reasonable. I still get odd looks when I tell people the name though, and nobody can spell it.

### Logo

The logo for Stroscot is inspired by the color scheme of the cover of Accelerando by Charles Stross (the red rise of the machines), the [cot icon](https://thenounproject.com/term/cot/154357/) by P Thanga Vignesh from the Noun Project, and a design I made a while back of "the infinite stack". The Paint picture I made is lost in time, but the general idea is you had a (potentially infinite) stack of reusable/composable components (the white/black blocks in the current icon) going left-to-right, and underneath it a processor (white) and various glue bits (red/blue).

### Contests

The current name and logo are made respectively to solve the issue of giving the project a URL and making it easier to find the browser tabs with Stroscot documentation open (the default icon is unhelpful). But they are not intended to be permanent; they are instances of "programmer art". There is an ongoing name and logo contest to select permanent branding.

For the name, first a list of around 200 names needs to be developed. The main criteria are:

- different from other existing programming language names
- pleasant in tonality and appearance
- pronounceable and spellable
- avoid the letters Y, H, K, J, and W because certain languages that use the Roman alphabet don't have them
- representative of the language in some way - abstract ideas, imagery or association, the flavor of the sound
- no existing trademarks
- no inappropriate meanings in any language

Then these will be narrowed down by a community survey, and I'll pick from like the top 5 or something. If you want to submit a name just file an issue or PR.

Similarly for the logo, interested parties will submit designs and once there's a decent amount of submissions there will be a vote and final choice. It used to be that logos were harder to come up with than names, because they required drawing skill, so 20 might have been a reasonable cutoff. But now that AI can generate logos and it's just writing a prompt and seeing what comes out, the cutoff should probably also be around 200 for the community to vote on.

Generally a logo comes in many variations:

- Icon logomark
- favicon (16/32 pixel raster)
- Horizontal logo + stylized name
- Vertical logo + stylized name
- Stylized name by itself
- Black and white variations
- Formats: source files, PNG, SVG, PDF, EPS

Maybe the contest will just be for the icon logo and someone artistic will create the other variations.

### Mascot

There's also the need for a mascot. Go has a gopher, Python has snakes, Ocaml has a camel, Rust has a crab, Zig has two iguana variations. I'm thinking alligator, inspired by a 2023 trip to Florida. Clearly the Go mascot artist had a lot of fun with poses and mediums and backstory, and from the YT video there's a bit of history in that it was similar to an avatar of bobf developed for Plan 9. I think Stroscot's mascot designs will develop naturally once someone makes a mascot, no need to force it.

### Theme

The current theme is just the default RTD theme. I chose the blue/red/orange of the current logo to go OK with the RTD blue. For typography, the RTD theme uses Roboto Slab for headers, Lato for bodies, and Consolas for monospace.

With a new name and logo would naturally come a new theme, probably having no relation to the RTD theme. There are many guides on how to choose color schemes that mesh well with a logo. For now, the RTD theme is generic enough, being used in countless Python projects, that at least for me it evokes no particular associations other than a young project. Patching the RTD theme to customize fonts or colors would add an extra build step, which is not the end of the world, but I would rather convey the absence of a theme identity rather than the wrong theme identity. Let's just say there is an ongoing theme contest running parallel with the name and logo contests.

### Brand

Go made a [brand book](https://go.dev/assets/go-brand-book-v1.9.5.pdf). Going through it and free associating with ChatGPT:

- Stroscot is an open source programming language that enables the production of complete, optimal, and verified software in non-zero quantities

- Stroscot enables the development of massive systems with minimal errors.

- Stroscot has reasonable build times, great tools, and is suitable for many use cases.

- Stroscot can optimize for build time, power consumption, or any metric you choose.

- Stroscot combines the expressiveness of a dynamic language with the tooling of a static language.

- Tenets / core values:

  - Systematic - Eliminate guesswork
  - Concise - Clear and direct
  - Optimal - Achieve your best results
  - Leading - Stay ahead of the curve
  - Intuitive - Natural and effortless
  - Seamless - Integrate with ease

- Tone of voice: Stroscot values open, collaborative relationships. Stroscot's communications should be constructive, transparent, inclusive, responsive, perceptive, and dedicated. They should not be condemning, secretive, elitist, unhelpful, ignorant, or defeatist.

- Audience: Stroscot aims to be all things to all people. We can list some of the most common groups: professionals, hobbyists, students, academics, sysadmins, entrepreneurs. We can also list some of the larger uncommon groups: females, non-technical people, older adults, non-native speakers, people with learning disabilities, people with no internet access. Just keep all of them in mind and introduce options to specifically support a group when necessary.

- Messages: The language for you. There are also the [Droid campaigns](https://www.youtube.com/watch?v=EVro7tBpOJU).

Drop commercial: Several stealth jets drop their payload over rural areas. People stare in amazement as they stream through the air. The capsules crash down into the desert, the forest, the ocean. The people approach the alien-appearing capsules and one is shown opening. "What the - What in the world is that?" one fellow asks. A Droid phone is revealed. "Drop date: 12-24-2009."

Fight commercial: Extended action sequence of a woman fighting with several robots in sequence. As the woman fights, she rips parts off the robots. Finally she defeats the last robot and extracts its "4G LTE" power core. Cut to her inserting it as the last component into a podium. The podium sinks out of view. The screen goes black and resumes with an abstract Matrix-style animation of a world constructed of symbols intermixed with phone specifications. "Made from machines to rule all machines. Forget what you thought you knew. Because you've never seen a droid like this. Defies the elements. Mind bending speed. Kevlar fiber durability. Turbo charge for up to 8 hours of battery in just 15 minutes. 48 hour battery. When it matters, Droid does."

Powerful commercial: opens with an extended heist sequence of a steel box. Cut to opening the box, a phone comes out suspended by sharp blades. "This is Droid. The droid that proves thin is no longer frail. Diamond-cut spun aluminum, a face reinforced with Gorilla Glass, and a backplate made Kevlar strong. Inside it's covered with water repellent nano coating. It's the thinnest 4G LTE smartphone. Too powerful to fall into the wrong hands."

World commercial: The commercial starts out in a Geiger-like futuristic world in a dark building. The camera movies forward and doors slide open to reveal a Droid phone held in the air by mechanical arms with a mechanical, robotic finger in front of it. "Does your phone do searches?" The finger selects a search box. 'Does your phone do searches, for example the word "human"?' The finger types in the word "human". "Does your phone do searches for human on the web?" Results are returned from the web. Then Google Maps for places named "human"? Maps results are displayed and a location is selected. "Your contacts for any friends named 'human'? (Which would be weird if you had one.)" The finger goes back and scrolls through contacts. "Even your music for songs you forgot you downloaded?" The finger navigates and starts playing a song "Tokio Hotel - Humanoid" stored on the phone. "does your phone surf the web? does your phone surf the web with the speed and power of a pro surfer at pipeline ? does it bring you the web in all its glory... ...in all its intended pixels... allowing you to reach the farthest expanses of its universe, deepest depths of its oceans, without getting as much as a grain of sand in your shorts?" (similar on-screen navigation) "Droid does. Search all digital creation. wave-shredding web speed. In a world of doesn't... Droid does."

iDon't commercial: The ad starts off with poppy music and plain text against a white background, flipping through sentences: "iDon't have a real keyboard. iDon't run simultaneous apps. iDon't take 5-megapixel pictures. iDon't customize. iDon't run widgets. iDon't allow open development. iDon't take pictures in the dark. iDon't have interchangeable batteries." The ad displays "Everything iDon't,", then the mood changes. The audio breaks apart and black takes over the screen. Then "DROID DOES" is displayed and a robotic voice is heard saying "Droid".

Princess commercial: "Should a phone be pretty? Should it be a tiara-wearing digitally clueless beauty pageant queen? Or should it be fast? Racehorse duct-taped to a Scud missile fast. We say the latter. So we built the phone that does. Does rip through the Web like a circular saw through a ripe banana. Is it a precious porcelain figurine of a phone? In truth? No. It’s not a princess. It's not a toy. It’s a robot, a droid. A phone that trades hair-do for can-do."

Next-gen commercial: Astronauts enter a research complex with hexagonal tunnels. A strange rock floats in midair. A man removes his suit's arm and places his bare arm inside a hole of the rock. "Droid DNA evolution initiated. Now it integrates your work emails so you can be hooked up to everything you need to do. Now it does 1Ghz speed on a more intuitive keyboard." His arm transforms into a robotic arm, then emits a phone into his palm. The man begins typing as both his arms turn robotic. "Turning you into an instrument of efficiency." Cut to the man sitting in a chair being studied in a lab. "Vision expanding to 5-inch 1080p display and camera. With blockbuster on demand, the way you see movies... will never be the same." (animations of user's eyes turning bionic) "Touch acquired. NFC. Hearing evolving with Beats Audio. Wireless charging activated." (similar animation of fingertips and hearing turning bionic, placing phone onto charger) "Part of the next generation of does. It's not an upgrade to your phone - it's an upgrade to yourself."

Apps commercial: "Can a smartphone augment your reality? Can it see through walls? Locate restaurants and shops through your phone's viewfinder? Droid can, with Layar. One of thousands of apps that runs simultaneously with others from the ever-expanding Android market. Can a smartphone see light-years into space? Can it pinpoint your location and find any star above you? Trace constellations? Even identify other planets you're currently not on? Droid can, with Google Sky Map. One of thousands of apps that run with other apps from the ever-expanding Android market. When there's no limit to what Droid gets, there's no limit to what Droid does."

I particularly like the 2:00 campaign from 2009, "In a world of doesn'ts, ~~Droid~~ Stroscot does".

## Culture

Stroscot has its identity, values, and tone of voice as a project, and when speaking on behalf of Stroscot it is important to follow these. The community around Stroscot is more diverse and naturally some off-brand messages will also be introduced. On the one hand it is important to be inclusive and transparent and allow these messages in communication channels, on the other it is also important to be constructive and responsive and to point out the deviation in these messages from the culture of Stroscot.

Carbon has an overarching goal of promoting a healthy and vibrant community with an inclusive, welcoming, and pragmatic culture. They say ["culture eats strategy for breakfast"](https://techcrunch.com/2014/04/12/culture-eats-strategy-for-breakfast/). That article describes a "Get Stuff Done" attitude where nobody can complain - if something is wrong, the complainer must come up with a solution and fix it. Anything can be changed and nobody is a victim.

Stroscot naturally has different goals and culture but it is still instructive to think about vision, mission, values, and purpose. It is important to "own the culture" with training, monthly communications, performance-appraisal, and role models.

- Board decisions are typically made through a consensus-building process, with a majority vote and the president having the final say in the event of a tie.
- Use collaborative tools for virtual meetings and decision tracking, like Docs, Zoom, forums, Discord, etc.
- Have regular board meetings and maintain transparent communication channels - publish meeting minutes, project updates, and financial reports regularly.
- Finances - regular audits or just publish the detailed ledger
- Channels for community feedback and suggestions.
- Surveys or open forums to gather input on significant project decisions.
- System for recognizing and rewarding outstanding contributions to the project.
- Community-voted awards or acknowledgments during project milestones.

## Safe space

One idea is to have a designated safe space channel, prohibiting inappropriate language or harmful language, including:

01. Behavior that is rude, disrespectful, or inconsiderate
02. Comments or jokes of a sexual nature
03. Sexually explicit or violent material
04. Language or behavior that belittles or diminishes others
05. Language that is explicitly racist or sexist
06. Using language or making jokes that denigrate or discriminate against individuals with disabilities
07. Using language or making jokes that discriminate based on Protected Attributes
08. Promoting or endorsing behavior that violates the established code of conduct
09. Personal internet or social media activities unrelated to project goals
10. Gossip or rumors about contributors or the project
11. False or harmful information (misinformation)

I can certainly make such a channel and even enforce its rules with a little 13b language model Discord bot. If it is successful, maybe the majority of beginner channels can be made safe spaces.

## Culture policing

Something different is culture policing, ensuring communication stays on-tone in project channels. This too can be automated but it is a little more tricky because the bot should not censor speech, only fight speech with more speech. So somehow we have to get the AI to understand a concept like [Graham's hierarchy of disagreement](https://themindcollection.com/revisiting-grahams-hierarchy-of-disagreement/) and make productive comments that encourage discussions to go up the hierarchy rather than down. Just calling out name-calling as name-calling is unlikely to dissuade a contributor from their path of unintelligent argument. Indeed calling their argument unintelligent is itself a form of name-calling, although when an emotionless language model says your argument is unintelligent it does carry a bit more weight. But if the bot can make level-6 arguments like "you said X is an idiot but actually X has won the Nobel prize", even if the information is hallucinated, it will still be funny enough to derail the conversation onto a more productive track. And if instead the bot starts an argument with a troll and the troll spends all their time arguing with the bot, that is in some sense a win too.

## Over-engineering

Discussions are a fundamental part of software development. They increase scalability and allow multiple people to work together. One common discussion pattern is "over-engineering", when one or more engineers make a subjective assessment that a design is "more robust or complicated than is necessary". This can be an early warning sign of potential quality problems and missed milestones.

Now there are ways to justify robustness and complexity based on design requirements. Maybe the robustness and complexity is a safety factor, or performance-critical, or broad functionality is required or necessary for the design. But often, systems are overbuilt because it is felt it will add "intrinsic value". The complexity is not tied to a design requirement but rather speculation of future requirements. Or else it is based on a "design principle" such as DRY or MVC or so on, with no investigation of the source of this design principle or whether the design principle is actually applicable. Maybe there is a new framework that a developer wants to try out. These can be evaluated using <https://xkcd.com/1205/> - the time to learn vs. time saved. If we build a website once a year, we can guess that even looking for a framework costs half an hour, so it is not worth it unless the framework saves us more than 6 minutes. And since we will have to spend time learning the framework, probably it is not worth it. But if we build 50 websites a day, we can spend a few days learning a framework and it will pay off even if it just saves a few seconds. And we are assuming that you have learned the basic JS API already - if you are starting from scratch, maybe there are more jQuery tutorials than basic DOM tutorials, so the framework is even more justifiable.

So the design requirements itself are not sufficient to determine over-engineering. The context of the software and the existing experience of the programmer must be taken into account. Knowing the design requirements is also necessary, e.g. to determine if a framework is applicable and can save time. However, even when the full picture is available, over-engineering is somewhat subjective in that building a better foundation may save time for maintainers in the future.
