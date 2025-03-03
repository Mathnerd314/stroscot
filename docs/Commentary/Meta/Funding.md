# Funding

At this point, open-source projects all start the same way - one person developing in a git repo in their spare time. But most never get off the ground, while others have budgets of \$400,000/year or more. How do they do it?

## Metrics

For the most part, open-source projects are like startups - what matters is burn rate and runway. In particular burn rate is the monthly expenses minus monthly income. Once the project is cash-flow-positive, it's an established, popular project and there is no longer much question of how to fundraise - just keep doing what it's doing. Building up month-over-month funding is part of a long-term strategy for sustainability. When people see that the project is on a firm financial footing, they will feel more confident about using it.

## Income

Money can come from various sources:

- Direct donations - Python, OCaml, Elm, Zig - The Babel project on OpenCollective has done a good job of tiers and contributors. It doesn't let you focus 100% on the language, as community management and public perception is important, but incentives are aligned with language usage. The open source way uses the analogy of a "dance party". Most people dance because they enjoy it. Although professional dancers exist, a lot of people will instead pay to get in on a good dance party. So you set up tip jars, cover charges, etc. and it should be overflowing with money if people are having a good time.
- Swag - sort of like donations but you contract to produce a physical product
- Corporate donations - JavaScript, C++ - it's important to understand that their money is to ensure their use case is supported and continues to remain supported. It’s not charity, it’s value in return. There can be issues if a company has different priorities.
- Corporate advertising - It's like the million dollar homepage, getting their logo out there for everyone who looks at the project. A good project's README is surprisingly valuable ad space. But charge for time, don't do a one-time payment for a perpetual ad, because companies (and projects) come and go.
- Grants - individual project developers (Segment), government research grants (Haskell, OCaml, Scala), nonprofit grants. Excellent if you can get them, but can be difficult to get and often come with restrictions on use.
- Monetizing content (Video learning is more of a premium feature for an open source project)
- Monetizing project governance, like prioritizing features
- Charge for a dedicated support channel
- Hosting events (online or in person) - can charge a fee over and above costs
- Complementary product - Swift (Apple phones), Kotlin (Android phones, IntelliJ), C# (Windows deskop), Rust (Firefox browser), Clojure (Datomic) - Built a platform/service that uses the language, then use revenue from the platform to develop the language. But it's hard to justify spending a lot - maybe 10 people max. And you have to make a successful complementary product.
- Rewarding talent - Dart, Go - technical success on V8 / Sawall, Google gives funding + 30 guys no questions asked. But eventually you’ll have to satisfy some business need in - and you strip all the passion out when you tell them they need to work for a company to contribute substantially a project.
- Consulting - Clojure, Elixir, Scala, Julia - language usage correlates with number of clients, more work = more resources to handle that work. Lots of extra stuff to manage (finding/attracting clients, finding a business partner, answering emails on time), distracting from the language, but can give insight into use cases.
- Usage licenses - Economists get their academic institutions to buy Stata, similarly most colleges license MATLAB and Mathematica. But forcing people to pay for your product is not "the open-source way", and particularly you can get "Jeff'd" if a software giant makes an open-source clone - e.g. Stata had R developed to replace it, and now R is wildly more popular.
- Open Core: Like VSCode, the project is open-source but then there are proprietary addons, costing money, and the add-on store itself is locked down by Microsoft.
- Download fee - somewhat rare, but for example OpenBSD sells CDs and Sourceforge shows ads during the download. For example, there could be a "please donate to download" wall. The wall can have varying levels of security (click small print to download, view source to download, google reddit question to download, ask on Discord to download, account credentials to download) - presumably some level is effective but not annoying enough for people to complain or distribute it themselves.

It’s more important to build good relationships with all of your users than to come across as desperate for cash. Similarly to salary negotiations, if you talk about the money first, you’re always going to lose out.

## Expenses

The costs of developing a language are significant. There will always be something to spend money on. But the needs can be prioritized. Spending should always consider the long-term impact.

- Spending should first go to supporting the main developers full-time, as the project's success comes first and no developers means no project. It is much easier for developers to work on the language when supported. Once I'm fully supported, then the focus would be on obtaining another compiler engineer, and a bus factor greater than 1. Once there are 2-3 people that can maintain the project, then other needs can be funded.
- Infrastructure - website hosting, forums, testing/CI (Travis, AWS), package repositories - Initially, there is some amount of cheaping out that can be done. Discord is free, CI is free, website hosting is free on Github, and files can be shared via Google Drive or a file host. But it only stretches so far - the free services have limits. At some point you have to pay for things.
- Subsidizing attending/hosting conferences and meetups
- Hiring a professional website designer
- Buying online ads
- Accounting, taxes, trademarks

Managing income and expenses via a nonprofit entity has many advantages. Open Collective makes it easy to transparently manage a project under the umbrella of a nonprofit.

Some funding setups are limited to paying for legal issues or infrastructure - but if you can't have a career with the project, why bother?

Planning long-term is important. It's important to have a roadmap in place so the developers use their time efficiently. It's also important to always be looking at how to evolve the project to address new challenges that might otherwise require new tools. It is important to always be looking for new opportunities, while also not overspending, so the project doesn't run out of funding at the end of a grant.

## Developer relations

It is important to do developer relations and marketing. The key is to monopolize internet presence. Particularly, SEO is important - if someone googles the project and spam is the top result, they will wonder if the project even exists. It's also important for ownership. Suppose Amazon forks your project and starts selling it as a commercial service. Without good SEO, people might not even know that your project is available for free. And similarly, even if there is a noncommercial fork, you want to bury that fork so you keep getting all the donations, contributors, and so on. The goal is to be so open and welcoming that nobody feels like forking in the first place - if someone wants to take the project further, I am happy to pass the torch and let others take over. It would be even better if we could combine our efforts and get a single version, instead of this open source vs. commercial fork split, but that's the freedom of the MIT license, is you can develop closed-source forks. Fortunately, I don't think a closed-source language is tenable in today's environment - even if Amazon wanted to take it proprietary, they would face community backlash, so I'll at least be able to merge their contributions back. Another question is why Amazon would feel the need to pay their own team, instead of me - I have more experience, right? Surely I would do the job better, as the inventor of the language. But note that maintaining control over the language requires backing it up with actual skills - it is completely possible that the inventor knows how to make the language but someone else will be better at improving it.

Webpack publishes a medium blog 4x/month, consisting of weekly summaries of what they're working on and articles submitted by others. They also publish documentation, online courses, workshops, tutorials, guides, books, recorded talks, and sample project templates.

It is also important to do support, not just on project channels but across the web. Webpack spends hours a day searching Twitter, Reddit, and Hacker News for project mentions and responding. Webpack tries to be first to respond to every post, positive or negative, so they can control the narrative. Whatever someone sees about the project, they always come away with a positive, on-brand perception. For example, most articles on Webpack have no negative comments these days, which is a pretty unusual accomplishment on the internet.

Collecting and summarizing feedback from developers is important. The only way to improve the project for people's actual needs is to identify those needs and work on them. Speaking to people about their use cases can sometimes identify key stakeholders who are willing to invest or sponsor. This wouldn't happen without doing the research beforehand and then reaching out.

Managing complaints is complex and involves both emotional and technical skills. Some projects use a CRM to ensure continuity of experience across channels. Sometimes there is a technical issue that should be recorded, and other times people just want to vent or be heard. A message can appear to be callous, misunderstanding, or trolling. Regardless, a response should start by showing that you have listened to them and care. "We’re sorry you feel that way" or "Hey, thank you so much". Next figure out if there is any way you can help - “What is this person’s need? What are they trying to accomplish? And how can we help?” Most of the time, simply showing your appreciation that they have given your project attention is enough to change their tune. Sometimes they’re just having a bad day, or need to be pointed in the right direction. They may even become a contributor.

## Design questions

Income sources affect design incentives. Let's say you're considering whether to add a feature - what choice maximizes your income?

- Direct donations - if it aligns with language brand
- Swag - if it attracts highly-invested users
- Corporate donations - if enough companies support it
- Corporate advertising - if it will give good press
- Grants - if it aligns with the restrictions of the grant
- Educational content - if it can be taught but not easily
- Project governance - if it has the most money allocated to it
- Support channel - if it will cause subtle but cryptic issues that happen occasionally
- Hosting events - if the language will be more popular
- Complementary product - if there is sufficient ROI
- Rewarding talent - if the lead wants it
- Consulting - if the clients need it
- Usage licenses - if users are complaining
- Open Core: if it can't be monetized directly, but enables future proprietary addons or increases popularity
- Download fee: if it increases popularity

So notably there are biases towards/against adding certain kinds of features - e.g. corporate donations is very big on backwards compatibility, but also "kitchen-sink" small features. Whereas with donations, maybe the paying users have a certain use case and new syntax would break the "simplicity" of the language. We have to consider these biases when reading committee votes, user forums. It's hard to generalize to a specific decision criterion. I think the donations work best again, because brand is everything in languages. People don't use a new language because of lock-in or ROI or whatever, they use it because they heard that it's cool from their friends. Maybe once the language is big, ROI and lock-in and so on come into play, but not now.

It is better to base decisions on non-funding related criterion. Just because someone is paying you now doesn't mean they'll pay you later. If someone walked up to you and gave you \$5 to implement feature F, are you going to do it? Maybe if feature F was on the roadmap and you already thought it was a good idea and it seems like the right time to implement feature F, you'll do it. But with donations there's no contract so you can just as easily laugh them off and work on something else. Even with a contract, like consulting or whatever, there is shirking, priorities, excuses, etc. so if you don't want to do something there is a way to weasel out of it. Of course, it is good in the long run to be honest and upfront and align incentives and ensure everyone is on the same page with goals and expectations and so forth, but it should be clear in those expectations that they are not paying you to set your goals and expectations. Giving up long-term language goals for short-term financial success is of course short-sighted. If the money comes with too many strings then don't take the money, or fork the language as an experimental branch or something.

It is also worth noting, the correlation between language value and income is very weak. Maybe the idea and implementation are genius but it's not the right time, or maybe the idea is super-hyped even though it's terrible and the implementation sucks. Success should be measured relative to real goals, not relative to other languages. Even if it is undeniable that you have worked hard and your language has value, there will be naysayers who think your language sucks. Most users don't pay and maybe some donators don't even use the language enough to count as users.

One great way to measure questions is experimentally - build in analytics into the compiler, bandit-style arm randomization, and look at actual metrics when you make decisions. Unfortunately you need a decent-sized user base and not all users are willing to be guinea pigs.

## License

Currently the main license is WTFPL, because I haven't gotten around to picking a real license and WTFPL annoys people and is avoided by large companies such as Google, thus temporarily avoiding the Jeff problem. Apache 2.0, MIT, and BSD are all good candidates for a real license. The license was Apache 2 briefly, switching back to that is probably the most likely outcome. License for contributing is CC0 until a real license gets picked, to allow relicensing.

It would be great to get into copyleft licensing but suffice to say, the market has moved on. Less than 30% of software is copyleft. The GPL is like cancer: nobody wants it. "Permissive licenses allow many more business models to work. We don’t need the strings of the GPL anymore." ([ref](https://www.r-bloggers.com/2020/04/why-dash-uses-the-mit-license-and-not-a-copyleft-gpl-license/))

## Sources

- <https://medium.com/open-collective/funding-open-source-how-webpack-reached-400k-year-dfb6d8384e19>
- the [Elm guy](https://www.youtube.com/watch?v=XZ3w_jec1v8) talking about open source programming language funding
