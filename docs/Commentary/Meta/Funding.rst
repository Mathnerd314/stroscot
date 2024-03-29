Funding
#######

The costs of developing a language are significant.

* the design
* the implementation (compiler)
* packages and website infrastructure
* test releases on all the different OS's and chips
* marketing (website design, Twitter, blogs, recorded talks)
* onboarding (Github, forums)
* mentoring (conferences, elmbridge)
* developer relations

  * evangelism - conferences and meetups, anything requiring physical presence / Zoom
  * advocacy - online support channels; writing tutorials, guides, templates; collecting/summarizing feedback from developers
  * community management - make sure everything's running smoothly, organizing online events

* foundation (accounting, taxes, trademarks, code of conduct, guidelines)

For now I have been doing the design unpaid, benefits of my circumstances, but if money was coming in, enough so I wouldn't have to worry about food and shelter and so on, it would become a bit easier to work on the language. Also people would see that the project is on a firm financial footing and feel more confident about using it. And things like marketing and support are necessary, because that's how you get community growth, and then more compiler engineers, and a bus factor greater than 1. And these are ongoing costs - it's not like the language ever gets 100% finished and everyone goes home.

For infrastructure, there is some amount of cheaping out that can be done - like DevRel, you set up a Discord and you get free support (maybe). And there is free CI for open-source projects. And free website hosting on Github. But it only stretches so far - you won't get free compiler engineers, and CI credits have limits. At some point you have to pay for things.

Business models
===============

A language is a tool. It doesn't create money directly (only the Fed does that). You have to have a business model where the bank (or an interested party) gives you money and then somehow they get a return on their investment in goods or services that they feel is worth the cost. So let's look at existing language models (from the `Elm guy <https://www.youtube.com/watch?v=XZ3w_jec1v8>`__):

* Company specific platform - Swift, Kotlin, C#, Rust - the money comes in from a product, like Apple phones, Android phones, Windows desktops, Firefox browser, and revenue streams thereto (phone kickback, app store, Google preinstalled apps / defaults, etc.). The language is a cost center that encourages uptake of the platform, so maybe gets 10 people.
* Corporate alliance - JavaScript, C++ - technical committee tc39, all these companies come together and negotiate how things should evolve to support the maximum user base and create the biggest marketplace and have the most commerce flowing. Contested platform kind of scenario. Fights over traffic - how easy features are to implement for individual companies.
* Rewarding talent - Dart, Go - technical success on V8 / Sawall, Google gives funding + 30 guys no questions asked
* Patronage/recruiting/donations - Python, OCaml, Elm, Zig - focus 100% on the language, someone else does the business and pays you. incentives aligned with language usage,  but very strongly related to public perception - maybe millions of people use your language, but how many of them donate? if you're not present in the minds of those people, that's going to affect you. Unpredictable revenue.
* Consulting - Clojure, Elixir, Scala, Julia - language usage correlates with number of clients, more work = more resources to handle that work. Lots of extra consulting stuff to manage (finding clients and so on), distracting from the language.
* Hosting - clojure, datomic - sort of like two problems, you build a language and you build a hosted database or whatever service with the langauge. A bit like consulting or company specific platform but you're the owner too.
* Research grants - Haskell, ocaml, Scala - excellent if you can get them, um the downside is that the language has to be good research. And no DevRel budget.
* Editor licenses - Kotlin - but you have to make a fancy editor first. and making language takes a long time.
* Usage licenses - Stata, Mathematica - pay for use, specifically academic institutions. Like economists get their institutions to buy it. but there's no established solution to the switching Cost question - e.g. Stata has R developed to replace it.

Now in his talk Evan is worried about getting "Jeff'd". You spend all this work on a language, still haven't set up a business model, but somehow the language gets traction anyway. A firm (Amazon) takes notice and puts 5 paid people on it to develop a fork. Because they're funded, they have a slicker website, better infrastructure, and more language features. They develop the language out beautifully and everyone switches to their version. You get nothing but a line in the credits or the license notices or whatever. I don't see this as a loss though. My goal by developing a language is to improve the state of the art. With the language out of my hands, as long as it seems to be following the features and goals I originally envisioned, I am happy to pass the torch and let others take over. It would be even better if we could combine our efforts and get a single version, instead of this open source vs. commercial fork split, but that's the freedom of the MIT license, is you can develop closed-source forks. Fortunately, I don't think a closed-source language is tenable in today's environment - even if Amazon wanted to take it proprietary, they couldn't, so I'll at least be able to merge their contributions back. Another question is why Amazon would feel the need to pay their own team, instead of me - I have more experience, right? Surely I would do the job better. If you want to have control over the language then you need to have actual skills, not just be the inventor of the language. What kind of genius are you if you can come up with the language but someone else knows how to improve its design better than you?

Now it would be great to get into copyleft licensing but suffice to say, the market has moved on. Less than 30% of software is copyleft. The GPL is like cancer, and nobody wants cancer. "Permissive licenses allow many more business models to work. We don’t need the strings of the GPL anymore." (`ref <https://www.r-bloggers.com/2020/04/why-dash-uses-the-mit-license-and-not-a-copyleft-gpl-license/>`__)

What model do I like? Well, I like the donation model. The open source way calls it a "dance party". Most people dance because they enjoy it, not because they're paid to. Actually a lot of people will pay to get in on a good dance party. So you set up a tip jar and it should be overflowing with money if people are having a good time. Then you use it to pay for the DJ and the sound equipment and so on. And once you get good enough, concerts will bring in the big bucks.

But it is also a good idea to make sure we are not missing any money that we should be getting:

* downloading: charge for
* enterprise support (companies, institutions): charge for
* editor plugins: charge for
* research grants: apply for these
* consulting: charge money
* talent: keep them on the project, they're money magnets
* side businesses: don't give the product away (???)
* corporate funding: somehow companies can be convinced to pay (???)

Evan specifically recommends consulting, when you start tackling real projects with a programming language then it opens up questions and avenues you haven't though about. Find a business partner, answer emails on time, develop reputation, drive incoming leads.

Design questions
================

The funding models affect design incentives. Let's say you're considering whether to add a feature. What criteria do you use?

* company specific platform - ROI
* corporate Alliance style - if the vote passes
* rewarding Talent - if the lead wants it
* patronage/recruiting/consulting - if the client/patron wants it
* Research grants - if it's research-worthy
* editor licenses - if it can be used in the fancy editor
* usage licenses - if users are complaining
* hosting - if it increases lock-in
* donations - if it aligns with language brand

So notably there are biases towards/against adding certain kinds of features - e.g. corporate alliance is very big on backwards compatibility, but also "kitchen-sink" small features. Whereas with donations, maybe the paying users have a certain use case and new syntax would break the "simplicity" of the language. We have to consider these biases when reading committee votes, user forums. It's hard to generalize to a specific decision criterion. I think the donations work best again, because brand is everything in languages. People don't use a new language because of lock-in or ROI or whatever, they use it because they heard that it's cool from their friends. Maybe once the language is big, ROI and lock-in and so on come into play, but not now.

It is better to base decisions on non-funding related criterion. Just because someone is paying you now doesn't mean they'll pay you later. If someone walked up to you and gave you $5 to implement feature F, are you going to do it? Maybe if feature F was on the roadmap and you already thought it was a good idea and it seems like the right time to implement feature F, you'll do it. But with donations there's no contract so you can just as easily laugh them off and work on something else. Even with a contract, like consulting or whatever, there is shirking, priorities, excuses, etc. so if you don't want to do something there is a way to weasel out of it. Of course, it is good in the long run to be honest and upfront and align incentives and ensure everyone is on the same page with goals and expectations and so forth, but it should be clear in those expectations that they are not paying you to set your goals and expectations. Giving up long-term language goals for short-term financial success is of course short-sighted. If the money comes with too many strings then don't take the money, or fork the language as an experimental branch or something.

It is also worth noting, the correlation between language value and income is very weak. Maybe the idea and implementation are genius but it's not the right time, or maybe the idea is super-hyped even though it's terrible and the implementation sucks. Success should be measured relative to real goals, not relative to other languages. Even if it is undeniable that you have worked hard and your language has value, there will be naysayers who think your language sucks. Most users don't pay and maybe some donators don't even use the language enough to count as users.

One great way to measure questions is experimentally - build in analytics into the compiler, bandit-style arm randomization, and look at actual metrics when you make decisions. Unfortunately you need a decent-sized user base and not all users are willing to be guinea pigs.

License
=======

Currently the main license is WTFPL, because I haven't gotten around to picking a real license and WTFPL annoys people and is avoided by large companies such as Google, thus temporarily avoiding the Jeff problem. Apache 2.0, MIT, and BSD are all good candidates for a real license. The license was Apache 2 briefly, switching back to that is probably the most likely outcome. License for contributing is CC0 until a real license gets picked, to allow relicensing.

