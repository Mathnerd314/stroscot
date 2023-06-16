Standard library
################

What is a good standard library? Well, in Stroscot, the goal of the standard library is to simplify the question "For this task, how do I use a 3rd party library?". Obviously, writing your own code is fun and all. But life is short, and reinventing the wheel is a waste of man‑hours that can be better spent elsewhere.
Still, there is a lot of due diligence necessary to use a 3rd party library. The standard library is the "opinionated and supported path" to build programs. (`Spotify <https://engineering.atspotify.com/2020/08/how-we-use-golden-paths-to-solve-fragmentation-in-our-software-ecosystem/>`__) The library team has already done the due diligence, identified the "golden path", and any caveats will be noted in the documentation.

Of course, sometimes the solution you want is not in the standard library, but that's where continuously expanding the standard library can help. In an ideal world, the standard library would offer a common set of abstractions used by all programs that covers all use cases. Eventually, even if a user needs an unusual data structure like a Y-fast trie, they should be able to find it in the standard library.

Checklist
=========

The standard library is essentially a shortcut for using 3rd party libraries in general. So let's look at that process for `Chromium <https://chromium.googlesource.com/chromium/src/+/main/docs/adding_to_third_party.md>`__:

* Identify a necessary piece of functionality.
* Find various implementations (e.g. by trawling through Google, Github, or Bitbucket search results)
* Filter to ones with acceptable licenses (e.g., no closed source components, MIT/X11/BSD/Apache 2.0/GPL/LGPL/MPL)
* Determine subjective quality: Is there an issue tracker? If so, how many bugs are reported and how quickly are they resolved? Is the API easy to use? What do users report the performance / memory usage is like?
* Determine objective quality: Is the code readable? Does it have good documentation?  Has the code been profiled and optimized?
* Determine stability: How long has the project been around? What have the recent releases been called (patches, betas, etc.), and is there a promised support schedule? Do release notes frequently describe breaking changes? Is there a test suite and release checklist for ensuring quality?
* Determine size: How large is the source code? How long does it take to build? What is the increase in binary size for a typical project?
* Determine maintenance: Who will maintain the library? (Existing maintainer, standard library team) What platforms will be supported?
* Choose the best choices among the alternatives, and make a pro/con matrix so it is easy to understand which library should be used where.
* Fork the code and mirror the history into a Git repo. This makes it easy to maintain the code, particularly time-sensitive security patches, segregates the code so it is easy to track license/credit/necessary attributions, and avoids any issues with hosting going down.
* Add metadata:

  * Name: Descriptive name of the package
  * Description: A short description of what the package is and is used for.
  * Version: (OPTIONAL) A version number for the package. Otherwise, the Git revision hash is used.
  * Local Modifications: Enumerate any changes that have been made locally to the package from the upstream version. (Note that e.g. Apache-2 also requires notice in each modified file)
  * License: The license under which the package is distributed. Probably best to use SPDX identifiers.
  * License File: File that contains a copy of the package's license or credits.
  * Security Critical: A package is security-critical if it accepts untrustworthy inputs from the internet, parses or interprets complex input formats sends data to internet servers, collects new data, or influences or sets security-related policy (including the user experience)
  * Identifiers: (OPTIONAL) A dictionary of identifiers which represent the upstream package, e.g. short name, upstream URL/repo (for detecting new versions), and 'common platform enumerations' (https://nvd.nist.gov/products/cpe/search), which allow automated detection and reporting of known vulnerabilities.
  * Hashes: technically this is in the git revisions, or git tags, but GPG signatures and SHA hashes should be recorded
  * Owners: at least two people on the standard library team, who have the responsibility to keep the library updated with upstream and any security packages. Of course most updates are automated, so it is really just fixing broken stuff.

* Security review by the security team, license review by script or random person
* Refactor other libraries to use the chosen library, so that there is not duplicate code

Pretty much all these steps are also necessary to include something in the standard library.

Scope
=====

Currently, since the language is unimplemented, the standard library doesn't exist. Although it would be nice if we had a standard library ready-made, the lack of a standard library is honestly not a bad state of affairs, compared to having a bad standard library. C's standard library is so small and old that barely anyone knows it is there, and C++ has a standard library but it has so many forks and unused areas that it's not really a standard. Of course, for implementing the language, it is necessary to have some basic functions; these are termed the "compiler library" and discussed in a separate document. In the near term, Stroscot's compiler library will grow, and it will also be encouraged that everyone develops their own competing non-standard libraries. Then, as the good and the bad parts of each library becomes clear, the "standard" library will emerge following an evolutionary process.

Generally speaking, non-standard libraries should be in active development. The main advantage of being outside the standard library is rapid iteration: there's less pressure to maintain compatibility, and development can focus on achieving a good design. Once a library is stable, in the sense of "no major API changes", it might as well go through the standards process and become available to import without the extra installation step. The few kilobytes overhead of additional code is pretty small. The standard library provides discoverability and maintenance benefits over isolated libraries. For example incorporation solves the `left-pad <https://qz.com/646467/how-one-programmer-broke-the-internet-by-deleting-a-tiny-piece-of-code/>`__ issue where key libraries are maintained by solo developers with no oversight. Since it's all FLOSS, licensing should not be an issue, and presumably most developers will be happy to share maintainershup and join the team, or relinquish maintainance entirely.

The main goal of standardization is to solve fragmentation. With no effort to standardize, over time, sharing code becomes problematic because pieces of code become tied to one or another mutually incompatible libraries, and there are endless flamewars and newcomers get turned off by decision paralysis. Example: `scalaz vs cats <https://github.com/fosskers/scalaz-and-cats>`__ was an issue with Scala for a long time, before `it became clear <https://www.reddit.com/r/scala/comments/afor0h/scalaz_8_timeline/>`__ that Scalaz 8 would never be released and scalaz was effectively dead, thus making cats the go-to choice. There is a possibility that standardizing a solution in the standard library will crowd out other solutions, but discussing trade-offs and linking alternative libraries in the standard library documentation is probably sufficient, as after all the discussion process will presumably have created some intelligent reasoning behind choosing one library as standard. It doesn't really matter if the wrong decision is made because a robust evolution process means it can always be changed later, and in the short term 50% standardized is better than 0% standardized even if there is a (not-at-all obvious) 60% option. What is problematic is letting a split continue to fester without a clear path forward.

Stability
=========

Try as we might, no design is perfect. Languages and libraries inevitably change or extend their semantics over time, resulting in ecosystem fragmentation where programs end up being written in different "dialects" (specifically, the old and new versions, e.g. we are considering C++11 and C++14 as different dialects). So, we cannot prohibit fragmentation; it will happen regardless, unless we bury our hands in the sand and freeze everything entirely.

But, we can aim to minimize the disruption of the evolutionary process to existing code. In particular, by discretizing evolution into units of "features" and "versions", we can provide a compatibility promise that the source code of existing programs written for an old version can be automatically migrated to a new version.

Furthermore, the versioning process aims to determine a standardized, stable set of features, so by encouraging the use of approved versions of the language, the overall community can avoid fragmentation, even if there are several dialects of the language in use at any one time.

A feature is a distinct chunk of functionality, such as a change to the semantics of the language, a compiler plugin, an external tool integration, or a new or updated standard library module. A feature can be alpha, beta, or stable.

Alpha features are experimental features with little formal testing, released to get feedback. They may be documented informally or on an "alpha features" page. Alpha features have no compatibility guarantee and may be changed freely. Alpha features are kept behind feature toggles, which allow conditioning code on a feature. This allows testing features and integrating them on the main branch while isolating them from other tests and software releases. Alpha features will be removed from the compiler if they have not made any progress towards beta over the course of a year.

Beta features are implemented features that may change further. They must have a reasonable test suite and be documented in the commentary / reference in full detail, describing edge cases. They must also have a how-to if the feature's usage is not obvious. Fundamental new features may affect the tutorial as well, although generally new features are too advanced. Beta features cannot be toggled off but have automigration functionality for old code that is enabled by specifying the language version. Automigration is distinct from a toggle because it is a source-to-source rewrite of the code. Beta features may still have significant bugs, such as the inability to migrate old code correctly, but these bugs should generate readable error messages mentioning the feature name rather than crashing the compiler or silently failing.

Stable features are frozen features - further changes will be done as new features. They are considered to have reached a level of stability sufficient for long-term use. There is no visible difference in the implementation code between beta features and stable features and the distinction is mainly for marketing purposes.

The list of features is centralized in the code to `this specific file <https://github.com/Mathnerd314/stroscot/blob/master/src/features.txt>`__, to make finding them easier and to standardize handling. The scope of a feature may be identified by grep'ing the code for its identifier.

Moving a feature from alpha to beta should have a PR with documentation links and test case links. The PR should:

* change the feature list to set the feature's status to beta released on the current date. This enables old code warnings, automigration, and compiler bootstrap workarounds.
* implement automigration code if not already present
* remove all uses of the feature toggle in the code by modifying to the case where the feature is present (avoiding toggle debt).

A (language) version is determined annually through some process. I don't have a good idea of this process, but here is a sketch: First, a survey is sent out where people describe features they use and don't use, and which ones break code or don't break code. Then, the committee goes through each feature, and select the ones the people like and the ones that don't break code.

Processes
=========

* It should be easy to add code to the standard library, and the standard library should always be trying to expand. Taking more than a year to add a new API is just too slow; a 6 month process from "let's add this" to being available in the most-unstable release branch seems about right. Obviously, if there is a single popular third-party library that has become the "go-to" library for some task, the process is straightforward: it should just be incorporated after it has been proven to be sufficiently stable. If there are multiple popular third-party libraries that do similar things but are incompatible, there are several strategies to deal with this:

  * Analyze the pros and cons and choose one library to make standard
  * Create a new library that combines all the pros and none of the cons of the existing libraries
  * Create a wrapper interface that provides the least common denominator among libraries, but allows importing specific libraries for more functionality

* It should also be easy to remove code from the standard library. Some APIs inevitably become obsolete as others are added and become more popular. Similarly it should be easy to fix names, implementation details, and API design, as conventions change. This is accomplished as an add-remove pair. But people need time to migrate, so there should be a 2-year deprecation process. There should be some amount of forward stability so that if code compiles with an old standard library, it will continue to do so with a new standard library. This means deprecated API isn't actually removed, it instead goes to a "compatibility graveyard" and stays around for old projects while being invisible to new ones.
* RFC Process: It should not be hard for people to make forks / small patches to the language / library as experimental language extensions. But making such changes standard is more involved. Per `Robert Virding <https://youtu.be/f3rP3JRq7Mw?t=102>`__, it is often hard to see the whole picture. An RFC process for language changes helps to flesh out details and establish what the full impact of a change will be. Making a very simple change can affect many other things, indirectly causing a lot of problems and a lot of strange behavior. Once the change is formalized, it has to be evaluated against the principles of the language and goals of the standard library. Although Stroscot aims to be a universal language, hence making everything possible, some things are just too weird to really be of use, or can be easily implemented in terms of the existing standard library. When you're firm on your no's, and explain the reasoning behind your decisions, eventually the users will go away and work around the decision, and, assuming your reasoning is sound, make a better solution than what they originally planned.
* The most important aspect is finding a group of people willing to maintain the code and keep up with patches / bug reports - a lot of code does just fine by itself and doesn't need much effort, but when there is a response needed, it should be a high-quality response. Third-party library maintainers should live up to the standards set by the standard library team, rather than the other way around.

Blessed prelude
===============

The standard library is blessed in that its prelude module is imported by default into every module. Other than this there is no special support from the compiler for the standard library. Furthermore there is a compiler option to override the prelude import to import no prelude or a different prelude module.

Since the standard prelude is imported by default it should be small, so that no name conflicts arise. The definition of small varies but we'll just take the community consensus. A truly minimal prelude would just have the import statement, which would also have some advantages.

Security
========

The first defense is security through obscurity - who is going to check the library for issues besides the maintainers? But of course, the more popular the library is, the more attention must be paid to security, and the standard library is probably the most popular of all. But, code is generally not vulnerable if it uses the library the intended way. Also, most security issues are due to unsafe semantics, such as unchecked memory access or manipulation of raw strings instead of structured data, which can be addressed through good language and library design. Still, it is worth having a security review for each new library, and a bounty program once sufficient funding is available. It seems from examining bounties that most standard library bugs are actually not too valuable, around $500.
