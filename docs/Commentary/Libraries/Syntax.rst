Syntax
######

Stroscot's syntax should be clear, readable, friendly, and consistent. It should be easy to learn and use for those who are new to programming. The language should be accessible to a wide range of users, including those with disabilities. Furthermore the syntax should be flexible, extensible, and customizable, so that developers can tailor aspects of the language to their specific needs and preferences.

S-expressions are sufficient for the core language, there is not a huge need for nice syntax. Syntax is one of the most boring aspects of building a language - there's not a lot you can innovate upon (except maybe using a large language model for parsing). So in Stroscot the syntax is defined as a compiler library.

Customization
=============

Lots of old crappy languages are still in use, terrible design choices or not. Most people can get used to almost any syntax, and some will even come to like it, even if that syntax performs terribly vs other options in controlled studies. So in that sense, most PL syntax decisions do not really have a "bad" option - whatever syntax we end up on will be the syntax used, each individual choice will probably not impact language adoption that much, and even if there are a lot of complaints about the syntax, there probably will not be a consensus so it will stay unchanged for many years. Since the syntax doesn't really matter, it makes sense to make it pluggable and customizable - switch out the standard library, switch out the syntax. This allows supporting use cases like non-English coding and demonstrations of new/changed syntax. Most likely common uses will only be simple formatting customizations like indentation size, tabs vs spaces, line length, and layout vs braces, but once there is a syntax definition file it makes sense to allow putting more things in it. One specific issue is allowing per-checkout customization, so someone can write a git smudge/clean filter that runs the formatter on checkout/commit.

On the other hand, some choice of syntax is necessary for writing the compiler, standard libraries, and documentation examples. Let's call this the "default syntax", since it makes sense to use the same syntax for all of these. Although the impact of individual choices may be minor, overall the default syntax is quite important - it defines the "feel" of the language and is probably the biggest factor of whether people try out the language or close the homepage. As a rough sketch, using numbers from `Firefox <https://blog.mozilla.org/metrics/2009/08/11/how-many-firefox-users-customize-their-browser/>`__, about 1/3 of people will bother to use some amount of customization, while 2/3 will use the default settings. The 1/3 that customize obviously have their reasons for wanting the customization. But with a million users, 2/3 of a million is quite a lot. Syntax is decided by usage, so the default syntax will most likely become the most popular. But it could also be that as the language becomes popular, an alternative syntax emerges and takes over, ending up in an evolution of the default syntax. So none of the choices of the default syntax are final, but neither should they be arbitrary. It's similar to the discussion of IDEs, where I settled on VSCode - of course I'm not going to force a choice of IDE, but neither am I going to spend much effort on supporting other IDEs, at least until the language gets popular and contributors start working on IDE support themselves.

Some studies use a "Randomo" language which randomizes design choices. It would be useful to implement syntax randomization as part of the customization, so choices could be compared and tested. Basically this uses the per-checkout customization and stores code in the repository as a non-readable base64 or Lisp-like AST dump. Then people get an initial randomized style, but can customize it to their liking, and once we reach 100 users we collect styles/opinions and decide on a final choice as default.

Principles
==========

Syntax is essentially the UI of the language. As such UI guidelines apply. One such book is `The Humane Interface <https://en.wikipedia.org/wiki/The_Humane_Interface>`__, we can go through the principles:

* Modelessness – in a programming context, this means the same program always produces the same output. It is a bit harder to interpret this principle in the context of language because language is naturally ambiguous with homophones and contextual meanings.
* Monotony of design – there should be only one way to accomplish a certain atomic task in an application, as opposed to toolbar button, menu dropdown, and keyboard shortcut. This principle makes sense when applied to programming design, one syntax for a given language feature, but is perhaps a bit subtle in the definition of "atomic" - for example Haskell has ``let`` and ``where`` which are almost identical syntaxes, except one puts the main expression above the definitions while the other puts the main expression below. A similar principle is Steelman 1E "The language should have uniform syntactic conventions and should not provide several notations for the same concept."
* Universal undo/redo – in programming undo/redo is handled by a combination of the IDE and VCS; the IDE does a limited undo/redo for small changes, and the VCS saves changes persistently and allows full control. Certainly a universal system for programming would be a better UI but it would most likely require making a new IDE. That will have to wait. For debugging there is also reversible debugging that allows undo/redo of execution.
* Elimination of warning screens – modern software applications often ask the user "are you sure?" before some potentially harmful action. In the context of programming, I think this is most similar to Rust's "unsafe" blocks or Kotlin's "experimental API opt-in" annotations. I agree with Raskin's argument that they are unhelpful because users tend to ignore them out of habit - in an IDE it just adds a monotonous "auto-fix all the stupid compiler issues" step. Similarly Steelman 1E "No arbitrary restriction should be imposed on a language feature."
* Universal use of text – this is discussed more in the `Text-based`_ section, certainly it seems that an icon-based language would be less usable

Another set of principles is Constantine and Lockwood's principles of usage-centered design:

* The structure principle - According to Domain Driven Design, the design of the code should follow the structure of the problem domain, as such the programmer must have most of the control over the layout of the code and it is not really a good idea to have brittle or opinionated syntax. It is also worth considering the overall structure of the standard library, the compiler, and the documentation.
* The simplicity principle: I would rephrase this as "common tasks should be easy and clear, difficult tasks should not be obscured by the syntax". This is an overall principle for Stroscot but I guess it needs special attention in the syntax.
* The visibility principle: This can go a few different ways. One is that a function should generally be at most one screenful of code. Another is that IDE navigation support is a must. Another is that if the programmer has a mockup, design document, or reference paper, then this should be able to be included in the code somehow. You can pretty much do this with comments (VSCode even hyperlinks them) but maybe there is a better solution. On the flip side, it also means syntax should not be noisy and it should always be clear how to write a program in the best style.
* The feedback principle: A program by itself is inert, no feedback. This principle therefore applies to the compiler: compilation progress, execution progress, program state, debugger state, warnings and errors. I am not sure it applies to syntax except to ensure that each syntactic construct is localized so it can give a good underline error in VSCode.
* The tolerance principle: This is sort of Stroscot's principle of no fatal compiler errors. Another interpretation is to tolerate syntax errors, e.g. using a large language model that will identify (some approximation of) intent of the program and allow varied syntaxes and input notation. "The design should be flexible and tolerant, reducing the cost of mistakes and misuse while also preventing errors"
* The reuse principle: The design should reuse internal and external components and behaviors, maintaining consistency with purpose rather than merely arbitrary consistency, thus reducing the need for users to rethink and remember.

Going through these, they are a bit hard to apply. Per `Wikipedia <https://en.wikipedia.org/wiki/Principle_of_least_astonishment>`__  there is the law of least astonishment, which in 1972 read as follows:

    For those parts of the system which cannot be adjusted to the peculiarities of the user, the designers of a systems programming language should obey the “Law of Least Astonishment.” In short, this law states that every construct in the system should behave exactly as its syntax suggests. Widely accepted conventions should be followed whenever possible, and exceptions to previously established rules of the language should be minimal.

Defaults should match typical usage patterns.

* easy for humans to type, parse, and scan
* no confusable symbols
* conceptual integrity: anywhere you look in your system, you can tell that the design is part of the same overall design. Uniform/coherent formatting, identifier naming, and module structure. This makes it much easier to take a problem with a program's behavior and trace it to a problem in the source code.
* Everything implicit: verbosity adds cognitive overhead and does not significantly reduce the amount of context the reader must remember
* do not try to prevent developers from misusing features

There is also Steelman 2B: "The language shall have free form syntax and should use familiar notations where such use does not conflict with other goals." As discussed next, I am not sure what familiar notations are usable besides maybe arithmetic.

https://www.youtube.com/watch?v=f3rP3JRq7Mw 15:08-15:59 Avoid adding special cases in your syntax just to save a few bytes. It doesn't really matter how long your code is. For example, Lua has a syntax for writing down a literal key-value table, and between each key-value pair, you put either a comma or a semicolon. It's very simple and straightforward. One guy worked out that in many cases you could avoid having to put this comma or semicolon in. It wouldn't always work, but sometimes you could do it, and it could save 200-300 bytes in his program. But it didn't work all the time, and if you omitted the separator in the wrong place you might get something behaving completely differently. So in the end special cases just make code more difficult to write and understand.

Steelman 4A. The parsing of correct expressions shall not depend on the types of their operands or on whether the types of the operands are built into the language.

4F. The precedence levels (i.e., binding strengths) of all (prefix and infix) operators shall be specified in the language definition, shall not be alterable by the user, shall be few in number, and shall not depend on the types of the operands.

4G. If present, explicit parentheses shall dictate the association of operands with operators. The language shall specify where explicit parentheses are required and shall attempt to minimize the psychological ambiguity in expressions. [Note that this might be accomplished by requiring explicit parentheses to resolve the operator-operand association whenever a nonassociative operator appears to the left of an operator of the same precedence at the least-binding precedence level of any subexpression.]

Design procedure
================

My original syntax design plan was to go through the syntax of other languages listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and pick out the nicest/most common examples. But I didn't really like the results. For example, take comments. Counting up examples on Rosetta Code's `comment page <https://rosettacode.org/wiki/Comments>`__ showed the most common choices:

* For EOL impl, C++ style ``//``
* For block impl, C style ``/* */``
* For EOL doc, D style ``///``.
* For block doc, Javadoc style ``/** */``
* For code comments (nesting block), Pascal style ``(* *)``

Going through RosettaCode was good for identifying the different styles of comments, but when I was going through concrete choices I saw lots of other options, some nearly as common: an EOL impl comment can be ``;`` or ``#`` or  ``->``. Similarly there are many choices for other syntactic elements.

In general, directly copying syntax from other languages is a mistake for several reasons:

* Different semantics - Programmers will come in with preconceived notions based on common syntax, and find the differences strange and confusing.
* Easily learned - Syntax is easy to explain: just write down the grammar, and a few examples since BNF isn't self-explanatory. It probably takes more time to write a complaint about new syntax than it does to just memorize it.
* Blind leading the blind - a lot of existing syntax choices in popular languages are bad. They're unintuitive, hard to use, and programmers just plain hate them, but live with them anyway.

Quorum and its associated set of studies by Stefik provide an alternative approach - design a survey and ask novices what they think is most intuitive. As Stefik says in the papers, choices ranked highly by experienced programmers but not chosen by novices are likely mistakes in PL design, learned helplessness since unintuitive PL syntax is hard to change.

Stefik used a mixture of CS university students (freshman year novices and junior/senior year programmers). But since we're designing for novices we don't actually care about or need to measure experienced programmers. So using a convenience sample by posting a Google Form / SurveyMonkey / LimeSurvey to r/learnprogramming isn't that hard - polls need approval but this seems like it might be allowed. (TODO: is there a better place on the internet to find programming novices?). General outline:

* Title: "Never programmed? Help design a new programming language! (Survey)"
* Demographics: age, gender, native English speaker. These are mentioned as questions in Stefik's survey :cite:`stefikEmpiricalInvestigationProgramming2013`, but aren't mentioned as having any statistically significant correlations, so can probably be left out or be highly stratified to make people less worried about giving out personal information. :cite:`dossantosImpactsCodingPractices2018` did find statistically significant differences for females to prefer multiple statements on one line and more intermediate variable assignments, but there were only 7 females so this is probably noise. OTOH, it would be useful to have demographics to compare to large surveys like the `Stack Overflow Developer Survey <https://insights.stackoverflow.com/survey/2021>`__.
* programming experience. Stefik found that programmers will list experience with individual languages even when reporting no experience total, so it seems best to skip an overall experience question and ask individually with a language matrix. Since not all languages will be listed we'll have an "All other programming languages" catch-all at the end. Stefik presumably asked an empty-box "how many years of experience do you have with X" question. But a matrix requires choices. Taking his mean experience reported as the data points, for experienced programmers, dividing into 5 buckets we have 0-0.03,0.03-0.09,0.9-0.22,0.28-0.48,0.67-1.76. For inexperienced programmers, we have 0-0 as a large bucket and then the remaining range is split into 0-0.01,0.01-0.03,0.05-0.09,0.12-0.23. Combining experienced and inexperienced, the 0-0 bucket stays and we have 4 more buckets 0.01-0.03 (±0.11-0.16), 0.03-0.09 (±0.18-0.31), 0.11-.28 (±0.41-1.06),0.39-1.76 (±.0.7-1.87) Translating into familiar units, these buckets are no experience, <11 days, 11-33 days, 40-102 days (1.32-3.36 months), 4.68+ months. Given the wide uncertainties we can round to no experience, < 2 weeks, 2 weeks to 1 month, >1 month and <4 months, or 4+ months. It's not as accurate as the empty-box but hitting a circle on mobile is much easier. In terms of the survey of :cite:`siegmundMeasuringModelingProgramming2014`, it is a quantified version of the self-estimation that rates experience as 1-5. We can sum the (approximate) years of experience to obtain overall years of experience, which should lead to Siegmund's "professional programming experience" factor, which IMO is the correct factor to use to measure programming experience. But we would have to do another validation study with university students to verify that this metric correlates with the ones in Siegmund's study. In fact though I am mainly going to use the metric as a binary variable, novice vs. not, so it's not too important.

  As far as languages, Study 1 had HTML, C++, Java, Matlab, JavaScript, Basic, COBOL reported with experience for non-programmers and Study 2 was similar with the addition of PHP. Considering programmers among both studies, we would add C#, Python, Ruby, and COBOL as <1 month languages, and Perl and FORTRAN as <2 week languages. Meanwhile the SO language list top 15 is JavaScript, HTML/CSS, Python, SQL, Java, Node.js, TypeScript, C#, Bash, C++, PHP, C, Powershell, Go, Kotlin, with a clear break in popularity from C at 21% to Powershell at 10%. The question asked for "extensive development work in the past year" though so is not really a question about which languages are most likely to have beginner exposure. Contrariwise TIOBE does not consider HTML a programming language because it is not Turing complete, but does list SQL. We do not want to list too many languages, because marking lots of languages as "no experience" is tedious, but a good selection is key to defining the question and ensuring the catch-all is answered accurately. One design point would be to preselect "no experience" for all but the catch-all, solving the tedium issue, but the survey tool would have to support this.
* reading experience: as an alternative to age, being able to gauge the vocabulary of the

* Have you heard of the Stroscot or Quorum programming languages before this survey? Yes/no. This is a question Stefik says he wished he had asked, to avoid confounding results. (2 questions actually)

The meat of the survey is questions of the form "<English description of PL concept>; how do you think the syntax of <concept> should look like for <example>?". First we want an open-ended text field to get unprimed responses, then multiple-choice to compare against existing PL syntaxes. We can of course use RosettaCode as the source of choices, top X choices either randomized or ranked. Stefik did individual rankings of each choice on a 0-100% scale by 10%'s, but I think "select first,second,third choice" is sufficient and less tedious. The descriptions should be targeted to a reading level of age 8. This is because, per `this article <https://www.kinvert.com/age-teach-kids-python/>`__, children simply do not have the motor coordination to type on a keyboard or the reading skills to use a text-based language before ages 6-8, and kids outgrow Scratch around age 8 so age 8 is a good target. In the US, grade 3 is ages 8-9 correspond to grades 1-3, so 3rd grade is the target to aim for with the Flesch-Kincaid grade level. A test-taking child of age 8 knows about 10,000 words (`ref <https://www.economist.com/johnson/2013/05/29/lexical-facts>`__). Of course, the subjects of the survey will most likely be much older, but the restrictive vocabulary of this low grade will ensure that the use of jargon is minimized. Naturally, I will not actually be writing these, ChatGPT will. Large language models have been demonstrated to perform very well at identifying the reading level of prose and devising better ways to express concepts. I don't know how well prompting ChatGPT performs versus more specialized training models, but the results seem reasonable.

Survey validity
---------------

According to :cite:`tewFCS1LanguageIndependent2011` there are two important classes of validity. First is content: establishing the topics to be surveyed, and ensuring they have reasonable coverage of the subject area. IMO Stefik failed on this point - he just picked some basic Java-style keywords and constructs. Hence his research was quite limited - he didn't systematically go through every design choice possible in a programming language. :cite:`tewDevelopingValidatedAssessment2010` went through "widely adopted" introductory textbooks to select a set of CS1 topics, but ended up with more than 400 topics - they pruned them to 29 by limiting to concepts missing from at most one textbook. And with the focus on "wide adoption" they ended up including OO but not FP. The fact that Tew tested their exam only on Java, Matlab, and Python programmers is telling. Since Stroscot is a functional logic programming language, it will likely have some different fundamental concepts, and "wide adoption" is not necessarily the right inclusion criteria. ChatGPT might help here - it knows the basic concept clusters used in programming. The PL tasks should be a mixture of basic tasks common to all languages (operations, control, data structures) and Stroscot-specific tasks that showcase its unique features. But it is quite important to pick representative tasks first, identify their semantics, break them down into constructs and "how do I do this task", and only then apply the principle of "form follows function" to magic up a syntax. But there is also some requirement to differentiate the tasks and avoid overlap - putting two questions with no significant differences on the survey will likely end up with identical responses, or even worse, variant syntaxes for writing the same thing.

A secondary form of validity is construct validity. This ensures that the survey is actually measuring what it is designed to measure, rather than something else. Without some procedures in place, it is easy to write bad questions. They can be unclear, resulting in participants answering the wrong question. They can be biased with "leading questions", resulting in canned answers rather than useful data. Unfortunately, some amount of priming is necessary, because novices do not know what the basic syntactic constructs of a language are. If you give novices a blank page and ask them to design a programming language, you will most likely get a simple language with glaring deficiencies. But similarly if you ask a novice "What syntax should be used for the if-else statement?" there is not much leeway in the question - most likely they will use the if and else keywords. So the wording of a question can be quite important. ChatGPT can probably help here a lot by devising neutral wording that avoids prompting with too much of the answer.

For further verifying construct validity, there are various sanity checks to do:

* Stefik and Tew showed their questionnaires to several experts and resolved all issues. For similarly validating the constructs of my survey, CCC is a forum of expert programmers, who can probably spot issues given the right prompting.
* Stefik did pilot studies with both experts and novices, and confirmed that they gave decent answers. Similarly, I can post the survey to /r/ProgrammingLanguages as a trial run.
* Tew used item-response statistics. I don't think this is directly applicable as I am not designing a test, but looking at statistical measures of agreement on a per-item basis will probably be useful after the fact.
* Tew validated their questions by conducting think-aloud interviews during pilot versions of the test, showing that correct mental models corresponded to correct answers and likewise for incorrect. The write-your-own-syntax freeform question is similar to the think-aloud study, and can validate the concept descriptions.
* Stefik and Tew submitted their studies for peer review, Stefik to "Software Quality Journal" and "ACM Transactions on Computing Education" and Tew to "ACM technical symposium on Computer science education". It seems for journals in education/UX research, around 1/3 of on-topic papers submitted get accepted. ACM publishing is free, so TOCE could be an option. But it seems you need pretty close to a camera-ready manuscript for peer review, so this would be something to do after the study is pretty much done.

Influences
==========

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR parser) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Haskell/Idris syntax is mostly awesome, use it. (TODO: check this. The weird function call syntax may lose too many users) Almost everything is an expression. But there's also block statements and layout.

Fortress has "mathematical syntax", with an ASCII form and typeset form. They used LaTeX but HTML / MathML output should be possible too. And juxtaposition was overloaded. Probably worth emulating.

A language encourages certain expressions of thought. If the syntax is awkward then the feature will be used less and a bias will be introduced. But the styles of programming people come up with after a language is released are often completely different to what was intended by the language (e.g. Java and its design patterns). It's not clear that anything can be done about this, besides capturing as many existing patterns as cleanly as possible and allowing macros.

Text-based
==========

There are some people who, when confronted with the complexity of syntax, think "It's better to use a binary format and store everything in a database." Now they have two problems. Math is textual, English is textual, the only programming stuff that isn't textual are flowcharts and tables. Flowcharts might be OK (e.g. Labview) but graph layout is hard - graphviz barely works, and most graph layout algorithms such as IDA Pro's are quite lacking. Labview struggles even to layout wires (edges). Tables lead into spreadsheet programming which is generally not expressive as a language - and the formulas and cell values are textual. If you show me a way to write 123.6 that doesn't involve text (sliders aren't precise enough to do 4 digits unless they fill the screen!), maybe I'll start to consider visual programming.

There's also structural editing, `lamdu <http://www.lamdu.org/>`__ and so on, but they are designing an IDE alongside a programming language. I'm not too interested in IDEs and given that half the IDEs are for languages that also have a textual syntax, syntax doesn't seem to be a big factor in writing such an IDE.

Legibility/readability
----------------------

There have been many legibility/readability studies, but they have to be evaluated carefully. Some are out of date, some were poorly designed, and some are just not relevant to programming. So we have to describe our assumptions and working setup.

The first question is the medium. Most code will be read on a computer screen. Computer monitors have improved greatly over the years. Comparing the monochrome 1024x780 114ppi 11" $10k+ Tektronix 4010 in 1972 to the 24-bit color 1600x1024 110ppi 17.3" $2.5k SGI 1600SW in 1998 to the 3840x2160 140ppi 32" $850 Dell U3223QE recommended by `RTings <https://www.rtings.com/monitor/reviews/best/by-usage/programming-and-coding>`__ as of 2023, we see cost has significantly decreased and also there has been a significant amount of readability improvements in PPI, contrast, brightness, and persistence / refresh rate. Per `WP <https://en.wikipedia.org/wiki/Pixel_density#Printing_on_paper>`__, PPI is about half DPI, so the 300 DPI "good quality typographic print" standard corresponds to 150ppi. With subpixel rendering enhancing horizontal resolution, the recent 140ppi monitors are finally starting to have decent text quality. But there are even higher PPI displays, e.g. a 23.8" 185ppi LG 24UD58 or 16.2" 254ppi Macbook Pro "retina" laptop display, and there are reports that these high PPI displays have perceivably better text quality. OTOH, looking at what people commonly use at home, it's 1920x1080 monitors per `Steam survey <https://store.steampowered.com/hwsurvey/Steam-Hardware-Software-Survey-Welcome-to-Steam>`__. Assuming the common 24" screen size that's only 92ppi. Multi-monitor is 2 1080p displays next to each other. So we can see there is a wide range of possibilities for PPI. `This guy <https://nickjanetakis.com/blog/how-to-pick-a-good-monitor-for-software-development>`__ says a programming monitor should cost $250-$350 or so, so that's what I'll aim for, a selection of popular $250-$350 monitors, but really we can't make any assumptions. As far as presentation, it is also a menagerie of choice - colors, fonts, font size, visible whitespace.

But let's go through the findings.

* Many sources mention in passing that consistency improves readability. In particular, reading is disrupted when unconventional layouts, colors, or fonts are used, or when different values of such attributes are mixed in unconventional ways. :cite:`beierTypefaceLegibilityDefining2009` This seems generally applicable. The IDE dictates fonts / spacing / coloring so we should ensure good IDEs are used for Stroscot. :cite:`walkerFontTuningReview2008` found it is 2% faster (15ms) to read a word if the preceding word is in the same font. Similarly there is also a short-term memory "font tuning" effect where one adapts to the glyphs of a specific font and slows down if a glyph from a random readable font is inserted. :cite:`beierHowDoesTypeface2013` found that this "font tuning" adaptation increases with exposure, so that reading the same font for 20 minutes improves the speed of reading paragraphs by 6.3% (0.43 paragraphs or 11wpm). Even after significant font exposure there is also a "familiarity" effect where common fonts such as Times and Helvetica do 9.4% better (0.64 paragraphs or 16wpm) than novel fonts. This could be due either to a quality effect, where common fonts are simply better, or to an exposure effect on longer timescales, e.g. that exposure to a font still increases reading speed in that font weeks or years later. The 20-minute exposure effect is the longest tested in the literature. Since long-term memory is anything longer than 30 seconds and observers can fairly accurately pick out a photo exposed for 3 seconds, even hours or days later, any training effect can probably be entirely explained as a function of the cumulative exposure/learning of the visual patterns of letters.
* Even so, just letting people pick a style they like does not work. Wallace found that people's preferred font (as found by asking "which font is this paragraph more readable in?") has no correlation with their fastest font among 5 fonts, and that the best font among 16 tested varied randomly per individual. So an actual timed test is really the only option to obtain optimal performance. Wallace found a 14% improvement in WPM reading speed over the subjectively preferred font and 35% over the worst font. Unfortunately Wallace's study did not control for exposure so they may just be measuring noise. Exposure accounted for 30ish wpm in Beier's study and in Wallace's study the difference between fastest and preferred was 39wpm.
* The general rule for spacing is that it should be significantly larger than its next-smallest spacing unit to clearly identify the boundary it marks.

  * Intra-letter spacing should be significantly smaller than inter-word spacing to allow identifying words.
  * The inconsistent inter-word spacing of justified text is harder to read than ragged-right. With good line breaking this is not as much of an issue but the effect is still there.
  * For sentences, there should be a little extra space after the period to emphasize the sentence boundary. Just a little, not a double space, because period-normal space is almost visually distinctive enough by itself and people have gotten used to that spacing.
  * The default inter-line spacing (line height, leading) in browsers is fine for most people. Some people with disabilities need more line height. Wider columns require a bit more line height.
  * To identify paragraphs, inter-paragraph spacing should be visibly larger than inter-line spacing, or paragraph indentation should be used.

* For English, and presumably other LTR languages, left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line.
* According to an old study: "Longer lines will be read faster, due to less time scrolling. Though print studies find faster reading at medium line lengths, reading text on a computer screen is really exhausting, and slower than print, so there is no benefit to short lines except at large font sizes. Reading from a computer screen is different from printed media." But there are newer monitors, so this has to be tested again. There are also physical constraints. Diff programs seem like a limiting case - on my monitor I can fit 2 108-character texts at the default font size side-by-side along with a space in the middle and the taskbar. Rounding this down to 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt / 24px I can fit 97 characters, while a little less (17.25pt / 23px) fits 102 characters. The standard is 80 characters but monitors are wider now than they were in teletype days, so again 100 seems plausible.
* Line-breaking (Knuth-Plass, hanging indents on soft linebreaks, etc.): This can really only be tested by finding long lines of code and asking what line-breaking placement is most readable;
* A widow is when a paragraph line-breaks and leaves a word on its own line at the end. An orphan is when a paragraph line-breaks across a page and leave less than a full line on the next page. Widow and orphan lines are commonly cited as decreasing legibility, but I didn't find any formal studies. I think people get used to bad line breaking. Also most code is viewed on a computer rather than printed out. So what should really be considered is code that doesn't fit on one screen.


Reading code top-to-bottom, left-to-right makes sense. So definitions should be on the left, blocks indented in, and lines themselves should read left to right. So Ruby's statement modifiers ``X if Y`` are a bad idea because the ``if Y`` is easy to miss when scanning control flow.  But operators like ``a = b xor c`` are fine because the assignment ``a =`` is clear and if the value of the expression matters you're reading the whole line anyway and can parse it in your head.

Unicode can improve legibility when the character is standard (e.g. θ for angles), but generally long names like ``Optimiser(learning_rate=...)`` are more readable than ``Optimiser(η=...)``. Programmers have neither the time nor the inclination to learn a new character set and accompanying set of conventions.

When the convention is established, short names are clearer than long names. Writing ``(+) { augend = 1, addend = 2 }`` is less clear than the simple ``1+2`` - the long names are not commonly used. But it is arguably still useful to include the long names, e.g. for currying.

A study :cite:`dossantosImpactsCodingPractices2018` found the following conventions were helpful for Java code readability:

* Putting opening braces in a line of their own (C# convention), as opposed to the same line of the statement, improved readability. The extra white space and matching vertical alignment of related curly braces makes blocks clearer. Closing curly braces terminating code blocks should be on their own line, except for secondary paths of execution, e.g.: closing brace of if statements followed by an else; closing braces of try statements followed by a catch.
* 80 character line lengths were helpful, although they did not test other lengths such as 100 or 120
* Each statement should be in a line of its own; do not separate multiple statements by a ‘‘;’’ in a single line.
* Use import clauses instead of qualified names to reference names in code.
* Frequent calls to sub-properties of class member properties should be made by storing a reference to that sub-property, avoiding multiple statements containing long chains of objects and sub-properties;
* Identifier names should use dictionary words.

These conventions were inconclusive:

* Grouping instructions using blank lines was 50/50. Some thought the blanks broke the flow, others liked it.
* On indenting 2 spaces vs 4 spaces, 58% preferred the 2 spaces.
* Nesting conditionals more than three levels deep was considered by some to be easy to read and clearer than using a complex condition. But the majority preferred refactoring to an ``else if`` chain.
* Using variables to store intermediate parts of long logical expressions was vetoed by 61%. It is only useful if that intermedate expression has a meaningful name and purpose or the expression is repeated. Otherwise it adds clutter, and you are better off just writing the complex expression.

:cite:`bauerIndentationSimplyMatter2019` studied indentation with eye tracking and found no statistically significant difference between 0,2,4,8 spaces. Looking at their graphs 4 spaces does seem to be a sweet spot though.

Another study :cite:`buseMetricSoftwareReadability2008` identified factors for readability, in decreasing order of significance:

* fewer identifiers per line
* shorter lines (characters)
* fewer '(' '{' '.' ','
* less indentation (preceding whitespace)
* fewer keywords
* more blank lines
* lower maximum occurrences of any single identifier
* shorter maximum length of identifier
* lower maximum occurrences of any single character
* more comments
* fewer '='  numbers spaces '==' '<' '>' 'if' 'for' 'while'
* higher number of '+' '*' '%' '/' '-'

They constructed several models using these factors, mainly a Bayesian classifier, all of which predicted average readability scores better than the original human raters. But the model is not public.

An Ada guideline is "language constructs which do not express similar ideas should not look similar".
Proportional fonts
------------------

For prose, a proportional fonts is more readable than monospace because it is denser and hence less eye movement is needed for reading. Spaces between words are easier to see. :cite:`arditiReadingFixedVariable1990` But proportional fonts have not caught on in programming. The main complaint is that identifiers do not line up nicely the way they do with a monospace font.

After reading about elastic tabstops I've come up with my own solution, "tablike spaces". The idea here is to use a proportional font for rendering, but to make the spaces jump to the pixel column they would use if the font was monospaced. So rendering "a bit of text" would render "a" at 0, "bit" at 2 ems, "of" at 6 ems, and "text" at 9 ems, where an em is the width of the widest character in the font.

A more complex algorithm treats the text as a giant table, so "a bit of text" gets split up into 4 cells "a ", "bit ", "of ", "text" which span 2,4,3,4 columns respectively. Then the column widths are calculated using the `auto table layout algorithm <https://www.w3.org/TR/CSS2/tables.html#auto-table-layout>`__ (simplified):

* Set the width of each column to 0.
* For each cell, calculate the width as rendered by the font, and increase the widths of the columns it spans so that together, they are at least as wide as the cell. Widen all spanned columns to be approximately the same.

Yet more complex is to treat it as a constraint problem. The constraints consist of minimum width constraints from the width of the tokens and order constraints that specify which chunks of text are before/after/line up with other chunks. The goal is to minimize the width of the table (sum of column widths), and as a secondary objective make the widths as uniform as possible (lowest standard deviation or absolute deviation). The Cassowary algorithm might work.

The constraint algorithm allows aligning the ends of text by justifying, so e.g. ``foo =`` and ``bar =`` have the identifiers stretched to the same width. But generally it is only the start of the text that needs to be aligned.

TODO: test it out by modifying https://github.com/isral/elastic_tabstops_mono.vsce

The advantage of tablike spaces over elastic tabstops is that the underlying text file looks fine when viewed in a monospaced font. So it's only the visual presentation that changes, hence it can be used on a team.

Typeable
--------

Per `Coding Horror <https://blog.codinghorror.com/we-are-typists-first-programmers-second/>`__, coding is just typing. So one amazing idea is to make the syntax easy to type. Here is ChatGPT's list of what makes text hard to type:

* Extremely Long, Complex, and Rare Words: Use long words or those with complex spelling patterns (like words with silent letters, double letters, or unusual letter combinations). Texts that frequently alternate between left-hand and right-hand typing can be more difficult, as they require greater coordination. Strings of the same letter or very similar keystrokes in succession can cause mistakes, especially if they're fast. Use highly specialized jargon, technical terms, or colloquialisms from fields like quantum physics, advanced mathematics, law, or philosophy that the typist is unfamiliar with. Words from scientific disciplines, like chemistry or medicine, can be incredibly lengthy and complex.
* Frequent and Unusual Punctuation: Incorporate a high frequency of commas, periods, semicolons, and other symbols. Also consider including unusual punctuation marks, such as interrobangs (‽), pilcrows (¶), and uncommon diacritics.
* Randomized Capitalization and Letter Case Switching: Randomly capitalize letters or switch between upper and lower case in a non-standard pattern.
* Numeric and Symbolic Content: Mix in long sequences of random numbers and symbols in unpredictable patterns that are hard to memorize and type accurately.
* Multiple Languages with Different Scripts: Integrate words from languages that use different scripts, such as Arabic, Russian (Cyrillic), Mandarin (Chinese characters), or Hindi (Devanagari). This requires the typist to constantly switch keyboard layouts.
* Palindrome and Anagram Overload: Create sentences that are palindromes or consist largely of anagrams, which are challenging to read and type.
* Syntax Scrambling: Write sentences with deliberately scrambled and awkward syntax or grammar that still somehow maintain grammatical correctness, making them difficult to process. Long sentences with multiple clauses, especially if they include subordinating conjunctions and relative pronouns, can be challenging to type quickly and accurately.
* Alliteration and Tongue Twisters: Use sentences that are full of alliteration or are tongue twisters, which are tricky to type quickly.
* Esoteric Literary References: Include obscure literary references or quotes in archaic language, which are hard to understand and type without prior knowledge.
* Incorporate ASCII Art or Unicode Art: This would require precise spacing and character placement, vastly increasing the difficulty.

I have seen pretty much all of these in real-world programs, besides maybe the palindromes. Many programmers have written about how their keyboards screwed up their hands and their livelihoods with RSI and other such conditions.

Familiarity
===========

Let's say one design has readability arguments for it, and another design has only familiarity on its side. For example, using single = for both assignment and comparison is much less confusing for newbies, as they are not used to writing ``==`` like in C, and often forget to double it. But of course ``==`` for comparison has been adopted by many languages.



 language designers of the future are implored to pick the former to stop propagating the same language design mistakes further and further into the future.

Language designers should give careful thought to how strange their langauge is, and choose the right amount to accomplish what they’re trying to accomplish.

Therefore, it’s best to treat familiarity as a tie-breaker: to be used sparingly, only when the pros and cons of different design options have been fully explored, and it has been determined that no design has an edge above the other.

But if

 the benefits of familiarity are fleeting, because once your language becomes standard people will be familiar with it anyway. This conflicts with

As Randomo shows, the choice of characters for operators is arbitrary. Using familiar syntax at least benefits existing programmers, while new programmers will be confused regardless.

Hungarian notation
==================

Hungarian notation puts abbreviated type annotations in variable names, so humans can check that the types are correct. But the compiler already checks declared types, automatically and much more thoroughly. So in the end it is noise. Mathematicians do use single-letter variables with subscripts, but these do not encode types, they are just abbreviations - e.g. ``x`` stands for "first coordinate". Per `Stroustrup <https://www.stroustrup.com/bs_faq2.html#Hungarian>`__ it is "a maintenance hazard and a serious detriment to good code. Avoid it as the plague."

Keep syntax and semantics separate
==================================

Although both syntax and semantics are essential to a programming language, they are not on an equal footing. A given semantics may have many valid syntaxes, but there is generally only one semantics for a given construct (otherwise, it would be a different construct). Most considerations involve both syntactic and semantic components. Consider some examples:

* Allowing both ``1+2`` and ``1.0+2.0``: One "solution" is to use syntactically different operators, such as ``+`` and ``+.`` in OCaml. But actually allowing the same syntax to mean different things requires a semantic solution, such as typeclasses, overloading, or union types.
* Allowing ``a.b()`` for a method defined outside a class: Again, there is a "solution" of writing ``b(a)``. But actually allowing this requires a semantic solution of extension methods or implicit classes, which Flix mentions requires a lot of semantic baggage and can incur unexpected performance penalties.
* Lambdas: There is a "solution" of passing a function pointer and closure struct. But actually allowing this in C++ required (just in the standard) 28 lines of BNF (~1/2 page) plus a note for syntax, and 6.5 pages plus 2 mentions for semantics.

The addition example makes it clear that it's pretty hard to design any sort of syntax without taking into account the semantics. You may want to emulate the syntax of another language (or mathematics, in this case), but it may not be possible. So Stroscot takes the approach of "form follows function": first design the semantics, then design the syntax based on that. Thus there is a clear dependency relationship, rather than them being separate.

Of course, the distinction is more fuzzy in practice. Papers define a syntax along with their semantics, and for clarity, we use that syntax when describing the semantics in the commentary. Similarly, the semantics of extension methods were proposed in order to allow a certain syntax. But formally speaking, the language commentary is only considering semantics. The syntax will be decided upon by a survey describing the abstract semantics of each language construct, with all preconceived ideas for syntax removed from the descriptions as much as possible.

Another question is where to draw the line of syntactic sugar vs. language feature. Per `Wikipedia <https://en.wikipedia.org/wiki/Syntactic_sugar>`__, syntactic sugar is a shorthand for an operation canonically expressed in a more verbose form. I would say that syntactic sugar can be clearly identified by being able to write the translation as an unconditional function or macro, like the parser combinator ``some x = x <|> some x`` or the for loop:

::

  for ( init; condition; update) { body } = init; go
    where
      go | condition = body; update; go
         | otherwise = return ()

Anything more complex is a language or library feature (I don't distinguish the language and the standard library). So for example, lambdas are a language feature, not because they introduce new syntax for lambda expressions, but because the syntax for calling a lambda overlaps with the syntax for calling a function.

Filenames
=========

It seems OS's generally support long extensions like ``.carbon`` so using a long extension like ``.stroscot`` seems fine for now. Maybe it will be shortened later to ``.sct`` or something. Filenames should be portable across platforms, i.e. in the intersection of each platform's valid names.

Unicode
=======

Per Rust, non-English beginner tutorials are more friendly if they can use localized variable names for familiarity. Identifiers seem to be the limit though. Per `quotes from Y studios <https://ystudios.com/insights-passion/codelanguage>`__,  localized keywords are very tricky to implement, and often don't work well with the rest of the syntax. Localized grammars and word order are even more tricky and also really confusing if you don't know they're in use. But there are experiments like ChinesePython that have seen limited interest. It is a lot of work to fully localize a language:

* parser - keywords/reserved words, grammar, word order
* error messages, warnings, diagnostics
* standard library method names and strings
* documentation

For Stroscot, at a minimum, comments in localized scripts should be supported. But `lots of languages <https://rosettacode.org/wiki/Unicode_variable_names>`__ support Unicode variable names too, so also seems good. Anything past that will be DSL territory and not part of the language proper.

TODO: see if there are any more Unicode guidelines relevant to writing a programming language parser

Usability
---------

Unicode character input still has no standard solution. Copy-pasting from websites or a cheat file is simple but it is too tedious to use frequently. Other methods include a language-specific keyboard, OS input methods like Character Map, or editor input methods like ``\name<tab>`` in Jupyter, `extensions <https://marketplace.visualstudio.com/items?itemName=brunnerh.insert-unicode>`__ for VSCode, or ``Ctrl+x 8 Enter`` in Emacs. Generally it seems there is no shortage of solutions and motivated people will put in the effort to find a good IME as required. It is really an editor problem, not a PL problem.

Unicode itself is quite complex and people can get confused by invisible characters, different width spaces, bidirectional text, and lookalike characters. Compiler warnings can reduce the chance of confusion.

Language fragmentation
----------------------

People aren't omniglots, so using multiple languages will cause library fragmentation. Past introductory tutorials that write throwaway code, it makes sense to use a common language. Which one though?

Per `Wikipedia <https://en.wikipedia.org/wiki/List_of_languages_by_total_number_of_speakers>`__ English has the most speakers, 1.452 billion, while the next, Standard Chinese, has 1.118 billion, and the next (Hindi) less than half English. If we count "second language" liberally, English is as high as 2 billion while Standard Chinese is only 1.5 billion, so the gap only increases slightly. And calculating growth rates from `2021 <https://en.wikipedia.org/w/index.php?title=List_of_languages_by_total_number_of_speakers&direction=prev&oldid=1073408213>`__ and earlier, English increased by 7.7%-9.8%/year while Chinese has remained mostly steady at -0.1% to 3.3%/year. Per `this WP page <https://en.wikipedia.org/wiki/Languages_used_on_the_Internet>`__ English websites are 61.1% English, 1.7% Chinese, while internet users are 25.9% English, 19.4% Chinese. The number of Chinese websites is probably skewed low because most Chinese content is on social sites rather than independent sites, and the firewall makes it hard to index. Still though, across all of these statistics, there is a clear pattern of English being first.

Choosing Standard Chinese also has political problems since the speakers are mainly "native" speakers in China that have been indoctrinated via the CCP systematically targeting ethnic minorities and forcing them to learn Standard Chinese in place of their original dialect. In contrast English is mainly a second language - its speakers are spread across many countries, and for the most part learn it as a course in school supplemented with additional voluntary self-education.

Also Chinese is `just plain hard <http://pinyin.info/readings/texts/moser.html>`__ to learn and remember. Per that article it takes 7-8 years to learn 3000 Chinese characters but half that time to learn a comparable number of French or Spanish words. Then there is the `character amnesia <https://en.wikipedia.org/wiki/Character_amnesia>`__ problem where people can read the character just fine but forget how to write it by hand, only remembering the pinyin Latin-based transcription.

So English it is.

Symbol overuse
--------------

There are several reasons to allow the use of Unicode mathematical symbols in Stroscot, as opposed to requiring lexical (word-based) identifiers:

* First is that many mathematical symbols are widely recognized. Programming languages have taken arithmetic syntax directly from mathematics, with good effect, so it makes sense to allow other widely recognized symbols, such as the summation sign, set union, dot product, constant pi, theta/phi for angles, floor, ceiling, and infinity.

* Second is that a symbol may be notably used in a specific domain. For example, it makes sense to allow matching the symbols and notation of a popular paper or textbook when transcribing an algorithm. There are many custom operators such as discrete difference and convolution.

* Third is that it can make code more concise and hence more readable. A lexical operator is generally several characters, so its repeated use in an expression may create a long line, requiring line breaks. A new symbol is more concise hence faster to read than the lexical version, if one knows the meaning of the symbol. Examples include a circle symbol for specifying circles, or a music note symbol for defining chords. Of course one does have to learn the meaning, so it introduces a learning barrier.

Stroscot's user-defined syntax is flexible enough to create symbolic operators if desired. But compare this example of computing the prime numbers less than ``R`` in APL vs. a Haskell lexical+prefix style:

.. code-block:: apl

  T←1↓⍳R
  (~T∊T∘.×T)/T

.. code-block:: haskell

  T = drop 1 (count R)
  scan (not (isElementOf T (tie 0 (*) T T))) T

The learning barrier is definitely real - IMO the Haskell style is much easier to read. The English words give many more clues as to what is happening. Although the number of APL hieroglyphs is not comparable to Chinese's thousands of ideograms, it seems likely that APL suffers from learnability issues similar to Chinese and is harder to learn than a language with lexical identifiers because it does not have a phonetic basis. Thus it does not make sense to uniformly adopt symbols for all operators as was done in APL. There is definitely a balance between concision and clarity. Hence, although Stroscot allows Unicode symbols, it does not encourage or require their use. I like to think that programmers have good taste and will avoid symbol overuse.

To implement the "symbols not encouraged or required" rule, the constraint on Stroscot's standard library is that every symbol should have a corresponding lexical operator, and the library code should always use the lexical version, to avoid "monkey see monkey do". Maybe some symbols can be encouraged and the rules ignored for those symbols, but that would require a standardized Unicode input method. The documentation should have a symbol dictionary showing the lexical and symbolic versions of all operators in the standard library, for easy searching and copy-paste. The dictionary should also document what the symbol means and its usage and pronunciation. As far as availability, unambiguous widely-used symbols can be available in the prelude. Custom operators can be exposed in an appropriate module - either the main module for some functionality if it is expected that end-users will use the symbol, or an internal or DSL module if the symbol is not expected to be used.

Encoding
--------

The Unicode Consortium has put in a great deal of effort to create a universal character set that is compatible with legacy systems. It would be foolhardy to ignore their work and attempt to create a competing incompatible standard. Considering Unicode formats, UTF-8 has 97.8% market share on the web (per Wikipedia), and has been adopted by many programming languages. Its variable-width encoding represents ASCII transparently, making English identifiers and markup characters easy to manipulate. It is thus the natural choice for input encoding.

There are other formats, like UTF-16, GB 18030, and SCSU/BOCU. UTF-16 is pretty much a legacy format since it cannot fully represent a Unicode character in one code unit and requires double the space of UTF-8. UTF-32 is even more inefficient and is simply not suitable for storage on disk. GB 18030 represents CJK somewhat efficiently but is not used much outside China and even within China has only ~5% market share (per `W3Techs <https://w3techs.com/technologies/segmentation/sl-cnter-/character_encoding>`__), although there are some popular sites using it. Regarding SCSU/BOCU, per `experiment <https://web.archive.org/web/20041206080839/http://www.cs.fit.edu/~ryan/compress/>`__, it seems gzip/bzip provide better compression. The difference between compressibility of encodings is on the order of 1% for bzip but for some gzip examples, converting to SCSU as a preprocessing step saved 25% over UTF-8. Per `FAQ <http://www.unicode.org/faq/compression.html>`__, SCSU/BOCU are mainly for avoiding the overhead of Unicode vs. legacy encodings. So overall, it doesn't seem to be worth supporting anything besides UTF-8 as the input encoding.

How dangerous is this assumption? Well, many systems support non-UTF-8 encodings by first running ICU and transforming the encoding to UTF-8, such as PostgresSQL. This would not be hard to add, if for some unforeseeable reason we suddenly had the need to support non-UTF-8 encoding. Although, there is a performance hit for transforming on the fly. We could alternatively design an abstract string library that allows manipulating data of various encodings in a uniform manner, most likely as a sequence of Unicode codepoints. But again there is likely some overhead, as the decoding and the parsing have to be written as coroutines. Likely, to get the same performance as UTF-8, we would have to fork the parser and spend some time tweaking. With suitable abstractions probably most of the code could be shared with the UTF-8 parser. So, at the end of the day, it is just some performance, some hacking, and the overall design does not really depend on assuming the encoding - it just makes the initial implementation a bit easier.

NFC
---

NFC solves the issue of having the same font grapheme but different codepoint encoding, like A + combining acute accent vs the precomposed character "latin capital letter a with acute". NFC is used by 98% of the web and a fair amount of software automatically normalizes input to NFC (e.g. web browsers). Also per `Unicode Normalization FAQ <http://www.unicode.org/faq/normalization.html>`__ "NFC is the best form for general text." It also seems that the unstated opinion of the Unicode Consortium is that text that cannot be NFC'd does not count as "Unicode". When there was an issue with NFC breaking `Biblical Hebrew <https://www.unicode.org/mail-arch/unicode-ml/y2003-m06/0423.html>`__ the solution was to change the input (inserting joiners) rather than modifying NFC.

So it seems correct to soft-require input to be NFC normalized. This might annoy someone somewhere, but they can work around it by putting in joiners, like Biblical Hebrew had to do. We cannot hard-require because `per someone <https://github.com/rust-lang/rfcs/pull/2457#issuecomment-395488644>`__ there exist some Vietnamese keyboards that produce combining characters not in NFC normal form.

NFC also means that unnormalized strings or raw binary data can't be included in files directly. But keeping those in separate files or encoding the bad bytes as hexadecimal seems fine.

NFKC
----

NFKC is often brought up as an alternative/extension of NFC. For example `Python <https://peps.python.org/pep-3131/>`__ uses NKFC for identifiers, and Go similarly has a `proposal <https://github.com/golang/go/issues/27896>`__ to use NFKC.

There are two choices for using NFKC, requiring input to be NFKC or applying NFKC to the input. Python only applies NFKC, so `the following <https://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/QcRz1gdfAQAJ>`__ is a valid Python program

.. code-block:: python

  def 𝚑𝓮𝖑𝒍𝑜():
    try:
      𝔥e𝗅𝕝𝚘︴ = "Hello"
      𝕨𝔬r𝓵ᵈ﹎ = "World"
      ᵖ𝖗𝐢𝘯𝓽(f"{𝗵ｅ𝓵𝔩º_}, {𝖜ₒ𝒓lⅆ︴}!")
    except 𝓣𝕪ᵖｅ𝖤𝗿ᵣ𝖔𝚛 as ⅇ𝗑c:
      𝒑rℹₙₜ("failed: {}".𝕗𝗼ʳᵐªｔ(ᵉ𝐱𝓬))


  # snippet from unittest/util.py
  _𝓟Ⅼ𝖠𝙲𝗘ℋ𝒪Lᴰ𝑬𝕽﹏𝕷𝔼𝗡 = 12
  def _𝔰ʰ𝓸ʳ𝕥𝙚𝑛(𝔰, p𝑟𝔢ﬁ𝖝𝕝𝚎𝑛, ｓᵤ𝑓𝗳𝗂𝑥𝗹ₑ𝚗):
    ˢ𝗸ｉ𝗽 = 𝐥ｅ𝘯(𝖘) - ｐr𝚎𝖋𝐢x𝗅ᵉ𝓷 - 𝒔𝙪ﬀｉ𝘅𝗹𝙚ₙ
    if sｋi𝘱 > _𝐏𝗟𝖠𝘊𝙴H𝕺Ｌ𝕯𝙀𝘙﹏L𝔈𝒩:
      𝘴 = '%s[%d chars]%s' % (𝙨[:𝘱𝐫𝕖𝑓𝕚ｘℓ𝒆𝕟], ₛ𝚔𝒊p, 𝓼[𝓁𝒆𝖓(𝚜) - 𝙨𝚞𝒇ﬁx𝙡ᵉ𝘯:])
    return ₛ

  if _︴ⁿ𝓪𝑚𝕖__ == "__main__":
    𝒉eℓˡ𝗈()


If we required the input to be in NFKC it would have to look like:

.. code-block:: python

 def hello():
  try:
    hello_ = "Hello"
    world_ = "World"
    print(f"{hello_}, {world_}!")
  except TypeError as exc:
    print("failed: {}".format(exc))

  # snippet from unittest/util.py
  _PLACEHOLDER_LEN = 12
  def _shorten(s, prefixlen, suffixlen):
    skip = len(s) - prefixlen - suffixlen
    if skip > _PLACEHOLDER_LEN:
      s = '%s[%d chars]%s' % (s[:prefixlen], skip, s[len(s) - suffixlen:])
    return s

  if __name__ == "__main__":
    hello()

Meanwhile with NFC the variable names would have to be consistent and built-in names could not be transformed, so a program could look like:

.. code-block:: python

  def 𝚑𝓮𝖑𝒍𝑜():
      try:
          𝔥e𝗅𝕝𝚘︴ = "Hello"
          𝕨𝔬r𝓵ᵈ﹎ = "World"
          print(f"{𝔥e𝗅𝕝𝚘︴}, {𝕨𝔬r𝓵ᵈ﹎}!")
      except TypeError as ⅇ𝗑c:
          print("failed: {}".format(ⅇ𝗑c))

  # snippet from unittest/util.py
  _𝐏𝗟𝖠𝘊𝙴H𝕺Ｌ𝕯𝙀𝘙﹏L𝔈𝒩 = 12
  def _𝔰ʰ𝓸ʳ𝕥𝙚𝑛(𝘴, p𝑟𝔢ﬁ𝖝𝕝𝚎𝑛, ｓᵤ𝑓𝗳𝗂𝑥𝗹ₑ𝚗):
      sｋi𝘱 = 𝐥ｅ𝘯(𝘴) - p𝑟𝔢ﬁ𝖝𝕝𝚎𝑛 - ｓᵤ𝑓𝗳𝗂𝑥𝗹ₑ𝚗
      if sｋi𝘱 > _𝐏𝗟𝖠𝘊𝙴H𝕺Ｌ𝕯𝙀𝘙﹏L𝔈𝒩:
          𝘴 = '%s[%d chars]%s' % (𝘴[:p𝑟𝔢ﬁ𝖝𝕝𝚎𝑛], ₛ𝚔𝒊p, 𝘴[len(𝘴) - ｓᵤ𝑓𝗳𝗂𝑥𝗹ₑ𝚗:])
      return 𝘴

  if __name__ == "__main__":
      𝚑𝓮𝖑𝒍𝑜()

Python's version where the variables do not have to be visually identical is really confusing. The NFKC input is too restrictive. IMO the NFC wins on both readability and flexibility.

In the Unicode TRs, NFKC usually is used in conjunction with case folding. In particular, the Unicode standard 3.13 R5 defines the mapping toNFKC_Casefold which case folds, normalizes, and removes default ignorable code points, and this operation is recommended for matching identifiers case-insensitively. Similarly `TR36 <https://www.unicode.org/reports/tr36/#Recommendations_General>`__ recommends processing identifiers by applying NFKC_Casefold. So NFKC doesn't make a lot of sense since Stroscot is case-sensitive. Many have `suggested <https://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/ki8XN66iAQAJhttps://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/ki8XN66iAQAJ>`__ that Python made the wrong choice when it picked NFKC because Python is case-sensitive.

Let's look at what NFKC actually does. Compared to NFC, it applies transformations with non-empty `Decomposition_type <https://www.unicode.org/reports/tr44/#Character_Decomposition_Mappings>`__, which are as follows:

* font: 𝐀 (bold mathematical A) to A, ℍ to H
* super/sub: ² to 2, ᵢ to i
* circle: ① to 1
* fraction: ¼ to 1/4
* square: ㍌ to メカ◌゙トン
* small: ﹛ to {
* initial, isolated, medial, final (Arabic): ﯕ to ڭ
* wide: Ｂ to B
* narrow: ﾁ to チ
* vertical: ︘ to the LTR variants like 〗
* noBreak: ‑ (non-breaking hyphen) to ‐ (hyphen)
* compat: a grab-bag that does many things:

  * decompose ligatures and compound characters like æ to ae, ﬁ to fi, ℃ to °C, and ⑿ to (12)
  * changes µ MICRO SIGN to μ GREEK SMALL LETTER MU and similarly various other compatibility symbols to alphabetical letters
  * changes kanxi to unified CJK (but not CJK compatibility ideographs)
  * changes en/em spaces to normal spaces

TR31 specifically recommends excluding font transformations (1194 characters, 32% of NFKC) to allow mathematical notation. The superscript/subscript transforms also `confuse people <https://stackoverflow.com/questions/48404881/unicode-subscripts-and-superscripts-in-identifiers-why-does-python-consider-xu>`__ and seem to be unwanted. For Go, bcmills says superscripts and subscripts are 'cutesy', which seems to be an acknowledgement of the fact that they should not be erased. Similarly circle, fraction, square, and small (collectively 12% of NFKC) look so different that they will confuse people as to why they are considered equivalent.

The symbol and ligature transformations in compat (20% of NFKC) do seem useful. Python `apparently <https://mail.python.org/pipermail/python-3000/2007-May/007995.html>`__ went with NFKC because they were worried about confusing ligatures, specifically ﬁnd vs find (the first using the U+FB01 LATIN SMALL LIGATURE FI character). In VSCode the fi ligature shows up compressed into one fixed-width space so is visibly different from the non-ligature version, but in proportional fonts this is indeed a problem. The Go issue mentions confusing micro and mu, which per Wikipedia look identical in most fonts, although some fonts do distinguish them. noBreak is also useful. However, since the main goal is to avoid confusion, the confusable detection algorithm seems more appropriate.

wide/narrow/vertical/Arabic do look clearly different in my fonts, but the characters are intended only to support legacy character encodings. They could be transformed or not, but if we did implement a partial NFKC transform then transforming them away would probably be best. (`CHARMOD <https://www.w3.org/TR/charmod-norm/#canonical_compatibility>`__)

Overall, given that the standard specifically recommends excluding font transformations, it should be clear that the stock NKFC transform isn't appropriate. Using a reasonable subset of NFKC (compat,noBreak,wide,narrow,vertical,Arabic), we're still barely using half, so at best we could say we are using "NFC with most decompositions from NFKC". The base is still NFC and it's just monkeying up the encoding. And the main benefit of such a partial NFKC transform is avoiding ligature confusion, but we also get that if we implement confusable detection. And there don't seem to be any other benefits to NFKC.

Confusables
-----------

There is an alternative to NFKC, namely the TR39 `confusable detection <https://www.unicode.org/reports/tr39/#Confusable_Detection>`__ transformation. `Rust <https://github.com/rust-lang/rfcs/blob/master/text/2457-non-ascii-idents.md>`__ uses this. The dataset `contains <https://www.unicode.org/Public/security/latest/confusables.txt>`__ conversions of:

* micro to mu, and other standardization of symbols (including kanxi and CJK compatibility ideographs to unified CJK)
* 1 to l (one to ell), 0 to O (zero to oh), and other similar looking characters
* ligatures like ffi to their expansion
* parenthesized expressions like ⑵ to (2)
* ℍ to H and other standardization of font styles
* noBreak to normal, various spaces to normal
* fullwidth to normal, vertical to similar characters like ︵ to ⏜
* some amount of initial, isolated, medial, and final Arabic NFKC normalizations
* no narrow, square, superscript, subscript, circle, or fraction NFKC mappings

Due to the standardization of similar looking characters the confusable transform is actually larger than NFKC, 6311 vs 3675. But the transform makes a lot more sense for detecting similar-looking identifiers. The only unwanted transformations are the font styles which can be excluded from confusable detection just like they can be excluded from NKFC.

Also per `dscorbett <https://github.com/rust-lang/rfcs/pull/2457#discussion_r192605996>`__ the confusable transform should also be extended to remove `default ignorable code points <https://unicode.org/reports/tr44/#Default_Ignorable_Code_Point>`__ (`consisting <https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt>`__ of combining grapheme joiner, zero width space, hangul filler, and 146 other invisible characters, plus 256 variation selectors and 3769 reserved). Per the `Variation Sequence FAQ <http://unicode.org/faq/vs.html>`__ variation sequences are morally equivalent to code points, in that they distinguish different glyphs, but they were unable to be assigned a new codepoint because an existing codepoint was considered to be "clearly the same character". This includes visually distinctive alterations such as rotating Egyptian Hieroglyphs 90 degrees or black-and-white vs color emoji, as well as less noticeable ones like adding serifs. From testing with my fonts (𓂑 vs 𓂑︀, ⊓ vs ⊓︀, 齋 vs 齋󠄁, ≩ vs ≩︀, ⛺︎ vs ⛺️) and various `open <https://github.com/w3c/csswg-drafts/issues/1710>`__ `bugs <https://gitlab.gnome.org/GNOME/pango/-/issues/206>`__ it seems variations besides emoji are not supported well and mostly fall back to the base character, so removing the selectors is appropriate for confusable detection. This could be revisited if more fonts start including variations or the `CSS font substitution logic <https://drafts.csswg.org/css-fonts-3/#cluster-matching>`__ that attempts to preserve variations is implemented. `As of 2017 <https://github.com/harfbuzz/harfbuzz/issues/515#issuecomment-317932409>`__ font substitution for variations doesn't work on Chrome, Firefox, or Word.

So overall the approach is "confusable detection with font variants distinguished and default ignorable code points ignored". Since the focus for developing the confusable database was on covering characters with Identifier_Status=Allowed for standard OS fonts, it may be incomplete. But it's the best production-quality database available, and Unicode claims to accept updates, and if not it's not too hard to fork.

There are also research projects. `ShamFinder <https://arxiv.org/pdf/1909.07539.pdf>`__ provides a database SimChar similar to the confusables database. Its generation is based solely on Unifont so it misses many homoglyphs from other fonts. The pixel metric adds extra pairs such as accents, e vs é, which IMO are not going to confuse anyone. The database is publicly available at `GitHub <https://github.com/FlowCrypt/idn-homographs-database>`__, but not the code used to generate it. `PhishGAN <https://arxiv.org/abs/2006.13742>`__ generates vectors from images and finds likely homoglyph identifiers, but was trained on a small identifier list, is Arial and Times only, trains over the whole identifier, and is not publicly available. But an ML approach which breaks up identifiers into fixations and compares these using a human visual perceptual model could theoretically be more accurate than a confusables database; getting it performant enough would require some specially crafted perceptual hash functions. This approach catches multicharacter homoglyphs like "vv" vs "w" (of course in a monospace font these are clearly distinguished by width). But, overall, neither of these approaches is ready for prime time with further work.

The transform generates a "skeleton" that can be compared with other skeletons to see if two identifiers are confusable. Per Rust the implementation should use the NFC form for compilation but hash the skeleton and generate an optional warning if the usage of an identifier is confusable with another identifier in scope. We could add an additional step that compares the actual identifiers and computes a confusion probability, but the skeleton alone is generally good enough. The warning can be turned off on a per-file or per-project basis if the user doesn't care or on a per-grapheme basis if the user is using a font that clearly distinguishes the confused characters. But most users will appreciate the warning and fix their code to use clearer identifier names.

Also for unresolvable identifiers we should compute an edit distance score between skeletons to find likely typos.

Confusable detection generally prevents homoglyph attacks using identifiers, although homoglyph attacks are difficult to exploit to begin with as the duplicate definitions are visible. It is still possible to use zero-width characters or homoglyphs in strings or comments. Comments have no effect. With strings a comparison can fail, but the weird characters may be desired. One possibility is a warning with recommendation to replace with an escape sequence.

TR31
----

"UAX #31 Unicode Identifier and Pattern Syntax" is often brought up. I looked at `revision 37 <https://www.unicode.org/reports/tr31/tr31-37.html>`__, Unicode 15. There is a lot of background material and examples but only 7 actual "requirements", a misleading name because some of them are mutually exclusive. In particular the normalization requirements (Section 5, R4-R7) are a mess. They are worded to allow NFKC or NFD in R4, but per the first paragraph "Generally if the programming language has case-sensitive identifiers, then NFC is appropriate; whereas, if the programming language has case-insensitive identifiers, then NFKC is more appropriate". Furthermore NFKC is generally applied in combination with casefolding with the transformation toNFKC_Casefold. So there are really only two normalization algorithms, NFC and toNFKC_Casefold. Also they put the definition of XID_Start/XID_Continue in R5/R7, even though the use of Start/Continue is defined in R1. So I have modified the text of the requirements here accordingly.

* R1-2. Default Identifiers: Declare a precise specification of identifiers. Advised is R1-1, which is to use the rule ``<Identifier> := <Start> <Continue>* (<Medial> <Continue>+)*`` for identifier syntax where Start/Continue are defined by XID_Start/XID_Continue and Medial is empty.

* R1a. Restricted Format Characters: Restrict the use of ZERO WIDTH JOINER and ZERO WIDTH NON-JOINER characters to the following contexts:

  * A1. /$LJ $T* ZWNJ $T* $RJ/
  * A2. /$L $M* $V $M₁* ZWNJ $M₁* $L/
  * B. /$L $M* $V $M₁* ZWJ (?!$D)/
  * E. emoji sequences defined in ED-17 in `UTS51 <https://unicode.org/reports/tr51/#Emoji_Sequences>`__

  where these classes are defined as follows

  .. code-block:: none

    $T = \p{Joining_Type=Transparent}
    $RJ = [\p{Joining_Type=Dual_Joining}\p{Joining_Type=Right_Joining}]
    $LJ = [\p{Joining_Type=Dual_Joining}\p{Joining_Type=Left_Joining}]
    $L = \p{General_Category=Letter}
    $V = \p{Canonical_Combining_Class=Virama}
    $M = \p{General_Category=Mn}
    $M₁ = [\p{General_Category=Mn}&\p{CCC≠0}]
    $D = \p{Indic_Syllabic_Category=Vowel_Dependent}

* R1b / R2. Omitted, they're just about forward/backward compatibility. Stroscot's language versioning mechanism allows arbitrary changes in a robust manner.

* R3-2. Pattern_White_Space and Pattern_Syntax Characters: Define the precise set of characters interpreted as lexical whitespace in parsing, and the precise set of syntactic characters, such as arithmetic operators, regular expression metacharacters, and `Java collation rule <https://docs.oracle.com/javase/8/docs/api/java/text/RuleBasedCollator.html>`__ syntax characters. All characters besides whitespace or syntactic characters must be available for use in identifiers or literals. Again there is an advisory requirement R3-1 which specifies to use the Pattern_White_Space and Pattern_Syntax properties for whitespace and syntactic characters respectively.

* R4. Equivalent Normalized Identifiers: Specify NFC and characters excluded from normalization, if any. Except for identifiers containing excluded characters, any two identifiers that have the same Normalization Form shall be treated as equivalent by the implementation.

* R5. Equivalent Case-Insensitive Identifiers: Specify that identifiers are transformed by toNFKC_Casefold before processing. Any two identifiers that have the same case-folded form shall be treated as equivalent by the implementation.

* R6. Filtered Normalized Identifiers: Specify NFC and characters excluded from normalization, if any. Except for identifiers containing excluded characters, allowed identifiers must be in the specified Normalization Form.

* R7. Filtered Case-Insensitive Identifiers: Specify that identifiers must be invariant under toNFKC_Casefold. Except for identifiers containing excluded characters, allowed identifiers must be in the specified case folded form. Note: filtering involves disallowing any characters in the set \p{NFKC_QuickCheck=No}, or equivalently, \P{isNFKC}, as well as any characters in the set \p{Changes_When_Casefolded}.

Case restrictions
-----------------

Go's rule is that identifier characters must be letters or digits as defined by Unicode, and exported identifiers must start with an upper-case letter, excluding combining characters and Devanagari. Haskell has a similar type/value distinction. But these sorts of restrictions mean 日本語 cannot be exported, and instead X日本語 must be used.

Generally it seeems that case distinctions only work for English, and are somewhat hard to get right. So we don't put it in the syntax and leave case as a style guideline.

Script restrictions
-------------------

`TR31 <http://www.unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers>`__ proposes lists of allowed and disallowed scripts, and recommends defaulting to allowing new characters in identifiers. IMO this sort of script restriction is not desired by default, e.g. I would want to be able to use the character 𓂸 (Egyptian Hieroglyphs script, on TR31 excluded list) without being forced to specify a flag.

TR39 defines a mixed script restriction, which prohibits text such as Ωmega, Teχ, HλLF-LIFE, and Toys-Я-Us. This could be used to enforce some uniformity on identifier parts, but again seems too restrictive to enable by default. It does prevent some homoglyph attacks, but the confusable detection approach is much more robust.

Bidi attack
-----------

:cite:`boucherTrojanSourceInvisible` proposes a "Trojan Source" bidi attack based on Unicode. The idea is someone copy-pastes from StackOverflow, submits a malicious PR, or just publishes a new project, and the source code looks safe but isn't. In particular the attack is based on bidi overrides, the LRE, RLE, LRO, RLO, LRI, RLI, FSI, PDF, and PDI invisible characters. For example RLI a b c PDI will display as cba, and RLI LRI a b c PDI LRI d e f PDI PDI will display as d e f a b c. This enables near-arbitrary reordering of strings, and even hiding parts of strings by overwriting characters.

Language syntax does not generally allow bidi overrides, but they can show up in comments and strings, and the bidi overrides can obsfuscate which part is the comment or string. For example ``"x"; return`` could look like ``"return x;"`` (early return), ``/* if { */`` could look like ``/* */ if {`` (commenting out), and ``"user // check"`` could look like ``"user" // check`` (stretched string). The overrides are visible in most syntax highlighting and when selecting/navigating through the text, but these cues are easy to miss.

The solution presented in the paper is to ban unterminated bidi override characters within string literals and comments. This prevents reordering across string and comment boundaries, but poses no restrictions on well-terminated uses of bidi reordering. There are more restrictive solutions like completely banning bidi overrides, but the paper's solution is sufficient to prevent the attack, so seems better.

Reduced representation
----------------------

Per Steelman 2A "The full set of character graphics that may be used in source programs shall be given in the language definition." We can already state this is the set of Unicode characters, pretty much.

Steelman 2A has this idea of a reduced representation. "Every source program shall also have a representation that uses only the following 55 character subset of the ASCII graphics: [...]  Each additional graphic (i.e., one in the full set but not in the 55 character set) may be replaced by a sequence of (one or more) characters from the 55 character set without altering the semantics of the program. The replacement sequence shall be specified in the language definition." 55 is too small, it doesn't even include lowercase letters. But there remains the idea of using a small number of characters found on every keyboard. Per ChatGPT this list is::

  A-Z
  a-z
  0-9
  space
  .,;:
  ()[]{}
  +-*/
  =_!?#&%^|\~<>
  "'
  `@$

That's a total of 26+26+10+1+4+6+4+13+2+3=95 characters.

So then we need some sort of compatibility syntax so every program / module using Unicode can also be expressed / interacted with using this reduced character set. I am not sure how to do this in a readable manner; there are short aliases defined for emoji, and HTML entities such as lceil, but they don't cover the whole Unicode space. So for example the only way to input 〖 seems to be ``\u3016`` which of course gives no advice to the reader, or else the full Unicode name "Left White Lenticular Bracket" which is a bit long to use. The full name does have the advantage of being ASCII and so on, so maybe it could work as an interim solution. Perhaps ChatGPT or another language model can create decent aliases for all of Unicode.

Natural language
================

Natural language is a source of many of the basic keywords for language constructs - their natural language menaing helps recall the PL meaning. It also is the source of spacing conventions, such as blanks and indentation, although the hanging indent style used in programming is somewhat unusual for prose.

It is possible to be too inspired by natural language, however. Inform 7, while interesting as a PL, is quite wordy, and the paragraph style is hard to scan through. Natural language also contains a lot of ambiguity, which is reflected in a NL-based programming language as reasonable-looking sentences being ambiguous or failing to parse. Machine learning is probably good enough to get a decent NL parser these days, but the parser will still not be perfect.

Desugaring
==========

One feature of Atomo I liked and thought was cool was that all the syntax was defined with the syntax extension mechanism - even the "core" syntax `was just <https://github.com/Mathnerd314/atomo/blob/master/prelude/core.atomo>`__ defined as rules desugaring to the basic message-sending syntax. I don't really like message-sending as the basic construct, but it should be easy enough to use a Lisp syntax instead. So for example we'd desugar ``a = b`` to ``Assign a b``. Similarly Stroscot should define desugarings for all the other syntactic constructs too. Then we can use this basic Lisp syntax to bootstrap the language, as well as for macro debugging and other tasks. That way the parser is almost completely self-contained as a transformation from sugary code to a basic AST - there are only a few complex interactions like line numbers, inline syntax extension, and macros/DSLs.

Assignment
==========

Steelman 5A. It shall be possible to declare constants of any type. Such constants shall include both those whose values-are determined during translation and those whose value cannot be determined until allocation. Programs may not assign to constants.

Steelman 5D. Procedures, functions, types, labels, exception situations, and statements shall not be assignable to variables, be computable as values of expressions, or be usable as nongeneric parameters to procedures or functions.

Stroscot does allow sets to be assigned to variables, also exceptions and labels and statements (blocks). Procedures and functions are a bit strange; you can store the symbol of the procedure/function, and you can store a lambda, but the rewrite rule itself is stored in a module and you can't really access it alone except as an AST.


`Discussion <https://craftofcoding.wordpress.com/2021/02/19/evolution-of-the-assignment-operator/>`__. Stroscot's assignment syntax is complicated because I want separate initialization (declarative assignment) and reassignment (mutating assignment).

.. list-table:: Comparison
   :header-rows: 1

   * - Language
     - Initialization
     - Reassignment
     - Equality
   * - Mathematics
     - ``=``
     - ``⟹`` or ``=>``
     - ``=``
   * - Algol
     - ``:=``
     - ``:=``
     - ``=``
   * - Fortran
     - ``=``
     - ``=``
     - ``.EQ.``
   * - PL/I
     - ``=``
     - ``=``
     - ``=``
   * - BCPL
     - ``=``
     - ``:=``
     - ``=``
   * - B
     - ``=``
     - ``:=``
     - ``==``
   * - C
     - ``=``
     - ``=``
     - ``==``
   * - APL
     - ``←``
     - ``←``
     - ``=``
   * - R
     - ``<-``
     - ``<-``
     - ``==``
   * - J
     - ``=:``
     - ``=:``
     - ``=``

Looking at precedents, the only languages with distinct initialization and reassignment are B and BCPL, so reassignment should definitely be ``:=``. Then we can either follow mathematical convention and PL/I in making initialization and comparison use the same symbol, or simplify parsing by making equality ``==``. Quorum uses the same symbol and apparently this is what novices expect. :cite:`stefikEmpiricalInvestigationProgramming2013`

Chained assignment
------------------

Chained assignment is an expression like ``w = x = y = z``. The value of ``z`` is assigned to multiple variables ``w``, ``x``, and ``y``. The `literature <http://www.cse.iitm.ac.in/~amannoug/imop/tr-3.pdf>`__ classifies this as "syntactic sugar", so handling it in the parser like Python seems the reasonable solution - C's "the assignment returns the lvalue" semantics seems contrived.

The evaluation strategy differs between languages. For simple chained assignments, like initializing multiple variables, the evaluation strategy does not matter, but if the targets (l-values) in the assignment are connected in some way, the evaluation strategy affects the result. Here C's RTL semantics makes more sense and seems more useful than `Python's LTR <https://docs.python.org/3/reference/simple_stmts.html#assignment-statements>`__ semantics. So a chain ``a := b := 2`` should expand to ``b := 2; a := b`` rather than ``t = 2; a := t; b := t`` .

Chained update with ``:=``, like ``a := b := 2``, seems the most useful to shorten some assignments. Chained ``a = b = 2`` with value semantics doesn't really seem that useful when you could just replace ``a`` with ``b`` in the rest of the expression and save yourself an identifier. Also it conflicts with using ``=`` for comparison, because it can be interpreted as ``a = (b == 2)``.

There is an issue with running I/O multiple times. For example if you need multiple variables with the same value then you would write ``[a,b,c] = replicateM 3 (ref 0)`` rather than using a chain, because a chain would alias to the same variable. Python already has this problem with aliasing for ``a = b = []``, because ``[]`` is mutable, but in Stroscot ``[]`` is pure so this is fine.

Embedded assignment
-------------------

This embeds assignments in expressions, like

::

  a = (b = 1) + (c = 2)

Clearly it conflicts with ``=`` as comparison.

But for chained update it is unambiguous and returning the value would be possible:

::

  a = (b := 1) + (c := 2)

But then statements like

::

  b := 1

would have an unused return value. Maybe this value could be marked as optional somehow.

Binding ambiguity
-----------------

As a syntax ambiguity, there are two different interpretations of assignment, pattern binding and clause definition. The difference:

::

  pair = (1,2)
  (x,y) = pair # binding B

  # B as a pattern binding - defines two clauses
  x = case pair of (x,y) -> x
  y = case pair of (x,y) -> y
  --> x = 1
  # B as a clause definition
  (,) = \x y -> pair
  --> x not in scope, (3,4) reduces to (1,2) reduces cyclically to itself

The pattern binding is more useful in this example than the clause definition. So we have a basic convention for assignments: if the head of the LHS is a constructor symbol then it's a pattern binding. What is a constructor symbol? Well, it's up to the code, defined by the predicate ``isConstructor``. Most symbols are not constructors, so the ones that are constructors are declared with ``isConstructor sym = true``  or the macro declaration ``constructor sym``.

Assignment pattern bindings are irrefutable, meaning they never fail directly and instead define unevaluated variables that will raise pattern matching exceptions when evaluated. But there is an alternative syntax that allows failure as a control operation (from Idris / Inko):

::

  pat = val | <alternatives>
  p

is desugared to

::

  case val of
    pat -> p
    <alternatives>



If a clause does not match, the expression does not reduce - there is no error at all.

In the case of a simple variable ``x = ...`` the definitions coincide - the end result is a clause definition.

Another way to resolve the ambiguity is to use separate syntaxes, e.g. to use ``(x,y) <- pair`` for pattern bindings. But remembering to switch between pattern bindings and clause definitions is tedious.

The explicit syntax does allow defining new reduction rules for constructors. But if overriding basic syntax is desired, ``isConstructor`` can be locally overridden, e.g. if we want a sorted pair:

::

  (x,y) | x > y = (y,x)
    where
      isConstructor (,) = false

Usually it's more natural to use a new symbol, like ``sortedPair (x,y)``, so that the global definition of pairs is not affected.

Constructor discipline
----------------------

Haskell has a division between constructors and functions:
* identifiers starting with lowercase letters are functions, and can only be used with function bindings.
* identifiers starting with uppercase letters are constructors, and assignments of the form ``X a b = ...`` are pattern bindings.

This rule reduces maintainability. If the representation is changed there is no way to replace the dumb constructor with a smart constructor. So instead libraries are littered with boilerplate pseudo-constructors like ``mkThing = Thing`` to get around this syntactic restriction. In fact in :cite:`kahrsNonomegaoverlappingTRSsAre2016` there is a boilerplate trick to turn any TRS into a constructor TRS, by duplicating ``foo`` into a constructor ``Foo`` and a function ``foo``, converting subterms of the original rules to match on constructors, and adding rules that turn stuck patterns into constructors. For example ``k x y = x; s x y z = (x z) (y z)`` turns into:

::

  app (App K x) y = x
  app K x = App K x
  k = K

  app (App (App S x) y) z = app (app x z) (app y z)
  app S x = App S x
  app (App S x) y = App (App S x) y
  s = S

This is pretty verbose but it's curried so it isn't as bad as it could be. For rules like associativity ``x*(y*z) = (x*y)*z`` and distributivity ``x*(y+z) = x*y+x*z`` handling all the stuck pattern rules for symbols ``+`` and ``*`` is a nightmare, and you also have to come up with alternative operator names for the constructors.

So Stroscot follows Pure in not having a constructor discipline. By appropriately setting ``isConstructor = true`` any symbol can be used as a constructor pattern on the left-hand side of an equation. Also any symbol may act as a constructor symbol in a value if it happens to occur in head position in a normal form term, regardless of ``isConstructor``.

There is a general convention for the standard library to use lowercase for potentially reducible expressions or "smart" constructors and uppercase for dumb data constructors. This is to vaguely follow Haskell.

Recursive definitions
---------------------

We want to support recursive definitions, like so:

::

  a = 1 : a

And also shadowing variables, like so:

::

  a = a + 1
  -- interpreted as
  a_new = a_old + 1


It is a bit tricky as we could also interpret ``a = 1 : a`` as a shadowing definition ``a_new = 1 : a_old``. The clever solution is to say it shadows if the variable is already in scope, otherwise it is recursive, but I think it is too clever and will cause confusion. So resolving this probably means a special syntax for something.  Choices:
* ``new a = a + 1`` for shadowing
* ``rec { a = 1 : a }`` for recursive definitions

The ``rec`` I think is too noisy as we would similarly need it for mutually recursive variables::

  rec {
    b = 1 : c
    c = 1 : b
  }

In the recursive version ``c`` can be in scope in the body of ``b`` even though it is defined later. It is just a weird syntax as it is not obvious that b/c are in scope outside the block. OTOH, per the discussion in State, MonadFix is only applicable in certain circumstances so it would make sense to have a noisier syntax so that the error messages are clearer. Also it alerts that ``rec`` is expensive, like ``rec {x = x * 2}`` will solve algebraically to get ``x=0`` rather than just doing the least fixed point.

OTOH recursive functions are really convenient, it would be a pain if ``fact x = if x==0 then 1 else x * fact (x-1)`` needed an extra keyword like ``rec``.

Meanwhile shadowing is banned in Coffeescript, Zig, and Elm, and frowned upon in `C++ <https://rules.sonarsource.com/cpp/rspec-1117/>`__. In `Rust <https://doc.rust-lang.org/rust-by-example/variable_bindings/scope.html>`__ it is common but the assignment syntax uses ``let``. I think requiring a keyword is fine, it is pretty uncommon.

Then there is sequential execution, with the monad stuff, like

::

  a = openFile "a.txt"
  b = openFile "b.txt"

Here it would be an error if the operation for ``a`` tried to refer to ``b`` (because there is no MonadFix continuation instance).

There is also the question of how module declaration work - technically these should be assignments somehow.

Type declarations
=================

::

  a = 2 : s8
  a = s8 2

``a = 2 : s8`` and ``a = s8 2`` seem more logical compared to other choices such as ``a : s8 = 2`` (Swift,Jai - hard to find the = with long types) or ``s8 a = 2`` (C,Rust - overlaps with function definition). The name is simply a syntactic handle to refer to the value; it doesn't have an innate type. In contrast the representation of the value must be specified to compile the program. The second syntax ``s8 2`` is similar to assembler syntax such as ``dword 0``.

`This <https://soc.me/languages/type-annotations>`__ says name should be ahead of type annotation, which only ``s8 a = 2`` breaks. The consistency stuff is not relevant.

In Go they introduced a special declare-and-initialize construct ``myFoo := new(foo.Foo)``, to replace the repetitive ``foo.Foo* myFoo = new(foo.Foo)``. But it doesn't need a special operator, ``myFoo = new(foo.Foo)`` works just as well.

Namespacing
===========

``.`` is preferred to ``::`` because it's shorter and because modules are first-class. And as in Go, no ``->``, always ``.``.

Partial loading
===============

The parser parses as much of the input as possible, but in general only a prefix of the input will be valid. Hence we can load a portion of the file by inserting junk / truncating the input buffer. The compiler will give a warning but the parser should handle it just fine.

Specificity
===========

This might seem overly complicated, but it's based on Zarf's `rule-based programming <https://eblong.com/zarf/rule-language.html>`__. When you're defining lots of rules for a IF game then specifying priorities by hand is tedious.

Comments
========

Steelman 2I "The language shall permit comments that are introduced by a special (one or two character) symbol and terminated by the next line boundary of the source program." This is just the simplest EOL comment, but there are other types.

::

  // comment
  /* multiline
      comment */
  (* nesting (* comment *) *)
   if(false) { code_comment - lexed but not parsed except for start/end }
  #! shebang at beginning of file

Comments allow writing documentation inline with the code. This speeds up development by keeping all the information in one file and avoiding having to jump around. It also encourages a standardized documentation format.

Tool support can be incomplete because there is a lot of freedom in comments. People can put comments in random places and they can be attached to whatever and indented strangely. With such freedom the reformatter will likely mangle some comments, but probably people will just learn not to do that.

The compiler should always ignore comments; they should have no effect on the semantics of the program.

Shebangs
--------

One "comment-like" thing is the shebang. The convention is that if a file starts with the characters "#!", the remainder of the first line will be treated as the name of the interpreter to use (and possibly arguments to be passed to that interpreter). Per `LWN <https://lwn.net/Articles/779997/>`__ the kernel will truncate the line  to 128 bytes, so the interpreter should detect the shebang and reread the first line to get the proper argument list. See Perl for implementation details.

Usually the interpreter is ``/usr/bin/env`` rather than the actual program, because the interpeter's full path must be specified and the user might have installed the program somewhere else. Even NixOS which has minimal files outside ``/nix`` has ``/usr/bin/env`` due to its ubiquity. The arguments are all combined into one string so a shebang like ``#!/usr/bin/env perl -w -T`` will gave an error that the program ``perl -w -T`` is not found, but ``#!/usr/bin/env -S perl -w -T`` will work as expected as will avoiding arguments like ``#!/usr/bin/env perl``. Languages whose main commands default to compilation usually provide a specialized interpreter command like ``language-run`` for use with env.

The shebang, like the byte order mark, can be hardcoded into the file-level syntax and does not need to affect the comment syntax. On RosettaCode the frequency of ``#`` was less than ``//`` and several languages such as D and Gema hardcoded shebang support, so shebangs don't seem to be a factor in deciding the syntax for normal comments.

Zig added and then removed shebang support. `The justification for removal <https://github.com/ziglang/zig/issues/2165#issuecomment-478813464>`__ is all over the place. What I get out of it is that he preferred a package compilation model and believed ``zig run`` sufficed for other cases. To make some counterpoints:

* There is significant demand for avoiding the overhead of a package build tool and edit-compile cycle and just working with the source.
* ``zig run x`` is more typing than ``x``, although the shebang and ``chmod +x`` do add an upfront cost
* If you have a significant command line then the shebang saves you from tediously typing it out every time or creating a separate shell script.

He also talks about environments and versions; you can specify those in the path, in the shebang line, or in the file proper and they all work.

Types
-----

Based loosely on `this <https://www.gavilan.edu/csis/languages/comments.html>`__, syntactically there are 3 types of comments:

* A full-line comment begins with a start indicator at the beginning of the line, and finishes at the end of the line. Used in early fixed-column languages; no longer in common use.
* An end-of-line comment begins with a start indicator anywhere in the line, and finishes at the end of the line.
* A block comment has a start indicator and an end terminator and can continue for several lines, or be less than one line.

Block comments can either nest, parsing ``start start end end`` as one comment, or not, parsing ``start start end`` as one comment and giving a syntax error on the nesting example.

Semantically there are 3 types:

* Implementation comments discuss the tricks used or maintainance advice, and have no effect on compilation.
* Documentation comments are ignored like implementation comments during code generation, but are read by the documentation generator.
* Code comments hide code that is not currently needed.  In this categorization code comments allow nesting other types of comments to allow commenting out huge chunks of code easily, while implementation and documentation comments do not.

This gives 5 comment types: EOL impl, block impl, EOL doc, block doc, and block code. There is no point in an EOL code comment because an EOL implementation comment suffices.

Comments at the beginning of the file are a little special and can be forbidden or restricted to specific types such as documentation comments or shebangs.

Parsing
-------

Generally EOL comments hide the start or end of a block comment. This is useful in some hacks like embedding Javascript in HTML or doing ``//* \n /*/ \n // */`` vs ``/* \n /*/ \n // */`` to switch between two blocks of code. But the parser could pick out the block start/end and not ignore it. There is a different code block trick ``/*/ \n /*/ \n /**/`` vs ``/**/ \n /*/ \n /**/`` which doesn't depend on this behavior.

Using two characters to start a comment helps prevent the accidental starting of a comment and allows more freedom in avoiding syntax conflicts in the language. The double slash // does pretty well in this context, but the /* does not do quite as well. For example in C ``a =1/*ptr;`` starts a comment instead of doing a division. There is the opposite issue that an extra space between the two comment characters, like ``/ / comment``, will cause the comment to be missed, but usually the contents of the comment will cause a compilation error.

Multiline block comments have the issue of forgetting the end terminator and matching some other end terminator. Some languages only have EOL comments, presumably to avoid this problem. Nesting solves this because there will be an unterminated comment. Similarly forbidding block start indicators from appearing in a block comment will work. The compiler can also check each line and see if it looks like code, although this prevents commenting out code.

Linebreaks
==========

Steelman 2D has this intruiging requirement "Lexical units (i.e., identifiers, reserved words, single and multicharacter symbols, numeric and string literals, and comments) may not cross line boundaries of a source program." This is in contrast to other languages, where backslashes can be used to continue lines.

Certainly the days of 80-character terminals are gone, and most editors support soft wrapping. Also for comments, it seems pretty stupid to not allow multiline comments. Tinman H5 clarifies that the requirement has almost no effect as a lexical unit may be composed from multiple sub-units spanning multiple lines.


Indentation
===========

The tabs vs. spaces debate is still going. So let's make some people unhappy by baking the decision into the default syntax.

* `Pike <https://groups.google.com/g/golang-nuts/c/iHGLTFalb54/m/zqMoq9JRBAAJ>`__ says tabs allow choosing 2,4,8 spaces. But this flexibility means linebreaking suffers. For example, assume 100 character lines. Then someone with a 2-space tab and an 8 tab indent can fit 84 characters of code, but someone with an 8-space tab will see that 84 characters of code as a 148 character line, 150% of a line and needing a linebreak. It's better that everyone sees pretty much the same thing. Linus Torvalds `says <https://www.yarchive.net/comp/linux/coding_style.html>`__ tabs are 8 spaces and not adjustable. Also `he says <https://www.kernel.org/doc/html/latest/process/coding-style.html>`__ the line-limit argument is invalid because 3 levels of indentation suffices, but deep indentation often comes up with nested literal data. Another point against Pike is that browsers offer no means to change the width of tabs, so this customization is incomplete - using spaces will at least ensure the display is consistent with the editor.
* Style guides for large companies/projects all agree on "no tabs" (e.g. `this <https://github.com/jrevels/YASGuide#linealignmentspacing-guidelines>`__)
* `GitHub stats <https://hoffa.medium.com/400-000-github-repositories-1-billion-files-14-terabytes-of-code-spaces-or-tabs-7cfe0b5dd7fd#.o7n8zeezx>`__ show spaces winning in the majority of languages
* The `2017 SO survey <https://stackoverflow.blog/2017/06/15/developers-use-spaces-make-money-use-tabs/>`__ showed spaces make 8.6% more salary
* "Tabs + spaces" still has the issues with resizing tabs, and more because the hardcoded spaces may be larger than the tabs. For example resizing an 8-space tab plus 4 spaces to a 2-space tab plus spaces will break. And it is even less common.

So I think the right solution is (by default) to completely forbid tabs, and only allow spaces.

As far as the indent size, :cite:`miaraProgramIndentationComprehensibility1983` studied 0,2,4,6 space indents in Pascal code and found 2 spaces was best, followed by 4 spaces. :cite:`bauerIndentationSimplyMatter2019` did a replication using much simpler stimuli with 0,2,4,8 and eye tracking, but concluded that the difference was too small and they needed 304 participants instead of 22 to find statistically significant results. Per Figure 1, it seems the maximum response time goes up with indentation, but the average is within noise. Looking at Figure 2, it seems the upper quartile fixation duration increases (increasing effort) with increasing indentation, fixation rate is lowest (least effort) at 4, and saccadic length is lowest (least effort) at 4. Likely the mild advantages for 2/4 were tuning effects from previous exposure - they really should have had a 15 minute reading period for each condition and used a crossover design with a lot of questions in each block. I can imagine an online study that just shows some code under varying conditions and asks "how many times does this identifier appear" and measures time to correct response, although doing it with eye tracking would be better.

So 2-4 is all the scientific literature narrows it down to; arguably we should include 1/5 for consideration as they were not explicitly studied, but it's clear from opinion that they are too big or small. A `poll <https://opensource.com/article/18/9/spaces-poll>`__ shows the popularity order is 4,2,3,5. Pros/cons of each per ChatGPT and other sources:

* 2 - Pros: more compact, common in many languages (`list <http://www.opimedia.be/DS/languages/tabs-vs-spaces/>`__, also Cliff's preference), start of line is within peripheral vision. Cons: doesn't mark blocks sufficiently
* 3 - Pros: marks blocks well, still relatively compact, still easy to find start of line. Standard in Ada. `Endorsed <https://twitter.com/clattner_llvm/status/715572957720870912>`__ by Chris Lattner as "looks the best". Lines up with "if ". Cons: uncommon, per `this <https://www.audero.it/blog/2015/10/21/the-revolution-of-3-spaces-code-indentation/>`__ some editors don't support it (only do 2/4/8), not a power of 2 so no easy conversion from 8-space tabs.
* 4 - Pros: most common (Java/Python standard), marks blocks well. Cons: excessive, hard to find start of line
* Mixture - Pros: some blocks are more visually distinguishable by themselves, so they can use less indentation, while others can be emphasized. Cons: Yet more knobs to tweak, complex rules are hard to remember, only works well in editors with 4-2 so is really 2 with some double indents.

Lattner's opinion seems representative; on that Twitter thread and in the Google search results for "3 space indent", several agreed 3 looks the best, and nobody argued against 3's aesthetics. Since 3 is the most uncommon, it should attract the most opinions out of the options, and Stroscot can switch to 2 or 4 if these opinions are backed up with good reasoning or there is sufficient pressure to switch. VSCode is fine with 3, so the editor problem is not a problem in the expected configuration. So we'll try 3.

Layout
======

The arrangement of characters in the document is primarily to assist the human reader. However, as Python and Haskell have shown, making the interpretation of the program indentation-sensitive (IS) can avoid mismatches between the compiler and the reader. A brace format, in constrast, can lead to confusing situations where the compiler interprets a missing brace unexpectedly. Drawing from `Reddit <https://www.reddit.com/r/ProgrammingLanguages/comments/uo0nq7/end_keywords_vs_pythonstyle_blocks_for_beginners/>`__, the advantages:

* IS requires less typing. All modern languages are presented with indentation, so IS is just omitting the hard-to-type curly braces or begin-end markers.
* IS avoids the issue of braces mismatching indentation. In "An Empirical Investigation into Programming Language Syntax", misplaced end tokens were the most common error. It also avoids confusion or arguments about where to put the braces - `WP <https://en.wikipedia.org/wiki/Indentation_style>`__ lists 8 different styles.
* IS is fewer lines of vertical space, because there are no braces on their own lines. This makes it cheaper to print code listings out on paper. Contrastinngly. brace styles with end braces not on their own line are uncommon.
* IS improves code legibility. There haven't been any formal studies that I can find, but Python syntax is often said to be "clean", whereas the punctuation looks "intimidating and alien". IS code looks very similar regardless of indent size, while braces are all over the place. Even Rob Pike says indentation sensitivity is nice.
* Copy-paste: Generally, to use the code you have to reformat it. Although brace reformatting can be done automatically, for small snippets it is less setup to manually fix it up, and manually reformatting mangled IS code is generally a bit easier than manually reformatting braced code. You only have to fix up the indentation by moving the block left/right (supported by all modern code editors), instead of navigating all over and moving the braces to your preferred location.

There are some disadvantages:

* Wordpress comment forms chomp indentation and special characters on unmarked text. Hence braces have a slight advantage over IS in terms of convenience because they can just be posted without any markup and are still valid code after chomping. This problem can be avoided by replacing special characters with entities and leading spaces with ``&nbsp;``, or on newer versions using the ``<pre>`` tag. More generally, most sites support some form of markup for code blocks which preserves indentation. Resolution: works for me.
* Tabs vs spaces - Programmers copy code with spaces from their browser to their editor which uses tabs, or vice-versa, and get issues with invisible mismatching. Although this is a hard error in IS style, it is also an issue in brace style, where once the code is committed and someone with a different tab size tries to open it they will get badly indented code. So I like the hard error - mixed whitespace is just wrong; someone should be forced to use a consistent indentation style (as described above, tabs + spaces is not an option).
* Embedding - Per Rob Pike, "a Python snippet embedded in another language through a SWIG invocation is subtly and invisibly broken by a change in the indentation of the surrounding code." This is solved by the tool processing the embedding being indentation-sensitive, or by using a separate file instead of embedding code in code.
* Screenreading for the blind - `Rune <https://github.com/google/rune/blob/main/doc/rune4python.md>`__ and `several <https://www.youtube.com/watch?v=94swlF55tVc>`__ `others <https://stackoverflow.com/a/453758>`__ say that brace languages like C# are usable with the default screenreader settings. In contrast, indentation-sensitive languages require setting up a new profile that enables reporting the indentation of the current line as speech or a tone. Although this is supported by "almost all" modern screenreaders (`HN <https://news.ycombinator.com/item?id=11419478>`__), and seems fairly easy to use, I guess there are still curmudgeons. Some people turn on indentation even for brace languages, because knowing the indentation level can be helpful in navigating code. But obviously everyone has their preference and supporting several options can improve accessibility.
* One-liners are no longer possible, because the newlines are required for layout

Given these minor disadvantages, it seems best to follow Haskell and define a secondary brace syntax that doesn't rely on indentation, always enabled in the parser. Perhaps they could be discouraged with a warning, but I could see using the braces for one-liners.


More discussion (basically summarized by the above):
https://unspecified.wordpress.com/2011/10/18/why-pythons-whitespace-rule-is-right/
https://wiki.python.org/moin/Why%20separate%20sections%20by%20indentation%20instead%20of%20by%20brackets%20or%20%27end%27



Haskell's layout rules are overly restrictive, for example this is not allowed:

::

  let bang_upper = Bang (Rule
    (Sequent newcut_bseq (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
    (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br))))

Although the parentheses make this unambiguous, Haskell requires indenting a lot more, past the ``=``:

::

  let bang_upper = Bang (Rule
                    (Sequent (fst bl_tseq, newcut_bseq) (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
                    (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br)))

Per `anecdote of Kmett <https://stackoverflow.com/a/2149878>`__ this requirement makes Haskell's layout rules too complex for blind people because it requires lining up columns. Similarly https://www.youtube.com/watch?v=SUIUZ09mnwM says a layout like this is bad style:

::

  unstable someExpresssion
           anotherExpression

because renaming ``unstable`` will require reindenting the rest of the expression. Prefer:

::

  stable
    someExpresssion
    anotherExpression

  // or

  stable someExpresssion
    anotherExpression



The most obvious instance of layout is the module declaration list, but blocks, lists, and records can start layout as well. For readability, clauses may span multiple lines, so some way of distingishing the start / end of clauses must be defined. This amounts to adding braces and semicolons so as to make it layout-insensitive. The inserted braces are "virtual": they don't grammatically match with braces explicitly written in the file, but are otherwise semantically identical.

::

  assertEqual
    {
      a
        b
        c
      d
    }
    { a {b; c}; d}

Behavior of a new line depends on its indentation level, relative to the indentation of the previous line. Indentation level is taken to be the sequence of whitespace characters related by "is prefix of", so "space, tab, space" is less than "space, tab, space, em space", more than "space, tab", and incomparable to "tab, space, space". Incomparable levels are an error.

* If it is indented more, it's a sequence given as an argument to the previous line, so a virtual open brace is inserted
* If it is at the same level, another item in the sequence, so a (virtual) semicolon is inserted
* If there is a line at lower indentation (or EOF), the sequence is ended as it's a new declaration (`off-side rule <https://en.wikipedia.org/wiki/Off-side_rule>`__). A virtual close brace is inserted at the start of the line.

Layout handling is complicated by the presence of grammar rules without layout that allow free choice of indentation, e.g.

::

  assertEqual
    a
      + b
      + c
    a {+ b; + c}
    a + (b + c)

It should be possible to handle these with a fixup phase.

Also, closed operators (e.g. parentheses) inhibit layout; this amounts to skipping whitespace layout when inside an explicit delimiter pair. But of course constructs inside the delimiter pair can start another layout. Finally for constructs that usually use layout we still want to parse 1-line things without braces:

::

  assertEqual
    let a = b in c
    let { a = b } in c

Steelman 2D says "All key word forms that contain declarations or statements shall be bracketed (i.e., shall have a closing as well as an opening key word)." This was written before indentation sensitive languages were a thing, so can be discounted.



Blind community
---------------

Roughly the experiences can be compared like this:

::

  -- original (sighted)

  if 1 < 2 then
    print "hello"
  else
    print "bye"

  -- screen reader with indentation on

  if one less then two then
  indent 2 print quote hello quote
  dedent 2 else
  indent 2 print quote bye quote

  -- braces

  if one less then two then open brace
  print quote hello quote
  close brace else open brace
  print quote bye quote
  close brace

  -- braces with indentation on

  if one less then two then open brace
  indent 2 print quote hello quote
  dedent 2 close brace else open brace
  indent 2 print quote bye quote
  dedent 2 close brace


Another option for blind people is the Braille display, but it is expensive and only shows at most 80 characters. Per `this user <https://stackoverflow.com/a/148880>`__ it can help with both indentation and complex punctuation, particularly lines with many nested parentheses. But the screen reader is usually faster. Comparing wpm, Braille is around 150 wpm starting out going up to 250 wpm, a physical limit of how fast fingers can run over the dots. 150 wpm is also about what TTS does by default but TTS can be sped up to around 500 wpm as the user becomes more accustomed to the synthesizer, :cite:`stentIntelligibilityFastSynthesized2011` and even at 900 wpm experienced users can still transcribe gibberish text with 50% accuracy. So TTS has markedly more bandwidth.

`emacspeak <http://tvraman.github.io/emacspeak/manual/emacspeak_002dpython.html>`__ has speech-enabled python-mode and `per ML thread <https://groups.google.com/g/comp.lang.python/c/Dm-qTzO8Db8?hl=en#3216b7a02047873a>`__ reads things like "closes block <block's opening line>" on dedent. But it seems like it is hard to install and not really that popular.


Delimiters
==========

Haskell uses parentheses for most grouping, ``{}`` for replacing whitespace with explicit syntax, ``[]`` for lists, and has no special meaning for angle brackets.

`Simon <https://soc.me/languages/stop-using-angle-brackets-for-generics>`__ says to use square brackets ``[]`` instead of angle brackets ``<>`` for generics. With Haskell syntax this is moot because parentheses suffices. But he argues collection literals and array lookup should use standard parentheses ``()`` instead of special syntax, because it will become dead weight once the standard library develops better data structures.

Seems a bit weird, he cites Python as an example but Python still uses list literals: the syntax for a NumPy array is ``np.array([1, 2, 3, 4, 5])``. The only thing overloaded is access ``arr[i] = x``.

Julia doesn't require parens around conditions in ``if`` and ``while``, ``if a == b`` instead of ``if (a == b)``.

Ada uses round brackets for array component references. "The advantage of this is a uniform notation within an expression for reference to array components and for function calls. The same argument of uniform referents justifies using the same syntax for component selection of records."

For brevity, trailing delimiters can be omitted:

::

  assertEqual
    3+1/(7+1/(15+1/1
    355/113

This goes directly against Steelman 2D "Programs may not contain unmatched brackets of any kind", but Tinman H8 admits that it makes programs easier to write; it's a useful convenience for saving writing extra parentheses. To address concerns about correctness, you can set Stroscot to warn/error/autofix unmatched delimiters. This is one of the innovations Ada doesn't have, the ability to have multiple language dialects under a flag.

6B. The language shall not impose arbitrary restrictions on programming style, such as the choice between statement terminators and statement separators, unless the restriction makes programming errors less likely.

According to the Green rationale, a study found that terminators were less error-prone than separators.

Function syntax
===============

Steelamn 7A. Functions (which return values to expressions) and procedures (which can be called as statements) shall be definable in programs. Functions or procedures that differ in the number or types of their parameters may be denoted by the same identifier or operator (i.e., overloading shall be permitted). [Note that redefinition, as opposed to overloading, of an existing function or procedure is often error prone.]

Steelman 5G. The language shall distinguish between open scopes (i.e., those that are automatically included in the scope of more globally declared variables) and closed scopes (i.e., those in which nonlocal variables must be explicitly Imported). Bodies of functions, procedures, and processes shall be closed scopes.

Stroscot has first-class functions with lexically scoped name binding.

Lambdas are defined using whatever syntax. The ``\x.y`` style is closest to the mathematical notation (barring Unicode), Haskell uses ``\x -> y``, Cliff likes ``{x => y}``.

Conceptually, higher-order term rewriting is the underlying model of computation. So definitions are not equivalent to lambdas, although in many cases they produce equivalent results.

Arguments
---------

Stroscot supports many types of arguments. Functions are extremely common, so the more styles supported,
the shorter the code will be.

Equations are tried in the order in which they are written; as soon as the left-hand side of an equation matches (and the condition part of the equation, if any, is satisfied), it can be applied to reduce the target term to the corresponding right-hand side. The term is rewritten until no more equations are applicable.

Call syntax
-----------

There are various ways to write function calls:

* Haskell style: ``f (g a 1) (h b 2)``.
* Lisp style: ``(f (g a 1) (h b 2))`` (Haskell with extra parentheses)
* Coffeescript: ``f (g a 1), (h b 2)`` (Haskell with extra commas)
* Explicit call, Haskell: ``call f (call g a 1) (call h b 2)`` (Haskell with call inserted before functions)
* C style: ``f(g(a,1),h(b,2))``
* C with spaces style: ``f(g(a 1) h(b 2))``
* Explicit call, C: ``call(f,call(g,a,1),call(h,b,2))`` (Haskell style with call inserted before parentheses and commas instead of spaces)
* Postfix: ``2 b h 1 a g f``
* Postfix with argument counts: ``2 b 2 h 1 a 2 g 2 f``

Comparing character counts for this example, postfix is 13, C is 16, and Haskell is 17. Lisp, Coffeescript, and the explicit calls are all similar to Haskell style, and are longer, so can be ignored. For a simple function application ``f a b c`` Haskell is shorter by one character than C (more if you add a space after the comma like ``f(a, b)``, as is common) and the spaces are easier to type than the commas. Haskell loses in character count only if you have a pattern like ``f (a) (b) (c)`` where all the expressions need parentheses.

Postfix is pretty much unreadable so I'm ignoring it. So the two main contenders are Haskell and C. The C style is incredibly common, whereas Haskell is only used by functional languages like Haskell and OCaml. But I'm still going with Haskell for now, because: (`Reddit thread <https://www.reddit.com/r/ProgrammingLanguages/comments/jde9xp/advantages_of_currying_in_a_programming_language/>`__)

* Haskell is more readable - the spaces and parentheses have more vertical variation compared to commas
* Haskell is pretty simple, only a bit more complex than S-expressions
* Haskell is good for writing curried functions. In contrast the C style makes it inconvenient to use curried functions, you have to write lots of parentheses ``f(1)(2)(3)``. Also comparing ``any (== x) l`` and ``any(\y -> y == x,l)``, in the C style the comma is almost unnoticeable and the syntax is ambiguous as it could be grouped ``(x,l)``
* Haskell style still allows passing a tuple and matching the C syntax, ``f (1,2)``, or no arguments ``f ()``. It also allows a record like ``f {a=1}`` which C would require separate support for. In contrast the C style forces a tuple object even if the combination of arguments doesn't represent a meaningful idea.

For beginners the main question is which style makes it easier to match parentheses - mismatching is a common novice programming error. :cite:`singerFunctionalBabyTalk2018` Also error messages for accidental partial application are important. TODO: test or survey some novice programmers later on. Provisionally I expect Haskell to do better because it has fewer parentheses in the common case.

Implicit arguments
------------------

Claim: Explicit argument passing cannot replace implicit arguments

See example:

::

  -- standard library
   log s = if (priority > loglevel) { logPrint s }

  -- components of an application
   foo = log "foo" { priority = DEBUG }
   bar = log "bar" { priority = WARNING }
   baz =
    foo
    bar

  -- main file
   logPrint x = writeFile file x
   file = "a"
   loglevel = WARNING

   main =
     baz
     foo {loglevel=DEBUG}
     bar { file = "b"}

``loglevel`` is defined at the top level, but each use site is scattered in the code. The implicit argument replaces the global variable that is often used. Similarly ``logPrint`` is passed implicitly instead of being a member of a global Logger instance. The ``file`` variable does not exist in the standard library; it is part of the user's code.

To use explicit argument passing, we'd have to add explicit ``loglevel`` and ``logPrint`` arguments to ``log`` and all its callers. To minimize callers we could partially apply it in ``main`` and pass around just the ``log : String -> Cmd`` function. But still, we have to modify every caller of ``log`` and its callers and so on to pass around the ``log`` function.

n+k patterns
============

This is a feature removed from Haskell that simplifies writing recursive integer functions, like factorial. Basically ``case v of { x+k -> e; _ -> e' }`` translates to ``if v >= k then (\x -> e) (v-k) else e'``, where ``k`` is a literal.

Arguments:
* concise special notation, like for tuples and lists
* unfamiliar: the symbol + is being abused
* unnatural: not clear that residue must always be ``>= 0``, i.e. pattern matches a natural number
* easy to change to a guard clause ``case v of { x | x >= k -> let x = v-k in e; _ -> e' }`` or a view pattern ``case v of { (dec k -> Just x) -> e; _ -> e' } where dec k v = if v >= k then Just (v-k) else Nothing``

GHC-specific:
* Pattern still applies even if ``(+)`` is rebound away from ``(Prelude.+)``.
* only works for ``k >= 0``, as writing ``n+(-1)`` is forbidden.

Pattern synonyms should allow defining this like a view pattern, but without the ugly ``Just``. Then the pattern like ``x@(dec k) -> e`` solves the main issues: dec is its own symbol, and the user has imported it so knows its semantics. And ``k`` should be evaluated so can be a negative number or constant expression.

Pattern matching / conditionals
================================

`This <https://soc.me/languages/unified-condition-expressions>`__ has a good overview of potential syntax patterns. The keyword can be if, match, when, switch, case, etc. Here we use ``:`` for if, ``~`` for then, ``,`` for else, ``=`` for is (Pattern matching), ``\`` for ``==``, ``^`` for ``===``, modeled on Randomo in :cite:`stefikEmpiricalComparisonAccuracy2011`. Figure out actual keywords based on novice surveys and what developers are familiar with.


::

  // simple if
  : x \ 1.0 ~ "a" , "z"

  // pattern matching using "if-let"

  : person = (Person "Alice" age) ~ "${age}" , "o"

  // comparison of one variable to multiple values
  : x \
    1.0 ~ "a"
    2.0 ~ "b"
    , "z"

  : x
    \ 1.0 ~ "a"
    \ 2.0 ~ "b"
    , "z"

  : x \ 1.0 ~ "a"
  , : x \ 2.0 ~ "b"
  , "z"

  // multi-way if
  : | x \ 1.0 ~ "a"
    | x \ 2.0 ~ "b"
    | , ~ "z"

  // different comparison operators (predicates)
  : x
      \ 1.0 ~ "a"
      ^ NaN ~ "n"
      , "z"

  : x \ 1.0 ~ "a"
  , : x ^ NaN ~ "n"
  , "z"

  // method call predicates

  : xs
    isEmpty ~ "e"
    contains {element = 0.0} ~ "n"
    , "z"

  : isEmpty xs ~ "e"
  , : contains xs 0.0 ~ "n"
  , "z"

  // pattern matching with bindings

  : alice
    age # < 18 ~ "18"
    = person@(Person "Alice" _) ~ "${age person}"
    = (Person "Bob" age) ~ "${age}"
    , "0"

  // wildcards and pattern guards::

  : person
    = (Person "Alice" _) ~ "alice"
    = (Person _ age) && age >= 18 ~ "adult"
    , "minor"

  : person =
    (Person "Alice" _) ~ "alice"
    (Person _ age) | age >= 18 ~ "adult"
                   | otherwise ~ "minor"

The condition can be split between a common discriminator and individual cases. This requires doing away with mandatory parentheses around the conditions. This strongly suggests using a keyword (then) to introduce branches, instead of using curly braces, based on readability considerations.

Conditionals
============

Steelman 6C. There shall be conditional control structures that permit selection among alternative control paths. The selected path may depend on the value of a Boolean expression, on a computed choice among labeled alternatives, or on the true condition in a set of conditions. The language shall define the control action for all values of the discriminating condition that are not specified by the program. The user may supply a single control path to be used when no other path is selected. Only the selected branch shall be compiled when the discriminating condition is a translation time expression.

There is a "unified" syntax by `Simon <https://github.com/soc/soc.me/tree/main/_languages>`__, also a very similar paper on the "ultimate" syntax :cite:`parreauxUltimateConditionalSyntax`. Interesting idea, but still has to be tested real-world somehow.

Per the internet, Ruby's ``unless-else`` is unintuitive. Only support ``if-else`` and ``unless`` without the else. Also ``if not`` is a possible replacement for ``unless``.

Per :cite:`ichbiahRationaleDesignADA1979` there is the `dangling else <https://en.wikipedia.org/wiki/Dangling_else>`__ ambiguity. I like Haskell's solution, there is a separate keyword for if-then, ``when () {}``, and if can only be used as ``if-then-else``. Else-if chains are pretty much necessary.

Tuples and records
==================

In `Maybe Not <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/MaybeNot.md>`__ Rich Hickey  says records/fields, and product types are "place oriented programming", hence bad. Well, in `The Value of Values <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/ValueOfValuesLong.md>`__ he says place-oriented programming is when you use in-place update. But maps (his proposed alternative) also support in-place update and are place-oriented. The only difference between maps and records seems to be that records have ordered fields.

So he seems have a different definition in mind, in particular that place-oriented means accessors are not first class - even when the fields are named, you cannot say ``object["name"]`` for an arbitrary object or an arbitrary name. But this is easily solved by adding such functionality. It also doesn't get into the mutable/immutable distinction that the values talk made.

His second point is that product types "complects" the meaning of things with their position in a list. "Complect" is from `Simple Made Easy <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/SimpleMadeEasy-mostly-text.md>`__ and is a pejorative version of "braid together".
Essentially he's saying that if you have ``(String, String)`` there is no way to know how the second string is different from the first string. Well, for commutative operations like addition the order literally doesn't matter. Adding any sort of information to ``(+) : (Int, Int) -> Int`` is complicating the picture. Similarly for Strings `coming up <https://gemma.msl.ubc.ca/resources/baseCode/apidocs/ubic/basecode/util/StringUtil.html#append-java.lang.String-java.lang.String-java.lang.String->`__ with names "appendee" and "appendant" for an  append operation is almost as bad as digging up "complect". Using numerical names ``s1`` and ``s2`` makes more sense. It still gives a record with named fields, but it makes sense to use positional arguments.

And if the types are different there's no ambiguity: ``(FirstName, LastName``, ``(Int,Bool)``, etc.

Precedence
==========

Julia has juxtaposed multiplication. `Jump <https://jump.dev/JuMP.jl/dev/developers/style/#Juxtaposed-multiplication>`__ recommends limiting to cases where RHS is a symbol, like ``2 pi``.

`This post <https://ericlippert.com/2020/02/27/hundred-year-mistakes/>`__ describes a mistake in C-style precedence: it should be ``&&, ==, &`` but is instead ``&&, &, ==``, causing a footgun. "Swift, Go, Ruby and Python get it right."

Per Tremblay, Experiments in language design (Gannon, 1975) have indicated that APL's "natural" right-to-left rule is in fact harder to understand than simple priority rules. Gannon, J. D.: "Language Design to Enhance Programming Reliability," Report CSRG-47,Computer Systems Research Group, University of Toronto, Toronto, Canada, January 1975.

Cycle
-----

Can we have a precedence cycle, like associating ``a + (b * c)``, ``a * (b ^ c)``, ``a ^ (b + c)``?

Well what about this expression:

::

  a + (b * (c ^ d))
  (a + (b * c)) ^ d

These are both consistent with the cyclic precedence so we would need more rules to decide between them.

Logic programming
=================

Prolog uses Horn clauses of the form ``H :- A1, A2, A3``. This is read "The clause head ``H`` is implied by the body's goals ``A1``, ``A2`` and ``A3``." A fact is a clause with no goals, ``F.`` or ``F :- ``, equivalent to ``F :- true`` (since ``true && a = a``). The head and goals are predicates of various syntax. There are various goals; they can be predicate terms with variables and list patterns (Herbrand domain, original Prolog), linear logic formulas (`linear logic programming <https://www.youtube.com/watch?v=rICThUCtJ0k>`__), or constraints (constraint logic programming).

For example, reversing a list::

  nrev([],[]).
  nrev([H|T],L2) :- nrev(T,R), append(R,[H],L2).

  :- nrev([1,2], X), write(X), nl
  % [2,1]


In practice Prolog syntax is pretty bad, due to being so uniform; programs are heavy on meaningless intermediate variables such as ``R`` in the above.

Functions
---------

There is a `func <https://www.swi-prolog.org/pack/list?p=func>`__ package that allows writing ``nrev $ T`` in an expression instead of ``nrev(T,R)`` and then using ``R`` in the expression. A "function" is defined as follows:

* Any predicate ``p(...in, out)`` is a function from ``in`` to ``out``
* A dictionary is a function from keys to values
* An arithmetic expression ``2*_+3`` is a function on numbers
* A format string is a function from argument list to interpolated output
* A term with a ``~`` is a function which takes no input values and produces an output at the ~ position. For example

::

  atom(atom_string(~,"hello world")).

  % is equivalent to

  atom_string(X,"hello world"),atom(X).

More generally we should be able to build compound expressions:

*  ``z = append a (append b c)`` instead of ``append(a,b,temp),append(temp,c,z)``
* ``t = f x + g x`` instead of ``f(x,y), g(x,z), t = y + z``
* in general, ``pred (f x)`` instead of ``f x y, pred(y)``

There is no issue with devising a consistent use of output variables for an expression syntax; Oz does this. In practice the "boring" deterministic code will take up 66%-95% of the program so special-casing familiar function syntax is important. Horn clauses are less readable than compound expressions, and programmers have gotten used to skimming over assignment clauses and parsing parentheses and infix expressions.

Relations
---------

For relations like ``precedes(x,y)``, Horn clauses are not necessarily the optimal syntax. We could say it is a nondeterministic function ``y = precedes(x)``. But the predicate syntax is traditional for boolean formulas in logic and unlike functions there is not a well-developed alternative syntax to use for formulas. Powerful high-level logic syntax is still an unexplored area.

miniKanren :cite:`byrdRelationalProgrammingMinikanren2009` doesn't have a global clause database so clauses of the same predicate must be grouped, this gives a local "match" syntax for relations::

  nrev l1 l2 :-
    matche l1 l2
      [] [] -> true
      (h . t) l2 -> fresh (\r -> conj
        [ nrev t r
        , append r [h] l2
        ])

  print (run (\x -> nrev [1,2] x)
  -- [[2,1]]


This seems helpful syntax-wise, but MiniKanren and Clojure core.logic are also quite tedious to use in practice.

Primitives
----------

``matche`` and other syntax desugars into a small set of primitives: :cite:`hemannMicroKanrenMinimalFunctional2013`

* ``fresh`` or ``exists (\x.<body>)``: true if ``body`` is true for some value of ``x``
* ``unify x y`` / ``x == y``: true if ``x`` unifies with ``y``
* ``disj [x,y,z]`` / ``conde [x,y,z]``: true if any of ``x,y,z`` are true (logical or / disjunction)
* ``conj [x,y,z]``: true if all of ``x,y,z`` are true (logical and / conjunction)
* ``run (\x y. <body>)`` delimits the boundary of the logic program. It returns the stream of substitutions of the given variables for which the body is true. Optionally the maximum length of the stream may be specified.

The expansion of ``nrev`` is given in :cite:`hemannFrameworkExtendingMicrokanren2017` page 137 (``define-relation`` is just DSL fluff around ``define`` per the appendix)::

  nrev l1 l2 =
    disj
      [ conj [ l1 == [], l2 == [] ]
      , fresh (\h -> fresh (\t ->
          conj
            [ (h,t) == l1
            ,fresh (\r -> conj [nrev t r, append r [h] l2])]
            ]
      ))]

  print (run (\x -> nrev [1,2] x)
  -- [[2,1]]

Functional programming
======================


In a true empirical comparison study :cite:`pankratiusCombiningFunctionalImperative2012`, Scala programs ended up taking a lot longer to write than Java - 72 hours vs 43 hours, with no working Scala programs submitted until the third week, versus a working Java program submitted the first week. The reasons given in the paper were Scala's complex type system, poor documentation, poor IDE/debugging support, complex memory model, and complex parallel programming abstractions. That gives an idea of what the important factors are in cycle time: making programming language features more intuitive so that they can be used correctly with little study, and implementing "creature comforts" to help to write and test code. For example, according to ChatGPT, Scala's FP features are complex and make programs less readable to newcomers, but once you have learned Scala's FP syntax and nomenclature, the FP style makes programs more readable because they are more concise. This suggests that redesigning the FP paradigm to use intuitive naming and syntax will improve readability. But it may be that no concise and understandable FP design exists and there will always be a learning curve for FP features.


Go syntax notes
===============

small set of keywords, without filler keywords (such as 'of', 'to', etc.) or other gratuitous syntax
prefer keywords (e.g.  'function') over operators or blocks
variables, simple control flow are expressed using a light-weight notation (short keywords, little syntax)

DSLs
====

Stroscot aims to be a "pluggable" language, where you can write syntax, type checking, etc. for a DSL and have it embed seamlessly into the main language. This may be possible due to macros, which allows pattern-matching the AST of any expression, like ``javascript (1 + "abc" { 234 })``, or may need special handling in the parser to also do character-level embedding or seamless integration of parsers / escape sequences.

Example DSLs:

* SQL

::

  run_sql_statement { SELECT ... }

* Assembly and C++.

::

  result = asm { sumsq (toregister x), (toregister y) }
  my_func = load("foo.cpp").lookup("my_func")

* TeX / mathematical expressions:

::

   tex { result = ax^4+cx^2 }
   math { beta = phi lambda }


It is not just fancy syntax. DSLs that use vanilla syntax are useful for staging computations, like passes that fuse multiple operations such as expmod and accuracy optimizers that figure out the best way to stage a computation.

Scala and JS have support for native XML literals. Scala had one syntax in 2 and a new syntax in 3. Similarly JS had E4X, but client-side E4X is dead today, replaced by server-side React JSX which is honestly pretty similar. More recently XML has been perceived as verbose and JSON has become popular. JSON is native to Javascript hence literals/interpolation are aleady supported, and then browsers added parsers/pretty printers. The lesson is not (like Flix says) that no support should be present - rather it is that the design should be flexible and allow easily adding markup syntaxes.

::

  // Scala 2 XML literals (dropped)
  val mails1 = for (from, to, heading, body) <- todoList yield
    <message>
      <from>{from}</from><to>{to}</to>
      <heading>{heading}</heading><body>{body}</body>
    </message>

  // Scala 3 XML string interpolation
  val mails2 = for (from, to, heading, body) <- todoList yield xml"""
    <message>
      <from>${from}</from><to>${to}</to>
      <heading>${heading}</heading><body>${body}</body>
    </message>"""
  println(mails2)

  // E4X
  var sales = <sales vendor="John">
    <item type="peas" price="4" quantity="6"/>
    <item type="carrot" price="3" quantity="10"/>
    <item type="chips" price="5" quantity="3"/>
  </sales>;
  sales.item += <item type="oranges" price="4"/>;

  var a = "bar";
  var data = <foo id={a+(3+4)}>{a+" "+"bat"}</foo>;
  print(data);
  => <foo id="bar7">bar bat</foo>

  // JSX
  return (
     <div>
       <h1>{ i === 1 ? 'true' : 'false' }</h1>
     </div>
  );

 All these support interpolation, so that's clearly a necessary feature.

Identifiers
===========

Let's go through Steelman 2E.

"Mnemonically significant identifiers shall be allowed." The mnemonically significant identifiers makes a lot of sense. I'm not sure what a language without them would look like - symbols like ``gen872060``? Even random numbers have some significance though. Maybe an esolang like Whitespace or Brainfuck that simply doesn't allow user-defined identifiers. Strange considerations aside, Stroscot allows defining identifiers so this requirement is satisfied.

The actual names chosen do affect readability. Binkley found that the phrases likely to be found in programs were answered incorrectly more often than prose phrases. This is likely due to the phrases being less memorable, e.g. it seems like it would be easier to confuse "full pathname" with "full pathnum" than "river bank" with "river tank" (unfortunately the most common distractor is not reported).

"There shall be a break character for use within identifiers." This is a bit trickier. There are many choices: whitespace in identifiers, snake case  (underscore_style), camel case (CapsStyle), kebab-case, or Unicode characters like a shaded box ░ or half box ▆. In contrast for identifiers of one part there are really only 3 styles, Uppercase vs lowercase vs ALLCAPS (ignoring stUDly CaPs). Per Binkley, a study of real-world code found identifiers were 43.8% 1 part, 29.5% 2 parts, 16.4% 3 parts, 6.2% 4 parts, and then we can calculate that 5+ parts were 4.1%. Longer identifiers require more time to read, due to requiring more fixations - Binkley found that going from 2 to 3 identifier parts increased the cloud word recognition task time by 0.981 seconds.

* The shaded box performed well in Epelboim's study, being only 4% harder to read than normal text in conditions such as "░this░ ░is░" and "░░an░░example░░". Unfortunately the pattern of my interest "some░filler" was untested, but just reading it with my eyes it doesn't seem too hard to read. Visually, a shaded box should allow easily distinguishing the extent of an identifier. Of course typing the shaded box character is somewhat difficult as it's not on standard keyboards.

* The underscore style provides clear separation between parts when looking at the top, but may require a little work to see the underscore and identify the extent of an identifier. Sharif's figure 4 of underscore fixations suggests that underscore-trained programmers do not need any extra fixations to see the underscores, but that the last identifier part may have a longer fixation. There is also a "bumpy case" variation that uses double underscores, I don't think this improves readability.

* Kebab case moves the line upwards, closer to the centers of the fixations, and thus most likely makes it easier to determine identifier extents than underscore style, although perhaps still performing worse than the shaded box ░. The half box ▆ could provide a similar benefit, probably again performing between underscore and shaded box. Searching Google Scholar for kebab case produced no academic studies on readability, but Lisp aficionados on Google say they prefer kebab to snake.

* Camel case requires identifying parts by letter height variation, which is visually difficult and requires longer or multiple fixations. Camel case does however provide good extent identification. In long lines of identifiers it is probably as bad as text with no spaces, which takes 44% mean percent longer to read. Binkley and Sharif found 13.5% and 20% longer times with camel case over underscores for the cloud task. 2-part identifiers take approximately the same time as underscores - it is 3-part identifiers that lose big.

* Whitespace - there are ways to allow true multi-word identifiers using the space character, but these suffer greatly from extent identification because there is no visual differentiation between within-identifier and between-identifier spacing. For example with Haskell syntax, ``do something = ...``` can either define the identifier ``do something``, or be a clause ``do _`` that binds ``something``. Even using a string, ``"do a really complicated thing" = ...``, it is a bit easy to miss the quotes at first glance and imagine it is a pattern. A solution like ``do ^something = ...`` is a bit tricky to evaluate - it matches the 2-atom ``do something``, so "do something" is not parsed as a single identifier but rather a term of two symbols. This allows some flexibility but also is distinct enough from an identifier-with-breaks that I don't think it counts.

When reading there is an accuracy vs speed tradeoff. If something is really easy to read, one tends to just skim it and gloss over small mispellings. Because camel case is harder to read, in Binkley's studies the participants took longer to answer the questions and had slightly improved accuracy overall compared to snake case. There was however one contrary question: ``attackerHealth/attackedHealth`` was easily confused in camel case but in snake case ``attacker_health/attacked_health`` it was more distinguishable due to the stem of the d.

So far the effects of training have not had enough statistical power to state any effects. Binkley found, if there are effects, in his studies they were most likely that training on an identifier style increased the speed of reading that style but made it harder to read other styles. Training on a style may also make it harder to read normal prose that had been randomly chunked into identifiers of that style. I would say, based on the studies of font tuning, there is likely is a tuning effect, and it is best to standardize on a single identifier style for common usage so as to encourage developers to tune their visual system to that style. But again based on the font tuning studies, this tuning effect is relatively small, less than 10%, and a developer will mostly be adapted within 20 minutes, so it is not a huge deal.

One question I'd like addressed is if using different identifier part styles for different semantic categories also is subject to the tuning effect. For example, if variables are in snake case, does having module names in camel case significantly slow down skimming a module vs. having module names in upper snake case? I think a developer can get used to having a mixed set of identifier styles but maybe it is slower.

Overall I think there are trade-offs for every break character, I don't see any reason to mandate the use of a particular style, other than forbidding whitespace due to implementation difficulties and lack of readability. For the standard library, kebabs should be the preferred style, as based on the limited evidence they are the most readable. The half box seems attractive for users that have access to full Unicode input. Another choice is the underscore style, as that puts us in the Python camp and Python is the most popular language currently.

Theoretically, the best identifier style could vary by individual and depend on external factors such as font or eye correction. It's unlikely, because the eye-tracking data says that camel case needs more eye fixations, and that's a physical constraint of the human visual system that probably doesn't change by individual. But, automatically translating between different identifier styles is possible, so identifier style should be yet another option in the code formatter, along with a compile option to translate the style of libraries.

"The language and its translators shall not permit identifiers or reserved words to be abbreviated. (Note that this does not preclude reserved words that are abbreviations of natural language words.)"

You might think the difference between ``pos`` and ``position`` is trivial but it's not - one is a relatively common English word, and one is programming jargon. I haven't looked for a study but I find I often end up "mispelling" an identifier by typing it out fully when it is abbreviated, or vice-versa. I think given the advent of large screens, gigabyte storage, and autocomplete, the best policy is zero abbreviations - neither in user identifiers nor in reserved / language identifiers. A policy that identifiers should always be fully spelled out is consistent and memorable.

Now regarding that the language should not permit abbreviations within the source, I have to disagree for reasons similar to allowing unmatched delimiters - at a REPL prompt the abbreviations are helpful in shortening the input necessary. But of course again it is worth turning on the warn/error/fix for an IDE. Notably, with IDE autocomplete, it is most likely harder to input an abbreviation than to input the full name.

Reserved words
==============

Steelman 2F: "The only reserved words shall be those that introduce special syntactic forms (such as control structures and declarations) or that are otherwise used as delimiters. Words that may be replaced by identifiers, shall not be reserved (e.g., names of functions, types, constants, and variables shall not be reserved). All reserved words shall be listed in the language definition."

I've never felt comfortable with reserved words, they always seem like restrictions on the use of the language. Why can't I define a variable named ``if``? It's just an arbitrary parser restriction - ``if`` is lexed as a "reserved token" and therefore it is not in the class of "identifier tokens" so the assignment grammar production does not match. But any human can read some code like ``if = 1; print(if+2)`` and understand the intent. In Java, they work around this issue with ``Class klazz`` and other weird mispellings. IMO it is better to just hack the parser so that you can write ``Class class`` like the programmer intended. So Stroscot shall have no reserved words.

Operators
=========

New operators can be declared with `mix <http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf>`__ `fix <http://www.bramvandersanden.com/publication/pdf/sanden2014thesis.pdf>`__ semantics, e.g.

::

  syntax _&&_ associate left above _and_ _or_ _not_ below _||_ equals _&_
  syntax [[_]]
  syntax if_then_else_
  syntax _postfix
  syntax prefix_

Operator precedence will be a poset, rather than levels. Infix symbols can be left or right associative.

Stroscot supports your typical PEMDAS:

::

  assertEqual
    1 + 2 * 3^2
    19
  assertEqual
    3+1/(7+1/(15+1/1))
    355/113
    3.14159292035...

Most operators are textual:

::

  assert
    true and false == false
    true or false == true
    true xor true == false
    5 div 2 == 2
    5 mod 2 == 1

Minus is both a unary prefix operator and a binary infix operator with special support to disambiguate the two. ``(-)`` denotes the binary minus operator and ``neg`` the unary minus operation.

Operators are syntactic sugar for functions. Enclosing an operator in parentheses turns it into an ordinary function symbol, thus ``x+y`` is exactly the same as ``(+) x y``.

String concatenation is ``++``.

Expressions
===========

Steelman 4D. Expressions of a given type shall be allowed wherever both constants and variables of the type are allowed.
4E. Expressions that can be evaluated during translation shall be permitted wherever literals of the type are permitted. Translation time expressions that include only literals and the use of translation time facilities (see 11C) shall be evaluated during translation.


Chained Comparison
==================

::

  assert
    1 <= 2 < 3
    9 > 2 < 3

Variables
=========

Identifiers cannot be directly reassigned; you can shadow, which generates a warning, but once an identifier is declared in a scope, that's what that identifier refers to for the duration of the scope. OTOH references behave like mutable variables.

::

  a = mut 1
  a := 2
  raise a by 1

Mutable assignment (``:=``) is completely distinct from name binding (``=``). They have distinct notation and mutable assignment cannot create new bindings.

Clauses
=======

Sequential matching:

::

  f 1 y = 1
  f x 2 = 2
  f x y = 3

Parallel matching:
::

  f 1 = 1
  ;
  f 2 = 2
  ;
  f y | y != 1 && y != 2 = 3

The extra ``;`` is an escape to avoid sequential matching of a sequence; if you alternate clauses of different functions or define clauses in different files they will also be combined with parallel matching.

Function application (juxtaposition) binds stronger than all operators and associates to the left, ``x y z --> (x y) z``.

Destructuring and function bindings
------------------------------------

Generally identifiers ``f`` in head positions of a LHS ``f a b c`` are taken as literal function symbols. Identifiers in head position in a sub-term are taken to be constructors, and destructure the function argument. Identifiers in non-head positions are taken to be variables. This is Pure's "head = function rule".

::

  x = 2 # x is function of no arguments
  x a = a # x is function of one argument, binds variable "a"
  x (foo a b) # x is function of one argument, destructures term with head foo and binds a/b

Certain symbols such as tuple heads as head of the LHS are assumed not to be function definitions. Instead matching on them destructures the right hand side. For example you can define functions using destructuring:

::

  (x < y, x > y) = (<0) &&& (>0) $ s' (digitsToBits digits) where (CR s') = x-y

This translates to:

::

  x > y = case (z x y) of { (x < y, x > y) -> (x > y) }
  -- equivalent to
  x > y = case (z x y) of { (a,b) -> a }

  x < y = case (z x y) of { (x < y, x > y) -> (x < y) }

  z x y = (<0) &&& (>0) $ s' (digitsToBits digits) where (CR s') = x-y
  -- z a fresh symbol

To force a function definition you can use an as pattern, ``_@(,)``

To force interpretation as a variable you can use an anonymous as pattern, ``(f@_) a b c``. Then ``f`` is a variable and will match any symbol, rather just ``f``. Example converting a function application to a list::

  foo ((x@_) y) = (foo x) ++ [y]
  foo x = [x]
  > foo (a b c d);
  [a,b,c,d]

To force interpretation as a literal you can use ``^``. The symbol will be interpreted as a literal even in variable position::

  foo ^foo = "self-application"

  foo bar # does not reduce

You can also declare ``foo`` to be a symbol::

  symbol foo

However this is a module definition and means the symbol cannot be used as a variable in the module anymore.


Symbols
-------

 To say that it is actually a symbol a special keyword ``symbol`` is used:

::

  symbol foo

  foo x = 1

  bar 2 = 2
  # equivalent to _ 2 = 2 because bar is interpreted as a variable

Furthermore you can define a function symbol with an arity. This resolves applying the function symbol to arguments for which no clauses are defined to the exception ``undefined``, which often has better semantics than an unevaluated normal form.

::

  function symbol foo arity 2

  foo 1 2 = "fine"

  foo 1 2 # "fine"
  foo 3 4 # undefined
  foo 1 # not affected - normal form

This just creates a low priority definition ``foo _ _ = undefined``.

Non-linear patterns
-------------------

Non-left-linear patterns such as ``foo a a`` are allowed, this is interpreted as ``foo a b | a == b`` - rename variables and check for equality using ``==``. See :ref:`trs-equality-linearity` for a discussion.

Pattern synonyms
----------------

::

  toPolar (Point x y) = (sqrt (x^2 + y^2), atan2 x y)
  pattern Polar r t = (toPolar -> (r,t))

Pattern definitions are unidirectional in that they define matchers for syntax used only in patterns, not in expressions. To make a bidirectional pattern simply define the builder:

::

  Polar r t = Point (r * cos t) (r * sin t)

Variable bindings
-----------------

::

  a --> if a then $arga[0] == a else True -- binds a if a is not defined as a symbol
  _a --> True -- hole, binds a even if a is an existing symbol


This defines the variables as a zero-arity function symbol. So for example you can write

::

  a | True = 1
  a | False = 2

which means the same thing as

::

  a | True = 1
    | False = 2

i.e. ``a = 1``.

Inline definitions
------------------

Definitions can be made inline; they are lifted to the closest scope that allows definitions.

::

   range = sqrt((dx=x1-x0)*dx + (dy=y1-y0)*dy)

  -- translates to
   dx=x1-x0
   dy=y1-y0
   range = sqrt(dx*dx + dy*dy)

Inheritance
-----------

The general idea of inheritance is, for ``Foo`` a child of ``Bar`` to rewrite calls ``exec (Foo ...) a b`` to calls ``exec (Bar ...) a b``, and this can be automated with a macro:

::

  inherit foopat barpat barmethodlist = {
    for (m : barmethodlist) {
      m foopat = m barpat
    }
  }

Lambdas
=======

::

  \a b -> stuff
  \a b. stuff
  lambda {
    a 1 = stuff
    a 2 = other
  }

A lambda raises an exception if no pattern matches (defined function), but otherwise is
a nameless local function. With the ``lambda{}`` syntax multiple clauses can be defined - they are matched sequentially. Multiple-argument lambdas are curried.

Because they're nameless lambdas aren't sufficient to define recursive function - use (named) local functions, or the function ``fix : (a -> a) -> a``.

Destructuring works in the arguments of lambdas as with named functions.

Matching
--------

``match`` is an expression:

::

  f = match (2+2) (5+5) | x y = 2
                        | 1 y = 2

It desugars to a lambda applied to the arguments.

``impossible`` is a special RHS used to help the verification analysis:

::

  f = match (2+2)
        | 5 = impossible

Reduce similarly reduces an expression to normal form using some rules:

::

  reduce x where
    x = y
    y = z
  # z

Operator sections
-----------------

Operator sections allow writing partial applications of infix operators.
A left section ``(x+)`` is equivalent to ``(+) x``. A right section ``(+y)`` is
equivalent to ``\x -> x + y``.

In contrast, ``(-x)`` denotes an application of unary minus; the
section ``(+-x)`` can be used to indicate a function which subtracts ``x``
from its argument.

Definitions
===========

You can define variables and functions pretty much anywhere: a module, a block, a clause, or a clause group. These are in a new lexical scope specific to the syntactic unit where they are defined. This avoids unexpected conflicts between different areas of the program. An inner definition is not visible from an outer context unless the inner context is explicitly imported.

::

  clause-group-let
    -- clause group definition, but uses "a" which is only defined for first clause
    h y = a * 2
  for
    foo a y | true = {
      f x = g x + h y -- block definition
      return (f 3)
    }
      where
        g a = 2 * a -- clause definition
    foo b y | false = impossible

An inner definition shadows an outer one, so for example you can write:

::

   f = f 4 where
     f 0 = 0
     f x = f (x-1)

The first ``f`` is a separate symbol as the LHS is in an outer scope compared to the rest. ``f 4`` and ``f (x-1)`` both refer to the function definition, but you will get a shadowing warning as it is bad style.

Control structures
==================

These are things that can show up in blocks and have blocks as arguments.

Steelman 6A. The (built-in) control mechanisms should be of minimal number and complexity.

Well, Stroscot aims for functionality, so maximal number and complexity.

6A (cont'd).  Each shall provide a single capability and shall have a distinguishing syntax. Nesting of control structures shall be allowed.

This part seems fine.

6A (cont'd) There shall be no control definition facility.

Nope nope nope, definitely you will be able to define control structures.

6A (cont'd) Local scopes shall be allowed within the bodies of control statements.

5G. The language shall distinguish between open scopes (i.e., those that are automatically included in the scope of more globally declared variables) and closed scopes (i.e., those in which nonlocal variables must be explicitly Imported). Bodies of classical control structures shall be open scopes.

This is pretty interesting, sort of a Python-style thing where variables are in scope in the whole body of a procedure.

6A (cont'd). Control structures shall have only one entry point and shall exit to a single point unless exited via an explicit transfer of control (where permitted, see goto), or the raising of an exception.

This is pretty vague with regards to what constitutes a "point" but I interpret it as the condition enforced on imperative program threads that they execute one action at a time and this action must be chosen deterministically. It seems that Stroscot's use of the continuation monad enforces this structured approach so it isn't an issue.

.. code-block:: none

  a = if true then 1 else 2 -- just a function if_then_else_ : Bool -> a -> a -> a
  {
    x = mut undefined
    if true { x := 1 } else { x := 2 } -- if on blocks
    print x
  }
  repeat while x > 0 { x -= 1 }
  repeat until x == 0 { x -= 1 }
  repeat 10 times { x -= 1 }
  repeat { x -= 1 } while x > 0
  repeat
    x = x * 2
    if (x % 2 == 0)
      break

.. code-block:: none

  check {
     risky_procedure
  } error {
     fix(error) or error("wtf")
  } regardless {
     save_logs
  }

More here: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions

Also the do-while block from `Rune <https://github.com/google/rune/blob/main/doc/rune4python.md#looping>`__:

.. code-block:: none

  do
    c = getNextChar()
  while c != ‘\0’
    processChar(c)

The do-block always executes, and the while-block only executes if the condition is true, after which we jump to the start of the do-block. If the condition is false, the loop terminates. This avoids the common C/C++ assignment-in-condition hack:

.. code-block:: cpp

  int c;
  while ((c = getNextChar()) != '\0') {
    processChar(c);
  }

Steelman 6E. There shall be an iterative control structure. The iterative control may be exited (without reentry) at an unrestricted number of places. A succession of values from an enumeration type or the integers may be associated with successive iterations and the value for the current iteration accessed as a constant throughout the loop body.

 https://www.ccs.neu.edu/home/shivers/papers/loop.pdf / https://www.youtube.com/watch?v=PCzNwWmQdb0

Goto/Break/continue
-------------------

Steelman 6G. There shall be a mechanism for control transfer (i.e., the go to). It shall not be possible to transfer out of closed scopes, into narrower scopes, or into control structures. It shall be possible to transfer out of classical control structures. There shall be no control transfer mechanisms in the form of switches, designational expressions, label variables, label parameters, or alter statements.

`Core <https://github.com/core-lang/core/issues/44>`__ proposes to drop break and continue due to implementation complexity and mental complexity. He argues that it is clearer to use an extra boolean variable and only slightly clunkier. Per the `structured program theorem <https://en.wikipedia.org/wiki/Structured_program_theorem#Implications_and_refinements>`__ it is possible to compute any computable function with three control structures, semicolon, if, and while (and no break/continue). There are drawbacks in that the theorem usually must introduce additional local variables and duplicate code. For example consider `this program <https://ecommons.cornell.edu/bitstream/handle/1813/34898/bbang.pdf?sequence=2>`__::

  start = state0
  state0 | a0 = halt
         | a1 = p01; state1
         | a2 = p02; state2
  state1 | a1 = halt
         | a0 = p10; state0
         | a2 = p12; state2
  state2 | a2 = halt
         | a1 = p21; state1
         | a2 = p20;

A translation into structured programming loosely based on the paper::

  state = mut 0
  halt = mut false
  while !halt
    if state == 0 then
      if α1 then
         p01; state := 1
      else if α2 then
         p02; state := 2
      else halt := true
    else if state == 1 then
      if α2 then
        p12; state := 2
      else if α0 then
        p10; state := 0
      else halt := true
    else
      assert (state == 2) //must be state 2
      if α0 then
         p20; state := 0
      else if α1 then
         p21; state := 1
      else halt := true

Notice this is longer than the original description using recursion, mainly due to the extra variables. S. Rao Kosaraju proved that with arbitrary-depth, multi-level breaks from loops it's possible to avoid adding additional variables in structured programming, but known algorithms still duplicate code. In common cases the duplication can be avoided by clever structuring though.

Per https://hal.inria.fr/inria-00072269/document Table 5, the most common flow-affecting constructs in Java were (as a percentage of methods) return (65.5%), short-circuit operators (13.2%), single-level break (3.6%), single-level continue (0.3%), and labelled break/continue (0.13%). A `different comparison <https://sci-hub.se/10.1002/spe.2298>`__ (Table IV) finds that continue and goto are about equal in frequency in C, that synchronized and continue are about equal in frequency in Java, and break is about half as common as try/catch/throw in Java.

In Stroscot, it's not too hard to provide break/continue/goto within the continuation-based I/O model, and many C programmers will expect these operators, so they are planned to be implemented. They will be provided as functions, rather than as keywords, so will be imported and not steal syntax by default.

Programs
========

A program is a block, and every declaration is a macro or control structure.

So top-level statements and function calls are allowed. For example you can implement a conditional definition:

::

   if condition
      a = 1
   else
      a = 2


Reasoning footprint
===================

Per `this Rust blog post <https://blog.rust-lang.org/2017/03/02/lang-ergonomics.html#implicit-vs-explicit>`__, the "reasoning footprint" is the information needed to confidently understand what a particular line of code is doing. Generally one has no control over the reasoning footprint - like an engine, complicated code has a lot of moving parts, and if you just look at a component without knowledge of the overall design you'll be lost.

So what is important is ensuring that the necessary contextual knowledge is easy to find. This can include documentation, but more specifically it means a chain of misbehavior is easy to track down to its source.

In the absence of any cues, a programmer might naturally assume that nothing interesting is going on in a piece of code. But maybe this code is very important - then the programmer will want to annotate the code, making it more verbose and noisy even though it is not necessary. A comment would suffice, but apparently programmers don't read comments, because comments aren't checked by the compiler and are often out-of-date. So what we want is syntactic control - the author can insert details of the code's execution, to explaibn how the code works in the context of the system. More formally we want properties as follows:

* All information can be elided, without any heads-up that this is happening
* The elided information can radically change program behavior
* The elided information can depend on the entire execution context (the rest of the code) to fill itself in
* There should be a clear process for inserting elided details and making it more explicit, without changing behavior
