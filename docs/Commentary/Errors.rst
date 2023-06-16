Errors
######

Error levels
============

In general flags can take 4 levels: ignore, warn, fatal, and fix. Ignore ignores the issue as much as possible and produces no output. Warn issues a warning but otherwise ignores the issue. Fatal stops the compilation from succeeding (but not from continuing, see next). Fix automatically constructs a fix for the issue and either modifies the source file(s) in-place or outputs a patch.

Don't stop
----------

(Soundtrack: `"Another Way" by Gigi D'Agostino <https://www.youtube.com/watch?v=0SdqOC8NA7s>`__)

Stroscot is designed so that as few errors as possible actually stop compilation. Generally one wants to see as many errors as possible, because rerunning the compiler every time you fix an error is slow. Continuing with compilation allows finding all the errors in a file, hence produce more information than errors that simply stop at the first one. This choice is inspired by the ``--keep-going`` option in many build systems and the ``-fdefer-type-errors`` flag in GHC. In both cases a "stop the world" error is turned into a diagnostic; this is clearly a trend that should be jumped on.

Of course in a CI environment, some "fatal" errors should still prevent the build from succeeding. Probably this is subjective and defined per-project, as a quality standard. Stroscot's build system should be smart enough to produce a build for debugging purposes while still propagating the fatal errors and not proceeding with tests or other downstream tasks.

Onw reason to stop at the first error is to avoid wasted CPU cycles, but CPU is cheap these days and incremental building means going as far as possible is probably cheaper overall than rerunning from scratch each time. Another reason to stop at the first error is to avoid console scroll; for this there is a flag ``-ferror-limit=123`` to limit output.

Level presets
-------------

One default is too restrictive, so we have several, tagged by year, that can be used just like the hard levels, e.g. you can write ``-Wunused-var=default2014``. The values are chosen by a voting process every year or some other reasonable method. More powerful are warning presets, sets of configuration values. You can apply several warning presets and then override them with a list of options like ``--warning-preset=default2014 --warning-preset=A --warning-preset=B -W3=error -Werror``.

We want to be able to set levels on a per-project, per-file, and even within-file (per code section) basis, so as to avoid having to configure them several times, and to allow distributing self-contained pieces of code. So we need syntax for all of these. The traditional Werror that turns warnings into errors is an example of a preset transformation. So we even have higher-level programming with presets.

The 'fatal' level can cause problems in build scripts. In particular it introduces a hard dependency on the compiler's diagnostics. For example, consider running:

* Compiler version 1 on program P with W1=ignore, W2=error
* Compiler version 2 on program P with W1=error, W2=ignore, W3=error

W2 is fine and will not cause any problems upgrading. But W1 and W3 will break the build when upgrading, if the error is triggered. W3 is usually easy to spot as it is new code. But W1 can happen without any notice if the compiler's diagnostics are refactored to be more precise. The solution in both cases is to change the fatal errors to warnings, and then to gradually fix them. The fatal->warning change should follow the pattern of code ownership: first the whole project as a quick fix, then back to fatal for the project and off for each affected file, then on for the files and off for affected sections, and finally fixing each affected section. The "don't stop" means we can actually short circuit this process and go straight from compiling the whole project with fatal errors disabled to fixing the warnings one by one, but of course that is not feasible in large projects.

We can formalize this process with two presets: a 'before' preset warns rather than fatally erroring on any issues that the old compiler didn't bother about, and an an 'after' preset that fatally errors on such issues. Of course this doesn't help with downgrading or cases where the fatal error cannot be turned into a warning.

Error types
===========

One question is whether you can run the program with some well-defined language semantics, despite the error. Typically syntax errors and unbound identifiers are treated as outside the language, so that programs cannot execute. But if we introduce the idea of "auto-correct" then we can find a minimal correction to apply to make the program work, and then syntax errors no longer mean the program is malformed. 


 per the language semantics, but a type signature or assertion is violated, meaning that the programmer's intentions are not satisfied. Typically we want to output a "crippled" program that fails when it encounters the error, rather than attempting a fix.

Another distinction is compile-time vs. runtime. Generally runtime errors have the execution trace available and produce more precise information, but only appear under the right testing conditions, while compile-time errors are reported earlier and cover all possible executions. In Stroscot, we muddy this distinction and try to get the best of both by using model checking. Model checking essentially simulates all runs of a program at compile time. So, like compile-time errors, it covers all executions and reports errors fairly quickly (although model checking, similarly to type checking, still has pathological cases that will cause state space explosion and timeouts). But like a run-time error, a model checking failure will end up producing a counterexample, basically a failing execution trace, so we get the precise information. The counterexample can be overly complicated compared to practical executions, so we may need to apply minimization. But a small price to pay for avoiding confusing type inference errors that don't correspond to any runtime execution failure.

Error messages
==============

Error messages are the UI of the compiler. Languages such as `Elm <https://elm-lang.org/news/compiler-errors-for-humans>`__ and Rust claim to have invested significant effort into improving their error messages, so Stroscot probably should too. Fortunately this has been researched since 1965, with a literature survey in :cite:`beckerCompilerErrorMessages2019`, so we aren't going in blind.

Location
--------

Providing the context, namely the location of the error and the relevant program code, is quite important. Ideally, the error message should reported at the location where the fix should be directed, however this is often hard to uphold. For example, a missing close brace or quote may be detected by the parser only at EOF. Indentation sensitivity mitigates the close brace issue. Figuring out where a quote should be inserted probably requires a ML model.
Another tricky example is ``a = <expr>; ...; assert (a != 0)``, where the assertion is much later than the creation of the value that caused the error. Furthermore, the assertion expression may involve multiple variables, and it could be any one of them that is wrong. For simple cases, we could have a summarizer that tries to guess the important variables, but most likely we cannot give a fix-directed location and will have to use a format where the assertion is the primary location and potential fixes are merely hinted at.

 and outputs the callstack or other traditional details. Macros have similar problems - is the error in the macro use site or definition site?

 such as  (explicitly or as an IDE annotation) and relevant symbols, identifiers, literals, and types involved in the error, as well as the program state such as variable values and stack traces. If an error message can appear in different contexts or could be sourced to multiple locations then disambiguate.


* Reduce cognitive load: Include all relevant information and reduce redundancy so the user does not process the same information twice. Use multiple modalities to provide feedback. The error message should use the minimal amount of boilerplate so that a developer can process the information quickly. But there should also be enough that someone who has never seen the message before can understand it.




Richer error handling such as a location system also introduces a performance concern, requiring more compiler engineering. For example we need an efficient mechanism for storing the start/end source location spans, consisting of two (filename, line number, column number) tuples, as passing around fully formatted strings would be slow. Go uses a map between locations and integers where file A maps to 1-100 and file B maps to 101-200, so that e.g. 150 maps to file B byte offset 50. But it isn't clear how to make this incremental, as removing a file causes all the integers to change. One idea is to store (filename hash : U32, byte offset : U32), since files are unlikely to be larger than 4 gigabytes. Whatever the solution, we should be able to compare same file, before/after within files, and if two locations are equal.

The wording may be important. A Java editor called Decaf intercepted and re-worded 30 of the most frequent Java error messages, and was found to significantly reduce error frequency and indications of struggling students. However a different study did not, suggesting the effects are weak. Still, some basic attempt at clear and friendly language is appropriate. Specific guidelines from :cite:`beckerCompilerErrorMessages2019`:

* Aim for readability and ensure comprehension by using plain/simple language, familiar vocabulary, and clear/concise/brief messages. Avoid cryptic jargon. There are multiple formal measures of readability for ‘normal’ prose, such as the Fry Readability Graph, Flesch formula, Dale-Chall formula, Farr-Jenkins-Paterson formula, Kincaid formula, Gunning Fog Index, and Linsear Write Index, but nobody has applied these to programming errors or devised a formal readability metric.

* Use a positive tone, and generally aim for a consumer UX: Novices are shaken, confused, dismayed, and discouraged by violent, vague, or obscure phrasing. Messages should be polite, restrained, friendly, and encouraging, making the computer seem subservient. Negative words like incorrect, illegal, and invalid should be avoided. Also `general UX guidelines <https://www.oreilly.com/library/view/designed-for-use/9781680501902/f_0298.xhtml>`__ advise to not place fault or blame, scold, or condemn the user (programmer). Sarcastic humor also seems counter-productive, although minor 'fun' humor may be OK but runs against briefness. Another `study <https://faculty.washington.edu/ajko/papers/Lee2011Gidget.pdf>`__ found personified I-messages such as "I don’t know what this is, so I’ll just go on to the next step" improved novice's knowledge acquisition rates and thus amount of levels completed in a set time. Of course `others <https://www.codewithjason.com/whos-blame-bad-code-coders/>`__ argue the coders are objectively the ones at fault, but this seems to be an impossible to win argument, like arguing that your girlfriend is fat. Even if it's true winning the argument doesn't make anyone better off. Psychology is weird. For children, the computer should not appear as if it is a sentient human, so as to develop the correct mental model.

* Provide a catalog of similar error examples (`Elm <https://github.com/elm/error-message-catalog>`__, `Rust <https://doc.rust-lang.org/error-index.html>`__): Providing handpicked, worked examples of how each error message is triggered can improve novices' understanding and also function as a compiler test suite. Particularly a side-by-side incorrect/correct layout with the differences highlighted has been studied and found helpful. However, brevity offers many advantages, and a study showed novice programmers can be confused as to whether the example code in the message is their code. There is also the issue of overdependence on programming by example. As such relegating the examples to a separate webpage, so there is a clear separation of example from actual, seems the best approach. For example, Rust and Microsoft give each error message a unique ID, and then has a page of all the IDs and their description. This catalog and ID mechanism has not been studied in the literature and poses a discoverability hazard, but a hyperlink in the error message seems sufficient - showing the catalog entry in the error message would be documentation overkill unless it is really short. The quintessential error catalog is Stack Overflow, which indexes both standard error messages and obscure library codes or memory addresses. Popular responses are upvoted and can be quite useful to both novices and experts. Compared to formal reference documentation, the catalog can provide briefer and more concrete and specific assistance. With a feedback loop between catalog and compiler, error message codes can be refined to cover common issues more precisely. However it should be noted that there is little point in trying to organize the catalog with categorization - agreement among category raters was only 60% in :cite:`mccallNewLookNovice2019`. It is better to use a flat list and focus effort on specific tricky error codes rather than attempting to find patterns among errors.

* Show solutions: The actual intent of the programmer may not be clear, but the compiler can analogize from the error catalog or other sources to guess what the programmer likely intended, and either provide a literal solution or sketch the requirements a solution must satisfy. Although debatable, my definition of the difference between an example and a solution is that the solution is phrased using specific information from the actual code, whereas the example is generic to the error ID. Also, the solution is produced only when there is a high degree of certainty for its applicability, avoiding leading the user down the wrong path. When guided appropriately by solutions, novices can repair errors approximately as fast as experts. With IDE integration, solutions may be interactively accepted and applied automatically instead of being transcribed by the user, allowing even experts to benefit from faster fixing. Elm says that every error should have a solution - this is probably overkill. Solutions are doable for trivial errors like unbound identifiers or uncaught exceptions, but many semantic errors have no obvious solution and can take weeks to work out.

* Allow dynamic interaction: A simple example is Rust's ``--explain`` flag that gives more context for some errors and for others reproduces the explanation from the catalog. This is a "tell-me-more" mechanism that allows requesting more error details. In Stroscot's case, where many contract errors take the form of failing program traces, another useful tool would be interactive omniscient debugging of these failing traces, so that the programmer can take a failure of ``assert (a != 0)`` and say "where did ``a`` come from?". Both of these cannot be the main interface, because the catalog is verbose and debugging is too time-consuming, but as options they are quite helpful.

* Provide cognitive scaffolding: A user may form the wrong conceptual model and/or move too quickly through writing the program. They then have a false sense of accomplishment. It is then the error messages's job to dislodge incorrect conceptual models and point out hasty errors. The user may also have misread the problem, but solving the wrong problem is a general issue in cognition, including startups launching and failing due to market fit, so the compiler generally can't tell that the wrong problem is being solved. Anyways, the goal is to use sufficient verbiage that the user can notice their conceptual model is wrong and search out documentation to repair it. To this end, the message should mention the key constructs and relationships that must be understood, e.g. syntactic construct names, compiler terminology, and library functions.

* Use logical argumentation (maybe): :cite:`barikHowShouldCompilers2018` analyzes error messages using Toulmin's argument model, which allows 6 components (extended to 7 by Barik):

  * The claim is the main assertion to be proven.
  * The grounds are evidence and facts that support the claim.
  * The warrant links the grounds to the claim.
  * The backing supports the warrant, usually by an example.
  * The qualifier limits the claim, explaining words such as "presumably".
  * The rebuttal acknowledges other valid views but explains why they are not appropriate.
  * A resolution is a claim that a defect will be removed with a specific change. (Added by Barik)

  StackOverflow and compiler error messages used 3 argument layouts: claim alone, a simple argument consisting of claim, grounds, and warrant, and an extended argument which is a simple argument plus backing. These layouts are multiplied times 2 depending on whether there was a resolution in the claim; my notation is that "claim" means a claim without resolution. The tested results were claim < {simple,extended}, extended < claim+resolution (claim+resolution being dubbed a non-logical "quick fix" instruction).

  Per the thesis :cite:`barikErrorMessagesRational` extended arguments are mainly useful for novices and unfamiliar code. Theorizing, if the developer knows what's going on, they likely want brief messages and their preference is claim+resolution > simple > extended > others. But with an ``--explain`` flag their preference is more like extended+resolution > simple+resolution > claim+resolution > extended > simple > others. It's probably worth a survey comparing error messages of varying verbosities to confirm.

* Report errors at the right time: Generally one wants to see errors as soon as possible, using static analysis tools.

Per Elm / `Tidyverse <https://style.tidyverse.org/error-messages.html>`__ the message should have a layout like "general summary, program code fragment (location),error details / hints / suggested fix". The general summary is shown on hover in VSCode, and can be expanded downwards to see the full message. The tooltip seems to be around 120 monospaced characters wide and 5 ish lines tall. The size differs based on popup type so recheck when developing for LSP; it used to be 50 characters wide for everything. There is `an old VSCode bug <https://github.com/microsoft/vscode/issues/14165>`__ open for expandable popups, and a `CSS hack <https://stackoverflow.com/questions/44638328/vs-code-size-of-description-popup>`__ that makes them larger, but probably Stroscot has to be designed to accommodate small popups.

The code fragment shows the full line of input code with file/line number, and marks the failing expression with ``^^^```. The error and location marks should be colored red so they are easy to spot. Similarly Elm uses a blue separator line ``----`` to separate messages. With the LSP integration this is already taken care of because VSCode underlines the error location in the editor and has its own UI for browsing through errors.
