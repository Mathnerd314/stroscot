Checklist
#########

This page goes through programming paradigms, principles of language design, and discusses some design points of quite a few other programming languages.

Paradigms
=========

The programming languages checklist has a few paradigms: functional, imperative, object-oriented, procedural, stack-based, "multi-paradigm". In linguistics, a paradigm is "a set of linguistic items that form mutually exclusive choices in particular syntactic roles," specifically "a table of all the inflected forms of a particular verb, noun, or adjective." This seems to be a usable definition of a PL paradigm - you have all related versions of a semantic entity.

Unfortunately people seem to use paradigms as labels of entire languages, rather than to refer to individual syntactic features. Stroscot, like every other language, is "multi-paradigm" - even assembly is multi-paradigm since it is imperative (syscalls) and structured (conditional jump). So the adjectives "object-oriented", "functional", etc. are avoided outside of this page in favor of the specific semantic constructs, since "functional object-oriented language" sounds weird. Still, it's good to have a map from paradigms to constructs, and to know which constructs embed into which other constructs. This list is based on Wikipedia's `list of paradigms <https://en.wikipedia.org/w/index.php?title=Template:Programming_paradigms&oldid=1114006717>`__:

* Action: `action descriptions <https://en.wikipedia.org/wiki/Action_language>`__ are given by the state trajectory relation
* Array-oriented functions are still functions
* Automata-based:

  * Nondeterministic automata are given by a transition relation.
  * Deterministic automata are given by a transition relation that is a function.

* concurrency - concurrent programs are given as imperative programs that use concurrent operations

  * agents/actors/flow-based processes are threads with a main dispatch loop

* data-driven programming is a main loop over condition-action pairs
* declarative is a logical relation or a function

  * functional - functions are total functional binary relations

    * lambas are anonymous functions

  * logic - a logical relation is a set of tuples

    * boolean operations are logical constraints, i.e. relations over a certain domain

  * constraint: constraints are 0-1 loss functions in an optimization problem
  * dataflow is a block in single static assignment form
  * a reactive or incremental program is a state value plus a state update function or command
  * a query is a function that takes a database and produces a list of results

* differentiable: the `derivative <https://en.wikipedia.org/wiki/Fr%C3%A9chet_derivative>`__ is a function mapping a function :math:`f` to a linear operator :math:`A` such that :math:`\lim _{\|h\|\to 0}{\frac {\|f(x+h)-f(x)-Ah\|}{\|h\|}}=0`.
* dynamic: eval is a function from strings to values (and optionally with an environment)
* event driven: an ED program is some event handler functions, data binding event handlers to events, and a main loop function (provided by a library) that repeatedly checks for events and calls the matching event handler
* generic functions are just functions over a large domain
* imperative programming:

  * commands can be represented as a tag (payload) plus a callback function returning another command
  * mutable variables are using read and modify functions on an implicitly passed/returned store.
  * procedures are functions from arguments to commands

* Metaprogramming:

  * Attribute-oriented: attributes are a function from symbols to metadata
  * Macros: macros are functions that take an AST and a lexical environment

* Nondeterministic: a nondeterministic function is a relation
* Parallel: a block in single static assignment form can be easily parallelized using a concurrent worker pool
* Process-oriented programs can be represented using concurrent operations
* probabilistic programs are functions from parameters to a log probability
* Quantum:

  * quantum logic gates are functions, in particular unitary operators on states of qubits
  * a quantum program is a block, consisting of gate applications and discarding information (Qunity)

* Set-theoretic: set membership is a boolean predicate function
* Stack-based: a stack-oriented program is a function on stacks, a.k.a. lists
* structured:

  * loops are recursive functions
  * conditionals are lazy functions
  * Block-structured: block sequencing is Kleisli arrow composition, a function
  * Object-oriented: objects are mutable variables containing records of mutable variables and functions
  * Class-based: classes are types
  * recursion is syntax for applying a fixpoint function

* Symbolic: an AST is a value
* Value-level: types are sets

In addition I've found some other paradigms too obscure for the WP list:

* term rewriting systems are given by the rewriting relation
* optimization problems are relations based on on objective functions
* optimization solvers are functions from objective functions to a list of solutions
* aspect-oriented: discussed on the "Aspects" page.

Some more paradigms that aren't really paradigms at all, just libraries or syntactic tricks:

* pattern matching: pattern matching, easily match and extract data from complex data structures.
* functional reactive programming: build applications that respond to changes in data over time.
* concurrent constraint programming: express and solve problems involving concurrent processes and constraints.
* genetic programming: evolve solutions to problems using principles from genetics and natural selection.

Graph of paradigms
------------------

.. graphviz::

  digraph paradigms {
    action -> relation
    array -> function
    "nondet automata" -> relation
    "det automata" -> function
    concurrency -> command
    actor -> concurrency
    agent -> concurrency
    flow -> concurrency
    actor -> loop
    agent -> loop
    flow -> loop
    "data-driven" -> loop
    "data-driven" -> condition
    "data-driven" -> function
    "data-driven" -> command
    declarative -> relation
    declarative -> function
    lambda -> function
    function -> relation
    relation -> set
    boolean -> constraint
    constraint -> optimization
    dataflow -> block
    reactive -> function
    reactive -> command
    query -> function
    differentiable -> function
    dynamic -> function
    event -> function
    event -> loop
    generic -> function
    command -> function
    "mutable variable" -> function
    procedure -> function
    attribute -> function
    macro -> function
    nondeterministic -> relation
    parallel -> block
    parallel -> concurrency
    process -> concurrency
    probabilistic -> function
    quantum -> function
    quantum -> block
    set -> boolean
    stack -> function
    loop -> function
    loop -> recursion
    conditional -> function
    block -> function
    object -> "mutable variable"
    class -> type
    recursion -> function
    type -> set
    "term rewriting" -> relation
    optimization -> relation
    optimization -> function
  }

Graphviz has chosen "function" as the central paradigm. This agrees well with experience. Quoting `Spivak <https://www.google.com/books/edition/Calculus/7JKVu_9InRUC?hl=en&gbpv=1&bsq=central%20objects>`__, "the most important concept in all of mathematics is that of a function - in almost every branch of modern mathematics functions turn out to be the central objects of investigation." Looking closer, function is part of an SCC ``function, relation, set, boolean, constraint, optimization``. Although lambdas provide a natural way to express many functions, the mathematical notion of function is broader than just lambdas - some mathematically definable functions have no efficient/constructive algorithm and are instead specified as a logical relation or optimization predicate. So we need constraint logic programming as well to get the full notion of "function". Hence the ultimate paradigm is functional logic programming. Thus, Stroscot is at its core designed to be a functional logic programming language, but with support for many other programming paradigms implemented via the embeddings described above.

Principles
==========

Paradigms are vague and only express common patterns or features; they cannot be used to determine the specific design of those features. So instead we have principles. What are these principles actually useful for? Mainly wasting time. Practical decisions are are made by judging pros and cons and these general principles are hard to apply. But, in theory, a pull request or design choice can be declared "wrong" according to a principle, and a list of such principles can avoid wasted work. Contributors can also discuss changing the principles if a desired change is not compatible.

* Immature poets imitate; mature poets steal; bad poets deface what they take, and good poets make it into something better, or at least something different. The good poet welds his theft into a whole of feeling which is unique, utterly different than that from which it is torn. (T. S. Eliot)
* Make the irreducible basic elements as simple and as few as possible without having to surrender the adequate representation of a single datum of experience. (Albert Einstein)
* Write a prototype implementation. Conduct an A* search through the possible solutions, stopping early if the potential solution is clearly worse than the prototype. Periodically take the best solution out of all discovered so far and implement it as the new prototype. (`Branch and bound <https://en.wikipedia.org/wiki/Branch_and_bound>`__)
* Never finished, never complete, but tracking the progress of technology (Lennart Poettering)
* Design to do the hard things every day. Take all the common daily tasks considered to be painful and hard, and figure out a design that will allow each task to be accomplished efficiently in a few seconds of actual work. It is unacceptable to require detailed pre-planning or coordination for each task. The biggest overheads should be testing the result and writing documentation. (Linus on `why Git works so well <https://web.archive.org/web/20170920135205/https://www.linuxfoundation.org/blog/10-years-of-git-an-interview-with-git-creator-linus-torvalds/>`__)
* As size and complexity increase, architectural design dominates materials. Create durable, non-leaky, beautiful interfaces. (`VPRI <http://www.vpri.org/pdf/tr2011004_steps11.pdf>`__, `John Regehr <https://blog.regehr.org/archives/666>`__)
* If it isn't documented, it doesn't exist (Coding Horror). Corollary: There is no undefined behavior, only undocumented behavior.
* Take a list of items. Imagine a specific walk through a familiar place. List distinctive features of the route. Combine each feature with an item to form new outrageous/memorable images. (Memory palace)
* People prefer a middle level of complexity: too simple and we are bored, too complex and we are confused. Moreover, the ideal level of complexity is a moving target, because the more expert we become at any subject, the more complexity we prefer. (Donald Norman)
* Better depends on your goodness metric (`Jim Waldo <http://web.archive.org/web/20210325222034/https://www.artima.com/weblogs/viewpost.jsp?thread=24807>`__)
* The shorter the [edit-test] cycle, the happier the programmer. (`Yue Yao <https://tripack45.github.io/2018/11/03/edit-compile-run/>`__)

  Per `here <https://web.archive.org/web/20060213015737/http://blogs.msdn.com/peterhal/archive/2006/01/04/509302.aspx>`__, the cycle time can be broken down into 70% Understanding Code, 25% Modifying Existing Code, 5% Writing New Code. In particular we estimate that there is 14x as much read time as write time. But this estimate is probably only appropriate for application code - the true average varies depending on scenario. Per APL, if a language is quick to program in, it may be faster to write small programs from scratch than to read and understand another person's program. So the 70/25/5 may turn into something more like 50/20/30 in a scripting context, only a 1.6x read-write factor. On the other hand, common library functions may be read many times but only modified or added rarely, giving read/write factors of 100x, 1000x, or more.

* Do all things without grumbling or complaining (Philippians 2:14)
* Secure by default: The default level of access should have the least privilege and the most number of checks. (OpenBSD)
* Organize functions by functionality into expressive components. (`Uli Weltersbach <https://reasoncodeexample.com/2016/03/06/a-place-for-everything-and-everything-in-its-place-thoughts-on-organizing-source-code-by-type/>`__)
* When an interface has multiple possibilities, and other principles conflict or are ambiguous, the behavior should be that which will least surprise most new novice users. In particular the behavior is not necessarily the behavior that would be the most easily implemented. (`POLA <https://en.wikipedia.org/wiki/Principle_of_least_astonishment>`__)

  This principle varies from the original in specifically defining a target audience (new novice users). Targeting other audiences such as existing programmers would make the language highly dependent upon the whims of culture, and create a vicious circle of learning ("To understand recursion, you must first understand recursion"). For contrast, per Matsumoto's `interview <https://www.artima.com/articles/the-philosophy-of-ruby>`__, Ruby was designed for *his* least surprise. That means that, in order to feel comfortable with Ruby, one must learn all of Ruby, program a few large programs in Ruby, and then constantly re-read the Ruby manual to refresh what has been forgotten. And even then you are not Matsumoto so there may be something that surprises you. Focusing on novices means that all an expert has to do is "think like an idiot" and the solution will be at hand. The expectations of novices are essentially constant over time, because they depend on human psychology rather than experience. This principle is essentially the approach taken in designing the Quorum programming language. Every person goes through a short "what is this feature" phase, which novice-friendly syntax will make straightforward, followed by a much longer cycle of routine reading and writing for which the syntax makes no difference.

Non-principles
--------------

Simplicity
~~~~~~~~~~

In his talk "Simple Made Easy", Rich Hickey lists four words (etymologies from Wiktionary rather than him):

1. simple - literally "same fold", consisting of a single part or aspect. An objective criterion about avoiding too many features, basically minimalism.
2. complex - braided together or weaved together. Hickey also uses "complect", meaning to braid things together and make them more complex. Also an objective criterion, about avoiding feature overlap.
3. easy - literally "lying next to", "bordering on". A subjective criterion about a task being within the grasp of a particular person and toolset.
4. hard - literally "strong" or "powerful". A subjective criterion about whether changing the software requires a lot of effort.

Hickey tries to say that simple is the opposite of complex and easy is the opposite of hard, but the etymologies and definitions don't really agree. We must be careful about distingishing these words. Consider this `$1 Split Woven Pouch Single String Sling <https://www.youtube.com/watch?v=M_wNutoddYE>`__. It's simple, because it's only one string. It's complex, because he weaved the string with itself. It's easy to make, because you just have to buy the string and follow the tutorial. It's hard, because he made the knots really tight and the finished product is quite stiff. So these qualities are not mutually exclusive at all.

Similarly, Stroscot aims for all four of these:

* simple - Stroscot does aim to be "simple", in the etymological sense of "minimalism". Stroscot concentrates the language into a "core", a basis of features that can express all others and is as small as possible.
* complex - The rest of the language, the standard libraries, user, libraries and user programs, braids the core features together and is "complex". Hickey argues that a project should not be complex (complected), but he is using a different metric, of how interleaved the conceptual parts of the program are, rather than its interleaving of language features. There is some benefit to ensuring a tree-structured call graph in a program, but I don't think this is a good argument to remove recursion.
* easy - Stroscot aims for its standard library to make things "easy". There's no downside, right?
* hard - Stroscot also aims to have a "strong", "powerful" standard library, that doesn't change often, in other words a "hard" standard library.

Looking at this, it doesn't seem that the language can be marketed as simple; there are just too many mixed messages. The fault does not lie with Stroscot, but rather the inadequacy of these words to express all aspects of an intricate design.

Familiarity
~~~~~~~~~~~

Per Grace Hopper, "the most dangerous phrase [one] can say is 'We've always done it that way'." According to `some guy <https://medium.com/geekculture/3-busted-myths-about-the-35-hour-week-that-you-should-present-to-your-boss-efa5403bb263>` the golden rule at his university was that anyone who said that phrase was a lousy engineer. Hopper `continues <https://books.google.com/books?id=3u9H-xL4sZAC&lpg=PA9&vq=%22most%20dangerous%22&pg=PA9#v=snippet&q=%22most%20dangerous%22&f=false>`__`: "If we base our plans on the present, we fall behind and the cost of carrying out something may be more costly than not implementing it. But there is a line. If you step over it, you don't get the budget. However, you must come as close to it as you can. And you must keep pushing the line out further. We must not only accept new concepts, we must manage their development and growth."

Per `Simon <https://soc.me/languages/familiarity>`__, C’s operator precedence, C++’s use of ``<>`` for generics, and C#’s design of properties are all examples of suboptimal, legacy decisions. They were designed based on limited information but in hindsight it has become clear that better choices exist. Nonetheless they continue to be adopted by new languages on the basis of "familiarity" - people are so used to the suboptimal behavior that they will complain if it changes.

For Stroscot, is it worth repeating these mistakes for the benefit of "familiarity"? Familiarity will not help beginners learn the language. Let us consider the various adopted principles:

* stealing ideas - we should understand why these choices were made, and consider if those reasons are still valid. For C's operator precedence, there is essentially no basis - it is just historical baggage.
* irreducible elements - do we need these operators at all? Probably so, because they are in these other languages and people have used these languages for a while. But it is just syntax, so extensible syntax is sufficient - it does not have to be part of the language core.
* branch and bound - this says we should aim for the optimal choice, but first we have to define optimal
* Poettering - he broke your audio. I think he'd decide to break your programming habits too
* Productivity - these sorts of syntax decisions are minor annoyances, so don't really impact the ability to accomplish things
* Beautiful interfaces - a consistent interface is more beautiful than an inconsistent one
* Documentation - whatever the choice, the process for arriving at it should be clearly written down
* Memory palace - a consistent interface is also easier to remember
* Medium complexity: programming is hard enough by itself, the language doesn't need to be more complex
* Goodness metric: these principles are sort of subjective, but I don't know what else to use
* Edit-test cycle: Experienced Stroscot programmers will get tripped up because it's a bad choice. Novice programmers will be slow regardless. Expert programmers from other languages may have to invest more time in learning the language.
* Philippians: With the good decision, expert programmers from other languages may complain due to change aversion. But nobody who uses Stroscot for a significant amount of time will complain. With the bad decision, complaints will come in forever.
* Secure by default: It is possible to avoid operator precedence by requiring parentheses all the time. This is safer as nobody will be confused.
* Organize:
* Least astonishment: we should aim for "the behavior that will least surprise someone who uses the program". , rather than that behavior that is natural from knowing the inner workings of the program. (`POLA <https://en.wikipedia.org/wiki/Principle_of_least_astonishment>`__)

Ideally, changes will make experienced programmers so fed up that they will post "ragequit" posts to social media. So long as discussion can point to a solid basis for the changes, these will most likely serve to draw positive attention to the language. Anybody who uses the language for a while will get used to it. And actually the people who are willing to learn a new language are likely looking for something new and are willing to adapt, so they won't ragequit. Succinct migration guides for users from various popular languages will get these users up to speed.

There is another sense of familiarity though in the sense of creating a "brand" for the language. Some languages take this in the sense of not allowing any room for major changes in the design once the language reaches a beta. Minor migrations would be possible, but for example switching from curried to uncurried functions would be forbidden because they would annoy too many people. This requires doing essentially all of the designing up-front. I'm kind of split on this. On the one hand, there is the "durable" part of the "create interfaces" principle -

Another important concept is being intuitive/memorable, as can be tested via cloze completion and "what does this piece of code do". Ideally someone should be able to read the manual and write some throwaway Stroscot code, abandon Stroscot for 6 months, and then come back and correctly type out some new Stroscot code without having to look at the manual again. If Stroscot the language is a moving target this goal is difficult to accomplish. That being said though, like Poettering said nothing is ever finished and it is better to track the progress of technology.

Readability
~~~~~~~~~~~

Using the literal definition, "ease of understanding code", readability is included as part of the edit-test cycle time principle. Cycle time has the benefit of being a lot more empirical - just provide some code and an editing task, time it, and average across a pool of subjects. In contrast, readability is more subjective - the author of some code will most likely consider his code perfectly readable, particularly immediately after writing said code, even if an average programmer would not. Of course, in a week or a few years, depending on the author's memory, any domain-specific knowledge will fade away and the author will struggle with his code just as much as any average programmer, but waiting ages just to measure readability is not feasible.

Most articles that discuss readability go on to describe "readable code", defined by various properties:

* Meaningful variable and function names ("self-commenting")
* Consistent identifier style, indentation, and spacing
* Comments that explain the purpose of each function
* Comments that explain non-obvious parts
* Intermediate variables to avoid complex expressions
* Intermediate functions to avoid deep nesting of control structures and ensure each function has a single purpose
* Parentheses that make the order of operations clear

These definitions are somewhat subjective and unreliable. What makes a name meaningful? How deep and complex can an expression/function get before it needs to be broken up? Should the "consistent identifier style" be camel case or snake case? With a loose reading, most libraries and style guides qualify as readable, in that there is always somebody who will argue that the existing choice is the best. The cycle time principle provides a framework for evaluating these choices objectively, although it is still dependent on a subject pool and hence the scientific literature. In fact studies have validated many specific guidelines as empirically reducing time to understand, e.g in the underscores vs camel case debate finding a definitive benefit for underscores.

Cycle time also accounts for the aphorism "Perfect is the enemy of good". One could spend hours optimizing for readability by fixing spelling mistakes and other nits and not get anything useful done. In the time it takes to write a long descriptive comment or poll coworkers for a meaningful variable name, one could have skipped writing comments, used 1-letter names, run and debugged the code, and moved on to a new task. Perfect readability is not the goal - the code just has to be understandable enough that any further readability improvements would take more cycle time than they will save in the future. And with hyperbolic discounting, reducing future maintenance effort is generally not as important as shipping working code now. This calculation does flip though when considering the programming language syntax and standard library, where small readability improvements can save time for millions of programmers (assuming the language becomes popular, so there is again a discounting factor).

Not included in cycle time (or readability) is the time to initially write a program. Maintainance cost is much more important in the long run than the initial investment.

Terseness
~~~~~~~~~

APL is terse mainly due to its use of symbols, and :cite:`holmesAPLProgrammingLanguage1978` mentions that some consider terseness an advantage. But is it really? Again the principle for Stroscot is the edit-test cycle time. An APL program may be short but if the APL program requires looking up symbols in a vocabulary while a normal word-based program is a little more verbose but self-contained, then the word-based program wins on cycle time.

Iverson argues the human mind has a limit on how many symbols it can manipulate simultaneously. A terser notation allows larger problems to be comprehended and worked with. But this ignores the role of chunking: a novice chess player works with symbols representing individual pieces, while an expert player works with symbols representing configurations of the entire board. Similarly, a novice programmer might have to look up individual functions, but a programming expert will work on the level of program patterns, for example CRUD or the design patterns of Java, and the amount of verbiage involved in writing such patterns is immaterial to mental manipulation but rather only becomes relevant in two places:
* the time necessary to scan through unfamiliar codebases and comprehend their patterns. This can be reduced by making programming patterns easy to recognize (distinctive). APL's overloading of monadic and dyadic function symbols seems to conflate distinct functions and go against this consideration.
* the time needed to write out patterns when moving to implementation. Most programmers type at 30-50 wpm and use autocomplete, which means that even a long identifier requires at most 1-2 seconds. In contrast, for APL, symbols might found with the hunt and peck method, per `Wikipedia <https://en.wikipedia.org/wiki/Typing#Alphanumeric_entry>`__ 27 wpm / 135 cpm or 0.4 seconds per symbol. So APL is faster for raw input. But in practice, most of the time programming is spent thinking, and the time writing the program out is only a small fraction of coding. So what is important is how easy it is to remember the words/symbols and bring their representations to mind (the "memory palace" principle), for which APL's symbols are at a disadvantage due to being pretty much arbitrary.

There is some advantage to terseness in that shorter code listings can be published more easily in books or blog posts, as inline snippets that do not detract from the flow of the text. Documentation works better when the commentary and the code are visible on the same medium. But readability of the code is more important - a barcode is terse too but provides no help without scanning it. Web UX design provides many techniques for creating navigable code listings, e.g. a 1000-line listings to be discussed in a short note with a hyperlink. Accordion folds can be used for 100-line listings, and 10-line listings can be in a two-column format or with a collapsed accordion fold. So this advantage of terseness seems minimal when considering that code is mostly published on the web these days.

Turtles all the way down
~~~~~~~~~~~~~~~~~~~~~~~~

This is an Ecstasy principle. But it's misleading - going infinitely downward would require infinite space. Actually it is a finite list plus a trick to make it infinite, namely that the objects at some point refer back to themselves. This pointing trick is the useful part, hence why Stroscot supports infinite structures. But this sort of "can you do this trick?" question is covered by the productivity principle.

Remember the Vasa
~~~~~~~~~~~~~~~~~

Bjarne Stroustrup `seems fond <https://www.stroustrup.com/P0977-remember-the-vasa.pdf>`__ of the phrase "Remember the Vasa" to warn against large last-minute changes. According to `Wikipedia <https://en.wikipedia.org/wiki/Vasa_(ship)>`__, the Vasa was a ship that sunk because the center of gravity was too high. Despite rumors that it was redesigned, there is no evidence that any alterations were performed during construction. It appears to have been built almost exactly as its designer Henrik Hybertsson envisioned it. And the design was obviously incorrect - a survey of shipwrights at the inquest after the sinking said the ship design "didn't have enough belly". So the only lesson I get is to learn from experienced designers to avoid making mistakes. But this is just T.S. Eliot's principle to steal from great poets.

Hungarian notation
~~~~~~~~~~~~~~~~~~

Hungarian notation puts abbreviated type annotations in variable names, so humans can check that the types are correct. But the compiler already checks declared types, automatically and much more thoroughly. So in the end it is noise. Mathematicians do use single-letter variables with subscripts, but these do not encode types, they are just abbreviations - e.g. ``x`` stands for "first coordinate". Per `Stroustrup <https://www.stroustrup.com/bs_faq2.html#Hungarian>`__ it is "a maintenance hazard and a serious detriment to good code. Avoid it as the plague."

Keep syntax and semantics separate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Although both syntax and semantics are essential to a programming language, they are not on an equal footing. A given semantics may have many valid syntaxes, but there is generally only one semantics for a given construct (otherwise, it would be a different construct). Most considerations involve both syntactic and semantic components. Consider some examples:

* Allowing both ``1+2`` and ``1.0+2.0``: One "solution" is to use different operators, such as ``+`` and ``+.`` in OCaml. But actually allowing this requires a semantic solution, such as typeclasses, overloading, or union types.
* Allowing ``a.b()`` for a method defined outside a class: Again, there is a "solution" of writing ``b(a)``. But actually allowing this requires a semantic solution of extension methods or implicit classes, which Flix mentions requires a lot of semantic baggage and can incur unexpected performance penalties.
* Lambdas: There is a "solution" of passing a function pointer and closure struct. But actually allowing this in C++ required (just in the standard) 28 lines of BNF (~1/2 page) plus a note for syntax, and 6.5 pages plus 2 mentions for semantics.

The addition example makes it clear that it's pretty hard to design any sort of syntax without taking into account the semantics. You may want to emulate the syntax of another language (or mathematics, in this case), but it may not be possible. So Stroscot takes the approach of "form follows function": first design the semantics, then design the syntax based on that. Thus there is a clear dependency relationship, rather than them being separate.

Of course, the distinction is more fuzzy in practice. Papers define a syntax along with their semantics, and for clarity, we use that syntax when describing the semantics in the commentary. Similarly, the semantics of extension methods were proposed in order to allow a certain syntax. But formally speaking, most of the commentary is only considering semantics. As discussed in :ref:`Syntax`, the final syntax will be decided upon by a survey describing the abstract semantics of each language construct, with all preconceived ideas for syntax removed from the descriptions as much as possible.

Another question is where to draw the line of syntactic sugar vs. language feature. Per `Wikipedia <https://en.wikipedia.org/wiki/Syntactic_sugar>`__, syntactic sugar is a shorthand for an operation canonically expressed in a more verbose form. I would say that syntactic sugar can be clearly identified by being able to write the translation as an unconditional function or macro, like the parser combinator ``some x = x <|> some x`` or the for loop:

::

  for ( init; condition; update) { body } = init; go
    where
      go | condition = body; update; go
         | otherwise = return ()

Anything more complex is a language or library feature (I don't distinguish the language and the standard library). So for example, lambdas are a language feature, not because they introduce new syntax for lambda expressions, but because the syntax for calling a lambda overlaps with the syntax for calling a function.

Adoption
~~~~~~~~

How many users should Stroscot have? Well, as with SPJ's motto of "avoid success at all costs", there is such a thing as too popular. A widely-adopted language becomes ossified, as nobody want their code broken. This can be addressed by developing "language change/evolution management" tools, like automatic migration (as in early Go) and the compiler supporting multiple language versions/dialects at once. These should allow any sorts of changes to be made with minimal breakage to users, even if the language is significantly popular, while still adding minimal delay and overhead to language development. Explicitly, I do not want governance procedures/processes like PEP's or the Rust council, which prevent language changes on a social level - never solve a problem through social means when there is a technical solution.

So with that out of the way, growth is really a social problem. Do I want to spend my days reading PR's and writing comments, as Linus Torvalds does, or spend my days coding? Well, I am not really that great a coder. I type code slowly and over-design. Honestly it would be great to design by English. But it is not like everyone will drop what they are doing and be at my beck and call. It is an exchange of interests - Stroscot will have to provide some value to users, and they will have to judge that contributing to Stroscot's vision is better than using other software. Still though, for individuals that do decide to contribute to Stroscot, I will not turn them away. I think once the technical tools for dealing with adoption are in place, SPJ's motto is in fact wrong and success is necessary and desirable.

Then there is the question of whether to focus on adoption. I think this is like performance - it definitely matters, it definitely contributes to long-term language progress, and it directly affects financial success (in terms of donations / visibility). So it is worth tracking. But like performance, it is often premature to expend significant effort on adoption. Like buying ads for the language - probably a waste of money compared to improving error messages or some such. Focusing on the core goals of Stroscot like functionality, minimality, learnability, and concision will naturally lead to user adoption in the long term. With link aggregators and a decent website, it is possible to go from zero to 100,000 users in a week (c.f. `hitting the front page <https://thehftguy.com/2017/09/26/hitting-hacker-news-front-page-how-much-traffic-do-you-get/>`__). But it takes "the perfect storm" of user interests, informative website, and positive comments and votes. I think one simple mark of progress is that the project becomes interesting enough that *someone else* - unrelated to the project - submits the project to a link aggregator. That is probably the point at which it is worth devoting attention to adoption (as opposed to learnability). A language need at least 10-15 years of development before charting on the TIOBE index or PyPL. Long-term, it is more important to avoid fading into obscurity than to shoot for #1.

Another problem, particularly for languages backed by industry, is that they get semi-popular very quickly, and then suddenly drop off the radar a few years later. This is due to being "all hype" and not really adding anything new. At least in the early days, there is some benefit to discouraging adoption, via burdensome installation requirements or frequent breaking changes. Although it slows adoption in the short term, such policies strengthen the community by forcing members to participate fully or not at all. Those who remain find that their welfare has been increased, because low-quality "what's going on'" content is removed and feedback loops are shorter. The overall language design benefits as result, and can evolve much faster. (Compare: strict religions that prohibit alcohol and caffeine consumption and modern technology, a random guy `pruning <https://castrio.me/why-i-kicked-out-90-of-my-free-members>`__ the 90% of members who have not posted a message in the past two weeks from his website)

But with this approach, one must be careful that the programming language still provides sufficient value to hold at least some amount of users - otherwise there is no feedback at all. The barriers to adoption must also be reasonable, and similarly barriers to prevent people from leaving are probably limited to implicit ones like language lock-in. It is not worth discouraging users too strongly, as these attempts can backfire with blog posts such as "my terrible experience trying to use Stroscot" or "my terrible experience trying to get rid of Stroscot", destroying what little reputation the language may have built up. Although discouraging adoption may be the policy, each individual user's interaction with the community should be engaging and issues that appear should actually be addressed.

There are not really any best practices to encourage adoption but :cite:`meyerovichSocioPLTPrinciplesProgramming2012` makes some observations.

* Numerous people have made efforts to design programming languages, but almost all of these have failed miserably in terms of adoption. Success is the exception to the rule. Contrariwise, others observe that language usage follows a "fat tail" distribution, meaning that failure is not as bad an outcome as one might expect and even a "failed" language can have some popularity.
* Successful languages generally have convoluted adoption paths, suggest that extrinsic factors are influential. (TODO: How influential? Top 10? Top 100?)
* Language failures can generally be attributed to an incomplete understanding of users' needs or goals.
* Evolution or re-invention, by basI suspect that most languages will need at least 5-10 years of development before reaching their first stable release, followed by another 5 years or so before it starts to take off. That's all assuming you end up lucky enough for it to actually take off, as there are many languages that instead fade into obscurity.ing a design on existing experiences, increases understanding.
* Surveying the literature is often advocated but rarely or never followed to a rigorous standard. The main sticking point is that it is difficult to evaluate language features accurately except by attempting to use them in a new language.
* In the `diffusion of innovation <https://en.wikipedia.org/wiki/Diffusion_of_innovations>`__ model, innovation is communicated over time through different channels along a network. Adoption is a 5-step linear process for each node:

  1. Knowledge: an individual is made aware of the language. Knowledge is spread by impersonal mass communication: blog posts advertised with SEO, links to the homepage on link aggregators such as Reddit and HN, and shoutouts on social media such as Facebook and Twitter. Generally, this process is limited by the relative advantage of the language, the amount of improvement over previous languages. The relative advantage is described succinctly as the "killer app", a story such as "we switched our <killer app> to Stroscot and sped things up by 300%" (note that this usage differs subtly from popular definitions of "killer app").
  2. Persuasion: an individual investigates and seeks information, evaluating pros and cons. An FAQ or comparison can provide initial evidence, but may be viewed as biased. Peer communication such as Discord is more effective because it is personalized. An individual may also evaluate reputation, so convincing influential, highly connected individuals and firms to promote the language can be effective. This process is limited by compatibility, how well an innovation integrates into an individual’s needs and beliefs. Consider :cite:`coburnChangeFunctionWhy2006`'s simple model ``Change Function = f ( Perceived crisis / Total perceived pain of adoption )``, where ``f`` is just the step function ``f x | x > 1 = DO_CHANGE; f _ = MAINTAIN_STATUS_QUO``. In the terminology of programming languages, a language provides a certain value, but has a switching cost that dissuades adoption, such as the effort of learning the language, or expense of writing bindings to legacy code. The weighing factor for a language is then ``Benefit / Switching Cost``. A firm will decide to adopt if the value of the new language minus the old exceeds the switching cost by a certain threshold. Otherwise, the firm maintains the status quo. A new language will have to provide significant value to be adopted, but an adopted language can survive simple by keeping up with its competitors and keeping the switching cost high. Even such a simple model can become complicated because the costs and benefits are subjective, and may not be correctly perceived.
  3. Decision: an individual makes a decision to adopt. A short elevator pitch allows summarizing the pros and cons. The limiting factor here is simplicity, how easy the idea is to use and understand, as a complex decision may never be made.
  4. Implementation: an individual tries out an innovation and analyzes its use. This is where the reference documentation gets a workout. The limiting factor here is trialability, how easy the language is to experiment with.
  5. Confirmation: an individual finalizes the adoption decision, such as by fully deploying it and publicizing it. Encouraging such individuals to publish experience reports can start the adoption cycle over and cause the language to spread further. The limiting factor here is observability, the ability to get results.

* Power - A language needs a unified design, and generally this means designating a single person to make the final decisions. Essentially, a single person weighing priorities based on their knowledge of the market and pain points is more effective than group voting. In a committee, nobody feels responsible for the final result, so each person does a shallow analysis of surface costs and benefits. In contrast, an individual feels empowered and really has the incentive to understand the issues deeply and design an effective solution. Absent mitigating factors such as a strong committee chair or shared vision, group design generally result in terrible `"kitchen sink" <http://www.mozillazine.org/talkback.html?article=2919>`__ languages. These languages have an incoherent design, with many features that sort of work, but no particular attraction to any actual users. "Kitchen sink" languages are generally short-lived due the the difficulty of implementing an endless stream of special-case features and maintaining the resulting complex, sprawling codebase. Of course, so long as the power structure is clear, delegation of roles and duties is quite reasonable, e.g. designating a person for data analysis.
* Evidence - Everyone has opinions, but if there's a disagreement, opinions don't help much in making a decision. Although common, "The CEO said so" is not really a good reason to choose a particular design. I would rank evidence as follows:

  * Mathematical theory and logic stand on their own, I guess I could verify it with Coq or something but generally a proof is a proof.
  * Semi-automated analysis of source code repositories and developer communications, with manual inspection/validation of the results
  * A survey of users who've actually used a language for a while.
  * Experience reports from language designers are also high-quality evidence. There is some error in evolving and repurposing insights from one language to a new language.
  * Anecdotal reports I would say are medium-quality, as the plural of anecdote `is <http://blog.danwin.com/don-t-forget-the-plural-of-anecdote-is-data/>`__ data (the "not" version `appeared later <https://quoteinvestigator.com/2017/12/27/plural/>`__). It requires filtering out the opinions - what we want are claims, supported or unsupported, rather than simply "I don't like it".
  * Testing / prototyping can confirm hypotheses but may fail at identifying broad design considerations.
  * Arguing via toy code examples seems pretty decent, although can suffer from "cherry-picking" meaning that the design may not work in practice for code dissimilar to the examples.
  * Flix suggests evaluating features against a list of principles, but I tried it and generally the principles are too vague or unrelated to be useful. Also, the choice of principles is subject to bias. I would say the biggest principle for Stroscot is inclusivity, because figuring out how to include a feature means the feature must actually be seriously considered, whereas in other languages it is easy to fall into the trap of "don't know, don't care".

* Feedback - It is quite useful to get feedback from potential users and other, early and often. Feedback, unlike the designer, is not impacted by project history or the designer's preconceptions. The `Pontiac Aztek <https://en.wikipedia.org/wiki/Pontiac_Aztek>`__ checked all the boxes regarding functionality, and had the highest customer satisfaction ratings for those who drove it, but every time the focus groups looked at it, they said "it's the ugliest car in the world and we wouldn't take it as a gift". Per `Bob Lutz <https://www.caranddriver.com/features/a14989657/pontiac-aztek-the-story-of-a-vehicle-best-forgotten-feature/>`__, managers at GM ignored the focus groups, and the Aztek was a flop - barely anybody bought it, because it was indeed too ugly (although it did develop a cult following). However, just showing around a design and asking "what do you think?" has several problems. First, people's opinions change as they are exposed more - maybe their gut reaction is that they hate it, but if they spend an hour trying it out, they'd love it. The solution is measure, measure, measure - for example, an initial poll and a poll after a tutorial. Another useful trick is limiting the stimulus to what is under study - if syntax is not relevant, don't present any syntax, and then the discussion will naturally focus on semantics. If the "feel" of the language is being discussed, present a collage of concepts. Second, unstructured responses usually answer the wrong question - what matters is estimating how the design impacts certain business objectives and success criteria, but maybe the interviewee will spend half an hour discussing a tangent. This can be addressed by structuring and timeboxing the feedback with a rubric, and perhaps explaining some background with a video. Of course, qualitative feedback is most important, so the questions should still be open-ended. It is also best to speak to interviewees individually, rather than in a group, so that their opinions do not influence or dominate each other. Individual discussion is more likely to present a balanced opinion, whereas groups can pile on negative feedback. OTOH, a group does enunciate the overall consensus more clearly, and e.g. submitting to HN is a convenient way of getting group but not individual feedback, unles a survey link or similar is included.
* Testing - When qualitative considerations are silent, decisions must be made on quantitative grounds. The standard for websites is A/B testing: allocate some traffic to version A, and some to version B, and measure metrics such as average time to completion of task. A little more complex is a stochastic k-armed bandit test with Thompson sampling, which allows testing arbitrarily variants and also automatically reduces testing of poor-performing variants. We can do this for a language too, with some difficulty: get a random ID from the service, randomize choices, measure metrics in the compiler, report back, have a privacy policy and ensure GPDR complance, require the ID so as to generate customized documentation, and voila. Given that the audience is programmers it probably makes sense to allow overriding the arm selection.

Performance
~~~~~~~~~~~

The V language compiles millions of lines a second. And assembly can provide the fastest speeds. But only enthusiasts use these languages - V is too buggy to be usable, and assembly development is so tedious and specialized that few programmers are comfortable going down to that level. Wadler states "interoperability and footprint are more important than performance." :cite:`meyerovichSocioPLTPrinciplesProgramming2012` But is this true? In fact, performance does play a significant role in the bottom line of software companies.

Let's just look at the costs a big software company (Google). The `balance sheet <https://www.sec.gov/Archives/edgar/data/1652044/000165204423000016/goog-20221231.htm>`__ lists cost of revenues, R&D, sales and marketing, general and administrative, property and equipment, and a bunch of financing considerations like loans, bonds, and stocks that don't really matter for our purposes. Really, the only costs affected by a programming language are R&D and IT assets. Per `2016 10K <https://abc.xyz/investor/static/pdf/20161231_alphabet_10K.pdf>`__ 27,169 employees (37.7% of total) worked in R&D, for about $513,379 per year. Trying to update that, the 2022 10K lists 190,234 employees and $39.5 billion R&D, so estimate about 71,718 R&D employees and $550,766 per man-year. Regarding asset costs, the main figure is "other costs of revenue", $48.955 billion, which contains data center operation and equipment depreciation.

Similarly, Meta's numbers are $35.338 billion R&D, $25.249 billion cost of revenue. Total employees at the end of 2022 were 86,482. Their precise R&D employee count isn't reported, but `this HN post <https://news.ycombinator.com/item?id=33150096>`__ says about 42.6% "work in tech", so we can estimate 36,899 R&D employees and a spend of $1,070,490 per man-year. Per levels.fyi, the median salary is $261k and for Facebook the median salary is $350k, 1/1.96 and 1/3 of man-year spend respectively. The spend is a bit high compared to the 1.2-1.4 rule of thumb for total employee cost. Probably the mean salary is higher than the median due to a small number of highly-paid employees, and the R&D figure includes significant costs besides employee salaries, maybe CI testing and software licenses. But it seems reasonable to assume that it scales by employee.

Given stories like Facebook `rewriting Relay <https://relay.dev/blog/2021/12/08/introducing-the-new-relay-compiler/#compiler-performance>`__ from JavaScript to Rust and making it 5x faster, or `redesign their Hack JIT compiler <https://dl.acm.org/doi/pdf/10.1145/3192366.3192374>`__ for a 21% speedup (via `Lemire <https://lemire.me/blog/2023/04/27/hotspot-performance-engineering-fails/>`__ / `Casey <https://www.computerenhance.com/p/performance-excuses-debunked>`__), it seems at least theoretically possible that going all-in on a new language could make everything 2x faster and reduce hardware costs by half. For Google, the 2x speedup will reduce "other cost of revenue" by $24.77 billion per year. To break even, they would have to have spent less than that on the switchover, i.e. less than 48k man-years or about 67% of their 70k-person R&D department occupied for a year. For Facebook, the 2x speedup saves $12.62 billion per year and it would break even at 12k man-years or about 31% of their R&D department for a year. Although this is a large investment, acquiring WhatsApp was $19 billion, so it's affordable assuming the speedup is guaranteed.

Performance not only affects costs, but also revenue. As an example of this, let's look at Facebook's `LightSpeed project <https://engineering.fb.com/2020/03/02/data-infrastructure/messenger/>`__ to rewrite Messenger - per the post, they got a 2x startup increase. Per stats from `Google <https://www.thinkwithgoogle.com/marketing-strategies/app-and-mobile/mobile-page-speed-new-industry-benchmarks/>`__ (`2 <https://android-developers.googleblog.com/2021/11/improving-app-startup-facebook-app.html>`_), that speedup probably was from 3s to 1.5s and decreased bounces by around 25%. Estimating the revenue from this is tricky but as a basic estimate Facebook's IAP revenue from Messenger `in 2022 <https://www.statista.com/statistics/1230892/facebook-messenger-annual-app-revenue-worldwide/>`__ was $2.91 million, iOS is about `48% of mobile traffic <https://www.adweek.com/performance-marketing/the-state-of-android-vs-ios-usage-among-facebook-users/>`__, so they should have gotten at least a $350k increase in revenue, about 1/3 of a man-year. That's hard to jibe with Facebook's statement that the rewrite took more than 100 engineers over 2-3 years, but `FastCompany <https://www.fastcompany.com/90470219/project-lightspeed-how-facebook-shrunk-messenger-down-by-75>`__ mentions that most of that involvement was just 40 different partner teams "buy[ing] in and updat[ing] their work". If we assume that only the initial 3-4 engineers were really spending substantial time working on it, and not full-out but only 1/3 of the time, the speedup could pay for itself over 3 years or so. And there are indirect benefits of performance like happier users and improved reputation. Now, Facebook's post also mentions that the codebase size decreased from 1.7 million LOC to 360k. This substantially reduces maintenance costs, to the tune of ~$2 million / year per `this random cost per LoC figure <https://steersman.works/code-audit>`__). Facebook likely also went ahead with the rewrite because of the maintenance savings (the cultural motto of "Move fast and break things" has apparently evolved to "do complete rewrites pretty often while meeting 100% test coverage"), but here we're focusing on performance so it's reasonable to discount the maintenance benefits.

Now in practice, there are a variety of services. The desirable performance characteristics will vary - apps and websites will aim to reduce latency, backends will aim for efficient resource utilization, compilers will aim for faster code, and binary size is also a consideration in deployment. Rewriting existing C code probably won't get much speedup, while JS probably will. There is a lot of uncertainty, and different companies will deal with this in different ways. For many companies, they are risk-averse and a 2x speedup is not large enough to take a risk; per Cliff they will need at least a 10x speedup before they start considering it seriously. For larger companies like Google or Facebook, they will consider even small speedups, but they will incrementally rewrite services one by one with a few developers, rather than going all-in.

What's the conclusion? It's that yes, performance matters. If you can't write fast code in the language, the language won't be suitable for many purposes. And if another language is faster, some companies (like Facebook) have processes by which they can and will completely rewrite their implementation if there is a sufficient performance advantage. Maybe most less-agile or less tech-savvy organizations will not, but that's their loss. Performance appears to be central to long-term business interests, and directly affects financial success.

But this observation doesn't mean that all code in the language has to be fast. As the numpy ecosystem has shown, for basic scripting tasks, the hard parts can be implemented in compiled modules. Almost no special effort is needed to use these modules and get sufficient performance. Even though interpreted CPython is one of the slowest, least performant runtimes, the end result still performs acceptably because the hardware is very fast and the expensive operations are implemented in C. Now, there are drawbacks to this design - writing an interpreted, unvectorized loop is a performance no-no, for example. A real JIT compiler, like for example Julia's use of LLVM, is more flexible and performant in that it can optimize such loops. But scripting and scientific computing are definitely niches in the industry where only a few "hot" regions of code are relevant, and need to be optimized, and the other "cold" regions can be ignored for performance purposes. And even in the most performance-sensitive apps, there are cold paths that simply never happen often enough to affect performance, such as assertion failures.

IMO, the primary goal of the language should always be to allow the programmer to express their intent as naturally as possible. Let's say a programmer is writing a program from scratch, e.g. me implementing the Stroscot compiler. It's tempting to prematurely optimize - maybe spend some time designing the architecture, pick out some algorithms with good time complexity. The programmer feels confident - "These are the hotspots, so I'm going to write them in low-level assembly style." But per `C2 <http://wiki.c2.com/?ProfileBeforeOptimizing>`__, "this almost never works". Per :cite:`knuthStructuredProgrammingGo1974`, "the universal experience of programmers who have been using measurement tools has been that their intuitive guesses fail." It is just not that easy for humans (even experts) to predict what parts of the code will be slow. Compilers have gotten quite good at working optimization magic. Automatic optimizations can transform code beyond recognition and even improve asymptotic complexity. When starting out, it is better to have the mindset of "rapidly prototyping" - get the program worked out as quickly as possible, by writing clear, correct code with no attention to performance. Then once there is an implementation, the performance can be profiled to see if it is good enough. But the most important thing is still getting that initial performance data point. To support this rapid prototyping phase, the language needs powerful constructs and high-level declarative abstractions - "specify what, not how".

So that's for the initial implementation. Now in Facebook's case, they already had this "prototype", namely the old version of the software. Similarly once the programmer has an implementation and has measured the performance characteristics, now they're focused on performance. The path forward is completely a function of the profile: identify the performance-critical code, and go from there. Maybe there will be excellent performance right out of the gate, or maybe the generated code will be unusably slow. In the easy case, there will be a few clear bottlenecks and it is just a matter of getting them optimized. In the hard case, there may be a smear of hot code, or the bottlenecks are up against physical limits, and the architecture will have to be rethought. Again, the goal of Stroscot is to allow expressing intent. Writing a block diagram should be easy, because maybe the architecture has to be reworked. Writing low-level assembly-style code should be easy, because maybe the bottlenecks have to be reworked. Similarly Stroscot should make it easy to use a different data structure, add a cache, reorder traversals, and filter out irrelevant data. Since Stroscot is the ultimate language there should never be a case where the optimization can't be expressed naturally in Stroscot. In a lot of cases though, that will mean moving away from the high-level declarative constructs, and into a more C-like dialect. There's a limit to compiler magic, and when the performance of high-level code isn't sufficient, only some companies will have the technical capability necessary to improve the compiler, so the rest will require language constructs that express such improvements.

So overall, what does performance mean for language design? Mainly that the language should have both high-level and low-level constructs, and the compiler should have a lot of optimization smarts. But these are already expressed in the "don't surrender the adequate representation of a single datum of experience" and "tracking the progress of technology" principles. Hence, as a design principle, performance is simply not that important a consideration. As long as Stroscot implements the best optimizations, it will naturally meet or beat the performance of C compilers on C-like programs, and similarly for other language styles. For cases where there is no strong existing referent, like logic programming, the simple presence of the feature to begin with is the attraction, and performance is secondary as well. Google has done some work on build speeds; the main performance-focused feature there will be fine-grained incremental compilation to reduce compile times.

But secondary does not mean unimportant. It is certainly worth fostering a "performance culture", with performance tests, profiling tools, and so on, for both the compiler and libraries. Still, in the near term, the project is still in its "rapid prototyping" phase and hence the compiler's performance is not a consideration. The potential for performance is though, e.g. optimal register allocation is considered a "feature" rather than a "premature optimization" hence will be implemented without concern for its potential performance impact (runtime speedup or compiler slowdown).

Goals
=====

The ultimate
------------

Stroscot aims to be the ultimate programming language, rather than something just alright. The goal is to win the `ultimate showdown of ultimate destiny <https://www.youtube.com/watch?v=HDXYfulsRBA>`__ w.r.t. programming languages. This has been called "silly" by Dennis Ritchie (author of C) and "the dream of immature programmers" by Bjarne Stroustrup (author of C++), :cite:`sutterFamilyLanguages2000` but I think it can be made to work. A lot of language features have become standardized, which wasn't the case in 2000, and for the other "unique" features there has been enough research to establish a clear hierarchy of power. To bring in an analogy with weapons, the question of which firearm is strongest is quite subjective and a matter of debate, among other reasons due to loading and capacity questions. But the Tsar Bomba is without question the strongest weapon in history, and makes such debates irrelevant - all you need is a single giant bomb, and making more of them would be a waste of resources. And when the standard interface for deploying such a weapon is pushing a button, the choice of what the button should look like is essentially a bikeshedding debate - it's just a button and any choice of style and color will do (although of course red is traditional). In this analogy Stroscot would be an early nuke prototype - I'm not claiming it's the biggest baddest language, but at least it will point the way towards designing such languages in the future.

Stroustrup claims there are "genuine design choices and tradeoffs" to consider, which I agree with up to a point. Many queries in a compiler are too expensive to compute exactly and the method used to approximate the answer can be refined or optimized. There are competing approaches to answering these questions and methods of combining solvers to obtain more precise answers. The time/precision tradeoff here is real. But these are implementation tradeoffs, and don't affect the overall design of the language. While there may not be a best solver, there is a best set of syntax and features, at least until you get to details so minor that they are matters of personal taste.

Global maximum
--------------

Stroscot aims to be a global maximum of features and syntax, based on the following optimization criteria in order:

Functionality
~~~~~~~~~~~~~

Stroscot aims to be a `wide-spectrum language <https://en.wikipedia.org/wiki/Wide-spectrum_language>`__. That is, for every way to do X, Stroscot should also allow doing X in that way. The logic behind this is simple: If Stroscot can't do X, then people will choose to use another language that can do X. Practically, I have limited the domain of X to activities expressed in research publications and other programming languages, i.e., a systematic survey, so that the amount of functionality to consider is at least finite. I've mainly found novel ideas and techniques in obscure papers from decades ago, but there have also been a rare few published in the past few years. It is actually really hard to come up with better ideas than the old papers. And I'm not aware of any other programming languages that have tried to do a systematic search through the literature for features; academic languages are narrowly focused and practical languages do not innovate much. So Stroscot is at least somewhat innovative in its design by aiming for functionality in this way.

Many languages suffer from "idea envy", where they try to retrofit new ideas from other languages. For example C++ and Java have added lambdas relatively recently. When a programming language changes significantly in this way, it loses its identity - for example, Python 2 and Python 3 are effectively separate programming languages, as are Perl 5 and Raku (Perl 6). There are already projects that advertise themselves as "modern C++17" rather than simply "C++". A new language needs new tools and new libraries; in this case, a split between non-updated and updated C++ tools. Minimizing the number of new languages / breaking language changes is best. The source of these changes is clear: ideas that were missed out on in the initial design. The lambda calculus dates to the 1930s, and anonymous functions were included in Lisp in 1958, long before C++ was designed in the 1980s. The retrofitting in C++ is due to a shallow intellectual base. By instead preferring coverage of all functionality from the start, we ensure a future-proof design. Even if new ideas emerge after the initial design, they are generally small tweaks on old ideas. With sufficient research these old ideas can be uncovered and incorporated, making it a minimal change to accommodate the new ideas.

You may point to INTERCAL's COMEFROM as something best avoided, but it's not hard to implement. The trickier parts are actually at the low level, interfacing memory management and calling conventions, and the value proposition there for a powerful interface should be clear. Providing a broad set of features will mean that the language is suitable for whatever project someone is thinking about. Another theory is that, even if Stroscot fails as a language, implementing lots of features will make people copy Stroscot's list of features.

Against Stroscot's goal of increasing functionality, there is a general sentiment in the industry that, as Jamie Willis put it, "if you increase power, you increase problems". This suggests that it might be better to avoid powerful features. Willis clarifies that, by including more specialized and restricted abstractions, the language is easier to use. I guess I agree with this second statement, broadly; structured programming with loops is easier for beginners to use than the goto statement. But, I do not think that adding structured programming constructs makes goto unnecessary. Indeed, C still has goto, and Linux kernel programmers use it regularly. Java further specialized constructs with a "break label" construct that functions exactly as goto. Except, the "break label" can only jump to the top and bottom of loops. This regularly causes complaints in various circles, such as decompilers which need a quite complex algorithm to translate the JVM "goto" instructions back into loops and break statements. In fact this algorithm fails often and the disassembled code actually contains invalid goto statements. It is much better to simply include the goto statement, the loops, and the "break label" concept too.

More generally, I think including powerful constructs makes the language more expressive, and thus more powerful. The programmer has less friction searching for the right construct, less difficulty expressing their intent, and less problems overall. For example, it's hard to argue that SQL is too powerful - quite the opposite, most people criticize it for its lack of expressiveness and poor portability. The declarative aspect does introduce certain unique tasks, such as query optimization, but performance would be a problem regardless so this is not introducing a new problem. And in fact it is easier to optimize a query using the appropriate tools than it is to rewrite the corresponding imperative program.

Finally, as `Edsger Dijkstra <http://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD896.html>`__ put it, "complexity sells better". If you spend all this time hyping up a language, and then it turns out it's so simple it fits on a postcard, your audience will feel cheated and dismiss the result as trivial. Popular languages are complicated and have lots of features. As measured by :cite:`yaofeichenEmpiricalStudyProgramming2005`, "simplicity" and "implementability" are both correlated with a lack of adoption as a developer's primary language, while "extensibility" and "generality" are preferred.

Minimal core
~~~~~~~~~~~~

As discussed just previously, minimalism is bad. If you build on an existing language but include no new features, then there’s no incentive to use your language. If your language only provides a minimal Turing-complete set of operations like Brainfuck, figuring out how to express programs in it will be difficult, and the resulting encoding most likely will be incomprehensible. Fortunately though, we have already listed functionality as our main goal. So the question is, given that we must provide all possible features, how to implement them in a methodical, useful manner.

Certainly, there is the possibility of just implementing them all independently, but I like GHC's structure of having a smallish "core" language (System FC), and translating the rest of the language down to it. In fact there is not much to Haskell besides System FC; the language proper is quite small, and most of the idioms of Haskell are implemented in libraries. Similarly, for Stroscot I would like to define a "core" language that provides only the basic, necessary abstractions and tools for defining more abstractions, such as macros and syntactic extensions. Then the compiler only has to focus on handling these core constructs well, but the standard library can implement all the parts that users interact with. With suitable abstraction facilities, this approach doesn't lose any expressiveness because we can still implement any language construct we can think of. We have not "surrender[ed] the adequate representation of a single datum of experience", but merely reduced the reducible elements. The surface language is still complex, modern, and slick. Developers can focus on learning the core language's general constructs, and then learn libraries by reading their source code, or they can follow more of a "learn by doing" approach where they learn the libraries they like from the documentation and examples, without understanding the implementation.

So what defines the "core" language? Well, per Einstein, each element should be basic, simple, and irreducible, and there should be as few elements as possible. More formally, we can consider the "core" as an orthonormal basis in an inner product space, with vectors as programming elements. Then our "core" must satisfy the following conditions:

* spanning: every element can be written (macro-expressed) as some combination of the core elements
* linear independence: this representation in terms of the core elements is unique (up to some notion of equivalence). In particular, no core element should be macro-expressible in terms of the other core elements.
* orthogonality: The dot product of any two core elements should be 0. Said another way, for all scalars :math:`r,s` and core elements :math:`x,y`, :math:`\|r x\|\leq \|r x+sy\|`. In words, the combination of two core elements is at least as powerful/expressive as either element individually.
* units: The norm of each core element should be 1. I interpret this as that each core element should be Turing-complete but not highly undecidable, and correspond to one syntactic construct. In terms of macro expressibility, there shouldn't be overly-specific elements or overly-general elements. Overly-specific elements cause clutter, while overly general elements are too hard to understand. Honestly this requirement is a ball of mud and just requiring an orthogonal basis or a basis at all seems sufficient.

For example, COMEFROM can be implemented with continuations and macros (c.f. `this Python decorator <https://github.com/snoack/python-goto/tree/master>`__). We can thus move COMEFROM to the standard library, and define a "core" subset of the language that contains only continuations and macros. By repeating this sort of exclusionary process, we can construct a minimal "basis" of core features, in the sense that none are redundant. Fewer concepts simplifies the whole language, and approximates Python's goal of "There should be one-- and preferably only one --obvious way to do it."

Also, a core improves stability. Cameron has pointed out that the "core" is not set in stone and may need changes, particularly early in development. We may find out that an element is simply not needed at all, or is too complex in its current form and can be further simplified. We may find new elements that were missed in the initial design. For example, comparing GHC's Core Expr datatype from `1998 <https://gitlab.haskell.org/ghc/ghc/-/blob/438596897ebbe25a07e1c82085cfbc5bdb00f09e/ghc/compiler/coreSyn/CoreSyn.lhs#L51>`__ to `the present day <https://gitlab.haskell.org/ghc/ghc/-/blob/0f3fda8179883048a2299c9c448bcfbc94fbb7ca/compiler/GHC/Core.hs#L249>`__, we find many changes: addition of a type field to cases, removal of constructor applications (in favor of an expanded Var type), addition of special-cased primitive literals, expansion of Note into Cast and Tick alternatives, removal of an "f" type parameter, addition of coercions. But in 20 years, 6 of the 8 constructors were essentially unchanged, and the remaining changes fall under the category of minor additions or "polishing". For the most part, by virtue of its design constraints, the core is remarkably stable and can safely be used as an interface between the compiler and the rest of the language (the standard library).

Learnability
~~~~~~~~~~~~

It's often not that easy to learn a language. Google searches will often yield irrelevant results. Official documentation can be useful, but is often filled with terse wording, links to lengthy discussions containing irrelevant detail, and TODOs. The truth can be found in the compiler source code, but this often has one-letter variable names, very few comments, and an assumption that you know the coding style and design of the compiler.

Learnability means making things easier for generations of beginners by making the language "intuitive" so that language choices can be guessed rather than looked up. There is some amount of English discrimination involved, as the learnability studies' "beginners" are limited to English speakers in Western colleges, but English is the most popular language, and there is the functionality to translate Stroscot to other languages.

Learnability does not necessarily mean making the language similar to existing languages. Such a language might be easier for experts to learn in the short run, but in the long run (assuming Stroscot is successful) there will be many more novices than experts that need to learn the language, so the novices should be prioritized.

Concision
~~~~~~~~~

If there is a verbose syntax and a terse syntax (as measured by characters or screen space usage), both equally learnable, then the terse syntax is better, because the program can be more cheaply printed out and literate documentation is mainly made up of the prose/code comments rather than code.

APL is sometimes criticized for being too concise, but the actual (learnability) issue with APL is that, like Chinese, it has a lot of symbols and hence novices and experts alike suffer from `character amnesia <https://en.wikipedia.org/wiki/Character_amnesia>`__. J uses ASCII symbols hence mitigates the issue and is `praised for its terseness <https://procyonic.org/blog/a-critique-of-the-programming-language-j/>`__. But it still is difficult for novices to learn (basically you have to memorize `this page <https://code.jsoftware.com/wiki/NuVoc>`__) so an syntax based on English words may be better.

World domination
----------------

Stroscot aims to replace all the programming languages in use today. Mainly this involves improving FFI support and interoperability with C and C++. In particular we need to be able to parse headers and use data from them with Stroscot. Since headers include code we need to be able to fully compile C/C++, so that Stroscot is the sole compiler and all of its global optimizations can be used (`zig cc <https://andrewkelley.me/post/zig-cc-powerful-drop-in-replacement-gcc-clang.html>`__ is an example of how this works). The linkage is asymmetric - you can export specific C-style constructs back to C, but C can't use functions that depend on more advanced features.

Once the C/C++ implementation is stable enough for production use, focus will shift to developing automated conversion tools for other languages like Python and Java, so that the surface syntax can be changed to Stroscot's. And yes, this is the `E-E-E strategy <https://en.wikipedia.org/wiki/Embrace,_extend,_and_extinguish>`__, but Stroscot is open source so it's all OK.

Standardization doesn't seem necessary. A popular language builds its own standard. Python, the world's most popular language as of `July 2022 <https://www.tiobe.com/tiobe-index/>`__, has `never been <https://stackoverflow.com/questions/1535702/python-not-a-standardized-language>`__ formally standardized. But there needs to be an open-source cross-platform implementation, with a committee process for changes to build consensus and ensure stability. Another alternative is to freeze Stroscot after release and design a new language every 3 years, but that requires creating new names and websites so it's easier to evolve gradually.

Criticisms
==========

These criticisms from the `programming language checklist <https://www.mcmillen.dev/language_checklist.html>`__ seem valid.

* Stroscot lacks reflection.
* Stroscot relies on an optimization which has never been shown possible
* Stroscot requires the compiler to be present at runtime
* Stroscot requires the language runtime to be present at compile-time
* Dangerous behavior is only a warning

Other programming languages
===========================

There are many existing programming languages to learn from. All of them have had effort put into their design so their features should be considered. Unfortunately there is not enough time to learn every language in depth and use it for 10 years to get an idea of its strengths and weaknesses, so we must rely on reports of other users on the web. Also even searching on the web for detailed reports of complexities encountered in large software projects is quite time-consuming, so we mainly examine popular languages.

Since we aim to be a popular language, we list the languages in order of popularity, so that more attention is focused on the earlier languages. There are several indexes with different measures of popularity (as of January 2023):

* `TIOBE <https://www.tiobe.com/tiobe-index/>`__ measures the quantity of search engine hits for "X programming"
* `PYPL <http://pypl.github.io/PYPL.html>`__ measures how often language tutorials are Googled.
* `Github Collection <https://github.com/collections/programming-languages>`__ measures stars and forks of implementation
* `GitHut 2.0 <https://madnight.github.io/githut/>` measures GitHub PRs, stars, forks, issues of language-detected repos
* `Languish <https://tjpalmer.github.io/languish/>`__ measures Github and Stackoverflow metrics
* `Github Octoverse <https://octoverse.github.com/2022/top-programming-languages>`__ measures amount of Github code written
* `RedMonk <https://redmonk.com/sogrady/2022/10/20/language-rankings-6-22/>`__ measures Github repositories and StackOverflow questions
* StackOverflow survey measures
* `IEEE Spectrum <https://spectrum.ieee.org/top-programming-languages-2022>`__ ranks 57 languages by 9 different measurements
* Google Trends

Out of these the PYPL index is probably most suited for designing a new language, because the tutorial measurement approximates the new PLs with features that programmers want. But it only has 28 languages - the TIOBE index is more complete with 50 languages ranked and 50 more languages listed. We also want to check that the top 20 PLs from the other rankings are also included.

PyPL index (top 28)
-------------------

1. Python

* Most popular on TIOBE index, said to be "easy to learn for beginners", "easy to write", "simple and elegant syntax" "similar to English".
* brevity, readability, developer-friendliness make it 5-10x more productive than Java
* "Batteries included" standard libraries, such as lists and dictionaries, numpy (BLAS wrapper) and scipy
* Twisted web framework, TensorFlow machine learning framework
* Mixed reference counting / tracing GC memory management
* Significant indentation - still a point of contention, e.g. whether it makes copy pasting code harder
* C++ interpreter CPython, slow performance. PyPy exists but has't been widely adopted due to incompatibilities.
* unintuitive "double underscore"/"dunder" method names like ``__getitem__`` and ``__str__``, C++'s ``operator[]`` and Java's ``toString`` are clearer
* paradigm: impure functional

2. Java

* Baroque type system, many types of class-like thing (interfaces, enumerations, anonymous adapters), with generics on top. Many compromises/holes such as covariant arrays
* Verbose. But there is a book on design patterns, which can be used to identify areas needing explicit syntax. The class-based syntax for the patterns is not worth emulating.
* try-finally and checked exceptions have wasted the time of many programmers.
* Keyword soup for declarations, such as "public static void main".
* Lack of operator overloading such as ``+`` for ``BigInteger``
* Every object has a 4-byte header and identity using ``==``. No value types besides primitives.
* Requirement that the class name must match the directory name.  When moving functionality around this implies a lot of changes inside source files. Led to IDEs with extensive support for refactoring.
* Static methods. Scoped to a class, but not related to objects. Can be very confusing.
* fulfils "compile once, run anywhere" promise at the expense of a huge runtime
* JIT is probably best in the world for throughput. Startup is slow but throughput matches C performance in many cases.
* Garbage collector takes big chunks of CPU time at irregular intervals. Low-pause GCs trade this for continuous overhead. Still not solved, around 15% overhead on wall clock time. :cite:`caiDistillingRealCost2022`
* paradigm: OO

3. JavaScript

* second-best JIT after Java, optimized for startup time - fast bytecode interpreters
* many strange features such as implicit type conversion, ``with`` statement, and ``eval``
* paradigm: impure functional

4. C#

* best designed C-style syntax - e.g. introduced async/await
* wide usage - desktop software (Windows), games (MonoGame, Unity), web development (ASP.NET Core), mobile (Xamarin)
* paradigm: OO

5. C

* most portable/widespread language. runs on just about every piece of silicon (although some require specialized compilers)
* language of most OS's, hence used for FFI stuff
* statically compiled, compilers are very efficient.
* difficult to work with -  unsafe pointers, common to see memory corruption and security vulnerabilities. valgrind, smart fuzzing, and static analysis have allowed catching these with great difficulty. Also there is the Boehm GC, used by many people who don't want to deal with memory management.
* header files slow down compilation as they have to be read many times during compilation
* paradigm: imperative

5. C++

* many features, which interact in messy/complex ways making C++ take a long time to learn
* fast, efficient standard libraries similar to hand-tuned code (but missing many features, see also Boost)
* templates, efficient at runtime but slow at compile time
* memory unsafe like C, although smart pointers and RAII make this a little better.
* Hard to debug, there is GDB, valgrind but really rr is the only way to track some errors down
* paradigm: imperative

6. PHP

* Initial design was hacked together quickly, inconsistent API design. Could be fixed but backwards compatibility was held to be more important.
* Like JS, several features with huge security or performance impact: eval, weak typing
* paradigm: imperative

7. R

* numerous libraries for statistics and data analysis
* lazy evaluation
* paradigm: functional

8. TypeScript

* `near superset <https://stackoverflow.com/questions/29918324/is-typescript-really-a-superset-of-javascript>`__ of JavaScript with an unsound type system
* doesn't really add anything besides the types, so only useful for ideas on gradual typing. Also the type inference is not too good.
* paradigm: OO

9. Swift

* Automatic reference counting, interesting but not something I want to copy
* syntax for exception handling, if let/guard let
* `exponentially slow <https://www.cocoawithlove.com/blog/2016/07/12/type-checker-issues.html>`__ type inference for numeric expressions, with bad heuristics
* paradigm: OO

10. Objective C

* deprecated by Apple in favor of Swift, but good to compare against C++
* paradigm: OO

11. Go

* opinionated design, touts meaningless features such as "strong typing"
* goroutines, killer feature - but stackless continuations are better
* finally added generics after a long time
* supposedly a Python replacement, but TensorFlow is mainly in Python and the Go binding `isn't officially supported <https://github.com/tensorflow/build/tree/master/golang_install_guide>`__
* paradigm: actor model

12. Rust

* good standard library design and documentation, probably worth copying
* voted "most loved" by StackOverflow
* ownership model/borrow checker has been found difficult to use by several studies (`1 <https://arxiv.org/pdf/1901.01001.pdf>`__, `2 <https://arxiv.org/pdf/2011.06171.pdf>`__, `https://dl.acm.org/doi/pdf/10.1145/3510003.3510107`__). Also it is incomplete - can't even write linked lists without `endless pain <https://rcoh.me/posts/rust-linked-list-basically-impossible/>`__. In practice Rust programmers `end up <https://rust-unofficial.github.io/too-many-lists/third-layout.html>`__  using reference counting or GC to ensure memory safety in complex cases
* concurrency safe, but async suffers from "borrow checker"-itis and uses atomic reference counting
* has claimed to be "blazing fast" since `2014 <https://github.com/rust-lang/prev.rust-lang.org/commit/863e4176f92483853338f6237dafdf1a127a91ce>`__. But we see in `the one test of this claim on Google <https://youtu.be/ou8kQ4rIGqQ?t=1948>`__ that C is faster.
* learning experience circa 2015 was "rough"
* compatibility pendulum has swung towards "too stable" - many changes that "should" be made for a better language that can't be
* paradigm: imperative

13. Kotlin

* JVM language with features tastefully copied from Groovy and Scala
* val keyword instead of final, null safety, extension methods, first-class type parameters
* coroutines
* mainly getting traction due to Google pushing it for Android
* paradigm: OO

14. MATLAB

* extensive numerical libraries
* array syntax confuses people, ``[1 [2 3]]`` is a flat array because ``[A B]`` means concatenate A&B. there is `no literal syntax <https://www.mathworks.com/help/matlab/math/multidimensional-arrays.html>`__ for 3D or higher dimension arrays.
* paradigm: imperative

15. Ruby

* weird syntax, e.g. expression by itself is return value - causes mistakes. Per Matsumoto `interview <https://www.artima.com/articles/the-philosophy-of-ruby>`__, Ruby was designed for *his* least surprise, and maybe for least surprise after memorizing the language, not for novice programmers or programmers familiar with other languages, so has many idiosyncrasies.
* complex library, e.g. both find_all and select methods that do the exact same thing
* Rails is `(still) <https://www.jetbrains.com/lp/devecosystem-2021/ruby/#Ruby_what-web-development-tools-and-or-frameworks-do-you-regularly-use-if-any>`__ the most popular framework. Requires reading the Rails guide to learn things like models having singular class names with capitals and no underscores but db tables with plurals, lower case and underscores. Or how in controllers you just reference params without anything suggesting if params is a variable, method, how its populated, where its scoped, etc. As compared to Django where novices can figure out the basics easily without needing a guide.
* slow, `YJIT <https://github.com/ruby/ruby/blob/master/doc/yjit/yjit.md>`__ added in 3.1
* paradigm: OO

16. (also 21) VBA / Visual Basic

* "mentally mutilates" programmers (according to Dijkstra)
* runs on .NET, so very similar to C# in semantics. There is also "Classic Visual Basic" but the differences are small.
* paradigm: imperative

17. Ada

* Still in use in aviation and DoD applications
* Considered somewhat legacy, but has many useful features
* SPARK language is a dialect which extends contract support
* paradigm: imperative

18. Dart

* targets JS, WASM (in progress), and native ARM/x86 with AOT and JIT, a pretty reasonable set of targets
* tied to Flutter UI framework, which is mostly for creating mobile apps but also supports desktop and web
* main advantage is sharing code between client and server
* sentiment seems to be that Kotlin is about the same language-wise and the JVM is better for enterprise work
* they have a package manager, but it doesn't support automatic vendoring so there are many version solving conflicts
* concurrency model is an async-await event loop similar to node plus actor-style "isolates"
* Google has been funding it, FUD about whether Google will kill it. It is an Ecma `standard <https://dart.dev/guides/language/spec>`__ though, probably will stick around for a decade regardless.
* paradigm: OO

19. Scala

* Type inference, allows avoiding repetition of Java such as ``SomeModule.MyClass v = new SomeModule.MyClass();``
* complex type system: implicit conversions, subtyping.
* complex syntax: scares off newbies, steep learning curve, not recommended. Scala 3 has 3 ways to end blocks (end, braces, indentation) and everyone is confused as to which one they should use.
* paradigm: impure functional

20. Lua

* Use of "tables" for everything is interesting
* LuaJIT was fast but the main developer left due to lack of income. Stroscot needs to avoid the same fate.
* paradigm: impure functional


22. ABAP (Advanced Business Application Programming)

* proprietary PL developed by SAP in 1983, only available as part of NetWeaver ERP suite.
* "German COBOL", popular in Eastern Europe / Germany.
* odd niche language, but with even bigger footprint than COBOL, gradually being phased out with Java/JS/etc.
* weird combination of BASIC and SQL. Built-in SQL syntax.
* all code is stored in databases, thousands of tables
* OOP extensions that make everything terrible to maintain
* good debugger
* paradigm: procedural/imperative

23. Julia

* good support for concurrency/parallelism
* C+Fortran+Python FFIs and syntax
* JIT design goes through LLVM and requires trampolines between functions, performance barrier
* paradigm: multiple dispatch

24. Groovy

* most "batteries included" JVM language... even has YAML support in the standard library.
* "kitchen sink" approach to language design, e.g. can declare variable bare, with var, or typed
* scripting language feel, more dynamic than Kotlin, also older
* good at DSLs, e.g. SQL DSL, used as Grails/Gradle configuration language
* starting to decline in popularity, even though it's still active
* paradigm: OO

25. Haskell

* "finest imperative programming language"
* small community, few core/maintenance developers (mainly SPJ) compared to size of codebase
* good in benchmarks and scripting but GC is still not usable in production
* poor library design, e.g. verbose naming conventions
* paradigm: pure functional

26. Perl

* A mess with the Raku split
* Various libraries on CPAN are good
* Contexts and sigils, terrible syntax for beginners
* paradigm: impure functional

27. COBOL

* staying far away from this
* paradigm: imperative

28. Delphi / Object Pascal

* still kicking, but proprietary
* paradigm: OO

TIOBE Next 22
-------------

8. SQL

* DSL used for databases, common enough that finding a good way to embed it is necessary
* query compilers do a lot of interesting data structure traversal optimizations
* PL/SQL is Oracle's extension which allows procedures, functions, and triggers; other databases have similar things
* paradigm: declarative

9. Assembly language

* Generally the last stage before a binary in any compilation pipeline
* full access to ISA and thus machine resources
* Intel and AT&T syntaxes for x86. `Several <https://blog.reverberate.org/2009/07/giving-up-on-at-style-assembler-syntax.html>`__ `posts <https://outerproduct.net/2021-02-13_att-asm.html>`__ say to use Intel.
* repetitive and tedious, too many side effects, not at all intuitive, full of many inconsistencies
* paradigm: imperative

18. Scratch

* Block-black visual programming language for children
* Essentially procedural, it has conditionals, loops, and functions
* paradigm: block-based

21. SAS

* Another statistics language, less popular than R and proprietary
* paradigm: data-driven

22. (Visual) FoxPro

* commercial language, don't know much about it
* paradigm: imperative

24. Fortran

* call-by-reference calling convention, avoids copying arrays but hard to program with
* still used for some numerical code
* handles floating point exceptions via signals
* paradigm: imperative

25. Lisp

* Easily parsable syntax, originator of macros
* Error messages involving macros are probably more confusing than the macros themselves
* Racket is probably the most popular Lisp now. Uses Chez Scheme's work on the nanopass framework.
* paradigm: functional

33. Prolog

* old language, dead/resting
* Few industrial-sized (>100k LOC) applications. Tends to be used as a DSL, e.g. IBM Watson uses it for pattern matching over natural language parse trees, but the rest of the application is written in C++ and Java.
* Blamed for contributing to the failure of Japan's `Fifth Generation Computer Systems <https://en.wikipedia.org/wiki/Fifth_Generation_Computer_Systems>`__ (FGCS) effort.
* great for querying relational databases, actually sort of a superset of SQL. You just load the relations and go.
* not well standardized - many implementations with different module systems/standard libraries
* What is standardized/hardcoded is the DFS search order, which for naive programs is often slow. Simply reordering the goals or clauses can give significant speedups. miniKanren uses a "fair" search strategy that avoids non-termination due to clause order, and it should be possible for a compiler to optimize the search even better with heuristics and profiling. Datalog restricts some features and has more efficient strategies.
* Prolog does not really have a way to examine or manipulate mid-level IR, e.g. something comparable to SQL EXPLAIN.
* Implementing speedups generally requires non-declarative workarounds that break the logical semantics. And e.g. cut is really hard to reason about. Mercury has "modes", not well-explored.
* Constraints allow avoiding cut in some cases, very powerful but not well explained in beginner courses
* paradigm: logic. generally agreed to be the highest-level paradigm.

34. D

* C/C++ style but different.
* Never really took off AFAICT - rift caused by the differences between D version 1 and 2, a general lack of learning resources and packages
* garbage collected
* many features that have been incorporated into C++, others that haven't like scope guards
* paradigm: imperative

35. PL/SQL is a dialect of of SQL

36. Bash shell

* Common on Unix systems, but I think Python is more useful when you're doing anything complex
* paradigm: imperative

37. Powershell

* Equivalent of Bash on Windows - I don't think there's much inspiring
* paradigm: imperative

39. Logo

* There is no official Logo implementation, but UCBLogo is popular.
* Simple procedural commands, functions, and Lisp-like linked lists
* Turtle graphics
* Personally I like the game `RoboWar <https://en.wikipedia.org/wiki/RoboWar>`__ better, which is based on Forth
* paradigm: block-based

40. Transact-SQL is a dialect of SQL

43. F#

* Mixture of C# and Haskell, not really as interesting as either
* paradigm: impure functional

44. OCaml

* paradigm: impure functional

45. CFML (ColdFusion Markup Language)

* paradigm: imperative

46. Scheme - discussed under Lisp

47. Erlang

* has a well-tested distributed, fault-tolerant, reliable, soft real-time, concurrent database
* designed to be crash-only, restart tolerant
* not used much outside Ericsson
* Per roastedby.ai, written by drunk masochistic Swedes. Just watch `Erlang: the movie <https://www.youtube.com/watch?v=xrIjfIjssLE>`__ and tell me they aren't drunk.
* paradigm: actor model

48. LabVIEW

* Pretty nice GUI approach to programming
* Lots of wires, and if you get a complicated enough program the auto wire layout command gives up
* paradigm: dataflow

50. ML

* paradigm: impure functional

Next 50 in alphabetical order
-----------------------------

ABC

ActionScript

* basically JS but with a different standard library

Alice

Apex

APL

AutoLISP

Awk

B4X

C shell

CL (OS/400)

CLIPS

Clojure

* one of few languages to use software transactional memory, custom implementation "MVCC"
* `interesting talks <https://github.com/matthiasn/talk-transcripts/tree/master/Hickey_Rich>`__ on functional programming and language design
* runs well on JVM
* slow
* never seen it used for anything performance-critical or that substantially affects a business

Common Lisp - discussed under Lisp

Crystal

* Ruby-like syntax - attracts Ruby programmers, but too complex for anyone else
* statically compiled with LLVM - the usual story of fast binaries but slow compile times
* HM-style type inference, the usual caveats

Elixir

* based on Erlang, new and supposedly great syntax

Emacs Lisp - discussed under Lisp

Forth

* Minimal design, almost as portable as C
* stack-based paradigm leads to annoying shuffling (dup, swap, etc.) to get anything done
* extended by Factor, Joy, and other "concatenative languages"

Hack

Icon

Io

J#

JScript

Korn shell

Ladder Logic

LPC

Modula-2

MQL5

MUMPS

NATURAL

Occam

OpenCL

OpenEdge ABL

PL/I

* Ye olde IBM language
* Weird exception handling style - "ON <EXCEPTION> GOTO <LABEL>", very similar to hardware implementation

Q

Racket - discussed under Lisp

Raku

* Perl 6, yay

Ring

RPG

S

Smalltalk

Solidity

SPARK - discussed under Ada

Stata

Tcl

VBScript

Verilog

* Two languages: a general purpose specification language, and a poorly-defined hardware synthesis language. Basically you have to run the synthesizer and see if it complains or generates a circuit.

VHDL

* also a hardware synthesis language

WebAssembly

X++

Xojo

Languages not in TIOBE
----------------------

Elm

* small ecosystem
* derivative of OCaml
* no substantial commits in main repo since 2019
* BDFL doing "exploratory work" closed-repo, most recently described in a 2021 `status update <https://discourse.elm-lang.org/t/status-update-3-nov-2021/7870>`__

Flix

* Long list of principles, most of which I disagree with. The corresponding Stroscot opinions are expressed on their respective pages.

Oz

* multi-paradigm, I don't like the way they integrated lgoci programming though

-  `Lever <https://github.com/cheery/lever/>`__
-  `Jai <https://github.com/BSVino/JaiPrimer/blob/4a2d14f3e1c8e82a4ba68b81d3fd7d8d438e955c/JaiPrimer.md>`__
-  `Pinafore <https://pinafore.info/>`__
-  `Macro Lambda Calculus <http://github.com/codedot/lambda>`__
-  `Wat <https://github.com/manuel/wat-js>`__
-  `Atomo <https://github.com/vito/atomo>`__ / `Atomy <https://github.com/vito/atomy>`__

