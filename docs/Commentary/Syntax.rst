Syntax
######

Design
======

The stuff here is mostly a dumping ground of ideas while the rest of the language is designed. The actual syntax will be designed by going through the syntax of other languages (primarily the ones listed in the influences section, but also all the ones listed on RosettaCode and `Rigaux's list of syntax across languages <http://rigaux.org/language-study/syntax-across-languages/>`__) and picking out the nicest examples. But in the end, syntax is decided by usage, so a lot of the syntax here will probably become final.

Quorum and its associated set of syntax studies provide useful datapoints on keywords and constructs. But Stroscot has a unique design so we can't use a lot of the research, and the research is limited to begin with.

Some studies use a "Randomo" language which randomizes design choices. It would be useful to implement syntax randomization so choices could be A/B tested.

Some languages offer a "simple" syntax. But simplicity is hard to define, and boils down to either a simple implementation (LR) or else just the syntax familiar to them from other languages (which implementation-wise is often quite complex). People seem to be afraid of new syntax so there is the tendency to make it explicit and loud while reserving the terse syntax for established features. But Stroscot's goal is to unify all the features, so all of the notation is designed to be short, terse, flexible, and general.

Haskell/Idris syntax is mostly awesome, use it. (TODO: check this. The weird function call syntax may lose too many users)

TODO: see if there are any more Unicode guidelines relevant to writing a programming language parser

Natural language like Inform 7, while interesting, is quite wordy. It's also hard to scan through.

Fortress has "mathematical syntax", with an ASCII form and typeset form. They used LaTeX but HTML / MathML output should be possible too. And juxtaposition was overloaded. Probably worth emulating.

A language encourages certain expressions of thought. If the syntax is awkward then the feature will be used less and a bias will be introduced. But the styles of programming people come up with after a language is released are often completely different to what was intended by the language (e.g. Java and its design patterns). It's not clear that anything can be done about this, besides capturing as many existing patterns as cleanly as possible.

Textual
-------

There are some people who, when confronted with the complexity of syntax, think "It's better to use a binary format and store everything in a database." Now they have two problems. Math is textual, English is textual, the only programming stuff that isn't textual are flowcharts and tables. Flowcharts might be OK (e.g. Labview) but graph layout is hard - graphviz barely works, and most graph layout algorithms such as IDA Pro's are quite lacking. Labview struggles even to layout wires (edges). Tables lead into spreadsheet programming which is generally not expressive as a language - and the formulas and cell values are textual. If you show me a way to write 123.6 that doesn't involve text maybe visual programming is worth considering.

There's also structural editing, `lamdu <http://www.lamdu.org/>`__ and so on, but they are designing an IDE alongside a programming language. I'm not too interested in IDEs and given that half the IDEs are for languages that also have a textual syntax, syntax doesn't seem to be a big factor in writing such an IDE.

Legibility research
-------------------

The main factor improving readability is consistency; reading is disrupted when unconventional layouts are used.

Spacing helps identify boundaries:
* For words, intra-letter spacing should be significantly smaller than inter-word spacing.
* For sentences, there should be extra space after the period, although the period's whitespace itself is distinctive.
* Justified text is harder to read than ragged-right due to the inconsistent spacing arising from bad line-breaking.
* The default inter-line spacing (line height, leading) is fine for most people. Some people with disabilities need more. Longer lines can use a little bit more line height.
* To identify paragraphs, inter-paragraph spacing should be visibly larger than inter-line spacing, or paragraph indentation should be used.

Left-aligned text is easier to read than centered or right-aligned text because the reader knows where to look to find the next line.

Maximum line length is an open question. Diff programs seem like the limiting factor, but on my monitor I can fit 2 108-character texts at the default font size side-by-side along with a space in the middle and the taskbar. Rounding this down to 100 leaves room for line numbers and similar decorations. Plus, most diffs these days are unified, and line-wrapping is always an option for smaller screens. OTOH it's a tiny font, 18-26pt is the most readable for websites so maybe that size is needed for programming. At 18pt / 24px I can fit 97 characters, while a little less (17.25pt / 23px) fits 102 characters. The standard is 80 characters but monitors are wider now, so again 100 seems plausible. This can really only be tested by finding long lines of code and asking what line-breaking placement is most readable.

Code legibility
---------------

IMO layout improves code legibility. There haven't been any formal studies that I can find, but Python syntax is often said to be "clean". Also layout makes arguments over where to put braces moot. Hence Stroscot has layout.

Reading code top-to-bottom, left-to-right makes sense. So definitions should be on the left, blocks indented in, and lines themselves should read left to right. So Ruby's statement modifiers ``X if Y`` are a bad idea because the ``if Y`` is easy to miss when scanning control flow.  But operators like ``a = b xor c`` are fine because the assignment ``a =`` is clear and if the value of the expression matters you're reading the whole line anyway and can parse it in your head.

Unicode can improve legibility when the character is standard (e.g. θ for angles), but generally long names like ``Optimiser(learning_rate=...)`` are more readable than ``Optimiser(η=...)``. Programmers have neither the time nor the inclination to learn a new character set and accompanying set of conventions.

When the convention is established, short names are clearer than long names. Writing ``(+) { augend = 1, addend = 2 }`` is less clear than the simple ``1+2`` - the long names are not commonly used. But it is arguably still useful to include the long names, e.g. for currying.

A study :cite:`dossantosImpactsCodingPractices2018` found the following conventions were helpful for readability:

* Putting opening braces in a line of their own (C# convention), as opposed to the same line of the statement, improved readability. The extra white space and matching vertical alignment of related curly braces makes blocks clearer. Closing curly braces terminating code blocks should be on their own line, except for secondary paths of execution, e.g.: closing brace of if statements followed by an else; closing braces of try statements followed by a catch.
* 80 character line lengths were helpful, although they did not test other lengths such as 100 or 120
* Each statement should be in a line of its own; do not separate multiple statements by a ‘‘;’’ in a single line.
* Use import clauses instead of qualified names to reference names in code.
* Frequent calls to sub-properties of class member properties should be made storing a reference to that sub-property, avoiding multiple statements containing long chains of objects and sub-properties;
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

They constructed several models using these factors, mainly a Bayesian classifier, all of which predicted average readability scores better than the human raters. But the model is not public.

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

The advantage of tablike spaces over elastic tabstops is that the underlying text file is just indenting with spaces in a monospaced font. So it's only the visual presentation that changes, hence it can be used on a team.

Familiarity
===========

C’s operator precedence, C++’s use of ``<>`` for generics, and C#’s design of properties are all "legacy" decisions. They were designed based on limited information and are kept for compatibility reasons even though in hindsight it has become clear that better choices exist.

So when designing a new language, is it worth repeating these mistakes for the benefit of "familiarity"? Familiarity will not help when novice programmers are learning your language - they will instead become confused because the design is mistaken. Copying mistakes increases perceived language complexity.

Aiming for a coherent, simple design will allow for a level playing field, regardless of experience, schooling or language history. Some experts may become discomfited by different choices, but their "ragequit" posts will most likely serve to draw positive attention to the language, as long as there is a solid basis for the design. Anybody who uses the languages will get used to it. What is more important to aim for being intuitive, in particular cloze completion - allowing someone to come back to a codebase after 6 months and correctly type out a new function with only half-remembered syntax.

Now, learning a language takes time and effort. Self-taught novices might want to start with a book. How long should it be? A `121 page Python book (60 pages double spaced) <https://www.amazon.com/Python-Programming-Beginners-Comprehensive-Hands/dp/B0BFV21L24/>`__ is derided as terse and useless, requiring to google every new keyword. `K&R C <https://www.amazon.com/C-Programming-Language-2nd-Edition/dp/0131103628/>`__ has 272 pages, but is "not beginner friendly". The `C# Programming Yellow Book <http://www.csharpcourse.com/>`__  is 217 8.5x11 pages or about 322 of the standard 7x9 pages. `Python for Kids <https://www.amazon.com/Python-Kids-Playful-Introduction-Programming/dp/1593274076/>`__ clocks in at 344 pages but is still missing critical functions such as the input command. On the other hand some chapters such as turtle graphics, tkinter, and classes/objects can be skipped (74 pages). My first programming book `Beginning Programming with Java For Dummies <https://www.amazon.com/Beginning-Programming-Java-Dummies-Computers/dp/0764526464/>`__ had 408 pages. The `5th edition <https://www.amazon.com/Beginning-Programming-Java-Dummies-Computer/dp/1119235537/>`__ is the most popular and has 560 pages. But it still only covers the basics. `Head First Java <https://www.amazon.com/Head-First-Java-2nd-Edition/dp/0596009208/>`__ is recommended by the r/learnprogramming subreddit and has 688 pages.

Others recommend skipping the "dead tree" format altogether and watching videos on YouTube or doing educational courses on edX, Udacity, and Coursera. On YouTube `MIT
6.0001 <https://ocw.mit.edu/courses/6-0001-introduction-to-computer-science-and-programming-in-python-fall-2016/video_galleries/lecture-videos/>` is around 12x45=540 minutes. `CS50P <https://www.youtube.com/playlist?list=PLhQjrBD2T3817j24-GogXmWqO5Q5vYy0V>`__ is 14x1.2=1005 minutes. The amateur `CS Dojo <https://www.youtube.com/playlist?list=PLBZBJbE_rGRWeh5mIBhD-hhDwSEDxogDg>` is 16x~13=217 minutes. `Digilent Inc.'s course <https://www.youtube.com/playlist?list=PL0845FEB57E5894C2>`__ is 87x6.5=561 minutes. Coursera's `Learn to program <https://www.coursera.org/learn/learn-to-program>`__ course is 291 minutes or less than 5 hours of video content but there are 43 readings and Coursera says it will take 25 hours to complete.

Learning a new language can be faster if you already know a language, but you can also bring over preconceptions. For example in :cite:`joostenTeachingFunctionalProgramming1993`, imperative gotchas became misconceptions in functional programming: variables can be defined after they are used, operators like ``tail``, ``take``, ``drop``, ``remove``, ``filter`` do not mutate their arguments, and there is no need to clone results to prevent them from being mutated and corrupted. It's not clear what can be done - people hate UI changes, and will complain when their cherished workarounds no longer work, even if they are now unnecessary. `Dijkstra <https://www.cs.utexas.edu/users/EWD/ewd04xx/EWD498.PDF>`__ similarly stated that COBOL "cripples the mind" and BASIC "mentally mutilates programmers beyond hope of regeneration", presumably because they give the programmer the wrong impression of what programming is. Maybe there is hope for a "Stroscot for Y programmers" series or maybe it will just make people's brains explode.


If your language is too different from other languages, not enough people may be willing to invest the time to give it a try.

it’s important to be considerate of how many things in your language will be strange for your target audience, because if you put too many strange things in, they won’t give it a try.

You can see us pandering to our major target audiences - C uses curly braces, so we do too.

If you include no new features, then there’s no incentive to use your language.  Language designers should give careful thought to how strange their langauge is, and choose the right amount to accomplish what they’re trying to accomplish.


Therefore, it’s best to treat familiarity as a tie-breaker: to be used sparingly, only when the pros and cons of different design options have been fully explored, and it has been determined that no design has an edge above the other.

But if one design has arguments for it, and another design has only familiarity on its side, language designers of the future are implored to pick the former to stop propagating the same language design mistakes further and further into the future.6

According to `this <https://soc.me/languages/familiarity>`__ the benefits of familiarity are fleeting, because once your language becomes standard people will be familiar with it anyway. This conflicts with the `notion <https://steveklabnik.com/writing/the-language-strangeness-budget>`__ of Rust's "strangeness budget", where a language can only be so weird before it gets discarded from consideration and can never become standard.

As Randomo shows, the choice of characters for operators is arbitrary. Using familiar syntax at least benefits existing programmers, while new programmers will be confused regardless.

But cases where newbies can benefit, such as single = for assignment and comparison, do seem worth discarding familiarity for.

Filenames
=========

* On Linux, the kernel defines filenames as arbitrary byte sequences that do not contain ASCII / or null, compared by byte equality. Most applications expect filenames in UTF-8, and produce NFC UTF-8, but this is not enforced.
* On Windows, NTFS defines filenames as sequences of 16 bit characters excluding 0x0000, compared case insensitively using an uppercase mapping table. The Windows APIs will error on filenames containing on the UTF-16 characters ``<>:"/\|?*``, and the UTF-16 filenames CON, PRN, AUX, CLOCK$, NUL, COM[0-9], LPT[0-9], $Mft, $MftMirr, $LogFile, $Volume, $AttrDef, $Bitmap, $Boot, $BadClus, $Secure, $Upcase, $Extend, $Quota, $ObjId and $Reparse are reserved by the system for internal use, including with file extensions such as aux.c or NUL.txt. Proper UTF-16 encoding is not enforced but most applications including the shell use NFC normalized UTF-16. The Windows shell does not support a filename ending with a UTF-16 space or a period, or displaying decomposed Hangul.
* On macOS, filenames are UTF-8, normalized via Unicode 3.2 NFD (HFS+) or not (APFS). Case is preserved but filename comparison is case insensitive and normalized via Unicode 3.2 NFD (NFS+) or modern NFD (APFS).

Taking union, we have that a filename is always a byte sequence. Taking intersection, we have that a NFC-normalized sequence of Unicode codepoints excluding ``<>:"/\|?*`` and the Windows reserved names is always transformable to a filename.

Unicode
=======

At a minimum comments in localized scripts should be supported. But `lots of languages <https://rosettacode.org/wiki/Unicode_variable_names>`__ support Unicode variable names too.

Usability
---------

Unicode character input still has no standard solution. Copy-pasting from websites or a small cheat file is simple but it is too tedious to use frequently. Other methods include a language-specific keyboard, OS input methods like Character Map, or editor input methods like ``\name<tab>`` in Jupyter, `extensions <https://marketplace.visualstudio.com/items?itemName=brunnerh.insert-unicode>`__ for VSCode, or ``Ctrl+x 8 Enter`` in Emacs. Generally it seems there is no shortage of solutions and people will put in the effort to find a good IME as required. It is really an editor problem, not a PL problem.

Unicode itself is quite complex and people can get confused by invisible spaces, bidirectional text, and lookalike characters. Compiler warnings can reduce these problems.

Non-English beginner tutorials are more friendly if they can use localized variable names for familiarity. Identifiers seem to be the limit though. Per `quotes from Y studios <https://ystudios.com/insights-passion/codelanguage>`__,  localized keywords are very tricky to implement, and often don't work well with the rest of the syntax, and localized grammars and word order are tricky too and also really confusing. But there are experiments like ChinesePython that have seen limited interest. It is a lot of work to localize the full language:

* error messages, warnings, diagnostics
* standard library method names and strings
* documentation
* parser - keywords/reserved words, grammar, word order

Language fragmentation
----------------------

People aren't omniglots, so using multiple languages will cause library fragmentation. Past introductory tutorials that write throwaway code it makes sense to use a common language. Which one though?

Per `Wikipedia <https://en.wikipedia.org/wiki/List_of_languages_by_total_number_of_speakers>`__ English has 1.452 billion total speakers and Standard Chinese 1.118 billion, with Hindi and others less than half English. Even if we count "second language" liberally, English is as high as 2 billion while Standard Chinese is only 1.5 billion, so the gap increases slightly. And calculating growth rates from `2021 <https://en.wikipedia.org/w/index.php?title=List_of_languages_by_total_number_of_speakers&direction=prev&oldid=1073408213>`__ and earlier, English increased by 7.7%-9.8%/year while Chinese has remained mostly steady at -0.1% to 3.3%/year. Per `this WP page <https://en.wikipedia.org/wiki/Languages_used_on_the_Internet>`__ English websites are 61.1% English, 1.7% Chinese, while internet users are 25.9% English, 19.4% Chinese. The number of Chinese websites is probably skewed low because most Chinese content is on social sites rather than independent sites, and the firewall makes it hard to index. Still though, across all of these statistics, there is a clear pattern of English being first.

Choosing Standard Chinese also has political problems since the speakers are mainly native speakers in China that have been artificially created via the CCP systematically targeting ethnic minorities and forcing them to learn Standard Chinese in place of their original dialect. In contrast English is mainly a second language and its speakers are spread across many countries.

Also Chinese is `just plain hard <http://pinyin.info/readings/texts/moser.html>`__ to learn and remember. Per that article it takes 7-8 years to learn 3000 Chinese characters but half that time to learn a comparable number of French or Spanish words. Then there is the `character amnesia <https://en.wikipedia.org/wiki/Character_amnesia>`__ problem where people can read the character just fine but forget how to write it by hand, only remembering the pinyin Latin-based transcription.

So English it is.

Unicode overuse
---------------

Stroscot's user-defined syntax is flexible enough to create APL-style operators if desired. But just compare this example of computing the prime numbers less than ``R`` in APL vs. a Haskell style:

::

  T←1↓⍳R
  (~T∊T∘.×T)/T

::

  T = drop 1 (count R)
  scan (not (isElementOf T (tie 0 (*) T T))) T

IMO the letter-based prefix operators are easier to read - particularly, the word choices give clues as to what is happening. Although the number of APL hieroglyphs is not comparable to Chinese's thousands of ideograms, it seems likely that APL suffers from learnability issues similar to Chinese and is harder to learn than letter-based identifiers because it does not have a phonetic basis.

Similarly Agda uses mathematical Unicode symbols extensively for both identifiers and syntax. This kind of mathematical jargon seems fine so long as there's clear documentation or it's limited to .

Encoding
--------

There are various encodings of Unicode, like UTF-8, UTF-16, and GBK/GB 18030. But UTF-8 has 97.8% market share, and general-purpose compression algorithms provide better compression than specialized encodings. So for now it's not worth supporting anything besides UTF-8. If it becomes necessary to support another encoding then it can be cheaply written as a prepass that run ICU and transforms the encoding to UTF-8. Maybe as an alternative it is possible to use an encoding abstraction that supports many encodings and doesn't assume UTF-8 properties like stream synchronization; it depends on if there is a performance hit for such an abstraction.

NFC
---

NFC solves the issue of having the same font grapheme but different codepoint encoding, like A + combining acute accent vs the precomposed character "latin capital letter a with acute". NFC is used by 98% of the web and a fair amount of software automatically normalizes input to NFC (e.g. web browsers). Also per `Unicode Normalization FAQ <http://www.unicode.org/faq/normalization.html>`__ "NFC is the best form for general text." It also seems that the unstated opinion of the Unicode Consortium is that text that cannot be NFC'd does not count as "Unicode". When there was an issue with NFC breaking `Biblical Hebrew <https://www.unicode.org/mail-arch/unicode-ml/y2003-m06/0423.html>`__ the solution was to change the input (inserting joiners) rather than modifying NFC.

So it seems correct to soft-require input to be NFC normalized. This might annoy someone somewhere, but they can work around it by putting in joiners, like Biblical Hebrew had to do. We cannot hard-require because `per someone <https://github.com/rust-lang/rfcs/pull/2457#issuecomment-395488644>`__ there exist some Vietnamese keyboards that produce combining characters not in NFC.

NFC also means that unnormalized strings or raw binary data can't be included in files directly. But keeping those in separate files or encoding the bad bytes as hexadecimal seems fine.

NFKC
----

NFKC is often brought up as an alternative/extension of NFC. For example `Python <https://peps.python.org/pep-3131/>`__ uses NKFC for identifiers, and Go similarly has a `proposal <https://github.com/golang/go/issues/27896>`__ to use NFKC.

There are two choices for using NFKC, requiring input to be NFKC or applying NFKC to the input. Python only applies NFKC, so `the following <https://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/QcRz1gdfAQAJ>`__ is a valid Python program::

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


If we required the input to be in NFKC it would have to look like::

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

::

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

In the Unicode TRs NFKC usually is used in conjunction with case folding. In particular Unicode 3.13 R5 defines the mapping toNFKC_Casefold which case folds, normalizes, and removes default ignorable code points, and this operation is recommended for matching identifiers case-insensitively. Similarly `TR36 <https://www.unicode.org/reports/tr36/#Recommendations_General>`__ recommends processing identifiers by applying NFKC_Casefold. So NFKC doesn't make a lot of sense since Stroscot is case-sensitive. Many have `suggested <https://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/ki8XN66iAQAJhttps://groups.google.com/g/dev-python/c/LkCtik9LyyE/m/ki8XN66iAQAJ>`__ that Python made the wrong choice when it picked NFKC because Python is case-sensitive.

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

TR31 specifically recommends excluding font transformations (1194 characters, 32% of NFKC) to allow mathematical notation. The superscript/subscript transforms also `confuse people <https://stackoverflow.com/questions/48404881/unicode-subscripts-and-superscripts-in-identifiers-why-does-python-consider-xu>`__ and seem to be unwanted. For Go, bcmills says superscripts and subscripts are 'cutesy' which seems to be an acknowledgement of the fact that they should not be erased. Similarly circle, fraction, square, and small look so different that they will confuse people as to why they are considered equivalent.

The symbol and ligature transformations in compat do seem useful. Python `apparently <https://mail.python.org/pipermail/python-3000/2007-May/007995.html>`__ went with NFKC because they were worried about confusing ligatures, specifically ﬁnd vs find (the first using the U+FB01 LATIN SMALL LIGATURE FI character). In VSCode the fi ligature shows up compressed into one fixed-width space so is visibly different from the non-ligature version, but in proportional fonts this is indeed a problem. The Go issue mentions micro and mu, which per Wikipedia look identical in most fonts, although some fonts do distinguish them. noBreak is also useful. wide/narrow/vertical/Arabic do look clearly different in my fonts, but the characters are intended only to support legacy character encodings so transforming them away is probably best. (`CHARMOD <https://www.w3.org/TR/charmod-norm/#canonical_compatibility>`__)

Overall, only 20% of the transformations NFKC does are clearly useful (compat), the standard recommends excluding 32% of transformations (fonts), and a further 12% (circle, fraction, square, small) also are clearly unwanted. It's clear that the stock NKFC transform isn't appropriate. Even if we used the reasonable subset of NFKC transforms, we're barely using half, so at best we could say we are using "NFC with most decompositions from NFKC". The base is still NFC and it's just monkeying up the encoding.

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

  ::

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

Case
----

Go's rule is that identifier characters must be letters or digits as defined by Unicode, and exported identifiers must start with an upper-case letter, excluding combining characters and Devanagari. But this upper case restriction means 日本語 cannot be exported, and instead X日本語 must be used.

Generally it seeems that case distinctions only work for English, and are somewhat hard to get right. So we don't put it in the syntax and leave case as a style guideline.

Confusables
-----------

So requiring NFKC is not a good idea - do we have to roll our own transform? Fortunately, the answer is not really - there is an alternative to NFKC, namely the TR39 `confusable detection <https://www.unicode.org/reports/tr39/#Confusable_Detection>`__ transformation. `Rust <https://github.com/rust-lang/rfcs/blob/master/text/2457-non-ascii-idents.md>`__ uses this. The dataset `contains <https://www.unicode.org/Public/security/latest/confusables.txt>`__ conversions of:

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

Script restrictions
-------------------

`TR31 <http://www.unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers>`__ proposes lists of allowed and disallowed scripts, and recommends defaulting to allowing new characters in identifiers. IMO this sort of script restriction is not desired by default, e.g. I would want to be able to use the character 𓂸 (Egyptian Hieroglyphs script, on TR31 excluded list) without being forced to specify a flag.

TR39 defines a mixed script restriction, which prohibits text such as Ωmega, Teχ, HλLF-LIFE, and Toys-Я-Us. This could be used to enforce some uniformity on identifier parts, but again seems too restrictive to enable by default. It does prevent some homoglyph attacks, but the confusable detection approach is much more robust.

Bidi attack
-----------

:cite:`boucherTrojanSourceInvisible` proposes a "Trojan Source" bidi attack based on Unicode. The idea is someone copy-pastes from StackOverflow, submits a malicious PR, or just publishes a new project, and the source code looks safe but isn't. In particular the attack is based on bidi overrides, the LRE, RLE, LRO, RLO, LRI, RLI, FSI, PDF, and PDI invisible characters. For example RLI a b c PDI will display as cba, and RLI LRI a b c PDI LRI d e f PDI PDI will display as d e f a b c. This enables near-arbitrary reordering of strings, and even hiding parts of strings by overwriting characters.

Language syntax does not generally allow bidi overrides, but they can show up in comments and strings, and the bidi overrides can obsfuscate which part is the comment or string. For example ``"x"; return`` could look like ``"return x;"`` (early return), ``/* if { */`` could look like ``/* */ if {`` (commenting out), and ``"user // check"`` could look like ``"user" // check`` (stretched string). The overrides are visible in most syntax highlighting and when selecting/navigating through the text, but these cues are easy to miss.

The solution presented in the paper is to ban unterminated bidi override characters within string literals and comments. This prevents reordering across string and comment boundaries.

Parsing
=======

I've got a basic Earley algorithm working in JS, but it's not used anywhere. But eventually I could extend it with BSRs and layout and other fun things. There's also `Yakker <https://github.com/attresearch/yakker>`__, which is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

  A new parsing engine, Yakker, capable of handling the requirements of modern applications including full scannerless context-free grammars with regular expressions as right-hand sides for defining nonterminals. Yakker also includes facilities for binding variables to intermediate parse results and using such bindings within arbitrary constraints to control parsing. Yakker supports both semantic actions and speculative parsing techniques such as backtracking and context-free lookahead and several parsing back ends (including Earley, GLR and backtracking).  In addition, nonterminals may be parameterized by arbitrary values, which gives the system good modularity and abstraction properties in the presence of data-dependent parsing. Finally, legacy parsing libraries, such as sophisticated libraries for dates and times, may be directly incorporated into parser specifications.

I've looked at various algorithms but I think the only way to handle it completely correctly and generically is to have a disambiguating pass on the set of parse tree generated by a nondeterministic automaton. The alternatives involve restricting parsers to be deterministic, for example PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

Treesitter implements incremental LR parsing with error recovery, but since it doesn't support ambiguity I don't think it's sufficient for a compiler.

Revisiting this, the goal is to use partial evaluation to generate the parser, by speeding up a naive brute-force algorithm applied to the grammar. There is already a paper on LR parsing by partial evaluation :cite:`sperberGenerationLRParsers2000` and also on specializing Earley, so with sufficiently powerful compiler optimization handling general grammars should be possible.

In particular the parser should be written as a nondeterministic finite state transducer that builds up trees (outputs a list in the style of start-children-end or S-expressions or something).

Formally:

* Q is a finite set, the set of states;
* I is a subset of Q, the set of initial states;
* F is a subset of Q, the set of final states; and
* Σ is a finite set, called the input alphabet;
* Γ is a finite set, called the output alphabet;
* The transition function is of type :math:`Q \times (\Sigma \cup \{\epsilon \})\to P(Q \times (\Gamma \cup \{\epsilon \}))`, where ε is the empty string and P(Q) denotes the power set of Q.

TODO: match this up with Parsec, attoparsec, trifecta, etc. the syntax should be similar except with nondeterministic choice ``|``.

Blocks
======

Blocks are inspired by Haskell's do notation. For an example of how natural this is you can look at :ref:`how I/O works <tasks>`. Since codensity/continuations are the mother of all monads, we don't lose anything by fixing the monadic operations in the do-notation to be the continuation monad operations.

There is also "not returning anything" versus returning a value ``()``. In Haskell these are generally consdiered the same. But using the continuation monad allows us to separate commands (not returning a value) and operations (returning a value). Haskell has the translation ``{e;stmts} = e >> stmts = \c -> e (\_ -> {stmts} c)``. But usually ``e`` returns ``()``, so ``(>>)`` is applied at the type ``f () -> f b -> f b`` and that ``\_`` is a ``\()``. With our translation, commands (which don't return a value) are functions ``r -> r``. Haskell's translation would require them to be ``Cont r () = (() -> r) -> r``, which is equivalent but has an extra ``()`` floating around. But in both translations operations (whose value is used) are of type ``Cont r a = (a -> r) -> r``. The non-uniform type for actions might make copying code from Haskell a little harder, but on the other hand we get function composition as a built-in syntax. That's right, the most basic operation in category theory is available as syntactic sugar in Stroscot. Take that, Haskell. And also we can easily use indexed monads, just change ``r) -> r`` to ``r) -> s``.

The return keyword should be invalid in short-form (pure) method definitions, like ``f x = x``, but should be required for blocks, i.e. ``f x = { return x }``. There is some question over whether to allow calling a function without return, i.e. ``f x = { return (g x) }`` versus ``f x = { g x }`` where ``g`` is itself a block.




ApplicativeDo
-------------

ApplicativeDo :cite:`marlowDesugaringHaskellDonotation2016` has two functions. The first is to make some do-notation sequences be Applicative rather than Monad. In fact though these are exactly the sequences handled by idiom brackets, of the form ``{a <- ax; b <- bx; return (f a b)} = return (f !a !b)``. Idiom brackets are shorter, so the value this provides is minimal.

The second function is to use applicative operations instead of monadic operations because in "some" monads the applicative operation is more efficient. Their example is the Haxl DSL:

::

  numCommonFriends :: Id -> Id -> Haxl Int
  numCommonFriends x y = do
    fx <- friendsOf x
    fy <- friendsOf y
    return (length (intersect fx fy))

Well, if you're writing a DSL then writing it as a macro is much more powerful than trying to shoehorn it into an applicative/monadic framework. They discuss in the paper that the translation to use applicative operations is ambiguous and the best one depends on details of the computation that are not accessible, because functions are opaque. It's exactly these kinds of details that *are* accessible in a DSL - you just write a pass that walks over the expression tree and estimates the costs. Similarly the `use/def analysis <https://en.wikipedia.org/wiki/Use-define_chain>`__ that they use for the rewriting is a standard compiler pass. The commutativity mentioned in the paper is another property one could know from the DSL and that changes the output significantly.

For regular do notation with continuations, the applicative notation translates to exactly the same functions as the monadic notation.

Verdict: DSL in disguise. Just write a DSL. Stroscot does not benefit at all by adding ApplicativeDo.

RecursiveDo
-----------

RecursiveDo :cite:`erkokValueRecursionMonadic2002` is an older extension to do notation. The motivating example is a circuit DSL:

::

   toggle : Signal Bool
   toggle = out
      where
         inp = inv out
         out = delay False inp

   counter : Signal Bool -> Signal Int
   counter reset = out
      where
         next = delay 0 inc
         inc = out + 1
         out = mux reset zero next
         zero = 0

But wait, where's the do notation? In fact, this is really just a DSL. There are no monads and no sequencing to be found. All of these operations happen in parallel. The uses for these circuit descriptions all depend on the circuits being specified using a small set of operations specified in a typeclass.

Investigating Hackage, mdo is uncommon. "Many Haskell programmers will never use it in their careers." (`1 <https://ro-che.info/articles/2015-09-02-monadfix>`__) Uses fall into categories:
* DSLs, where variable assignments are interpreted as data
* Gratuitous (no/one binding, or bindings do not refer to bindings from later)
* Examples where it would be clearer to use mfix or the do-rec notation that is just ``(a,b,c) <- mfix (\(a,b,c) -> (_,_,_))``
* I/O monad, mfix is used to write the code in a recursive style instead of modifying a variable, e.g. forking two threads that kill each other:

::

   mdo
      a <- fork $ killThread b
      b <- fork $ killThread a

   -- vs
   bId <- newEmptyMVar
   a <- fork $ readMVar b >>= killThread
   b <- fork $ killThread a
   writeMVar bId b

The code for IO's mfix uses unsafeDupableInterleaveIO. This has been the subject of at least one `bug <https://gitlab.haskell.org/ghc/ghc/-/issues/5421>`__ (`two <https://gitlab.haskell.org/ghc/ghc/-/issues/15349>`__ counting fixST), and is why there is both fixIO and `unsafeFixIO <https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Unsafe.html#v:unsafeFixIO>`__. Reasoning about fixIO seems to `require <https://wiki.haskell.org/Evaluation_order_and_state_tokens>`__ laziness semantics and maybe also an understanding of Haskell's State-based I/O model.

Also, most monads fail to satisfy monadic right shrinking, which IMO makes the notation completely unintuitive:

::

   mdo
      z <- f z
      w <- g z
      return (z,w)

   -- is NOT equivalent to

   z <- mdo
            z <- f z
            return z
   w <- g z
   return (z,w)

The only price to pay for leaving mdo out is that value-recursive monadic computations have to be written with ``mfix`` or its tuple-heavy cousin ``rec{}``. We can still implement ``mfix`` for the monads that matter, like ``State``. According to all available knowledge, ``mfix`` can't be implemented for continuations, so nothing is lost from regular programs.

Verdict: Not only a DSL in disguise, but also a footgun. mfix and the rec{} notation are better for those who care.

Arrows
------

You might be getting the pattern here. Arrows were inspired by a parsing DSL. Any arrow which supports the ArrowApply class is a monad. Arrows not supporting ArrowApply must write operations for every language element supported (variable, function, conditional, grammar production choice, and so on). Continuations require ArrowApply to even implement the basic arrow interface. Verdict: trash, a leaky "abstraction" that just wastes everyone's time.

Idiom brackets
--------------

While do notation is defined for monads, idiom brackets are defined for applicative functors, ``[[ f a b ]] = pure f <*> a <*> b``. But DSL notation works too: ``apply { a + b }``.

The issue with translating to ``<*>`` is that it assumes left-to-right evaluation. You can see this in the `translation <https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html#t:Applicative>`__ for Monads: ``m1 <*> m2`` binds ``m1`` before ``m2``. In Stroscot the program is required to be equivalent under all evaluation orders. So to enforce this we need a function ``parallel : [m a] -> m [a]`` that checks there is no issue with evaluating in parallel. Then using parallel the translation of ``apply { f a b x }`` looks like ``{ (av,bv,cv) = parallel (a,b,c); return (f av bv cv) }``

Idris defines `!-notation <http://docs.idris-lang.org/en/latest/tutorial/interfaces.html#notation>`__, "implicitly bound application". The scoping is `unintuitive <https://github.com/idris-lang/Idris-dev/issues/4395>`__, but the notation itself is powerful. Binding it to a syntactic block seems reasonable. And it can easily express idiom brackets, ``[[ f a b ]]`` becomes ``{ f !a !b }``. Idiom brackets save characters with more arguments, but bang notation looks natural if there are multiple bindings in the block.

C-like reference access
-----------------------

For example we want to do:

::

  a = ref 1
  b = ref 2
  c = a + b
  a := c

Translated this looks like:

::

   ref 1 >>= \a ->
   ref 2 >>= \b ->
   parallel (read a, read b) >>= \(av,bv)  ->
   let c = av + bv in
   writeRef a c

I think the solution is another DSL. Inserting ``read a`` is not too complicated, just follow the C/C++ rules about converting lvalues to rvalues.

Assignment
==========

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

This rule reduces maintainability. If the representation is changed there is no way to replace the dumb constructor with a smart constructor. So instead libraries are littered with boilerplate pseudo-constructors like ``mkThing = Thing`` to get around this syntactic restriction. In fact in :cite:`kahrsNonOmegaOverlappingTRSsAre2016` there is a boilerplate trick to turn any TRS into a constructor TRS, by duplicating ``foo`` into a constructor ``Foo`` and a function ``foo``, converting subterms of the original rules to match on constructors, and adding rules that turn stuck patterns into constructors. For example ``k x y = x; s x y z = (x z) (y z)`` turns into:

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

We want to support mutually recursive definitions, like so:

::

  a = 1 : a

  b = 1 : c
  c = 1 : b

And also sequential execution, like so:

::

  a = openFile "a.txt"
  b = openFile "b.txt"

And also shadowing variables, like so:

::

  a = a + 1
  -- interpreted as
  a_new = a_old + 1

In the recursive version ``c`` can be in scope in the body of ``b`` even though it is defined later. Presumably it isn't in scope in the sequential version.

In the recursive version ``a`` is in scope in its own body. In the shadowing version ``a`` is not.

Resolving this probably means a special syntax for something. Choices:
* ``a <- openFile "a.txt"`` for sequenced actions
* ``a <- a + 1`` for shadowing
* ``rec { a = 1 : a }`` for recursive definitions

Generally sequential blocks do not use recursion. But recursion is used all the time at the module/function level.

Type declarations
=================

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

Comments allow writing documentation inline with the code. This speeds up development by keeping all the information in one file and avoiding having to jump around. It also encourages a standardized documentation format.

Tool support can be incomplete because there is a lot of freedom in comments. People can put comments in random places and they can be attached to whatever and indented strangely. With such freedom the reformatter will likely mangle some comments, but probably people will just learn not to do that.

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

Common choices
--------------

Counting up examples on Rosetta Code's `comment page <https://rosettacode.org/wiki/Comments>`__ showed the most common choices:

* For EOL impl, C++ style ``//``
* For block impl, C style ``/* */``
* For EOL doc, D style ``///``.
* For block doc, Javadoc style ``/** */``
* For code comments (nesting block), Pascal style ``(* *)``

Parsing
-------

Generally EOL comments hide the start or end of a block comment. This is useful in some hacks like embedding Javascript in HTML or doing ``//* \n /*/ \n // */`` vs ``/* \n /*/ \n // */`` to switch between two blocks of code. But the parser could pick out the block start/end and not ignore it. There is a different code block trick ``/*/ \n /*/ \n /**/`` vs ``/**/ \n /*/ \n /**/`` which doesn't depend on this behavior.

Using two characters to start a comment helps prevent the accidental starting of a comment and allows more freedom in avoiding syntax conflicts in the language. The double slash // does pretty well in this context, but the /* does not do quite as well. For example in C ``a =1/*ptr;`` starts a comment instead of doing a division. There is the opposite issue that an extra space between the two comment characters, like ``/ / comment``, will cause the comment to be missed, but usually the contents of the comment will cause a compilation error.

Multiline block comments have the issue of forgetting the end terminator and matching some other end terminator. Some languages only have EOL comments, presumably to avoid this problem. Nesting solves this because there will be an unterminated comment. Similarly forbidding block start indicators from appearing in a block comment will work. The compiler can also check each line and see if it looks like code, although this prevents commenting out code.

Whitespace
==========

Whitespace in identifiers... this doesn't work well with Haskell syntax. With whitespace ``do something = ...``` would define the identifier ``do something``, but in Haskell it's a clause ``do _`` that binds ``something``.

OTOH using a string works fine: ``"do something" = ...``

You could also make something an atom, then you can write ``do something`` in code but the clause definition is ``do ^something = ...``. The semantics are similar to a single identifier but different enough that I don't think it counts.

Indendation sensitivity
=======================

Indentation-sensitivity like Python and Haskell seems great. It requires less typing, fewer lines of code (no line for closing brace), and when copy-pasting code you only have to fix up the indentation by moving the block left/right (supported by all modern code editors) instead of messing with braces.

Mixing tabs and spaces can lead to errors, but erroring on this is fine.

Haskell's layout rules seem overly restrictive, for example this is not allowed:

::

  let bang_upper = Bang (Rule
    (Sequent newcut_bseq (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
    (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br))))

Although the parentheses make this unambiguous, Haskell requires indenting a lot more, past the ``=``:

::

  let bang_upper = Bang (Rule
                    (Sequent (fst bl_tseq, newcut_bseq) (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
                    (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br)))

Per `anecdote of Kmett <https://stackoverflow.com/a/2149878>`__ this requirement makes Haskell's layout rules too complex for blind people because it requires lining up columns.

Rob Pike says indentation sensitivity "is nice for small programs" but causes issues with embedding. For example, "a Python snippet embedded in another language, for instance through a SWIG invocation, is subtly and invisibly broken by a change in the indentation of the surrounding code." This seems like an issue caused by the embedding style - if the snippet was in a separate file then the tools might deal with it better. Haskell defines a translation from indentation style to brace syntax, and just requiring brace syntax in these embeddings might be sufficient.

Blind programmers have diverse opinions. The majority seem to use screen readers, and per `this HN comment <https://news.ycombinator.com/item?id=11419478>`__ "almost all" screen readers have a setting to report the indentation of the current line, and this is relatively easy to use. For example per `this video <https://www.youtube.com/watch?v=qvg-uo_I7JM>`__ NVDA can be set up to automatically switch between different profiles for different tasks based on the focused window / process, and can announce indentation level using both beep tones and a TTS description of the number of spaces or tabs when navigating. Per `this <https://github.com/microsoft/vscode/issues/147386>`__ JAWS provides similar functionality. Roughly the experiences can be compared like this:

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

Some people turn on indentation even for brace languages, because knowing the indentation level can be helpful in navigating code. However, `Rune <https://github.com/google/rune/blob/main/doc/rune4python.md>`__ and `several <https://www.youtube.com/watch?v=94swlF55tVc>`__ `others <https://stackoverflow.com/a/453758>`__ say that brace languages like C# are usable without indentation on, and then they don't have to set up profiles. IMO the indentation version seems a little easier to navigate, and this is backed up by Python still being a popular coding language for blind people (e.g. NVDA is written in Python). But obviously everyone has their preference and supporting several options can improve accessibility. There is a pindent script in the Python distribution that adds/removes block open/close markers, so you can run it as a git smudge/clean filter. It seems Stroscot similarly should provide an explicit but optional brace syntax and a smudge/clean filter to convert to/from this syntax.

Another option for blind people is the Braille display, but it is expensive and only shows at most 80 characters. Per `this user <https://stackoverflow.com/a/148880>`__ it can help with both indentation and complex punctuation, particularly lines with many nested parentheses. But the screen reader is usually faster. Comparing wpm, Braille is around 150 wpm starting out going up to 250 wpm, a physical limit of how fast fingers can run over the dots. 150 wpm is also about what TTS does by default but TTS can be sped up to around 500 wpm as the user becomes more accustomed to the synthesizer, :cite:`stentIntelligibilityFastSynthesized2011` and even at 900 wpm experienced users can still transcribe gibberish text with 50% accuracy. So TTS has markedly more bandwidth.

`emacspeak <http://tvraman.github.io/emacspeak/manual/emacspeak_002dpython.html>`__ has speech-enabled python-mode and `per ML thread <https://groups.google.com/g/comp.lang.python/c/Dm-qTzO8Db8?hl=en#3216b7a02047873a>`__ reads things like "closes block <block's opening line>" on dedent. But it seems like it is hard to install and not really that popular.

Braces and brackets
===================

Haskell uses parentheses for most grouping, ``{}`` for replacing whitespace with explicit syntax, ``[]`` for lists, and has no special meaning for angle brackets.

`Simon <https://soc.me/languages/stop-using-angle-brackets-for-generics>`__ says to use square brackets ``[]`` instead of angle brackets ``<>`` for generics. With Haskell syntax this is moot because parentheses suffices. But he argues collection literals and array lookup should use standard parentheses ``()`` instead of special syntax, because it will become dead weight once the standard library develops better data structures.

Seems a bit weird, he cites Python as an example but Python still uses list literals: the syntax for a NumPy array is ``np.array([1, 2, 3, 4, 5])``. The only thing overloaded is access ``arr[i] = x``.

Julia doesn't require parens around conditions in ``if`` and ``while``, ``if a == b`` instead of ``if (a == b)``.

Function syntax
===============

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

Chained assignment
==================

Chained assignment is an expression like ``w = x = y = z``. The value of ``z`` is assigned to multiple variables ``w``, ``x``, and ``y``. The `literature <http://www.cse.iitm.ac.in/~amannoug/imop/tr-3.pdf>`__ classifies this as "syntactic sugar", so handling it in the parser like Python seems the reasonable solution - C's "the assignment returns the lvalue" semantics seems contrived.

The evaluation strategy differs between languages. For simple chained assignments, like initializing multiple variables, the evaluation strategy does not matter, but if the targets (l-values) in the assignment are connected in some way, the evaluation strategy affects the result. Here C's RTL semantics makes more sense and seems more useful than `Python's LTR <https://docs.python.org/3/reference/simple_stmts.html#assignment-statements>` semantics. So a chain ``a := b := 2`` should expand to ``b := 2; a := b`` rather than ``t = 2; a := t; b := t`` .

Chained update with ``:=``, like ``a := b := 2``, seems the most useful to shorten some assignments. Chained ``a = b = 2`` with value semantics doesn't really seem that useful when you could just replace ``a`` with ``b`` in the rest of the expression and save yourself an identifier. Also it conflicts with using ``=`` for comparison, because it can be interpreted as ``a = (b == 2)``.

There is an issue with running I/O multiple times. For example if you need multiple variables with the same value then you would write ``[a,b,c] = replicateM 3 (ref 0)`` rather than using a chain, because a chain would alias to the same variable. Python already has this problem with aliasing for ``a = b = []``, because ``[]`` is mutable, but in Stroscot ``[]`` is pure so this is fine.

Embedded assignment
===================

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

Unless
======

Ruby's ``unless-else`` is unintuitive. Only support ``if-else`` and ``unless`` without the else. Also ``if not`` is a possible replacement for ``unless``.

Integers
========

Flix says binary and octal literals are rarely used in practice, and uses this as a reason to drop support for them. Despite this most languages include support. Clearly there is a conflict here, so let's dive deeper.

Per `Wikipedia <https://en.wikipedia.org/wiki/Octal>`__, octal is indeed rare these days because bytes do not divide evenly into octets whereas they do divide into 2 hex digits. But it can still be useful in certain cases like the ModRM byte which is divided into 2/3/3 just like how a byte divides unevenly into octets, or chmod's Unix file permission specifications which use 3-bit modes. Of course such usages are more likely to confuse than elucidate and using symbolic notation like ``modrm direct eax`` or ``u=rwx,g=rw,o=r`` is clearer. Nonetheless octal still crops up in legacy code as an omnipresent C feature, so should be included for compatibility.

For binary literals, Java 7 added binary literals in 2011, C++ in 2014, and C# 7 in 2017, suggesting significant demand. The `Java proposal <https://mail.openjdk.org/pipermail/coin-dev/2009-March/000929.html>`__ lists bitmasks, bit arrays, and matching protocol specifications as killer usages. Hexadecimal is just artifical for these usages and obscures the intent of the code. Key to the usage of binary literals is a digit separator, so you can break up a long sequence like ``0b1010_1011_1100_1101_1110_1111``.

The alternative to not including literal support is to use a function parsing a string, so one would write for example ``binary "001100"``. Since Stroscot does compile-time evaluation this would work with no runtime overhead and give compile-time exceptions. But it is a little verbose. It is true that humans have 10 fingers but this isn't much reason to restrict literals to decimal, and once you have hex, binary and octal are just more cases to add.

The main thing to avoid is the prefix 0 for octal, as leading zeros are useful for other purposes as well. ``0o`` has been introduced and widely adopted, with no obvious complaints. Similarly ``0b1`` could be confused with ``0xB1``, but teaching programmers about the standardized ``0-letter`` pattern should mostly solve this.

Julia has juxtaposed multiplication? `Jump <https://jump.dev/JuMP.jl/dev/developers/style/#Juxtaposed-multiplication>`__ recommends limiting to cases where RHS is a symbol.

Tuples and records
==================

In `Maybe Not <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/MaybeNot.md>`__ Rich Hickey  says records/fields, and product types are "place oriented programming", hence bad. Well, in `The Value of Values <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/ValueOfValuesLong.md>`__ he says place-oriented programming is when you use in-place update. But maps (his proposed alternative) also support in-place update and are place-oriented. The only difference between maps and records seems to be that records have ordered fields.

So he seems have a different definition in mind, in particular that place-oriented means accessors are not first class - even when the fields are named, you cannot say ``object["name"]`` for an arbitrary object or an arbitrary name. But this is easily solved by adding such functionality. It also doesn't get into the mutable/immutable distinction that the values talk made.

His second point is that product types "complects" the meaning of things with their position in a list. "Complect" is from `Simple Made Easy <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/SimpleMadeEasy-mostly-text.md>`__ and is a pejorative version of "braid together".
Essentially he's saying that if you have ``(String, String)`` there is no way to know how the second string is different from the first string. Well, for commutative operations like addition the order literally doesn't matter. Adding any sort of information to ``(+) : (Int, Int) -> Int`` is complicating the picture. Similarly for Strings `coming up <https://gemma.msl.ubc.ca/resources/baseCode/apidocs/ubic/basecode/util/StringUtil.html#append-java.lang.String-java.lang.String-java.lang.String->`__ with names "appendee" and "appendant" for an  append operation is almost as bad as digging up "complect". Using numerical names ``s1`` and ``s2`` makes more sense. It still gives a record with named fields, but it makes sense to use positional arguments.

And if the types are different there's no ambiguity: ``(FirstName, LastName``, ``(Int,Bool)``, etc.

Precedence
==========

`This post <https://ericlippert.com/2020/02/27/hundred-year-mistakes/>`__ describes a mistake in C-style precedence: it should be ``&&, ==, &`` but is instead ``&&, & ==``, causing a footgun. "Swift, Go, Ruby and Python get it right."


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


In practice Prolog syntax is pretty bad; programs are heavy on meaningless intermediate variables such as ``R`` in the above.

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

Go syntax notes
===============

small set of keywords, without filler keywords (such as 'of', 'to', etc.) or other gratuitous syntax
slight preference for expressive keywords (e.g.  'function') over operators or other syntactic mechanisms
variables, simple control flow are expressed using a light-weight notation (short keywords, little syntax)

DSLs
====

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
