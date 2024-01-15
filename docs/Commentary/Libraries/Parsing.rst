Parsing
#######

Source code by itself is rather difficult to work with. To do anything useful, one would prefer to work with the AST or IR. Extensible syntax complicates the issue because there is no fixed grammar for the language. So we need a parser that can take arbitrary rules. And of course once we have a good parser, there is no harm in exposing it in the language, similar to Raku's inline `grammars <https://docs.raku.org/language/grammars.html>`__. `Per Vognsen <https://mastodon.social/@zwarich@hachyderm.io/109559009711883166>`__ says lexers (and consequently parsers) can be useful for searching through code and indexing it. And also, parser combinator libraries in general are pretty popular, so I'm sure people will write parsers for HTML, JSON, XML, and other common data formats.

There is also Steelman 2B, "The language should have a simple, uniform, and easily parsed grammar and lexical structure." I interpret this requirement loosely as that the language shall have a parser, and this parser shall allow easy access to grammatical and lexical productions.

Similarly, for Steelman 2E, "The user shall not be able to modify the source language syntax. In particular the user shall not be able to introduce new precedence rules or to define new syntactic forms.", we take the opposite: the user shall be able to modify syntax, introduce new precedence rules, and new syntactic forms. There is some justification under Tinman H2. My reply is as follows: the argument is predicated on a distinction between coining new words and moving to another natural language. But in programming, no such distinction exists: all languages can be roughly specified as context-free grammars. The distinction between XML and JSON is much less than that of English and French; in fact, there are xml2json tools which automatically and losslessly convert from one to the other, whereas English-French translations are still described as `imperfect <https://lptranslations.com/learn/how-accurate-is-google-translate/>`__ and subjective. Similarly with HTML, one has a mixture of CSS, JavaScript, and pseudo-XML that is even more of a mishmash than "Franglais", yet no one would claim this undermines the basic understanding of HTML. Rather, different modes of expression suited for different roles are mixed in a complementary manner, guided by standards and formal specifications that are stronger than the "commonalities" found in natural language. The fact that one can write in JS or CSS or so on within an HTML document does not change the basic character or understanding of HTML. The ability to modify the source language syntax simply increases the power of the language in the relevant application areas, and has no drawbacks besides that the new syntax must be learned before it is used.

Scannerless generalized parsing
===============================

The conventional structure of a parser has two levels, the lexer (a regular language) and the grammar (a context-free language). The lexer "tokenises" the input into a stream/sequence of tokens, for example whitespace, keywords, or identifiers. This tokenization uses implicit disambiguation rules such as longest match. Then whitespace tokens are ignored and the rest of the tokens are processed by the parser into a single tree, deterministically. But there is an alternative: scannerless parsing. Per :cite:`visserSyntaxDefinitionLanguage1997`, in scannerless parsing, the grammar may still be conceptually divided into a lexical part and grammar part. However, these are processed into a single character-level grammar, that is mostly context-free. Lexical rules are converted from regular to context-free, while the context-free syntax's rules have optional layout tokens inserted into every concatenation (``a b -> a layout? b``). Since lexical rules and context-free rules may overlap they are renamed for clarity, ``X-LEX`` and ``X-CF``.

Scannerless parsing avoids the need for a streaming interface between scanner and parser. It allows using much more flexible rules for lexical syntax, such as defining nested comments. Furthermore the unified grammar handles some situations that are tricky for conventional parsers. For example, in Pascal, there is an ambiguity between a subrange type ``1 .. 2`` and a list of floating point number constants ``[ 1. .2 ]`` (spaces for clarity). A conventional parser will have to do some lexer hack, while a scannerless parser can disambiguate based on whether a list is being parsed. As another example, with whitespace-sensitive syntax, whitespace is significant in some places, but it is still desirable to be able to ignore it for most rules and centralize its handling in one place. The lack of "tokens" as a layer in the AST makes identifying line numbers and columns easy and doesn't throw away whitespace information, making reformatting and documentation generation easier. It also allows handling constructs like file path literals that are conceptually a single nonterminal but have internal lexical structure.

The main problem with scannerless parsing is it requires effectively handling ambiguity. Character-level grammars have a large number of states because a literal splits into a state for each character. Most character-level grammars are not LR(k) (i.e., not deterministic) due to lookahead needed for lexical elements - arbitrary amounts of whitespace must be consumed to identify the next rule. Also, lexical disambiguation rules such as longest match are not context-free. The most natural way to formulate them is as ad-hoc disambiguation filters that filters potential parse trees in post-processing, as in :cite:`visserSyntaxDefinitionLanguage1997` 4.4.1. Of course, for efficiency, such disambiguation should be "local" and "incremental" (4.4.5-4.4.6) and done on the fly. Fortunately, :cite:`economopoulosFasterScannerlessGLR2009` says scannerless GLR parsers with post-processed disambugation are fast enough in practice that they can be used for real programming language tasks.

Overall, scannerless seems to be a tradeoff of correct and simple to use but tricky to implement fast. That could be said for programming languages in general: easy to use but hard to write. Given that we are writing the ultimate programming language, it seems worth the effort to also write the ultimate parser.

Parser combinators
==================

Per `this <http://www.semdesigns.com/products/DMS/DMSParsers.html?Home=DMSLexers>`__, Ira Baxter says he did EBNF for 30 years, then he "got smart" and switched to BNF, and found he didn't miss the EBNFisms at all. But from a functional programming perspective, it is kind of restrictive that we can't abstract out common BNF patterns; everything must be expressed as an explicit, named rule. IMO the extra complexity is worth it if we can add a few definitions and halve the size of our grammar. But with this line of reasoning, even EBNF isn't sufficient, because it can't define new patterns. Parser combinators allow arbitrary higher-order abstractions, using FP to abstract patterns. And they allow "nameless" grammar expressions, useful when writing simple regex-like parsers for scripting and the like.

Per :cite:`mightParsingDerivativesFunctional2011`, a parser combinator consumes a string and produces the set of all (partial) parses of the string, where a partial parse is a parse tree and the remaining unconsumed input. A "full" parse consumes all of the input, meaning a parse where the remaining unconsumed input is empty - we obtain the traditional parse set by considering only the full parsers. But this only describes first-order parser combinators. Operationally, parser combinators are higher-order, and are whatever is defined as such in papers and libraries. I went and found as many combinators as I could, then classified them into primitive and derived combinators. So, the primitive parser combinators:

* ``failure``, ``mzero``, ``empty``, ``parseError (msg : ParseError)``, ``fail (msg : String)``, ``unexpected (msg : String)`` - unsuccessful parse (empty set of parses). always fails with the given error message msg, at the current location. ``failure`` uses a fixed uninformative message.
* ``unit = return ()`` - no-op parser, matches empty string. consumes no input and always succeeds.
* ``anyToken``, ``anyByte``, ``item`` - consumes the next token, fails at eof. Although seemingly simple, it has non-trivial behavior, like suspending the parser on incomplete input and tracking line/column numbers.
* ``p <|> q``, ``mplus p q``, ``alt p q`` - Choice (disjunction), specifically unbiased (nondeterministic) choice - both p and q are tried at the same time.
* ``p <> q``, ``seq p q``, ``p >>= q``, ``p <* q``, ``p <*> q``, ``p *> q`` - Concatenation. parses p followed by q
* ``not p`` - (absolute) set complement of p, succeeds iff p would fail. For example ``[^abc]`` is ``not (oneOf "abc") <&&> anyChar``
* ``try`` / ``attempt`` - This is a primitive in backtracking parsers, but in a nondeterministic parser, it is simply a no-op: all combinators behave as though they are wrapped in ``try``, and failure with consumed input is not observable.
* ``lookAhead p`` - parses p. If p succeeds, consumes no input and succeeds. If p fails, so does lookAhead. It is possible to rewrite lookAhead into intersection, via ``lookAhead a <> b = (a <> many any) <&&> b``, but it is a global CPS-like transformation, whereas the reverse ``x <&&> y = lookAhead x <> y`` is direct, so ``lookAhead`` is a more useful choice of primitive.
* ``disambiguateFilter (test : ParseForest -> ParseTree -> Bool) p`` - disambiguation filter (:cite:`visserSyntaxDefinitionLanguage1997` 4.4.1). First ``p`` is run, identifying all partial parses. These partial parses are grouped into parse forests by amount of input consumed. A filter ``test`` identifies, for each parse tree within each parse forest, whether that parse is "correct" or "wrong". The wrong parses are removed from the forest and only the correct parses are further considered. Not commonly used in this form, but this formulation is the most powerful.
* recursion - in our notation it's built into the ambient language, but formally we should use a construct like ``let a = a <|> b in a``, where the bound appearances of the identifier ``a`` are pseudo-combinators. It is also possible to use a graph with cycles, or just unroll it to an infinite tree, but the ``let`` is more human-readable. There is also parameterized parsing, e.g. ``flip count p`` is technically an infinite family of productions indexed by integers, which needs some accomodation as well for data-dependent parsing (but is of course very useful).
* ``p <?> (msg : String)``, ``label p msg`` - Capture group / label. behaves as parser p, but labels the output tree / parse error message with msg. Usually the label is the type of production expected (expression, number, end quote, etc.)
* ``hidden p`` - like p, but suppresses any labels in error messages

Then we have derived combinators:

* ``p - q = p <&&> not q``, ``p {reject} q`` - set subtraction, relative complement, reject production. Acts as p but fails if q would succeed. :cite:`brachthauserParsingFirstclassDerivatives` has ``p <|> not (p <> always) <&&> q``. This maps 1-1 to the "prefer literals" implicit lexer rule, via the transform in :cite:`visserSyntaxDefinitionLanguage1997` section 3.6.
* ``p {prefer} q = p <|> (q - p)``, ``q {avoid} p`` - Biased (preferential) choice, similar to that used in PEG's or backtracking parsers. If ``p`` matches, returns only the parses from ``p``. Otherwise, acts as ``q``.
* ``always = not mzero`` - always succeeds, consumes an arbitrary amount of input
* ``x <&&> y = lookAhead x <> y, and x y`` - conjunction / set intersection. must match both x and y, returns parse tree of ``y``.
* ``notFollowedBy p = lookAhead (not p)`` - succeeds iff parser ``p`` fails, consuming no input.
* ``eof = notFollowedBy anyToken`` - succeeds at the end of the input.
* ``more = lookAhead anyToken`` - succeeds if there is more input.
* ``A -/- B = A <> notFollowedBy B`` - follow restriction, consumes ``A`` but does not match if ``B`` can be parsed afterwards. For example ``[a-z]+ -/- [a-z]`` specifies an identifier not followed by another identifier character. :cite:`visserSyntaxDefinitionLanguage1997` section 3.6 explains that, although follow restrictions work to identify the longest match in all practical cases, in contrived cases the follow restriction is too strict and gives no parse even when some longest-match parse exists.
* ``filter f p, satisfy f p, p {with} test`` - semantic predicate / property filter. Restricts parses of ``p`` to those for which ``f`` returns true.
* ``filterMin r p, filterMax r p`` - comparison filters, which selects the maximal/minimal parses according to some strict partial order ``r``.
* ``longestMatch``: A comparison filter. Each parse tree is converted to a token stream (list) by depth-first left-to-right traversal of the parse tree, stopping at lexical rules / literal rules. Then a token stream ``xs`` is a longer match than ``ys`` if  ``map length xs > map length ys``, where ``(>)`` is lexicographical ordering and ``length`` counts the number of matched characters in the subtree.
* ``between open close p = open <> p <> close`` - just a convenience
* ``done p = p <&&> unit`` - nullability parser. Simplifies to ``unit`` if ``p`` is nullable and ``failure`` otherwise.

There are also derived repetition combinators:

* ``many p = return () <|> p *> many p`` - Kleene closure/star. applies the parser p zero or more times.
* ``some p = many1 p = p <|> (p *> many p)`` - Kleene plus. applies the parser p one or more times.
* ``manyTill p end = end <|> (p >*< manyTill p end)``, ``manyUntil`` - applies parser p zero or more times until parser end succeeds.
* ``someUntil p end`` - applies parser p one or more times until parser end succeeds.
* ``count n p = p <> count (n-1) p; count 0 p = unit``, ``exactly n p`` - applies the parser p exactly n times. n must be nonnegative.
* ``manyN n p = count n p <> many p`` - applies the parser p n or more times.
* ``count' m n p`` - applies the parser p between m and n times. 0 <= m < n.
* ``option p``, ``optional p`` - applies p zero or one times
* ``choice ps`` - apply the parsers in the list ps in order, until one of them succeeds.
* ``sepBy p sep`` parses zero or more occurrences of p, separated by sep.
* ``sepBy1 p sep`` parses one or more occurrences of p, separated by sep.
* ``endBy p sep`` parses zero or more occurrences of p, separated and ended by sep.
* ``endBy1 p sep`` parses one or more occurrences of p, separated and ended by sep.
* ``sepEndBy p sep`` parses zero or more occurrences of p, separated and optionally ended by sep.
* ``sepEndBy1 p sep`` parses one or more occurrences of p, separated and optionally ended by sep.
* ``chainl p op = unit <|> chainl1 p op`` parses zero or more occurrences of p, separated by op. (left-associative)
* ``chainl1 p op = p <|> ((chainl1 p op) <> op <> p)`` - parses one or more occurrences of p, separated by op (left-associative).
* ``chainr p op = unit <|> chainr1 p op`` parses zero or more occurrences of p, separated by op. (right-associative)
* ``chainr1 p op = p <|> (p <> op <> (chainr1 p op))`` - parses one or more occurrences of p, separated by op (right-associative).

Character/byte/token combinators:

* ``space = oneOf "\t "``, ``space_unicode``, ``spaces = many space``, ``space1 = some space``, ``tab``, ``newline``, ``crlf``, ``endOfLine = newline <|> crlf``, ``whitspace = space <|> oneOf "\n\r\f\u000B"``, ``upper``, ``lower``, ``alphaNum``, ``letter``, ``letter_iso8859_15``, ``digit``, ``hexDigit``, ``octDigit``, ``control``, ``comma``, ``colon``, ``dot``- obvious (unqualified definitions based on ASCII)
* ``anyUTF8Char = b1 <|> b2 <> bx <|> b3 <> (count 2 bx) <|> b4 <> (count 3 bx) where b1 = [\x00-\x7F]; b2 = [\xC2-\xDF]; b3 = [\xE0-\xEF]; b4 = [\xF0-\xF4]; bx = [\x80-\xBF]`` - byte-based UTF8 parsing
* ``charCategory cat``, ``combining``, ``numeric``, ``punctuation``, ``symbol``, ``ascii``, ``latin1`` - parses a Unicode character in the given Unicode general category.
* ``semiSep``, ``semiSep1``, ``commaSep``, ``commaSep1`` - sepBy/sepBy1 with the given separator
* ``oneOf cs``, ``inClass cs`` - succeeds if the current character is in the supplied list/class/range of characters cs.
* ``noneOf cs``, ``notInClass cs`` - dual of oneOf, succeeds if the current character is not in the supplied list/class/range of characters cs.
* ``satisfyC f``- succeeds for any character for which the supplied function f returns True.
* ``string s = filter (== s) (count (length s) anyToken)`` - matches a sequence of characters identical to s.
* ``char c = string [c]``, ``single s`` - parses a single character c / token s.
* ``notChar c = anyChar <&&> not (char c)`` - parses any single character besides c.
* ``take n = count n anyChar`` - consumes exactly n characters of input
* ``takeWhile p = many (satisfy p)`` - matches input as long as the predicate ``p`` is true. Always succeeds, at worst it will simply match nothing.
* ``takeTill p`` - matches input as long as the predicate ``p`` is false. Always succeeds, at worst it will simply match nothing.
* ``takeWhile1 p = some (satisfy p)`` - matches input as long as the predicate ``p`` is true. Fails if no input is consumed.
* ``takeWhileIncluding p = many (satisfy p) >*< anyChar`` - matches input as long as the predicate ``p`` is true, and the following character. Fails if no input is consumed.
* ``scan s_0 p`` - stateful version of ``takeWhile``. As long as ``p s_i c_i`` return ``Just s_(i+1)``, the parser will continue matching input.
* ``identifier``, ``reserved``, ``operator``, ``reservedOperator`` - a legal identifier is one of the form ``start letter*`` that does not match a reserved word. Similarly operators are ``opStart opLetter*`` and also some operators are reserved.
* ``charLiteral``, ``stringLiteral``, ``natural``, ``integer``, ``rational``, ``float``, ``naturalOrFloat``, ``decimal``, ``hexadecimal``, ``octal`` - parses as in the Haskell report
* ``buildExpressionParser table term`` builds a (mixfix) expression parser. The expressions use ``term`` as the lowest building block of an expression, commonly an (atomic) identifier or a parenthesised expression. The table is a lists of lists; the outer list is ordered in ascending precedence (least tight to most tight), while all operators in one inner list have the same precedence. Each operator is specified as a list of productions and holes, and also has an associativity (none, left, or right - "both" or "assoc" is ambiguous and parsed as left in a "don't-care" manner), taken into account when an identifier starts or ends with holes. :cite:`visserSyntaxDefinitionLanguage1997` defines priority and associativity for SDF using disambiguation filters, essentially priority specifies that a use of ``E_i`` in a production ``E_j`` must have ``i>j``. The associativity filters out trees with the same production in the first/last position, like ``func = (E - func) "→" E``. Definition 3.4.1 defines it formally, these are disallowed:

  * a parse ``A = ... B ...`` where ``B`` is a direct child and has lower precedence than ``A``
  * a parse ``A = B ... A ...`` where ``B`` is right-associative or non-associative w.r.t. A
  * a parse ``A = ... A ... B`` where ``B`` is left-associative or non-associative w.r.t. A

  The precedence rule should be clear. For the associativity, consider a binary operator ``_+_`` left-associative w.r.t. ``Term``. Parsing ``(1+(2+3))`` will give a tree like ``Add Term (Add Term Term)`` - ``Term`` appears before ``Add``.

* ``whiteSpace`` - zero or more occurrences of a space, a line comment or a block (multi line) comment. Block comments may be nested. The only point where the whiteSpace parser should be called explicitly is the start of the main parser in order to skip any leading white space.
* ``lexeme p = p >*< whiteSpace``
* ``symbol s = lexeme (string s)``
* ``parens p, braces p, angles p, brackets p`` - respectively ``(p), {p}, <p>, [p]``.

"Selective" combinators  (Mokhov et al. 2019) decide which branch to take based on the result of another parser, somewhere between monads and applicatives. For example ``branch either left right`` parses ``either``, then, if successful and ``Left`` is returned, tries ``left`, otherwise, if ``Right`` is produced, the parser ``right`` is executed. This can be mimicked without the dependent behavior by narrowing the productions, ``eitherL left <|> eitherR right`` where ``eitherL`` is the language of ``either`` that returns ``Left`` and similarly for ``eitherR``. I don't really like having to compute the set of all strings for which a function returns a given value, so it seems good to avoid this. But maybe it can be implemented easily.

Per :cite:`brachthauserParsingFirstclassDerivatives` it is worth exposing the derivative function as a parser combinator ``feed p c = p << c``. It's not clear though if this functionality is useful without being able to do monadic bind and write something like ``char >>= \c -> feed p c``.

Layout: per :cite:`erdwegLayoutsensitiveGeneralizedParsing2013`, can be implemented with "layout constraints", specialized semantic predicates. The constraints examine the starting/ending line and column of the first/last/leftmost of middle lines/rightmost of middle lines characters of each direct sub-tree of the parse. Then they can express boolean formulas of comparison constraints (equal, less than, greater than), e.g. the offside rule is ``1.first.startCol < 1.left.startCol``. :cite:`adamsPrincipledParsingIndentationsensitive2013` says it can be done in a more principled manner by annotating each production with its column and using constraints that the sub-production must be at column 0 or must be equal, greater than, or greater than or each to to the column of the start of of the production. :cite:`amorimDeclarativeSpecificationIndentation2018` specifies some higher-level constaints like ``align`` that can be used for both parsing (translating to column-based layout constraints) and for pretty-printing, and gives the full algorithm for incrementally constructing parse trees with column information.

Type
----

Per :cite:`mightParsingDerivativesFunctional2011`, the nominal type of a parser combinator is :math:`A^* \to P(T \times A^*)`, where ``T`` is the type of parse trees and ``A`` the type of tokens. Similarly :cite:`swierstraCombinatorParsingShort2009` uses the Haskell type ``Parser s t = [s] -> [(t,[s])]``. Let's compare this simple nominal type with the definitions in actual libraries, namely `parsec <https://hackage.haskell.org/package/parsec-3.1.16.1/docs/Text-Parsec-Prim.html>`__, `Trifecta <https://hackage.haskell.org/package/trifecta-2.1.2/docs/Text-Trifecta-Parser.html>`__, `Attoparsec <https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-Internal-Types.html#t:Parser>`__, and `Megaparsec <https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec-Internal.html#t:ParsecT>`__. These are collected in Parser.hs. First note that most definitions (implicitly) use ``CodensityT m a = forall b. (a -> m b) -> m b``, because ``(a -> r) -> (b -> r) -> r = (Either a b -> r) -> r``. The codensity monad's sole purpose is to right-associate the monad bind to increase performance, so we can simplify the type by removing it, replacing ``CodensityT m a`` with ``m a``. Also, some parser types act as monad transformers; this is not really relevant either so we can assume pure parsing ``m=Identity``. The type of the parse result is a parameter; it simplifies things to just assume the parser builds up the AST as a fixed type ``ParseTree``. Similarly we can standardize ``[Byte]`` as the input type, at least while we're designing.  Regarding trifecta's rope, kmett `says <https://github.com/ekmett/trifecta/issues/49#issuecomment-322073854>`__ he's exploring removing the rope machinery entirely, so it can be simplified to ``[Byte]`` as well. Considering parsec's ``State`` type, it seems `u = ()`` in almost all cases, but maintaining a separate ``State`` type as an extension point is reasonable, so we replace ``([Byte],Pos,...)`` with ``State`` in the other parsers. At this point, all the parsers are of the form ``Parser = State -> ... | Ok ParseTree State``, differing only in the handling of errors and incomplete input. So yes, the nominal type is pretty close to actual behavior. The main differences are tracking position in the ``State`` type,  and also that these libraries use PEG-style backtracking, hence only return a single parse tree and have to handle errors specially, whereas derivatives and other non-deterministic parsers return a set of parses, modelling multiple or zero parses more naturally.

Output
======

The main output of the parser is an AST. DMS extols automatic AST construction - the grammar is the documentation for the AST, and it allows rapid development of complex grammars. I tend to agree; parsec's profusion of tuple-returning concatenation operators shows that people want to be lazy and work with an untyped tree. It's just Haskell's distaste for heterogeneous lists that forces an ADT, and the lack of any standard parse result type that leads to the typed AST result parameter. DMS can apparently drop nodes and contract unary nodes, but I think this goes against the spirit of automation. The result should be completely automatic, with no annotations allowed - any further efforts should be post-processing. This ensures a uniform representation of the parse tree, and enables reformatting.

The use of combinators instead of BNF does complicate the definition of AST a bit. We need some concept that merges callstacks with AST trees.

Per :cite:`tomitaEfficientParsingNatural1986` section 2.4 pages 17-20, it is desirable to produce all possible parses and disambiguate them afterwards. However, the number of parses of a grammar ``A = A A | anyChar`` grows as the Catalan numbers, which tends to :math:`O(4^n / n^{3/2})`, basically exponential, and cyclic grammars may have an infinite number of parses. A compact representation is needed. Tomita describes a "shared packed parse forest". Sharing deduplicates identical sub-trees - each node is identified by a pointer and the sub-tree relation is represented by a points-to, so that a node may have multiple parents. Packing localizes ambiguity - it creates "packed nodes" that represent a certain non-terminal symbol and parse span, with each subnode of the packed node representing a different parse. However, per :cite:`johnsonComputationalComplexityGLR1991`, a grammar like ``S = a | S S | S^{m+2}`` and string of "a"s of length :math:`n` requires constructing at least :math:`O(n^m)` nodes, due to having to represent all the positions. The solution per :cite:`billotStructureSharedForests1989` is binarization: splitting a node with n child trees into a right-biased sequence of nodes where each node has two children. More specifically, binarization converts a production ``S = A B C`` to productions ``S1 = A S2; S2 = B C``, where the new non-terminals are unique. Then one obtains cubic parsing :math:`O(n^3 G)` where :math:`n` is the length and :math:`G`` is the size of the (normalized) grammar. :cite:`scottSPPFStyleParsingEarley2008` and the earlier BRNGLR paper by the same author presents specific worst case cubic order parsers using modified SPPFs.

Another potential representation is as the shift/reduce stream of an LR parser, it's not clear though how to represent nondeterminism.

For an analysis of tree sharing into forests and the effect on parsing complexity, you may want to read "Observations on Context Free Parsing" by Beau Sheil In Statistical Methods in Linguistics, 1976:71-109. The point is that all general CF parsing algorithms walk this shared forest completely. And it has size O(n3)

Some later paper uses something called "BSR", have to look it up again.

It is also important to have some kind of demand-driven / streaming interface. Parsing a whole file at a time can lead to OOM. There is the coroutine pattern where the semantic analysis calls down to the parser when it needs more input, but this has too many context switches. It is possible to use buffers of e.g. 1024 characters but this requires handling incomplete input. Attoparsec handle it as returning a continuation, but we'd like more than a just an opaque function, like a partial parse tree. For compilers, Per Vognsen says using declarations as the chunk granularity is best - it's a natural boundary, since usually a decl is the largest syntactic unit in a file. Even if your language requires unbounded token lookahead, it would be really weird to require parsing past the end of a decl into the next decl. But even so a decl is typically not too large so you don't have to worry about OOM or buffer refills or anything like that. Although for very large decls (like a generated array) you probably still need a incomplete-input fallback, that's a very cold path and some awkward cache misses are fine.

Another idea Per Vognsen brings up is that, for indexing, the full file-level AST is not needed. Only some relevant subsection needs to be parsed, and the rest just needs start/end of each declaration but no interior detail. He proposes "random access" to the AST, building a dictionary of name -> (src span, decl kind). But his performance hacks assume all legal top-level declarations must have a keyword in column 0, which isn't necessarily true


For purposes of the language server, we want several properties of the syntax tree:

* holds all the source information in full fidelity - can reconstruct source text
* best-guess parse tree if the program is incomplete or malformed, representing skipped or missing tokens in the syntax tree.
* immutable and thread-safe, persistent data structure
* nodes: grammatical construct (declarations, statements, clauses, and expressions), lexical token (keywords, identifiers, literals, and punctuation), trivia (whitespace, comments, and preprocessor directives)
  * structure: constructs contain constructs and tokens (never leaf node). tokens are always leaves. trivia is stored as leading/trailing attribute of tokens.
* stores source position spans (beginning/end position). can be bytes, codepoints, line/column. The convention  is that zero-length is between two characters.

Compilation
===========

If we compile a scannerless parser, we should be able to get the regular portion of the grammar to be a finite  state automaton, and the context-free to use at most memory proportional to the maximum expression depth. There is also some amount of state explosion in the conversion from nondeterministic to deterministic, so the compiled code may be large and we can trade-off compiled states and runtime memory usage. But many standard techniques of optimizing programs apply, so getting scannerless parsers to have performance competitive with hand-rolled recursive descent parsers is a possibility. Comparing with conventional two-level parsers is possible as well but is not really fair since two-level parsers are not very expressive grammar-wise and are generally table-based rather than directly generating machine code. Looking at :cite:`wankadiaRedgrepRegularExpression2013` which does LLVM compilation and parses directly from UTF-8 encoded bytes, we should expect about a 3x speedup for properly compiling the grammar, vs. using a decent table-based implementation (re2 vs redgrep, second non-trivial regex example).

The EBNF formalisms complicate building an AST automatically, and can be encoded in straight BNF.

Algorithm
=========

PEG is popular, but uses backtracking, which is not efficient enough on highly ambiguous grammars. The original Packrat paper proposed to use memoization to get linear time, but this uses memory proportional to the size of the input file, so is not really a good option compared to other methods. There is also the issue that PEG doesn't natively support left recursion, but per some paper's trick, this can be worked around in a parse-preserving manner by splitting each production into definitely-null, possibly-null, and not-null productions, e.g. for ``A = A B | C``::

  A = Anull B | Anotnull B | C
  Anull = <empty>
  Anotnull = Anull Bnotnull | Cnotnull

This does collapse trivial parses, but they are after all trivial and the possible parses of the empty string can simply be grafted onto the parse forest after-the-fact. But it makes life even simpler if trivial parses have trivial parse trees, since then there isn't even a grafting step.

There are many algorithms: GLR, GLL, CYK, Earley, and derivatives. We want a single algorithm that combines the advantages of all. Derivatives are the newest, so that's the place to start.

Also error recovery. Treesitter implements incremental LR parsing with error recovery.

`Yakker <https://github.com/attresearch/yakker>`__ is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

to have a disambiguating pass on the set of parse tree generated by a nondeterministic automaton. The alternatives involve restricting parsers to be deterministic, for example PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

Revisiting this, the goal is to use partial evaluation to generate the parser, by speeding up a naive brute-force algorithm applied to the grammar. There is already a paper on LR parsing by partial evaluation :cite:`sperberGenerationLRParsers2000` and also on specializing Earley, so with sufficiently powerful compiler optimization handling general grammars should be possible.

In particular the parser should be written as a nondeterministic finite state transducer that builds up trees (outputs a list in the style of start-children-end or S-expressions or something).

Formally:

* Q is a finite set, the set of states;
* I is a subset of Q, the set of initial states;
* F is a subset of Q, the set of final states; and
* Σ is a finite set, called the input alphabet;
* Γ is a finite set, called the output alphabet;
* The transition function is of type :math:`Q \times (\Sigma \cup \{\epsilon \})\to P(Q \times (\Gamma \cup \{\epsilon \}))`, where ε is the empty string and P(Q) denotes the power set of Q.

There are various problems. Their complexity:

Membership problem: Given a grammar G and string w, is w ∈ L(G)? - decidable for recursive grammars (includes context sensitive). Cubic time (actually matrix-mult time) for context-free grammars, by Earley or CYK.

Nullability: Given a grammar G, is ε ∈ L(G)? Decidable for context-free grammars, by normalizing (Chomsky normal form and removing useless productions) and inspecting the result (determining nullability of each rule)

Emptiness problem: is the language empty, i.e. given a grammar G, is L(G) = ∅? - solveable similar to nullability
Finiteness problem: is the language L(G) finite? - again solveable similar to nullability

Completeness problem: Does a grammar G match every string, i.e. L(G) = Σ*? - decidable for deterministic context-free gramars (LR(k) parseable)

Regularity problem: Does a grammar G describe a regular language, i.e. L(G) = L(R) for some regular grammar R? - decidable for deterministic context-free gramars (LR(k) parseable)

Equality problem: Given grammars G1, G2, is L(G1) = L(G2)? - decidable for deterministic context-free gramars.

Minimizing problem: Find smallest grammar G' with L(G) = L(G'). - decidable for deterministic context-free gramars.

Subset problem: Is L1 subset of L2? - decidable for regular languages

Overlap problem: Is L1 intersection of L2 = null? - decidable for regular languages

Complement: closed for recursive languages but not recurisvely enumerable languages

Intersection: closed for recursively enumerable languages

it's easier and faster to match in a byte oriented way than to decode utf-8 in a preprocessing step. It works because there is only one representation of each character as a UTF-8 byte sequence.

Normalizing/compacting grammars is important for equality comparison and efficiency

.. code-block:: none

  (r∗)∗ ≈ r∗
  ∅∗ ≈ ε
  ε∗ ≈ ε
  \C∗ ≈ ¬∅
  (r · s) · t ≈ r · (s · t)
  ∅ · r ≈ ∅
  r · ∅ ≈ ∅
  ε · r ≈ r
  r · ε ≈ r
  ¬(¬r) ≈ r
  ∅ & r ≈ ∅
  r & ∅ ≈ ∅
  (r & s) & t ≈ r & (s & t)
  r & s ≈ s & r
  r & r ≈ r
  ¬∅ & r ≈ r
  r & ¬∅ ≈ r
  ¬∅ + r ≈ ¬∅
  r + ¬∅ ≈ ¬∅
  (r + s) + t ≈ r + (s + t)
  r + s ≈ s + r
  r + r ≈ r
  ∅ + r ≈ r
  r + ∅ ≈ r

A nullable expression is one that matches the empty string. Nullability is important to know, as the derivative of a concatenation (defined next) depends on whether the first expression is nullable. Recursion is handled via the least fixed point of the equations (e.g., ``L = L & L`` is not nullable).

.. code-block:: none

  ν(∅) = F
  ν(ε) = T
  ν(\C) = F
  ν('a') = F
  ν(S) = F
  ν(r∗) = T
  ν(r · s) = ν(r) && ν(s)
  ν(¬r) = not ν(r)
  ν(r & s) = ν(r) && ν(s)
  ν(r + s) = ν(r) || ν(s)

The derivative of an grammar expression E with respect to a character (or set of strings) C is a grammar expression d_C E such that its language is { s : exists c in C. c s in L(E) }. I.e., you take the strings in L(E) that begin with C, and then you chop off the C. For example the derivative of ``ab|ac|de`` w.r.t. ``a`` is ``b|c``. Some derivatives are as follows:

.. code-block:: none

  ∂a ∅ = ∅
  ∂a ε = ∅
  ∂a \C = ε
  ∂a a = ε
  ∂a b = ∅ for b ≠ a
  ∂a S = ε if a ∈ S
         ∅ if a ∉ S
  ∂a (r∗) = ∂ar · r∗
  ∂a (r · s) = ∂ar · s + (ν(r) ? ε : ∅) · ∂as
  ∂a (¬r) = ¬(∂ar)
  ∂a (r & s) = ∂ar & ∂as
  ∂a (r + s) = ∂ar + ∂as

With this we can already implement an interpreter-style recognizer, by computing the derivative on the fly. The loop is read next char, compute derivative, normalize, repeat. Then at EOF the input string matched if the final grammar expression is nullable.

To compile a derivative parser to a DFA, we do a traversal of the state graph of grammar expressions, e.g. depth-first. Starting at the original expression ``E``, we compute successive derivatives with respect to all possible characters, normalize the resulting expressions, and minimize the resulting DFA state graph by interning equivalent grammar expressions. The nullable expressions are accepting states. The textbook approach to compiling regular expressions constructs an NFA, constructs the DFA from that, and then minimizes the DFA. But derivative parsing allows you to avoid the NFA entirely, and produces a result much closer to the minimal DFA right off the bat, saving a lot of work.

An important speedup of minimization is identifying partitions of state transitions w.r.t. byte values. Basically, rather than computing the derivatives w.r.t. 0, 1, 2, up to 255 individually and checking for equality afterwards, you can determine from the structure of the expression that it can transition to up to n other states and that each of some set of byte values will transition to a given state. This can be represented by n bitsets of length 256 for n possible next states, with the AND of any two bitsets 0 and the OR of all of them the bitset of all 1's (basically redgrep's representation, although it specifically inverts the first one to model it as a "default" case), or as a packed array-table with ceil(log_2(n)) bits for each byte value, or maybe with ranges if the states are generally clustered in contiguous ranges. The rules for partitions are as follows:

.. code-block:: none

  C(∅) = {Σ}
  C(ε) = {Σ}
  C(\C) = {Σ}
  C(a) = {Σ \ a, a}
  C(S) = {Σ \ S, S}
  C(r∗) = C(r)
  C(r · s) = C(r) ∧ C(s) if ν(r) = ε
             C(r)        if ν(r) = ∅
  C(¬r) = C(r)
  C(r & s) = C(r) ∧ C(s)
  C(r + s) = C(r) ∧ C(s)

With the DFA in hand, we can implement a table-based recognizer: just read the character, look up the state, and at EOF check if the state's corresponding grammar is nullable. But LLVM is more interesting. The overall function takes a pointer and length, and jumps to state 0's initial block. Then, for each DFA state, we create two basic blocks. The first (entry) one checks if we've hit the end of the string, and branches to return true or return false depending on whether the state was accepting. Otherwise it branches to the second basic block. The second basic block increments the pointer, decrements the length, and then enters a C-style switch on the current byte. Each case is simply a jump to the basic block corresponding to the next state. We need two main optimizations, register allocation and loop removal. Also LLVM optimizes the switch.

An important speed-up is vectorization - examine a glob of memory at once, use vector operations to compare all of them at once, and then reduce the result. Byte-at-a-time is 1.1 GB/s, vectorized is 12 GB/s and memory-constrained. For example look at memchr from libc.

Going from a recognizer to a parser, we must produce a parse tree. So rather than looking at the language (set of strings), we consider the set of pairs ``(string,tree)``.

Valentin Antimirov of came up with Antimirov partial derivatives these are used to construct an NFA and the only real difference between them is that when you have a disjunction in an in your DFA you split that into two separate NFA States. French scientists I think came up with some set set of set-based techniques to essentially allow you to bubble up or surface disjunctions from inside conjunctions and complements using again using like de Morgan's laws and distributions on the sets of the conjunctions. using Antimirov partial derivatives, I construct an NFA that I can apply the parentheses as tagged epsilon transitions, then use Laura carry tagged transitions to convert the tagged NFA to a tagged DFA and assuming that works that I can try translating into machine code.

PWD can, in fact, be implemented in cubic time.

PWD involves four recursive functions: nullable?, derive, parse-null, and parse.
The nullable? and derive functions implement δ (L) and D c (L), respectively
the parse-null function extracts the final AST;
and parse implements the outer loop over input tokens.

error isolation so you can just bail on a decl and know the other decls will be independently handled
