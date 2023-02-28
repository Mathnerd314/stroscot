Syntax
######

Unicode
=======

The decoding algorithm is as follows: Start with bytes. Decode using UTF-8, replacing invalid bytes/characters with Unicode's REPLACEMENT CHARACTER U+FFFD. `NFC <http://unicode.org/reports/tr15/#Norm_Forms>`__ normalize the input, warning if input isn't already normalized. Then the following algorithms in order to do lexical analysis:

* `line-breaking <https://www.unicode.org/reports/tr14/#BreakingRules>`__ (specifically, lines end at hard / mandatory breaks)
* `word-breaking <http://www.unicode.org/reports/tr29/#Word_Boundary_Rules>`__ to split up lines into tokens
* `extended grapheme clustering <https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries>`__ to split up tokens into characters

Stroscot is case-sensitive, i.e. it does not do any case transformations on identifiers and it compares graphemes literally, but the grammar also does not make any syntactic distinctions based on case.

