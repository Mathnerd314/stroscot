Implementation
##############

Steelman 1F. "The language shall be composed from features that [...] can be implemented. [...] To the extent that it does not interfere with other requirements, the language shall facilitate the production of translators that are easy to implement and are efficient during translation."

13C. Translators shall implement the standard definition. Every translator shall be able to process any syntactically correct program. Every feature that is available to the user shall be defined in the standard, in an accessible library, or in the source program.

13E. Translators for the language will be written in the language and will be able to produce code for a variety of object machines. The machine independent parts of translators should be separate from code generators. Although it is desirable, translators need not be able to execute on every object machine. The internal characteristics of the translator (i.e., the translation method) shall not be specified by the language definition or standards.

13F. Translators shall fail to translate otherwise correct programs only when the program requires more resources during translation than are available on the host machine or when the program calls for resources that are unavailable in the specified object system configuration. Neither the language nor its translators shall impose arbitrary restrictions on language features. For example, they shall not impose restrictions on the number of array dimensions, on the number of identifiers, on the length of identifiers, or on the number of nested parentheses levels.

Language
========

A near-term goal is to write Stroscot in itself. However, it has to generate code first. I originally picked JavaScript to start for a number of reasons:

* It's the fastest interpreted language available
* It has reasonably up-to-date syntax and features thanks to TC39
* A lot of the inspiring projects were written in JS (Wat, macro lambda calculus)
* LLVM compiles to JS and there are LLVM bindings available for JS
* TypeScript doesn't add much besides compilation overhead

Since then, development has shifted to Haskell, for other reasons:

* The compiler/type system prevents a lot of common errors (particularly typos, which JS doesn't detect until late)
* A lot of other compiler-theory-heavy projects are written in Haskell or similar functional languages
* I'm most familiar with Haskell.

