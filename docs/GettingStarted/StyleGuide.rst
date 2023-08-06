Style guide
###########

This guide outlines general conventions for Stroscot code. It's an evolving document though so nothing is final. Think of it as the boot camp where the drill sergeant is yelling in your ears to scare you straight, rather than a description of fact.

Naming Guidelines
=================

* Filenames should be meaningful and end in ``.sct``. Use all-lowercase ASCII names, with words separated by underscores or dashes.
* Type names and constructor names should be uppercase CamelCase, like Haskell.
* Module names should be lower_with_under (snake case), like Python.
* Function, constant, and variable names should be snake_case (all lowercase), like Julia, or lowercase camelCase like Haskell, or kebab-case like Lisp or Raku. Generally, variable names should be nouns and function names should be verbs.
* If an argument is unused use the wildcard pattern ``_`` or (less preferred) a name starting with ``_``
* Use whole words unless you have a good reason. Single letters can be okay for internal variable names or to match a reference paper or algorithm but may be unreadable in a year so should be documented with comments. Don't use abbreviations, because they tend to be inconsistent and too terse; use a text editor with autocomplete.
* Multi-word names are allowed, but consider whether the binding might be decomposable into separate values.
* Bang convention: Append ``!`` to names of functions that modify their arguments. Not too sure about this, the ``Ref`` type that marks a value as allowing modification may be enough indication.

Conversion into CamelCase follows the `Google Java styleguide algorithm <https://google.github.io/styleguide/javaguide.html#s5.3-camel-case>`__. First substitute accented characters such as ü -> ue and remove any apostrophes. Split into word-parts on spaces, hyphens, and camel-case (Ad-Words, but not i-OS). Lowercase everything (including acronyms), then uppercase the first letter of each word part. Join together; it is in upper CamelCase. Lowercase the first letter to get lowercase camelCase.

Comment Guidelines
==================

* Use proper English sentences with appropriate punctuation and case.
* TODO to mark todo comments
* XXX to mark comments about currently broken code
* The beginning of the module and every exported binding should have a documentation comment.
* Terminate lists with punctuation or not as randomly as possible
* Comments should explain the “why” not the “what” or “how”.

Spacing Guidelines
==================

Whatever the auto-formatter gives is probably right. Guidelines for the auto-formatter:

* All files should have a newline character at the end of the file. (Git)
* Use spaces around binary operators (e.g. ``x + y`` instead of ``x+y``), unless you need to compact the expression so it fits on one line. Another exception is keyword arguments, which can be squashed to emphasize the atomic nature of the call.
* Use a single space after commas: ``[1, 2, 3]``.
* Do not use extra spaces for unary operators, parentheses, or brackets. Use extra spaces for braces, ``{ x }``.
* Use spaces around ``=``, ``x = y``, but only one pair (no ``x    = y`` or ``x =    y`` to make it horizontally line up with another line)
* Use 3 spaces for indentation; no tabs, ever.
* No hard line limit, but use an editor that soft-wraps lines to 100 characters. This fits comfortably on a modern screen with a reasonably sized font. Wrapping is a good indication that you should encapsulate some of the work in a separate line.
* Use semicolons to put multiple commands on one line only if they fit on one line.
* Function calls, lists, etc. can be spaced out so each element is on its own line, with a single level of indentation:

::

  long_function_name = some_function
    "a long argument"
    "another argument"
    "another long argument"

  long_assignment = // break after assignment
    something +  // break after symbol
    something_else +
    another_thing

  list =
    // Haskell style
    [ elem1
    , elem2
    ]

* Top-level assignments, definitions, and comment lines are written out using "logical groupings" of elements that are related by functionality or purpose. There are several styles for presenting these:

  1. Separate group elements with a single newline, separate groups with two newlines (an empty line)
  2. Separate group elements with two newlines, separate groups with three or more newlines (2+ empty lines)
  3. Separate group elements with 1-2 newlines, and separate groups with banners:

::

    ////////////
    // A
    a1 = 1
    a2 = 2

    ////////////
    // B


  Generally the single newline style should be preferred, unless each group element takes up more than one screen (~50 lines). If a function exceeds about 40 lines, think about whether it can be broken up without harming the structure of the program. The multiple newline style suggests that your code is too complex and should be rewritten. The banner style is good for skimming and may be suitable if you have large sections of code, but splitting into more modules is probably better.

* Numeric literals: Common digit groupings are every 3 decimal digits, every 4 binary digits, and every 2 hex digits, omitting the separator if there are only two groups. Other groupings may be used if appropriate, e.g. a date YYYY_MM_DD, a postal code 12345_7890, a double separator at 8 bytes, or other formats.

Type Guidelines
===============

* Use the largest types possible for type dispatch restrictions. If the restrictions were removed, the function should not work as intended on value outside the type. If you want to document that a generic function works on some "known" types you should not restrict the function at all - instead, leave the domain unrestricted and write separate type signature assertions.
* If you implicitly depend on values, like ``sum = foldl (+) 0``, then write those in the signature, like ``sum : { (+) : a -> a -> a, 0 : a } -> [a] -> a``.
* Avoid converting to a known type ``f (x:Any) = convert Int x`` and instead use a type constraint ``f (x:Int) = x`` or ``f (x:Convertible Int) = x``. It is better to require an explicit conversion, and the type constraint can be statically checked.
* Avoid writing large union types ``A|B|C`` - define a new type ``SomeUnion`` and use the set syntax ``elem SomeUnion A``.
* If an argument or field can be anything, explicitly annotate it with ``: Any``.
* Assertions throw an exception and are enforced by the compiler pretty strongly so can be used for error handling/input checking/etc. It is better to prove the absence of errors than to try to handle them.

Parameters
==========

The standard library adheres to this general order as much as possible when calling functions to give a more consistent function call style:

* Output type: For a return-type-overloaded function such as ``read`` or ``convert``, the output type must be specified. This comes early so that the function can be read as a single operation ``read Float`` or ``convert(To)Float``.

* Arguments overriding defaults: In Stroscot arguments overriding defaults are usually listed first, because they have to be syntactically part of the function call. For example the I/O stream is an argument that defaults to stdout, so one would write ``print {stream=stderr} "Hello error!"``.

* Positional parameters should be the "primary data" that is operated on. There should be 3 or fewer positional parameters, otherwise positional-based calls become a big ball of mud. It's OK to have no positional parameters. Examples, in the order they should appear, include:

  * Input list, array, reference, etc.
  * Key or index
  * Main value - If there is a main "data" positional parameter, it should be last, to support function composition pipelines.
  * Varargs, which must be the last positional parameters in a function call.

* Keyword arguments should ideally have a default, hence be listed first, but otherwise they can be positioned pretty much anywhere. Keyword-only parameters should define details or options of the computation.

* Overload functions only if the variants have the same semantic behavior, i.e. can be understood without knowing which variant was chosen. You should be able to document all variants with a single comment.

As far as naming:

* The parameters should be labeled descriptively according to their nature, role, or both. The role is to be preferred, since an accompanying type signature will often show the nature.

  * A sole positional parameter will probably have a nature/type label such as "filename", "buffer", etc. since the function name makes its role clear
  * If there are two or more positional parameters, extra care should be put into finding distinct roles, for example addition takes the "augend" and "addend".
  * If the parameters are elements with the same nature and role, then they can be numbered, for example ``a1, a2``, although varargs or a list argument may be more appropriate.

Common labels:

* f - a function to be applied
* position - a position in a string, array or byte sequence
* length - a length
* buffer - a byte sequence or string used as buffer
* source - the source of an operation
* destination - the destination of an operation
* initial - the initial value for an iterator
* compare - a comparison function
* mode - an operation mode or a flag list

Ideally, the function name, labels, and signature(s) should be enough to convey the function’s meaning and usage, because this information is easily available with the "all defs" page in the documentation index or at the REPL with ``:browse``, whereas reading the full function documentation is more involved.

Scoping
=======

* Generally naming functions is preferable as anonymous functions are implicitly converted to named functions anyways.
* For clarity, imports should bring only the module into scope, rather than its members, and one should write ``module.function`` every time. But this can get verbose so if it's a common module with recognizable function names you can use member imports. Of course some modules such as CSV are intended to be used qualified and use vague names such as ``CSV.read`` so should never have their members imported directly.
* A module should export all bindings that are part of the module's intended API. Non-exported bindings are typically internal and subject to change, unless the documentations states otherwise.
* Put code in a function instead of the top-level, to allow re-using it and testing it more easily
* Place code in a namespace, except if it is the main module of a throwaway script
* The main application or library module should be in in the root or ``src/`` directory.
* The imports should be in three groups, separated by a blank line: standard library, third-party libraries, project modules. Within each group the modules should be listed alphabetically.

Structuring
===========

* Only create an intermediate variable if it has a meaningful name. But if an expression has a meaningful name, then put it in a variable.
* Refactor a case list with 15 or more cases to use a table or loop. `10 percentage cases <https://github.com/MinBZK/woo-besluit-broncode-digid-app/blob/master/Source/DigiD.iOS/Services/NFCService.cs#L182>`__ is pushing it but probably OK, but 100 Fizzbuzz cases written out is definitely a maintenance burden.

Sources
=======

* `Exploring ReasonML <http://reasonmlhub.com/exploring-reasonml/ch_functions.html>`__
* `Yet Another Style Guide For Julia <https://github.com/jrevels/YASGuide#other-syntax-guidelines>`__
* `Julia Style Guide <https://docs.julialang.org/en/v1/manual/style-guide/>`__ (v1.8.2)
* `JuMP Style Guide <https://jump.dev/JuMP.jl/dev/developers/style/>`__
* `Google Style Guides <https://github.com/google/styleguide/tree/0b003a9ae1de0bcacdf3232004bcc35df00faa51>`__
* `Wikipedia Digit Grouping <https://en.wikipedia.org/wiki/Decimal_separator#Digit_grouping>`__
