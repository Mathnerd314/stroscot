Modules
#######

Developers can add new language features and functionality through libraries and modules.

3-5A. It shall be possible to encapsulate definitions. An encapsulation may contain declarations of anything (including the data elements and operations comprising a type) that is definable in programs. The language shall permit multiple explicit instantiations of an encapsulation.

3-5B. An encapsulation may be used to inhibit external access to implementation properties of the definition. In particular, it shall be possible to prevent external reference to any declaration within the encapsulation including automatically defined operations such as type conversions and equality. Definitions that are made within an encapsulation and are externally accessible may be renamed before use outside the encapsulation.

3-5C. Variables declared within an encapsulation, but not within a function, procedure, or process of the encapsulation, shall remain allocated and retain their values throughout the scope in which the encapsulation is instantiated.

Terminology
===========

(based on Carbon)

* a *name path* is a dot-separated list of identifiers or symbols. For example, ``stdlib.getTime`` is a name path and ``stdlib`` and ``getTime`` are identifiers. A bare identifier ``getTime`` is a trivial name path; generally name paths can be shortened to bare identifiers using imports.
* an *entity* is a value, such as a function or constant. It is generally referred to by an associated name path. For example, in ``stdlib.getTime stdlib.clocks.SystemClock``, ``stdlib.getTime`` is resolved to an entity which is an I/O action and ``stdlib.clocks.SystemClock`` is resolved to an entity which is a symbol. Multiple name paths may refer to the same entity.
* *libraries* are considered an output format in Stroscot. Similarly to building an executable, you can build a shared or static library. The nomenclature of "library" is not otherwise used in programming Stroscot as it is ambiguous.
* a *module* or namespace is a convenience grouping of entities, more formally a map from names to entities. Modules may be nested, recursively/cyclically, and an entity may appear in multiple modules. The dot-notation and name path syntax is closely associated with accessing a module's entities.
* a *package* is a group of files, and is the standard unit for distribution (i.e., a package is typically downloaded as a zip file or such). A package may contain many modules or none at all (providing only top-level entities). Generally however a package is imported and encapsulated as a module.

Module
======

Modules are reusable components containing code that are themselves first-class objects, replacing more simplistic ways of namespacing and encapsulating units of code such as structs of methods. Modules allow code reuse and abstraction. For example, one can create an "abstract data type" by defining a type in a module ``set C = { Csym a b c | a : Int, b : Float, c : Double }`` and not exporting its underlying constructor symbol ``Csym``. This is not really the mathematical notion of an ADT, where one takes the "free object" defined by the operations and constraints, but since Stroscot supports term rewriting it is possible to define the mathematical model as a series of rewrite rules and then export the symbols, fulfilling the concept of ADT in both theory and practice.

Stroscot's module system is based on F#, OCaml, Standard ML, Agda, 1ML, MixML, Backpack, and Newspeak's module systems. (TODO: reread the systems to see if I missed anything. But they're mostly type systems, rather than functionality)

A module is a set of definitions - a set rather than a list because the order is not relevant. These definitions use various symbols. Some symbols are declared "visible" and are exposed/exported in the signature. We write: ``export a, b, c`` at the top of the module, with ``a, b, c`` a list of exported names. There should be a reasonable definition of the module in the absence of imports/exports, so no module syntax is needed to write simple programs. For example, the Prelude is implicitly imported, and all symbols are exported.

Symbols not in the signature are considered "internal". Normally they should not be accessed, but since every rule has an exception Stroscot allows accessing these symbols through the ``_internal`` pseudo-module, like ``a._internal.b``. Thus definitions are always accessible somehow. This avoids Java's issue of reflection allowing access to "private" data. If you really want to lock an API down then you can clear out ``_internal`` by defining an immediately invoked function expression that first computes the module as a value and then returns a separate module that only re-exports the public definitions. But it is probably better to make the methods themselves use a secret password, token, crypto, etc. to prevent unauthorized access.

Modules can be nested within other modules, to an arbitrary depth. Modules can also re-export other modules by importing the modules or by individual copy definitions using the dot syntax, ``a = C.a``.

Functors
========

A function that produces a module is called a functor. Strictly speaking, a functor is not a module, they are distinct types. But the import system can resolve functors to modules - it binds imported modules (functor arguments) to actual modules, and ties recursive knots in the case of recursive imports. With pervasive and automatic import resolution, the distinction between functors and modules becomes blurred because functors with module arguments can be used in places where a module is expected.

In practical programs most code is contained in functors. For example every package in Nixpkgs is a functor like ``{pkgs}: pkgs.mkPackage { ... }``.

Lexical scoping
===============

5C. Everything (including operators) declared in a program shall have a scope (i.e., a portion of the program in which it can be referenced). Scopes shall be determinable during translation. Scopes may be nested (i.e., lexically embedded). A declaration may be made in any scope. Anything other than a variable shall be accessable within any nested scope of its definition.

7C. A reference to an identifier that is not declared in the most local scope shall refer to a program element that is lexically global, rather than to one that is global through the dynamic calling structure.

An identifier is visible from the point of its declaration to the end of the immediately surrounding block.
There is only one effective scope at any point in the code (for instance, variables and types are looked up in the same scope), which pulls in variables from various lexical scopes. Uniform scoping rules make programs easier to read and to understand.

Imports
=======

Importing a module, either top-level or within a block scope, imports all visible symbols in the module for that scope. The dot syntax ``a.b`` allows retrieving a specific symbol ``b`` within a module ``a``, and extends to an expression level import, ``a.(expr) = { import a; expr }``. There is also the `dreaded <https://2ality.com/2011/06/with-statement.html>`__ ``with`` statement.

Name lookup maps a name to the entities in various modules. Name lookup succeeds when it finds a single entity or multiple; multiple entities are called an overload set. Overload resolution occurs after name lookup and resolves the overload set to a single entity using the context in which the name appears.

Lookup for a name n from a lexical scope s returns all entities such that there exists a name in the declaration space of s whose stem identifier is equal to the stem identifier of n. Outer scope entities are shadowed by inner scope entities, and later identifiers shadow earlier identifiers with the same name; shadowing generates a warning. Another warning checks for similarly-named identifiers based on `confusables <http://www.unicode.org/reports/tr39/#Confusable_Detection>`__.

Some ways of importing (based on Python):

* fully qualified: no import, write ``module_a.module_b.module_c.method()``
* module import: ``import module_a.module_b.module_c``, ``module_c.method()``
* member import: ``import module_a.module_b.module_c.method``, ``method()``.
* wildcard import: ``import module_a.module_b.module_c.*``, ``method()``.
* renamed module import: ``import module_a.module_b.module_c as module_d``, ``module_d.method()``
* renamed member import: ``import module_a.module_b.module_c.method as method2``, ``method2()``.
* namespace import: ``import module_a.module_b``, ``module_b.module_c.method()``

Fully qualified names are mainly useful at the REPL, when you just want to access something but don't want to write two statements. They are also useful conceptually, as a way to talk about and use the global namespace. Indeed Nixpkgs does not expose some values in the global namespace and it is annoying to try to access them. In library code though, they are a bit verbose - it could be usable for a function that is only used in one place, or for a quick hack, but it is better style to list an explicit import. Still though, it seems worth supporting fully qualified names everywhere for consistency.

The module import is clear about the origin of each function, avoiding name clashes and some modules deliberately use short, ambiguous names such as ``CSV.read`` so that they can only be used with module imports. It certainly is a common style that should be supported.

The member import is the most concise, and for unambiguous math function like ``sin`` it doesn't make much sense to use the module import. So it also should be supported. Similarly importing all math functions is kind of annoying so the wildcard import makes a lot of sense.

The renaming imports are useful to solve name clashes. But it is questionable whether name clashes need to be resolved in this way, and whether the renamed version is recognizable. For example, Haskell has gotten to the point where people write ``import Set as S``, a terrible name. It is because the names of functions clash (e.g. ``map`` in the Prelude and ``map`` on a ``Set``). But Stroscot has overloading so this clash isn't an issue at all. And a strong standard library committee means that there will be very few name clashes. Still though, I could see it being useful if third-party libraries conflict, or some other rare cases; it doesn't seem worth excluding renaming import entirely like Java.

The namespace import is the bastard child of a fully qualified import and a renaming import. It is still pretty verbose, but it is a little clearer than renaming the module to something unrelated. I don't think it will be used much in practice, but if fully qualified imports are supported it makes sense to also allow partially qualified namespace imports.

Type piracy
===========

Julia has "type piracy" where method definitions are visible even if you don't import that module. For example

::

  module A
  import Base.*
  *(x::Symbol, y::Symbol) = Symbol(x,y)
  end

  module B
  import Base.*
  test = (Symbol "A") * (Symbol "B")

Apparently in Julia B will use A's definition. This is not wanted, because it means there is a global rule definition space. In Stroscot, only rules that are in the transitive closure of explicitly imported dependencies should apply.

Speed
=====

Stroscot has no forward declarations and no header files; everything is declared exactly once. This is in contrast to C++ header includes which are slow because each include must be scanned every time it is included, as much as 30x in bad cases. This explodes code from 2000 files totaling 4.2 MB to 8 GB of disk usage. See for example how Go caches compiled files hence avoiding C++'s issues.

Split definition
================

`Carbon <https://github.com/carbon-language/carbon-lang/blob/trunk/docs/design/code_and_name_organization/README.md#small-programs>`__ mentions the possibility of splitting definitions into interface and implementation, and putting these into separate files, similar to the standard C .h / .c split. They give two reasons for liking this: (1) may help organize code. (2) may let the build system distinguish between the dependencies of the API itself and its underlying implementation.

Organizing code seems reasonable. It is definitely possible for someone to get enthusiastic with the modules and to end up with a mutually recursive import cycle. Like Carbon's example:

::

  module A {
    import B
    symbol Red, Blue, Green
    set Color = {Red, Blue, Green}
    name : Color -> String
  }
  module B {
    import A
    name Red = "Red"
    name Blue = "Blue"
    name Green = "Green"
  }

Now in practice, I have read many arguments against recursive imports. For example Go prohibits import cycles (`thread <https://github.com/golang/go/issues/30247#issuecomment-463940936>`__). Summary of Pike's arguments:

* with recursive dependencies, the dependency graph tends towards one huge cyclical blob
* programmers are less lazy and manage their dependencies better
* the dependency graph is cleaner and more understandable to developers
* builds are faster
* it is simpler to implement
* it is simpler to trace import chains ("detangle")

But it is less convenient - programs that "should" compile don't. One's brain naturally accepts circular dependencies and so there is a mismatch. One wants a definition at a particular place in the module hierarchy and it is only while implementing that one notices it requires circular imports. Many libraries use circular imports - Go standard library, Haskell standard library, etc. When one want to express a circular import structure in Go, one has to contort it - either by manually condensing the definitions into SCC's, or by using hacks such as ``compile:"together"``, interfaces, or runtime private calls with ``go:linkname``. View in the context of these workarounds, Pike's arguments are less convincing:

* at the conceptual level, the dependency structure is unchanged
* programmers are actively working against the language to achieve their desired dependency structure
* the module layout is artificially constrained and end up with huge, bloated, difficult-to-navigate modules
* the builds are slow anyway, with the hacks included
* the workarounds waste significantly more developer time than just implementing cyclic imports
* detangling import chains is more difficult with the hacks than if it was built-in

For Stroscot, we are doing whole-program analysis anyway for optimization purposes, so it is fairly straightforward to allow recursive imports. The incremental build system is designed to handle recursive imports and other such oddities. Pike's concerns about understandability can be addressed by a lint analysis that finds recursive imports and suggests ways to resolve them.

Now moving on to Carbon's second point, letting the build system distinguish between the dependencies of the API itself and its underlying implementation. Steelman 1B similarly says "The language shall require some redundant specifications in programs." This contradicts the DRY best practice ("Don't repeat yourself"), because now there are multiple points of truth - first you write the implementation, then you are required to write the specification on top as a duplication of the implementation. These are coupled and now when you change one you have to change the other. In Stroscot, this redundancy is avoided by making the implementation the only source of truth. Type signatures (specifications) do not affect the runtime behavior at all, but rather are compiler-checked assertions about the behavior of the implementation. These assertions are not redundant because it could indeed be the case that the implementation does not satisfy them. But such assertions are not required, because the implementation itself is the sole source of behavior. Because they are not required, there are no restrictions on where they appear - they may appear next to the implementation, or near the site of API use, or anywhere else in the program.

Module versioning
=================

With packages, we have not only simple modules in our app like ``MyModule`` but also versioned modules like ``packageA-1.2.3.4:SomeModule``. One may ask how much information is needed in the version. Based on `MS <https://learn.microsoft.com/en-us/windows/apps/desktop/modernize/package-identity-overview>`__, the full form of a versioned module identifier should have the following information about its package:

* Package Name: A memorable name chosen by the package developer. Names are not guaranteed to be unique in the general ecosystem, but are unique to a given publisher.
* Publisher: The real-world author, as identified by their signing certificate's public key. Pretty much globally unique, the hard part is rather identifying when two certificates represent the same entity.
* Version: Version number of the package, ordered by some canonical version comparison algorithm. The module developer can choose arbitrary version numbers, or just leave it 0 if the date is sufficient, but usually they will follow guidelines like `SemVer <https://semver.org/>`__, "Major.Minor.Build.Revision" or so.

  * Version comparison algorithm: Split both strings into parts, ``[A-Za-z0-9~]`` and complement, and compare starting from left to right. Then if the first character of both parts is a tilde, it is trimmed. Otherwise, the ~ (tilde) character indicates that a given package or version should be considered older (even if it is numerically larger), so if ``a`` begins with a tilde, ``b`` is newer, and vice-versa. Numbers and words are popped off as units and compared in the following order: any string not found in this list < dev < alpha = a < beta = b < pre < RC = rc < # < pl = p. If one side runs out of characters, the other side is newer, except that a present release specifier is treated as comparing with an absent number. Otherwise, if the last part compares equal, the versions are equal.

* Date: The date of the module's release, used for preferring updated versions of package.
* Hash: Sometimes you want to fork a module rather than update it. As such there is a hash, to specify Git-like fine grained changes while avoiding collisions.

Versioned module identifiers in source code should primarily use name. Publisher and version can be used to disambiguate. Neither publisher keys, dates, nor hashes should appear in actual source code, to avoid the "magic number" antipattern. Instead, they should be centralized in a lock file. Publisher keys should be named by the lockfile and their symbolic name used in the code, to support key expiration and so on. If a module depends on modules with colliding names, the lockfile should specify renamings for the modules so that they can be used together. The lockfile also solves issues like diamond dependencies and import cycles. E.g. suppose we have foo-1:A and bar-1:B depending on each other. If foo-2 is released, do we want foo-2:A -> bar-1:B <-> foo-1:a or else foo-2:A <-> bar-1:B.

The module identifier specifies the module source, not its built form, so processor architecture as given by MS is not really relevant.

I wanted a constraint that the dates must monotically increase, but it is hard to express. For example we may release (in chronological order) 1.0, 2.0, 1.0.1, 2.0.1, 1.1.0. It is really a constraint of the package repository that nobody may release back-dated software, always stamped with the current date, but we have no way of verifying this client-side just given the package list.
