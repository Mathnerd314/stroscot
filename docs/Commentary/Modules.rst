Modules
#######

Developers can add new language features and functionality through libraries and modules.

Module
======

Modules are reusable components containing code that are themselves first-class objects, replacing more simplistic ways of namespacing and encapsulating units of code such as structs of methods. Modules allow code reuse and abstraction. Stroscot's module system is based on F#, OCaml, Standard ML, Agda, 1ML, MixML, Backpack, and Newspeak's module systems. (TODO: reread the systems to see if I missed anything. But they're mostly type systems, rather than functionality)

A module is a set of definitions - a set rather than a list because the order is not relevant. Some definitions are declared "visible" and are exposed/exported in the signature. We write: ``export a, b, c`` at the top of the module, with ``a, b, c`` a list of exported names.

Definitions not in the signature are considered "internal". Normally they should not be accessed, but since every rule has an exception Stroscot allows accessing these definitions through the ``_internal`` pseudo-module, like ``a._internal.b``. Thus definitions are always accessible somehow. This avoids Java's issue of reflection allowing access to "private" data. If you really want to lock an API down then you can clear out ``_internal`` by defining an immediately invoked function expression that first computes the module as a value and then returns a separate module that only re-exports the public definitions. But it is probably better to make the methods themselves use a secret password, token, crypto, etc. to prevent unauthorized access.

Modules can be nested within other modules, to an arbitrary depth. Modules can also re-export other modules by importing the modules or by individual copy definitions using the dot syntax, ``a = C.a``.

Functors
========

A function that produces a module is called a functor. Strictly speaking, a functor is not a module, they are distinct types. But the import system can resolve functors to modules - it binds imported modules (functor arguments) to actual modules, and ties recursive knots in the case of recursive imports. With pervasive and automatic import resolution, the distinction between functors and modules becomes blurred because functors with module arguments can be used in places where a module is expected.

In practical programs most code is contained in functors. For example every package in Nixpkgs is a functor like ``{pkgs}: pkgs.mkPackage { ... }``.

Lexical scoping
===============

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

Libraries
=========

Modules are collected into mini-libraries, and mini-libraries into larger libraries, eventually agglomerating into large collections such as the standard library.

The full form of a module identifier should have the following (`MS <https://learn.microsoft.com/en-us/windows/apps/desktop/modernize/package-identity-overview>`__):

* Name: A memorable name chosen by the module developer. Names are not guaranteed to be unique in the general ecosystem, but are unique to a given publisher.
* Publisher: The real-world author, as identified by their signing certificate's public key. Pretty much globally unique, the hard part is rather identifying when two certificates represent the same entity.
* Version: Version number of the package, ordered by some canonical version comparison algorithm. The module developer can choose arbitrary version numbers, or just leave it 0 if the date is sufficient, but usually they will follow guidelines like `SemVer <https://semver.org/>`__, "Major.Minor.Build.Revision" or so.

  * Version comparison algorithm: Split both strings into parts, ``[A-Za-z0-9~]`` and complement, and compare starting from left to right. Then if the first character of both parts is a tilde, it is trimmed. Otherwise, the ~ (tilde) character indicates that a given package or version should be considered older (even if it is numerically larger), so if ``a`` begins with a tilde, ``b`` is newer, and vice-versa. Numbers and words are popped off as units and compared in the following order: any string not found in this list < dev < alpha = a < beta = b < pre < RC = rc < # < pl = p. If one side runs out of characters, the other side is newer, except that a present release specifier is treated as comparing with an absent number. Otherwise, if the last part compares equal, the versions are equal.

* Date: The date of the module's release, used for preferring updated versions of package. The dates must monotically increase, i.e. it is forbidden to release a mainline version of a module with a date earlier than a previously released module.
* Hash: Sometimes you want to fork a module rather than update it. As such there is a hash, to specify Git-like fine grained updates while avoiding collisions.

The module identifier specifies the module source, not its built form, so processor architecture is not really relevant.

Modules in source code should primarily use name, and only occasionally publisher or version. Neither dates nor hashes should appear in actual source code, to avoid the "magic number" antipattern. Instead, they should be centralized in a lock file,  If a module depends on modules with colliding names, the lockfile should specify renamings for the modules so that they can be used together.

The modules should also be downloadable independently, so really the "standard library" is a software repository with high standards for inclusion.
