Modules
#######

Modules are reusable components containing code that are themselves first-class objects, replacing more simplistic ways of namespacing and encapsulating units of code such as structs of methods. Modules allow code reuse and abstraction.

Stroscot's module system is based on F#, OCaml, Standard ML, Agda, 1ML, MixML, Backpack, and Newspeak's module systems. (TODO: reread the systems to see if I missed anything. But they're mostly type systems, rather than functionality)

Module
======

A module is a set of definitions - a set rather than a list because the order is not relevant. Some definitions are declared "visible" and are exposed/exported in the signature. We write: ``export a, b, c`` at the top of the module, with ``a, b, c`` a list of exported names.

Definitions not in the signature are considered "internal". Normally they should not be accessed, but since every rule has an exception Stroscot allows accessing these definitions through the ``_internal`` pseudo-module, like ``a._internal.b``. Thus definitions are always accessible somehow. This avoids Java's issue of reflection allowing access to "private" data. If you really want to lock an API down then use a secret password or crypto.

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

Apparently in Julia B will use A's definition. This is not wanted, because it means there is a global rule definition space. In Stroscot only rules that are in the transitive closure of explicitly imported dependencies should apply.

Speed
=====

Stroscot has no forward declarations and no header files; everything is declared exactly once. This is in contrast to C++ header includes which are slow because each include must be scanned every time it is included, as much as 30x in bad cases. This explodes code from 2000 files totaling 4.2 MB to 8 GB of disk usage. See for example how Go caches compiled files hence avoiding C++'s issues.
