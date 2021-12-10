Modules
#######

Modules from the programming perspective are records; they contain a list of bindings. They use the same brace syntax as statements.

::

  C = {
  a = 1
  b = 2
  }

  C = { a = "x"; b = "y" }

Access
======

To access a member of a module we use dot notation ``C.a``. Modules can be nested arbitrarily deep, ``A.B.C.a``. The uniform notation with records seems cleaner than notation like ``A::B::C::a.b``.

Later in the pipeline:
* A similarly-named identifiers warning based on `confusables <http://www.unicode.org/reports/tr39/#Confusable_Detection>`_


With statement
==============

To save on typing there is the `dreaded <https://2ality.com/2011/06/with-statement.html>`__ ``with`` statement.

::

  # equivalent for most purposes
  C.a
  with C { a }

Without ``with``, bare identifiers or the first component of a dotted name are qualified to the current module. If multiple modules are in scope through ``with``, then an identifier may refer to any of the modules as well. These are handled through the normal overloading mechanism, as if ``a = C.a`` were written for every identifier ``a`` and module ``C``. A warning/error will be generated if the overloading cannot be resolved statically or if it is ambiguous.

.. code-block:: none

  C = complex computation returning a module
  D = complex computation returning a module

  with D { with C { a } }
  -- refers to C.a or D.a

To avoid ambiguity, ``with`` statements can be restricted by hiding identifiers or limiting the introduced identifiers to a list.

::

  with C hiding (b,c)
  with D (a,b,c)

Module exports can similarly be limited:

::

  C = (a) { a = "x"; b = "y" }
  # only a is accessible

Sometimes it is necessary to access internal members, so they are actually still accessible with ``C.__internal.b`` .

You can use ``with`` on an argument with the dot syntax: ``f . = ...`` translates to ``f x = with x { ... }``. This is an abbreviation:

::

  f . ->  ary  // syntax
  f x -> {.}=x; ary} // namespace unpack
  f x -> x.ary}       // plan old symbol

Parameters
==========

Modules also take parameters, passed implicitly; every undefined identifier becomes a parameter. In a working program these parameters will mostly be other modules, but macro definitions can also be passed in. Undefined values in a declaration will be part of that declaration's parameters rather than the module's. Technically, what we have been talking are really module definitions rather than modules, i.e. they are functions producing modules. A fully instantiated module doesn't take any parameters and is simply a record.


Linking
=======

Linking all of these module definitions together is done at the top level. The list of modules is defined in a project file, explicitly or as an enumeration of the file tree. Then they are all tied into one big recursive knot of overloaded definitions. But this process is actually defined by the project file; the project file acts like a build script and can do arbitrarily complex linking things if necessary. There's a set of convenient functions that cover common cases.


::

   project ["src/**.sct"]

Imports
-------

The primitive underlying the project file is the import; this reads a file path and parses it into an implicit function. The file path can be relative and resolved relative to the path of the importing file. For example, if the file dir1/dir2/foo contains import "bar", the compiler will look for dir1/dir2/bar, and import "../bar" would be dir1/bar.

Direct importing is easier to understand conceptually but the recursive fixed point is more powerful and supports libraries better. Direct importing allows IDE tools to statically analyze files without configuring the project file location.


::

  {a, b, c} = import "Alphabet"          # import a, b, c from Alphabet
  {a, b, c=d} = import "Alphabet"  # import a, b, c from Alphabet, import ‘c’ as ‘d’

  ( . ) = import "Alphabet"     # import * from Alphabet
  { . } = import "Alphabet" # import * from Alphabet

  Alphabet = import "Alphabet" # import Alphabet, Alphabet.X

Overrides
=========

By default, methods are scoped to their module. Every definition ``foo = a`` binds the identifier ``Module.foo``, and each module creates a new identifier. The ``override`` statement prevents creating a new identifier, so that instead a base identifer can be extended.

.. code-block:: python3

  # module 1
  foo 1 = 1

  # module 2
  import 1
  override foo
  foo 2 = 3

  # module 3
  import 1, 2
  foo 1 # 1
  foo 2 # 3

If the override statement was not in module 2, then using ``foo`` in module 3 would result in an ambiguous name resolution error.

1ML
===

The above module system is a superset of 1ML, MixML, Backpack, and Newspeak's module system, so those don't need to be considered further. (TODO: reread the papers to check this. but they're mostly type systems, rather than functionality)