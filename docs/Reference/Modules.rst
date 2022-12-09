Modules
#######

Modules from the programming perspective are records; they contain a list of bindings. But they have a top-level scope and take advantage of declaration syntax. They also define namespaces for symbols.

A module starts with a module identifier followed by import declarations followed by other declarations.

.. code-block:: none

  module

  a = 1
  b = 2

  # C = { a = 1; b = 2 }


Access
======

By default bare symbols are interpreted as belonging to the current module. So ``a`` in module ``A`` refers to ``A.a``.

To access a member of a module we use dot notation ``C.a``. Modules can be nested arbitrarily deep, ``A.B.C.a``. The uniform notation with records seems cleaner than notation like ``A::B::C::a.b``. The first component of the dotted name is still qualified to the current module, so ``C.a`` in module ``A`` refers to ``A.C.a``. The recursive knot in the linker defines ``A.C.a = <global>.C.a`` after it sees that ``A`` imports ``C``.

With statement
==============

To save on typing there is the ``with`` statement.

::

  # equivalent for most purposes
  C.a
  with C { a }

If multiple modules are in scope through ``with``, then an identifier may refer to any of the modules as well. These are handled through the normal overloading mechanism, as if ``a = C.a`` were written for every identifier ``a`` and module ``C``. A warning/error will be generated if the overloading cannot be resolved statically or if it is ambiguous.

.. code-block:: none

  C = complex computation returning a module
  D = complex computation returning a module

  with D { with C { a } }
  -- refers to C.a or D.a

To avoid ambiguity, ``with`` statements can be restricted by hiding identifiers or limiting the introduced identifiers to a list.

::

  with C hiding (b,c)
  with D (a,b,c)

You can use ``with`` on an argument with the dot syntax: ``f . = ...`` translates to ``f x = with x { ... }``. This is an abbreviation:

::

  f . ->  ary  // syntax
  f x -> {.}=x; ary} // namespace unpack
  f x -> x.ary}       // plan old symbol

Exports and internal symbols
============================

Module exports can similarly be limited:

::

  module C (a)

  a = "x"
  b = "y"
  # only a is accessible, b is termed an "internal" symbol

Sometimes it is necessary to access internal symbols, so they are actually still accessible with ``C.__internal.b`` .

You can also limit the exports to exclude named clauses of reduction rules (by default all reduction rules are exported regardless of limiting exported symbols):

::

  module C (f) hiding c2

  symbol f

  clause c1
  f 1 = 2

  clause c2
  f 2 = 0

  # f 2 does not reduce outside the module

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


.. code-block:: python3

  {a, b, c} = import "Alphabet"          # import a, b, c from Alphabet
  {a, b, c=d} = import "Alphabet"  # import a, b, c from Alphabet, import ‘c’ as ‘d’

  ( . ) = import "Alphabet"     # import * from Alphabet
  { . } = import "Alphabet" # import * from Alphabet

  Alphabet = import "Alphabet" # import Alphabet, Alphabet.X

Overrides
=========

By default, methods are scoped to their module. Every definition ``foo = a`` binds the identifier ``Module.foo``, and each module creates a new identifier. This means a use ``Module.foo`` refers to only the declarations within that module.

Dispatch will resolve bare identifiers to their appropriate modules when it can be determined from context, and nondeterminism even allows some overloading. For example::

  A = module
    foo (x:{A}) = 1
  B = module
    foo (x:{B}) = 2

  import A, B
  print (map foo [A,B])
  # [1,2]
  # print (force foo)


This defines two symbols ``A.foo : A -> {1}`` and ``B.foo : B -> {2}``, and resolves ``map foo [A,B]`` to ``[A.foo A, B.foo B]``. However, because the bare identifier ends up resolving to different symbols, there are cases where dispatch is not sufficient to resolve the ambiguity. In this example, ``force foo`` results in an error because it could be either ``A.foo`` or ``B.foo``.

The ``override`` statement allows reusing an identifier defined in a different module, extending a method definition as if the clauses were all defined in the same module. This avoids the nondeterminism issues. For example with the following::

  A = module
    foo (x:{A}) = 1
  B = module
    import A
    override foo
    foo (x:{B}) = 2

defines one symbol ``A.foo : {A,B} -> {1,2}``. Thus ``force foo`` will resolve to the single symbol ``A.foo``.
