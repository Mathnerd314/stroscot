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

To access a member of a module we use dot notation ``C.a``. Modules can be nested arbitrarily deep, ``A.B.C.a``. To save on typing there is the `dreaded <https://2ality.com/2011/06/with-statement.html>`__ ``with`` statement.

::

  # equivalent for most purposes
  C.a
  with C { a }

Without ``with``, bare identifiers or the first component of a dotted name are qualified to the current module. If multiple modules are in scope through ``with``, then an identifier may refer to any of the modules as well. These are handled through the normal overloading mechanism, as if ``a = C.a`` were written for every identifier ``a`` and module ``C``. A warning/error will be generated if the overloading cannot be resolved statically or if it is ambiguous.

::

  C = complex computation returning a module
  D = complex computation returning a module
  with D { with C { a } } # refers to C.a or D.a

To avoid ambiguity, ``with`` statements can be restricted by hiding identifiers or limiting the introduced identifiers to a list.

::

  with C hiding (b,c)
  with D (a,b,c)

Module exports can similarly be limited:

::

  C = (a) { a = "x"; b = "y" }
  # only a is accessible

Sometimes it is still necessary to access internal members, so they are actually still accessible with ``C.__internal.b`` .

Parameters
==========

Modules also take parameters, passed implicitly; every undefined identifier becomes a parameter. In a working program these parameters will mostly be other modules, but macro definitions can also be passed in. Undefined values in a declaration will be part of that declaration's parameters rather than the module's. Technically, what we have been talking are really module definitions rather than modules, i.e. they are functions producing modules. A fully instantiated module doesn't take any parameters and is simply a record.

Linking
=======

Linking all of these module definitions together is done at the top level. The list of modules is defined in a project file or automatically extracted from traversing the file tree. Then they are all tied into one big recursive knot of overloaded definitions. But this process is actually defined by the project file; the project file acts like a build script and can do arbitrarily complex linking things if necessary.
