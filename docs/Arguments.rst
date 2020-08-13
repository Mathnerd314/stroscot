Argument passing
################

Stroscot supports many types of arguments. Functions are extremely common, so the more styles supported,
the shorter the code will be.

Keyword arguments
=================

::

   foo w x y z = z - x / y * w

   v = foo (y:2) (x:4) (w:1) (z:0)
   # 0-4/2*1
   v == foo {x:4,y:2,w:1,z:0}
   # true

Positional arguments
====================

::

   v == foo 1 4 2 0
   # true

You can mix positional and keyword arguments freely; positions are
assigned to whatever is not a keyword argument.

::

   v == foo {z:0} {w:1} 4 2
   # true

Arguments are curried:

::

   c y = y+10
   b x = c

   b 2 1
   # 11

Implicit arguments
==================

These behave similarly to arguments in languages with dynamical scoping.

::

   bar = foo + 2
   baz = bar {x:4,y:2}

   v + 2 == baz {z:0,w:1}
   # true
   v + 2 == baz 1 _ _ 0
   # true

Explicit argument passing cannot replace our implicit variable example,
because in general the author of the system cannot know what all the
parameters will be. Imagine that bar is part of the standard library, a
complex function. The variables x and y do not exist in the standard
library; it is part of the user's code. To use explicit argument passing
would require adding a new argument to bar, but this would break anyone
else using the library. Not to mention that just one intervening
function is rare and we'd probably need to modify 20 or 30 functions in
a bad case.

A typical usage example is logging; the decision of whether to enable
logging is generally a flag defined close to the top level, but each use
site is scattered in the code. An implicit loglevel argument replaces
the global variable that is often used.

Using positional arguments inhibits passing positional arguments
implicitly:

::

   bar = foo + 2
   baz a = bar {x:4,y:2} - a

   v + 2 == baz 0 {z:0,w:1}
   # true
   v + 2 == baz 1 _ _ 0
   # Error: too many arguments to baz

Similarly keyword arguments inhibit passing down that keyword
implicitly:

::

   a k = 1
   b k = k + a

   b {k:2}
   # Error: no definition for k given to a

A proper definition for b would simply omit k:

::

   a k = 1
   b = k + a

   b {k:2}
   # 3

For functions with no positional arguments, positions are assigned
implicitly left-to-right:

::

   a = x / y + y
   a 4 1
   # 5

Atoms that are in lexical scope are not assigned positions, hence (/)
and (+) are not implicit positional arguments for a in the example
above. But they are implicit keyword arguments:

::

   a = x / y + y
   a {(+):(-)} 4 1
   # 4/1-1=3

The namespace scoping mechanism protects against accidental use in large
projects.

Symbols
=======

If a name is not bound anywhere, implicitly or explicitly, it is treated as a symbol. Symbols are essentially records with a distinguished constructor and, in simple cases, zero fields. They evaluate to themselves and can be used freely. If a function is applied to something outside of its domain, this also produces a symbol, but here there would be fields for the arguments.


Other types of arguments
========================

Default arguments, output arguments, and variadic arguments are
supported:

::

   a {k:1} = k + 1
   a # 2

   b = out {a:3}; 2
   b + a
   # 5

   c = sum $arguments
   c 1 2 3
   # 6
   c *([1 2])
   # 3

Modula-3 added keyword arguments and default arguments to Modula-2. But I think they also added a misfeature: positional arguments with default values. In particular this interacts very poorly with currying. If ``foo`` is a function with two positional arguments, the second of them having a default value, then ``foo a b`` is either passing ``b`` to the result of ``f a`` or overriding the default value of the second argument. So specifying/overriding default arguments always requires the use of keyword syntax.

Implicit arguments use keywords as well, so they override default arguments:

::

   a {k:1} = k
   b = a
   c = b {k:2}
   c # 2

Concatenative arguments
=======================

Results not assigned to a variable are pushed to a stack:

::

   1
   2
   3

   %stack
   # 1 2 3

``%`` is the most recent result, with ``%2`` ``%3`` etc. referring to
less recent results:

::

   {a = 1}
   extend % {b=2}
   extend % {c=3}
   shuffle

These stack arguments are used for positional arguments when not
supplied.
