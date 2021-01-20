Overloading
###########

Stroscot supports pattern matching as well as predicate dispatch:

::

   f 1 = 1
   f 2 = 2
   f y | y != 1 && y != 2 = 3

The cases are not ordered. If multiple predicates match, it is required that all matching cases produce the same result. For example if we were to modify the last case:

::

   f 1 = 1
   f 2 = 2
   f y = 2

   f 1
   # Error: rule conflict


But ``f 2 = 2`` would be allowed. This behavior is useful for creating optimized methods for specific types, and also for tests:

::

   fib (-1) = 0
   fib 0 = 1
   fib n = fib (n-1) + fib (n-2)

   # test
   fib 5 = 5

Semantics
=========

The exact semantics is that all cases are run in parallel using the `lub operation <http://conal.net/blog/posts/merging-partial-values>`__ (or maybe its less powerful cousin ``unamb``). The ``lub`` operation is implemented as a primitive that desugars in the middle of compilation, in a deterministic manner based on termination checking. Predicate failure, failed assertions, and nontermination are all treated as bottom.

In fact the semantics is even more complicated, because return values that are not accepted by the surrounding context are also discarded. This falls out naturally from doing the analysis on the CPS-transformed version of the program.

Sequential matches
==================

The pipe syntax matches cases from top to bottom:

::

   f
   | 1 y = 1
   | x 2 = 2
   | x y = 3

It expands to an unordered set of matches:

::

   f | p1 = ...
   f | p2 and not(p1) = ...
   f | p3 and not(p1) and not(p2) = ...

   # p1, p2, etc. functions of $args

So our previous example would be

::

   f 1 y = 1
   f x 2 | x != 1 = 2
   f x y | x != 1 && y != 2 = 3

Maybe you will also be able to use ``match`` within a function, if the syntax details work out:

::

   f = match (2+2) (5+5) | x y = 2
                         | 1 y = 2

Method qualifiers
=================

Stroscot also supports Common Lisp's method qualifiers ``before``, ``after``, and ``around``, as well as a custom qualifier ``list``. Although the dispatch algorithm is complex, the idea is that it is so generic that including it once in the language proper will save everyone's time by avoiding the need to reimplement it.

The basic idea of method combination can be seen in this `illustration <https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/Method-combination.png>`__, except that primaries are not ordered by specificity. Also, there is no reference to classes. Specificity is defined by an SMT solver: ``a`` is less specific than ``b`` if ``SAT(a & not b)`` and ``UNSAT (not a & b)``. This is used to group the methods with a topological sort. To make the sort unique we put recently-defined methods first if possible.

``list`` exposes the topological sort dispatch mechanism directly and simply produces the sorted list of lists of applicable methods, that can then be applied or manipulated as needed. It is an error to define a list method if there are any other types of methods (primary, before, after, or around).

Implementation
==============

The implementation is similar to that used for checking equality of dependent types, i.e. it does a lot of normalization but isn't omniscient. The optimizer decides which case is dead code and will be dropped. The full dispatch mechanism is as follows:

::

   dispatch methods args = do
     [arounds, befores, afters, primaries] = map topological_sort $ partition methods
     next-method = DispatchError
     f arounds where
     f = \x ->
      case x of
         a:as -> call a { next-method = f as }
         [] -> g
     g = case primaries of
       [] -> DispatchError
       _ ->
         map call befores
         x = call (concat primaries)
         map call (reverse afters)
         return x

   call binds args = fold lub DispatchError (map ($ args) binds)


Patterns
========

::

   _ # matches anything
   a # matches anything and binds a
   ^a # matches the atom a
   [(1, "x"), {c: 'a'}] # literal matching itself
   [1, ...] # matches any list starting with 1
   {a: 1, ...: rest} # matches a and the rest of the record
   pat AND pat # matches both patterns simultaneously
   pat OR pat # matches either pattern
   ~pat # desugars to u_ = let pat = u_ in ..., where u_ is a unique name

Guards allow arbitrary functions:

::

   a with a > 0

View patterns

::

   (f -> a)

Functions patterns

::

   Int z = toInteger z

   Int a

Pattern synonyms

::

   pattern F a b = ["f",a,b]

Arbitrary patterns

::

   _f a # matches any function application

Overrides
=========

By default, methods are scoped to their module. Every definition ``foo = a`` binds the identifier ``Module.foo``, and each module creates a new identifier. The ``override`` statement prevents creating a new identifier, so that instead a base identifer can be extended.

::

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

Implicit conversion
===================

There is a function ``convert`` in the core library. It includes as cases / requirements:

* ``convert a = a`` (reflexivity)
* ``convert a = convert (convert a))`` (transitivity)

A pass early in compilation adds a call to ``convert`` around every literal, e.g. ``1+2`` becomes ``convert (convert (+) (convert 1) (convert 2)``.

New cases can be added; this is useful in various instances. For example we can create subtyping.
