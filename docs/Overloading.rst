Overloading
###########

Stroscot supports typical pattern matching and predicate dispatch:

::

   f x y = 1
   f 1 y = 2
   f x 2 with x == 1 = 3

The cases are not ordered, instead they are matched most-specific to least-specific. Specificity is defined by an SMT solver, i.e. SAT(a & not b) and UNSAT (not a & b) means that a is less specific than b.

If there is a tie regarding specificity, e.g. we were to modify the last case:

::

   f x y = 1
   f 1 y = 2
   f x 2 = 3

   f 1 2
   # Error: rule conflict

Then it is an error.

However, cases that overlap but produce the same result are allowed.
It could be useful for tests:

::

   fib (-1) = 0
   fib 0 = 1
   fib n = fib (n-1) + fib (n-2)

   # test
   fib 5 = 5

The implementation is similar to that used for checking equality of dependent types, i.e. it does some normalization but isn't omniscient. The optimizer decides which case is dead code and can be dropped.

Sequential matches
==================

The pipe syntax matches cases from top to bottom:

::

   f
   | x y = 1
   | 1 y = 2
   | x 2 = 3

Maybe you will also be able to use match within a function, if the syntax works out:

::

   f = match | x y = 2
             | 1 y = 2

Patterns
========

::

   _ # matches anything
   a # matches anything and binds a
   ^a # matches the atom a
   [(1, "x"), {c: 'a'}] # literal matching itself
   [1, ...] # matches any list starting with 1
   {a: 1, ...: rest} # matches a and the rest of the record
   pat = pat # matches both patterns simultaneously
   pat or pat # matches either pattern

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

