Overloading
###########

Stroscot supports typical pattern matching as well as predicate dispatch:

::

   f x y = 1
   f 1 y = 2
   f x 2 | x == 1 = 3

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

The implementation is similar to that used for checking equality of dependent types, i.e. it does a lot of normalization but isn't omniscient. The optimizer decides which case is dead code and will be dropped.

Internally, predicate failure is treated like any other error condition. All cases are run in parallel using the `lub operation <http://conal.net/blog/posts/merging-partial-values>`__ (or maybe its less powerful cousin `unamb`). The `lub` operation is implemented as a primitive that desugars in the middle of Core compilation, in a deterministic manner based on termination checking. I'm not sure how to handle continuations interacting with `lub`.

::

   call binds args = do
    fold lub DispatchError (map ($args) binds)



Sequential matches
==================

The pipe syntax matches cases from top to bottom:

::

   f
   | x y = 1
   | 1 y = 2
   | x 2 = 3

It expands to an unordered set of matches:

::

   f | p1 = ...
   f | p2 and not(p1) = ...
   f | p3 and not(p1) and not(p2) = ...

   # p1, p2, etc. functions of $args

Maybe you will also be able to use match within a function, if the syntax works out:

::

   f = match (2+2) (5+5) | x y = 2
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

