Posets
######

Posets are used in a few places in Stroscot (operator precedence, method combination).

Sets
====

Sets are defined by a declaration and overriding a method ``(:) : Any -> Set -> Bool``.

::

  set A
  1 : A = true
  2 : A = false
  a : A = (a : B) and not (a : C)

The usual method combination mechanisms apply.

You can also define sets with set-builder notation, set literals, and set operations (union, intersection, complement, difference, symmetric difference).

Relations
=========

There are various types of relations: https://en.wikipedia.org/wiki/Binary_relation#Special_types_of_binary_relations

The question is, what data types do we need for relations?

Function: we need functions, obviously.
Functional: This is a function too, just add a "no clause defined" element.
One-to-one: a function with an assertion, ``assume(x y; if f x == f y { assert x == y}``
Many-to-one: A function, no constraints
Injective: This is the converse of a function, just use the function.
One-to-many: the converse of a function, again just use the function.

So the only relation that can't be represented by a one-argument function is a many-to-many relation. Here we really do have a set of tuples. There are choices of how to implement sets.

We could use a function of two arguments returning a boolean, if the domain/codomain are infinite. Or if both domain and codomain are finite, a set data structure containing tuples. Or a boolean matrix, if there are lots of tuples. Or a map of sets if one of the elements is sparse. Or a directed simple graph if we have a graph library.

Then we have the reflexive, symmetric, transitive closures for many-to-many relations.

Posets
======

A poset is a set with a comparison operation where exactly one of these holds: x < y, x > y, x = y, x ~ y (~ is incomparable).

To specify a poset, we specify a subset of these relations. The relations are then checked/extended to satisfy the properties of transitivity, antisymmetry, reflexivity, and that > is the dual of <. If there is ambiguity in the definition, ~ is preferred, otherwise it's an error.

Constraints
-----------

The simplest relation is specifying the results of comparison. For flexibility one can specify any set of the 4 outcomes (2^4 possibilities, minus the trivial none/all, so 14). The syntax is to combine characters in the order ``<>=~``:

::

  symbol x y
  elemR P x <=~ y
  elemR P x >= y
  elemR P x < y

Specifying each of the elements individually is tedious, so you can also define relations on all elements of two sets:

::

  symbol z
  setR P {x,y} < {z}

We can specify some common properties this way:

::

  -- C total order/chain
  setR P C <>= C
  -- C antichain
  setR P C ~= C
  -- g upper bound of A
  setR P A <= {g}
  -- m lower bound of A
  setR P A >= {m}
  -- g max of A
  setR P A <=~ {g}
  -- m min of A
  setR P A >=~ {m}

There's also Zarf's Precedes thing:

  ::

    precedes P A B =
      if A subsetOf B
        setR P A (B \ A)
      else if B subsetOf A
        setR P (A \ B) B
      else if A disjoint B
        setR P A B
      else
        error "no idea what to do here"

b covers a: no element c exists such that a < c < b

a right above b: a > b and forall c > b, c >= a

Pseudo-elements
---------------

Although they might not exist in the set, you can always talk about:

meet A: upper bound of lower bounds of A
join A: lower bound of upper bounds of A

To put the meet/join in the set use something like

::

  prio P x
  elemR P x = meet A

greatest element: join/upper bound of universal set
least element: meet/lower bound of universal set

Combinations
------------

- lexicographical order
- product order
- direct product
- ordinal sum
- disjoint union
