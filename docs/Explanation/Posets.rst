Posets
######

Posets are used in a few places in Stroscot (operator precedence, method combination).

Sets
====

Sets are defined by a method ``(:) : Any -> Set -> Bool``.

::

  1 : A = true
  2 : A = false
  a : A = (a : B) and not (a : C)

The usual method combination mechanisms apply.

You can also define sets with set-builder notation, set literals, and set operations (union intersection difference complement).

Posets
======

A poset is a set with a comparison operation where exactly one of these holds: x < y, x > y, x = y, x ~ y (~ is incomparable).

To specify a poset, we specify a subset of these relations. The relations are then checked/extended to satisfy the properties of transitivity, antisymmetry, reflexivity, and duality. If there is ambiguity in the definition, ~ is preferred, otherwise it's an error.

Relations
---------

The simplest relation is specifying the results of comparison. For flexibility one can specify any set of the 4 outcomes (2^4 possibilities, minus the trivial none/all). The syntax is to combine characters in the order ``<>=~``:

::

  elemR P x <=~ y
  elemR P x >= y
  elemR P x < y

Specifying each of the elements individually is tedious, so you can also define relations on all elements of two sets:

::

  setR P {x,y} < {z}

Priorities
----------

Similar to overloading you can specify priorities for the rules:

::

  prioDef plow < phigh

  prio plow
  setR P {x,y} < {z}

  prio phigh
  elemR P x > y




There's also Zarf's Precedes thing:

::

  precedes P A B =
    if A subsetOf B
      setR P A (B \ A)
    else if B subsetOf A
      setR P (A \ B) B


b covers a: no element c exists such that a < c < b
checkable assertion, not sure if useful

defining a relation on all elements of 2 sets is automatic:
{a} <> {b}

chain C: C <>= C
antichain C: C ~= C

upper bound of A:
  element g s.t. A ≤ g
lower bound of A:
  element m s.t. A ≥ m
max of A:
  element g s.t. A ≤~ g
min of A:
  element m s.t. A ≥~ m
meet of A: upper bound of lower bounds of A
join of A: lower bound of upper bounds of A
greatest element: join/upper bound of universal set
least element: meet/lower bound of universal set

combinations of posets
- lexicographical order
- product order
- direct product
- ordinal sum
- disjoint union

Total order
===========

Making the above generate a total order is fairly easy, just pick a linearization or error if it's ambiguous.

price (obj : Treasure) = 10

hit (obj : MingVase) =
  print "it cracks"
  price obj := 0

main =
  mv = MingVase
  print (price mv)
  hit mv
  print (price mv)

This is defining a rule at runtime to override the static rule. := is an fexpr; otherwise price obj would be resolved to 10. Similarly all the price calls are dynamic. So translated this looks like:

static_price (obj : Treasure) = 10

hit (obj : MingVase) =
  print "it cracks"
  set_dynamic "price" obj 0

price obj | has_dynamic state "price" obj = run_dynamic state "price" obj
          | otherwise = static_price obj

main =
  state = ...
  mv = MingVase
  print (price mv)
  hit mv
  print (price mv)

With a uniform translation everything will be wrapped in this dynamic override. Since dynamic overrides static, nothing from static needs to carry over - the rule system can be completely different. In particular the dynamic system does not need to implement priorities, method combination, or specificity overriding. (Although, it could, with enough work).

As far as rulesets go, set theory seems sufficient. Exceptions are just setminus {x} and then there's a macro for defining a group of rules all in a set.
