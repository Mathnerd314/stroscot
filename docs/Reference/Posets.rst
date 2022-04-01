Posets
######

Posets are used in a few places in Stroscot (operator precedence, method combination).

A poset is a set with a comparison operation where exactly one of these holds: x < y, x > y, x = y, x ~ y (~ is incomparable).

To specify a poset, we specify the elements in the set via set definitions, and then the relations between elements via constraints. The constraints are then checked/extended to satisfy the properties of transitivity, antisymmetry, reflexivity, and that > is the dual of <. Ambiguity in the definition is an error.

Constraints
-----------

The simplest constraint is specifying the results of comparison. For flexibility one can specify any set of the 4 outcomes (2^4 possibilities, minus the trivial none/all, so 14). The syntax is to combine characters in the order ``<>=~``:

::

  symbol x y : P
  elemR P x <=~ y
  elemR P x >= y
  elemR P x < y

Specifying each of the elements individually is tedious, so you can also define relations on all elements of two sets:

::

  symbol z : P
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

There's also Zarf's `Precedes thing <https://eblong.com/zarf/essays/rule-based-if/ruleday-log.html>`__:

::

    precedes P A B =
      if A subsetOf B
        setR P A < (B \ A)
      else if B subsetOf A
        setR P (A \ B) < B
      else if A disjoint B
        setR P A < B
      else
        error "no idea what to do here"

It's so vague and "do what I mean" that I'm not sure it's a real constraint.

a covers b or is "right above" it (corresponding to a line from b to a in a Hasse diagram): a > b and no element c exists such that a > c > b, i.e. forall c > b, c >=~ a.

Meet and join
-------------

We want to talk about meet and join, but those do not exist in every poset. But with the Dedekind-MacNeille completion every poset is a subset of a complete lattice, in a way that preserves order / incomparability and any meets and joins that exist in the poset. So when we write ``meet {a,b}`` we are referring to a unique element in the completion that may or may not be in the poset, that is the upper bound of the lower bounds of ``{a,b}``.

To put the meet/join in the poset use something like

::

  symbol x : P
  elemR P x = meet A

There are also strict bounds, mainly defined for total orders / chains:
* greatest element: upper bound of all elements of P
* least element: lower bound of all elements of P

Combinations
------------

- lexicographical order ``lex(A,B,C)``
- product order ``product(A,B,C)``
- direct product ``direct-product(A,B,C)``
- ordinal sum ``sum(A,B,C)``
- disjoint union ``union(A,B,C)``

