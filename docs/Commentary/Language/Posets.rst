Posets
######

Posets are used in a few places in Stroscot (operator precedence, method combination).

Laws
====

In Stroscot, a poset is defined by a "partial comparison operator", a single function ``comp : Any -> Any -> {LessThan,Equal,GreaterThan,Incomparable}``, satisfying the requirements:

* symmetry: ``comp x y = comp y x /. { LessThan -> GreaterThan, GreaterThan -> LessThan }``
* transitivity:

::

  forall y, case (comp x y, comp y z) of
    (t,Equal) -> comp x z = t
    (Equal,t) -> comp x z = t
    (LessThan,LessThan) -> comp x z = LessThan
    (GreaterThan,GreaterThan) -> comp x z = GreaterThan

Considering ``\x y -> comp x y /. { GreaterThan -> Incomparable, LessThan -> Incomparable }``, we obtain a `partial equivalence relation <https://en.wikipedia.org/wiki/Partial_equivalence_relation>`__, hence the name.

We deliberately omit the reflexivity property ``comp x x = Equal``. What extra freedom does omitting reflexivity give? Per symmetry, an element cannot be greater or less than itself, so can only be self-equal or self-incomparable. Self-equal elements can be in any relation to other elements. A self-incomparable element cannot be equal to any other element (quasi-reflexivity), but can be less than, greater than, or incomparable with other elements. If we set the element equal to itself ``comp' A A = Equal; comp' = comp`` or similarly incomparable to itself, we obtain another partial comparison operator. So we can obtain a reflexive partial order (traditional poset) as the reflexive closure, or an irreflexive partial order as the irreflexive kernel, or anything in between.

The traditional definition of a poset is obtained by considering the set ``{ x | comp x x == Equal }``. Thus we see that the domain of the poset is specified implicitly in a flexible way. Specifying the domain of the poset explicitly is not as extensible.

Definition
==========

To specify a poset, we have to specify the relations between elements. The constraints are then checked/extended to satisfy the laws.

Generally, ambiguity in the definition is an error. But it would be tedious to list every element not in the poset and say it is self-incomparable, so we want an overridable assumption that ``a ~ a``. Similarly ``1 <~ 2`` should resolve to ``1 ~ 2`` by default. Maybe this isn't safe under extension and is too fragile. There seems to be no literature on syntax for specifying posets.

Constraints
-----------

The simplest constraint is specifying the results of comparison. For flexibility one can specify any set of the 4 outcomes (2^4 possibilities, minus the trivial none/all, so 14).  The syntax is to combine characters in the order ``<>=~``:

::

  elemR P x = x, y = y
  elemR P x <=~ y
  elemR P x >= y
  elemR P x < y

`IEEE 754 <https://grouper.ieee.org/groups/msc/ANSI_IEEE-Std-754-2019/background/predicates.txt>`__ omits ``=~`` and ``<>`` as "insufficiently useful to be mandatory", but they seem worth including for completeness.

Specifying inclusion in the poset is common, this is just reflexivity:

::

  symbol P {z}
  -- equivalent to
  elemR P z = z

Specifying each of the elements individually is tedious, so you can also define relations elementwise on two sets:

::

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

We want to talk about meet and join, but those do not exist in every poset. But with the Dedekind-MacNeille completion every poset embeds into a complete lattice, in a way that preserves order / incomparability and any meets and joins that exist in the poset. Specifically, we consider subsets A such that ``A = { y | forall x, a in A. y <= x, x >= a }``, and map elements s in the poset to ``{ y | forall x. y <= x, x >= s }``,

So when we write ``meet {a,b}`` we are referring to a unique element in the completion that may or may not be in the poset, that is the upper bound of the lower bounds of ``{a,b}``.

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

