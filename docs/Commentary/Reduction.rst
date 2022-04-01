Reduction
#########

Linear logic
============

Linear logic has boxes, The difference is not observable if we do not use duplication. e.g. ``(\x.print(x+1)) {print("x"); 2}`` can only print ``x3``. But if we change ``x+1`` to ``x+x`` then CBV/optimal/lazy is ``x4`` while CBN is ``xx4``.

So how do we specify the difference between the two, in linear logic?

::

  s x =
    (y,z) = dup x
    print(y+z)
  s (print("x"); 2)

Boxes do have some performance cost, so how can they be avoided? There are cases where boxes are not necessary:

1. When the term is linear or affine and does not need to duplicate anything.
2. When the duplication is duplication of a graph without any cuts, such as a boolean, integer, list of integers, etc. Even when there are cuts, the value can be forced and then copied directly, using a fold. (per :cite:`filinskiLinearContinuations1992`) Q: Does this change the evaluation semantics to be stricter?
3. Inlining, when the duplication is carried out, resulting in two terms.
4. More complex cases enforced by a typing system, such as Elementary Affine Logic.

Implementation
==============

Reduction is fairly simple to implement without duplication, as it is just pairs of constructors and destructors annihilating and joining their wires, or, for ``case``, joining some eraser nodes. But what about duplication?

Stroscot takes its general inspiration from the delimiter system found in Lambdascope. However, instead of having levels Stroscot keeps explicit track of "environments" or "scopes". In particular a delimiter has an inside scope and an outside scope. Initially, all delimiters look like opening/closing delimiters where the outside scope is the default/root scope ``0`` and the inside scope is the scope of the multiplexer involved. When two delimiters meet, the touching outer scopes are compared for equality (they should always be equal) and one inner scope remains the inner scope while the other inner scope become the new delimiter's outer scope.

To determine which scope becomes the outer scope, delimiters are also marked as "head", "full", or "empty" depending on whether they represent a reference to the result of a duplication, the target of a duplication, or a path that crosses the scope but doesn't duplicate. Interactions are allowed only between head delimiters and other delimiter; the head delimiter's scope stays on the inside.

For multiplexers the situation is a little more complicated. A multiplexer also has two scopes, an inner "label"/identity-like scope and an outer "ambient" scope. When a multiplexer crosses a delimiter, from outside to inside, its "ambient" scope is changed to the delimiter's inside scope. Meanwhile the delimiter's scope is split into a new set of scopes, and this is indexed by the label scope. In the Stroscot code these are referred to as "variant" scopes. In particular, multiplexers with the same label scope must split other scopes into the same set of variant scopes at each interaction. This is not too hard to keep track of, just give each scope a map ``other scope -> variant scope set`` that's lazily created.

For efficient graph reduction we want to reduce a term completely, if we are able to. The top-level instruction evaluation loop can be written strictly, using a code pointer for conditional nodes.

stack machines: waste of time. the stack manipulation takes up more resources than register allocation.

The implementation of overloading is similar to that used for checking equality of dependent types, i.e. it does a lot of normalization but isn't omniscient. The optimizer decides which case is dead code and will be dropped.
The ``lub`` operation is implemented as a primitive that desugars in the middle of compilation, in a deterministic manner based on termination checking.

Semantics
=========

The dispatch semantics is that all methods are run in parallel using the `lub operation <http://conal.net/blog/posts/merging-partial-values>`__. Predicate failure, failed assertions, exceptions, and nontermination are all treated as bottom. Furthermore ``lub`` is an oracle, analyzing the whole program - we want return type overloading, return values that are not accepted by the surrounding context are discarded.

This falls out naturally from doing the analysis on the CPS-transformed version of the program.

* Dead code elimination
* Recursion
  * induction variable analysis to replace multiplication by a loop index with addition
  * loop reversal - changing condition to zero comparison
  * loop unswitching - moving conditional outside loop
  * hoisting invariants, partial/total redundancy elimination
  * parallelization - multi-threaded or vectorized code



