Reduction
#########

In graph reduction, a graph node identifies a closed expression, and can be referenced by many other nodes. This sharing allows reduction in a more efficient manner, only computing the expression's reduction once rather than repeatedly for each occurrence. But this sharing is relative to the initial term - if there are duplicated expressions in the initial program, or identical expressions arise from distinct reduction paths, graph reduction will not merge them.  But this labelling is only preserved under weak reduction, reduction not inside lambdas.

An extension of graph reduction is optimal reduction.  In optimal reduction, terms with the same family / Levy-label are identified as one node, and strong reduction is valid, i.e. reducing any redex. Levy-labelling is again relative to the initial expression (each sub-term annotated with a unique label) and the reduction history from that expression to the current. There is then the implementation task of representing the shared redexes efficiently so that they may be reduced together.



Although BOHM and Lambdascope have created interaction net machines where a Levy family beta-reduction is an atomic operation, it does not seem that these interaction net machines are actually that efficient. In :cite:`guerriniOptimalImplementationInefficient2017` they find a quadratic slowdown of the fan-fan swap interactions, relative to normal order reduction on lambdas. Furthermore in :cite:`aspertiParallelBetaReduction2001` they find that a Levy family beta-reduction step can take time that is not elementary recursive. It seems like an easier path to optimal reduction is to track the families directly and perform family reductions individually using an efficient non-sharing algorithm such as :cite:`shiversBottomupVreductionUplinks2004` or a limited-sharing algorithm such as graph reduction. Such an approach gives up the exponential speedups of sharing contexts, but has a much more understandable cost model. It is still "optimal" in the sense that outermost family reduction performs the minimal number of family reductions, and the overall reduction follows this strategy, hence the only inefficiency is the need to evaluate some unnecessary individual reductions as part of the family. Potentially, there is the possibility to use the homogeneity of the redexes within a family to speed up within-family reduction, but most likely not.


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
3. Inlining, when the duplication is carried out, resulting in two terms.
4. More complex cases enforced by a typing system, such as Elementary Affine Logic.

Implementation
==============

Reduction is fairly simple to implement without duplication, as it is just pairs of constructors and destructors annihilating and joining their wires, or, for ``case``, joining some eraser nodes. But what about duplication?

Stroscot takes its general inspiration from the delimiter system found in Lambdascope. However, instead of having levels Stroscot keeps explicit track of "environments" or "scopes". In particular a delimiter has an inside scope and an outside scope. Initially, all delimiters look like opening/closing delimiters where the outside scope is the default/root scope ``0`` and the inside scope is the scope of the multiplexer involved. When two delimiters meet, the touching outer scopes are compared for equality (they should always be equal) and one inner scope remains the inner scope while the other inner scope become the new delimiter's outer scope.

To determine which scope becomes the outer scope, delimiters are also marked as "head", "full", or "empty" depending on whether they represent a reference to the result of a duplication, the target of a duplication, or a path that crosses the scope but doesn't duplicate. Interactions are allowed only between head delimiters and other delimiter; the head delimiter's scope stays on the inside.

For multiplexers the situation is a little more complicated. A multiplexer also has two scopes, an inner "label"/identity-like scope and an outer "ambient" scope. When a multiplexer crosses a delimiter, from outside to inside, its "ambient" scope is changed to the delimiter's inside scope. Meanwhile the delimiter's scope is split into a new set of scopes, and this is indexed by the label scope. In the Stroscot code these are referred to as "variant" scopes. In particular, multiplexers with the same label scope must split other scopes into the same set of variant scopes at each interaction. This is not too hard to keep track of, just give each scope a map ``other scope -> variant scope set`` that's lazily created.

For efficient graph reduction we want to reduce a term completely, if we are able to. The top-level instruction evaluation loop can be written strictly, using a code pointer for conditional nodes.
