Reduction
#########

In graph reduction, a graph node identifies a closed expression, and can be referenced by many other nodes. This sharing allows reduction in a more efficient manner, only computing the expression's reduction once rather than repeatedly for each occurrence. But this sharing is relative to the initial term - if there are duplicated expressions in the initial program, or identical expressions arise from distinct reduction paths, graph reduction will not merge them.  But this labelling is only preserved under weak reduction, reduction not inside lambdas.

An extension of graph reduction is optimal reduction.  In optimal reduction, terms with the same family / Levy-label are identified as one node, and strong reduction is valid, i.e. reducing any redex. Levy-labelling is again relative to the initial expression (each sub-term annotated with a unique label) and the reduction history from that expression to the current. There is then the implementation task of representing the shared redexes efficiently so that they may be reduced together.



Although BOHM and Lambdascope have created interaction net machines where a Levy family beta-reduction is an atomic operation, it does not seem that these interaction net machines are actually that efficient. In :cite:`guerriniOptimalImplementationInefficient2017` they find a quadratic slowdown of the fan-fan swap interactions, relative to normal order reduction on lambdas. Furthermore in :cite:`aspertiParallelBetaReduction2001` they find that a Levy family beta-reduction step can take time that is not elementary recursive. It seems like an easier path to optimal reduction is to track the families directly and perform family reductions individually using an efficient non-sharing algorithm such as :cite:`shiversBottomupVreductionUplinks2004` or a limited-sharing algorithm such as graph reduction. Such an approach gives up the exponential speedups of sharing contexts, but has a much more understandable cost model. It is still "optimal" in the sense that outermost family reduction performs the minimal number of family reductions, and the overall reduction follows this strategy, hence the only inefficiency is the need to evaluate some unnecessary individual reductions as part of the family. Potentially, there is the possibility to use the homogeneity of the redexes within a family to speed up within-family reduction, but most likely not.


Since reduction is confluent, it does not change anything to reduce in non-normal order for a time. The reduction will still terminate when going back to normal order. So terminating reductions can always be performed and even non-terminating reductions can be reduced somewhat. Hence during compilation we want to reduce the program as much as possible - ideally the compiled core should be cut-free. We can detect diverging terms and replace them with error terms. But we can't eliminate cuts involving complex recursion, so have to create a heap or a stack allocation. For example the Fibonacci list ``let fibs = 0 :: 1 :: zipWith (+) fibs (tail fibs) in { repeat forever { n <- readInt; print (fibs !! n) } }``, this needs some kind of reduction graph or memo stack involved.


Def/use
=======

Graph reduction using def/use is not hard - assign each definition node a static integer identifier. Then the root is a distinguished definition, which is mutable, while everything else is copied into the root graph when reduction goes past its use point. Assuming the definition data is stored on disk and paged in/out as needed, we can minimize runtime memory use in a compiler pass by introducing as many use-def indirections as possible, one for every sequent in the derivation. This also makes the connections between rules uniform. But having lots of indirections is inefficient so a later pass would remove indirections that will be immediately used (chunkification).

The optimal fixedpoint algorithm outlined in :cite:`shamirFixedpointsRecursiveDefinitions1976` (10.18, PDF pages 240-242) is a variation of Tarjan's strongly connected component algorithm. Cuts between two definitions ``f x`` are memoized in a list, and if the SCC algorithm finds a component ``f x -> let g = ... in g (f x)`` then this component is solved. If it has a unique solution then that's the answer, otherwise ``f x`` diverges and is replaced with a ``RecursionError`` or ``AmbiguousError``. We assume the solver allows uninterpreted "holes", so that the SCC can be solved before its sub-computations.

For comparison, to compute the least fixed point we would maintain a "working graph" and incrementally unfold the definition when encountered. But with the optimal fixed point we first reduce the definition to a value while copying other definitions in.

The solver is an SMT solver on the predicate ``SAT(y == g y)``, and for uniqueness ``UNSAT(y == g y && y != y0)`` where ``y0`` is the first solution found. We exclude error values as possible solutions since the recursion error will be more informative.

The posets the paper uses appear to be pointed directed-complete partial orders `(cppo's) <https://en.wikipedia.org/wiki/Complete_partial_order>`__.


Hashing
=======

To hash the graphs we can use the tree structure of the sequent derivations. Each upward slot in a node is hashed with a fixed value and each downward slot is hashed with a value corresponding to the path through the derivation tree followed by the label of the upward slot. It is written as a single DFS traversal with the leaves as base case that stores the hashed subtree and a map from edge name to partial path.

Hashing infinite graphs is harder, we have to hash each SCC as a unit. See :cite:`mauborgneIncrementalUniqueRepresentation2000`.

Primitives
==========

Primitives (integers) can be handled by hacking special cases into Cut; we add primitive functions of type PiR that use the arguments provided by PiL during a cut, and also literals, special values of type SigmaR. Alternately we can use a specialized proof trees: 64-bit integers are represented as a sigma type with 2^64 possibilities. So addition is represented as a case expression, where each case contains another case expression, and then each case constructs the integer corresponding to the addition. There is a lot of fan-out at each step, which would require 2^128 values to represent, clearly infeasible. So although this is the conceptual representation, the actual representation has no fan-out for the cases - instead the case nodes create symbolic variables ``a`` and ``b``, and the constructed value has the tag ``a+b``.


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
