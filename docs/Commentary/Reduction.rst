Reduction
#########


Stroscot takes after Haskell in that all of the language is compiled to a smallish core language. But the core is the jumbo two-sided linear logic sequent calculus. Reduction of this core corresponds to cut elimination - the normal forms (values) are cut-free terms.

Reduction order
===============

Lazy evaluation doesn't introduce more than a constant factor more of CPU overhead over similar strict runtime systems. (`ref <https://stackoverflow.com/questions/63698539/how-is-lazy-evaluation-implemented-in-a-way-that-doesnt-require-more-overhead-t>`__ `2 <https://apfelmus.nfshost.com/blog/2013/08/21-space-invariants.html>`__) The overhead of creating a thunk and forcing its evaluation replaces the overhead of creating a stack frame, calling the function, and returning. The function call is shifted in time but the cost is not significantly increased. Optimal reduction is similar - ignoring bookkeeping, the number of graph reduction steps is linearly related to the number of beta reductions, except for fan-fan duplication, which is quadratic. :ref:`guerriniOptimalImplementationInefficient2017` But my implementation avoids bookkeeping and fan-fan duplication and hence is linear (TODO: prove this).

Lazy evaluation only helps timewise if a box is weakened. The top-level instruction evaluation loop can be written strictly, using a code pointer for conditional nodes.

However memory is a different story. The attraction of non-strict evaluation is the possibility for evaluating modular programs with memory-efficient evaluation orders. For example, consider a data processor, where one wants to parse and process input *in constant memory*, i.e. the memory consumption is not allowed to depend on the size of the input file. With non-strict evaluation, the task can be split up into logical function (lexer, parser, and emitter), each producing a full data structure. When these are composed with non-strict evaluation the intermediate data structures will be evaluated away, hence using constant memory. With strict evaluation the data structures would instead be evaluated fully at each step, giving terrible performance. In a strict language, one would have to use a streaming abstraction and manage the control flow explicitly to obtain constant memory.

Lazy evaluation only partially addresses this. With the definition ``sum = foldl (+) 0``, unevaluated addition thunks pile up and are only forced at the end, hence the sum operation takes O(n) memory instead of running in a streaming fashion over the list. Haskell has introduced the ``seq`` command, but this is by no means a solution - it means every basic function must come in several strictness variants.

What we need is a "smart" reduction order, that reduces so as to minimize memory usage. Optimal would be nice, but is probably impossible - really it just has to be better than strict evaluation, because strict evaluation has a "reasonable" cost model. So rather than GHC's strictness analysis or demand analysis which proves usage and falls back to lazy, we want a laziness analysis which identifies nontermination and opportunities for optimization and falls back to strict.

``avg xs = sum xs / length xs`` keeps the whole list in memory because it does the sum and then the length (`ref <https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/>__`) - an optimal strategy would have to switch evaluation back and forth between the sum and the length. With the sequent calculus IR this might actually be solved, because cuts get pushed down continually and the natural strategy is to reduce the topmost cut. So in the average calculation we cycle through the redexes for each list element and can discard the beginning of the list once it is processed.

for efficient graph reduction we want to reduce a term completely, if we are able to.

Reduction of our linear logic trees is not confluent, but only because of commuting cuts. If we drop the black edges and only consider proof nets, then the system is confluent. A cut only interacts with other cuts at identity rules, but with a cut-identity-cut pattern it doesn't matter which cut reduces with the identity.

Since reduction is confluent, it does not change anything to reduce in non-normal order for a time. The reduction will still terminate when going back to normal order. So terminating reductions can always be performed and even non-terminating reductions can be reduced somewhat. Hence during compilation we want to reduce the program as much as possible - ideally the compiled core should be cut-free. We can detect diverging terms and replace them with error terms. But the way to handle complex recursion isn't clear. For example the Fibonacci list ``let fibs = 0 :: 1 :: zipWith (+) fibs (tail fibs) in { repeat forever { n <- readInt; print (fibs !! n) } }``, this needs some kind of reduction graph or memo stack involved.

Compressed graphs
=================

64-bit integers are represented as a sigma type with 2^64 possibilities. So addition is represented as a case expression, where each case contains another case expression, and then each case constructs the integer corresponding to the addition. There is a lot of fan-out at each step, which would require 2^128 values to represent, clearly infeasible. So although this is the conceptual representation, the actual representation has no fan-out for the cases - instead the case nodes create symbolic variables ``a`` and ``b``, and the constructed value has the tag ``a+b``.


the value representation is optimized for the platform, and redundant checks are optimized out


The Implementation of Functional Programming Languages
Implementing functional languages: a tutorial
Implementing Lazy Functional Languages on Stock Hardware: The Spineless Tagless G-Machine
How to make a fast curry: push/enter vs eval/apply

a program is a dependency graph which is evaluated through a series of local reductions
the graph itself can be represented as code. In particular, we can represent a node as a function that when invoked, returns the desired value. The first time it is invoked, it asks the subnodes for their values and then operates on them, and then it overwrites itself with a new instruction that just says "return the result."

The short answer is "because it was designed to do exactly that." GHC uses the spineless tagless g-machine (STG). You can read a paper about it here (it's quite complex). GHC does a lot of other things as well, such as strictness analysis and optimistic evaluation.

    The reason I say C and other imperative languages are somewhat similar to Turing Machines (but not to the extent that Haskell is similar to Lambda Calculus) is that in an imperative language, you have a finite number of states (a.k.a. line number), along with a Tape (the ram), such that the state and the current tape determine what to do to the tape.

Is the point of confusion then that mutability should lead to slower code? Haskell's laziness actually means that mutability doesn't matter as much as you think it would, plus it's high-level so there are many optimizations the compiler can apply. Thus, modifying a record in-place will rarely be slower than it would in a language such as C.


In this answer I provide a somewhat detailed comparison of function calls in the GHC RTS and a typical Java VM implementation. That answer is focused on memory usage (because the question was about garbage collection), but much of the discussion applies to performance more generally.

Summarizing the relevant bits, if you are trying to determine the overhead in calling a function to multiply two numbers:

bar :: Int -> Int -> Int
bar a b = a * b

as invoked by some other function:

foo :: Int -> Int -> Int -> Int
foo x y z = let u = bar y z in x + u

then in a typical strict implementation, like the Java JVM, the byte code would probably look something like:

The overhead of the bar function call (i.e., the difference between the above and if bar was inlined) looks like it's two argument pushes, the call itself, and the return.

For the lazy version, GHC (without optimization) compiles this code as something like the following pseudocode:

foo [x, y, z] =
    u = new THUNK(sat_u)                   // thunk, 32 bytes on heap
    jump: (+) x u

sat_u [] =                                 // saturated closure for "bar y z"
    push UPDATE(sat_u)                     // update frame, 16 bytes on stack
    jump: bar y z

bar [a, b] =
    jump: (*) a b

The overhead of the lazy bar function call is the creation of a thunk on the bump heap (as fast as stack) that includes two arguments and a pointer to sat_u (plus room for the return value, though there's no "cost" for this), and a "call" (not visible in the above code) when the (+) function forces the value u by jumping to sat_u. The update frame more or less replaces the return. (In this case, it can be optimized away.)

The bottom line is that, at least to a first approximation, lazy evaluation as implemented in GHC is roughly as fast as strict evaluation, even when everything is actually evaluated.


Recursion
=========

Following :cite:`jonesImplementationFunctionalProgramming1987` chapter 12 we give each definition node a static integer. Then the root is a distinguished definition. Assuming the static data is stored on disk and paged in/out as needed, we can minimize runtime memory use in a compiler pass by introducing as many use-def indirections as possible, one for every sequent in the derivation. This also makes the connections between rules uniform. But having lots of indirections is inefficient so a later pass would remove indirections that will be immediately used (chunkification).

The optimal fixedpoint algorithm outlined in :cite:`shamirFixedpointsRecursiveDefinitions1976` (10.18, PDF pages 240-242) is a variation of Tarjan's strongly connected component algorithm. Cuts between two definitions ``f x`` are memoized in a list, and if the SCC algorithm finds a component ``f x -> let g = ... in g (f x)`` then this component is solved. If it has a unique solution then that's the answer, otherwise ``f x`` diverges and is replaced with a ``RecursionError`` or ``AmbiguousError``. We assume the solver allows uninterpreted "holes", so that the SCC can be solved before its sub-computations.

For comparison, to compute the least fixed point we would maintain a "working graph" and incrementally unfold the definition when encountered. But with the optimal fixed point we first reduce the definition to a value while copying other definitions in.

The solver is an SMT solver on the predicate ``SAT(y == g y)``, and for uniqueness ``UNSAT(y == g y && y != y0)`` where ``y0`` is the first solution found. We exclude error values as possible solutions since the recursion error will be more informative.

The posets the paper uses appear to be pointed directed-complete partial orders `(cppo's) <https://en.wikipedia.org/wiki/Complete_partial_order>`__.

Syntax
======

Proofs are programs by the Curry-Howard correspondence. So we can use all these logical rules as a programming language. But we need a syntax for all the proofs.

Example
~~~~~~~

We use a simple program, boolean "and", presented in a Haskell-ish language:

::

  and = \x -> case x of { False -> \_ -> False; True -> \y -> y }
  r = and False True : Bool

Derivation tree
~~~~~~~~~~~~~~~

See the connectives :math:`\text{B} = \text{Bool}` and :math:`\multimap` defined :ref:`above <connectives>`. :math:`\multimap` is right associative as usual. Our program then has the following derivation tree, among others (we could add a bang to the first argument, use a multiple-argument function, expand out the identity, etc.).

.. image:: /_static/Stroscot_AND_Proof_Tree.svg

The derivation tree representation suffers from what Girard calls "bureaucratic problems of transposing rules" (:cite:`girardGeometryInteraction1989`, page 30 / 98). Girard's example is based on cut elimination; but it is also visible in the specification of the proof tree as rule reordering. For example the cuts on :math:`\Sigma_{F R}` and "Use and" can be swapped without changing the meaning. The derivation tree is also ambiguous in that rule invocations with a sequent like :math:`A, A \vdash B` are ambiguous as to which proposition :math:`A` is used - this isn't resolved by using the exchange rule as then exchange on :math:`A, A, A \vdash B` is ambiguous as to which pair of formulas was swapped.

Nets
~~~~

We can split up the derivation tree into a graph, where each node is a rule instance (the vertical bar in the derivation tree) and the syntactic subderivation relationship is a black edge.

.. image:: /_static/AND_net.svg

.. image:: /_static/AND_net_r.svg

To solve the bureaucratic problems there is another set of connecting edges, the red/blue edges in the graph. The edges are each proposition's introduction/elimination (highest and lowest usage). Exchange rules can be omitted because we reference the propositions directly. The color is for clarity - a proposition on the left (antecedent) is blue and likewise right (succedent) is red. In the code each edge is identified as a unique variable in a slot, so there is no coloring. But depicting n-ary ports in a visual way without ambiguity seems hard.

Most rules do not modify the contexts :math:`\Gamma, \Delta, \Theta, \Lambda` and so the proposition edge skips the node as it is not an introduction/elimination (gray in the presentation above). But there are exceptions that do need the context:

  * :math:`\Pi_R` and :math:`\Sigma_L` rename and combine the context from each case, similar to a phi-node. This can be skipped if there's exactly one case.
  * Exponential promotion defines a box around a subtree of the derivation, and the box edge must be clearly defined so we can duplicate/erase it properly, so we keep the context there.
  * Use/Assign, so that substitution has something to work with and the free variables are identified

Expression "tree"
~~~~~~~~~~~~~~~~~

For doing stuff, we do not need the syntactic subderivation inclusion relationship at all, all the important bits can be gotten from the blue/red edges. (TODO: is this true? how hard is it to preserve the syntactic relationship under cut elimination?)

If we drop the syntactic inclusion relationship, reverse the directions of the blue edges, and drop the sequents, then the graph looks much more like your traditional expression tree. In particular cuts and identities become straight edges rather than top/bottom. PiL is an application node, PiR is a lambda, SigmaL is case, and SigmaR is a constructor (depicted in the graph as True/False).

.. graphviz::

  digraph {
  and [label="and ="]
  and -> d [color="red"]

  d [label="PiR"]
  e -> d [color="blue",dir=back]
  d -> e /* f */ [color="red"]
  e [label="SigmaL"]
  e -> f1 [color="red"]
  e -> f2 [color="red"]
  f1 [label="PiR"]
  g1 -> f1 [color="blue",dir=back]
  f1 -> h1 [color="red"]
  g1 [label="!w"]
  h1 [label="False"]
  f2 [label="PiR"]
  g2 -> f2 [color="blue",dir=back]
  f2 -> h2 /* h2r */ [color="red"]
  g2 [label="!d"]
  h2 -> g2 [color="blue",dir=back]
  h2 [label="I"]
  }

.. graphviz::

  digraph {
  r [label="r ="]
  r -> p /* pr */ [color="red"]
  c1 [label="Cut"]
  c1 -> a [color="red"]
  n -> c1 [color="blue",dir=back]
  a [label="!"]
  a -> b [color="red"]
  b [label="True"]
  c2 [label="Cut"]
  c2 -> l /* lr */ [color="red"]
  m -> c2 [color="blue",dir=back]
  c3 [label="Cut"]
  c3 -> c [color="red"]
  k -> c3 [color="blue",dir=back]
  c [label="False"]
  c4 [label="Cut"]
  c4 -> d2 [color="red"]
  j -> c4 [color="blue",dir=back]
  d2 [label="Use and"]

  j [label="PiL"]
  j -> k /* kr */ [color="red"]
  l -> j [color="blue",dir=back]
  k [label="I"]
  l [label="I"]
  m [label="PiL"]
  m -> n /* nr */ [color="red"]
  p -> m [color="blue",dir=back]
  n [label="I"]
  p [label="I"]
  }

The identity nodes function like thunk identifiers; the stuff in between the identity and the cut is what will get pushed on the stack to execute the thunk. So the I - Cut connections for True and False can be squinched together. Then the path from the assignment node to the use node reads "push !True to stack, push False to stack, reduce with and" (recall the original expression was ``and False True``).

Bad syntax
~~~~~~~~~~

The bad syntax serializes the non-simplified net into a textual form. Each edge is assigned a unique identifier, then all the nodes are written out. The order of the nodes is not important, but the pretty-printer can choose something for readability. The syntax for each node is just Haskell's datatype syntax. You can see how it looks in `Core.hs <https://github.com/Mathnerd314/stroscot/blob/fb648be1ecc3e5c062dbb000d6887a2ce7ac7eb0/src/Core.hs#L50>`__. We should also write out the types of the formulas, so they can be used to get back the full sequent as in the presentation above. But for now Core is untyped, so there is only one universal type and the types of the edges are not written out.

Real syntax
~~~~~~~~~~~

An idea of how a real syntax might look:

::

  "and" = Assign d
    d = PiR
      ^func \[e],[]. [f],[] ->
        f = SigmaL e
          ^False \. f1 ->
            f1 = PiR
              ^func \[g1]. h1
                h1 = False
                [] = BangW g1
          ^True \. f2 ->
            f2 = PiR
              ^func \g2. h2r
                h2 = BangD g2
                  h2r = I h2

  "r" = Assign pr
    n = Cut a
      a = Bang b
        b = True
      m = Cut lr
        k = Cut c
          c = False
          j = Cut d2
            d2 = Use "and"
            [(l,
              lr = I l
            )] = PiL ^func j [(kr,
              kr = I k
            )]
        [(p,
          pr = I p
        )] = PiL ^func m [(nr,
          nr = I n
        )}

Cut elimination
===============

For all reductions:

* Move the two nodes of the cut next to the cut, replacing use with copy-paste

Identity

* Copy the cut's non-identity edge to the non-cut edge that links to the identity node. If the cut has levels then label with (id edge)->(non-id edge).
* Delete cut and identity

Matching left/right Pi/Sigma

* Erase non-matching cases, if any
* Link context variables of matching case with outside variables, if any
* Nodes should have same # of edges. Create cut nodes for each edge-pair.
* Link cut nodes with matching variable on each side.
* Preserve cut's level numbers (but not active status) for red-red, swap for red-blue
* Delete PiR/PiL nodes

Promotion/contraction

* Create duplication nodes for each out-edge of promotion, color=red/blue
* Link duplication nodes, if multiple
* Duplicate !p node and point to duplication node, marking one with arrowhead
* Duplicate cut and point to each side of contraction + !p nodes
* Create new contraction node for each context variable pointing to the two !p's
* Change references to !p to references to context contraction nodes
* Delete original !c contraction node

Promotion/Dereliction

* Replace cut's blue edge with edge of dereliction
* Move level on cut's red edge to cut's blue edge, if not set
* Replace cut's red edge with red edge of promotion
* For each context edge going into !p, replace edge with !p's edge
* Delete dereliction and promotion

Duplication:

* Duplicate node
* Create duplication node for each out-edge of node, copying edge from node
* Point each copy's edge to duplication node
* Add arrowheads to one of the copy's edges (higher number)
* Change dup node's references to point to node copies, deleting arrowheads & using to disambiguate
* Link duplication nodes / fix links

Multi-node Duplication:

* Duplicate nodes & rename
* Fixup node copies to point within themselves
* Create duplication node for each non-active out-edge of node, copying edge from node
* Replace each copy's non-active edges with edge to duplication node.
* Remove arrowheads from first copy's non-active edges. Add arrowheads to second copy set's non-active edges.
* Change dup node's references to point to node copies, deleting arrowheads & using arrowheads to disambiguate
* Link duplication nodes / fix links

Dup-Id-Dup:

* Duplicate identity node
* Replace references to dup nodes with identity reference (odot=higher)
* Delete duplication nodes and links

* For !w/?w we delete the rule with ! and create no cuts.
* To eliminate :math:`\forall` or :math:`\exists`, we extend the variable substitution to the other side.
* The commuting cases are mostly handled by our graph formalism; cuts on the context are pushed down to where they apply. But for PiRight/SigmaLeft there can be a cut on the context variables. To push it down we need to duplicate the cut and its other side for each case. Of course, if there are only 0/1 cases, there is no duplication. But in general we need to introduce a phi-duplication node; when the phi-dup node encounters an identity it adds the edge from the other side to the context variables. Overall commuting deletes code or exposes more reduction opportunities. There is also the potential to reverse-commute and deduplicate part of the cases, if they are the same.
* To expand identities, start with PiRight/SigmaLeft, then use the opposite rule on the opposite side for each case, and terminate with identities. But actually we should prefer to contract identities, searching for expanded identities and replacing them with identities on larger types.

Normal Order Reduction
======================

Looking at the graph for our simple ``and False True`` example we can see the general pattern for reduction, at least normal-order leftmost-outermost reduction. We start at the root and go down. The first node we encounter is not a data node SigmaRight / PiRight / etc. (we would stop with WHNF, and evaluate the children with NF), instead it is an identity node. Since there is only one root node, the highest parent of the other side of the identity must be a cut node (unless the root node has a blue input edge). We then reduce this cut node. In this case it disappears and we move up to the PiL-PiR cut node. Here we create two cut nodes, one cuts the PiL near the root and the output of the SigmaL while the other cuts Id-Cut-False and the input of the SigmaL. The cut on the output of the SigmaL is blocked, so we go up the input of the SigmaL and our reduction stack looks like Root -> I -> PiL -> Cut -> SigmaL -> Cut. First we cut the Id and then we cut False-SigmaL. This selects the left PiR with !w/False for our output. We cut this with the PiL with !True, creating a disconnected cut between !True and !w (that is immediately erased) and another cut between our result and the root identity node. We erase the identity and end with our result False.

So the general pattern is, go down red / go up blue until you get to a redex (cut node, dup node)

Optimal reduction
=================

Optimal reduction ensures the minimal amount of cut elimination (reduction) steps. It's basically normal order, but we have to be careful to avoid duplicating cuts. See the example.

Primitives
==========

Primitives can be handled by hacking special cases into Cut; we add primitive functions of type PiR that use the arguments provided by PiL during a cut, and also literals, special values of type SigmaR.

Linear logic
============

Linear logic has boxes,  The difference is not observable if we do not use duplication; e.g. ``(\x.print(x+1)) (print("x"); 2)`` can only print ``x3``. But if we change ``x+1`` to ``x+x`` then CBV is ``x4`` while CBN is ``xx4``.

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

Recursion
=========

Sequent Core :cite:`downenSequentCalculusCompiler2016a` also introduces two more rules "multicut" and "rec" that are illogical but computationally useful:

.. math::

    \begin{array}{cc}
      \rule
        {\Gamma, \Theta \vdash \Delta, \Lambda \quad \Gamma', \Lambda \vdash \Theta, \Delta' }
        {\Gamma, \Gamma' \vdash \Delta, \Delta' }{\text{multicut}}
      &
      \rule
        { \overrightarrow{\Gamma, \vec \Lambda, \Theta_i \vdash \Lambda_i, \vec \Theta, \Delta }}
        {\Gamma, \overrightarrow{\Theta_i} \vdash \overrightarrow{\Lambda_i}, \Delta }{\text{rec}}
    \end{array}

These probably aren't needed, as let can be encoded as a record and recursion via a fixed-point combinator or a cycle in the graph. In particular :cite:`kiselyovManyFacesFixedpoint2013` outline a polyvariadic combinator:

::

  fix_poly fl = fix (\self -> map ($ self) fl)

To implement ``fix`` we can use the variant of the Y combinator :math:`\lambda f.(\lambda x.x x) (\lambda x.f (x x))`. To type it we need the cyclic/recursive type :math:`Wr = \Pi[(^w, Wr, r)]` (in the sense of an infinite, regular tree). Though, once we have recursive types, we could allow recursive proof trees as well; then implementing recursion directly is probably not too hard. BOHM uses a fan/duplication node combined with a loop.

Optimal reduction
=================

In call-by-value reduction, work is duplicated quite frequently. And lazy or call-by-need reduction, although more efficient computation-wise than call-by-value, still duplicates work. An example is

::

  import System.IO.Unsafe
  i = \w -> (unsafePerformIO (print "i")) `seq` w
  z = 2 :: Integer
  t = 3 :: Integer
  f = \x -> (x z) + (x t)
  main = print (f (\y -> i y) :: Integer)

This produces ``5`` in Haskell. However, without GHC's optimizations, ``"i"`` is evaluated (printed) twice. With optimal reduction, all function applications with known arguments are evaluated exactly once. In particular, the only time a function is evaluated twice is when it is called with different arguments. In the example above it corresponds to a "hoisting" transformation that makes ``i = (unsafePerformIO (print "i")) `seq` \w -> w``, but more complex cases have higher-level sharing that no code transformation can mimic.

Although GHC will do this with ``-O``, it does it messily; the interaction of ``seq`` and inlining is the source of `numerous bugs <https://gitlab.haskell.org/ghc/ghc/issues/2273>`__. In contrast, optimal reduction is based on a principled approach to sharing. The graph corresponds almost exactly to linear logic proof nets. Also, since the sharing is part of the reduction semantics rather than a compiler optimization, it is available in the interpreter (and in the runtime system too). There are no thunks, so there is no need for ``seq``; instead there are boxes and duplicators.

How does strict/lazy/optimal interact with memory management?

* Strict: use a stack
* Lazy: evaluation graph stored on the heap
* Optimal: also an evaluation graph, I guess

Implementation
==============

Reduction is fairly simple to implement without duplication, as it is just pairs of constructors and destructors annihilating and joining their wires, or, for ``case``, joining some eraser nodes. But what about duplication?

Stroscot takes its general inspiration from the delimiter system found in Lambdascope. However, instead of having levels Stroscot keeps explicit track of "environments" or "scopes". In particular a delimiter has an inside scope and an outside scope. Initially, all delimiters look like opening/closing delimiters where the outside scope is the default/root scope ``0`` and the inside scope is the scope of the multiplexer involved. When two delimiters meet, the touching outer scopes are compared for equality (they should always be equal) and one inner scope remains the inner scope while the other inner scope become the new delimiter's outer scope.

To determine which scope becomes the outer scope, delimiters are also marked as "head", "full", or "empty" depending on whether they represent a reference to the result of a duplication, the target of a duplication, or a path that crosses the scope but doesn't duplicate. Interactions are allowed only between head delimiters and other delimiter; the head delimiter's scope stays on the inside.

For multiplexers the situation is a little more complicated. A multiplexer also has two scopes, an inner "label"/identity-like scope and an outer "ambient" scope. When a multiplexer crosses a delimiter, from outside to inside, its "ambient" scope is changed to the delimiter's inside scope. Meanwhile the delimiter's scope is split into a new set of scopes, and this is indexed by the label scope. In the Stroscot code these are referred to as "variant" scopes. In particular, multiplexers with the same label scope must split other scopes into the same set of variant scopes at each interaction. This is not too hard to keep track of, just give each scope a map ``other scope -> variant scope set`` that's lazily created.

Random old junk
###############

Levels
======

For the reduction implementation of contraction we add level indices to the terms in the promotion and dereliction rules of :math:`\bangc/\whimc`, as in :cite:`martiniFineStructureExponential1995` and :cite:`guerriniTheoreticalPracticalIssues1996`. Conceptually all terms have indices, but we can recover the indices in a proof tree by propagating the indices from the promotion/dereliction rules up/down according to the criteria that the indices involved in all non-:math:`\bangc/\whimc` promotion/dereliction rules must be the same.

To handle level indices in infinite trees, we store the difference function ``\a -> a + (j-i)`` and recover the levels by tracing from the root of the derivation tree (which is always level 0) and applying the difference function when encountered.

The level of a context is the maximum of the levels of its terms, 0 if it is empty.

.. math::

    \begin{array}{ccc}
      \rule{\bangc\Gamma^i \vdash A^j, \whimc\Delta^i }{\bangc\Gamma^i \vdash \bangc A^i, \whimc\Delta^i}{\bangc}_{j = i+1}
      & \rule{\sk{\Gamma^i}, A^i \vdash \sk{\Delta^i} }{\sk{\Gamma^i}, \bangc A^j \vdash \sk{\Delta^i}}{\bangc d}_{j\leq i}
      & \rule{\sk{\Gamma}, \overrightarrow{\bangc A, \bangc A, \cdots} \vdash \sk{\Delta} }{\sk{\Gamma}, \bangc A \vdash \sk{\Delta}}{\bangc c_n}
    \end{array}

.. math::

    \begin{array}{ccc}
      \rule{\bangc\Gamma^i, A^j \vdash \whimc\Delta^i }{\bangc\Gamma^i, \whimc A^i \vdash \whimc\Delta^i}{\whimc}_{j = i+1}
      & \rule{\sk{\Gamma^i} \vdash A^i, \sk{\Delta^i} }{\sk{\Gamma^i} \vdash \whimc A^j, \sk{\Delta^i}}{\whimc d}_{j \leq i}
      & \rule{\sk{\Gamma} \vdash \overrightarrow{\whimc A, \whimc A, \cdots}, \sk{\Delta} }{\sk{\Gamma} \vdash \whimc A, \sk{\Delta}}{\whimc c_n}
    \end{array}


To handle level mismatches we might also need lifting operators. The conditions are unclear.

.. math::

    \begin{array}{cc}
      \rule{\Gamma^i \vdash A^j, \Delta^i }{\Gamma^i \vdash A^i, \Delta^i}{\text{lift}_R}_{j > i}
      &
      \rule{\Gamma^i, A^j \vdash \Delta^i }{\Gamma^i, A^i \vdash \Delta^i}{\text{lift}_L}_{j > i}
    \end{array}

F2 G2
=====

For example, the term ``F2 G2 = (\x. x (\w. w) x x) (\y. (\x. x x) (y z))`` from page 17 of :cite:`aspertiOptimalImplementationFunctional1999`. To write the derivation tree we must define a recursive type; the simplest is :math:`\Omega = \, !\Omega \to \Omega` (which is sufficient for the untyped call-by-name lambda calculus) but here we use the mutually recursive types :math:`S,T,f` parametrized over :math:`a` to obtain a finer scheme and make the derivation tree less ambiguous.

.. math::

   S &= \, \left(!T \to !T \right) \to f

   T &= \, !T \to f

   f &= \, !S \to !S \to a

.. math::

   F2 &: \, !S \to a

   G2 &: \, !T \to S

.. image:: /_static/Stroscot_F2G2_Proof_Tree.svg

::

  Root z ret
    Cut f2r f2l
      f2r/[]/[] = PiRight ^func, [x_f2], [ret_f2], [], [] ->
        x_f2 = BangC x1_f2 x23_f2
          x23_f2 = BangC x2_f2 x3_f2
            Cut id_r id_l
              id_r/[]/[] = PiRight ^func, [al], [ar], [], [] ->
                al/ar = Identity
              x1_f2 = BangD x1i_f2
                x1i_f2 = PiLeft ^func [(id_r2, id_l/id_r2 = Identity)] [(ssa,
                  ssa = PiLeft ^func [(x2r_f2, x2_f2/x2r_f2 = Identity)] [(sa,
                    sa = PiLeft ^func [(x3r_f2, x3_f2/x3r_f2 = Identity)] [(a1, a1/ret_f2 = Identity)])])]
      f2l = PiLeft ^func [(g2,
        g2 = Bang g2s
          g2s/[]/[] = PiRight ^func, [y], [ret_g2], [], [] ->
            y = PiLeft ^func [(y_arg,
              Cut o o_app
                o/[]/[] = PiRight ^func, [x_g2], [ret_o], [], [] ->
                  x_g2 = BangC x1_g2 x2_g2
                    x1_g2 = BangD x1i_g2
                      x1i_g2 = PiLeft ^func [(x2r_g2, x2_g2/x2r_g2 = Identity Bool)] [(f, f/ret_o = Identity)]
                o_app = PiLeft ^func [(tr, y_arg/tr = Identity)] [(f, f/ret_g2 = Identity)]
            )] [(zr, z/zr = Identity)]
      )] [(a2, a2/ret = Identity)]

.. graphviz::

  digraph {

  Root -> c1 [style=invis]
  y -> c2 [style=invis]
  x23_f2 -> c3 [style=invis]

  Root -> ret [color="red"]

  c1 [label="Cut"]
  c1 -> f2r [color="red"]
  c1 -> f2l [color="blue"]

  f2r [label="PiR"]
  f2r -> x_f2 [color="blue"]
  f2r -> ret_f2 [color="red"]

  x_f2 [label="!c"]
  x_f2 -> x1_f2 [color="blue"]
  x_f2 -> x23_f2 [color="blue"]

  x23_f2 [label="!c"]
  x23_f2 -> x2_f2 [color="blue"]
  x23_f2 -> x3_f2 [color="blue"]

  c2 [label="Cut"]
  c2 -> id_r [color="red"]
  c2 -> id_l [color="blue"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1_f2 [label="!d"]
  x1_f2 -> x1i_f2 [color="blue"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_l /* id_r2 */ [color="red"]
  x1i_f2 -> ssa [color="blue"]

  id_l [label="I"]

  ssa [label="PiL"]
  ssa -> x2_f2 /* x2r_f2 */ [color="red"]
  ssa -> sa [color="blue"]

  x2_f2 [label="I"]

  sa [label="PiL"]
  sa -> x3_f2 /* x3r_f2 */ [color="red"]
  sa -> ret_f2 /* a1 */ [color="blue"]

  x3_f2 [label="I"]
  ret_f2  [label="I"]

  f2l [label="PiL"]
  f2l -> g2 [color="red"]
  f2l -> ret /* a2 */ [color="blue"]

  ret [label="I"]

  g2 [label="!"]
  g2 -> g2s [color="red"]

  g2s [label="PiR"]
  g2s -> y [color="blue"]
  g2s -> ret_g2 [color="red"]

  y [label="PiL"]
  y -> y_arg [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  c3 [label="Cut"]
  c3 -> o [color="red"]
  c3 -> o_app [color="blue"]

  o [label="PiR"]
  o -> x_g2 [color="blue"]
  o -> ret_o [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_o /* f */ [color="blue"]

  x2_g2 [label="I"]
  ret_o [label="I"]

  o_app [label="PiL"]
  o_app -> y_arg /* tr */ [color="red"]
  o_app -> ret_g2 /* f */ [color="blue"]

  y_arg [label="I"]
  ret_g2 [label="I"]

  }

We want this to reduce without duplicating a cut. So first we reduce everything except the contraction:

.. graphviz::

  digraph {

  Root -> ret [color="red"]

  c2 [label="Cut"]
  c2 -> x_f2 [color="blue"]
  c2 -> g2 [color="red"]

  x_f2 [label="!c"]
  x_f2 -> x1_f2 [color="blue"]
  x_f2 -> x23_f2 [color="blue"]

  x23_f2 [label="!c"]
  x23_f2 -> x2_f2 [color="blue"]
  x23_f2 -> x3_f2 [color="blue"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1_f2 [label="!d"]
  x1_f2 -> x1i_f2 [color="blue"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_r [color="red"]
  x1i_f2 -> ssa [color="blue"]

  ssa [label="PiL"]
  ssa -> x2_f2 /* x2r_f2 */ [color="red"]
  ssa -> sa [color="blue"]

  x2_f2 [label="I"]

  sa [label="PiL"]
  sa -> x3_f2 /* x3r_f2 */ [color="red"]
  sa -> ret /* a2 */ [color="blue"]

  x3_f2 [label="I"]
  ret [label="I"]

  g2 [label="!"]
  g2 -> g2s [color="red"]

  g2s [label="PiR"]
  g2s -> y [color="blue"]
  g2s -> ret_g2 [color="red"]

  y [label="PiL"]
  y -> x_g2 [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_g2 /* f */ [color="blue"]

  x2_g2 [label="I"]

  ret_g2 [label="I"]

  }

Next we move the !c over to a duplication node:

.. graphviz::

  digraph {

  Root -> ret [color="red"]

  c2 [label="Cut"]
  c2 -> x23_f2 [color="blue"]
  c2 -> d1 [color="red"]

  c1 [label="Cut"]
  c1 -> x1_f2 [color="blue"]
  c1 -> d1 [color="red"]

  d1 [label="Dup"]
  d1 -> g2 [color="red"]

  x23_f2 [label="!c"]
  x23_f2 -> x2_f2 [color="blue"]
  x23_f2 -> x3_f2 [color="blue"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1_f2 [label="!d"]
  x1_f2 -> x1i_f2 [color="blue"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_r [color="red"]
  x1i_f2 -> ssa [color="blue"]

  ssa [label="PiL"]
  ssa -> x2_f2 /* x2r_f2 */ [color="red"]
  ssa -> sa [color="blue"]

  x2_f2 [label="I"]

  sa [label="PiL"]
  sa -> x3_f2 /* x3r_f2 */ [color="red"]
  sa -> ret /* a2 */ [color="blue"]

  x3_f2 [label="I"]
  ret [label="I"]

  g2 [label="!"]
  g2 -> g2s [color="red"]

  g2s [label="PiR"]
  g2s -> y [color="blue"]
  g2s -> ret_g2 [color="red"]

  y [label="PiL"]
  y -> x_g2 [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_g2 /* f */ [color="blue"]

  x2_g2 [label="I"]

  ret_g2 [label="I"]

  }

Then we incrementally move the duplication node down and do cut elimination:

.. graphviz::

  digraph {

  Root -> ret [color="red"]

  c2 [label="Cut"]
  c2 -> x23_f2 [color="blue"]
  c2 -> g2b [color="red"]

  c1 [label="Cut"]
  c1 -> x1i_f2 [color="blue"]
  c1 -> d1 [color="red"]

  d1 [label="Dup"]
  d1 -> g2s [color="red"]

  x23_f2 [label="!c"]
  x23_f2 -> x2_f2 [color="blue"]
  x23_f2 -> x3_f2 [color="blue"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_r [color="red"]
  x1i_f2 -> ssa [color="blue"]

  ssa [label="PiL"]
  ssa -> x2_f2 /* x2r_f2 */ [color="red"]
  ssa -> sa [color="blue"]

  x2_f2 [label="I"]

  sa [label="PiL"]
  sa -> x3_f2 /* x3r_f2 */ [color="red"]
  sa -> ret /* a2 */ [color="blue"]

  x3_f2 [label="I"]
  ret [label="I"]

  g2b [label="!"]
  g2b -> d1 [color="red"]

  g2s [label="PiR"]
  g2s -> y [color="blue"]
  g2s -> ret_g2 [color="red"]

  y [label="PiL"]
  y -> x_g2 [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_g2 /* f */ [color="blue"]

  x2_g2 [label="I"]

  ret_g2 [label="I"]

  }

.. graphviz::

  digraph {

  Root -> ret [color="red"]

  c1 [label="Cut"]
  c1 -> x1i_f2 [color="blue"]
  c1 -> d1 [color="red"]

  d1 [label="Dup"]
  d1 -> g2s [color="red"]

  d2 [label="Dup"]
  d2 -> d1 [color="red"]

  g2a [label="!"]
  g2a -> d2 [color="red"]

  g2b [label="!"]
  g2b -> d2 [color="red"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_r [color="red"]
  x1i_f2 -> ssa [color="blue"]

  ssa [label="PiL"]
  ssa -> g2a [color="red"]
  ssa -> sa [color="blue"]

  sa [label="PiL"]
  sa -> g2b [color="red"]
  sa -> ret /* a2 */ [color="blue"]

  ret [label="I"]

  g2s [label="PiR"]
  g2s -> y [color="blue"]
  g2s -> ret_g2 [color="red"]

  y [label="PiL"]
  y -> x_g2 [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_g2 /* f */ [color="blue"]

  x2_g2 [label="I"]

  ret_g2 [label="I"]

  }

.. graphviz::

  digraph {

  Root -> ret [color="red"]

  c1 [label="Cut"]
  c1 -> x1i_f2 [color="blue"]
  c1 -> g2sb [color="red"]

  d2 [label="Dup"]
  d2 -> g2sa [color="red"]

  g2a [label="!"]
  g2a -> d2 [color="red"]

  g2b [label="!"]
  g2b -> d2 [color="red"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_r [color="red"]
  x1i_f2 -> ssa [color="blue"]

  ssa [label="PiL"]
  ssa -> g2a [color="red"]
  ssa -> sa [color="blue"]

  sa [label="PiL"]
  sa -> g2b [color="red"]
  sa -> ret /* a2 */ [color="blue"]

  ret [label="I"]

  dy [label="Dup"]
  dy -> y [color="blue"]

  dr [label="Dup"]
  dr -> ret_g2 [color="red"]

  g2sa [label="PiR"]
  g2sa -> dy [color="blue"]
  g2sa -> dr [color="red"]

  g2sb [label="PiR"]
  g2sb -> dy [color="blue"]
  g2sb -> dr [color="red"]

  y [label="PiL"]
  y -> x_g2 [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_g2 /* f */ [color="blue"]

  x2_g2 [label="I"]

  ret_g2 [label="I"]

  }

.. graphviz::

  digraph {

  Root -> ret [color="red"]

  c1 [label="Cut"]
  c1 -> x1i_f2 [color="blue"]
  c1 -> g2sb [color="red"]

  d2 [label="Dup"]
  d2 -> g2sa [color="red"]

  g2a [label="!"]
  g2a -> d2 [color="red"]

  g2b [label="!"]
  g2b -> d2 [color="red"]

  id_r [label="PiR"]
  id_r -> al [color="blue"]
  id_r -> al /* ar */ [color="red"]

  al [label="I"]

  x1i_f2 [label="PiL"]
  x1i_f2 -> id_r [color="red"]
  x1i_f2 -> ssa [color="blue"]

  ssa [label="PiL"]
  ssa -> g2a [color="red"]
  ssa -> sa [color="blue"]

  sa [label="PiL"]
  sa -> g2b [color="red"]
  sa -> ret /* a2 */ [color="blue"]

  ret [label="I"]

  dy [label="Dup"]
  dy -> y [color="blue"]

  dr [label="Dup"]
  dr -> ret_g2 [color="red"]

  g2sa [label="PiR"]
  g2sa -> dy [color="blue"]
  g2sa -> dr [color="red"]

  g2sb [label="PiR"]
  g2sb -> dy [color="blue"]
  g2sb -> dr [color="red"]

  y [label="PiL"]
  y -> x_g2 [color="blue"]
  Root -> y /* z/zr */ [dir="back"] [color="red"]

  x_g2 [label="!c"]
  x_g2 -> x1_g2 [color="blue"]
  x_g2 -> x2_g2 [color="blue"]

  x1_g2 [label="!d"]
  x1_g2 -> x1i_g2 [color="blue"]

  x1i_g2 [label="PiL"]
  x1i_g2 -> x2_g2 /* x2r_g2 */ [color="red"]
  x1i_g2 -> ret_g2 /* f */ [color="blue"]

  x2_g2 [label="I"]

  ret_g2 [label="I"]

  }

Readback
~~~~~~~~

The real hard part is doing "readback", i.e. proving that all of these transformations are either no-ops on the original lambda term or valid beta reductions. Since there is so much scope popping and pushing and varianting it is definitely a little complex. Also I decided to keep the levels from the original Lambdascope implementation for correctness verification purposes, so there is twice the work. But if you read Ian Mackie's paper on efficient interaction nets implementation you will see that this "readback" algorithm also provides a way to directly execute the proof net / graph with call-by-value semantics at each reduction step. So it also has a close connection with how to compile the graph to machine code. Since our scopes are unique and the "stack" stores only one director index for a scope at a time, the scopes can correspond directly to machine registers or memory locations.

Scopes are nested, forming a simple tree hierarchy. When an opening head delimiter of scope A encounters a closing full or empty delimiter of scope B, then A is reparented under B. In particular the one closing delimiter of scope B vanishes, and duplicates of it are propagated to the outside of all other delimiters of A.

It should be possible to avoid this ``O(n)`` operation by keeping a depth counter in the scope and each delimiter, with the total number of scopes that the delimiter enters/exit being the sum of the scope's depth and the delimiter's depth. Then the interaction would decrement the A scope delimiter's local depth counter by 1, increment A's scope-wide depth counter by 1, and vanish B's delimiter as before. But this hasn't been implemented yet.

First we reduce the top cut:

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  f -> c2
  h_di -> c3

  Root -> ret [color="red",penwidth=2]

  c1a [label="Cut"]
  c1a -> ret_o1 [color="red"]
  c1a -> ret [color="blue"]

  c1b [label="Cut"]
  c1b -> fp [color="red",label="0"]
  c1b -> x_o1 [color="blue"]

  x_o1 [label="!c"]
  x_o1 -> x1_o1 [color="blue"]
  x_o1 -> x2_o1 [color="blue"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret_o1 /* ret_o1l */ [color="blue"]

  fp [label="!p"]
  fp -> f [color="red",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2 [label="Cut",shape=doublecircle]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  ret_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

Next the other PiR-PiL cut:

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  f -> c2a
  f -> c2b
  h_di -> c3

  Root -> ret [color="red",penwidth=2]

  c1a [label="Cut",shape=doublecircle]
  c1a -> ret_o1 [color="red"]
  c1a -> ret [color="blue"]

  c1b [label="Cut"]
  c1b -> fp [color="red",label="0"]
  c1b -> x_o1 [color="blue"]

  x_o1 [label="!c"]
  x_o1 -> x1_o1 [color="blue"]
  x_o1 -> x2_o1 [color="blue"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret_o1 /* ret_o1l */ [color="blue"]

  fp [label="!p"]
  fp -> f [color="red",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2a [label="Cut",shape=doublecircle]
  c2a -> ret_o2 [color="red"]
  c2a -> f_ret [color="blue"]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut",shape=doublecircle]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  ret_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

Then, to clean up the graph, the trivial cuts:

.. graphviz::

  digraph {
  Root -> c1b
  f -> c2b

  Root -> ret [color="red",penwidth=2]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fp [color="red",label="0"]
  c1b -> x_o1 [color="blue"]

  x_o1 [label="!c"]
  x_o1 -> x1_o1 [color="blue"]
  x_o1 -> x2_o1 [color="blue"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret [color="blue"]

  fp [label="!p"]
  fp -> f [color="red",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

To handle the duplication from the contraction we duplicate the cuts/promotion rule and introduce a duplication node Dup to incrementally duplicate the rest of the structure:

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  f -> c2b

  Root -> ret [color="red",penwidth=2]

  d1 [label="Dup"]
  d1 -> f [color="red"]

  c1a [label="Cut",shape=doublecircle]
  c1a -> fpa [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fpb [color="red",label="0"]
  c1b -> x2_o1 [color="blue"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret [color="blue"]

  fpb [label="!p"]
  fpb -> d1 [color="red",label="1"]

  fpa [label="!p"]
  fpa -> d1 [color="red",arrowhead="odot",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

Then we do cut elimination on what remains. Cut/id is trivial.

.. graphviz::

  digraph {
  Root -> c1b
  f -> c2b

  Root -> ret [color="red",penwidth=2]

  d1 [label="Dup",shape=doublecircle]
  d1 -> f [color="red"]

  c1b [label="Cut"]
  c1b -> d1 [color="red",label="1"]
  c1b -> x2i_o1 [color="blue",label="0"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> fpa [color="red",label="0"]
  x2i_o1 -> ret [color="blue"]

  fpa [label="!p"]
  fpa -> d1 [color="red",arrowhead="odot",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }


.. graphviz::

  digraph {
  Root -> c1b
  fa -> c2b
  fb -> c2b
  d1 -> d2 [dir=none,style=dashed,constraint=false]

  Root -> ret [color="red",penwidth=2]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup"]
  d2 -> hi_retp /* h_d */ [color="blue",label="1"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fb [color="red",label="1"]
  c1b -> x2i_o1 [color="blue",label="0"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> fpa [color="red",label="0"]
  x2i_o1 -> ret [color="blue"]

  fpa [label="!p"]
  fpa -> fa [color="red",label="1"]

  fa [label="PiR"]
  fa -> d2 [color="blue",arrowhead="odot"]
  fa -> d1 [color="red",arrowhead="odot"]

  fb [label="PiR"]
  fb -> d2 [color="blue"]
  fb -> d1 [color="red"]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

Then the 3rd PiL-PiR cut / beta reduction, corresponding to Fig 2.14 (4) / (5) on page 31 of :cite:`aspertiOptimalImplementationFunctional1999`:

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  fa -> c2b
  c1b -> c2b
  d1 -> d2 [dir=none,style=dashed,constraint=false]

  Root -> ret [color="red",penwidth=2]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup"]
  d2 -> hi_retp /* h_d */ [color="blue",label="1"]

  c1a [label="Cut",shape=doublecircle]
  c1a -> d1 [color="red",label="1"]
  c1a -> ret [color="blue",label="0"]

  c1b [label="Cut"]
  c1b -> fpa [color="red",label="0"]
  c1b -> d2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> fa [color="red",label="1"]

  fa [label="PiR"]
  fa -> d2 [color="blue",arrowhead="odot"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

.. graphviz::

  digraph {
  Root -> c1b
  fa -> c2b
  c1b -> c2b
  d1 -> d2 [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup"]
  d2 -> hi_retp /* h_d */ [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpa [color="red",label="0"]
  c1b -> d2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> fa [color="red",label="1"]

  fa [label="PiR"]
  fa -> d2 [color="blue",arrowhead="odot"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut",shape=doublecircle]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",label="1"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

At this stage we can't duplicate any more because of the !p box. So we do the other cut.

.. graphviz::

  digraph {
  Root -> c1b
  fa -> c2b
  c1b -> c2b
  fa -> c2a
  c1b -> c2a
  d1 -> d2 [dir=none,style=dashed,constraint=false]
  d3 -> d4 [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup"]
  d2 -> hi_retpc [color="blue"]

  hi_retpc [label="!c"]
  hi_retpc -> hi_retpa [color="blue",label="1"]
  hi_retpc -> hi_retpb [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpa [color="red",label="0"]
  c1b -> d2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> fa [color="red",label="1"]

  fa [label="PiR"]
  fa -> d2 [color="blue",arrowhead="odot"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2a [label="Cut",shape=doublecircle]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue",label="1"]

  c2b [label="Cut",shape=doublecircle]
  c2b -> hi_retpb [color="red",label="1"]
  c2b -> x2_o2 [color="blue",label="1"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpb [label="!p"]
  hi_retpb -> d3 [color="red",label="2"]
  hi_retpb -> d4 [color="blue",label="1"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2",arrowhead="odot"]
  hi_retpa -> d4 [color="blue",label="1",arrowhead="odot"]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red"]

  d4 [label="Dup"]
  d4 -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

.. graphviz::

  digraph {
  Root -> c1b
  fa -> c2b
  c1b -> c2b
  d1 -> d2 [dir=none,style=dashed,constraint=false]
  d3 -> d4 [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup",shape=doublecircle]
  d2 -> hi_retpc [color="blue"]

  hi_retpc [label="!c"]
  hi_retpc -> hi_retpa [color="blue",label="1"]
  hi_retpc -> d4 [color="blue"]

  c1b [label="Cut"]
  c1b -> fpa [color="red",label="0"]
  c1b -> d2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> fa [color="red",label="1"]

  fa [label="PiR"]
  fa -> d2 [color="blue",arrowhead="odot"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2",arrowhead="odot"]
  hi_retpa -> d4 [color="blue",label="1",arrowhead="odot"]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red"]

  d4 [label="Dup"]
  d4 -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

The !c is exposed so we can create another duplication.

.. graphviz::

  digraph {
  Root -> c1b
  fa -> c2b
  c1b -> c2b
  d1 -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d3 -> d4 [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup"]
  d2b -> d4 [color="blue"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  hi_retpc2 [label="!c"]
  hi_retpc2 -> d2a [color="blue"]
  hi_retpc2 -> d2b [color="blue"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fpa [color="red",label="0"]
  c1b -> hi_retpc2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> fa [color="red",label="1"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2",arrowhead="odot"]
  hi_retpa -> d4 [color="blue",label="1",arrowhead="odot"]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red"]

  d4 [label="Dup"]
  d4 -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  fa -> c2b
  c1a -> c2b
  c1b -> c2b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]
  d3 -> d4 [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup"]
  d2b -> d4 [color="blue"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> d2b [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2",arrowhead="odot"]
  hi_retpa -> d4 [color="blue",label="1",arrowhead="odot"]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red"]

  d4 [label="Dup",shape=doublecircle]
  d4 -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

More duplication

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  fa -> c2b
  c1a -> c2b
  c1b -> c2b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]
  d3 -> d4 [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup"]
  d2b -> h_dib [color="blue",label="1"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> d2b [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2",arrowhead="odot"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red"]

  d4 [label="Dup",shape=doublecircle]
  d4 -> h [color="blue"]

  h_dia [label="!d"]
  h_dia -> d4 [color="blue",label="2",arrowhead=odot]

  h_dib [label="!d"]
  h_dib -> d4 [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> i [color="red",label="2"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  i_ret [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  fa -> c2b
  c1a -> c2b
  c1b -> c2b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]
  d3 -> d4a [dir=none,style=dashed,constraint=false]
  d4a -> d4b [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup"]
  d2b -> h_dib [color="blue",label="1"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> d2b [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2",arrowhead="odot"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  d3 [label="Dup",shape=doublecircle]
  d3 -> hi_ret [color="red"]

  d4a [label="Dup"]
  d4a -> i [color="red",label="2"]

  d4b [label="Dup",shape=doublecircle]
  d4b -> hi_ret /* hi_retl */ [color="blue"]

  h_dia [label="!d"]
  h_dia -> ha [color="blue",label="2"]

  h_dib [label="!d"]
  h_dib -> hb [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  ha [label="PiL"]
  ha -> d4a [color="red",arrowhead=odot]
  ha -> d4b [color="blue",arrowhead=odot]

  hb [label="PiL"]
  hb -> d4a [color="red"]
  hb -> d4b [color="blue"]

  i_ret [label="I"]
  hi_ret [label="I",shape=doublecircle]
  f_ret [label="I"]

  }

Up until now we have avoided duplicating identity nodes; this is because duplicating an identity node from only one side would require creating an up-moving duplicator; since cuts reduce downwards this would break the reduction semantics and possibly lead to work duplication. But here we have duplicators on both sides of the identity node, and can duplicate it without reversing directions.

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  fa -> c2b
  c1a -> c2b
  c1b -> c2b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup"]
  d2b -> h_dib [color="blue",label="1"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> d2b [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  c2b [label="Cut",shape=doublecircle]
  c2b -> hi_retb [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_reta [color="red",label="2"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  d4a [label="Dup"]
  d4a -> i [color="red",label="2"]

  h_dia [label="!d"]
  h_dia -> ha [color="blue",label="2"]

  h_dib [label="!d"]
  h_dib -> hb [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  ha [label="PiL"]
  ha -> d4a [color="red",arrowhead=odot]
  ha -> hi_reta [color="blue"]

  hb [label="PiL"]
  hb -> d4a [color="red"]
  hb -> hi_retb [color="blue"]

  i_ret [label="I"]
  hi_reta [label="I"]
  hi_retb [label="I"]
  f_ret [label="I"]

  }

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup"]
  d2b -> h_dib [color="blue",label="1"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> d2b [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_reta [color="red",label="2"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  d4a [label="Dup",shape=doublecircle]
  d4a -> i [color="red",label="2"]

  h_dia [label="!d"]
  h_dia -> ha [color="blue",label="2"]

  h_dib [label="!d"]
  h_dib -> hb [color="blue",label="2"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  ha [label="PiL"]
  ha -> d4a [color="red",arrowhead=odot]
  ha -> hi_reta [color="blue"]

  hb [label="PiL"]
  hb -> d4a [color="red"]
  hb -> x2i_o2 [color="blue",label="2->1"]

  i_ret [label="I"]
  hi_reta [label="I"]
  f_ret [label="I"]

  }

At this point we finish out the duplication of the identity function, similar to Fig 2.19(8).

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2b [label="Dup",shape=doublecircle]
  d2b -> h_dib [color="blue",label="1"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> d2b [color="blue",arrowhead=odot]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> d2b [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> hi_retpa [color="red",label="1"]
  x2i_o2 -> f_ret [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_reta [color="red",label="2"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  h_dia [label="!d"]
  h_dia -> ha [color="blue",label="2"]

  h_dib [label="!d"]
  h_dib -> hb [color="blue",label="2"]

  ha [label="PiL"]
  ha -> i1 [color="red",label="2"]
  ha -> hi_reta [color="blue"]

  hb [label="PiL"]
  hb -> i2 [color="red",label="2"]
  hb -> x2i_o2 [color="blue",label="2->1"]

  hi_reta [label="I"]
  f_ret [label="I"]

  i1 [label="!p"]
  i1 -> i_i1 [color="red",label="3"]

  i_i1 [label="PiR"]
  i_i1 -> il1 [color="blue"]
  i_i1 -> i_ret1 [color="red"]

  il1 [label="!d"]
  il1 -> i_ret1 [color="blue"]

  i_ret1 [label="I"]

  i2 [label="!p"]
  i2 -> i_i2 [color="red",label="3"]

  i_i2 [label="PiR"]
  i_i2 -> il2 [color="blue"]
  i_i2 -> i_ret2 [color="red"]

  il2 [label="!d"]
  il2 -> i_ret2 [color="blue"]

  i_ret2 [label="I"]
  }

Now we can do the other duplication. Duplicating the !p requires all inputs to be covered. similar to identity nodes.

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2ba [dir=none,style=dashed,constraint=false]
  d1 -> d2bb [dir=none,style=dashed,constraint=false]

  Root -> d1 [color="red",label="0->1"]

  d1 [label="Dup",shape=doublecircle]
  d1 -> f_ret [color="red"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  h_dib1 [label="!d"]
  h_dib1 -> hb1 [color="blue",label="2"]

  h_dib2 [label="!d"]
  h_dib2 -> hb2 [color="blue",label="2"]

  hb1 [label="PiL"]
  hb1 -> i2a [color="red",label="2"]
  hb1 -> x2i_o2a [color="blue",label="2->1"]

  hb2 [label="PiL"]
  hb2 -> i2b [color="red",label="2"]
  hb2 -> x2i_o2b [color="blue",label="2->1"]

  i2a [label="!p"]
  i2a -> i_i2a [color="red",label="3"]

  i_i2a [label="PiR"]
  i_i2a -> il2a [color="blue"]
  i_i2a -> i_ret2a [color="red"]

  il2a [label="!d"]
  il2a -> i_ret2a [color="blue"]

  i_ret2a [label="I"]

  i2b [label="!p"]
  i2b -> i_i2b [color="red",label="3"]

  i_i2b [label="PiR"]
  i_i2b -> il2b [color="blue"]
  i_i2b -> i_ret2b [color="red"]

  il2b [label="!d"]
  il2b -> i_ret2b [color="blue"]

  i_ret2b [label="I"]

  d2ba [label="Dup"]
  d2ba -> hi_retpa [color="red",label="1"]

  d2bb [label="Dup",shape=doublecircle]
  d2bb -> f_ret [color="blue"]

  x2i_o2a [label="PiL"]
  x2i_o2a -> d2ba [color="red",arrowhead=odot]
  x2i_o2a -> d2bb [color="blue",arrowhead=odot]

  x2i_o2b [label="PiL"]
  x2i_o2b -> d2ba [color="red"]
  x2i_o2b -> d2bb [color="blue"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> h_dib1 [color="blue",label="1"]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> h_dib2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> d1 [color="red",arrowhead="odot"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_reta [color="red",label="2"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  h_dia [label="!d"]
  h_dia -> ha [color="blue",label="2"]

  ha [label="PiL"]
  ha -> i1 [color="red",label="2"]
  ha -> hi_reta [color="blue"]

  hi_reta [label="I"]
  f_ret [label="I",shape=doublecircle]

  i1 [label="!p"]
  i1 -> i_i1 [color="red",label="3"]

  i_i1 [label="PiR"]
  i_i1 -> il1 [color="blue"]
  i_i1 -> i_ret1 [color="red"]

  il1 [label="!d"]
  il1 -> i_ret1 [color="blue"]

  i_ret1 [label="I"]

  }

.. graphviz::

  digraph {
  Root -> c1a
  Root -> c1b
  d2a -> d2ba [dir=none,style=dashed,constraint=false]

  Root -> f_ret1 [color="red",label="0->1"]

  f_ret1 [label="I"]
  f_ret2 [label="I"]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  h_dib1 [label="!d"]
  h_dib1 -> hb1 [color="blue",label="2"]

  h_dib2 [label="!d"]
  h_dib2 -> hb2 [color="blue",label="2"]

  hb1 [label="PiL"]
  hb1 -> i2a [color="red",label="2"]
  hb1 -> x2i_o2a [color="blue",label="2->1"]

  hb2 [label="PiL"]
  hb2 -> i2b [color="red",label="2"]
  hb2 -> x2i_o2b [color="blue",label="2->1"]

  i2a [label="!p"]
  i2a -> i_i2a [color="red",label="3"]

  i_i2a [label="PiR"]
  i_i2a -> il2a [color="blue"]
  i_i2a -> i_ret2a [color="red"]

  il2a [label="!d"]
  il2a -> i_ret2a [color="blue"]

  i_ret2a [label="I"]

  i2b [label="!p"]
  i2b -> i_i2b [color="red",label="3"]

  i_i2b [label="PiR"]
  i_i2b -> il2b [color="blue"]
  i_i2b -> i_ret2b [color="red"]

  il2b [label="!d"]
  il2b -> i_ret2b [color="blue"]

  i_ret2b [label="I"]

  d2ba [label="Dup"]
  d2ba -> hi_retpa [color="red",label="1"]

  x2i_o2a [label="PiL"]
  x2i_o2a -> d2ba [color="red",arrowhead=odot]
  x2i_o2a -> f_ret2 [color="blue"]

  x2i_o2b [label="PiL"]
  x2i_o2b -> d2ba [color="red"]
  x2i_o2b -> f_ret1 [color="blue"]

  hi_retpc1 [label="!c"]
  hi_retpc1 -> d2a [color="blue",arrowhead=odot]
  hi_retpc1 -> h_dib1 [color="blue",label="1"]

  c1a [label="Cut"]
  c1a -> fpa [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> fpb [color="red",label="0"]
  c1b -> h_dib2 [color="blue",label="1"]

  fpa [label="!p"]
  fpa -> d5 [color="red",label="1"]

  fpb [label="!p"]
  fpb -> d5 [color="red",label="1"]

  d5 [label="Dup"]
  d5 -> fa [color="red"]

  fa [label="PiR"]
  fa -> hi_retpc1 [color="blue"]
  fa -> f_ret2 [color="red"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_reta [color="red",label="2"]
  hi_retpa -> h_dia [color="blue",label="1",arrowhead="odot"]

  h_dia [label="!d"]
  h_dia -> ha [color="blue",label="2"]

  ha [label="PiL"]
  ha -> i1 [color="red",label="2"]
  ha -> hi_reta [color="blue"]

  hi_reta [label="I"]

  i1 [label="!p"]
  i1 -> i_i1 [color="red",label="3"]

  i_i1 [label="PiR"]
  i_i1 -> il1 [color="blue"]
  i_i1 -> i_ret1 [color="red"]

  il1 [label="!d"]
  il1 -> i_ret1 [color="blue"]

  i_ret1 [label="I"]

  }

Junk
====

Our 4th beta reduction

Duplicate more

Add the last duplication node and reduce, for consistency with figure 2.17 (7)

2.17(7) has 5 duplicator nodes, 3 app nodes, and 3 lambda nodes. In comparison, our graph has 5 duplicators, 4 cuts, 3 identities, and one each of PiL, PiR, !p, and !d. So there are still the two pairs of fans but the nodes otherwise look completely different.

Anyway, for the pair on the right, if we scroll up a bit we can see that we were duplicating a single PiR-!d-I loop, the identity function. So the duplication of the I must resolve to another identity function, with the two connecting.

stack machines: waste of time. the stack manipulation takes up more resources than register allocation.

The implementation of overloading is similar to that used for checking equality of dependent types, i.e. it does a lot of normalization but isn't omniscient. The optimizer decides which case is dead code and will be dropped.
The ``lub`` operation is implemented as a primitive that desugars in the middle of compilation, in a deterministic manner based on termination checking.
