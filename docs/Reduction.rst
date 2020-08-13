Reduction
#########

Core
====

Stroscot takes after Haskell in that all of the language is compiled to a smallish core language. Considerations:

* Based on :cite:`downenSequentCalculusCompiler2016a`, we use the full two-sided sequent calculus with cuts instead of an intuitionistic or one-sided calculus.
* Based on optimal reduction, mostly :cite:`guerriniTheoreticalPracticalIssues1996`, we use linear logic sequents, with operators for contraction (duplication) and weakening (erasing).
* Based on :cite:`levyJumboLCalculus2006`, we aim for the largest allowable set of operators. In particular we generalize all of the different operators into two jumbo operators, sigma and pi. :math:`\Sigma` contains LL's synchronous/positive operators 0, 1, plus ⊕, and times ⊗. :math:`\Pi` contains LL's lollipop implication ⊸ and asynchronous/negative operators top ⊤, bottom ⊥, with &, and par ⅋.

We start with the generalized :math:`\Pi` rule. This is similar to Levy's rule except it allows multiple conclusions. We have indexed variables :math:`A_{ij}` and :math:`B_{ik}` where :math:`0 \leq i < N, 0 \leq j < m_i, 0 \leq k < n_i`. We call :math:`N` the length of the jumbo type and the list :math:`(m_i,n_i)` the jumbo-arity.

.. math::

    \newcommand{\rule}[3]{ \dfrac{\displaystyle ~~#1~~ }{\displaystyle ~~#2~~ } \  (#3)}
    \begin{array}{cc}
    \rule{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta }}
      {\Gamma \vdash \prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right), \Delta }
      {\Pi_R}
    &
    \rule{\overrightarrow{ \Gamma_j \vdash A_{i j}, \Delta_j } \quad \overrightarrow{ \Theta_k, B_{i k} \vdash \Lambda_k }}
      {\overrightarrow{\Gamma}, \vec \Theta, \prod \limits_{i} \left(\overrightarrow{A_i} \multimap \overrightarrow{B_i}\right) \vdash \overrightarrow{\Delta}, \vec\Lambda}
      {\Pi_{i} {}_{L}}
    \end{array}

Next we have the generalized :math:`\Sigma` rule. This allows premises as well. Following :cite:`wadlerCallbyvalueDualCallbyname2003` :cite:`crolardFormulaeasTypesInterpretationSubtractive2004` the dual of implication is called "subtraction" or "difference" and is denoted :math:`-`. For normal ADTs, the RHS of the difference is empty, i.e. it looks like :math:`A - []`. Ideally the difference operator allows creating zippers, or maybe it is useless.

.. math::

    \begin{array}{cc}
    \rule{\overrightarrow{ \Gamma_k, B_{i k} \vdash \Delta_k } \quad \overrightarrow{ \Theta_j \vdash A_{i j}, \Lambda_j } }
      {\overrightarrow{\Gamma}, \overrightarrow{\Theta} \vdash \sum \limits_{i} \left( \overrightarrow{A_i} - \overrightarrow{B_i} \right), \overrightarrow{\Delta}, \overrightarrow{\Lambda}}
      {\Sigma_{i} {}_{R}}
    &
    \rule{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta } }
      {\Gamma, \sum \limits_{i} \left ( \overrightarrow{A_i} - \overrightarrow{B_i} \right ) \vdash \Delta }
      {\Sigma_L}
    \end{array}

To allow/restrict contraction and weakening we have two S4 modalities, bang/!/"of course" and whim/whimper/?/"why not". A call-by-value function type is ``A -o ?B`` while call-by-name is ``!A -o B``. To enforce the S4 rules we add a level index to every term, as in :cite:`martiniFineStructureExponential1995` and :cite:`guerriniTheoreticalPracticalIssues1996`. The level of a context is the maximum of the levels of its terms (0 if empty). As a notational convention, the indices are omitted when they are all the same, i.e. in all the rules besides promotion and dereliction. Normally promotion has :math:`j=i+1` instead of :math:`j>i`, shrug.

.. math::

    \begin{array}{cccc}
      \rule{\Gamma^i \vdash A^j, \Delta^i }{\Gamma^i \vdash !A^i, \Delta^i}{!}_{j > i}
      & \rule{\Gamma^i, A^i \vdash \Delta^i }{\Gamma^i, !A^j \vdash \Delta^i}{!d}_{i\leq j}
      & \rule{\Gamma, !A, !A \vdash \Delta }{\Gamma, !A \vdash \Delta}{!c}
      & \rule{\Gamma \vdash \Delta }{\Gamma, !A \vdash \Delta}{!w}
    \end{array}

.. math::

    \begin{array}{cccc}
      \rule{\Gamma^i, A^j \vdash \Delta^i }{\Gamma^i, ?A^i \vdash \Delta^i}{?}_{j > i}
      & \rule{\Gamma^i \vdash A^i, \Delta^i }{\Gamma^i \vdash ?A^j, \Delta^i}{?d}_{i\leq j}
      & \rule{\Gamma \vdash ?A, ?A, \Delta }{\Gamma \vdash ?A, \Delta}{?c}
      & \rule{\Gamma \vdash \Delta }{\Gamma \vdash ?A, \Delta}{?w}
    \end{array}

To handle level mismatches we might also need lifting operators. The conditions are unclear.

.. math::

    \begin{array}{cc}
      \rule{\Gamma^i \vdash A^j, \Delta^i }{\Gamma^i \vdash A^i, \Delta^i}{\text{lift}_R}_{j > i}
      &
      \rule{\Gamma^i, A^j \vdash \Delta^i }{\Gamma^i, A^i \vdash \Delta^i}{\text{lift}_L}_{j > i}
    \end{array}

There are also quantifier rules, probably unnecessary but I'll write them down for reference. For these :math:`x` must have no free occurrence in :math:`\Gamma` or :math:`\Delta`, while :math:`y` may occur. :math:`A[t/x]` stands for the formula :math:`A` where all free occurrences of the variable :math:`x` have been replaced by the formula/term :math:`t` (and bound variables have been renamed when necessary).

.. math::

    \begin{array}{cccc}
      \rule{\Gamma \vdash A, \Delta}{\Gamma \vdash \forall x. A, \Delta}{\forall_R}
      &
      \rule{\Gamma, A[t/x] \vdash \Delta}{\Gamma, \forall x. A \vdash \Delta}{\forall_L}
      &
      \rule{\Gamma \vdash A[t/x], \Delta}{\Gamma \vdash \exists x. A, \Delta}{\exists_R}
      &
      \rule{\Gamma, A \vdash \Delta}{\Gamma, \exists x. A \vdash \Delta}{\exists_L}
    \end{array}

Finally we have the structural rules. Exchange isn't necessary because we assume multisets of formulas.

.. math::

    \begin{array}{cccc}
      \rule{}{A \vdash A}{\text{id}}
      &
      \rule{\Gamma \vdash A, \Delta \quad \Theta, A \vdash \Lambda }{\Gamma, \Theta \vdash \Delta, \Lambda }{\text{cut}}
      &
      \rule{\Gamma \vdash \Delta, A, B, \Theta}{\Gamma \vdash \Delta, B, A, \Theta}{\text{x}_R}
      &
      \rule{\Gamma, A, B, \Delta \vdash \Theta}{\Gamma, B, A, \Delta \vdash \Theta}{\text{x}_L}
    \end{array}

So in the end our propositions can be:

* A variable in the context
* Pi / Sigma
* A bang !A or whim ?A
* A forall or exists type, where the proposition is in the context extended by the variable

Syntax
======

Since proofs are programs by the Curry-Howard correspondence, we can use the rules as a programming language. But we need a syntax for it, since writing sequents all the time is tedious. The simplest, most explicit syntax enumerates the free variables (context/signature) of each sequent, all propositions, and all subderivations, for example ``Bang context Gamma Delta A subderivation``. But we can slim this down:

* The signature can be reconstructed by finding the free variables of the cedents (antecedent/succedent). We still need to add a type signature to each variable.
* The level indices can be recovered by a constraint solving (in particular a topological sort)
* The identified elements can be identified by variable names. Each variable occurs exactly twice. For identifying elements in the hypotheses we simple use the name, ``x``, ``y``, ``z``. For the conclusion, almost all the formulas have a single identified element in the conclusion, so we can use ``x = ...`` to identify it. For identity we assign names to both left and right, ``xl/xr = ...``.
* Gamma, delta, theta, and lambda can be inferred in most cases by taking the subderivations and removing the identified elements. Since they are unchanged we do not need to rename the variables. For PiRight/SigmaLeft we do need to rename and combine gamma/delta from each case (similar to a phi-node).
* Weakening and identity need a type argument ``T`` so we know the type of what's being introduced. Similarly the absurdity cases of PiRight/SigmaLeft (0 and top) need type annotations on Gamma/Delta. (But these can often be omitted/inferred like any other type signatures)
* For tags we use ``^i``, because Levy's notation ``#i`` is interpreted as a comment

::

  x/[Gamma_m]/[Delta_n] = PiRight
    {^i, [A_ij],[B_ik],[Gamma_mi],[Delta_ni] -> subderivation_i }
  x = PiLeft ^i [(Ai_j,left_subderivation_j)] [(Bi_k,right_subderivation_k)]
  x = SigmaRight ^i [(Bi_k,left_subderivation_k)] [(Ai_j,right_subderivation_k)]
  x/[Gamma_m]/[Delta_n] = SigmaLeft
    {^i, [A_ij],[B_ik],[Gamma_mi],[Delta_ni] -> subderivation_i }
  x = Bang A subderivation
  x = BangD A subderivation
  x = BangC A A subderivation
  x = BangW subderivation : T
  x = Whim A subderivation
  x = WhimD A subderivation
  x = WhimC A A subderivation
  x = WhimW subderivation : T
  x = LiftRight A subderivation
  x = LiftLeft A subderivation
  l/r = Identity : T
  Cut Al Ar subderivation_left subderivation_right

Example
=======

So let's look at a simple program, boolean "and":

::


  and = \x -> case x of { False -> \_ -> False; True -> \y -> y }
  and False True : Bool

We define the types :math:`\text{B} = \Sigma [(F,[],[]),(T,[],[])]` and :math:`a \to b = \Pi [(\text{func}, [a], [b])]`. :math:`\to` is right associative as usual. Our program then has the following derivation tree, among others (we could add a bang to the first argument, use a multiple-argument function, expand out the identity, etc.).

.. image:: _static/Stroscot_AND_Proof_Tree.svg
.. LaTeX Source is same path .tex (paste into Overleaf, pdf2svg)

Next is Core. We start with the rules, then assign variable names (alphabetically), then fill in the arguments, to obtain:

::

  # free variable pr : Bool
  Cut a n
    a = Bang b (b = SigmaRight ^True [] [])
    Cut lr m
      Cut c k
        c = SigmaRight ^False [] []
        Cut d j
          d/[]/[] = PiRight { ^func, [e], [f], [], [] ->
            e/[]/[f] = SigmaLeft {
              ^False, [], [], [], [f1] ->
                f1/[]/[] = PiRight { ^func, [g1], [h1], [], [] ->
                  g1 = BangW (h1 = SigmaRight ^False [] [])
                }
              ^True, [], [], [], [f2] ->
                f2/[]/[] = PiRight { ^func, [g2], [h2r], [], [] ->
                  g2 = BangD h2 (h2/h2r = Identity Bool)
                }
            }
          }
          j = PiLeft ^func [(kr, k/kr = Identity)] [(l, l/lr = Identity)]
      m = PiLeft ^func [(nr, n/nr = Identity)] [(p, p/pr = Identity)]

Nets
====

As a description the syntax is fine, but it suffers from what Girard calls "the bureaucracy of syntax". For example the cuts ``Cut a n`` and ``Cut lr m`` and the associated ``a``/``m`` can be swapped without changing the meaning. In fact, for computation, we do not need the syntactic subderivation inclusion relationship at all, only the variables. Furthermore, since each variable appears exactly twice, we can replace all the rule instances with nodes and variables with connecting edges. We thus obtain a graph, similar to a proof net / interaction net:

.. graphviz::

  digraph {
  Root -> c1 [style=invis]
  c1 -> c2 [style=invis]
  c2 -> c3 [style=invis]
  c3 -> c4 [style=invis]

  Root -> p /* pr */ [color="red"]
  c1 [label="Cut"]
  c1 -> a [color="red"]
  c1 -> n [color="blue"]
  a [label="!"]
  a -> b [color="red"]
  b [label="True"]
  c2 [label="Cut"]
  c2 -> l /* lr */ [color="red"]
  c2 -> m [color="blue"]
  c3 [label="Cut"]
  c3 -> c [color="red"]
  c3 -> k [color="blue"]
  c [label="False"]
  c4 [label="Cut"]
  c4 -> d [color="red"]
  c4 -> j [color="blue"]
  d [label="PiR"]
  d -> e [color="blue"]
  d -> e /* f */ [color="red"]
  e [label="SigmaL"]
  e -> f1 [color="red"]
  e -> f2 [color="red"]
  f1 [label="PiR"]
  f1 -> g1 [color="blue"]
  f1 -> h1 [color="red"]
  g1 [label="!w"]
  h1 [label="False"]
  f2 [label="PiR"]
  f2 -> g2 [color="blue"]
  f2 -> h2 /* h2r */ [color="red"]
  g2 [label="!d"]
  g2 -> h2 [color="blue"]
  h2 [label="Id"]
  j [label="PiL"]
  j -> k /* kr */ [color="red"]
  j -> l [color="blue"]
  k [label="Id"]
  l [label="Id"]
  m [label="PiL"]
  m -> n /* nr */ [color="red"]
  m -> p [color="blue"]
  n [label="Id"]
  p [label="Id"]
  }

Technically, the edges connect ports of nodes. The directionality and left=blue/right=red coloring is enough to identity the ports for the graph above, but disambiguating n-ary graphs in a clear way seems hard.

If we reverse the directions of the blue edges, then the graph is almost a tree, except for the backedges that can show up in PiRight/SigmaLeft.

Cut elimination
===============

* To eliminate cut with identity we remove both and link the unbound variables together
* To eliminate matching left/right Pi/Sigma, we erase all of the non-matching cases, directly link the context variables inside the matching case with the outer variables, and connect the captured variables to the other side with cut nodes.
* To eliminate !/!d or ?/?d, we remove them and insert a lift rule on one side to fix the levels, retaining the cut.
* To eliminate !/!c or ?/?c, we duplicate the rule with ! and create two cuts with the variables under the contraction.
* For !w/?w we delete the rule with ! and create no cuts.
* To eliminate :math:`\forall` or :math:`\exists`, we extend the variable substitution to the other side.
* The commuting cases are mostly handled by our graph formalism; cuts on the context are pushed down to where they apply. But for PiRight/SigmaLeft there can be a cut on the context variables. To push it down we need to duplicate the cut and its other side for each case.
* To expand identities, start with PiRight/SigmaLeft, then use the opposite rule on the opposite side for each case, and terminate with identities. But actually we should prefer to contract identities, searching for expanded identities and replacing them with identities on larger types.

Optimal reduction
=================

Optimal reduction ensures the minimal amount of cut elimination (reduction) steps. It's basically lazy evaluation, leftmost-outermost reduction order, but we have to be careful to avoid duplicating cuts. In particular, we want to avoid commuting under PiRight/SigmaLeft when there are two or more cases, and instead reduce the primary conclusions to identify the matching case. Also, for contraction, we want to avoid duplicating cuts, but also want to avoid doing unneeded reduction, so we create a duplicating node and incrementally push it down, doing reduction when needed instead of duplicating a cut.

For example, the term ``F2 G2 = (\x. x (\w. w) x) (\y. (\x. x x) y z)`` from page 18 of :cite:`aspertiOptimalImplementationFunctional1999`.

<insert reduction here>

Random old junk
###############

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

Implementation
==============

Reduction is fairly simple to implement without duplication, as it is just pairs of constructors and destructors annihilating and joining their wires, or, for ``case``, joining some eraser nodes. But what about duplication?

Stroscot takes its general inspiration from the delimiter system found in Lambdascope. However, instead of having levels Stroscot keeps explicit track of "environments" or "scopes". In particular a delimiter has an inside scope and an outside scope. Initially, all delimiters look like opening/closing delimiters where the outside scope is the default/root scope ``0`` and the inside scope is the scope of the multiplexer involved. When two delimiters meet, the touching outer scopes are compared for equality (they should always be equal) and one inner scope remains the inner scope while the other inner scope become the new delimiter's outer scope.

To determine which scope becomes the outer scope, delimiters are also marked as "head", "full", or "empty" depending on whether they represent a reference to the result of a duplication, the target of a duplication, or a path that crosses the scope but doesn't duplicate. Interactions are allowed only between head delimiters and other delimiter; the head delimiter's scope stays on the inside.

For multiplexers the situation is a little more complicated. A multiplexer also has two scopes, an inner "label"/identity-like scope and an outer "ambient" scope. When a multiplexer crosses a delimiter, from outside to inside, its "ambient" scope is changed to the delimiter's inside scope. Meanwhile the delimiter's scope is split into a new set of scopes, and this is indexed by the label scope. In the Stroscot code these are referred to as "variant" scopes. In particular, multiplexers with the same label scope must split other scopes into the same set of variant scopes at each interaction. This is not too hard to keep track of, just give each scope a map ``other scope -> variant scope set`` that's lazily created.

Readback
~~~~~~~~

The real hard part is doing "readback", i.e. proving that all of these transformations are either no-ops on the original lambda term or valid beta reductions. Since there is so much scope popping and pushing and varianting it is definitely a little complex. Also I decided to keep the levels from the original Lambdascope implementation for correctness verification purposes, so there is twice the work. But if you read Ian Mackie's paper on efficient interaction nets implementation you will see that this "readback" algorithm also provides a way to directly execute the proof net / graph with call-by-value semantics at each reduction step. So it also has a close connection with how to compile the graph to machine code. Since our scopes are unique and the "stack" stores only one director index for a scope at a time, the scopes can correspond directly to machine registers or memory locations.

Scopes are nested, forming a simple tree hierarchy. When an opening head delimiter of scope A encounters a closing full or empty delimiter of scope B, then A is reparented under B. In particular the one closing delimiter of scope B vanishes, and duplicates of it are propagated to the outside of all other delimiters of A.

It should be possible to avoid this ``O(n)`` operation by keeping a depth counter in the scope and each delimiter, with the total number of scopes that the delimiter enters/exit being the sum of the scope's depth and the delimiter's depth. Then the interaction would decrement the A scope delimiter's local depth counter by 1, increment A's scope-wide depth counter by 1, and vanish B's delimiter as before. But this hasn't been implemented yet.

