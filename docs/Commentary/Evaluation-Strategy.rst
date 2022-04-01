Evaluation strategy
###################


This page summarizes the arguments for strict vs lazy evaluation and lazy vs optimal evaluation. The quick summary is that optimal reduction is optimal, hence has better reduction and expressiveness properties than lazy or strict, but it is a complex strategy and in some cases there may be significant space overhead compared to strict due to graph reduction overhead, and there are also cases where the graph reduction overhead exceeds the runtime of the program, so programs can be slower with optimal reduction. To address this C-like programs have a special-cased optimization to give the expected performance.

Strict vs lazy
==============

Referential transparency
------------------------

In a lazy language any subexpression can be named and "pulled out", modulo name capture. For instance ``e + e`` is the same as ``(\x -> x + x) e``. This gives common subexpressions a name (common subexpression elimination). In a strict language this transformation is invalid, e.g. with ``e = undefined`` the CSE'd version throws even if ``e`` was not originally evaluated.

In a lazy language, the transformation ensures ``e`` will be evaluated exactly zero times or once, hence improving performance (although it could be a tie with the compiler de-optimizing and splitting ``e`` again if it is really cheap). In a strict language ``e`` will always be evaluated hence it is not an optimization if ``e`` could be skipped in some cases.

Control constructs
------------------

Laziness allows defining new control constructs, e.g. ``and c t = if c then t else False``, similar to macros but without explicit AST manipulation. With strictness ``and false undefined`` throws even though its substitution does not. Another example is ``fromMaybe (error "BOOO") x``.

But this doesn't work for ``while``, because the condition and body must be evaluated multiple times. What we want is `call by name <https://docs.scala-lang.org/tour/by-name-parameters.html>`__.

::

  while condition body =
    c <- condition
    if c then
      body
      while condition body
    else return ()

  i = 2
  while (i > 0) {
    println(i)
    i -= 1
  }

Fexprs provide a complete solution for this.

Function composition
--------------------

Consider the ``any`` function, which scans the list from the head forwards and as soon as an element that fulfills the predicate is found it returns true and stops scanning the list, otherwise returns false. It's quite natural to express the ``any`` function by reusing the ``map`` and ``or`` functions, ``any p = or . map p``. All the functions involved need to be lazy to get the desired semantics, processing the list in constant memory.

Unfortunately, it doesn't behave like we would wish in a strict language. The predicate ``p`` will be applied to every element before the ``or`` examines the elements, creating a new fully-evaluated intermediate list the size of the entire list, using lots of memory. To address this we have to expand out ``or``, ``map``, and ``foldr``, producing ``any p = \case { [] -> False; (y:ys) -> y || any p ys }``, invent a new version of foldr that delays the recursive call, or use a streaming abstraction. Either way function reuse becomes much harder.

Similarly there is ``within eps (improve (differentiate h0 f x))`` in :cite:`hughesWhyFunctionalProgramming1989`.

The related deforestation optimization removes all intermediate cons cells from the lazy definition of ``any``, making it as efficient as the expanded strict version. In a strict language deforestation can have the effect of making an undefined program defined, hence is invalid. More careful handling of termination can fix this for strict programs (says a random comment in a blog post).

Partial evaluation
------------------

``snd (undefined,3)`` only works in a lazy language - ``undefined`` would throw in a strict language. So strict languages must do strictness analysis to discard any code as unneeded.

:cite:`filinskiDeclarativeContinuationsCategorical1989` says the transformation from ``if e then (1,3) else (2,3)`` to ``(if e then 1 else 2, 3)`` is valid in a strict language, but not in a lazy language, because ``e`` could diverge. The point is that some transformations are valid in a strict language but in a lazy language need a divergence analysis. But this example is rather fragile, e.g. the transformation from ``3`` to ``if e then 3 else 3`` is invalid in strict and lazy, but only if ``e`` can diverge. And as Conal `writes <http://conal.net/blog/posts/lazier-functional-programming-part-2>`__ we can define a laxer pattern match which allows the transformation in all cases, ``ifThenElse c a b = (a glb b) lub (\True -> a) c lub (\False -> b) c``.

Totality
--------

In a total language all evaluation strategies give the same result, so referential transparency and function composition hold. But totality gives up all the benefits of laziness w.r.t. infinite data structure expressiveness. Meanwhile the actual evaluation strategy is compiler-specified. In practice, this strategy still has to be decided (e.g. Idris is strict, Agda/Coq have both strict and lazy backends), so this doesn't resolve the question. The number of times an expression is evaluated is still observable via the performance.

Arguments in a lazy language are passed as computations, so they can include non-terminating computations, whereas in a strict language arguments are evaluated values. But when we actually use a value it gets evaluated, so these computations resolve themselves. There is no way in a lazy language (barring runtime reflection or exception handling) to observe that an argument is non-termination as opposed to a real value, i.e. to make a function ``f _|_ = 0, f () = 1``. So stating that non-termination or ``undefined`` is a value in lazy languages is wrong. Similarly ``Succ undefined`` is not a value - it is WHNF but not normal form. These are programs (unevaluated expressions) that only come up when we talk about totality.

Simulation
----------

One can implement call-by-name in a strict language by doing ``(\x -> x () + x ()) (\_ -> e)``, but if ``e`` is expensive this will evaluate ``e`` twice.
Thunks can implement lazy computation, have ``Thunk a = Var (Evaluated a | Unevaluated (() -> a))`` and do ``(\x -> force x + force x) (delay (\() -> e))``. But there is syntactic overhead - even ignoring force/delay, ``e`` must be passed as a function to avoid evaluating it. This is too ugly to even consider. Perhaps ``{e}`` for a thunk, or Scala's call-by-name type syntax, ``(\(x : CallByName Int) -> x + x) e``.

Evaluating lazy code in a strict language can lead to non-termination, slowdowns, and space leaks. In contrast there is no non-termination possibility to interpreting strict programs in a lazy language. But slowdown and space leaks are still an issue.

Data structures
---------------

Laziness allows writing certain amortized data structures, as per :cite:`okasakiPurelyFunctionalData1998`.
It also allows defining infinite data structures, e.g. ``omega = Succ omega`` or the Fibonacci stream. These are hard to replicate in strict code except via simulation. Arguably the simulation makes the amortized data structures clearer and easier to analyze.

A strict, imperative stream (iterator) is one where reading from the stream is an operation ``next : Stream -> Op (Nil | Cons a Stream)``. It is not the same as a lazy stream - accessing elements does I/O, not just pure reduction of thunks. Iterators are ephemeral data structures (objects). An iterator can be turned into a pure data structure by reading it to exhaustion, or buffered using a thunk-like data structure to create a fake-lazy abstraction that still uses I/O but allows pure access to previous elements. Regardless, iterators can be implemented in a lazy langauge as well using an I/O monad, with little overhead.

Normal order
------------

Laziness has the joyous property that you can write down any old cyclic rubbish and get a value out if there's any sensible way to resolve the data dependencies. Strictness is much more strict.

Time complexity
---------------

Regarding (sequential) time complexity, lazy reduction uses at most as many reduction steps as the corresponding strict reduction. Lazy corresponds to strict extended with an oracle that skips evaluation of unneeded terms. :cite:`hackettCallbyneedClairvoyantCallbyvalue2019`

Consider for example this program:

::

  bar a b = a * b

  foo :: Int -> Int -> Int -> Int
  foo x y z = let u = bar y z in x + u

In Java the overhead of the bar function call is two argument pushes, the call itself, and the return.
GHC (without optimization) compiles this code as something like the following pseudocode:

foo [x, y, z] =
    u = new THUNK(sat_u)                   // thunk, 32 bytes on heap
    jump: (+) x u

sat_u [] =                                 // saturated closure for "bar y z"
    push UPDATE(sat_u)                     // update frame, 16 bytes on stack
    jump: bar y z

bar [a, b] =
    jump: (*) a b

The overhead of the lazy bar function call is the creation of a thunk on the bump heap (as fast as stack) that includes two arguments and a pointer to sat_u (plus room for the return value, though there's no "cost" for this), and a "call" (not visible in the above code) when the (+) function forces the value u by jumping to sat_u. The update frame more or less replaces the return. (In this case, it can be optimized away.)

Hence the overhead in terms of pseudo-instruction count is about the same. The function call is shifted in time but the cost is not significantly increased. However in practice cache locality and memory access times play a large role in speed and by the time the thunk is evaluated all of its references may have gone cold.

Implementation complexity
-------------------------

Compiling a subset of C is succinct, 2048 bytes for the `obfuscated tiny C compiler <https://bellard.org/otcc/>`__. It's essentially a macro assembler - each operation translates to an assembly sequence that uses the stack.
I can make a similar compiler for STG (lazy functional language) with a similar macro translation - I'd just need to write a GC library as heap allocation is not built into the hardware, unlike stack allocation. Meanwhile production-quality compilers (GCC, clang/LLVM) are huge and do so many code transformations that the original code is unrecognizable. Similarly GHC is huge. So the argument that strict languages fit the hardware better than lazy is weak.

Space complexity
----------------

The space complexity is very messy in a lazy language, whereas the stack in a strict language is predictable. For example, lazily evaluating the definition ``sum = foldl (+) 0``, unevaluated addition thunks pile up and are only forced at the end, hence the sum operation takes O(n) memory.

GHC's demand analysis works for ``sum``, but is still incomplete. Haskell has added workarounds "seq", the Strict Haskell extension, and bang markers, so strictness can be specified as part of the program. But this is not a solution - it means every basic function must come in several strictness variants.

Space leaks in particular are hard to spot. The difficulty lies in characterizing the evaluatedness of arguments being passed around.

Debugging
---------

For debugging the logic, lazy and strict debugging can both be modeled as term reduction, so it's just a matter of tracking the term being reduced. The logic that tracks lazy reduction state is more complex, but not impossibly so.

Parallelism
-----------

Parallel execution is bound up with I/O operations, which are sequential, so the evaluation strategy doesn't have any room to play a role.

Purity
------

Laziness offers a form of "hair shirt", an excuse to keep the language pure. Strict languages are undisciplined in their use of effects.

Lazy vs optimal
===============

Optimal reduction is similar to lazy reduction in that the evaluation loop computes a "needed" redex and reduces it. It differs in that it can share the bodies of lambda abstractions. It's optimal in the sense that it ensures the minimal amount of family reduction steps. but this does not imply the fastest real-world performance.

Sharing
-------

Although thunks prevent some forms of duplication, lazy reduction still duplicates work. An example is

::

  import System.IO.Unsafe
  i = \w -> (unsafePerformIO (print "i")) `seq` w
  z = 2 :: Integer
  t = 3 :: Integer
  f = \x -> (x z) + (x t)
  main = print (f (\y -> i y) :: Integer)

This produces ``5`` in Haskell. However, without GHC's optimizations, ``"i"`` is evaluated (printed) twice. With optimal reduction, all function applications with known arguments are evaluated exactly once. In particular, the only time a function is evaluated twice is when it is called with different arguments. In the example above it corresponds to a "hoisting" transformation that makes ``i = (unsafePerformIO (print "i")) `seq` \w -> w``. Although GHC will do this with ``-O``, it does it messily; the interaction of ``seq`` and inlining is the source of `numerous bugs <https://gitlab.haskell.org/ghc/ghc/issues/2273>`__. More complex cases have higher-level sharing that no GHC code transformation mimics. For example consider:

::

  id a = a
  dbl g = g (g id)
  y h = dbl (\w -> h (w id))
  x = dbl y

In contrast, optimal reduction is based on a principled approach to sharing. The graph corresponds almost exactly to linear logic proof nets. Also, since the sharing is part of the reduction semantics rather than a compiler optimization, it is available in the interpreter (and in the runtime system too). There are no thunks, so there is no need for ``seq``; instead there are boxes and duplicators.

Better composition
------------------

Lazy evaluation of ``avg xs = sum xs / length xs`` keeps the whole list in memory because it does the sum and then the length (`ref <https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/>__`). My implementation of optimal reduction switches evaluation back and forth between the sum and the length. More specifically, with the sequent calculus IR, cuts get pushed down continually and the natural strategy of reducing the topmost cut performs this alternation. So the average calculation can discard the beginning of the list once it is processed.

But although this case is improved, evaluating a thunk can still be delayed arbitrarily long, in particular it can take a while to discard an unused value.

Complicated
-----------

Lazy reduction can be simulated in a strict language using thunks, but the sharing graph of optimal reduction is intrusive, so one would have to represent functions via their AST. I guess it could be done. Generally, the issue is that optimal reduction is complicated. Although both lazy and optimal can be modeled as graph reduction, optimal reduction uses a more complex graph.

Complexity
----------

* Optimal reduction has exponential savings over lazy evaluation when evaluating Church numeral exponentiation.
* The optimal non-family reduction sequence is uncomputable for the lambda calculus (best known is essentially a brute force search over all reduction sequences shorter than leftmost-outermost reduction), while the optimal family reduction is simply leftmost-outermost.
* For elementary lambda terms the number of sharing graph reduction steps is at most quadratic compared to the number of leftmost-outermost reduction steps. :cite:`guerriniOptimalImplementationInefficient2017` Actually my implementation avoids bookkeeping and fan-fan duplication and hence is linear instead of quadratic (TODO: prove this). It would be nice to have a bound of optimal graph reduction steps vs. call-by-value (strict) steps but I couldn't find one. I think it is just the same quadratic bound, because lazy is 1-1 with strict.
* A simply-typed term, when beta-eta expanded to a specific form, reduces to normal form in a number of family reduction steps linearly proportional to the "size" of the term ("size" is defined in a way polynomially more than usual). Since the simply typed terms can compute functions in E4 but not E3 (i.e. strictly E4), one concludes there are terms that will take strictly E4 time to compute on a Turing machine, for any implementation of family reduction. In particular there are terms taking graph reduction steps proportional to the iterated exponential of 2 to the size of the term, i.e. :math:`2^{2^{2^n}}` for any number of 2's.

