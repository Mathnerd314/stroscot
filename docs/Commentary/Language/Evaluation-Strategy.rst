Evaluation strategy
###################


This page summarizes the arguments for different types of evaluation strategies:

* pure vs impure - whether an expression has a denotational value, and how complex this denotation is
* strict vs non-strict - whether to allow a function to return a result without fully evaluating its arguments
* eager vs lazy - whether to make arguments non-strict by default
* call-by-need vs optimal - if arguments are non-strict, whether to evaluate once per closure or do deeper sharing

The quick summary is that optimal reduction is optimal, hence has better reduction and expressiveness properties than lazy or strict, but it is a complex strategy and in some cases there may be significant space overhead compared to strict due to graph reduction overhead, and there are also cases where the graph reduction overhead exceeds the runtime of the program, so programs can be slower with optimal reduction. To address this Stroscot will special-case optimization for C-like programs to give the expected performance.

"The next Haskell will be strict". - not necessarily. laziness may yet have a role as well.

Traditionally a function is only defined on values, but lazy evaluation allows functions to produce useful behavior for non-values as well.

Purity
======

SPJ says laziness offers a form of "hair shirt", an excuse to keep the language pure. The big benefit of wearing this shirt was the discovery of the benefits of purity. It is really great when the denotation of an integer is that integer, rather than a function or a tuple or whatever structure. Certainly some things like I/O do need more complex denotations, but by and large it is just a big conceptual win. Strict languages are often undisciplined in their use of effects and have unclear semantics given by "whatever the compiler does".

:cite:`jonesWearingHairShirt2003` concluded that laziness, in particular the purity that non-strictness requires, was jolly useful. I/O did cause prolonged embarrassment with a confusing variety of solutions (continuations, streams, monads) but Haskell has settled on monads and it is no longer a source of embarrassment.

In Haskell, the interaction of ``seq`` and inlining is the source of `numerous bugs <https://gitlab.haskell.org/ghc/ghc/issues/2273>`__. In contrast, optimal reduction is based on a principled approach to sharing - any reduction sequence in the sharing graph will not duplicate work.

Control constructs
==================

Non-strictness is required to define if-then-else and short-circuit functions, e.g. ``and c t = if c then t else False``. With strictness ``and false undefined`` evaluates its arguments first and throws even though its substitution does not. Another example is ``fromMaybe (error "BOOO") x``. Haskell has put non-strictness to good use with parser combinator libraries and so on.

Function composition
====================

Consider the ``any`` function, which scans the list from the head forwards and as soon as an element that fulfills the predicate is found it returns true and stops scanning the list, otherwise returns false. It's quite natural to express the ``any`` function by reusing the ``map`` and ``or`` functions, ``any p = or . map p``. All the functions involved need to be non-strict to get the desired semantics, processing the list in constant memory.

Unfortunately, it doesn't behave like we would wish in a strict language. The predicate ``p`` will be applied to every element before the ``or`` examines the elements, creating a new fully-evaluated intermediate list the size of the entire list, using lots of memory. To address this we have to expand out ``or``, ``map``, and ``foldr``, producing ``any p = \case { [] -> False; (y:ys) -> y || any p ys }``, invent a new version of foldr that delays the recursive call, or use a streaming abstraction. Either way function reuse becomes much harder.

Similarly there is ``within eps (improve (differentiate h0 f x))`` in :cite:`hughesWhyFunctionalProgramming1989`.

The related deforestation optimization removes all intermediate cons cells from the non-strict definition of ``any``, making it as efficient as the expanded strict version. In a strict language deforestation can have the effect of making an undefined program defined, hence is invalid. More careful handling of termination can fix this for strict programs (says a random comment in a blog post).

Lazy evaluation of ``avg xs = sum xs / length xs`` keeps the whole list in memory because it does the sum and then the length (`ref <https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/>__`). My implementation of optimal reduction switches evaluation back and forth between the sum and the length. More specifically, with the sequent calculus IR, cuts get pushed down continually and the natural strategy of reducing the topmost cut performs this alternation. So the average calculation can discard the beginning of the list once it is processed.

But although this case is improved, evaluating a thunk can still be delayed arbitrarily long, in particular it can take a while to discard an unused value.

Partial evaluation
==================

``snd (undefined,3)`` only works in a non-strict language - ``undefined`` would throw in a strict language. So strict languages must do strictness analysis to discard any code as unneeded.

:cite:`filinskiDeclarativeContinuationsCategorical1989` says the transformation from ``if e then (1,3) else (2,3)`` to ``(if e then 1 else 2, 3)`` is valid in a strict language, but not in a non-strict language, because ``e`` could diverge. But this example is rather fragile, e.g. the transformation from ``3`` to ``if e then 3 else 3`` is invalid in strict and lazy, but only if ``e`` can diverge. And as Conal `writes <http://conal.net/blog/posts/lazier-functional-programming-part-2>`__ we can define a laxer pattern match which allows the transformation in all cases, ``ifThenElse c a b = (a glb b) lub (\True -> a) c lub (\False -> b) c``.

Overall, a win for non-strictness, and an argument for lax pattern match semantics and termination checking.

Totality
========

In a total language all evaluation strategies give the same result. But since in particular, strict evaluation must work, totality gives up all the benefits of non-strictness - exceptions and failing conditionals are simply forbidden. Meanwhile the actual evaluation strategy is compiler-specified. In practice, the strategy still has to be decided (e.g. Idris is strict, Agda/Coq have both strict and call-by-need backends), so this doesn't resolve the question. The number of times an expression is evaluated is still observable via the performance.

Conclusion: totality is a compromise that means the worst of strict and non-strict, and in practice is a Trojan horse for strictness.

Simulation
==========

To emulate non-strict argument passing in a strict language, there are three options:

* don't modify the code and just see if it works: can lead to non-termination, slowdowns, and space leaks. For example anything with infinite lists will break as it tries to construct the infinite list.
* call-by-name: Pass expressions as thunks ``\() -> e``. Augustss has called this `"too ugly to even consider" <http://augustss.blogspot.com/2011/05/more-points-for-lazy-evaluation-in.html>`__, but fortunately many languages have introduced special support for wrapping arguments as thunks, such as Swift's lightweight closure syntax ``{e}`` and annotation ``@autoclosure``, and Scala's automatic call-by-name types, ``(\(x : CallByName Int) -> x + x) e``. Passing thunks removes nontermination / infinite evalation loops but can still introduce slowdowns and space leaks as expressions are evaluated multiple times.
* Thunk data type: To fully mimic call-by-need semantics, a new type ``Thunk a = Var (Evaluated a | Unevaluated (() -> a))`` can be introduced with operations force/delay. Then one does ``(\x -> force x + force x) (delay e)``. There is a lot of syntactic overhead, but it is a faithful emulation.

Generally, ignoring orthogonal aspects such as the handling of side effects, there is no issue with using non-strict argument passing with a program written with strict semantics in mind; the program can simply be used unmodified. It will have the same semantics in normal conditions and possibly terminate without error in conditions where the strict version would loop infinitely. Slowdown and space leaks are possible issues, though not non-termination. Efficiency can be recovered by compiler optimizations that add back strictness.

Lazy reduction can be simulated in a strict language using thunks, but the sharing graph of optimal reduction is intrusive, so one would have to represent functions via their AST. I guess it could be done. Generally, the issue is that optimal reduction is complicated. Although all of strict, lazy, and optimal reduction can be modeled as graph reduction, optimal reduction uses a more complex graph.

Conclusion: Non-strictness wins in terms of simulation usability (use programs as-is). Performance-wise, practically, both directions of simulation can introduce slowdown and space leaks. With invasive syntax and careful design, strict can simulate non-strict without overhead.

Data structures
===============

Laziness allows writing certain amortized data structures, as per :cite:`okasakiPurelyFunctionalData1998`.
It also allows defining infinite data structures, e.g. ``omega = Succ omega`` or the Fibonacci stream, that have finite time and memory usage if only a part of the data is used. These are hard to replicate in strict code except via thunk simulation. When analyzing performance, the explicit simulation makes the data structures clearer and easier to analyze, but analyzing core IR of a lazy language should provide the same benefit.

A strict, imperative stream (iterator) is one where reading from the stream is an operation ``next : Stream -> Op (Nil | Cons a Stream)``. It is not the same as a lazy stream - accessing elements does I/O, not just pure reduction of thunks. Iterators are ephemeral data structures (objects). An iterator can be turned into a pure data structure by reading it to exhaustion, or buffered using a thunk-like data structure to create a fake-lazy abstraction that still uses I/O but allows pure access to previous elements. Regardless, iterators can be implemented in a lazy langauge as well using an I/O monad, with little overhead.

Normalizing
===========

Laziness has the joyous property that you can write down any cyclic rubbish and get a value out if there's any sensible evaluation order.

Strict order can evaluate unnecessarily, so it can fail needlessly if there is an expression that errors when evaluated in the wrong conditions, e.g. ``a`` in ``r where a = b / c; r = if c != 0 then a else 0``.

Time complexity
===============

Regarding (sequential) time complexity, lazy reduction uses at most as many reduction steps as the corresponding strict reduction. Lazy corresponds to strict extended with an oracle that skips evaluation of unneeded terms. :cite:`hackettCallbyneedClairvoyantCallbyvalue2019`

Also the cost of each reduction step is about the same. Consider for example this program:

::

  bar a b = a * b

  foo :: Int -> Int -> Int -> Int
  foo x y z = let u = bar y z in x + u

In Java the overhead of the bar function call is two argument pushes, the call itself, and the return.
GHC (without optimization) compiles this code as something like the following pseudocode:

::

  foo [x, y, z] =
      u = new THUNK(sat_u)                   // thunk, 32 bytes on heap
      jump: (+) x u

  sat_u [] =                                 // saturated closure for "bar y z"
      push UPDATE(sat_u)                     // update frame, 16 bytes on stack
      jump: bar y z

  bar [a, b] =
      jump: (*) a b

The overhead of the lazy bar function call is the creation of a thunk on the bump heap (as fast as stack) that includes two arguments and a pointer to sat_u (plus room for the return value, though there's no "cost" for this), and a "call" (not visible in the above code) when the (+) function forces the value u by jumping to sat_u. The update frame more or less replaces the return. (In this case, it can be optimized away.) Hence the function call is shifted in time but the overhead in terms of pseudo-instruction count is not significantly increased.

So big-O time complexity is within a constant factor. In practice the constant factor is quite important; cache locality and memory access times play a large role in speed. There is some memory fetching overhead with laziness because by the time the thunk is evaluated all of its references may have gone cold.

Although thunks prevent some forms of duplication, and thus can give speedups, lazy reduction still duplicates work. An example is

::

  import System.IO.Unsafe
  i = \w -> (unsafePerformIO (print "i")) `seq` w
  z = 2 :: Integer
  t = 3 :: Integer
  f = \x -> (x z) + (x t)
  main = print (f i :: Integer)

Without GHC's optimizations, ``print "i"`` is evaluated twice. With ``-O`` GHC does a "hoisting" transformation that makes ``i = (unsafePerformIO (print "i")) `seq` \w -> w``. But it doesn't optimize another example:

::

  foo 0 = 1
  foo n =
    let a = \x -> (foo (n - 1))
    a r + a s

Without optimizations, this is exponential with lazy, vs linear with optimal. The reason is that with optimal reduction, sub-expressions of function bodies are shared between calls. In particular, the only time an expression is re-evaluated is when it depends on the arguments. Again with ``-O`` this improves: GHC inlines ``a`` and does CSE, giving ``foo n = let a = foo (n-1) in a + a``.

However, there should more complex cases have higher-level sharing that no GHC code transformation mimics. TODO: find some.

Regarding optimal evalation, there are some results:

* Optimal reduction has exponential savings over lazy evaluation when evaluating Church numeral exponentiation. :cite:`aspertiBolognaOptimalHigherorder1996`
* The optimal non-family reduction sequence is uncomputable for the lambda calculus (best known is essentially a brute force search over all reduction sequences shorter than leftmost-outermost reduction), while the optimal family reduction is simply leftmost-outermost.
* For elementary linear lambda terms the number of sharing graph reduction steps is at most quadratic compared to the number of leftmost-outermost reduction steps. :cite:`guerriniOptimalImplementationInefficient2017` Actually my implementation avoids bookkeeping and fan-fan duplication and hence is linear instead of quadratic (TODO: prove this). It would be nice to have a bound of optimal graph reduction steps vs. call-by-value (strict) steps but I couldn't find one. I think it is just the same quadratic bound, because lazy is 1-1 with strict.
* A simply-typed term, when beta-eta expanded to a specific "optimal root" form, reduces to normal form in a number of family reduction steps linearly proportional to the "size" of the term ("size" is defined in a way polynomially more than its number of characters). Since the simply typed terms can compute functions in ℰ4\\ℰ3 of the Grzegorczyk hierarchy with linear size (Statman), one concludes there is a sequence of terms which reduces in a linear number of family reductions but takes ℰ4 time to compute on a Turing machine, for any implementation of family reduction. In particular there are terms taking optimal graph reduction steps proportional to the iterated exponential of 2 to the size of the term, i.e. :math:`2^{2^{2^n}}` for any number of 2's. :cite:`coppolaComplexityOptimalReduction2002`


Implementation complexity
=========================

Compiling a subset of C is succinct, 2048 bytes for the `obfuscated tiny C compiler <https://bellard.org/otcc/>`__. It's essentially a macro assembler - each operation translates to an assembly sequence that uses the stack.
I can make a similar compiler for STG (lazy functional language) with a similar macro translation - I'd just need to write a GC library as heap allocation is not built into the hardware, unlike stack allocation. Meanwhile production-quality compilers (GCC, clang/LLVM) are huge and do so many code transformations that the original code is unrecognizable. Similarly GHC is huge. So strict languages don't really fit the hardware any better than lazy - they're both significant overhead for naive translations and huge compilers to remove that overhead.

Space complexity
================

The space complexity is very messy in a lazy language, whereas the stack in a strict language is predictable. For example, lazily evaluating the definition ``sum = foldl (+) 0``, unevaluated addition thunks pile up and are only forced at the end, hence the sum operation takes O(n) memory.

GHC's demand analysis works for ``sum``, but is still incomplete. Haskell has added workarounds "seq", the Strict Haskell extension, and bang markers, so strictness can be specified as part of the program. But this is not a solution - it means every basic function must come in several strictness variants.

Space leaks in particular are hard to spot. The difficulty lies in characterizing the evaluatedness of arguments being passed around. R fully evaluates expressions in a number of places which helps a lot, but there is still a lot of code that manually calls ``force`` and ``force_all`` to remove laziness, and each omission is a potential slowdown. And of course all this forcing means there are few libraries taking advantage of laziness. :cite:`goelDesignImplementationUse2019`

Debugging
=========

For debugging the logic, lazy and strict evaluation can both be modeled as term reduction, so it's just a matter of tracking the term being reduced. The logic that tracks lazy reduction state is more complex, hence is harder to show alongside the term, but not impossibly so.

Parallelism and concurrency
===========================

Parallel execution is slightly better in a strict language, because expressions are known to be evaluated and can be immediately sent off to a worker thread. Lazy evaluation requires proving or waiting for demand which can be slow. But lenient evaluation is non-strict and eager, and gives more parallelism than either strict or lazy. Even more parallelism can be obtained from speculative execution.

Concurrency is bound up with I/O operations, which are sequential, so the evaluation strategy doesn't have any room to play a role.

Types
=====

In Ocaml, a simple list type ``List Nat`` is guaranteed to be finite. In Haskell, a list type ``List Nat`` instead accepts infinite lists like ``fib = [1,1,2,3,...]``. In the denotational semantics, however, infinite lists are still values. So we should be able to define types independent of the evaluation semantics, i.e. have both finite and infinite types in both strict and lazy languages.

With strict languages, using the thunk simulation one gets a natural "thunk" constructor that marks infinite structures. So uList. (Nat + Thunk List) is an infinite list, while uList. (Nat + List) is a finite list, and this extends to more complicated data structures. With a subtyping coercion ``Thunk x < x`` one could use a finite list with an infinite list transformer, but it is not clear how to add such a coercion.

With lazy languages, GHC has developed "strictness annotations" which seem about as useful. So uList. (Nat + List) is an infinite list, while uList. (Nat + !List) is a finite list. There is an alternate convention implied by StrictData which uses ``a`` to denote values of type a and ``~a`` to denote expressions evaluating to type ``a``.

Pipes
=====

One practical case where laziness shows up is UNIX pipes. For finite streams the "strict" semantics of pipes suffices, namely that the first program generates all of its output, this output is sent to the next program, which generates all of its output that is then sent to the next program, etc., until the output is to the terminal. Most programs have finite output on finite input and block gracefully while waiting for input, so interleaved execution or laziness is not necessary.

However, for long outputs, interleaved or "lazy" execution saves memory and improves performance dramatically. For example with ``cat large_file | less``, ``less`` can browse a bit without loading the file into memory. It is really just a generalization that infinite streams like ``yes fred | less`` work. Of course interleaving is not magic, and not all programs support interleaving. For example, ``cat large_file | sort | less`` is slow and ``yes fred | sort | less`` is an infinite loop, because ``sort`` reads all its input before producing any output.

But laziness means you can implement interleaving once in the language (as the evaluation strategy) as opposed to piecemeal for each program.

Referential transparency
========================

Common subexpression elimination "pulls out" a repeated expression by giving it a fresh name and generally improves performance by sharing the result (although it could be a tie with the compiler inlining the expression again if it is really cheap). For instance ``e + e`` is the same as ``(\x -> x + x) e``, but in the second ``e`` is only evaluated once.

In a strict language this transformation can only be performed if the expression is guaranteed to be evaluated. E.g. ``if c then undefined else f`` to ``let e = undefined in if c then e else f``, the second version always evalautes ``e`` and throws ``undefined`` whereas the original could succeed with ``f``. This is a form of speculative execution hazard.

In a lazy language, this can be performed unconditionally because the expression will not be evaluated if it is not used. Similarly adding or removing unused expressions does not change the semantics, ``e`` versus ``let x= y in e``. Nontermination has the semantics of a value.

A win for laziness.


Non-strict arguments are passed as computations, so they can include non-terminating computations, whereas in a strict language arguments are evaluated values. But when we actually use a value it gets evaluated, so these computations resolve themselves. There is no way in a lazy language (barring runtime reflection or exception handling) to observe that an argument is non-termination as opposed to a real value, i.e. to make a function ``f _|_ = 0, f () = 1``. So stating that non-termination or ``undefined`` is a value in lazy languages is wrong. Similarly ``Succ undefined`` is not a value - it is WHNF but not normal form. These are programs (unevaluated expressions) that only come up when we talk about totality. Some people have confused the notions of "value" and "argument" in lazy languages. The term "laziness" has a lot of baggage, perhaps it is better to market the language as "normal order".


Sharing strategies with non-strictness don't extend to ``while``, because the condition and body must be evaluated multiple times. So more generally for iteration constructs we need `call by name <https://docs.scala-lang.org/tour/by-name-parameters.html>`__, macros, fexprs, or monads.

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

Hence this is only a partial win for laziness.
