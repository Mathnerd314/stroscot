Imperative programming
######################

Limitations of purity
=====================

At present, destructive update is required to implement some algorithms efficiently. In particular consider some classes of language:

1. PURE: a "pure" CBV Lisp using a small set of primitive Lisp operations, ``ATOM EQ READ WRITE CONS CAR CDR`` assumed to be of constant cost, and "flow-chart style" control flow, assumed free
2. IMPURE: the Lisp extended with destructive mutation operators ``RPLACA RPLACD`` also of constant cost
3. HASK: a Haskell with lambdas, case, tuples, and lists
4. CYCLE: PURE but with an operation to construct cyclic data structures, CYCLE

It has been established that PURE <= CYCLE < HASK <= IMPURE as far as expressing efficient online programs.

For the first relation, PURE programs can be run unmodified in CYCLE with equivalent reduction steps, showing inclusion. :cite:`ben-amramNotesPippengerComparison1996` says that it is an open problem to demonstrate an advantage of CYCLE over PURE.

For the second relation, lazy languages allow cycles. :cite:`ben-amramNotesPippengerComparison1996` says that :cite:`pippengerPureImpureLisp1997` shows that for a specific online problem "perm" any CYCLE solution will require at least O(n log n) time. The proof depends on the property of CYCLE that a cons cell can refer only to itself or previously-constructed values. :cite:`birdMoreHasteLess1997` demonstrate that HASK can solve "perm" in amortized O(n) time, via the use of lazy streams, hence HASK is strictly more efficient than CYCLE.

For the third relation, it should be clear that the thunk mechanism of HASK can be emulated in IMPURE. :cite:`ben-amramNotesPippengerComparison1996` theorizes that for IMPURE programs following a read-update-write structure, there is a correspondingly efficient HASK program. Since Haskell 1.0 programs use a lazy stream ``[Response] -> [Request]`` for I/O, this read-update-write model seems to encompass all programs, hence it seems likely that the two languages are of equal efficiency, although nobody has formally proved this (TODO). But until then it is safer to assume HASK < IMPURE and provide destructive update facilities.

The log(n) gap is calculated using the cost of updating a balanced binary tree. This is the cost of the predecessor problem in the `pointer machine <https://en.wikipedia.org/wiki/Pointer_machine>`__. In the more accurate RAM model the update cost is optimally O(log log m) time under some assumptions. :cite:`strakaFullyPersistentArrays`

Automatic destructive update
============================

Although pure programs do not have operators for destructive update, they can still express similar programs using an update operation that traverses and copies the data. :cite:`hudakAggregateUpdateProblem1985` shows that with a compiler analysis a language can provide O(1) update operations. The compiler searches through possible evaluation orders for an evaluation order that never accesses the old version of data after updating, and transforms such "single-threaded" programs to destructively update, giving the speedup. Programming with pure arrays in a "single-threaded" style is at least as expressive as imperative arrays - per Hudak, all the natural translations of imperative algorithms are single-threaded. Some of :cite:`okasakiPurelyFunctionalData1998`'s lazy data structures have a similar single-threaded use condition for amortized good performance, so the single-threaded condition seems reasonable.

Haskell avoided automatic destructive update because it seemed too complicated, and instead relies on monads. Monadic style fixes an evaluation order hence guarantees single threading because the old version is inaccessible. Side effects like in Ocaml also are single-threaded. Clean has uniqueness types, which also enforce single threadedness. Uniqueness types disallow a simple example of implementing id in terms of const:

::

  id = const x x
  const x y = x

  a = array [1,2,3]
  b = id a
  b !! 10



Roc and Koka seem to be going down the automatic destructive update route via alias analysis and ref-counting optimizations.


Above we take streams as primitive and define continuations in
terms of them. Conversely, with some cleverness it is also possi-
ble to take continuations as primitive and define streams in terms
of them (see (Hudak and Sundaresh, 1989), where the definition
of streams in terms of continuations is attributed to Peyton Jones).
However, the definition of streams in terms of continuations was
inefficient, requiring linear space and quadratic time in terms of
the number of requests issued, as opposed to the expected constant
space and linear time. For this reason, Haskell 1.0 defined streams
as primitive, and continuations in terms of them, even though con-
tinuations were considered easier to use for most purposes.

State
=====

Conceptually the state of a program could include a lot of things, including the state of the computer, the stock market, quantum fluctuations, etc. - all information within the chronological past of a program. But since we are running on hardware we only care about the hardware's state, and since the hardware is all digital it is deterministic and expressible as a long binary string. This string would include the kernel and peripheral devices and other processes not related to ours. If we assume we are running as a user process then we can limit ourselves to the process state. Conveniently the CRIU project has a `list <https://criu.org/Images>`__ of what's in the state of a Linux process. We reproduce it here:

* Core process info

  * name, sigmask, itimers, etc.
  * Task credentials: uids, gids, caps, etc.
  * Process tree linkage
  * arch-dependent information (registers, etc.)
  * Signal handling map
  * IDs of objects (mm, files, sihand, etc.) and namespaces

* Address space information (VMAs, segments, exe file, etc.)

  * Info about which virtual regions are populated with data
  * 4k page data dumps that are to be put into the memory according to the pagemap.

* Filesystem info

  * chroot and chdir information
  * Open file descriptors
  * Paths to files opened with open(2) syscall
  * File paths remaps (e.g. for invisible files)
  * Ghost invisible files
  * Mountpoints information
  *	Contents of a tmpfs filesystem

* Special fd's / sockets

  * Eventfd file information
  * Eventpoll file information
  * Target file descriptors of eventpoll fds
  * Inotify file information
  * Watch descriptors of inotify fds
  * signalfd info
  * Pipes information
  * Contents of pipes (data sitting in a pipe)
  * FIFO information
  * Contents of FIFOs
  * Unix sockets
  * PF_INET sockets, both IPv4 and IPv6
  * Contents of socket queues
  * Interval timers state
  * TCP connection state (including data in queues)
  * Uname nodename and domainname of a UTS namespace
  * Information about opened TTYs, including Termios and similar stuff
  * Info about PF_PACKET sockets
  * Info about network devices
  * IP addresses on network devices
  * Routing tables

What operations are there on this state? Well, it is an aggregate value, so we can read and update fields to form a new state:

::

  readField : Field -> State -> Any
  setField : Field -> Any -> State -> State

But more interestingly we can load the state with CRIU (frozen), and attach a debugger. Let's assume we have symbols, then there are lots of operations available from a debugger:

* dump memory, disassemble memory, print backtrace, print call stack, evaluate (pure) expression in context
* patch executable, jump to address, return early from function, send signal
* run subset of threads until breakpoint (breakpoint can be syscall, call, return, signal injection, etc.)
* evaluate code in current context (e.g. set memory to value)

The dumping and patching are not too interesting as they are just extending the field get/set to language-specific data formats. But with the breakpoints, particularly by setting breakpoints on syscalls, we get a view of the program as an I/O machine:

::

  injectSysCallRet : [SysCallRet] -> State -> State

  runToTimeout : State -> Timeout -> (State, [SysCallReq])

  runSysCallsToSysCalls : [SysCallRet] -> State -> Timeout -> (State, [SysCallReq])
  runSysCallsToSysCalls r s t = runToTimeout (injectSysCallRet r s) t

There are multiple syscalls in flight because of multithreading. Technically we do not need ``[SysCallReq]`` because it can be determined from the state which threads are blocked on a syscall and what they requested, but the debugger knows this information from the breakpoint trap and it is clearer this way.

.. _tasks:

Tasks
=====

Tasks are a direct approach to I/O - sequences of I/O operations are values of type ``Task``, similar to a `free monad <https://www.reddit.com/r/haskell/comments/swffy/why_do_we_not_define_io_as_a_free_monad/>`__. Statements that don't return are directly of the Task type, like ``Exit { code : Int}``. Statements that continue in a sequential fashion have a ``continuation`` argument, like ``Print { s : String, continuation : Task }``, so are of type ``Command = Task -> Task``. Statements that return a value use a continuation of type ``a -> Task``, e.g. ``ReadFile { path : Fd, continuation : String -> Task}``, so are of type ``Operation a = (a -> Task) -> Task``. And since tasks are values we can also use them as arguments, like the ``delayed_task`` in ``SetTimeout { delay : Int, delayed_task : Task, continuation : Task}``.

To see how I/O works, consider printing hello world: ``print "Hi"``. As a task this looks like ``Print "Hi" exit``, where ``exit`` is what happens after (the continuation). The operation is ``print a = \cont -> Print a cont``. With the continuation as the last argument we can just use the partially-applied function, ``print = Print``. ``print a >> print b = \cont -> Print a (Print b cont)``. Now consider ``read ref >>= print``. The operation is ``Read ref >>= Print`` where ``>>=`` is the continuation monad's bind operation, which expands to ``\cont -> Read ref (\v -> Print v cont)``.

So conceptually the "Hello World" program is simply the value ``Print "Hello World" (Exit 0)``. Except print isn't a primitive operation, it's more like:

::

  Data "Hello, world!\n" (\msg ->
    Block "_start" [Sys_write stdout (addr msg) (length msg) (Sys_exit 0)])

with Stroscot's internal assembler language.

Task isn't really a monad, but we can compose operations that return values using the continuation monad's bind operation, as implemented with do-notation.

The datatype is similar to the "fudgets" mentioned in :cite:`erkokValueRecursionMonadic2002`, except we don't have a pure constructor. Or `this <http://comonad.com/reader/2011/free-monads-for-less-3/>`__ type ``FFI o i``, but with control flow represented explicitly instead of using ``o`` or ``i`` parameters.

I/O model showdown
==================


Continuations
-------------

Stroscot use continuations for its I/O model because continuations are simple and universal. They're the supercharged typed equivalent of a goto. A continuation is a function that takes as argument "the rest of the program", or "its future". Executing a continuation fills in a skeleton program with this future - or it can discard the future if it is not relevant. The implementation can compile continuations to jumps under most circumstances and closures otherwise, so the execution model is also conceptually simple.

Continuations are the basis in formal denotational semantics for all control flow, including vanilla call flow, loops, goto statements, recursion, generators, coroutines, exception handling, and backtracking. This allows a uniform and consistent interface.

We can turn the continuation approach into data by modeling I/O operations as constructor terms (members of a ``Task`` type). With this approach an I/O operation is data that can be pattern-matched over, allowing many metaprogramming techniques. It's a little harder for the compiler to optimize that readIORef has no observable side effects, as it's a reordering property (commutativity), but strict languages have been doing this for years.

Monads
------

Continuations are `the mother of all monads <http://blog.sigfpe.com/2008/12/mother-of-all-monads.html>`__ as all other monads can be embedded in the continuation type via ``m >>=`` and retrieved via ``f return``. In particular the Codensity monad ``Codensity m a = forall b. (a -> m b) -> m b`` is a monad regardless of ``m``. (`See comment <http://blog.sigfpe.com/2008/12/mother-of-all-monads.html#c3279179532869319461>`__) Without the forall, callcc is implementable and the type is too large, see :cite:`wadlerEssenceFunctionalProgramming1992` section 3.4 for an example.

Using the ``Codensity monad`` instead of a monad stack is often faster - the case analysis is pushed to the monad's operations, and there is no pile-up of binds. It converts the computation to continuation-passing style. In particular free tree-like monads :cite:`voigtlanderAsymptoticImprovementComputations2008` and `MTL monad stacks <http://r6.ca/blog/20071028T162529Z.html>`__ are much cheaper when implemented via Codensity. As a contrary point, in the `case <https://www.mail-archive.com/haskell-cafe@haskell.org/msg66512.html>`__ of the Maybe monad an ADT version seemed to be faster than a Church encoding. Unfortunately hpaste is defunct so the code can't be analyzed further. It's not clear if the "CPS" version mentioned is similar to Codensity.

Yoneda
------

`Kmett <http://comonad.com/reader/2011/free-monads-for-less-2/>`__ says to use ``Yoneda (Rec f)``, i.e. ``newtype F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }``, instead of ``Codensity f a``. The claim is that this type is "smaller" than Codensity in the sense that the inhabitants of ``F`` are in a one-to-one correspondence with those of ``Free f a``. But what we are interested in is ``f a``; the recursive layering actually adds extra inhabitants as well, and there is also the ``Pure`` constructor that doesn't make much sense for I/O. For example ``F Identity ()`` is the type of Church numerals, while ``Codensity Identity () = forall r. r -> r = () = Identity ()``. So in this case it is actually ``F`` that is larger.

Just looking at the types, F has more arrows. Similarly compare the instances:

::

  -- F f
  return a = F (\kp _ -> kp a)
  F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)

  -- C f
  return x = C (\k -> k x)
  m >>= k = C (\c -> runC m (\a -> runC (k a) c))

The instance for ``C`` is fewer characters.

Finally there is :cite:`rivasNotionsComputationMonoids2014` which derives the Codensity monad from the Yoneda lemma and the assumption that ``f`` is a small functor. Whereas the Yoneda-Rec seems to have no category theory behind it.

Generally it seems that the Yoneda thing solves a problem Stroscot doesn't have.

Multi-prompt delimited continuations
------------------------------------

Multi-prompt delimited continuations are described in :cite:`dyvbigMonadicFrameworkDelimited2007` . These might appear more expressive than standard delimited continuations ``Cont b a = (a -> b) -> b``, but as the paper shows, multi-prompt continuations can be implemented as a monad and hence as a library to use with the standard continuations. So the simplicity of the standard continuations wins out. With the multi-prompt continuations you have to have a unique supply and a stack. The unique supply complicates multithreading, and the stack can overflow and requires care to handle tail recursion. Whereas standard continuations translate to pure lambdas, and tail recursion is dealt with by the host language's semantics.

World token
-----------

Haskell uses a state monad ``IO a = s -> (# s, a #))`` for implementing I/O, where ``s = World`` is a special zero-sized token type. Clean is similar but ``s = *World`` has the uniqueness type annotation so the state tokens must be used linearly. Regardless, this approach seems quite awkward. Programs like ``(a,_) = getChar s; (b,s') = getChar s; putChar (a,b) s'`` that reuse the world are broken and have to be forbidden. Similarly commands like ``exit 0`` have to be modeled as returning a world token, even though they don't return at all. Ensuring that linearity holds during core-to-core transformations requires many hacks. Also, an I/O operation is an abstract function which makes it quite difficult to inspect IO values or implement simulations of I/O such as `PureIO <https://hackage.haskell.org/package/pure-io-0.2.1/docs/PureIO.html>`__.

Algebraic effects
-----------------

The two approaches are quite similar, both using a data type to represent operations. But continuations are much simpler syntactically than the handler functionality. In the effect approach computations are not first-class values.

OTOH effect types are quite useful, because you can define code that is polymorphic over the effect type, hence can be used as both pure and impure code. They use a monadic translation, I think with the lazy identity monad you can recover lazy pure code.

Call by push value
------------------

CBPV has "values" and "computations". The original presentation has these as separate categories, but :cite:`eggerEnrichedEffectCalculus2014` presents an alternative calculus EC+ where every computation type is also a value type. There is exactly one primitive that sequences computation, ``M to x. N``, which acts like the monadic bind ``M >>= \x -> N``, and similarly there is ``return``. And the evaluation is CBV. So stripping away the thunk stuff it seems to be a disguised version of monads. And the thunk stuff is a rather fragile way to implement CBN - it doesn't generalize to call by need. :cite:`mcdermottExtendedCallbyPushValueReasoning2019` And then there is jump-with-argument (JWA) which uses continuations and is equivalent to CBPV.

Applicative
-----------

All uses of Applicative can be rewritten using the laws to be of the form ``pure f <*> a <*> b ... <*> d`` (where ``<*>`` is left associative), hence all uses can be rewritten to the idiom bracket syntax. And the idiom bracket syntax ``([ f a b c ])`` can always be replaced with variadic function syntax, ``apply_thing f a b c``. So variadic functions are sufficient.

Applicative can also be represented typeclass-free as functions using their Cayley representation and the Yoneda lemma, see :cite:`rivasNotionsComputationMonoids2014` and `this email <https://fa.haskell.narkive.com/hUgYjfKJ/haskell-cafe-the-mother-of-all-functors-monads-categories#post3>`__.

::

  Rep f v = forall a. f a -> f (b,a)
  Yoneda f a = forall b. (a -> b) -> f b
  Applicative f a = Rep (Yoneda f) a
  pure : a -> Applicative f a
  (<*>) : Applicative f (a -> b) -> Applicative f a -> Applicative f b

  lift : (pure : a -> f a) -> ((<*>) : forall b. f (a -> b) -> f a -> f b) -> f a -> Applicative f a
  lower : Applicative f a -> f a

So every function ``Applicative f => f a -> f b -> ...`` can be replaced with ``Applicative f a -> Applicative f b -> ...`` - the normalization enabled by Cayley and Yoneda means you don't have to worry about instance coherency.

Async
-----

In JavaScript

::

  async function foo() {
    v = await f
    return g(v)
  }

translates to

::

  function foo() {
    return f().then(v => { return g(v) })
  }

The ``then`` operation is basically monadic bind, so this is another form of monad syntax. There are `inconsistencies <https://buzzdecafe.github.io/2018/04/10/no-promises-are-not-monads>`__ with the Monad laws due to Promise flattening, which are enshrined in the spec and `unfixable <https://github.com/promises-aplus/promises-spec/issues/94>`__ without creating a wrapper API. But ignoring those, the Promise type is something like ``Promise err a = Fulfilled a | Rejected err | Pending ({ resolve : a -> IO (), reject : err -> IO ()} -> IO ())``, which focusing on ``Pending`` is a CPS monad ``(Either err a -> IO ()) -> IO () = EitherT err (Cont (IO ())) a``.

Some arguments against:

* Promises do not conform to functor or monad laws and thus are not safe for compositional refactoring.
* JS promises allow execution after the promise is resolved or rejected, resulting in untraceable behavior (fixed in C# by using return/throw)
* async/await notation requires marking core library calls with "await" and the whole call chain with "async", a tedious syntactic burden that Bob Nystrom calls `function coloring <http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/>`__\ .

It's better to make the async behavior automatic. Zig has done this but has `tons of bugs <https://gavinhoward.com/2022/04/i-believe-zig-has-function-colors/>`__\ . Monads in general and continuations in particular seem like a more principled approach, e.g. there is a `JS CPS library <https://github.com/dmitriz/cpsfy/blob/master/DOCUMENTATION.md>`__\ .

Colored values
==============

Let's revisit Bob Nystrom's traits of `function coloring <http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/>`__\ . `tel on HN <https://news.ycombinator.com/item?id=8985436>`__ suggested using red = impure, and `Gavin <https://gavinhoward.com/2022/04/i-believe-zig-has-function-colors/#review-of-function-colors>`__ suggested replacing "call" with "use". Most of it is then about "impure functions", which we can call actions. We allow running actions in a pure environment if you provide an I/O simulation. With these modifications the traits read:

1. Values include pure functions and actions.
2. The way you use a value depends on its type.
3. You can only use an action from within another action, or within an action simulator.
4. Actions are more painful to use (than pure functions).
5. Some core library members are actions.

The only trait here that seems disadvantageous is 4. Nystrom lists the following pain points for JS:
* verbose to compose in expressions because of the callbacks / promise goop
* annoying hoops to use error-handling
* can’t be used with try/catch or inside a lot of other control flow statements.
* can't call a function that returns a future from synchronous code

But then he says C# async-await "solves" all of these but the first - actions can be used similarly to pure functions, but require "a liberal garnish of await". But he seems to feel that requiring await is a deal-breaker. He further says the real solution is "threads/goroutines/coroutines/fibers. more precisely: multiple independent callstacks that can be switched between." In fact it is not threads but *continuations* that make callstacks first-class. By using continuations as the I/O abstraction there is no distinction between sync and async, or rather it is all async. Particularly, all low-level operations are implemented in async style (taking a callback). You can still write sequential code in sync style, but for more complex cases you have to drop back down to the callbacks/continuation model or compose sequential code with combinators.

But this only solves the async/sync distinction Nystrom was complaining about, not the pure/impure dichotomy. Regardless of clever syntactic tricks, impurity cannot be hidden completely. Actions will always have some conceptual overhead compared to pure functions because they are sensitive to execution order. I don't know if this will make anyone "spit in your coffee and/or deposit some even less savory fluids in it", but I/O is unfortunately awkward in a pure or mathematical world. A program that does no I/O must be an infinite loop (it cannot even exit, because that requires a syscall). :cite:`jonesTacklingAwkwardSquad2001` classifies I/O under the "awkward squad".

"Unsafe" I/O
============

Haskell has ``runST`` and ``unsafePerformIO`` that allow turning impure computation into pure computations. These can be implemented by throwing a resumable exception that's caught in a top-level handler that does the I/O. ``runST`` scrutinizes its computation for impure behavior such as printing or returning allocated references, while ``unsafePerformIO`` does not and exposes the internal evaluation order.

If one wants to understand the evaluation order or is dealing with commutative operations, these functions are quite useful, e.g. Debug.Trace.trace looks like a non-I/O function but actually outputs something on the console, and allocation can be done in any order.

The main things to avoid is global variables like ``var = unsafePerformIO (newIORef 1)`` pattern. Implicit parameters initialized in main compose much better. Similarly C's ``static`` variables inlined in functions should be forbidden. Although, optimal reduction should mean an unsafePerformIO is only evaluated once, hence reading a file or something should be fine.