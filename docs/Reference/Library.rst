Library
#######

Stroscot will support the standard libraries of other languages, so e.g. if you want the C functions with C semantics you would ``import Library.C``. Compatibility is a natural step to world domination.

Standard libraries:
* `Rust <https://github.com/rust-lang/rust/tree/master/library>`__ (MIT + Apache 2.0)
* `Go <https://github.com/golang/go/tree/master/src>`__ (BSD-style)
* `Haskell <https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries>`__ (BSD-style)

  * The alternate prelude `Foundation <https://github.com/haskell-foundation/foundation>`__ (BSD)

* Julia `1 <https://github.com/JuliaLang/julia/tree/master/base>`__ `2 <https://github.com/JuliaLang/julia/tree/master/stdlib>`__ (MIT)
* C

  * `glibc <https://sourceware.org/git/?p=glibc.git;a=tree>`__ (LGPLv2.1, some files BSD/ISC/etc.)
  * `Musl <https://git.musl-libc.org/cgit/musl/tree/>`__ (MIT)

* Python `1 <https://github.com/python/cpython/tree/master/Modules>`__ `2 <https://github.com/python/cpython/tree/master/Lib>`__ (PSFv2)
* `Zig <https://github.com/ziglang/zig/tree/master/lib/std>`__ (MIT)
* Slate `1 <https://github.com/briantrice/slate-language/tree/master/src/core>`__ `2 <https://github.com/briantrice/slate-language/tree/master/src/lib>`__ `3 <https://github.com/briantrice/slate-language/tree/master/src/i18n>`__

But building on the work of others isn't enough, we also have to improve and synthesize a new, universal standard library for new programs to use. The standard library should be well-designed and built up steadily but should eventually include even things like audio and graphics (and way before that — cryptography). If it’s not an unsolved mathematical problem, there’s no reason it can’t have a standard solution. The same common problems getting solved by different people is a waste of man‑hours that can be spent developing something new. But the library should be divided up into modules and the modules should be versioned so that there's a deprecation cycle in place.

For this, the proposals of the various languages are useful, as they encapsulate changes and include motivation as to why the change was made. A feature of a language might be historical accident but a proposal is always a deliberate design choice. Even the rejected proposals are useful as they indicate language/library "smells", areas that could use improvement.

* `GHC <https://github.com/ghc-proposals/ghc-proposals/pulls>`__
* `Python <https://github.com/python/peps>`__
* `Rust <https://github.com/rust-lang/rfcs/pulls>`__ (`accepted <https://rust-lang.github.io/rfcs/>`__)
* `Go <https://github.com/golang/go/labels/Proposal>`__

TODO: go through these, unfortunately there’s a lot

Numbers
=======

In the mathematical world there are integers and real numbers, which have well-defined arithmetic operations (besides division by 0). In the computer world we do not have either of these luxurious spaces, but rather various formats for numbers which represent subsets of the space.

Integers
--------

.. raw:: html

  <div style="display: none">
  \[
  \newcommand{\seq}[1]{{\langle #1 \rangle}}
  \newcommand{\abs}[1]{{\vert #1 \rvert}}
  \newcommand{\sem}[1]{[\![ #1 ]\!]}
  \]
  </div>


The most common integer format is a signed/unsigned integer with the range :math:`[0,2^{k}-1]` or :math:`[-2^{k-1},2^{k-1}-1]`, taking :math:`k` bits. But it is not too tricky to implement efficient arithmetic operations for arbitrarily-ranged integers :math:`[a,b)`, where the modulus :math:`b-a` is a power of 2. We can represent as :math:`a+k` or :math:`b-k` where :math:`k` is unsigned or :math:`(a+b)/2 + k` for signed :math:`k`. The operations use :math:`\log_2 (b-a)` bits and expand the constants out (:math:`(x+a)+(y+a)=(x+y)+2*a`, etc. - there's definitely clever ways to structure the computations for efficiency). When the range can be determined statically there is no overhead besides the extra operations (and there are no extra operations if the range fits into the machine-sized integer). If we use branching operations we can go even farther and use a tag bit to represent unions of ranges, :math:`[-23,2] \cup [56,100]`.

With these extended ranges, the key difference between "signed" and "unsigned" is not that signed can represent negative numbers, but rather that signed integers represent an unbounded integer, that errors if the result is not representable (overflow, underflow, gap missing), while unsigned integers represent equivalence classes :math:`\sem{a} = \{ a + k m \mid k \in \mathbb{N} \}`, :math:`m` being the modulus. The format defines the representatives used, operations are done in :math:`\mathbb{Z}` on the representatives, and then the result is converted via the equivalence class to a representative. So better names might be signed integer format = erroring integer format, unsigned integer format = wrapping integer format.

Division for all of these formats is defined using the `division algorithm for Euclidean domains <https://en.wikipedia.org/wiki/Euclidean_domain>`__. For :math:`a, b \mid b \neq 0`, :math:`a divMod b` produces :math:`(q,r)` such that :math:`a = bq + r` and the norm :math:`\abs{r}` is minimized. This gives "round to nearest" behavior and is different from most other programming languages, e.g. ``11 divMod 4 = (3,-1)`` rather than ``(2,3)``. But mathematically it has nice properties. Ties are broken by choosing positive :math:`r`, this amounts to tweaking the norm function so :math:`\abs{+x} = x - 0.1`. We can also consider other variants like setting :math:`\abs{-x} = \infty`, this gives Euclidean division. For a complicated split-range number number format, the computation will probably have to use brute force to determine the result. The range of :math:`q` is another question, most likely we have to give it as an argument.

This division is different from `most other programming languages <https://en.wikipedia.org/wiki/Modulo_operation#In_programming_languages>`__. In particular the C / assembly behavior of truncation is just wrong and cannot be emulated with a norm function - there is no consistent ranking giving ``1 divmod 2 = (0, 1)``, ``-1 divmod 2 = (0, -1)``. But of course C's behavior can still be defined for the relevant formats, it just is not universal.

Fractions
---------

The simplest is ratios :math:`a / b`, using integers over some domain. Fixed-point arithmetic is a special case of this where :math:`b` is fixed. Floating point numbers are an integer mantissa times an integer radix raised to an integer exponent. The radix is usually 2 but `IEEE-754 <https://en.wikipedia.org/wiki/IEEE_754>` has also defined decimal floating point (radix 10). The exponent itself is another integer, usually restricted to a quite small range. We can also include posits; these are mantissa * radix ^ exponent * useed ^ regime, where the first part is the floating point stuff, useed is 2 ^ 2 ^ maximum exponent size, and the regime is nonnegative.

Actual types
------------

We could try to define generic integer/float types with a statically inferred range of possible values, but only a few have efficient arithmetic operations, and YAGNI. So in practice we have only ``sN`` / ``uN`` (for ``N`` restricted to 8/16/32/64), ``Float``, and ``Double``. Differently-ranged integers, fixed-point arithmetic, unums, and posits can all be defined in libraries. It would also be good to have arbitrary-precision types, like `GMP <https://gmplib.org/>`__'s integer/rational and `MFPR <https://www.mpfr.org/>`__'s float that uses an s32/s64 exponent and an arbitrary precision mantissa. The binding could be at the C level like `Haskell's integer-gmp <https://hackage.haskell.org/package/integer-gmp>`__ or it could use the assembly routines directly.

Operations
----------

For arithmetic we define implicit conversions, ``convert : s8 -> Arb`` and so on to an arbitrary precision type ``Arb`` with the usual arithmetic operations, ``(+) : Arb -> Arb -> Arb`` and so on. Then narrowing the result back into a restrictive format is represented explicitly with an operation, ``narrow s16 (2+30*x)`` and so on. The compiler then figures out how to compute the answer as efficiently as possible. For floating point the narrowing also takes a precision argument, or optimizes for the best precision like Herbie, depending on whether speed or accuracy is preferred.

For compatibility with other languages we can define narrowed arithmetic operations, like ``a + b = assert(a is s16 && b is s16); x = narrow s16 (a+b); assert(x is s16)``. These give an error if the result doesn't fit. We can also support implicit conversions ``convert : s8 -> s16`` and so on; the compiler has to check that the narrowed arbitrary-precision computation matches the various fixed-width computations, but it should be resolvable.

Floating points numbers don't have implicit conversions between each other, besides the conversion from literals. The arithmetic operations are defined normally, ``(+) :: f32 -> f32 -> f32`` and so on.

Literals with leading zeros must be stored in a type that can hold the digits all replaced with their highest value, e.g. 0001 cannot be stored in a byte (type must be able to contain 9999).

Iterators
=========

lazy iterators - aren't these just linked lists?

Haskell Foldable / Traversable

some way to get imperative for-in loops.

Rust: https://doc.rust-lang.org/std/iter/trait.Iterator.html

Clojure transducers:
https://clojure.org/news/2012/05/15/anatomy-of-reducer
https://cognitect.com/blog/2014/8/6/transducers-are-coming
https://clojure.org/reference/transducers
https://juliafolds.github.io/Transducers.jl/dev/

Strings
=======

The standard, terrible null-terminated C string will always be needed, but most purposes should be satisfied by using an array / buffer of bytes together with a length. There can be different encodings: UTF8, UTF16, UTF32, or some other encodings like Shift JIS or Big5. UTF8 is the most common so it should be the default, `UTF-8 everywhere <https://utf8everywhere.org/>`__.

Normalization to NFC is an operation. Refinement type for always-normalized, let operations take a parameter.

Operations can take place through code points, graphemes, bytes (code units, but utf-8 everywhere so there’s no difference). Provide each type unless there's a good reason not to. Moving forward or backward in a text editor would use graphemes. Writing a file would use bytes.

Invalid characters can be handled different ways according to a mode parameter: delete from string, preserve, transcode to private use area, etc.

* slices/views: these are a string value plus data.
* indexing / length
* next / previous (using utf8 synchronization)
* regexes / parsers
* I/O - do like Go and always open files in binary mode. stream API
* packed arrays
* ropes for mutable strings (so splitting the string apart and inserting things is efficient)
* hierarchical streams/generators.
* https://juliastrings.github.io/utf8proc/

Time
====

The JSR-310 `ThreeTen <https://www.threeten.org/>`__ library in `Java 8 <https://docs.oracle.com/javase/16/docs/api/java/time/package-summary.html>`__ seems to have undergone the most peer review.

I/O
===

The general API for I/O follows the io_uring design, we write a bunch of operations to a buffer and then execute callbacks based on the result.
We also need datatypes for dealing with streaming I/O, but continuations work for that.

The functions themselves are written in the token-passing style ``RealWorld, a -o RealWorld, b``, passing around the ``RealWorld`` token.

The standard library wraps all relevant functions in :ref:`destructors <destructors>` to ensure safety. But there is also a corresponding .Raw module which provides the unwrapped versions.

Errors
======

Safety - errors in your program lead to error messages, as opposed to unpredictable crashes.

Values
------

Most errors behave by producing a `poison value <https://llvm.org/devmtg/2020-09/slides/Lee-UndefPoison.pdf>`__. For example ``{}.x`` produces like ``NoSuchAttributeError {} "x"``. Similarly invalid pointer reads return ``InvalidPointer``, rather than crashing the program. Division by zero is handled in the same way, producing ``DivisionByZeroError``. There's also standard poison values like ``undefined`` and ``panic "string"``.

Behind the scenes this requires some work to implement. Pointer reads generate page faults, which if they are invalid will be returned to the program via the signal "Segmentation fault" (SIGSEGV). C/C++ `can't handle these easily <https://stackoverflow.com/questions/2350489/how-to-catch-segmentation-fault-in-linux>`__ because they are `synchronous signals <https://lwn.net/Articles/414618/>`__ and signal behavior is mostly left undefined, but in fact signals are `fairly well-behaved <https://hackaday.com/2018/11/21/creating-black-holes-division-by-zero-in-practice/>`__ (`OpenSSL <https://sources.debian.org/src/openssl/1.1.1k-1/crypto/s390xcap.c/?hl=48#L48>`__'s method of recovering from faults even seems standards-compliant). It definitely seems possible to implement this as an error value in a new language. Go `allows <https://stackoverflow.com/questions/43212593/handling-sigsegv-with-recover>`__ turning (synchronous) signals into "panics" that can be caught with recover.

Similarly DIV by 0 on produces a fault, which on Linux the kernel picks up and sends to the application as a SIGFPE. OTOH, UDIV by 0 on ARM simply produces 0. So on ARM producing the division by 0 error definitely requires checking if the argument is zero beforehand - the people that really can't afford this check will have to use the division instruction in the assembly module. But on x86 we can decide between inserting a check and handling the SIGFPE; it'll require testing to see which is faster in typical programs - my guess is the handler, since division by zero is rare.

Traces
------

Most operations on an error will produce another error, e.g. ``case {}.x of 1 -> ...`` produces ``MissingCaseError (NoSuchAttributeError ...)``. So the errors bubble up until we get something that handles the error, e.g. the main program handler that prints the error and exits. With fancy formatting the nested errors will look like a stacktrace. The semantics are a little different because it's demand-driven, but close enough. TODO: make sure the stack trace can't become infinite.

We can redefine this error value to be something else, e.g. add a definition ``NoSuchAttributeError {} "x" = 3``. Then ``{}.x == 3`` and the error is silenced. Similarly we can do ``case {}.x of NoSuchAttributeError {} "x" -> 3``, or pass the error to a function that does such error-handling. We can also match on generic errors, ``case {}.x of e | isError e -> 3``. The alternative to ``isError`` is a single standard error constructor, IDK.

The errors can also keep track of their continuation, e.g. a ``MissingCaseError`` can store its continuation ``\x -> case x of ...``. These compose up the stack so that we can pass in a value at any point and resume computing.

Stateful exceptions
-------------------

For a stateful function, the ``RealWorld`` token also is replaced with an error value. So no further states can be executed until the error is handled. But the error value itself contains a new ``RealWorld`` token to allow resuming the computation. We can define the standard levels of safety: no-throw is that the normal state will be returned, strong exception safety of a function is the assertion that the state in the error value is no different from the state passed in, and basic safety is that all documented invariants are maintained for the state in the error value. Most operations with basic safety can be made strongly safe by copying all relevant data beforehand, besides actual I/O operations.

try-catch-else-finally: we can handle the try-catch part with continuations and the error-redefining trick, ``case reset (Left (foo {e | isDesiredError e = shift (const e)}) of e | isDesiredError e -> handle e``. We can also use the bubbling: ``case x of e | isError e and isDesiredError (firstError e) -> ...``. For finally we want a state field to extract the token, ``case x of e -> e { state = cleanup (state e) }``. Python also supports an else clause - it is executed if control flows normally off the end of the try clause and is not protected by the catch clauses of the try.

asynchronous exceptions: this instruments every memory allocation and I/O operation to check for calls to ``throwTo ThreadId`` and if so return ``Interrupted``, ``ThreadKilled`` (``PleaseStop``), etc. But every operation is also given a parameter ``Masked`` (for memory and nonblocking I/O operations) or ``Interruptible`` (for blocking I/O operations) that disables this behavior. Then there's the mask function, ``mask io = if Masked then io {unmask = id} else io {Masked = True, unmask io = io {Masked = False} }`` and similarly ``uninterruptibleMask`` which also checks/sets ``Interruptible``.

IMO GHC's `asynchronous exception system <https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/>`__ is broken. The system is so complicated that nobody can agree on the desired behavior / correct form of even simple examples. The prototypical example of using it is `bracket <https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Exception.html#v:bracket>`__:

::

  bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
  bracket before after thing = withRunInIO $ \run -> EUnsafe.mask $ \restore -> do
    x <- run before
    res1 <- EUnsafe.try $ restore $ run $ thing x
    case res1 of
      Left (e1 :: SomeException) -> do
        _ :: Either SomeException b <- EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ run $ after x
        EUnsafe.throwIO e1
      Right y -> do
        _ <- EUnsafe.uninterruptibleMask_ $ run $ after x
        return y

Here we use 4 operations: mask, try, ``uninterruptibleMask_``, throwIO. mask shields the cleanup action from being attacked by asynchronous exceptions, allowing exceptions inside restore. try catches exceptions and allows cleanup to occur. ``uninterruptibleMask_`` blocks interrupts from interrupting the after handler. Finally throwIO rethrows the exception, so that any exception inside the after handler will be swallowed.

Apparently, though, nobody can agree on whether the after handle should run with an uninterruptible mask.

Another issue with exceptions is handling them. The top-level can throw exceptions, or it can catch them, printing them and exiting with an error code. Usually people write their own handlers, hence making it uncomposable.


.. _concurrency-library:

Concurrency
===========

High-level concurrency is implemented with transactions. The syntax is ``atomically { if x { retry }; y := z }``. Transactions nested inside another transaction are elided, so that one big transaction forms.

The implementation guarantees eventual fairness (maybe): A transaction will succeed and be committed eventually, provided it doesn't retry all the time. Also transactions are serializable.

Low-level, shared memory uses the memory model of the architecture, so all synchronization methods can be used according to their semantics.

Mixing transactions with low-level code might work, IDK. There could be ``atomically {order=relaxed} { ... }`` to use the CPU's memory model instead of totally ordered. Generally transactions are preferred, because they compose. Transactions matching atomic instructions should compile to the atomic instructions if Stroscot can prove there are no waiting threads to wake up.