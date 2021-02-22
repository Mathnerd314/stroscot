Library
#######

Stroscot will support the standard libraries of other languages, so e.g. if you want the C functions with C semantics you would ``import Library.C``. Compatibility is a natural step to world domination.

But building on the work of others isn't enough, we also have to improve and synthesize a new, universal standard library.

Numbers
=======

In the mathematical world there are integers and real numbers, which have well-defined arithmetic operations (besides division by 0). In the computer world we do not have either of these luxurious spaces, but rather various formats for numbers which represent subsets of the space.

Literals
-------

Literals are parsed into records like ``NumberLiteral { digits = "123", exponent = "24" }``. We can define implicit conversions to the various the numeric types. Leadings 0's restrict the type, so ``010`` must be stored in a type that can contain 999.

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

The behavior is different from `most other programming languages <https://en.wikipedia.org/wiki/Modulo_operation#In_programming_languages>`__. In particular the C / assembly behavior of truncation is just wrong and cannot be emulated with a norm function - there is no consistent ranking giving ``1 divmod 2 = (0, 1)``, ``-1 divmod 2 = (0, -1)``. But of course C's behavior can still be defined for the relevant formats.

Fractions
---------

The simplest is ratios :math:`a / b`, using integers over some domain. Fixed-point arithmetic is a special case of this where :math:`b` is fixed. Floating point numbers are an integer mantissa times an integer radix raised to an integer exponent. The radix is usually 2 but `IEEE-754 <https://en.wikipedia.org/wiki/IEEE_754>` has also defined decimal floating point (radix 10). The exponent itself is another integer, usually restricted to a quite small range. We can also include posits; these are mantissa * radix ^ exponent * useed ^ regime, where the first part is the floating point stuff, useed is 2 ^ 2 ^ maximum exponent size, and the regime is nonnegative.

Actual types
------------

We could try to define generic integer/float types, but only a few have efficient arithmetic operations. So in practice we have only ``sN`` / ``uN`` (for ``N`` restricted to 8/16/32/64), ``Float``, and ``Double``. Differently-ranged integers, fixed-point arithmetic, unums, and posits can all be defined in libraries. It would also be good to have arbitrary-precision types, like `GMP <https://gmplib.org/>`__'s integer/rational and `MFPR <https://www.mpfr.org/>`__'s float that uses an s32/s64 exponent and an arbitrary precision mantissa. The binding could be at the C level like `Haskell's integer-gmp <https://hackage.haskell.org/package/integer-gmp>`__ or it could use the assembly routines directly.

Operations
----------

For arithmetic we define implicit conversions, ``convert : s8 -> Arb`` and so on to an arbitrary precision type ``Arb`` with the usual arithmetic operations, ``(+) : Arb -> Arb -> Arb`` and so on. Then narrowing the result back into a restrictive format is represented explicitly with an operation, ``narrow s16 (2+30*x)`` and so on. The compiler then figures out how to compute the answer as efficiently as possible. For floating point the narrowing also takes a precision argument, or optimizes for the best precision like Herbie, depending on whether speed or accuracy is preferred.

For compatibility with other languages we can define narrowed arithmetic operations, like ``a + b = assert(a is s16 && b is s16); x = narrow s16 (a+b); assert(x is s16)``. These give an error if the result doesn't fit. We can also support implicit conversions ``convert : s8 -> s16`` and so on; the compiler has to check that the narrowed arbitrary-precision computation matches the various fixed-width computations, but it should be resolvable.

Floating points numbers don't have implicit conversions between each other, besides the conversion from literals. The arithmetic operations are defined normally, ``(+) :: f32 -> f32 -> f32`` and so on.

Strings
=======

The standard, terrible null-terminated C string will always be needed, but most purposes should be satisfied by using an array / buffer together with a length. There can be different encodings: 8-bit UTF8, 16-bit UTF16, 32-bit UTF32, or some other encoding like Shift JIS or Big5. There are some optimizations that can be made for non-mutating views (substrings), e.g. storing an offset too to gives zero-copy slices (although ignoring allocators, a pointer is sufficient instead of start+offset). Iterating through strings is an interesting API design problem, particularly seeking for the :math:`n` th character, but isn't too hard overall. Dealing with invalid characters is a little trickier, but an implicit mode parameter should be sufficient. We also need datatypes for dealing with streaming I/O, but continuations work for that.
