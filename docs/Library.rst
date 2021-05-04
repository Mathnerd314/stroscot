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

But building on the work of others isn't enough, we also have to improve and synthesize a new, universal standard library for new programs to use. For this, the proposals of the various languages are useful, as they encapsulate changes and include motivation as to why the change was made. A feature of a language might be historical accident but a proposal is always a deliberate design choice. Even the rejected proposals are useful as they indicate language/library "smells", areas that could use improvement.

* `GHC <https://github.com/ghc-proposals/ghc-proposals/pulls>`__
* `Python <https://github.com/python/peps>`__
* `Rust <https://github.com/rust-lang/rfcs/pulls>`__ (`accepted <https://rust-lang.github.io/rfcs/>`__)
* `Go <https://github.com/golang/go/labels/Proposal>`__

Assembly
========

The first step in dealing with assembly is to decide which ISA's to support. The two primary architectures are 64-bit x86 (desktops) and ARMv8-A AArch64 (mobile devices). Later contributors may extend it to 32-bit x86/ARM systems, MIPS, POWER, RISC and SPARC, but the peephole optimization is a lot of work and from a design perspective supporting 2 architectures is no different than 10.

Next we need the list of instructions in a machine-readable form. For this the following looks useful:
* Google's `EXEgesis <https://github.com/google/EXEgesis>`__ for Intel x86-64 and ARM, which for x86 has micro-op and timing information for x86 but no information on flags (maybe)
* `Intel XED <https://intelxed.github.io/>`__ for x86, which has info on read/written flags
* ARM XML `instruction tables <https://developer.arm.com/architectures/cpu-architecture/a-profile/exploration-tools>`__ (parsed by EXEgesis)

Other possibilities:
* NASM tables for x86, `instructions <https://github.com/netwide-assembler/nasm/blob/master/x86/insns.dat>`__ `registers <https://github.com/netwide-assembler/nasm/blob/master/x86/regs.dat>`__, good mnemonics but not much else
* K Framework `formal semantics <https://github.com/kframework/X86-64-semantics>`__
* LLVM's tables, e.g. `x86 <https://github.com/llvm/llvm-project/blob/main/llvm/lib/Target/X86/X86.td>`__
* `OSACA <https://github.com/RRZE-HPC/OSACA/tree/master/osaca/data/isa>`__ (AGPL licensed)
* MazeGen's X86 `XML reference <http://ref.x86asm.net/x86reference.xml>`__ (`Github <https://github.com/Barebit/x86reference>`__)

After that we implement register allocation. What this means is that each instruction, instead of reading/writing various physical registers/flags, reads/writes from instances of virtual register/flag classes. For example there is the typical "general purpose register" class, and we could adopt a convention where ``0_gpreg, 1_gpreg``, etc. are instances of this class. Then an instruction would look like ``2_gpreg = 0_gpreg + 1_gpreg`` but using Intel/AT&T syntax. Virtual registers are in SSA form so ``dst`` is different from ``src``. Other register classes include floating-point, SSE, SIMD, and GPR registers, low/high registers (these can also be written as r5.high) .

Addresses and the program counter are virtualized as well, using labels. A label refers to a memory location with a specific block of code loaded. The blocks are not ordered, so unconditional jumps must be inserted between blocks if necessary. The block order can be determined using profiling, removing the unconditional jump that is taken most often.

Memory references should be virtualized as well, so we also have memory labels. The alignment and format of the memory address should be specified.

Instructions and blocks are marked by the virtual registers they consume and use (input / output registers). The call and jump instructions are special in that a mapping may be given between the virtual registers and physical registers. Instruction constraints:
* Output: the register must not contain a value used after the block
* Output early clobber: output and the register must not be used for any inputs of the block
* Input: the register is read but not written to. Multiple inputs may all be assigned to the same register, if they all contain the same value.
* Tied input: register that is read and written
* Tied input early clobber: register that is read and written and does not share a register with any other input
* alignstack, sideeffect

There are also constraints from the ABI calling convention: https://gitlab.com/x86-psABIs/x86-64-ABI

Numbers
=======

In the mathematical world there are integers and real numbers, which have well-defined arithmetic operations (besides division by 0). In the computer world we do not have either of these luxurious spaces, but rather various formats for numbers which represent subsets of the space.

Literals
--------

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

The standard, terrible null-terminated C string will always be needed, but most purposes should be satisfied by using an array / buffer of bytes together with a length. There can be different encodings: UTF8, UTF16, UTF32, or some other encodings like Shift JIS or Big5. UTF8 is the most common so it should be the default.

Invalid characters can be handled different ways according to a mode parameter: delete from string, preserve, transcode to private use area, etc.

Non-mutating views are easy to implement as auxiliary data structures that share the underlying string. So we can have substrings / slices and codepoint/grapheme/word indexing.

For mutation we can't in general replace the contents in-place, because they're different lengths. So copying is the way to go. But a more advanced implementation would use ropes or similar.

I/O
===

The general API for I/O follows the io_uring design, we write a bunch of operations to a buffer and then execute callbacks based on the result.
We also need datatypes for dealing with streaming I/O, but continuations work for that.

The functions themselves are written in the token-passing style ``RealWorld, a -o RealWorld, b``, passing around the ``RealWorld`` token.

Errors
======

``{}.x`` produces an error. But how do errors behave? Since it is a value and we are dealing with value operations we get back a special kind of value, an error value like ``NoSuchAttributeError {} "x"``.

Most operations on an error will produce another error, e.g. ``case {}.x of 1 -> ...`` produces ``MissingCaseError (NoSuchAttributeError ...)``. So the error bubbles up until we get something that has a catch-all to handle errors, e.g. the main program handler that prints the error and exits. With fancy formatting the nested errors will look like a stacktrace - but the stack is the stack of future operations, rather than where the program has been.

We can redefine this error value to be something else, e.g. add a definition ``NoSuchAttributeError {} "x" = 3``. Then ``{}.x == 3`` and the error is silenced. Similarly we can do ``case {}.x of NoSuchAttributeError {} "x" -> 3``, or pass the error to a function that does such error-handling.

The errors can also keep track of their continuation, e.g. a ``MissingCaseError`` can store its continuation ``\x -> case x of ...``. These compose up the stack so that we can pass in a value at any point and resume computing.

For a stateful function, the ``RealWorld`` token also is replaced with an error value. So no further states can be executed until the error is handled. But the error value itself contains a new ``RealWorld`` token to allow resuming the computation. We can define the standard levels of safety: no-throw is that the normal state will be returned, strong exception safety of a function is the assertion that the state in the error value is no different from the state passed in, and basic safety is that all documented invariants are maintained for the state in the error value. Most operations with basic safety can be made strongly safe by copying all relevant data beforehand, besides actual I/O operations.

try-catch-else-finally: we can handle the try-catch part with continuations and the error-redefining trick, ``case reset (Left (foo {e | isDesiredError e = shift (const e)}) of e | isDesiredError e -> handle e``. We can also use the bubbling: ``case x of e | isError e and isDesiredError (firstError e) -> ...``. For finally we want a state field to extract the token, ``case x of e -> e { state = cleanup (state e) }``. Python also supports an else clause - it is executed if control flows normally off the end of the try clause and is not protected by the catch clauses of the try.

asynchronous exceptions: this instruments every memory allocation and I/O operation to check for calls to ``throwTo ThreadId`` and if so return ``Interrupted``, ``ThreadKilled`` (``PleaseStop``), etc. But every operation is also given a parameter ``Masked`` (for memory and nonblocking I/O operations) or ``Interruptible`` (for blocking I/O operations) that disables this behavior. Then there's the mask function, ``mask io = if Masked then io {unmask = id} else io {Masked = True, unmask io = io {Masked = False} }`` and similarly ``uninterruptibleMask`` which also checks/sets ``Interruptible``.

Concurrency
===========

Concurrency is the ability to execute units of a program in varying orders. Generally this is done for performance, so we want to verify that the order does not affect the output of the program, i.e. there are no race conditions. To this end we need to specify which outputs are equivalent, which can be accomplished by applying a ``deterministic`` predicate to important outputs, and also the allowed/possible execution orders.

An order is usually defined as a linear order. But if we consider from a physics point of view it is more complicated, event separation can be timelike or spacelike. So really we want to use a partial order. Hence an execution produces a directed graph of local states, where :math:`\to` is read "can casually influence". We can annotate the arrows with the information passed, and take the transitive closure to get a poset.

So an execution forms a poset - but often execution is nondeterministic. So in general the possible executions form a set of posets. In the least tractable case this set is arbitrary and the verifier must check all possible orderings. But if we assume that events can be independent, i.e. for specific events a,b reordering ...a b... to ...b a... and vice-versa does not change the behavior or whether the ordering is allowed, then the problem can be reduced to checking a set of posets. The posets are more resistant to state space explosion.

Various synchronization primitives:

* Linux kernel internal operations: `model <https://github.com/torvalds/linux/blob/3d5c70329b910ab583673a33e3a615873c5d4115/tools/memory-model/linux-kernel.def>`__ `atomic x86 operations <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/x86/include/asm/atomic64_64.h>`__ `lock types <https://www.infradead.org/~mchehab/kernel_docs/locking/locktypes.html>`__
* atomic operations
* memory barrier
* spin lock
