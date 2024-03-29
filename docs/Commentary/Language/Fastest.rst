As fast as C
############

How do you prove that Stroscot is "as fast as C"? Well, we must show that every C program has a natural Stroscot translation, that performs at least as fast. Since Stroscot is an expressive language there may be many natural translations, and all must perform as fast.

Stroscot should also have fast performance on programs that aren't natural translations of C programs, but this is secondary to the claim. Even the lambda calculus, after years of study, has no optimal implementation on modern machines. So long as the implementation is reasonable, the actual performance on non-C-like programs doesn't matter - worst-case, the feature becomes a footgun for people are performance sensitive.

Benchmarks
==========

One aspect is benchmarks implemented in both C and Stroscot. This is the approach taken by the programming language shootout. The Stroscot programs could cheat by being "better written" than "naive" C programs, but requiring that they be "natural translations" avoids this.

Controlling the computer hardware and other running processes, we can test the strength of the code generator.

But writing benchmarks is tedious, and my hand-crafted code generator will take a lot of work to beat LLVM.

C programs
==========


C programs consist of subroutines, with parameters passed by value. Variables have lexical scope and are stored on the stack. Recursion has no TCO. Subroutines execute a series of instructions, which can be blocks, structured programming constructs (if/else, for, do/while, while, continue/break, switch), function calls, assignments or returns. Instructions contain expressions, which can be arithmetic (``*/%+-``, unary '-' (negation), '+', '!', '~'), bitwise (``>> << & ^ |``), or logic operators ( '<' '<=' '>' '>=', '==' '!=', '&&', '||'), or pointer reference/dereference. Types include signed/unsigned integer of various sizes, struct, union, pointer. The sizeof function is built-in. Enums are ints and arrays are pointers, so they can be ignored.

Natural translations
====================

With functional programming the natural translation of a set of loops is a set of mutually recursive functions. To get the same performance as C they need to be tail-recursive (hence translatable to jumps). So we need to mark that these functions should be inlined into the main (C convention) function. Or do the reverse like Haskell and have TCO by default and C convention as marked. So then subroutines translate to functions that do I/O marked with the C calling convention. If we devise a default CC that's just as fast but allows TCO then the C annotation can be dropped, although it's still necessary for the FFI.

Naked blocks are just a variable scoping mechanism. The equivalent is ``let`` or ``where``.

if/else and switch are case expressions. Arithmetic and bitwise operations are strict primitive functions. Logic operators are strict too except for &&/|| which are lazy in the second argument. That covers signed/unsigned integers and booleans.

For I/O we use Task + continuation monad. The ``return`` statement is preserved. C function calls can be embedded as normal.

Variables can be defined/assigned with the ``ref``/``:=`` imperative operations. But also natural is to use pure values to represent them and add extra function parameters.

structs and unions can be represented as arrays of bytes with read/write operations. The operations can be imperative, or we can provide pure update operations and do Hughes's optimization to turn them back into destructive updates.

Pointer reference/dereference can be imperative or we could represent memory as an associative array and use pure operations with Hughes's optimization.

Asymptotics
===========

A direct mapping works for small programs, but what about large programs? We must ensure that the asymptotic overhead of each of our features is not too large. In particular time and space usage.

Tail-recursive term rewriting compiles into a state machine just like tail-recursive functions.

Laziness can affect space usage. But forcing at each imperative operation should avoid this, it just means the compiler has to propagate demand and see that everything is strict.

Exceptions add some complexity to the control flow, but if you don't use exception handling then they all turn into panics, which are pretty cheap.

The verification stuff is all at compile time, so it gets cached. So not a big factor.

Tail calls
==========

Recursive tail calls encode loops. Similarly non-recursive tail calls encode state machines, that can be encoded as a switch-on-enum wrapped in a loop.

In general, caller cleanup is faster, while callee cleanup gives smaller code. The C calling convention uses caller cleanup; the caller pops a fixed number of arguments off the stack after returning from the callee. The C convention lets caller reuse one set of outgoing argument space for all calls.

To perform tail calls in the case in which the callee has more arguments than the
caller, you have to make sure that callees pop all their arguments, i.e. it requires callee cleanup.

The difference is on the order of 20% in practice.

Nobody seems to have benchmarked 64-bit calling conventions. These use more registers so are less sensitive to stack stuff.

Thorin implements lambda mangling which makes basic blocks as fast as C.

Laziness
========

The overhead of laziness means Haskell often loses to C. Laziness only helps timewise if a box is discarded, and otherwise hurts. And intended laziness is actually quite rare.

So in addition to GHC's demand analysis which has to prove usage to optimize to strict and falls back to lazy, we want a cost analysis which identifies nontermination and opportunities for saving a lot by discarding and falls back to strict. Also we should evaluate in the case where evaluating directly is cheaper than allocating a thunk (numerical computations and such): http://blog.ezyang.com/2011/05/anatomy-of-a-thunk-leak/

Avoiding slow operations
========================

Another thing that slows down a language are certain kinds of operations. E.g. dynamic lookups, weak typing, variant types. See examples of what makes PHP slow in this `video <https://www.youtube.com/watch?v=p5S1K60mhQU>`__. In some cases you can replace these operations with faster ones (specialization). JIT has more information and can specialize based on the observed values. Profile-guided ahead of time optimization can do the same thing but with the JIT the profiling is built in and you don't have to do a separate build.
