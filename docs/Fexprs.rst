.. _fexprs:

Metaprogramming
###############

In Stroscot, as in `Kernel <http://web.cs.wpi.edu/~jshutt/kernel.html>`__, fexprs are functions that take code AST's and a lexical environment instead of evaluated values. So when you write `f a b`, and `f` is an operative, then `f` has a type like `f : Env -> Ast -> ...`, and it receives an opaque env that might or might not have bindings to `a` and `b` and an AST fragment like `App (Sym 'a') (Sym 'b')`. Then `f` can do arbitrary operations with those, with the full power of the programming language, and in particular `f` can `eval` AST fragments with the env it's given (or with envs from elsewhere).

The main power fexprs give over macros is that there's no phase distinction. A macro is like an fexpr that builds up a single AST and calls eval at the end. But fexprs can call eval multiple times, and these can depend on the results of previous evaluations, so for example you can lookup a variable name stored in an argument and evaluate that name.

Atoms
=====

Any atom without a definition can be used as a symbolic expression. For example this will produce ``4`` if ``foo`` is undefined:

::

  f (foo x) = x + 2

  f (foo 2)

Due to the fexpr semantics any expression can be used and pattern-matched, like ``javascript (1 + "abc" { 234 })``.

To export the semantics to other modules a special keyword ``atom`` is used:

::

  atom foo

This ensures that no rules for ``foo`` are defined and adds the symbol to the export list. It is good practice to use this even if the identifier is not exported.

Examples of predefined atoms include null, true, and false.
