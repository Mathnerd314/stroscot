.. _fexprs:

Metaprogramming
###############

In Stroscot, as in `Kernel <http://web.cs.wpi.edu/~jshutt/kernel.html>`__, fexprs are functions that take code AST's and a lexical environment instead of evaluated values. So when you write `f a b`, and `f` is an operative, then `f` has a type like `f : Env -> Ast -> ...`, and it receives an opaque env that might or might not have bindings to `a` and `b` and an AST fragment like `App (Sym 'a') (Sym 'b')`. Then `f` can do arbitrary operations with those, with the full power of the programming language, and in particular `f` can `eval` AST fragments with the env it's given (or with envs from elsewhere).

The main power fexprs give over macros is that there's no phase distinction. A macro is like an fexpr that builds up a single AST and calls eval at the end. But fexprs can call eval multiple times, and these can depend on the results of previous evaluations, so for example you can lookup a variable name stored in an argument and evaluate that name.
