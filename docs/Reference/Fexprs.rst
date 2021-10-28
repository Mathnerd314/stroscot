.. _fexprs:

Metaprogramming
###############

In Stroscot, as in Kernel :cite:`shuttFexprsBasisLisp2010`, fexprs are functions that take code AST's and a lexical environment instead of evaluated values. So when you write ``f a b``, and ``f`` is an operative, then ``f`` has a type like ``f : Env -> Ast -> ...``. The ``Env`` is an opaque map that might or might not have bindings for ``a`` and ``b``, and the AST is fragment like ``((Sym 'f') `App` (Sym 'a')) `App` (Sym 'b')``. Then ``f`` can do arbitrary operations with those, with the full power of the programming language, and in particular ``f`` can ``eval`` AST fragments with the env it's given (or with envs from elsewhere).

The main power fexprs give over macros is that there's no phase distinction. A macro is like an fexpr that builds up a single AST and calls eval at the end. But fexprs can call eval multiple times, and these can depend on the results of previous evaluations, so for example you can lookup a variable name stored in an argument and evaluate that name.
