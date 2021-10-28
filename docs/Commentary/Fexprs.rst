Macros
######

macro - a syntactic extension to a programming language

A macro is a function translating an AST node into an expanded AST node.

defmacro is more complex. You can run ``macroexpand "(f a b)"`` to get the expanded AST.
but with a raw expression ``f a b``, f is called with ASTs as arguments, namely 'a, 'b. The resulting AST then replaces the expression, and execution resumes as if the new piece of code had occurred in the first place. The macro form is said to expand into the new piece of code. So it behaves like ``eval (macroexpand "f a b")`` where the environment is the environment of the raw expression.
