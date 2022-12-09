Macros
######

Comparison
==========

Macros, micros, and fexprs are facilities to allows allow defining syntactic extensions to a programming language. Comparing them:

0. A (C) macro is a function translating text into text. Textual macros suffer from the scoping/name capture problem. Applying a macro ``double x=x+x`` to a side-effectful expression like ``double(i++)`` executes the side effect twice. They allow unbalanced/incorrect expansions like ``startLoop = while(true) {`` and ``double(+) = +++``. One often can't easily determine the expanded expression due to multiple layers of macros. Furthermore looking for control flow paths is hard.
1. A (Scheme) macro is a function translating an AST node into an AST node. Macro substitution of an AST reduces away all macros and produces an AST. Scheme macros have a complex hygiene system to make naive macros handle scoping correctly and avoid name capture.
2. A micro :cite:`krishnamurthiLinguisticReuse2001` is a function translating from a source AST node and an environment to an expression in the intermediate language. Micro dispatch of an AST reduces away all micros and produces an intermediate representation.
3. An f-expression (fexpr) :cite:`shuttFexprsBasisLisp2010` is a function translating an AST node and an environment to a value. Fexpr evaluation of an AST reduces away all fexprs and produces a value.

Fexprs are the most powerful: IR expressions and AST nodes are values, hence micros and macros can be implemented in terms of fexprs, but conversely fexprs can't be emulated. E.g. we can implement Scheme macros as f-expressions by computing the AST and evaluating, ``defmacro f = \vau $args $env -> eval $env (f $args)``. But there is no macro corresponding to an eval function ``evalF = \vau $args env -> \f -> eval f $env`` that evaluates a value in an environment at runtime.

Another advantage of fexprs is that there are no phases - the full language is always available and immediately executed. For example predicate dispatch of fexprs is possible - you just have to look up the argument values in the environment like with predicate dispatch on applicatives. In contrast macros can only overload on number of arguments because the precise values are not available in the preprocessing phase, and similarly micros have a linking phase for the IR. Hence implementing the compiler using the Futamura projection of specializing an interpreter to a program is only sensible for fexprs.

Terminology
===========

Loosely adapting :cite:`shuttFexprsBasisLisp2010`'s terminology we get the following:

* Evaluation is a function from an AST node and a (lexical) environment to produce a value. Values are AST nodes that evaluate to themselves in every environment. A non-value AST node (reducible expression) is usually a variable ``x`` or an application ``f x``, but since Stroscot is term-rewriting it can be anything.
* A reduction rule maps particular AST nodes and environments to new AST nodes and environments. These AST nodes are of course reducible.
* An applicative is a reduction rule that uses each pattern variable once by transforming it to an "argument" by evaluating it using the current environment.
* An operative is a non-applicative reduction rule.
* A (Scheme) macro is an operative that uses its operands to derive a syntax tree and then returns the result of evaluating the entire tree.
* A (Scheme) special form is an operative built into the language, accessible by a reserved symbol.
* A (Kernel) fexpr is a non-macro operative that is expressed as a ``$vau`` lambda taking unevaluated operands and the environment.

Although it gives each concept its own name, "fexpr" is an unusual word with no prior referent except maybe the old Lisp I fexpr which didn't take an environment hence couldn't implement lexical scope. So Stroscot's terminology follows `newLISP <http://www.newlisp.org/index.cgi?page=Differences_to_Other_LISPs>`__  and calls fexprs "macros":

* A Stroscot function is a Shutt applicative
* A Stroscot macro is a Shutt operative

Stroscot's macros operate on ASTs like macros in other languages, so it's clearer to the uninitiated to call them macros. But, they return a value instead of an AST, so they are more powerful than other languages' macros.

Hygiene
=======

Scheme macros are supposed to be "hygienic" in that they always evaluate expressions in the lexical environment of the macro's definition site, as opposed to use site. But :cite:`kiselyovHowWriteSeemingly2002` shows that in fact the environment of the use site is fully accessible through some tricks. The newer syntax-case allows explicit access through ``datum->syntax``.

Fexprs in contrast get an explicit environment. They can do staged lookup, ``eval $env (eval $env (f $args))``, where an expression evaluates to an AST symbol and the AST symbol is looked up, and other weird things. :cite:`shuttFexprsBasisLisp2010` chapter 5 discusses various hygiene-breaking problems and concludes they aren't too worrisome.

``eval`` is hard to compile, because it makes the full power of an interpreter available. But we can often simplify ``eval (a + b)`` to ``eval a + eval b``, reducing the amount of code that is evaluated each loop. If all of the variable lookups are static, we can furthermore optimize the environment to remove all unneeded variables. Hence we can recover macro-level performance on macros. Dynamic lookups need the full environment unfortunately. But dynamic lookups are essentially a REPL or debugging tool, so does not need to be too efficient, and we can warn that they are not optimized.

Fexprs make the equational theory of ASTs trivial, (:cite:`shuttFexprsBasisLisp2010`, chapter 15) in that ASTs can be completely deconstructed, so no two ASTS are behaviorally equivalent. But this is good, because it means the programmer's intent can be fully examined. If ``(\x. x) y`` was equivalent to ``y`` then many DSL's would not be possible. The behavior of programs containing fexprs is decidedly nontrivial and quite varied.
