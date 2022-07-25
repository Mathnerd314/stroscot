Macros
######

Comparison
==========

Macros, micros, and fexprs are facilities to allows allow defining syntactic extensions to a programming language. Comparing them:

1. A (Scheme) macro is a function translating an AST node into an AST node. Macro substitution of an AST reduces away all macros and produces an AST.
2. A micro :cite:`krishnamurthiLinguisticReuse2001` is a function translating from a source AST node and an environment to an expression in the intermediate language. Micro dispatch of an AST reduces away all micros and produces an intermediate representation.
3. An f-expression (fexpr) :cite:`shuttFexprsBasisLisp2010` is a function translating an AST node and an environment to a value. Fexpr evaluation of an AST reduces away all fexprs and produces a value.

Fexprs are the most powerful: IR expressions and AST nodes are values, hence micros and macros can be implemented in terms of fexprs, but conversely fexprs can't be emulated. E.g. we can implement a naive form of Scheme macros as f-expressions by computing the AST and evaluating, ``defmacro f = \vau $args $env -> eval $env (f $args)``.

Another advantage of fexprs is that there are no phases - the full language is always available and immediately executed. For example predicate dispatch of fexprs is possible - you just have to look up the argument values in the environment like with predicate dispatch on applicatives. In contrast macros can only overload on number of arguments because the precise values are not available in the preprocessing phase, and similar micros have a linking phase for the IR. Hence implementing the compiler using the Futamura projection of specializing an interpreter to a program is only sensible for fexprs.

Terminology
===========

:cite:`shuttFexprsBasisLisp2010`'s terminology as follows:

* Evaluation is interpreting an AST node to produce a value.
* Values evaluate to themselves.
* The value of a variable is the result of looking up the variable's symbol the current binding.
* A combination is an AST node ``f x``, representing applying a symbol (operator) ``f`` to an operand AST ``x``, usually composed of a list of operands.
* A combiner is the result of evaluating an operator; it is a special type of value
* An argument is the result of evaluating an operand, and may in general be any value
* An applicative or applicative combiner is a combiner that first evaluates all of its operands to arguments in the current environment, and only uses the arguments thereafter. The combination of the applicative is an applicative combination.
* An operative or operative combiner is a non-applicative combiner. The combination is an operative combination.
* A (Scheme) macro is an operative that uses its operands to derive a syntactic replacement for the entire combination of the macro call.
* A (Scheme) special form is a hard-coded operative accessed by a reserved symbol
* A (Kernel) fexpr is a non-macro operative that is expressed as a ``$vau`` lambda taking unevaluated operands and the dynamic environment where the function is called

Although it gives each concept its own name, "fexpr" is an unusual word with no prior referent except maybe the old Lisp I fexpr which didn't take an environment hence couldn't implement lexical scope. So Stroscot's terminology follows `newLISP <http://www.newlisp.org/index.cgi?page=Differences_to_Other_LISPs>`__  and calls fexprs "macros":

* A Stroscot function is a Shutt applicative
* A Stroscot macro is a Shutt fexpr

The rationale is that Stroscot's macros operate on ASTs like macros in other languages, so it's clearer to the uninitiated to call them macros. But, they return a value instead of an AST, so they are more powerful than other languages' macros.

Hygiene
=======

Scheme macros are supposed to be "hygienic" in that they always evaluate expressions in the lexical environment of the macro's definition site, as opposed to use site. But :cite:`kiselyovHowWriteSeemingly2002` shows that in fact the environment of the use site is fully accessible through some tricks. The newer syntax-case allows explicit access through ``datum->syntax``.

Fexprs in contrast get an explicit environment. They can do staged lookup, ``eval $env (eval $env (f $args))``, where an expression evaluates to an AST symbol and the AST symbol is looked up, and other weird things. :cite:`shuttFexprsBasisLisp2010` chapter 5 discusses various hygiene-breaking problems and concludes they aren't too worrisome.

``eval`` is hard to compile, because it makes the full power of an interpreter available. But we can often simplify ``eval (a + b)`` to ``eval a + eval b``, reducing the amount of code that is evaluated each loop. If all of the variable lookups are static, we can furthermore optimize the environment to remove all unneeded variables. Hence we can recover macro-level performance on macros. Dynamic lookups need the full environment unfortunately. But dynamic lookups are essentially a REPL or debugging tool, so does not need to be too efficient, and we can warn that they are not optimized.

Fexprs make the equational theory of ASTs trivial, (:cite:`shuttFexprsBasisLisp2010`, chapter 15) in that ASTs can be completely deconstructed, so no two ASTS are behaviorally equivalent. But this is good, because it means the programmer's intent can be fully examined. If ``(\x. x) y`` was equivalent to ``y`` then many DSL's would not be possible. The behavior of programs containing fexprs is decidedly nontrivial and quite varied.
