Symbolic computing
##################

Symbolic computation is mainly distinguished by what it is not: it is not "numerical computation" with machine precision numbers, where one arithmetic operation translates to one machine code instruction. Symbolic computation can provide exact solutions, perform algebraic manipulation, prove mathematical properties, and solve equations. In education, it allows using mathematical numbers without complications such as approximation error and number formats. In industry, it allows manipulating and analyzing complex equations in robust ways, and provides more accuracy and precision in cases where required.

Machine precision numerical computation is typically much faster. In many tasks such as scientific simulations, data analysis, and engineering design, the approximation errors cancel out or are negligible and numerical computation delivers acceptable results in orders of magnitude less time. But often, these numerical equations are obtained with the help of symbolic computation. Many real-world problems require both numerical and symbolic analysis. Symbolic computing is thus a valuable tool, and Stroscot would be lacking if it left symbolic computing out of its functionality.

Features
========

One simple way of characterizing features is by the values (normal forms) they add to the language and then the operations on those values. There is a wide range of values supported by CAS's such as Mathematica, Maple, Magma, and SAGE: symbolic expressions (terms - trees, including symbols, variables, and constants, and binders so as to write derivatives, integrals, and summations), numbers to a specified (arbitrary) precision, machine-precision numbers, rational numbers, (computable) real numbers, complex numbers, strings, lists, sets, polynomials, truncated power series, systems of equations, probability distributions, special functions, records, tables, units, arrays, matrices, tensors (including sparse), functions, graphics, images, plots, files, directories, directed and undirected graphs and networks, dates and times, geometric objects, piecewise and interpolated numerical functions, sound and audio data, signal (time-series) data, algebraic number fields/rings/ideals (finite fields, algebraic extensions, and elements thereof), crytographic keys, combinatorial objects (permutations, combinations, and partitions), generating functions, modular forms, p-adic numbers, combinatorial designs (block designs, Latin squares, orthogonal arrays), databases, logical connectives (and, or, not, forall, exists).

Then we need algorithms and operations on these values. For the numbers, it is mainly arithmetic and special functions. For expression trees, there is simplification (and related transformations such as distributing, combining, and factoring), applying algebraic rules or equations (pattern matching and substitution, such as automatic differentiation), and equation solving. More complex algorithms such as automatic integration combine these in interesting ways. Then there are SMT solvers which can take almost any formula and determine if it is satisfiable.

Design
======

Following Stroscot's philosophy of "minimal core", as little as possible of the implementation should be in the language core. We will need to represent symbolic expressions - the easiest thing is to make these syntax trees, so that algorithms are macros that operate on syntax. But of course specific algorithms will operate on restricted subsets of syntax trees.

Oscar Benjamin has been working on speeding up SymPy and has written about `how he would redesign SymPy <https://oscarbenjamin.github.io/blog/czi/post1.html>`__. It is certainly worth taking his design points into consideration. According to Benjamin, fast symbolic computation depends on implementing three things efficiently: robust numerical evaluation, symbolic manipulation, and computational algebra.

Calculations
============

Consider the expression ``e = cos(1)^2 + (sin(1)^2 - 1)``. Evaluation can be done in several ways.

* The simplest and most common is machine-precision evaluation, where each operation is done to within a given tolerance (1 ulp usually). Numerical error can accumulate and there is no recourse other than to restructure the computation. ``evalDouble e = double 5.551115123125783e-17``
* In arbitrary-precision evaluation (mpmath), each operation is done to a specified, configurable tolerance / precision. But error may still accumulate and cause catastrophic cancellation and other issues. This can be addressed by manually increasing the precision. ``mpf e {digits=50} = mpf('1.3363823550460978230702682335757409394075984400525243e-51')``
* In interval arithmetic, each number's upper and lower bounds are tracked. Accumulated errors within computations are visible as large bounds relative to the number. The computation can be retried with more precision if it is not sufficiently accurate. ``interval e {digits=3} = {low = 0, high = 1e-3 }``
* In robust numerical evaluation (sympy.evalf, Arb), the result is guaranteed to be within a given accuracy or tolerance. The precisions needed for each intermediate computation are calculated using back-propagation of error and estimates of the actual value. Robust numerics are generally formally verified to produce accurate results. ``robust_evalf e {digits=50} = Float(-(1 +- 1)*2^(-531))`` An algorithm to determine if an expression is equal to 0 would also fall under robust numerics.
* In symbolic computation, the expression is simplified through rewrite rules. The expression may be returned unchanged, or reduce to a number, depending on the expression and simplification rules. ``simplify e = 0``

Typically, operations on tree-expressions and polynomials use a combination of robust numeric evaluation and symbolic computation. Robust numerical evaluation uses a mixture of machine-precision, arbitrary-precision, and interval arithmetic.

Polynomials
===========

Polynomials consist of a finite summation of terms, where each term is a product of a coefficient and powers of variables with integer exponents (generally positive). The coefficients are drawn from some domain (set), which can be integers, rationals, or symbolic expressions, while the variables are drawn from a different (finite) set of expressions. A univariate polynomial can be stored as a dense list of coefficients, while for multivariate polynomials it is generally more efficient to use a sparse representation. It is better to store all the coefficients using a uniform representation, so that operations do not have to check types. There is a conversion function that takes an arbitrary expression and converts it to the most efficient polynomial representation.

One question is whether the variables should form a basis. For example, with ``sin(t)`` and ``cos(t)``, the expression ``sin^2(t)+cos^2(t)-1`` is zero, even though its coefficient matrix ``{(2,0): 1,(0,2): 1,(0,0): -1}`` is not. It certainly makes it easier if the algorithms can assume that only the 0-coefficient polynomial is 0, but in general this doesn't hold. The conclusion is that polynomials with orthogonal variables should be a subset. Arbitrary expressions should be usable as the "variables" of the polynomial as well as the coefficients.

Staging
=======

Many systems do "automatic" simplification in that for example ``print(cos(pi/4)) = sqrt(2)/2``. Maybe this simplification is what you wanted, but if it is not (e.g. you wanted to evaluate the cosine with machine precision) you are stuck - the information about the original expression has been lost. Simplification and conversion is in general an expensive, lossy operation. Therefore the main API should be staged: first you construct an expression tree / polynomial / etc., and then you apply one or more functions to specify how to transform and evaluate it. This sort of API is precise and allows fine control over evaluation via tree manipulation, avoiding unexpected costs and unnecessarily repeated simplifications. Auto-simplifying operations can be defined on top of the staged API, like ``(AutoSimplify a) + (AutoSimplify b) = AutoSimplify $ simplify (a + b)``, and are useful if that's the desired behavior. But it would be a mistake to expose only auto-simplifying operations.

To look at
==========

* SMTLIB2
* internal datatypes of z3
* logic programming - solve for input to program that produces value
* matrix manipulation library
* pde solver
* https://github.com/uwplse/ruler
* http://www.sc-square.org/CSA/welcome.html They're trying to bridge cas and smt.
* linear system in a finite field
* BLAS - just because both CAS and BLAS have A in them, does not mean they are the same thing.
* https://herbie.uwplse.org/
* https://egraphs-good.github.io/egglog/?example=herbie
* https://flintlib.org/ - well-designed per Oscar Benjamin
* Maxima, Axiom and its two (!) forks, SymPy, REDUCE, Symbolics.jl, Mathics, SageMath, FORM, Yacas, Xcas, Oscar.jl, Macaulay2, Singular, `emmy <https://github.com/mentat-collective/emmy/tree/main>`__ - Axiom (or one of its forks) is probably the best-designed "winner" that everybody should copy (Lisp-based)
* Mathlib - could lead to a standardized language/format for machine-readable mathematics, like JSON or SMTLIB https://leanprover-community.github.io/
