Symbolic computing
##################

Symbolic computation is mainly distinguished by what it is not: it is not "numerical computation" with machine precision numbers, where one arithmetic operation translates to one machine code instruction. Symbolic computation can provide exact solutions, perform algebraic manipulation, prove mathematical properties, and solve equations. In education, it allows using mathematical numbers without complications such as approximation error and number formats. In industry, it allows manipulating and analyzing complex equations in robust ways, and provides more accuracy and precision in cases where it is required.

Numerical computation is typically much faster. In many tasks such as scientific simulations, data analysis, and engineering design, the approximation errors cancel out or are negligible and numerical computation delivers acceptable results in orders of magnitude less time. But often, these numerical equations are obtained with the help of symbolic computation. Many real-world problems require both numerical and symbolic analysis. Symbolic computing is thus a valuable tool, and Stroscot would be lacking if it left symbolic computing out of its functionality.

Features
========

One simple way of characterizing features is by the values (normal forms) they add to the language and then the operations on those values. There is a wide range of values supported by CAS's such as Mathematica, Maple, Magma, and SAGE: symbolic expressions (terms, including symbols, variables, and binders so as to write derivatives, integrals, and summations), numbers to a specified (arbitrary) precision, machine-precision numbers, rational numbers, (computable) real numbers, complex numbers, strings, lists, sets, polynomials, truncated power series, systems of equations, probability distributions, special functions, records, tables, units, arrays, matrices, tensors (including sparse), functions, graphics, images, plots, files, directories, directed and undirected graphs and networks, dates and times, geometric objects, piecewise and interpolated numerical functions, sound and audio data, signal (time-series) data, algebraic number fields/rings/ideals (finite fields, algebraic extensions, and elements thereof), crytographic keys, combinatorial objects (permutations, combinations, and partitions), generating functions, modular forms, p-adic numbers, combinatorial designs (block designs, Latin squares, orthogonal arrays), databases, logical connectives (and, or, not).

Then we need algorithms and operations on these values. For the numbers, it is mainly arithmetic and special functions. For expression trees, there is simplification (and related transformations such as distributing, combining, and factoring), applying algebraic rules or equations (pattern matching and substitution, such as automatic differentiation), and equation solving. More complex algorithms such as automatic integration combine these in interesting ways.

"Robust" numerical evaluation is bounding a result to within a given accuracy or tolerance. This is contrasted with arbitrary-precision or machine-precision evaluation where each step is done within a given accuracy or tolerance, but numerical error may accumulate over the course of the computation. In this latter case, augmenting the computation with interval arithmetic or other error tracking methods may reveal a wide range for the actual answer and the produced answer may be quite different from the actual answer. Generally robust calculations use arbitrary-precision calculations internally but adaptively increase the precision so as to avoid accumulating too much error in intermediate calculations. Robust numerics should be formally verified that they produce accurate results.

::

  e = cos(1)^2 + (sin(1)^2 - 1)

  double e # Machine precision (double) floating point
  --> 5.551115123125783e-17

  mpf e {digits=50} # Arbitrary precision floating point
  --> mpf('1.3363823550460978230702682335757409394075984400525243e-51')

  robust_evalf e {digits=50} # Robust floating point
  --> sympy.Float(-(1 +- 1)*2^(-531))

  simplify e # exact/symbolic
  --> 0



Another good feature is logic programming - it would be nice to allow equation solving to be seamlessly integrated with logic programming using a satisfiability-modulo-theories (SMT) solver. Similarly, one should be able to write a loop that sums the numbers from 1 to n, and then solve for the n that produces a desired value. This leads nicely into theorem proving and verification tools.

Design
======

Following Stroscot's philosophy of "minimal core", as little as possible of the implementation should be in the language core. We will need to represent symbolic expressions - the easiest thing is to make these syntax trees, so that algorithms are macros that operate on syntax. But of course specific algorithms will operate on restricted subsets of syntax trees.

Oscar Benjamin has been working on speeding up SymPy and has written about `how he would redesign SymPy <https://oscarbenjamin.github.io/blog/czi/post1.html>`__. It is certainly worth taking his design points into consideration. According to Benjamin, fast symbolic computation depends on implementing three things efficiently: robust numerical evaluation, symbolic manipulation, and computational algebra.
