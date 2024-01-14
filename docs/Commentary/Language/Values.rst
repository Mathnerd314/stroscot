Values
######

A value represent an element within the `universe of discourse <https://en.wikipedia.org/wiki/Domain_of_discourse>`__. As Stroscot aims to maximize functionality, the goal is to have an unfettered universe of discourse in which all applications of words are understood according to their common conditions and meanings. The notion of value in Stroscot is thus more a linguistic property, like "proper noun", than a semantic property. Specifically we can define a value in Stroscot as follows:

* Canonical representation - A value has a canonical representation, usually via text (UTF-8) but perhaps a binary file format like a PNG or network packet. This representation should generally be used consistently for writing and displaying the value. It can also be hashed and compared for equality. Generally, equality is obvious, but for lambdas etc. I will define it as observable equality.

* Variations - A value may have minor variations in syntax or notation that do not change its meaning. For example, "1/3" and "⅓" represent the same rational number value, but they differ in orthography, or a list (tuple) may be written as ``[1,2]`` or ``(1,2)``, or a regex value may be written as ``new Regex("[0-9]+")``.

* Context-Independence: Values maintain their meaning and significance across different contexts and languages. Beyond variations in syntax, they are not dependent on the context in which they appear, the specific programming language used, or compilation options.

* Non-Reducibility: Ideally, values are normal forms, meaning they are not subject to evaluation, reduction, or computation. For example, "1/3" and "let x=3 in 1/x" may be equivalent under reduction, but only "1/3" is considered a value because it cannot be further reduced or evaluated. WHNF is not sufficient to ensure a value, e.g. ``[1,undefined]`` reduces to ``undefined`` hence is not a value. In practice, reduction rules are dependent on context, so we take a liberal perspective and accept values as "values" even if they are not normal forms in all contexts.

On account of their canonical representation, the simplest concrete representation of a value is as a string, but in practice values will generally be represented in memory as more efficient formats, for example a list as a contiguous array of pointers - see :ref:`Memory`. This representation must be lossless, in the sense that converting from a string to the format and then back to the string should be the identity.

Values are immutable (as `Rich Hickey says <https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg>`__). In terms of memory management, values are just data - they can be copied freely, and discarded once they will no longer be used.

Colors
======

So each value has at least two parts (conceptually): there is the "human-readable" value, like for example the integer 1. Then there is the "color" or user annotated type, and that's generally a symbol like ``MyAmazingInteger64``. So when we talk about a value in Stroscot, we are talking about the human-readable value plus the color - ``MyAmazingInteger64 1``. Stroscot has no way to talk about the human-readable value alone, because it would be ambiguous; every value in Stroscot has a color. Would we be talking about the ``1`` stored as the standard library's color (written as just ``1``)? Or about a more specific library's colored ``1``? The question is somewhat pedantic, because we can generally easily convert between values of different colors, and most of the time this conversion is somewhat automatic. But, for purposes of the language semantics, values of different colors are not "equal", because it is easy to distinguish the color.

Once we get into optimization, we may start using identical memory representations for values of different colors, erasing checks and conversions and so on, because checking the color at each point is slow. For example, SQLSafeString and String will most likely have the same memory representation (it's not like there's an amazingly efficient way of representing sanitized strings that's different from representing normal strings). But during optimization, we are already considering equivalence classes of programs rather than programs - simply by optimizing, we are improving the speed of the program, which is an observable property, and therefore we are changing the behavior of the program by optimizing. The reason this is allowed is because optimization considers programs with the same output value to be equivalent.

Bare symbols
============

Bare symbols are perhaps the simplest value. Bare symbols are simply a sequence of characters. Vaguely like `Raku <https://docs.raku.org/language/syntax#Identifiers>`__, an ordinary bare symbol is a Unicode word, and an extended symbol uses an escape sequence ``@"sym str"`` to allow spaces etc. following the syntax for a string. An extended symbol matches a corresponding ordinary symbol if that form exists, i.e. ``null == @"null"``. It is a bit tricky to write a bare symbol in proper Stroscot as most symbols are understood to be qualified.

Term
====

A term is a value applied to other values, with no reduction rules applicable.

::

  tree (combine (leaf 1 2) (leaf 1 2))
  (++++) a b
  (*) (some (weird thing)) 12
  1 2

Terms subsume algebraic data types. Whereas an ADT value is a symbol applied to other values, with restrictions on the types of those values, a term is "untyped" and has no restrictions on its constructor or arguments.

Term application also includes the other function syntax - keyword arguments, implicit arguments, and so on - so is more than just positional application.

SCC
===

An SCC value is a strongly connected component of terms that reduce to each other but not anything else. For example ``\(\x. x)(\x. x)`` is an SCC because it reduces to itself and not anything else. It is a bit strange as a value though because there may be multiple ways of writing it; for example with the rewrite system ``a = b; b = a``, both ``a`` and ``b`` refer to the same SCC. Because of this strangeness SCCs are specially handled by the execution engine.

Infinite term
=============

According to the infinite term reduction semantics, we may get an infinite term, defined as a Cauchy sequence of finite terms or simply a Cauchy sequence of terms. If there are no reduction of this infinite term then it is a value. For example with a definition ``x = [1,...x]`` we may get the infinite list ``x = [1,1,1,...]``. To avoid issues with nontermination, infinite terms also need special handling by the execution engine.

Functions/Sets
==============

Ostensibly, a function is a set consisting of tuple pairs ``(a,b)`` with the property that for each value of ``a`` there is at most one value of ``b``. But this definition is circular, so we also hack in many special "base" cases for functions, such as lambdas, rewrite rules, and primitive symbols.

Similarly, a set is ostensibly a function ``isElemOf : Any -> {Present|Absent}``, but it also has hacked in special instances.

Binders/Logical proofs
======================

Conceptually a binder is a structure with some number of "slots" and numbered references to these slots in the body (a value but with "holes" for the slots). In practice, the binder is specified using variables (symbols), and these variable names are preserved for debugging purposes. Nonetheless, renaming the variables should not change the meaning of the binder (alpha-equivalence). The easiest way to ensure this is to use a nameless graph representation but to preserve variable names at the site of the binder as metadata. For example, a lambda expression ``\x. x x`` is really more like ``(\. 1 1, x)``.

The sequent calculus is used to represent binders via derivation trees built up using the various rules. We use the logic described in :ref:`Logic`.

* Jumbo: The Jumbo break rule contains a set of values and a function from those values to a derivation tree. It also specifies a target in each derivation for each side formula. The Jumbo build rule takes a value and a list of left/right derivation trees and combines them, specifying a target for each tree.
* Exponentials: Promotion specifies a target for each formula in the sequent. Dereliction has one target, Weakening has no target, contraction has n (n>2) targets.
* Identity: This is a unique value.
* Quantifiers: This has quantifier build/break, a bit similar to promotion. No-ops at the untyped level.
* Use/def: We must have a global map from identifiers to definitio0ns, and use nodes then list these identifiers
* Syntax definition (equality, sets): These don't even have build/break duality. Again no-ops at the untyped level.

These rules are not included in values of the sequent calculus:

* Cut: Since cut-reduction can eliminate all cuts, a proof tree containing cut is a reducible expression.
* Exchange: This is represented by the target pointers in the derivation tree.

Rewriting system
================

A rewriting system consists of a set of rewrite rules. A (conditional) rewrite rule has the form ``l -> r | C1, ..., Cn`` where ``l`` and ``r`` are both (open) values, i.e. some set of symbols is declared as pattern variables in ``l``/``r``. The conditions are combined with ``and``, and take the form of predicates ``Pi(x1, ..., xm, ->)``, where the ``xi`` are the pattern variables of ``l`` and ``r``, and ``->`` is the rewrite relation of the system. Example predicates are type predicates ``a : B``, and ``a`` joins with, rewrites to, or is convertible to ``b``.
