Types
#####

Stroscot has sets and assertions about whether values are in sets. But are sets types? Is Stroscot typed? These get down to the semantics of type systems. Unfortunately these terms are ambiguous in the literature.  This is an attempt to exhaustively answer the question by considering each definition.

What is a type?
===============

Parnas, Shore & Weiss 1976 identified various definitions of a "type":

Syntactic
    A type is a purely syntactic label
Representation
    A type is defined in terms of a composition of machine types.
Value space
    A type is a set of possible values
And behavior
    One of the above plus a set of operators or functions that can be applied to the type
Their definition
    A mode is a property of a variable that defines data representation and access. Any value that can be stored in one variable of a given mode can be stored in another variable of the same mode. A type is a set of modes.

Academics seem to have gravitated to the syntactic definition because it's easier to publish meaningless gibberish. Similarly Haskell, Java, and C++ use a syntactic definition AFAICT. I don't think there are many languages that use representation as this would imply that all pairs of floats are the same type.

Stroscot follows Julia in using the "value space without behavior" definition. So it has types with meaning! Castagna calls them "set-theoretic types with semantic subtyping". A related approach is :cite:`dolanAlgebraicSubtyping2016`, which uses syntactic labels forming a distributive lattice. Although distributive lattices are isomorphic to collections of sets, this IMO falls on the syntactic side.

`This post <https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/>`__ says "a type is something that describes a set of values that have a bunch of operations in common", i.e. value space plus behavior. Stroscot's sets don't have behavior so are not gradual types.

There are also Curry-style types, called sorts in :cite:`pfenningChurchCurryCombining2008` to distinguish from Church-style types. Sorts define properties that can be checked or ignored, extrinsic to the terms themselves. A term may satisfy several sorts, or none at all. Since the sorts are optional there must necessarily be an operational semantics that does not refer to any sorts, and hence the language is unityped. Stroscot's sets do indeed seem to be sorts or Curry-style types.

Overall, while I would be justified in calling Stroscot's sets types, it's not perfectly consistent with the common usage today so it could invite a flamewar. It's easier to call them sets.

What is a type system?
======================

Wikipedia defines a type system as "a set of rules that assigns a property called a type to the various constructs of a computer program". In this sense Stroscot is unityped, because if you ignore all type warnings the program still compiles and runs and produces a value.

Per `Robert Harper <https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/>`__ the main point of a type system is to allow "stating and enforcing the invariant that the value at a particular program point must be a certain type" (e.g. an integer). Particularly this is to optimize inter-modular calls, so that the ABI is efficient. Stroscot's sets allow expressing this kind of invariant.

Another definition is "something that allows catching errors quickly at compile time", where example errors per Cliff Click are calling a non-function or applying a primitive operation to the wrong type. Stroscot's static verification should catch these. Verification also allows catching "hard" program logic bugs, if you express invariants as assertions. This is IMO more natural in many cases than dependent and refinement types, although those are available with Stroscot's sets as well.

Type inference
==============

It used to be that people preferred type systems that had only one principal type for a given expression, i.e. one where every other type signature was an instance of the principal type. Haskell has grown to a system where this is not the case. Specifically with a GADT ``data R where R : { unR : Int } -> R Int``, ``unR`` may have the type ``forall a. R a -> a`` or ``forall a. R a -> Int``, and these have no unifying type. Type inference would have to pick a type somehow, which would be visible in the ABI. The easier strategy is to give up type inference - Haskell requires a type signature.

Type inference is sometimes useful when playing around at the REPL, but with subtyping it is not as informative. Although ``1 : Int, 1 : Nat``, the principal type would be ``1 : {1}``. So overall it seems we can get away without any type inference.

With a unityped type system, type inference is trivial - if no type annotations are given the universal type can be assumed.

Haskell has a `universal type <https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Dynamic.html>`__, but it can only contain monomorphic values due to lack of polymorphic ``Typeable`` instances. But this seems to just be a GHC limitation; Clean's dynamic can store polymorphic types.

Soundness and completeness
--------------------------

Type soundness means "type preservation", i.e. if ``a : T`` then evaluating ``a`` must produce a value in the type's domain ``〚T〛`` in every denotational semantics. A sound type system rejects incorrect programs by pointing out their type  with a diagnostic. An example of an unsound type system feature is Java's covariant arrays. The program ``String[] strs = { "a" };Object[] objs = strs;objs[0] = 1;String one = strs[0];`` typechecks but produces an ArrayStoreException at ``objs[0] = 1``. Soundness is qualified to a subset of programs S of a language L. If L is unsound but L/S is sound we say L is sound up to S. Java is sound up to covariant arrays, null pointers, and a few other warts. TypeScript is sound up to first class functions and downcasts from the any type. Most type systems are also unsound with respect to nontermination - an infinite loop is of any type but does not produce a value of that type (modeling nontermination as evaluating to ⊥). Type systems sound with respect to nontermination, such as System F, are called "total".

An unsound type system does not prove anything about its programs, so a compiler has to assume the worst and compile with a unityped semantics. Fortunately most "unsound" type systems can be made sound by extending the domains of types to include the missing values. E.g. Haskell is not total but can be made sound with respect to nontermination by including ⊥ in the domain of every type as well as partially defined values like ``(⊥,2)``.

Type completeness is a more vague notion; the common definition is that "all correct programs are accepted, given sufficient type annotations". Java's unsound null pointers allows it to accept some uses of null pointers that would be ruled out with a ``Nullable<T>`` type, making it complete relative to null pointers.

There is also soundness and completeness in logic, which is different:

* A theory is logically sound (valid) if all of its theorems are tautologies, i.e. every formula that can be proved in the system is valid in every semantic interpretation of the language of the system.
* A theory is logically satisfiable if it has a model, i.e., there exists an interpretation under which all provable formulas in the theory are true.
* A theory is semantically complete when all its tautologies are theorems, i.e. every formula that is true under every interpretation of the language of the system can be proven using the rules of the system.
* A theory is syntactically complete if, for each formula φ of the language of the system, either φ or ¬φ is a theorem. Alternately, for all unprovable sentences φ, φ ⊢ ⊥ is a theorem.
* A theory is logically consistent if there is no formula φ such that both φ and its negation ¬φ are provable.

Via the Curry-Howard correspondence we can interpret formulas as types and provability of a formula as a program term of that type existing. We restrict to semantic interpretations that map formulas/types to sets and evaluate terms to values in those sets. So then:

* A TS is logically sound/valid if every inhabited type T in every semantic interpretation of the language has a  nonempty type domain 〚T〛.
* A TS is logically satisfiable if a semantics exists where all of its inhabited types have elements in their type domains.
* A TS is semantically complete when all nonempty type domains 〚T〛 have program terms of type T (T inhabited).
* A TS is syntactically complete if, for each type T, either T or ¬T is inhabited. Alternately, for all empty types T, there is a program of type T -> Void.
* A TS is logically consistent if there is no type T such that both T and ¬T are inhabited.

Semantic completeness and logical soundness only care about types being inhabited and hence are weaker than type completeness/soundness which care about all specific programs.


The tension between consistency and completeness is key. A compiler can forgo some consistency while being sound and complete by using a forgiving type system. Or it can forgo completeness and use a sound type system that removes expressiveness but ensures type safety. The consistent type systems are generally more complex as they attempt to support patterns seen in the complete type systems, while complete type systems can simply make everything unityped.

 but not complete. System F, which most type systems are based on, is sound but `cannot type some strongly normalizing terms <https://cstheory.stackexchange.com/questions/48884/are-there-strongly-normalizing-lambda-terms-that-cannot-be-given-a-system-f-type>`__, hence is incomplete. Similarly any decidable type system will be incomplete if it is sound and total. However, this is merely convention. Systems that are complete are:

* "unsound" type systems, which can be formalized as sound type systems with expanded type domains. For example Haskell's type system is sound but includes nontermination in all type domains.
* Robert Harper's "unitype" or dynamic type system, an "unsound" type system consisting of a single universe type whose domain contains all values
* intersection type systems, which is not "unsound": the principal type contains ω iff the term is not strongly normalizing. :cite:`ghilezanStrongNormalizationTypability1996`.  Similarly :cite:`naikTypeSystemEquivalent2008` provides a method to interpret the model produced by a model checker as a type derivation using flow, intersection, and union types. Stroscot could be written to output Church-style types reflecting the properties it verifies for every expression, and those types would unquestionably be types, and very similar to the set annotations specified, although unlikely to exactly match a set annotation. But the types would be complex and precise, e.g. ``length : (Nil-->0) & (Cons a b-->1+(length b))``, and likely hard to interpret, so this will likely never be implemented.

Most type systems are logically inconsistent because an infinite nonterminating loop inhabits all types.

Static vs dynamic
-----------------

Per Robert Harper all type systems are static. So this is really about unityped vs not.

Non-unityped programs are a subset of unityped programs. Every non-unityped program has a corresponding unityped program where the values are extended to contain the type information as a tag (reification). Often the operational semantics does not depend on the type and we can simply erase the type. In the specific case of return type overloaded type classes, where type inference is key, the semantics can be made nondeterministic and type annotations can be incorporated explicitly as pruning possibilities.

Non-unityped is at most as expressive - there are programs which unityped allows which most non-unityped systems reject.

As far as the amount of type declarations, the `Zero one infinity rule <https://en.wikipedia.org/wiki/Zero_one_infinity_rule>`__ applies. A program should run without any type declarations, with one declaration for the root of the program, or with any amount of type declarations scattered through the program. The no type declarations is an "untyped" setting and ensures there is a complete dynamic semantics, and that programs can evolve distinctly from types. The one type declaration enables checking the program for bad behavior, and ruling out common errors such as typos. The infinite declarations allows using the power of static verification to its fullest, and may require many iterations of tweaking to get right. But verification is more powerful than simply debugging or unit testing and can catch hard bugs quickly and prove the absence of classes of bugs, allowing rapid development of quality software.

"Soft typing" is similar to the verification approach, but uses failure of type inference instead of model checking. This means it cannot prove that it actually found an error, and it must stay within the boundaries of type systems, an open research problem. The verification approach is well-explored and its algorithm produces three types of outcomes: hard errors, passing programs, or verification algorithm failure. Similar to Haskell's "deferred type errors" flag, hard errors can still be ignored, but they will trigger an error at runtime. Similar to soft type checking, verification algorithm failure can be ignored - these may or may not trigger an error.

    Principle: Good type systems must balance permissiveness with strictness.

Type systems must be permissive enough to allow useful programs to be written, and strict enough to catch useful errors. (One important aspect of permissiveness that we've already seen is polymorphism, which allows code to be written for an entire family of types, rather than for one type only.)

    Principle: Even sound static type systems compromise on some "type-like" errors and check them dynamically.

Consider the hd function in ML. The type of this function is 'a list -> 'a. However, clearly, when applied to the nil list, which is a well-typed application, hd cannot return a useful value. One could imagine some type system in which lists are further subdivided into empty and non-empty static types. ML does not take this approach. Instead, it checks dynamically and raises an exception.

In practice, in order to be useful, all statically typed languages compromise and define some "type-like" errors as dynamic errors. The other classic example is an array-out-of-bounds error: one can imagine a language in which all arrays have statically known size. In such a language there would be no such thing as a generic array of integers; instead, there would be an array of integers of length 1, an array of integers of length 2, etc. One would then be able to check statically that all array accesses were in bounds.

The drawback of such a system is that it would not be possible to write functions over arrays of unknown size. This is not considered acceptable for most practical programming. Indeed, the original version of the Pascal language had array types that fixed the size --- it was not possible to write routines that were polymorphic over array size --- and this was one of the reasons that programmers rejected this language.

In practice, most type-safe languages allow array size polymorphism, and check array bounds dynamically.


Practically one cannot encode Harper's "unitype" scheme in existing static languages such as ML, because ML's type system is incomplete and hence some dynamic terms are untypable. Further datatypes in ML require pattern matching to extract the value, endless tedium. A dynamic language provides easy syntax.

multiple forms of complex numbers
  rectangular: 1+2 i
  polar: sqrt(5) e^(i arctan(2))

They are mostly interchangeable, a 1-1 conversion between polar and rectangular. But in practice not, e.g. 0 has only one rectangular form but many polar forms, and the polar angle can differ by any multiple of 360 degrees. Restricting the domain to theta in [0,360 degrees) and r=0 -> theta=0 fixes this.

Some forms are more convenient for some computations. A given computation may require and return results in a specific form. We may overload computations work on both forms, testing which form is given and dispatching to the appropriate sub-computation. Data structures such as sets can contain any form and also other types of values. So there are several sets relevant to programming with complex numbers:
* the set of all rectangular forms
* the set of all polar forms
* the disjoint union of the above (sum type)
* the universal set containing the above and all other values

even if a particular value is an integer, it is a value of universal set.
how do you represent, check, remove, and apply the tag on the value each time it is used?

    Consider:

    x = 4 : int
    y = x : nat

    We’re assigning supertype to subtype here (nat <: int), which could potentially fail. But we can be sure from inspection that it will succeed.

        when you prove properties of your program, you end up finding bugs, almost regardless of what properties you are trying to prove.

        If complex numbers are classified as either rectangular or polar, then if you see:
        f : Complex -> ...
        You know the argument is one of polar or rectangular, although which one is not known until runtime. You have ruled out all other values. With a unityped language you cannot express this restriction.

        Sufficiently fancy types can give enough information to write ‘obvious’ pieces of code automatically, and with proof assistants this can be a dialogue. An elementary example of this is Lennart Augustsson’s djinn, which will take types like ``fmap : (a -> b) -> Maybe a -> Maybe b``  or ``callCC : ((a -> Cont r b) -> Cont r a) -> Cont r a`` and write code that has the type. These can be non-trivial to write if you’re just thinking about how it should behave, but the type completely determines the implementation.
