Types
#####

Stroscot has sets and assertions about whether values are in sets. But are sets types? Is Stroscot typed? If so, is Stroscot statically typed or dynamically typed? Is it strongly typed? Weakly typed? Gradually typed? These sound like reasonable questions. Unfortunately, these terms are ambiguous and controversial. This is an attempt to answer these questions by enumerating definitions, considering each definition, and ultimately discussing it to death.

Are sets types?
===============

:cite:`parnasAbstractTypesDefined1976` identified various definitions of a "type":

* Syntactic - A type is a purely syntactic label
* Representation - A type is defined in terms of a composition of machine types.
* Value space - A type is a set of possible values
* Syntactic and behavior, representation and behavior, value space and behavior - One of the above plus a set of operators or functions that can be applied to the type
* Set of modes - A type is a set of modes. A mode is a property of a variable that defines data representation and access. Any value that can be stored in one variable of a given mode can be stored in another variable of the same mode.

Academics seem to have gravitated to the syntactic definition because it's easier to publish meaningless gibberish. Similarly Haskell, Java, and C++ use a syntactic definition AFAICT. I don't think there are many languages that use representation as this would imply that all pairs of floats are the same type. The set of modes definition (Parnas et al.'s own) does not seem to have been adopted.

Stroscot follows Julia in using the "value space without behavior" definition. (At least, Julia's documentation `says <https://docs.julialang.org/en/v1/devdocs/types/>`__ it follows the "set of values" definition) Castagna calls them "set-theoretic types with semantic subtyping". A related approach is :cite:`dolanAlgebraicSubtyping2016`, which uses syntactic labels forming a distributive lattice. Although distributive lattices are isomorphic to collections of sets, Dolan's approach IMO falls on the syntactic side.

Overall, while I would be justified in calling Stroscot's sets types, as it is one of the listed definitions, it's not perfectly consistent with the common usage today so it could invite a flamewar. It's easier to call them sets.

Is Stroscot typed?
==================

By a "typed" programming language, someone generally means that the language has a type system or type discipline. Wikipedia defines a type system as "a set of rules that assigns a property called a type to the various constructs of a computer program". This phrasing assumes each construct has exactly one "principal" type. But more complex type systems don't have this property. For example with a Haskell GADT ``data R where R : { unR : Int } -> R Int``, ``unR`` may have the type ``forall a. R a -> a`` or ``forall a. R a -> Int``, and these have no unifying type. Haskell just picks a type ``unR : R Int -> Int`` in this case. Really the mapping from expressions to types is many-to-many. Particularly, with Stroscot's definition of types as sets, any set containing the value is a valid type. For example, ``1 : Int``, but also ``1 : {1}`` and ``1 : Any``. So certainly Stroscot has some ambient notion of "the valid types for a value", but in Stroscot's case this is just set membership so it is a little vague in determining whether Stroscot has a type system. ChatGPT lists some features of type systems we can look for: type annotations, type errors, type compatibility, type conversion, type hierarchy.

The purpose of type annotations is clear if we consult `Robert Harper <https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/>`__. Per him, a type system should allow "stating and enforcing the invariant that the value at a particular program point must be a certain type", in the form of a type declaration. Stroscot's set membership assertions have a specific syntax for type declarations and type restrictions, that accomplish exactly this purpose. There are also annotations for declaring the type of a value, for example ``int8 1`` is different from ``int32 1``.

Type errors are also clear. Stroscot's type declarations have an operational semantics as assertions, they check membership in the type and throw an exception if it is not. The overlap with assertions may seem a bit strange but ultimately, they are both expressing program invariants, so in the end they are different syntaxes for the same feature. And assertions can be more natural in some cases than type declarations, e.g. ``divide a b = assert (b != 0); ...`` rather than ``divide : Int -> Int\{0} -> Int``. Although, higher-order types can be somewhat more succinct than writing out logical set membership assertions by hand, ``f : (Int -> Int) -> Int`` vs ``{ s = arbElem; a = arbElem; assume(a isElemOf Int); assume(s a isElemOf Int); assert(f s isElemOf Int) }``.

Type compatibility follows naturally from type errors, for example assembly instructions can only operate on certain bitwidths. Stroscot has a natural form of type conversion and type hierarchy, subtype inclusion, but this is actually not too powerful and most conversions are explicit. Still though, we can talk about the normal data types like integers, floats, etc., so most programmers will find it natural.

It is also worth looking for "untyped" programming languages - ChatGPT lists assembly language, machine code, and Brainfuck. The particular feature of these languages is that all data is represented as bits, and there are no other types. Stroscot does have a related property of "untyped memory access", where for example a floating point number can be stored to memory and read back as an integer. But I would say that because Stroscot has types for non-bit data, like structs for example, it is not untyped.

The conclusion therefore is that Stroscot, like almost every language made these days, is a typed language.

Is Stroscot static?
===================

Definition of static typing are complicated by the omnipresent references to compile-time. For example, Cliff Click's broad definition of a static type system is "something that allows catching errors quickly at compile time". He gives the example errors of calling a non-function or applying a primitive operation to the wrong type. But Stroscot does not use a traditional compilation model - it has an execution engine with a JIT compiler that runs projects directly from source code, and a language server that provides code feedback during development. As such we have the "writing phase" and the "execution phase". We can thus redefine a static type system as "something that allows catching errors quickly while writing the code".

Now, Stroscot's static verification system is very powerful. Invariants can be expressed as assertions and type annotations. This includes invariants on function arguments and invariants on variable values. These invariants allow catching the errors Cliff listed, similar to traditional static type systems, and it also allows functionality like unit testing and model checking. This suite of tools allows catching hard bugs quickly and proving the absence of classes of bugs. Since it all happens during the writing phase, as part of the language server, it allows rapid development of quality software.

The problem is that Stroscot has even more functionality. Whereas a traditional statically typed programming language requires strict and explicit definitions of types, Stroscot allows specifying types, but doesn't require them. Developers can choose - if they want to specify the type, it will be checked during the writing phase, but even if they don't specify any types, they can still run the program without types. Stroscot also still checks typeless programs, identifying potential mistakes and errors.

It's also worth noting an alternative static property, usually referred to as "early binding", where a method call is resolved at compile time to a fixed procedure. Here it is clear, from the usage with for example C++'s virtual methods, that "compile time" refers to the actual generation of the machine code. This is an "execution phase" property of the JIT compiler. I have coined the name "direct method resolution" for this property, namely that the JIT compiler is able to optimize method calls to assembly jumps to specific addresses. This is indeed a planned feature for the JIT.

So is Stroscot static? Yes - it supports all the static features, and more. The problem is that static and dynamic are considered opposites, and Stroscot is also dynamic. So I can't simply say "Stroscot is a static and dynamic language." According to ChatGPT, the "dynamic" aspect gets more emphasis, presumably because "static" has a slightly negative connotation. So I have to phrase it like "Stroscot is a dynamic language which supports static typing features". As such, because Stroscot is dynamic, and has an abundance of functionality, rather than because of any lack of functionality, Stroscot is not a "static language" per se.

Is Stroscot dynamic?
====================

Dynamic programming languages allow flexible and adaptable coding practices. But dynamic languages vary widely in their features. We can identify some common features using ChatGPT:

* Introspection: A mechanism to determine the type of some arbitrary data and retrieve its attributes or representation at runtime. This includes normal values, modules, objects, and functions.
* Type adaptation: The ability to take data of an unknown type and conditionally use it as a value of a specific type during execution. This creates the ability to determine the type of the data.
* Late-binding: Choosing methods at the latest possible moment during program execution. For example, method selection may depend on the real-time types of involved objects, the current state of the source code files (hot-reloading), complex dispatch conditions based on properties of the data, and/or values of unrelated variables in the code.
* Flexible variables: Allowing variables to accommodate any possible data value during program execution.
* Direct execution: Executing source code with a single command, without the need for an intermediate compilation step
* Metaprogramming: Writing macros (code that manipulates other code), and executing code with eval.
* Runtime modification: adding and modifying methods and properties, monkey patching, module loading at runtime
* Good: As a rule of thumb, dynamic languages are good, all other things being equal.

8. **Aspect-Oriented Programming:** Some dynamic languages support aspect-oriented programming, where you can modify the behavior of specific code segments (aspects) independently, allowing for cross-cutting concerns like logging or error handling to be added dynamically.

9. **Dynamic Class Creation:** Dynamic languages often allow you to create new classes at runtime. This is particularly useful for scenarios where you want to create new classes based on data-driven specifications or user inputs.

10. **Dependency Injection and IoC Containers:** Some dynamic languages support runtime dependency injection and inversion of control (IoC) containers, allowing you to change the behavior and dependencies of objects without modifying their source code.

It's important to note that while these capabilities can offer significant flexibility and power, they also come with potential complexities and challenges, including maintenance difficulties and potential performance impacts. Additionally, misuse of runtime modification features can lead to code that is harder to understand and debug. Therefore, careful consideration and judicious use are recommended when employing such dynamic features.

For example, you can add or remove methods and attributes from objects dynamically.

2. **Limited Runtime Modifications:** While Java's reflection allows you to access and invoke methods and constructors at runtime, it doesn't provide the same level of flexibility as dynamic languages for modifying behavior, such as adding or removing methods dynamically.

In summary, dynamic programming languages go beyond the introspection and access capabilities offered by static languages' reflection features. They allow you to modify and extend behavior more freely at runtime, making them particularly powerful for scenarios where adaptability and runtime customization are crucial. On the other hand, static languages like Java focus on providing information about class structures and allow for limited runtime interactions, but these interactions are still subject to compile-time type checking and restrictions.


Here are some key characteristics of dynamic programming languages:

3. **Reflection:** Dynamic languages often provide features that allow programs to inspect and manipulate their own structure and behavior at runtime. This is known as reflection and enables powerful meta-programming techniques.

Some examples of dynamic programming languages include Python, Ruby, JavaScript, PHP, and Perl. These languages are well-suited for tasks where rapid development and flexibility are important, but they might sacrifice some performance optimizations that statically-typed languages offer through their compile-time checks and optimizations.

    Dynamic Typing: On the other hand, "typed" languages can also refer to languages with dynamic typing, where variable types are determined at runtime. While dynamic typing offers flexibility, it can lead to runtime errors if incompatible types are used together.

    Strong Typing: "Typed" languages typically exhibit strong typing, which means that type conversions and interactions between different types are strictly controlled. This prevents unintended type-related errors and promotes safer code.

    Weak Typing: In contrast, some programming languages are weakly typed, allowing for implicit type conversions and interactions between different types with less strict control. "Typed" languages usually lean towards strong typing to ensure code robustness.

    Type Inference: Many "typed" languages feature type inference, a mechanism that allows the compiler or interpreter to automatically deduce the data type of a variable based on its initialization and usage. This reduces the need for explicit type annotations while maintaining the benefits of static typing.

    Type Annotations: In "typed" languages, developers often annotate variables and functions with explicit type information. These annotations serve as documentation and aid in code readability, while also enabling the type checker to catch potential errors.

    Type Safety: "Typed" programming languages prioritize type safety by preventing operations that could lead to data corruption or unexpected behavior due to type mismatches. This contributes to more predictable and reliable software.


Type inference
==============

Type inference is often used with the idea that its failure means there is a type error in the program. But static verification finds those errors already and distinguishes between real errors and algorithm failures, whereas type inference failure could be either.

Type inference means many signatures can be omitted, like unityping with implicitly assigning the universal type. But type-inference algorithms are complex- they can fail, and even if they succeed their results are not obvious to humans. Unityping means the semantics doesn't depend on types at all, meaning one less thing to think about, hence making programming easier. Type inference allows writing some programs without thinking about types, but there is always the chance the program is untypeable - and there are many examples of untypeable programs, e.g. ``\z. (z 1, z "x")`` for H-M. Cliff Click's system can type this but fails on `a more complex program <https://github.com/cliffclick/aa/issues/28>`__ that runs fine in a unityped system. The errors on these untypeable programs will always be verbose (because of the inferred type) and confusing (because the programmer was not thinking about the inference algorithm).

Types can used to describe the ABI, :cite:`jonesTypesAreCalling2009` hence type inference is a form of ABI selection. But the ABI selection is based on performance. Furthermore the ABI types can be conditioned on state, and there is a fallback catch-all format for hard cases. So overall ABI selection uses a separate notion of type based on representation, with no principality requirement like for normal type inference.

At the REPL systems such Haskell provide a command to display the inferred type of an expression, and similarly Haddock can show pretty-printed inferred type signatures. But this doesn't extend well to complex type systems:

* :cite:`naikTypeSystemEquivalent2008` provides a method to interpret the model produced by a model checker as a type derivation using flow, intersection, and union types. Stroscot could similarly be written to output Church-style types reflecting the properties it verifies for every expression. But the types would be complex and precise, e.g. ``length : (Nil-->0) & (Cons a b-->1+(length b))``, hence hard to interpret.
* With subtyping the principal type would presumably be the minimal type containing the value, which is not very informative. E.g. instead of ``1 : Int`` or ``1 : Nat`` the inferred type would just be ``1 : {1}``.
* It is of high complexity to infer `dependent <https://github.com/UlfNorell/insane/>`__ and `circular <https://github.com/gelisam/circular-sig>`__ types

Maybe these issues can be solved by heuristics for inferring types. But it seems that we can solve it more easily:

* REPL inferred types can be replaced by smarter value printing, e.g. ``:show id`` gives ``Prelude.id = \x -> x``, or ``:show [1..100]`` gives ``list of 100 integers``.
* Documentation can simply show the list of developer-defined type signatures (``:t (+)`` giving ``Int -> Int -> Int`` and the other overloadings). Haddock has been able to use GHC's inferred type signatures `since 2008 <https://github.com/haskell/haddock/commit/d300632cbc2346f6d95188426e5db5fbeb7c9f34>`__, but it still encourages explicit type signatures.

So overall it seems type inference is not necessary with the correct design.

Soundness and completeness
==========================

Type soundness means "type preservation", i.e. if ``a : T`` then evaluating ``a`` must produce a value in the type's domain ``〚T〛`` in every denotational semantics. A sound type system rejects incorrect programs by pointing out their type  with a diagnostic. An example of an unsound type system feature is Java's covariant arrays. The program ``String[] strs = { "a" };Object[] objs = strs;objs[0] = 1;String one = strs[0];`` typechecks but produces an ArrayStoreException at ``objs[0] = 1``. Soundness is qualified to a subset of programs S of a language L. If L is unsound but L/S is sound we say L is sound up to S. Java is sound up to covariant arrays, null pointers, and a few other warts. TypeScript is sound up to first class functions and downcasts from the any type. Most type systems are also unsound with respect to nontermination - an infinite loop is of any type but does not produce a value of that type (modeling nontermination as evaluating to ⊥). Type systems sound with respect to nontermination, such as System F, are called "total".

An unsound type system does not prove anything about its programs, so a compiler has to assume the worst and compile with a unityped semantics. Fortunately most "unsound" type systems can be made sound by extending the domains of types to include the missing values. E.g. Haskell is not total but can be made sound with respect to nontermination by including ⊥ in the domain of every type as well as partially defined values like ``(⊥,2)``.

Type completeness is a more vague notion; the common definition is that "all correct programs are accepted, given sufficient type annotations". Java's unsound null pointers allows it to accept some uses of null pointers that would be ruled out with a ``Nullable<T>`` type, making it complete relative to null pointers.

There is also soundness and completeness in logic, which is different:

* A theory is logically sound (valid) if all of its theorems are tautologies, i.e. every formula that can be proved in the system is valid in every semantic interpretation of the language of the system.
* A theory is logically satisfiable if it has a model, i.e., there exists an interpretation in ZFC under which all provable formulas in the theory are true.
* A theory is semantically complete when all its tautologies are theorems, i.e. every formula that is true under every interpretation of the language of the system can be proven using the rules of the system.
* A theory is syntactically complete if, for each formula φ of the language of the system, either φ or ¬φ is a theorem. Alternately, for all unprovable sentences φ, φ ⊢ ⊥ is a theorem.
* A theory is logically consistent if there is no formula φ such that both φ and its negation ¬φ are provable.

Via the Curry-Howard correspondence we can interpret formulas as types and provability of a formula as a program term of that type existing. We restrict to the semantic interpretation that maps formulas/types to sets and evaluate terms to values in those sets. So then:

* A TS is logically sound/valid if every inhabited type T in the semantic interpretation of the language has a  nonempty type domain 〚T〛.
* A TS is logically satisfiable if a semantics exists where all of its inhabited types have elements in their type domains.
* A TS is semantically complete when all nonempty type domains 〚T〛 have program terms of type T (T inhabited).
* A TS is syntactically complete if, for each type T, either T or ¬T is inhabited. Alternately, for all empty types T, there is a program of type T -> Void.
* A TS is logically consistent if there is no type T such that both T and ¬T are inhabited.

Semantic completeness and logical soundness only care about types being inhabited and hence are weaker than type completeness/soundness which care about all specific programs.

Java does not have a ``Void`` type (``void`` is a unit type), but if it did it would most likely be logically inconsistent because a nonterminating program could inhabit the function type ``A -> Void``. In general most type systems are logically inconsistent because a nonterminating loop inhabits all function types. However since all non-Void types are inhabited Java is syntactically complete. Furthermore we can likely formalize the execution model of Java and obtain that Java is logically satisfiable, logically sound, and semantically complete.

So the difficult property to ensure is logical consistency. By Godel's first incompleteness theorem there are no consistent, syntactically complete systems with inference rules of complexity at most :math:`\Delta_{1}^{0}` that contain integer arithmetic. For example System F is consistent and of complexity :math:`\Sigma_1^0 > \Delta_{1}^{0}` but still is incomplete and `cannot type some strongly normalizing terms <https://cstheory.stackexchange.com/questions/48884/are-there-strongly-normalizing-lambda-terms-that-cannot-be-given-a-system-f-type>`__. Intersection type systems extended with negation are complete but inconsistent due to ω. However they are consistent when extended with a complexity :math:`\Sigma_1^0` oracle that computes principal types such that the type contains ω iff the term is not strongly normalizing. :cite:`ghilezanStrongNormalizationTypability1996`

The simplest complete and consistent system is the unitype system. This consists of a universe type whose domain contains all values and its negation the empty type. To ensure consistency we must ensure that the empty type is uninhabited, so all programs must be of the universe type. This means nonterminating programs must have a value in the semantic domain. If we add termination checking we can put nonterminating programs in the empty type and restrict the universe to terminating programs, but this increases the complexity.

Unityping
=========

Per `Robert Harper <https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/>`__ all type systems are static, and dynamic languages are simply "unityped" static languages. "[A dynamic language] agglomerates all of the values of the language into a single, gigantic (perhaps even extensible) [static] type".In Stroscot we follow this description literally, interpreting "unityped" to be short for "universally typed", i.e. the language has a universal type that contains all values. This definition is slightly different from Harper's post, where he interprets "unityped" to mean that there is only the single universal type in the language. We will call Harper's definition "single-typed". If a language is single-typed it must be unityped, since all values are in the single universal type, but not every uni-typed language is single-typed.

Consider the notion of Curry-style types, called sorts in :cite:`pfenningChurchCurryCombining2008`. Sorts define properties that can be checked or ignored, extrinsic to the terms themselves. A term may satisfy several sorts, or none at all. Since the sorts are optional there must necessarily be an operational semantics that does not refer to any sorts, and hence the language is unityped if it has a trivial sort that checks no properties. But even if the language is unityped, it doesn't have to be single-typed, because there can still be more than one type (sort) - in fact there can be a whole language of properties/sorts.

A unityped language means if you write zero type signatures and ignore all warnings the program still compiles and runs and produces a value (although it may be an error). Every non-unityped program has a corresponding unityped program where the values are extended to contain the type information as a tag (reification). Often the operational semantics does not depend on the type and we can simply erase the type. In the specific case of return type overloaded type classes, where type inference is key, the semantics can be made nondeterministic and type annotations can be incorporated explicitly as pruning possibilities.

Practically, one cannot encode unityping scheme in existing static languages. For example, ML's type system is incomplete and hence some terms allowed in a dynamic system, such as the Y combinator, are untypeable. Haskell has unsafeCoerce, which solves the typeability problem, and a `Dynamic type <https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Dynamic.html>`__ which allows interacting with the existing type system. Specifically Clean's Dynamic type (but not GHC's) can store all types. But even though Dynamic can store all values 1-1 it is not a universal type because ``a : Int`` and ``toDyn a : Dynamic`` are distinct values. So unityping also requires subtyping.

Unityping makes the language more expressive: variables can contain all values, and type tests can dynamically check against some type. It does add some overhead to represent members of diverse types, check the tags/types of specific values, and convert between representations, but there are well-known optimizations (Self, Smalltalk, LuaJIT), and it seems that adding unityping will not necessarily decrease the performance of non-unityped programs.

Overall, unityping seems good, hence Stroscot is a unityped language.

Regarding single-typing, Harper gives the example of the complex numbers. Doel extends this: one would like to write a function on the complex numbers, and rule out other forms of input. This can be represented as a runtime check, ``f x = assert (x : Complex); ...``, but clearly if the universal type is the only type it becomes very difficult to express it concisely. Many dynamic languages such as Python, Perl, Lua provide concise type test syntax in some way or another (``isinstance``, ``isa``, ``type``). So it seems a strawman property. In CCC 6/9/23 it was brought up that since the check is formally at run-time, it will generally require running the program over all inputs, rather than being detected at compile time. But this is where model checking comes in, as it can detect potential runtime errors at compile time.

There is the benefit of type signatures that many ‘obvious’ pieces of code can be written automatically. For example Lennart Augustsson’s djinn takes a type like ``fmap : (a -> b) -> Maybe a -> Maybe b``  or ``callCC : ((a -> Cont r b) -> Cont r a) -> Cont r a`` and writes code that has that type. These can be non-trivial to write if you’re just thinking about how it should behave, but the type completely determines the implementation. This sort of functionality seems like it can be offered through a macro, completely separate from the type system of the language.

Model checking
==============

Type systems, and model checkers, both aim to catch some types of errors while allowing valid programs. But most practical type systems like those of ML or Haskell have corner cases. For example the "head of list" function errors on the empty list, but this is not reflected in the type ``[a] -> a``. With dependent type signatures we can accurately capture the behavior for specific cases, ``{x : [a] | nonempty x } -> a`` and ``[a] -> a|Error`` (and even ``{ x : [a] | empty x } -> Error``, but these type signatures overlap and there is no best type signature. In practice, in order to be useful, type systems compromise and treat some "type-like" errors as dynamic errors not handled by the type system. Similarly there is a tension between knowing the size of an array (preventing out of bounds errors) and writing code that is independent of array size.

In contrast, with model checking, we are verifying predicates or sorts. There is no issue with writing multiple overlapping type signatures; simply check them all. We are analyzing the full dynamic behavior of the program, rather than a simplification, so there are no corner cases. Consider ``x = 4 : int; y = x : nat``. We’re assigning an int to a nat here, which could potentially fail. But model checking concludes that 4 is a nat, and therefore that the check will succeed and the program has no errors. In contrast, no type system can soundly allow assignment from a supertype to a subtype, at least without also recording the actual set of values similarly to model checking.

In general, trying to prove any non-trivial property will find all the bugs in a program. But a type system is simpler than a model checker, hence will find false positives more often. Model checking allows unityping, while there are very few unityped type systems.

"Soft typing" is similar to model checking, but uses failure of type inference instead of model checking. This means it cannot prove that it actually found an error, and it must stay within the boundaries of type systems, an open research problem. The verification approach is well-explored and its algorithm produces three types of outcomes: hard errors, passing programs, or verification algorithm failure. Similar to Haskell's "deferred type errors" flag, hard errors can still be ignored, but they will trigger an error at runtime. Similar to soft type checking, verification algorithm failure can be ignored - these may or may not trigger an error.

Roles
=====

GHC's roles are just an optimization for ``coerce``. There are better ways to implement optimizations. It seems like a dirty hack to solve a pressing problem. I think Stroscot can get by without them.



* strongly typed - `8 definitions <https://perl.plover.com/yak/12views/samples/slide045.html>`__, all different. It's the semantic equivalent of "amazing", i.e. "My language is strongly typed" == "My language is amazing". Again discussed solely in the "Types" page.


`This post <https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/>`__ says "a [gradual] type is something that describes a set of values that have a bunch of operations in common", i.e. value space plus behavior. Stroscot's sets don't have behavior so are not gradual types.

But I would also add that Stroscot is optionally typed, because in Stroscot, the `Zero one infinity rule <https://en.wikipedia.org/wiki/Zero_one_infinity_rule>`__ applies. A program can run without any type declarations, with one declaration for the root of the program, or with any amount of type declarations scattered through the program. The no type declarations is an "untyped" setting and ensures there is a complete operational semantics distinct from the type system. The one type declaration enables checking the program for bad behavior, and ruling out common errors such as typos. The unlimited/infinite declarations allows using the power of static verification to its fullest, and may require many iterations of tweaked declarations to get right.
