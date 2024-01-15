Objects
#######

Many people like to use the word "object", as in "object-oriented programming". Stroscot aims to support all the paradigms, so being able to claim that Stroscot is OOP would be a great feature. But our analysis is stymied before it begins: per the C2 wiki, `nobody agrees on what OO is <https://wiki.c2.com/?NobodyAgreesOnWhatOoIs>`__ and there are `many definitions for OO <https://wiki.c2.com/?DefinitionsForOo>`__. For that reason the word "object" is not used in the rest of the documentation. But here we try to figure out what OO is and how to make Stroscot support OO.

For example, Julia sort of is OO but users have `said <https://discourse.julialang.org/t/workaround-for-traditional-inheritance-features-in-object-oriented-languages/1195/27>`__ "Julia doesn’t do a good job here" and "[Inheritance] can help you build deep hierarchies that present data as nice, rolled up extension points for consumers of your library. Currently I don’t see any way to do something similar in Julia without a bunch of trickery/hackery/copying/shims."

Definition
==========

It is 2023, so we can use ChatGPT to cut the Gordian knot of defining OO. It has analyzed many discussions of OOP and distilled the common themes. ChatGPT does not actually give a consistent definition either, changing when you re-generate, but we can distill the common parts of 10 ChatGPT definitions:

* OOP uses objects to represent and manipulate data. An object contains properties and methods, which group related data and behavior.
* The core principles of OOP are encapsulation, inheritance, and polymorphism.

ChatGPT list more aspects of OOP, but inconsistently, including them only sometimes. Similarly `ISO-IEC-2382 <https://www.iso.org/obp/ui/#iso:std:iso-iec:2382:ed-1:v2:en>`__ lists extra concepts. Since we want only what is agreed on, it is clear that these should be ignored, but I will also justify why they can be ignored:

* Objects are instances of classes (8/10, IEC-2382) - prototypes instead of classes is a common twist on OO, e.g. as done in Self, and :cite:`borningClassesPrototypesObjectoriented1986` argues that prototype-based inheritance is simpler than class-based.
* It is widely used / popular in software development (6/10) - We could just copy the OO systems of popular languages like Java. Python, or C++. But copying without understanding is pointless. The goal is rather to deconstruct OO, identify each use case, and cover those use cases. If Stroscot is popular enough it should define OO, just like these languages.
* Abstraction as a third or fourth core principle (4/10) - arguably this is included in the "OOP uses objects to represent and manipulate data" bullet point. In 3/4 appearances it just meant encapsulation, "hiding implementation details", while in the other one it was "representing complex systems or processes through simpler, more generalized concepts." In :cite:`stroustrupWhyNotJust1995`, Stroustrup used abstraction instead of encapsulation, so the variation is probably due to that source. In a more recent `2015 talk <https://youtu.be/xcpSLRpOMJM?t=37>`__, Stroustrup used "encapsulation" instead of "abstraction".
* Message passing (IEC-2382) - this is included in Alan Kay's definition of Smalltalk, but C++/Java do not use message passing, so it is an inaccurate definition of OOP.
* Dynamic binding (IEC-2382) - this is just an implementation technique for polymorphism

Getting back to the definition. It is essentially Bjarne Stroustrup's definition: in :cite:`stroustrupWhyNotJust1995` he similarly requires three principles, abstraction, inheritance, and run-time polymorphism. But it's evolved a bit. I think the ChatGPT version is influenced by the wording of C2, `PolymorphismEncapsulationInheritance <https://wiki.c2.com/?PolymorphismEncapsulationInheritance>`__. I also think the ChatGPT definition is a bit clearer because it separates out "providing objects" from the 3 principles. But given the similarities and that Stroustrup calls his definition the "traditional" one, I think ChatGPT has converged to a reliable definition. Notably, this definition is listed second on C2, pretty close to the top. It is similar to `Rees's <http://paulgraham.com/reesoo.html>`__  "conventional Simula 67-like pattern" of encapsulation, ad hoc polymorphism, inheritance = subtyping, and sum-of-product-of-function pattern, which he states "many people take as a definition of OO". C2 does list caveats:

* It is C++ centric - ChatGPT vehemently disagrees with this, and says the principles also apply to Java, Python, and Ruby. It is true though that the author of C++ coined it, so it is a more pragmatic definition than others.
* The words "encapsulation", "inheritance", "polymorphism" only express so much, and are ambiguous out of context. There is a large amount of convention and code constructs associated with expressing these principles. In fact though, ChatGPT understands these principles quite well and we can just follow up by asking ChatGPT what it means by these various words.

There is also the first definition on C2, associated with the BETA language :cite:`madsenObjectOrientedProgrammingBeta1993`. Per :cite:`kristensenWhenWhyWhy2007` this was first published in :cite:`madsenWhatObjectorientedProgramming1988`. This defines OOP as a language where "a program execution is regarded as a physical model, simulating the behavior of either a real or imaginary part of the world." C2 states "many consider this to be the definitive definition." But I don't like this definition. The "physical" in physical model refers to physics. There are many approaches to physics, but in modern physics we decompose the system into a state, noting the position and velocity of each atom, and the laws, specifying the evolution of the state over time. Similarly, we can decompose a program execution as a state (composed of bits) and a transition rule between states. At this point we are back to the functional view of programming, as we can express the full program execution as a function of the initial state. Objects only appear as an abstraction for manipulating subsets of state, and it is just ChatGPT's definition of objects as collections of properties (attributes) and methods (actions). Indeed, it is not even clear that object "actions" in this framework are well-defined; actions operate on the entire state and may affect multiple objects or state that has not been identified as an object. So this definition raises more questions than it answers, although as far as I can tell Stroscot's continuation approach to I/O fits perfectly into this definition of the OO paradigm.

There is also Alan Kay's "definition". As most sources are careful to note, it is not a definition of OOP at all - it is a definition of Smalltalk. I have looked and it is not clear at all that Smalltalk is a particularly OOP language. Certainly, the Smalltalk manual `describes <https://archive.org/details/bitsavers_xeroxsmallructionManualMar76_5750953/page/n11/mode/2up>`__ a concept of "object", but the word "object" was common at the time, for binary files and database records, and the concept came from Simula, so Kay coining it for his notion is not a big stretch. Kay has `said <http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html>`__ that if he could go back in time he would rename object to something else. He uses "modules" in that email but I think following Erlang a better term might be actor. Does module-oriented programming or actor-oriented programming sound like it is related to OOP? I think not. As of August 2023, Wikipedia has a "citation needed" tag for Alan Kay being the father of OOP - it has been in the article (unsourced) since `2002 <https://en.wikipedia.org/w/index.php?diff=29999>`__.

Briefly looking at the other definitions on C2:

* William Cook: Once you get past the terminology, this defines an object as a record of data and functions. Pretty similar to the "properties and methods" of ChatGPT, but with more of a mathematical flavor.
* Theory of Objects book: An object is a reference to a record of attributes. Similar but with the reference included.
* ObjectsAreDictionaries - Cook's definition, basically.
* Everything is a behavior - no? Even BETA :cite:`madsenWhatObjectorientedProgramming1988` acknowledges that for example addition is not a behavior ``.plus``, it is just an operation
* The "natural extension" of programmer-defined types - there are other forms of types, e.g. ADTs and refinement types, which seem more natural.
* No polymorphism - a weird thing to exclude. I could see excluding inheritance, focusing on encapsulation, but just allowing functions to be included in the record means you get virtual dispatch and polymorphism.
* Generic functions (multimethods) - This misses out on the objects as records. But certainly some of the aspects of OO polymorphism are covered.
* DavidMoon - C++ and Java are not OO? What?
* `Jonathan Rees <http://paulgraham.com/reesoo.html>`__ - he has 9 concepts. Grouping them by ChatGPT's principles, they are: objects (sum-of-product-of-function pattern - 9), encapsulation (encapsulation - 1 and protection - 2), inheritance (specification inheritance/subtyping - 7, implementation inheritance/reuse - 8), and polymorphism (ad hoc polymorphism - 3, parametric polymorphism - 4). Then he has "everything is an object" (EIAO) (5) and "all you can do is send a message" (AYCDISAM, 6). Then he considers the following propositions:

  * Lisp is OO - it has polymorphism, EIAO, and subtyping.
  * Simula 67 is OO - it has encapsulation, specification  inheritance, and objects
  * Java is OO - it has encapsulation, polymorphism, inheritance, and objects
  * E is OO - it has encapsulation, polymorphism, specification inheritance, and objects

  Applying the ChatGPT definition, it is clear here that, as considered by Rees, Lisp is not OO because it doesn't have object values. But actually there are Lisp libraries with ``defclass`` that allow making object-like dictionaries - if you use those, Lisp is OO. And all the other languages are clearly OO, at least if specification inheritance is considered sufficient. The principles are essentially the same as ChatGPT except for EIAO (see :cite:`madsenWhatObjectorientedProgramming1988` again, not an OO principle), and AYCDISAM which is more like Kay's actor-oriented programming. Also sub-type polymorphism, the form of polymorphism most associated with OOP, is missing from Rees's list.

  Another argument against EIAO comes from its `definition in Crystal <https://crystal-lang.org/reference/1.9/syntax_and_semantics/everything_is_an_object.html>`__ - a value is an object if it can "respond to some methods". But with multimethods, for any value we can write a function which operates on that value, so EIAO trivially holds.

* Chris Date "An object is essentially just a value (if immutable) or a variable (otherwise)." - certainly a good property, but I think the dictionary definitions capture much more of the intuition
* Binding Behavior to References - unless "binding behavior" means dictionaries, it's missing a key component of OO, the field and method names

Definition of object
====================

For the first part of OO, we must define objects, and explain how they can contain properties and methods. For the most part I agree with `ObjectsAreDictionaries <https://wiki.c2.com/?ObjectsAreDictionaries>`__, i.e. objects are a mapping from strings to values. But I want one fewer concept in the core language so I will instead define objects to be modules, in the ML sense: a module is a list of definitions, and these definitions collectively define a mapping from expressions to values. The expression evaluated in the context of an object/module will usually be an identifier, corresponding to a dictionary lookup, but this definition lets us define values for function calls and other bits of syntax as well, so we can write DSLs more easily.

We want to create and manipulate objects. So we have some operations on modules:

* literal syntax
* evaluate an expression in the context of the module
* inspect/replace/remove a definition
* list all definitions
* change module imports/exports

Per some cursory reading, these include all the object creation and manipulation idioms of `Self <https://handbook.selflanguage.org/SelfHandbook2017.1.pdf>`__ and `Javascript <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Working_with_Objects>`__.

No autoboxing
-------------

Stroscot does not subscribe to Smalltalk's notion that "everything is an object". Rather, in Stroscot, "everything is a value", and booleans, integers, and objects (modules) are different types of values.  This is a little similar to Java's notion of "primitive types", but unlike Java, Stroscot does not make objects reference types - objects have value semantics, and are part of the universal type. Stroscot's notion of object is more similar to the notion of "pure object" in :cite:`cookUnderstandingDataAbstraction2009`, where an object is an immutable record of functions that themselves take and return immutable objects and primitive values. To express the Java notion of mutable object, you would use Stroscot's mutable variables, in particular a Java object could be represented as a variable containing an object whose methods/functions took mutable variable references pointing to more objects, and whose data fields are mutable variables.

Java and C# have implemented a feature called "autoboxing" where primitive types are automatically converted to "wrapped primitive" objects like Boolean or Integer. This allows using primitives in places where an object is required. But this is a leaky abstraction; autoboxing actually changes behavior. For example in Java ``new Integer(0) != new Integer(0)``, you have to do ``Integer.valueOf(0) == Integer.valueOf(0)`` or use ``.equals``. In JS ``false`` is falsy but ``new Boolean(false)`` is truthy. Properly speaking, primitives are distinct from objects, in that they do not support key object operations such as looking up identifiers. Ultimately, discarding the "wrapping primitive" notion and simply representing primitive values directly is the most logical. Supporting a universal type that can contain both primitives and objects does complicate code generation, but it's not that bad.

No object identity
------------------

In Java, objects have an identity. For example, ``new Object() == new Object()`` returns false because two different object references are constructed and their addresses are different. But when you try to do ``new Object() <  new Object()`` it's disallowed because the address is an implementation detail. So just for consistency's sake ``==`` shouldn't work either - if the address is an implementation detail, then don't expose anything about it! But apparently the desire for a fast pointer equality hack won out over the desire to avoid exposing implementation details of the language. As :cite:`cookUnderstandingDataAbstraction2009` says, "primitive equality exposes representation and prevents simulation of one object by another."

In Stroscot, objects are values, so they don't have identity. Syntactically identical objects will always compare equal. However, mutable variables have identity - their value is the address, and allocating a new mutable variable creates a new address hence a new identity.. So in the emulation of Java objects as mutable variables containing objects, we can compare ref-to-object for address equality, and also dereference the refs and compare the object values. Hence the Java notion naturally decomposes into the combination of two concepts. Immutable objects work much better with the functional programming paradigm. Furthermore JSON cannot easily represent the notion of object identity, whereas object values are easily written.

No implicit synchronization lock
--------------------------------

Another (mis)feature of Java is the ability to write ``synchronize (random_object)`` and use any object as a lock. This adds some bytes of header to every object allocation. It has been acknowledged by `the Java tutorial <https://docs.oracle.com/javase/tutorial/essential/concurrency/locksync.html>`__ that the ability to use any object was overly broad and it is better to use specific lock objects. `This post <https://shipilev.net/blog/2016/close-encounters-of-jmm-kind/#_horror_circus_kinda_works_but_horrifying>`__ points out that you can synchronize on primitives (actually the corresponding autoboxed objects) and on strings. Although it works for small examples due to interning caches, it will fail if your program uses too many primitives/strings as locks. This sort of tomfoolery seems pretty stupid, and there is an easy fix: if you want a mutex you should have to create a value of the ``Mutex`` type.

Encapsulation
=============

According to 8/10 ChatGPT definitions, encapsulation refers to hiding the internal workings of an object from the outside world. The object's data can only be accessed or modified through a well-defined interface. Typically this interface is defined by marking certain methods and fields as "public", meaning they can be accessed by the outside world, while others are marked "private", meaning they can only be accessed by the object itself.

In 4/10 definitions ChatGPT thought encapsulation referred to the idea of bundling data and methods into a single entity. But it should be clear that this is the basic definition of "object", rather than a principle of OOP. (The numbers for bundling and hiding sum to 12/10 because 2 definitions had both bundling and hiding)

:cite:`snyderEncapsulationInheritanceObjectoriented1986` says this controlled interface serves as a contract between the object and its clients. So long as this contract is maintained, the object can be reimplemented without affecting any clients. To maximize the advantages of encapsulation, the contract should avoid including implementation details. For example, it should be possible to rename instance variables without affecting clients. Encapsulation provides a way to protect an object's integrity, ensuring a consistent and valid internal state. Encapsulation promotes modular software evolution and maintenance.

:cite:`cookUnderstandingDataAbstraction2009` argues that encapsulation is really a property of
ML modules, because only ML modules provide sophisticated sharing mechanisms that allow multiple implementations and uses of multiple abstractions to coexist. ML of course allow the basic hiding feature, by not exporting the representation of a type. Unlike Smalltalk, which disallows ``this.foo == b.foo`` even if ``b`` is an instance of the current class, ML modules allow inspecting the representation of more than one value at the same time. ML modules also allow defining multiple abstract data types in the same module, so that a complex internal representation may be defined and manipulated without recourse to C++'s "friend" qualifier. The only feature missing from ML modules is mixing values of two different implementations - this is solved in Stroscot by allowing functions to use duck typing, so that two values may be mixed if they both conform to the proper interface. Since Stroscot defines objects to be ML modules, all the benefits of encapsulation are provided.

Cook goes on to state that "any programming model that allows inspection of the representation of more than one abstraction at a time is not object-oriented." So by his definition C++ and Java are not object-oriented - bleh. In fact this is just a limitation of ML - ML cannot inspect/pattern match on functions; they are opaque. In Stroscot, it is possible to match on the lambdas in Cook's Figure 8 and determine if an ISet was constructed via the Empty, Insert, or Union implementations. We might as well have written ``data ISet = Empty | Insert int ISet | Union ISet ISet`` as in the ADT implementation, except that the lambda presentation is an open data type that allows adding more cases. In Stroscot, we use multimethods to solve the expression problem, so it is just defining symbols and adding more dispatch cases to the relevant multimethods.

.. code-block:: none

  interface ISet = {
    isEmpty : bool,
    contains : int → bool,
    insert : int → ISet,
    union : ISet → ISet
  }


Stroscot has modules, which provide encapsulation. We can create a type and not export its constructor symbols, so that the type becomes an abstract data type: only functions defined in the module can access the concrete representation of the type, and functions outside the module can only use the public interface. This indeed allows renaming fields of the data type without affecting external clients.

No constructors
---------------

A Java constructor has many limitations compared to a factory function: it must allocate new memory, it cannot return a subclass, and it has to be called with a noisy "new" syntax and a fixed name.

For example, consider a boxed primitive boolean. It only needs two values: a factory function can construct one true and one false and then return those from then on. But using a constructor forces the program to produce millions of distinct trues and falses, creating significant overhead.

Another difference is that a factory function computes the field values first and then can use an allocate-and-initialize primitive. The primitive can ensure that its allocation is private, hence appears atomic for concurrency. In contrast a constructor allocates memory initialized to a default value and then overwrites each field. This implicit memory writing means that concurrency and constructors interact poorly because you can access partially-constructed objects. `This page <https://counterexamples.org/under-construction.html>`__ describes several bugs in real-world systems.

Deserialization bypasses defined constructors and directly creates objects via the runtime - it is an implicit public constructor. In fact this deserialization constructor is exactly the allocate-and-initialize primitive that a factory function needs.

One use of constructors is to enforce invariants (validity checking); for example a time constructor that ensures ``0 <= minutes < 60``. In Stroscot, invariants like these are defined in types, and checked on use, rather than on construction. It is often very helpful to be able to talk about about an object whose fields are unnormalized, which the constructor pattern prevents. And when you need the invariants, the types establish object integrity. Whereas in Java you must reason about all mutating methods to identify the possible states of an object, in Stroscot only the type needs to be examined.

A minor downside of doing away with constructors is that factory functions are not automatically marked in the documentation, so can be harder to find. Organizing the source code and documentation to group factory methods is not hard, the hard part is enforcing that such a convention is followed consistently. But it's not even clear that grouping factory functions together is the best organization.

All-or-nothing internal access
------------------------------

Inheritance should respect encapsulation, so that inheriting classes do not get any more access. Thus we see that Java's protected keyword is a hack. Allowing non-public instance variables to be accessed by subclasses breaks encapsulation. Instance variables should either be public and accessible to everyone, or private and not accessible to anything outside a module.

In Stroscot, if you can access the term's constructor symbol, you have full data access to all fields and can destruct and create values with that constructor. But, you can avoid exporting a constructor symbol from a module - that means a user will have to use the defined factory functions and accessors, or else deliberately import the ``._internal`` module.

Inheritance
===========

Per 10/10 ChatGPT definitions, and Wikipedia, inheritance allows objects to inherit properties and methods from a parent class (class-based inheritance, 8/10) or other objects (prototype-based inheritance, 2/10). This allows for code reuse (3/10) and forms a hierarchy of objects (2/10).

Inheritance originated from Simula where per :cite:`nygaardDevelopmentSIMULALanguages1978` they were trying to model a toll booth on a bridge, with a queue of cars which were either trucks or buses. The queue was modeled with a "circular list" structure, consisting of a "set head" and a variable number of "links", each with a predecessor and successor reference. The trucks and buses are modeled as collections of static properties according to a schema. Inheritance thus appeared as a "concatenation" or "prefixing" mechanism for "gluing" each of the various vehicles (trucks, buses) together with a "link" for an intrusive list to make one record instance. As `this post <https://catern.com/inheritance.html>`__ argues, inheritance was invented as a performance hack.

Bjarne Stroustrup has explored `OO without inheritance <https://www.youtube.com/watch?v=xcpSLRpOMJM>`__, and similarly :cite:`cookUnderstandingDataAbstraction2009` states "[inheritance] is neither necessary for, nor specific to, object-oriented programming." If we had a definitive study showing that inheritance makes systems more complex, bug-ridden, and unmaintainable, then we could just leave inheritance out on the grounds of it being a footgun. Certainly there are suggestive blog posts like "Inheritance is evil and must be destroyed" (`1 <https://blog.berniesumption.com/software/inheritance-is-evil-and-must-be-destroyed/index.html>`__). But :cite:`elemamConfoundingEffectClass2001` lists several cases where a promising study found that inheritance caused difficulties, but follow up studies/replications have found the opposite conclusion. And the regression model in that paper invalidates all of the tested inheritance metrics (DIT, NOC, NMO, NMA, SIX) as having no statistical relationship after controlling for lines of code. It is still possible that inheritance may make programs more difficult to understand, but nobody has created a metric and done a study with enough statistical power to confirm or deny that conclusion definitively. TODO: Maybe Jan Vitek (co-author of some large scale Github studies) would be interested.

Since there is little evidence, and it is customary to include inheritance in OOP, it is better to err on the side of inclusion. Following :cite:`cookDenotationalSemanticsInheritance1989` we shall see we can implement pretty much all the common patterns of inheritance as library functions.

Implementation
--------------

Combining records is a basic operation. There are a few choices for handling conflicting definitions, like ``combine {a = 1} {a = 2}``: we can error (``combine_bot``), we can be right-biased like Simula (``combine_r``), ``{a = 2}``, or we can be left-biased ((``combine_l``, as in :cite:`cookDenotationalSemanticsInheritance1989`). But most OO languages also support a "qualified lookup" mechanism that allows specifically accessing shadowed attributes, so really combining in the context of inheritance stores both fields, like ``{ parent: {a = 1}, main: {a = 2} }`` or ``{ a = { parent: 1, main: 2} }``. It is just convenient to omit this extra structure when no names are shadowed and unqualified lookup suffices. :cite:`taivalsaariNotionInheritance1996` also mentions "defeating" or "cancelling" a property. This involves the subclass's record containing a "whiteout" entry so that looking up that property returns a not found exception rather than a value. It really is an extension of the combination algorithm and doesn't affect much of the design.

The difficulty in inheritance lies in that inheritance allows self-reference, for example method calls in Java can refer to ``this`` and ``super``. This means constructing the object requires tying up a recursive knot, so that the methods refer to the parts properly. :cite:`cookDenotationalSemanticsInheritance1989` models the inheritance process as follows: you have a "generator" parent function (a function whose fixed point is an object), and a "wrapper" child function (that takes self and super parameters representing the final object and the superstructure). Cook lists several variants:

* wrapping with distributive application, ``extends_app w g = \self. w self (g self)``. This allows full control of the result by the wrapper function.
* wrapper application with combination, ``extends_combine w g = \self. combine (g self) (w self (g self))``. This is equivalent to the previous with ``w' w = \self super. combine super (w self super)``. This is the more practical operator in most cases. For example it is used in `Nixpkgs <https://github.com/NixOS/nixpkgs/blob/d44a67c4ba1a01614f236213b3f64e17bb107879/lib/fixed-points.nix#L91>`__.
* Selective inheritance, ``extends_select w g = \self. combine_compose (w self) (g self)`` where ``combine_compose m p s = m s . p s``.
* Multiple inheritance. This can apply to all the previous; the distributive application is easiest, the single generator ``g`` is replaced with a list ``gs``: ``extends_app_mi w gs = \self. w self (map gs self)``. Similarly combination-based multiple inheritance is ``extends_combine_mi w gs = \self. combine (reduce combine_bot (map gs self)) (w self (map gs self))``.

Looking at the multiple inheritance variants, it is clear that composing wrappers and generators in a one-by-one fashion is getting unwieldy. For example, it is unclear how to represent C++'s virtual inheritance, where not only is there a list of direct parents but also a map of "virtual" parents. A lot of the intermediate stages of composition are not relevant; they are abstract, uninstantiable classes. For example mixins cannot be instantiated at all; how do we prevent attempting to ``fix`` them? I think a more flexible and expressive model is a function ``mkObject`` - rather than writing ``fix (extends f (extends g base))``, we write ``mkObject [f,g,base]``. This captures the usage pattern more clearly. Although, as Cook hints at in the comparison with Kamin's semantics (11.2), ``extends`` is a little bit more compositional because it assigns a meaning to the partially constructed classes, it is not clear that this has much more meaning that applying ``mkObject`` to a partial list of classes. Also, with ``mkObject``, it becomes clear that we can replace the list of class generators with other data structures, like a tree for multiple inheritance and an auxiliary map structure for C++'s pattern of named virtual superclasses. Similarly, with ``static``, the structure becomes further complicated because we must distinguish class and instance variables and methods. Due to the recursive knot, and the variance of the type of the subclass reference ``self``, specifying the type of ``mkObject`` is tricky - it requires dependent types and heterogenous lists. You can see the full implementation in Stroscot `here <https://github.com/Mathnerd314/stroscot/blob/master/library/inheritance.txt>`__. Perhaps this is another reason why Cook uses ``extends``, it at least has a relatively simple type.

There is definitely a lot of choice in how to implement inheritance and its semantics. For example, in Ecstasy, ``super`` refers only to the parent method, and other methods of the superclass cannot be called. In my implementation, I went the other direction, preserving the ability of ``super`` to access the full parent object, and adding a ``thislvl`` keyword to represent the current level of the constructed object, in case a descendant object shadows a variable/method. Cook goes through inheritance in Simula, Smalltalk, Beta, and Flavors and shows how they can be modeled with the "fixed point of composition of wrappers" model and translation into records and functions. It is not exhaustive, but I would say that most likely, users will be able to write a ``mkObject`` or ``extends`` variant that satisfies pretty much all of their inheritance needs, and then wrap it up in nice syntax with a macro. But to be safe, I would also say that an auxiliary lookup function (like an overloaded ``.`` operator) to handle shadowing must be provided as well.

BETA is one example of strange inheritance - per `this <https://journal.stuffwithstuff.com/2012/12/19/the-impoliteness-of-overriding-methods/>`__, it inverts the dispatch order. It is the least derived class in the chain that is called first, that then can call ``inner()`` to dispatch to a subclass. We can implement this kind of inheritance using a prefix-biased lookup method and a similarly reversed ``extends`` method. :cite:`taivalsaariNotionInheritance1996` pg. 463 mentions that although BETA's method order looks completely different from the Smalltalk order, they can in fact simulate each other by systematically placing explicit calls to super/inner in the right places.

Usage patterns
--------------

Having defined inheritance, and implemented several variants as a library, we are still not finished with inheritance in Stroscot. We must also examine the common patterns of inheritance to see if there are any other amazing operations hiding in the dark.

:cite:`temperoWhatProgrammersInheritance2013` found that in Java, on average, 3 out of 4 types were defined using some form of inheritance, and that >99% of inheritance in Java can be classified as subtyping or reuse. Specifically (S a subclass of T):

* Subtyping is where an object of type S is supplied where an object of type T is expected. This can be done by assigning an object of type S to a variable declared to be type T, passing an actual parameter of type S to a formal parameter of type T, returning an object of type S when the formal return type is T, or casting an expression of type S to type T. 76% (range 11% - 100%) of class-class relationships had a subtype usage somewhere in the code. Meyer describes several patterns of subtyping inheritance (assuming B inherits from A):

  * Subtype inheritance - A is partitioned into several disjoint subsets B1, B2, B3.
  * Restriction inheritance - B is the subset of A that satisfies a certain constraint, enforced by the constructor and public interface of B. New features should directly follow from the added constraint.
  * Reification inheritance - subtype inheritance but specialized to data structures
  * Structure inheritance - restriction inheritance but specialized to structures
  * View inheritance - A is split into several types B1, B2 where each subtype represents a way of viewing a certain value space. For example 2d_Coordinates is split into Cartesian_Coordinates and Polar_Coordinates, or List is split into ArrayList and LinkedList.

  Stroscot's expressive type system allows expressing all of these patterns directly, without using inheritance. And the ``or_subclass`` predicate allows expressing the subtyping inherent to inheritance.

* Reuse is when a method not in T, either in S (internal) or not in S (external), invokes a method m() or accesses a field f on an object constructed from type S, and m() or f is declared in T. 22% (4%-88%) of CC edges were external reuse and did not have a subtype usage, while 2% (0.5% - 30%) of CC edges had internal reuse but no subtype use or external reuse. So together these accounted for 24% of edges. Meyer describes a few patterns that seem to fall into this category:

  * Extension inheritance - B introduces attributes not present in A and features not applicable to direct instances of A. This changes the set of values. Myers tries to argue that open records give a subtyping relation, but this is flawed because a self type can appear in both covariant and contravariant positions, so in general B is neither a subtype nor supertype of A.
  * Variation inheritance, uneffecting inheritance - B redefines some features of A and does not introduce new attributes or features.
  * Implementation inheritance - we want to write a new version of the class with most of the methods shared, but not actually duplicate the file

  These forms of inheritance seem suited for the inheritance library described before. Java-style inheritance actually seems less useful for these cases because it imposes a rigid class structure on reuse. And also, since these patterns are reuse, we could implement them without inheritance, by constructing the objects directly.

:cite:`temperoWhatProgrammersInheritance2013` also did some analysis on the remaining <0.1% of other relationships. The "constants class" was where a class or interface of only static final constants was inherited from to gain access to the constants. Meyers calls this "facility inheritance". This accounted for 1% of CC edges in some systems but most had no constant classes. A wildcard import seems a lot more straightforward. One system used a lot of "super" calls in the constructor, but for no discernible reason. Probably not worth emulating. Some "framework" relationships used third party types and could not be analyzed fully due to lack of source code. "Generic" containers cast to Object and back so impeded subtyping analysis - again Stroscot's type system seems sufficient here. And still other inheritance relationships were just there and no amount of inspection by the authors could discern a useful purpose.

So the conclusion is that no, inheritance is not a silver bullet and the usage patterns are much covered in Stroscot.

Inheritance is not subtyping
----------------------------

This is the title of :cite:`cookInheritanceNotSubtyping1989`, and their argument seems correct. The existence of ``Lens' (a+b) a = { view : (a+b) -> a, over :: (a -> a) -> (a+b) -> (a+b) }`` does not imply a subtype relation - ``A`` is not a subtype or supertype of ``A+B``. Rather ``A`` is related to ``A+B`` by a separate "is subcomponent of" relation, as formalized in the Lens type. For example, in :cite:`cookInheritanceNotSubtyping1989` section 3.2 page 129 we have a parent constructor ``P self super thislvl = { i = 5, id = self, eq = \o -> self.i == o.i }`` and a child constructor ``C self _ _ = { b = true, eq  = \o -> o.i == self.i && o.b == self.b }``. We can work out some types: ``mkObject [P] : mu self. { i : int, id : self, eq : {i : int}_open -> bool }`` and ``mkObject [C,P] : mu self. { i : int, id : self, b : bool, eq : {i : int, b : bool }_open -> bool }``. The second has more fields than the first, so with closed records they are unrelated types. We might think (as Meyers does) that with open record types we could say that the second (child) type is a subtype of the first. But looking at ``eq``, since ``{i : int, b : bool }_open`` is a subtype of ``{i : int }_open``, by contravariance the first ``eq`` type is actually a subtype of the second. So even relaxing our record subtyping definition these are unrelated types.

More generally, all combinations of subtyping and inheritance are possible:

* S is neither a subtype nor a child type of T - independent types, Boolean and Float
* S is a subtype but is not a child type of T - Int32 and Int64, subset but unrelated by inheritance
* S is not a subtype but is a child type of T - S child of T, S -> S is not a subtype of T -> T
* S is both a subtype and a child type of T - when all inherited fields and methods of the derived type have types which are subtypes of the corresponding fields and methods from the inherited type, and the type is an "open record"

Note that subtype + derived type is only possible with open records - with closed records no derived type is a proper subtype. :cite:`abdelgawadNOOPDomaintheoreticModel2018` formalizes this notion of open records and shows that in Java and other nominally-typed OOP languages, "inheritance is subtyping". More specifically, "a class B is a subtype
of a class A, in the open record sense, iff B inherits from A." But this property is obtained by placing restrictions on inheritance - in Java, a method only overrides its parent method if its type matches the parent method, and methods cannot be removed. :cite:`taivalsaariNotionInheritance1996` calls this "strict inheritance". Strict inheritance is a pretty weird restriction from a unityped perspective - for example in Smalltalk we can override a field and change its value from an int to a string. So this "inheritance is subtyping" property is a form of type discipline, rather than a free property.

Inheritance-as-subtyping is easy to misuse and the Java platform libraries made numerous mistakes: Stack extends Vector, Properties extends Hashtable - in both cases, not using inheritance and thus avoiding the accompanying field/property inclusion would have been preferable. For example, with Properties (`1 <https://codeblog.jonskeet.uk/2006/03/04/inheritancetax/>`__), ``(Properties) p.getProperty(key)`` takes defaults into account, while ``p.get(key)`` which is inherited from Hashtable does not, and direct access to the underlying Hashtable allows adding non-String objects, when the designers intended that Properties should only be Strings. Once this invariant is violated, it is no longer possible to use other parts of the Properties API (load and store). Without inheritance-as-subtyping, ``get`` could have been overridden to be a subtype, and the other Hashtable methods deleted.

Inheritance as subtyping breaks encapsulation, because superclass methods that expect to receive themselves may receive a subclass instance that doesn't support an expected contract. In particular, a call to self.b in A.a may resolve to an inherited implementation B.b, and this B.b may violate a contract that A.b satisfies. Even adding a method in the subclass can be unsafe, because the superclass can later add the same method and then you are unintentionally overriding it. For this reason languages have added the override annotation so that unintentional overriding generates a warning.

When separating inheritance from type classification, one question is how many different language mechanisms are needed. Bertrand Meyer says that 10 would be needed and implies this is too many, but his list of types of inheritance is duplicative, so he overestimates it. Also, even 10 is not that many, e.g. C has 10 control structures - ternary operator, if, if-else, while, do-while, for, switch, break, continue, and goto. It probably is true that deciding between ``for`` and ``while`` wastes some time as Meyer says, but nobody has argued for removing ``for`` or ``while`` - although the constructs overlap, they are used in different situations and help to express the intent of the programmer, enhancing readability. Structured programming argues that one should have various loop constructs, even though goto can express any loop. Similarly, even if inheritance can express all the patterns of interest, it is still better to have separate syntax for each pattern of inheritance. Meyer says he has seen no compelling argument, but papers like "Inheritance is not subtyping" seem pretty compelling to me.

After separating inheritance from subtyping, what does it mean to have an abstract method in an interface, ``foo : T1``? Declaring ``{ foo : T1 } + { foo : ... }`` is meaningless (assuming we never use ``super.foo``) because only the type of ``foo`` on the right matters. So we see that the declaration is an assertion about the result of lookup, that ``lookup foo x : T1``. Then an interface of abstract methods is the intersection of these types, ``{ x | foo x : T1 } intersect { x | bar x : T2 } = { x | (foo x : T1) && (bar x : T2) }``.

Polymorphism
============

Polymorphism is a pretty confusing concept. ChatGPT's definition was (6/10) "the ability of objects to take on different forms, depending on the context in which they are used", which honestly makes little sense. The object's methods and properties do not change based on context. Rather, it is (per 2/10 ChatGPT definitions) that a procedure (context) is able to interchangeably handle objects with differing properties and methods.
Specifically, there are three forms of polymorphism. (Per ChatGPT there is no standard "fourth" type of polymorphism in the realm of object-oriented programming.) In order of most common to least in OOP, they are:

* subtype/interface polymorphism or method overriding. Per `old Wikipedia <https://en.wikipedia.org/w/index.php?diff=580604701>`__  and 2/10 ChatGPT definitions this is what "polymorphism" unqualified generally refers to, and per `C2 <https://wiki.c2.com/?PolymorphismEncapsulationInheritance>`__ is also the main sense intended for OOP. In this form, a procedure declares itself as taking a parameter typed as some parent class or interface, and then any subclass or instance of that interface may be passed to that procedure. Method overriding may be further classified into "implementation inheritance" or "standard method overriding", where the method being overridden has a concrete implementation in the superclass, and "interface inheritance" or "interface method implementation", where the method is abstract in the superclass. This form originated from Simula basically simultaneously with inheritance, for example in :cite:`dahlCommonBaseLanguage1970` page 25 they present an example of overriding a string hashing function with a subclass method that skips underscore characters.
* parametric polymorphism (Java generics/C++ templates). The procedure declares a type parameter and constraints and then may be used with any type satisfying those constraints. The same code is used for multiple types.
* ad-hoc polymorphism or method overloading. Multiple procedure implementations are declared with the same name but different types, and the appropriate procedure is called based on the arguments passed. It is not supported in Python; the most recent definition of a name overwrites any previous ones.

Minimal OO
----------

Uncle Bob `defines <https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html>`__ OO by distinguishing ``f o`` from ``o.f()``. With Uniform Function Call Syntax there is no difference. But, he argues, in an OO language ``o.f()`` is overloaded - it does dynamic dispatch based on the type of ``o``. Whereas with ``f o`` there is usually only one group of clauses for ``f``. Bob also wants to exclude implementations of dynamic dispatch that work by modifying ``f`` to use switch statements or long if/else chains. So he excludes dynamic dispatch that creates a source code dependency from ``f o`` to ``f``, i.e. ``f o`` "knows" ``f``. Instead there must be several clauses for ``f`` which may be called. Concretely, Bob says, one should be able to write ``f o`` in source file A and an implementation of ``f`` in source file B and there should be no use/require/import declaration from A to B.

Stroscot has predicate dispatch and multimethods. So all functions can be overloaded and do dynamic dispatch. Stroscot solves the expression problem, so there is no boilerplate needed when extending ``f``. Furthermore, Stroscot uses a recursive knot so definitions are properly in scope. So Stroscot's multimethods are enough to make it OO in Bob's minimalist sense.

Interfaces
----------

The "fragile base class" problem is that a subclass may break if its parent changes its self-use of methods, even though the subclass's code has not been touched. Every downcall generates a code coupling that must be documented and maintained. For example in :cite:`ArtimaJavaDesign`, there is mentioned the situation where a List class has add and addAll methods and one wants to write a CountingList class that overrides add and addAll to count the total number of elements added. This cannot be done properly without knowing whether List.addAll does a downcall to List.add.

One solution is to remove downcalls: if ``self`` is not an argument to the constructor passed to ``mkObject``, and only ``thislvl`` is used, then there is no encapsulation issue. Unfortunately :cite:`temperoWhatProgrammersInheritance2013` measured that 0-86% (median 34%) of inheritance relationships have a downcall. Although some projects are at 0, suggesting it is possible to avoid downcalls, the prevalance of this practice suggests they cannot be removed categorically and some form of downcalling must be supported. But it certainly makes senser to give downcalls more verbose syntax rather than the simple syntax ``this.method()``.

A less restrictive solution is to ensure that for every downcall, the downcalled method is abstract in the current class. That way the code coupling is self-documenting - the developer can just check the class definition to see what is a downcall. So what is prohibited is "implementation inheritance", i.e. the situation where there is a concrete method ``A.a`` being overridden by a concrete method ``B.a`` and a method in A calls ``A.a``. Rust, Julia, Go, and Swift have all adopted this style of programming, under various names such as trait, interface, or prototype-oriented programming. Even Java 8 added default methods and static methods to interfaces. There is an associated nomenclature change. Now a concrete method is referred to (in Swift) as "a default implementation of a required method", and the most-derived class is given the normal name "class" while the other classes in the chain are given a more unusual name like interfaces, mixins, or traits, and are not allowed to be directly instantiated. This terminology makes the special role of the final class in the inheritance chain clear. And multiple inheritance works better - conflicting definitions can simply error if there is no most-specific definition, and the developer can solve such issues by defining the method in the most-derived class.

But actually, these languages have not "solved" the issue at all in a technical sense. Although most of the time the method is left abstract and implemented in the most-derived class, so the fragile base class problem is avoided, implementation inheritance is still there: you can override an interface and replace one of its default methods. The difference is rather a culture change: overridable methods in interfaces are all marked as default, so it is clear that they are expected to be overridden. And in Swift, non-overridable (final) methods are the default, so you have to jump through several hoops to actually implement the "bad" implementation inheritance pattern.

Multimethods
------------

Interfaces etc. are a morass of complexity. Generally these declare one, two, three, four functions or more. But it's not particularly clear how to structure that: How many interfaces do you have? Do you have one interface per function, one interface with all the functions, or something in between? There's no clear guidance. And it's an important decision because you can't remove a method from an interface later on without breaking lots of code. The safest decision is one function per interface, and never more or less, because that way you'll never need to remove a function from an interface, and a zero-function marker interface is trivial and mostly useless.

Obviously though this will require a lot more interfaces. There is already a naming problem where you don't know which interface a method is coming from, and this will make it worse. Who is going to remember that ``summarize`` comes from the ``Summary`` interface rather than ``Summarizer``, or that ``next`` comes from ``Iterator``?  The solution is to once again introduce some order into the chaos, this time by mandating a uniform naming scheme based on the method name. For example we could call each interface ``<method_name>_interface``. It's not going to win any writing awards, but it works.

At this point though developers will start complaining about how tedious it is. We've taken all the fun out of using interfaces, and it is just tedious boilerplate now:

::

  interface lookup_protocol
    lookup : T1

  class A implements lookup_protocol
    lookup : T1
    lookup = ...

Fortunately in Stroscot we don't need this boilerplate, we can just use multimethods:

::

  lookup_protocol T = { lookup : T -> T1 }

  lookup (self : A) = ...

  assert (lookup_protocol A)

tl;dr interfaces are just a verbose chaotic version of multimethods. As a corollary of this, Stroscot has no methods defined "inside" a type - you write ``type = ...; method = ...`` rather than ``type = { ...; method ; ... }``. They are all "free functions" or "extension methods".

We can also implement virtual methods via Stroscot's multimethods, assigning everything the same priority to use the specificity mechanism to implement overriding, and using module definition recursion to do the knot tying. This separates data from behavior which is a more functional style. Careful use of single arguments and lambdas allow mimicking single dispatch, matching Smalltalk's virtual method semantics. Smalltalk also allows accessing the parent method like ``super.method1``; in Stroscot this rather is done with ``next_method`` when in ``C.method1``. Outside ``C.method1``, we can call ``P.method1`` with something like ``(lookup_clause method1 (self : C or_subclass)).next_method``; we have to use this convoluted mechanism if we want to mimic calling ``super.method2``. So similar to Ecstasy, we have a restricted ``super`` call for the most part. Similarly doing ``(lookup_clause method1 (self : C or_subclass))`` without the ``next_method``, we can access ``thislvl``. Again it is more convoluted than a keyword, although a macro could fix this.

If we code the arguments naturally using multiple dispatch then of course we get multiple dispatch. E.g. I implemented equality on ColorPoints and Points, the only non-degenerate one per `Artima <https://www.artima.com/articles/how-to-write-an-equality-method-in-java>`__. I would argue that the multimethods are a clear win here over Artima's implementation as we can just write the clauses - the ``instanceof`` is implicit in the specificity matching, and there is no separate ``canEqual`` method. And if we removed ``or_subclass`` then we would not be overriding at all and the ``false`` clauses and priority equalization would not be needed - Points and ColorPoints would simply be treated as disjoint types and comparison between them would not be defined.

The biggest issue with multimethods is per :cite:`taivalsaariNotionInheritance1996` pg. 473 they "do not feel object-oriented". Because the operations are not logically "contained" in the object, but rather live in a separate "method dispatch" namespace, e.g. using the traditional function syntax ``f a b`` rather than the infix ``a.f(b)``, there is no clear boundary for the internal vs. external methods of an object. We can define such a boundary using module encapsulation, but it is not as tidy as the methods-fields package offered by traditional OO. It is not clear that such a boundary is useful, though.

Multiple implementations
------------------------

In a lot of languages there's a restriction that interfaces can be implemented only once for a given type. This is Stroscot's restriction too: because of how overloading works, a function can be implemented only once in a module. There are ways to work around this. Java has the adapter pattern, and similarly Idris allows `named implementations <https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#named-implementations>`__. In Stroscot, we can just write ``a { method1 = ..., method2 = ... }`` and override the methods using implicit parameters.

Now with multiple implementations floating around we often want to use these as a value. There's no issue with this in Stroscot. For example, sets and maps need a comparison operator, and this has to be consistent so that you don't insert with comparison A and removing with comparison B. To avoid inconsistent comparisons the map or set can store the comparison operator as a parameter on creation - it is simply a function after all.

Design patterns
===============

Design patterns are not really OO, but :cite:`gammaDesignPatternsElements1994` is subtitled "reusable object-oriented software", and the naming patterns of extremely long Java class names like ``AbstractVisitorManagerFactoryProvider`` originated from that book, so there's not really a better place to discuss it. Essentially, the "Gang of Four" described 23 patterns of structuring objects. Since then, the patterns have held up pretty well - only a few changes per :cite:`obrienDesignPatterns15` (which I have added). I have also included other patterns from Wikipedia's "Software design patterns" infobox (`rev <https://en.wikipedia.org/w/index.php?title=Template:Design_patterns&oldid=1032062304>`__).

As many have observed, e.g. as cited on `Wikipedia <https://en.wikipedia.org/wiki/Software_design_pattern#Criticism>`__ and `C2 <https://wiki.c2.com/?DesignPatternsAreMissingLanguageFeatures>`__, design patterns are not examples of great program design, but rather desirable language features. To use :cite:`norvigDesignPatternsDynamic1996`'s words, in an expressive language, the patterns should be "invisible", so much a part of the language that you don’t notice using them. The point of examining these patterns is to ensure that Stroscot can easily express each pattern's intent with minimal boilerplate.

* Abstract factory - In Stroscot, if there is a need for a cross-platform or pluggable interface, then overloading can be used to seamlessly combine multiple implementations into one interface. Each implementation can guard that a configuration option is a specific value. This option can be specified as an implicit parameter, rather than as an option on a singleton. As in the book's Smalltalk example, the creation methods themselves can be redefined using implicit parameters to use specialized behavior for a specific type of object creation.
* Active object
* Active record
* Actor
* Adapter - this is just writing a module that imports another module and wraps its functionality.
* ADR
* Applicative
* Balking
* Barrier
* Binding properties
* Blackboard
* Bridge - this is done by passing in a parameter to the module or function, and overloading on its value.
* Broker
* Builder - in Stroscot, macros make it easy to parse an AST and return a value. The AST does not need to be valid Stroscot code at all (the typical verbose ``buildX``, ``buildY`` chain of statements), giving the flexibility of using a DSL such as JSON or XML to specify the data in a more compact manner. Furthermore, it is easy to define an intermediate representation and convert the data to that, rather than directly returning an object, allowing multiple forms of "building" such as the size counting example.
* Business delegate
* CBD
* Chain of responsibility
* Circuit Breaker
* Client–server
* Closure
* Command
* Comonad
* Compensating Transaction
* Composite - this is represented using the tree structure of symbols, or as a graph of references. Since Stroscot is unityped there is no need for inheritance.
* Composite entity
* Compute kernel
* Coroutine
* CQRS
* Currying
* Data access object (DAO)
* Data transfer object (DTO)
* DDD
* Decorator - In a unityped language, this is just defining a wrapper around another value. Hard to distinguish from the adapter or facade patterns.
* Delegation - this is overloading each method to also work on the wrapper. Maybe can be automated with a macro.
* Dependency injection - from PLOP3 per :cite:`obrienDesignPatterns15`. Constructor injection is simply including a field. Stroscot allows a simple form of setter injection by validating the fields of an object before classifying it as a member of a type. One can also write an explicit setter method, and properly express the type as ``PartiallyInitializedObject -> FullyInitializedObject``. Interface injection can be expressed by defining a type that is a broader set of objects than one specific class, but it is just type hackery and doesn't really affect the semantics. DI frameworks that create objects from textual specifications can be expressed as macros.
* Double-checked locking - The goal of this is lazy initialization, it is really the implementation of that pattern.
* ECB
* ECS
* EDA
* Event-based asynchronous
* Extension Object - from PLOP3 per :cite:`obrienDesignPatterns15`
* Facade - this is writing a record with multiple fields and an operation on those fields. not particularly complex.
* Factory - renamed from "factory method" per :cite:`obrienDesignPatterns15`. As discussed in the section "No constructors" above, in Stroscot, every "constructor" is simply an ordinary unrestricted function and has the power of a factory method to return multiple types of concrete objects and hide these behind an abstract type signature.
* Fiber
* Filters
* Flyweight - this is just using a shared reference, and I think Stroscot will hash cons shared immutable data automatically or at least use optimal reduction to avoid duplicating data too much
* Free monad
* Front controller - This is using a handler function or three and an overloaded controller function. Seems straightforward.
* Function composition
* Functor
* Futex
* Futures and promises
* Generator
* Guarded suspension
* HOF
* Identity map
* Immutable object
* Implicit invocation
* Index Table
* Intercepting filter
* Interceptor
* Interpreter
* Inversion of control
* Iterator
* Join
* Lazy initialization - memoization of computing the value of a variable. Computations without side effects are automatically delayed to their point of use and evaluated exactly once if it will improve performance, but I guess Stroscot should have a ``compute_once`` function for imperative actions that works through the combination of lazy evaluation and unsafePerformIO. It should use a race-free version of double-checked locking.
* Lazy loading
* Leader Election
* Leaders/followers
* Lock
* Mangler
* MapReduce
* Marker interface - in Stroscot we just use a set, like ``set Market default empty; FooClass subset Marker``
* Materialized View
* Mediator
* Memento
* Messaging
* Method chaining
* Microservices
* Mock object
* Model 2
* Module - Stroscot directly supports modules
* MOM
* Monad
* Monitor
* Monoid
* Monolithic
* Multitier (n-tier)
* Multiton - per :cite:`odochertyObjectorientedAnalysisDesign2005` pg. 341, a multiton is "any type with a restricted set of values". Taking this literally, this is simply a refinement type - for example we can make an enumeration of cases and get an ADT.
* MVA
* MVC
* MVC
* MVP
* MVVM
* Naked objects
* Nuclear
* Null object - from PLOP3 per :cite:`obrienDesignPatterns15`
* Object pool - this is a memory/resource management technique, combined with the factory method pattern
* Observer
* ORB
* P2P
* PAC
* Pipes
* Proactor
* Prototype - objects are just data, so modifying an object always creates a new object, without the need for an explicit clone operation
* Proxy - just another name for writing wrappers, like the delegation pattern
* Publish–subscribe
* Publisher-Subscriber
* RAII - this is implemented in Stroscot with finalizers, which generalize RAII to heap usage
* Reactor
* Read write lock
* Resource acquisition is initialization
* REST
* SBA
* Scheduled-task pattern
* Scheduler
* Servant
* Service locator
* Sharding
* Singleton - this is essentially a global variable, and is now considered an antipattern per :cite:`obrienDesignPatterns15`. Stroscot instead has implicit variables, which are passed down from the program start to its site of usage.
* SN
* SOA
* Specification
* State
* STM
* Strategy
* Template method
* Thread pool
* Thread-local storage
* Throttling
* Twin - this seems like implementing multiple inheritance manually
* Type object - from PLOP3 per :cite:`obrienDesignPatterns15`
* Type tunnel
* Visitor
