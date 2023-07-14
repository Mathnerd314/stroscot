Objects
#######

Many people like to use the word "object", as in "object-oriented programming". Stroscot aims to support all the paradigms, so being able to claim that Stroscot is OOP would be a great feature. But our analysis is stymied before it begins: per the C2 wiki, `nobody agrees on what OO is <https://wiki.c2.com/?NobodyAgreesOnWhatOoIs>`__ and there are `many definitions for OO <https://wiki.c2.com/?DefinitionsForOo>`__. For that reason the word "object" is not used in the rest of the documentation.

Definition
==========

Fortunately, it is 2023, so we can use ChatGPT to cut the Gordian knot. It has analyzed many discussions of OOP and distilled the common themes. ChatGPT does not actually give a consistent definition either, but we can distill the common parts of 10 ChatGPT definitions:

* OOP uses objects to represent and manipulate data. An object contains properties and methods, which group related data and behavior.
* The core principles of OOP are encapsulation, inheritance, and polymorphism.

There are some other statements included in some definitions but not others. Since we want only what is agreed on, it is clear that these should be ignored, but I will also justify why they can be ignored:

* Objects are instances of classes (8/10) - prototypes instead of classes is a common twist on OO, e.g. as done in Self, and :cite:`borningClassesPrototypesObjectoriented1986` argues that prototype-based inheritance is simpler than class-based.
* It is widely used / popular in software development (6/10) - this doesn't really matter in an argument on whether Stroscot is OO.
* Abstraction is a fourth core principle (4/10) - arguably this is included in the "use objects to represent and manipulate data" bullet point.

Getting back to the definition, the second part is just the `PolymorphismEncapsulationInheritance <https://wiki.c2.com/?PolymorphismEncapsulationInheritance>`__ definition found on C2. We thus see that this is the "correct" definition. Notably, this definition is one of the few definitions on C2 not associated with a particular pundit, and it is listed second, pretty close to the top. It is similar to `Rees's <http://paulgraham.com/reesoo.html>`__  "conventional Simula 67-like pattern" of encapsulation, ad hoc polymorphism, inheritance = subtyping, and sum-of-product-of-function pattern, which he states "many people take as a definition of OO". C2 does list caveats:

* It is C++ centric - ChatGPT vehemently disagrees with this, and says the principles also apply to Java, Python, and Ruby.
* The words "encapsulation", "inheritance", "polymorphism" only express so much, and are ambiguous out of context. There is a large amount of convention and code constructs associated with expressing these principles. - Each ChatGPT definition devotes a sentence or two to each of these words expressing the intended meaning, so doing the same common theme distillation, there is much less ambiguity. And we can always follow up by asking ChatGPT what it means by these various words. In 4/10 definitions it thought encapsulation referred to the idea of bundling data and methods into a single entity. But it should be clear that this is the basic definition of "object", rather than a principle of OOP. In 8/10 definitions, it said more reasonably that (2 definitions had both bundling and hiding) This hiding ensures that the object's internal data can only be accessed through a public interface provided by the object, which can prevent unintended changes to an object's state, and makes it easier to maintain and modify code over time.

Objects
=======

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

Stroscot does not subscribe to Smalltalk's notion that "everything is an object". Rather, in Stroscot, "everything is a value", and booleans, integers, and objects (modules) are different types of values.  This is a little similar to Java's notion of "primitive types", but unlike Java, Stroscot does not make objects reference types - objects have value semantics, and are part of the universal type. Stroscot's notion of object is more similar to the notion of "pure object" in :cite:`cookUnderstandingDataAbstraction2009`, where an object is an immutable record of functions that themselves take and return immutable objects and primitive values. To express the Java notion of mutable object, you would use Stroscot's mutable variables, in particular a Java object could be represented as a variable containing an object whose methods/functions took mutable variable references pointing to more objects.

Java and C# have implemented a feature called "autoboxing" where primitive types are automatically converted to "wrapped primitive" objects like Boolean or Integer. This allows using primitives in places where an object is required. But this is a leaky abstraction; autoboxing actually changes behavior. For example in Java ``new Integer(0) != new Integer(0)``, you have to do ``Integer.valueOf(0) == Integer.valueOf(0)`` or use ``.equals``. In JS ``false`` is falsy but ``new Boolean(false)`` is truthy. Properly speaking, primitives are distinct from objects, in that they do not support key object operations such as looking up identifiers. Ultimately, discarding the "wrapping primitive" notion and simply representing primitive values directly is the most logical. Supporting a universal type that can contain both primitives and objects does complicate code generation, but it's not that bad.

No object identity
------------------

In Java, objects have an identity. For example, ``new Object() == new Object()`` returns false because two different object references are constructed and their addresses are different. But when you try to do ``new Object() <  new Object()`` it's disallowed because the address is an implementation detail. So just for consistency's sake ``==`` shouldn't work either - if the address is an implementation detail, then don't expose anything about it! But apparently the desire for a fast pointer equality hack won out over the desire to avoid exposing implementation details of the language. As :cite:`cookUnderstandingDataAbstraction2009` says, "primitive equality exposes representation and prevents simulation of one object by another."

In Stroscot, objects are values, so they don't have identity. Syntactically identical objects will always compare equal. However, mutable variables have identity - their value is the address, and allocating a new mutable variable creates a new address hence a new identity.. So in the emulation of Java objects as mutable variables containing objects, we can compare ref-to-object for address equality, and also dereference the refs and compare the object values. Hence the Java notion naturally decomposes into the combination of two concepts. Immutable objects work much better with the functional programming paradigm. Furthermore JSON cannot easily represent the notion of object identity, whereas object values are easily written.

No implicit synchronization lock
--------------------------------

Another (mis)feature of Java is the ability to write ``synchronize (random_object)`` and use any object as a lock. This adds some bytes of header to every object allocation. It has been acknowledged by `the Java tutorial <https://docs.oracle.com/javase/tutorial/essential/concurrency/locksync.html>`__ that the ability to use any object was overly broad and it is better to use specific lock objects. `This post <https://shipilev.net/blog/2016/close-encounters-of-jmm-kind/#_horror_circus_kinda_works_but_horrifying>`__ points out that you can synchronize on primitives (actually the corresponding autoboxed objects) and on strings. Although it works for small examples due to interning caches, it will fail if your program uses too many primitives/strings as locks. This sort of tomfoolery seems pretty stupid, and there is an easy fix: if you want a mutex you should have to create a value of the ``Mutex`` type.

More ChatGPT definitions
========================

Polymorphism refers to the ability of objects to take on different forms, depending on the context in which they are used.
Polymorphism allows different objects to be used interchangeably, even if they have different implementations or behaviors, which can make code more flexible and extensible.
Polymorphism - allowing objects to take on different forms or behaviors depending on the context in which they are used.
Polymorphism allows objects to take on multiple forms, depending on their context.
polymorphism, which allows objects to be treated as instances of their parent classes, or as instances of their own specific class.
Polymorphism allows objects of different classes to be treated as if they were of the same class, by providing a common interface that can be used to interact with them.
Polymorphism allows objects of different classes to be used interchangeably, as long as they share a common interface.
polymorphism, which refers to the ability of objects to take on different forms or behaviors depending on the context in which they are used.
Polymorphism: the ability of an object to take on different forms, depending on the context in which it is used.
polymorphism allows objects to take on multiple forms and behave in different ways depending on the context in which they are used.

Abstraction - representing complex systems or processes through simpler, more generalized concepts.
Abstraction is the practice of defining interfaces without specifying implementation details, allowing for flexibility in how the interface is implemented.
Abstraction: the ability to represent complex real-world objects in simplified form, by focusing on the essential features and ignoring the non-essential details.
abstraction involves defining simplified interfaces for objects that hide their implementation details.


Encapsulation
=============

According to 8/10 ChatGPT definitions, encapsulation refers to hiding the internal workings of an object from the outside world. The object's data can only be accessed or modified through a well-defined interface. Typically this interface is defined by marking certain methods and fields as "public", meaning they can be accessed by the outside world, while others are marked "private", meaning they can only be accessed by the object itself.

:cite:`snyderEncapsulationInheritanceObjectoriented1986` says this controlled interface serves as a contract between the object and its clients. So long as this contract is maintained, the object can be reimplemented without affecting any clients. To maximize the advantages of encapsulation, the contract should avoid including implementation details. For example, it should be possible to rename instance variables without affecting clients. Encapsulation provides a way to protect an object's integrity, ensuring a consistent and valid internal state. Encapsulation promotes modular software evolution and maintenance.

:cite:`cookUnderstandingDataAbstraction2009` argues that encapsulation is really a property of
ML modules, because only ML modules provide sophisticated sharing mechanisms that allow multiple implementations and uses of multiple abstractions to coexist. ML of course allow the basic hiding feature, by not exporting the representation of a type. Unlike Smalltalk, which disallows ``this.foo == b.foo`` even if ``b`` is an instance of the current class, ML modules allow inspecting the representation of more than one value at the same time. ML modules also allow defining multiple abstract data types in the same module, so that a complex internal representation may be defined and manipulated without recourse to C++'s "friend" qualifier. The only feature missing from ML modules is mixing values of two different implementations - this is solved in Stroscot by allowing functions to use duck typing, so that two values may be mixed if they both conform to the proper interface. Since Stroscot defines objects to be ML modules, all the benefits of encapsulation are provided.

Cook goes on to state that "any programming model that allows inspection of the representation of more than one abstraction at a time is not object-oriented." So by his definition C++ and Java are not object-oriented - bleh. In fact this is just a limitation of ML - ML cannot inspect/pattern match on functions; they are opaque. In Stroscot, it is possible to match on the lambdas in Cook's Figure 8 and determine if an ISet was constructed via the Empty, Insert, or Union implementations. We might as well have written ``data ISet = Empty | Insert int ISet | Union ISet ISet`` as in the ADT implementation, except that the lambda presentation is an open data type that allows adding more cases. In Stroscot, we use multimethods to solve the expression problem, so it is just defining symbols and adding more dispatch cases to the relevant multimethods.

::

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


All-or-nothing field access
---------------------------

Inheritance should respect encapsulation, so that inheriting classes do not get any more access. Thus we see that Java's protected keyword is a hack. Allowing non-public instance variables to be accessed by subclasses breaks encapsulation. Instance variables should either be public and accessible to everyone, or private and not accessible to anything outside a module.

In Stroscot, if you can access the term's constructor symbol, you have full data access to all fields and can destruct and create values with that constructor. But, you can avoid exporting a constructor symbol from a module - that means a user will have to use the defined factory functions and accessors, or else deliberately import the ``._internal`` module.

Inheritance
===========

Inheritance allows objects to inherit properties and methods from a parent class, making it possible to create specialized subclasses with additional functionality.
Inheritance allows objects to inherit properties and behaviors from other objects, which can be useful for creating related objects with similar functionality.
Inheritance - allowing classes to inherit properties and methods from other classes, forming a hierarchy of related objects.
inheritance allows classes to inherit properties and methods from other classes.
inheritance, which allows classes to inherit properties and methods from other classes
Inheritance allows new objects to be based on existing objects, inheriting their attributes and behaviors.
Inheritance is the ability of a class to inherit properties and behaviors from a parent class.
Classes can inherit properties and behaviors from parent classes, allowing for code reuse and the creation of hierarchies of objects. Inheritance allows classes to inherit properties and behaviors from parent classes
Inheritance: the ability of one class to inherit properties and methods from another class.
Inheritance allows classes to derive attributes and behaviors from parent classes

Inheritance originated from Simula where per :cite:`nygaardDevelopmentSIMULALanguages1978` they were trying to model a toll booth on a bridge, with a queue of cars which were either trucks or buses. The queue was modeled with a "circular list" structure, consisting of a "set head" and a variable number of "links", each with a predecessor and successor reference. The trucks and buses are modeled as collections of static properties according to a schema. Inheritance thus appeared as a "concatenation" or "prefixing" mechanism for "gluing" each of the various vehicles (trucks, buses) together with a "link" for an intrusive list to make one record instance. As `this post <https://catern.com/inheritance.html>`__ argues, inheritance was invented as a performance hack.

We can implement the basic "concatenation" pattern in Stroscot as a library, `here <https://github.com/Mathnerd314/stroscot/blob/master/library/inheritance.txt>`__. It doesn't seem to require any weird tricks besides polymorphism, so it could easily be in the standard library somewhere. I chose ``+`` to represent the inheritance operator that plays a key role. Per Simula lookup is asymmetric (right-biased); for example ``Record { a : integer } + Record { a : real }`` gives something like ``Record { shadowed_a : integer, a : real }``, renaming the field on the left when it collides with the right. So we have a lookup operator for that. We also have a more specialized "qualified lookup" operator for accessing shadowed prefix attributes. There are other choices for how to deal with duplicate field names, e.g. removing the superclass field. But shadowing allows us to always lift operations on ``A`` or ``B`` to ``A+B``. We will want to use these lifting operations to be able to apply ``in`` on ``truck_link``, for example.

:cite:`taivalsaariNotionInheritance1996` also mentions "defeating" or "cancelling" a property. This involves the subclass containing a "whiteout" entry so that looking up that property returns a not found exception rather than a value. It really is an extension of the lookup algorithm and doesn't affect much of the design.


Multiple inheritance
--------------------

Multiple inheritance makes things more complicated. There is diamond inheritance: if D extends B and C, and B and C each extend A, then are there two copies of the fields of A, or only one copy? Most languages with MI pick one or the other, but C++ seems to be unique in allowing each superclass to be declared either non-virtual or virtual. Each virtual superclass appears only once in the composite object, while non-virtual superclasses appear once for each non-virtual path. This means that the hierarchy is no longer simple aggregation but also has a dictionary mapping from virtual class names to virtual class pieces.

There is no convenient "subclass takes precedence" rule when properties conflict between superclasses, so we must accept that such cases are ambiguous errors, and rely more often on specialized lookup syntaxes that makes clear which part of the composite object we are referring to. For example C++ uses syntax like ``p->A::next`` or ``((A) p)->next``, as well as class casts, to allow access to ambiguous properties.

Non-virtual inheritance makes specifying properties even more complicated because we have to specify the full path of the superclass and not just its name. C++ forbids direct duplication of base classes like ``A extends (B, B)``, so can always specify a path unambiguously using class names - basically, instead of directly duplicating ``B``, you do ``B1 extends B, B2 extends B, A extends (B1, B2)``, and then ``B1,B2`` are unique and can specify a path. For now I'll ignore this detail and specify paths by number.

Patterns of inheritance
-----------------------

Having defined inheritance, and implemented it as a library, we are still not finished with inheritance in Stroscot. We must also examine the common patterns of inheritance to see if there are any other libraries hiding in the dark. If we had a definitive study showing "Inheritance makes systems more complex and bug-ridden and thus unmaintainable" then we could stop on the grounds of inheritance being a footgun. Certainly there are suggestive blog posts like "Inheritance is evil and must be destroyed" (`1 <https://blog.berniesumption.com/software/inheritance-is-evil-and-must-be-destroyed/index.html>`__). But :cite:`elemamConfoundingEffectClass2001` lists several cases where a promising study found that inheritance caused difficulties, but follow up studies/replications have found the opposite conclusion. And their subsequent regression model invalidates all of the tested inheritance metrics (DIT, NOC, NMO, NMA, SIX) as having no statistical relationship after controlling for lines of code. It is still possible that inheritance may make programs more difficult to understand, but nobody has created a metric and done a study with enough statistical power to confirm or deny that conclusion definitively. TODO: Maybe Jan Vitek (co-author of some large scale Github studies) would be interested.

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

  These forms of inheritance seem suited for the inheritance library described before. Java-style inheritance actually seems less useful because it imposes a rigid class structure on reuse. And also, since these patterns are reuse, we could implement them without inheritance, by calling the method directly.

:cite:`temperoWhatProgrammersInheritance2013` also did some analysis on the remaining <0.1% of other relationships. The "constants class" was where a class or interface of only static final constants was inherited from to gain access to the constants. Meyers calls this "facility inheritance". This accounted for 1% of CC edges in some systems but most had no constant classes. A wildcard import seems a lot more straightforward. One system used a lot of "super" calls in the constructor, but for no discernible reason. Probably not worth emulating. Some "framework" relationships used third party types and could not be analyzed fully due to lack of source code. "Generic" containers cast to Object and back so impeded subtyping analysis - again Stroscot's type system seems sufficient here. And still other inheritance relationships were just there and no amount of inspection by the authors could discern a useful purpose.

Inheritance is not subtyping
----------------------------

This is the title of :cite:`cookInheritanceNotSubtyping1989`, and their argument seems correct. The existence of ``Lens (a+b) (a'+b) a a'`` does not imply a subtype relation - ``A`` is not a subtype or supertype of ``A+B``. Rather ``A`` is related to ``A+B`` by a separate "is subcomponent of" relation, as formalized in the Lens type. For example, in :cite:`cookInheritanceNotSubtyping1989` section 3.2 page 129 we have a parent constructor ``P self super thislvl = { i = 5, id = self, eq = \o -> self.i == o.i }`` and a child constructor ``C self _ _ = { b = true, eq  = \o -> o.i == self.i && o.b == self.b }``. We can work out some types: ``mkObject [P] : mu self. { i : int, id : self, eq : {i : int}_open -> bool }`` and ``mkObject [C,P] : mu self. { i : int, id : self, b : bool, eq : {i : int, b : bool }_open -> bool }``. The second has more fields than the first, so with closed records they are unrelated types. We might think (as Meyers does) that with open record types we could say that the second (child) type is a subtype of the first. But looking at ``eq``, since ``{i : int, b : bool }_open`` is a subtype of ``{i : int }_open``, by contravariance the first ``eq`` type is actually a subtype of the second. So even relaxing our record subtyping definition these are unrelated types.

More generally, all combinations of subtyping and inheritance are possible:

* S is neither a subtype nor a child type of T - independent types, Boolean and Float
* S is a subtype but is not a child type of T - Int32 and Int64, subset but unrelated by inheritance
* S is not a subtype but is a child type of T - S child of T, S -> S is not a subtype of T -> T
* S is both a subtype and a child type of  - when all inherited fields and methods of the derived type have types which are subtypes of the corresponding fields and methods from the inherited type, and the type is an "open record"

Note that subtype + derived type is only possible with open records - with closed records no derived type is a proper subtype. :cite:`abdelgawadNOOPDomainTheoreticModel2018` formalizes this notion and shows that in Java and other nominally-typed OOP languages, "inheritance is subtyping". More specifically, "a class B is a subtype
of a class A iff B inherits from A." But this property is obtained by placing restrictions on inheritance - in Java, a method only overrides its parent method if its type matches the parent method, and methods cannot be removed. :cite:`taivalsaariNotionInheritance1996` calls this "strict inheritance". Strict inheritance is a pretty weird semantics from a unityped perspective - for example in Smalltalk we can override a field and change its value from an int to a string. So this "inheritance is subtyping" property is a form of type discipline, rather than a free property.

Inheritance-as-subtyping is easy to misuse and the Java platform libraries made numerous mistakes: Stack extends Vector, Properties extends Hashtable - in both cases, not using inheritance and thus avoiding the accompanying subtyping constraint would have been preferable. For example, with Properties (`1 <https://codeblog.jonskeet.uk/2006/03/04/inheritancetax/>`__), ``(Properties) p.getProperty(key)`` takes defaults into account, while ``p.get(key)`` which is inherited from Hashtable does not, and direct access to the underlying Hashtable allows adding non-String objects, when the designers intended that Properties should only be Strings. Once this invariant is violated, it is no longer possible to use other parts of the Properties API (load and store). Without inheritance-as-subtyping, ``get`` could have been overridden to be a subtype, and the other Hashtable methods deleted.

Inheritance as subtyping breaks encapsulation, because superclass methods that expect to receive themselves may receive a subclass instance that doesn't support an expected contract. In particular, a call to self.b in A.a may resolve to an inherited implementation B.b, and this B.b may violate a contract that A.b satisfies. Even adding a method in the subclass can be unsafe, because the superclass can later add the same method and then you are unintentionally overriding it. For this reason languages have added the override annotation so that unintentional overriding generates a warning.

When separating inheritance from type classification, one question is how many different language mechanisms are needed. Meyers says that 10 would be needed and implies this is too many, but his list of types of inheritance is duplicative, so he overestimates it. Also, even 10 is not that many, e.g. C has 10 control structures - ternary operator, if, if-else, while, do-while, for, switch, break, continue, and goto. It probably is true that deciding between ``for`` and ``while`` wastes some time as Meyer says, but nobody has argued for removing ``for`` or ``while`` - although the constructs overlap, they are used in different situations and help to express the intent of the programmer, enhancing readability. Structured programming argues that one should have various loop constructs, even though goto can express any loop. Similarly, even if inheritance can express all the patterns of interest, it is still better to have separate syntax for each pattern of inheritance. Meyer says he has seen no compelling argument, but papers like "Inheritance is not subtyping" seem pretty compelling to me.

After separating inheritance from subtyping, what does it mean to have an abstract method in an interface, ``foo : T1``? Declaring ``{ foo : T1 } + { foo : ... }`` is meaningless (assuming we never use ``super.foo``) because only the type of ``foo`` on the right matters. So we see that the declaration is an assertion about the result of lookup, that ``lookup foo x : T1``. Then an interface of abstract methods is the intersection of these types, ``{ x | foo x : T1 } intersect { x | bar x : T2 } = { x | (foo x : T1) && (bar x : T2) }``.

Virtual methods
===============

The other part of Simula's subclass mechanism was the concept of virtual attributes. For example in :cite:`dahlCommonBaseLanguage1970` page 25 they present two hashing functions for strings: the base class does a standard hash, while the subclass skips underscore characters. The hash function is a "replaceable part" that allows access to subclass behavior from superclasses. This complicates the semantics of inheritance quite a bit, because now the superclass takes a reference to the subclass, and constructing the object requires tying up a recursive knot. Simula's semantics are somewhat restrictive so I instead chose to copy Nixpkgs's ``extends`` function, which overrides methods in a manner similar to Smalltalk's inheritance model. In my library there is a function ``mkObject`` to capture the usage pattern of ``extends`` in Nixpkgs. Each "constructor" function in the list passed to ``mkObject`` takes three arguments, ``self``, ``super``, and ``thislvl``, and returns an attribute set.

* ``self`` is the final resulting attribute set / object. It may refer to itself recursively but conceptually all of this recursion is unrolled. This allows late/virtual binding to an overridden method in the subclass.
* ``super`` is the unmodified attribute set returned from the parent constructor function. This allows statically binding to the parent.
* ``thislvl`` represents the return value of the current constructor. This allows statically binding to the current level. This argument is not present in Nixpkgs, but can be modeled by using ``rec { }`` for the attribute set.

With these three parameters we can choose for each self-call how it is bound, and arbitrarily mix binding levels. It is thus the most expressive. In Ecstasy in contrast, access is restricted so only the parent method can be called.

``mkObject`` implements what might be termed "value-level" inheritance. We don't have any types, just agglomerations of records. Specifying the type of ``mkObject`` is tricky. Since the subclass fields can vary, the type of the subclass reference ``self`` also varies, hence specifying the type of ``mkObject`` requires dependent types and heterogenous lists.

Multimethods
------------

We can also implement virtual methods via Stroscot's multimethods, assigning everything the same priority to use the specificity mechanism to implement overriding, and using module definition recursion to do the knot tying. This separates data from behavior which is a more functional style. Careful use of single arguments and lambdas allow mimicking single dispatch, matching Smalltalk's virtual method semantics. Smalltalk also allows accessing the parent method like ``super.method1``; in Stroscot this rather is done with ``next_method`` when in ``C.method1``. Outside ``C.method1``, we can call ``P.method1`` with something like ``(lookup_clause method1 (self : C or_subclass)).next_method``; we have to use this convoluted mechanism if we want to mimic calling ``super.method2``. So similar to Ecstasy, we have a restricted ``super`` call for the most part. Similarly doing ``(lookup_clause method1 (self : C or_subclass))`` without the ``next_method``, we can access ``thislvl``. Again it is more convoluted than a keyword, although a macro could fix this.

If we code the arguments naturally using multiple dispatch then of course we get multiple dispatch. E.g. I implemented equality on ColorPoints and Points, the only non-degenerate one per `Artima <https://www.artima.com/articles/how-to-write-an-equality-method-in-java>`__. I would argue that the multimethods are a clear win here over Artima's implementation as we can just write the clauses - the ``instanceof`` is implicit in the specificity matching, and there is no separate ``canEqual`` method. And if we removed ``or_subclass`` then we would not be overriding at all and the ``false`` clauses and priority equalization would not be needed - Points and ColorPoints would simply be treated as disjoint types and comparison between them would not be defined.

The biggest issue with multimethods is per :cite:`taivalsaariNotionInheritance1996` pg. 473 they "do not feel object-oriented". Because the operations are not logically "contained" in the object, but rather live in a separate "method dispatch" namespace, e.g. using the traditional function syntax ``f a b`` rather than the infix ``a.f(b)``, there is no clear boundary for the internal vs. external methods of an object. We can define such a boundary using module encapsulation, but it is not as tidy as the methods-fields package offered by traditional OO.

Minimal OO
----------

Uncle Bob `defines <https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html>`__ OO by distinguishing ``f o`` from ``o.f()``. With Uniform Function Call Syntax there is no difference. But, he argues, in an OO language ``o.f()`` is overloaded - it does dynamic dispatch based on the type of ``o``. Whereas with ``f o`` there is usually only one group of clauses for ``f``. Bob also wants to exclude implementations of dynamic dispatch that work by modifying ``f`` to use switch statements or long if/else chains. So he excludes dynamic dispatch that creates a source code dependency from ``f o`` to ``f``, i.e. ``f o`` "knows" ``f``. Instead there must be several clauses for ``f`` which may be called. Concretely, Bob says, one should be able to write ``f o`` in source file A and an implementation of ``f`` in source file B and there should be no use/require/import declaration from A to B.

Stroscot has predicate dispatch and multimethods. So all functions can be overloaded and do dynamic dispatch. Stroscot solves the expression problem, so there is no boilerplate needed when extending ``f``. Furthermore, Stroscot uses a recursive knot so definitions are properly in scope. So Stroscot's multimethods are enough to make it OO in Bob's minimalist sense.


BETA
----

Per `this <https://journal.stuffwithstuff.com/2012/12/19/the-impoliteness-of-overriding-methods/>`__, BETA inverts the dispatch order. It is the least derived class in the chain that is called first, that then can call ``inner()`` to dispatch to a subclass. We can implement this kind of inheritance using a prefix-biased lookup method and a similarly reversed ``extends`` method. :cite:`taivalsaariNotionInheritance1996` pg. 463 mentions that although BETA's method order looks completely different from the Smalltalk order, they can in fact simulate each other by systematically placing explicit calls to super/inner in the right places.

Fragile base classes (Interfaces)
---------------------------------

The "fragile base class" problem is that a subclass may break if its parent changes its self-use of methods, even though the subclass's code has not been touched. Every downcall generates a code coupling that must be documented and maintained. For example in :cite:`ArtimaJavaDesign`, there is mentioned the situation where a List class has add and addAll methods and one wants to write a CountingList class that overrides add and addAll to count the total number of elements added. This cannot be done properly without knowing whether List.addAll does a downcall to List.add.

One solution is to remove downcalls: if ``self`` is not an argument to the constructor passed to ``mkObject``, and only ``thislvl`` is used, then there is no encapsulation issue. Unfortunately :cite:`temperoWhatProgrammersInheritance2013` measured that 0-86% (median 34%) of inheritance relationships have a downcall. Although some projects are at 0, suggesting it is possible to avoid downcalls, the prevalance of this practice suggests they cannot be removed categorically and some form of downcalling must be supported.

A less restrictive solution is to ensure that for every downcall, the downcalled method is abstract in the current class. That way the code coupling is self-documenting. So what is prohibited is "implementation inheritance", i.e. the situation where there is a concrete method A.a being overridden by a concrete method B.a and a method in A calls A.a. Rust, Julia, Go, and Swift have all adopted this style of programming, under various names such as trait, interface, or prototype-oriented programming. Even Java 8 added default methods and static methods to interfaces. There is an associated nomenclature change. Now a concrete method is referred to as "a default implementation of a required method", and the most-derived class is given the normal name "class" while the other classes in the chain are given a more unusual name like interfaces or traits and are not allowed to be directly instantiated. This terminology makes the special role of the final class in the inheritance chain clear. And of course there is multiple inheritance, with a nice solution to the problem of multiple definitions, namely "error if there is no most-specific definition, which you can fix by defining it in the most-derived class".

But actually, these languages have not "solved" the issue at all in a technical sense. Although most of the time the method is left abstract and implemented in the most-derived class, so the fragile base class problem is avoided, implementation inheritance is still there: you can override an interface and replace one of its default methods. The difference is rather a culture change: overridable methods in interfaces are all marked as default, so it is clear that they are expected to be overridden. And in Swift, non-overridable (final) methods are the default, so you have to jump through several hoops to actually implement the "bad" implementation inheritance pattern.

Dissolving interfaces into multimethods
---------------------------------------

Interfaces, traits, etc. are a morass of complexity. Generally these declare one, two, three, four functions or more. But it's not particularly clear how to structure that: How many interfaces do you have? Do you have one interface per function, one interface with all the functions, or something in between? There's no clear guidance. And it's an important decision because you can't remove a method from an interface later on without breaking lots of code. The safest decision is zero or one functions per interface, and never more, because that way you'll never need to remove a function from an interface.

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

Multiple implementations
------------------------

In a lot of languages there's a restriction that interfaces can be implemented only once for a given type. This is Stroscot's restriction too: because of how overloading works, a function can be implemented only once in a module. There are ways to work around this. Java has the adapter pattern, and similarly Idris allows `named implementations <https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#named-implementations>`__. In Stroscot, we can just write ``a { method1 = ..., method2 = ... }`` and override the methods using implicit parameters.

Now with multiple implementations floating around we often want to use these as a value. There's no issue with this in Stroscot. For example, sets and maps need a comparison operator, and this has to be consistent so that you don't insert with comparison A and removing with comparison B. To avoid inconsistent comparisons the map or set can store the comparison operator as a parameter on creation - it is simply a function after all.
