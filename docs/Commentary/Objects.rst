Objects
#######

Many people like to use the phrase "object-oriented programming". Per ChatGPT the minimal set of features necessary to be support OOP is:

1. Encapsulation: The ability to group data and behavior within an object and hide the implementation details from the outside world.
2. Inheritance: The ability to create a hierarchy of classes, where subclasses can inherit attributes and behaviors from their superclasses.
3. Polymorphism or dynamic dispatch: The ability to define methods in a base class that can be overridden by derived classes.

As far as I can tell this definition is not standard - OOP is a broad term that can refer to many concepts, principles, techniques, patterns, and philosophies used in software development. So for that reason the phrase is not used in the rest of the documentation. But this definition or similar shows up in several places when you google it and I don't particularly disagree with it.

Encapsulation
=============

Per :cite:`snyderEncapsulationInheritanceObjectoriented1986`. a module is encapsulated if clients are restricted to access the module only via its defined external interface. The external interface of a module serves as a contract between the module and its clients. So long as this contract is maintained the module can be reimplemented without affecting any clients. Thus, software evolution and maintenance is easier. To maximize the advantages of encapsulation, the contract should avoid including implementation details. For example, it should be possible to rename instance variables without affecting clients.

Stroscot has modules, which provide encapsulation. We can create a type and not export its constructor symbols, so that the type becomes an abstract data type: only functions defined in the module can access the concrete representation of the type, and functions outside the module can only use the public interface. This indeed allows renaming fields of the data type without affecting external clients.

Per ChatGPT, for modules themselves to be considered objects, we must be able to create and manipulate them at runtime. So: literal module syntax, get/replace/remove a definition, and evaluate an expression in the context of the module. Then we have all the object creation and manipulation features of Self.

No constructors
---------------

A constructor has many limitations compared to a factory function: it must allocate new memory, it cannot return a subclass, and it has to be called with a noisy "new" syntax and a fixed name.

For example, consider a boxed primitive boolean. It only needs two values: a factory function can construct one true and one false and then return those from then on. But using a constructor forces the program to produce millions of distinct trues and falses, creating significant overhead.

Another difference is that a factory function computes the field values first and then can use an allocate-and-initialize primitive. The primitive can ensure that its allocation is private, hence appears atomic for concurrency. In contrast a constructor allocates memory initialized to a default value and then overwrites each field. This implicit memory writing means that concurrency and constructors interact poorly because you can access partially-constructed objects.

Deserialization bypasses defined constructors and directly creates objects via the runtime - it is an implicit public constructor. In fact this deserialization constructor is exactly the allocate-and-initialize primitive that a factory function needs.

One use of constructors is to enforce invariants (validity checking); for example a time constructor that ensures ``0 <= minutes < 60``. In Stroscot, invariants like these are defined in types, and checked on use, rather than on construction. It is often very helpful to be able to talk about about an object whose fields are unnormalized, which the constructor pattern prevents. And when you need the invariants, the types establish object integrity. Whereas in Java you must reason about all mutating methods to identify the possible states of an object, in Stroscot only the type needs to be examined.

A minor downside of doing away with constructors is that factory functions are not automatically marked in the documentation, so can be harder to find. Organizing the source code and documentation to group factory methods is not hard, the hard part is enforcing that such a convention is followed consistently. But it's not even clear that grouping factory functions together is the best organization.

No autoboxing
-------------

Smalltalk had this idea that everything should be an object, including boolean and integer values. Java and C# have implemented this using "wrapped primitive" objects like Boolean or Integer. Autoboxing then converts between the primitive, which has no methods, and the wrapped primitive, which does. But this is a leaky abstraction; autoboxing actually changes behavior. For example in Java ``new Integer(0) != new Integer(0)``, you have to do ``Integer.valueOf(0) == Integer.valueOf(0)``. In JS ``false`` is of course false but ``new Boolean(false)`` is truthy. Also, wrapped primitives are distinct from normal OOP objects, in that when properly implemented they are immutable singletons constructed through a factory method, in other words "value types". Ultimately, discarding the "primitive" notion entirely and simply representing numbers as values is the most logical, although it does complicate code generation.

No object identity
------------------

In Java, ``new Object() == new Object()`` returns false because the two object references refer to different object instances that are stored in different locations in memory, i.e. their identities are different. It's pretty stupid IMO because they are syntactically identical: the only reason they are different is that allocation has side effects. When you try to do ``new Object() <  new Object()`` it's disallowed because identity is an implementation detail. But apparently fast pointer equality checks win over avoiding exposing implementation details of the language.

When we don't have object identity, it works much better with the functional programming paradigm. Furthermore JSON cannot even represent the notion of object identity.

No implicit synchronization lock
--------------------------------

If you want a mutex you have to create a value of the ``Mutex`` type, not just write ``synchronize (random_object)``.

Inheritance
===========

It would be convenient to just skip inheritance in Stroscot entirely, and Go, Rust, and Julia have done this. We can always rewrite a program to avoid language-supported inheritance by using the "delegation" or "composition" pattern: create a record with the "parent" as an instance field, and forward each non-overridden parent method and field via definitions of the form ``child.method = child.parent.method``. But this is manually implementing inheritance, and as described requires some boilerplate. Language-supported inheritance is more usable because you just write ``A extends B`` and all the forwarding happens automatically. There have been complaints about languages lacking inheritance, ranging from beginners asking "how do I do the Shape-Rectangle-Circle hierarchy in Rust" to experts saying "I coded this using inheritance in C++ and I see no way to define the data structures I need efficiently in Julia". So for example Nim has inheritance as an opt-in feature.

If we had a definitive study "Inheritance makes systems more complex and thus unmaintainable" then we could leave out inheritance on the grounds of it being a footgun. But studies have had mixed results:

inheritance had a negative effect on maintenance time and made it harder to modify systems
John Daly, Andrew Brooks, James Miller, Marc Roper, and Murray Wood. Evaluating in- heritance depth on the maintainability of object-oriented software. Empirical Software En- gineering, 1(2):109–132, January 1996.

inheritance had a positive effect on maintenance
Michelle Cartwright. An empirical view of inheritance. Information and Software Technol- ogy, 40:795–799, 1998.

size and functionality of a system affect understandability more than the “amount of inheritance” used.
 R. Harrison, S. Counsell, and R. Nithi. Experimental assessment of the effect of inheritance on the maintainability of object-oriented systems. Journal of Systems and Software, 52:173– 179, 2000.

So we need a more detailed analysis, of patterns of inheritance. :cite:`temperoWhatProgrammersInheritance2013` categorizes each class-class inheritance relationship S-T, (i.e., S directly inherits from T), as one of the following:

* subtype: an object of type S is supplied where an object of type T is expected, by assigning an object of type S to a variable declared to be type T, passing an actual parameter of type S to a formal parameter of type T, returning an object of type S when the formal return type is T, or casting an expression of type S to type T. 76% (range 11% - 100%) of class-class relationships had a subtype usage.

* reuse: a method not in S or T (external) or in S (internal) invokes a method m() or accesses a field f on an object constructed from type S, and m() or f is declared in T. 22% (4%-88%) of CC edges were external reuse and did not have a subtype usage, while 2% (0.5% - 30%) of CC edges had internal reuse but no subtype use or external reuse. These 24% of edges could be replaced with the delegation pattern.

* constants: T has only fields declared in it and the fields are constants (static final), and all outgoing edges
from T either have the constants attribute or are to java.lang.Object. This accounted for 1% of CC edges in some systems but most had no constant classes.

* framework or generic: T is a descendant of a third-party type, or there has been a cast from Object to T and there is an edge from S to some (non-Object) type T'. Excluding external/internal reuse and subtype, most systems had 0 framework or generic. The highest was 17% and 17 had 1%.

* Super: a constructor for S explicitly invokes a constructor in T via super. Most systems use no super calls but one used 38%.

* Other: No inferred purpose for the inheritance relationship. 57/93 systems were fully classified, and only 15 systems had more than 1% other. The JRE had 8% other. Manual inspection suggests these are framework or generic edges, or else subtypes intended for use by clients of the library.

Overall, almost all inheritance in Java can be classified as subtyping or reuse; other usages can be considered as project-specific design patterns.

Meyer describes 12 patterns of inheritance
Taivalsaari defines a taxonomy



 Inheriting to reuse code is a bad idea: the method could be called directly. Similarly modules provide the encapsulation benefits of classes without the rigid inheritance structure. Really inheritance should only be used to separate concerns. It allows combining data fields in a concise manner, and it allows using subtyping and dynamic binding to replace manual switch statement dispatch. But subtyping can be defined directly, and dynamic binding is better done with multimethods. So really the data fields seem to be the only useful pattern of inheritance.

Two toxic inheritance patterns are deep hierarchies and fragile base classes. These can lead to complex and hard-to-understand code.


Per :cite:`temperoWhatProgrammersInheritance2013`, in Java, an average 3 out of 4 types were defined using some form of inheritance.

 Fortunately, since Stroscot aims to cover all use cases, we don't really have to debate




Favoring composition over inheritance seems generally accepted. But


Inheritance lets you reuse code from a superclass. But with no classes you could just call the function directly.


In some cases, inheritance can also make it easier to optimize a program, because you can take advantage of polymorphism, which is the ability of a subclass to override or extend the behavior of its superclass. For example, if you have a superclass called Shape with a calculateArea method that uses a generic algorithm to calculate the area of any shape, you can create subclasses for specific types of shapes that override the calculateArea method with more efficient algorithms that are specific to their respective shapes. This can allow you to achieve better performance without having to make changes to the superclass.

changes to the superclass will automatically be propagated to all of its subclasses.  which can save time and reduce the risk of introducing errors. However, it is important to be mindful of the potential for unintended consequences when making changes to a superclass, as these changes can affect the behavior of all of its subclasses.




General guidelines are to use inheritance judiciously: limit hierarchies to 3-4 levels, not too deep. 10 levels is going to result in more work tracking down fields than it saves.

Concatenation
-------------

Inheritance originated from Simula where per :cite:`nygaardDevelopmentSIMULALanguages1978` they were trying to model a toll booth on a bridge, with a queue of cars which were either trucks or buses. The queue was modeled with a "circular list" structure, consisting of a "set head" and a variable number of "links", each with a predecessor and successor reference. The trucks and buses are modeled as collections of static properties according to a schema. Inheritance thus appeared as a "concatenation" or "prefixing" mechanism for "gluing" each of the various vehicles (trucks, buses) together with a "link" to make one record instance.

How would we do this in Stroscot? Let us modify the code from section 7 in :cite:`dahlClassSubclassDeclarations1967`:

::

  type linkage = Record { suc, pred : ref linkage }
  type link = linkage
  type list = linkage

  // remove a link
  out (l : link) | l.suc != None =
    l.pred.suc := l.suc
    l.suc.pred := l.pred
    l.suc := l.pred := None

  // add a link to the beginning of the list
  in (l : link) (L : list) =
    if l.suc != none
      out l
    l.suc := L
    l.pred := L.pred
    l.suc.pred := l.pred.suc := l

  // then your standard OO-style objects/records

  type vehicle = Record { license_number : integer, weight : real }
  type truck = vehicle + Record { load : ref list }
  type bus = vehicle + Record { capacity : integer }
  type bridge = Record { load : real }

  // then the goal - gluing these together

  type truck_link = link + truck
  type bus_link = link + bus
  type bridge_list = list + bridge

The operator ``+`` is the inheritance operator that plays a key role here. Lookup is asymmetric (right-biased); for example ``Record { a : integer } + Record { a : real }`` gives something like ``Record { shadowed_a : integer, a : real }``, renaming the field on the left when it collides with the right. We also have a more specialized "qualified lookup" operator for accessing shadowed prefix attributes. More formally we have code like the following:

::

  type A + B = Block { prefix : A, main : B }

  lookup x (Block {prefix,main})
    | x in main = lookup x main
    | otherwise = lookup x prefix

  qualified_lookup x ty (Block {prefix,main})
    | main : ty = lookup x main
    | otherwise = lookup x prefix

There are other choices for how to deal with duplicate field names, e.g. removing the superclass field. But this choice of representation allows us to always lift operations on ``A`` or ``B`` to ``A+B``. These are pretty useful operations, in fact they are just the standard tuple operations in Haskell. For example:

::

  flip (.) fst : (a -> x) -> (a+b) -> x
  flip (.) snd : (b -> x) -> (a+b) -> x
  (***) = \f g -> over _1 f . over _2 g : (a -> a') -> (b -> b') -> (a+b) -> (a'+b')
  _1 : Lens (a+b) (a'+b) a a'
    view _1 = fst : (a+b) -> a
    set _1 : a' -> (a+b) -> (a'+b)
    over _1 = first : (a -> a') -> (a+b) -> (a'+b)
  _2 : Lens (a+b) (a+b') b b'
    view _2 = snd :: (a+b) -> b
    set _2 :: b' -> (a+b) -> (a+b')
    over _2 = second : (b -> b') -> (a+b) -> (a+b')

Virtual methods
===============

The other part of Simula's subclass mechanism was the concept of virtual attributes. For example in :cite:`dahlCommonBaseLanguage1970` page 25 they present two hashing functions for strings: the base class does a standard hash, while the subclass skips underscore characters. The hash function is a "replaceable part" that allows access to subclass behavior from superclasses. This complicates the semantics quite a bit, because now the superclass takes a reference to the subclass, and constructing the object requires tying up a recursive knot. Simula's semantics are somewhat restrictive so I will instead look at Nixpkgs's ``extends`` function, which overrides methods in a manner similar to Smalltalk's inheritance model. I add a function ``mkObject`` to capture the pattern of usage of ``extends`` in Nixpkgs.

::

  extends : (self -> super -> thislvl -> thislvl) -> (self -> super) -> self -> (super + thislvl)
  extends f rattrs self =
    super = rattrs self
    thislvl = f self super thislvl
    return (glue super thislvl)

  catTy = foldl (+) {}

  mkList self [] = []
  mkList self (t:ts) = (self -> catTy ts -> t -> t) : mkList ts

  mkObject : forall (ts : [Type]). mkList (catTy ts) ts -> catTy ts
  mkObject xs = fix (foldr extends (const {}) xs)

Since the subclass fields can vary, the type of the subclass reference ``self`` also varies, hence specifying the type of ``mkObject`` requires dependent types and heterogenous lists. It might be easier to think about an example. If we take ``ts = [ht,gt,ft]`` then ``mkObject : [self -> (({} + ft) + gt) -> ht -> ht, self -> ({} + ft) -> gt -> gt, self -> {} -> ft -> ft] -> self where self = ((({} + ft) + gt) + ht)``. It is used like ``mkObject [subClassConst,middleClassConstr,superClassConstr]``. Each "constructor" function in the list takes three arguments, ``self``, ``super``, and ``thislvl``, and returns an attribute set. ``self`` is the final resulting attribute set / object. It may refer to itself recursively but conceptually all of this recursion is unrolled. ``super`` is the unmodified attribute set returned from the parent constructor function. Finally, if we imagine that ``f``'s returned attribute set is being wrapped in a ``rec { } `` we can make a third argument ``thislvl``, representing the return value of the current constructor (this argument is not present in nixpkgs). With these three parameters we can choose for each self-call whether it should be bound late/virtually and possibly have been overridden by the subclass (self), bound in the parent (super), or bound at the current level (thislvl). ``mkObject`` implements what might be termed "value-level" inheritance as opposed to the type-level inheritance we saw previously.

Multimethods
------------

We can also implement virtual methods via multimethods, assigning everything the same priority to use the specificity mechanism to implement overriding, and module definition recursion to do the knot tying. This separates data from behavior which is a more functional style.

::

  postfix or_subclass
  type T or_subclass = T | for_some S. (T+S) or_subclass

  type P = P { p1 : T_P1, p2 : T_P2, ... }

  prio obj {
    method1 (self : P or_subclass) = \x1 x2 -> ...
    method2 (self : P or_subclass) = \x1 -> ...
  }

  type C = P + C { c1 : T_C1, c2 : T_C2, ... }

  prio obj {
    method1 (self : C or_subclass) = \x1 x2 -> ...
    method2 (self : C or_subclass) = \x1 -> ...
  }

The careful use of single arguments and lambdas forces single dispatch, matching Smalltalk's virtual method semantics. Smalltalk also allows accessing the parent method like ``super.method1``; in Stroscot this rather is done with ``next_method`` when in ``C.method1``. Outside ``C.method1``, we can call ``P.method1`` with something like ``(lookup_clause method1 (self : C or_subclass)).next_method``; we have to use this convoluted mechanism if we want to mimic calling ``super.method2``. Ecstasy only allows calling ``super()`` which suggests that ``next_method`` is sufficient for most purposes. Similarly doing ``(lookup_clause method1 (self : C or_subclass))`` without the ``next_method``, we can access ``thislvl``. Again it is more convoluted than a keyword, although a macro could fix this.

If we code the arguments naturally then we get multiple dispatch, e.g. we could write:

::

  type Point = Point { x : float }

  prio obj
  equal (p1 : Point or_subclass) (p2 : Point or_subclass) = p1.x == p2.x

  type ColorPoint = Point + ColorPoint { color : Color }

  prio obj {
    equal (p1 : ColorPoint or_subclass) (p2 : ColorPoint or_subclass) = p1.x == p2.x && p1.color == p2.color
    equal (p1 : ColorPoint or_subclass) (p2 : Point or_subclass) = false
    equal (p1 : Point or_subclass) (p2 : ColorPoint or_subclass) = false
  }

This is the only non-degenerate equality on ColorPoints and Points per `Artima <https://www.artima.com/articles/how-to-write-an-equality-method-in-java>`__. I would argue that the multimethods are a clear win here as we can just write the clauses - the ``instanceof`` is implicit in the specificity matching, and there is no separate ``canEqual`` method. And if we removed ``or_subclass`` then we would not be overriding at all and the ``false`` clauses and priority equalization would not be needed - Points and ColorPoints would simply be treated as disjoint types and comparison between them would not be defined.

BETA
----

Per `this <https://journal.stuffwithstuff.com/2012/12/19/the-impoliteness-of-overriding-methods/>`__, BETA inverts the dispatch order. It is the least derived class in the chain that is called first, that then can call ``inner()`` to dispatch to a subclass.



Inheritance is not subtyping
============================

This is the title of :cite:`cookInheritanceNotSubtyping1989`, and it is true: the existence of ``Lens (a+b) (a'+b) a a'`` does not imply a subtype relation - ``A`` is not a subtype or supertype of ``A+B``. Rather ``A`` is related to ``A+B`` by a separate "is subcomponent of" relation, as formalized in the Lens type. For example, in :cite:`cookInheritanceNotSubtyping1989` section 3.2 page 129 we have a parent constructor ``P self super thislvl = { i = 5, id = self, eq = \o -> self.i == o.i }`` and a child constructor ``C self _ _ = { b = true, eq  = \o -> o.i == self.i && o.b == self.b }``. We can work out some types: ``mkObject [P] : mu self. { i : int, id : self, eq : {i : int}_open -> bool }`` and ``mkObject [C,P] : mu self. { i : int, id : self, b : bool, eq : {i : int, b : bool }_open -> bool }``. The second has more fields than the first, so with closed records they are unrelated types. We might think with open record types we could say that the second (child) type is a subtype of the first. But looking at ``eq``, since ``{i : int, b : bool }_open`` is a subtype of ``{i : int }_open``, by contravariance the first ``eq`` type is actually a subtype of the second. So even relaxing our record definitions these are unrelated types.

More generally, all combinations of subtyping and inheritance are possible:

* S is neither a subtype nor a derived type of T - independent types, Boolean and Float
* S is a subtype but is not a derived type of T - Int32 and Int64, subset but unrelated by inheritance
* S is not a subtype but is a derived type of T - S derived from T, S -> S is not a subtype of T -> T
* S is both a subtype and a derived type of  - when all inherited fields and methods of the derived type have types which are subtypes of the corresponding fields and methods from the inherited type, and the type is an "open record"

But note that subtype + derived type is only possible with open records - with closed records no derived type is a proper subtype.

In Java, a method only overrides its parent method if its type matches the parent method. This is a pretty weird restriction: in Smalltalk we can override a field and change its type from an int to a string, so why can't we in Java? This is a holdover from Simula of their pointer-based implementation technique - if there was no such restriction, then calling a method could lead to a type mismatch. So really this restriction is an instance of premature optimization: rather than letting the semantics be as free as possible and figuring out how to optimize it, Java chose to cripple the semantics so they could hardcode an optimization.

Inheritance-as-subtyping is easy to misuse and the Java platform libraries made numerous mistakes: Stack extends Vector, Properties extends Hashtable - in both cases, no subtyping would have been preferable. For example, (Properties) p.getProperty(key) takes defaults into account, while p.get(key) which is inherited from Hashtable does not, and direct access to the underlying Hashtable allows adding non-String objects, when the designers intended that Properties should only be Strings. Once this invariant is violated, it is no longer possible to use other parts of the Properties API (load and store). Without inheritance-as-subtyping, get could have been overridden directly, and the other Hashtable methods not exported.

Fragile base classes
====================

Inheritance should respect encapsulation, so that inheriting classes do not get any more access. Thus we see that Java's protected keyword is a hack. Allowing non-public instance variables to be accessed by subclasses breaks encapsulation. So instance variables should either be public and accessible to everyone, or private and not accessible to anything outside a module.

Inheritance as subtyping also breaks encapsulation, because superclass methods that expect to receive themselves may receive a subclass instance that doesn't support an expected contract. In particular Java inheritance allows overriding methods. So a call to self.b in A.a may resolve to an inherited implementation B.b, and this B.b may violate a contract that A.b satisfies.

Similarly, a subclass may break if its parent changes its self-use of methods, even though the subclass's code has not been touched. This is the "fragile base class" problem. Every downcall generates a code coupling that must be documented and maintained. For example :cite:`ArtimaJavaDesign`, a List class has add and addAll method and you want to write a CountingList that overrides add and addAll to count the total number of elements added. You cannot do this properly without knowing whether List.addAll does a downcall to List.add.


 There is actually a solution for this: remove ``self`` as an argument to each function passed to ``mkObject``, and only use ``thislvl``. This makes downcalls impossible, removing the code coupling. Unfortunately :cite:`temperoWhatProgrammersInheritance2013` measured that 0-86% (median 34%) of inheritance relationship have a downcall. Although some projects are at 0, suggesting it is possible to avoid downcalls, the prevalance of this practice suggests they cannot be removed categorically and must be supported.

A less restrictive solution is to ensure that whenever you do a downcall, the downcalled method is abstract in the current class. That way you know that you are calling a subclass method, hence the code coupling is self-documenting. So we prohibit "implementation inheritance", i.e. when there is a concrete method A.a being overridden by a concrete method B.a and a method in A calls A.a. Rust, Julia, Go, and Swift have all adopted this style of programming, under various names such as trait, interface, or prototype-oriented programming. Even Java 8 added default methods and static methods to interfaces. There is an associated nomenclature change. Now a concrete method is referred to as "a default implementation of a required method", and the most-derived class is given the normal name "class" while the other classes in the chain are given a more unusual name like interfaces or traits or whatever and are not allowed to be directly instantiated. This terminology makes the special role of the final class in the inheritance chain clear. And of course there is multiple inheritance, with a nice solution to the problem of multiple definitions, namely "error if there is no most-specific definition, which you can fix by defining it in the most-derived class".

But it is kind of a lie though because we aren't really getting rid of implementation inheritance at all. You can still override an interface and replace one of its default methods. Rather it is a culture change: overridable methods in interfaces are all marked as default, so it is clear that they can be overridden, and most of the time the method is left abstract and implemented in the most-derived class. In Swift non-overridable (final) methods are the default.

What does it mean to have an abstract method in an interface, ``foo : T1``? Since we have separated inheritance from subtyping, declaring ``{ foo : T1 } + { foo : ... }`` is meaningless (assuming we never use ``super.foo``) because only the type of ``foo`` on the right matters. It is rather an assertion about the final value, that ``lookup foo x : T1``. Similarly in the multimethod style, it is an assertion that ``foo x : T1``. With refinement types we can write this type explicitly, as ``{ x | foo x : T1 }``. Then an interface of abstract methods is the intersection of these types, ``{ x | foo x : T1 } intersect { x | bar x : T2 } = { x | (foo x : T1) && (bar x : T2) }``.

Even adding a method in the subclass can be unsafe, because the superclass can later add the same method and then you are unintentionally overriding it. For this reason languages have added the override annotation so that unintentional overriding generates a warning.

Overall, inheritance is quite fragile and relies on things just magically working out. For this reason it is avoided in high-reliability applications. It is better to allow controlling behavior with lambda argument hooks, so that behavior can be modified without inheritance and the hooks can have well-defined types. And as far as combining data, inheritance is too restrictive (even multiple inheritance is not a good fit) - entity component systems are more flexible and game engines such as Unreal and Unity have switched to ECSs.

Dissolving interfaces into multimethods
=======================================

Abstract interfaces are a morass of complexity. The interface could declare one, two, three, four functions or more. But it's not particularly clear how to structure that: How many traits do you have? Do you have one trait per function, one trait with all the functions, or something in between? There's no clear guidance. And it's an important decision because you can't remove a method from an interface later on without breaking lots of code. The safest decision is zero or one traits per function, and never more.

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



the Decorator pattern [Gamma95, p. 175]
delegation [Lieberman86; Gamma95, p. 20].
SELF problem [Lieberman86]

 “Why extends is evil” [15] Allen Holub. Why extends is evil: Improve your code by replacing concrete base classes
with interfaces. JavaWorld.com, August 2003.

 or “Inheritance is evil, and must be destroyed” [23]. Bernie Sumption.
 Inheritance is evil, and must be destroyed.
 Last ac-
cessed
 December
 2012.
 http://berniesumption.com/software/
inheritance-is-evil-and-must-be-destroyed, 2007.

 Gamma et al. “Favor object composition over class inheritance” [12] Erich Gamma, Richard Helm, Ralph Johnson, and John Vlissides. Design Patterns. Addison
Wesley Publishing Company, One Jacob Way, Reading, Massachusetts 01867, 1994.

Meyer described what he regarded as 12 different valid uses of inheritance [20]. Bertrand Meyer. The many faces of inheritance: a taxonomy of taxonomy. IEEE Computer,29(5):105–108, May 1996.

Taivalsaari discussed the many varieties and uses of inheritance, and provided a taxonomy for analysing inheritance [24]. Antero Taivalsaari. On the notion of inheritance. Comp. Surv., 28(3):438–479, 1996.

Taivalsaari’s taxonomy identifies three dimensions
for analysing inheritance — what he called incremental modification, property inheritance, and interface inheritance.


"Good" is a bit subjective, but I find that inheritance is often the right tool to use when modeling things like config file formats with recursive schemas (such as JSON), where you need to have a number of disparate types that can be easily accessed in similar terms.

newer languages aim to replace or abolish inheritance, like Rust and Go

A more natural approach to typing objects is structural, as in OCaml. In the type system you want information on what methods do you have, not some parent-chasing rigid encoding of an inheritance structure.

And he mentions the need for a "Mathematical binding" to this concept to stop people making a mess.

But I guess what really is the distinction between a class that has members of type A B and C, and a class that implements interfaces A B and C? I feel like those are basically isomorphic.

I would like to emphasize traits are a evolution of inheritance not something separate. These things are inheritance ++. They do the same job as inheritance while having none of the downsides.

in most cases subtyping plus dynamic dispatch are straight up the best.

you want the explicit notion of an interface for clarity.

Case 1: Implementing a trait on a type or adding a method to a class. In kotlin scala swift, using a extension method is the way to go. But there is the self type problem.
Case 2: reimplementing a single method. This is perfectly fine to use inheritance for.
Case 3: adding a single instance variable.
Case 5. Code reuse. Just implement the code on the trait not the class. See rust for example where you get like fifty methods by just defining a next method.


Conceptually JSON is a "closed" system - there are a fixed/closed set of variants to work with. Inheritance is an "open" system and can't model closed schemas without extra baggage. Sum types are a better tool for modeling closed variants. This has been disguised by OO=style languages with sum types bolted on after the fact.

I just haven't seen a clean object-oriented codebase that uses inheritance yet. The Handmade Hero codebase from Casey Muratori is one example where inheritance is not used at all, Even though it's a game built from scratch without even a single library. Casey's post on Semantic compression: https://caseymuratori.com/blog_0015

most code uses dependency injection, which is not inheritance. Genuine human usage of inheritance is few and far between, and codebases using it are a nightmare to extend (I've had to deal with it). I'd say that the impact of inheritance is close to zero for the library authors (they could just as well use DI, if the language made it comfortable to use) and a big negative for client code using those libraries (inheriting from code you don't control is a nightmare).

Go kind of does have it though. You can put a struct inside of a struct and it behaves just like inheritance.

unlabeled GOTO (i.e. classic Assembly style, as contrasted to goto LocallyScopedLabel) is liable to cause problems, but labeled GOTO is underrated.



I could have sworn I've heard at least some people talking about composition as being something like using (multiple) inheritance from interfaces instead of classes. Is that not correct?

I don't think inheritance is a general-purpose tool, but providing it in a library for legacy compatibility will probably always be necessary. I made this post to see if anyone wanted to argue for inheritance as a core feature and the answer is no, nobody cares that much.

I haven't actually seen a reduction in the usage of inheritance in any code base I've ever seen, whether it was new or old.

most people use inheritance for reducing lines of code, but that wasn't its intended purpose.

Inheritance is a clumsy tool that makes code dangerous and unmaintainable. It is drastically overused for situations it is not suited for. There doesn't seem to be any real world software that use inheritance without it being an utter catastrophe. So Stroscot's recommendation is that inheritance should never be used in new code.

OOP adds too much complexity to the syntax. You end up spending more time thinking about the design of your code than the design of your application.

features may be designed to solve a specific problem (lambdas - name capture, exceptions - domain holes), but once they are formulated the original motivation becomes almost irrelevant and the question is rather how many problems they can solve and which features are the most powerful.

In my world, languages features may be designed to solve a specific problem (lambdas - name capture, exceptions - domain holes), but once they are formulated the original motivation becomes almost irrelevant and the question is rather how many problems they can solve and which features are the most powerful. From your statements it seems inheritance seems really weak and hence is not suitable as a core language feature.

OO models objects with common attributes and behaviors. Inheritance has proven useful for this purpose. But there are better ways to express reuse with deltas. So inheritance is bad because there are good alternatives. In Java it’s traits. In rust it’s also traits but work differently (really cool).

Kinda cool but also people generally agree that inheritance is bad. GoF talks about composition over inheritance, Go doesn't have it, and yeah James Gosling who's basically the king of modern OOP says it's bad.
Kinda cool but also people generally agree that inheritance is bad. GoF talks about composition over inheritance, Go doesn't have it, and yeah James Gosling who's basically the king of modern OOP says it's bad.
Kinda feels like you're beating a dead horse. I guess it's still used a lot, but an axiomatic argument like this probably isn't very compelling to that demographic
Kinda feels like you're beating a dead horse. I guess it's still used a lot, but an axiomatic argument like this probably isn't very compelling to that demographic

Lastly, languages have rules, and to assume that all implementations of the concept of "inheritance" will look exactly like whatever-your-favorite-whipping-boy of a language is (maybe Java?) is a poor start to a thought exercise. Step back and ask yourself what exactly it is about the concept of inheritance that you so viscerally dislike: Is it too many rules? Not enough rules? The wrong rules? The idea itself?
Lastly, languages have rules, and to assume that all implementations of the concept of "inheritance" will look exactly like whatever-your-favorite-whipping-boy of a language is (maybe Java?) is a poor start to a thought exercise. Step back and ask yourself what exactly it is about the concept of inheritance that you so viscerally dislike: Is it too many rules? Not enough rules? The wrong rules? The idea itself?
Many of us who still use C will use it as an "OO-ish" language, by basically re-creating many of the concepts from OO languages in C, but by hand.
Many of us who still use C will use it as an "OO-ish" language, by basically re-creating many of the concepts from OO languages in C, but by hand.

GUI framework - entity-component probably works better

More reading about it in https://github.com/rust-lang/rfcs/pull/2046#issuecomment-311230800.
More reading about it in https://github.com/rust-lang/rfcs/pull/2046#issuecomment-311230800.



you can define an interface value to be a record of functions. Often these functions share some mutual state; at that point it's confusing to not call it an interface.

You can only directly use lambdas in this situation because your example is contrived to have only one method with which the implementation needs to be concerned. This is certainly a case which would be ideal to replace with lambdas, but real life use cases are often not this simple; when associated state or additional methods need to be bundled along with said lambdas, a structure like this becomes much more reasonable than you've made it appear.


Multiple inheritance
====================

Multiple inheritance makes things a little more complicated but conceptually is still taking a bunch of object pieces and gluing them together. There is no convenient "subclass takes precedence" rule when properties conflict between superclasses, so we must accept that such cases are ambiguous and rely more often on the specialized lookup syntax that makes clear which part of the composite object we are referring to. For example C++ uses syntax like ``p->A::next`` or ``((A) p)->next``, as well as class casts, to allow access to ambiguous properties.

There is the diamond inheritance pattern: if D extends B and C, and B and C each extend A, then are there two copies of the fields of A, or only one copy? C++ allows each superclass to be declared either non-virtual or virtual. Each virtual superclass appears only once in the composite object, while non-virtual superclasses may appear multiple times. Non-virtual inheritance makes specifying properties even more complicated because we have to specify the full path of the superclass and not just its name. C++ forbids direct duplication of base classes like ``A extends (B, B)``, so gets the ability to specify a path without numbers, only using class names - basically, you have to do ``B1 extends B, B2 extends B, A extends (B1, B2)`` instead of directly duplicating ``B`.

The code is something like:

::

  As + B = Block { prefixes : As, main : B }

  lookup x (Block {prefixes,main})
    | x in main = lookup x main
    | otherwise = merge $ map (lookup x) prefixes

  qualified_lookup x path (Block {prefixes,main})
    | [] <- path = lookup x main
    | [Superclass n,..path2] <- path = qualified_lookup x path2 (prefixes !! n)

  extends_n f rattrs_n self =
    supers = map (\rattrs_i -> rattrs_i self) rattrs_n
    thislvl = f self supers thislvl
    return (merge supers // thislvl)

Where ``merge`` uses the ordering of the superclasses or assigns properties error values on conflicts.


With MI, all classes above the join class (and often the join class itself) should be interfaces. An interface is a class with no data fields and all its methods pure virtual. This avoids inheriting an implementation along two paths.

Suppose you have vehicles, of N different geography types: land, water, air, space, amphibious, etc.
Suppose we also have M different power sources for vehicles: gas, electric, nuclear, pedal, etc.

    With the composition pattern, you have three types: Vehicle, Geography, and Engine. A Vehicle value is a record with entries for Geography and Engine, and Geography and Engine are ADTs with variants LandGeo, WaterGeo, etc., and GasPoweredEngine, NuclearPoweredEngine, etc. We can use refinement types to specify a vehicle with a certain engine, a vehicle with a certain geography, or both, with a compound conditional like ``PedalPoweredLandVehicle = { v : Vehicle | v.geo : LandGeo && v.engine : PedalPoweredEngine }``.

    With nested generalization, you have a root Vehicle class, then derive classes LandVehicle, WaterVehicle, etc., and those would each have further derived classes, one per power source type. This requires duplicating the power source fields for each geography.

    With multiple inheritance, you have GeographyVehicle and EngineVehicle, and subclasses named like GasPoweredVehicle and LandVehicle. Instead of a Vehicle record you create N×M derived classes that inherit from the geographies and the power sources. These derived classes generally have no additional fields.


Then there are methods. With composition we can put the methods wherever. With inheritance we have to put the methods in the classes and it is quite restrictive.

    With composition adding a new geography is one line, extending the relevant ADT. Depending on the desired behavior each function may also need a line, if the behavior is generic over power source, or M lines, if the behavior is specialized to power. The types can grow to N×M but this limit will likely not be reached.
    With inheritance adding a new geography is M new classes and some work on a factory method to allow creating these classes.

In this example, we have only two categories of vehicles: land vehicles and water vehicles. Then somebody points out that we need amphibious vehicles. Now we get to the good part: the questions.

    Do we even need a distinct AmphibiousVehicle class? Is it also viable to use one of the other classes with a “bit” indicating the vehicle can be both in water and on land? Just because “the real world” has amphibious vehicles doesn’t mean we need to mimic that in software.
    Will the users of LandVehicle need to use a LandVehicle& that refers to an AmphibiousVehicle object? Will they need to call methods on the LandVehicle& and expect the actual implementation of those methods to be specific to (“overridden in”) AmphibiousVehicle?
    Ditto for water vehicles: will the users want a WaterVehicle& that might refer to an AmphibiousVehicle object, and in particular to call methods on that reference and expect the implementation will get overridden by AmphibiousVehicle?

If we get three “yes” answers, multiple inheritance is probably the right choice. To be sure, you should ask the other questions as well, e.g., the grow-gracefully issue, the granularity of control issues, etc.



The diamond refers to a class structure in which a particular class appears more than once in a class’s inheritance hierarchy. For example, Join extends Der1 and Der2, and Der1 and Der2 both extend Base. The key is to realize that Base is inherited twice, which means any data members declared in Base will appear twice within a Join object. This can create ambiguities: which member did you want to change? C++ lets you resolve the ambiguities, so you could say j->Der2::data_ = 1. However the better solution is typically virtual inheritance. Using the virtual keyword for each class that inherits from the base class will ensure that an instance of Join will have only a single Base subobject. This eliminates the ambiguities. It also allows "cross delegation" so if Der1 implements foo then a method bar in Der2 can call Der1's foo if it is declared in the base class. Virtual base classes are constructed before all non-virtual base classes, in particular by the “most derived” class’s constructor.


abstraction principles:

classification: define a set as some condition, things are in the set if the condition holds [Borgida et al. 1984]
derived sets [Mattos 1988]:
 - generalization - define a set as union of some sets to capture a commonality [Knudsen and Madsen 1988; Smith and Smith 1977b; Borgida et al. 1984].
 - specialization/refinement - define a set as {x in C: extra condition} [Pedersen 1989], restricting to those elements with some additional, more specific properties. One specific form of specialization is "trait inheritance" or "interface inheritance", where a list of fields or methods that must be in a record is extended with more fields or methods. This is distinct from the "implementation inheritance" obtained in Java by the extends keyword.

instantiation:  [Knudsen and Madsen 1988]. proving that a specific element inhabits a set. Collectively the elements of a set form that set.

aggregation, composition, grouping, association, partitioning or cover aggregation [Brodie 1983; Mattos 1988; Borgida 1984; Smith and Smith 1977a]
 - treating possibly heterogenous collections of values (parts) as a single higher-level value, an aggregate or whole - list, set, bag, record, dictionary
individualization/decomposition yields the individual components of an aggregate.





as discussed here, by Bob's minimal definition, Stroscot is OO. Still compared to other "OO" languages it leaves out many "OO" features.

Minimal OO
==========

Uncle Bob `defines <https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html>`__ OO by distinguishing ``f o`` from ``o.f()``. With Uniform Function Call Syntax there is no difference. But, he argues, in an OO language ``o.f()`` is overloaded - it does dynamic dispatch based on the type of ``o``. Whereas with ``f o`` there is usually only one group of clauses for ``f``. Bob also wants to exclude implementations of dynamic dispatch that work by modifying ``f`` to use switch statements or long if/else chains. So he excludes dynamic dispatch that creates a source code dependency from ``f o`` to ``f``, i.e. ``f o`` "knows" ``f``. Instead there must be several clauses for ``f`` which may be called. Concretely, Bob says, one should be able to write ``f o`` in source file A and an implementation of ``f`` in source file B and there should be no use/require/import declaration from A to B.

Stroscot has predicate dispatch and multimethods. So all functions can be overloaded and do dynamic dispatch. Stroscot solves the expression problem, so there is no boilerplate needed when extending ``f``. Furthermore, Stroscot uses a recursive knot so definitions are properly in scope. So Stroscot is OO in Bob's minimalist sense.

Serialization
=============

Serialization is the ability to convert an object graph into a stream of bytes, and more broadly the reverse as well (deserialization). Serialization interacts with nearly everything; it is a critical facility. In Java the OO model was defined first and serialization was added later as a "magic function". The design has various problems, as described in `Project Amber <https://openjdk.org/projects/amber/design-notes/towards-better-serialization>`__:

* serialization can access private classes and fields, an implicit public set of accessors
* deserialization bypasses defined constructors and directly creates objects via the runtime, an implicit public constructor
* serialization/deserialization uses magic private methods and fields to guide the process, such as readObject, writeObject, readObjectNoData, readResolve, writeReplace, serialVersionUID, and serialPersistentFields
* The Serializable marker interface doesn’t actually mean that instances are serializable. Objects may throw during serialization, as e.g. Java has no way to express the constraint that a TreeMap is serializable only if the Comparator passed to the constructor is serializable. Also there are objects such as lambdas, which are easily serializable but error due to lacking Serializable, requiring special type casts.
* Serialization uses a fixed encoding format that cannot be modified to JSON/XML/a more efficient/flexible format, or one with version markers. There are no checks that serialization/deserialization is a round trip.

Serial form: a logical at-rest state that can be written to a stream or stored in memory. This state should be orthogonal to the choice of bytestream encoding. Java serialization strongly encourages using an object’s in-memory state as its serial form. Sometimes this is a sensible choice, but sometimes this is a terrible choice, and overriding this choice currently involves using a difficult and error-prone mechanism (readObject and writeObject.)

State extraction/reconstruction. Java serialization uses reflection to extract/set the non-transient fields of an object, using its privileged status to access otherwise inaccessible fields.

Versioning. Classes evolve over time. Unless you plan for versioning from the beginning, it can be very difficult to version the serialized form with the tools available without sacrificing compatibility. Serialization should force implementations to confront past (and possibly future) versions of their serial form, and make clear which old versions a class agrees to or refuses to deserialize, and how they map to the current representation. It should be easy and explicit to mediate between different versions of serial form and live object state.

Stream format/wire encoding. The choice of stream format is probably the least interesting part of a serialization mechanism; once a suitable serial form is chosen, it can be encoded with any number of encodings.

Project Amber proposes to restrict serialization to using public object facilities, so that the serialization functions for each type could be written piecemeal as external library functions. But universal serialization is important, so Stroscot goes further: we restrict the object model to objects that can be serialized easily, so that serialization functions are simply generic functions and aren't written individually for each type. This makes a large part of the serialization weirdness just vanish. Of course you can always define specialized serialization behaviors on top of the generic facility.

persist data, or to exchange data with other applications. Not objects; data.


Cycles and non-serializable data
--------------------------------

Cyclic data occurs in many places, e.g. a doubly linked list ``rec { a = {next: b, prev: None}; b = {next: None, prev: a} }``. We also have non-serializable data such as finalizers that does not live across program restarts. These cannot be serialized to JSON etc. as-is, because the format doesn't support it. The solution is a replacer, which transforms cyclic and non-serializable data to a form suitable for serialization. The replacer produces a bijection from bad values to good values, so that we can serialize the good values in place of the bad values and do the opposite transformation on deserialization. Then we serialize this bijection separately (out-of-band).

It is much easier to do replacement out of band because in-band replacement leads to DOS attacks such as "billion laughs". Basically the attacker defines a system such as ``a = "lol"; b = a+a; c=b+b; d=c+c;``, etc., constructing a string of a billion laughs, or similarly a large object that takes up too much memory. A simple solution is to cap memory usage, but this means some objects fail to serialize. Instead in-band entities must be treated lazily and not expanded unless necessary. Out-of-band avoids the issue by not allowing references in data.

All-or-nothing field access
===========================

In Stroscot, if you can access the term's constructor symbol, you have full data access to all fields and can destruct and create values with that constructor. But, you can avoid exporting a constructor symbol from a module - that means a user will have to use the defined factory functions and accessors, or else deliberately import the ``._internal`` module.

Similarly all fields are final - mutations are made by creating a new value. But if the field's value is a reference then you can mutate the reference as much as you want. You can just read the value if you don't want it to change, this is what Java calls "defensive copying".

.. _No inheritance:

No inheritance
==============

Overriding a method only works when you know what is calling the function and its context and invariants. It defines an interface rather than a behavior. Languages have addressed this with explicit interface types, so that a value may satisfy many interface specifications. But interfaces are simply type specifiers and do not incorporate the complexities of inheritance. There is no inheritance relationship betwen interfaces - an interface extends another by including the methods of the other interface. In general one doesn't need an explicit type hierarchy at all. Types just are, they don't have to announce their relationships. The relationships between types such as subset and superset can be inferred on demand.

For inheritance as subtyping, inheritance isn't even compatible with the `Liskov substitution principle <https://en.wikipedia.org/wiki/Liskov_substitution_principle>`__. Supposing ``A extends B``, the predicate ``\x -> not (x instanceof A)`` is satisfied by ``B`` but not by ``A``. So by LSP, A is not substitutable for B. If this is too abstract, consider ``Circle extends Ellipse``, ``class Ellipse { final float x, y; }``. We must make the class immutable, otherwise one could make a circle non-circular. But even this is not enough, because ``Ellipse { x = 1, y = 1 }`` is a circle but is not a member of the Circle class. The only solution is to forbid this value somehow, e.g. requiring to construct the objects using a factory function. A more natural solution is avoid inheritance and instead declare Circle as a refinement type of Ellipse, ``Circle = { e : Ellipse | e.x == e.y }``. Then an ellipse with equal components is automatically a circle. Similarly with serialization, a class ``A`` may be serializable but a class ``B extends A { Unserializable f; }`` will not be.

Composition can replace inheritance in at least 22% of cases :cite:`temperoWhatProgrammersInheritance2013` - just include the "parent" as a field. This offers better encapsulation and `composition over inheritance <https://en.wikipedia.org/wiki/Composition_over_inheritance>`__ has been recommended as an OO best practice.  Consider a list with ``add`` and ``addAll`` methods. Suppose you want a "counting list" that tracks the total number of objects added (the length plus the number of objects removed). With composition you can count what's passed to ``addAll`` and ``add`` and update a counter, and all works well. With inheritance, and the counting list as a subclass, it doesn't work as expected because the list's ``addAll`` method calls the subclass's ``add``, and the added objects are double counted.

One pain point when using composition to replace inheritance is that there are lots of boilerplate forwarding functions that simply pass through to the parent. But even traditional OO languages are full of these boilerplate wrappers. So this is not really a problem so much as an opportunity. Scala has `export clauses <https://docs.scala-lang.org/scala3/reference/other-new-features/export.html>`__. But Julia's solution of macros such as `TypedDelegation.jl <https://github.com/JeffreySarnoff/TypedDelegation.jl>`__ seems more appropriate, something like ``forward CountingList to list for List`` which expands to lots of declarations like ``delete a (x : CountingList) = x { list = delete a x.list }``. It has to read all symbols from the List module, look up the types, filter to the ones using the List type that have not already been redefined, then write out a new clause that applies the wrapper based on the type. Another option is to write a catch-all handler that traps accessing methods or properties and redirects to the field, but using the built-in dispatch like with the macro is more straightforward.

* resurrection via finalizers

Emulating typical OO
====================

Let's suppose you are unconvinced by the arguments above, and want classes regardless. E.g. you are translating this Java program:

::

  class Foo {
    public int x, y;
    public Foo(int a, int b) {
      x = a + b;
      y = a - b;
    }
    public int getFoo(int arg) {
      x += arg;
      return x + y;
    }
  }

It is not too hard to emulate objects using a reference cell. It stores a tag to allow dynamic dispatch, and the tag is attached to a record that stores the state of the object. So you'd write something like:

::

  newFoo (a : Int) (b : Int) =
    oid <- mut undefined
    oid := Foo { x: a+b, y : a-b }
    return oid

  oid@(read -> Foo { x : Int, y : Int }).getFoo(arg : Int) =
    x += arg
    oid := Foo { x, y }
    return x + y

Emulating inheritance
---------------------

::

  class A {
      String s;
      Int i;

      A(String s, Int i) s(s), i(i){ constructor_A(); }

      virtual void Display(){
          printf("A %s %d\n", s.c_str(), i);
      }

      virtual Int Reuse(){
          return i * 100;
      }
  };


  class B: public A{
      Char c;

      B(String s, Int i, Char c) : A(s, i), c(c){ constructor_B(); }

      -- overrides the base class version
      virtual void Display(){
          printf("B %s %d %c", s.c_str(), i, c);
      }

      virtual void Extra(){
          printf("B Extra %d\n", Reuse());
      }

  };


Inheritance involves creating cases for each inherited method that wrap the reference to look like a superclass reference and call the superclass method. Here we have simply put the superclass in a reference:

::

  constructA (s : String, i : Int) = { r = mut (A s i); constructor_A r }
  display (read -> A s i) = printf "A %s %d\n" s i
  reuse (read -> A s i) = i * 100

  constructB (s : String, i : Int, c : Char) =
    a = constructA s i
    r = mut (B a c)
    constructor_B r

  extra b@(read -> B a c) = putStrLn $ "B Extra " ++ show (reuse b)
  -- overloads but does not override base class
  display b@(read -> B a@(read -> A s i) c) = printf "B %s %d %c" s i c
  -- reuse delegates to A
  reuse b@(read -> B a c) = reuse a


The more general approach is to make the derived class have all the fields from the class and all superclasses, and call superclass methods by passing a wrapper:

::

  convertToA b@(read -> B _ _ _) = newWrapper {
    read = A { s = (read b).s, i = (read b).i }
    modify (A newS newI) =
      old = read b
      b := old // { s = newS, i = newI }
  }
  reuse b@(read -> B _ _) = reuse (convertToA b)

vtables
-------

::

  class Base {
      public:
          virtual void method() = 0;
  };

  class Derived: public Base{
      public:
          void method() {}
  };

If you really want to match OO languages perfectly you can construct the vtables.

::

  vtable Derived = { method = Derived_method }
  object = { vtable = vtable Derived, props = ... }

  vinvoke "method" object = object.vtable.method


Headers
-------

To mimic Java 100% you need the full object header, with the synchronization lock, type of object, etc., a total of 16 bytes.
