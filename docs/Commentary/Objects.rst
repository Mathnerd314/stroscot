Objects
#######

Many people like to use the phrase "object-oriented". As far as I can tell this has no standard meaning - OO can refer to any of many concepts, principles, techniques, patterns, and philosophies used in software development. So for that reason the phrase is not used in the rest of the documentation. But as discussed here, by Bob's minimal definition, Stroscot is OO. But it leaves out many typical OO features.

Minimal OO
----------

Uncle Bob `defines <https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html>`__ OO by distinguishing ``f o`` from ``o.f()``. With Uniform Function Call Syntax there is no difference. But, he argues, in an OO language ``o.f()`` is overloaded - it does dynamic dispatch based on the type of ``o``. Whereas with ``f o`` there is usually only one group of clauses for ``f``.

Well, Stroscot has predicate dispatch and multimethods. So all functions can be overloaded and do dynamic dispatch. So in Bob's sense Stroscot is OO.

Bob also wants to exclude implementations of dynamic dispatch that work by modifying ``f`` to use switch statements or long if/else chains. So he excludes dynamic dispatch that creates a source code dependency from ``f o`` to ``f``, i.e. ``f o`` "knows" ``f``. Instead there must be several clauses for ``f`` which may be called.

Well, Stroscot solves the expression problem, so there is no issue with extending ``f``.

Concretely, Bob says, one should be able to write ``f o`` in source file A and an implementation of ``f`` in source file B and there should be no use/require/import declaration from A to B.

This seems completely unrelated, honestly. But indeed, Stroscot uses a recursive knot so all definitions are in scope.

No constructors
---------------

A constructor is a special type of subroutine that produces an object. So what distinguishes it from a factory function? Mainly its limitations: it must allocate new memory instead of being able to memoize common values, it cannot return a subclass, and it has to be called with a noisy "new" syntax and a fixed name. Factory functions have none of these limitations. There is no reason to have constructors.

For example, a boxed primitive boolean should only have two values. A constructor forces the program to produce millions of trues and millions of falses. But this just creates overhead. A factory function can construct one true and one false and then return those from then on. And with value semantics there is no construction involved at all.

The only downside is that constructor-like functions can be harder to find in the documentation, because they are not automatically marked. But organizing the documentation to put methods of the form ``a -> Foo`` where ``Foo`` doesn't appear in ``a`` first is not hard. It's not clear though that that's the best organization.

.. _No inheritance:

No inheritance
--------------

Inheritance is broken. Supposing ``A extends B``, ``\x. !(x instanceof A)`` is satisfied by ``B`` but ``A``. So by the `Liskov substitution principle <https://en.wikipedia.org/wiki/Liskov_substitution_principle>`__
A is not substitutable for B. If this is too abstract, consider ``Circle extends Ellipse``, ``class Ellipse { final float x, y; }``. We must make the class immutable, otherwise one could make a circle non-circular. But even this is not enough, because ``Ellipse { x = 1, y = 1 }`` is a circle but is not a member of the Circle class. The real solution is to give up on inheritance, make them disjoint classes, and use a factory function which returns either a circle or an ellipse.

Inheritance is not composable; composition is better. Consider a list with add and addAll methods. Suppose you want a "counting list" that tracks the total number of objects added (the length plus the number of objects removed). With composition you can count what's passed to addAll and add and update a counter. With inheritance and the counting list as a subclass it doesn't work as expected because, the list's addAll method calls the subclass's add, and the added objects are double counted.

Functions define common interfaces but not common behaviors; they should be overloaded but not overridden. Overriding only works when you know what is calling the function and its context and invariants.

One pain point might be when there are lots of function to pass through to a subobject. But here you can use macros, something like ``wrap delete,replace,find of List with CountingList using getList`` which expands to lots of declarations like ``delete a (x : CountingList) = delete a (getList x)``. It has to look up the clauses for the named symbols that apply to ``List``, then replace ``l : List`` with ``getList cl : CountingList``, then write out a new clause for ``CountingList`` that preserves the other types involved. Not impossible but the kind of thing that needs to be in the standard library to make sure it works. The symbols list could also be automated by taking all symbols from a module, with munging like hiding symbols, to reduce maintenance.

No traits or methods
--------------------

Traits (Scala/Rust terminology, also called Java/Idris interfaces, Haskell typeclasses, etc.) are collections of methods. They are a morass of complexity. The trait could declare one, two, three, four functions or more. Already, there's an issue. It's not particularly clear how to structure that. How many traits do you have? Do you have one trait per function or one trait with all the functions and leave some functions unimplemented? There's no clear guidance. Without traits, each function is its own trait and there is no decision to make - you write exactly the functions you need.

So, you fudged that out, and defined an trait that you think represents a good set of functions. Now, you have to implement the trait. In Java you need to inherit the trait. In Haskell you have to implement a typeclass. In both cases a big problem comes up: you can only implement the trait once for a given type. There are ways to work around this. Java has the adapter pattern to create a record of functions, and similarly Idris allows `named implementations <https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#named-implementations>`__. But it's a big mess. Sets and maps need a comparison operator. With multiple implementations floating around the comparisons can become inconsistent, e.g. inserting with comparison A and removing with comparison B.

Without traits, the functions get passed as implicit parameters, so there is no syntax needed for the default case. Multiple implementations can be accomplished using keyword parameter assignment. And to avoid inconsistent comparisons the map or set can store the comparison operator as a parameter on creation - it is simply a function after all.

As a corollary of this, Stroscot has no methods defined "inside" a type - you write ``type = ...; method = ...`` rather than ``type = { ...; method ; ... }``. They are all "free functions" or "extension methods".