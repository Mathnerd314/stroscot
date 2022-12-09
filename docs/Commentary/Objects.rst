Objects
#######

Many people like to use the phrase "object-oriented". As far as I can tell this has no standard meaning - OO can refer to any of many concepts, principles, techniques, patterns, and philosophies used in software development. So for that reason the phrase is not used in the rest of the documentation. But as discussed here, by Bob's minimal definition, Stroscot is OO. Still compared to other "OO" languages it leaves out many "OO" features.

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

No constructors
===============

A constructor is a special type of subroutine that produces an object and returns it. Meanwhile a factory function is an ordinary function that produces an object and returns it. What is the difference? Mainly the limitations: a constructor must allocate new memory, it cannot return a subclass, and it has to be called with a noisy "new" syntax and a fixed name. Factory functions have none of these limitations.

For example, a factory function can memoize common values. A boxed primitive boolean should only have two values. But a constructor forces the program to produce millions of distinct trues and falses, creating significant overhead. A factory function can construct one true and one false and then return those from then on, avoiding the overhead entirely.

Another difference is that a factory function computes the field values first and then (typically) allocates and initializes memory, while a constructor allocates memory initialized to a default value and then overwrites each field. This implicit memory write means that concurrency and constructors interact poorly. With factory functions using an allocate-and-initialize primitive, the memory is treated as immutable, so the only issue is ensuring the allocation is private.

Deserialization bypasses defined constructors and directly creates objects via the runtime - it is an implicit public constructor. In fact this deserialization constructor is exactly the allocate-and-initialize primitive that a factory function needs.

One use of constructors is to enforce invariants (validity checking); for example a time constructor that ensures ``0 <= minutes < 60``. In Stroscot, invariants like these are defined in types, and checked on use, rather than on construction. It is often very helpful to be able to talk about about unnormalized data, which the constructor pattern prevents. And when you need the invariants, the types establish object integrity. Whereas in Java you must reason about all mutating methods to identify the possible states of an object, in Stroscot only the type needs to be examined.

A minor downside of doing away with constructors is that factory functions are not automatically marked in the documentation, so can be harder to find. Organizing the source code and documentation to group factory methods is not hard, the hard part is enforcing that such a convention is followed consistently. But it's not even clear that grouping factory functions together is the best organization.

No traits or methods
====================

Traits (Scala/Rust terminology, also called Java/Idris interfaces, Haskell typeclasses, etc.) are collections of methods. They are a morass of complexity. The trait could declare one, two, three, four functions or more. Already, there's an issue. It's not particularly clear how to structure that. How many traits do you have? Do you have one trait per function or one trait with all the functions and leave some functions unimplemented? There's no clear guidance. Without traits, each function is its own complete unit and there is no decision to make - you write exactly the functions you need.

Suppose you fudged that out, and defined an trait that you think represents a good set of functions. Now, you have to implement the trait. In Java you need to inherit the trait. In Haskell you have to implement a typeclass. In both cases a big problem comes up: you can only implement the trait once for a given type. There are ways to work around this. Java has the adapter pattern to create a record of functions, and similarly Idris allows `named implementations <https://docs.idris-lang.org/en/latest/tutorial/interfaces.html#named-implementations>`__. But it's a big mess. Sets and maps need a comparison operator. With multiple implementations floating around the comparisons can become inconsistent, e.g. inserting with comparison A and removing with comparison B.

Without traits, the functions get passed as implicit parameters, so there is no syntax needed for the default case. Multiple implementations can be accomplished using keyword parameter assignment. And to avoid inconsistent comparisons the map or set can store the comparison operator as a parameter on creation - it is simply a function after all.

As a corollary of this, Stroscot has no methods defined "inside" a type - you write ``type = ...; method = ...`` rather than ``type = { ...; method ; ... }``. They are all "free functions" or "extension methods".

No autoboxing
=============

Smalltalk had this idea that everything should be an object, including boolean and integer values. However, "wrapped primitive" objects like Boolean or Integer are distinct from normal OOP objects, in that when properly implemented they are immutable final singletons constructed through a factory method, in other words "value types". Caching all of these immutable singleton objects is of course quite inefficient in terms of memory.

Many languages have implemented "wrapped primitives" as a leaky abstraction. For example in Java ``new Integer(0) != new Integer(0)``, but ``Integer.valueOf(0) == Integer.valueOf(0)``. In JS ``false`` is of course false but ``new Boolean(false)`` is truthy.

Ultimately, discarding OO entirely and simply representing values as values is the most logical. Numbers are a distinct type of atomic value with a special meaning, and that’s okay. The idea that “everything should be an object” is mistaken and doesn’t actually simplify anything in practice. It's better to say "everything is a value" and make reference values explicit like with ``ref false``.

This is simpler than Java's, because we lack:

No object identity
==================

``new A() == new A()``. Java has this convoluted explanation of why it should be false and it just confuses people. Furthermore JSON cannot even represent the notion of object identity.

No implicit synchronization lock
================================

If you want a mutex you have to create a value of the Mutex type, not just write ``synchronize (random_object)``.

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
