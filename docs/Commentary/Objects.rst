Objects
#######

Many people like to use the phrase "object-oriented". As far as I can tell this has no standard meaning - OO can refer to any of many concepts, principles, techniques, patterns, and philosophies used in software development. So for that reason the phrase is not used in the rest of the documentation. But as discussed here, by Bob's minimal definition, Stroscot is OO. But it leaves out many OO features.

Minimal OO
==========

Uncle Bob `defines <https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html>`__ OO by distinguishing ``f o`` from ``o.f()``. With Uniform Function Call Syntax there is no difference. But, he argues, in an OO language ``o.f()`` is overloaded - it does dynamic dispatch based on the type of ``o``. Whereas with ``f o`` there is usually only one group of clauses for ``f``.

Well, Stroscot has predicate dispatch and multimethods. So all functions can be overloaded and do dynamic dispatch. So in Bob's sense Stroscot is OO.

Bob also wants to exclude implementations of dynamic dispatch that work by modifying ``f`` to use switch statements or long if/else chains. So he excludes dynamic dispatch that creates a source code dependency from ``f o`` to ``f``, i.e. ``f o`` "knows" ``f``. Instead there must be several clauses for ``f`` which may be called.

Well, Stroscot solves the expression problem, so there is no issue with extending ``f``.

Concretely, Bob says, one should be able to write ``f o`` in source file A and an implementation of ``f`` in source file B and there should be no use/require/import declaration from A to B.

This seems completely unrelated, honestly. But indeed, Stroscot uses a recursive knot so all definitions are in scope.

Rejected features
=================

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




Objects in Java are stored in heap.

You can ask what the difference is between pointers and references. Well, references are administered pointers. When the GC moves an object in memory, it has to update all the references so that they will all know where the object is at that moment. Integration with native code is also a bit cumbersome. The Java GC cannot know where the native code was copying the pointer in the memory managed by the native code. Also, there is no pointer arithmetic in Java. There are arrays, but other than that, you can just forget the nice pointer arithmetic that you could get used to when programming in C or C++. The benefit is that you can also forget memory corruption caused by erroneous pointer value calculations.
SEE ALSO: Does 32-bit or 64-bit JVM matter anymore?

Nevertheless, the objects are in the memory and when the processor needs to do some calculation with it, the object has to travel from the main memory to the processor. It was not an issue back in the day when CPUs were running 4MHz. The speed of memory access was comparable with the speed of the processor. Today, processors run 4GHz and the memory access is hardly any faster than it was in old times. This is not bad technology, rather physics. Calculate the time needed to travel from the CPU to the memory and back with the speed of light—and that is it. There is only one way to speed it up: put the memory closer to the processing unit. And actually, that is what modern CPUs do: they have memory caches on the CPU itself. Unfortunately, not only the CPU speed increased, but also the need for memory. In old times, we had 640kB on a computer and it had to be enough for everybody. Today my Mac has 16GB. Physics come again: you cannot put 16GB or more on the CPU because there is no room and also there is no effective way to cool the system, and we want to use the CPU to calculate and not to cook.

When the CPU needs a memory location, it reads into the cache. When a program needs something from the memory, it is likely that soon it will need something from the next memory location, and so on. Therefore, the CPU goes ahead and reads whole memory pages into the cache. There can be many pages in the cache from different memory areas. When we have an array and are accessing the first element, it may be slow (in CPU terms slow means a few tens of nanoseconds), but when we need the second element from the array, it is already in the cache. And it is very effective, except when this is an array of objects. In the case of an array of objects, the array itself is a continuous area of references. The CPU accessing the second element of the array will have the reference in the cache, but the object itself may be on a totally different page than the page where the first object was. There are some memory layout optimization techniques to solve this issue partially, but the real deal would be if the objects were stored lined up in the memory like the partridges after hunting. But they cannot be. Even if they were lined up one after the other in the memory, they would be separated by the so-called object header.

The object header is a few bytes ahead of the object memory that describes the object. It holds the lock that is used in synchronized statements, and also the type of the object. When we have a reference in a variable that is, for example, of the Serializable type, we will not know the actual type of the object from the variable itself. We have to get access to the object, and the Java runtime will read the actual type of the object. For Java, this object header helps in inheritance and polymorphism. Also, the few bytes in the 32bit implementations amount to 12bytes and to 16bytes on 64bit architecture. This means that an Integer stores a 32bit int value and additionally 128bit of extra administrative bits. That is a 4:1 ratio.
ValueType

Value types try to solve these issues. A value type is something like a class as far as it can have fields and methods.

Value types are being developed for Java within the Valhalla project under JEP 169. Currently, there is an early access version available to give it a try. This version is a branch off from the Java 11 version of Java and has some limitations. The syntax is preliminary with some keywords starting with a double underscore that cannot be in the final release and some features that are not implemented. Nevertheless, the possibility to give it a try is there.

What makes the value type different from an object is that the value type does not have an object header or an identity, there are no references to a value type, value types are immutable, and there is no inheritance between value types—and for this reason, there is no polymorphism. Some of these, like the lack of an object header, are an implementation detail, while others are design decisions. Let’s look at these features of value types.
No identity

Value types do not have an identity. When we are dealing with objects, two objects can be identical. Essentially, we are just speaking about the same object twice and objects can be equal. In the latter case, we have two different objects, but they are instances of the same class and the equals() method returns true when we compare them. Identity is checked using the == operator in Java, and equality is checked, as we already mentioned, with the equals() method. Primitives, like byte, char, short, int, long, float, double or Boolean also do not have an identity. In this case, this is fairly obvious. Saying that two Boolean values are both true but still different instances is nonsense. As logical values, numbers like zero, one, or pi do not have instances either. They are values. We can compare them using the == operator for equality and not for identity. The idea of value types is to extend the set of these eight primitive values with programmer-defined types that also represent values.
No references

The values are stored in the variables and not in the heap. When the bit representation of the value is in a variable, the compiler will know the type—just like it knows that the bits in a variable should be handled as a 32bit signed integer number when the type of the variable is int. It is also important to note that when a value type is passed to a method as an argument, the method will receive the “copy” of the original value type. This is because Java passes all arguments by value and never by reference. This will not change with the introduction of value types. When a value type is passed as an argument to a method call, all the bits of the value type are copied to the local argument variable of the method.
No object header

Because value types are values, stored in the variables and not in the heap, they do not need a header. The compiler just knows what type a variable is and in what way the program should handle the bits in that variable. This is crucially important when the value type arrays come into the picture. Just like in the case of the primitives, when we create an array of a value type, the values are packed one after the other in the memory. This means that with value types we will not have the problem that object arrays have. There are no references to the individual elements of the array and they cannot be scattered in the memory. When the CPU loads the first element, it loads all the elements that are on the same memory page and accessing consecutive elements will use the advantage of the processor cache.
No inheritance

There could be inheritance between value types, but it would be extremely difficult to manage by the compiler and would not bring many benefits. I dare say that allowing inheritance would not only cause issues to the compiler but would also lure inexperienced programmers into creating constructs that would do more harm than good. In the upcoming Java version, which supports value types, there will be no inheritance between value types or between classes and value types. This is a design decision and to explain among the many reasons let us look at some simple examples.

When a class C contains a field of the type of another class P, it contains a reference to that other class. It can also be that C is the child of the P parent class. This is not a problem. For example, there is a linked list of P instances. There is a field called next that is either null or holds the reference to the next P in the list. If the list can also contain instances of C (remember: C extends P) then C also has a reference to the next P. The list can contain C instances because, as the inheritance implies, a C is also a P.

What is the situation if P is a value type? We cannot link the elements together. There is no reference to the next P because there is no such thing as a reference to a value type. Classes can reference each other via field values. Value types implement only containment. When a value type has a field that is of another value type, it will contain all the bits of that other value type. A value type, therefore, can never contain a field that is of its own type. It would mean that the value type contains itself and this is an infinite recursion in the definition of the type. Such a type would be infinitely large and is therefore explicitly forbidden in the specification. If value types could inherit from each other, the restriction would be more complex. In that case, we could not simply say that a value type must not contain itself. We should have forbidden any other value type that is a descendant of the current value type.

Also, try to imagine a variable that is of the type V. Such a variable should be large enough to hold all the bits of V. But it should also be large enough to hold all the bits of K if it were possible to extend value objects and K would hypothetically extend V. In that case, K will contain all the bits of V and some more of its own. How many bits should a variable of the type V have? The number of bits of V? Then we cannot store a K value in it, K would not fit. All variables of type V should be large enough to also hold a K. But we should not stop at K since there could be more value types that extend K, still assuming that there was inheritance, which there is not. In that case, a variable of the type V should have as many bits as the largest descendant of V could have, which is unknown at the time of the compilation. It becomes known only when all the value types are loaded.
No polymorphism

Since there is no inheritance, there cannot be value type polymorphism. However, there are more reasons that suggest that it is not reasonable to implement polymorphism for value types. Let’s look at the example above and imagine the variable that can hold all the bits of the largest descendant of V. When we call a method on this variable, which one should we invoke during runtime? The variable does not hold information about the actual value type’s type. Is it V, is it K or some other descendant? The compiler has to know which method to call because there is no header information that would signal the type that is in the variable at that moment.
Immutability

Immutability is a design decision, but it is a natural way. Value types in Java are immutable. Immutability is generally a good thing. Immutable objects help a lot with coding in a clean and thread-safe way. Immutability does not solve all the problems, but many times it is handy. Also if you think about an int as a number, it is fairly obvious that you cannot change its value. If a variable holds the integer value 2, then you can change the value stored in the variable but you cannot change the value itself to 3. If you could, then suddenly 2 times 2 would be nine in the whole universe. The similar philosophy holds for value types. You can change the content of the variable that holds the value type, but you do not change the value type itself. When you change a single bit then essentially, by philosophy, you created a new value type and stored the new value in place of the old one. Have a look at the following simple example (Listing 1):

package javax0.valuetype;

public __ByValue class Point {
    public int x;
    public int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Point pushedRight(int d) {
        return __WithField(this.x, x+d);
    }

    public String toString() {
        return "[" + x + "," + y + "]";
    }
}

This is a simple value type that uses a preliminary syntax that was compiled using the build 11-lworldea+0-2018-07-30-1734349.david.simms.valhalla version of the early access Java. It makes it possible to “push” a point along the X axis. The main program using it does that, as seen in Listing 2:

package javax0.valuetype;

public class Main {

    public static void main(String[] args) {
        Point a = new Point(3,4);
        a = a.pushedRight(1);
        System.out.println(a);
    }
}

When we invoked pushedRight, the variable a got a new value. The point (3,4), however, did not move, the two-dimensional space did not get distorted. That point remains there forever, only the variable value is changed. If we now try to change the line a = a.pushedRight(1); to a.x = 4;, we get a compilation error that says (Listing 3):

/.../src/javax0/valuetype/Main.java:7: error: cannot assign a value to final variable x

Note that the field x was not declared final, but since it is in a value type, it is automatically final.

The immutability as a feature correlates strongly with the fact that there can be no references to a value type. Java could theoretically allow us to modify a field of a value type. The result would essentially be the same: we get a different value. This way, immutability in case of value types is not a restriction. It is only a matter of how we write our program, and how we think of value types. Thinking of them as values like numbers, which are immutable by essence, is deemed to be a healthy way of thinking.