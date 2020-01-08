Types
#####

Types are hard. Academics have spent decades in search of the perfect
type system, often not even paying attention to the work of others.

Roles
=====

There's really two kinds of types (three if you include erased phantom types, but those are just dead code). First, there's representation types, which describe how the data type is represented in memory, how to pass it to a function, how many pointers you have to dereference to access it, and lifetime information for the garbage collector or memory deallocator. You have pointers, bits strings, structs, and not much else. Dependent types come in handy, for field lengths/types that depend on the value of other fields. But subtyping and polymorphism get erased as the compiler defaults variables to their generic pointer implementation or specializes the function to a more specific/efficient representation.

The other kind of types are nominal types, which are what you often see when people talk about static typing. So these types prevent you from using them in place of their underlying representation unless you've added the name to the type using a type constructor or cast. Usually they express some restriction about the data, for example with SQL statements you might have types for escaped expressions and unescaped expressions. All you're saying is that there aren't any unescaped commands in this data, but because programming languages don't memoize functions it's more efficient to say ``typeof stmt == EscapedStmt`` than it is to say ``isEverythingEscaped stmt == True``. And often the type is correct by construction, because all the methods you're using only construct values of the type, so it provides a handy way to assert an invariant without having to prove it like you would with a dependently typed environment.

Type inference
==============

If types are hard, type inference is harder. There's the `sub <https://github.com/stedolan/fyp>`__\ `typing <https://github.com/stedolan/mlsub>`__ stuff which actually has some pretty powerful type inference, better than Hindley-Milner. But `dependent <https://github.com/UlfNorell/insane/>`__
`types <https://github.com/gelisam/circular-sig>`__ will presumably ruin all the fun and require type signatures. However, bidirectional type checking should be able to minimize the amount of signatures required.

Condition checking
==================

There's some interesting `work <http://mmjb.github.io/T2/>`__ I found on termination checking by Microsoft, called `TERMINATOR <https://web.archive.org/web/20131005142732/http://research.microsoft.com:80/en-us/um/cambridge/projects/terminator/papers.htm>`__. There's an interesting representation of terms as sets, which ends up mapping out all the paths through the program, and then identifying termination is fairly easy. But since you can check all these conditions it's a very powerful analysis that can also check buffer overflows and array bounds and `resource use <https://arxiv.org/abs/1907.10096>`__ and things of that nature. I'm not sure how to integrate it into a type system but we'll figure it out.

Terminology
===========

At this point you might be getting confused about terminology. I certainly am. So here we are:

value
   A string of bits, paired with some interpretation as a data type

data type
   A representational type, that can generally be narrowed down to a pair of functions doing pickle/unpickle on the value's bits

reference
   A value providing a way to access another value, usually a memory address. It's distinct from an API because at any given time there's only one unique value that can be accessed using the reference.

variable
   A name (identifier) in the current scope that's associated to a reference.

l-value
   A variable, basically. The difference is some people don't consider ``arr[i]`` to be a variable, while they'll all agree it's an l-value.

r-value
   A value without a reference, basically an anonymous temporary value. Except in C++11 you can still bind its memory address. Segfaults galore. ¯\\_(ツ)_/¯

Type declaration syntax
=======================

There are a few options for type declarations. Jai's syntax for types is pretty simple, it's just:

::

   name: type = value;

Whereas in C and Rust the syntax is

::

   type name = value;

The third choice is to put the type on the right with the expression:

::

   name = type value;
   name = value : type; # (:) is just an operator x : y = y x

This seems like the most logical place for it. For example, if I'm declaring a new mutable variable, I don't want to say that the name is the mutable part of it, because that's just a reference to it. It's a handle so I can write my program.

What we want is to declare that the mutability as part of the value; it's a mutable value. So putting it in the expression tells the compiler that you can't store this like you would store a normal constant value; you have to create a data structure on the stack or the heap or whatever to access this variable.

By comparison, these other syntaxes don't make sense. If I had ``mut a = 1``, I'm not going to talk about "mut a" for the rest of the program, I'm going to talk about ``a``. And what happens if I write ``mut = 1``? Terrible things. Similarly ``: mut = 1`` makes no sense. Whereas ``a = mut`` actually makes some sense if I decide that 3 characters is too long, or if I'm doing higher-order functional programming that creates references. Similarly ``mut 1`` is quite sensible, as a reference cell.

So getting back to our syntax, ``name = type value``. It should be clear that we have two types of assignments. The first, with the type included, creates/allocates the stuff we need to access it, and then ``name`` is bound to a memory address.

When we assign again, it will look like ``name := value``, without a type. This has a completely different semantics: we take the thing on the left, the l-value, and we access the memory that it refers to, and we change the memory. Since the semantics is so different it uses a different syntax.

There is no kind of syntax or semantics for changing or redefining identifiers (besides fexprs/macros which we'll get to later); you can shadow, but once an identifier is declared in a scope, that's what that identifier refers to for the duration of the scope.

Concrete types
==============

Primitive types include bitstring patterns ``bits`` and pointers ``ptr``. Can't go wrong
there.

::

   bits 0xDEADBEEF
   ptr 0xDEADBEEF

The default arithmetic type is a ``number`` type.
This is arbitrary-precision magic which can store just about anything.
You can use ``integer`` for a strict integer, ``decimal`` for banking, and
``float`` for any IEEE 754 thing. For ``integer`` the ``size`` argument restricts the size and ``sign`` restricts the sign. Those aren't the full constructors, it'll probably have a range too.

::

   number 100!
   integer 123
   decimal 123.45
   float 1e20
   i8 = integer { size = 2^8 }
   i8 4
   # bits 0b00000100 or bits 0x04
   i8 129
   # Error: out of range, cannot convert
   u8 = i8 { sign = unsigned }
   u8 129
   # bits 0b10000001 or bits 0x81
