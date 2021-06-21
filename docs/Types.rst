Types
#####

Types are hard. Academics have spent decades in search of the perfect type system. But really precise types end up looking like ``(Nil-->0) & (Cons a b-->1+(length b))``, which is just the original program. So Stroscot opts instead for model checking: leave programs with a dynamic, unityped semantics, but try to prove properties about execution. In particular the idea "well-typed programs don't go wrong" is simplified to "prove programs don't go wrong", "wrong" is defined via assertions, and these assertions are statically checked. ``assert`` is deeply special, since it has to work with descriptions executable properties, so unfortunately not all properties can be checked.

Dynamically typed doesn't mean that Stroscot has no types; types can still be specified and used to control your program, a la TypeScript.




The implementation of sets is based on syntax tree manipulation. Non-set values include all the core expressions, ADT elements and lambda expressions and so on. Doing logic in Stroscot is confusing because the reduction semantics itself uses logic. The proof tree in the reduction semantics is the program being executed, while the proof tree in type theory is automatically deduced from the type (formula) by a meta-program (theorem prover).

Types
=====

Types are simply sets. Since distributive lattices are isomorphic to collections of sets this is equivalent to the definition of types in :cite:`dolanAlgebraicSubtyping2016`. Often a type will be a simple definition checking whether an element has a given tag, but the set can describe almost any side effect free computation.

::

  a : T = { assert(a isElemOf T); a }
  T a = a : T

Functions
=========

Function type declarations come in two forms. The first version simply checks compatibility, that the return type is as expected on the given input.

::

   A : S -> Int
   A = ...

   -- expands to

   s = arbElem()
   assume(s isElemOf S)
   assert(A s isElemOf Int)
   A = ...

The second version, "strict typing", restricts the definition of the function so it is only defined on the type, i.e. it will throw an error if given something outside its type. You can define multiple strict functions to obtain overloaded behavior on different types.

::

   strict
      A : (S,T) -> Int
      A = ...

   -- expands to

   A$untyped = ...
   A = {
      assert ($args isElemOf (S,T))
      ret = A$untyped $args
      assert (ret isElemOf Int)
      ret
   }

Dependent types
---------------

Since the value is in scope, defining an (insanely) dependent type is easy:

::

  A : (s : S s) -> T s
  A = ...

  -- expands to
  s = arbElem()
  assume(s isElemOf S s)
  assert(A s isElemOf T s)

ADTs
====

Abstract data types are sets containing trees of uninterpreted symbols. So a datatype declaration (from `here <https://github.com/UlfNorell/insane/blob/master/Context.agda>`__)

::

   data Cxt [ Ty : Cxt Ty -> Set ] : Set where
      nil  : Cxt Ty
      snoc : (G : Cxt Ty) -> Ty G -> Cxt Ty

is equivalent to

::

   symbol nil
   symbol snoc
   Cxt Ty = a = arbElem(); assume(a isOfType Cxt Ty); assert(Ty a isElemOf Set); Set { x where
      (x == nil
      or exists G y. x == (snoc G y) && G isElemOf (Cxt Ty) && y isElemOf (Ty G))
   }

The recursive appearance of ``Cxt Ty`` is interpreted using the least pre-fixed point and Bekić's theorem as in :cite:`dolanPolymorphismSubtypingType2017` section 2.2.

Records
=======

You can pass ``{a: undefined, b: 2}`` to a function ``f rec = rec.b``. This is similar to structural subtyping. If the function used ``a``, it would give an error.

Type synthesis
==============

Type synthesis is tricky, but with the termination checker we don't have any visible types. The optimizer does a form of type synthesis when it assigns formats to values, but the formats can be conditional on state, and the optimizer will use a catch-all format for hard cases, so the formats are complete but not sound. The only useful case for a complex type synthesis algorithm might be pretty-printed type signatures in documentation, but there having the developer specify type signatures is a viable option.

But `dependent <https://github.com/UlfNorell/insane/>`__
`circular <https://github.com/gelisam/circular-sig>`__ dependent types will presumably ruin all the fun and require type signatures.

Roles
=====

Roles are just an optimization for ``coerce``, but there are better ways to implement optimizations. It seems like a dirty hack to solve a pressing problem. I think Stroscot can get by without them.
