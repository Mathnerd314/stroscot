Types
#####

Programmers can use types to annotate syntactic constructs. Types restrict the possible range of values an identifier may take, allowing the compiler to optimize to a specific runtime representation.

Stroscot mainly follows a dynamically typed semantics, allowing ad-hoc polymorphism and other nice dynamic features that a static semantics would make difficult. But the compiler still tries to prove properties about execution, so you can get errors if a function is not defined on a value, and the whole-program analysis narrows the storage class of values.

Type inference is undecidable in general, and the reduction is to weird problems like `semi-unification <https://www.quora.com/Why-is-type-inference-in-System-F-undecidable>`__, so Stroscot doesn't deal with types internally. Instead it uses model checking / automata theory, which better encodes undecidable properties. :cite:`naikTypeSystemEquivalent2008` provides a method to interpret the model produced by a model checker as a type derivation using flow, intersection, and union types; maybe this can be used as type inference. But the types will be complex and precise, e.g. ``length : (Nil-->0) & (Cons a b-->1+(length b))``. OTOH when there's a type error, model checking produces a concrete program trace of a failing path, which should be easy to turn into a good error message.

Although types are not first-class, sets are. Type annotations are translated to assertions, and these assertions are statically checked. ``assert`` is deeply special, since it has to work with descriptions of executable properties, so unfortunately not all programs/properties will produce an answer.

We don't have substructural types because the language itself is substructural (based on linear logic).

Sets
====

Since types are assertions and assertions are boolean-valued just like set membership, we can think of types as sets of values defined by a predicate ``isElemOf S``. Since distributive lattices are isomorphic to collections of sets this is equivalent to the definition of types in :cite:`dolanAlgebraicSubtyping2016`. Often a type will be a simple definition checking whether an element has a given tag, but the set can describe almost any side effect free computation.

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

The types (assertions) can bind the value, so Stroscot can express dependent types.
And the values are in scope in the type, so even `insanely dependent types <https://github.com/UlfNorell/insane/>`__ can be defined:

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
