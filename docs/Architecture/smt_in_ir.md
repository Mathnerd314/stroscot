ok so lessons
- use predicates to label cases, not an integer function - can be nondeterministic but is easier for solver to handle, maps better to typical way symbolic execution is handled
- typical SMT solvers are not capable enough to define a symbolic function `BV64 -> ProofTree` and do meaningful things with it
- using separate `SMTSplit` node to represent **runtime/logical uncertainty** (which branch applies may depend on an unknown symbolic input) makes sense

I don't understand what you are saying about path contexts, there is no "reduce" operation in the IR tree, it is pure data.

- if our solver cannot represent the proof tree type, we can approximate the `BV64 -> ProofTree` function by splitting it into a symbolic "path condition" function of type `BV64 -> Int` and a concrete type `BV64_ref -> Int -> ProofTree`, where the symbolic function attains a finite $$n$$ number of values and then in the concrete trees we can reference the BV64 symbolically but we cannot change the proof structure based on the actual value.
- and if the solver can represent ADTs, we can also dissociate the proof tree representation from the break node; the break node can simply introduce a symbolic variable whose type is the ADT cases, and then this proof-tree-splitting can be a special new node that appears anywhere in the tree and splits based on a symbolic function of the ADT case variable, and potentially other variables - as long as all the variables in the symbolic formula are introduced by parent break nodes. so then instead of replacing our ProofTree type with this (BV64 -> Int, Int -> ProofTree) representation only in the break node, we are augmenting it, ProofTree = Break | Build | ... | SMTSplit (BV64 -> Int, Int -> ProofTree).
- and then this is where letting build nodes use symbolic functions is useful, because they can also reference the variables introduced by the break nodes


- and then cut elimination works as follows:
  - with the native SMT proof type, we can simply compute the composition in the SMT solver, and the solver will handle all the representation details
  - with the split proof tree representation, it is a more complex operation of substituting the build formula's symbolic expression into the path condition and then simplifying to remove impossible conditions - but this is where the dissociated representation is useful because sometimes we may not be able to simplify it down to just 1 case and so there will be a residual SMT-based split node. Essentially we need to represent "SMT choice" explicitly in the IR.
  - and in many cases, like with the operation nodes I mentioned, the proof structure will be independent of the value and we can represent them entirely as SMT formulas without any "SMT choice" nodes

- you want an explicit typed SMT language, that is kind of orthogonal to this, every SMT library has its own treatment of formulas. We will use whatever native type the SMT library uses for formulas. the question is whether to have an explicit SMT node that does this proof tree choice splitting.

- this stuff with e-graphs is another layer, now we not only have SMT choices but also SMT-validated equivalences.

## Keep a path context external

It may be cleaner to avoid storing the whole path condition redundantly inside every `SMTSplit`. Instead reduce under a context:

```text
Reduce(context: SmtTerm[Bool], tree: ProofTree)
```

Then:

```text
Reduce(P, SMTSplit { g1 -> t1; ...; gn -> tn })
```

queries satisfiability of \(P \land g_i\), and enters each surviving child under context \(P \land g_i\).

This makes the path condition a property of *where the reducer is*, rather than a syntactic burden copied into every node. The split itself needs only its local guards.

## E-graphs are separate

Correct: e-graphs are another layer, not the mechanism for symbolic choice.

- SMT proves **feasibility and equality**: whether a guard can hold or two terms are equal under constraints.
- An e-graph represents **known equivalence**: multiple expressions that are already justified as denoting the same value.

For instance, these are different claims:

\[
x = 0 \quad\text{or}\quad x \ne 0
\]

is a symbolic choice represented by `SMTSplit`.

\[
\operatorname{popcount}(x)
=
\text{shift-mask-add-network}(x)
\]

is an equivalence, represented as a rewrite/e-class relation after it has been established. Equality saturation’s purpose is to preserve multiple equivalent expressions rather than prematurely selecting one. [dl.acm](https://dl.acm.org/doi/pdf/10.1145/3434304)

## Recommended architecture

```text
ProofTree
  = BuildFinite(...) - simple small-datatype constructor
  | BreakFinite(...)
  | Build(...) - builds SMT formula, can use symbolic variables in scope
  | BreakSMT() -- binds x : \mathrm{BV64}, x is in scope in subtree
  | Cut(producer, consumer)
  | SMTSplit([(SmtBool, ProofTree)])
  | ...

Symbolic values
  = typed native SMT terms

Reduction environment
  = path condition
  + variable bindings
  + solver session
  + rewrite/e-graph context
```

The governing rule is:

> Use explicit `BreakFinite` for real, small structural alternatives; use formulas to construct symbolic values; use `SMTSplit` only when proof-tree structure itself depends on an unresolved symbolic predicate.

That gives you the residual symbolic choice you identified, without pretending that a bit-vector is an enumerable ADT or requiring SMT to normalize arbitrary recursive proof trees.