---
# The Stroscot IR: A Compiler Engineer's Guide
### From Data-Control Symmetry to Machine Code
---

## I. The Problem: When Optimization Breaks Optimization

Modern compiler IRs contain a fundamental asymmetry: data values are first-class citizens, but control continuations are second-class. This asymmetry causes real breakdowns. Consider the following program in a typical Sea of Nodes (SoN) compiler:

```javascript
switch(x) {
  case 1: return a/b;
  case 2: return a/b;
  case 3: return 42; // Hot path
}
```

Global Common Subexpression Elimination (CSE) merges the two `a/b` computations into one node. Global Code Motion (GCM) then schedules it at the Least Common Ancestor of its consumers — the top of the `switch` — causing the expensive division to execute unconditionally, even when taking the hot `case 3` path. A sinking pass pushes it back down, CSE merges it again, and the cycle never terminates.

This is not a bug in CSE or GCM individually. It is a **vocabulary failure** of the IR. The IR has no way to say: *"Share this computation, but bind it to a specific control context without hoisting it globally."* The root cause is that continuations — the "rest of the program after a branch" — cannot be named, shared, or manipulated the way data values can.

---

## II. The Solution: Data-Control Symmetry

The 2016 ICFP paper *"Sequent Calculus as a Compiler Intermediate Language"* (Downen, Maurer, Ariola, Peyton Jones) identified the precise mathematical source of this asymmetry. Under the Curry-Howard correspondence — where propositions are types, proofs are programs, and cut elimination corresponds to evaluation — the classical two-sided Sequent Calculus makes the asymmetry explicit:

- A `let` binding names and shares a **data value** — a *producer*.
- A **Shared Tail-Target** names and shares a **control continuation** — a *consumer*.

In standard IRs, only the producer side is first-class. The two-sided classical Sequent Calculus, and its extension into Linear Logic, treats producers and consumers with complete symmetry. This **Data-Control Symmetry** is the theoretical foundation of the Stroscot IR.

Note: the Sequent Core paper (Downen et al., ICFP 2016) uses an *intuitionistic* restriction of the sequent calculus (single-conclusion sequents) to enforce functional purity. The Stroscot IR uses the full **classical two-sided** formulation, \(\Gamma \vdash \Delta\), which is the key design departure that achieves complete Data-Control Symmetry.

The PLDI 2017 paper *"Compiling Without Continuations"* (Maurer, Downen, Ariola, Peyton Jones) showed that Shared Tail-Targets (there called "Join Points") directly solve the scheduling problem above, compiling to unconditional `jmp` instructions:

```assembly
  jump to division_cont if x == 1
  jump to division_cont if x == 2
  return 42 if x == 3
division_cont:
  return a/b
```

---

## III. The Infinite Regular Tree Model

Before introducing the physical representation, it is important to establish the conceptual model that unifies all of the Stroscot IR's structure.

Conceptually, a program is a **single, infinite, acyclic proof tree**: the full unrolling of every loop and recursive call into an infinite tree of operations. In this ideal model, all operations — data, control, and effects — are nodes in the same logical structure, and all optimization rules apply uniformly throughout.

In practice, this infinite tree is **finitely encoded** using cyclic references. When a leaf of one proof tree points back to the root of an ancestor tree, it is not creating a second logical tier — it is a memory-efficient shorthand for "continue unrolling the tree here." This is directly analogous to how any looping program can be unrolled into an infinite sequence of operations, and the loop is simply the finite device for representing that sequence.

This infinite-tree model has a critical consequence: **all global optimization passes are structurally uniform**. An optimizer does not need to behave differently "inside a block" versus "across a block boundary." All optimization passes — CSE, DCE, constant folding — operate as **Structural Fold Optimization** (cut elimination). Conceptually these are always traversing a single, infinite, acyclic tree. The cyclic references are an implementation detail, not a semantic boundary.

In practice, it is often convenient to work locally and treat cyclic references as opaque node types rather than a transparent representation. The proof blocks are locally acyclic, so local optimizations are guaranteed to reach a normal form, whereas working with infinite trees can lead to infinite loops without special handling. A special "relooper" pass can normalize the cyclic references to best represent the infinite tree.

---

## IV. Physical Architecture: Proof Blocks

The finite physical encoding of this infinite tree consists of two components:

### Proof Blocks (PBs)

A PB is a single, locally acyclic Linear Logic derivation tree (a DAG). It corresponds directly to a Basic Block in a standard SSA/CFG compiler. The PB has a root type, $\Gamma \vdash \Delta$. This corresponds to the block's incoming and outgoing arguments (SSA $\Phi$-nodes signature). The interior derivation steps correspond to the block's individual SSA instructions. The PB type boundary enforces dominance structurally, eliminating an entire class of scheduling bugs. Local optimizations operate within a single PB, global optimizations operate across PB boundaries. Because a PB is acyclic, local optimizations are guaranteed to reach a normal form.

### Inter-Block Continuations

An Inter-Block Continuation is an explicit reference from a leaf of one PB to the root of another. It carries typed argument values that feed the target PB's $\Gamma$ context. This maps to:

| Inter-Block Continuation Form | SSA/CFG Equivalent |
|---|---|
| Leaf → another PB | Unconditional `jmp` |
| Leaf → own PB root | Loop back-edge + header `jmp` |
| Mutual A→B→A references | Mutually recursive blocks |

Because these references are the only way recursion and looping appear in the IR, the **cycle lives exclusively in the Inter-Block Continuation graph**, not inside any single PB's derivation tree. Inter-Block Continuations are a representation convenience — they are the finite encoding of what is conceptually a single infinite proof tree.

Inter-Block Continuations support two optimizations:
- **Global CSE:** Two identical sub-derivations producing the same $A$ are folded into one IBC reference. Unlike traditional CSE on SSA, the CSE here can share any combination of data and code.
- **Inlining:** Expanding an Inter-Block Continuation by copying the target PB's tree into the source. This is structurally equivalent to function inlining and requires the same standard dominance checks as in a traditional compiler.

---

## V. The Logical Engine: Logical Op-Nodes

All program operations inside a PB — arithmetic, loads, stores, branches, calls — are represented as **Logical Op-Nodes** built from two universal rule families.

### Positive Logical Op-Nodes ($\mathbb{J}^+$)

A positive Logical Op-Node represents **constructed data** such as a tuple or variant. Its right-side rule ($\mathbb{J}^+_{R,i}$) builds a value from a set of proofs and refutations. Its left-side rule ($\mathbb{J}^+_L$) eliminates (pattern-matches) a positive value.

$$
\dfrac{\overrightarrow{ \Theta_j \vdash A_{i j}, \Lambda_j } \quad \overrightarrow{ \Gamma_k, B_{i k} \vdash \Delta_k }}{\overrightarrow{\Gamma}, \overrightarrow{\Theta} \vdash \mathop{\mathbb{J}^+} \limits_{i} \left( \overrightarrow{A_i} \vdash \overrightarrow{B_i} \right), \overrightarrow{\Delta}, \overrightarrow{\Lambda}} \ (\mathbb{J}^+_{R,i})
\qquad
\dfrac{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta } }{\Gamma, \mathop{\mathbb{J}^+} \limits_{i} \left ( \overrightarrow{A_i} \vdash \overrightarrow{B_i} \right ) \vdash \Delta } \ (\mathbb{J}^+_L)
$$

Examples: an integer literal is a trivial $\mathbb{J}^+$ with 2^16 cases and no sub-structure. A struct constructor with two fields is a $\mathbb{J}^+$ with one case that takes two proofs as input. Both are constructed with the right rule and pattern-matched with the left.

### Negative Logical Op-Nodes ($\mathbb{J}^-$)

A negative Logical Op-Node represents a **control flow operation** such as a function or continuation. Its right-side rule ($\mathbb{J}^-_R$) builds a function from a set of premise derivations. Its left-side rule ($\mathbb{J}^-_{L,i}$) applies (calls) the function or dispatches on the chosen continuation.

$$
\dfrac{\overrightarrow{ \Gamma, \overrightarrow{A_{i j}} \vdash \overrightarrow{B_{i k}}, \Delta }}{\Gamma \vdash \mathop{\mathbb{J}^-} \limits_{i} \left(\overrightarrow{A_i} \vdash \overrightarrow{B_i}\right), \Delta } \ (\mathbb{J}^-_R)
\qquad
\dfrac{\overrightarrow{ \Gamma_j \vdash A_{i j}, \Delta_j } \quad \overrightarrow{ \Theta_k, B_{i k} \vdash \Lambda_k }}{\overrightarrow{\Gamma}, \overrightarrow{\Theta}, \mathop{\mathbb{J}^-} \limits_{i} \left(\overrightarrow{A_i} \vdash \overrightarrow{B_i}\right) \vdash \overrightarrow{\Delta}, \overrightarrow{\Lambda}} \ (\mathbb{J}^-_{L,i})
$$

Examples: A function call is a $\mathbb{J}^-$ with one input and one output. A continuation is a $\mathbb{J}^-$ with one input and no outputs.

The **Data-Control Symmetry** is visible in the rules: $\mathbb{J}^+$ and $\mathbb{J}^-$ are structural duals of each other. A Shared Tail-Target is precisely the $\mathbb{J}^-$ dual of a `let` binding's $\mathbb{J}^+$.

---

## VI. Resource Management: Exponential Rules

By default, Linear Logic treats every value as a single-use linear resource — it must be consumed exactly once. This models side-effecting values (handles, file descriptors, unique references) naturally. For values that must be freely copied or discarded (ordinary integers, booleans), the **Exponential Rules** lift this restriction.

The exponential modality comes in two polarities: $\bang^+$ ("of course", positive, freely shareable) and $\bang^-$ ("why not", negative, the dual). Their core rules are:

- **Promotion:** Wraps a value in a shareable box. Requires all context values to already be shareable.
$$\dfrac{\overrightarrow{\bang^+_{M_i} \Gamma_i} \vdash A, \overrightarrow{\bang^-_{N_i} \Delta_i}}{\overrightarrow{\bang^+_{M_i} \Gamma_i} \vdash \bang^+_Z A, \overrightarrow{\bang^-_{N_i} \Delta_i}} \ (\bang^+)$$

- **Dereliction ($\bang^+ d$, $\bang^- d$):** Uses a shareable value once, reverting to a linear value.
- **Weakening ($\bang^+ w$, $\bang^- w$):** Discards a shareable value.
- **Contraction ($\bang^+ c$, $\bang^- c$):** Duplicates a shareable value $n$ times.

There are also admissible rules derivable from these core rules:
- **Weak promotion ($\bang_\text{weak}$):** Promotes an entire sequent to a shareable box.
- **Digging ($\bang_\text{dig}$):** Removes multiple layers of boxing.
- **Multiplexing ($\bang_\text{multiplex}$):** Promotes $n$ bare copies to a single boxed value.
- **Absorption ($\bang_\text{absorb}$):** Removes a redundant bare copy already present alongside the box.

This structure means the cost of copying is always explicit in the proof tree. There is no implicit duplication, which is a significant advantage over intuitionistic-style IRs where copying can happen anywhere during reduction.

---

## VII. Structural Fold Optimization (Cut Elimination)

Optimization in the Stroscot IR is not a collection of ad hoc pattern-matching passes. It is a single, mathematically grounded rewrite system: **Structural Fold Optimization**, which is precisely the cut-elimination theorem of Linear Logic applied to the PB's proof tree.

The cut rule:
$$\dfrac{\Gamma \vdash A, \Delta \quad \Theta, A \vdash \Lambda}{\Gamma, \Theta \vdash \Delta, \Lambda} \ (\text{cut})$$

...states that if we have produced a value $A$ and then consumed it, the two derivations can be fused, eliminating the intermediate $A$. Standard compiler optimization passes are special cases:

- **DCE:** A derivation producing $A$ where $A$ appears in a weakening ($\bang^+ w$) step is eliminated.
- **Constant Folding:** A $\mathbb{J}^+$ or $\mathbb{J}^-$ constructor node immediately consumed by a matching eliminator reduces by cut elimination.

Because each PB is a locally acyclic DAG, Structural Fold Optimization within a single PB is confluent (Church-Rosser) and cannot cause over-hoisting. No data node can escape its control region because the PB's boundaries enforce dominance structurally. Note that confluence applies to the intra-PB fragment; once the cyclic Use/Def rules are added for Turing-completeness, strong normalization is intentionally absent — the optimizer must use standard loop termination guards when operating across PB boundaries.

---

## VIII. Infinite Proof Structures and Turing-Completeness

Under the Curry-Howard correspondence (propositions are types, proofs are programs, cut elimination is evaluation), typed Linear Logic proofs always terminate: the cut-elimination procedure is strongly normalizing, so any language whose programs are exactly typed Linear Logic proofs is not Turing-complete.

Rather than adding type-level fixed points ($\mu$MALL-style, following Baelde), which would make proof-search complexity hyperarithmetical, or guarded modalities (Nakano's "Later" modality, which restores strong normalization — exactly the wrong property for a Turing-complete IR), the Stroscot IR uses **Use/Def rules** — the formal machinery for Inter-Block Continuations:

$$
\dfrac{X}{\Gamma[\overrightarrow{x \mapsto t}] \vdash \Delta[\overrightarrow{x \mapsto t}]} \ (\text{Use})
\qquad
\dfrac{\Gamma \vdash \Delta}{X = } \ (\text{Def})
$$

A `Def` rule names a PB derivation $X$. A `Use` rule inserts that derivation at any leaf, potentially with substituted variables. When a `Use` points back to its own enclosing `Def`, this is a loop. When two `Def`s mutually `Use` each other, this is mutual recursion.

Each individual PB, with its `Use` leaves left unexpanded, remains a locally valid, cut-eliminable Linear Logic proof. The cycle exists only in the Inter-Block Continuation graph, not inside any derivation tree. This is why the conceptual model — a single infinite regular tree — is coherent: expanding all `Use` nodes produces the infinite unrolling.

---

## IX. The Stroscot–Kelsey Correspondence

The Stroscot IR implements the Kelsey Correspondence (CPS $\Leftrightarrow$ SSA) in a logically grounded form. The correspondence holds for the **SSA fragment** of the Stroscot IR, defined by a precise type-based restriction.

### The SSA Fragment

**Definition (SSA Fragment).** A Stroscot PB is in the *SSA fragment* if and only if, for every sequent node $\Gamma' \vdash \Delta'$ appearing in the *interior* of the PB's derivation tree (i.e., at any non-root node), the left context $\Gamma'$ contains no occurrence of any $\mathbb{J}^-$ type or $\bang^-$ type.

In other words: negative types (continuations) may only appear in $\Delta$ (the right/output context) or at the PB root. They are never introduced into $\Gamma$ by any internal rule step.

This restriction has a direct operational reading: it is precisely the condition that no instruction inside a block takes a continuation as a data argument. This is exactly what SSA enforces syntactically — block labels are not values.

**Theorem (Stroscot–Kelsey).** There is a bijection between SSA-fragment Stroscot PB systems with Use/Def graph $G$ and SSA CFGs with control-flow graph $G$, given by the Kelsey CPS↔SSA translation lifted through Curry-Howard:

- Each PB root $\Gamma \vdash \Delta$ $\leftrightarrow$ block with $\Phi$-node parameters $\Gamma$ and successor slots $\Delta$
- Each $\mathbb{J}^+_R$ node $\leftrightarrow$ SSA instruction producing a data value (`add`, `load`, etc.)
- Each $\mathbb{J}^+_L$ node $\leftrightarrow$ SSA use/deconstruction of a data value
- Each $\mathbb{J}^-_{L,i}$ at a leaf $\leftrightarrow$ the block terminator (`jmp` or `br`) dispatching to successor $i$

The SSA restriction (no $\mathbb{J}^-$ in $\Gamma$) guarantees no step introduces a continuation type into the left context, exactly mirroring SSA's syntactic prohibition on block-label values.

Programs that go beyond the SSA fragment — by placing a $\mathbb{J}^-$ type in $\Gamma$ (e.g., `callcc`, exception handlers in data structures, higher-order functions accepting continuation arguments) — remain valid in full Stroscot but exit the SSA-equivalent region into general first-class continuation territory.

### Construct Correspondence

| Stroscot IR Construct | SSA/CFG Construct | Notes |
|---|---|---|
| Single PB (minus cyclic Use leaves) | Basic Block | Interior nodes topologically sort into instructions |
| Root type of a PB | Block arguments / $\Phi$-nodes | Shared values become $\Phi$-nodes |
| Internal derivation steps (Logical Op-Nodes, cuts) | SSA instructions (`add`, `load`, etc.) | Single-assignment mirrors linear use of propositions |
| Inter-Block Continuation leaf → another PB | Unconditional `jmp` | Passed values feed the target's $\Phi$-nodes |
| $\mathbb{J}^-$ Conditional Op-Node at a leaf | Conditional `br cond, T, F` | The two $\mathbb{J}^-$ cases become the two target blocks |
| Cyclic `Use` back to own `Def` root | Loop back-edge + header `jmp` | Updated $\Gamma$ values feed the loop header's $\Phi$-nodes |
| Mutual `Use`/`Def` cross-references | Mutually recursive blocks | Identical structure to single back-edges |

### Relationship to Other IRs

| IR | Relationship to Stroscot |
|---|---|
| **SSA CFG** | Exact output target via Stroscot–Kelsey Correspondence (SSA fragment) |
| **Sea of Nodes** | Intra-PB DAG is an acyclic SoN; the Inter-Block Continuation graph is the CFG skeleton |
| **CPS (Flat)** | Isomorphic via Kelsey for the SSA fragment; each PB is a CPS continuation |
| **Sequent Core (GHC)** | Theoretical ancestor; uses intuitionistic (single-conclusion) sequents; Stroscot extends to full classical two-sided $\mathbb{J}^\pm$ connectives with explicit cyclic Use/Def rules |
| **RVSDG** | Similar region-based structure; Stroscot's PBs correspond to RVSDG regions |
| **MLIR** | Stroscot block-arguments map directly to MLIR's block argument $\Phi$ convention |
| **µMALL** | Avoided; type-level fixed points introduce hyperarithmetical proof-search complexity; recursion is handled at the term level via Use/Def |

---

## X. Key Design Decisions

1. **AOT context, not JIT.** Retaining full data, control, effect, and schedule edge information in the PB graph adds proportional but small overhead relative to the IR size already held in memory during whole-program LTO. For a project at the scale of Chromium, Full LTO already requires 15–18 GB of RAM at link time; the additional edge fields per PB node represent a modest constant-factor increase (~10–20%) within the same memory tier, and do not change the feasibility tradeoff of LTO itself. This overhead is therefore justified by the deeper inter-procedural optimization it enables.

2. **Cyclic Use/Def, not type-level fixed points or guarded modalities.** Adding $\mu/\nu$ operators to the type system (following Baelde's $\mu$MALL) forces recursion into the type structure; provability in $\mu$MALL is complete for the hyperarithmetical hierarchy, making proof search intractable. Nakano's "Later" modality restores strong normalization — exactly the wrong property for a Turing-complete compiler IR. Keeping recursion at the term level via Use/Def keeps the type system clean and proof search decidable for the base fragment.

3. **First-class continuations via heap allocation.** By heap-allocating closures, continuations become first-class without requiring stack-discipline restrictions. Stack-allocated activation records enforce a last-in-first-out discipline that invalidates any captured continuation once the frame is popped; heap allocation removes this restriction. This enables full Data-Control Symmetry at runtime, extending beyond the SSA fragment into general first-class continuation territory.

4. **Structural Fold Optimization as the optimizer.** Because all standard optimizations (CSE, DCE, constant folding, algebraic identities) are instances of the Linear Logic cut-elimination rewrite rules, the intra-PB optimizer has a confluent (Church-Rosser) rewrite system rather than a collection of ad hoc pattern matchers. Confluence holds within individual acyclic PBs; cross-PB optimization requires standard loop termination guards as in any compiler.
