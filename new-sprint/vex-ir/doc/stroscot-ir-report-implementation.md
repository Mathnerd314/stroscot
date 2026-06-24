# Stroscot IR: Implementation Working Notes
### Effect Representation, Edge Structure, and the Program ADT
---
## I. Motivation and Scope
*These notes are a companion to the design record*. The design record establishes the theoretical foundation of the Stroscot IR: a classical two-sided Linear Logic proof calculus, using J⁺/J⁻ Opcodes, Basic Blocks (BBs), and a Use/Def graph of Block Edges (BEs). It proves the Stroscot–Kelsey Correspondence for the SSA fragment and motivates Data-Control Symmetry as the core design principle.

Sea of Nodes, as the motivating comparison, has three explicit edge kinds: data edges, control edges, and effect edges. The design record already handles two of these:

- **Data edges** — explicit J⁺/J⁻ typed wires throughout
- **Control edges** — already unified with data: the linearity of the proof means a value's existence is simultaneously a proof that execution reached its producer; there is no separate control token because linear logic's structure makes one unnecessary

What is missing is **effect edges**: the mechanism that constrains the ordering of operations with side effects (memory writes, I/O, syscalls). These notes work out how to represent them and support a relaxed memory model.

***
## II. Why Effect Tokens Are Rejected
The obvious approach — introduce a `Mem` token that every effectful node consumes and produces — was considered and rejected. The reasons form a coherent family:

**The `exit` problem.** `exit()` is a function that terminates the program. It consumes the memory state and produces nothing — it never returns. A token-passing scheme requires `exit` to have type `(ExitCode, Mem) ⊢ Mem'`, but there is no `Mem'` to produce. The only options are to introduce a special `Bottom` type as an escape hatch, or to use `!⁻` weakening on the output. Either way, the linear discipline has been violated for a fundamental, ubiquitous operation. The same hole reappears for `longjmp`, `throw`, infinite loops, `fork`, and any coroutine `yield` — i.e., for essentially all interesting control flow. GHC uses the linear token abstraction and reports that it causes issues - all passes must be written to preserve linearity, and this invariant is not easy to maintain precisely because it does not map to actual machine semantics. Fundamentally, the linear token is a square peg in a round hole for a ubiquitous operation.

**Over-serialization.** A single token chain imposes a total order on all effects. Recovering parallelism then requires alias analysis as a separate bolt-on pass to prove that some effects can be reordered — fighting back against the over-serialization the token introduced in the first place.

**The fundamental category error.** An effect token conflates two distinct things: *sequencing* (this effect happens before that one) and *reachability* (this code executes at all). In the Stroscot IR, reachability is already handled intrinsically by the linearity of the proof structure — a value's existence is a proof of reachability. Sequencing is a separate concern and deserves a separate representation.

***
## III. The Program ADT: Effects as First-Class Data
The correct representation is the **effect tree** pattern. The top-level type of a complete program is not an integer or a machine state — it is a value of type `Program`, which is a large **sum type**:

```haskell
data Program
  = DirtyCall FnPtr [Arg] (Result -> Program)  -- opaque C call, callback for continuation
  | Print     String Program                    -- write to stdout, then continue
  | Read      (String -> Program)               -- read from stdin, feed to callback
  | WriteFile Path   String Program
  | Exit                                        -- terminal: NO callback field
```

Each constructor represents one effectful operation. The callback field — the `Program`-typed argument — is *what to do next*. `Exit` has no callback because there is no "next." This is not a special case requiring an escape hatch; it is simply a leaf constructor of the ADT, with zero continuation fields. The linear logic proof tree terminates cleanly at an `Exit` node; no linearity is violated because there are no resources left to discharge. It is not a free monad because there is no `lift` or `Pure` operation; the literature on Coq interaction trees is much closer to what we are using.

The top-level sequent of the entire program is `⊢ Program`. The IR builds up a `Program` value through the rule tree; some external runtime interpreter executes it. From the IR's perspective, `Program` is an opaque linear type that flows upward to the root.

**Key consequence:** `Program` is not a special token or a privileged edge kind. It is a normal proposition type. It appears on normal typed wires, subject to all the same linear logic rules as any other type. The effect tree structure — the callback chain — is carried by the wire topology of the proof tree, not by any special field or edge annotation.

***
## IV. The Two-Level Distinction: Effect ADT vs. IR Rule
For each effectful operation there are two distinct levels, which must not be conflated:

**Level 1 — The ADT constructor (semantic/denotational).** `DirtyCall` is a constructor of the `Program` type. It is a value. It *describes* what to do; it does not execute it. Execution happens only when the runtime interpreter pattern-matches the `Program` value at the top level.

**Level 2 — The IR rule (syntactic/structural).** `DirtyCall` as an IR rule is a J⁺ right-rule that constructs this ADT entry from its components. It is a shorthand — a named compound rule in the multi-I/O category of opcodes — for the atomic-level derivation that builds the `DirtyCall` constructor:

```
J⁺_R(DirtyCall) : (fn: FnPtr, args: [Arg], k: Result → Program) ⊢ Program
```

$$
\mathbb{J}^+_{R}(\text{DirtyCall}) : (\text{fn}: \text{FnPtr},\ \text{args}: \overrightarrow{A},\ k: \text{Result} \to \text{Program}) \vdash \text{Program}
$$


This is a perfectly ordinary J⁺ constructor with fixed input slots and one output slot of type `Program`. The linearity of the `Program` output wire enforces that the callback is called exactly once — no special machinery needed.

The J⁺ left-rule (eliminator) for `Program` in the IR is a full pattern match over the `Program` ADT — `case DirtyCall: execute; case Print: write; ...` — it is not opcode-specific and is mainly useful for writing a virtual runtime interpreter. Outside this use case, inside the IR, `Program` values only flow upward; they are never destructed.

***
## V. Node and Wire Data Structures
### Typed Wires (Data and Control)
When a sequent calculus derivation is written on paper, entries in the context Γ or Δ are identified only by their type. There is no indication of which `A` at one rule node is the *same* `A` at the parent rule — especially when multiple propositions have the same type. Reconstructing "which value flows where" requires additional assumptions, such as that contexts are ordered lists and the exchange rule is used whenever an argument is not in the right place. However, using the exchange rule is similar to sorting a list and can require O(n log n) operations in the worst case. On paper, a typical convention is to assume contexts are multisets organized by type, which often avoids the exchange rule but still fails to distinguish duplicated propositions. The robust solution is **wire identities**: an explicit per-occurrence identifier for each wire occurrence, connecting from parent to child. However, connecting each intermediate step is purely syntactic and is actually unnecessary - we can store the wire identity as a direct pointer from introduction site to elimination site, skipping all intermediate passthrough steps. 

A wire connects one specific output slot of one node to one specific input slot of another. It carries the proposition type on that connection. Because data and control are unified in the linear logic structure, there is no separate "control edge" kind — the proof that a value exists is simultaneously the proof that execution reached its producer.

```haskell
data SlotRef = SlotRef
  { node  :: NodeId
  , slot  :: SlotIndex
  , side  :: Side        -- L (left) | R (right) -- be careful not to confuse the left/right of the sequent L ⊢ R with the input/output of parent and child IR rules
  }

data Wire = Wire
  { from    :: SlotRef   -- def site: output slot of producer
  , to      :: SlotRef   -- use site: input slot of consumer
  , propTy  :: PropType  -- the Linear Logic type carried on this wire
  }
```

A wire is **typed by the proposition it carries**. Data wires carry J⁺ types (integers, structs, enums). Continuation wires carry J⁻ types (function types, continuations). Effect wires carry the `Program` type (technically a form of data). All three wire types are the same data structure — the distinction is in `propTy`, not in any separate edge kind.

### Structural Wires

The syntactic parent-child relationship between rule applications in the rule tree gives a canonical notion of scope: the nearest common ancestor of two nodes is their shared rule scope, governing dominance, CSE validity, and BB boundary enforcement. It is an important relation - Girard constructed a proof net formalism, but he observed that proof nets lost information, in that multiple derivation trees can correspond to a single proof net. Proof nets also are one-sided and do not lend themselves well to representing a two-sided logic. But in many ways the typed wire representation is simply an adaptation of proof nets, and structural wires are exactly the additional information needed to maintain a closer relationship to textual derivation trees. But Girard's concept of boxes is still relevant in that a promotion rule does indeed have all its inputs and outputs as "active" - it is a boundary and use-def edges cannot pass through.

### Nodes
Each node in the rule tree stores its opcode, its slot types, and bidirectional wire indices:

```haskell
data Node = Node
  { nodeId      :: NodeId
  , opcode      :: Opcode         -- the compound rule (derived rule / multi-I/O operation)
  , propTypes   :: [PropType]     -- type of each slot: left slots then right slots
  , keySlot  :: SlotIndex      -- the key slot slot for this rule
  , inputs      :: [Wire]         -- use→def: primary, always present
  , outputs     :: [Wire]         -- def→use: derived index, kept in sync
  , parent      :: Maybe NodeId   -- rule tree parent (Nothing = BB root)
  , children    :: [NodeId]       -- rule tree children
  }
```

**Directionality.** `inputs` (use→def) is the primary representation; `outputs` (def→use) is maintained in sync. Any wire rewrite must update both atomically. Storing both directions gives O(1) lookup for "what feeds this slot?" and "where does this slot go?" — both are needed by different passes (scheduling needs forward; DCE needs backward).

**The `parent` field.** This is the rule tree annotation. When wires are rewired during instruction reordering, `parent` pointers update as a consequence — although the flow of the `Program` proposition use-def changes are perhaps the more obvious part of the edit, the nesting of effectful sub-derivations is semantically significant and so the rule tree structure must also change when the effect chain changes.

**Slot identity and passthrough.** Most sequent calculus rules pass propositions through unchanged as side formulas. Rather than materializing every passthrough step as a node, the annotation records for each wire occurrence a direct pointer from its introduction site (J⁺ right-rule) to its elimination site (J⁻ left-rule or sequent boundary), skipping all intermediate structural shuffling. This is the "follow the edges and remove intermediate passthrough" construction — a def-use chain lifted to the rule-tree level. The rule tree is still present and authoritative; the annotation is an efficiency index over it.

***
## VI. Opcodes as Derived Rules (Polycategory Morphisms)
Standard presentations of sequent calculus work at the level of atomic rules, each touching exactly one key slot. Real IR opcodes — `add`, `load`, `DirtyCall`, a loop header — are **compound rules**: named abbreviations for fixed rule subtrees of atomic rules. The formal structure is that of a **multi-I/O operation**: a generalized function with multiple inputs *and* multiple outputs, matching the `Γ ⊢ Δ` sequent structure. Most IRs use natural deduction (single output), which loses the ability to express the full two-sided classical sequent calculus. The multi-I/O framing is what makes Data-Control Symmetry possible at the opcode level.

```haskell
data OpcodeSignature = OpcodeSignature
  { inputs     :: [PropType]   -- input context: wires going in
  , outputs    :: [PropType]   -- output context: wires going out
  , keySlots   :: [SlotIndex]  -- which slots are the key slots of this rule
  , witness    :: DerivTree    -- atomic-level expansion (may be abstract/opaque)
  }
```

For pure arithmetic opcodes (`add`, `mul`, etc.), the `witness` is abstract — the IR never needs to expand it. For compound opcodes (loop headers, pattern matches, `DirtyCall`), the `witness` may be inspectable for optimization purposes. Cut elimination (the optimizer) works by finding a cut between a J⁺ constructor and its matching J⁻ eliminator and recursively reducing using the sub-derivation structure — the `witness` is what makes this local and confluent within a single BB.

The `keySlot` field on each node records which slots are the **key slots** of the compound rule — the ones that change type. All other slots are side formulas that pass through unchanged. This classification is what makes the passthrough-elimination annotation efficient: only non-passthrough slots need wire entries in the def-use index.

***
## VII. Memory Model Relations as Program wire metadata
The `Program`-typed wires — the effect tree callback spine — carry the intra-thread effect ordering. The semantics of the use-def relations is exactly what we need in order to follow the evolution of a program. For reasoning about concurrent correctness and instruction scheduling across threads, additional annotation is needed. The **relaxed memory model relations** from the herd-tools `.cat` model language provide an excellent resource for x86-TSO, ARMv8, RISC-V RVWMO, and similar architectures. The program typed wires represent the `po` relation in herd-tools. But we can add additional wires on `Program`-typed typed wires to represent the rest of the relaxed memory model relations as a separate graph overlay. This layer is **not** carried on typed wires. It is a separate graph overlay on the set of effectful nodes, recording the primitive relations of the herd-tools `.cat` memory model formalism

```haskell
data MemOrderEdge = MemOrderEdge
  { rel        :: MemOrder
  , from       :: SlotRef
  , to         :: SlotRef
  , pointerClass :: PointerClass   -- which pointer class partition this relation inhabits
  }

data MemOrder
  = ReadFrom        -- rf: this read observed this write
  | CoherenceOrder  -- co: modification order between two writes to same location
  | SyncsWith       -- sw: release/acquire pairing across threads
  -- Derived relations (not stored, computed by graph query):
  -- fr  = rf⁻¹ ; co          (from-reads)
  -- hb  = tc(sw ∪ relevant po)  (happens-before)
```

**Memory model axiom checking** is a validity predicate over the `MemOrderEdge` graph: an execution is valid iff the declared relation edges satisfy the target model's `.cat` constraints (e.g., `acyclic(hb)`, `acyclic(co;rf;fr)`). Different target models are different predicate instances over the same `MemOrderEdge` structure, parameterized by an `AtomicOrdering` annotation on each effectful node. We may be stymied in our analysis by conditional branches or other data/control flow dependencies, but when we have a simple chain of `Program`-typed use-def wires (corresponding to an SSA basic block), the wire formalism gives us exactly what we need.

**Pointer classes as types.** Following Cliff Click's type-based alias analysis, the pointer class partition is expressed as a **type distinction** on pointer types, not a separate analysis table. Two accesses with pointer types in different pointer classes `C1 ≠ C2` cannot have `rf`, `co`, or `fr` edges between them — the type system prohibits such edges from being well-formed. The partition is enforced structurally rather than checked post-hoc.

**The two-layer architecture.** The full effect/ordering structure is:

| Layer | Representation | What it answers |
|---|---|---|
| **Layer 1: Program order** | `Program`-typed wires in the normal wire structure | Intra-thread sequencing: what executes after what within one thread |
| **Layer 2: Cross-thread relations** | `MemOrderEdge` overlay | Cross-thread observability: what is visible to other threads, under which memory model |

Layer 1 is structural — it is the effect tree callback spine materialized as wire topology. Layer 2 is an annotation — it records the specific `rf`/`co`/`sw` assignments that make the execution valid under the target memory model axioms. The `MemOrderEdge` overlay adds, separately and independently:
- A `ReadFrom` edge from whatever write this call's memory reads observed
- A `CoherenceOrder` edge ordering this call's writes relative to other writes on the same pointer class
- A `SyncsWith` edge if the call includes acquire/release fence semantics

None of these appear in the node's wire structure. They are queried from the `memOrderEdges` table when needed (by the memory model checker or the instruction scheduler). The separation is clean: **most passes edit only Layer 1 (typed wires carrying `Program` type)**. The instruction scheduler and other memory-aware passes read Layer 2 for these `Program`-typed typed wires to check that the reordering does not violate the memory model axioms. Passes that reason about pure data flow (CSE, constant folding, DCE) touch neither layer's effect structures — they operate on typed wires carrying non-`Program` types.

It is not yet specified when and how `MemOrderEdge` entries are created. They must be inserted by the lowering pass that converts source-level concurrent operations (C11 atomics, Rust `Arc`, etc.) into IR nodes. The policy for which `MemOrder` edges are inserted, and how they interact with pointer class typing, needs a complete specification, which will be in a separate document.

***
## VIII. The DirtyCall Node: All Edge Kinds Together
The `DirtyCall` node (a side-effecting call to an FFI function) is the most complex in the IR because it participates in all the structures described above simultaneously. Here is its complete profile:
**As an ADT constructor:**

```haskell
DirtyCall :: FnPtr -> [Arg] -> (Result -> Program) -> Program
```

**As a IR rule (J⁺ right-rule):**

```
J⁺_R(DirtyCall) : (fn: FnPtr, arg1: A, arg2: B, k: Result → Program) ⊢ Program
```

Slot inventory:
- Left slot 0: `fn : FnPtr` — the function pointer, data wire in
- Left slot 1: `arg1 : A` — first argument, data wire in
- Left slot 2: `arg2 : B` — second argument, data wire in  
- Left slot 3: `k : Result → Program` — the callback (J⁻ type), data wire in
- Right slot 0: `Program` — the constructed ADT value, data wire out (linear)

The `Program` output wire carries the intra-thread effect ordering. Its consumer is the *calling* effectful node (the one that builds the `Program` value that wraps this `DirtyCall`). Because `Program` is linear, exactly one consumer exists, either the top level or a calling function — the wire is the *incoming* program-order edge. No separate "effect edge" storage is needed; it is a normal typed wire.

The callback slot L3 carries a `Result → Program` value — a J⁻ type. The `Result` is what the C function returns; the `Program` is the continuation of the program after the call returns. This is the "what to do next" field of the ADT constructor, materialized as a wire input to the node. If we trace where the `Result → Program` comes from, we will find the actual `Program` value that specifies what to do next, but the linear logic formalism allows arbitrarily complex first-class control flow, so it may be difficult to actually find the jump target.

---
## IX. Instruction Reordering: Correctness and Efficiency
Consider swapping two independent memory reads in the same BB on the same pointer class C1:

```
// Before reorder: readA then readB
readA = Load(ptrA, k_A : Program)   -- pointer class C1, produces result_A and Program_mid
readB = Load(ptrB, k_B : Program)   -- pointer class C2 (ptrA ≠ ptrB by type or other analysis), uses Program_mid, produces Program_out

// After reorder: readB then readA
readB = Load(ptrB, k_B : Program)   -- now consumes Program_in
readA = Load(ptrA, k_A : Program)   -- now consumes Program_mid'
```

Currently, `readA`'s `Program` output feeds `readB`'s callback slot — they are sequenced. After the reorder, `readB`'s `Program` output feeds `readA`'s callback slot.

**Step 1 — Check Layer 2 (`MemOrderEdge`).**  
Two plain loads cannot have a `CoherenceOrder` edge between them (co is write-write). They cannot have a `ReadFrom` edge between them (rf goes write→read). They have no `SyncsWith` unless annotated with acquire/release. On pointer class C1, there are no `MemOrderEdge` constraints between them. The reorder is memory-model valid. ✓

**Step 2 — Perform the wire rewrite.**  
Swap the `Program`-typed wires: rewire `readA`'s right slot 0 to feed `k_B`, and `readB`'s right slot 0 to feed `k_A`. No other updates to `inputs` and `outputs` are needed. Update `parent` pointers on affected nodes. Edit touches: 2 nodes' `inputs`, 2 nodes' `outputs`, `parent` pointers on the two nodes and their immediate derivation-tree neighbors. Can be done in-place, but with concurrency will need synchronization.

**Step 3 — Verify linearity is preserved.**  
After the rewrite, `Program` is still consumed exactly once (by the next node in the new chain). No contraction or weakening of `Program` occurred. The proof tree is still valid. ✓

The rewrite is local and O(degree) in the number of wires at the affected nodes. The `MemOrderEdge` table is read-only during this operation. No BB boundary or BE edge is touched. No global scan is required. ✓

***
## X. Relationship to Sea of Nodes
Sea of Nodes has three explicit edge kinds: data, control, and effect. The Stroscot IR has a different factoring:

| Concern | Sea of Nodes | Stroscot IR |
|---|---|---|
| **Data flow** | Data edge | Typed wire on non-`Program` J⁺/J⁻ type |
| **Control reachability** | Control edge (ctrl token) | Intrinsic to proof linearity — no separate structure |
| **Intra-thread effect order** | Effect edge (single chain per pointer class) | `Program`-typed typed wire (effect tree callback) |
| **Cross-thread observability** | Not represented | `MemOrderEdge` overlay analysis (rf, co, sw) |
| **Alias partition** | Separate alias analysis | Combined with `MemOrderEdge` overlay analysis |
| **Non-returning calls (`exit`)** | Requires `Bottom` escape hatch | Leaf ADT constructor with no callback field |
| **Commutativity of effects** | Proved by alias analysis, implicit | Proved by absence of `MemOrderEdge` between nodes |
| **Edge count per effectful node** | 3 (data in, ctrl in, effect in/out) | Variable: one `Program` wire + one per data arg; `MemOrderEdge` separate |

The key difference is that Stroscot's control representation is strictly stronger than Sea of Nodes': control is not a bolted-on token but a consequence of using linear logic. The effect representation is somewhat bolted on but also strictly more expressive: the `Program`-wire / effect tree approach handles non-returning calls naturally, and the `MemOrderEdge` overlay represents the full partial-order structure of relaxed memory models rather than a single linear chain.