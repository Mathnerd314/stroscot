# Design of a VEX-like Sea-of-Nodes IR with Precise Interrupt Semantics

## Overview

This document specifies an intermediate representation (IR) and code generation backend for a Dynamic Binary Translator (DBT) in the spirit of Valgrind's VEX. The IR is redesigned around a Sea-of-Nodes (SoN) graph with strict side-effect separation and explicit safepoints for precise interrupts, deoptimization, and signal-driven bailouts. The code generation backend couples this IR with a combinatorial optimizer powered by Google OR-Tools CP-SAT, targeting mathematically optimal machine code for hot traces.

The design is informed by VEX IR (Valgrind/angr), LLVM Machine IR, HotSpot C2, Unison, and the academic literature on combined register allocation and instruction scheduling. The IR is implemented in Python, prioritizing clarity and flexibility over raw compilation speed. The design assumes the host CPU provides precise architectural state at interrupts via its own reorder buffer; the IR's job is to make guest state *observable and reconstructible* at selected points in the optimized native code.

## Code Generator Design Goals

1. **Mathematically optimal code generation.** Instruction selection, register allocation, and instruction scheduling are solved as a single unified combinatorial optimization problem.
2. **Separation of concerns.** Profiling, speculative optimization, and CFG mutation belong in the SoN middle-end. The code generator receives a frozen CFG and solves a purely algebraic problem.
3. **Tiered execution.** The same infrastructure operates across all compilation tiers, parameterized by time budget and profile confidence.
4. **Fast path.** Low-overhead code emission for cold paths where compile time is at a premium.

## Compilation Pipeline

The pipeline proceeds as follows. Lifting and middle-end transformation are not the primary focus of this document but are included for completeness.

```
Binary Guest Code
      │
      ▼
 [Lift to SoN IR]         ← VEX-style microcode lifting per guest instruction (not current focus)
      │
      ▼
 [Middle End]             ← rewriting, speculation, constant folding, GVN, CSE, etc. (not current focus)
      │
      ▼
 [Global Code Motion]     ← assign floating nodes to blocks via dominator tree
      │
      ▼
 [LSSA Lowering]          ← convert SoN to linear block-argument CFG; insert copies and alternatives
      │
      ▼
 [Trace Formation]        ← profile-guided identification of hot linear path through CFG
      │
      ▼
 [Combinatorial Solve]    ← unified instruction selection + register allocation + scheduling
      │
      ▼
 Machine Code
```

All CFG-mutating optimizations—guard insertion, type specialization, indirect-jump elimination, alignment promotion—happen in the SoN middle-end before the code generator is invoked. The code generator is forbidden from changing the CFG.

### Middle-End Sketch

Key middle-end transformations:

- **Devirtualization:** An indirect `Jump(target)` with a hot observed target becomes `If(target == 0x1234) { direct_call } else { fallback_indirect }`.
- **Alignment Promotion:** A `Load_V256` node whose pointer is observed aligned becomes `AssertAligned(ptr, 32); Load_Aligned_V256(ptr)`, where the `Assert` becomes a cheap runtime guard.
- **Alias Disambiguation:** Two memory nodes whose addresses never overlapped at runtime get their effect-chain dependency removed, with an `AliasGuard(ptrA, ptrB)` inserted on the cold path.

After these rewrites, GVN and constant folding propagate the new assumptions through the graph naturally. This sketch will likely grow significantly more complex in practice.

---

---

## Part 3: Optimization Passes

### Dominance and Global Code Motion (GCM)

GCM schedules floating nodes within dominance constraints. The implementation uses:

- **Cooper–Harvey–Kennedy** simple dominance algorithm for dominators and dominance frontiers over the CFG.
- **Cliff Click's GCM** to assign each node an *earliest* and *latest* legal block, then choose a placement—usually the least-deep block in that range—for loop-invariant hoisting and global code motion.

GCM subsumes LICM and many other code-motion optimizations globally. Note that GCM is not fully deterministic: Click's original heuristics use the late block adjusted upward to escape loop nests, but other choices can sometimes produce better code, and the trace-informed placement described in §4.4 provides a more principled alternative. The GCM pass can also be expensive, [Ben Titzer](https://www.youtube.com/watch?v=Vu372dnk2Ak) reports 20-30% of time spent in scheduling.

### Constant Folding and Propagation

For any node whose inputs are all constants, the IR performs local constant folding by evaluating the operation in Python and replacing the node with a new constant (or reusing an existing one via interning). Constant propagation arises naturally from SSA and CSE: once a node is assigned a constant value, all its uses see that constant through the graph.

### Common Subexpression Elimination (CSE)

A global CSE pass maintains a hash-consing table keyed by `(opcode, type, input-node-ids, imm-params)`. When creating a new pure node, the builder first checks the table; if an equivalent node exists, it returns that node instead of creating a duplicate. This is especially effective for redundant address calculations and repeated flag computations.

### Dead Code Elimination (DCE)

Because the IR tracks `uses` for each node, DCE is straightforward: a node with no uses and no side effects is dead. Recursively deleting dead nodes—or relying on Python's GC after severing references—suffices. Safepoints and the framestates they reference are never considered dead by DCE, even if not used by subsequent computation (they may be removed by a separate safepoint-reduction pass).

### Copy Propagation

Copy-like nodes (e.g., `MOVE`, type-compatible `BITCAST`) are simplified by rewiring uses to the original source node: for `MOVE x → y`, replace all uses of `y` with `x` and eliminate the `MOVE`. This reduces graph noise and improves the effectiveness of CSE and DCE.

---

## Part 4: Combinatorial Code Generation

### The Trifecta

The code generator solves three classically interleaved compiler problems as one unified constraint model:

| Problem | Variables | Key Constraints |
|---|---|---|
| Instruction Selection | Boolean `chose_opcode[i][k]` per instruction | Exactly one opcode per instruction; latency follows opcode choice |
| Register Allocation | Integer `reg[v]` per live value | No two overlapping live ranges in the same physical register |
| Instruction Scheduling | Integer `cycle[i]` per instruction | Data dependencies respected; port throughput limits enforced |

### Solver: OR-Tools CP-SAT

The solver is Google OR-Tools CP-SAT rather than Gecode (used in Unison). CP-SAT is a Lazy Clause Generation (LCG) solver combining CDCL SAT, LP relaxation, and constraint propagation. The critical advantage over Gecode is **Conflict-Driven Clause Learning (CDCL)**: when CP-SAT encounters a dead end, it generates a learned clause and never revisits that combination of variables. Gecode, as a pure finite-domain CP solver, lacks this and frequently thrashes by repeating identical failures.

CP-SAT primitives map directly to the compiler domain:

- `NewOptionalIntervalVar` + boolean presence literal: models optional instructions (spills, copies) that exist only if needed.
- `AddNoOverlap`: enforces that no two live ranges occupy the same physical register at the same cycle.
- `OnlyEnforceIf`: ties latency constraints to opcode-choice booleans without auxiliary variables.
- `AddMaxEquality`: computes the throughput bottleneck as the maximum over port pressures, issue width, and critical-path depth.

### Objective Function: Algebraic uiCA

The uiCA throughput model is fully encodable as a CP-SAT objective. Throughput is the maximum of four independent bottleneck terms:

\[ \text{Minimize} \;\; \max(T_\text{issue},\; T_\text{frontend},\; T_\text{ports},\; T_\text{precedence}) \]

- **\(T_\text{issue}\):** Total uops divided by issue width; a linear sum over opcode-choice variables.
- **\(T_\text{frontend}\):** A linear sum of instruction byte lengths, affected by DSB vs. legacy decoder.
- **\(T_\text{ports}\):** For each execution port, the sum of uops dispatched to it. `AddMaxEquality` computes the bottleneck port directly. When uops have flexible port assignment, CP-SAT's internal LP relaxation finds the optimal fractional distribution automatically.
- **\(T_\text{precedence}\):** The critical-path length through the data-dependency DAG, implicit in the `cycle` variables. The transitive closure of all dependencies is pre-computed in Python (BFS/Floyd-Warshall) before handing the problem to CP-SAT, providing tight lower bounds that dramatically reduce search depth.

### Register Allocation Encoding

Physical registers of the same class are interchangeable, creating combinatorial symmetry. Symmetry is broken by forcing a canonical ordering: the first allocated temporary is assigned to the lowest-indexed available register. Calling-convention constraints enter as hard boundaries:

- `IndepClobbered` (scratch): kill all live ranges at the call cycle.
- `DepClobbered` (argument): force the value into the specific physical register by the call cycle.
- `IndepPreserved` (callee-saved): freely extend live ranges across calls in these registers.
- `DepPreserved` (read-only): pass values in these registers without inserting spills.

Beyond this, uiCA's per-port micro-op data further constrains instruction selection choices, because the solver knows exactly which opcodes are interchangeable from a throughput perspective.

### Warm-Starting

Before invoking CP-SAT, a fast linear-scan register allocator and list scheduler generates a valid but unoptimized schedule, fed to the solver via `model.AddHint()`. Because CP-SAT is an anytime solver, this instantly establishes an upper bound on the objective and restricts the search to schedules that improve on it—mirroring how Unison bootstraps from LLVM's greedy allocator output.

### Time Budget and Anytime Solving

CP-SAT is parameterized by a time limit:

```python
solver.parameters.max_time_in_seconds = budget
solver.parameters.num_search_workers = 8  # parallel portfolio
```

When the timer fires, the solver returns the best schedule found so far. This makes the system well-behaved across all compilation tiers:

| Tier | Trace Size | Profile Source | CP-SAT Budget |
|---|---|---|---|
| Tier 1 (Fast JIT) | 1 block | ML hallucination | 0 ms (emit hint directly) |
| Tier 2 (Warm JIT) | 3–10 blocks | Software counters | 50–200 ms |
| Tier 3 (Hot optimizer) | 50–500 instructions | Real runtime data | 500 ms–5 s |
| AOT / Microbenchmarks | Function-wide traces | Hardware PEBS/LBR | Unlimited |

A later exploration will explore the speedup with different parameters / solver budgets to characterize the tradeoffs, packaging it up as a neat Pareto frontier specification.

---

## Part 5: Global Optimization and Trace Partitioning

### The Problem with Whole-Function Optimization

In principle, the code generator should operate over whole functions—block placement, register allocation, and scheduling interact globally. However, the CP-SAT model scales exponentially with the number of free variables, making whole-function solving computationally intractable.

### Global Code Motion in the Backend

GCM pins every floating node to a specific basic block, transforming the Sea into a conventional CFG (see also §3.1 for the optimizer perspective). For each floating node:

1. **Schedule Early:** The earliest legal block is the deepest block dominating all input definitions.
2. **Schedule Late:** The latest legal block is the lowest common dominator (LCA) of all blocks consuming the node's output.

The standard heuristic uses the late block, adjusted upward to escape loop nests for loop-invariant hoisting. However, GCM is not deterministic, and the trace-based cost model below provides a more principled placement strategy.

### LSSA Lowering

After GCM, LSSA lowering:

1. Identifies every value whose definition block differs from at least one use block.
2. Adds that value as a block-argument to the jump on the relevant CFG edge and as a formal parameter to the successor block.
3. Inserts optional `Move` instructions on edges where source and destination blocks need register-assignment coordination.

The result is a flat CFG where every block has an ordered list of operations with explicit block-argument passing at all exits. LSSA lowering happens *after* GCM, which means GCM decisions affect IR structure—this is why GCM cannot be folded directly into the CP-SAT model.

### Trace-Based Partitioning

The solution to tractability is **trace-based partitioning**: profiling data identifies the hot basic block path through the CFG. The trace weights uiCA throughput terms by execution probability:

\[ \text{Minimize} \;\; \sum_{\text{block } b \in \text{trace}} P(b) \cdot \text{uiCA}(b) + \sum_{\text{exit } e} P(e) \cdot \text{Copies}(e) \]

This directly optimizes expected real performance rather than peak throughput of a single block. Trace-informed GCM assigns side computations costs and probabilities, making placement decisions more principled than naive late-block heuristics. The approach also enables **incremental GCM**: first generate code for the hot path, then treat it as a fixed boundary condition for cold side paths. The register assignment and cycle layout at trace exit points become fixed constants. If the solver wants a variable in `$r8` through the trace but a cold side-exit block expects it in `$r9`, it inserts a `Move $r9, $r8` on the side-exit edge—weighted by the side-exit's 0.01% probability so it freely compensates in exchange for benefiting the 99.99%-probability hot path.

### Sliding Window for Oversized Traces

When a trace exceeds roughly 300–500 instructions, a sliding window subdivides it. Optimize instructions 0–300, freeze 0–150, optimize 150–450 using the frozen state as fixed input, and so on. Optimization proceeds from endpoints toward the start, because callers depend on callees and not the reverse.

---

## Part 6: Profiling Architecture

### The Universal Profile Interface

The code generator accepts a `ProfileData` object at every compilation. It is agnostic to how the data was collected and treats it as ground truth. If generated code is slow, the fault is either in the solver (verifiable via the objective function value) or in the profiling data (improvable independently).

### What to Collect

Profiling requirements are derived from the solver's degrees of freedom. The code generator itself needs only branch edge execution probabilities. The remaining data informs the middle-end SoN optimizer:

| Profiling Data | Middle-End Use |
|---|---|
| Indirect jump target histograms | Devirtualization before the solver sees the code |
| Store-to-load alias observations | Alias guard insertion; unlocks instruction reordering |
| Pointer alignment histograms | Instruction selection between aligned/unaligned SIMD opcodes |
| Integer value range histograms | Instruction width selection (64-bit vs. 32-bit vs. 8-bit) |

### Profiling Tiers

- **Tier 1 (ML Hallucination):** A lightweight feedforward network or decision tree trained offline on static IR features predicts the full `ProfileData` struct. Classic static prediction rules (backward branches are usually taken, forward branches past error handlers are not) serve as fallbacks.
- **Tier 2 (Software Counters):** Tier 1 code injects branch counters and polymorphic inline caches (PICs) at block entries and branch sites. Once a counter crosses a threshold (~5,000 executions), the runtime triggers background Tier 3 compilation and removes the counters.
- **Tier 3+ (Hardware Counters):** Intel PEBS, Last Branch Records (LBR), or ARM CoreSight provide low-overhead control-flow profiling. Hardware counters are excellent for branch probabilities but cannot observe value ranges or alignment; software PICs supplement those specific cases.

