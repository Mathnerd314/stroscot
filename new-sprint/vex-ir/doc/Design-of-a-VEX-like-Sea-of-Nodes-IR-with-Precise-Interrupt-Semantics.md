# Design of a VEX-like Sea-of-Nodes IR with Precise Interrupt Semantics

## Overview

This document specifies an intermediate representation (IR) and code generation backend for a Dynamic Binary Translator (DBT) in the spirit of Valgrind's VEX. The IR is redesigned around a Sea-of-Nodes (SoN) graph with strict side-effect separation and explicit safepoints for precise interrupts, deoptimization, and signal-driven bailouts. The code generation backend couples this IR with a combinatorial optimizer powered by Google OR-Tools CP-SAT, targeting mathematically optimal machine code for hot traces.

The design is informed by VEX IR (Valgrind/angr), LLVM Machine IR, HotSpot C2, Unison, and the academic literature on combined register allocation and instruction scheduling. The IR is implemented in Python, prioritizing clarity and flexibility over raw compilation speed. The design assumes the host CPU provides precise architectural state at interrupts via its own reorder buffer; the IR's job is to make guest state *observable and reconstructible* at selected points in the optimized native code.

## IR Design Goals

1. **Side-effect-free expressions.** All observable machine effects (register writes, flag updates, memory stores, PC changes) are explicit statement-like nodes pinned in control/effect order. All other expressions are pure and referentially transparent.
2. **SSA-like value discipline.** Every temporary is assigned once; all uses reference that single definition. Merges at join points use block arguments rather than explicit φ-nodes, integrating cleanly with Unison's LSSA model.
3. **Typed temporaries.** Value nodes carry explicit low-level types aligned with common ISA widths.
4. **First-class precise-interrupt observability.** Guest instruction boundaries are modeled as `Safepoint` nodes attached to `Framestate` snapshots, ensuring every guest boundary is observable and recoverable.
5. **Sea-of-Nodes optimizer.** Separate data, control, and effect edges support global optimizations (constant folding/propagation, DCE, copy propagation, CSE, GCM) using techniques from HotSpot C2.
6. **AVX-512 from the start.** The x86 lifter includes AVX-512 support from day one; the IR is designed to easily accommodate new instructions and can pass through unknown ones.
7. **Pure Python core.** The IR object model, optimizer, and analysis passes are pure Python. The decoder/lifter may call a compiled library (e.g., iced-x86) but the IR itself has no mandatory C dependency.
8. **Instruction superblock tracking (IRSB).** Every guest machine instruction lifts to a group of micro-ops tracked as metadata throughout compilation. Debug printouts make use of these groups.
9. **Lazy flag computation.** Flags are output as a small pseudo-register rather than individual bits on every arithmetic instruction. Flag values are extracted via projection operations, mirroring VEX's flag-thunk design.
10. **Guest state as named memory.** Architectural registers are modeled as offsets into a typed guest state struct, accessed via `Get`/`Put` nodes. Optimizations will fold this struct away, but it keeps the lifter clean and readable.

## Compilation Pipeline

The pipeline proceeds as follows. Lifting and middle-end transformation are not the primary focus of this document but are included for completeness.

```
Binary Guest Code
      │
      ▼
 [Lift to SoN IR]         ← VEX-style microcode lifting per guest instruction
      │
      ▼
 [Middle End]             ← rewriting, speculation, constant folding, GVN, CSE, etc. (not current focus)
      │
      ▼
 [Code Generation]        ← unified instruction selection + register allocation + scheduling (not current focus)
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

## Core IR Representation

### Node Graph Fundamentals

The IR is a Sea-of-Nodes graph. **Nodes** represent operations: arithmetic/logical ops, control flow, safepoints, framestates, loads/stores, and machine-specific micro-ops. **Edges** are typed:

- **Data edges:** from definitions to uses (pure values, no side effects).
- **Control edges:** from control producers (`Start`, `If`, `Region`) to control-dependent nodes.
- **Effect edges:** from side-effecting nodes (stores, loads, I/O) to subsequent effect consumers, encoding ordering constraints for observable state.

Nodes are Python objects with explicit `inputs` and `uses` collections; Python's GC naturally handles cycles. Pure expressions have only data inputs and uses. Side-effecting nodes additionally carry control and/or effect inputs. Safepoints consume control, effect, and a framestate node.

This decouples *computation* (floating SoN) from *commit* (pinned effect chain), mirroring hardware's separation of out-of-order execution from in-order retirement.

However, [Ben Titzer](https://www.youtube.com/watch?v=Vu372dnk2Ak) reports that TurboFan spent 20-30% of its time in global code motion analysis, scheduling instructions into basic blocks. 

### SSA Discipline

The SoN IR is SSA-like:

- Every value node is a single assignment.
- All uses of a value reference that node directly through data edges.
- Merges at control flow join points use **block arguments** rather than Phi nodes (see §1.5).

Because the graph is not structured as explicit basic blocks, SSA properties arise from the graph topology rather than from textual dominance over linear code. Classic SSA-based dataflow optimizations operate directly on this representation.

### Type System

Each value-producing edge has a static type from a small, low-level lattice. The lattice is based purely on bits and bit-wdith:

- **Boolean:** `U1`
- **Integer/Floating-point:** `U8`, `U16`, `U32`, `U64`
- **Vector:** `V128`, `V256`, `V512` (aligned with SIMD ISA widths)

Types enable basic static checking of register usage but deliberately do not prevent e.g. feeding the result of an `ADD_F64` function into integer `AND`, as is often done in advanced numerical hacking. Nonetheless types still provide part of a node's canonicalization key for CSE and guide instruction selection to an extent.

Architectural register sets correspond to a struct using a **named register file**, e.g., from Ghidra processor descriptions or MLIR register set metadata.

### Pure Expression Nodes

All pure computation—arithmetic, flag derivation, address calculation, condition evaluation—is modeled as nodes with only data inputs/outputs. These nodes have no direct effect on architectural state and can be freely moved or eliminated, subject only to data dependencies.

Pure node types:

- `RdTmp(tmp)`, `Const(IRConst)`
- `Get(offset, ty)`, `GetI(descr, ix, bias)` — read from guest register state
- `Put(offset, expr)`, `PutI(descr, ix, bias, expr)` — write to guest register state
- `Binop(op, e1, e2)`, `Unop(op, e)`, `Triop(op, e1, e2, e3)`, `Qop(op, e1, e2, e3, e4)`
- `ITE(cond, iftrue, iffalse)` — if-then-else expression

Note that projection nodes for guest registers and flags are also side-effect-free; data flow ensures state is propagated correctly. See VEX IR for the full op list; the minimal arithmetic/logic subset is `ADD`, `SUB`, `AND`, `OR`, `XOR`, `NOT`, `NEG`, `INC`, `DEC`, `IMUL`, `IDIV`.

Floating-point operation nodes encode rounding mode explicitly. When lifting guest instructions that consult a dynamic rounding-mode control register (e.g., `MXCSR`), the lifter initially emits an if-then-else chain over all possible modes. SoN constant folding and GVN reduce this to a single fixed-rounding operation once the control register value is known, which is the common case.

### Control Flow Nodes

Control flow is explicit via control nodes even though the IR is not block-based by default:

- `Start`, `End`
- `If` with true/false control outputs
- `Region`/`Merge` joining multiple control predecessors
- `Return`, `Trap`, `Exit`

A conventional CFG is extracted from these control edges for dominance analysis and global code motion. Jump kinds follow VEX's enumeration:

```
JumpKind: Boring | Call | Ret | NoDecode | SigTRAP | SigSEGV | SigBUS
          SigFPE | SigILL | SigSYS | Yield | EmFail | MapFail
          InvalICache | FlushDCache | NoRedir | SigACCESS
```

### Block Arguments

At control-flow join points the IR uses **block arguments** rather than Phi nodes, following the Cranelift and MLIR approach. Branch and jump instructions carry explicit value lists targeting successor blocks; successor entry nodes declare typed formal parameters.

Key advantages:

- **Parallel-copy semantics at the branch site.** Passing arguments live at the branch makes parallel-copy semantics explicit and avoids critical-edge ambiguity.
- **Trivial LSSA lowering.** A block-argument jump `br block2(v1, v2)` directly becomes a parallel move `[param1, param2] ← [v1, v2]` at the edge; no phi-elimination pass is needed.
- **Unified calling-convention formalism.** Block arguments are structurally identical to function call arguments, enabling the same formalism to describe both intra-block transitions and FFI boundaries.

---

## Part 2: Side Effects and Safepoints

### Effectful Statement Nodes

All interactions with observable machine state other than control flow are nodes carrying effect edges:

- `Store(end, addr_expr, data_expr)` — memory write
- `Load(dst, end, cvt, addr, alt)` — memory read
- Compare-and-swap (CAS), load-linked/store-conditional (LLSC)
- `MBE(event)` — memory bus event (fence, etc.)
- `CallDirty(cee, ty, args)` — call to a helper function with side effects

VEX's `LoadG`/`StoreG` guarded memory operations are not cleanly representable in SoN. Instead, the guard is materialized as an `If` node producing two separate control paths: a hot path with an unconditional load and a cold path that skips it. This is the canonical SoN approach and makes control and data flows explicit.

### Effect Chains

Instead of a monolithic state token, side-effecting nodes participate in **effect chains** forming per-alias-class orderings, akin to memory/effect edges in HotSpot C2 and TurboFan. Effect chains can be partitioned by alias class (e.g., by page or alias set), giving the optimizer more freedom than a single global token while preserving necessary ordering.

For memory, effect edges follow the weak memory models defined in [herdtools7](https://github.com/herd/herdtools7/) (e.g., x86-TSO, ARMv8) to allow maximum optimizability and correctly model fences and synchronization instructions.

Effect edges do *not* represent linear or unique tokens; they are "large values" containing the entire process state up to the process boundary. This matters for operations like `fork(2)` that manipulate process state as a value and would break linearity assumptions.

### Host Register State

Following LLVM Machine IR's design, instructions that implicitly define or use host architectural state carry explicit `def` and `use` annotations rather than relying on a dirty-helper escape hatch. This makes calling-convention effects transparent to the register allocator. Every call boundary is modeled with four location-effect categories per-register:

| Class | Depends on call? | Clobbered? | Compiler role |
|---|---|---|---|
| `IndepPreserved` | No | No | Live range may freely cross the call |
| `DepPreserved` | Yes | No | Must be in designated register; live range preserved |
| `IndepClobbered` (Scratch) | No | Yes | Kill any live range in this register at the call cycle |
| `DepClobbered` (Argument/Return) | Yes | Yes | Must be in designated register; live range ends at call |

This formalism is general enough to describe both FFI boundaries and intra-block transitions, unifying host calling conventions across the entire compiler pipeline.

### Safepoints

Every guest instruction boundary is modeled as an explicit **`Safepoint`** node pinned in control flow. A `Safepoint`:

- Consumes the current control input and all relevant effect-chain heads.
- Consumes a **`Framestate`** node aggregating SoN values for the full guest architectural state (general-purpose registers, flags, stack pointer, PC, and all other guest-visible state).
- Produces new control and effect outputs for subsequent nodes.

Safepoints are a *liveness* constraint, not a *scheduling* constraint: they anchor live ranges for architectural state and are never pruned by DCE (though a dedicated safepoint-reduction pass may remove them). This mirrors `SafePointNode` in HotSpot C2, which captures JVM state at garbage collection and deoptimization points.

According to Cliff Click, a safepoint is conceptually a type of control flow instruction—just one that is rarely taken—so it fits naturally in the control node family.

The Safepoint acts as the IR's equivalent of VEX's `IMark`, but there is no concept of a "lightweight" instruction marker: the instruction boundary is definitionally the point at which the full architectural state must be reconstructible. This is observable in the guest machine via interrupts and is not negotiable.

When moving beyond DBT to high-level-language (HLL) compilation, safepoints will be inserted at relevant HLL boundaries following the Java C2 approach. For now, DBT is the target; the 1-to-1 mapping between original and translated instructions provides a strong correctness benchmark.

### Framestate Nodes

A **`Framestate`** is a side-effect-free structural node that aggregates the SoN values representing guest architectural state:

- General-purpose registers (e.g., `EAX`, `EBX`, `RIP`)
- Flags (e.g., `ZF`, `CF`, or a packed flags value)
- Stack pointer and base pointer
- Any additional guest-visible state (segment registers, control registers, etc.)

A `Framestate` is conceptually a mapping `name → value-node`. It has data edges to each value node but no effects or control of its own—it encodes a *projection* of the SoN at a point, not a mutation. When a safepoint is lowered to native code, the code generator uses the framestate to build deoptimization or debug metadata in a compact stack-map format (fast deopt) or DWARF-like format (debugger integration).

### Asynchronous Interrupt Strategy

The hardware's reorder buffer (ROB) provides precise interrupt behavior for the host CPU's out-of-order execution. The IR's safepoints extend this to the *guest* state, which the host hardware does not natively understand. When an asynchronous interrupt fires:

1. A signal handler captures the native PC and register set, then uses `siglongjmp` to escape to a safe runtime trampoline (avoiding async-signal-unsafe operations inside the handler).
2. Outside the handler, the runtime maps the captured PC to the nearest safepoint using a compact stack-map table keyed by native code address.
3. The framestate at that safepoint reconstructs the guest architectural state from live values in the native register set and stack frame.
4. The runtime transfers control to the interpreter or Tier 1 code at the corresponding guest instruction address using the reconstructed state.

The safepoint semantics require only that *some* reasonable safepoint is reconstructible when an interrupt fires—compatible with the observed effects, not a maximally precise one—and that reconstruction is *possible*, not that all values are immediately in machine registers. This latitude is what allows the code generator to perform optimizations despite precise interrupt semantics.

### FFI and Native Calls

Call nodes are safepoints by construction: they take a partial framestate as input and the remaining registers don't matter. Different FFI modes (unsafe, safe, interruptible) are specified at a higher semantic layer and influence runtime behavior, not IR structure. The SoN representation is oblivious to FFI cancellation policies; it only guarantees that the framestate at the call's safepoint accurately reflects guest state prior to entering native code.

---

## Comparison with HotSpot C2

The design closely parallels Java HotSpot's C2 compiler:

- **Sea-of-Nodes core.** Both use SoN as the core IR, combining SSA dataflow with explicit control and memory/effect edges.
- **Safepoints + state nodes.** `Safepoint`/`Framestate` are structurally equivalent to C2's `SafePointNode`/`JVMState`.

Key differences arise from the target domain:

- The IR targets **dynamic binary translation** of low-level machine instructions, not a high-level managed language.
- Guest register state is more prominent; framestates model architectural registers rather than Java locals and operand stacks.
- The guest microcode domain has cleaner purity boundaries than Java's VM semantics, making SoN more effective here.

---

## Implementation Notes

A Python prototype implements this IR with minimal boilerplate:

- **Node objects:** Each with `opcode`, `inputs`, `uses`, type information, and optional `framestate` or metadata.
- **Graph rewriting helpers:** Utilities like `add_input`, `replace_with`, and builders for common patterns (e.g., guest instruction templates).
- **Printing and debugging:** A printer that uses the saved and preserved node ordering to linearize the SoN into scheduled blocks, producing readable text dumps suitable for diffing across passes.

Performance is not a primary concern at this stage; Python is intended for experimentation and correctness validation. Once the IR is well-understood, critical passes (GCM, CSE) can be optimized or ported.

---

## References

1. [Sea of Nodes – Wikipedia](https://en.wikipedia.org/wiki/Sea_of_nodes)
2. [The Sea of Nodes and the HotSpot JIT – Cliff Click, JokerConf 2019](https://jokerconf.com/archive/2019/talks/7awcra2dd8cnxs6oi9dxhi/)
3. [Global Code Motion / Global Value Numbering – Cliff Click, PLDI 1995](https://dl.acm.org/doi/abs/10.1145/207110.207154)
4. [Implementation of Precise Interrupts in Pipelined Processors – Smith & Pleszkun](http://people.eecs.berkeley.edu/~kubitron/courses/cs252-F00/handouts/papers/p291-smith.pdf)
5. [A Simple, Fast Dominance Algorithm – Cooper, Harvey, Kennedy](https://repository.rice.edu/items/99a574c3-90fe-4a00-adf9-ce73a21df2ed)
6. [C2: The JIT in HotSpot – Cliff Click](https://assets.ctfassets.net/oxjq45e8ilak/12JQgkvXnnXcPoAGoxB6le/5481932e755600401d607e20345d81d4/100752_1543361625_Cliff_Click_The_Sea_of_Nodes_and_the_HotSpot_JIT.pdf)
7. [Design of the Java HotSpot Client Compiler for Java 6 – Kotzmann et al.](http://www.cs.cmu.edu/~fp/courses/15411-f14/cmu-only/kotzmann08.pdf)
8. [Land ahoy: leaving the Sea of Nodes – V8 Blog](https://v8.dev/blog/leaving-the-sea-of-nodes)
9. [Deoptimize me not, v8 – Fedor Indutny](https://darksi.de/a.deoptimize-me-not/)
10. [SeaOfNodes/Simple – GitHub](https://github.com/SeaOfNodes/Simple)
11. [herdtools7 – weak memory models](https://github.com/herd/herdtools7/)
12. [VEX IR internals – Valgrind notes.txt](https://fossies.org/linux/valgrind/docs/internals/notes.txt?m=b)
13. [VEX: Where next for Valgrind's dynamic instrumentation – FOSDEM 2017](https://archive.fosdem.org/2017/schedule/event/valgrind_vex_future/attachments/slides/1842/export/events/attachments/valgrind_vex_future/slides/1842/valgrind_vex_future.pdf)
14. [Re-order buffer – Wikipedia](https://en.wikipedia.org/wiki/Re-order_buffer)
15. [siglongjmp – POSIX](https://pubs.opengroup.org/onlinepubs/7908799/xsh/siglongjmp.html)
16. [JIT Compilation System – LuaJIT DeepWiki](https://deepwiki.com/LuaJIT/LuaJIT/2-jit-compilation-system)
17. [Introduction to HotSpot JVM C2 JIT Compiler, Part 1](https://eme64.github.io/blog/2024/12/24/Intro-to-C2-Part01.html)
18. [Growing the DWARF tougher – Tobast](https://tobast.fr/files/oracle18.pdf)


6. [ROB: Ensuring In-Order Commit in OoO Execution](https://medium.com/@jason890418123/rob-ensuring-in-order-commit-in-ooo-execution-da34853375ed) - Explore the ROB’s role, its internal structure, and why it ensures in-order commit while allowing ou...

10. [Navigating the Sea of Nodes: Understanding Compiler ...](https://www.oreateai.com/blog/navigating-the-sea-of-nodes-understanding-compiler-optimization/e912c1aadd7b34a80ecd318ea9f4b9a8) - This article explores how compilers transform source code into machine language using concepts like ...

11. [What's the difference between effect and control edges of V8's TurboFan?](https://stackoverflow.com/questions/65196085/whats-the-difference-between-effect-and-control-edges-of-v8s-turbofan) - I've read many blog posts, articles, presentation and videos, even inspected V8's source code, both ...

12. [A friendlier visualization of Java JIT's compiler control flow](https://robcasloz.github.io/blog/2022/05/24/a-friendlier-visualization-of-javas-jit-compiler-based-on-control-flow.html) - The unified nature of the sea-of-nodes representation eases the implementation of a number of optimi...

13. [[PDF] C2 The JIT In HotSpot](https://assets.ctfassets.net/oxjq45e8ilak/12JQgkvXnnXcPoAGoxB6le/5481932e755600401d607e20345d81d4/100752_1543361625_Cliff_Click_The_Sea_of_Nodes_and_the_HotSpot_JIT.pdf)

14. [There are solutions, but with tradeoffs. In theory DWARF ...](https://news.ycombinator.com/item?id=33172563) - In theory DWARF debugging metadata is Turing complete, so you could bundle an exact copy of the unop...

15. [Growing the DWARF tougher:  synthesis, validation and compilation](https://tobast.fr/files/oracle18.pdf)

16. [(2.1.1) Implementing Precise Interrupts in Pipelined Processors](https://pages.cs.wisc.edu/~ragh/Qualfiles/(2.1.1)%20Implementing%20Precise%20Interrupts%20in%20Pi.html)

17. [Introduction to HotSpot JVM C2 JIT Compiler, Part 1](https://eme64.github.io/blog/2024/12/24/Intro-to-C2-Part01.html) - Introduction to HotSpot JVM C2 JIT Compiler, Part 1 · The compilation happens in parallel with execu...

18. [A simple, fast dominance algorithm](https://repository.rice.edu/items/99a574c3-90fe-4a00-adf9-ce73a21df2ed) - The problem of finding the dominators in a control-flow graph has a long history in the literature. ...

19. [Dominator (graph theory)](https://en.wikipedia.org/wiki/Dominator_(graph_theory)) - ↑ Cooper, Keith D.; Harvey, Timothy J; Kennedy, Ken (2001). "A Simple, Fast Dominance Algorithm" (PD...

20. [Cliff Click](https://dblp.org/pid/39/4499) - Cliff Click: Global Code Motion / Global Value Mumbering. PLDI 1995 ...

22. [The GNU C Library - Longjmp in Handler](http://ftp.gnu.org/old-gnu/Manuals/glibc-2.2.3/html_node/libc_479.html)

23. [Deoptimize me not, v8 - Fedor Indutny's Blog](https://darksi.de/a.deoptimize-me-not/) - The more general solution is to find all live values (the ones that may be used by later functions) ...

24. [JIT Compilation System | LuaJIT/LuaJIT | DeepWiki](https://deepwiki.com/LuaJIT/LuaJIT/2-jit-compilation-system) - This document explains the core JIT (Just-In-Time) compilation pipeline in LuaJIT, which transforms ...

28. [The Java HotSpot Performance Engine Architecture](https://www.oracle.com/java/technologies/whitepaper.html) - The Java HotSpot VM architecture addresses the Java language performance issues described above. The...
