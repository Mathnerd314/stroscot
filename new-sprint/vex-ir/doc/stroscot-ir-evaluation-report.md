# Evaluating the Stroscot IR: A Design Review

## Executive Summary

The Stroscot IR is a novel compiler intermediate representation grounded in classical two-sided sequent calculus and linear logic. Its central thesis is **data-control symmetry**: continuations should be first-class citizens of the IR, symmetric with data values, in order to solve scheduling pathologies (e.g., CSE/GCM oscillation) that arise in conventional Sea-of-Nodes and SSA/CFG IRs.

This report evaluates whether the Stroscot IR is well designed by:

1.  Placing it in the general IR design space (structural form, value model, control-flow model).
2.  Comparing it against MLIR specifically, since both claim extensible operation sets.
3.  Assessing engineering complexity, strengths/weaknesses/opportunities/threats, and risk-return profile.
4.  Synthesizing a final verdict.

------------------------------------------------------------------------

## 1. Motivation: The Problem Stroscot Solves

Modern IRs (SSA/CFG, Sea of Nodes) treat data values as first-class (nameable, shareable via `let`) while treating control continuations as second-class (not nameable, not shareable as values). This causes concrete pathologies:

``` javascript
switch(x) {
  case 1: return a/b;
  case 2: return a/b;
  case 3: return 42; // Hot path
}
```

Global CSE merges the two `a/b` computations; Global Code Motion hoists the merged node to the Least Common Ancestor (the switch entry), executing the division unconditionally even on the hot path that never needs it. A sinking pass pushes it back down, CSE re-merges it, and the cycle can fail to terminate. The Stroscot report identifies this as a **vocabulary failure**: the IR has no way to say "share this computation but bind it to a specific control context."

The theoretical fix, drawn from Downen/Maurer/Ariola/Peyton Jones (ICFP 2016, PLDI 2017), is to treat the two sides of a sequent symmetrically: a `let` binding names a producer (data); a **Shared Tail-Target** (join point) names a consumer (control). Stroscot extends the intuitionistic Sequent Core formalism to the full **classical two-sided** sequent calculus with linear logic, achieving complete data-control symmetry.

------------------------------------------------------------------------

## 2. Structural Anatomy

| Component | Role | Nearest conventional analogue |
|----|----|----|
| Proof Block (PB) | Locally acyclic derivation DAG; root type Γ⊢Δ | Basic block with typed parameter/successor interface |
| Inter-Block Continuation (IBC) | Explicit reference from a PB leaf to another PB's root | Jump / back-edge / mutual recursion |
| Logical Op-Node (𝕁⁺) | Constructed data (tuples, variants, literals) | Data-producing instructions |
| Logical Op-Node (𝕁⁻) | Control operations (functions, continuations) | Calls, branches, join points |
| Exponential rules (!⁺/!⁻) | Explicit duplication/discard of non-linear values | Implicit copy semantics in ordinary IRs |
| Use/Def rules | Named recursive references realizing cycles | Loop back-edges, mutual recursion |
| Structural Fold Optimization | Cut elimination over the proof tree | CSE, DCE, constant folding, algebraic simplification |

**Conceptual model:** a program is a single infinite, acyclic proof tree (full unrolling of all loops/recursion). Physically, this is finitely encoded via PBs (locally acyclic) plus IBCs (which carry all cycles). This split is what guarantees that *local* optimization within a PB is confluent (Church-Rosser) — no infinite-loop risk — while *global* optimization across the IBC graph still needs ordinary termination guards, exactly as in any compiler with loops.

**The SSA Fragment and Stroscot–Kelsey Correspondence:** Stroscot defines a syntactic restriction (no 𝕁⁻ or !⁻ type in the left context of any interior sequent) under which there is a proven bijection to SSA CFGs, mirroring the Kelsey CPS↔SSA correspondence. This is the report's strongest formal result: it gives Stroscot a principled fallback into ordinary SSA/CFG territory, and a precise characterization of exactly when a program needs full first-class continuations.

------------------------------------------------------------------------

## 3. Placement in the IR Design Space

Using axes established in the broader survey (structural form, value model, control-flow model, phi representation, granularity, fixity of instruction set):

| Axis | Stroscot |
|----|----|
| Structural form | **Hybrid**: locally acyclic graph (PB) + explicit global reference graph (IBC) |
| Value model | **SSA-compatible subset**, with explicit linear/exponential resource tracking beyond ordinary SSA |
| Control-flow model | **Explicit continuations / join points**, not just branch terminators |
| Phi representation | PB root type Γ⊢Δ acts as block-argument signature (MLIR-style, not classical phi nodes) |
| Granularity | Small logical core (𝕁⁺/𝕁⁻) with derived "pure operation" opcodes layered on top |
| Instruction-set fixity | **Extensible**: pure ops/types are a swappable set, similar in spirit to MLIR dialects |
| Effects | Modeled as an explicit, never-deconstructed Program value (linear, single-threaded) |

Its closest relatives, per the report itself, are: SSA CFG (exact target of the SSA fragment), Sea of Nodes (the intra-PB DAG resembles an acyclic SoN, with the IBC graph as the CFG skeleton), CPS (isomorphic to the SSA fragment via Kelsey), RVSDG (PBs resemble RVSDG regions), and Sequent Core / GHC (direct theoretical ancestor, extended from intuitionistic to classical logic).

------------------------------------------------------------------------

## 4. Comparison with MLIR

Both IRs claim "extensible operation sets," but the extensibility operates at different layers.

| Question | MLIR | Stroscot |
|----|----|----|
| What is fixed (the substrate)? | Operations, Values, Blocks, Regions; SSACFG or Graph regions; generic pass infrastructure | Proof Blocks, Inter-Block Use/Def, 𝕁⁺/𝕁⁻ logical op-nodes, exponential rules |
| What is left open? | The operation set — dialects define new ops, types, attributes, conversions | The operation set — pure ops/types are a swappable, extensible layer over the logical core |
| Semantic discipline | **Federated**: region/op semantics are supplied by the containing operation; no universal semantic core; multiple dialects may encode the same idea differently; conversion between dialects is first-class | **Canonicalizing**: extensions are meant to elaborate through one logical account of data, control, and resources; in principle there is one correct derived form for a given operation |
| Governing slogan | "Let many abstractions coexist, then legalize/lower between them" | "Give all abstractions one deeper logical account, then optimize through that account" |
| Character | An **extensible IR framework** | An **extensible semantic calculus** |
| Primary strength | Ecosystem-scale flexibility; battle-tested tooling (TableGen, FileCheck, dominance utilities, pass manager) reused from LLVM | Unified semantics; data/control symmetry closes a real vocabulary gap; explicit resource/copy tracking; proven SSA-fragment correspondence |
| Primary risk | Semantic fragmentation — dialects can drift, requiring ongoing conversion/legalization engineering | Pressure on every useful abstraction to fit cleanly into the logical core; unproven at production scale; heavier upfront theory investment |

This is the report's most important finding: **the two IRs are not opposed** on "extensibility vs. non-extensibility." They differ in *what kind of discipline* governs the extension mechanism. MLIR's dialects introduce semantics; Stroscot's dialects are meant to specialize/package semantics that are already grounded in the logical substrate. MLIR optimizes for pluralism; Stroscot optimizes for semantic compression.

------------------------------------------------------------------------

## 5. Engineering Complexity and Pain Points

**Genuine pain points (not resolved by the "wordiness vs. complexity" objection):**

- **Global correctness reintroduces ordinary hard problems.** Once cyclic Use/Def references are in play, strong normalization is intentionally abandoned, and the optimizer needs standard loop termination guards — this is true of Stroscot, MLIR, and every Turing-complete IR alike, so it is not a Stroscot-specific weakness, only a reminder that the "confluent, elegant" story is strictly local (intra-PB).
- **Tooling has to be built from scratch**, but this is symmetric with any new IR — MLIR itself is still maturing its own tooling, and even V8's Sea-of-Nodes-derived Turboshaft team continues to invest in tooling years into that IR's life. This is a cost of novelty, not a unique defect.
- **Verification burden is not "extra"** — any IR needs a well-formedness/type checker; Stroscot's checker additionally verifies linear-resource discipline (linearity, exponential promotion/dereliction/weakening/contraction) and SSA-fragment membership, which are more numerous checks than a conventional SSA verifier but each is a well-defined, mechanical invariant, not an open-ended design problem.
- **Onboarding is a naming problem more than a comprehension problem** for engineers already fluent in SSA/CFG, CPS, and region-based IRs (RVSDG, MLIR) — the underlying moves (block arguments, join points, linear resource tracking) all have precedents; the friction is mostly in learning the sequent-calculus vocabulary layered on top of familiar structures.
- **Runtime coupling is optional, not mandatory** — heap-allocated first-class continuations are a design choice for the *full* IR; a compiler can restrict itself to the proven SSA fragment and avoid the runtime/closure-representation burden entirely, trading away first-class continuations for a conventional calling convention.

**Where the complexity is real and unavoidable:**

- The IR genuinely combines more moving parts than a conventional SSA/CFG IR: two node polarities (𝕁⁺/𝕁⁻), two exponential polarities (!⁺/!⁻) with four core rules and four admissible rules, Proof Blocks, Inter-Block Continuations, and Use/Def — each individually simple, but the union is a wider surface than "blocks + instructions + phi nodes."
- The claim that "most traditional optimizations are special cases of cut elimination" is a strong, falsifiable engineering claim, not yet validated against a real optimizer's workload (profitability heuristics, phase ordering, compile-time budgets) — the theory gives a *focus point* for rewrites (cut nodes), which is a genuine structural advantage over ad hoc pattern matching, but it does not by itself solve profitability or scheduling under compile-time limits.

------------------------------------------------------------------------

## 6. SWOT Analysis

|  | Assessment |
|----|----|
| **Strengths** | Directly targets a real, named pathology in SoN/SSA scheduling (CSE/GCM oscillation) via a principled mechanism (join points as first-class continuations). Has a proven bijective bridge to ordinary SSA CFGs (Stroscot–Kelsey correspondence), so it is not "all or nothing" relative to existing backends. Explicit linear/exponential resource tracking makes copying and discarding visible in the IR rather than implicit. Optimization has one theoretical center of gravity (cut elimination) rather than a bag of unrelated passes. |
| **Weaknesses** | No production implementation or benchmark evidence yet — the beauty of the formalism is unproven against real compile-time and code-quality budgets. Full generality (arbitrary first-class continuations, cyclic Use/Def) sacrifices strong normalization and reintroduces conventional termination/scheduling problems at the global level. Requires engineers to learn a genuinely new vocabulary (two-sided sequents, polarity, exponentials) even if the underlying moves are familiar. |
| **Opportunities** | A capable niche for languages/compilers whose core value proposition is first-class control (continuations, effect handlers, exceptions) or fine-grained resource/copy tracking. Usable as a specialized mid-end that lowers to ordinary SSA/CFG backends via the proven correspondence, de-risking adoption. Could differentiate against a landscape where many teams already view LLVM/MLIR as large and fragmented. |
| **Threats** | MLIR/LLVM have overwhelming infrastructure and ecosystem inertia (TableGen, FileCheck, dominance analysis, existing backends); a new IR must clear a high bar to justify divergence. Risk of the "theory trap": the parts that are mathematically elegant may not be the parts that dominate real compile-time, debuggability, or code-quality outcomes. |

------------------------------------------------------------------------

## 7. Risk–Return Positioning

Stroscot IR sits at **high risk / potentially high return**:

- **Return case:** if Structural Fold Optimization genuinely subsumes CSE/DCE/constant-folding/ algebraic simplification under one confluent local rewrite system, and if the SSA-fragment correspondence lets it interoperate cleanly with existing SSA/CFG backends, the IR offers a materially better handling of control-sensitive sharing than SoN or plain SSA — a real, citable differentiator, not just aesthetic novelty.
- **Risk case:** the complexity is concentrated in the *global* (cross-PB, cyclic Use/Def) regime, exactly where the elegant local guarantees stop applying — production quality there depends on the same profitability heuristics, phase ordering, and compile-time engineering that any compiler needs, so the theoretical foundation does not eliminate the hardest 20% of the work.
- There is no meaningful "safe default" framing for this IR — unlike MLIR, which can be adopted incrementally dialect-by-dialect, Stroscot's argument that "an IR is all-or-nothing because passes cannot be changed later" implies a large one-time commitment before the return is visible, which raises the effective risk relative to an ecosystem-based bet like MLIR.

------------------------------------------------------------------------

## 8. Verdict: Is the Stroscot IR Well Designed?

**By internal consistency and theoretical grounding: yes.** The IR is not an arbitrary pile of node kinds; it derives from a specific, well-known formal system (classical two-sided sequent calculus / linear logic), and the report demonstrates real formal payoff — a working correspondence to SSA CFGs, a principled explanation for why other approaches to recursion (μMALL fixed points, guarded modalities) were rejected, and a concrete mechanism (join points/IBCs) that resolves the motivating CSE/GCM pathology at the vocabulary level rather than via a patch.

**By engineering-readiness criteria: not yet proven.** The report is a design document and formal justification, not a validated implementation with compile-time, code-quality, or debuggability data. The hardest and most conventional part of compiler engineering — profitable global optimization under a fixed compile-time budget across a cyclic control graph — is explicitly acknowledged as outside the elegant local (intra-PB) guarantees, and remains as real work.

**By comparison with MLIR specifically:** the two are answering different questions. MLIR is a proven, federated extensibility framework — the safer bet for teams wanting ecosystem leverage. Stroscot is a canonicalizing semantic calculus — a more ambitious bet that a single logical account of data, control, and resources will produce a more disciplined and more powerful IR. Both are legitimate design points, and Stroscot's approach is not "worse" for being newer or more mathematically dense — it is different in kind, trading ecosystem maturity for semantic unity.

**Overall assessment:** Stroscot IR is a **well-designed formal proposal** whose central innovation (data-control symmetry via join points as first-class continuations) is a genuine and well-targeted contribution to a documented real problem in optimizing compilers. Its design is internally coherent, has a principled SSA off-ramp, and is no more structurally complex than MLIR's own concept inventory. Whether it is well designed *as production infrastructure* is an open question that can only be answered by implementation and measurement — the recommended path is to build the SSA fragment first, validate the join-point mechanism against the canonical CSE/GCM oscillation benchmark, and expand into the full continuation-capable fragment only once that value is demonstrated.
