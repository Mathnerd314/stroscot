# Stroscot Engineering, Assurance, and Knowledge Architecture
**Comprehensive Design Document**

**Status:** Baseline Architecture Specification (consolidated from project research & design iterations; later decisions supersede earlier drafts)  
**Project:** Stroscot Programming Language & Compiler  
**Scope:** Project estimation, milestone roadmap, maturity-scaled SDLC, agent workflows, formal kernel/verification architecture, epistemic hygiene, ontology-backed assurance schemas, code traceability, and Zotero integration.

---

## 0. Supersession & Reconciliation Log

Because this document consolidates an iterative design exploration where later decisions corrected earlier proposals, the table below records all authoritative overrides:

| Earlier Proposal | Overriding Decision | Authoritative Design Rule |
| :--- | :--- | :--- |
| Turn 1 single-project budgets & fixed $\times 1.25 / \times 1.5 / \times 2.0$ multipliers treated as P50/P80 forecasts | Turns 2–3 | **Withdrawn as empirical forecasts.** Replaced by a multi-reference-class portfolio (Ada/Ed, York, DDC, CompCert, Project Oberon, FORTRAN, Concurrent Pascal), distinct completion contracts, explicit correlated risk-event modeling, and personal PSP/PROBE calibration. |
| Turn 4 academic pass/subset milestone list | Turns 5–6 | **Reframed under constraint/replacement and product-adoption boundaries.** Technical milestones follow real-world constraint shifts (8cc, Zig, Hare, Odin) and sit beneath an external product-level maturity ladder. |
| Turn 9 heavyweight OpenJDK/GCC/CPython change-control gates applied uniformly | Turns 10–12 | **Scaled by component maturity and commitment readiness.** Exploratory prototypes optimize for learning; baseline and stable components enforce strict protection and promotion gates. |
| Turn 17 bespoke local YAML/Markdown schema + KBpedia KKO backbone | Turn 23 | **Profile existing standards instead of inventing bespoke semantics.** Use **SEPIO (LinkML)** for claims/evidence, **OMG SACM v2.3** for assurance cases, **W3C PROV-O** for provenance, **W3C Web Annotation (`oa:`)** for passage anchors, and **FaBiO/CiTO** for bibliographic/citation typing; treat **KKO** only as an optional upper-level classifier. |
| Turn 20 using Lean's kernel directly to validate Stroscot derivations | Turn 21 | **Hybrid two-kernel split.** Stroscot uses its own **two-sided linear logic with extensions**, requiring a dedicated **Stroscot object-level kernel/checker**; **Lean** serves as the **metalogic** proving checker soundness and metatheorems. |

---

## 1. System Scope & Technical Architecture

Stroscot is a solo, multi-year language and compiler project featuring an unusual composition of semantic and runtime mechanisms:

```text
Surface Languages
       │
       ▼
Poset-Aware Name Resolution & Unified Proof-Search Engine
  (finite cut-free proofs in two-sided linear logic + extensions)
       │  produces Normal Proof Terms
       ▼
Elaboration ──► Imperative CPS IO Program
       │
       ▼
Finalizer Pass on Imperative CFG
       │
       ├──────────────────────────────────────────┐
       ▼                                          ▼
Baseline Execution Path                  Advanced Execution Path
(Python semantic oracle /                (Token Machine [GoI/IAM, needed]
 simple native baseline)                  + Redex-Family Memoisation
                                          + HotSpot-style Tiered JIT w/
                                            guards & de-optimisation for fexprs)
```

### Key Architectural Boundary Decisions
1. **Python Prototype as Semantic Oracle, Not Permanent Build Dependency:** Retained for differential testing and semantic arbitration (analogous to NYU Ada/Ed's executable SETL model) until removed from the required bootstrap build path (Zig's C++ removal boundary).
2. **Co-Design of IRs and Assurance:** Following CompCert, CakeML, CoreJIT, and FM-JIT, intermediate representations and state boundaries are defined so simulation/equivalence relations and invariant checkers can be stated cleanly at every pass boundary rather than retrofitted.
3. **Bootstrap Decoupling:** Tiered JIT/de-optimisation and non-bootstrap surface features are excluded from the initial self-hosting dependency closure unless strictly required by the compiler's own source semantics.

---

## 2. Product Maturity & Technical Milestone Roadmap

Technical completion ("pass X is coded") is separated from both **constraint/replacement milestones** (what constrains development) and **product maturity** (what an outsider can entrust to Stroscot).

### 2.1 External Product-Level Maturity Ladder (Top-Level Roadmap)

Informed by DOE Adoption Readiness Levels, Meyerovich & Rabkin's heavy-tail language adoption study, and Cliff Click's *Simple* (Sea-of-Nodes) trajectory, Stroscot defines its **First Product** explicitly (e.g., *research evaluation platform* vs. *embeddable proof-search/dispatch engine* vs. *compiler substrate* vs. *application language*) and tracks external readiness separately from internal architecture:

| Product Stage | Outsider Expectation | Required Acceptance Evidence |
| :--- | :--- | :--- |
| **1. Demonstration** | "This shows an interesting idea." | Curated examples run; scope and limitations are explicitly documented. |
| **2. Reproducible Research Artifact** | "I can reproduce the author's claims." | Clean-environment build instructions, pinned dependencies, and reproducible semantic/benchmark scripts. |
| **3. Independent Prototype** | "I can write something the author did not anticipate." | An outsider completes a nontrivial task without continuous author intervention. |
| **4. Evaluation-Ready Niche Tool** | "We can afford to investigate this for our needs." | Explicit supported subset, integration path, known limitations, and diagnostic failure modes. |
| **5. External Pilot** | "It works on our problem under controlled conditions." | External workload passes explicit success criteria with recorded workaround costs. |
| **6. Dependable Bounded Use** | "We can rely on it for this bounded role." | Repeatable deployment, diagnostics, tolerable failure modes, and version pinning. |
| **7. Transferable Ownership & Repeatable Adoption** | "We can keep relying on it if circumstances change." | Another engineer can maintain or extend a component; independent users repeat adoption. |

### 2.2 Constraint & Replacement Technical Milestones

Synthesizing Ghuloum's incremental vertical slices, ACATS conformance, Rust's release axes, and practitioner narratives (8cc, Zig, Hare, Odin):

| Milestone | Boundary Crossed (Before → After) | Acceptance Contract | Explicitly Deferred |
| :--- | :--- | :--- | :--- |
| **M1. Executable Kernel Slice** | Disconnected passes → Working vertical slice | Small graph/proof example runs end-to-end through real lowering & execution with checked output. | Full syntax, general search, JIT. |
| **M2. Semantic Application** | Toy examples → Coherent workload | A nontrivial symbolic rewrite or logic-programming application runs on the reference path. | Self-hosting, native performance. |
| **M3. Fixed Corpus / End of Input Freedom** | Author-chosen examples → Externally fixed workload (8cc system-headers boundary) | A frozen acceptance corpus or compiler tree dictates requirements; omissions are tracked as gaps, not worked around by editing examples. | Full language coverage. |
| **M4. Baseline Native Equivalence** | Reference-only → Native parity | Baseline native path matches Python oracle on declared positive, negative (rejection), and effectful cases. | Memoisation, tiered JIT. |
| **M5. Bootstrap Closure & Self-Host Sequence** | External host required → Self-sustaining compiler | Split into 5 verifiable sub-claims:<br>1. *Source closure* (compiler uses only supported subset)<br>2. *First self-compilation* (passes acceptance corpus)<br>3. *Repeated self-compilation* (Gen 2 compiles Gen 3)<br>4. *Bootstrap consistency* (Gen 2 vs Gen 3 comparison)<br>5. *Build-path independence* (Python removed from build loop, retained only as oracle; Zig boundary). | Optimized JIT; external interoperability unless contracted. |
| **M6. Contributor-Usable vs. User-Usable** | Author-assisted → Independent operation (Hare boundary) | Clean build + non-compiler application succeeds without author's unwritten knowledge; diagnostics identify user vs. compiler errors. | Broad ecosystem / package manager (explicitly evaluated as non-goal unless justified, per Odin). |
| **M7. Advanced Runtime Equivalence** | Baseline execution → Token machine + Memoisation + JIT | Token machine, family memoisation, and tiered JIT/de-opt preserve declared baseline observations on adversarial/sharing/fresh-code-shape suites. | Peak throughput claims. |
| **M8. Performance & Resource Qualification** | Correctness-only → Operational fitness | Named workloads meet compile-time, execution-time, retained-memo-memory, and code-size budgets. | Universal benchmark dominance. |

> **Commit-History Hygiene Warning (from `chibicc`):** Curated or history-rewritten repositories (pedagogical commits) must never be treated as empirical velocity logs. All Stroscot repositories and benchmarks must be tagged as `chronological`, `reconstructed`, or `pedagogical`.

---

## 3. Effort Estimation & Forecasting Framework

### 3.1 Reference-Class Portfolio (Outside View)

Evidence labels used strictly throughout: **P** = Published, **O** = Observed, **A** = Accounting Assumption ($1\text{ person-year} = 2{,}000\text{ h}$; $1\text{ person-month} = 1/12\text{ py}$), **D** = Derived, **U** = Unknown.

| Reference Class / Project | Scope & Boundary | Published Effort (P) | Normalized Hours (D) | Transferability to Stroscot |
| :--- | :--- | :--- | :--- | :--- |
| **Kang et al. (POPL 2016)** | Extending CompCert 2.4 to separate compilation | $< 2$ person-months | $< 333\text{ h}$ | Architecture-leverage lower bound for modular extensions only. |
| **CompCert (Leroy 2009 / ERTS 2016)** | Coq formalization & proof (42k→100k Coq lines, 14 passes, 8 IRs; unverified CIL frontend & asm/linker) | $3\text{ py}$ (2009) → $6\text{ py}$ (2016) | $6{,}000\text{ h} \to 12{,}000\text{ h}$ | Strong anchor for multi-IR lowering & verification; excludes unverified frontend/toolchain and novel runtime research. |
| **Project Oberon (Wirth & Gutknecht 1995)** | Compact language, compiler, OS, display, networking, editors (2 people, 1986–1989) | $\approx 5\text{ py}$ | $\approx 10{,}000\text{ h}$ | Counterweight showing lean integrated systems are feasible if complexity is ruthlessly minimized. |
| **CP-6 Pascal (1982)** | Production ISO draft-standard Pascal compiler | $\approx 5\text{ py}$ (1.5 elapsed yr) | $\approx 10{,}000\text{ h}$ | Conventional production compiler baseline. |
| **NYU Ada/Ed (1979–1983)** | Executable SETL semantic model + parser + static checks reaching ANSI validation (25k SETL lines) | $100\text{ person-months}$ | $\approx 16{,}700\text{ h}$ | **Best anchor for Python semantic prototype → validated reference.** Excludes efficient native runtime/codegen. |
| **Siemens 3003 COBOL / FORTRAN (1957)** | Assembly COBOL (10 programmers) / Optimizing FORTRAN translator | $15\text{ py}$ / $18\text{ py}$ | $30{,}000\text{ h}$ / $36{,}000\text{ h}$ | Pioneering optimization/tooling budgets; poor modern baseline due to era. |
| **York Ada Compiler (1986/1987)** | Native-code compiler + runtime (14 people part-time) | $\approx 20\text{ py}$ | $\approx 40{,}000\text{ h}$ | Anchor for baseline native compiler + runtime. |
| **DDC Ada Compiler (1980–1984)** | Formal-specification-refinement commercial compiler + supporting subset compiler (under 128 KB constraint) | $42\text{ py}$ tech/mgmt ($+2$ overhead) | $84{,}000\text{ h}$ ($88{,}000\text{ h}$) | **Strongest spec-first refinement process comparator.** Shows formal specs do *not* eliminate representation/pass design cost. |
| **LLVM (2000–2005) / Rust (2006–2015)** | Infrastructure + GCC frontend reuse / Personal project → Mozilla-funded 1.0 (41,373 commits, 563,809 SLOC) | **U** (no direct labor timesheets) | **U** (only FTE sensitivity ranges) | Useful for reuse, churn (84% deletions/additions in Rust), and milestone sequencing; unusable as direct hour anchors. |
| **LLM Compilers (CCC, Elo, CURSED)** | 2025–2026 agentic builds (e.g., CCC: 16 agents, ~2k sessions, ~100k LOC, ~$20k API in 2 wks) | **U** human hours (API/session data only) | **U** | Proves rapid implementation under strong oracles (GCC/test suites), but shows regressions, poor optimization, and incomplete specs. |

### 3.2 Estimation Method & Equations

Following Jørgensen's systematic review of expert estimation, NASA cost-risk guidance, and PSP/PROBE:
1. **Subsystem-Specific Analogies + Independent Bottom-Up:** Estimate increments between defined Milestone Acceptance Contracts (Section 2.2) rather than monolithic "phases." Reconcile top-down analogy estimates against bottom-up deliverable sums explicitly.
2. **Correlated Risk-Impact Effort Model:** Separate baseline implementation effort from coupled redesign risk:
   $$H_{\text{remaining}} = \sum_{i} H_{\text{baseline}, i} + \sum_{j} I_j H_{\text{rework}, j}$$
   where $I_j \in \{0, 1\}$ indicates occurrence of a coupled architectural risk (e.g., proof-term change cascading across search/elaboration/tests; family-identity revision invalidating memoisation/finalization; runtime-state revision impacting interpreter/JIT/de-opt).
3. **AI-Assisted Work Accounting (No Blanket Discount Multiplier):** Controlled studies show both throughput gains (+26% tasks in enterprise field studies) and slowdowns (+19% time in METR's early-2025 RCT, with noisy 2026 follow-up). Therefore, track AI-assisted effort explicitly per work package:
   $$H_i^{\text{AI}} = H_{\text{spec}, i} + H_{\text{harness}, i} + H_{\text{supervision}, i} + H_{\text{review}, i} + H_{\text{manual}, i} + H_{\text{rework}, i}$$
   and evaluate delegation economics via **Cost per Accepted Work**:
   $$C_{\text{accepted}} = C_{\text{model}} + C_{\text{tools/compute}} + v H_{\text{human}} + \mathbb{E}[C_{\text{escaped defects}}]$$
4. **Personal Effort Dataset (PSP-Lite) & Calendar Conversion:** Log actual hours across *semantic/spec*, *implementation*, *test/oracle*, *debugging/rework*, *integration*, *docs/tooling*, and *agent review* tied to **accepted vertical slices** (not raw LOC or commit counts). Convert remaining hours to calendar years strictly via observed capacity:
   $$\text{Calendar Years} = \frac{H_{\text{remaining}}}{C_{\text{observed h/year}}}$$
   *(Reference capacities: Friday-only $\approx 368\text{ h/yr}$ [8 h × 46 wks]; heavy part-time $\approx 920\text{ h/yr}$ [20 h × 46 wks]; full-time $\approx 2{,}000\text{ h/yr}$. Note: Friday-only pace yields $\approx 1{,}840\text{ h}$ over 5 years, confirming that restricted scope is mandatory for multi-year milestones.)*

---

## 4. SDLC, Information Dependencies & Maturity-Scaled Governance

### 4.1 Three Connected Planning Structures

Stroscot separates its **execution pipeline** from its **development plan** using three linked structures:
1. **Product Capabilities (`docs/capabilities/`)**: External/internal acceptance contracts (Section 2).
2. **Compiler Architecture (`src/` + contracts)**: Semantic layers, IRs, passes, and runtime components.
3. **Versioned Information-Dependency Graph (`planning/dependencies.yaml`)**: Dependency Structure Matrix (DSM) modeling what decisions, mappings, and evidence each task consumes and produces.

#### Dependency Edge Classification
| Dependency Kind | Scheduling Rule |
| :--- | :--- |
| `hard_prerequisite` | Task cannot meaningfully execute without input; block until available. |
| `contract_dependency` | Agree on interface/invariant boundary; internals proceed in parallel. |
| `assumption_dependency` | Proceed provisionally; record explicit assumption and rework exposure. |
| `evidence_dependency` | Implementation permitted; block baseline promotion until evidence passes. |
| `integration_dependency` | Schedule explicit cross-component verification task. |
| `resource_conflict` | Serialize or isolate workspace edits (no intellectual coupling implied). |

#### Cyclic Investigation Clusters (DSM Grouping)
Mutually dependent design cycles are grouped into bounded **Investigation Clusters** rather than forced into a fake DAG:
- **Cluster A:** Proof-search control, normal proof terms, and CPS IO elaboration.
- **Cluster B:** Needed evaluation (IAM/GoI), graph identity, and redex-family memoisation.
- **Cluster C:** Effects, finalizer CFG semantics, and evaluator state.
- **Cluster D:** Guards, invalidation, and de-optimisation state reconstruction.

### 4.2 Risk-Driven Incremental SDLC & Kanban Flow

Each capability runs its own small lifecycle loop concurrently:
`Frame Capability → Identify Risky Assumptions → Investigate Cluster → Accept Bounded Contract → Implement Vertical Slice → Qualify Against Contract → Promote to Baseline → Observe & Revise`

- **Kanban Board Columns:** `Questions` → `Ready` → `Investigating / Implementing` → `Validating` → `Accepted` (plus explicit `Blocked` with missing decision recorded).
- **Spiral Priority Rule:** Always select the next investigation by asking: *"What assumption, if false, would invalidate the most remaining work?"*
- **TDD Scope:** Use test-first development strictly where the semantic contract is settled (binding/shadowing, CFG invariants, minimized miscompilations, error paths). Never use TDD to prematurely lock in unresolved semantic research questions.
- **Friday Session Re-Entry Protocol:** Every Friday session ends with a structured handoff note recording: *(1) What changed, (2) Established vs. conjectural status, (3) Exact reproduction command, (4) Single next concrete action.*

### 4.3 Three Persistent Project Ledgers

To prevent "epistemic laundering" (model summaries turning guesses into assumed requirements), state is partitioned into three distinct Git-tracked ledgers:
1. **Decisions Ledger (`docs/decisions/`)**: Accepted Architecture Decision Records (ADRs) with context, alternatives, rationale, and revisit conditions. Superseded rather than rewritten.
2. **Evidence Ledger (`assurance/evidence/`)**: Revision-pinned experiment logs, differential test runs, counterexamples, resource measurements, and Lean proof checks.
3. **Work State (`planning/tasks.yaml`)**: Active tasks, Kanban states, blockers, and session handoffs.

### 4.4 Maturity-Scaled Governance & Promotion Gates

Borrowing Rust's *experiment vs. RFC adoption*, Java's *JEP 12 preview* (high implementation quality without permanent commitment), Python's *PEP 411 lesson* (labels alone do not prevent reliance), and OpenJDK/GCC/CPython layered gates:

| Maturity Stage | Primary Objective | Permitted Freedom | Protected Boundaries & Stopping Gate |
| :--- | :--- | :--- | :--- |
| **1. Disposable Experiment** | Resolve one uncertainty | Throwaway code, restricted inputs, local representation rewrites. | Must state question, shortcuts, and reproducible output. Negative results are valid completions. |
| **2. Exploratory Prototype** | Validate mechanism & interactions | Competing prototypes, interpreter instead of native, unoptimized paths. | Cannot alter protected reference observations or baseline contracts. |
| **3. Integrated Baseline** | Sustain incremental progress | Internal implementation choices within agreed contracts. | Must pass exact-patch comparative baseline tests, blast-radius suite, and invariants. **No silent test/oracle edits.** |
| **4. External Pilot** | Test real outsider utility | Scope-bounded features behind explicit capability statements. | Reproducible clean build, clear diagnostics, pinned versions. |
| **5. Stable Supported** | Preserve user/downstream promises | Minimal patches; refactors require intent justification. | Compatibility checks, reversion/rollback plan, full regression suite. |

**Promotion Gate Rule:** Promotion (`Experiment Accepted` → `Implementation Adopted` → `Contract Stabilized`) is always an **explicit separate task**, never an automatic consequence of passing prototype unit tests.

---

## 5. Agent-Assisted Workflows & Verification Harness

### 5.1 Verification Leverage & Operational Model-Tier Routing

Agents shift human effort from typing code to **specification, oracle design, diagnosis, and review**. Tasks are routed dynamically using a **Dated Capability Frontier** rather than static assumptions:

| Operational Tier | Example Task Profile | Verification Leverage & Admission Rule |
| :--- | :--- | :--- |
| **Tier 0: Human Direct** | Tiny edits fully understood; final semantic/research compromises | Bypass AI when prompt/review overhead exceeds direct edit time (METR 2025 lesson). |
| **Tier 1: Economical Bounded Worker** | Serializers, boilerplate fixtures, mechanical refactors, doc formatting | High verification leverage (round-trip/schema tests); fast review. |
| **Tier 2: General Coding Agent** | Local CFG passes, parser extensions, test-driven bug fixes | Explicit pre/post invariants and unit/differential test harness. |
| **Tier 3: Strong Reasoning / Integration Agent** | Cross-module changes, hard failure diagnosis, competing algorithm prototypes | Multi-file contract checks, invariant validators, comparative baseline runs. |
| **Tier 4: Frontier Research Assistant** | Ambiguous semantic formalizations, counterexample search, architecture critique | Evaluated on whether it finds real counterexamples or produces independently checkable artifacts. |

### 5.2 Three Stage-Specific Agent Workflow Topologies

1. **Exploration Workflow:** `Question → Parallel Competing Prototypes / Counterexample Search → Comparative Evidence Report → Human Decision (Adopt / Revise / Reject)`. Agents are explicitly rewarded for finding decisive counterexamples.
2. **Baseline Implementation Workflow:** `Accepted Contract + Protected Oracle → Agent Implementation → Independent Deterministic Runner (exact revision + comparative baseline) → Human Semantic Review → Integration`.
3. **Stable Maintenance Workflow:** `Confirmed Reproducer → Minimal Patch → Blast-Radius Regression Comparison → Release / Revert Decision`.

### 5.3 Mandatory External Enforcements (OpenJDK / GCC / CPython Synthesis)

- **Protected Oracle & Harness:** Implementation agents have read-only access to the semantic reference evaluator, protected acceptance tests, CI scripts, and Lean theorem statements. Changing an expected test behavior requires a separately authorized semantic-decision task (CPython rule).
- **Exact-Revision Binding:** An independent CI runner checks out the exact candidate Git SHA, records baseline vs. candidate failures, and invalidates prior approvals if any subsequent commit alters the patch (GCC rule).
- **Blast-Radius Testing & Reversion Policy:** Shared runtime/memory/proof-term changes trigger full differential, stress, and bootstrap suites. Breaking main triggers immediate rollback rather than unbounded agent repair loops.

---

## 6. Formal Verification, Logical Kernel & Code Traceability

### 6.1 Hybrid Kernel Architecture (Stroscot Object Logic + Lean Metalogic)

Because Stroscot uses its own **two-sided linear logic with extensions** ($\Gamma \vdash_{\text{Stroscot}} \Delta$), Lean's kernel cannot directly serve as Stroscot's object-level logic. Instead, Stroscot adopts CompCert's **untrusted producer + trusted validator** architecture across two distinct layers:

```text
┌────────────────────────────────────────────────────────────────────┐
│ METALOGIC LAYER (Lean 4 — Pinned Toolchain + `#print axioms` gate) │
│  • Inductive definition of Stroscot calculus (⊢_Stroscot)          │
│  • Metatheorems: Cut admissibility, normalization, canonicity      │
│  • Soundness proof: check(Γ, cert, Δ) = true ⇒ Γ ⊢_Stroscot Δ      │
│  • Pass simulation proofs (CoreJIT/FM-JIT style de-opt & state)    │
└───────────────────────────────┬────────────────────────────────────┘
                                │ specifies & justifies
                                ▼
┌────────────────────────────────────────────────────────────────────┐
│ OBJECT-LEVEL LAYER (Stroscot Implementation)                       │
│  • Heuristic Proof Search Engine (untrusted, swappable, AI-tunable)│
│        │ emits finite derivation certificate                       │
│        ▼                                                           │
│  • Small Trusted Stroscot Derivation Checker (Linear Kernel)       │
│        │ verifies resource linearity, side conditions, rules       │
│        ▼                                                           │
│  • Elaborator (normal proof term ──► Imperative CPS IO)            │
└────────────────────────────────────────────────────────────────────┘
```

- **Trust Ledger:** Every trusted assumption (Python oracle, unverified parser/FFI, native evaluation flags in Lean vs. kernel reduction, external assembler/linker) is explicitly logged in `assurance/trust_ledger.yaml` with scope and reduction plan.
- **Assurance Bundles:** Each IR/pass boundary bundles: syntax/data model, invariants, semantics/oracle, state-correspondence relation, correctness obligation, and revision-pinned evidence.

### 6.2 Three-Level Code Traceability

1. **Source-Level:** Core modules and data structures carry compact docstrings linking `Contract ID`, `Decision ID (ADR)`, `Research Mapping Note`, and explicit **Semantic Role Specifications** (*Represents, Allowed States, Identity/Equality relation [proof vs. graph vs. family vs. runtime address], Ownership/Lifetime, Mutation rules, Validity conditions, Checking strategy*).
2. **Build/Test-Level:** Automated manifests bind `implementation_commit + contract_revision + corpus_revision + environment` to produced evidence.
3. **Runtime-Level (Selective):** Carry provenance at runtime *only* where operationally required (source spans, proof-term origin for diagnostics, guard/invalidation dependencies, de-opt reconstruction maps). Never attach bibliographic metadata to runtime AST/graph nodes.
4. **Single Source of Truth:** Maintain mappings in one declarative YAML registry (`assurance/code_bindings.yaml`) and generate reverse indexes automatically (code $\leftrightarrow$ contracts $\leftrightarrow$ decisions $\leftrightarrow$ evidence).

---

## 7. Epistemic Hygiene & Ontology-Backed Assurance Graph

### 7.1 Epistemic Hygiene Rules

1. **Seven Orthogonal Dimensions (No Single `confidence` Scalar):** Track *Claim Kind*, *Provenance*, *Checking Status*, *Applicability to Stroscot*, *Qualitative Confidence + Rationale*, *Project Disposition*, and *Freshness/Staleness*.
2. **Workflow:** `Capture (unchecked) → Source-Fidelity Check → Applicability Bridge → Evidence Gathering → Separate Adoption Decision`.
3. **Anti-Epistemic-Laundering:** Summaries must never strengthen input claims; repeated citations of one source count as one source; numeric conversions must preserve original units, boundaries, formulas, and assumptions (`Published` vs. `Derived` vs. `Assumed`).
4. **Staleness & Contradiction States:** Upstream edits mark dependent assessments `needs_reassessment` (never silently deleted or kept green). Contradictions are preserved explicitly with competing evidence lines.

### 7.2 Integrated Ontology Stack

Following Turn 23, Stroscot profiles established semantic standards rather than inventing a bespoke ontology:

| Layer | Standard / URL | Role in Stroscot |
| :--- | :--- | :--- |
| **Epistemic Claims & Evidence** | **SEPIO Core-IM (LinkML)**,<br>`https://sepio-framework.github.io/sepio-linkml/` | Separates `Proposition` (testable statement), `Statement` (agent assertion/assessment with `direction`, `strength`, `score`), `EvidenceLine` (argument linking evidence items via `directionOfEvidenceProvided` & `strengthOfEvidenceProvided`), `DataItem`, `StudyResult`, `Document`, `Method`, and `Contribution`. |
| **System Assurance Cases** | **OMG SACM v2.3**<br>`https://www.omg.org/spec/SACM/2.3/PDF` | Structures capability-level arguments (`AssuranceCasePackage`, `ArgumentPackage`, `ArtifactPackage`, `TerminologyPackage`) with `Claim`, `AssertedInference`, `AssertedEvidence`, `AssertedContext`, `ArgumentReasoning`, `ArtifactReference`, `isCounter`, `metaClaim`, and `AssertionDeclaration` (`asserted`, `needsSupport`, `assumed`, `axiomatic`, `defeated`, `asCited`). |
| **Provenance** | **W3C PROV-O**<br>`https://www.w3.org/TR/prov-o/` | Tracks `prov:Entity`, `prov:Activity`, `prov:Agent` (`Person`, `SoftwareAgent`, `Organization`), `prov:Plan`, `prov:Bundle`, `wasDerivedFrom`, `wasQuotedFrom`, `wasRevisionOf`, `hadPrimarySource`, and qualified influences (`qualifiedDerivation`, `qualifiedAssociation`, `qualifiedInvalidation`). |
| **Passage Anchoring** | **W3C Web Annotation**,,<br>`https://www.w3.org/TR/annotation-model/` | Binds extracted passages via `oa:Annotation`, `oa:SpecificResource`, `oa:FragmentSelector`, `oa:TextQuoteSelector` (`exact`, `prefix`, `suffix`), `oa:TextPositionSelector`, and `oa:refinedBy`. |
| **Bibliography & Citations** | **SPAR FaBiO & CiTO**,,<br>`http://purl.org/spar/fabio/` & `cito/` | FRBR separation (`fabio:Work`, `Expression`, `Manifestation`, `Item`, `Algorithm`) and typed citation intent (`cito:citesAsEvidence`, `supports`, `disputes`, `extends`, `usesMethodIn`). |
| **Validation** | **LinkML + W3C SHACL**<br>`https://www.w3.org/TR/shacl/` | LinkML generates JSON Schema & Pydantic v2 models; SHACL validates structural & epistemic completeness across RDF/JSON-LD exports. |
| **Optional Extensions** | **OMDoc** / **KBpedia KKO**<br>`https://www.kbpedia.org/docs/kko-upper-structure/` | OMDoc (`theory`, `assertion`, `proof`, `derive`) for formal math interchange if needed; KKO (`Monads`, `Particulars`, `Generals`) strictly as an optional upper-level classifier for cross-collection indexing. |

---

### 7.3 Detailed Schemas

#### Schema A: Unified JSON-LD Context & Namespace Registry (`schemas/context.jsonld`)

```json
{
  "@context": {
    "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "dcterms": "http://purl.org/dc/terms/",
    "prov": "http://www.w3.org/ns/prov#",
    "oa": "http://www.w3.org/ns/oa#",
    "sh": "http://www.w3.org/ns/shacl#",
    "sepio": "https://w3id.org/sepio-model/",
    "sacm": "http://omg.org/spec/SACM/2.3/",
    "fabio": "http://purl.org/spar/fabio/",
    "cito": "http://purl.org/spar/cito/",
    "frbr": "http://purl.org/vocab/frbr/core#",
    "kko": "https://kbpedia.org/kko/rc/",
    "stroscot": "https://stroscot.org/schema/assurance/"
  }
}
```

#### Schema B: Stroscot SEPIO-Profile & Engineering Schema (`schemas/stroscot_assurance.yaml` — LinkML)

This LinkML schema profiles SEPIO Core-IM (`Statement`, `EvidenceLine`, `Proposition`, `DataItem`, `Document`, `Method`, `Contribution`) and bridges it directly to SACM v2.3 assurance artifacts, PROV-O provenance, W3C Web Annotation selectors, and Stroscot's code/decision/Zotero objects:

```yaml
id: https://stroscot.org/schema/assurance
name: stroscot-assurance-profile
title: Stroscot Assurance, Research, and Engineering Profile
version: 0.1.0
license: Apache-2.0

prefixes:
  linkml: https://w3id.org/linkml/
  stroscot: https://stroscot.org/schema/assurance/
  sepio: https://w3id.org/sepio-model/
  sacm: http://omg.org/spec/SACM/2.3/
  prov: http://www.w3.org/ns/prov#
  oa: http://www.w3.org/ns/oa#
  fabio: http://purl.org/spar/fabio/
  cito: http://purl.org/spar/cito/
  dcterms: http://purl.org/dc/terms/
  xsd: http://www.w3.org/2001/XMLSchema#

default_prefix: stroscot
default_range: string

imports:
  - linkml:types

enums:
  ClaimKindEnum:
    permissible_values:
      SOURCE_REPORT: { description: "Claim attributed to an external publication or doc" }
      OBSERVATION: { description: "Measured or executed experimental result" }
      DERIVATION: { description: "Arithmetic or logical consequence of explicit inputs" }
      ASSUMPTION: { description: "Unverified premise relied upon provisionally" }
      HYPOTHESIS: { description: "Testable research proposition" }
      FORECAST: { description: "Effort, capacity, or risk scenario" }
      NORMATIVE_REQUIREMENT: { description: "Committed Stroscot semantic/system contract" }

  DirectionEnum:
    description: "Maps to SEPIO direction / directionOfEvidenceProvided"
    permissible_values:
      supports: { exact_mappings: ["sepio:supports"] }
      disputes: { exact_mappings: ["sepio:disputes"] }
      neutral: { exact_mappings: ["sepio:neutral"] }

  StrengthEnum:
    permissible_values:
      STRONG: {}
      MODERATE: {}
      WEAK: {}
      CONJECTURAL: {}

  CheckingStatusEnum:
    permissible_values:
      UNCHECKED: {}
      SOURCE_CHECKED: {}
      REPRODUCTION_ATTEMPTED: {}
      REPRODUCED_IN_SCOPE: {}
      INDEPENDENTLY_VALIDATED: {}
      FORMALLY_CHECKED: {}

  ApplicabilityStatusEnum:
    permissible_values:
      NOT_ASSESSED: {}
      CONDITIONAL: {}
      APPLICABLE_IN_SCOPE: {}
      MISMATCH_FOUND: {}
      INAPPLICABLE: {}

  DispositionEnum:
    permissible_values:
      CANDIDATE: {}
      ADOPTED_PROVISIONALLY: {}
      ADOPTED: {}
      REJECTED: {}
      SUPERSEDED: {}
      NEEDS_REASSESSMENT: {}
      CONTRADICTED: {}
      WITHDRAWN: {}

  SACMAssertionDeclarationEnum:
    description: "Exact literals from OMG SACM v2.3 Clause 11.8"
    permissible_values:
      asserted: {}
      needsSupport: {}
      assumed: {}
      axiomatic: {}
      defeated: {}
      asCited: {}

  MaturityStageEnum:
    permissible_values:
      DISPOSABLE_EXPERIMENT: {}
      EXPLORATORY_PROTOTYPE: {}
      INTEGRATED_BASELINE: {}
      EXTERNAL_PILOT: {}
      STABLE_SUPPORTED: {}

  LibraryRoleEnum:
    permissible_values:
      DEVELOPMENT_ONLY: {}
      BOOTSTRAP_DEPENDENCY: {}
      COMPILER_IMPL_DEPENDENCY: {}
      GENERATED_PROGRAM_DEPENDENCY: {}
      RUNTIME_DEPENDENCY: {}
      REFERENCE_ORACLE_DEPENDENCY: {}

  DocumentTypeEnum:
    permissible_values:
      PRIMARY_LITERATURE: {}
      TECHNICAL_REPORT: {}
      THESIS: {}
      SPECIFICATION: {}
      BENCHMARK_OR_CORPUS: {}
      REPOSITORY_DOC: {}

  DataItemTypeEnum:
    permissible_values:
      TEXT_SPAN: {}
      FORMAL_THEOREM: {}
      EXECUTION_LOG: {}
      BENCHMARK_METRIC: {}
      COUNTEREXAMPLE: {}
      NUMERICAL_DERIVATION: {}

classes:
  NamedThing:
    abstract: true
    attributes:
      id:
        identifier: true
        range: uriorcurie
        required: true
      title:
        range: string
      description:
        range: string
      git_revision:
        range: string
        description: "Pinned repository commit SHA where applicable"

  Agent:
    is_a: NamedThing
    class_uri: prov:Agent
    attributes:
      agent_type:
        range: string
        description: "prov:Person | prov:SoftwareAgent | prov:Organization"
      model_version:
        range: string
        description: "Exact LLM version/harness if SoftwareAgent"

  Contribution:
    class_uri: sepio:Contribution
    attributes:
      contributor:
        range: Agent
        required: true
      activity_type:
        range: string
        required: true
        description: "extraction | source_check | applicability_review | experiment | proof_check | approval"
      date:
        range: datetime
        required: true
      plan_ref:
        range: uriorcurie
        description: "prov:hadPlan task contract ID"

  # --- Zotero + FaBiO + Web Annotation Source Layer ---
  ZoteroSourceRef:
    attributes:
      library_type:
        range: string
        required: true
      library_id:
        range: string
        required: true
      item_key:
        range: string
        required: true
      imported_version:
        range: integer
        required: true
      attachment_key:
        range: string
      attachment_sha256:
        range: string
      annotation_key:
        range: string

  Document:
    is_a: NamedThing
    class_uri: sepio:Document
    attributes:
      document_type:
        range: DocumentTypeEnum
        required: true
      fabio_type:
        range: uriorcurie
        description: "e.g., fabio:ConferencePaper, fabio:JournalArticle, fabio:Thesis"
      doi:
        range: string
      citekey:
        range: string
        description: "Pinned Better BibTeX citation key"
      urls:
        range: uri
        multivalued: true
      zotero_ref:
        range: ZoteroSourceRef

  PassageTarget:
    class_uri: oa:SpecificResource
    attributes:
      source_document:
        range: Document
        required: true
        slot_uri: oa:hasSource
      page_label:
        range: string
      section_locator:
        range: string
        description: "Section, theorem, lemma, or algorithm number"
      fragment_value:
        range: string
        description: "oa:FragmentSelector value, e.g., page=12"
      quote_exact:
        range: string
        slot_uri: oa:exact
      quote_prefix:
        range: string
        slot_uri: oa:prefix
      quote_suffix:
        range: string
        slot_uri: oa:suffix
      zotero_annotation_key:
        range: string

  # --- SEPIO Epistemic Core ---
  Proposition:
    is_a: NamedThing
    class_uri: sepio:Proposition
    attributes:
      subject:
        range: string
        required: true
      predicate:
        range: string
        required: true
      object:
        range: string
        required: true
      assumptions:
        range: string
        multivalued: true
      exclusions:
        range: string
        multivalued: true

  DataItem:
    is_a: NamedThing
    class_uri: sepio:DataItem
    attributes:
      data_type:
        range: DataItemTypeEnum
        required: true
      value:
        range: string
        required: true
        description: "Excerpt, theorem statement, metric, or derivation formula"
      reported_in:
        range: Document
      passage:
        range: PassageTarget
      formula:
        range: string
        description: "Required when data_type == NUMERICAL_DERIVATION"
      derived_from:
        range: uriorcurie
        multivalued: true
        slot_uri: prov:wasDerivedFrom

  EvidenceLine:
    is_a: NamedThing
    class_uri: sepio:EvidenceLine
    attributes:
      target_proposition:
        range: Proposition
        slot_uri: sepio:targetProposition
      direction_of_evidence_provided:
        range: DirectionEnum
        required: true
        slot_uri: sepio:directionOfEvidenceProvided
      strength_of_evidence_provided:
        range: StrengthEnum
        slot_uri: sepio:strengthOfEvidenceProvided
      score_of_evidence_provided:
        range: float
      evidence_outcome:
        range: string
      specified_by:
        range: uriorcurie
        description: "Method / evaluation protocol URI"
      has_evidence_items:
        range: DataItem
        multivalued: true
        required: true
        slot_uri: sepio:hasEvidenceItems

  Statement:
    is_a: NamedThing
    class_uri: sepio:Statement
    attributes:
      proposition:
        range: Proposition
        required: true
        slot_uri: sepio:proposition
      claim_kind:
        range: ClaimKindEnum
        required: true
      direction:
        range: DirectionEnum
        required: true
      strength:
        range: StrengthEnum
      confidence_reason:
        range: string
        required: true
      checking_status:
        range: CheckingStatusEnum
        required: true
      applicability_status:
        range: ApplicabilityStatusEnum
        required: true
      disposition:
        range: DispositionEnum
        required: true
      safe_conclusion:
        range: string
        required: true
      not_established:
        range: string
        required: true
      contributions:
        range: Contribution
        multivalued: true
        required: true
      has_evidence_lines:
        range: EvidenceLine
        multivalued: true
        slot_uri: sepio:hasEvidenceLines
      depends_on:
        range: uriorcurie
        multivalued: true
      revisit_when:
        range: string
        multivalued: true

  # --- Stroscot Research-to-Engineering Bridge & Contracts ---
  ApplicabilityBridge:
    is_a: NamedThing
    attributes:
      evaluates_statement:
        range: Statement
        required: true
      target_contract:
        range: PassContract
        required: true
      term_mapping:
        range: string
        multivalued: true
        required: true
        description: "Mapping from source formalism to Stroscot representation"
      satisfied_assumptions:
        range: string
        multivalued: true
      uncovered_mismatches:
        range: string
        multivalued: true
        required: true
      required_adaptation:
        range: string
      status:
        range: ApplicabilityStatusEnum
        required: true

  AdoptionDecision:
    is_a: NamedThing
    description: "Architecture Decision Record (ADR)"
    attributes:
      status:
        range: DispositionEnum
        required: true
      context:
        range: string
        required: true
      alternatives_considered:
        range: string
        multivalued: true
      chosen_option:
        range: string
        required: true
      rationale:
        range: string
        required: true
      informed_by_assessments:
        range: ApplicabilityBridge
        multivalued: true
      library_role:
        range: LibraryRoleEnum
      unresolved_obligations:
        range: string
        multivalued: true
        required: true
      revisit_triggers:
        range: string
        multivalued: true
        required: true
      supersedes:
        range: AdoptionDecision

  PassContract:
    is_a: NamedThing
    attributes:
      maturity_stage:
        range: MaturityStageEnum
        required: true
      input_language:
        range: string
        required: true
      output_language:
        range: string
        required: true
      invariants:
        range: string
        multivalued: true
        required: true
      state_relation:
        range: string
        description: "Simulation / bisimulation relation connecting source & target"
      failure_semantics:
        range: string
        required: true
      exclusions:
        range: string
        multivalued: true
      chosen_by:
        range: AdoptionDecision
        multivalued: true

  CodeBinding:
    is_a: NamedThing
    attributes:
      module_path:
        range: string
        required: true
      symbol_name:
        range: string
      implements_contracts:
        range: PassContract
        multivalued: true
        required: true
      chosen_by_decisions:
        range: AdoptionDecision
        multivalued: true
      adapts_sources:
        range: Document
        multivalued: true
      semantic_role:
        range: string
        description: "Identity/equality, ownership, lifetime, mutation rules"
      qualification_suites:
        range: uriorcurie
        multivalued: true

  ExecutionEvidence:
    is_a: NamedThing
    class_uri: sacm:Artifact
    attributes:
      candidate_commit:
        range: string
        required: true
      baseline_commit:
        range: string
      contract_revision:
        range: string
        required: true
      environment_spec:
        range: string
        required: true
      commands:
        range: string
        multivalued: true
        required: true
      outcome:
        range: string
        required: true
      lean_axioms_checked:
        range: boolean
      supports_statements:
        range: Statement
        multivalued: true
```

#### Schema C: OMG SACM v2.3 Assurance Case Profile (`assurance/cases/example_memo_case.jsonld`)

SACM v2.3 organizes capability-level arguments (`AssuranceCasePackage` containing `ArgumentPackage`, `ArtifactPackage`, and `TerminologyPackage`), explicitly linking SEPIO `Statement`s as `Claim`s and `ExecutionEvidence` / `Document`s via `ArtifactReference` and `AssertedEvidence`:

```json
{
  "@context": "../../schemas/context.jsonld",
  "id": "stroscot:ac/memo-subset-v1",
  "type": "sacm:AssuranceCasePackage",
  "sacm:gid": "AC-MEMO-001",
  "sacm:name": { "lang": "en", "content": "Stroscot Family Memoisation Assurance Case (Subset S)" },
  "sacm:terminologyPackage": [
    {
      "id": "stroscot:term/eval-terms",
      "type": "sacm:TerminologyPackage",
      "sacm:terminologyAsset": [
        {
          "id": "stroscot:term/redex-family",
          "type": "sacm:Term",
          "sacm:value": "RedexFamilyIdentity",
          "sacm:externalReference": "https://stroscot.org/schema/assurance/RedexFamilyIdentity"
        }
      ]
    }
  ],
  "sacm:artifactPackage": [
    {
      "id": "stroscot:artifacts/memo-v1",
      "type": "sacm:ArtifactPackage",
      "sacm:artifactElement": [
        {
          "id": "stroscot:ev/diff-run-042",
          "type": "sacm:Artifact",
          "sacm:version": "git:9a4f1c2",
          "sacm:date": "2026-11-13",
          "stroscot:commands": ["pytest tests/differential/test_memo_equiv.py"],
          "stroscot:outcome": "PASS (142/142 cases; pure subset S)"
        },
        {
          "id": "stroscot:ev/lean-soundness-01",
          "type": "sacm:Artifact",
          "sacm:version": "lean4.20.0@git:9a4f1c2",
          "stroscot:lean_axioms_checked": true,
          "stroscot:outcome": "Check passed; axioms: [propext, Quot.sound]"
        }
      ]
    }
  ],
  "sacm:argumentPackage": [
    {
      "id": "stroscot:arg/memo-v1",
      "type": "sacm:ArgumentPackage",
      "sacm:argumentationElement": [
        {
          "id": "stroscot:claim/C1",
          "type": "sacm:Claim",
          "sacm:gid": "C1",
          "sacm:assertionDeclaration": "asserted",
          "sacm:description": {
            "content": "Memoised token-machine evaluation preserves observable IO behavior on pure subset S."
          },
          "stroscot:linkedSepioStatement": "stroscot:stmt/MEMO-CLAIM-A"
        },
        {
          "id": "stroscot:claim/C1-mem",
          "type": "sacm:Claim",
          "sacm:gid": "C1-MEM",
          "sacm:assertionDeclaration": "needsSupport",
          "sacm:description": {
            "content": "Retained memo-table memory stays within bound B on bootstrap workloads."
          }
        },
        {
          "id": "stroscot:claim/A1",
          "type": "sacm:Claim",
          "sacm:gid": "A1",
          "sacm:assertionDeclaration": "assumed",
          "sacm:description": {
            "content": "Effectful IO primitives are excluded from memoisation caching in subset S."
          }
        },
        {
          "id": "stroscot:ref/E1",
          "type": "sacm:ArtifactReference",
          "sacm:gid": "E1",
          "sacm:referencedArtifactElement": ["stroscot:ev/diff-run-042"]
        },
        {
          "id": "stroscot:rel/AE1",
          "type": "sacm:AssertedEvidence",
          "sacm:isCounter": false,
          "sacm:source": ["stroscot:ref/E1"],
          "sacm:target": "stroscot:claim/C1"
        },
        {
          "id": "stroscot:rel/AC1",
          "type": "sacm:AssertedContext",
          "sacm:source": ["stroscot:claim/A1"],
          "sacm:target": "stroscot:claim/C1"
        }
      ]
    }
  ]
}
```

#### Schema D: W3C SHACL Epistemic & Structural Validation Shapes (`schemas/stroscot_shapes.ttl`)

Automated CI validation enforces structural and epistemic hygiene (record validity) before any assurance graph update can merge:

```turtle
@prefix sh:       <http://www.w3.org/ns/shacl#> .
@prefix xsd:      <http://www.w3.org/2001/XMLSchema#> .
@prefix prov:     <http://www.w3.org/ns/prov#> .
@prefix oa:       <http://www.w3.org/ns/oa#> .
@prefix sepio:    <https://w3id.org/sepio-model/> .
@prefix sacm:     <http://omg.org/spec/SACM/2.3/> .
@prefix stroscot: <https://stroscot.org/schema/assurance/> .

# 1. Every Statement must have a Proposition, explicit safe/not-established boundaries,
#    checking status, applicability, disposition, and contributor provenance.
stroscot:StatementShape a sh:NodeShape ;
  sh:targetClass sepio:Statement ;
  sh:property [
    sh:path sepio:proposition ;
    sh:minCount 1 ; sh:maxCount 1 ;
    sh:class sepio:Proposition ;
  ] ;
  sh:property [
    sh:path stroscot:safe_conclusion ;
    sh:minCount 1 ; sh:datatype xsd:string ; sh:minLength 10 ;
  ] ;
  sh:property [
    sh:path stroscot:not_established ;
    sh:minCount 1 ; sh:datatype xsd:string ; sh:minLength 10 ;
  ] ;
  sh:property [
    sh:path stroscot:checking_status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path stroscot:applicability_status ;
    sh:minCount 1 ;
  ] ;
  sh:property [
    sh:path stroscot:contributions ;
    sh:minCount 1 ;
  ] .

# 2. Every TEXT_SPAN DataItem must point to a Document AND a PassageTarget with exact quote or locator.
stroscot:TextSpanEvidenceShape a sh:NodeShape ;
  sh:targetClass sepio:DataItem ;
  sh:sparql [
    sh:severity sh:Violation ;
    sh:message "TEXT_SPAN DataItem must reference a PassageTarget with oa:exact or section_locator." ;
    sh:select """
      PREFIX stroscot: <https://stroscot.org/schema/assurance/>
      PREFIX oa: <http://www.w3.org/ns/oa#>
      SELECT $this WHERE {
        $this stroscot:data_type "TEXT_SPAN" .
        FILTER NOT EXISTS {
          $this stroscot:passage ?p .
          ?p stroscot:source_document ?doc .
          { ?p oa:exact ?q } UNION { ?p stroscot:section_locator ?loc }
        }
      }
    """ ;
  ] .

# 3. Every NUMERICAL_DERIVATION DataItem must expose its formula and prov:wasDerivedFrom inputs.
stroscot:NumericalDerivationShape a sh:NodeShape ;
  sh:targetClass sepio:DataItem ;
  sh:sparql [
    sh:severity sh:Violation ;
    sh:message "NUMERICAL_DERIVATION DataItem must specify formula and prov:wasDerivedFrom inputs." ;
    sh:select """
      PREFIX stroscot: <https://stroscot.org/schema/assurance/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      SELECT $this WHERE {
        $this stroscot:data_type "NUMERICAL_DERIVATION" .
        FILTER (NOT EXISTS { $this stroscot:formula ?f } || NOT EXISTS { $this prov:wasDerivedFrom ?in })
      }
    """ ;
  ] .

# 4. Every AdoptionDecision must record rationale, unresolved_obligations, and revisit_triggers.
stroscot:AdoptionDecisionShape a sh:NodeShape ;
  sh:targetClass stroscot:AdoptionDecision ;
  sh:property [ sh:path stroscot:rationale ; sh:minCount 1 ; sh:minLength 10 ] ;
  sh:property [ sh:path stroscot:unresolved_obligations ; sh:minCount 1 ] ;
  sh:property [ sh:path stroscot:revisit_triggers ; sh:minCount 1 ] .

# 5. Every SACM AssertedEvidence source must be an ArtifactReference (OMG SACM v2.3 Clause 11.15 OCL).
stroscot:SACMAssertedEvidenceShape a sh:NodeShape ;
  sh:targetClass sacm:AssertedEvidence ;
  sh:property [
    sh:path sacm:source ;
    sh:minCount 1 ;
    sh:class sacm:ArtifactReference ;
  ] ;
  sh:property [
    sh:path sacm:target ;
    sh:minCount 1 ; sh:maxCount 1 ;
  ] .

# 6. Every ExecutionEvidence artifact must pin candidate_commit, contract_revision, and commands.
stroscot:ExecutionEvidenceShape a sh:NodeShape ;
  sh:targetClass sacm:Artifact ;
  sh:property [ sh:path stroscot:candidate_commit ; sh:minCount 1 ; sh:pattern "^[0-9a-f]{7,40}$" ] ;
  sh:property [ sh:path stroscot:contract_revision ; sh:minCount 1 ] ;
  sh:property [ sh:path stroscot:commands ; sh:minCount 1 ] .
```

#### Schema E: Agent Task Contract (`schemas/task_contract.schema.yaml`)

```yaml
# Example instance conforming to the Stroscot Task Contract schema
task_id: task/memo-family-comparison-01
mode: investigation # investigation | prototype | baseline_impl | qualification | maintenance
maturity_stage: EXPLORATORY_PROTOTYPE
model_tier: tier_3_strong_reasoning
question_or_capability: >
  Compare candidate redex-family identity definitions FAM-A and FAM-B
  against needed-evaluation sharing patterns.
consumes:
  - id: stroscot:contract/obs-v1
    revision: git:9a4f1c2
  - id: stroscot:oracle/python-ref
    revision: git:9a4f1c2
produces:
  - counterexamples_or_agreement_report
  - reproducible_test_corpus
  - proposed_decision
protected_read_only:
  - tests/oracle/
  - schemas/stroscot_shapes.ttl
  - lean/Stroscot/KernelSoundness.lean
allowed_workspace:
  - experiments/memo_family/
required_evidence:
  - execution_log_with_commit_sha
  - reduced_counterexamples_if_any
budget:
  max_agent_cost_usd: 25.00
  max_wall_minutes: 45
stop_when:
  - "Decisive counterexample found distinguishing FAM-A and FAM-B"
  - "Full comparison matrix completes on corpus"
  - "Missing semantic rule prevents interpretation (escalate)"
  - "Budget exhausted"
promotion_authority: human_only
```

---

## 8. Zotero, Better BibTeX & Repository Integration

Zotero acts strictly as the **bibliographic, PDF, and reading-annotation interface**, never as a second editable database for engineering decisions or epistemic claims.

### 8.1 Ownership & Synchronization Topology

```text
┌────────────────────────────────────────────────────────────────┐
│ ZOTERO DESKTOP / WEB LIBRARY (Authoritative Source Layer)      │
│  • Metadata, DOIs, collections, workflow tags (status/*,role/*)│
│  • Managed PDF attachments + reader highlights/annotations     │
│  • Better BibTeX (pinned citekeys) ──► auto-export csljson/bib │
└──────────────────────────┬─────────────────────────────────────┘
                           │ One-way incremental sync (API v3)
                           │ ?format=versions&since=<v> & /deleted?since=<v>
                           ▼
┌────────────────────────────────────────────────────────────────┐
│ GIT ASSURANCE & CODE REPOSITORY (Authoritative Knowledge Layer)│
│  • zotero/snapshot.json (pinned item keys, versions, checksums)│
│  • assurance/statements/ (SEPIO Statements, DataItems, Targets)│
│  • assurance/cases/      (SACM ArgumentPackages & Artifacts)   │
│  • docs/decisions/       (ADRs & ApplicabilityBridges)         │
│  • schemas/              (LinkML + SHACL validation in CI)     │
└──────────────────────────┬─────────────────────────────────────┘
                           │ Optional guarded backlink write-back
                           │ (If-Unmodified-Since-Version + child note)
                           ▼
             Zotero Read-Only Backlink Note
             ("Used by Stroscot: ADR-..., Claim-...")
```

### 8.2 Authoritative Data Ownership

| Artifact / Field | Authoritative Owner | Sync Direction |
| :--- | :--- | :--- |
| Title, authors, venue, DOI, Better BibTeX citekey | **Zotero** | Zotero → Repo (`csljson` / `biblatex` generated) |
| PDF attachments & reading highlights/comments | **Zotero** | Zotero → Repo (metadata/text/coords + SHA-256) |
| Extracted SEPIO `Proposition`, `DataItem`, `Statement` | **Git Repo** | Repo authoritative; read-only summary backlink optional |
| `ApplicabilityBridge`, `AdoptionDecision`, `PassContract` | **Git Repo** | Repo authoritative |
| Live SQLite (`zotero.sqlite`) & PDF binaries | **Local + Independent Backup** | Never committed to public Git |

### 8.3 Zotero Web API v3 Sync & Web Annotation Mapping Schema

Zotero Web API v3 (`https://api.zotero.org` or local desktop API `http://localhost:23119/api/`) requires `Zotero-API-Version: 3` (header) and `Zotero-API-Key` for private/group libraries.

#### Zotero PDF Annotation JSON (Input from `/items/<attachmentKey>/children`)
```json
{
  "key": "NBN6MQF4",
  "version": 4065,
  "itemType": "annotation",
  "parentItem": "QCWSFB2N",
  "annotationType": "highlight",
  "annotationText": "Cut admissibility holds by structural induction on the cut formula and derivations.",
  "annotationComment": "Check if extension X breaks principal-cut rank reduction.",
  "annotationColor": "#ffd400",
  "annotationPageLabel": "3",
  "annotationSortIndex": "00002|002000|00549",
  "annotationPosition": "{\"pageIndex\":2,\"rects\":[[108,233.484,209.539,242.51]]}",
  "tags": [],
  "relations": {},
  "dateModified": "2026-11-10T14:22:00Z"
}
```

#### Deterministic Mapping to W3C Web Annotation + SEPIO `DataItem`
```yaml
id: stroscot:dataitem/pfenning94-cut-adm
type: sepio:DataItem
data_type: TEXT_SPAN
value: "Cut admissibility holds by structural induction on the cut formula and derivations."
reported_in:
  id: stroscot:doc/pfenning1994
  type: sepio:Document
  document_type: PRIMARY_LITERATURE
  fabio_type: fabio:TechnicalReport
  citekey: Pfenning1994StructuralCut
  zotero_ref:
    library_type: group
    library_id: "5123456"
    item_key: "ABCD2345"
    imported_version: 4065
    attachment_key: "QCWSFB2N"
    attachment_sha256: "e3b0c44298fc1c149afbf4c8996fb924..."
    annotation_key: "NBN6MQF4"
passage:
  type: oa:SpecificResource
  source_document: stroscot:doc/pfenning1994
  page_label: "3"
  section_locator: "Theorem 1 (Structural Cut Elimination)"
  fragment_value: "page=3"
  quote_exact: "Cut admissibility holds by structural induction on the cut formula and derivations."
  zotero_annotation_key: "NBN6MQF4"
  stroscot:zotero_annotation_position:
    pageIndex: 2
    rects: [[108, 233.484, 209.539, 242.51]]
```

### 8.4 Sync Protocol & Change-Impact Rules

1. **Incremental Pull:** Fetch `<prefix>/collections/<key>/items?format=versions&since=<last_lib_ver>&includeTrashed=1` and `<prefix>/deleted?since=<last_lib_ver>`. Respect `Backoff` / `Retry-After` headers and cache `Last-Modified-Version`.
2. **Safe Write-Back (Optional Backlinks Only):** Use `If-Unmodified-Since-Version: <version>` (or JSON `version` property; `version: 0` / `Zotero-Write-Token` for new child notes) to prevent overwriting concurrent Zotero edits (`412 Precondition Failed` triggers re-fetch).
3. **Change-Event Classification:**

| Zotero Event | Automated Graph Response |
| :--- | :--- |
| Author/title/venue metadata fixed | Update `Document` display fields; **no** epistemic reassessment. |
| Attachment SHA-256 changed / new PDF edition | Mint new `Document` artifact version; mark dependent `PassageTarget`s & `Statement`s `NEEDS_REASSESSMENT`. |
| Highlight text or comment edited | Flag linked `DataItem` and dependent `EvidenceLine`s `NEEDS_REASSESSMENT`. |
| Item deleted or trashed | **Never delete graph records.** Mark `zotero_ref.unavailable: true`; retain historical provenance. |
| Agent extracts new passage | Create `Statement` with `checking_status: UNCHECKED`, `disposition: CANDIDATE`; never overwrite human-reviewed records. |

4. **Backup Policy:** Sync is not backup. Periodically back up the closed Zotero data directory (`zotero.sqlite` + `storage/`) independently of Zotero Cloud and Git.

---

## 9. Repository Layout & Implementation Sequence

### 9.1 Concrete Repository Structure

```text
stroscot/
├── docs/
│   ├── capabilities/         # M1–M8 Capability Contracts & Product Maturity Targets
│   ├── contracts/            # PassContracts, IR schemas, Trust Ledger
│   ├── decisions/            # ADRs (AdoptionDecision) & ApplicabilityBridges
│   └── handoffs/             # Friday session re-entry notes
├── schemas/
│   ├── context.jsonld
│   ├── stroscot_assurance.yaml   # LinkML profile (SEPIO + SACM + PROV + OA + Zotero)
│   ├── stroscot_shapes.ttl       # SHACL epistemic/structural CI validators
│   └── task_contract.schema.yaml # Agent task & gate schema
├── assurance/
│   ├── statements/           # SEPIO Statements, Propositions, EvidenceLines, DataItems
│   ├── cases/                # SACM AssuranceCasePackages per milestone
│   ├── code_bindings.yaml    # Single authoritative code ↔ contract ↔ decision map
│   └── trust_ledger.yaml     # Explicit unverified/trusted boundaries
├── zotero/
│   ├── library.csl.json      # Generated one-way by Better BibTeX
│   ├── sync_state.json       # Last-Modified-Version + pinned keys/checksums
│   └── sync_bridge.py        # Zotero API v3 → DataItem/PassageTarget importer
├── planning/
│   ├── dependencies.yaml     # Information-dependency graph & DSM clusters
│   ├── tasks.yaml            # Kanban board state
│   └── psp_time_log.csv      # Personal hours by activity & accepted vertical slice
├── lean/                     # Lean 4 metalogic (kernel soundness, cut admissibility)
├── experiments/              # Isolated prototypes (memoisation, search heuristics)
├── tests/
│   ├── oracle/               # Protected Python semantic reference & differential runner
│   └── conformance/          # Positive, negative, interaction, and bootstrap tests
└── src/                      # Stroscot compiler, linear-logic kernel, Layer 0 runtime
```

### 9.2 Immediate Phased Execution Plan

1. **Freeze the Restricted Self-Hosting Contract (`docs/capabilities/M5-restricted-selfhost.md`):** Explicitly exclude tiered JIT/de-optimisation and non-compiler surface extensions from the bootstrap dependency closure.
2. **Protect the Python Oracle (`tests/oracle/`):** Freeze positive, negative, and effectful acceptance cases for the bootstrap subset and run them as the read-only differential baseline.
3. **Implement the Minimal Object-Level Linear Checker (`src/kernel/`):** Separate proof-certificate checking from heuristic search; optionally mirror the core judgment in `lean/` to verify checker soundness on the restricted calculus.
4. **Stand Up the Minimal Assurance Pipeline:** Deploy `schemas/stroscot_assurance.yaml` + `schemas/stroscot_shapes.ttl` in CI, import one Zotero collection via `zotero/sync_bridge.py`, and trace **one complete vertical thread** (paper passage → `ApplicabilityBridge` → `AdoptionDecision` → `PassContract` → `CodeBinding` → `ExecutionEvidence` → `SACM Claim`).
5. **Begin PSP Slice Tracking (`planning/psp_time_log.csv`):** Log the next 100–200 project hours against completed vertical slices and update the correlated risk-impact budget from observed Stroscot data.

---

## References (Primary Specifications & Sources)
- **SEPIO LinkML & VA-Spec Profiling Methodology:** [sepio-framework/sepio-linkml](https://sepio-framework.github.io/sepio-linkml/),,; [GA4GH VA-Spec Statement & EvidenceLine](https://va-spec.ga4gh.org/),,
- **OMG Structured Assurance Case Metamodel (SACM) v2.3:** [Formal/23-05-08 PDF](https://www.omg.org/spec/SACM/2.3/PDF)
- **W3C PROV-O:** [https://www.w3.org/TR/prov-o/](https://www.w3.org/TR/prov-o/)
- **W3C Web Annotation Data Model:** [https://www.w3.org/TR/annotation-model/](https://www.w3.org/TR/annotation-model/),,
- **W3C SHACL:** [https://www.w3.org/TR/shacl/](https://www.w3.org/TR/shacl/)
- **SPAR Ontologies (FaBiO & CiTO):** [http://purl.org/spar/fabio/](https://sparontologies.github.io/fabio/current/fabio.html), [http://purl.org/spar/cito/](https://sparontologies.github.io/cito/current/cito.html),
- **OMDoc:** [OMDoc Specification](https://www.omdoc.org/about/)
- **KBpedia KKO:** [https://www.kbpedia.org/docs/kko-upper-structure/](https://www.kbpedia.org/docs/kko-upper-structure/)
- **Zotero Web API v3:** [Basics](https://www.zotero.org/support/dev/web_api/v3/basics), [Types & Fields](https://www.zotero.org/support/dev/web_api/v3/types_and_fields), [Syncing](https://www.zotero.org/support/dev/web_api/v3/syncing), [Write Requests](https://www.zotero.org/support/dev/web_api/v3/write_requests)
