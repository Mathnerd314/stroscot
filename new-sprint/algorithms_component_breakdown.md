# Overview 

pretty and slightly more up to date version: https://docs.google.com/document/d/1dU7rOA4sHvxigsqyu_OEyhvxpQKlFFlw2ZtiL8o9uac/edit?tab=t.0#heading=h.sbujs0c4cs3k

So first we must distinguish systems:
- system architecture: commonly identified by the "triple" of CPU architecture, operating system, and environment, although in practice there are sub-architectures and various configuration options for specializing the architecture.
- host system: the system architecture of the system currently executing the tool or compiler
- target system: the system architecture to target for JIT code generation and the final distributed image

The language semantics specifies how a source program will run on the target system ("run time behavior"). In particular, the semantics defines two things:
- a "naive" P-code machine (a binary built in machine code of the target system), implementing input and output operations
  - Memory and reference manipulation
  - Process virtualization
  - Exceptions, Traps, and Error Handling
- a representation of the program as a mathematical function that specifies the behavior the P-code machine should execute
  - The mathematical function is specified in terms of a reduction engine, a specific way to set up and evaluate the IR.
  - This function can be formalized in code as a Python program, essentially a Stroscot language interpreter when combined with the P-code machine

In practice, the image running on the target system is more complex, and will include:
- cache of optimized code and IR
- stitching logic to link cache entries together (may be optimized to direct machine code jumps, or incorporate JIT engine logic with guard, safepoint, and deoptimization code paths)
- runtime profiling and instrumentation code
- optimized P-code implementation

The protocol between the host and the target can be decomposed as various operations. Two are essential even when distributing a static, ahead-of-time image:
- deploy: transfer and execute an image from host to target
- profile: send profiling data from target to host

The others make sense with synchronous JIT execution, and would not be feasible with ahead-of-time compilation.
- update: send optimized code from host to target
- escape: send P-code response data from target to host
- download: send P-code data from host to target

The optimized IR and code sections are produced on the host system. Components include a tier controller and tiered execution modes, including a fast-start abstract interpreter and an optimizing compiler.

The abstract interpreter constructs a control flow graph of the program. It constructs symbol tables, resolves names, and identifies which parts of the program are static and which must be retrieved by P-code operations on the target system. It makes decisions using the optimization objective engine as to how to evaluate the program, such as evaluating and optimizing static portions of the program to residual constants, propagating information through the IR, graph-based supercombinator reduction and supercompilation, and removing dead code through high-powered analysis with the verification and logic engine. A term rewriting and strategy module implements language semantics and rewrite-based optimizations. The partial evaluator can operate with access to the target system and make decisions on the fly (internal process communication, if host is target, or through the network), or can operate based on a baseline performance heuristics. This is similar to the traditional online/offline distinction but in practice the abstract interpreter preprocesses the IR to some extent regardless. For example, files can be specified to be read at "compile time" from the host system. The "runner" and "bundler" components of the lange semantics specify the precise split between data included from the host vs. data included from the target (the "bundler" makes host data appear on the target, while the "runner" specifies that the operation must be run on the target).

The verification and logic engine integrates with the abstract interpreter for verification of safety and temporal and properties over the IR, similar in spirit to CPAchecker but extended. It uses an SMT/SAT and logic engine as well as model checking techniques such as Counterexample-Guided Abstraction Refinement, predicate abstraction, coarsening, approximation, search strategies, acceleration, and smart fuzzing. There is also invariant checking between compoennts which ensures contracts are maintained and cleans up IR when needed.

The optimizing compiler takes IR code annotated with control flow graph information and profiling information and produces code. It can run with incomplete or missing profiling information and uses a baseline performance analysis of the target to fill in the gaps. It makes decisions according to a tuneable optimization objective engine, which can lead to low-overhead baseline copy-and-patch compilation or the incorporation of advanced tracing and loop analysis for hot loops. It uses a code generator component to generate machine code for the target system. Code is generated with guards for speculative assumptions and maintains deoptimization paths back to safer tiers.

The tier controller manages the overall decisions of how to apply the abstract interpreter and optimizing compiler. It is also responsible for constructing the image to offload to the target system. The tier controller incorporates an incremental and parellel build engine to make use of multicore systems and efficient compile and execute large projects.

Bootstrapping the toolchain:
- The Stroscot interpreter written in Python and the P-Code machine are used to interpret the host-based compilation system, effectively resulting in a slow but usable compilation and partial evaluation system.
- This interpreter-based PE system is applied to itself, producing an optimized PE system targeted for the host.
- This optimized PE system is used as a generating extension generator and applied to a Stroscot interpreter (written in Stroscot) as the source program, to generate the final host system toolchain supporting all language features
- This whole process is wrapped up in a build system for building the compiler

In addition to the main execution toolchain, there are hooks for interactive execution, stepping, time-travel debugging, and notebooks. A test suite includes golden tests and benchmarks for regression detection. There is a general compiler feature flag and diagnostic system for testing new features, debugging output, configuring optimizations, etc.

# Component List

## Layer 0 – Language Semantics / Semantic Foundation

These components define how the source program runs on the target system — the "naive" P-code machine plus the mathematical function (reduction engine + IR) over it.

***

### 0.1 P-Code Machine (Target Runtime)

**Function:** A binary built in machine code of the target system forming the operational basis of `run`. Implements input/output operations, memory/reference manipulation, process virtualization, and exception/trap/error handling.

**Inputs**

- Bytecode or graph IR programs.
- Embedded assembly blocks (user-provided or generated stubs).
- Instruction database (e.g. XED) for decoding, including hex-encoded undocumented instructions.

**Processing**

- Conceptual top-level loop: fetch IR command (symbol + arguments + continuation-like function), dispatch, evaluate, invoke continuation.
- For inline assembly:
  - Decode/decompile using instruction database.
  - Translate to internal IR and/or directly execute in a sandbox “literal interpreter” that tracks side-effects.
  - Special handling for syscalls, traps, out-of-memory: convert to language-level values/exceptions.
- Integrate with runtime profiler to count calls, branches, traps.

**Outputs**

- Program results and side effects.
- Execution traces and counters (hot blocks, branch frequencies, trap stats) for profiling/JIT.

**Key references**

- EaRing style "dynamic assembler" https://github.com/zedshaw/earing
- https://github.com/aguinet/dragonffi for generating stubs
- XED library https://intelxed.github.io/
- [ar5iv.labs.arxiv](https://ar5iv.labs.arxiv.org/html/2011.03516)


### 0.1.1 Memory & Reference Representation

**Function:** Implement heap allocators and reference types (opaque pointers, arenas, worlds) with higher-level semantics that support transparent Hudak aggregate update optimization.

- **Input**
  - Allocation/deallocation requests tagged with memory type (private/shared/TLS/enclave).
  - Safety policies (stack smashing protection, hardened heaps).
  - Program IR marking aggregates (arrays, records) and update patterns.

- **Processing**
  - Use appropriate allocator strategy per memory type/kind.
  - Track concrete metadata for GC and debuggers
  - Track abstract metadata for abstract interpretation/verification (ownership, region, lifetime).
  - Analyze patterns of functional updates to aggregates. Hudak aggregate update optimization/compiler analysis (hereafter called "automatic destructive update"). Prove when updates can be implemented in-place without changing semantics. [isp.uni-luebeck](https://isp.uni-luebeck.de/sites/default/files/publications/CGO22_AggregateUpdateProblemDataflow_1.pdf) aggregate-update analysis affects what can safely be transformed into destructive updates. [dl.acm](https://dl.acm.org/doi/10.1145/318593.318660)
  - Integrate with “worlds”/arena abstractions for memory virtualization and C FFI.

- **Output**
  - Stable references to objects and aggregates (pointers/references).
  - Region/world descriptors supporting snapshot/restore and world merging.
  - Transformation decisions for aggregates (persistent vs. in-place).

**Key references**

- Hudak & Bloss, “The aggregate update problem…” [dl.acm](https://dl.acm.org/doi/10.1145/318593.318660)
- https://users.cs.northwestern.edu/~chrdimo/pubs/vmcai09-dw.pdf

### 0.1.2 Process Virtualization

**Desired function**  
Represent OS process state as a pure, deterministic value and realize it via CRIU-like mechanisms.

**Futamura role**  
Part of the effectful semantics of `run`; allows \(i\) and \(p_x\) to manipulate whole processes “symbolically” and then realize them.

**Inputs**

- Abstract description of process state (Fds, mappings, env, signals, etc.).

**Processing**

- Model process as a structured value / deterministic data structure.
- Allow transformations and optimizations on this structure (e.g., remove redundant operations).
- Use CRIU-like APIs to “realize” snapshots into real OS processes.

**Outputs**

- First-class process values / process state components.
- Realized OS processes when requested.

### 0.1.3 Exceptions, Traps, and Error Handling

**Desired function**  
Lift machine-level traps and other low-level failures into structured language-level exceptions and logic semantics with consistent unwinding.

**Futamura role**  
Shapes the error component of `run`; must be preserved under all projections and compilation.

**Inputs**

- Machine traps, signals, syscall error codes
- assembly/C/FFI stubs
- Unwinding tables, frame metadata.
- interpreted and compiled code segments

**Processing**

- Catch at assembly/C boundary; map to language-level exception values.
- Integrate with logic semantics (“choice between success and exception resolves to success” where allowed).
- Manage unwinding tables, return codes, stack reification for debuggers and abstract values.
- Maintain consistent unwinding semantics across interpreted and compiled frames.

**Outputs**

- Structured exceptions at language level.
- Stack inspection/unwinding behavior usable by debugger, profiler, and verification.
- Consistent unwinding semantics across compiled and interpreted code.

***

### 0.2 Graph-Based Optimal Reduction Engine and IR

**Goal:** Represent the program as a mathematical function over the P-code machine,  serving as the *concrete implementation* of the semantic core of the Stroscot language interpreter. This defines the "specification" of behavior when combined with the P-code machine. All projections specialize/compile this semantics; they don’t replace it.

**Implementation:** Execute and transform programs represented as graphs with explicit sharing and duplication control (Lamping-style, linear logic). Supports sea-of-nodes, SSA, traced paths. 

**Inputs**

- Graph IR: nodes as combinators/primitives; edges for data/control; labels and boxes for sharing/duplication.
    - Flexible/extensible IR: combination of sea-of-node, SSA, traced paths, etc. - lived observation is "IR's don't have to be clean, they can be as messy as you want. they just have to express everything you want to express"
- Reduction rules (beta-reduction, primitive operations, control constructs).
- Parsed AST with lexical scopes and imports, poset-style declaration constraints.
- Optional annotations for CPS/join points.

**Processing**

- Maintain bidirectional, locally-nameless graph with named “chunks” for modularity.
- Use label-based hash-consing to detect/share common subgraphs.
- Apply linear-logic-style rules (boxes, contraction/weakening) to control duplication and garbage.
- Support modes: “active” interpreter-style nodes vs. “frozen” compiled representation.
- Optionally perform local normalization/cleanup (e.g., trivial join simplification, small CSE).

**Outputs**

- Reduced graph (normal form or partially reduced with suspended redexes).
- fully resolved IR with symbol IDs
- diagnostics for unresolved names or incoherent poset constraints.
- Metrics: sharing statistics, duplication counts, reduction logs (for profiling).

**Key references**

- Optimal reduction & sharing (Lamping, etc.).
- GHC supercombinators (Peyton Jones)Reduction Engine & Graph-Based IR

### 0.2.1 Term Rewriting & Strategy Module

**Goal:** Implement language semantics, macro systems, and rewrite-based optimizations using higher-order pattern matching, strategy control (outermost-first, inner-strict, tactic combinations), confluence checks, and support for infinitary/regular infinite terms.

**Futamura role**  
Implements the reduction steps of the specializer, a sub-component of the partial evaluation engine (2.2), also used by 0.1 (the graph reduction engine) for primitive evaluation rules.

- **Input**
  - Rewrite rules with patterns, guards, and conditions.
  - Terms (including higher-order, regular infinite trees, suspended computations, "isolated" terms like a polynomial and an interval that identifies a root).

- **Processing**
  - Perform higher-order pattern matching; evaluate alternatives via logic engine, guards via meta-programming.
  - Strategy control: outermost-first, strict inner-first, tactic combinations.
  - Confluence checks where possible; if confluent, allow more aggressive strategy choices.
  - Termination/normalization checks (when provable) or watchdogs/limits when not.
  - Support infinitary/regular infinite terms, cycle condensation, meaningless term detection.

- **Output**
  - Normal form or partially normal form.
  - Information about convergence, divergence, or ambiguity.

### 0.2.2 Symbol Tables & Name Resolution (Including Posets)

**Desired function**  
Resolve names over scope chains, modules, and poset-style declarations; solve partial-order constraints via a bespoke constraint solver; detect ambiguities and cycles.

**Inputs**

- Parsed AST with lexical scopes, imports, modules.
- Poset-style declaration constraints across files (partial order, meet/join operations).

**Processing**

- Build scope chains, resolve names to declarations.
- Solve poset constraints via a bespoke constraint-solver:
  - Multiple declarations related by partial orders; compute consistent meets/joins when required.
- Detect ambiguities, cycles, or unsatisfied constraints.

**Outputs**

- Fully resolved AST/IR with symbol IDs.
- Diagnostics for unresolved/ambiguous names or incoherent poset constraints.
- **Symbol Tables & Name Resolution** — 

***

## Layer 1 – Deployed Image (Target-Side)

These are the components present in the image running on the target system — beyond a minimal "naive" implementation, the deployed image includes optimized code and stitching/runtime logic.

***

### 1.1 Tiered IR & Code Cache

**Function:** Maintain the cache of optimized code and IR on the target, deduplicated and partitioned by granularity (instruction, block, method, trace) and "ways" (runtime configurations). Provides the basis for "image mode" where all code is preloaded.

**Inputs**

- Interpreter \(i\).
- Profiler output
- IR fragments and residual programs
- Compiled code objects (baseline and optimized)
- hashes/binding-time signatures
- "ways" / Runtime configuration or system options (JIT allowed, profiling enabled, image mode)
- State per entry: empty, profiling, compiled, invalidated.

**Processing**

- Partition by granularity and ways
  - Granularity: instruction, block, method, trace.
  - Ways: runtime configurations (profiling, heap layout, etc.).
- Tier 0: interpret cold/unprofiled code.
- Tier 1: when hot, emit quickly compiled code via template-based copy-and-patch JIT.
- Maintain simple but stable cache mapping IR fragments to compiled code object.
- Associate IR fragments with code objects via stable enough mappings for debugging/deopt (not necessarily 1–1 after aggressive transforms).
- Handle deopt back to interpreter in simple cases.
- Check whether precompiled entries are valid for current program version and ways.
- Evict/demote cache objects based on usage/memory

**Outputs**

- Active code pointers for hot functions.
- Baseline compiled code cache.
- Cache hits/misses statistics
- Canonical IR/code reuse across runs and configurations
- Preloaded executable image with optional reduced capabilities (no dynamic code execution, no profiling, tree-shaken).

**Key references**

- JIT compilation basics and tiering patterns (e.g., JVM/JS engines). [en.wikipedia](https://en.wikipedia.org/wiki/Just-in-time_compilation)


### 2.7 Tiered IR & Code Cache Manager

***

### 1.2 Stitching Logic & Guards/Deoptimization

**Function:** Link cache entries together. Insert guards for speculative assumptions and maintain deoptimization paths back to safer tiers. May be optimized to direct machine code jumps, or incorporate JIT guard, safepoint, and deoptimization code paths back to safer tiers. Ensures that compiled artifacts can remain sound even when runtime assumptions fail.

**Inputs**

- Optimized IR with embedded assumptions (types, bounds, targets).
- Mapping from optimized state to interpreter/baseline state.

**Processing**

- Insert guards at assumption points.
- Build deopt metadata:
  - For each guard, mapping from optimized frame to IR-level live values.
  - Target continuation in interpreter or lower tier.
- Optionally leverage verification engine to prove some guards redundant and remove them.

**Outputs**

- Guarded machine code.
- Deoptimization tables.
- Basis for “image mode” where all code is preloaded and JIT/profiling disabled. An executable may be generated with reduced capabilities, such as no dynamic code execution, no profiling, and all code pre-loaded into the executable. Not clear which exact configurations should be supported, or what subset of the language will work. Tree-shaking to find best coherent version of the program, links together code.

***

### 1.3 Runtime Profiling, Instrumentation, & Baseline Behavior Estimator

**Desired function**  
Collect execution traces and behavioral statistics on the target and send profiling data back to the host via the **profile** protocol operation. Build predictive statistical models of program behavior to drive tiering and specialization.

**Inputs**

- Sampling profiler streams (PC samples, stack traces).
- Optional inline counters.
- Performance time series (latency, throughput).
- Optional static throughput predictions from uiCA or similar models. [ar5iv.labs.arxiv](https://ar5iv.labs.arxiv.org/html/2310.13212)

**Processing**

- Estimate hotness of functions/loops/traces (call counts, time share).
- Perform changepoint/regime analysis on hotness and type distributions.
- Maintain guard failure and deopt statistics.
- Combine dynamic data with static throughput models like uiCA (or successors such as Facile) to predict bottlenecks and expected gains. [publikationen.sulb.uni-saarland](https://publikationen.sulb.uni-saarland.de/handle/20.500.11880/33280?locale=en)

**Outputs**

- Hot/cold classifications.
- Per-site behavior models (likely types, branch biases).
- Worklist entries for tier upgrades/deoptimizations (sent to host).

**Key references**

- Use high-level heuristics inspired by HotSpot/SELF/JS engines to decide when to tier-up/down.
- uiCA: accurate basic-block throughput prediction on Intel. [scidok.sulb.uni-saarland](https://scidok.sulb.uni-saarland.de/bitstream/20.500.11880/33280/1/3524059.3532396.pdf)
- copy hard-won Java HotSpot statistics, tune them, and create entirely new analysis - look at SELF, Javascript, PyPy, Java, luajit
- statistical analysis https://arxiv.org/pdf/1602.00602
- profiling, statistics, benchmarking - sampling profilers and approximating accuracy by deliberate slowdown

***

## Layer 2 – Host↔Target Protocol

These components implement the operations between host and target.

***

### 2.1 Deploy

**Function:** Transfer and execute an image from host to target. Applicable even in static, ahead-of-time scenarios.

**Inputs:**
- Compiled image (code cache + stitching logic + runtime)
- target system address/configuration.

**Outputs:** Running image on target.

***

### 2.2 Profile

**Function:** Send profiling data from target to host. Applicable even in static, ahead-of-time scenarios.

**Inputs:** Profiling counters, traces, and behavior models from Layer 1.3.

**Outputs:** Profiling data delivered to host-side tier controller and baseline behavior estimator.

***

### 2.3 Update (JIT-only)

**Function:** Send optimized code from host to target, replacing or augmenting entries in the target's code cache.

**Inputs:**
- Optimized version of the P-code machine native to the target, replacing the "naive" implementation for hot paths as directed by the tier controller. Generated by the JIT on-demand.

***

### 2.4 Escape (JIT-only)

**Function:** Send P-code response data from target to host when the target encounters a dynamic computation that must be evaluated by the host's partial evaluator.

***

### 2.5 Download (JIT-only)

**Function:** Send P-code data from host to target, providing dynamic data needed to continue target execution (the complement of escape).

***

## Layer 3 – Host-Side Compiler

These components run on the host system and produce the optimized IR and code to send to the target.

***

### 3.1 Abstract Interpreter

**Desired function** Provide a configurable abstract interpreter for safety and temporal properties over your IR, similar in spirit to CPAchecker but extended. [arxiv](https://arxiv.org/abs/0902.0019) Uses partial evaluation techniques (both online and offline) and constructs the control flow graph; resolves names and identifies static vs. dynamic parts; drives specialization, supercompilation, and optimization decisions. Implements the specializer \(s\) that builds residual programs \(p_x\) specialized on static input. Corresponds to the "fast-start abstract interpreter" tier.  [ar5iv.labs.arxiv](https://ar5iv.labs.arxiv.org/html/1611.09906) Realizes the **first Futamura projection**: specialize programs, including the interpreter \(i\). [courses.grainger.illinois](https://courses.grainger.illinois.edu/cs421/su2016/static/partial-evaluation-slides.pdf)


**Inputs**

- Target program \(p\) and data \(d\).
- Access to target (optional).
- BTA-annotated IR (2.1).
- Interpreter \(i\) and the graph engine (0.1–0.2).
- Static inputs \(x\) (configuration, compile-time files, etc.).
- Verification-ready IR (post-normalization/partial evaluation).
- Properties: safety, temporal formulas (including μ-calculus-style), concurrency properties. [di.ens](https://www.di.ens.fr/~cousot/publications.www/Cousot-NFM2012.pdf)- assertions, bounds, finalizers, reachability, termination, dead code, security/safety properties, deadlock/race conditions using memory model and concurrency model

**Processing**

- Interpret \(p\) via the graph engine + mini-VM + runtime.
- Call into literal assembly interpreter for inline assembly.
- Offline mode:
  - Use BTA to guide specialization and drive supercompilation/partial evaluation.
- Online mode:
  - Run a “quick start” interpreter; specialize as static data is encountered; suspend when dynamic interaction is needed (via escape/download protocol).
- Policy for host file reads: read during specialization, cache contents, respect security policies.
- Integrate logic engine and term rewriting to drive choice and guard evaluation.
- Use IR normalization as needed to satisfy invariants during execution.
- Evaluate and optimize static portions to residual constants.
- Integrates with term rewriting and strategy module for language-semantics-defined reductions.
- Concurrent and incremental analysis

**Outputs**

- Result value or exception for \(\mathrm{run}(p,d)\).
- Profiling traces (if enabled).
- Residual programs \(p_x\) with static parts inlined and removed.
- Specialized hot-loop versions for JIT (via compiler).

**Key references**

- Abstract interpretation for verification. [di.ens](https://www.di.ens.fr/~cousot/publications.www/CousotCousot-Marktoberdorf-2009.pdf)
- Partial evaluation and Futamura projections. [gist.github](https://gist.github.com/fredfeng/d48dee989cc3677090ea25e17d1ca246)

### 3.1.1 Binding-Time Analysis (BTA)

**Desired function**  
Classify expressions/values as static, dynamic, or partially static, and define abstract values for specialization.

**Futamura role**  
Core analysis used to implement the specializer \(s\) and generating extensions.

**Inputs**

- IR/AST with annotations for known compile-time inputs.
- Allowed host interactions (file I/O, etc.).
- Exception semantics (Layer 0.6).

**Processing**

- Dataflow fixed-point analysis propagating binding times.
- Build abstract values: static part + constraints on dynamic part.
- Track static computations that may raise exceptions; mark those as disallowed or special.

**Outputs**

- Binding-time annotated IR.
- Abstract value lattice for partial evaluator and JIT.

**Key references**

- Classic partial evaluation / BTA literature. [gist.github](https://gist.github.com/tomykaira/3159910)

### 3.1.2 Meta-Programming and AOP

**Goal:** Specialize macros/fexprs, and weave aspects at compile-time or runtime.

- **Input**
  - Programs with macro/f-expression constructs and aspect declarations (pointcuts, advice).
  - Staging/binding-time information.

- **Processing**
  - Specialize macro-like and function-like “operatives” using partial evaluation.
  - Resolve join points and weave advice into base code (statically or dynamically).
  - Fall back to general dynamic meta-evaluation when specialization fails.

- **Output**
  - Residual code with aspects woven and macros expanded as far as possible.
  - Remaining dynamic meta-level constructs for runtime.

- **Futamura Artifact Suite (`g_p`, `n`, runner `r`, bundler `b`)** — Library-level API for producing compilers, residual programs, generating extensions, compiler generators, runners, and bundled executables; defines the precise split between host-bundled data (bundler) and data that must be fetched from the target at runtime (runner).

**Processing:** Build CFG; propagate information through IR; graph-based supercombinator reduction and supercompilation; evaluate static portions to constants; propagate constants and value ranges; remove dead code via verification/logic engine; operate online with target access or offline with baseline heuristics.

**Outputs:** Annotated residual IR with static parts inlined; control flow graph; binding-time annotations; residual programs `p_x`.

***

### 3.2 Verification & Logic Engine

**Function:** Integrates with the abstract interpreter for verification of safety and temporal properties over the IR; also provides invariant checking between components that ensures contracts are maintained and cleans up IR when needed. Configurable and modular checking of safety and temporal properties over IR, similar in spirit to CPAchecker but extended. [arxiv](https://arxiv.org/abs/0902.0019)

**Inputs**

- Verification-ready IR (post-normalization/partial evaluation).
- Properties: safety, temporal formulas (including μ-calculus-style), concurrency properties. [di.ens](https://www.di.ens.fr/~cousot/publications.www/Cousot-NFM2012.pdf)- assertions, bounds, finalizers, reachability, termination, dead code, security/safety properties, deadlock/race conditions using memory model and concurrency model

**Processing**

- Integrate logic engine and term rewriting to drive choice and guard evaluation.
- Represent each abstract domain/analysis as a Configurable Program Analysis (CPA).
- Run configurable reachability over combinations of CPAs, maintaining an Abstract Reachability Graph (ARG). [arxiv](https://arxiv.org/pdf/0902.0019.pdf)
- Extend to handle mu-modal temporal logic (e.g., via automata-based constructions or fixpoint computations). [en.wikipedia](https://en.wikipedia.org/wiki/Abstract_interpretation)
- Concurrent and incremental analysis

**Outputs**

- Verdict: safe/unsafe/unknown.
- Counterexamples or proofs.
- Result value or exception for \(\mathrm{run}(p,d)\).
- Profiling traces (if enabled).
- Residual programs \(p_x\) with static parts inlined and removed.
- Specialized hot-loop versions for JIT (via compiler).

**Key references**

- CPAchecker and CPA framework. [github](https://github.com/sosy-lab/cpachecker)
- Abstract interpretation for verification. [di.ens](https://www.di.ens.fr/~cousot/publications.www/CousotCousot-Marktoberdorf-2009.pdf)
- Partial evaluation and Futamura projections. [gist.github](https://gist.github.com/fredfeng/d48dee989cc3677090ea25e17d1ca246)

### 3.2.1 CEGAR & Predicate Abstraction \(s^\sharp\)

**Desired function**  
Implement Counterexample-Guided Abstraction Refinement and predicate abstraction on top of the CPA framework.

**Futamura role**  
Specializer of \(i^\sharp\): effectively a program \(s^\sharp\) that specializes a generic abstract interpreter to a given program/property.

**Inputs**

- Program, property, initial coarse abstraction.

**Processing**

- Model-check the coarse abstraction; examine counterexamples.
- Check feasibility concretely; if spurious, compute interpolants and new predicates.
- Refine abstraction locally and resume.
- Apply lazy abstraction and local/predicate slicing.

**Outputs**

- Refined abstraction and either a real counterexample or a proof.

### 3.2.2 SMT/SAT & Logic Engine for Verification

**Desired function**  
Provide solvers for logical constraints from logic programming, invariants, language semantics, verification, and abstraction refinement.

**Futamura role**  
Shared infrastructure used by \(i^\sharp\) and \(s^\sharp\); could itself be viewed as an abstract interpreter on constraints.

**Inputs**

- Logical constraints from program states, invariants, and abstraction refinement.

**Processing**

- CDCL SAT with backjumping and learned clauses
- SMT with bit-vectors, arithmetic, arrays, ADTs.
- Model generation and unsat cores.
- Interpolation for CEGAR.

**Outputs**

- sat/unsat/unknown results.
- Models and interpolants.

### 3.2.3 Verification Meta-techniques & Portfolios

**Desired function**  
Systematize coarsening, approximation, search strategies, acceleration, and smart fuzzing.

**Futamura role**  
Meta-level machinery to choose and compose abstract interpreters/analyses (can itself be expressed in terms of higher-order “interpreters” over analyses).

**Inputs**

- State-space representations and property descriptions.
- Library of analyses/abstractions.

**Processing**

- Coarsen states into equivalence classes suited to each property.
- Approximate stronger/weaker properties for quick yes/no/unknown.
- Accelerate loops and transitions via summarization.
- Use heuristic exploration orders (“smart fuzzing”) to find bugs quickly.

**Outputs**

- Verification results with trade-offs.
- Reusable summaries/abstractions.

### 3.2.4 Invariant Checker & Adapter Layer

**Desired function**  
Maintain contracts between passes and repair IR when possible.

**Futamura role**  
Cross-cutting infrastructure ensuring all passes see IR satisfying their expectations.

**Inputs**

- IR before/after major passes.
- Invariant specifications per pass.

**Processing**

- Check invariants; on failure either:
  - Report diagnostics, or
  - Apply adapter transformations (e.g., reinsert primitives, re-normalize types).
- Track which “hard algorithms” require/produce which IR flavors.

**Outputs**

- Verdict (safe/unsafe/unknown); counterexamples or proofs
- IR satisfying downstream contracts.
- Reports on violations and repairs.

***

### 3.3 Optimizing Compiler

**Function:** Takes IR annotated with control flow graph and profiling information and produces machine code for the target system. Fills in missing profiling data from baseline target performance analysis. Makes decisions according to the tunable optimization objective engine.

### 3.3.1 IR Normalization & Cleanup Passes

**Desired function**  
Maintain IR invariants expected by downstream algorithms and tools.

**Futamura role**  
Normalizes \(i\)’s internal representation so specialization and compilation see a reasonably canonical IR.

**Inputs**

- IR graphs that may violate local invariants after transformations.

**Processing**

- Dead code elimination, unreachable block removal.
- Recursion-to-loop conversion when beneficial.
- Strength reduction for expensive operations.
- Defunctionalization where desired to separate control and data.
- Canonicalization of patterns, elimination of dangling edges, enforcing SSA/sea-of-nodes invariants.
- Code motion and other standard local optimizations
    - removal of unused parameters
    - replacement of parameters passed by reference by parameters passed by value.
    - replace standard functions with faster alternatives when possible
    - inlining
    - deduplication of constants, functions, code sequences (tail merging / cross jumping)
    - common subexpression elimination (CSE)
    - dead code/store eliminate (DCE/DSE)
    - conditional dead code elimination (DCE) for calls to built-in functions that may set errno but are otherwise free of side effects
    - global constant and copy propagation
    - constant propagation - which values/bits of values passed to functions are constants, function cloning
    - value range propagation - like constant propagation but value ranges
    - sparse conditional constant propagation (CCP), including bit-level
    - elimination of always true/false conditions
    - move loads/stores outside loops
    - loop unrolling/peeling
    - loop exit test
    - cross-jumping transformation
    - constant folding
    - specializing call dispatch (possible targets, likely targets, test/branch)
    - Code hoisting - evaluate guaranteed-evaluated expressions as early as possible
    - copy propagation - eliminate unnecessary copy operations
    - Discover which variables escape
    - partial/full redundancy elimination (PRE/FRE)
    - modified/referenced memory analysis, points-to analysis, aliasing
    - strips sign operations if the sign of a value never matters
    - convert initialization in switch to initialization from a scalar array
    - termination checking
    - loop nest optimizer based on the Pluto optimization algorithms. It calculates a loop structure optimized for data-locality and parallelism.
    - graphite - loop distribution, loop interchange, unroll, jam, peel, split, unswitch, parallelize, copy variables, inline to use first iteration values, predictive commoning, prefetch
    - final value replacement - loop to calculation using initial value and number of loop iterations
    - explode structures to scalars in registers
    - vectorization - loop vectorization, basic block vectorization, cost free (for debugging), likely faster, or code size
    - reorder blocks, duplicate blocks, partition into hot/cold to improve paging and cache locality
    - specialization of division operations using knowledge of the denominator


**Outputs**

- Canonical IR suitable for specialization, verification, and codegen.

### 3.3.2 Tunable Optimization Objective Engine

**Desired function**  
Provide a scalar cost for candidate codegen/optimization decisions, incorporating performance and resource objectives.

**Futamura role**  
Used by \(c\) (and by online JIT decisions) to select among multiple valid compiled results.

**Inputs**

- Metrics: estimated runtime (mean/tail), code size, compile time, memory usage, power, latency of key loops.
- Distribution parameters (e.g., or heavy-tail latency).
- User weights per metric.

**Processing**

- Normalize metrics to common scale.
- Combine via weighted linear combination or lexicographic ordering.
- Optionally include risk terms (variance, tail probabilities).
- Provide simple sensitivity analysis: contributions of each metric.

**Outputs**

- Scalar cost for search/heuristics.
- Sensitivity breakdown.
- drives copy-and-patch vs. advanced tracing/loop analysis decisions.

### 3.3.3. Unison-Style Global Code Generator (Compiler Core)

**Desired function**  
Solve instruction selection, register allocation, and scheduling in one integrated NP-hard model, with tunable optimization budgets. [github](https://github.com/unison-code/unison)

**Futamura role**  
Provides the low-level back-end used by the **compiler \(c\)** to map residual programs or interpreter specializations to machine code.

**Inputs**

- Low-level IR from partial evaluator or front-end.
- Target machine model (instructions, register classes, pipeline resources, ABI).
- Optional profile data and throughput predictions
  - edge frequencies, block hotness, branch statistics
  - uiCA/Facile [semanticscholar](https://www.semanticscholar.org/paper/uiCA:-accurate-throughput-prediction-of-basic-on-Abel-Reineke/79a5a38372bee32f6ad6c0d499ef8d9eded9e72f)
- Optimization weights (runtime vs. size vs. compile time vs. power).

**Processing**

- Build unified constraint model:
  - Instruction selection alternatives, register constraints, scheduling constraints, ABI constraints.
- Seed with heuristic solution (pattern matching + linear scan + list scheduling).
- Run combinatorial search (ILP/CP-SAT/branch-and-bound), using implied constraints and dominance rules as in Unison presolver. [past.date-conference](https://past.date-conference.com/date14/files/file/date14/ubooth/2592.pdf)
- Evaluate candidates via the tunable optimization objective engine (2.4).

**Outputs**

- Machine code per function/trace.
- Mapping IR → instructions/registers/schedule.
- Metadata: live ranges, spills, estimated cost.

**Key references**

- Unison, implied constraints for the presolver. [diva-portal](https://www.diva-portal.org/smash/get/diva2:862630/FULLTEXT01.pdf)

### 3.3.4 Tracing & Loop Analysis

**Function:** Identifies and extracts hot traces and loops for advanced optimization when the optimization objective warrants it; incorporates speculative inlining, prefetching, and cache shaping.

optimizing JIT with:
    - Method compilation.
    - Trace compilation for hot paths, using meta-hybrid policy to avoid pathological traces. [ar5iv.labs.arxiv](https://ar5iv.labs.arxiv.org/html/2011.03516)
    - Speculative optimizations: inlining, specialization, prefetching, cache shaping.

**Outputs:** Machine code per function/trace with guards; mapping of IR to instructions/registers/schedule; deoptimization metadata.

**Key references**

- Meta-hybrid JIT: amalgamating method and tracing JITs. [arxiv](https://arxiv.org/pdf/2011.03516.pdf)
- Cloud/offload JIT ideas similar to Azul.

***

### 3.4 Tier Controller

**Function:** Manages overall decisions about how to apply the abstract interpreter and optimizing compiler; also responsible for constructing the image to offload to the target and making use of multicore systems via an incremental and parallel build engine. Orchestrates multi-tier execution (interpreter, baseline, optimizing JIT) and distributed compilation à la Azul.

**Inputs**

- Profiler output.
- Unison-based compiler and partial evaluator.
- Precompiled code cache (local/remote).
- Configuration (tiers allowed, image modes, profiling).

**Processing**

- **Tiering Policy (Tiers 0–N)** — Coordinates tiers: Tier 0 (interpreter for cold/unprofiled code, JIT disablied), Tier 1 (baseline copy-and-patch JIT), Tier 2+ (optimizing JIT); triggers on-the-fly recompilation based on behavior shifts.
- Hybrid offload:
  - Ask remote “cloud compiler” to compile hot units.
  - Cache by IR hash + environment, Azul-style.
- Trigger on-the-fly compilation and re-compilation based on behavior shifts.
- **Image Construction & Offload** — Assembles the final image (code cache + stitching + runtime + profiling instrumentation) and dispatches it to the target via the deploy protocol; manages remote/cloud compiler offload (Azul-style, keyed by IR hash + environment) for hot compilation units.

**Outputs**

- Active code per function/trace.
- Updated distributed code cache.
- Constructed image ready for deploy.

### 3.4.1 Incremental/Parallel Build Engine

**Desired function**  
Coordinate builds, specializations, compilations, and packaging. Determines minimal recompilation sets from change sets; schedules parallel compilation tasks; invokes partial evaluator and compiler; reuses tiered IR & code cache (Layer 1.1); coordinates multicore for large projects.

**Futamura role**  
Orchestrator of calls to \(s\), \(c\), \(g_p\), \(n\), \(r\), \(b\).

**Inputs**

- Dependency graph of modules/functions.
- Change sets from VCS/timestamps.
- Configuration of target triples, ways, and optimization objectives.

**Processing**

- Determine minimal recompilation set.
- Schedule parallel compilation tasks.
- Invoke partial evaluator and compiler as needed.
- Reuse caches (Tiered IR & code cache, 2.7).

**Outputs**

- Updated compiler/runtime artifacts.
- Logs and dependency metadata.



***

## Layer 4 – Bootstrapping
**Desired function**  
Provide a principled, library-level API for producing compilers, residual programs, generating extensions, compiler generators, runners, and bundled executables from within the language. [arxiv](https://arxiv.org/abs/1611.09906)

**Futamura role**  
Implements the full Futamura stack around \(i\), \(s\), \(c\).

**Inputs**

- Interpreter program \(i\).
- Specializer \(s\) (2.2).
- Compiler pipeline \(c\) (2.3+2.4+2.5+2.6).
- User programs \(p\).

**Processing**

- Define:
  - \(p_x = \mathrm{run}(s,(p,x))\).
  - \(g_p\): \(\mathrm{run}(g_p,x) = p_x\).
  - \(n\): \(\mathrm{run}(\mathrm{run}(\mathrm{run}(n,p),x),y) = \mathrm{run}(p,(x,y))\). [ecommons.udayton](https://ecommons.udayton.edu/cgi/viewcontent.cgi?article=1087&context=cps_fac_pub)
  - Runner \(r\): \(\mathrm{run}(\mathrm{run}(r,c),(p,x)) = \mathrm{run}(\mathrm{run}(c,p),x)\).
  - Bundler \(b\): \(\mathrm{run}(b(c,p),x) = \mathrm{run}(\mathrm{run}(c,p),x)\).
- Expose them as library APIs.

**Outputs**

- Library functions for:
  - Compiling programs.
  - Specializing programs.
  - Generating compilers and generators.
  - Creating bundled executables.

**Key references**

- Diagrammatic and practical presentations of Futamura projections. [blog.sigfpe](http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html)

Let \( \mathrm{run} : \text{Program} \times \text{Data} \to \text{Result} \) be the master evaluator.

Classic Futamura artifacts (concrete semantics): [ecommons.udayton](https://ecommons.udayton.edu/cgi/viewcontent.cgi?article=1087&context=cps_fac_pub)

- **Interpreter \(i\)**: program implementing `run` such that \(\mathrm{run}(i,(p,d)) = \mathrm{run}(p,d)\).
- **Specializer \(s\)**: partial evaluator: \(\mathrm{run}(\mathrm{run}(s,(p,x)),y) = \mathrm{run}(p,(x,y))\).
- **Compiler \(c\)**: specialization of \(i\) w.r.t. programs: \(\mathrm{run}(\mathrm{run}(c,p),d) = \mathrm{run}(p,d)\). [ar5iv.labs.arxiv](https://ar5iv.labs.arxiv.org/html/1611.09906)
- **Residual program \(p_x\)**: \(\mathrm{run}(p_x,y)=\mathrm{run}(p,(x,y))\).
- **Generating extension \(g_p\)**: \(\mathrm{run}(g_p,x)=p_x\). [arxiv](https://arxiv.org/abs/1611.09906)
- **GNG \(n\)**: \(\mathrm{run}(\mathrm{run}(\mathrm{run}(n,p),x),y) = \mathrm{run}(p,(x,y))\). [ecommons.udayton](https://ecommons.udayton.edu/cgi/viewcontent.cgi?article=1087&context=cps_fac_pub)

Your **runner** and **bundler** are packaging around \(c\) (and \(p\)), not new projections; they’re included as higher-level artifacts.

On the **verification** side, there is an **abstract interpreter** \(\mathrm{run}^\sharp\) for properties, which fits the same pattern at the abstract level. [pcousot.github](https://pcousot.github.io/talks/CousotP_Slides_EDF_1-1.pdf)


The toolchain bootstraps itself in stages:

### 4.1 Python Interpreter + P-Code Machine (Stage 0 Seed)

**Function:** The Stroscot interpreter written in Python combined with the P-code machine interprets the host-based compilation system, producing a slow but usable compilation and partial evaluation system.

***

### 4.2 Self-Applied Partial Evaluator (Stage 1)

**Function:** The interpreter-based PE system is applied to itself, producing an optimized PE system targeted for the host. Implements the second Futamura projection applied to the seed.

***

### 4.3 Generating Extension Generator (Stage 2)

**Function:** The optimized PE system is used as a generating extension generator and applied to a Stroscot interpreter written in Stroscot as the source program, generating the final host-system toolchain supporting all language features.

***

### 4.4 Self-Hosting & Cross-Compilation

**Function:** Produce a compiler for the language written in the language itself, verifiable via multi-stage bootstrap (stage-0 → stage-1 → stage-2, byte-for-byte fixpoint check). Cross-compilation is the compiler instantiated with a different target triple. Seed maintenance mitigates "Trusting Trust" attacks. Goal is to compile itself for arbitrary targets without an external host compiler. [dev](https://dev.to/mortoray/what-is-self-hosting-and-is-there-value-in-it-2p9p)

**Inputs:**

- Source of the eight core Futamura artifacts written in the language itself (\(i\), \(s\), \(c\), \(g_p\), \(n\), \(r\), \(b\), plus residual program machinery).
- A **seed compiler**: minimal trusted external compiler capable of running the language (could be an interpreter; could be a compiled prior version).
- Host and target triples (build system, host system, target system — following the standard three-way distinction). [gnu](https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Hosts-and-Cross_002dCompilation.html)
- Compiler "ways": incompatible configurations for runtime, profiling, heap, etc.

**Outputs:** Self-hosted compiler binary per supported target; cross-compiler binaries for each (host, target) pair; reproducible bootstrap chain with hash records.

- \(c\) applied to the compiler's own source gives a new version of the compiler.
- \(b(c, \text{compiler source})\) gives a standalone self-hosted binary.
- Cross-compilation is \(c\) instantiated with a different target triple, producing a compiler artifact that runs on the host but targets another machine. [gnu](https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Hosts-and-Cross_002dCompilation.html)

**Processing**

- **Bootstrap stages** (following standard multi-stage bootstrap): [github](https://github.com/sysprog21/shecc)
  - Stage 0: compile compiler source using seed to produce stage-0 compiler binary.
  - Stage 1: run stage-0 compiler on compiler source to produce stage-1 binary.
  - Stage 2: run stage-1 compiler on compiler source to produce stage-2 binary.
  - Verify: check stage-1 and stage-2 outputs are byte-for-byte identical (or otherwise reproducibly equivalent) to confirm fixpoint.
- **Cross-compilation**: apply the same stages but configure the back-end (codegen, ABI, OS bindings) for a different target triple. The host compiler cross-compiles; the resulting artifact runs on the target.
- **Conditional compilation**: use compiler "ways" and target triple flags to include/exclude features (e.g., disable JIT for bare-metal targets, strip profiling for release images, enable secure-enclave memory mode).
- **Seed maintenance**: keep a seed compiler/interpreter at a known-good version; seed itself is periodically refreshed by a previous verified self-hosted build (Trusting Trust mitigation).

**Outputs**

- Self-hosted compiler binary for each supported target.
- Cross-compiler binaries for each (host, target) pair.
- Reproducible bootstrap chain (stage-N binaries, hash records).

**Depends on**

- 4.3 Futamura artifact suite (provides \(c\), \(s\), \(b\), \(n\)).
- 2.3 Global codegen (provides back-end code generation for targets).
- 0.7 Name resolution (must handle cross-compiled standard library declarations).
- 4.2 Build engine (orchestrates the multi-stage pipeline).

***

### 4.5 Build System

**Function:** Wraps up the entire bootstrapping process into an automated build pipeline for building the compiler itself, managing the multi-stage sequence, dependency tracking, and reproducibility.

build system for building the compiler

***

## Layer 5 – Developer Tooling & Meta

***

### 5.1 Interactive Execution & Debugger

**Function:** Hooks for interactive execution, stepping, time-travel debugging, and notebooks. Steps at source/IR/machine level; performs reversible debugging via snapshots/logs; supports hot edit-and-continue via JIT patching while preserving debugger state.

**Desired function**  
Provide interactive execution, stepping, time-travel debugging, and notebooks.

**Futamura role**  
Consumer of \(i\), \(p_x\), compiled code from \(c\), and verification information from \(i^\sharp\).

**Inputs**

- Running program, breakpoints, user commands.
- Mappings between source, IR, compiled code.
- Deopt and unwinding metadata.

**Processing**

- Step at source/IR/machine level.
- Perform reversible debugging via snapshots/logs.
- Hot “edit-and-continue” via JIT patching while preserving debugger state.

**Outputs**

- Interactive sessions and outputs.

***

### 5.2 Test Suite

**Function:** Golden tests and benchmarks for regression detection. Exercises compiler correctness (golden outputs), performance (benchmarks), and behavior under flag/configuration variations.

test suite - golden tests, benchmarks, regression detection

***

### 5.3 Compiler Feature Flags & Diagnostic System

**Function:** A general flag and diagnostic system for testing new features, controlling debugging output, configuring optimizations, and reporting errors and warnings. Provides pluggable/customizable syntax extensions and a general compiler "ways" configuration system.

compiler feature flags for testing new features, debugging output, etc. - general flag system for configuring optimizations etc.

error/warning reporting and diagnostics

***

### 5.4 Standard Library

**Function:** Concurrency operations, wait-free software transactional memory implementation, and other foundational runtime libraries needed by compiled programs.

standard library - concurrency operations,  wait-free software transactional memory implementation

