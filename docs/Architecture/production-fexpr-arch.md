# Production F-Expression Compilation Architecture
## Synthesizing Kraken, Fexpress, and JIT/Partial Evaluation Best Practices

---

## Executive Summary

This document presents a comprehensive, production-ready architecture for compiling first-class f-expressions (fexprs) by synthesizing insights from:

1. **Kraken** (Braswell et al., 2023): Online partial evaluation of fexprs with 70,000x speedup
2. **Fexpress** (rocketnia, 2021): Pragmatic type-driven fexpr compilation in Racket
3. **Academic JIT Research**: Meta-tracing (PyPy), method-based JIT (Graal/Truffle), partial evaluation theory
4. **Production JITs**: Java HotSpot (tiered compilation), Chrome V8 (speculative optimization), LuaJIT (tracing)

**Key Insight**: The fexpr compilation problem is fundamentally an instance of the well-studied **partial evaluation** and **JIT compilation** problem. By applying established terminology and techniques from this literature, we can design a production system that inherits decades of proven optimization strategies while addressing fexpr-specific challenges.

---

## Part I: Unified Terminology and Concepts

### A. Core Abstractions

#### 1. **Partially-Static Data Structures** (Central Concept)

Both Kraken and Fexpress represent the core challenge as managing data where parts are **known at compile/specialization time** and parts are **only known at runtime**.

**In JIT Literature:**
- **Static data**: Values/types known during JIT compilation
- **Dynamic data**: Values that depend on runtime inputs
- **Binding time**: Classification of when a value becomes known

**In Kraken:**
- Real environment frames (static data known)
- Fake environment frames (placeholder for dynamic data)
- Environment IDs distinguish the two

**In Fexpress:**
- Positive types with static values (specific-value/t+)
- Positive types with dynamic placeholders (any-value/t+)
- Type hints (negative types) guide specialization

**In Production JIT (HotSpot):**
- Inline caches (IC) track monomorphic/polymorphic call sites
- Type feedback records observed types (static-ish until proven wrong)
- Feedback vector aggregates profiling data

**Unified Term**: **Partially-Static Value Domain** – A structured representation where variables map to either:
- Concrete values (tagged STATIC)
- Type constraints (tagged DYNAMIC_TYPED)
- Completely unknown (tagged DYNAMIC_UNTYPED)

#### 2. **Progress/Availability** (Determining When Specialization Can Proceed)

**In Kraken:**
- `needed-for-progress` relation: Which environment IDs must the computation depend on?
- If `needed-for-progress ∩ current_environment_stack = ∅`, no progress possible
- Prevents infinite recursion in partial evaluation

**In Fexpress:**
- Type hints propagate through expressions
- If argument type is `any-value/t+`, can't fully specialize
- If argument type is `non-fexpr-value/t+`, can generate direct call

**In JIT Literature:**
- **Specialization predicate**: Can we generate specialized code for this expression?
- **Guard conditions**: What assumptions enable optimization?
- **Fallback paths**: What happens if assumptions fail?

**In Production JITs:**
- HotSpot: Type feedback enables specialization; uncommon types trigger deoptimization
- V8: Monomorphic/polymorphic feedback controls inlining decisions
- LuaJIT: Trace exits allow path-specific optimization

**Unified Term**: **Specialization Predicates and Guard Generation**

### B. Key Data Structures

#### 1. **Binding-Time Metadata**

**In Kraken:**
```
CombinerId: unique identifier per combiner definition
EnvironmentId: tagged (i_r=real | i_f=fake) with dependency info
ArrayMark: (val | freshCall | attemptedCall)
```

**In Fexpress:**
```
TypePlus: Symbolic value representation
TypeMinus: Optimization hints
FreeVars: Set of lexical variable references
```

**In JIT Literature (Abstract Interpretation):**
```
AbstractValue: Type/value abstraction
BindingTime: {static, dynamic}
GuardCondition: Assertion enabling optimization
```

**In Production JIT (HotSpot):**
```
FeedbackEntry: (call_count, receiver_type_profile, ...)
ICacheEntry: (receiver_class, method_reference, ...)
ProfileData: Profiling information for optimization
```

**Production Synthesis**:
```
// Core metadata structure for partially-static values
struct SpecializationMetadata {
  BindingTime binding_time;     // STATIC | DYNAMIC_TYPED | DYNAMIC
  TypeInfo type_info;            // Type constraints/hints
  GuardConditions guards;        // Necessary assertions
  EnvironmentDependencies env_deps;  // Which contexts matter
  SpecializationID spec_id;      // Trace/version identity
};

// Per-fexpr/operative specialization info
struct OperativeVersion {
  SpecializationID id;
  PartiallyStaticEnvironment static_env;
  CompiledCode code;
  GuardChecks runtime_guards;
  DependencySet dependencies;   // Other operatives it depends on
};
```

#### 2. **Compilation Result Representation**

**In Kraken:**
- Intermediate representation for partial evaluation
- Marks on forms indicating progress
- Eventually emitted as optimized WebAssembly

**In Fexpress:**
```racket
(compilation-result
  #:depends-on-env? boolean
  #:free-vars set-of-variables
  #:expr s-expression)
```

**In JIT Literature:**
- Compiler IR: Intermediate representation during compilation
- Trace IR: Linear representation of executed path (LuaJIT)
- SSA form: Static Single Assignment intermediate form (most JITs)
- Sea of nodes: Graph-based IR (Graal/Truffle)

**Production Synthesis**:
```cpp
// Multi-level intermediate representation
struct CompilationResult {
  IntermediateRepresentation ir;
  
  // Source-level (readable, debuggable)
  union {
    SourceExpression src_form;     // For tracing/debugging
    SSAGraph ssa_graph;             // For optimization
    TraceIR trace_ir;               // For tracing-based JIT
    Bytecode bytecode;              // For bytecode backends
  } representations;
  
  // Metadata
  PartiallyStaticEnvironment dependencies;
  GuardList guards;
  CodeQuality quality;  // Optimization level achieved
  
  // For versioning/specialization
  SpecializationID version_id;
  InvalidationTriggers triggers;    // When to re-specialize
};
```

---

## Part II: Operational Phases (The Full Pipeline)

### Phase 0: Initialization and Interpretation

**Goal**: Gather profiling data while executing naively

**Components**:

1. **Baseline Interpreter**
   - Execute fexpr code directly without compilation
   - Track: call counts, receiver types, argument patterns, environment usage
   - Cost: ~1-10x slower than optimized code

2. **Profiling Infrastructure**
   - Per-call-site feedback vectors (like V8 Ignition)
   - Call count threshold (HotSpot: ~10,000; PyPy: ~3,000)
   - Type feedback: monomorphic → polymorphic → megamorphic
   - Loop counters for backedge-based hotspot detection

3. **Sampling/Triggering Mechanism**
   - When call count exceeds threshold, **mark for optimization**
   - Don't compile immediately (avoids long GC pauses)
   - Queue for background compilation

**Relationship to Kraken/Fexpress**:
- This is the profiling that `mark` pass implicitly does in Kraken
- Fexpress relies on programmer-provided type hints instead

**Best Practices from Production JITs**:
- HotSpot uses 2-tier profiling: C1 compiler (fast, light profiling) → C2 compiler (slow, aggressive profiling)
- PyPy uses meta-tracing: trace the interpreter's execution
- V8 uses inline caches + feedback vectors for monomorphic fast-paths
- LuaJIT samples loops for tracing candidates

**Implementation Strategy**:
```cpp
struct ProfileData {
  uint64_t call_count;
  std::map<CallSite, FeedbackVector> site_feedback;  // Per call site
  std::set<LoopHeader> hot_loops;
  std::map<Variable, TypeSet> type_profile;          // Observed types
  
  bool should_compile() const {
    return call_count > COMPILE_THRESHOLD 
           && (site_feedback.size() > 0 || hot_loops.size() > 0);
  }
};
```

### Phase 1: Marking/Binding-Time Analysis

**Goal**: Annotate program with static/dynamic classification

**In Kraken**: The `mark` pass
```
Input: surface syntax
Output: syntax with:
  - Combiner IDs
  - Environment IDs (i_r, i_f)
  - Array marks (val, freshCall, attemptedCall)
```

**In Fexpress**: Type inference/annotation
```racket
(the (->/t_ (list (non-fexpr-value/t+)) ...) fexpr-code)
```

**In JIT Literature**: Binding-time analysis (BTA)
- Classify expressions as compile-time (CT) or runtime (RT)
- Compute fixed-point: CT depends only on CT values

**Production Approach**:

1. **Forward Data-Flow Analysis**
   - Traverse code; track what information is available at each point
   - Build partially-static environment at each node
   - Use fixed-point iteration for recursive definitions

2. **Type Inference**
   - Infer type constraints from:
     - Programmer annotations (like Fexpress `the` form)
     - Call site feedback (from Phase 0)
     - Literal values in code
   - Propagate constraints through expressions

3. **Dependency Analysis**
   - Determine which operatives depend on dynamic environment
   - Build dependency graph for scheduling
   - Detect pure (no env dependency) vs. impure operatives

**Implementation Strategy**:
```cpp
struct BindingTimeAnalysis {
  // For each node in AST
  std::map<ASTNode*, BindingTime> node_binding_time;
  std::map<ASTNode*, PartiallyStaticValue> node_value;
  std::map<CallSite*, FeedbackVector> call_feedback;
  
  // For each environment
  std::map<EnvironmentID, EnvironmentFrame> env_frames;
  std::map<Variable, VariableBindingInfo> var_info;
  
  // Fixed-point iteration
  bool iterate() {
    bool changed = false;
    for (auto& node : all_nodes) {
      BindingTime old_bt = node_binding_time[node];
      BindingTime new_bt = compute_binding_time(node);
      if (new_bt != old_bt) {
        node_binding_time[node] = new_bt;
        changed = true;
      }
    }
    return changed;
  }
};
```

### Phase 2: Online Partial Evaluation / Method-Based Specialization

**Goal**: Generate specialized code by reducing expressions with static data

**This is the core compilation phase; it unifies multiple JIT strategies:**

#### Strategy A: Online Partial Evaluation (Kraken Approach)

Execute this phase **during execution** when code reaches threshold.

```
Input: 
  - Form T to specialize
  - Partially-static environment E
  - Set of executing forms (prevents infinite recursion)
  - Call continuation context
  
Algorithm (simplified):
  1. If can make progress with E:
       - Partially evaluate T in E
       - Return optimized code
     Else:
       - Return T as suspended computation
       - Fall back to interpretation
  
  2. Progress check (needs-for-progress):
       - What environment IDs does T depend on?
       - Are those IDs in the current stack?
       - Has T already been evaluated before?
```

**Key Operations**:

1. **Unval** (Unevaluate): Suspend operands
   - For operative (wrap_level=0): Don't evaluate arguments
   - For applicative (wrap_level=1): Evaluate once
   - For applicative2 (wrap_level=2): Evaluate in result environment

2. **PEval** (Partially Evaluate): Reduce with known data
   - Inline static functions
   - Fold constant expressions
   - Eliminate static conditionals
   - Propagate known values

3. **Environment Reification**: Make runtime environment when needed
   - If operative takes `se` parameter: build frame
   - If variable lookup needs frame: build frame
   - Otherwise: omit environment (lazy reification)

**Implementation**:
```cpp
struct OnlinePartialEvaluator {
  // Specialization state
  PartiallyStaticEnvironment env;
  std::set<(Operative, EnvironmentID)> executing;  // Recursion guard
  
  // Core evaluation
  SpecializationResult peval(Term t) {
    // Check progress
    auto nfp = compute_needed_for_progress(t);
    if (!(nfp & env.available_frames).empty()) {
      return {Status::CAN_PROCEED, ...};
    }
    if ((t, env.id) in executing) {
      return {Status::RECURSIVE, ...};  // Mark for recursion handling
    }
    
    // Dispatch by term type
    if (is_literal(t)) {
      return {Status::CONSTANT, make_literal_code(t)};
    } else if (is_symbol(t)) {
      return {Status::LOOKUP, make_variable_ref(t)};
    } else if (is_call(t)) {
      return peval_call(t);
    } else if (is_operative(t)) {
      return peval_operative(t);
    }
  }
  
  // Specialize calls
  SpecializationResult peval_call(CallTerm call) {
    auto f_result = peval(call.function);
    if (f_result.status != Status::OPERATIVE) {
      // Can't specialize: f is not statically known
      return {Status::FALLBACK, generate_dynamic_call(call, f_result)};
    }
    
    Operative f = f_result.operative;
    
    // Unval + specialize arguments
    std::vector<Term> args;
    for (auto& arg : call.arguments) {
      if (f.wrap_level == 0) {
        // Operative: keep unevaluated
        args.push(arg);
      } else {
        // Applicative: evaluate
        auto arg_result = peval(arg);
        args.push(arg_result.code);
      }
    }
    
    // Create specialized environment
    auto specialized_env = env.extend(f.parameters, args);
    
    // Recursively specialize operative body
    executing.insert({f.id, env.id});
    auto body_result = peval_in_env(f.body, specialized_env);
    executing.erase({f.id, env.id});
    
    return body_result;
  }
};
```

#### Strategy B: Method-Based Specialization (Truffle Approach)

Execute this phase as a **separate compilation step** (not during runtime).

```
Input:
  - AST or bytecode of fexpr
  - Profiling feedback from Phase 0
  - Target specialization
  
Algorithm:
  1. Walk AST/bytecode
  2. At each node:
       - Check if node is "simplifiable" (constant operand, typed argument, etc.)
       - If yes: inline/specialize
       - If no: keep as residual
  3. Generate optimized code
```

This is simpler than online PE because:
- No recursion guard needed (compile-time only)
- Can be more aggressive (doesn't affect runtime responsiveness)
- Better for debugging (known execution paths)

**Implementation**:
```cpp
struct MethodBasedSpecializer {
  // Specialization directives (from programmer or feedback)
  struct Directive {
    ASTNode* target;
    std::string operation;  // "inline", "constant_fold", "type_specialize"
    PartiallyStaticValue context;
  };
  std::vector<Directive> directives;
  
  Code specialize(ASTNode* ast, ProfileData profile) {
    // Compute binding times using BTA + profile
    auto bta = run_binding_time_analysis(ast, profile);
    
    // Walk AST and specialize based on BTA
    return specialize_node(ast, bta);
  }
  
  Code specialize_node(ASTNode* node, BTA bta) {
    auto bt = bta.node_binding_time[node];
    
    if (bt == BindingTime::STATIC) {
      // Can evaluate at specialization time
      auto value = evaluate_statically(node);
      return make_constant_code(value);
    } else if (is_call(node)) {
      auto call = as<CallNode>(node);
      auto func_code = specialize_node(call.function, bta);
      
      if (can_determine_function_statically(func_code)) {
        // Specialize argument for known function
        auto func = extract_function(func_code);
        auto args_code = specialize_arguments(call.arguments, func, bta);
        return generate_specialized_call(func, args_code);
      }
    }
    
    // Default: keep as dynamic
    return generate_runtime_code(node, bta);
  }
};
```

#### Strategy C: Tracing-Based JIT (LuaJIT Approach)

Execute this phase by **recording an execution trace** and compiling it.

```
Input:
  - Loop/hot path identified by Phase 0 profiling
  - State at loop entry
  
Algorithm:
  1. Execute interpreter, recording all operations
  2. Stop at:
       - Loop backedge (completed one iteration)
       - Uncommon branch (exit trace)
  3. Compile recorded trace + guards
```

This is excellent for:
- Loops (common in partial evaluation)
- Programs with many dynamic branches (trace specializes to common path)
- Languages where call sites are hard to analyze statically

**Implementation**:
```cpp
struct TracingJIT {
  struct RecordingBuffer {
    std::vector<IRInstruction> instructions;
    std::vector<GuardCondition> guards;
    std::map<Variable, PartiallyStaticValue> value_snapshot;
  };
  
  void trace_execution(LoopHeader loop) {
    RecordingBuffer buf;
    
    // Record one iteration
    auto execution_log = record_interpreter_execution(loop);
    
    for (auto& step : execution_log) {
      // Translate interpreter step to IR
      if (step.is_call()) {
        // Record call with guard on receiver
        buf.guards.push_back(
          make_guard_receiver_is(step.call_target));
        buf.instructions.push_back(
          make_direct_call(step.call_target));
      } else if (step.is_branch()) {
        // Record branch taken; guard on condition
        buf.guards.push_back(
          make_guard_condition(step.condition, step.taken));
        if (step.taken) {
          buf.instructions.push_back(...taken_path...);
        } else {
          buf.instructions.push_back(...exit_trace...);
        }
      } else {
        // Regular operation
        buf.instructions.push_back(translate_instruction(step));
      }
    }
    
    // Compile trace
    auto code = compile_trace(buf);
    install_as_loop_version(loop, code);
  }
};
```

### Phase 3: Code Generation

**Goal**: Emit machine code (or bytecode) from specialized representation

**Options**:

1. **WebAssembly** (like Kraken)
   - Portable, sandboxed, JIT-friendly
   - Emits bytecode; can be further optimized

2. **Native Code** (like HotSpot C2, LuaJIT)
   - Direct to machine code (x86-64, ARM, etc.)
   - Requires architecture-specific backend
   - Fast execution; slower compilation

3. **Bytecode** (like HotSpot C1, V8 Ignition)
   - Compromise: simpler than native, faster than interpreter
   - Often used as intermediate tier
   - Fast to generate; reasonable execution speed

4. **Source-to-Source** (like Fexpress)
   - Generate Racket/Python/etc. code
   - Let host language compiler optimize further
   - Simplest to implement; potentially slower

**Production Approach**: Tiered code generation

```cpp
struct CodeGenerator {
  enum CodeQuality {
    INTERPRETED,        // Tier 0: Direct interpretation (no compile)
    BYTECODE,           // Tier 1: Fast baseline compiler
    OPTIMIZED,          // Tier 2: Aggressive optimization
    SPECULATIVE,        // Tier 3: Speculative optimization (with guards)
  };
  
  Code generate(SpecializationResult spec_result, CodeQuality tier) {
    switch (tier) {
      case INTERPRETED:
        return spec_result.interpreted_form;
      
      case BYTECODE:
        // Fast: 1-2s to compile per method
        // Result: ~50% slower than optimized
        return generate_bytecode(spec_result.ir, 
          CodeGenOptions{.inline_depth = 2, .optimize = false});
      
      case OPTIMIZED:
        // Slower: 5-50s to compile per method
        // Result: 1-3x faster than bytecode
        return generate_native_code(spec_result.ir,
          CodeGenOptions{.inline_depth = 10, .optimize = true, 
                         .vectorize = true, .unroll_loops = true});
      
      case SPECULATIVE:
        // Aggressive: assume types match profile
        // Fallback on guard failure
        return generate_speculative_code(spec_result.ir,
          spec_result.guards,
          CodeGenOptions{.aggressive_inlining = true,
                         .assume_monomorphic = true});
    }
  }
};
```

### Phase 4: Guard Compilation and Deoptimization

**Goal**: Ensure correctness when specialization assumptions fail

**When specialization assumes something** (type, value, environment), **we must check it at runtime**:

1. **Type guards**: If we assume argument X is an integer, emit:
   ```cpp
   if (!is_integer(X)) deoptimize();  // Fall back to interpreter
   ```

2. **Monomorphic call guards**: If we assume `f` is a specific operative:
   ```cpp
   if (receiver_operative_id != expected_id) deoptimize();
   ```

3. **Environment guards**: If we assume environment frame `env_5` is available:
   ```cpp
   if (!has_frame_5(env)) deoptimize();
   ```

**Deoptimization Strategy**:

```cpp
struct DeoptimizationManager {
  struct DeoptPoint {
    uint64_t code_address;
    SpecializationID original_version;
    PartiallyStaticEnvironment saved_state;
    
    // Jump target in baseline code
    uint64_t fallback_address;
  };
  
  void on_guard_failure(DeoptPoint point) {
    // 1. Restore interpreter state
    auto state = point.saved_state;
    
    // 2. Jump to fallback code (typically re-interpretation)
    //    Can also tier-up: try different specialization with more feedback
    
    // 3. If failure repeats, track and adapt strategy
    track_failed_speculation(point);
  }
};
```

**Best Practices**:
- **Implicit guards** (like null checks): Signal handler catches failure
- **Explicit guards**: Direct comparison before optimized code
- **Guard removal**: Identify provably-always-true/false guards and eliminate them

### Phase 5: Versioning and Invalidation

**Goal**: Manage multiple specialized versions of the same operative

As profiling data changes, earlier specializations may become suboptimal.

**Versioning Strategy**:

```cpp
struct SpecializationVersionManager {
  struct Version {
    SpecializationID id;
    PartiallyStaticEnvironment spec_env;
    Code code;
    Set<InvalidationTrigger> triggers;
    uint64_t install_time;
    uint64_t execution_count;
  };
  
  std::vector<Version> versions;
  
  // Decide which version to use
  Version* select_version(PartiallyStaticEnvironment current_env,
                          ProfileData profile) {
    // Try most specific version first
    for (auto& v : versions) {
      if (is_compatible(v.spec_env, current_env, profile)) {
        return &v;
      }
    }
    
    // Fall back to generic version
    return versions.back();  // Most general (least specialized)
  }
  
  // Invalidate versions when assumptions fail
  void invalidate_on_trigger(InvalidationTrigger trigger) {
    for (auto& v : versions) {
      if (v.triggers.count(trigger)) {
        v.valid = false;
        // Can keep code around for quick re-validation,
        // or free it immediately
      }
    }
  }
};
```

---

## Part III: Production Architectural Components

### A. Runtime System

#### 1. Profiling Infrastructure

```cpp
struct ProfilingSystem {
  // Per-call-site feedback
  struct FeedbackVector {
    uint64_t call_count;
    std::map<Operative, uint64_t> callee_counts;  // Who was called?
    std::set<Type> observed_types;                 // Type diversity
    std::set<EnvironmentID> observed_env_frames;   // Env usage
    
    CallPatternInfo call_pattern;  // Monomorphic/polymorphic/mega
  };
  
  std::map<CallSite, FeedbackVector> site_feedback;
  
  // Loop hotness tracking
  struct LoopInfo {
    LoopHeader id;
    uint64_t backedge_count;
    bool traced;  // Has this loop been traced?
  };
  std::map<LoopHeader, LoopInfo> loop_profile;
  
  // Operational hooks
  void on_call(CallSite site, Operative target, PartiallyStaticEnvironment env) {
    auto& fb = site_feedback[site];
    fb.call_count++;
    fb.callee_counts[target]++;
    // ... update type info ...
    
    if (fb.call_count == COMPILE_THRESHOLD) {
      trigger_compilation(site);
    }
  }
};
```

#### 2. Memory Management

```cpp
struct FexprMemoryManager {
  // Garbage collection for fexpr values
  GarbageCollector gc;
  
  // Code cache for compiled code
  struct CodeCache {
    size_t current_size;
    size_t max_size;  // Trigger eviction when full
    std::vector<CompiledCodeBlock> blocks;
    
    LRU_EvictionPolicy eviction;
  } code_cache;
  
  // Versioning/specialization cache
  struct SpecializationCache {
    // (Operative, SpecializationEnv) → CompiledCode
    std::map<std::tuple<OperativeID, PartiallyStaticEnvID>, CompiledCode> cache;
    
    // Invalidation tracking
    std::map<CompiledCode, InvalidationTriggers> dependencies;
  } spec_cache;
};
```

### B. Compilation System

#### 1. Compilation Queue

```cpp
struct CompilationQueue {
  // Priority queue of compilation tasks
  struct CompilationTask {
    OperativeID operative_id;
    PartiallyStaticEnvironment spec_env;
    ProfileData profile;
    
    // Priority: hotness * wait_time (prevent starvation)
    uint64_t priority() const;
  };
  
  // Background compilation threads
  ThreadPool compiler_threads;
  std::priority_queue<CompilationTask> queue;
  
  // Limits
  struct Limits {
    size_t max_queue_size = 1000;
    size_t max_concurrent_compilations = 4;
    Duration max_compilation_time = 5s;
  } limits;
};
```

#### 2. Optimization Pipeline

```cpp
struct OptimizationPipeline {
  // Sequence of optimization passes
  std::vector<OptimizationPass*> passes;
  
  // Standard passes
  register_pass(new ConstantFoldingPass());
  register_pass(new DeadCodeEliminationPass());
  register_pass(new InliningPass());
  register_pass(new EscapeAnalysisPass());
  register_pass(new LoopUnrollingPass());
  register_pass(new VectorizationPass());
  
  // Fexpr-specific passes
  register_pass(new EnvironmentReificationPass());
  register_pass(new RedundantVevalEliminationPass());  // dropRV
  register_pass(new OperativeSpecializationPass());
  register_pass(new GuardOptimizationPass());
  
  Code optimize(Code input) {
    Code current = input;
    for (auto pass : passes) {
      current = pass->transform(current);
      if (!pass->is_valid(current)) {
        // Fallback: return un-optimized
        return input;
      }
    }
    return current;
  }
};
```

#### 3. Compiler Tiers

```cpp
struct TieredCompiler {
  enum Tier { BASELINE = 0, OPTIMIZED = 1, SPECULATIVE = 2 };
  
  // Tier 1: Fast baseline compiler
  class BaselineCompiler {
    // Quick compilation: ~100ms per method
    // Limited optimization
    // Can collect profiling for tier-up
    Code compile(OperativeID id) {
      auto ir = generate_ir_from_bytecode(id);
      // Skip heavy optimizations; focus on fast code gen
      return code_gen_fast(ir);
    }
  } baseline;
  
  // Tier 2: Optimizing compiler
  class OptimizingCompiler {
    // Aggressive compilation: ~1-10s per method
    // Full optimization pipeline
    Code compile(OperativeID id, ProfileData profile) {
      auto ir = generate_ir_from_bytecode(id);
      ir = optimize_pipeline.transform(ir);
      return code_gen_optimized(ir);
    }
  } optimizing;
  
  // Tier 3: Speculative compiler
  class SpeculativeCompiler {
    // Assume-and-guard compilation: ~10-100s per method
    // Aggressive specialization with deoptimization
    Code compile(OperativeID id, ProfileData profile) {
      auto ir = generate_ir_from_bytecode(id);
      ir = add_speculative_assumptions(ir, profile);
      ir = add_guard_checks(ir, profile);
      ir = optimize_pipeline.transform(ir);
      return code_gen_speculative(ir);
    }
  } speculative;
};
```

### C. Execution and Fallback Handling

```cpp
struct ExecutionEngine {
  // Primary execution: optimized code
  void execute_optimized(CompiledCode code, FexprValue& result) {
    try {
      result = run_machine_code(code);
    } catch (SpeculationFailure& e) {
      // Fall back to less specialized version
      invalidate_version(e.failed_version);
      execute_fallback(e.fallback_operative, e.saved_state, result);
    }
  }
  
  // Fallback: baseline or tier-up
  void execute_fallback(OperativeID op, FexprValue inputs, FexprValue& result) {
    // Option 1: Interpret (slowest, always correct)
    result = interpret(op, inputs);
    
    // Option 2: Use lower tier (faster than interpret, might re-compile)
    if (auto baseline_code = get_baseline_version(op)) {
      result = run_machine_code(baseline_code);
    }
    
    // Option 3: Re-specialize with more feedback
    auto new_version = recompile_with_updated_profile(op);
    result = run_machine_code(new_version);
  }
};
```

---

## Part IV: Handling Fexpr-Specific Challenges

### A. Recursive Operatives and the Needed-for-Progress Problem

**Challenge**: Same operative may need to be partially evaluated multiple times in different contexts.

**Solution** (Kraken): Track (operative, environment_id) pairs in execution set.

```cpp
struct RecursionHandling {
  std::set<std::pair<OperativeID, EnvironmentID>> executing;
  
  SpecializationResult peval_operative(OperativeID op, 
                                        PartiallyStaticEnvironment env) {
    auto key = std::make_pair(op, env.id);
    
    if (executing.count(key)) {
      // Already evaluating this operative in this environment
      // Return as suspended computation (will handle at runtime)
      return {Status::RECURSIVE, make_runtime_call(op, env)};
    }
    
    executing.insert(key);
    auto result = peval_body(op, env);
    executing.erase(key);
    
    return result;
  }
};
```

**Alternative** (Loop/fixed-point iteration): Re-specialize until convergence.

```cpp
struct FixedPointSpecialization {
  Code specialize_to_fixed_point(OperativeID op) {
    Code prev_code = nullptr;
    Code curr_code = specialize(op);
    
    int iterations = 0;
    while (curr_code != prev_code && iterations < MAX_ITER) {
      prev_code = curr_code;
      curr_code = specialize_with_known_code(op, prev_code);
      iterations++;
    }
    
    return curr_code;
  }
};
```

### B. Environment Reification

**Challenge**: Operatives taking dynamic environment parameters (`se` in vau) require representing the environment at runtime, but we want to avoid building massive structures.

**Solution**: Lazy, distributed environment building (both Kraken and Fexpress do this).

```cpp
struct LazyEnvironmentReification {
  // Only build environment frames where actually needed
  
  struct ReificationPoint {
    OperativeID requires_frame;
    Variable frame_variable;
    
    // What values go in this frame?
    std::map<Symbol, PartiallyStaticValue> bindings;
  };
  
  std::vector<ReificationPoint> reification_points;
  
  Code generate_code_with_lazy_env() {
    Code code;
    
    // Outer scope: no environment yet
    code += lambda([&] {
      // When we encounter an operative that needs env:
      if (operative_uses_se) {
        // Build environment at that point
        code += make_env_frame();
      }
      
      // Call operative with environment (if needed)
      code += call_operative_with_env();
    });
    
    return code;
  }
};
```

### C. Operatives that Generate Code (Meta-Programming)

**Challenge**: Some operatives evaluate their arguments and process the results as code (like `eval` or `macro-like` operatives).

**Kraken's approach**: Recognize these patterns via `wrap_level` and special handling.
- wrap_level = 0 (operative): unevaluated → can specialize like macro
- wrap_level = 1 (applicative): evaluate → different specialization
- Special cases (eval, vau, etc.): handle specially

```cpp
struct MetaProgrammingHandling {
  // For (eval body env)
  if (is_eval_call(call)) {
    auto body_result = peval(call.body_arg);
    auto env_result = peval(call.env_arg);
    
    if (body_result.is_static && env_result.is_static) {
      // Can evaluate eval at compile time!
      auto static_result = evaluate_static_eval(
        body_result.value, env_result.value);
      return make_constant(static_result);
    } else {
      // Generate code that performs eval at runtime
      return make_runtime_eval(body_result.code, env_result.code);
    }
  }
  
  // For (macro-like operative)
  if (is_wrapped_zero_operative(op)) {
    // Body evaluates arguments, then processes as code
    // Can inline and specialize the arguments directly
    auto args_code = specialize_arguments(args);
    auto body_code = specialize_body_with_args(op.body, args_code);
    return body_code;  // Already evaluated!
  }
};
```

### D. Type System and Static Analysis

**Challenge**: Dynamic typing makes static analysis hard, but we can use:
- Programmer-provided type hints (Fexpress `the` forms)
- Runtime profiling feedback (HotSpot-style)
- Constraint propagation

**Unified Type System**:

```cpp
struct FexprType {
  // Can be:
  // 1. Concrete type (int, double, operative, ...)
  // 2. Union type (int | double | operative | ...)
  // 3. Constrained (int where value > 0, ...)
  // 4. Unknown
  
  bool is_concrete() const;
  bool is_fexpr() const;
  bool must_be_fexpr() const;  // Never non-fexpr
  bool can_be_fexpr() const;   // Might be fexpr
  bool definitely_not_fexpr() const;  // Always non-fexpr
};

// Type propagation
struct TypeInference {
  std::map<ASTNode*, FexprType> node_types;
  
  void infer() {
    // Forward pass: literals, annotations
    for (auto node : ast) {
      if (is_literal(node)) {
        node_types[node] = infer_literal_type(node);
      } else if (is_annotated(node)) {
        node_types[node] = node->annotation;
      }
    }
    
    // Backward pass: propagate constraints
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto node : ast) {
        auto old_type = node_types[node];
        auto new_type = infer_from_context(node);
        if (new_type != old_type) {
          node_types[node] = new_type;
          changed = true;
        }
      }
    }
  }
};
```

---

## Part V: Implementation Roadmap

### Phase 1: Foundation (3-6 months)

1. **Interpreter**
   - Baseline fexpr execution engine
   - Profiling infrastructure (call counts, type feedback, loop detection)
   - Stack unwinding for debugging

2. **Binding-Time Analysis**
   - Forward data-flow analysis for partially-static environments
   - Type inference from annotations + profiling
   - Dependency analysis (which operatives depend on dynamic data)

3. **Baseline Compiler (Tier 0)**
   - Generate bytecode from AST
   - Fast, ~100ms per operative
   - Collect type feedback for tier-up

### Phase 2: Online Partial Evaluation (6-9 months)

4. **Partial Evaluator**
   - Implement `peval` + `unval` algorithm
   - Recursion guard via (operative, env_id) set
   - Lazy environment reification

5. **Optimizing Compiler (Tier 1)**
   - IR generation and optimization pipeline
   - Inlining, constant folding, dead code elimination
   - Code generation to native code or WebAssembly

6. **Deoptimization**
   - Guard compilation and failure handling
   - Fallback mechanisms
   - Version invalidation and re-specialization

### Phase 3: Advanced Optimizations (9-12 months)

7. **Speculative Compilation (Tier 2)**
   - Assumption-based specialization
   - Profile-guided optimization
   - Monomorphic/polymorphic call optimization

8. **Tracing-Based Specialization**
   - Loop tracing and compilation
   - Guard specialization for hot paths
   - Trace versioning and side-exits

9. **Environment and Meta-Programming Handling**
   - Special-casing eval, vau, and macro-like operatives
   - Environment reification optimization (dropRV pass)
   - Meta-programming analysis

### Phase 4: Production Hardening (12-18 months)

10. **Multi-language Support**
    - Generate code to bytecode, native code, or source (C, Rust, etc.)
    - Runtime for each target
    - Debugging/profiling tools

11. **Performance Tuning**
    - Benchmark suite (Kraken-style evaluation)
    - Cache tuning, GC optimization, compilation cost analysis
    - Warmup phase characterization (à la virtual machine warmup papers)

12. **Tooling**
    - IDE integration (real-time type inference, specialization hints)
    - Profiler and flame graph generation
    - Debugger (source-level + specialized code)

---

## Part VI: Comparison with Kraken and Fexpress

| Aspect | Kraken | Fexpress | Production Architecture |
|--------|--------|----------|------------------------|
| **Code Generation** | WebAssembly | Racket source | Tiered: bytecode → native |
| **Specialization Strategy** | Online PE | Type hints | Online PE + profiling + tracing |
| **Recursion Handling** | (form, env) set | Implicit (Racket) | (operative, env_id) set + fixed-point |
| **Environment Reification** | Needed-for-progress | Lazy in clambda | Lazy + distributed + profiling-guided |
| **Compilation Tiers** | Single-tier | N/A | 3-tier: baseline, optimizing, speculative |
| **Guard/Deoptimization** | Not detailed | Not detailed | Full framework with invalidation |
| **Type System** | Implicit (wrap_level) | Pragmatic hints | Static + dynamic + profiling |
| **Production Ready** | Benchmarked, sound | Proof-of-concept | Full framework |
| **Warm-up Handling** | Not discussed | Not discussed | Statistical analysis + profiling |

---

## Conclusion

By synthesizing insights from Kraken (partial evaluation rigor), Fexpress (pragmatic simplicity), and established JIT/PE literature (production practices), we arrive at a comprehensive framework for production fexpr compilation.

**Key Principles**:

1. **Partial-Static Domains**: Represent data where parts are compile-time-known and parts are runtime-dependent
2. **Progress Tracking**: Determine when specialization can proceed via dependency analysis
3. **Tiered Specialization**: Progression from baseline → optimized → speculative
4. **Lazy Reification**: Build environments only where needed
5. **Guard + Deoptimization**: Ensure correctness when assumptions fail
6. **Profiling**: Use runtime feedback to guide specialization decisions
7. **Versioning**: Maintain multiple specialized versions for different contexts

This framework combines the rigor of academic partial evaluation with the pragmatism of production JIT engineering, providing a solid foundation for practical fexpr systems that achieve both correctness and performance.
