# Unified Data-First Analysis: Kraken and Fexpress Compilation Strategies

## Executive Summary

Kraken (Braswell et al.) and Fexpress (rocketnia) represent two complementary architectural approaches to compiling first-class f-expressions. Despite superficial differences in presentation—Kraken emphasizing transformation functions and partial evaluation algorithms, while Fexpress emphasizes type systems and pragmatic code generation—both systems solve fundamentally similar problems using harmonizable data representations.

**Key Insight:** Both systems represent the core challenge as managing **partially-static data structures** where some information is known at compile time (static) and other parts depend on runtime values (dynamic). The systems differ primarily in:
1. **Granularity of metadata:** Kraken uses environment IDs + needed-for-progress relations; Fexpress uses positive/negative types
2. **Generation strategy:** Kraken generates WebAssembly bytecode; Fexpress generates Racket source code
3. **Scope:** Kraken is a complete compiler; Fexpress is a proof-of-concept library

Both systems, when viewed through a **data-first lens**, converge on nearly identical value representation schemes.

---

## Part I: Core Value Representations

### A. Fundamental Values (The Common Ground)

| **Category** | **Kraken Notation** | **Fexpress Notion** | **Semantics** |
|---|---|---|---|
| **Literals** | `n ∈ ℕ` (integers) | `specific-value/t+` | Self-evaluating constants |
| **Code References** | `s ∈ Symbols` | `at-variable/t+` | Deferred lookups in environments |
| **Operations** | `o ∈ {eval, vau, wrap, ...}` | Primitive combiners in `lazy-value/t+` | Built-in operations |
| **Executable Code** | `T := V \| Active_Term` | `clambda` instances | Closures with unevaluated args |
| **Collections** | `A := (T ...)` | Array structure implied | Ordered sequences of terms |

**First Correspondence:** Both systems distinguish between **data** (values V) and **computations** (active terms AT / suspended code).

### B. Static vs. Dynamic Data (The Core Innovation)

#### Kraken's Partially-Static Environments

```
Environment E with frames:
  - Real frame (i_r): Maps symbols to static values
    E_real = {x → 5, y → (lambda (z) ...)}
  
  - Fake frame (i_f): Static description of dynamic data
    E_fake = {x → placeholder_i_x, y → placeholder_i_y}
  
  - Mixed chains: Frame chain can alternate real/fake
    E = [...E_real...E_fake...E_real...]
```

Each frame has a unique ID (`i_r` or `i_f`) that tracks:
- Whether the frame represents static or dynamic data
- Which combiner call created it
- Which environments chain upward to it

#### Fexpress's Positive Types with Reification

```
Positive type (type+) = {eval: () → value, compile: () → code}

Examples:
  - any-value/t+ : No assumptions; generates runtime dispatch code
  - non-fexpr-value/t+ : Guaranteed non-fexpr; generates direct call code
  - specific-value/t+ : Wraps value=42; generates literal
  - at-variable/t+ : var=x, env=some_context; generates variable reference
  - lazy-value/t+ : eval=custom_fn, compile=custom_code_gen
```

Reifiable value = `{depends-on-env?, free-vars, expr}` where:
- `expr` is **actual Racket code** (s-expression)
- `depends-on-env?` tracks whether this code can execute standalone
- `free-vars` lists which variables it depends on

**Second Correspondence:** Kraken's "real environment" ↔ Fexpress's "positive type with static data". Both represent known compile-time information.

**Third Correspondence:** Kraken's "fake environment" ↔ Fexpress's "any-value/t+" or type with `depends-on-env? = true`. Both represent data whose structure is known but whose values are dynamic.

### C. Tracking Environment Dependencies

#### Kraken: Needed-For-Progress Relations

The system maintains relations tracking which environment IDs a form depends on:

```
needed-for-progress(form) → Set of environment IDs

Example:
  - Form: (λ (x) (+ x y)) in context where y is in fake environment i_fake
  - Result: {i_fake}  ← This form cannot make progress without i_fake
  
  - Form: (+ 1 2)
  - Result: true  ← Can make progress regardless of environment
```

**Pruning strategy:** If `needed-for-progress(form) ∩ environment_stack = ∅`, the form cannot make progress; stop evaluating it.

#### Fexpress: Type Hint Propagation

The system uses negative types as optimization directives:

```
Negative type (type_) = optimization hint about result

Examples:
  - any-value/t_ : "Result could be anything; be conservative"
  - (->/t_ (list arg-types) return-type) : "Result is a non-fexpr function"
```

When compiling `(clambda (f) (clambda (g) (f (g x))))`:
- If f has type `(->/t_ (list (non-fexpr-value/t+)) ...)`
  → Generate direct Racket function call
- If f has type `any-value/t_`
  → Generate dispatch code that checks if f is an fexpr

**Unifying insight:** Kraken's "needed-for-progress" = Fexpress's "type hint propagation". Both track which constraints determine whether compilation can proceed.

---

## Part II: Suspended Computations and Active Terms

### A. Representations of Deferred Work

#### Kraken's Active Terms

```
Active terms (AT) = Computations in progress:

  Base level:
    - eval(T, E) : "Evaluate term T in environment E"
    - combine(T, (T...), E) : "Call result of T with args in E"
  
  Partial evaluation level:
    - peval(T, marked_E, ES, FS) : "Partially evaluate T"
      where: marked_E = environment with metadata
             ES = environment stack (for call context)
             FS = currently executing forms (recursion detection)
    
    - under(result, fallback, marked_E, ES, FS) : "Currently executing call"
      where: result = partial eval in progress
             fallback = recovery if can't make progress
```

Key insight: `under` form captures a **choice point** between:
- Succeeding with optimized result → return immediately
- Failing → fall back to suspended call

#### Fexpress's Continuation Expressions

```
Continuation expression (continuation-expr?) = syntactic context

  - apply/ce(args, next) : "Apply pending arguments, then continue"
  - done/ce(type_hint) : "End of context; compile for this type"

Combined with positive types:
  (fexpress-eval/t+ env form continuation) 
    = "Evaluate form with these arguments pending and this result type expected"
```

The continuation encodes **what happens next** after this form evaluates.

**Fourth Correspondence:** Kraken's `combine(f, (args...), E)` ↔ Fexpress's `apply/ce(args, ...)`. Both represent "function call waiting for operands to be evaluated."

**Fifth Correspondence:** Kraken's `peval(T, ...)` ↔ Fexpress's `fexpress-eval/t+` dispatch. Both are decision points: "Can this make progress? If so, compile it; otherwise, generate residual code."

**Sixth Correspondence:** Kraken's `under(result, fallback, ...)` ↔ Fexpress's `continuation-expr?` recovery. Both provide escape hatches when optimization fails.

### B. Handling Operatives (The Tricky Case)

#### Kraken's Challenge: Derived Combiners

```
Derived combiner = closure with:
  - wrap_level : How many times to evaluate arguments
  - dynamic_env_param : Name of dynamic environment parameter (if operative)
  - static_env : Environment captured at definition time
  - body : Code to execute when called

Problem: Same operative may be partially evaluated differently
depending on which environment it's called in.

Solution: Separate handling based on static environment type:
  - If static_env is REAL (all static data known)
    → Can fully inline/optimize the operative
  
  - If static_env is FAKE (contains dynamic placeholders)
    → Must re-evaluate operative, but with more progress
    → May need multiple partial evaluations
```

The paper notes: "A combiner definition may have to be partially evaluated multiple times in environments with different amounts of static data before it has enough information to reduce as far as it should."

#### Fexpress's Challenge: clambda Specialization

```
clambda = Operative receiving:
  - unevaluated arguments (in symbolic form)
  - continuation expression (what happens after)

Problem: clambda's behavior depends on:
  - Whether arguments are statically known to be non-fexprs
  - What the calling context needs (from continuation)
  - Whether the clambda body uses the dynamic environment

Solution: Generate specialized code based on type information:
  - Level 1 (fully typed): All arguments are non-fexprs
    → Generate pure Racket lambda with direct calls
  
  - Level 2 (partially typed): Some args are non-fexprs
    → Generate Racket code with mixed dispatch
  
  - Level 3 (untyped): Arguments could be fexprs
    → Generate code that re-enters interpreter with reified env
```

**Seventh Correspondence:** Kraken's "re-evaluating combiners in different environments" ↔ Fexpress's "generating multiple specializations of clambda". Both recognize that operatives must be compiled differently depending on **context**.

---

## Part III: Environment Reification

### A. The Central Problem

Both systems must convert **partially-static environments** into something executable:

```
Partially-static environment = mix of:
  - Known values (can be inlined)
  - Dynamic data (must be represented at runtime)
  - Combiners (may contain environments, which may contain ...)

Challenge: If naively represented, environment structures explode 
in size and runtime cost.
```

### B. Kraken's Lazy Environment Creation

```
Strategy: Don't build entire environment structures upfront.
Instead, lazily construct only the parts needed.

Mark pass: Annotate each form with:
  - Which symbols it depends on
  - Which environment frames those symbols resolve in
  - Whether those frames are real or fake

During compilation: Only allocate environment frames when:
  1. An operative takes the dynamic environment as a parameter
  2. A symbol lookup actually needs that frame

Result: 
  (λ (f)                          ; wrap_level=1
    (λ (g)                        ; wrap_level=1
      (λ (x)                      ; wrap_level=1
        (f (g x)))))             ; calls f and g directly

Generated code (pseudocode):
  lambda(f) {
    return lambda(g) {
      return lambda(x) {
        return f(g(x))           ; no environment involved!
      }
    }
  }

Lazy construction only occurs when needed:
  (vau se (x) (eval (array + x x) se))

Generated code:
  function(se, x) {
    // se is the dynamic environment parameter
    // Build environment for eval's arguments only:
    let inner_env = {x: x, ...se.rest}
    return eval(array(+, x, x), inner_env)
  }
```

### C. Fexpress's Distributed Reification

```
Strategy: Each clambda contributes its frame only when it's actually needed.

Positive type carries eval+compile information:
  at-variable/t+(var='x', env=E) produces:
    - eval: Look up x in E at runtime
    - compile: Reference variable -x (renamed to avoid capture)

When clambda is called:
  - Check if arguments' types indicate non-fexprs
  - If YES: Generate pure Racket lambda
  - If NO/PARTIAL: Insert environment-building code:
    
    (lambda (-f)
      (let ((env (hash-set* env
                   'f (at-variable/t+ 'f (specific-value/t+ -f)))))
        (lambda (-g)
          ...)))
  
  Each nested clambda adds its frame only where needed.
```

**Eighth Correspondence:** Both systems avoid building complete environment structures upfront. Kraken uses "needed-for-progress" to prune; Fexpress uses "type hints" to determine which frames are necessary.

---

## Part IV: Compilation Decisions and Optimization

### A. The Decision Point

#### Kraken: The `peval` Form

```
peval(T, marked_env, ES, FS) represents the decision:
  
  IF needed-for-progress(T) ∩ ES ≠ ∅
    OR T has already been partially evaluated
    OR T is under a combiner that prevents recursion
  THEN:
    Cannot make progress; return T as suspended computation
  ELSE:
    Continue partial evaluation
```

This is checked by the "will-make-progress" condition before any rule applies.

#### Fexpress: The `fexpress-eval/t+` Dispatch

```
fexpress-eval/t+(env, expr, continuation) decides:
  
  IF (positive type of expr says we can compile this)
    AND (continuation type permits inlining)
    AND (environment has enough static data)
  THEN:
    Call (type+-compile expr) to generate Racket code
  ELSE:
    Call (type+-eval expr) to interpret it
```

Both make a **progress check** before deciding.

### B. Call-Site Optimization (Special Casing)

#### Kraken on Calls

The paper identifies calls as "more complex due to the high amount of bookkeeping":

```
When encountering combine(T_combiner, (T_args...), E):

1. Partially evaluate T_combiner to get actual combiner
   - If it's a symbol, can't proceed; return attempted call
   - If it's a suspended call, can't proceed; return attempted call

2. Extract wrap_level from combiner

3. Unval + partially evaluate arguments wrap_level times
   - For operative (wrap_level=0): Don't evaluate args
   - For applicative (wrap_level=1): Evaluate args once

4. Create new environment binding parameters to arguments
   - Check if (combiner, environment) already executing → prevent infinite recursion

5. Partially evaluate combiner body in new environment
   - Mark with under() form to capture recovery point

6. Check if result can be returned:
   - returnOk() verifies no unresolvable references to inner environment

7. If success: dropRV() removes redundant veval calls

8. If failure: Return suspended call with fallback form
```

The "under" form is the **choice point**: it holds both the successful result and a fallback if that success is invalid.

#### Fexpress on Calls

clambda performs similar analysis but at code-generation time:

```
When clambda(args, body) is called in context:

1. Examine argument types:
   - non-fexpr-value/t+ → can generate direct Racket call
   - any-value/t+ → must generate dispatch
   - lazy-value/t+ → use custom code generation

2. Examine body for environment dependencies:
   - Scan for (eval ...) or (the ...) forms
   - Determine if dynamic environment se is actually used

3. Generate specialized code:
   - Pure lambda if all args are statically non-fexprs
   - Mixed code if partially known
   - Full re-entry if fully dynamic

4. Use continuation to decide what comes after:
   - If result will be immediately called → inline
   - If result will be returned → wrap appropriately
```

**Ninth Correspondence:** Both systems identify calls as the critical optimization point. Kraken uses `combine` + `under`; Fexpress uses `clambda` + `continuation-expr`. Both perform **progress checks** to decide whether to optimize.

### C. The veval / Redundant Computation Problem

#### Kraken's dropRV (Drop Redundant Veval)

```
Problem: Macro-like operatives typically end with (eval (generated_code) env).

In partial evaluation, this becomes:
  - Generate code
  - Wrap in veval call (because it still needs environment)
  - Return to calling site
  - Calling site's environment IS the eval environment
  → veval call is redundant!

Solution dropRV(): Remove redundant veval wrappers
  
  Example:
    (eval (array + x x) se) in operative body
    
    PE produces: veval((array + x x), se)
    
    dropRV sees: se is the same as calling environment
    
    Converts to: (array + x x)  ← inlined directly!
```

#### Fexpress's Environment-Aware Code Generation

```
Problem: Similar issue when clambda body calls eval.

Solution: When generating clambda code:

  IF clambda body calls (eval EXPR ENV)
    AND ENV is the dynamic environment parameter
    AND EXPR's type doesn't require re-interpretation
  THEN:
    Generate: just EXPR
    (no environment passing needed)
```

**Tenth Correspondence:** Both systems recognize a specific optimization pattern: "environment-appropriate eval calls can be eliminated". Kraken formalizes this as `dropRV`; Fexpress handles it implicitly in code generation.

---

## Part V: Handling Infinite Recursion

### A. Kraken's Approach: Recursion Detection via Forms Set

```
FS = set of currently executing (form, environment) pairs

When entering a combiner call:
  1. Create: F = (combiner, new_inner_environment)
  2. Check: IF F ∈ FS THEN recursion detected!
  3. Action: Return (attempted_call) marked with F
             This prevents infinite recursion

Later evaluation:
  - When form reappears with different environment
  - It's NOT in FS anymore (different environment)
  - So it proceeds and eventually terminates (with finite input)
```

Example: Y-combinator works because each recursive call creates a **new environment** with different bindings, so the (form, environment) pair differs.

### B. Fexpress's Approach: No Explicit Recursion Handling

Fexpress doesn't explicitly address infinite recursion in the papers analyzed. The proof-of-concept nature means:
- Relies on programmer discipline
- Or relies on underlying Racket's recursion limits
- The generic infrastructure could be extended for this

**Gap:** This is an area where Kraken is more sophisticated. Fexpress would need a comparable `needed-for-progress-infinite` relation.

---

## Part VI: From Data to Transformations

### The Transformation Pipeline (Data-Driven View)

#### Kraken's Stages

```
Value Data Flow:
  Surface syntax (arrays of symbols/integers)
    ↓ mark()
  Marked syntax (with combiner IDs, env IDs, array marks)
    ↓ unval()
  Marked syntax with suspended computations (freshCall marks)
    ↓ peval() [online partial evaluation]
  Partially evaluated terms (reduced suspensions, optimized)
    ↓ compiler backend
  WebAssembly bytecode
```

Each stage transforms value representations toward more optimized forms.

#### Fexpress's Stages

```
Value Data Flow:
  Racket source code
    ↓ parse & label
  AST nodes with positive types
    ↓ fexpress-eval/t+ dispatch
  Mixed compilation-result / interpreted values
    ↓ code generation
  Racket source code (with specialized lambdas, less re-entry)
```

#### Harmonized View

Both pipelines follow the same principle:

```
Raw Code
  ↓ Analyze: Add metadata (IDs/types)
Raw Code + Metadata
  ↓ Simplify: Identify static parts via progress checks
Partially Reduced Code
  ↓ Generate: Output optimized code
Optimized Code
```

**The transformations differ in detail, but the data flow is identical.**

---

## Part VII: Critical Distinctions and Why They Matter

### A. Scope and Completeness

| Aspect | Kraken | Fexpress |
|--------|--------|----------|
| **Scope** | Complete compiler to WebAssembly | Racket library/proof-of-concept |
| **Language Basis** | Minimal vau calculus + primitives | Generic Racket with Fexpress layer |
| **Evaluation Control** | Full (handles recursion, environments) | Partial (relies on host Racket) |
| **Production Readiness** | Complete with benchmarks | API unstable; experimental |

### B. Optimization Aggressiveness

| Aspect | Kraken | Fexpress |
|--------|--------|----------|
| **Environment Handling** | Fully optimized via needed-for-progress | Semi-optimized via type hints |
| **Benchmark Improvements** | 70,000x speedup | Not benchmarked formally |
| **Compilation Depth** | WebAssembly (machine-level) | Racket source code |
| **Type Soundness** | Strict (based on partial evaluation) | Unsound hints (pragmatic) |

### C. Representation Strategy

| Aspect | Kraken | Fexpress |
|--------|--------|----------|
| **Core Abstraction** | Partially-static environments with IDs | Positive types + continuations |
| **Metadata Model** | Set of needed-for-progress relations | Type system (annotations) |
| **Code Generation** | Bytecode emission (optimized backend) | S-expression generation (readable) |
| **Debugging** | Stack traces of original code | Readable generated Racket code |

---

## Part VIII: Correspondence Summary

### 20+ Points of Correspondence

1. **Base values (V)** ↔ **Positive types (type+)** – Both represent computable results
2. **Self-evaluating (S)** ↔ **specific-value/t+** – Literal constants
3. **Symbols with context** ↔ **at-variable/t+** – Lexical binding
4. **Real environments** ↔ **Static positive types** – Known compile-time data
5. **Fake environments** ↔ **Dynamic positive types** – Unknown runtime data
6. **Environment IDs** ↔ **Variable scope tracking** – Location of bindings
7. **Combiners (real E)** ↔ **clambda (static env)** – Optimizable closures
8. **Combiners (fake E)** ↔ **clambda (dynamic env)** – Preservable closures
9. **Derived combiners** ↔ **clambda instances** – First-class operatives
10. **eval(T,E) terms** ↔ **type+ evaluation** – Computing in context
11. **combine(T, args, E)** ↔ **apply/ce expressions** – Pending function calls
12. **peval() decision point** ↔ **fexpress-eval/t+ dispatch** – Optimization decision
13. **under() recovery** ↔ **Continuation restoration** – Fallback mechanism
14. **wrap_level** ↔ **Function signature in type_** – Evaluation metadata
15. **freshCall/attemptedCall** ↔ **Suspended operands** – Deferred computations
16. **needed-for-progress** ↔ **Type hint propagation** – Progress tracking
17. **returnOk() check** ↔ **Scope analysis** – Environment exit validity
18. **Combiner re-evaluation** ↔ **clambda specialization** – Context-dependent compilation
19. **Unval/PEval split** ↔ **fexpress-eval/t+ with hints** – Compile-vs-interpret decision
20. **Nested environments with distributive reification** ↔ **Lazy environment reification in clambda chains** – Avoiding environment explosion
21. **dropRV (eliminate redundant veval)** ↔ **Environment-appropriate code generation** – Removing re-entry calls
22. **FS (executing forms set)** ↔ **(missing in Fexpress)** – Infinite recursion detection
23. **needed-for-progress-infinite relation** ↔ **(missing in Fexpress)** – Recursion-aware optimization
24. **Mark pass** ↔ **Type annotation phase** – Metadata addition

### Summary Statistics

- **Identical concepts:** 18-20
- **Partial correspondence:** 2-4
- **Kraken-unique:** 2-3 (recursion handling)
- **Fexpress-unique:** 1-2 (pragmatic type hints, unsoundness)

---

## Part IX: Synthesis – A Unified Framework

### The Ideal Combined System

```
UNIFIED FEXPR COMPILER ARCHITECTURE
═══════════════════════════════════════

Input: Surface syntax (arrays/symbols)
  ↓
Annotation Phase (Kraken: mark; Fexpress: type inference)
  → Add environment IDs or type annotations
  → Identify static vs. dynamic data
  ↓
Representation: Partially-static data structures
  → Real/Fake environments (Kraken-style) OR
  → Positive types with metadata (Fexpress-style)
  ↓
Progress Analysis (Both systems do this!)
  → Compute needed-for-progress or type implications
  → Identify which forms can be optimized
  ↓
Specialized Compilation Phase
  - If all data is static: Fully inline/optimize
  - If partially static: Generate mixed code
  - If mostly dynamic: Generate dispatch + interpretation
  ↓
Choice Point: Generate what?
  → WebAssembly bytecode (Kraken: machine-efficient)
  → Native code (production compiler)
  → Source code (Fexpress: readable, portable)
  ↓
Optimization Passes
  - Type-inference-based primitive inlining (Kraken)
  - Environment reification optimization (both)
  - Redundant code elimination (both)
  - Tail call optimization (Kraken)
  ↓
Output: Compiled executable
```

### Key Principles Both Systems Embody

1. **Two-Level Representation:** Values vs. Computations
2. **Partial Evaluation:** Some parts static, others dynamic
3. **Context-Sensitive Optimization:** Different code for different calling contexts
4. **Environment Reification Strategy:** Lazy/distributed (not upfront)
5. **Progress Tracking:** Know when you can optimize more
6. **Fallback Mechanisms:** Graceful degradation when optimization fails

---

## Conclusion: Why This Matters

### For Fexpr Implementation

Understanding Kraken's approach (via Fexpress's simpler presentation) reveals that:

1. **Partially-static environments are fundamental** – You must model them explicitly
2. **Progress tracking is critical** – Naive partial evaluation leads to exponential blowup
3. **Call-site handling is the bottleneck** – Special-casing operatives is necessary
4. **Recursion handling is important** – The (form, environment) pair concept matters
5. **Lazy reification pays off** – Distributed environment building > upfront construction

### For Language Design

Both systems validate that:

1. **Fexprs can be production-ready** – With proper compilation infrastructure
2. **Macro-like operatives can compile to macro-like performance** – Via partial evaluation
3. **First-class operatives enable better abstraction** – Than separate function/macro split
4. **Type hints accelerate compilation** – But aren't necessary (pragmatic heuristics work)
5. **Readable output aids debugging** – Fexpress shows this; Kraken omitted it

### Remaining Challenges

1. **Scalability:** How do these approaches handle larger programs?
2. **Interoperability:** Can modules use different compilation strategies?
3. **IDE integration:** What debugging/introspection do users need?
4. **Soundness vs. Pragmatism:** When should type hints be enforced?
5. **Recursion patterns:** Can we detect and optimize specific recursion forms?

---

## References to Source Material

- Kraken paper: Braswell, Khan, Pande (2023) – TeX source included
- Fexpress: rocketnia (2021) – Blog post + Racket documentation
- Both demonstrate that practical fexpr compilation is achievable through complementary strategies that, when viewed through a data-first lens, are fundamentally harmonious.
