I now have comprehensive information from the user's files and extensive web research. Let me create a detailed foundational plan for the compiler design.

## Compiler Design: High-Level Architecture and Foundational Plan

Your compiler design incorporates several advanced concepts from modern compiler research and addresses ambitious goals for a production-quality system. Based on your stream-of-consciousness notes and extensive research, here's a structured foundational plan that scopes the design and identifies the high-level architecture.

### Frontend: Parsing and Semantic Analysis

**Lexical Analysis & Parsing**

The frontend transforms source code into an abstract syntax tree (AST). You've identified several approaches worth considering:

- **Scannerless parsing** eliminates the traditional separate lexer phase, though you may use a deterministic Lisp-like syntax initially for practical bootstrapping
- **General parsers** (Earley, GLR) can handle arbitrary BNF grammars but require on-the-fly disambiguation to avoid exponential parse forest growth
- **Language model preprocessing** could standardize input, though this remains exploratory

The key challenge is managing **F-expressions (fexprs)**, which receive unevaluated operands, acting as first-class special forms. Unlike traditional macros that expand at compile-time, fexprs operate at runtime, requiring the AST to persist throughout compilation. This architectural choice has profound implications:

- Name resolution must be delayed until macro evaluation completes
- The AST incrementally converts to IR only when determining whether an operator is applicative (function) or operative (fexpr/macro)
- Your use of `$vau` (Kernel-style) enables first-class operatives without phase separation between compile-time and runtime

**Semantic Analysis**

Semantic analysis is intertwined with the macro system and partial evaluation phase:

- **Name resolution** operates in a dynamic scoping environment where names can be overridden from parent function contexts
- **Type checking** happens during partial evaluation, treating types as assertions that must be formally verified to never trigger
- **Desugaring** is implemented as macro evaluation, where syntactic sugar translates to core language primitives

### Core Language: Linear Logic Sequent Calculus

You've chosen a **linear logic-based core language** over traditional lambda calculus, which offers several advantages for optimal evaluation:

**Structure**

The core uses **two-sided sequent calculus** where programs are represented as sequent proofs:
- Each linear logic connective has corresponding left and right introduction rules
- Function application corresponds to left rules (eliminating implications)
- Function abstraction corresponds to right rules (introducing implications)
- Explicit tracking of duplication and contraction through linear logic rules

**Benefits**

- **Optimal reduction support**: Linear logic explicitly marks where duplication/contraction occurs, simplifying optimal evaluation analysis
- **Broader expressiveness**: Captures more computational patterns than pure lambda calculus while maintaining formal properties
- **Direct correspondence**: Maps naturally to interaction nets and graph reduction for optimal evaluation

The implementation will be more complex than lambda calculus (which has a single constructor), requiring careful handling of multiple constructs.

### Middle End: Hybrid Evaluation Engine

The middle end is the most complex component, unifying macro evaluation, partial evaluation, JIT compilation, and optimization.

**Hybrid JIT Architecture**

Your design follows **hybrid tracing** approaches that combine trace-based and method-based JIT compilation:

**Multi-tier compilation**:
1. **Fast-start interpretation**: Immediate execution without compilation overhead
2. **Baseline JIT** (copy-and-patch): Rapid template-based compilation for frequently executed code
3. **Optimizing JIT** (tracing/method-based): Aggressive optimization for hot code paths
4. **Profile-guided optimization**: Reoptimization based on observed runtime behavior

**Copy-and-Patch Compilation**

This technique generates code with negligible compilation cost:
- Pre-compile "stencils" (small code templates) using LLVM/Clang at -O3
- At runtime, copy pre-compiled machine code fragments and patch in constants, addresses, and operands
- Achieves compilation speeds orders of magnitude faster than traditional JITs while approaching partially-optimized code quality
- Python 3.13 adopted this approach for its tier-1 JIT

**Partial Evaluation**

Partial evaluation specializes code based on known inputs:
- **For fexprs**: Since fexpr inputs (AST structure) are fully known at compile time, partial evaluation can expand them similarly to macros while maintaining runtime flexibility
- **For general code**: The engine performs Futamura-projection-style specialization, where specializing an interpreter with a program yields a compiled version

**Optimal Evaluation**

Based on Lamping's algorithm and interaction nets:
- **Interior sharing**: Incremental duplication of shared functions prevents unnecessary β-reductions
- **Fan nodes**: Manage multiple overlapping sharing contexts with indexed delimiters
- **Label-based memoization**: Track expressions by source location rather than pure hash-consing, ensuring expressions from the same source location evaluate as a family
- Combines structural sharing with value-level deduplication to avoid redundant computation

**Intermediate Representation**

The IR must support:
- **Control flow graph**: After optimization, code exists as basic blocks with assembly intrinsics
- **Assembly intrinsics**: Each assembly instruction wrapped as a pure function (one-to-one mapping)
- **Inline assembly**: Assembly operations embedded throughout the compilation pipeline as intrinsics
- **Type annotations**: Preserved for formal verification during partial evaluation

### Backend: Code Generation

**Unison-Based Architecture**

The backend uses **constraint-based integrated register allocation and instruction scheduling**:

- **Unison** performs register allocation and instruction scheduling simultaneously using constraint programming
- Solves both problems optimally (for functions up to ~1000 instructions) while considering their interdependencies
- Mean speedup of 1.1%-10% and code size reduction of 1.3%-3.8% over LLVM for various architectures
- Integrated with LLVM toolchain, operates on LLVM IR as input

**Key Backend Components**:
1. **Instruction selection**: Maps IR operations to assembly intrinsics (already done by intrinsic-based IR design)
2. **Register allocation**: Constraint-based optimal assignment of variables to registers
3. **Instruction scheduling**: Reordering to maximize instruction-level parallelism while respecting dependencies
4. **Code emission**: Generate native machine code using libraries like XED (for x86)

**Assembly Intrinsics**

Your assembly-centric approach differs from traditional IRs:
- **Architecture-dependent by design**: Forces explicit handling of different instruction sets
- **Complete coverage**: Supports obscure instructions that platform-independent IRs cannot represent
- **Direct lowering**: Intrinsics map one-to-one to assembly, simplifying code generation
- While this adds tedious enumeration work, it enables full exploitation of processor-specific features

### Linker and Executable Generation

**Custom Linker**

Replace traditional linkers with a language-specific solution:
- **Motivation**: Traditional object formats (ELF) are slow and ill-suited for modern compilation at the function level
- **Fast linking techniques**: Memory management, parallel processing, minimized deserialization
- **Integration**: Works with incremental build system rather than separate object files
- **Format**: Eventually eliminate object files entirely, storing information in the incremental build database

**Output Formats**:
- **Static binaries**: Standard executables
- **Position-independent executables (PIE)**: Support ASLR for security
- **Shared libraries**: For compatibility, though statically-linked is preferred
- **Debug information**: DWARF format for debugger integration

### Incremental Build System

Your **forward-model incremental compilation** differs from query-based systems (like Rust's):

**Architecture**:
- **Task-based execution**: Each compilation unit is a task that records read/write dependencies
- **Dependency tracking**: Automatically trace inputs/outputs without predefined query structure  
- **Forward execution**: Run tasks in natural order, checking if dependencies match cached results
- **Reuse on match**: Skip execution if dependencies unchanged; rerun otherwise

**Integration with Compiler**:
- **Fine-grained units**: Individual functions or smaller units maximize reuse
- **Parallel execution**: Task scheduler manages multi-threaded compilation
- **Shared database**: All phases store results in unified incremental database rather than separate object files
- **Profile caching**: Optimization decisions can be rechecked rather than rediscovered

**Trace Database**:
- Records input-output relationships during execution
- Supports clean builds (from scratch) and incremental builds (only affected tasks)
- Enables build cache with constructive traces for reproducibility

### Memory Management and Formal Verification

**Memory Management**

Use **formal verification** to determine when memory can be freed:
- **Reachability analysis**: Find the first point where memory is provably not used (rather than the last use, which is incomputable)
- **Control flow graph analysis**: Detailed analysis of all execution paths
- **Undecidable properties**: Handle Turing-complete programs using heuristics from formal verification literature
- Implement as **automatic destructors/finalizers** that are inserted by the compiler

**Static Verification**:
- **Type safety**: Types as assertions checked during partial evaluation
- **Bounds checking**: Buffer overflow prevention
- **Balance checking**: Resource usage verification  
- **Control flow integrity**: Ensuring valid execution paths
- **Deadlock detection**: Thread safety analysis for concurrent code

**Approach**: Treat verification as proving unreachability of error states in the control flow graph generated during partial evaluation.

### Safepoints and Deoptimization

**Safepoints**

Points where program state is well-defined for GC, profiling, and deoptimization:
- **Placement**: Method exits, calls, and loop backedges (not every instruction)
- **State reconstruction**: Map compiled code locations to interpreter state for deoptimization
- **Stack traces**: DWARF information + safepoint metadata enables debugging

**Your exploration**:
- Consider making "everything a safepoint" using traps and stack trace reconstruction
- Requires evaluating if overhead is acceptable vs. traditional sparse safepoint placement

### Debugging and Profiling Tools

**Debugger**:
- **Omniscient debugging**: Record entire execution for backward/forward navigation
- **Hot reloading**: Modify code during execution (requires JIT recompilation)
- **Breakpoints/watchpoints**: Standard debugging facilities
- **Step execution**: Single-step, step-out, continue
- **State inspection**: View variables, call stack, memory

**Profiler**:
- **Profile-guided optimization**: Record counters, branch frequencies, call patterns
- **Instrumentation vs. sampling**: Trade accuracy for overhead
- **Multi-tier support**: Different profiling at interpreter, baseline JIT, and optimizing JIT tiers

### Practical Considerations

**Language for Implementation**

Still to be determined, but considerations include:
- Performance for compiler infrastructure
- Support for the incremental build system
- Ability to eventually self-host

**Testing Strategy**:
- **Golden tests**: Serialize compiler outputs and compare against expected results
- **Unit tests**: Test individual passes and components
- **Integration tests**: Full compilation pipelines with example programs
- **Fuzzing**: Generate random valid programs to stress-test
- **Regression tests**: Prevent reintroduction of fixed bugs

**Bootstrapping**:
- Support **self-hosting**: Compiler can compile itself
- Multi-stage bootstrap from simple to full-featured compiler
- Critical for validation and dogfooding

### High-Level Compilation Pipeline

Here's the complete flow:

```
Source Code
    ↓
Parser (Lisp-like syntax initially)
    ↓
AST (with F-expressions preserved)
    ↓
Macro Evaluation Engine
    ↓  (incremental conversion)
Core Language (Linear Logic Sequent Calculus)
    ↓
Partial Evaluation / Optimization
    ↓
Control Flow Graph (with Assembly Intrinsics)
    ↓
Unison Backend (Register Allocation + Scheduling)
    ↓
Native Code Generation
    ↓
Custom Linker
    ↓
Executable (Static/PIE/Shared)
```

**Parallel Execution Modes**:
1. **Interactive Mode**: Full JIT with interpreter fallback, profiling, and on-stack replacement
2. **AOT Mode**: Ahead-of-time compilation with optional profile-guided optimization
3. **Image Mode**: Reduced-capability executable with minimal runtime

### Key Challenges and Research Areas

1. **F-expression partial evaluation**: Developing efficient strategies to expand fexprs at compile-time while maintaining runtime flexibility
2. **Optimal evaluation implementation**: Managing delimiter accumulation and ensuring the overhead doesn't exceed benefits
3. **IR design for multiple concerns**: Balancing support for optimal evaluation, partial evaluation, and code generation
4. **Formal verification scalability**: Handling large programs with complex control flow for memory management and type checking
5. **Incremental compilation granularity**: Finding the right balance between fine-grained caching and overhead
6. **Multi-tier JIT coordination**: Smoothly transitioning between interpretation, baseline JIT, and optimizing JIT

### Phase 0: Foundation & Infrastructure (Week 1-2)

1. **Project structure and build system**
   - Directory layout for modular components
   - Build configuration (Make/CMake or your incremental build system)
   - Version control setup

2. **Testing infrastructure**
   - Test harness that can run programs and compare outputs
   - Golden test framework (save expected outputs)
   - Simple test runner that reports pass/fail
   - Initial test suite with ~10 trivial programs

3. **Basic tooling**
   - REPL skeleton (read-eval-print loop)
   - Command-line argument parsing
   - Logging/debugging output system

**Deliverable:** A working test harness that can execute a "hello world" equivalent, even if it's hardcoded.

***

### Phase 1: End-to-End Skeleton (Week 3-4)

**Goal:** Create the thinnest possible slice through the entire system - from source text to execution.

**Components (all minimal/buggy versions):**

1. **Parser (skeletal)**
   - S-expression parser for simplest Lisp syntax
   - Produces basic AST nodes
   - Only handles: literals (numbers, strings), symbols, lists
   - No error handling yet

2. **Core representation**
   - Define AST node types
   - Basic value types (integers, symbols, lists)
   - Source location tracking structure (even if not populated)

3. **Evaluator (skeletal)**
   - Tree-walking interpreter
   - Hardcoded primitives: `+`, `-`, `print`
   - Simple environment (hash table: symbol → value)
   - No functions, no macros yet - just evaluate primitives

4. **Memory management (skeletal)**
   - Use host language GC for now
   - Document where destructors will be inserted later

**Example test program:**
```scheme
(print (+ 1 2))  ; Should output: 3
```

**Deliverable:** Can parse, evaluate, and execute trivial arithmetic programs. Success = passing 20-30 basic tests.

***

### Phase 2: Core Language Features (Week 5-8)

**Goal:** Add enough language features to write simple programs, including the most basic form of functions and macros.

**Components to expand:**

1. **Parser (expand)**
   - Add quote/quasiquote syntax
   - Better error messages with line numbers
   - Handle comments

2. **AST & Core Language**
   - Implement hash consing for AST nodes
   - Add function values (closures)
   - Add operative ($vau) values
   - Environment chains for lexical scope

3. **Evaluator (expand)**
   - Function application (applicatives)
   - Operative application (fexprs) - receive unevaluated args
   - `define` for global bindings
   - `lambda` or `$vau` for creating functions/operatives
   - Basic control flow: `if`

4. **Type system (skeletal)**
   - Runtime type tags on values
   - Type annotation syntax (parsed but not checked yet)

**Example test programs:**
```scheme
; Functions
(define (square x) (* x x))
(print (square 5))  ; 25

; Simple macro (operative)
(define ($my-if $vau (test then else) env)
  (if (eval test env)
      (eval then env)
      (eval else env)))
```

**Deliverable:** Can write simple recursive functions and basic macros. ~100 tests passing.

***

### Phase 3: Static Analysis Infrastructure (Week 9-12)

**Goal:** Build the CFG and SSA infrastructure needed for destructor analysis.

**Components to add:**

1. **Control Flow Graph (CFG) builder**
   - Convert AST/bytecode to basic blocks
   - Identify control flow edges
   - Entry and exit nodes

2. **SSA construction (skeletal)**
   - Dominator tree computation (simple iterative algorithm)
   - Dominance frontier calculation
   - φ-function insertion (even if buggy initially)
   - Variable renaming

3. **Liveness analysis (skeletal)**
   - Backward dataflow analysis
   - Compute last-use points
   - Conservative approximation for now

4. **Destructor handling (skeletal)**
   - Syntax for resources: `(resource x (acquire ...)) ... (destroy x)`
   - Insert destructor calls at computed points
   - Allow manual `destroy` for testing

**Example test program:**
```scheme
(define (test-file filename)
  (resource file (open-file filename))
  (if (valid? file)
      (process file)
      (log-error))
  ; destructor inserted here automatically
  'done)
```

**Deliverable:** Can correctly insert destructors for simple linear code and basic branches. ~150 tests.

***

### Phase 4: JIT/Compilation Pipeline (Week 13-18)

**Goal:** Replace tree-walking interpreter with bytecode VM and basic JIT.

**Components to add:**

1. **Bytecode format**
   - Define instruction set (stack-based)
   - Bytecode emitter from AST
   - Bytecode printer for debugging

2. **Virtual Machine**
   - Stack-based execution engine
   - Call frames for function calls
   - Integration with static analysis results

3. **Simple JIT (optional but recommended)**
   - Copy-and-patch style code generation
   - Or just emit assembly for hot functions
   - Target x86-64 initially

4. **Integration of static analysis with compilation**
   - Run SSA/liveness analysis on functions before compiling
   - Emit bytecode with destructor calls inserted
   - Verify correctness with extensive tests

**Deliverable:** Bytecode VM working correctly. Programs run faster than tree-walker. ~200 tests.

***

### Phase 5: Advanced Features & Refinement (Week 19-24)

**Goal:** Fill in missing features and improve correctness/performance.

**Components to refine:**

1. **Macro system**
   - Full fexpr semantics working correctly
   - Macro expansion caching
   - Compile-time vs runtime boundary handling

2. **Type checking**
   - Convert types to runtime assertions
   - Eventually: static type checking during partial evaluation
   - Clear error messages for type violations

3. **Optimal evaluation (begin)**
   - Source location-based labeling
   - Sharing via hash consing
   - Defer interaction nets to later

4. **IR improvements**
   - Better CFG construction
   - More sophisticated SSA construction
   - Improved liveness analysis (interprocedural later)

5. **Optimization passes (basic)**
   - Dead code elimination
   - Constant folding
   - Simple inlining

**Deliverable:** A relatively complete compiler that can compile non-trivial programs. ~500 tests.

***

### Phase 6: Production Readiness (Week 25+)

**Goal:** Make the system robust, performant, and usable for real work.

**Ongoing refinement:**

1. **Error handling & diagnostics**
   - Beautiful error messages with context
   - Warnings for common mistakes
   - Stack traces for debugging

2. **Standard library**
   - Data structures: lists, vectors, hash tables
   - I/O primitives
   - String manipulation
   - Numeric tower

3. **Performance optimization**
   - Profile-guided optimization
   - Better JIT tiers
   - Assembly intrinsics system

4. **Advanced static analysis**
   - Interprocedural analysis
   - More precise destructor placement
   - Formal verification infrastructure

5. **Tooling**
   - Debugger integration
   - Profiler
   - Package manager
   - Documentation generator

6. **Bootstrap**
   - Rewrite compiler in itself
   - Self-hosting verification

**Deliverable:** Production-quality compiler comparable to GHC/Rust/Zig.

***

### Work Breakdown Structure (WBS) Summary

| **Phase** | **Duration** | **Key Deliverable** | **Test Count** |
|-----------|--------------|---------------------|----------------|
| 0: Foundation | 1-2 weeks | Test infrastructure | ~10 |
| 1: Skeleton | 2 weeks | End-to-end execution | ~30 |
| 2: Core Language | 4 weeks | Functions & macros working | ~100 |
| 3: Static Analysis | 4 weeks | Destructor insertion | ~150 |
| 4: JIT/VM | 6 weeks | Bytecode compilation | ~200 |
| 5: Advanced Features | 6 weeks | Complete feature set | ~500 |
| 6: Production | Ongoing | Robust, optimized system | 1000+ |

***

### Key Principles

1. **Always maintain a working system**
   - Every commit should leave the compiler functional
   - Tests gate all changes
   - Continuous integration

2. **Incremental test-driven development**
   - Write tests before implementing features
   - Each feature has 5-10 tests minimum
   - Golden tests for regression prevention

3. **Modular architecture**
   - Clear interfaces between components
   - Components can be refined independently
   - Document component contracts

4. **Progressive elaboration**
   - Start with simplest cases
   - Add complexity gradually
   - Refactor when patterns emerge

5. **Concurrent refinement**
   - Work on multiple components simultaneously
   - Don't wait for one to be "finished"
   - Integrate frequently

6. **Bootstrap mindset**
   - Design for self-hosting from the start
   - Use your language to test itself early
   - Dog-food your own compiler

***

### Risk Management

**High-risk areas to address early:**

1. **Fexpr + static analysis integration**
   - Prototype this interaction in Phase 3
   - Have contingency: conservative destructor placement

2. **JIT complexity**
   - Start with interpreter, add JIT later if too hard
   - Copy-and-patch is simplest JIT approach

3. **Memory management**
   - Host language GC acceptable initially
   - Document precise semantics early

4. **Linear logic core language**
   - May be too complex - fallback to lambda calculus
   - Prototype before committing

***

### Tracking Progress

**Daily/Weekly habits:**

- Commit working code daily
- Run full test suite before each commit
- Weekly review: what's working, what's blocked
- Maintain backlog of known issues
- Prioritize blockers and high-value features

**Metrics to track:**

- Test pass rate (should stay >95%)
- Lines of code (growth indicates progress)
- Test coverage (aim for >80% eventually)
- Performance benchmarks (track over time)

***

### Summary

This plan gives you a structured approach to evolutionary prototyping:

- **Weeks 1-4:** Working skeleton end-to-end
- **Weeks 5-12:** Core language + static analysis
- **Weeks 13-24:** Compilation pipeline + advanced features
- **Week 25+:** Polish and production readiness

The key is maintaining a **fully functional system at every stage** while all components evolve concurrently. This matches how successful compilers like GCC, LLVM, Rust, and GHC were developed - not as isolated phases, but as continuous refinement of an integrated whole.

Start coding on Week 1 with the test infrastructure, and you'll have a working (if limited) compiler by Week 4!

Phase 1: Minimal Viable Compiler
    Simple Lisp-like parser
    Basic AST representation
    Core language translation (without full optimal evaluation)
    Simple interpreter
    Minimal type checking
Phase 2: Incremental Build System

    Task scheduler with dependency tracking

    Trace database

    File monitoring and change detection

    Basic caching

Phase 3: Evaluation Engine

    Macro/fexpr evaluation

    Basic partial evaluation

    Simple optimization passes

    IR with assembly intrinsics

Phase 4: Code Generation

    Target x86-64 initially

    Basic instruction selection

    Simple register allocation

    Code emission

Phase 5: Advanced Features

    Unison integration for optimal register allocation

    Optimal evaluation with interaction nets

    Multi-tier JIT compilation

    Formal verification for memory management

    Profile-guided optimization
