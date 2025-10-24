### Phase 0: Foundation & Infrastructure (Week 1-2)

1. **Project structure and build system**
   - Directory layout for modular components
   - Build configuration (Make/CMake or your incremental build system)
   - Version control setup

2. **Testing infrastructure**[3]
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
   - Implement hash consing for AST nodes[8][9]
   - Add function values (closures)
   - Add operative ($vau) values[10]
   - Environment chains for lexical scope

3. **Evaluator (expand)**
   - Function application (applicatives)
   - Operative application (fexprs) - receive unevaluated args[11][10]
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

1. **Control Flow Graph (CFG) builder**[12][13]
   - Convert AST/bytecode to basic blocks
   - Identify control flow edges
   - Entry and exit nodes

2. **SSA construction (skeletal)**[14][15][16]
   - Dominator tree computation (simple iterative algorithm)[17]
   - Dominance frontier calculation
   - φ-function insertion (even if buggy initially)
   - Variable renaming

3. **Liveness analysis (skeletal)**[18][19][20]
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
   - Define instruction set (stack-based)[21][22][23]
   - Bytecode emitter from AST
   - Bytecode printer for debugging

2. **Virtual Machine**[22][21]
   - Stack-based execution engine
   - Call frames for function calls
   - Integration with static analysis results

3. **Simple JIT (optional but recommended)**
   - Copy-and-patch style code generation[24][25][26]
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
   - Full fexpr semantics working correctly[10][11]
   - Macro expansion caching
   - Compile-time vs runtime boundary handling

2. **Type checking**
   - Convert types to runtime assertions[27][28]
   - Eventually: static type checking during partial evaluation
   - Clear error messages for type violations

3. **Optimal evaluation (begin)**
   - Source location-based labeling[29]
   - Sharing via hash consing[9][8]
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
   - Profile-guided optimization[30][31]
   - Better JIT tiers
   - Assembly intrinsics system

4. **Advanced static analysis**
   - Interprocedural analysis[32][33]
   - More precise destructor placement
   - Formal verification infrastructure

5. **Tooling**
   - Debugger integration
   - Profiler
   - Package manager
   - Documentation generator

6. **Bootstrap**[5][6][7]
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

1. **Always maintain a working system**[4][3]
   - Every commit should leave the compiler functional
   - Tests gate all changes
   - Continuous integration

2. **Incremental test-driven development**[3]
   - Write tests before implementing features
   - Each feature has 5-10 tests minimum
   - Golden tests for regression prevention

3. **Modular architecture**[34]
   - Clear interfaces between components
   - Components can be refined independently
   - Document component contracts

4. **Progressive elaboration**[1][3]
   - Start with simplest cases
   - Add complexity gradually
   - Refactor when patterns emerge

5. **Concurrent refinement**
   - Work on multiple components simultaneously
   - Don't wait for one to be "finished"
   - Integrate frequently

6. **Bootstrap mindset**[7][5]
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
   - Copy-and-patch is simplest JIT approach[25][24]

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

The key is maintaining a **fully functional system at every stage** while all components evolve concurrently. This matches how successful compilers like GCC, LLVM, Rust, and GHC were developed - not as isolated phases, but as continuous refinement of an integrated whole.[6][5][7]

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
