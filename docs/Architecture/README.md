# Architecture & Design

This documentation is for **compiler developers and contributors**.

Use this section to understand:
- How the compiler works
- How components interact
- Why design decisions were made
- Performance characteristics

**For end users learning the language**, see `/Documentation/`.
**For understanding language features**, see `/LanguageSemantics/`.

**Purpose:** Help compiler developers and contributors understand how the system works, why components exist, and how they interact.

**Contents:**
- Compiler phases and their interactions
- IR representations and transformations for understanding transformations and passes
- Component responsibility mapping
- **ADR log** provides design decisions and trade-offs (via Architecture Decision Records) without cluttering code
- Data flow through the compilation pipeline
- Performance characteristics and optimization strategies

## Layout

- **Architecture/Overview.md** is the starting point for understanding the system
- **Compiler-Phases/** shows data flow and transformations
- **DecisionLog/** explains why things are the way they are
- **ADRs capture architectural knowledge** in structured format

### When writing about "how compiler components interact":
→ Goes in `Architecture/Compiler-Phases/` or `Architecture/IR/`

### When writing about "how the type system works":
→ Goes in `LanguageSemantics/CoreConcepts/` (for users understanding the language)  
→ **AND** possibly in `Architecture/Components/TypeChecker.md` (for developers implementing it)

### When writing about "IR transformations and optimization strategies":
→ Goes in `Architecture/IR/Passes/`

### When writing about "memory layout and management in compiled code":
→ Goes in `Architecture/DataStructures.md` or `Architecture/Performance/`

### When documenting "why we chose term rewriting":
→ Goes in `Architecture/DecisionLog/` as an ADR
