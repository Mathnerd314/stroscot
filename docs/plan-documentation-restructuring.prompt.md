## Plan: Documentation Restructuring and Distillation

### TL;DR
Systematically restructure the Stroscot documentation by:
1. Creating a new target structure based on Diataxis framework and the planned organization in `Documentation-Organization.md`
2. Writing new consolidated documentation pieces for each section
3. Verifying sentence-by-sentence coverage of existing docs against new structure
4. Fixing gaps, inconsistencies, and outdated content

### Current State Analysis
- **Architecture/** (27 files): Compiler internals, IR, phases, optimization, memory, parsing, errors
- **LanguageSemantics/** (30+ files): Language features, types, syntax, standard library
- **ProjectMeta/** (10 files): Project governance, documentation meta-docs
- **User Documentation/** (5 files): Minimal user-facing content (FAQ, Install, StyleGuide, Introduction)
- **Root**: index.md, INDEX.md, myst.yml, structure.txt

### Target Structure (from Documentation-Organization.md)
```
docs/
├── Architecture/                      # Compiler developer documentation
│   ├── README.md
│   ├── Overview.md
│   ├── CompilerPhases/
│   │   ├── Overview.md
│   │   ├── Lexical.md
│   │   ├── Parsing.md
│   │   ├── TypeChecking.md
│   │   ├── IRConstruction.md
│   │   ├── Optimization.md
│   │   ├── Reduction.md
│   │   ├── TermRewriting.md
│   │   └── CodeGeneration.md
│   ├── IR/
│   │   ├── Overview.md
│   │   ├── Representation.md
│   │   ├── CoreDataStructures.md
│   │   ├── Passes/
│   │   │   ├── Reduction.md
│   │   │   ├── TermRewriting.md
│   │   │   └── Reduction-Example.md
│   │   └── Transformations.md
│   ├── Components/
│   │   ├── Lexer.md
│   │   ├── Parser.md
│   │   ├── Optimizer.md
│   │   ├── TypeChecker.md
│   │   ├── CodeGenerator.md
│   │   └── ErrorReporting.md
│   ├── DecisionLog/ # Architecture Decision Records
│   │   ├── README.md
│   │   ├── ADR-001-IR-Strategy.md
│   │   ├── ADR-002-Multiple-vs-Single.md
│   │   └── [more ADRs...]
│   ├── DataStructures.md
│   ├── Performance/
│   │   ├── Profiling-Guide.md
│   │   ├── Benchmarking.md
│   │   └── Optimization-Strategy.md
│   └── Tools/
│       ├── CompilerTools.md
│       └── DebuggingTechniques.md
│
├── LanguageSemantics/                 # Language users & designer documentation
│   ├── README.md
│   ├── Overview.md
│   ├── CoreConcepts/                  # Diataxis: Explanations
│   │   ├── Types.md
│   │   ├── Evaluation-Strategy.md
│   │   ├── Memory-Model.md
│   │   ├── Modules.md
│   │   ├── Objects.md
│   │   ├── Dispatch.md
│   │   ├── State.md
│   │   ├── State-Management.md
│   │   ├── Expressions.md
│   │   └── Aspects.md
│   ├── AdvancedFeatures/               # Diataxis: Explanations
│   │   ├── Concurrency.md
│   │   ├── Exceptions.md
│   │   ├── Concurrency-Model.md
│   │   ├── Exception-Handling.md
│   │   ├── Logic-Programming.md
│   │   ├── Functional-Logic.md
│   │   ├── Macros.md
│   │   ├── Resource-Management.md
│   │   ├── Security.md
│   │   ├── Security-Model.md
│   │   ├── Verification.md
│   │   ├── Verification-Framework.md
│   │   ├── Sets-and-Posets.md
│   │   ├── Sequent-Calculus.md
│   │   ├── Advanced-Algebra.md
│   │   ├── Assembly-Interface.md
│   │   ├── Assembly-Level.md
│   │   └── Optimization-Hints.md
│   └── References/                    # Diataxis: Reference
│       ├── Type-System.md
│       ├── Complete-Type-Spec.md
│       ├── Built-in-Primitives.md
│       ├── Aspect-Annotations.md
│       ├── Standard-Annotations.md
│       └── Performance-Characteristics.md
│
├── Documentation/                     # User-focused (Diataxis)
│   ├── GettingStarted/                 # Onboarding - Entry point for users
│   │   ├── Introduction.md
│   │   ├── Installation.md
│   │   ├── StyleGuide.md
│   │   └── FAQ.md
│   ├── Tutorials/                     # Learning-oriented
│   │   ├── Getting-Started.md
│   │   ├── Your-First-Program.md
│   │   ├── Building-Your-First-Program.md
│   │   ├── Building-Basic-Functions.md
│   │   ├── Working-with-Types.md
│   │   └── ...
│   ├── HowTo/                         # Goal-oriented
│   │   ├── BuildSystem.md
│   │   ├── Using-Transactions.md
│   │   ├── Memory-Management.md
│   │   ├── Destructors.md
│   │   ├── DynamicOverriding.md
│   │   └── ...
│   ├── Reference/                     # Information-oriented
│   │   ├── Project-Structure.md
│   │   ├── Grammar/
│   │   │   ├── EBNF.md
│   │   │   ├── Lexical.md
│   │   │   ├── Syntax.md
│   │   │   └── Abstract-Syntax.md
│   │   ├── Error-Messages/
│   │   │   ├── Ambiguous-Identifier.md
│   │   │   ├── Type-Errors.md
│   │   │   └── ...
│   │   ├── Libraries/
│   │   │   ├── Standard-Library.md
│   │   │   ├── BuildSystem.md
│   │   │   ├── Compiler-Library.md
│   │   │   ├── Package-Manager.md
│   │   │   ├── Parsing-Utilities.md
│   │   │   ├── Symbolic-Computation.md
│   │   │   ├── Time-Utilities.md
│   │   │   └── Units.md
│   │   ├── Glossary.md
│   │   └── Tooling-Reference.md
│   └── Explanations/                  # Understanding-oriented
│       ├── Design-Philosophy.md
│       ├── Why-Term-Rewriting.md
│       ├── Why-Logic-Programming.md
│       ├── Evaluation-Model.md
│       └── Security-Rationale.md
│
├── ProjectMeta/                       # Project governance
│   ├── Contributing.md
│   ├── Code-of-Conduct.md
│   ├── Roadmap.md
│   ├── Community.md
│   ├── Funding.md
│   ├── Learning-Resources.md
│   ├── Documentation/       # How is all this organized? Why organize it this way?
│   └── Guidelines.md                  # Writing guidelines
│
└── Archive/Deprecated/                # ← Cleanup old reference docs
    ├── README.md                      # Explains what's here and why
    └── Old-Reference/                 # Old versions of docs
        ├── ...
        └── [All outdated material]
```

### Steps

#### Phase 1: Setup and Inventory (Week 1)
1. **Create target directory structure** - Create all directories in the target structure
2. **Inventory all existing content** - Map every existing .md/.txt file to target location(s)
3. **Create coverage tracking spreadsheet** - Track sentence-by-sentence coverage

#### Phase 2: Architecture Documentation (Weeks 2-3)
4. **Write Architecture/Overview.md** - Consolidate CompilerOverview.md, Compiler_documentation.md, PhasePlan.md
5. **Write Architecture/CompilerPhases/** - Create 9 phase documents from existing content
6. **Write Architecture/IR/** - Consolidate IR/Overview.md, CoreSyntax.md, Passes/
7. **Write Architecture/Components/** - Create 6 component docs from Errors.md, Components/
8. **Write Architecture/DecisionLog/** - Create ADR template and migrate key decisions
9. **Write Architecture/DataStructures.md** - From Memory.md, Glossary.md
10. **Write Architecture/Performance/** - From Performance/, CompilerOverview.md optimization sections
11. **Write Architecture/Tools/** - From Tools/, testing_notes.md

#### Phase 3: Language Semantics Documentation (Weeks 3-4)
12. **Write LanguageSemantics/Overview.md** - New overview of the language
13. **Write LanguageSemantics/CoreConcepts/** - 10 documents from AdvancedFeatures/ and CoreConcepts/
14. **Write LanguageSemantics/AdvancedFeatures/** - 18 documents from AdvancedFeatures/
15. **Write LanguageSemantics/References/** - 7 reference documents from References/Syntax/

#### Phase 4: User Documentation (Weeks 4-5)
16. **Write Documentation/GettingStarted/** - 4 documents from User Documentation/GettingStarted/, HowTo/
17. **Write Documentation/Tutorials/** - Create progressive tutorial series
18. **Write Documentation/HowTo/** - Task-oriented guides from existing content
19. **Write Documentation/Reference/** - Reference materials from LanguageSemantics/References/, StandardLibrary/
20. **Write Documentation/Explanations/** - Design rationale from Architecture/, LanguageSemantics/

#### Phase 5: Project Meta (Week 5)
21. **Write ProjectMeta/** - Consolidate governance docs
22. **Write ProjectMeta/Documentation/** - Documentation meta-docs

#### Phase 6: Verification and Cleanup (Week 6)
23. **Sentence-by-sentence coverage verification** - Compare old vs new docs
24. **Fix gaps and inconsistencies** - Address missing content, contradictions
25. **Create Archive/Deprecated/** - Move old docs with README explaining
26. **Update index.md and myst.yml** - Configure new TOC
27. **Build and verify** - Run myst build, check for broken links

### Relevant Files to Modify/Create

**New directories to create:**
- `docs/Architecture/CompilerPhases/` (9 files)
- `docs/Architecture/IR/Passes/` (3 files)
- `docs/Architecture/Components/` (6 files)
- `docs/Architecture/DecisionLog/` (template + ADRs)
- `docs/Architecture/Performance/` (3 files)
- `docs/Architecture/Tools/` (2 files)
- `docs/LanguageSemantics/CoreConcepts/` (10 files)
- `docs/LanguageSemantics/AdvancedFeatures/` (18 files)
- `docs/LanguageSemantics/References/` (7 files)
- `docs/Documentation/GettingStarted/` (4 files)
- `docs/Documentation/Tutorials/` (5+ files)
- `docs/Documentation/HowTo/` (6+ files)
- `docs/Documentation/Reference/Grammar/` (4 files)
- `docs/Documentation/Reference/Error-Messages/` (multiple)
- `docs/Documentation/Reference/Libraries/` (7 files)
- `docs/Documentation/Explanations/` (5 files)
- `docs/ProjectMeta/Documentation/` (existing)
- `docs/Archive/Deprecated/Old-Reference/` (for old content)

**Key source files to mine:**
- `docs/Architecture/CompilerOverview.md` (500+ lines)
- `docs/Architecture/PhasePlan.md` (500+ lines)
- `docs/Architecture/Compiler_documentation.md` (300+ lines)
- `docs/Architecture/IR/Overview.md`, `CoreSyntax.md`, `Passes/Reduction.md`
- `docs/Architecture/Errors.md` + `Components/errors/` (40+ error files)
- `docs/Architecture/Memory.md`, `Parsing.md`, `Verification.md`
- `docs/LanguageSemantics/AdvancedFeatures/Types.md` (300+ lines)
- `docs/LanguageSemantics/AdvancedFeatures/Syntax.md` (300+ lines)
- `docs/LanguageSemantics/References/Syntax/` (10 files including 3 EBNF grammars)
- `docs/LanguageSemantics/StandardLibrary/` (10 files)
- `docs/ProjectMeta/Documentation/Documentation-Organization.md` (target structure)
- `docs/ProjectMeta/Documentation/Documentation.md` (guidelines)

### Verification

1. **Automated coverage check**: Script to compare sentence embeddings between old and new docs
2. **Manual review**: For each target document, verify all relevant old content is incorporated
3. **Build verification**: `myst build` succeeds with no warnings
4. **Link checking**: All internal cross-references resolve
5. **Completeness checklist**: Every planned target file exists and has content

### Decisions

- **Scope**: Restructure only - no new technical content creation beyond synthesis
- **Forbidden terms**: Enforce Documentation.md forbidden words list (types, dynamic/static, strongly typed, pure, undecidable, primitive, ⊂, abomination, etc., homoiconic)
- **Diataxis compliance**: Each document must clearly serve one of: Tutorial, HowTo, Reference, Explanation
- **Archive strategy**: Move (not delete) old content to Archive/Deprecated/ with explanatory README
- **Incremental approach**: Can deliver phases independently; Phase 1-2 usable for compiler devs, Phase 3-4 for language users

### Further Considerations

1. **LLM-assisted restructuring**: Use LLM to help synthesize content from multiple source files into coherent target documents
2. **Diagram generation**: Plan to create Mermaid/Graphviz diagrams for Architecture/Overview.md and IR/Overview.md
3. **Example programs**: Need to create/update example programs in tutorials and reference docs
4. **Glossary integration**: Central glossary in ProjectMeta/Documentation/ or Architecture/Glossary.md with cross-refs
5. **Versioning**: Consider how to version docs alongside language versions