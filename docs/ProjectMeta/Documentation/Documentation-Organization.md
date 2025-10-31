# Compiler Documentation Organization Scheme

## Folder Structure

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
│   ├── DataStructures.md     # Core DS used throughout
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
│   ├── Overview.md                    # What is this language?
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
│   │   ├── Advanced-Algebra.md         # Sets, posets, sequent calc, etc
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
│   ├── HowTo/                         # Goal-oriented (some existing content)
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

## Folders

See individual README.md files in each folder for more details.
