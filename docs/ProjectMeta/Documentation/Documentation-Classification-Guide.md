# Quick Reference: Where Does This Document Go?

## Decision Tree for Classifying Documentation

### Step 1: What is the primary purpose of this document?

#### A) Help someone understand how the compiler works internally
→ **Go to Step 2 (ARCHITECTURE)**

#### B) Help someone understand what the language is and how it behaves  
→ **Go to Step 3 (LANGUAGE SEMANTICS)**

#### C) Help someone use or learn the language/tool
→ **Go to Step 4 (USER DOCUMENTATION)**

#### D) Document project information, governance, or meta-knowledge
→ **Go to Step 5 (PROJECT META)**

---

## Step 2: ARCHITECTURE Classification
*"How does the compiler work?"*

### Question: What aspect of the compiler?

| Aspect | Location | Examples |
|--------|----------|----------|
| **Compiler phases and their interactions** | `Architecture/CompilerPhases/` | "Overview of lexing → parsing → type checking → code generation" |
| **Intermediate representation (IR)** | `Architecture/IR/` | "IR design decisions", "IR data structures", "Pass descriptions" |
| **Individual compiler components** | `Architecture/Components/` | "How the TypeChecker works", "Parser implementation strategy" |
| **Design decisions & trade-offs** | `Architecture/DecisionLog/` | Create an ADR (Architecture Decision Record) |
| **Performance & optimization** | `Architecture/Performance/` | "Profiling guide", "Optimization strategies" |
| **Build system & tools** | `Architecture/Tools/` | "Compiler tooling", "Debugging techniques" |
| **Data structures used throughout** | `Architecture/DataStructures.md` | "Core DS used in compilation" |

**Quick test:** If you're explaining *why a component exists* or *how it interacts with others*, it's architectural.

---

## Step 3: LANGUAGE SEMANTICS Classification
*"What is this language? How does it work?"*

### Question: Is this for...

| User Type | Location | Examples |
|-----------|----------|----------|
| **Language users learning concepts** | `LanguageSemantics/CoreConcepts/` | "Understanding the type system", "How evaluation works", "Module system" |
| **Advanced language users** | `LanguageSemantics/AdvancedFeatures/` | "Concurrency model", "Logic programming", "Macros", "Security model" |
| **Language reference spec** | `LanguageSemantics/References/` | "Complete type specification", "Built-in primitives", "Performance characteristics" |

**Quick test:** If you're explaining *what the language does* or *why a language feature exists*, it's language semantics. If you're explaining *how the compiler builds it*, it's architecture.

**Example distinction:**
- "The type system supports both static and dynamic typing through a union type mechanism" → Language Semantics
- "The TypeChecker uses two-pass inference to handle forward references" → Architecture

---

## Step 4: USER DOCUMENTATION Classification (Diataxis)
*"How do I learn and use this tool?"*

### Question: What does the user need?

| User Need | Diataxis Type | Location | Examples |
|-----------|---------------|----------|----------|
| **"How do I get started?"** | Entry point | `Documentation/GettingStarted/` | "Installation", "First program", "FAQ" |
| **"How do I learn this from scratch?"** | **Tutorial** | `Documentation/Tutorials/` | Step-by-step learning path; hands-on exercises; assumes no knowledge |
| **"How do I accomplish [specific task]?"** | **How-To** | `Documentation/HowTo/` | "Building a project", "Using the build system", "Handling transactions" |
| **"Where do I find [specific information]?"** | **Reference** | `Documentation/Reference/` | "Grammar", "Error codes", "Library APIs", "Glossary" |
| **"Why does this work this way?"** | **Explanation** | `Documentation/Explanations/` | "Design philosophy", "Why term rewriting", "Security model rationale" |

**Diataxis principles:**
- **Tutorials**: Learning-oriented, step-by-step, hands-on
- **How-To**: Goal-oriented, assumes intermediate knowledge
- **Reference**: Information-oriented, for lookup, factual
- **Explanations**: Understanding-oriented, conceptual, context-providing

**Quick test:** If you're teaching someone *how to use* the tool, use Diataxis. If you're explaining *what the tool is*, it might belong in Language Semantics instead.

---

## Step 5: PROJECT META Classification
*"Documentation about the project itself"*

### Question: What aspect of the project?

| Aspect | Location |
|--------|----------|
| **How to contribute code** | `ProjectMeta/Contributing.md` |
| **Community standards** | `ProjectMeta/Code-of-Conduct.md` |
| **Future direction** | `ProjectMeta/Roadmap.md` |
| **Community & organization** | `ProjectMeta/Community.md` |
| **Funding & support** | `ProjectMeta/Funding.md` |
| **Documentation strategy** | `ProjectMeta/Documentation-Strategy.md` |
| **Learning resources** | `ProjectMeta/Learning-Resources.md` |

---

## Common Ambiguities & How to Resolve Them

### Question: "My document is about memory. Where does it go?"

**Answer: Depends on the perspective:**

| If explaining... | Goes to... |
|------------------|-----------|
| "How memory works in [the language]" (user perspective) | `LanguageSemantics/CoreConcepts/Memory-Model.md` |
| "How we represent memory in the IR" (compiler internals) | `Architecture/DataStructures.md` or `Architecture/IR/` |
| "Memory layout in compiled code" (implementation) | `Architecture/Components/CodeGenerator.md` or `Architecture/Performance/` |

### Question: "My document is about types. Where does it go?"

**Answer: Depends on the perspective:**

| If explaining... | Goes to... |
|------------------|-----------|
| "What the type system is and how to use it" (user) | `LanguageSemantics/CoreConcepts/Types.md` |
| "How type checking works" (compiler implementation) | `Architecture/Components/TypeChecker.md` |
| "Type system specification" (reference) | `LanguageSemantics/References/Complete-Type-Spec.md` |
| "How to write type annotations" (user how-to) | `Documentation/HowTo/Working-With-Types.md` |

### Question: "My document is about evaluation strategy. Where does it go?"

**Answer:**
- "How evaluation strategy works in the language" → `LanguageSemantics/CoreConcepts/Evaluation-Strategy.md`
- "Why we chose term rewriting for evaluation" → `Architecture/DecisionLog/ADR-XXX.md`
- "How to reason about evaluation order" → `Documentation/Explanations/Evaluation-Model.md`

### Question: "My document is about term rewriting. Where does it go?"

**Answer: Likely Architecture or Language Semantics (not user docs)**
- "Term rewriting as a language feature" → `LanguageSemantics/CoreConcepts/` or `AdvancedFeatures/`
- "How we implement term rewriting in the compiler" → `Architecture/CompilerPhases/` or `IR/Passes/`
- "Reduction example (showing transformation)" → `Architecture/IR/Passes/Reduction-Example.md`

### Question: "My document explains IR transformations. Where does it go?"

**Answer: Architecture**
- `Architecture/IR/Passes/` for individual pass documentation
- `Architecture/IR/Transformations.md` for overview of how IR evolves through compilation

**Not** in user documentation unless teaching users about how to reason about IR (which is unlikely).

---

## Writing Your New Compiler Design Documents

### If writing: "How compiler components interact"
→ Goes in `Architecture/`  
- Create overview in `Architecture/CompilerPhases/Overview.md` or `Architecture/Components/`  
- Use diagrams showing data flow  
- If documenting a *decision* about which component should own responsibility, create an ADR

### If writing: "IR structure and transformations"
→ Goes in `Architecture/IR/`  
- `Architecture/IR/Overview.md` - What is the IR?  
- `Architecture/IR/Representation.md` - Data structures  
- `Architecture/IR/Passes/` - Individual transformation passes

### If writing: "How type checking works"
→ Likely both places:
- `LanguageSemantics/CoreConcepts/Types.md` - What the type system is (user perspective)
- `Architecture/Components/TypeChecker.md` - How type checking is implemented (developer perspective)

### If writing: "Performance and optimization strategy"
→ Goes in `Architecture/Performance/`  
- `Architecture/Performance/Optimization-Strategy.md`  
- `Architecture/Performance/Profiling-Guide.md`

### If writing: "Here's why we made this architectural choice"
→ Create an ADR in `Architecture/DecisionLog/`  
Use template:
```
# ADR-NNN: [Brief Title]

## Status
Accepted/Proposed/Deprecated

## Context
[What was the situation and why did we need to decide?]

## Decision
[What did we decide to do and why?]

## Consequences
[What positive and negative outcomes resulted?]

## Alternatives Considered
[What other options did we evaluate?]
```

---

## File Organization Checklist

When creating a new document, ask:

- [ ] Is this for *compiler developers* learning how the system works? → **Architecture/**
- [ ] Is this for *language users/designers* understanding the language? → **LanguageSemantics/**
- [ ] Is this for *end users* learning to use the tool? → **Documentation/** (apply Diataxis)
- [ ] Is this for *project contributors* or project governance? → **ProjectMeta/**
- [ ] Does this document a significant design decision? → **Architecture/DecisionLog/** (ADR format)
- [ ] Does this document multiple audiences? → Separate into multiple documents, one per audience

---

## Still Confused? Ask These Questions

1. **"Who am I writing this for?"**
   - If: Internal compiler developers → Architecture
   - If: Language users → LanguageSemantics or Documentation
   - If: Project community → ProjectMeta

2. **"Am I explaining HOW something works internally?"**
   - If yes → Architecture

3. **"Am I explaining WHAT something is or WHY it exists?"**
   - If it's a language feature → LanguageSemantics
   - If it's a design decision → Architecture/DecisionLog (ADR)
   - If it's why users should care → Documentation/Explanations

4. **"Could someone use this information without understanding the implementation?"**
   - If yes → Documentation (user docs) or LanguageSemantics
   - If no → Architecture

5. **"Is this Diataxis-compatible?"**
   - Is it a step-by-step learning guide? → Tutorials
   - Is it "how to do X"? → How-To
   - Is it reference information? → Reference
   - Is it conceptual/understanding? → Explanations
   - If none of the above, it probably belongs in Architecture or LanguageSemantics, not user docs
