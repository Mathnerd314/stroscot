# Core Concepts of Stroscot Programming Language

Stroscot is an ambitious systems programming language that synthesizes multiple sophisticated programming paradigms and design principles. Rather than committing to a single approach, Stroscot deliberately integrates functional, imperative, logic, and object-oriented programming styles into a cohesive framework. The language aims to balance multiple competing concerns: expressiveness with performance, high-level abstractions with low-level control, and automatic resource management with deterministic cleanup semantics.

## Aspect-Oriented Programming (AOP)

**Aspect-Oriented Programming** forms a foundational pillar of Stroscot's design philosophy. AOP allows separating cross-cutting concerns—aspects that affect multiple modules or components—from the primary business logic. When properly implemented, AOP increases codebase modularity, maintainability, reusability, readability, consistency, and testability. Rather than having logging, security, profiling, or cleanup logic scattered throughout the codebase, these concerns can be centralized and managed separately. These aspects can be dynamically enabled or disabled at runtime without altering the core application logic, providing significant flexibility in how programs behave.

The challenge with AOP lies in its learning curve and increased complexity during debugging. However, the benefits—particularly in isolating business logic and managing concerns that would otherwise spread across the entire codebase—make it a worthwhile investment for complex systems.

## Assembly and Hardware Abstraction

Stroscot takes a deliberate stance on assembly language integration: it treats a high-level programming language as **a macro assembler with particularly complex macros**. Rather than forcing a false dichotomy between "high-level" and "low-level" code, Stroscot allows developers to write even the lowest-level code using Stroscot syntax without context-switching to a separate assembly language. Assembly intrinsics expose hardware operations directly—not as an afterthought but as a first-class facility.

This design philosophy recognizes that pursuing 100% automatic code generation is unrealistic given current compiler technology. C itself has runtime overhead: generated instruction sequences may be suboptimal, malloc/free may be slower than custom allocators, data layouts may waste space, calling conventions may not be optimal, and standard libraries may be slower than hand-written code. By exposing assembly facilities as intrinsics within the language, developers can solve performance and functionality issues—such as SIMD optimization—without abandoning the language entirely.

The phrase "direct assembly" is somewhat misleading: Stroscot mainly operates at the level of intrinsics rather than raw machine instructions. Register allocation, which is not particularly difficult to optimize, is handled automatically. This approach provides a robust fallback strategy for addressing problems that higher-level facilities cannot solve, while maintaining the language's coherence and usability for typical development.

## Concurrency and Parallelism

The rise of multicore CPUs demands that languages provide **first-class support for concurrency and parallelism**. Stroscot aims to get this right, as concurrent and multi-threaded programming have developed an unfortunate reputation for difficulty.

The language distinguishes between multiple thread types:

- **OS Threads**: Managed by the operating system with standard semantics.
- **UMS Threads** (User Mode Scheduling): Windows-specific threads providing more application control than OS threads.
- **Fibers** (Green Threads): Lightweight context-switching threads managed by the language runtime.

At the lowest level, threads use hardware memory operations (read/write on shared memory and memory barrier instructions) combined with OS syscalls for synchronization. Higher-level abstractions include mutexes, condition variables, channels, MVars, and **software transactional memory (STM)** as a built-in language feature. Unlike some languages that impose a single concurrency model, Stroscot allows libraries to implement various abstractions suited to different use cases.

**Memory model semantics** are architecture-specific rather than universally homogenized. Stroscot targets x86-TSO for x86 and multicopy atomicity for ARM, matching each processor's actual behavior rather than imposing a cross-platform model with excessive memory fences. This approach yields better performance while maintaining correctness.

A key principle underlying Stroscot's concurrency design is that **processes may have consistent semantics whether implemented on multicomputers, multiprocessors, or with interleaved execution**. This flexibility enables the same concurrent code to work across different deployment scenarios.

## Term Rewriting Formalism

Stroscot is fundamentally built upon **term rewriting**, a mathematical framework where computation occurs through iterative transformation of terms. The language supports:

- **Higher-order terms**: Terms can contain other terms, enabling powerful abstraction.
- **Pattern matching**: Powerful destructuring of terms with wildcards, literals, lists, records, guards, and view patterns.
- **Predicate dispatch**: Conditional rewriting based on guards and properties of matched patterns.
- **Logic programming**: Goals expressed declaratively, solved through automated reasoning.

In general, rules are not ordered—they overlap and run in parallel. If multiple rules match, all cases are tried nondeterministically. A requirement for deterministic semantics is that for each possibility, the program either throws an exception or produces identical output (confluence).

If none of the rules match, the term does not reduce and becomes a **normal form**. To raise an exception for unmatched cases, a catch-all clause must be defined.

**Local functions** are applied identically to global ones: argument patterns are matched nondeterministically against actual function arguments, and optimal reduction is performed.

## Dispatch and Function Overloading

Stroscot's **clause-based dispatch system** uses sophisticated pattern matching with extensive pattern types: wildcards, literal matches, list patterns with `...` notation, record patterns, guard conditions, and view patterns. A clause combines a pattern with a body that transforms matched terms.

The language supports several advanced parameter mechanisms:

- **Curried positional arguments**: Enable partial application and higher-order function usage without explicit lambda wrappers.

- **Keyword arguments**: Allow reordering and provide more readable, maintainable APIs.

- **Default arguments**: Enable API evolution without breaking compatibility—parameters can be added with defaults, later made mandatory, or removed with deprecation windows.

- **Implicit arguments**: Behave like dynamically-scoped parameters, enabling patterns similar to Haskell's implicit instance resolution.

- **Output arguments**: Provide a functional way to return multiple values by modifying the caller's scope in a controlled manner.

- **Variadic arguments**: Support functions accepting varying numbers of arguments.

Dispatch clauses are prioritized using lexicographically-ordered tuples of (declared priority, specificity). **Specificity** is determined by an SMT solver: a clause is more specific if certain pattern combinations are unsatisfiable. Higher-priority clauses shadow lower ones, but developers can access shadowed implementations via `next-method`. This mechanism supports method combination and allows overriding while maintaining access to original implementations.

## Evaluation Strategy

Stroscot employs **optimal reduction** as its core evaluation strategy. Optimal reduction offers superior reduction and expressiveness properties compared to lazy or strict evaluation. Stroscot special-cases optimization for C-like programs to achieve expected performance while retaining the expressiveness benefits. Stroscot uses Hudak's aggregate update optimizations, which implement an advanced form of copy-on-write semantics for data structures, enabling efficient in-place updates.

## Resource Management: Destructors and Finalizers

**Automatic resource management** is essential for safe, correct systems. Manual memory management is slow, tedious, and error-prone. Stroscot's solution involves **destructors**. A destructor specifies a cleanup operation. Control flow analysis determines the "last use" of a resource and ensures cleanup happens exactly once and at precisely the right moment.

This design enforces the ubiquitous allocate-cleanup pattern. For example, `AutoCloseFD` wraps system resources like file descriptors with destructors to ensure files close automatically. The same approach scales to malloc/free patterns and other resource types, eliminating entire categories of resource leaks.

## Exception Handling

Exceptions in Stroscot do not immediately halt computation in pure code. Instead, they produce **exception values** that propagate through pure code until reaching imperative operations. Unlike LLVM's single "poison" value, Stroscot maintains **hierarchical exception sets**, allowing fine-grained exception discrimination.

Exception values can be stored in variables, arrays, and pattern-matched for recovery. When an exception reaches the top-level Task structure, a handler prints it and exits. Developers can redefine exception values or match generic exceptions using `isException`.

**Assertions** serve as inline contracts expressing expectations about program state. They use the exception mechanism (`assert cond` equals `when cond (throw AssertionFailure)`) and can combine with non-deterministic value generation using `assume` to express universal properties—useful for verification and testing.

## Dynamic Function Overriding

Stroscot provides **dynamic function overriding** through the `dynamicF` macro, which adds high-priority clauses that check a runtime hash table of overrides. The `:=` operator sets overrides at runtime; `delete` removes them. This enables interactive systems where behaviors can be modified without recompilation—useful for applications like interactive fiction or live-coding environments.

The implementation uses whole-program analysis to optimize away dynamic checks when the override table cannot contain values, recovering performance when dynamism is unnecessary.

## Operational Primitives

**Operational primitives** are the stateful operations forming the building blocks of imperative programs: memory operations (read, write), foreign function calls, compiler intrinsics, and OS system calls. These are distinct from elements of the runtime like lambda reduction and term rewriting.

Every expression in Stroscot has an operational interpretation. For instance, the operational interpretation of a value is returning that value; addition on machine integers involves storing values to memory/registers, executing the `add` instruction, handling traps, and packaging results. Generally, programs consist of assembly sequences interleaved with higher-level "glue" code. Optimization converts and reduces glue code to assembly; each switch into glue code corresponds to a jump back into the interpreter with associated overhead.

Stroscot follows Steelman 1G: there is a facility for defining program portions dependent on object machine configuration and conditionally compiling based on actual configuration. The language exposes the full machine configuration at runtime, allowing code to use this information in conditions and control structures.

## Functional Logic Programming

Stroscot integrates **functional logic programming**, drawing inspiration from languages like Curry, Verse, Flix, and Oz. This paradigm combines functional programming's benefits (immutability, composability, mathematical reasoning) with logic programming's declarative power (goal-driven solving, nondeterminism). Users can express problems declaratively and allow automated reasoning to find solutions.

## Logic and Sequent Calculus Foundation

**The Curry-Howard correspondence** maps logic to programming: a logical system specifies well-formed proofs of propositions, which correspond to well-formed programs in a type system. By proving the logic sound and complete, one obtains an expressive programming language.

Stroscot uses **linear logic in sequent calculus form** as a key component of its core language semantics. This provides a rigorous mathematical foundation for the language. Additionally, Stroscot employs a novel foundational system for set theory based on sequent calculus, where "well-formed formulas" are defined as properties of derivability rather than purely syntactic constructs. This approach eliminates set-theoretic paradoxes (such as Russell's paradox) by rendering the problematic propositions as "unwell-formed" (non-derivable as identities).

This system introduces two forms of membership: $$\in_f$$ for propositions and $$\in_s$$ for sets, with a reductive definition of the latter. It grounds all set-theoretic discourse in base propositional logic while recovering key features of stratified comprehension reminiscent of Quine's New Foundations (NF).

## Stateful Programming and Denotational Semantics

Despite supporting imperative operations, Stroscot embraces **denotational semantics**: a program is conceptually a mathematical function where any given input always produces the same output, with no implicit external state mutations. Practically, this means programs work like calculators—input an expression, receive an answer.

Stroscot has settled on **continuations** (or "tasks") as its denotational semantics model. Continuations are highly general, expressing many different control flow mechanisms without the complexity of state-passing monads (as in Haskell) or uniqueness types (as in Clean). This foundation enables reasoning about program behavior—asking whether two programs are equivalent—while supporting practical imperative programming.

## Types and Sets

The relationship between types and sets in Stroscot is nuanced. Stroscot has sets and assertions about whether values belong to sets. Are sets types? Is Stroscot typed? Statically or dynamically typed? Strongly or weakly typed? These terms are ambiguous and controversial.

The pragmatic answer is: **Stroscot is both**. Developers can build prototypes and simple scripts quickly without annotations (dynamic typing). They can also add type annotations for additional safety (static typing). Stroscot notably lacks explicit type inference—the language is straightforward about what is and isn't checked.

More precisely, Stroscot allows **specifying properties about execution**, which the compiler then attempts to prove or at least fuzz through verification. The most common form is membership in a set of values, enabling many useful optimizations.

## Values and Expressions

In Stroscot, **values** are defined as expressions in normal form (strongly reduced), meaning they evaluate to themselves. Weak Head Normal Form (WHNF) is insufficient: `[1,undefined]` reduces to `undefined`, so it is not a value. While traditionally functions are only defined on values, lazy evaluation enables functions to produce useful behavior for non-values.

Values are **immutable** and have notions of equality, hashing, literal syntax, and deconstruction. In memory management terms, values can be copied freely and discarded when no longer needed. For convenience, "value" describes the equivalence class of expressions that reduce to a value, described by their concise syntactic form.

An **expression** represents an element within the universe of discourse. Stroscot aims to maximize functionality by maintaining an unfettered universe where all applications of words follow their common conditions and meanings. Expressions possess canonical serialized representations in both text (UTF-8) and specialized in-memory forms.

## Macros and Metaprogramming

**Macros** implement syntactic extensions to the language, embodying the essence of Lisp: code can be used as data, and data as code. While Lisp's S-expression syntax makes this obvious, Stroscot employs more complex syntax with corresponding mental overhead. Macros enable:

- **Code generation**: Automatically generate code from templates or other input.
- **Metaprogramming**: Write code that manipulates and generates other code at runtime.

Though McCarthy originally intended Lisp to use M-expression (mathematical expression) syntax with S-expressions as mere convenience, Stroscot deliberately trades the syntactic simplicity of S-expressions for more expressive and readable syntax, leveraging compiler libraries to define syntax separately.

## Modules and Encapsulation

**Modules** allow developers to add new language features through libraries. From the programming perspective, modules are records containing bindings, but they feature top-level scope and leverage declaration syntax while defining namespaces for symbols.

Steelman requirements 3-5A through 3-5C specify encapsulation needs:

- Encapsulations may contain any definable declarations, with multiple explicit instantiations possible.
- Encapsulations inhibit external access to implementation properties; even automatically defined operations like type conversions and equality can be hidden.
- Externally accessible definitions may be renamed before use outside the encapsulation.
- Variables declared within encapsulations (but outside functions/processes) remain allocated and retain values throughout instantiation scope.

## Object-Oriented Programming

The term "object" is fraught with definitional ambiguity. The C2 wiki notes that **nobody agrees on what OO is**, with numerous competing definitions. Rather than enforcing a single OO model, Stroscot aims to support all programming paradigms, including those associated with OO.

However, languages attempting partial OO support have limitations. Julia, for instance, supports some OO features but lacks traditional inheritance, making it difficult to build deep hierarchies and extension points. Stroscot's multi-paradigm approach sidesteps these restrictions by supporting the underlying capabilities without forcing a particular organizational pattern.

## Memory Models

Stroscot supports two complementary memory models:

- **Concrete model**: Models memory as an integer-indexed array of $$2^{32}$$ or $$2^{64}$$ words, corresponding to pointers.
  
- **Symbolic model**: Models memory as an associative array from symbols (potentially infinite) to "cells" (arrays of words of various lengths), corresponding to references.

The language supports combinations like the **quasi-concrete model**, which uses a data type starting as a reference, implementing arithmetic operations symbolically, then switching to a pointer when an integer address is requested. Finalizers or destructors manage the lifetime of allocated memory.

**Automatic memory management** is essential. Manual management is slow, tedious, and error-prone. Automatic management must be flexible enough for all tasks manual management addresses, which Stroscot achieves through its destructor/finalizer system.

## Syntax

Stroscot's syntax should be **clear, readable, friendly, and consistent**—easy to learn and accessible to new programmers and those with disabilities. The syntax should be **flexible, extensible, and customizable**, allowing developers to tailor language aspects to specific needs.

While S-expressions are sufficient for the core language, syntax is "one of the most boring aspects of building a language"—with limited innovation possible beyond applying large language models to parsing. Stroscot defines its syntax as a **compiler library**, decoupling syntax from core semantics and enabling flexibility.

## Transactions and Synchronization

**Software transactional memory (STM)** is a built-in language feature providing an alternative to low-level assembly-based synchronization primitives. With transactions, developers can write various common synchronization constructs in a higher-level, more maintainable manner.

## Verification and Static Analysis

Stroscot aims to be practical while providing **strong guarantees about program behavior**, such as ensuring array accesses remain in bounds. In most cases, these guarantees are ensured statically through the **verification system**.

Steelman requirements emphasize this goal:

- The language shall be designed to maximize automatic detection of programming errors.
- No language restrictions shall exist that are not enforceable by translators.

Static verification and symbolic execution extend unit testing with far greater power. Building standardized verification APIs and UX into the language enables robust programs to emerge naturally. Practical scalability is a concern—verification is time and memory intensive—but Stroscot employs advanced techniques to make it practical. The seL4 microkernel (8700 lines of C) has been successfully statically modeled and verified, demonstrating feasibility for real-world systems.

## Posets

**Posets** (partially ordered sets) are used in specific places throughout Stroscot: operator precedence and method combination. The language provides specific support for defining and working with posets, enabling precise specification of priority and ordering relationships.

## Security

Stroscot aims to have **built-in security features**, providing security functionality in the standard library (encryption algorithms, communication protocols) while designing the library and language to make secure code easy and insecure code difficult. This philosophy guides architectural decisions throughout the system.
