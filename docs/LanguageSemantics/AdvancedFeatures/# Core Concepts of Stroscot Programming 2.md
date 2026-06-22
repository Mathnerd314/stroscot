# Core Concepts of Stroscot Programming Language

Stroscot is an ambitious systems programming language designed with several interconnected core concepts that aim to unify high-level expressiveness with low-level control. The language's philosophy centers on providing powerful abstractions while maintaining explicit control over performance and resource management.

## Aspect-Oriented Programming (AOP)

At the foundation of Stroscot lies **Aspect-Oriented Programming**, a paradigm that enables developers to separate cross-cutting concerns from business logic. An aspect combines a pointcut (a predicate specifying where code should apply) with advice (the code to execute at those points). This design pattern appears throughout Stroscot's architecture—for example, destructors are implemented as aspects that insert cleanup code before imperative operations.[1]

Join points, which are specific execution moments like method calls or field accesses, correspond in Stroscot to imperative values with their continuations. The weaving process (integrating aspects into the application) can occur at compile-time or runtime by modifying dispatch behavior. While AOP introduces a learning curve and makes debugging more complex, it provides significant benefits in modularity, code reusability, and maintenance when implemented correctly.[1]

## Assembly and Hardware Abstraction

Stroscot treats a high-level programming language as a **macro assembler with particularly complex macros**. Rather than hiding assembly entirely, Stroscot exposes low-level operations as intrinsic functions, allowing developers to write even the lowest-level code using Stroscot's syntax without switching to a separate assembly language.[1]

The language supports multiple instruction set architectures: x86-64 and ARM64 are primary targets, with RISC-V, 32-bit ARM, and C as secondary backends. This multi-architecture approach requires maintaining an instruction database tracking encoding, metadata, timing information, and semantics for each instruction variant.[1]

Operations abstract away register references—instead, they work with temporaries of fixed bitwidths. For example, x86-64 division, addition, and add-with-carry operations are exposed as intrinsic functions that take and return these temporaries. Memory access is handled through separate operations, with validation for canonical addresses, alignment checking, and physical memory presence.[1]

## Build System: Cot

Stroscot's build system, called **Cot**, organizes builds into tasks—the smallest unit of execution addressable by the build system. Each task reads and writes shared state before synchronizing. The system implements a resource-based scheduler where tasks consume resources (defaulting to CPU cores), with configurable priorities and support for parallel execution.[1]

Cot provides multiple key types for tracking changes: dirty bits for simple in-memory flags, hashed keys for intern values, and file-based keys with various modification detection strategies including modification time, content digests, and filesystem watchers. The system supports incremental builds and distributed builds across multiple machines via SSH.[1]

A significant feature is the cache system, which can record and replay task outputs in a reproducible manner, dramatically speeding up subsequent builds when few files change. The build system also supports remote execution on multiple platforms simultaneously.[1]

## Concurrency Model

Stroscot's concurrency design is grounded in the principle that **processes may have consistent semantics whether implemented on multicomputers, multiprocessors, or with interleaved execution**. The language distinguishes between several thread types: OS threads managed by the operating system, UMS (User Mode Scheduling) threads on Windows with more application control, and fibers (green threads) that provide lightweight context switching.[1]

At the lowest level, threads use hardware memory operations (read/write on shared memory and memory barrier instructions) combined with OS syscalls for synchronization. Higher-level abstractions include mutexes, condition variables, channels, MVars, and transactional memory. The language avoids imposing a single concurrency model; instead, it allows libraries to implement various abstractions.[1]

Stroscot explicitly addresses memory races using relaxed memory models matching target hardware—x86-TSO for x86 and multicopy atomicity for ARM. Rather than enforcing a cross-platform model with excessive fences, Stroscot targets the specific processor's memory model for better performance.[1]

## Function Dispatch and Pattern Matching

Stroscot's **clause-based dispatch system** uses pattern matching with extensive pattern types including wildcards, literal matches, list patterns with `...` notation, record patterns, guard conditions, and view patterns. A clause combines a pattern with a body that transforms matched terms.[1]

The language supports **curried positional arguments**, allowing partial application and higher-order function usage without lambda wrappers. Keyword arguments enable reordering and provide more readable, maintainable APIs. The system also features **default arguments** that enable API evolution without breaking compatibility—parameters can be added with defaults, later made mandatory, or removed with deprecation windows.[1]

**Implicit arguments** behave like dynamically-scoped parameters, enabling patterns similar to Haskell's implicit instance resolution. **Output arguments** provide a functional way to handle returning multiple values, modifying the caller's scope in a controlled manner.[1]

Dispatch clauses are prioritized using lexicographically-ordered tuples of (declared priority, specificity). Specificity is determined by an SMT solver—a clause is more specific if certain patterns are unsatisfiable together. Higher-priority clauses shadow lower ones, but developers can access shadowed implementations via `next-method`.[1]

## Resource Management: Destructors and Finalizers

Stroscot implements automatic resource management through **destructors** (which run immediately after an operation) and **finalizers** (which run lazily at some point after use). This design enforces the allocate-cleanup pattern. Destructors determine the "last use" of a resource through control flow analysis and ensure cleanup happens exactly once.[1]

The system abstracts manual management: AutoCloseFD demonstrates wrapping system resources (like file descriptors) with destructors to ensure files close automatically. This approach scales to malloc/free patterns and other resource types.[1]

## Error Handling

Exceptions in Stroscot do not immediately halt programs in pure computation—instead they produce **exception values** that propagate through pure code. Unlike LLVM's single "poison" value, Stroscot maintains hierarchical exception sets.[1]

Exception values can be stored in variables, arrays, and pattern-matched for recovery. When an exception reaches the top-level Task structure, the handler prints it and exits. Developers can redefine exception values or match generic exceptions with `isException`.[1]

**Assertions** serve as inline contracts expressing expectations about program state. They use the exception mechanism (`assert cond` equals `when cond (throw AssertionFailure)`) and can be combined with non-deterministic value generation using `assume` to express universal properties.[1]

## Dynamic Function Overriding

Stroscot provides **dynamic function overriding** through the `dynamicF` macro, which adds high-priority clauses that check a hash table of runtime overrides. The `:=` operator sets overrides at runtime, while `delete` removes them. This enables interactive systems where behaviors can be modified at runtime without recompilation, useful for applications like interactive fiction.[1]

The implementation uses whole-program analysis to optimize away dynamic checks when the override table cannot contain values.[1]

***

These interconnected concepts form a cohesive system where concerns are separated cleanly (via AOP), hardware-level control is accessible without context-switching (via intrinsics), builds are reproducible and distributable (via Cot), concurrency is correctly reasoned about (via hardware memory models), functions compose elegantly (via dispatch), resources are deterministically managed (via destructors), errors propagate safely (via exception values), and systems can adapt at runtime (via dynamic overrides). Together they enable systems programming with both performance and expressiveness.

[1](https://ppl-ai-file-upload.s3.amazonaws.com/web/direct-files/collection_93b0493f-e3e9-4ed9-b6ed-ef2a52f194d2/f18a4856-5b20-4838-8ed0-43721fee47f7/notes.md)