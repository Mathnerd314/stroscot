# Aspects

Aspect-Oriented Programming (AOP) is a programming paradigm that allows separating concerns (aspects) that cut across multiple modules or components. Used properly, AOP increases codebase modularity, maintainability, reusability, readability, consistency, and testability. In particular, developers may isolate business logic, and separately manage cross-cutting aspects which would otherwise be spread across different parts of the code. These aspects can even be dynamically managed, providing flexibility to enable or disable certain behaviors at runtime without altering the core application logic.

# Assembly

8D. The language shall not require the presence of an operating system. [Note that on many machines it will be necessary to provide run-time procedures to implement some features of the language.]

8E. There shall be a few low level operations to interrogate and control physical resources (e.g., memory or processors) that are managed (e.g., allocated or scheduled) by built-in features of the language.

11E. There shall be a machine independent interface to other programming languages including assembly languages. Any program element that is referenced in both the source language program and foreign code must be identified in the interface. The source language of the foreign code must also be identified.

Creating a high-level programming language with absolutely no runtime overhead is a challenging task. For example C has runtime overhead: the generated instruction sequences may not be optimal, memory management with malloc/free may be slower than a custom allocator, data type layouts may not be optimal, function calls follow the C calling convention which is not necessarily the fastest, and the standard library may be slower than hand-written routines.

So writing code in assembly can be advantageous. It is best to think of a high-level programming language as a macro assembler with particularly complex macros - this begs the question of why most languages make it so hard to use inline assembly. Even in a JIT-style execution model, some amount of code is compiled to assembly. The difference is there are also jumps into the interpreter, which sort of "glues together" regions of compiled code.

On the other hand, typical assembly syntaxes deviate from the rules and structures that people expect. By exposing assembly facilities as intrinsics, developers can solve performance and functionality issues without the need to learn a separate set of "low-level" assembly language conventions. Writing even the lowest level code in Stroscot style allows developers to "open up the hood" without switching to another language. Admittedly, using assembly to solve a performance problem is sometimes like using a sledgehammer to crack open an egg, but in situations such as SIMD optimization, the research is not yet at a level that allows 100% perfect code generation. Assembly intrinsics provide a viable solution to addressing this issue. Furthermore, this solution is robust, in that almost any issue can be solved by writing the desired program in assembly. Not every developer will need to use assembly to satisfy their requirements, and indeed it is generally better to use higher-level facilities, but as a fallback strategy, allowing direct assembly provides a level of control not otherwise possible. I say "direct" but in fact register allocation is not too hard to do optimally so Stroscot assembly mainly lives at the level of intrinsics rather than machine code instructions.

## Architectures

The first step in dealing with assembly is to decide which instruction set architectures to support. I couldn't find a list of processor architectures by popularity, but from [this quora answer](https://www.quora.com/What-kind-of-instruction-set-architecture-do-modern-processors-use) and checking it by googling numbers of units sold for other random ISAs, the two primary architectures are x86-64 AMD64 (desktops) and ARM64 (mobile devices).

Others to consider as well:

- C: compilation to a self-contained C program makes porting much easier, and obviates the need for many of these architectures. Verdict: on the roadmap. Note though that this is only compiling to a subset of C - not every C program can be produced. For example, jumps (tail calls) are hard to encode in C - you either do one massive function with goto's, or [trampolining](https://en.wikipedia.org/wiki/Tail_call#Through_trampolining), or non-portable TCO conventions like Manticore's JWA convention.
- WASM: it still doesn't support [tail calls](https://github.com/WebAssembly/proposals/issues/17). Given the lack of progress it seems like a low priority. Verdict: Contributor.
- LLVM: The bitcode format may be worth targeting at some point. Per blog posts the API is much more unstable than the IR, and generating the IR in memory and parsing it is about as fast as using the API. Verdict: Contributor.
- RISC-V: There are \$100-ish dev boards listed at <https://riscv.org/exchange/boards/>. No non-dev systems yet. It's a relatively simple ISA, similar to ARM. Verdict: Contributor
- 32-bit ARM: Old phones, the Raspberry Pi Zero. The XML database is similar. Verdict: Contributor.
- 32-bit x86: Old desktop PCs. From a time/effort perspective it seems cheaper to buy a new computer instead of writing support for these. Verdict: C backend or contributor.
- POWER: [Raptor](https://secure.raptorcs.com/content/base/products.html) sells \$5K-ish systems. Much more expensive and niche than RISC-V. Verdict: C backend.
- MIPS: the company that develops it went bankrupt and is now doing RISC-V. There are consumer systems available in China (Loongson), but the rumor is that they too are moving to RISC-V or else to their own architecture LoongArch. Verdict: C backend.
- z/Architecture: really expensive, weird OS. Verdict: C backend.
- SPARC: It's end-of-life but I guess you can still buy servers second-hand. Verdict: C backend.

From a design perspective supporting 2 architectures is not much different from supporting 10, it's just a larger set of cases, but 10 is 5x the work of 2. ARM support will be tested through QEMU, x86 natively. There are also CI services that could work (Drone). Code bloat is an issue but keeping each ISA in its own folder should avoid drift.

In addition to the basic ISAs, there are also extensions and [microarchitectures](https://en.wikipedia.org/wiki/Microarchitecture) to consider. For example ARM64 is divided into v8-A, v9-A, and others. [PassMark](https://www.cpubenchmark.net/share30.html) has a list of CPU shares, it's probably wildly skewed to gaming but it's better than nothing. The data on CPU cycles, ports, etc. is rather detailed and has to be generated by running benchmarking programs, so it will probably depend on user submissions; for now I'll use my own CPU (AMD A6-3650 APU).

## Operating systems

In planned order:

1. Linux for AMD64, because it's what I'm typing on now
2. Android for ARM, because it's my phone and it's easy to hook up
3. Windows for AMD64, which I can emulate with WINE and test fairly easily

We'll exclude Apple for now because their OS documentation sucks, they charge \$100/year for a "developer license", and their anti-competitive practices mean that they would probably find some way to shut Stroscot down once Stroscot starts being a serious competitor with Swift. Of course there is nothing stopping someone else from jumping through all the hoops needed to placate Apple and making a port.

## Instruction database

x86 has a lot of instructions - somewhere around 1000 unique mnemonics, and many variants of those instructions. ARM too has at least a thousand instruction variants. With so many, it is clear that a structured database of instruction information is needed.

### Goals

- 99% Completeness - it is not too hard to cover all of the instructions mentioned in official sources, and all of the "undocumented" instructions discovered so far by tools such as sandsifter and haruspex. But outside of this, it is impossible to be complete - there are simply too many bit patterns. sandsifter/haruspex take days to run and do not even explore the full instruction space, making assumptions about the format of instructions. But these tools have confirmed that there are many undocumented instructions. Therefore, it must be assumed that the database is incomplete - more instructions may be discovered in the future. We should therefore allow raw bit patterns not present in the database, `instr('f0 0f')` or similar, throughout the pipeline.
- Accuracy - Generally, all data should either come directly from official sources or measurement, and be automatically generated. This allows adding new instructions, processors, and microarchitectures as quickly as they become available. Furthermore it is easy to verify the information by checking it against the sources or rerunning the tool.
- Consistency - the database should have a consistent format, structure, and representation, so that it can be easily used in the compiler. This format should be documented for accessibility.

### Definition of an instruction

An instruction is a finite sequence of binary data (generally some number of bytes). The general idea is that instructions are a syntactic unit above bits, like words in a character string. Except unlike words, there's no instruction separator character; instructions are all run together like `afewinstructions`. Segmenting ARM instructions is simple because they are all 32 or 64 bits. For x86, the length varies from 1 to 15 bytes and is affected by almost all parts of the instruction. [sandsifter](https://github.com/xoreaxeaxeax/sandsifter) can determine the length of the first instruction in some bytes by finding an index for which `seq|uence` does not trigger a page fault, but `se|quence` does (where `|` is a page boundary). [haruspex](https://blog.can.ac/2021/03/22/speculating-x86-64-isa-with-one-weird-trick/) is even more tricky and examines the microcode speculation buffer performance counters to see how many nops after the byte sequence were speculated. With these tools we can segment arbitrary data into x86 instructions, assuming access to the processor.

# Concurrency

As Go says, the rise of multicore CPUs means that a language should provide first-class support for concurrency and parallelism. But concurrency and multi-threaded programming have over time developed a reputation for difficulty. So let's get it right.

# Using destructors

General principle: Use a destructor or finalizer whenever you have an allocate-clean up pair. Use a destructor if you want the guarantee that cleanup is called immediately after an operation, otherwise use a finalizer.

# Dispatch

clauses, patterns, positional arguments, keyword arguments, default arguments, implicit arguments, output arguments, variadic arguments, priorities, overloading, "lub" semantics, next-method, method combination

Stroscot is based on a term rewriting formalism and supports higher order terms, pattern matching, and predicate dispatch (conditional rewriting). In general rules are not ordered - they overlap and run in parallel. If multiple rules match, all cases are tried nondeterministically and run. It is required that for each possibility the program either throws an exception or produces the same output.

Local functions are applied in the same way as global ones, i.e., the argument patterns of each rule are matched against the actual function arguments nondeterministically and optimal reduction is performed.

If none of the rules match then the term does not reduce (it becomes a normal form) - to raise an exception a catch-all clause must be defined.

# Dynamically override functions

Here we are implementing a model where a function can be static (entirely determined at compile-time) or dynamic (possibly determined by run-time code). Dynamic functions can be changed at run time and can call any other functions, static or dynamic.

# Evaluation strategy

This page summarizes the arguments for different types of evaluation strategies:

- pure vs impure - whether an expression has a denotational value, and how complex this denotation is
- strict vs non-strict - whether to allow a function to return a result without fully evaluating its arguments
- eager vs lazy - whether to make arguments non-strict by default
- call-by-need vs optimal - if arguments are non-strict, whether to evaluate once per closure or do deeper sharing

The quick summary is that optimal reduction is optimal, hence has better reduction and expressiveness properties than lazy or strict, but it is a complex strategy and in some cases there may be significant space overhead compared to strict due to graph reduction overhead, and there are also cases where the graph reduction overhead exceeds the runtime of the program, so programs can be slower with optimal reduction. To address this Stroscot will special-case optimization for C-like programs to give the expected performance.

"The next Haskell will be strict". - not necessarily. laziness may yet have a role as well.

Traditionally a function is only defined on values, but lazy evaluation allows functions to produce useful behavior for non-values as well.

# Exceptions

10A. There shall be an exception handling mechanism for responding to unplanned error situations detected in declarations and statements during execution.

Exceptions allow us to focus on the normal (non-exceptional) case when performing operations that might fail. The downside is that it is more tricky to reason about how the failure cases are handled, because the handling code may be many levels of function calls removed from the operation.

# Expressions

An expression represents an element within the [universe of discourse](https://en.wikipedia.org/wiki/Domain_of_discourse). As Stroscot aims to maximize functionality, the goal is to have an unfettered universe of discourse in which all applications of words are understood according to their common conditions and meanings. Expressions are immutable (as [Rich Hickey says](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg)). In terms of memory management, expressions are just data - they can be copied freely, and discarded once they will no longer be used. Expressions thus have a canonical serialized representation. For convenience we assume two serialization forms: a text (UTF-8) representation, for human interaction, and also a distinct in-memory representation that is perhaps more compact.

# Functional logic

Curry, Verse, Flix, Oz, ... all functional logic languages.

# Logic

The Curry-Howard correspondence maps logic to programming. A logical system specifies well-formed proofs of propositions. These correspond to well-formed programs in a simple type system. By proving the logic sound and complete, we get an expressive programming language. Hence we use a linear logic sequent calculus as a key component of the core language semantics.

# Logic programming

logic programming: express goals in a declarative form and solve problems using automated reasoning.

# Macros

- code generation: automatically generate code based on templates or other input.
- metaprogramming: write code that can manipulate and generate other code at runtime.

Macros are a facility that allows defining syntactic extensions to a programming language. Macros implement the essence of Lisp: code can be used as data, and data can be used as code. Although in Lisp the transformation of code into data (specifically, an AST tree) is obvious as the S-expression syntax is just nested lists, in Stroscot we choose to have more complex syntax and accompanying mental overhead. It should be noted that even McCarthy the inventor of Lisp intended to use an M-expression (mathematical expression) syntax and S-expressions were only a matter of convenience.

# Memory

The language should have automatic memory management. Manual memory management is slow, tedious, and error prone. Automatic memory management is better in all respects, but the implementation has to be flexible enough to be usable for all the things manual memory management is.

There are two main models of memory. The concrete model models memory as a an integer-indexed array of 2^32 or 2^64 words. The symbolic model models memory as an associative array from symbols (potentially infinite in number) to "cells", arrays of words of various lengths. In Stroscot these models correspond to pointers and references respectively. Combinations of these can be made, for example the "quasi-concrete model" which uses a data type that starts out containing a reference, implements various arithmetic operations symbolically, but switches to a pointer once an integer address is requested. {cite}`kangFormalMemoryModel2015` Finalizers or destructors can be used to manage the lifetime of allocated memory.

# Modules

Developers can add new language features and functionality through libraries and modules.

3-5A. It shall be possible to encapsulate definitions. An encapsulation may contain declarations of anything (including the data elements and operations comprising a type) that is definable in programs. The language shall permit multiple explicit instantiations of an encapsulation.

3-5B. An encapsulation may be used to inhibit external access to implementation properties of the definition. In particular, it shall be possible to prevent external reference to any declaration within the encapsulation including automatically defined operations such as type conversions and equality. Definitions that are made within an encapsulation and are externally accessible may be renamed before use outside the encapsulation.

3-5C. Variables declared within an encapsulation, but not within a function, procedure, or process of the encapsulation, shall remain allocated and retain their values throughout the scope in which the encapsulation is instantiated.

Modules from the programming perspective are records; they contain a list of bindings. But they have a top-level scope and take advantage of declaration syntax. They also define namespaces for symbols.

# Objects

Many people like to use the word "object", as in "object-oriented programming". Stroscot aims to support all the paradigms, so being able to claim that Stroscot is OOP would be a great feature. But our analysis is stymied before it begins: per the C2 wiki, [nobody agrees on what OO is](https://wiki.c2.com/?NobodyAgreesOnWhatOoIs) and there are [many definitions for OO](https://wiki.c2.com/?DefinitionsForOo). For that reason the word "object" is not used in the rest of the documentation. But here we try to figure out what OO is and how to make Stroscot support OO.

For example, Julia sort of is OO but users have [said](https://discourse.julialang.org/t/workaround-for-traditional-inheritance-features-in-object-oriented-languages/1195/27) "Julia doesn’t do a good job here" and "[Inheritance] can help you build deep hierarchies that present data as nice, rolled up extension points for consumers of your library. Currently I don’t see any way to do something similar in Julia without a bunch of trickery/hackery/copying/shims."

# Operational primitives

8A. There shall be a few low level input-output operations that send and receive control information to and from physical channels and devices. The low level operations shall be chosen to insure that all user level input-output operations can be defined within the language.

As used in Stroscot, operational primitives refer to the stateful operations that form the building blocks of imperative programs. Examples include memory operations (read, write), foreign function calls, compiler intrinsics, and OS system calls. It would also be possible to call them "primitive operations", but this term is less precise and could be read as including elements of the runtime such as lambda reduction and term rewriting.

There is an operational interpretation of every expression in Stroscot. For example, the operational interpretation of a value is returning that value. The operational interpretation of addition on two machine integers consists of storing the integers to memory or fixed registers, executing the `add` assembly instruction, appropriately handling any error conditions or traps, packaging up the result as a value, and returning it. And so on. Generally, a program may be viewed as assembly instruction sequences interleaved together with higher-level "glue" code. During optimization, one goal is to convert and reduce as much of this "glue" code as possible into assembly. Each switch into "glue" code corresponds to a jump back into the interpreter, with associated overhead.

Steelman 1G says "There shall be a facility for defining those portions of programs that are dependent on the object machine configuration and for conditionally compiling programs depending on the actual configuration." Stroscot follows this fully, exposing the full machine configuration at runtime and allowing code to use this information in conditions or other control structures.

11C. To aid conditional compilation, it shall be possible to interrogate properties that are known during translation including characteristics of the object configuration, of function and procedure calling environments, and of actual parameters. For example, it shall be possible to determine whether the caller has suppressed a given exception, the callers optimization criteria, whether an actual parameter is a translation time expression, the type of actual generic parameters, and the values of constraints characterizing the subtype of actual parameters.

11D. The object system configuration must be explicitly specified in each separately translated unit. Such specifications must include the object machine model, the operating system if present, peripheral equipment, and the device configuration, and may include special hardware options and memory size. The translator will use such specifications when generating object code. [Note that programs that depend on the specific characteristics of the object machine, may be made more portable by enclosing those portions in branches of conditionals on the object machine configuration.]

# Posets

Posets are used in a few places in Stroscot (operator precedence, method combination). There is specific support for defining and working with posets.

# Resource management

Resources are things that can be acquired and released, and are available in limited quantities due to OS or physical limitations. Examples include memory allocations, file handles, internet sockets, mutexes/locks, process table entries, and process identifiers (PIDs). A resource leak happens if the program does not promptly release a resource it has acquired after the program is finished with the resource. A resource management technique prevents resource leaks by releasing resources promptly.

It is possible to manage resources by hand, but it is quite error-prone and tedious. Automatic resource management aims to provide a concise and safe replacement. Stroscot's solution is called "destructors" or "finalizers" or something like that.

# Security

Stroscot aims to have built-in security features. This means providing security functionality in the standard library, such as encryption algorithms and communication protocols, but also designing the library and the language so that it is easy to write secure code and hard to write insecure code.


# A Sequent Calculus Foundation for Set Theory: Eliminating Paradoxes by Derivability

This proposes a novel foundational system for set theory that redefines the notion of a "well-formed formula" not as a purely syntactic construct, but as a property of derivability within a sequent calculus. Leveraging the robust properties of cut-elimination and the finite nature of derivations in systems like Gentzen's LK, this approach intrinsically eliminates set-theoretic paradoxes, such as Russell's paradox, by rendering the propositions that characterize them as "unwell-formed" (i.e., not derivable as identities). The system introduces two forms of membership ($\in_f$ for propositions and $\in_s$ for sets) with a reductive definition of the latter, grounding all set-theoretic discourse in a base propositional logic. This framework yields a powerful yet paradox-free set theory that implicitly recovers key features of stratified comprehension, reminiscent of Quine's New Foundations (NF), while offering a distinct proof-theoretic pathway to consistency. This novel set theory is used for Stroscot's language semantics.

Stroscot allows specifying properties about execution, which the compiler then attempts to prove or at least fuzz (see {ref}`Commentary/Language/Verification:Verification`). The most common form of property is membership in a set of values, described here. Constraining the set of values enables many useful optimizations to be performed.

Steelman 3C "It shall be possible to define new data types in programs. A type may be defined as an enumeration, an array or record type, an indirect type, an existing type, or a subtype of an existing type. It shall be possible to process type definitions entirely during translation. An identifier may be associated with each type. No restriction shall be imposed on user defined types unless it is imposed on all types."

# Stateful programming

In Stroscot, like in denotational semantics, a program is conceptually a mathematical function. That is, for any given input, the program will always produce the same output, and this output is the entire scope of the program - there are no implicit side effects, like mutating some external state. Practically, this works well for using the programming language like a calculator: put in an expression, get an answer. And theory-wise the denotational semantics is useful for reasoning about the behavior of programs, such as asking whether two programs are equivalent. Stroscot has settled on continuations or "tasks" as the denotational semantics model, as continuations are very general and can express many different control flow mechanisms, while not having the complexity of the state-passing monad in Haskell or uniqueness types of Clean.

# Syntax

Stroscot's syntax should be clear, readable, friendly, and consistent. It should be easy to learn and use for those who are new to programming. The language should be accessible to a wide range of users, including those with disabilities. Furthermore the syntax should be flexible, extensible, and customizable, so that developers can tailor aspects of the language to their specific needs and preferences.

S-expressions are sufficient for the core language, there is not a huge need for nice syntax. Syntax is one of the most boring aspects of building a language - there's not a lot you can innovate upon (except maybe using a large language model for parsing). So in Stroscot the syntax is defined as a compiler library.

# Term rewriting

Stroscot is based on a term rewriting formalism and supports logic programming, currying, higher order terms, pattern matching, and predicate dispatch (conditional rewriting). Stroscot uses an advanced and complex rewrite relation that detects cycles, handles infinitary rewriting by including limits of rewriting sequences, and rewrites "meaningless" terms to exceptions. Its evaluation strategy is specified to be normalizing - it uses outermost fair reduction for interpreted programs, and applies confluence and strong normalization analysis to compile programs to efficient assembly code or terminate reduction of meaningless terms. Top-level nondeterminism is not allowed, but local nondeterminism is, with special functions to reify the nondeterministic choices as a set. Meaningless terms are defined to be those that are root-active - they always reduce to a term with a top-level redex no matter how much they are reduced. Left-nonlinear patterns are supported using strict equality.

# Transactions

With transactions we can write various common synchronization constructs. Stroscot provides software transactional memory (STM) as a built-in feature of the language, as an alternative to low-level assembly-based synchronization primitives.

# Types

Stroscot has sets and assertions about whether values are in sets. But are sets types? Is Stroscot typed? If so, is Stroscot statically typed or dynamically typed? Is it strongly typed? Weakly typed? Gradually typed? These sound like reasonable questions. Unfortunately, these terms are ambiguous and controversial. This is an attempt to answer these questions by enumerating definitions, considering each definition, and ultimately discussing it to death.

Short answer: Stroscot lets you to build a prototype/simple script in a short amount of time, without any annotations. So it is dynamically typed. It also lets you add type annotations, so it is statically typed. Stroscot does not have explicit type inference.

# Values

In Stroscot values are defined to be expressions that are in normal form (strongly reduced), i.e. they evaluate to themselves. WHNF is not sufficient to ensure a value, e.g. `[1,undefined]` reduces to `undefined` hence is not a value. Traditionally a function is only defined on values, but lazy evaluation allows functions to produce useful behavior for non-values as well.

Values are immutable (as [Rich Hickey says](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg)) and have notions of equality, hashing, literal syntax, and deconstruction. In terms of memory management values can be copied freely, and discarded if they are no longer needed.

For convenience, "value" really describes the equivalence class of expressions that reduce to a value. In particular here we describes values by their concise syntactic form. These syntactic forms typically are reducible terms, rather than normal forms - usually they reduce to a value of the compiler's preferred data structure implementation. Programs may define the syntax here to other values.

# Verification

> 1B. The language shall be designed to [...] maximize automatic detection of programming errors.
> 1E. There shall be no language restrictions that are not enforceable by translators.
> -- Steelman

> The attitude today is that you can write any sloppy piece of code and the compiler will run diagnostics. If it doesn’t spit out an error message, it must be done correctly.
> -- `Peter G. Neumann <https://www.technologyreview.com/2002/07/01/40875/why-software-is-so-bad/>`__

> Compile-time static checking is wonderful, and as much as possible should be done 100% statically so that people cannot write incorrect programs.
> -- `Linus Torvalds <https://lkml.org/lkml/2022/9/19/1250>`__

> If you are willing to settle for anything less than full [static] verification of all properties then you might as well give up and use a dynamic type system.
> -- Stroscot

Stroscot aims to be a practical programming language, but it also aims to provide strong guarantees about program behavior, for example that array accesses are not out of bounds. In most cases these can be ensured statically by the verification system.

Verification is the process of verifying that a system satisfies a property. Static verification and symbolic execution is a natural extension of unit testing, and much more powerful. Building it into the language with a standardized API and UX will allow many amazingly robust programs to emerge.

Scalability: Verification is always a time and memory hog, but Stroscot will use advanced techniques to make it practical for real-world programs. The seL4 microkernel (8700 lines of C) has been successfully statically modeled and verified. Simple type-checking-like things are probably fairly straightforward to implement.
