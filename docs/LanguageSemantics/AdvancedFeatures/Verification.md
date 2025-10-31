---
# Math frontmatter:
math:
  '\true': '{\mathit{true}}'
  '\false': '{\mathit{false}}'
  '\seq': '{{\langle #1 \rangle}}'
  '\sem': '{[\![ #1 ]\!]}'
  '\setsem': '{\bigcup_{e \in #1} \sem{e}}'
  '\locs': '{\mathit{L}}'
  '\op': '{\mathit{op}}'
  '\pc': '{\mathit{pc}}'
  '\pcvar': '{\mathit{pc}}'
  '\pco': '{\mathit{pc_0}}'
  '\pce': '{\mathit{pc_{err}}}'
  '\meet': '{\sqcap}'
  '\cpa': '{\mathbb{D}}'
  '\Nats': '{\mathbb{N}}'
  '\Bools': '{\mathbb{B}}'
  '\Ints': '{\mathbb{Z}}'
  '\strengthen': '{\mathord{\downarrow}}'
  '\transconc': '{\smash{\stackrel{#1}{\rightarrow}}}'
  '\transabs': '{\smash{\stackrel[#2]{#1}{\rightsquigarrow}}}'
  '\merge': '{\mathsf{merge}}'
  '\stopop': '{\mathsf{stop}}'
  '\wait': '{\mathsf{waitlist}}'
  '\reached': '{\mathsf{reached}}'
  '\result': '{\mathsf{result}}'
  '\compare': '{\preceq}'
  '\implies': '{\Rightarrow}'
  '\BUG': '{{\sc fa}}'
  '\flag': '{\mathit{flag}}'
  '\Itp': '{\smash{\mbox{\sc Itp}{(#2,#3)(#1)}}}'
---


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

## Properties

The most common property in Stroscot is membership of a value in a set (bound checks, type safety, etc.). But there are "temporal" properties which cannot be described as sets - liveness, termination etc. Stroscot will support a variety of properties, generally anything expressible in modal μ-calculus.

### Reachability

A reachability (safety) task consists of a program annotated with a set of error states, with the goal to show that the error states are unreachable, or otherwise to find a feasible program path to an error state. This can be used to verify assertions and check for type errors.

To prove unreachability we exhibit a covering domain with no concrete error states in any of the abstract states. To prove reachability we produce a concrete feasible path ending in an error state. The counterexample can then be fed into a debugger to determine what changes to make to the program.

#### Exceptions

The main reachability analysis figures out which exceptions a piece of code may throw. Top-level unhandled exceptions are reported as warnings.

#### Assertions

10F. It shall be possible to include assertions in programs. If an assertion is false when encountered during execution, it shall raise an exception. [Note that assertions can be used to aid optimization and maintenance.]

Assertions have a simple form `assert expr` that throws `AssertionFailed`, equivalent to `when expr (throw AssertionFailed)`. Java's complex form `assert expr : exception` that throws a specific `exception` on failure seems pointless - it's only a little less verbose than `when expr (throw exception)`. Could be worth it though, throw it to the standard library to decide.

!0F. It shall also be possible to include assertions, such as the expected frequency for selection of a conditional path, that cannot be verified.

This part we can ignore, Stroscot verifies everything. The example given is more like a pragma for optimization.

#### Dead code

Reachability can also find dead (unreachable) code, like unused declarations, unused variables, or unsatisfiable conditions. Code is only dead if it is unreachable on all compilation configurations, so the build configurations must be interfaced. Assertions can exercise code too.

Many exceptions are unwanted, e.g. "no patterns matched in case". Reachability can verify these are dead code.

### Termination

Termination checking verifies properties like "A function call must eventually return" or "A program execution that calls malloc() must eventually call free()". An infinite state transition sequence that doesn't call free is a counterexample. Termination is a liveness property - it's different from a safety property "A call to free must be preceded by a call to malloc". It's also different from "If the program ends gracefully then all memory has been freed". A lot of programs look like `repeat { handleCommand{} }` and for those we can prove termination of `handleCommand` but not the loop. But we can prove graceful exit.

### Equivalence

Since the semantics of method dispatch and concurrency are non-deterministic, we would like to verify that the program is well-defined. This takes the form of checking that all execution paths of a program produce equivalent results. It's similar to confluence but a little weaker. The most tricky part is defining equivalence as an [equivalence relation](https://en.wikipedia.org/wiki/Equivalence_relation).

Equivalence of pure programs is based on comparing the set of values each program evaluats to - two programs are equivalent if they return the same set of exception and non-exception values.

Equivalence of I/O programs is based on comparing events: we represent all I/O actions in a datatype and then compare as for pure programs. We get a tree of functions returning functions and so on, based on the external I/O. Observable behavior is defined by an I/O model that interprets the actions. For example, equivalent executions must write the same files and the same contents to the files, but not necessarily in the same order. But really it is up to the user to decide, maybe writing files in a different order is bad.

In the literature there is a notion of bisimulation. But here our state transition graph includes computation transitions, while the amount of computation is not relevant for equivalence. But of course bisimulation implies equivalence.

Equivalence gives a stronger notion of dead or redundant code. For example, if the program is equivalent when commenting out an I/O statement, or if all the paths of a conditional statement are the same. It also allows verifying optimizations.

### Thread safety

Thread safety means avoiding race conditions and deadlocks. Basically each thread executes some number of actions, nondeterministically. We also have to use a hardware memory model that determines what behavior is visible across threads. The goal is to verify a lack of race conditions, i.e. that observable behavior of the program is not affected by the choices of the scheduler. Deadlock is when there is no runnable thread and the program has not exited; it is only defined for programs using locks or similar blocking synchronization primitives.
