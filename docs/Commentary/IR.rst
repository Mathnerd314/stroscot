Intermediate representation
###########################

An IR is necessary to interpret and compile efficiently. But what to use?
EaRing used GNU Lightning for its IR, which let it support multiple CPUs (x86-32, x86-64, PPC, SPARC) from the same source, but per LLVM the IR should actually be designed to be target-specific. So Stroscot handles instruction set differences the same way it handles OS differences, by providing portable abstractions in the standard library while still allowing low-level access to the platform.

TODO: read GNU lightning IR for design and features of assembly opcodes

Thread safe (hopefully) and library capable. Can even access .so libraries with C calling conventions.

Goals
=====

* Literal machine: All assembly instructions embed directly in the IR
* Wide-spectrum, PyPy compilation model: the source language is valid IR. all constructs are gradually translated to a restricted subset that the code generator understands.
* Debuggable: capture source locations and other information necessary for good error messages
* Target-specific: no point in trying to retarget IR for a different platform, the relevant code has probably been ifdef'd out already. So feel free to assume specific processor features, like 80-bit floats, calling conventions, alignment requirements, etc.
* Portable: if there is an intrinsic with the same behavior across several platforms, then it should use the same name, so that optimization rules only have to be written once
* Minimal primitives: there should be enough information in the IR to force the code generator to generate all assembly instruction behaviors, but no more
* Like Thorin: SSA plus CPS (explicit non-local control flow)
* Localized optimizations: read small portion, write small portion.
* Mostly pure: fixes evaluation order only for stateful operations

The minimal primitives is a question. Chopping up instructions into lots of mathematical operations means that there is a lot of overhead in code generation recognizing patterns of operations as instructions. On the other hand it allows writing a fewer number of more generic and powerful optimizations, instead of many processor-specific instruction patterns. So this choice favors ahead-of-time compilation at the expense of interpretation and JITing.

ASL's primary aim is enabling formal verification of ARM processors. see my upcoming paper “Trustworthy Specifications of ARM v8-A and v8-M System Level Architecture”


    ASL is an imperative rather than a functional language. Functional languages are often simpler for use during verification but functional languages and the coding tricks used to describe state changes are not familiar to many programmers so imperative languages are an obvious choice.

    ASL supports unbounded integers, infinite precision reals, fixed-size bitvectors, booleans, enumerations, records (aka structs) and arrays. Virtually all processor state is defined using the bitvectors but most instructions start by unpacking the bitvector into an integer, perform calculations on the unbounded types and then rounds or truncates the final result at the end to pack it back into the bitvectors.

    Using the more mathematical types “integer” and “real” means that as you read the specification you don’t need to be distracted by corner cases due to overflow or wraparound: all rounding, truncation, wraparound, etc. is explicit in the conversion back to bitvectors.

    ASL does not have a concise syntax for dereferencing pointers. In a processor, there are many different flavours of memory accesses: aligned or unaligned, cached or uncached, secure or non-secure, etc. and it is critical that every memory access is explicit about what type of memory access is being used. A side effect of this is that there in no type representing a pointer to some type: there are only bitvectors.

    ASL is strongly typed but uses type inference to give the early error-detection of strong typing without the clutter of having to annotate every variable declaration. The combination of fixed-size bitvectors and strong typing requires a slightly unusual type system where the length of the bitvector can depend on values calculated earlier in the code. A simple example is the function “Replicate(x,n)” which creates n copies of a vector x so if x has type “bits(8)” then “Replicate(x, 4)” has type “bits(32)”. When implementing the type inference system, this requires that bitvector lengths are represented as polynomials.

    ASL has support for throwing and catching exceptions. As you would expect, exceptions allow us to focus on the normal (non-exceptional) case when specifying instructions that might raise exceptions. The one downside is that it is more difficult to think about the tricky cases that happen if an exception does occur.

    Finally, ASL is used to write executable specifications. This is not necessary when using specifications for verification but for most of the other uses, it is critical. ASL does have some non-determinism though so executing specifications aims to follow one correct interpretation of the specification while for verification we use all correct interpretations of the spec.


IR Style
========

https://cs.stackexchange.com/questions/74794/why-is-static-single-assignment-preferred-over-continuation-passing-style-in-man


LLVM IR sucks
-------------

LLVM IR contains:

 * Undefined Behavior, with C "nasal demons" semantics and optimization passes that assume behavior
 * Intentional vagueness and edge cases that no one is interested in fixing.
 * Inconsistent IEEE-754 arithmetic

Interpreting LLVM is slow. High level abstractions are chopped up into lots of small low-level instructions. The interpreter has to execute a relatively large number of instructions to do virtual method calls. Languages built for interpretation use fewer more expensive instructions, and have lower per-instruction overhead.

JITing LLVM is faster than static C compilers, but it's not fast compared to purpose-built JIT compilers. The verbose instruction set requires recognizing patterns in groups of instructions, and then emitting code for the patterns. This works, but it's more involved than a simple template generator.

LLVM IR also isn't capable of representing the necessary semantic information for high level languages such as Objective-C without embedding the information into it using hacky mechanisms. Objective-C works around this by reverse-engineering Objective C information out of the lowered code, which isn't guaranteed to be safe by LLVM IR rules alone and only works because the Objective C frontend generated the IR.


Control flow
============

The ADD instruction is not so simple

Control flow graph

Blocks
======

A basic block (BB) is a sequence of instructions that is entered only from the top, and that contains no terminator instructions except for a single one at the end. The last instruction in a BB must be a terminator instruction, so execution cannot fall through the end of the BB but instead jumps to a new BB.

Terminator instructions are unconditional branches.

Per cranelift:

EBB parameter
    A formal parameter for an EBB is an SSA value that dominates everything
    in the EBB. For each parameter declared by an EBB, a corresponding
    argument value must be passed when branching to the EBB. The function's
    entry EBB has parameters that correspond to the function's parameters.

EBB argument
    Similar to function arguments, EBB arguments must be provided when
    branching to an EBB that declares formal parameters. When execution
    begins at the top of an EBB, the formal parameters have the values of
    the arguments passed in the branch.


A basic block is a mixture of jump and non-jump instructions that is complete, in the sense that any execution of the program will take one of the jumps. Any arbitrary sequence of instructions can be turned into a basic block by adding an unconditional jump at the end.

Although phi nodes were an interesting idea all the `cool kids <https://mlir.llvm.org/docs/Rationale/Rationale/#block-arguments-vs-phi-nodes>`__ are now using block arguments. Blocks arguments fit better into various analysis passes.

Blocks
======

From a user perspective there are two types of jumpable addresses:

memory - effective address computation
SIB addressing form, where the index register is not used in address calculation, Scale is ignored. Only the base and displacement are used in effective address calculation.
VSIB memory addressing



Memory and the program counter are virtualized as well, using labels. A label refers to a memory location with a specific block of code loaded. The blocks are not ordered, so unconditional jumps must be inserted between blocks if necessary. The block order can be determined using profiling, removing the unconditional jump that is taken most often.

Memory references should be virtualized as well, so we also have memory labels. The alignment and format of the memory address should be specified.

Instructions and blocks are marked by the virtual registers they consume and use (input / output registers). The call and jump instructions are special in that a mapping may be given between the virtual registers and physical registers. Instruction constraints:
* Output: the register must not contain a value used after the block
* Output early clobber: output and the register must not be used for any inputs of the block
* Input: the register is read but not written to. Multiple inputs may all be assigned to the same register, if they all contain the same value.
* Tied input: register that is read and written
* Tied input early clobber: register that is read and written and does not share a register with any other input
* alignstack, sideeffect

There are also constraints from the ABI calling convention: https://gitlab.com/x86-psABIs/x86-64-ABI

