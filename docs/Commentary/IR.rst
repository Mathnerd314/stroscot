Control flow
============

The ADD instruction is not so simple

Control flow graph

Blocks
======

A basic block (BB) is a sequence of instructions that is entered only from the top, and that contains no terminator instructions except for a single one at the end. The last instruction in a BB must be a terminator instruction, so execution cannot fall through the end of the BB but instead jumps to a new BB.

Terminator instructions are unconditional branches.

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

IR Style
========

Goals:

* represent non-local control flow (faults)
* optimizations are localized (read small portion, write small portion)
* all known optimizations can be implemented
* fixes evaluation order only for stateful operations

https://cs.stackexchange.com/questions/74794/why-is-static-single-assignment-preferred-over-continuation-passing-style-in-man


LLVM IR sucks
-------------

LLVM IR contains:

 * Explicitly Target-specific features, like x86_fp80
 * Target-specific ABI code, to interoperate with native C ABIs
 * Implicitly Target-specific features, like Linkages.
 * Target-specific limitations: alignment on alloca, supported integer types
 * Undefined Behavior, with C "nasal demons" semantics and optimization passes that assume behavior
 * Intentional vagueness and edge cases that no one is interested in fixing.
 * Inconsistent IEEE-754 arithmetic

Interpreting LLVM is slow. High level abstractions are chopped up into lots of small low-level instructions. The interpreter has to execute a relatively large number of instructions to do virtual method calls. Languages built for interpretation use fewer more expensive instructions, and have lower per-instruction overhead.

JITing LLVM is faster than static C compilers, but it's not fast compared to purpose-built JIT compilers. The verbose instruction set requires recognizing patterns in groups of instructions, and then emitting code for the patterns. This works, but it's more involved than a simple template generator.

LLVM IR also isn't capable of representing the necessary semantic information for high level languages such as Objective-C without embedding the information into it using hacky mechanisms. Objective-C works around this by reverse-engineering Objective C information out of the lowered code, which isn't guaranteed to be safe by LLVM IR rules alone and only works because the Objective C frontend generated the IR.

