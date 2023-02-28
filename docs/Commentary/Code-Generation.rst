Code generation
###############

The back-end is complicated. There are three main operations that need to be done.

#. Instruction selection
#. Register allocation
#. Instruction scheduling

Per Unison/Blindell, all of these are interdependent and must be solved as a single, large constrained combinatorial optimization problem to find the optimal solution. These three tasks encompass many optimizations, such as:

* Peephole optimization
* Code motion
* Block ordering
* Spilling
* Rematerialization
* Common subexpression elimination

Stroscot uses assembly instruction intrinsics in the language and IR. It doesn't provide "inline assembly", meaning literal blocks of texual assembly code - the closest is writing the hexadecimal machine code and pretending it's a single instruction. So it makes sense to provide a performance guarantee that the code generated from intrinsics will meet or beat anything someone could hand-code. Unison can do a search over the complete space of instruction sequences, hence can actually find the optimal solution and satisfy this guarantee.

Register allocation
===================

Definitions:

* A temporary is an identifier for a bitstring value - it is not modified. A "virtual register" would be a temporary that has write access but these are eliminated by the codegen's SSA pass.
* A location is a logical register or virtual memory address.
* An instruction takes some temporaries as inputs and produces other output temporaries, then jumps to an address. A program point is the logical start of the execution of an instruction (as opposed to the logical end). To encode an instruction each temporary must be assigned to a fixed location supported by the instruction (proper register class or encodable memory location) at the program point.
* A (logical) register is a register identifier as encoded in an instruction. The number of logical/ISA registers is relatively small, e.g. 31 on ARM and 16 on AMD64. Logical registers are dynamically renamed/mapped to physical registers, storage locations in the microarchitecture's physical register file, making many logical register assignment decisions immaterial. For example, per `Anandtech <https://www.anandtech.com/show/3922/intels-sandy-bridge-architecture-exposed/3T>`__, Sandy Bridge has 160 physical integer registers.
* A "subregister", mainly in x86, is a logical register that is really part of a larger logical register. For example, on x86-64, rax has subregisters eax, ax, al, ah. These allow some instruction encoding tricks for sub-64-bit bitstrings, but have to be specially handled to detect conflicts.
* A register class is the set of logical registers that can be used for a specific operand for a specific instruction. ARM only has a small number of register classes used consistently across all of its instructions, e.g. floating point registers, (scalar) integer registers, or vector registers, and any register in the class can be used with any instruction. But on x86 there are exceptions, e.g. the div instruction hardcodes edx:eax, so div uses its own special register class for those operands.
* Register allocation creates a multimap at each program point from temporaries to registers or memory. Similarly at each program point there is a 1-1 map from registers and memory to bitstrings. Memory is not really 1-1 because of the memory model but private memory for register allocation is deterministic so we can ignore it.

Registers and memory have relatively similar APIs, read and write, so they can be exposed as temporaries or virtual registers before codegen. But on the hardware, registers have shorter access times, and are required for many instructions, but they are limited in number, forcing temporaries to be stored in a mixture of registers and memory. The straightforward approach "spill-everywhere" is to wrapping each instruction with spill instructions that load each input from memory and then store each output to memory, and then use standard memory allocation techniques. But it's slow. So the problem is to apply various conflicting optimizations to get the fastest program:

* Register assignment: Store a temporary in a register and use that register instead of reading from memory
* Spilling is "materializing" a temporary in memory by generating spill code, instructions that store/load the value in memory. In practice C2 talks about "live range splitting" instead of spilling - if a temporary is used in several places, C2 makes an effort to reuse the same register, which in post-regalloc will coalesce and emit no instructions, but otherwise the reg-reg or mem-reg movement instructions are emitted post-regalloc. A temporary t is live at a program point if t holds a value that might be used later by another instruction j. The instruction j is said to be dependent on i. LRS maps a temporary to different registers in different parts of its live range. An interference point is a program point where each of a set of output temporaries is live (could be used later).
* Multi-allocation: Map a temporary to a register as well as memory
* Load-store optimization: Reuse values loaded in previous parts of the spill code
* Rematerialization: Recompute values at their use points rather than loading them from memory
* Coalescing: Eliminate move instructions by ensuring the source and destination are the same.
* Packing: assign temporaries of small bit-widths to subregisters of larger-width registers

There are various heuristics, e.g. assigning the most used variables to registers first. Because of register renaming / memory buffering, the actual register / address assignment doesn't matter, only the spill pattern. (TODO: check this with some benchmarks)

C2 register allocator per Cliff:

* all the X86 registers & constraints are directly represented; no special code for e.g. div/mod
* stack gets colored just like a register (which makes temps "free" when thinking about them). "stack" registers have an infinite number, so always color in one pass.
* SSA form thru the entire reg allocation process. No requirement for special cycle-breaking for any registers, including XMM, other than you got a "free" break when the SSA form got linearized.
* During the copy-coalesce phases C2 can skip some lds if a the value in already known to be in the correct place. Can "rematerialize" constants, and can have a value in a cpu register and in a "stack" register. There's no "home" location like in some other allocators (home locations never played well with the JMM)
* If coloring fails on the cpu regs, the live range(s) are split with copies that can sink/source stack and cpu regs & you color again.

algorithm: you start with a single large live range, of many def's and use's, of many branches & arms & cycles in the cfg. This is over coalesced to make max live ranges. This typically over-constrains colors (returning reg from fcn Call is in RAX, but needed in argument RCX for next call) and fails to color, so you split it at places with hard register constraints. Still has huge live ranges and fails to color, but in fewer places. Track the losers and split the losers around high-pressure/high-frequency blocks. Still fails to color, but again fewer losers. Construct a "hard win" split, an inefficient coloring that uses many smaller ranges, connected by copies into stack. Very rarely must repeat hard win splitting a few times. Then you try to coalesce things, under the hard-win max split by picking colors from copy-connected live ranges to match - "smart coloring" based on neighbors and split history. In the absence of better options, you pick colors based on the original hard-win split, figuring that those live ranges are "more likely" to re-coalesce together. At no time after splitting a v to the stack, do you later spill v again to the stack, you already got that one done. There is another post-pass smart coloring attempt using local history and not live ranges.

End result is typically very good on all the hot paths, never very bad on cold paths and never gets into a death-spiral of spilling.  I've done at least 4 for-production-use allocators in my life, including C2.
Oh yeah, and its by far the fastest register graph-coloring allocator I've ever seen.  Key point for a JIT, as reg-alloc can be expensive.  Its often about 40% of C2's compile budget.


Instruction selection
=====================

Instruction selection transforms a sequence of IR instructions into the cheapest/shortest sequence of processor-specific instructions.

Blindell's universal instruction selection thesis is the main reference here. STOKE can find probabilistically optimal straightline assembly sequences using a specialized search algorithm, which is also a form of instruction selection so should be integrated.


for literal assembly, we can either emit it as-is or try to optimize it. If we can actually optimize it to a faster but equivalent sequence, great, but we don't want to transform a compound operation into several simpler instructions, fuse the simpler instructions with nearby instructions from other operations, fail to identify the compound operation due to the fusion, and lose performance.


The IR is split into a series of instruction patterns, a forest of trees. Usually a tree rewrite system is used - bottom up rewrite generator (BURG). See pyburg. One way is to write a lot of patterns and try all these patterns in turn. If a pattern matches a specific sequence of instructions, the pattern can be applied, and the instructions are substituted by the pattern substitute.

The combiner approach per :cite:`davidsonDesignApplicationRetargetable1980` integrates peephole optimizations. The effects of instructions are specified in a machine-independent register transfer language ISP. The definition of ISP is somewhat vague but basically you have read and assign register/memory, literals, conditionals, and math.
A compiler can directly emit ISP or you can start with assembly instructions and convert one-at-a-time into ISP using the effect descriptions. Then there are standard optimization like dead store elimination.

 Another way, is to define per instruction the effects of the instruction, and a combiner that specifies how to combine two instructions given their effects, if there exist an instruction which has the same effect as the combined effect of the two original instructions. This is the combiner approach as described by [Davidson1980]. The advantage of specifying effects is that the amount of work to define peephole optimization patterns is N * N + M rather than M * M, where N is the number of effect patterns and M=81 is the number of instructions.

*
* Peephole optimizations / strength reduction - like ``x*2`` by ``x << 1``/``x+x``, or setting a register to 0 using XOR instead of a mov, exploiting complex instructions such as decrement register and branch if not zero.
* Sparse conditional constant propagation - dead code / dead store elimination, constant folding/propagation
* Partial evaluation
* common subexpression elimination, global value numbering - tricky with blocks
* code factoring - CSE but for control flow
* Test reordering - do simpler tests first - treat control flow as data
* Removing conditional branch cases if can prove won't be taken
* Inlining

* Space optimizations - anti-inlining
* Trampolines allow placing code at low addresses
* Macro compression compresses common sequences of code

Instruction Scheduling
======================

Instruction scheduling assigns issue cycles to program instructions. Valid instruction schedules
must satisfy instruction dependencies and constraints imposed by limited processor resources.

Latency
  the minimum number of cycles that must elapse between the issue of the depending instructions. Variable latencies (such as those arising from cache memory accesses) are typically handled by assuming the best case and relying on the processor to stall the execution otherwise.

Resources
  resource model where each resource s has a capacity cap(s) and each instruction i consumes con(i, s) units of each resource s during dur(i, s) cycles. VLIW processors can be modeled by an additional resource with capacity equal to the processor’s issue width.

CPU model:

::

  Fetch / decode / cache / fuse instructions into micro-ops and place into queues
  Retrieve the next uop instruction from the head of the instruction queues.
  record physical register names of logical register inputs
  assign new physical registers to output logical registers
  stall the instruction until a station is free.
  assign the station to the instruction
  stall the instruction until all physical input registers become available.
  execute the instruction at the station.
    store/load interact with memory order buffer
      memory prefetching - Processor does lookahead and fetches early. Stall if not available/
    zeroing a register is a no-op because all physical registers are initialized to zero
    "retired" - finished executing
  buffer outputs in reorder buffer until earlier instructions have completed
  un-stall instructions at stations that now have their inputs available - can take some cycles for cross-station RAW dependencies

The instruction scheduler schedules the instructions intelligently to avoid stalling, i.e. an instruction requesting data before it is available. Ideally each instruction arrives at the front of the pipeline at the exact cycle when the necessary data and execution station become available.

Data hazards: RAW is unavoidable. WAR/WAW are eliminated in modern processors by renaming as in the `Tomasulo algorithm <https://en.wikipedia.org/wiki/Tomasulo_algorithm>`__. WAW can be also ignored if the value isn't used.

timing of instructions - most are fixed. load operations depend on what's cached.

* Scheduling / reordering / pipelining
* minimize pipeline stalls, when an instruction in one stage of the pipeline depends on the result of another instruction ahead of it in the pipeline but not yet completed.
* ensure the various functional units are fully fed with instructions to execute.
* avoid cache misses by grouping accesses
* clear out unconditional jumps (inlining). Avoid inlining so much that it cannot fit in the cache.
* splitting/combining recursive calls / basic blocks
* Bias conditional jumps towards the common case

branch prediction: branch target buffer (BTB), indirect branch target array, loop detector and renamed return stack buffer. mispredicted branch clears cache and restarts.

Layout
======

For example getting rid of the jump here:

.. code-block:: asm

    jmp my_label
    my_label:

even if the jump can't be avoided, memory layout can affect program performance. see profile guided memory layout thesis

Cliff says a list scheduler is generally sufficient

C target
========

When we compile to C it is quite similar to writing an interpreter in C with specialized opcodes. So LuaJIT is relevant. LuaJIT's interpreter is fast, because:

* It uses indirect threading (aka labeled goto in C).
* It has a very small I-cache footprint (the core of the interpreter fits in 6K).
* The parser generates a register-based bytecode.
* The bytecode is really a word-code (32 bit/ins) and designed for fast decoding.
* Bytecode decode and dispatch is heavily optimized for superscalar CPUs.
* The bytecode is type-specialized and patched on-the-fly.
* The dispatch table is patched to allow for debug hooks and trace recording. No need to check for these cases in the fast paths.
* It uses NaN tagging for object references. This allows unboxed FP numbers with a minimal cache footprint for stacks/arrays. FP stores are auto-tagging.
* It inlines all fast paths.
* It uses special calling conventions for built-ins (fast functions).
* Tuning and tricks.

The control-flow graph of an interpreter with C switch-based dispatch looks like this:

::

  repeat {
    load instruction
    dispatch instruction
    switch(instruction_type) {
      case X:
        decode operations
        if good
          fast instruction execution
        else
          slow execution
    }
  }

There are dozens of instructions and hundreds of slow paths. The compiler doesn't know which paths are fast. Even if it did, it's still a single giant loop body. The standard register allocation heuristics fail at this scale, so the compiler has trouble keeping important variables in registers. There's just no way to give it a goal function like "I want the same register assignment before each goto". Diamond-shaped control-flow is known to be the worst-case scenario for most optimizations and for register allocation. Nested diamond-shaped control-flow is even worse. Tail-merging and CSE will happily join all these common tails of each instruction and generate a single dispatch point. Ick. You can try to disable some optimizations for this piece of code, but this will negatively impact all paths. Almost nothing can be hoisted or eliminated, because there will be a slow path where an aliasing store kills all opportunities.. The slow paths kill the opportunities for the fast paths and the complex instructions kill the opportunities for the simpler instructions.

We can use direct or indirect threading with computed goto. clang/LLVM optimizes the looped switch to indirect threading at ``-O``. (`ref <https://internals.rust-lang.org/t/computed-gotos-tco-threaded-interpreters-experiments-and-findings/4668/6>`__)

::

  static void* dispatch_table[] = { &&OP1, &&OP2, ... };

  // indirect
  #define DISPATCH(ip) goto *dispatch_table[memory[ip] >> 12]
  // direct
  #define DISPATCH(ip) jump *ip++

  DISPATCH();

  OP:
      decode operands
      execute instruction
      ip = reg[R_PC]++ // load next instruction
      DISPATCH(ip); // dispatch next instruction
  ...


This effectively replicates the load and the dispatch, which helps
the CPU branch predictors.

If you compile directly to assembly, you can do better:

* Total control over the register assignment
* Can fix the calling convention and keep all important state in registers for the fast paths. Spill/reload only in the slow paths. (No C compiler manages to do that on x86.)
* Only a single fast path in every bytecode instruction
* The fast paths are always the straight line fall-through paths.
* Move the slow paths elsewhere, to help with I-Cache density.
* Pre-load instructions and pre-decode operands.
* Remove stalls. Interleave operations based on the data dependencies.

The C compiler does have these optimizations but figuring out the right C code to generate so that the program will optimize properly is hard.

More on optimization
====================

https://mastodon.social/@zwarich@hachyderm.io/109559009711883166

high-performance programming

coroutine switching and resource competition (I/uop cache, D cache, BTB) makes it slow - use buffering
SIMD/AVX2 branch-free code
avoid branch mispredictions. Branch mispredicts are highly data dependent so it's all about your use case. There's a lot of variance. The more you micro-optimize for one case, the bigger the variance gets for others. Part of optimizing is building an understanding of the empirical statistics of your data so you can make the right optimization trade-offs. Reducing L1D pressure while increasing branch mispredicts can be a net win (L1 load latency 4-6 cycles).

"hot state" should be in registers at all times. Store non-hot state in memory. Register allocators can really only be trusted to do two things: move spill code out of loops and reduce the impact of calling conventions. Register allocation in handwritten bytecode interpreters often relies on reasoning of the form "this opcode is going to be slower anyways, so it's okay to put the spill code there", which is not captured by most register allocators. The compiler is not perfect. In some cases better usage of profile info by the register allocator would suffice. In other cases, a better cost model for spill code would be required, e.g. Proebsting & Fischer's work on "probabilistic" register allocation: https://dl.acm.org/doi/abs/10.1145/143103.143142 Once you are trying to optimize things to this level you really want control. Systems languages should really have more ways of constraining the compiler (best-effort constraints as well constraints that generate compile-time errors if they can't be satisfied). From a constraint solving perspective it should be exactly as easy/hard as constraining the hot state to be in the ABI argument registers and ABI register targeting for function calls is already a core competency of any usable register allocator.

This affects coding style for dispatch loops:

* a loop with a big switch statement. In theory, the loop-switch gives the compiler the ability to look at the whole block graph and make optimal decisions about register allocation, hoisting, etc. In practice, the compiler will make terrible decisions (e.g. register pressure on one rare branch will screw all the other branches) and there's no tools available to control the compiler's register allocation.
* unchecked table load - you can compress an 8 byte pointer to a 2 byte offset. You just have to use a separate linker section so you can guarantee they're physically clustered. Only the "head blocks" that are targeted by a jump table need to be in the section. So most space-efficient option.
* tailcalls - can let you control the convention at the IR level, but still no control at language level
* inline assembly. you can specify the register convention with input/output constraints. But not really maintainable.
* computed goto - sort of like tailcalls + asm - decoupling of having separate functions, maintainable and reliable

The intrinsic branch mispredict penalty (IBMP) is the minimum time it takes from when a mispredicted control dependency retires to when the first uops from the correct PC can retire. It is always <= to the minimum pipeline depth starting at the uop cache and finishing at retirement; the pipeline depth may be larger because there are additional "clean-up" cycles that have to be serialized with the pipeline redirect and restart. For x86 the penalty is around 20 cycles, although some say it can be as low as 15. I always use 20 cycles as a round number regardless of uarch.

If a dependency chain is only consumed as a control dependency, its latency essentially doesn't matter (within limits) if the branch never mispredicts. But as soon as the consuming branch mispredicts, you end up paying for that latency in full. In two versions of the same code where you add 10 extra cycles of latency to a mispredicted control dependency for one version but not the other, the effective mispredict penalty increases by 10 cycles because you discover the mispredict 10 cycles later than in the other version. So I define the effective mispredict penalty, effective_penalty = IBMP + control_latency. But it doesn't always work like that because control latency is affected by other things. However, you often find that, if I reduce the latency of this control dependency by 10 cycles, it should reduce the effective mispredict penalty by 10 cycles.

ops already in flight, from before the mispredicted branch, will have IBMP cycles of free time relative to the same path if the branch had been correctly predicted. Or to put it differently, when you restart at a PC after a mispredict, reading a register for the result of a pre-branch mid-latency op like an L2 load is effectively zero latency. So before you take a hard-to-predict branch, you really want to issue as many of these medium latency ops as you can, even speculatively hoisting those ops from different successors into the common predecessor, so long as you have free pipeline slots to spare. branch-free computations are inherently latency sensitive, so need the data preloaded. This preloading idea is effective both in the ideal dependency graph sense (which assumes infinite pipeline width and lookahead) and also that after restarting from a branch mispredict the scheduling window starts out very narrow and so as a programmer if you manually kick off critical ops early like you were on an in-order machine, it's going to reduce latency.

A high fan-out jump table is the most efficient when a branch is really unpredictable e.g. 8 choices with 1/8 probability. For more skewed conditions use a series of conditional tests.

uica analysis

