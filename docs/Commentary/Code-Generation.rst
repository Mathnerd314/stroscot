Code generation
###############

The back-end is complicated. There are three main operations that need to be done.

#. Instruction selection
#. Register allocation
#. Instruction scheduling

Per Unison/Blindell, all of these are interdependent and must be solved as a single, large constrained combinatorial optimization problem to find the optimal solution. These three criteria encompass many other optimizations, so solving them properly is worthwhile.

* Peephole optimization
* Code motion
* Block ordering
* Spilling, rematerialization

Register allocation
===================

Definitions:

temporary
  A location storing a bitstring, mapped to a logical register or memory location.
virtual register
  A temporary that has write access. Not used because the codegen uses SSA.
(logical) register
  A register as used in an instruction
physical register
  A CPU register in the hardware register file. Logical registers are dynamically mapped to these, making many register assignment decisions immaterial.
program point
  start of an instruction
live
  An output t is live at a program point if t holds a value that might be used later by another instruction j. The instruction j is said to be dependent on i.
interference point
  A program point where each of a set of output temporaries could be used later.
spilling
  Spilling is assigning a temporary into memory. It requires the generation of spill code, store/load instructions to move the value to and from memory.
register class
  Most CPUs group registers into classes, such that a given instruction can only use a certain class of registers. For example, there may be floating point registers, scalar integer registers, and vector registers.
register alias
  Some registers may be aliases to parts of registers in another class. A good example are the x86 registers al and ah, which alias to the low/high part of ax, which in turn aliases to the low part of eax, itself the alias of the low part of rax.

At each program point there is a map from variables to registers or memory. Registers and memory have relatively similar APIs: read and write. Processor registers have shorter access times, but they are limited in number, forcing some temporaries to be stored in memory. The straightforward approach spill-everywhere is to store every temporary to memory, wrapping each instruction with spill instructions that store/load each input/output from memory. But it's slow. So the problem is to apply various conflicting optimizations to spill-everywhere to get the fastest program:

* Register assignment: Preserve a temporary in a register
* Live range splitting: Map a temporary to different registers in different parts of its live range
* Multi-allocation: Map a temporary to a register as well as memory
* Load-store optimization: Reuse values loaded in previous parts of the spill code
* Rematerialization: Recompute values at their use points rather than loading them from memory
* Coalescing: eliminates move instructions by ensuring the source and destination are the same.
* Packing: assign temporaries of small bit-widths to different parts of larger-width registers

There are various heuristics, e.g. assigning the most used variables to registers first. Because of register renaming / memory buffering, the actual register / address assignment doesn't matter, only the spill pattern. (TODO: check this with some benchmarks)

Instruction selection
=====================

The IR is split into a series of instruction patterns, a forest of trees. Usually a tree rewrite system is used - bottom up rewrite generator (BURG). See pyburg.

One way is to write a lot of patterns and try all these patterns in turn. If a pattern matches a specific sequence of instructions, the pattern can be applied, and the instructions are substituted by the pattern substitute. Another way, is to define per instruction the effects of the instruction, and for each pair of instructions that are evaluated, combine the effects of these instructions. If there exist an instruction which has the same effect as the combined effect of the two original instructions, the substitution can be made. This is the combiner approach as described by [Davidson1980].

The advantage of having the combiner, is that only per instructions the effects of the instruction must be defined. After this, all instructions with effects can be potentially combined. This reduces the amount of work to define peephole optimization patterns from N * N to N. Namely, not all instruction combinations must be described, but only the effects per instruction.

* Instruction selection - replacing sequences of instructions with cheaper/shorter sequences of instructions.
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