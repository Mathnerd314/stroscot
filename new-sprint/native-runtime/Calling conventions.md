# Calling conventions

## What is a calling convention?

So let's imagine we have some code in the caller and we want to pass control to another code — the callee. We have machine resources - registers, stack, etc. The typical characterization of a calling convention is as a protocol between caller and callee regarding these resources. But what if we want to call arbitrary machine code (especially with LTO, JITs, obfuscated binaries, or hostile code)? In this case, we have no control over the callee, while we have full control over the caller setup. Even if we can modify the callee, via binary patching or JIT recompiling mechanisms, the situation is asymmetric - it is fairly straightforward to modify the calling code to call a trampoline and set up the appropriate machine state. But it is much harder to modify the called code to satisfy a desired property regarding the output machine state.

So a “calling convention” is best understood as a set of summarized properties of the called code, specifically **a set of assertions about the callee’s behavior**. The caller does not *define* or *affect* the convention in any way; the caller simply **ingests** the information about whatever calling convention the callee’s machine code implements, and uses that to inform the machine state it wants to set up. This set of assertions can take the form of fixed templates like “this code conforms to the cdecl ABI” or “this code follows the SysV AMD64 ABI” but essentially it is a problem of **predicate abstraction** and we may want to make more informative assertions about the callee if we are optimizing or less informative assertions if we are trying to be more generic. For example we may be interested in the empirical set of preserved registers (not just the registers guaranteed preserved by the ABI), or we may be interested in writing a generic interface to call assembly stubs and just need to verify certain assumptions about stack alignment.

Because a calling convention is a predicate abstraction, and predicates are defined over a combinatorial space, we will never have a complete description of all the properties we might care about, so the code should be designed so we can just hack in new properties as we see fit. But we should ensure our set of properties is complete in that all logical posssibilities for the classification are enumerated. The classification code should never say that a piece of code is invalid or nonsensical, but just bail out to a sensible catch-all classification.

## How do I use a calling convention?

Pretty much the only way is a trampoline, a little piece of assembly code which can be entered with a fixed, known calling convention. The caller calls the trampoline, the trampoline sets up the machine state for the callee's calling convention, and then uses the appropriate transfer of control to the callee. However, when a compiler is generating assembly code, it can inline the trampoline into the rest of the assembly and optimize the call, which leads to cool things like zero-instruction trampolines that just fallthrough because there is nothing to do. And so the trampoline can be hard to see, but if you squint, you can pretty much always find a trampoline hiding in there somewhere.

## How do I transfer control to the block?

There are different ISA-level instruction or transfer mechanisms:
- Fallthrough — contiguous code, no explicit control transfer at ISA level
- CALL — standard call instruction (pushes return address)
- JMP — jump without saving return address
- RET - control transferred via stack pop mechanism
- SYSCALL — dedicated system call instruction (syscall, svc, int 0x80)
- VMCALL/HVC/SMC — hypervisor or secure monitor call instructions
- Interrupt/signal — control transferred asynchronously by hardware/kernel (not initiated by program)

And these specify the target address in different ways - statically encoded in machine code or dynamically in register / memory.

In practice though it doesn't matter, you can use whatever mechanism and they all set up the same initial machine state from the perspective of the callee.

## So what is a calling convention really?

We model a callee as a mathematical state transformer \(f : S_{in} \rightarrow S_{out}\), where \(S\) is an abstract machine state (registers, stack, flags, hidden state, etc.). The “calling convention” is then a structured summary of:

- Which locations are logically inputs, which are destroyed, and which are preserved.
- A *stable mapping* of logical arguments and return values into those locations.
- How the stack and frame behave (alignment, stack deltas, red zone, shadow space, TCO, unwinding).
- Extra relational and architectural invariants (SysV variadic `AL`, struct‑return pointer aliasing, CET, DF/MXCSR, x87 depth, segment bases, etc.).

This abstraction corresponds to what a just-in-time foreign function interface layer needs to generate a trampoline: given such a contract, this layer can implement safe calls into arbitrary blocks by serializing the logical arguments to match the callee’s expectations, then deserializing the callee’s outputs back into a logical representation.

***

## Core state and liveness model

### Abstract locations

We work with **abstract locations** rather than hard‑coded “RAX/RBX/…”, so we can uniformly represent:

- General‑purpose registers (e.g., `RAX`, `RDI`, `RCX`).
- SIMD registers and sub‑lanes (`XMM0`, `YMM6[0:128]`, etc.).
- Stack locations relative to some base (`[SP_in + 8]`, `[CFA - 16]`).
- Hidden flags and control registers (`DF` in `EFLAGS`, `MXCSR`).
- Segment bases like `FS`/`GS` for TLS.

Each location has:

- A **name** (string, e.g., `"RAX"` or `"[SP+16]"`).
- A **kind** (e.g., `"gpr"`, `"simd"`, `"stack"`, `"flag"`, `"segment"`, `"fpu"`, `"shadow_stack"`).
- An optional **bit subrange** (e.g., lower 128 bits of `YMM6`), which we use for SIMD partial clobbering.

### Location effect categories

Based on behavior at the call boundary (entry and exit), every location can be classified with respect to the block by 2 either-or semantic properties:

- Entry dependence: `Dep` ("Block's behavior or outputs depend on the initial value.") vs. `Indep` (independent of entry).
- Output effect: `Preserved` ("Exit value = entry value.") vs. `Clobbered` ("Final value at end of block changes—there is a possibility where it is not preserved.")

This leads to four possibilities:

| Abbrev | Short name | Dependency | Effect | Intuition |
|---|---|---|---|---|
| IP | **`IndepPreserved`** | Entry-independent | Value-preserving | Value passes through untouched, block doesn't care (similar to no-op or callee-saved) |
| DP | **`DepPreserved`** | Entry-dependent | Value-preserving | Block uses the value but hands it back intact (read-only location) |
| IC | **`IndepClobbered`** | Entry-independent | Clobbering | Block writes something new without reading old value (caller-saved / traditional clobber) |
| DC | **`DepClobbered`** | Entry-dependent | Clobbering | Block reads and then overwrites (in-place location use) |

The **LocationEffects** object is just a list of `(Location, LocationEffect)` entries. For SIMD, you can have multiple entries for the same architectural register with disjoint `BitRange`s (e.g., lower 128 bits inert, upper 128 bits clobbered).

***

## Arguments and returns as ordered views

The location effects tell us what *matters* for behavior, but for a JIT or FFI layer you also need a **stable ordering** and a **logical mapping** for argument and return value data to location values.

We therefore define four **slot** types, each a refinement of the location-effect classification:

- **Argument slots**: logical positions `arg0`, `arg1`, …, each mapped to one or more locations (for multi-register values, split structs, or hidden arguments like struct-return pointers). These are always entry-dependent: the block's behavior varies with their values.
- **Convention slots**: locations whose entry values are determined by structural constraints of the calling convention rather than by the caller's logical data — for example, the stack pointer, segment bases, the SysV variadic `AL` count, or the direction flag. These are also entry-dependent, but their values are fixed by call shape, not by user-supplied arguments.
- **Return slots**: logical outputs `ret0`, `ret1`, …, each mapped to one or more locations (e.g., an integer return in `RAX`, a vector return in `XMM0`, or a pair for complex values). These are always clobbering: the block writes a result the caller is expected to consume.
- **Scratch slots**: locations that are clobbered by the block but whose output is not consumed by the caller. These correspond to what ABI documentation typically calls caller-saved or scratch registers.

The slot classification is a refinement of location effects in one direction: only entry-dependent locations appear as argument or convention slots, and only clobbering locations appear as return or scratch slots. The refinement adds **ordering** and **marshalling role** on top of the bare effect characterization — two things that location effects alone do not determine.

For a trampoline, the ordered view is what you use when marshalling and unmarshalling. To call the block, walk the argument slots in order and fill each mapped location with the corresponding logical value; then set each convention slot to its structurally required value. Jump to the block. On return, walk the return slots in order and read each mapped location into the corresponding logical output. Scratch slots require no action in either direction.

***

## Stack behavior and frame invariants

### Identifying the stack pointer

The stack pointer can be `SP`, `RSP`, or `ESP`, it is typically not hard to identify which one is actually in use, but it is additional metadata.

### Stack delta

The stack pointer is typically treated as dep_clobbered because the callee uses it to locate scratch space and frame slots. We can refine this analysis because it often satisfies the additional invariant \(SP_{out} = SP_{in} + \Delta\) for some constant \(\Delta\). For example, most standard conventions have an effectively constant delta when viewed at the call boundary:

- SysV AMD64: callee adjusts `RSP` in prologue/epilogue but returns with the same value the caller had at the call site (caller cleans up parameters pushed on stack). [cs61.seas.harvard](https://cs61.seas.harvard.edu/site/2025/pdf/x86-64-abi-20210928.pdf)
- Windows x64: caller must reserve 32‑byte shadow space above the return address and maintain 16‑byte alignment; callee can assume that space exists for spills, but also returns with `RSP` restored to the original (minus any varargs stack params that caller owns). [blog.s-schoener](https://blog.s-schoener.com/2025-01-17-access-violation-aligned-access/)


And within call boundaries, basic blocks typically have predictable constant-offset stack behavior to allow restoring the stack to satisfy these calling requirements. But the stack delta being data‑dependent or path‑dependent or otherwise variable is also a (rare) possibility.

### Stack region policy (red zone, shadow space)

The stack is semantically partitioned relative to entry `SP_in` (or CFA):

- **Inert region above SP_in** (higher addresses on downward‑growing stacks): caller’s locals and saved state. Callee must not modify this; if symbolic analysis shows writes here, that’s either a bug (overflow) or intentional exploitation. [cs61.seas.harvard](https://cs61.seas.harvard.edu/site/2025/pdf/x86-64-abi-20210928.pdf)
- **Volatile region below SP_in**: callee’s scratch and frame; caller cannot make assumptions about contents after return. [sra.uni-hannover](https://www.sra.uni-hannover.de/Lehre/SS25/V_BSB/doc/x86-abi.html)

Real ABIs refine this with:

- **Red zone** (SysV AMD64): for Linux and many Unix‑like ABIs, there is a 128‑byte red zone immediately below `RSP` that is guaranteed *not* to be clobbered by interrupts or signal handlers, so leaf functions can use it without doing a formal stack allocation. [hackeradam](https://hackeradam.com/x86-64-calling-conventions/)
- **Shadow space** (Windows x64): the caller must allocate 32 bytes above the return address before calling; the callee can treat this as owned scratch space for spilling the first four parameter registers or saving state. [github](https://github.com/simon-whitehead/assembly-fun/blob/master/windows-x64/README.md)

We capture these in a **StackRegionPolicy**:

- `has_red_zone`, `red_zone_size`.
- `has_shadow_space`, `shadow_space_size`.

Your JIT/trampoline layer can then adjust how it allocates frames and where it stores temporary spill slots based on this policy.

### Canonical Frame Address and unwinding

For C++ exceptions, stack traces, and debuggers, there is an **unwind contract** expressed via DWARF `.eh_frame` or platform‑specific equivalents. Conceptually, this revolves around the **Canonical Frame Address (CFA)**: typically the caller’s stack pointer at the call site. [refspecs.linuxbase](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)

The contract says:

- At every instruction in the callee, there must be a computable expression that reconstructs the CFA from the current machine state.
- Given the CFA, there must be expressions that reconstruct the caller’s preserved registers (those classified as Inert for the boundary) and the return address. [refspecs.linuxbase](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)

We capture this as a **CFAInvariant**:

- `cfa_register`: e.g., the caller’s SP at call site.
- `expression`: an opaque DWARF‑like expression or AST that can be used by your tooling (if you want to integrate with actual unwind tables).

If you generate trampolines that change the stack pointer in ways not reflected in the unwind tables, you violate this contract and unwinding will break.

### Tail calls and frame transfer

True **Tail Call Optimization (TCO)** or tail call elimination is effectively a controlled violation of stack and return discipline:

- Function A’s frame may be partially or fully overwritten by function B’s.
- The original caller’s return address is now “owned” by B.
- The final stack depth may reflect B’s needs, not A’s original layout.
- The function may never return to A.

TCO can’t be expressed cleanly as the constant-delta “SP_out = SP_in + const”. Instead, we mark it explicitly as using TCO, i.e. “ownership moves to some other block”.


***

## Relational and non‑local predicates

### Variadic SysV AL rule

In the System V AMD64 ABI, when calling a variadic function (like `printf`), the low byte of `RAX` (`AL`) must be set to the number of floating‑point arguments passed in vector registers `XMM0`–`XMM7` (or an upper bound, depending on ABI revision). [stackoverflow](https://stackoverflow.com/questions/58830670/vararg-x86-64-abi-number-of-floating-point-parameters-in-registers)

This is not “AL is live” but specifically:

- \( AL_{in} = \text{count}(\text{live FP args in XMM0–XMM7}) \).

We model this as a **RelationalPredicate** with a name (e.g., `"sysv_variadic_al"`) and an opaque expression describing that constraint.

### Hidden struct‑return pointer

For large struct returns, both Windows and SysV use a hidden pointer argument:

- Caller allocates space for the struct.
- Caller passes a pointer to that space as a hidden first argument (often bumping other arguments down one register).
- Callee writes the struct into that space and returns the same pointer in `RAX` (or equivalent register). [sra.uni-hannover](https://www.sra.uni-hannover.de/Lehre/SS25/V_BSB/doc/x86-abi.html)

This is an **aliasing postcondition**:

- \( RAX_{out} = H_{in} \), where \(H_{in}\) is the hidden argument location.
- Memory at \(H_{in}\) transitions from “uninitialized clobberable” to “live output struct”.

We encode that again as a relational predicate with an expression referencing the relevant locations.

### Other relational constraints

- **Partial register liveness** (SIMD subregisters): by using `BitRange`s and separate Location entries.
- **Stack depth constraints** for x87 FPU: “stack depth at exit = depth at entry + 1, and no overflow across execution”. [sra.uni-hannover](https://www.sra.uni-hannover.de/Lehre/SS25/V_BSB/doc/x86-abi.html)
- **Frame transfer (TCO)**: “this block transfers return responsibility to a different callee and may overwrite our own frame in the process.”

***

## Hidden and architectural invariants

### Direction Flag and MXCSR

Most x86/x64 ABIs require:

- Direction Flag (`DF`) must be clear (0) on function entry and must be clear on exit. Functions that temporarily set it must restore it before returning.
- Floating‑point control registers (like `MXCSR`) must be preserved across calls.

We treat these as:

- **Preserved locations** (`DF_in == DF_out`, `MXCSR_in == MXCSR_out`).
- **Convention-set inputs** like `DF_in = 0`.

### Segment bases (`FS` / `GS`)

On modern 64‑bit OSes, `FS`/`GS` are used for TLS and the thread environment block / TCB; these are how code discovers the current thread, stack canaries, errno, etc. [docs.kernel](https://docs.kernel.org/arch/x86/shstk.html)

We model them as:

- Locations with kind `"segment"`.
- Classified as **Preserved** for essentially all normal code.
- **Convention-set inputs**: re-use the existing valid OS‑managed structures.

### Hardware shadow stack (CET) and IBT

With Intel CET, there are two major constraints: [cubeyond](https://www.cubeyond.net/posts/pwn-notes/intel-cet-bypass/)

- **Shadow stack (SHSTK)**: every `CALL` pushes the return address on both the normal stack and the hidden shadow stack; every `RET` pops and compares. If mismatched, the CPU raises a control‑flow protection fault. You can no longer synthesize return addresses by doing a “push address; ret” sequence.
- **Indirect Branch Tracking (IBT)**: indirect `CALL`/`JMP` transitions the CPU into a state where the *very next* instruction must be an `ENDBRANCH` (`ENDBR32`/`ENDBR64`); otherwise, a fault is raised. [edc.intel](https://edc.intel.com/content/www/us/en/design/ipla/software-development-platforms/client/platforms/alder-lake-desktop/12th-generation-intel-core-processors-datasheet-volume-1-of-2/006/indirect-branch-tracking/)

These constraints don’t change liveness per se but restrict which control‑flow patterns are legal implementations of the contract.

### Hardware stacks (x87 FPU stack)

The x87 unit has its own 8‑deep hardware stack of `ST0`–`ST7`. Old ABIs like 32‑bit `cdecl` used `ST0` as the return value location for floats/doubles. [sra.uni-hannover](https://www.sra.uni-hannover.de/Lehre/SS25/V_BSB/doc/x86-abi.html)

We model this with a **HardwareStackInvariant**:

- `stack_location`: conceptual “x87 stack”.
- `depth_delta`: typically `+1` if the callee leaves one result on the stack.
- `must_not_overflow`: enforces that the callee does not leave extra junk that would overflow later.

This is orthogonal to the main memory stack and coexists with the SP delta model.

***

## Heap usage summary

The heap is very difficult to reason about precisely, especially under aliasing, but for the calling‑convention contract we usually only need a coarse summary:

- Which argument locations are expected to contain **pointer arguments** whose pointees are live inputs (i.e., the callee will dereference them). [medium](https://medium.com/@ayyouboussamas/deep-dive-into-variadic-arguments-in-c-an-x86-64-system-v-abi-analysis-a2ee0ba64834)
- Whether the callee **may allocate or free** heap memory (affecting how you reason about side effects but not the core calling contract).

We capture this as a **HeapUsageSummary** with:

- `pointer_args`: list of locations expected to hold pointer inputs.
- `may_allocate`: boolean.

For a trampoline, this may not matter much, but for sandboxing or static verification it can be useful metadata.

***

## Integrating liveness with practical trampolines

Given this structured contract, a trampoline that wants to call an arbitrary machine‑code block can proceed as follows (conceptually):

1. **Compute or load the CallingConvention object** for the target block (via prior analysis).
2. **Marshal arguments**:
   - Walk the `ArgumentOrdering` in order.
   - For each arg, write its value into the associated locations (registers, stack slots) in the real machine state.
3. **Establish invariants**:
   - Ensure the stack satisfies `StackRegionPolicy` (alignment, red zone/shadow space expectations). [stackoverflow](https://stackoverflow.com/questions/30190132/what-is-the-shadow-space-in-x64-assembly)
   - Ensure hidden state invariants: clear `DF`, set up `FS`/`GS` bases, etc. [docs.kernel](https://docs.kernel.org/arch/x86/shstk.html)
   - Respect `HardwareControlFlowConstraint` (use real `CALL`, not push+ret; indirect calls only to `ENDBRANCH`‑marked code if required). [brain.tamnd](https://brain.tamnd.com/research/memory-management/hardware/07_intel_cet_shadowstack/)
4. **Perform the call** in a way compatible with CET/IBT and the unwind/CFA model. If the call is TCO and doesn't return, we can stop here.
5. **Unmarshal outputs**:
   - Read return slots in `ReturnOrdering` from their locations.
   - Update your logical state based on `Clobbered` vs `Inert` sets as needed.
6. **Restore or propagate state**:
   - For locations classified as Inert, assert that they have their original values; if not, you’ve detected a contract violation.
   - For LiveInput locations that are also Clobbered (e.g., caller‑saved regs), restore them from your own saved copy if your language requires that.

This is exactly the usage pattern your Python structures are meant to support.
