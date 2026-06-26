# VEX IR: A Prose Reference

VEX IR is the architecture-neutral intermediate representation at the heart of the VEX library, which underpins Valgrind's dynamic binary instrumentation framework. Rather than resembling assembly language (a flat list of machine instructions), VEX IR is designed more like a compiler's internal representation: typed, structured, and amenable to analysis and transformation.

## Design Philosophy

The IR carries two key formal invariants that it enforces mechanically. First, it is **fully typed**: every value in the IR has a statically known type, and VEX will reject and abort on any block that fails a typecheck. Second, it obeys **SSA (Static Single Assignment)**: every temporary may be assigned to exactly once. Both properties are checked together at typecheck time. These constraints make the IR easier to reason about, facilitate optimization passes, and make it much simpler to build a reliable JIT compiler.

Memory management is arena-based: every allocation within a translation session is reclaimed automatically once translation is complete, so there are no explicit deallocation calls for IR data structures.

## Code Blocks (IRSBs)

The top-level unit of IR is the **superblock** (`IRSB`), which typically represents one to fifty machine instructions. An IRSB has three components:

- A **type environment** (`IRTypeEnv`), which maps each temporary number to its `IRType`.
- A **statement list**, the sequence of side-effecting operations.
- A **final exit**, an unconditional jump that terminates the block, expressed as an `IRExpr` destination and an `IRJumpKind`.

IRSBs are single-entry but multiple-exit: conditional `Exit` statements within the body can transfer control out of the block before reaching the final jump. Because of this, a single IRSB can cover up to three non-consecutive ranges of guest instruction addresses.

At the time instrumentation tools see IR, the code is guaranteed to be in **flattened form**: all sub-expressions of statements are "atoms," meaning either constants or temporaries. For example, `Add32(GET:I32(0), GET:I32(12))` in unflattened form becomes three separate temporary-assignment statements in flattened form. Flattening makes instrumentation substantially simpler, and equivalent flattened and unflattened IR produce the same generated code.

## Types

Every IR value carries an `IRType`, which encodes both the bit-width and the interpretation of a value. The available types are:

- **Integer**: `I1` (a single bit, used for boolean conditions), `I8`, `I16`, `I32`, `I64`, `I128`.
- **Binary floating-point**: `F16`, `F32` (IEEE 754 single), `F64` (IEEE 754 double), `F128` (implementation-defined 128-bit).
- **Decimal floating-point**: `D32`, `D64`, `D128`.
- **SIMD vectors**: `V128` (128-bit), `V256` (256-bit).

The `I1` type is important: comparisons and condition codes produce `I1` values, which control conditional exits. The vector types are opaque bit-vectors at the IR level; their lane structure is determined by the primitive operation applied to them.

## The Guest State

VEX models the simulated machine's registers as a flat byte array called the **guest state**. Registers have no special names in IR — they are identified purely by byte offset into this array. To use a register, one **Get**s it into a temporary; to write it back, one **Put**s a value at the appropriate offset. This uniformity means any architecture's register file can be described without adding new IR constructs.

For architectures with **rotating or indexed register files** (x87's FPU stack, SPARC register windows, Itanium's rotating file), a designated region of the guest state is described as an `IRRegArray`, specifying the base offset, element type, and element count. The `GetI` expression and `PutI` statement index into this region at runtime using a sum of a variable index and a constant bias, computed modulo the array size. This circular indexing scheme is chosen specifically to make alias analysis tractable for the optimizer.

## Expressions (IRExpr)

Expressions are **pure**: they have no side effects and may be freely hoisted, duplicated, or eliminated. The expression variants are:

- **`Get`**: reads a fixed-offset guest register into the IR.
- **`GetI`**: reads from an indexed (`IRRegArray`) guest register region.
- **`RdTmp`**: reads a previously assigned temporary.
- **`Const`**: a literal constant value (`IRConst`).
- **`Unop`, `Binop`, `Triop`, `Qop`**: primitive operations of arity 1, 2, 3, and 4 respectively, applied to sub-expressions.
- **`Load`**: loads a value from memory with a specified endianness and type. Note that load-linked is *not* an expression because it has side effects (placing a hardware reservation); it is instead a statement.
- **`CCall`**: calls a pure C helper function. The callee must be truly pure — it may not read or write guest state or guest memory, because doing so would hide those accesses from instrumentation tools. The return type must not be V128 or V256 (those require the dirty-call mechanism instead).
- **`ITE`**: a strict if-then-else. Both branches are always evaluated; the condition selects which value is returned. This is analogous to a ternary expression, not to a conditional branch.
- **`VECRET`** and **`GSPTR`**: special pseudo-expressions that can appear only in dirty helper argument lists. `VECRET` passes a pointer to a region where the helper will write a V128/V256 result; `GSPTR` passes a pointer to the thread's guest state area.

## Constants (IRConst)

Constants are typed literals that appear within `Const` expressions and `Exit` statements. Integer constants cover `U1` through `U128`. Floating-point constants exist in both "native" form (e.g., `F64` as a C `double`) and "raw bitpattern" form (e.g., `F64i` as a `ULong` to be reinterpreted), which allows exact representation of special values like NaN or infinity. Vector constants (`V128`, `V256`) are restricted: they store one bit per byte-lane (repeated 8 times), so they can only represent all-zero or all-one lane patterns, not arbitrary vectors.

## Primitive Operations (IROp)

The `IROp` enumeration is the IR's vocabulary of scalar and vector computations. Operations are named systematically: the base name describes the operation, a numeric suffix gives the bit-width, and optional prefixes or suffixes qualify signedness (`S`/`U`), saturation (`Q`), or rounding mode.

**Scalar integer** operations cover the full set of arithmetic, bitwise logic, shifts, comparisons, and division for 8-, 16-, 32-, 64-, and 128-bit widths. The shift semantics follow the C standard and are undefined for out-of-range shift amounts. Several comparison operations carry Memcheck-specific hints: `CasCmpEQ` variants signal that a comparison is part of a CAS success check, and `ExpCmpNE` variants request expensive definedness tracking in Valgrind's Memcheck tool. Division operations come in several flavors including combined quotient-and-remainder forms (where the output is a wider integer whose low half holds the quotient and high half the remainder), and "extended dividend" forms where the dividend is implicitly a zero-extended wider value.

**Integer conversion** operations handle widening (zero-extending `Uto` and sign-extending `Sto`), narrowing (taking the low or high half of a value), and bit-combining (assembling two halves into a wider value with `HLto`). A complete set is provided even for combinations that are technically redundant, because doing so reduces IR size and simplifies instruction selection.

**Floating-point** operations strive for IEEE 754 compliance. Most rounding binary operations take an explicit rounding mode as their first argument, encoded as an `I32` using the Intel convention (00b = nearest, 01b = toward −∞, 10b = toward +∞, 11b = toward zero). Frontend translators for non-Intel architectures (PPC, ARM VFP) must convert their native encoding to this standard before generating IR. Conversions between float and integer types are explicit and always carry a rounding mode argument; the deprecated `_DEP` suffix marks older operations that omit the rounding mode and are therefore underspecified. Beyond the IEEE mandated set, the IR also includes x86-specific transcendentals (`SinF64`, `CosF64`, `TanF64`, `AtanF64`, `Yl2xF64`, etc.), PowerPC fused multiply-add variants, and ARM64 reciprocal-estimate operations — each labelled with which guest architecture they exist to serve.

**Decimal floating-point** (DFP) operations support IBM POWER's decimal types D32, D64, and D128, including arithmetic, rounding, quantize, significand-shift, exponent extraction/insertion, BCD-to-DPB conversion, and full cross-conversion with binary floating-point and integer types.

**SIMD** operations are organized by total register width (32, 64, 128, or 256 bits) and then by lane configuration. The operation name encodes both: for example, `Add16x8` adds sixteen 8-bit lanes packed in a 128-bit register (interpreting the suffix as `<lane-type><lane-count>`), while `Add16Fx8` does the same with 16-bit float lanes. The full repertoire includes arithmetic, saturating arithmetic, averaging, min/max, lane-wise comparison (producing all-ones or all-zeros masks), shifts (both vector×vector and vector×scalar), widening and narrowing, pairwise reduction, interleaving, permutation, lane extraction and insertion, duplicate (broadcast), byte-reversal, and polynomial multiplication for GF(2) arithmetic. Reciprocal-estimate and reciprocal-square-root-estimate operations (and their Newton–Raphson "step" refinements) are included to model ARM and MIPS approximate-computation instructions.

## Statements (IRStmt)

Statements are the side-effecting operations that constitute the body of an IRSB. Two kinds are **meta**: they convey information about the original machine code but do not themselves execute.

- **`IMark`** marks the start of a new guest instruction, recording its address, byte length, and a delta value. The delta is subtracted from a guest PC value before comparing to the instruction address; on Thumb, this compensates for the interworking address encoding where the LSB is set.
- **`AbiHint`** carries a platform ABI annotation: specifically, that the address range `[base, base+len)` has become undefined (used for stack-redzoning on amd64/PPC) and that the next instruction address is `nia` (used by Memcheck for origin tracking).

The operational statements are:

- **`WrTmp`**: assigns an expression to a temporary. SSA requires each temporary to appear on the left-hand side exactly once.
- **`Put`**: writes a value to a fixed-offset guest register.
- **`PutI`**: writes to an indexed `IRRegArray` guest register region.
- **`Store`**: unconditional memory write with endianness.
- **`StoreG`**: guarded (conditional) memory write. The address and data expressions are evaluated regardless of the guard; only the actual store is conditional.
- **`LoadG`**: guarded memory load with an inline conversion (e.g., zero-extend an 8-bit load to 32 bits). Both the address and the alternate value are always evaluated; the conversion (`ILGop`) is applied to the loaded value if the guard is true, and the alternate is used otherwise.
- **`CAS`**: an atomic compare-and-swap. Supports both single-element and double-element (DCAS) forms. In the double-element case, the two elements are adjacent in memory and their order depends on endianness. After the operation, the original value at the address is written to the `old` temporaries regardless of success or failure; success is determined by comparing `old` with `expd`.
- **`LLSC`**: load-linked or store-conditional, distinguished by whether `storedata` is null (LL) or non-null (SC). For SC, the result temporary receives `I1`: 1 on success, 0 on failure. The address must be naturally aligned.
- **`Dirty`**: a call to an impure C helper function. Unlike `CCall`, a dirty helper may read/write guest state and memory, and may have internal state. The caller must annotate exactly which guest state regions (via `fxState`) and which memory region (via `mFx`, `mAddr`, `mSize`) the helper accesses. These annotations are required for correct instrumentation. The helper arguments are evaluated unconditionally; the guard expression is evaluated *after* the arguments and memory-effects expression. If the helper returns a V128 or V256, the return convention uses `IRExpr_VECRET()` as a pointer argument rather than a normal return value.
- **`MBE`** (Memory Bus Event): encodes a memory fence (`Imbe_Fence`) or, on ARM, a load-linked reservation cancellation (`Imbe_CancelReservation`).
- **`Exit`**: a conditional branch out of the IRSB. It carries a guard expression (`I1`), a destination constant, an `IRJumpKind` hint, and the guest PC offset to update. If the guard is true, the IRSB exits to the specified address.

## Jump Kinds (IRJumpKind)

Every control-flow transfer in VEX IR — both the final exit and any `Exit` statements — carries an `IRJumpKind` hint. This allows the VEX dispatcher to handle control flow correctly beyond just jumping to an address:

- **`Ijk_Boring`**: ordinary computed goto.
- **`Ijk_Call` / `Ijk_Ret`**: guest call or return; used for code-coverage or profiling.
- **`Ijk_Sys_*`**: system calls via various mechanisms (`syscall`, `int 0x80`, `sysenter`, `svc`, etc.).
- **`Ijk_InvalICache` / `Ijk_FlushDCache`**: cache management; the range to operate on is stored in the pseudo-registers `guest_CMSTART` and `guest_CMLEN` in the guest state before the jump.
- **`Ijk_EmWarn` / `Ijk_EmFail`**: emulation warnings or fatal errors; the reason is stored in `guest_EMNOTE`.
- **`Ijk_SigILL`, `Ijk_SigTRAP`, `Ijk_SigSEGV`, `Ijk_SigBUS`, `Ijk_SigFPE_*`**: the current instruction synthesizes a signal.
- **`Ijk_NoDecode`**: the instruction at the current PC could not be decoded.
- **`Ijk_Extension`**: invokes a guest-specific extension mechanism.

## A Worked Example

The x86 instruction `addl %eax, %ebx` translates to the following flattened VEX IR:

```
------ IMark(0x24F275, 7, 0) ------
t3 = GET:I32(0)          # read %eax from guest state offset 0
t2 = GET:I32(12)         # read %ebx from guest state offset 12
t1 = Add32(t3, t2)       # compute the sum
PUT(0) = t1              # write result back to %eax
```

Each sub-expression is an atom (a temporary or constant), there are no cycles, every temporary appears on the left exactly once, and all types are inferrable from the context. The IMark records that the original instruction was 7 bytes at address `0x24F275`.

## Typing and Type Inference

VEX IR is typed via a **type environment** (`IRTypeEnv`), which is a flat array mapping each `IRTemp` integer to its `IRType`. The type of every subexpression is derivable purely from this environment plus the operation's fixed type signature. For `Get`, the type is explicit in the instruction. For `Binop`/`Unop`/etc., the type of the result is determined by which `IROp` enum value is used — every `IROp` has exactly one type signature, there is no overloading. So in your IR, you don't need a separate type environment because your graph edges already carry type information directly at the source node. The `IROp` vocabulary effectively acts as the typing oracle.

The naming uses `I8`, `I16`, `I32`, `I64`, `I128` exclusively for all `IRType` values — these are signless. The `U8`, `U16`, etc. names appear **only** in `IRConst` tags (`Ico_U8`, `Ico_U32`, etc.), which encode the constant's bit-width. This is a naming inconsistency in the C implementation, not a semantic distinction. The canonical type names are `I8`, `I16`, `I32`, `I64`. Signedness is **not** a type property; it is a property of individual operations (e.g., `Iop_DivS32` vs. `Iop_DivU32`).

## F32i and Constant Representation

The `Ico_F32i` / `Ico_F64i` constant variants are a **purely pragmatic host-language artifact**: C doesn't guarantee that a `UInt` bit-pattern is preserved when passed through a `float`, so VEX keeps two ways to write floating-point constants. In your IR, this distinction disappears entirely. You already have `F32` as a first-class type, so a float constant is just `Const(F32, <bits>)`. There is no need for a separate `F32i` type — you simply need your constant representation to store the bits faithfully, which Python's `struct.pack/unpack` handles. The right encoding is: `Const(type=F32, value=<int bitpattern>)` where the bits are interpreted as IEEE754. This is not a new type; it is just a constant of type `F32`.

## Guest State Type

The **guest state** is not assigned any IR type in VEX — it is a raw C `void*` buffer, and `Get`/`Put` are effectively pointer arithmetic into it. In your IR, you are right to give it a type. The natural encoding is:

```
GuestState = Struct(fields=[
    (offset=0,  type=I32,  name="eax"),
    (offset=12, type=I32,  name="ebx"),
    ...
])
```

But the `IRRegArray` construct (for rotating files) suggests a sub-region that is an array: `RegArray(base_offset, elem_type, n_elems)`. So the full guest state type is essentially a **heterogeneous record** with some sub-regions being homogeneous arrays.

For `GetI`, the full signature is:
```
GetI(descr: IRRegArray, ix: I32, bias: Int) -> descr.elemTy
```
The `ix + bias` sum is taken modulo `descr.nElems`, then multiplied by `sizeof(descr.elemTy)`, then added to `descr.base` to get the byte offset. In your IR, `GetI(gs: GuestState, descr: RegArray, ix: I32) -> T` and `PutI(gs: GuestState, descr: RegArray, ix: I32, val: T) -> GuestState`. The guest state value flows as a pure data argument; the optimizer then decides whether to make the update in-place.

## Operations: Where the Types Really Live

You are correct that the types are **not** in this file in a machine-readable way. The `IROp` enum is just a numbered list. The type information is encoded **only in the comments** (e.g., `/* :: IRRoundingMode(I32) x F64 x F64 -> F64 */`) and enforced at runtime by VEX's typecheck pass (in `typeOfPrimop()` in `ir_opt.c`/`guest_generic_bb_to_IR.c`). This means to build your typed `FlatOperation` table, you will need to **manually parse or transcribe** the comment signatures. The header is the authoritative source; there is no separate machine-readable type file.

Your proposed representation is exactly right. For a uniform encoding:

```python
@dataclass
class Op:
    name: str
    args: tuple[IRType, ...]
    result: IRType

# examples:
add_i32     = Op("add_i32",    (I32, I32),         I32)
add_f64     = Op("add_f64",    (RMode, F64, F64),   F64)  # RMode is just I32 with semantic tag
madd_f64    = Op("madd_f64",   (RMode, F64, F64, F64), F64)
add_i32x4   = Op("add_i32x4",  (V128, V128),        V128)  # lane structure is in the name only
```

Note that `IRRoundingMode` is simply `I32` — it has no distinct IR type. You may want to introduce `RoundingMode` as a semantic subtype of `I32` in your IR for clarity, even if the underlying representation is the same.

## Decimal Floating Point (D32/D64/D128)

Yes, `D32`, `D64`, and `D128` are **first-class `IRType` values** in VEX, just like `F32` and `F64`. They are distinct types, not aliases for integers. All the DFP arithmetic, conversion, quantize, significance-round, BCD, and exponent-manipulation operations take and return these types explicitly. So you need `D32`, `D64`, `D128` as IR types in your system, parallel to `F32`, `F64`, `F128`.

## SIMD: Formal Type vs. Lane Configuration

This is a key point. VEX has exactly **two SIMD types**: `V128` and `V256`. The lane configuration is encoded **purely in the operation name** — `Add16x8` means "add sixteen 8-bit lanes in a V128", but the type of the operands and result is just `V128`. Your IR must decide: do you parameterize the type (e.g., `Vector(I8, 16)`), or keep opaque `V128`/`V256` with lane structure in the op?

The VEX approach — opaque `V128`/`V256` — means that `Add8x16` and `Add16x8` are different operations both of type `(V128, V128) -> V128`. This is workable and simple. A richer alternative is `Vector(elem_type, count)` as the formal type, making `Add` a generic operation over any vector type. Both are defensible; VEX's flatter approach avoids parametric polymorphism at the cost of a very long op list.

## CCall vs. Binop/Unop

The distinction is simple: `Binop`/`Unop` operations map to **inline code generation** — the backend emits a few host instructions directly. `CCall` is a **function call** at the IR level: VEX generates an actual call instruction to the helper. The helper must be side-effect-free, but it may contain complex logic (e.g., x87 stack operations). In your IR, the right encoding is indeed to give each `CCall` a unique opcode name — essentially treating CCall as a way to extend the `IROp` vocabulary with named function-backed operations. The type signature is the same: `name, args -> result`.

## Statement Classification

Your taxonomy is clean. Here is the refined version:

**Pure (return new value of their input type):**
- `Get(gs, offset) -> T` — reads from `GuestState`
- `GetI(gs, descr, ix) -> T` — indexed read from `GuestState`
- `Put(gs, offset, val) -> GuestState` — returns updated guest state
- `PutI(gs, descr, ix, val) -> GuestState` — indexed write
- `Load(mem, addr, endianness) -> T` — reads from `Memory`
- `Store(mem, addr, val, endianness) -> Memory` — returns updated memory
- `LoadG(mem, addr, alt, guard, cvt) -> T` — conditional load with conversion
- `StoreG(mem, addr, val, guard, endianness) -> Memory` — conditional store

**Side-effecting (nodes in your Program sum type):**
- `IMark(addr, len, delta)` — bookkeeping, no semantic effect but required by instrumentation tools
- `AbiHint(base, len, nia)` — ABI annotation; could be folded into `IMark` conceptually, though it serves a distinct role (stack redzone notification + Memcheck origin hint)
- `CAS(addr, expected, new, mem) -> (old_val, Memory)` — atomic; returns both the old value seen and an updated memory
- `LLSC` — either returns `(value, reservation)` for LL or `(I1_success, Memory)` for SC
- `Dirty(callee, guard, args)` — opaque side effects on memory and guest state, with explicit effect annotations
- `MBE(Fence | CancelReservation)` — memory ordering primitive
- `Exit(guard, dst, jumpkind)` — control flow; in your continuation-passing encoding this is a node with no success continuation (or with a fallthrough continuation for the guard-false case)

The key insight you've identified is correct: **nearly everything is pure**. The side-effecting set is small: `IMark`, `AbiHint`, `CAS`, `LLSC`, `Dirty`, `MBE`, and `Exit`. Memory and guest state are just values passed through a chain of pure functional updates. `WrTmp` disappears entirely in your graph representation. `Store`/`Load` become pure functions over a `Memory` type.

## AbiHint vs. IMark

They are semantically separate. `IMark` marks instruction boundaries and is used for things like single-stepping, coverage, and origin tracking. `AbiHint` carries a distinct piece of information: that a stack range is now undefined (for redzone tools) and what the next PC is (for Memcheck's origin-tracking propagation). They co-occur but serve different consumers. Folding them would lose the separate semantics for tools that only care about one. Keep them distinct.

## The Operation Vocabulary Going Forward

The practical next step is indeed to generate the full typed op list. The signature information is entirely in the comments in the `IROp` enum — every group of ops in the header has a comment of the form `/* :: T1 x T2 x ... -> Tresult */`. You would parse that file, extract each comment-signature block, and for each `Iop_Foo` in that block, emit:

```python
Op(name="add_f64", args=(RoundingMode, F64, F64), result=F64)
```

Dropping all `_DEP` suffixed ops (they omit rounding modes and are deprecated ) and all ops that exist purely as Memcheck hints (`CasCmpEQ*`, `ExpCmpNE*`) — or keeping them as semantically-equivalent aliases with annotation metadata — is a reasonable cleanup step.