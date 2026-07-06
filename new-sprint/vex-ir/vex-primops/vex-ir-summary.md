types:
U1, U8, U16, U32, U64, F32, F32i, F64, F64i, V128, V256 - subclasses of IRConst
IrOp - can be reconstructed from get_enum_from_int, op_arg_types of expr.py on IRExpr.op
PyvexArch - metadata for guest state, pretty-prints registers
IRRegArray - base: Int, elemty : IRType, nElems: Int
IRRoundingMode, IRCmpFResult - not exposed in pyvex, erased to I32 semantically in vex
IRExprTag - exposed as pyvex IRExpr classes
IREndness - exposed as string via get_enum_from_int
IRJumpKind - exposed as string via get_enum_from_int
IRCallee - tuple of (regparms: Int, addr: void*, mcx_mask: UInt, name string for pretty printing)
IRLoadGOp - exposed as string via get_enum_from_int
GuestState - a large struct, lots of UInts, UChars, etc with some bit-hacking to get compact representation. varies by architecture.
Memory - a (large) flat array of bytes, per Cliff
Program - this is a positive jumbo type with complex structure

Nodes:

these are all FlatOperations:
- `ITE : Bool, A, A |- A`: a pure if-then-else, like a ternary expression. Both branches have to be pure and evaluate to a value; the condition selects which value is returned.
- `Get : (gs : GuestState, offset: Int) |- T` — reads from `GuestState`
- `GetI : (gs : GuestState, descr: RegArray, ix: I32, bias: Int) |- T` — indexed read from `GuestState`
- `Put : (gs : GuestState, offset : Int, val : T) |- GuestState` — writes a value to a fixed-offset guest register, returns updated guest state
- `PutI : (gs : GuestState, descr: RegArray, ix: I32, bias: Int, data: T) |- GuestState` — writes to an indexed `IRRegArray` guest register region, returns updated guest state
- `Load : (mem : Memory, addr : Ptr, endianness: IREndness) |- T` — reads from `Memory`
- `Store : (mem : Memory, addr : Ptr, val : T, endianness: IREndness) |- Memory` — unconditional memory write with endianness, returns updated memory
- `LoadG : (mem : Memory, addr : Ptr, alt: T, guard: Bool, cvt : LoadGop) |- T` — guarded memory load with an inline conversion (e.g., zero-extend an 8-bit load to 32 bits). Both the address and the alternate value are always evaluated; the conversion (`ILGop`) is applied to the loaded value if the guard is true, and the alternate is used otherwise. Returns updated memory.
- `StoreG |- (mem : Memory, addr : Ptr, val: T, guard: Bool, endianness: IREndness) |- Memory` — guarded (conditional) memory write. The address and data expressions are evaluated regardless of the guard; only the actual store is conditional. Returns updated memory
- `IrOp : (IrOpEnum : Str , [IRExpr]) |- T` - a built-in primitive operation, see vex-ops-description.json
- `PureCall : (IRCallee, args: [IRExpr]) |- retty: A`: calls a pure helper function. The callee must be truly pure — it may not read or write guest state or guest memory, because doing so would hide those accesses from instrumentation tools.
- `Const : |- T` a literal constant value. The `Ico_F32i` / `Ico_F64i` constant variants are a pragmatic artifact because int-to-float conversion isn't well-defined in C.

other than `Const`, these operations will typically be used in combination with `Cut`

**Side-effecting (nodes in your Program sum type):**
- IMark - bookkeeping, no semantic effect but required by instrumentation tools. marks instruction boundaries and is used for things like single-stepping, coverage, and origin tracking. this has a complex structure:
    addr: Addr, len : UInt, delta : UChar, Program
    ----------------------------------------------
        |- Program

    for convenience we will abbreviate this as addr: Addr, len : UInt, delta : UChar, Program -> Program
- `AbiHint(base : Addr, len : Int, nia : Addr, Program) -> Program` — ABI annotation; marks a stack range is now undefined (for redzone tools) and what the next PC is (for Memcheck's origin-tracking propagation).
- `CAS(IRTemp oldHi, IRTemp oldLo, IREndness end, IRExpr* addr, IRExpr* expdHi, IRExpr* expdLo, IRExpr* dataHi, IRExpr* dataLo, Program) -> Program` — an atomic compare-and-swap. Supports both single-element and double-element (DCAS) forms. In the double-element case, the two elements are adjacent in memory and their order depends on endianness. After the operation, the original value at the address is written to the `old` temporaries regardless of success or failure; success is determined by comparing `old` with `expd`. Returns both the old value seen and an updated memory
- `LLSC : addr : Ptr; storedata : Option[A], end : IREndness; ((value, reservation) -> Program) -> Program`, `SC : ((I1_success, Memory) -> Program) -> Program`: load-linked or store-conditional, distinguished by whether `storedata` is null (LL) or non-null (SC). For SC, the result temporary receives `I1`: 1 on success, 0 on failure. The address must be naturally aligned.
- `DirtyCall : IRCallee, guard IRExpr, args [IRExpr], memory effects R/W/M address/size, guest effects list of (R/W/M offset, size, nRepeats, repeatLen) denotes [offset, +size) or for (i = 1; i <= nRepeats; i++) {  [offset + i * repeatLen, +size) },  result (none / IRExpr -> Program) -> Program` — a call to an impure C helper function. Unlike `CCall`, a dirty helper may read/write guest state and memory, and may have internal state. The caller must annotate exactly which guest state regions (via `fxState`) and which memory region (via `mFx`, `mAddr`, `mSize`) the helper accesses. These annotations are required for correct instrumentation. The helper arguments are evaluated unconditionally; the guard expression is evaluated *after* the arguments and memory-effects expression. If the helper returns a V128 or V256, the return convention uses `IRExpr_VECRET()` as a pointer argument rather than a normal return value. Can also do opaque side effects on the world at large. `VECRET` and `GSPTR` are helpers for arguments.
- `MBE(Fence | CancelReservation, Program) -> Program` — Memory Bus Event, encodes a memory fence (`Imbe_Fence`) or, on ARM, a load-linked reservation cancellation (`Imbe_CancelReservation`).
- `Exit(guard : Bool, dst : Const, jumpkind : IRJumpKind, offsIP : Int, Program) -> Program` — a conditional branch out of the IRSB. It carries a guard expression, a destination constant, an `IRJumpKind` hint, and the guest PC offset to update. If the guard is true, the IRSB exits to the specified address. The offsIP is the guest state offset for the instruction pointer. It is conditional hence still has a sub-program element.
- `ExitU(dst : Const, jumpkind : IRJumpKind, offsIP : Int) -> Program` — an unconditional branch out of the IRSB. It carries a destination constant, an `IRJumpKind` hint, and the guest PC offset to update. The IRSB always exits to the specified address. The offsIP is the guest state offset for the instruction pointer. There is no sub-program element, hence this is a terminating instruction.