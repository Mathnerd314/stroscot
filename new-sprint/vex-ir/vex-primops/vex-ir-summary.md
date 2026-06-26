- `ITE(Bool, A, A) : A`: a pure if-then-else, like a ternary expression. Both branches have to be pure and evaluate to a value; the condition selects which value is returned.
- `Get(gs : GuestState, offset) -> T` — reads from `GuestState`
- `GetI(gs : GuestState, descr: RegArray, ix: I32, bias: Int) -> T` — indexed read from `GuestState`
- `Put(gs : GuestState, offset, val) -> GuestState` — writes a value to a fixed-offset guest register, returns updated guest state
- `PutI(gs : GuestState, descr: RegArray, ix: I32, val) -> GuestState` — writes to an indexed `IRRegArray` guest register region, returns updated guest state
- `Load(mem : Memory, addr : Ptr, endianness) -> T` — reads from `Memory`
- `Store(mem : Memory, addr : Ptr, val, endianness) -> Memory` — unconditional memory write with endianness, returns updated memory
- `LoadG(mem : Memory, addr : Ptr, alt, guard, cvt) -> T` — guarded memory load with an inline conversion (e.g., zero-extend an 8-bit load to 32 bits). Both the address and the alternate value are always evaluated; the conversion (`ILGop`) is applied to the loaded value if the guard is true, and the alternate is used otherwise. Returns updated memory.
- `StoreG(mem : Memory, addr : Ptr, val, guard, endianness) -> Memory` — guarded (conditional) memory write. The address and data expressions are evaluated regardless of the guard; only the actual store is conditional. Returns updated memory
- `IrOp` - a built-in primitive operation
- `PureCall`: calls a pure helper function. The callee must be truly pure — it may not read or write guest state or guest memory, because doing so would hide those accesses from instrumentation tools.
- **`Const`**: a literal constant value (`IRConst`). The `Ico_F32i` / `Ico_F64i` constant variants are a pragmatic artifact because int-to-float conversion isn't well-defined in C.

**Side-effecting (nodes in your Program sum type):**
- `IMark(addr, len, delta, Program) -> Program` — bookkeeping, no semantic effect but required by instrumentation tools. marks instruction boundaries and is used for things like single-stepping, coverage, and origin tracking.
- `AbiHint(base, len, nia, Program) -> Program` — ABI annotation; marks a stack range is now undefined (for redzone tools) and what the next PC is (for Memcheck's origin-tracking propagation).
- `CAS(addr, expected, new, mem, (old_val, Memory) -> Program) -> Program` — an atomic compare-and-swap. Supports both single-element and double-element (DCAS) forms. In the double-element case, the two elements are adjacent in memory and their order depends on endianness. After the operation, the original value at the address is written to the `old` temporaries regardless of success or failure; success is determined by comparing `old` with `expd`. Returns both the old value seen and an updated memory
- `LL : ((value, reservation) -> Program) -> Program`, `SC : ((I1_success, Memory) -> Program) -> Program`: load-linked or store-conditional, distinguished by whether `storedata` is null (LL) or non-null (SC). For SC, the result temporary receives `I1`: 1 on success, 0 on failure. The address must be naturally aligned.
- `DirtyCall(callee, guard, args, Program) -> Program` — a call to an impure C helper function. Unlike `CCall`, a dirty helper may read/write guest state and memory, and may have internal state. The caller must annotate exactly which guest state regions (via `fxState`) and which memory region (via `mFx`, `mAddr`, `mSize`) the helper accesses. These annotations are required for correct instrumentation. The helper arguments are evaluated unconditionally; the guard expression is evaluated *after* the arguments and memory-effects expression. If the helper returns a V128 or V256, the return convention uses `IRExpr_VECRET()` as a pointer argument rather than a normal return value. Can also do opaque side effects on the world at large. `VECRET` and `GSPTR` are helpers for arguments.
- `MBE(Fence | CancelReservation, Program) -> Program` — Memory Bus Event, encodes a memory fence (`Imbe_Fence`) or, on ARM, a load-linked reservation cancellation (`Imbe_CancelReservation`).
- `Exit(guard, dst, jumpkind : IRJumpKind) -> Program` — a conditional branch out of the IRSB. It carries a guard expression (`I1`), a destination constant, an `IRJumpKind` hint, and the guest PC offset to update. If the guard is true, the IRSB exits to the specified address.

GuestState, Memory - these are flat types. GuestState could be represented as a more complex structure but it's not worth bothing.
RegArray - kind of a flat type but really a tuple of base_offset, elem_type, n_elems
Program - this is a positive jumbo type with complex structure

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
