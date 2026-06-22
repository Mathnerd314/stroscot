"""
trampoline.py  –  Generate and invoke W^X trampolines from a CallingConvention

Requires: iced-x86  (pip install iced-x86)

Public surface
--------------
  build_puts_cc()          -> CallingConvention  (SysV AMD64 puts)
  build_add_cc()           -> CallingConvention  (ADD RAX, RBX inline snippet)
  assemble_trampoline(cc, target_addr, arg_values) -> bytes
  make_executable(code: bytes) -> (base_addr: int, cleanup: callable)
  call_trampoline(cc, target_addr, arg_values) -> int | dict
  hello_world()            -> None  (demo: puts via SysV ABI trampoline)
  demo_add()               -> None  (demo: ADD RAX, RBX with RFLAGS capture)

Quick demo
----------
  python trampoline.py          # runs hello_world() then demo_add()
"""

from __future__ import annotations

import ctypes
import ctypes.util
import mmap
import sys
from typing import Dict, List, Optional, Sequence, Union

import iced_x86
from iced_x86 import (
    BlockEncoder,
    Code,
    Instruction,
    Register,
)

# ── import the calling-convention model ──────────────────────────────────────
from calling_convention_model import (
    CallingConvention,
    CFAInvariant,
    Location,
    LocationEffect,
    SlotKind,
    StackDeltaInvariant,
    StackRegionPolicy,
)

# ── register name → iced Register enum ───────────────────────────────────────
_GPR_MAP: Dict[str, int] = {
    "RAX": Register.RAX, "RBX": Register.RBX, "RCX": Register.RCX,
    "RDX": Register.RDX, "RSI": Register.RSI, "RDI": Register.RDI,
    "RBP": Register.RBP, "RSP": Register.RSP,
    "R8":  Register.R8,  "R9":  Register.R9,  "R10": Register.R10,
    "R11": Register.R11, "R12": Register.R12, "R13": Register.R13,
    "R14": Register.R14, "R15": Register.R15,
    "AL":  Register.AL,
}


def _iced_reg(name: str) -> int:
    n = name.upper()
    if n not in _GPR_MAP:
        raise ValueError(f"Unknown register in trampoline model: {name!r}")
    return _GPR_MAP[n]


# ──────────────────────────────────────────────────────────────────────────────
# CallingConvention builder:  puts  (SysV AMD64)
# ──────────────────────────────────────────────────────────────────────────────

def build_puts_cc() -> CallingConvention:
    """
    Describe the calling convention for  int puts(const char *s)  under
    the System V AMD64 ABI.

    Slots
    -----
    ARGUMENT 0  RDI  – pointer to the NUL-terminated string
    RETURN   0  RAX  – return value (int, zero-extended into 64 bits)

    Convention slots
    ----------------
    AL  – count of XMM args for variadic calls (must be 0 for puts, though
          puts is not variadic – included as a standard SysV convention slot)
    DF  – direction flag must be 0 on entry

    Scratch (caller-saved, clobbered, callee may freely modify)
    ------
    RCX, RDX, RSI, R8, R9, R10, R11

    Callee-saved (preserved across the call by the callee)
    -----------------------------------------
    RBX, RBP, R12–R15

    Stack
    -----
    SysV AMD64: RSP must be 16-byte aligned *before* the CALL instruction
    pushes the 8-byte return address (so (RSP % 16) == 0 at CALL).
    fixed_delta = 0: callee restores SP to its entry value before RET.
    Red zone: 128 bytes below RSP are caller-owned (Linux ABI).
    """
    cc = CallingConvention(name="puts_sysv_amd64")

    # Argument: RDI – const char *s
    rdi = cc.add_location("RDI", "gpr", LocationEffect.DEP_CLOBBERED)
    cc.add_slot(index=0, kind=SlotKind.ARGUMENT, locations=[rdi], name="s")

    # Return value: RAX – int (puts result, or EOF on error)
    rax = cc.add_location("RAX", "gpr", LocationEffect.INDEP_CLOBBERED)
    cc.add_slot(index=0, kind=SlotKind.RETURN, locations=[rax], name="retval")

    # Convention: AL = 0 (XMM arg count for variadic ABI register-save area)
    al = cc.add_location("AL", "gpr", LocationEffect.DEP_PRESERVED)
    cc.add_slot(index=0, kind=SlotKind.CONVENTION, locations=[al], name="variadic_al")

    # Convention: DF = 0 (direction flag cleared on entry)
    df = cc.add_location("DF", "flag", LocationEffect.DEP_PRESERVED)
    cc.add_slot(index=1, kind=SlotKind.CONVENTION, locations=[df], name="direction_flag")

    # Scratch (caller-saved)
    for i, name in enumerate(["RCX", "RDX", "RSI", "R8", "R9", "R10", "R11"]):
        loc = cc.add_location(name, "gpr", LocationEffect.INDEP_CLOBBERED)
        cc.add_slot(index=i, kind=SlotKind.SCRATCH, locations=[loc], name=f"scratch_{name}")

    # Callee-saved (the callee guarantees these are unchanged on return)
    for name in ["RBX", "RBP", "R12", "R13", "R14", "R15"]:
        cc.add_location(name, "gpr", LocationEffect.DEP_PRESERVED)

    # Stack pointer
    rsp = cc.add_location("RSP", "sp", LocationEffect.DEP_CLOBBERED)
    cc.stack_delta = StackDeltaInvariant(sp_location=rsp, fixed_delta=0)
    cc.stack_regions = StackRegionPolicy(has_red_zone=True, red_zone_size=128)

    # CFA (canonical frame address for unwinding)
    cc.cfa = CFAInvariant(cfa_register=rsp)

    return cc


# ──────────────────────────────────────────────────────────────────────────────
# CallingConvention builder:  ADD RAX, RBX  (inline machine snippet)
# ──────────────────────────────────────────────────────────────────────────────

def build_add_cc() -> CallingConvention:
    """
    Describe the "calling convention" for the bare inline snippet:
        ADD RAX, RBX
        RET

    This is not a real SysV function — no stack alignment requirement, no
    variadic AL, no DF obligation, no red zone.  The trampoline passes in
    RAX and RBX directly, calls the stub, then captures both RAX (result)
    and RFLAGS before they are disturbed.

    Locations
    ---------
    RAX   DEP_CLOBBERED  – input operand; clobbered by ADD result
    RBX   DEP_PRESERVED  – input operand; unchanged by ADD RAX, RBX
    RFLAGS  INDEP_CLOBBERED  – written fresh by ADD, no prior value matters

    Slots
    -----
    ARGUMENT 0  RAX  – left operand (addend)
    ARGUMENT 1  RBX  – right operand (addend)
    RETURN   0  RAX  – sum
    RETURN   1  RFLAGS – processor flags after ADD (via R10 shuttle)

    No CONVENTION, no SCRATCH slots.
    No stack_delta / red_zone / CFA needed (inline, not a real call).
    """
    cc = CallingConvention(name="add_rax_rbx_inline")

    # RAX: input and output (DEP_CLOBBERED – read on entry, overwritten by ADD)
    rax = cc.add_location("RAX", "gpr", LocationEffect.DEP_CLOBBERED)
    cc.add_slot(index=0, kind=SlotKind.ARGUMENT, locations=[rax], name="rax")
    cc.add_slot(index=0, kind=SlotKind.RETURN,   locations=[rax], name="retval")

    # RBX: right operand, preserved across ADD RAX, RBX
    rbx = cc.add_location("RBX", "gpr", LocationEffect.DEP_PRESERVED)
    cc.add_slot(index=1, kind=SlotKind.ARGUMENT, locations=[rbx], name="rbx")

    # RFLAGS: output only – ADD writes all arithmetic flags (IC)
    rflags = cc.add_location("RFLAGS", "flags", LocationEffect.INDEP_CLOBBERED)
    cc.add_slot(index=1, kind=SlotKind.RETURN, locations=[rflags], name="rflags")

    return cc


# ──────────────────────────────────────────────────────────────────────────────
# ctypes struct for the two-value return from an inline-snippet trampoline
# ──────────────────────────────────────────────────────────────────────────────

class _AddResult(ctypes.Structure):
    """
    Plain C struct returned by the inline-snippet trampoline:
        struct { uint64_t retval; uint64_t rflags; };

    The trampoline packs these into RDX:RAX (high:low 128 bits) so ctypes
    can read both without any pointer tricks:
      RAX  → retval  (the ADD result)
      RDX  → rflags  (RFLAGS snapshotted immediately after the snippet RET)

    We declare the ctypes return type as this struct; on the System V AMD64
    ABI, a struct of two uint64_t members is returned in RAX + RDX.
    """
    _fields_ = [
        ("retval", ctypes.c_uint64),
        ("rflags", ctypes.c_uint64),
    ]

# ──────────────────────────────────────────────────────────────────────────────
# CallingConvention builder:  sys_exit (inline machine snippet)
# ──────────────────────────────────────────────────────────────────────────────

def build_sys_exit_cc() -> CallingConvention:
    cc = CallingConvention(name="sys_exit_inline")
    rax = cc.add_location("RAX", "gpr", LocationEffect.DEP_CLOBBERED)
    rdi = cc.add_location("RDI", "gpr", LocationEffect.DEP_CLOBBERED)
    cc.frame_transfer.enabled = True
    return cc



# ──────────────────────────────────────────────────────────────────────────────
# Trampoline assembler
# ──────────────────────────────────────────────────────────────────────────────

# Sentinel name used to identify the inline-snippet calling convention path.
_INLINE_CC_NAME = "add_rax_rbx_inline"


def _has_convention(cc: CallingConvention, slot_name: str) -> bool:
    """Return True if *cc* has a CONVENTION slot with the given name."""
    return any(s.name == slot_name for s in cc.slots.conventions())


def assemble_trampoline(
    cc: CallingConvention,
    target_addr: int,
    arg_values: Sequence[int],
    *,
    rip: int = 0,
) -> bytes:
    """
    Generate x86-64 trampoline machine code driven by *cc*.

    Two paths:

    SysV-ABI path  (puts and similar real functions)
    ─────────────────────────────────────────────────
    1. PUSH RBP / MOV RBP, RSP  – standard frame, re-aligns RSP to 16 bytes
    2. MOV each argument register ← arg_values[i]
    3. XOR EAX, EAX  (only if cc has a "variadic_al" convention slot)
    4. CLD           (only if cc has a "direction_flag" convention slot)
    5. MOV R11, target_addr / CALL R11
    6. MOV RSP, RBP / POP RBP / RET
    Returns a plain int via ctypes.CFUNCTYPE(c_int64).

    Inline-snippet path  (bare machine stubs like ADD RAX, RBX; RET)
    ─────────────────────────────────────────────────────────────────
    1. PUSH RBP / MOV RBP, RSP  – same frame prologue (RSP alignment)
    2. MOV each argument register ← arg_values[i]
       (no XOR EAX or CLD — the snippet has no ABI convention slots)
    3. MOV R11, target_addr / CALL R11
       The snippet executes (e.g. ADD RAX, RBX; RET) and returns here.
    4. PUSHFQ               – push RFLAGS onto the stack while they are hot
       MOV RDX, [RSP]       – copy RFLAGS into RDX (the second return reg)
       POPFQ                – restore flags so the epilogue runs clean
    5. MOV RSP, RBP / POP RBP / RET
       RAX = snippet result, RDX = RFLAGS → returned as _AddResult struct.

    Parameters
    ----------
    cc          : CallingConvention describing the target.
    target_addr : Absolute virtual address of the snippet / function to call.
    arg_values  : Positional argument values in index order.
    rip         : BlockEncoder base IP (0 is fine; no RIP-relative instructions).

    Returns
    -------
    bytes  Raw x86-64 machine code.
    """
    I = Instruction
    R = Register
    C = Code

    arg_slots = sorted(cc.slots.arguments(), key=lambda s: s.index)
    if len(arg_values) != len(arg_slots):
        raise ValueError(
            f"Expected {len(arg_slots)} argument(s), got {len(arg_values)}"
        )

    instrs: list[iced_x86.Instruction] = []

    # ── Determine which path we're on ─────────────────────────────────────────
    is_inline = not cc.slots.conventions()  # no CONVENTION slots → inline snippet

    # ── 1. Frame prologue: PUSH RBP / MOV RBP, RSP ───────────────────────────
    instrs.append(I.create_reg(C.PUSH_R64, R.RBP))
    instrs.append(I.create_reg_reg(C.MOV_R64_RM64, R.RBP, R.RSP))

    # ── 2. Load argument registers from Python values ─────────────────────────
    for slot, value in zip(arg_slots, arg_values):
        if len(slot.locations) != 1:
            raise NotImplementedError(
                f"Multi-location slot {slot.name!r} not supported"
            )
        reg = _iced_reg(slot.locations[0].name)
        instrs.append(
            I.create_reg_u64(C.MOV_R64_IMM64, reg, value & 0xFFFF_FFFF_FFFF_FFFF)
        )

    # ── 3. SysV convention setup (only when the CC declares these slots) ──────
    if _has_convention(cc, "variadic_al"):
        # XOR EAX, EAX  →  AL = 0 (no XMM arguments in register-save area)
        instrs.append(I.create_reg_reg(C.XOR_R32_RM32, R.EAX, R.EAX))

    if _has_convention(cc, "direction_flag"):
        # CLD  →  DF = 0 (string operations go forward)
        instrs.append(I.create(C.CLD))

    # ── 4. Load target address into R11 and CALL ──────────────────────────────
    instrs.append(
        I.create_reg_u64(C.MOV_R64_IMM64, R.R11, target_addr & 0xFFFF_FFFF_FFFF_FFFF)
    )
    instrs.append(I.create_reg(C.CALL_RM64, R.R11))

    # ── 5. Capture RFLAGS for inline snippet (before any other instruction) ───
    if is_inline and not cc.frame_transfer.enabled:
        # PUSHFQ  → pushes RFLAGS (8 bytes) onto the stack; RSP -= 8
        instrs.append(I.create(C.PUSHFQ))
        # MOV RDX, qword ptr [RSP]  → read the just-pushed RFLAGS into RDX
        instrs.append(
            I.create_reg_mem(
                C.MOV_R64_RM64,
                R.RDX,
                iced_x86.MemoryOperand(R.RSP),
            )
        )
        # POPFQ  → restore RFLAGS, RSP += 8
        instrs.append(I.create(C.POPFQ))

    # ── 6. Frame epilogue: MOV RSP, RBP / POP RBP / RET ──────────────────────
    if not cc.frame_transfer.enabled:
        instrs.append(I.create_reg_reg(C.MOV_R64_RM64, R.RSP, R.RBP))
        instrs.append(I.create_reg(C.POP_R64, R.RBP))
        instrs.append(I.create(C.RETNQ))

    enc = BlockEncoder(64)
    enc.add_many(instrs)
    return enc.encode(rip)


# ──────────────────────────────────────────────────────────────────────────────
# W^X: allocate anonymous RW page, copy code, mprotect to R-X
# ──────────────────────────────────────────────────────────────────────────────

def make_executable(code: bytes):
    """
    Allocate a private anonymous mmap region, write *code* into it, then
    use mprotect(2) to remove write permission and add execute permission
    (W^X discipline: memory is never simultaneously writable and executable).

    Returns
    -------
    (base_addr: int, cleanup: callable)
        base_addr – integer address of the first byte of *code*.
        cleanup   – call this when finished to unmap the pages.
    """
    page_size = mmap.PAGESIZE
    alloc_size = (len(code) + page_size - 1) & ~(page_size - 1)

    buf = mmap.mmap(
        -1, alloc_size,
        mmap.MAP_PRIVATE | mmap.MAP_ANONYMOUS,
        mmap.PROT_READ | mmap.PROT_WRITE,
    )
    buf.write(code)
    buf.flush()

    base_addr = ctypes.addressof(ctypes.c_char.from_buffer(buf))

    libc = ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True)
    PROT_READ = 0x1
    PROT_EXEC = 0x4
    ret = libc.mprotect(ctypes.c_void_p(base_addr), alloc_size, PROT_READ | PROT_EXEC)
    if ret != 0:
        buf.close()
        raise OSError(ctypes.get_errno(), "mprotect failed")

    def cleanup():
        buf.close()

    return base_addr, cleanup


# ──────────────────────────────────────────────────────────────────────────────
# High-level call
# ──────────────────────────────────────────────────────────────────────────────

def call_trampoline(
    cc: CallingConvention,
    target_addr: int,
    arg_values: Union[Sequence[int], Dict[str, int]],
) -> Union[int, dict, None]:
    """
    Full pipeline: assemble → make executable → call via ctypes → free.

    arg_values may be either:
    - a sequence of ints in slot-index order, or
    - a dict mapping slot name → int  (looked up in ARGUMENT slots by name).

    Return value
    ------------
    SysV-ABI path  (cc has CONVENTION slots):
        Returns a plain int (the value in the first RETURN slot, typically RAX).
        Backwards-compatible with the original call_trampoline behaviour.

    Inline-snippet path  (cc has no CONVENTION slots, e.g. build_add_cc()):
        Returns a dict whose keys are the RETURN slot names.
        For build_add_cc() this is {"retval": <int>, "rflags": <int>}.
    """
    # ── Resolve dict-style arg_values to a sorted list ────────────────────────
    if isinstance(arg_values, dict):
        arg_slots = sorted(cc.slots.arguments(), key=lambda s: s.index)
        positional: list[int] = []
        for slot in arg_slots:
            key = slot.name
            if key not in arg_values:
                raise KeyError(f"Missing argument {key!r} in arg_values dict")
            positional.append(arg_values[key])
    else:
        positional = list(arg_values)

    # ── Assemble and make executable ──────────────────────────────────────────
    code = assemble_trampoline(cc, target_addr, positional, rip=0)
    base_addr, cleanup = make_executable(code)

    is_inline = not cc.slots.conventions()

    try:
        ret_slots = sorted(cc.slots.returns(), key=lambda s: s.index)

        if is_inline:
            # Return type: struct { uint64_t retval; uint64_t rflags; }
            # SysV AMD64 ABI returns two-uint64 structs in RAX + RDX.
            cfunc_t = ctypes.CFUNCTYPE(_AddResult)
            fn = cfunc_t(base_addr)
            raw = fn()
            result: dict = {}
            # Map the two struct fields back to RETURN slot names by index
            field_values = [raw.retval, raw.rflags]
            for i, slot in enumerate(ret_slots):
                result[slot.name] = field_values[i]
            return result
        else:
            # Plain int return (backwards compatible)
            cfunc_t = ctypes.CFUNCTYPE(ctypes.c_int64)
            fn = cfunc_t(base_addr)
            result_int = fn()
            return result_int if ret_slots else None
    finally:
        cleanup()


# ──────────────────────────────────────────────────────────────────────────────
# Hello-World demo  (SysV ABI path – unchanged behaviour)
# ──────────────────────────────────────────────────────────────────────────────

def hello_world() -> None:
    """End-to-end demo: call puts("Hello, World!") via a generated trampoline."""

    # ── Resolve puts in libc ──────────────────────────────────────────────────
    libc_path = ctypes.util.find_library("c")
    if not libc_path:
        raise RuntimeError("Cannot locate libc")
    libc = ctypes.CDLL(libc_path)
    puts_addr = ctypes.cast(libc.puts, ctypes.c_void_p).value
    assert puts_addr is not None

    # ── Keep the string buffer alive for the duration of the call ─────────────
    msg = b"Hello, World!\x00"
    buf = ctypes.create_string_buffer(msg)
    msg_addr = ctypes.addressof(buf)

    # ── Build the CallingConvention model for puts ────────────────────────────
    cc = build_puts_cc()

    # ── Assemble trampoline and show its disassembly ──────────────────────────
    code = assemble_trampoline(cc, puts_addr, [msg_addr], rip=0)
    print("=== Calling convention summary ===")
    cc.print_summary()
    print()
    print("=== Trampoline disassembly ===")
    dec = iced_x86.Decoder(64, code, ip=0)
    fmt = iced_x86.Formatter(iced_x86.FormatterSyntax.INTEL)
    for instr in dec:
        print(f"  {instr.ip:04x}  {fmt.format(instr)}")
    print()

    # ── Execute the trampoline ────────────────────────────────────────────────
    print("=== puts output ===")
    sys.stdout.flush()
    ret = call_trampoline(cc, puts_addr, [msg_addr])
    sys.stdout.flush()
    print(f"=== puts returned {ret} ===")


# ──────────────────────────────────────────────────────────────────────────────
# demo_add: inline ADD RAX, RBX snippet with RFLAGS capture
# ──────────────────────────────────────────────────────────────────────────────

def demo_add() -> None:
    """
    Demo: JIT-assemble  ADD RAX, RBX; RET  and call it from Python,
    passing both register values in and getting back the result + RFLAGS.
    """
    I = Instruction
    R = Register
    C = Code

    cc = build_add_cc()
    print("=== Calling convention summary ===")
    cc.print_summary()
    print()

    # Assemble the snippet: ADD RAX, RBX; RET
    instrs = [
        I.create_reg_reg(C.ADD_R64_RM64, R.RAX, R.RBX),
        I.create(C.RETNQ),
    ]
    enc = BlockEncoder(64)
    enc.add_many(instrs)
    instr_code = enc.encode(rip=0)
    base_addr, cleanup = make_executable(instr_code)

    print("=== ADD RAX, RBX snippet disassembly ===")
    dec = iced_x86.Decoder(64, instr_code, ip=base_addr)
    fmt = iced_x86.Formatter(iced_x86.FormatterSyntax.INTEL)
    for instr in dec:
        print(f"  {instr.ip:016x}  {fmt.format(instr)}")
    print()

    # Assemble and disassemble the trampoline that calls the snippet
    tramp_code = assemble_trampoline(cc, base_addr, [40, 2], rip=0)
    print("=== Trampoline disassembly (for ADD snippet) ===")
    dec2 = iced_x86.Decoder(64, tramp_code, ip=0)
    for instr in dec2:
        print(f"  {instr.ip:04x}  {fmt.format(instr)}")
    print()

    try:
        result = call_trampoline(cc, base_addr, {"rax": 40, "rbx": 2})
    finally:
        cleanup()

    print(f"40 + 2 = {result['retval']}")
    print(f"RFLAGS = 0x{result['rflags']:016x}")
    # Decode interesting flags for sanity-check
    rflags = result['rflags']
    cf = (rflags >> 0) & 1
    pf = (rflags >> 2) & 1
    zf = (rflags >> 6) & 1
    sf = (rflags >> 7) & 1
    of = (rflags >> 11) & 1
    print(f"  CF={cf}  PF={pf}  ZF={zf}  SF={sf}  OF={of}")
    print("  (40+2=42: ZF=0, CF=0, SF=0, OF=0 as expected)")


def sys_exit():
    I = Instruction
    R = Register
    C = Code

    cc = build_sys_exit_cc()
    print("=== Calling convention summary ===")
    cc.print_summary()
    print()

    # Assemble the snippet
    instrs = [
        I.create_reg_i64(C.MOV_R64_IMM64, R.RDI, 42),
        I.create_reg_i64(C.MOV_R64_IMM64, R.RAX, 60),
        I.create(C.SYSCALL),
    ]
    enc = BlockEncoder(64)
    enc.add_many(instrs)
    instr_code = enc.encode(rip=0)
    base_addr, cleanup = make_executable(instr_code)
    print("=== SYS_EXIT snippet disassembly ===")
    dec = iced_x86.Decoder(64, instr_code, ip=base_addr)
    fmt = iced_x86.Formatter(iced_x86.FormatterSyntax.INTEL)
    for instr in dec:
        print(f"  {instr.ip:016x}  {fmt.format(instr)}")
    print()


    # Assemble and disassemble the trampoline that calls the snippet
    tramp_code = assemble_trampoline(cc, base_addr, [], rip=0)
    print("=== Trampoline disassembly (for SYS_EXIT snippet) ===")
    dec2 = iced_x86.Decoder(64, tramp_code, ip=0)
    fmt = iced_x86.Formatter(iced_x86.FormatterSyntax.INTEL)
    for instr in dec2:
        print(f"  {instr.ip:04x}  {fmt.format(instr)}")
    print()

    try:
        result = call_trampoline(cc, base_addr, [])
        print(f"SYS_EXIT returned {result} (should not return)")
    finally:
        cleanup()

if __name__ == "__main__":
    hello_world()
    print("---")
    demo_add()
    print("---")
    sys_exit()
