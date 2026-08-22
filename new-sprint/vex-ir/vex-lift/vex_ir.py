from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from typing import Any, Generic, Type, TypeVar
import struct

from ir_types import (
    Polarity, Side,
    InstantiatedRule, RuleSchema,
    array_minus_key_slots,
    Formula, SidedFormula,
    TypedSequent,
)
from core_ir import AdmissibleRule, Case, JumboFormula, OpaqueType, FlatType, FlatOperation, Box
from dirty_ir import Const
import pyvex

# ── Primitive types ───────────────────────────────────────────────

Bool    = FlatType(label_type=bool,  name="Bool",    polarity=Polarity.POS) # I1/U1 in Valgrind, treated as an opaque type in the IR for uniformity
Int8   = FlatType(label_type=int,   name="Int8",   polarity=Polarity.POS)
Int16   = FlatType(label_type=int,   name="Int16",   polarity=Polarity.POS)
Int32   = FlatType(label_type=int,   name="Int32",   polarity=Polarity.POS)
Int64   = FlatType(label_type=int,   name="Int64",   polarity=Polarity.POS)
#Int128   = FlatType(label_type=int,   name="Int128",   polarity=Polarity.POS)
#Float16 = FlatType(label_type=float, name="Float16", polarity=Polarity.POS)
Float32 = FlatType(label_type=float, name="Float32", polarity=Polarity.POS)
Float64 = FlatType(label_type=float, name="Float64", polarity=Polarity.POS)
#Float128 = FlatType(label_type=float, name="Float128", polarity=Polarity.POS)
Vec128 = FlatType(label_type=int, name="Vec128", polarity=Polarity.POS) # 128-bit vector type, represented as arbitrary-precision int
Vec256 = FlatType(label_type=int, name="Vec256", polarity=Polarity.POS) # 256-bit vector type, represented as arbitrary-precision int

Ptr   = FlatType(label_type=int,   name="Ptr",   polarity=Polarity.POS)  # pointer or address type. There is some wonkiness because the VEX IR is actually loosely typed and uses I64, I32, even I16 in memory operations, depending on guest, but we will try to be more strict in our IR.

IRRoundingMode = FlatType(label_type=int, name="IRRoundingMode", polarity=Polarity.POS) # rounding mode for floating-point operations (I32 in vex)
IRCmpFResult = FlatType(label_type=int, name="IRCmpFResult", polarity=Polarity.POS) # result of a floating-point comparison (I32 in vex)
IRRegArray = FlatType(label_type=pyvex.IRRegArray, name="IRRegArray", polarity=Polarity.POS) # slice of guest state, represented as the Pyvex type
IRCallee = FlatType(label_type=pyvex.IRCallee, name="IRCallee", polarity=Polarity.POS) # a callee for a pure or dirty helper function, represented as the Pyvex type
GuestState = FlatType(label_type=bytes, name="GuestState", polarity=Polarity.POS) # Guest state, represented as a flat array of bytes (not exposed in pyvex, defined as per-architecture in vex)
Memory = FlatType(label_type=bytes, name="Memory", polarity=Polarity.POS) # Memory, represented as a flat array of bytes (not exposed in pyvex, Cliff's recommended way to handle memory effects in the IR)
type IREndness_unwrapped = str
IREndness = FlatType(label_type=str, name="IREndness", polarity=Polarity.POS) # endianness for memory operations (enum in vex, represented as a string in pyvex)
IRLoadGOp = FlatType(label_type=str, name="IRLoadGOp", polarity=Polarity.POS) # loadG operation (enum in vex, represented as an string in pyvex)

type IROpEnum = str
type IRExprTag = str
type IRJumpKind = str
type IREffect = str
type IRMBusEvent = str

# a pure if-then-else, like a ternary expression. Both branches have to be pure and evaluate to a value; the condition selects which value is returned.
@dataclass(frozen=True)
class ITE(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="ITE", args=(Bool, type, type), result=type)

# reads from GuestState - gs, offset, type
@dataclass(frozen=True)
class Get(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="Get", args=(GuestState, Int32), result=type)

# indexed read from GuestState - gs, descr, ix 
@dataclass(frozen=True)
class GetI(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="GetI", args=(GuestState, IRRegArray, Int32, Int32), result=type)

# writes a value to a fixed-offset guest register, returns updated guest state - gs, offset, val
@dataclass(frozen=True)
class Put(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="Put", args=(GuestState, Int32, type), result=GuestState)

# writes to an indexed `IRRegArray` guest register region, returns updated guest state - descr, ix, bias, data
@dataclass(frozen=True)
class PutI(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="PutI", args=(GuestState, IRRegArray, Int32, Int32, type), result=GuestState)

# reads from `Memory`
@dataclass(frozen=True)
class Load(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="Load", args=(Memory, Ptr, IREndness), result=type)

# unconditional memory write with endianness, returns updated memory
@dataclass(frozen=True)
class Store(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="Store", args=(Memory, Ptr, type, IREndness), result=Memory)
        
# guarded memory load with an inline conversion (e.g., zero-extend an 8-bit load to 32 bits). Both the address and the alternate value are always evaluated; the conversion (`ILGop`) is applied to the loaded value if the guard is true, and the alternate is used otherwise. Returns updated memory.
# mem, addr, alt, guard, cvt
@dataclass(frozen=True)
class LoadG(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="LoadG", args=(Memory, Ptr, type, Bool, IRLoadGOp, IREndness), result=type)

# guarded (conditional) memory write. The address and data expressions are evaluated regardless of the guard; only the actual store is conditional. Returns updated memory
# mem, addr, val, guard, endianness
@dataclass(frozen=True)
class StoreG(FlatOperation):
    def __init__(self, type: FlatType) -> None:
        super().__init__(name="StoreG", args=(Memory, Ptr, type, Bool, IREndness), result=Memory)

# a built-in primitive operation, see vex-ops-description.json
@dataclass(frozen=True)
class IrOp(FlatOperation):
    op: IROpEnum
    def __init__(self, op: IROpEnum, args: tuple[FlatType], result: FlatType) -> None:
        object.__setattr__(self, "op", op)
        super().__init__(name="IrOp", args=args, result=result)

# calls a pure helper function. The callee must be truly pure — it may not read or write guest state or guest memory, because doing so would hide those accesses from instrumentation tools.
@dataclass(frozen=True)
class PureCall(FlatOperation):
    def __init__(self, args: tuple[FlatType], result: FlatType) -> None:
        super().__init__(name="PureCall", args=(IRCallee, *args), result=result)

# Constant helpers - Valgrind has restricted constructors for values of certain types.

def int_bits_to_float32(int_val: int) -> float:
    # Pack the int as 4 bytes, then unpack as a 32-bit float
    return struct.unpack('>f', struct.pack('>I', int_val))[0]

def int_bits_to_float64(int_val: int) -> float:
    # Pack the int as 8 bytes, then unpack as a 64-bit float
    return struct.unpack('>d', struct.pack('>Q', int_val))[0]

def v128_from_ushort(ico_v128: int) -> int:
    """VEX Ico_V128 semantics: bit i (0-15) of ico_v128 selects byte i (0-15) of the
    128-bit result. Set bit -> 0xFF byte, clear bit -> 0x00 byte."""
    result = 0
    for i in range(16):
        if (ico_v128 >> i) & 1:
            result |= 0xFF << (8 * i)
    return result

def v256_from_uint(ico_v256: int) -> int:
    """VEX Ico_V256 semantics: bit i (0-31) selects byte i (0-31) of the
    256-bit result. Set bit -> 0xFF byte, clear bit -> 0x00 byte."""
    result = 0
    for i in range(32):
        if (ico_v256 >> i) & 1:
            result |= 0xFF << (8 * i)
    return result

@dataclass(frozen=True)
class Const_F32i(Const):
    restrict_value: int
    def __init__(self, value: int) -> None:
        object.__setattr__(self, "restrict_value", value)
        super().__init__(principal=Float32, case_label=int_bits_to_float32(value))

@dataclass(frozen=True)
class Const_F64i(Const):
    restrict_value: int
    def __init__(self, value: int) -> None:
        object.__setattr__(self, "restrict_value", value)
        super().__init__(principal=Float64, case_label=int_bits_to_float64(value))

@dataclass(frozen=True)
class Const_V128(Const):
    restrict_value: int
    def __init__(self, value: int) -> None:
        object.__setattr__(self, "restrict_value", value)
        super().__init__(principal=Vec128, case_label=v128_from_ushort(value))

# This is not actually implemented or used in VEX except for v=0, but we can define it for completeness
@dataclass(frozen=True)
class Const_V256(Const):
    restrict_value: int
    def __init__(self, value: int) -> None:
        object.__setattr__(self, "restrict_value", value)
        super().__init__(principal=Vec256, case_label=v256_from_uint(value))

@dataclass(frozen=True)
class ProgramOp():
    pass

# |- Program
# ---------- NoOp
# |- Program
@dataclass(frozen=True)
class IRStmt_NoOp(ProgramOp):
    pass

# |- Program
# ---------- IMark(addr, len, delta)
# |- Program
@dataclass(frozen=True)
class IRStmt_IMark(ProgramOp):
    addr: int
    len: int
    delta: int

# |- base:Ptr __ |- nia:Ptr __ |- Program
# ------------------------------------- AbiHint(len)
# |- Program
@dataclass(frozen=True)
class IRStmt_AbiHint(ProgramOp):
    len: int

# |- addr:Ptr __ |- expdHi:Int64 __ |- expdLo:Int64 __ |- dataHi:Int64 __ |- dataLo:Int64 __
# |- mem:Memory __ |- cont : (oldHi:Int64, oldLo:Int64, mem:Memory -o Program)
# ------------------------------------- AbiHint(end)
# |- Program
@dataclass(frozen=True)
class IRStmt_CAS(ProgramOp):
    end: IREndness_unwrapped

# SC:
# |- addr:Ptr __ storedata:Int64 __ |- mem:Memory __ |- cont : (result:Int64, mem:Memory -o Program)
# LL:
# |- addr:Ptr __ |- mem:Memory __ |- cont : (result:Int64, mem:Memory -o Program)
# ------------------------------------- LLSC(end)
# |- Program
@dataclass(frozen=True)
class IRStmt_LLSC(ProgramOp):
    end: str
    is_sc: bool  # True for SC, False for LL

# |- GuestState __ |- Ptr __ |- Memory __ |- other args __ |- IRCallee __ |- guard:Bool __ |- cont: (Memory, GuestState, result:T -o Program)
# ------------------------------------- Dirty(mem_effect, fxState, args, result)
# |- Program
@dataclass(frozen=True)
class IRStmt_Dirty(ProgramOp):
    mem_effect: str | tuple[str, int]  # "Ifx_None" or (mFx, mSize)
    fxState: list[tuple[str, int, int, int, int]]  # (fx, offset, size, nRepeats, repeatLen)
    args: list[Formula | str]  # The args vector may contain "GSPTR" and/or "VECRET", in both cases, at most once.
    result: Formula | None  # None for void return, or a formula for the result

# |- Memory __ |- cont: (Memory -o Program)
# ------------------------------------- MBE(event)
# |- Program
@dataclass(frozen=True)
class IRStmt_MBE(ProgramOp):
    event: IRMBusEvent

# |- guard:Bool __ |- GuestState __ |- Memory __ |- dst: Addr |- cont: (Memory, GuestState, result:T -o Program)
# ------------------------------------- Exit(jumpkind, offsIP)
# |- Program
@dataclass(frozen=True)
class IRStmt_Exit(ProgramOp):
    jumpkind: IRJumpKind
    offsIP: int

# |- guard:Bool __ |- GuestState __ |- Memory
# ------------------------------------- ExitU(jumpkind, offsIP)
# |- Program
@dataclass(frozen=True)
class IRStmt_ExitU(ProgramOp):
    # these show up in IRSBs as the last statement, and are unconditional exits. Basically, they are the same as Exit but without a guard and without a sub-program element.
    jumpkind: IRJumpKind
    offsIP: int

class Singleton(type):
    _instances = {}

    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(*args, **kwargs)
        return cls._instances[cls]

@dataclass(frozen=True)
class Program(OpaqueType,metaclass=Singleton):
    """Program type - could be represented as a positive jumbo type but it's recursive and messy, so we treat it as a singleton opaque type which is slightly cleaner."""
    
    name: str = "Program"
    polarity: Polarity = Polarity.POS

    def is_cartesian(self) -> bool:
        return False
    
    def is_cocartesian(self) -> bool:
        return False

    def get_case_type(self, case_label: ProgramOp) -> tuple[SidedFormula, ...]:
        L, R = Side.LEFT, Side.RIGHT
        if isinstance(case_label, IRStmt_NoOp):
            return ((R, Program()),)
        if isinstance(case_label, IRStmt_IMark):
            return ((R, Program()),)
        if isinstance(case_label, IRStmt_AbiHint):
            return ((R, Ptr), (R, Ptr), (R, Program())) # base, nia
        if isinstance(case_label, IRStmt_CAS):
            cont = JumboFormula(Polarity.NEG, (Case("#f", ((L,Int64),(L,Int64),(L,Memory),(R,Program()))),)) # oldHi, oldLo, mem
            return ((R, Ptr), (R, Int64), (R, Int64), (R, Int64), (R, Int64), (R,Memory), (R, cont)) # addr, expdHi, expdLo, dataHi, dataLo, mem, cont
        if isinstance(case_label, IRStmt_LLSC):
            cont = JumboFormula(Polarity.NEG, (Case("#f", ((L,Int64),(L,Memory),(R,Program()))),)) # result, mem
            if case_label.is_sc:
                return ((R, Ptr), (R, Int64), (R, Memory), (R, cont)) # addr, storedata, mem, cont
            else:
                return ((R, Ptr), (R, Memory), (R, cont)) # addr, mem, cont
        if isinstance(case_label, IRStmt_Dirty):
            retty = [(R,Program())]
            if case_label.mem_effect != "Ifx_None":
                retty = [(L,Memory)] + retty
            if case_label.fxState:
                retty = [(L,GuestState)] + retty
            if case_label.result is not None:
                retty = [(L,case_label.result)] + retty
            cont = JumboFormula(Polarity.NEG, (Case("#f", tuple(retty)),))
            args = [(R, cont)]
            if case_label.fxState:
                args = [(R, GuestState)] + args
            if case_label.mem_effect != "Ifx_None":
                args = [(R, Ptr), (R, Memory)] + args # mem, addr
            for arg in case_label.args:
                if isinstance(arg, str):
                    continue # "GSPTR" and "VECRET" are not actual arguments, they are just markers for the call semantics
                args = [(R, arg)] + args
            args = [(R, IRCallee),(R, Bool)] + args # callee, guard
            return tuple(args)
        if isinstance(case_label, IRStmt_MBE):
            return ((R, Program()),)
        if isinstance(case_label, IRStmt_Exit):
            cont = JumboFormula(Polarity.NEG, (Case("#f", ((L, GuestState), (L, Memory),(R,Program()))),))
            return ((R, Bool), (R, cont), (R, Program())) # guard, cont_true, cont_false
        if isinstance(case_label, IRStmt_ExitU):
            return ((R, GuestState), (R, Memory)) # no cont, end of IRSB
        raise ValueError(f"Unknown ProgramOp case label: {case_label}")
