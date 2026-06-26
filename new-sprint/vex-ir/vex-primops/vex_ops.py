# --------------- Primops (arity 1,2,3 and 4) ---------------
# Primitive operations that are used in Unop, Binop, Triop and Qop
# IRExprs.  Once we take into account integer, floating point and SIMD
# operations of all the different sizes, there are quite a lot of them.
# Most instructions supported by the architectures that Vex supports
# (x86, PPC, etc) are represented.  Some more obscure ones (eg. cpuid)
# are not;  they are instead handled with dirty helpers that emulate
# their functionality.  Such obscure ones are thus not directly visible
# in the IR, but their effects on guest state (memory and registers)
# are made visible via the annotations in IRDirty structures.
# 2018-Dec-27: some of int<->fp conversion operations have been renamed so as
# to have a trailing _DEP, meaning "deprecated".  This is because they don't
# specify a rounding mode to be used for the conversion and so are
# underspecified.  Their use should be replaced with equivalents that do
# specify a rounding mode, either as a first argument or using a suffix on the
# name, that indicates the rounding mode to use.

from enum import IntEnum, auto

class IROp(IntEnum):
    # -- Do not change this ordering.  The IR generators rely on
    # (eg) Iop_Add64 == IopAdd8 + 3. --
    INVALID = 0x1400
    Add8 = auto()
    Add16 = auto()
    Add32 = auto()
    Add64 = auto()
    Sub8 = auto()
    Sub16 = auto()
    Sub32 = auto()
    Sub64 = auto()  # Signless mul.  MullS/MullU is elsewhere.

    Mul8 = auto()
    Mul16 = auto()
    Mul32 = auto()
    Mul64 = auto()
    Or8 = auto()
    Or16 = auto()
    Or32 = auto()
    Or64 = auto()
    And8 = auto()
    And16 = auto()
    And32 = auto()
    And64 = auto()
    Xor8 = auto()
    Xor16 = auto()
    Xor32 = auto()
    Xor64 = auto()  # Bitwise shift ops  Semantics as per C standard:  If the value of the right operand is negative or is greater  than or equal to the width of the left operand, the behaviour is  undefined.  For Shl: The result of E1 << E2 is E1 left-shifted E2 bit positions.  Vacated bits are filled with zeroes.  For Shr: The result of E1 >> E2 is E1 right-shifted E2 bit positions.  Vacated bits are filled with zeroes.  For Sar: The result of E1 >> E2 is E1 right-shifted E2 bit positions.  Vacated bits are filled with the most significant bit of E1 prior  to shifting.

    Shl8 = auto()
    Shl16 = auto()
    Shl32 = auto()
    Shl64 = auto()
    Shr8 = auto()
    Shr16 = auto()
    Shr32 = auto()
    Shr64 = auto()
    Sar8 = auto()
    Sar16 = auto()
    Sar32 = auto()
    Sar64 = auto()  # Integer comparisons.

    CmpEQ8 = auto()
    CmpEQ16 = auto()
    CmpEQ32 = auto()
    CmpEQ64 = auto()
    CmpNE8 = auto()
    CmpNE16 = auto()
    CmpNE32 = auto()
    CmpNE64 = auto()  # Tags for unary ops

    Not8 = auto()
    Not16 = auto()
    Not32 = auto()
    Not64 = auto()

    # Exactly like CmpEQ8/16/32/64, but carrying the additional
    # hint that these compute the success/failure of a CAS
    # operation, and hence are almost certainly applied to two
    # copies of the same value, which in turn has implications for
    # Memcheck's instrumentation.

    CasCmpEQ8 = auto()
    CasCmpEQ16 = auto()
    CasCmpEQ32 = auto()
    CasCmpEQ64 = auto()
    CasCmpNE8 = auto()
    CasCmpNE16 = auto()
    CasCmpNE32 = auto()
    CasCmpNE64 = auto()

    # Exactly like CmpNE8/16/32/64, but carrying the additional
    # hint that these needs expensive definedness tracking.

    ExpCmpNE8 = auto()
    ExpCmpNE16 = auto()
    ExpCmpNE32 = auto()
    ExpCmpNE64 = auto()

    # -- Ordering not important after here. --
    # Widening multiplies

    MullS8 = auto()
    MullS16 = auto()
    MullS32 = auto()
    MullS64 = auto()
    MullU8 = auto()
    MullU16 = auto()
    MullU32 = auto()
    MullU64 = auto()

    # Counting bits
    # Count leading/trailing zeroes, with "natural" semantics for the
    # case where the input is zero: then the result is the number of bits
    # in the word.

    ClzNat64 = auto()
    ClzNat32 = auto()
    CtzNat64 = auto()
    CtzNat32 = auto()  # Population count -- compute the number of 1 bits in the argument.

    PopCount64 = auto()
    PopCount32 = auto()

    # Standard integer comparisons

    CmpLT32S = auto()
    CmpLT64S = auto()
    CmpLE32S = auto()
    CmpLE64S = auto()
    CmpLT32U = auto()
    CmpLT64U = auto()
    CmpLE32U = auto()
    CmpLE64U = auto()

    # As a sop to Valgrind-Memcheck, the following are useful.

    CmpNEZ8 = auto()
    CmpNEZ16 = auto()
    CmpNEZ32 = auto()
    CmpNEZ64 = auto()
    CmpwNEZ32 = auto()
    CmpwNEZ64 = auto()  # all-0s -> all-Os; other -> all-1s

    Left8 = auto()
    Left16 = auto()
    Left32 = auto()
    Left64 = auto()  # \x -> x | -x

    Max32U = auto()  # unsigned max
    # PowerPC-style 3-way integer comparisons.  Without them it is
    # difficult to simulate PPC efficiently.
    # op(x,y) | x < y  = 0x8 else
    # | x > y  = 0x4 else
    # | x == y = 0x2

    CmpORD32U = auto()
    CmpORD64U = auto()
    CmpORD32S = auto()
    CmpORD64S = auto()

    # Division
    # Semantics of division as per C standard:
    # If the value of the divisor is zero, the behaviour is undefined.
    # When integers are divided, the result of division is the algebraic
    # quotient with any fractional part discarded. In other words:
    # truncation towards zero. If the quotient a/b is representable,
    # the expression (a/b)*b + a%b shall equal a; otherwise, the behaviour
    # of division and modulo operation is undefined.

    DivU32 = auto()  # :: I32,I32 -> I32 (simple div, no mod)
    DivS32 = auto()  # ditto, signed
    DivU64 = auto()  # :: I64,I64 -> I64 (simple div, no mod)
    DivS64 = auto()  # ditto, signed
    DivU128 = auto()  # :: I128,I128 -> I128 (simple div, no mod)
    DivS128 = auto()  # ditto, signed

    DivU32E = auto()  # :: I32,I32 -> I32 (dividend is 32-bit arg (hi)
    DivS32E = auto()  # ditto, signed
    DivU64E = auto()  # :: I64,I64 -> I64 (dividend is 64-bit arg (hi)
    DivS64E = auto()  # ditto, signed
    DivU128E = auto()  # :: I128,I128 -> I128 (dividend is 128-bit arg (hi)
    DivS128E = auto()  # ditto, signed

    DivModU64to32 = auto()  # :: I64,I32 -> I64
    DivModS64to32 = auto()  # ditto, signed

    DivModU128to64 = auto()  # :: I128,I64 -> I128
    DivModS128to64 = auto()  # ditto, signed

    DivModS64to64 = auto()  # :: I64,I64 -> I128
    DivModU64to64 = auto()  # :: I64,I64 -> I128
    DivModS32to32 = auto()  # :: I32,I32 -> I64
    DivModU32to32 = auto()  # :: I32,I32 -> I64

    ModU128 = auto()  # :: I128,I128 -> I128  normal modulo operation
    ModS128 = auto()  # ditto, signed

    # Integer conversions.  Some of these are redundant (eg
    # Iop_64to8 is the same as Iop_64to32 and then Iop_32to8), but
    # having a complete set reduces the typical dynamic size of IR
    # and makes the instruction selectors easier to write.
    # Widening conversions

    Widen8Uto16 = auto()
    Widen8Uto32 = auto()
    Widen8Uto64 = auto()
    Widen16Uto32 = auto()
    Widen16Uto64 = auto()
    Widen32Uto64 = auto()
    Widen8Sto16 = auto()
    Widen8Sto32 = auto()
    Widen8Sto64 = auto()
    Widen16Sto32 = auto()
    Widen16Sto64 = auto()
    Widen32Sto64 = auto()

    # Narrowing conversions

    Narrow64to8 = auto()
    Narrow32to8 = auto()
    Narrow64to16 = auto()
    
    # 8 <-> 16 bit conversions

    Extract16to8 = auto()  # :: I16 -> I8, low half
    Extract16HIto8 = auto()  # :: I16 -> I8, high half
    Pack8HLto16 = auto()  # :: (I8,I8) -> I16  16 <-> 32 bit conversions

    Extract32to16 = auto()  # :: I32 -> I16, low half
    Extract32HIto16 = auto()  # :: I32 -> I16, high half
    Pack16HLto32 = auto()  # :: (I16,I16) -> I32  32 <-> 64 bit conversions

    Extract64to32 = auto()  # :: I64 -> I32, low half
    Extract64HIto32 = auto()  # :: I64 -> I32, high half
    Pack32HLto64 = auto()  # :: (I32,I32) -> I64  64 <-> 128 bit conversions

    Extract128to64 = auto()  # :: I128 -> I64, low half
    Extract128HIto64 = auto()  # :: I128 -> I64, high half
    Pack64HLto128 = auto()  # :: (I64,I64) -> I128  1-bit stuff

    Not1 = auto()  # :: Ity_Bit -> Ity_Bit

    And1 = auto()  # :: (Ity_Bit, Ity_Bit) -> Ity_Bit.  Evaluates both args!

    Or1 = auto()  # :: (Ity_Bit, Ity_Bit) -> Ity_Bit.  Evaluates both args!

    Extract32to1 = auto()  # :: Ity_I32 -> Ity_Bit, just select bit[0]

    Extract64to1 = auto()  # :: Ity_I64 -> Ity_Bit, just select bit[0]

    Widen1Uto8 = auto()  # :: Ity_Bit -> Ity_I8,  unsigned widen

    Widen1Uto32 = auto()  # :: Ity_Bit -> Ity_I32, unsigned widen

    Widen1Uto64 = auto()  # :: Ity_Bit -> Ity_I64, unsigned widen

    Widen1Sto8 = auto()  # :: Ity_Bit -> Ity_I8,  signed widen

    Widen1Sto16 = auto()  # :: Ity_Bit -> Ity_I16, signed widen

    Widen1Sto32 = auto()  # :: Ity_Bit -> Ity_I32, signed widen

    Widen1Sto64 = auto()  # :: Ity_Bit -> Ity_I64, signed widen
    # ------ Floating point.  We try to be IEEE754 compliant. ------
    # --- Simple stuff as mandated by 754. ---
    # Binary operations, with rounding.
    # :: IRRoundingMode(I32) x F64 x F64 -> F64

    AddF64 = auto()
    SubF64 = auto()
    MulF64 = auto()
    DivF64 = auto()

    # :: IRRoundingMode(I32) x F32 x F32 -> F32

    AddF32 = auto()
    SubF32 = auto()
    MulF32 = auto()
    DivF32 = auto()

    # Variants of the above which produce a 64-bit result but which
    # round their result to a IEEE float range first.
    # :: IRRoundingMode(I32) x F64 x F64 -> F64

    AddF64r32 = auto()
    SubF64r32 = auto()
    MulF64r32 = auto()
    DivF64r32 = auto()

    # Unary operations, without rounding.
    # :: F64 -> F64

    NegF64 = auto()
    AbsF64 = auto()

    # :: F32 -> F32

    NegF32 = auto()
    AbsF32 = auto()

    # :: F16 -> F16

    NegF16 = auto()
    AbsF16 = auto()

    # Unary operations, with rounding.
    # :: IRRoundingMode(I32) x F64 -> F64

    SqrtF64 = auto()

    # :: IRRoundingMode(I32) x F32 -> F32

    SqrtF32 = auto()

    # :: IRRoundingMode(I32) x F16 -> F16

    SqrtF16 = auto()

    # :: IRRoundingMode(I32) x F16 x F16 -> F16

    SubF16 = auto()
    AddF16 = auto()

    # Comparison, yielding GT/LT/EQ/UN(ordered), as per the following:
    # 0x45 Unordered
    # 0x01 LT
    # 0x00 GT
    # 0x40 EQ
    # This just happens to be the Intel encoding.  The values
    # are recorded in the type IRCmpF64Result.
    # :: F64 x F64 -> IRCmpF64Result(I32)

    CmpF64 = auto()
    CmpF32 = auto()
    CmpF16 = auto()
    CmpF128 = auto()

    # --- Int to/from FP conversions. ---
    # For the most part, these take a first argument :: Ity_I32 (as
    # IRRoundingMode) which is an indication of the rounding mode
    # to use, as per the following encoding ("the standard
    # encoding"):
    # 00b  to nearest (the default)
    # 01b  to -infinity
    # 10b  to +infinity
    # 11b  to zero
    # This just happens to be the Intel encoding.  For reference only,
    # the PPC encoding is:
    # 00b  to nearest (the default)
    # 01b  to zero
    # 10b  to +infinity
    # 11b  to -infinity
    # Any PPC -> IR front end will have to translate these PPC
    # encodings, as encoded in the guest state, to the standard
    # encodings, to pass to the primops.
    # For reference only, the ARM VFP encoding is:
    # 00b  to nearest
    # 01b  to +infinity
    # 10b  to -infinity
    # 11b  to zero
    # Again, this will have to be converted to the standard encoding
    # to pass to primops.
    # If one of these conversions gets an out-of-range condition,
    # or a NaN, as an argument, the result is host-defined.  On x86
    # the "integer indefinite" value 0x80..00 is produced.  On PPC
    # it is either 0x80..00 or 0x7F..FF depending on the sign of
    # the argument.
    # On ARMvfp, when converting to a signed integer result, the
    # overflow result is 0x80..00 for negative args and 0x7F..FF
    # for positive args.  For unsigned integer results it is
    # 0x00..00 and 0xFF..FF respectively.
    # Rounding is required whenever the destination type cannot
    # represent exactly all values of the source type.

    F64toI16S = auto()  # IRRoundingMode(I32) x F64 -> signed I16

    F64toI32S = auto()  # IRRoundingMode(I32) x F64 -> signed I32

    F64toI64S = auto()  # IRRoundingMode(I32) x F64 -> signed I64

    F64toI64U = auto()  # IRRoundingMode(I32) x F64 -> unsigned I64

    F64toI32U = auto()  # IRRoundingMode(I32) x F64 -> unsigned I32

    I32StoF64 = auto()  # signed I32 -> F64

    I64StoF64 = auto()  # IRRoundingMode(I32) x signed I64 -> F64

    I64UtoF64 = auto()  # IRRoundingMode(I32) x unsigned I64 -> F64

    I64UtoF32 = auto()  # IRRoundingMode(I32) x unsigned I64 -> F32

    I32UtoF32 = auto()  # IRRoundingMode(I32) x unsigned I32 -> F32

    I32UtoF64 = auto()  # unsigned I32 -> F64

    F32toI32S = auto()  # IRRoundingMode(I32) x F32 -> signed I32

    F32toI64S = auto()  # IRRoundingMode(I32) x F32 -> signed I64

    F32toI32U = auto()  # IRRoundingMode(I32) x F32 -> unsigned I32

    F32toI64U = auto()  # IRRoundingMode(I32) x F32 -> unsigned I64

    I32StoF32 = auto()  # IRRoundingMode(I32) x signed I32 -> F32

    I64StoF32 = auto()  # IRRoundingMode(I32) x signed I64 -> F32
    # Conversion between floating point formats

    F32toF64 = auto()  # F32 -> F64

    F64toF32 = auto()  # IRRoundingMode(I32) x F64 -> F32
    # Reinterpretation.  Take an F32/64/128 and produce an I32/64/128
    # with the same bit pattern, or vice versa.

    ReinterpV128asI128 = auto()
    ReinterpI128asV128 = auto()
    ReinterpF128asI128 = auto()
    ReinterpI128asF128 = auto()
    ReinterpF64asI64 = auto()
    ReinterpI64asF64 = auto()
    ReinterpF32asI32 = auto()
    ReinterpI32asF32 = auto()

    # Support for 128-bit floating point

    F64HLtoF128 = auto()  # (high half of F128,low half of F128) -> F128

    F128HItoF64 = auto()  # F128 -> high half of F128 into a F64 register

    F128LOtoF64 = auto()  # F128 -> low  half of F128 into a F64 register
    # :: IRRoundingMode(I32) x F128 x F128 -> F128

    AddF128 = auto()
    SubF128 = auto()
    MulF128 = auto()
    DivF128 = auto()
    MAddF128 = auto()  # (A * B) + C
    MSubF128 = auto()  # (A * B) - C
    NegMAddF128 = auto()  # -((A * B) + C)
    NegMSubF128 = auto()  # -((A * B) - C)

    # :: F128 -> F128

    NegF128 = auto()
    AbsF128 = auto()

    # :: IRRoundingMode(I32) x F128 -> F128

    SqrtF128 = auto()

    I32StoF128 = auto()  # signed I32  -> F128

    I64StoF128 = auto()  # signed I64  -> F128

    I32UtoF128 = auto()  # unsigned I32  -> F128

    I64UtoF128 = auto()  # unsigned I64  -> F128

    F32toF128 = auto()  # F32  -> F128

    F64toF128 = auto()  # F64  -> F128

    I128UtoF128 = auto()  # unsigned I128 -> F128

    I128StoF128 = auto()  # signed I128 -> F128

    F128toI32S = auto()  # IRRoundingMode(I32) x F128 -> signed I32

    F128toI64S = auto()  # IRRoundingMode(I32) x F128 -> signed I64

    F128toI32U = auto()  # IRRoundingMode(I32) x F128 -> unsigned I32

    F128toI64U = auto()  # IRRoundingMode(I32) x F128 -> unsigned I64

    F128toI128S = auto()  # IRRoundingMode(I32) x F128 -> signed I128

    F128toF64 = auto()  # IRRoundingMode(I32) x F128 -> F64

    F128toF32 = auto()  # IRRoundingMode(I32) x F128 -> F32

    RndF128 = auto()  # IRRoundingMode(I32) x F128 -> F128
    # Truncate to the specified value, source and result
    # are stroed in a F128 register.

    TruncF128toI32S = auto()  # truncate F128 -> I32

    TruncF128toI32U = auto()  # truncate F128 -> I32

    TruncF128toI64U = auto()  # truncate F128 -> I64

    TruncF128toI64S = auto()  # truncate F128 -> I64

    TruncF128toI128U = auto()  # truncate F128 -> I128

    TruncF128toI128S = auto()  # truncate F128 -> I128
    # --- guest x86/amd64 specifics, not mandated by 754. ---
    # Binary ops, with rounding.
    # :: IRRoundingMode(I32) x F64 x F64 -> F64

    AtanF64 = auto()  # FPATAN,  arctan(arg1/arg2)

    Yl2xF64 = auto()  # FYL2X,   arg1 * log2(arg2)

    Yl2xp1F64 = auto()  # FYL2XP1, arg1 * log2(arg2+1.0)

    PRemF64 = auto()  # FPREM,   non-IEEE remainder(arg1/arg2)

    PRemC3210F64 = auto()  # C3210 flags resulting from FPREM, :: I32

    PRem1F64 = auto()  # FPREM1,  IEEE remainder(arg1/arg2)

    PRem1C3210F64 = auto()  # C3210 flags resulting from FPREM1, :: I32

    ScaleF64 = auto()  # FSCALE,  arg1 * (2^RoundTowardsZero(arg2))
    # Note that on x86 guest, PRem1{C3210} has the same behaviour
    # as the IEEE mandated RemF64, except it is limited in the
    # range of its operand.  Hence the partialness.
    # Unary ops, with rounding.
    # :: IRRoundingMode(I32) x F64 -> F64

    SinF64 = auto()  # FSIN

    CosF64 = auto()  # FCOS

    TanF64 = auto()  # FTAN

    Op2xm1F64 = auto()  # (2^arg - 1.0)

    RoundF128toInt = auto()  # F128 value to nearest integral value (still  as F128)

    RoundF64toInt = auto()  # F64 value to nearest integral value (still  as F64)

    RoundF64toIntA0 = auto()  # As Iop_RoundF64toInt but ties to above zero

    RoundF64toIntE = auto()  # As Iop_RoundF64toInt but ties to even

    RoundF32toInt = auto()  # F32 value to nearest integral value (still  as F32)

    RoundF32toIntA0 = auto()  # As Iop_RoundF32toInt but ties to above zero

    RoundF32toIntE = auto()  # As Iop_RoundF32toInt but ties to even
    # --- guest s390 specifics, not mandated by 754. ---
    # Fused multiply-add/sub
    # :: IRRoundingMode(I32) x F32 x F32 x F32 -> F32
    # (computes arg2 * arg3 +/- arg4)

    MAddF32 = auto()
    MSubF32 = auto()

    # --- guest ppc32/64 specifics, not mandated by 754. ---
    # Ternary operations, with rounding.
    # Fused multiply-add/sub, with 112-bit intermediate
    # precision for ppc.
    # Also used to implement fused multiply-add/sub for s390.
    # :: IRRoundingMode(I32) x F64 x F64 x F64 -> F64
    # (computes arg2 * arg3 +/- arg4)

    MAddF64 = auto()
    MSubF64 = auto()

    # Variants of the above which produce a 64-bit result but which
    # round their result to a IEEE float range first.
    # :: IRRoundingMode(I32) x F64 x F64 x F64 -> F64

    MAddF64r32 = auto()
    MSubF64r32 = auto()

    # :: F64 -> F64

    RSqrtEst5GoodF64 = auto()  # reciprocal square root estimate, 5 good bits

    RoundF64toF64_NEAREST = auto()  # frin

    RoundF64toF64_NegINF = auto()  # frim

    RoundF64toF64_PosINF = auto()  # frip

    RoundF64toF64_ZERO = auto()  # friz
    # :: F64 -> F32

    TruncF64asF32 = auto()  # do F64->F32 truncation as per 'fsts'
    # :: IRRoundingMode(I32) x F64 -> F64

    RoundF64toF32 = auto()  # round F64 to nearest F32 value (still as F64)
    # NB: pretty much the same as Iop_F64toF32, except no change
    # of type.
    # --- guest arm64 specifics, not mandated by 754. ---

    RecpExpF64 = auto()  # FRECPX d  :: IRRoundingMode(I32) x F64 -> F64

    RecpExpF32 = auto()  # FRECPX s  :: IRRoundingMode(I32) x F32 -> F32
    # --------- Possibly required by IEEE 754-2008. ---------

    MaxNumF64 = auto()  # max, F64, numerical operand if other is a qNaN

    MinNumF64 = auto()  # min, F64, ditto

    MaxNumF32 = auto()  # max, F32, ditto

    MinNumF32 = auto()  # min, F32, ditto
    # ------------------ 16-bit scalar FP ------------------

    F16toF64 = auto()  # F16 -> F64

    F64toF16 = auto()  # IRRoundingMode(I32) x F64 -> F16

    F16toF32 = auto()  # F16 -> F32

    F32toF16 = auto()  # IRRoundingMode(I32) x F32 -> F16
    # ------------------ 32-bit SIMD Integer ------------------
    # 32x1 saturating add/sub (ok, well, not really SIMD :)

    QAdd32S = auto()
    QSub32S = auto()

    # 16x2 add/sub, also signed/unsigned saturating variants

    Add16x2 = auto()
    Sub16x2 = auto()
    QAdd16Sx2 = auto()
    QAdd16Ux2 = auto()
    QSub16Sx2 = auto()
    QSub16Ux2 = auto()

    # 16x2 signed/unsigned halving add/sub.  For each lane, these
    # compute bits 16:1 of (eg) sx(argL) + sx(argR),
    # or zx(argL) - zx(argR) etc.

    HAdd16Ux2 = auto()
    HAdd16Sx2 = auto()
    HSub16Ux2 = auto()
    HSub16Sx2 = auto()

    # 8x4 add/sub, also signed/unsigned saturating variants

    Add8x4 = auto()
    Sub8x4 = auto()
    QAdd8Sx4 = auto()
    QAdd8Ux4 = auto()
    QSub8Sx4 = auto()
    QSub8Ux4 = auto()

    # 8x4 signed/unsigned halving add/sub.  For each lane, these
    # compute bits 8:1 of (eg) sx(argL) + sx(argR),
    # or zx(argL) - zx(argR) etc.

    HAdd8Ux4 = auto()
    HAdd8Sx4 = auto()
    HSub8Ux4 = auto()
    HSub8Sx4 = auto()

    # 8x4 sum of absolute unsigned differences.

    Sad8Ux4 = auto()

    # MISC (vector integer cmp != 0)

    CmpNEZ16x2 = auto()
    CmpNEZ8x4 = auto()

    # Byte swap in a 32-bit word

    Reverse8sIn32_x1 = auto()

    # ------------------ 64-bit SIMD FP ------------------------
    # Conversion to/from int

    I32UtoF32x2_DEP = auto()
    I32StoF32x2_DEP = auto()  # I32x2 -> F32x2

    F32toI32Ux2_RZ = auto()
    F32toI32Sx2_RZ = auto()  # F32x2 -> I32x2
    # Fixed32 format is floating-point number with fixed number of fraction
    # bits. The number of fraction bits is passed as a second argument of
    # type I8.

    F32ToFixed32Ux2_RZ = auto()
    F32ToFixed32Sx2_RZ = auto()  # fp -> fixed-point

    Fixed32UToF32x2_RN = auto()
    Fixed32SToF32x2_RN = auto()  # fixed-point -> fp
    # Binary operations

    Max32Fx2 = auto()
    Min32Fx2 = auto()  # Pairwise Min and Max. See integer pairwise operations for more  details.

    PwMax32Fx2 = auto()
    PwMin32Fx2 = auto()  # Note: For the following compares, the arm front-end assumes a  nan in a lane of either argument returns zero for that lane.

    CmpEQ32Fx2 = auto()
    CmpGT32Fx2 = auto()
    CmpGE32Fx2 = auto()

    # Vector Reciprocal Estimate finds an approximate reciprocal of each
    # element in the operand vector, and places the results in the destination
    # vector.

    RecipEst32Fx2 = auto()

    # Vector Reciprocal Step computes (2.0 - arg1 * arg2).
    # Note, that if one of the arguments is zero and another one is infinity
    # of arbitrary sign the result of the operation is 2.0.

    RecipStep32Fx2 = auto()

    # Vector Reciprocal Square Root Estimate finds an approximate reciprocal
    # square root of each element in the operand vector.

    RSqrtEst32Fx2 = auto()

    # Vector Reciprocal Square Root Step computes (3.0 - arg1 * arg2) / 2.0.
    # Note, that of one of the arguments is zero and another one is infiinty
    # of arbitrary sign the result of the operation is 1.5.

    RSqrtStep32Fx2 = auto()

    # Unary

    Neg32Fx2 = auto()
    Abs32Fx2 = auto()

    # ------------------ 64-bit SIMD Integer. ------------------
    # MISC (vector integer cmp != 0)

    CmpNEZ8x8 = auto()
    CmpNEZ16x4 = auto()
    CmpNEZ32x2 = auto()

    # ADDITION (normal / unsigned sat / signed sat)

    Add8x8 = auto()
    Add16x4 = auto()
    Add32x2 = auto()
    QAdd8Ux8 = auto()
    QAdd16Ux4 = auto()
    QAdd32Ux2 = auto()
    QAdd64Ux1 = auto()
    QAdd8Sx8 = auto()
    QAdd16Sx4 = auto()
    QAdd32Sx2 = auto()
    QAdd64Sx1 = auto()

    # PAIRWISE operations
    # Iop_PwFoo16x4( [a,b,c,d], [e,f,g,h] ) =
    # [Foo16(a,b), Foo16(c,d), Foo16(e,f), Foo16(g,h)]

    PwAdd8x8 = auto()
    PwAdd16x4 = auto()
    PwAdd32x2 = auto()
    PwMax8Sx8 = auto()
    PwMax16Sx4 = auto()
    PwMax32Sx2 = auto()
    PwMax8Ux8 = auto()
    PwMax16Ux4 = auto()
    PwMax32Ux2 = auto()
    PwMin8Sx8 = auto()
    PwMin16Sx4 = auto()
    PwMin32Sx2 = auto()
    PwMin8Ux8 = auto()
    PwMin16Ux4 = auto()
    PwMin32Ux2 = auto()  # Longening variant is unary. The resulting vector contains two times  less elements than operand, but they are two times wider.  Example:  Iop_PAddL16Ux4( [a,b,c,d] ) = [a+b,c+d]  where a+b and c+d are unsigned 32-bit values.

    PwAddL8Ux8 = auto()
    PwAddL16Ux4 = auto()
    PwAddL32Ux2 = auto()
    PwAddL8Sx8 = auto()
    PwAddL16Sx4 = auto()
    PwAddL32Sx2 = auto()

    # SUBTRACTION (normal / unsigned sat / signed sat)

    Sub8x8 = auto()
    Sub16x4 = auto()
    Sub32x2 = auto()
    QSub8Ux8 = auto()
    QSub16Ux4 = auto()
    QSub32Ux2 = auto()
    QSub64Ux1 = auto()
    QSub8Sx8 = auto()
    QSub16Sx4 = auto()
    QSub32Sx2 = auto()
    QSub64Sx1 = auto()

    # ABSOLUTE VALUE

    Abs8x8 = auto()
    Abs16x4 = auto()
    Abs32x2 = auto()

    # MULTIPLICATION (normal / high half of signed/unsigned / plynomial )

    Mul8x8 = auto()
    Mul16x4 = auto()
    Mul32x2 = auto()
    Mul32Fx2 = auto()
    MulHi16Ux4 = auto()
    MulHi16Sx4 = auto()  # Plynomial multiplication treats it's arguments as coefficients of  polynoms over {0, 1}.

    PolynomialMul8x8 = auto()

    # Vector Saturating Doubling Multiply Returning High Half and
    # Vector Saturating Rounding Doubling Multiply Returning High Half
    # These IROp's multiply corresponding elements in two vectors, double
    # the results, and place the most significant half of the final results
    # in the destination vector. The results are truncated or rounded. If
    # any of the results overflow, they are saturated.

    QDMulHi16Sx4 = auto()
    QDMulHi32Sx2 = auto()
    QRDMulHi16Sx4 = auto()
    QRDMulHi32Sx2 = auto()

    # AVERAGING: note: (arg1 + arg2 + 1) >>u 1

    Avg8Ux8 = auto()
    Avg16Ux4 = auto()

    # MIN/MAX

    Max8Sx8 = auto()
    Max16Sx4 = auto()
    Max32Sx2 = auto()
    Max8Ux8 = auto()
    Max16Ux4 = auto()
    Max32Ux2 = auto()
    Min8Sx8 = auto()
    Min16Sx4 = auto()
    Min32Sx2 = auto()
    Min8Ux8 = auto()
    Min16Ux4 = auto()
    Min32Ux2 = auto()

    # COMPARISON

    CmpEQ8x8 = auto()
    CmpEQ16x4 = auto()
    CmpEQ32x2 = auto()
    CmpGT8Ux8 = auto()
    CmpGT16Ux4 = auto()
    CmpGT32Ux2 = auto()
    CmpGT8Sx8 = auto()
    CmpGT16Sx4 = auto()
    CmpGT32Sx2 = auto()

    # COUNT ones / leading zeroes / leading sign bits (not including topmost
    # bit)

    Cnt8x8 = auto()
    Clz8x8 = auto()
    Clz16x4 = auto()
    Clz32x2 = auto()
    Cls8x8 = auto()
    Cls16x4 = auto()
    Cls32x2 = auto()
    Clz64x2 = auto()

    # Vector COUNT trailing zeros

    Ctz8x16 = auto()
    Ctz16x8 = auto()
    Ctz32x4 = auto()
    Ctz64x2 = auto()

    # VECTOR x VECTOR SHIFT / ROTATE

    Shl8x8 = auto()
    Shl16x4 = auto()
    Shl32x2 = auto()
    Shr8x8 = auto()
    Shr16x4 = auto()
    Shr32x2 = auto()
    Sar8x8 = auto()
    Sar16x4 = auto()
    Sar32x2 = auto()
    Sal8x8 = auto()
    Sal16x4 = auto()
    Sal32x2 = auto()
    Sal64x1 = auto()

    # VECTOR x SCALAR SHIFT (shift amt :: Ity_I8)

    ShlN8x8 = auto()
    ShlN16x4 = auto()
    ShlN32x2 = auto()
    ShrN8x8 = auto()
    ShrN16x4 = auto()
    ShrN32x2 = auto()
    SarN8x8 = auto()
    SarN16x4 = auto()
    SarN32x2 = auto()

    # VECTOR x VECTOR SATURATING SHIFT

    QShl8x8 = auto()
    QShl16x4 = auto()
    QShl32x2 = auto()
    QShl64x1 = auto()
    QSal8x8 = auto()
    QSal16x4 = auto()
    QSal32x2 = auto()
    QSal64x1 = auto()  # VECTOR x INTEGER SATURATING SHIFT

    QShlNsatSU8x8 = auto()
    QShlNsatSU16x4 = auto()
    QShlNsatSU32x2 = auto()
    QShlNsatSU64x1 = auto()
    QShlNsatUU8x8 = auto()
    QShlNsatUU16x4 = auto()
    QShlNsatUU32x2 = auto()
    QShlNsatUU64x1 = auto()
    QShlNsatSS8x8 = auto()
    QShlNsatSS16x4 = auto()
    QShlNsatSS32x2 = auto()
    QShlNsatSS64x1 = auto()

    # NARROWING (binary)
    # -- narrow 2xI64 into 1xI64, hi half from left arg
    # For saturated narrowing, I believe there are 4 variants of
    # the basic arithmetic operation, depending on the signedness
    # of argument and result.  Here are examples that exemplify
    # what I mean:
    # QNarrow16Uto8U ( UShort x )  if (x >u 255) x = 255;
    # return x[7:0];
    # QNarrow16Sto8S ( Short x )   if (x <s -128) x = -128;
    # if (x >s  127) x = 127;
    # return x[7:0];
    # QNarrow16Uto8S ( UShort x )  if (x >u 127) x = 127;
    # return x[7:0];
    # QNarrow16Sto8U ( Short x )   if (x <s 0)   x = 0;
    # if (x >s 255) x = 255;
    # return x[7:0];

    QNarrowBin16Sto8Ux8 = auto()
    QNarrowBin16Sto8Sx8 = auto()
    QNarrowBin32Sto16Sx4 = auto()
    NarrowBin16to8x8 = auto()
    NarrowBin32to16x4 = auto()

    # INTERLEAVING
    # Interleave lanes from low or high halves of
    # operands.  Most-significant result lane is from the left
    # arg.

    InterleaveHI8x8 = auto()
    InterleaveHI16x4 = auto()
    InterleaveHI32x2 = auto()
    InterleaveLO8x8 = auto()
    InterleaveLO16x4 = auto()
    InterleaveLO32x2 = auto()  # Interleave odd/even lanes of operands.  Most-significant result lane  is from the left arg.  Note that Interleave{Odd,Even}Lanes32x2 are  identical to Interleave{HI,LO}32x2 and so are omitted.

    InterleaveOddLanes8x8 = auto()
    InterleaveEvenLanes8x8 = auto()
    InterleaveOddLanes16x4 = auto()
    InterleaveEvenLanes16x4 = auto()

    # CONCATENATION -- build a new value by concatenating either
    # the even or odd lanes of both operands.  Note that
    # Cat{Odd,Even}Lanes32x2 are identical to Interleave{HI,LO}32x2
    # and so are omitted.

    CatOddLanes8x8 = auto()
    CatOddLanes16x4 = auto()
    CatEvenLanes8x8 = auto()
    CatEvenLanes16x4 = auto()

    # GET / SET elements of VECTOR
    # GET is binop (I64, I8) -> I<elem_size>
    # SET is triop (I64, I8, I<elem_size>) -> I64
    # Note: the arm back-end handles only constant second argument

    GetElem8x8 = auto()
    GetElem16x4 = auto()
    GetElem32x2 = auto()
    SetElem8x8 = auto()
    SetElem16x4 = auto()
    SetElem32x2 = auto()

    # DUPLICATING -- copy value to all lanes

    Dup8x8 = auto()
    Dup16x4 = auto()
    Dup32x2 = auto()

    # SLICE -- produces the lowest 64 bits of (arg1:arg2) >> (8 * arg3).
    # arg3 is a shift amount in bytes and may be between 0 and 8
    # inclusive.  When 0, the result is arg2; when 8, the result is arg1.
    # Not all back ends handle all values.  The arm32 and arm64 back
    # ends handle only immediate arg3 values.

    Slice64 = auto()  # (I64, I64, I8) -> I64

    # REVERSE the order of chunks in vector lanes.  Chunks must be
    # smaller than the vector lanes (obviously) and so may be 8-, 16- and
    # 32-bit in size.  Note that the degenerate case,
    # Iop_Reverse8sIn64_x1, is a simply a vanilla byte-swap.
    # Examples:
    # Reverse8sIn16_x4([a,b,c,d,e,f,g,h]) = [b,a,d,c,f,e,h,g]
    # Reverse8sIn32_x2([a,b,c,d,e,f,g,h]) = [d,c,b,a,h,g,f,e]
    # Reverse8sIn64_x1([a,b,c,d,e,f,g,h]) = [h,g,f,e,d,c,b,a]

    Reverse8sIn16_x4 = auto()
    Reverse8sIn32_x2 = auto()
    Reverse16sIn32_x2 = auto()
    Reverse8sIn64_x1 = auto()
    Reverse16sIn64_x1 = auto()
    Reverse32sIn64_x1 = auto()

    # PERMUTING -- copy src bytes to dst,
    # as indexed by control vector bytes:
    # for i in 0 .. 7 . result[i] = argL[ argR[i] ]
    # argR[i] values may only be in the range 0 .. 7, else behaviour
    # is undefined.  That is, argR[i][7:3] must be zero.

    Perm8x8 = auto()

    # PERMUTING with optional zeroing:
    # for i in 0 .. 7 . result[i] = if argR[i] bit 7 is set
    # then zero else argL[ argR[i] ]
    # argR[i][6:3] must be zero, else behaviour is undefined.

    PermOrZero8x8 = auto()

    # MISC CONVERSION -- get high bits of each byte lane, a la
    # x86/amd64 pmovmskb

    GetMSBs8x8 = auto()  # I64 -> I8
    # Vector Reciprocal Estimate and Vector Reciprocal Square Root Estimate
    # See floating-point equivalents for details.

    RecipEst32Ux2 = auto()
    RSqrtEst32Ux2 = auto()

    # ------------------ Decimal Floating Point ------------------
    # ARITHMETIC INSTRUCTIONS   64-bit
    # ----------------------------------
    # IRRoundingMode(I32) X D64 X D64 -> D64

    AddD64 = auto()
    SubD64 = auto()
    MulD64 = auto()
    DivD64 = auto()

    # ARITHMETIC INSTRUCTIONS  128-bit
    # ----------------------------------
    # IRRoundingMode(I32) X D128 X D128 -> D128

    AddD128 = auto()
    SubD128 = auto()
    MulD128 = auto()
    DivD128 = auto()

    # SHIFT SIGNIFICAND INSTRUCTIONS
    # The DFP significand is shifted by the number of digits specified
    # by the U8 operand.  Digits shifted out of the leftmost digit are
    # lost. Zeros are supplied to the vacated positions on the right.
    # The sign of the result is the same as the sign of the original
    # operand.
    # D64 x U8  -> D64    left shift and right shift respectively

    ShlD64 = auto()
    ShrD64 = auto()

    # D128 x U8  -> D128  left shift and right shift respectively

    ShlD128 = auto()
    ShrD128 = auto()

    # FORMAT CONVERSION INSTRUCTIONS
    # D32 -> D64

    D32toD64 = auto()

    # D64 -> D128

    D64toD128 = auto()

    # I32S -> D128

    I32StoD128 = auto()

    # I32U -> D128

    I32UtoD128 = auto()

    # I64S -> D128

    I64StoD128 = auto()

    # I64U -> D128

    I64UtoD128 = auto()

    # IRRoundingMode(I32) x I128S -> D128

    I128StoD128 = auto()

    # IRRoundingMode(I32) x D64 -> D32

    D64toD32 = auto()

    # IRRoundingMode(I32) x D128 -> D64

    D128toD64 = auto()

    # I32S -> D64

    I32StoD64 = auto()

    # I32U -> D64

    I32UtoD64 = auto()

    # IRRoundingMode(I32) x I64 -> D64

    I64StoD64 = auto()

    # IRRoundingMode(I32) x I64 -> D64

    I64UtoD64 = auto()

    # IRRoundingMode(I32) x D64 -> I32

    D64toI32S = auto()

    # IRRoundingMode(I32) x D64 -> I32

    D64toI32U = auto()

    # IRRoundingMode(I32) x D64 -> I64

    D64toI64S = auto()

    # IRRoundingMode(I32) x D64 -> I64

    D64toI64U = auto()

    # IRRoundingMode(I32) x D128 -> I32

    D128toI32S = auto()

    # IRRoundingMode(I32) x D128 -> I32

    D128toI32U = auto()

    # IRRoundingMode(I32) x D128 -> I64

    D128toI64S = auto()

    # IRRoundingMode(I32) x D128 -> I64

    D128toI64U = auto()

    # IRRoundingMode(I32) x D128 -> I128

    D128toI128S = auto()

    # IRRoundingMode(I32) x F32 -> D32

    F32toD32 = auto()

    # IRRoundingMode(I32) x F32 -> D64

    F32toD64 = auto()

    # IRRoundingMode(I32) x F32 -> D128

    F32toD128 = auto()

    # IRRoundingMode(I32) x F64 -> D32

    F64toD32 = auto()

    # IRRoundingMode(I32) x F64 -> D64

    F64toD64 = auto()

    # IRRoundingMode(I32) x F64 -> D128

    F64toD128 = auto()

    # IRRoundingMode(I32) x F128 -> D32

    F128toD32 = auto()

    # IRRoundingMode(I32) x F128 -> D64

    F128toD64 = auto()

    # IRRoundingMode(I32) x F128 -> D128

    F128toD128 = auto()

    # IRRoundingMode(I32) x D32 -> F32

    D32toF32 = auto()

    # IRRoundingMode(I32) x D32 -> F64

    D32toF64 = auto()

    # IRRoundingMode(I32) x D32 -> F128

    D32toF128 = auto()

    # IRRoundingMode(I32) x D64 -> F32

    D64toF32 = auto()

    # IRRoundingMode(I32) x D64 -> F64

    D64toF64 = auto()

    # IRRoundingMode(I32) x D64 -> F128

    D64toF128 = auto()

    # IRRoundingMode(I32) x D128 -> F32

    D128toF32 = auto()

    # IRRoundingMode(I32) x D128 -> F64

    D128toF64 = auto()

    # IRRoundingMode(I32) x D128 -> F128

    D128toF128 = auto()

    # ROUNDING INSTRUCTIONS
    # IRRoundingMode(I32) x D64 -> D64
    # The D64 operand, if a finite number, it is rounded to a
    # floating point integer value, i.e. no fractional part.

    RoundD64toInt = auto()

    # IRRoundingMode(I32) x D128 -> D128

    RoundD128toInt = auto()

    # COMPARE INSTRUCTIONS
    # D64 x D64 -> IRCmpD64Result(I32)

    CmpD64 = auto()

    # D128 x D128 -> IRCmpD128Result(I32)

    CmpD128 = auto()

    # COMPARE BIASED EXPONENET INSTRUCTIONS
    # D64 x D64 -> IRCmpD64Result(I32)

    CmpExpD64 = auto()

    # D128 x D128 -> IRCmpD128Result(I32)

    CmpExpD128 = auto()

    # QUANTIZE AND ROUND INSTRUCTIONS
    # The source operand is converted and rounded to the form with the
    # immediate exponent specified by the rounding and exponent parameter.
    # The second operand is converted and rounded to the form
    # of the first operand's exponent and the rounded based on the specified
    # rounding mode parameter.
    # IRRoundingMode(I32) x D64 x D64-> D64

    QuantizeD64 = auto()

    # IRRoundingMode(I32) x D128 x D128 -> D128

    QuantizeD128 = auto()

    # IRRoundingMode(I32) x I8 x D64 -> D64
    # The Decimal Floating point operand is rounded to the requested
    # significance given by the I8 operand as specified by the rounding
    # mode.

    SignificanceRoundD64 = auto()

    # IRRoundingMode(I32) x I8 x D128 -> D128

    SignificanceRoundD128 = auto()

    # EXTRACT AND INSERT INSTRUCTIONS
    # D64 -> I64
    # The exponent of the D32 or D64 operand is extracted.  The
    # extracted exponent is converted to a 64-bit signed binary integer.

    ExtractExpD64 = auto()

    # D128 -> I64

    ExtractExpD128 = auto()

    # D64 -> I64
    # The number of significand digits of the D64 operand is extracted.
    # The number is stored as a 64-bit signed binary integer.

    ExtractSigD64 = auto()

    # D128 -> I64

    ExtractSigD128 = auto()

    # I64 x D64  -> D64
    # The exponent is specified by the first I64 operand the signed
    # significand is given by the second I64 value.  The result is a D64
    # value consisting of the specified significand and exponent whose
    # sign is that of the specified significand.

    InsertExpD64 = auto()

    # I64 x D128 -> D128

    InsertExpD128 = auto()

    # Support for 128-bit DFP type

    D64HLtoD128 = auto()
    D128HItoD64 = auto()
    D128LOtoD64 = auto()

    # I64 -> I64
    # Convert 50-bit densely packed BCD string to 60 bit BCD string

    DPBtoBCD = auto()

    # I64 -> I64
    # Convert 60 bit BCD string to 50-bit densely packed BCD string

    BCDtoDPB = auto()

    # BCD arithmetic instructions, (V128, V128) -> V128
    # The BCD format is the same as that used in the BCD<->DPB conversion
    # routines, except using 124 digits (vs 60) plus the trailing 4-bit
    # signed code.

    BCDAdd = auto()
    BCDSub = auto()

    # Conversion signed 128-bit integer to signed BCD 128-bit

    I128StoBCD128 = auto()

    # Conversion signed BCD 128-bit to 128-bit integer

    BCD128toI128S = auto()

    # Conversion I64 -> D64

    ReinterpI64asD64 = auto()

    # Conversion D64 -> I64

    ReinterpD64asI64 = auto()

    # ------------------ 128-bit SIMD FP. ------------------
    # --- 16x8 vector FP ---
    # binary :: IRRoundingMode(I32) x V128 -> V128

    Sqrt16Fx8 = auto()

    # ternary :: IRRoundingMode(I32) x V128 x V128 -> V128

    Add16Fx8 = auto()
    Sub16Fx8 = auto()

    # binary

    CmpLT16Fx8 = auto()
    CmpLE16Fx8 = auto()
    CmpEQ16Fx8 = auto()

    # unary

    Abs16Fx8 = auto()
    Neg16Fx8 = auto()

    # --- 32x4 vector FP ---
    # ternary :: IRRoundingMode(I32) x V128 x V128 -> V128

    Add32Fx4 = auto()
    Sub32Fx4 = auto()
    Mul32Fx4 = auto()
    Div32Fx4 = auto()

    # binary

    Max32Fx4 = auto()
    Min32Fx4 = auto()
    Add32Fx2 = auto()
    Sub32Fx2 = auto()  # Note: For the following compares, the ppc and arm front-ends assume a  nan in a lane of either argument returns zero for that lane.

    CmpEQ32Fx4 = auto()
    CmpLT32Fx4 = auto()
    CmpLE32Fx4 = auto()
    CmpUN32Fx4 = auto()
    CmpGT32Fx4 = auto()
    CmpGE32Fx4 = auto()

    # Pairwise Max and Min. See integer pairwise operations for details.

    PwMax32Fx4 = auto()
    PwMin32Fx4 = auto()

    # unary

    Abs32Fx4 = auto()
    Neg32Fx4 = auto()

    # binary :: IRRoundingMode(I32) x V128 -> V128

    Sqrt32Fx4 = auto()

    # Vector Reciprocal Estimate finds an approximate reciprocal of each
    # element in the operand vector, and places the results in the
    # destination vector.

    RecipEst32Fx4 = auto()

    # Vector Reciprocal Step computes (2.0 - arg1 * arg2).
    # Note, that if one of the arguments is zero and another one is infinity
    # of arbitrary sign the result of the operation is 2.0.

    RecipStep32Fx4 = auto()

    # Vector Reciprocal Square Root Estimate finds an approximate reciprocal
    # square root of each element in the operand vector.

    RSqrtEst32Fx4 = auto()

    # Scaling of vector with a power of 2  (wd[i] <- ws[i] * 2^wt[i])

    Scale2_32Fx4 = auto()

    # Vector floating-point base 2 logarithm

    Log2_32Fx4 = auto()

    # Vector floating-point exponential 2^x

    Exp2_32Fx4 = auto()

    # Vector Reciprocal Square Root Step computes (3.0 - arg1 * arg2) / 2.0.
    # Note, that of one of the arguments is zero and another one is infiinty
    # of arbitrary sign the result of the operation is 1.5.

    RSqrtStep32Fx4 = auto()

    # --- Int to/from FP conversion ---
    # Unlike the standard fp conversions, these irops take no
    # rounding mode argument. Instead the irop trailers _R{M,P,N,Z}
    # indicate the mode: {-inf, +inf, nearest, zero} respectively.

    I32UtoF32x4_DEP = auto()
    I32StoF32x4_DEP = auto()  # I32x4 -> F32x4

    I32StoF32x4 = auto()  # IRRoundingMode(I32) x V128 -> V128

    F32toI32Sx4 = auto()  # IRRoundingMode(I32) x V128 -> V128

    F32toI32Ux4_RZ = auto()
    F32toI32Sx4_RZ = auto()  # F32x4 -> I32x4

    QF32toI32Ux4_RZ = auto()
    QF32toI32Sx4_RZ = auto()  # F32x4 -> I32x4 (saturating)

    RoundF32x4_RM = auto()
    RoundF32x4_RP = auto()  # round to fp integer

    RoundF32x4_RN = auto()
    RoundF32x4_RZ = auto()  # round to fp integer
    # Fixed32 format is floating-point number with fixed number of fraction
    # bits. The number of fraction bits is passed as a second argument of
    # type I8.

    F32ToFixed32Ux4_RZ = auto()
    F32ToFixed32Sx4_RZ = auto()  # fp -> fixed-point

    Fixed32UToF32x4_RN = auto()
    Fixed32SToF32x4_RN = auto()  # fixed-point -> fp
    # --- Single to/from half conversion ---
    # FIXME: what kind of rounding in F32x4 -> F16x4 case?

    F32toF16x4_DEP = auto()  # F32x4(==V128) -> F16x4(==I64), NO ROUNDING MODE

    F32toF16x4 = auto()  # IRRoundingMode(I32) x V128 -> I64

    F16toF32x4 = auto()  # F16x4 -> F32x4
    # -- Double to/from half conversion --

    F64toF16x2_DEP = auto()  # F64x2 -> F16x2, NO ROUNDING MODE
    F16toF64x2 = auto()

    # Values from two registers converted in smaller type and put in one
    # IRRoundingMode(I32) x (F32x4 | F32x4) -> Q16x8

    F32x4_2toQ16x8 = auto()

    # --- 32x4 lowest-lane-only scalar FP ---
    # In binary cases, upper 3/4 is copied from first operand.  In
    # unary cases, upper 3/4 is copied from the operand.
    # binary

    Add32F0x4 = auto()
    Sub32F0x4 = auto()
    Mul32F0x4 = auto()
    Div32F0x4 = auto()
    Max32F0x4 = auto()
    Min32F0x4 = auto()
    CmpEQ32F0x4 = auto()
    CmpLT32F0x4 = auto()
    CmpLE32F0x4 = auto()
    CmpUN32F0x4 = auto()

    # unary

    RecipEst32F0x4 = auto()
    Sqrt32F0x4 = auto()
    RSqrtEst32F0x4 = auto()

    # --- 64x2 vector FP ---
    # ternary :: IRRoundingMode(I32) x V128 x V128 -> V128

    Add64Fx2 = auto()
    Sub64Fx2 = auto()
    Mul64Fx2 = auto()
    Div64Fx2 = auto()

    # binary

    Max64Fx2 = auto()
    Min64Fx2 = auto()
    CmpEQ64Fx2 = auto()
    CmpLT64Fx2 = auto()
    CmpLE64Fx2 = auto()
    CmpUN64Fx2 = auto()

    # unary

    Abs64Fx2 = auto()
    Neg64Fx2 = auto()

    # binary :: IRRoundingMode(I32) x V128 -> V128

    Sqrt64Fx2 = auto()

    # Scaling of vector with a power of 2  (wd[i] <- ws[i] * 2^wt[i])

    Scale2_64Fx2 = auto()

    # Vector floating-point base 2 logarithm

    Log2_64Fx2 = auto()

    # see 32Fx4 variants for description

    RecipEst64Fx2 = auto()  # unary
    RecipStep64Fx2 = auto()  # binary
    RSqrtEst64Fx2 = auto()  # unary
    RSqrtStep64Fx2 = auto()  # binary

    # Values from two registers converted in smaller type and put in one
    # IRRoundingMode(I32) x (F64x2 | F64x2) -> Q32x4

    F64x2_2toQ32x4 = auto()

    # --- 64x2 lowest-lane-only scalar FP ---
    # In binary cases, upper half is copied from first operand.  In
    # unary cases, upper half is copied from the operand.
    # binary

    Add64F0x2 = auto()
    Sub64F0x2 = auto()
    Mul64F0x2 = auto()
    Div64F0x2 = auto()
    Max64F0x2 = auto()
    Min64F0x2 = auto()
    CmpEQ64F0x2 = auto()
    CmpLT64F0x2 = auto()
    CmpLE64F0x2 = auto()
    CmpUN64F0x2 = auto()

    # unary

    Sqrt64F0x2 = auto()

    # --- pack / unpack ---
    # 64 <-> 128 bit vector

    ExtractV128to64 = auto()  # :: V128 -> I64, low half
    ExtractV128HIto64 = auto()  # :: V128 -> I64, high half
    Pack64HLtoV128 = auto()  # :: (I64,I64) -> V128

    Pack64UtoV128 = auto()
    SetV128lo64 = auto()

    # Copies lower 64/32/16/8 bits, zeroes out the rest.

    ZeroHI64ofV128 = auto()  # :: V128 -> V128
    ZeroHI96ofV128 = auto()  # :: V128 -> V128
    ZeroHI112ofV128 = auto()  # :: V128 -> V128
    ZeroHI120ofV128 = auto()  # :: V128 -> V128

    # 32 <-> 128 bit vector

    Pack32UtoV128 = auto()
    ExtractV128to32 = auto()  # :: V128 -> I32, lowest lane
    SetV128lo32 = auto()  # :: (V128,I32) -> V128

    # ------------------ 128-bit SIMD Integer. ------------------
    # BITWISE OPS

    NotV128 = auto()
    AndV128 = auto()
    OrV128 = auto()
    XorV128 = auto()

    # VECTOR SHIFT (shift amt :: Ity_I8)

    ShlV128 = auto()
    ShrV128 = auto()
    SarV128 = auto()

    # MISC (vector integer cmp != 0)

    CmpNEZ8x16 = auto()
    CmpNEZ16x8 = auto()
    CmpNEZ32x4 = auto()
    CmpNEZ64x2 = auto()
    CmpNEZ128x1 = auto()

    # ADDITION (normal / U->U sat / S->S sat)

    Add8x16 = auto()
    Add16x8 = auto()
    Add32x4 = auto()
    Add64x2 = auto()
    Add128x1 = auto()
    QAdd8Ux16 = auto()
    QAdd16Ux8 = auto()
    QAdd32Ux4 = auto()
    QAdd64Ux2 = auto()
    QAdd8Sx16 = auto()
    QAdd16Sx8 = auto()
    QAdd32Sx4 = auto()
    QAdd64Sx2 = auto()

    # ADDITION, ARM64 specific saturating variants.
    # Unsigned widen left arg, signed widen right arg, add, saturate S->S.
    # This corresponds to SUQADD.

    QAddExtUSsatSS8x16 = auto()
    QAddExtUSsatSS16x8 = auto()
    QAddExtUSsatSS32x4 = auto()
    QAddExtUSsatSS64x2 = auto()  # Signed widen left arg, unsigned widen right arg, add, saturate U->U.  This corresponds to USQADD.

    QAddExtSUsatUU8x16 = auto()
    QAddExtSUsatUU16x8 = auto()
    QAddExtSUsatUU32x4 = auto()
    QAddExtSUsatUU64x2 = auto()

    # SUBTRACTION (normal / unsigned sat / signed sat)

    Sub8x16 = auto()
    Sub16x8 = auto()
    Sub32x4 = auto()
    Sub64x2 = auto()
    Sub128x1 = auto()
    QSub8Ux16 = auto()
    QSub16Ux8 = auto()
    QSub32Ux4 = auto()
    QSub64Ux2 = auto()
    QSub8Sx16 = auto()
    QSub16Sx8 = auto()
    QSub32Sx4 = auto()
    QSub64Sx2 = auto()

    # MULTIPLICATION (normal / high half of signed/unsigned)

    Mul8x16 = auto()
    Mul16x8 = auto()
    Mul32x4 = auto()
    MulHi8Ux16 = auto()
    MulHi16Ux8 = auto()
    MulHi32Ux4 = auto()
    MulHi8Sx16 = auto()
    MulHi16Sx8 = auto()
    MulHi32Sx4 = auto()  # (widening signed/unsigned of even lanes, with lowest lane=zero)

    MullEven8Ux16 = auto()
    MullEven16Ux8 = auto()
    MullEven32Ux4 = auto()
    MullEven8Sx16 = auto()
    MullEven16Sx8 = auto()
    MullEven32Sx4 = auto()

    # Widening multiplies, all of the form (I64, I64) -> V128

    Mull8Ux8 = auto()
    Mull8Sx8 = auto()
    Mull16Ux4 = auto()
    Mull16Sx4 = auto()
    Mull32Ux2 = auto()
    Mull32Sx2 = auto()

    # Signed doubling saturating widening multiplies, (I64, I64) -> V128

    QDMull16Sx4 = auto()
    QDMull32Sx2 = auto()

    # Vector Saturating Doubling Multiply Returning High Half and
    # Vector Saturating Rounding Doubling Multiply Returning High Half.
    # These IROps multiply corresponding elements in two vectors, double
    # the results, and place the most significant half of the final results
    # in the destination vector.  The results are truncated or rounded.  If
    # any of the results overflow, they are saturated.  To be more precise,
    # for each lane, the computed result is:
    # QDMulHi:
    # hi-half( sign-extend(laneL) *q sign-extend(laneR) *q 2 )
    # QRDMulHi:
    # hi-half( sign-extend(laneL) *q sign-extend(laneR) *q 2
    # +q (1 << (lane-width-in-bits - 1)) )

    QDMulHi16Sx8 = auto()
    QDMulHi32Sx4 = auto()  # (V128, V128) -> V128

    QRDMulHi16Sx8 = auto()
    QRDMulHi32Sx4 = auto()  # (V128, V128) -> V128
    # Polynomial multiplication treats its arguments as
    # coefficients of polynomials over {0, 1}.

    PolynomialMul8x16 = auto()  # (V128, V128) -> V128

    PolynomialMull8x8 = auto()  # (I64, I64) -> V128
    # Vector Polynomial multiplication add.   (V128, V128) -> V128
    # Below is the algorithm for the instructions. These Iops could
    # be emulated to get this functionality, but the emulation would
    # be long and messy.
    # Example for polynomial multiply add for vector of bytes
    # do i = 0 to 15
    # prod[i].bit[0:14] <- 0
    # srcA <- VR[argL].byte[i]
    # srcB <- VR[argR].byte[i]
    # do j = 0 to 7
    # do k = 0 to j
    # gbit <- srcA.bit[k] & srcB.bit[j-k]
    # prod[i].bit[j] <- prod[i].bit[j] ^ gbit
    # end
    # end
    # do j = 8 to 14
    # do k = j-7 to 7
    # gbit <- (srcA.bit[k] & srcB.bit[j-k])
    # prod[i].bit[j] <- prod[i].bit[j] ^ gbit
    # end
    # end
    # end
    # do i = 0 to 7
    # VR[dst].hword[i] <- 0b0 || (prod[2×i] ^ prod[2×i+1])
    # end

    PolynomialMulAdd8x16 = auto()
    PolynomialMulAdd16x8 = auto()
    PolynomialMulAdd32x4 = auto()
    PolynomialMulAdd64x2 = auto()

    # PAIRWISE operations
    # Iop_PwFoo16x4( [a,b,c,d], [e,f,g,h] ) =
    # [Foo16(a,b), Foo16(c,d), Foo16(e,f), Foo16(g,h)]

    PwAdd8x16 = auto()
    PwAdd16x8 = auto()
    PwAdd32x4 = auto()
    PwAdd32Fx2 = auto()

    # Longening variant is unary. The resulting vector contains two times
    # less elements than operand, but they are two times wider.
    # Example:
    # Iop_PwAddL16Ux4( [a,b,c,d] ) = [a+b,c+d]
    # where a+b and c+d are unsigned 32-bit values.

    PwAddL8Ux16 = auto()
    PwAddL16Ux8 = auto()
    PwAddL32Ux4 = auto()
    PwAddL64Ux2 = auto()
    PwAddL8Sx16 = auto()
    PwAddL16Sx8 = auto()
    PwAddL32Sx4 = auto()

    # This is amd64 PMADDUBSW, (V128, V128) -> V128.  For each adjacent pair
    # of bytes [a,b] in the first arg and [c,d] in the second, computes:
    # signed/signed sat to 16 bits ( zxTo16(a) * sxTo16(b)
    # + zxTo16(c) * sxTo16(d) )
    # This exists because it's frequently used and there's no reasonably
    # concise way to express it using other IROps.

    PwExtUSMulQAdd8x16 = auto()

    # Other unary pairwise ops
    # Vector bit matrix transpose.  (V128) -> V128
    # For each doubleword element of the source vector, an 8-bit x 8-bit
    # matrix transpose is performed.

    PwBitMtxXpose64x2 = auto()

    # ABSOLUTE VALUE

    Abs8x16 = auto()
    Abs16x8 = auto()
    Abs32x4 = auto()
    Abs64x2 = auto()

    # AVERAGING: note: (arg1 + arg2 + 1) >>u 1

    Avg8Ux16 = auto()
    Avg16Ux8 = auto()
    Avg32Ux4 = auto()
    Avg64Ux2 = auto()
    Avg8Sx16 = auto()
    Avg16Sx8 = auto()
    Avg32Sx4 = auto()
    Avg64Sx2 = auto()

    # MIN/MAX

    Max8Sx16 = auto()
    Max16Sx8 = auto()
    Max32Sx4 = auto()
    Max64Sx2 = auto()
    Max8Ux16 = auto()
    Max16Ux8 = auto()
    Max32Ux4 = auto()
    Max64Ux2 = auto()
    Min8Sx16 = auto()
    Min16Sx8 = auto()
    Min32Sx4 = auto()
    Min64Sx2 = auto()
    Min8Ux16 = auto()
    Min16Ux8 = auto()
    Min32Ux4 = auto()
    Min64Ux2 = auto()

    # COMPARISON

    CmpEQ8x16 = auto()
    CmpEQ16x8 = auto()
    CmpEQ32x4 = auto()
    CmpEQ64x2 = auto()
    CmpGT8Sx16 = auto()
    CmpGT16Sx8 = auto()
    CmpGT32Sx4 = auto()
    CmpGT64Sx2 = auto()
    CmpGT8Ux16 = auto()
    CmpGT16Ux8 = auto()
    CmpGT32Ux4 = auto()
    CmpGT64Ux2 = auto()

    # COUNT ones / leading zeroes / leading sign bits (not including topmost
    # bit)

    Cnt8x16 = auto()
    Clz8x16 = auto()
    Clz16x8 = auto()
    Clz32x4 = auto()
    Cls8x16 = auto()
    Cls16x8 = auto()
    Cls32x4 = auto()

    # VECTOR x SCALAR SHIFT (shift amt :: Ity_I8)

    ShlN8x16 = auto()
    ShlN16x8 = auto()
    ShlN32x4 = auto()
    ShlN64x2 = auto()
    ShrN8x16 = auto()
    ShrN16x8 = auto()
    ShrN32x4 = auto()
    ShrN64x2 = auto()
    SarN8x16 = auto()
    SarN16x8 = auto()
    SarN32x4 = auto()
    SarN64x2 = auto()

    # VECTOR x VECTOR SHIFT / ROTATE
    # FIXME: I'm pretty sure the ARM32 front/back ends interpret these
    # differently from all other targets.  The intention is that
    # the shift amount (2nd arg) is interpreted as unsigned and
    # only the lowest log2(lane-bits) bits are relevant.  But the
    # ARM32 versions treat the shift amount as an 8 bit signed
    # number.  The ARM32 uses should be replaced by the relevant
    # vector x vector bidirectional shifts instead.

    Shl8x16 = auto()
    Shl16x8 = auto()
    Shl32x4 = auto()
    Shl64x2 = auto()
    Shr8x16 = auto()
    Shr16x8 = auto()
    Shr32x4 = auto()
    Shr64x2 = auto()
    Sar8x16 = auto()
    Sar16x8 = auto()
    Sar32x4 = auto()
    Sar64x2 = auto()
    Sal8x16 = auto()
    Sal16x8 = auto()
    Sal32x4 = auto()
    Sal64x2 = auto()
    Rol8x16 = auto()
    Rol16x8 = auto()
    Rol32x4 = auto()
    Rol64x2 = auto()

    # VECTOR x VECTOR SATURATING SHIFT

    QShl8x16 = auto()
    QShl16x8 = auto()
    QShl32x4 = auto()
    QShl64x2 = auto()
    QSal8x16 = auto()
    QSal16x8 = auto()
    QSal32x4 = auto()
    QSal64x2 = auto()  # VECTOR x INTEGER SATURATING SHIFT

    QShlNsatSU8x16 = auto()
    QShlNsatSU16x8 = auto()
    QShlNsatSU32x4 = auto()
    QShlNsatSU64x2 = auto()
    QShlNsatUU8x16 = auto()
    QShlNsatUU16x8 = auto()
    QShlNsatUU32x4 = auto()
    QShlNsatUU64x2 = auto()
    QShlNsatSS8x16 = auto()
    QShlNsatSS16x8 = auto()
    QShlNsatSS32x4 = auto()
    QShlNsatSS64x2 = auto()

    # VECTOR x VECTOR BIDIRECTIONAL SATURATING (& MAYBE ROUNDING) SHIFT
    # All of type (V128, V128) -> V256.
    # The least significant 8 bits of each lane of the second
    # operand are used as the shift amount, and interpreted signedly.
    # Positive values mean a shift left, negative a shift right.  The
    # result is signedly or unsignedly saturated.  There are also
    # rounding variants, which add 2^(shift_amount-1) to the value before
    # shifting, but only in the shift-right case.  Vacated positions
    # are filled with zeroes.  IOW, it's either SHR or SHL, but not SAR.
    # These operations return 129 bits: one bit ("Q") indicating whether
    # saturation occurred, and the shift result.  The result type is V256,
    # of which the lower V128 is the shift result, and Q occupies the
    # least significant bit of the upper V128.  All other bits of the
    # upper V128 are zero.

    QandUQsh8x16 = auto()
    QandUQsh16x8 = auto()
    QandUQsh32x4 = auto()
    QandUQsh64x2 = auto()
    QandSQsh8x16 = auto()
    QandSQsh16x8 = auto()
    QandSQsh32x4 = auto()
    QandSQsh64x2 = auto()

    QandUQRsh8x16 = auto()
    QandUQRsh16x8 = auto()
    QandUQRsh32x4 = auto()
    QandUQRsh64x2 = auto()
    QandSQRsh8x16 = auto()
    QandSQRsh16x8 = auto()
    QandSQRsh32x4 = auto()
    QandSQRsh64x2 = auto()

    # VECTOR x VECTOR BIDIRECTIONAL (& MAYBE ROUNDING) SHIFT
    # All of type (V128, V128) -> V128
    # The least significant 8 bits of each lane of the second
    # operand are used as the shift amount, and interpreted signedly.
    # Positive values mean a shift left, negative a shift right.
    # There are also rounding variants, which add 2^(shift_amount-1)
    # to the value before shifting, but only in the shift-right case.
    # For left shifts, the vacated places are filled with zeroes.
    # For right shifts, the vacated places are filled with zeroes
    # for the U variants and sign bits for the S variants.

    Sh8Sx16 = auto()
    Sh16Sx8 = auto()
    Sh32Sx4 = auto()
    Sh64Sx2 = auto()
    Sh8Ux16 = auto()
    Sh16Ux8 = auto()
    Sh32Ux4 = auto()
    Sh64Ux2 = auto()

    Rsh8Sx16 = auto()
    Rsh16Sx8 = auto()
    Rsh32Sx4 = auto()
    Rsh64Sx2 = auto()
    Rsh8Ux16 = auto()
    Rsh16Ux8 = auto()
    Rsh32Ux4 = auto()
    Rsh64Ux2 = auto()

    # The least significant 8 bits of each lane of the second
    # operand are used as the shift amount, and interpreted signedly.
    # Positive values mean a shift left, negative a shift right.  The
    # result is signedly or unsignedly saturated.  There are also
    # rounding variants, which add 2^(shift_amount-1) to the value before
    # shifting, but only in the shift-right case.  Vacated positions
    # are filled with zeroes.  IOW, it's either SHR or SHL, but not SAR.
    # VECTOR x SCALAR SATURATING (& MAYBE ROUNDING) NARROWING SHIFT RIGHT
    # All of type (V128, I8) -> V128
    # The first argument is shifted right, then narrowed to half the width
    # by saturating it.  The second argument is a scalar shift amount that
    # applies to all lanes, and must be a value in the range 1 to lane_width.
    # The shift may be done signedly (Sar variants) or unsignedly (Shr
    # variants).  The saturation is done according to the two signedness
    # indicators at the end of the name.  For example 64Sto32U means a
    # signed 64 bit value is saturated into an unsigned 32 bit value.
    # Additionally, the QRS variants do rounding, that is, they add the
    # value (1 << (shift_amount-1)) to each source lane before shifting.
    # These operations return 65 bits: one bit ("Q") indicating whether
    # saturation occurred, and the shift result.  The result type is V128,
    # of which the lower half is the shift result, and Q occupies the
    # least significant bit of the upper half.  All other bits of the
    # upper half are zero.

    QandQShrNnarrow16Uto8Ux8 = auto()
    QandQShrNnarrow32Uto16Ux4 = auto()
    QandQShrNnarrow64Uto32Ux2 = auto()
    QandQSarNnarrow16Sto8Sx8 = auto()
    QandQSarNnarrow32Sto16Sx4 = auto()
    QandQSarNnarrow64Sto32Sx2 = auto()
    QandQSarNnarrow16Sto8Ux8 = auto()
    QandQSarNnarrow32Sto16Ux4 = auto()
    QandQSarNnarrow64Sto32Ux2 = auto()

    QandQRShrNnarrow16Uto8Ux8 = auto()
    QandQRShrNnarrow32Uto16Ux4 = auto()
    QandQRShrNnarrow64Uto32Ux2 = auto()
    QandQRSarNnarrow16Sto8Sx8 = auto()
    QandQRSarNnarrow32Sto16Sx4 = auto()
    QandQRSarNnarrow64Sto32Sx2 = auto()
    QandQRSarNnarrow16Sto8Ux8 = auto()
    QandQRSarNnarrow32Sto16Ux4 = auto()
    QandQRSarNnarrow64Sto32Ux2 = auto()

    # NARROWING (binary)
    # -- narrow 2xV128 into 1xV128, hi half from left arg
    # See comments above w.r.t. U vs S issues in saturated narrowing.

    QNarrowBin16Sto8Ux16 = auto()
    QNarrowBin32Sto16Ux8 = auto()
    QNarrowBin16Sto8Sx16 = auto()
    QNarrowBin32Sto16Sx8 = auto()
    QNarrowBin16Uto8Ux16 = auto()
    QNarrowBin32Uto16Ux8 = auto()
    NarrowBin16to8x16 = auto()
    NarrowBin32to16x8 = auto()
    QNarrowBin64Sto32Sx4 = auto()
    QNarrowBin64Uto32Ux4 = auto()
    NarrowBin64to32x4 = auto()

    # NARROWING (unary) -- narrow V128 into I64

    NarrowUn16to8x8 = auto()
    NarrowUn32to16x4 = auto()
    NarrowUn64to32x2 = auto()  # Saturating narrowing from signed source to signed/unsigned  destination

    QNarrowUn16Sto8Sx8 = auto()
    QNarrowUn32Sto16Sx4 = auto()
    QNarrowUn64Sto32Sx2 = auto()
    QNarrowUn16Sto8Ux8 = auto()
    QNarrowUn32Sto16Ux4 = auto()
    QNarrowUn64Sto32Ux2 = auto()  # Saturating narrowing from unsigned source to unsigned destination

    QNarrowUn16Uto8Ux8 = auto()
    QNarrowUn32Uto16Ux4 = auto()
    QNarrowUn64Uto32Ux2 = auto()

    # WIDENING -- sign or zero extend each element of the argument
    # vector to the twice original size.  The resulting vector consists of
    # the same number of elements but each element and the vector itself
    # are twice as wide.
    # All operations are I64->V128.
    # Example
    # Iop_Widen32Sto64x2( [a, b] ) = [c, d]
    # where c = Iop_32Sto64(a) and d = Iop_32Sto64(b)

    Widen8Uto16x8 = auto()
    Widen16Uto32x4 = auto()
    Widen32Uto64x2 = auto()
    Widen8Sto16x8 = auto()
    Widen16Sto32x4 = auto()
    Widen32Sto64x2 = auto()

    # INTERLEAVING
    # Interleave lanes from low or high halves of
    # operands.  Most-significant result lane is from the left
    # arg.

    InterleaveHI8x16 = auto()
    InterleaveHI16x8 = auto()
    InterleaveHI32x4 = auto()
    InterleaveHI64x2 = auto()
    InterleaveLO8x16 = auto()
    InterleaveLO16x8 = auto()
    InterleaveLO32x4 = auto()
    InterleaveLO64x2 = auto()  # Interleave odd/even lanes of operands.  Most-significant result lane  is from the left arg.

    InterleaveOddLanes8x16 = auto()
    InterleaveEvenLanes8x16 = auto()
    InterleaveOddLanes16x8 = auto()
    InterleaveEvenLanes16x8 = auto()
    InterleaveOddLanes32x4 = auto()
    InterleaveEvenLanes32x4 = auto()

    # Pack even/odd lanes.

    PackOddLanes8x16 = auto()
    PackEvenLanes8x16 = auto()
    PackOddLanes16x8 = auto()
    PackEvenLanes16x8 = auto()
    PackOddLanes32x4 = auto()
    PackEvenLanes32x4 = auto()

    # CONCATENATION -- build a new value by concatenating either
    # the even or odd lanes of both operands.  Note that
    # Cat{Odd,Even}Lanes64x2 are identical to Interleave{HI,LO}64x2
    # and so are omitted.

    CatOddLanes8x16 = auto()
    CatOddLanes16x8 = auto()
    CatOddLanes32x4 = auto()
    CatEvenLanes8x16 = auto()
    CatEvenLanes16x8 = auto()
    CatEvenLanes32x4 = auto()

    # GET elements of VECTOR
    # GET is binop (V128, I8) -> I<elem_size>
    # SET is triop (V128, I8, I<elem_size>) -> V128
    # Note: the arm back-end handles only constant second argument.

    GetElem8x16 = auto()
    GetElem16x8 = auto()
    GetElem32x4 = auto()
    GetElem64x2 = auto()
    SetElem8x16 = auto()
    SetElem16x8 = auto()
    SetElem32x4 = auto()
    SetElem64x2 = auto()

    # DUPLICATING -- copy value to all lanes

    Dup8x16 = auto()
    Dup16x8 = auto()
    Dup32x4 = auto()

    # SLICE -- produces the lowest 128 bits of (arg1:arg2) >> (8 * arg3).
    # arg3 is a shift amount in bytes and may be between 0 and 16
    # inclusive.  When 0, the result is arg2; when 16, the result is arg1.
    # Not all back ends handle all values.  The arm64 back
    # end handles only immediate arg3 values.

    SliceV128 = auto()  # (V128, V128, I8) -> V128

    # REVERSE the order of chunks in vector lanes.  Chunks must be
    # smaller than the vector lanes (obviously) and so may be 8-,
    # 16- and 32-bit in size.  See definitions of 64-bit SIMD
    # versions above for examples.

    Reverse8sIn16_x8 = auto()
    Reverse8sIn32_x4 = auto()
    Reverse16sIn32_x4 = auto()
    Reverse8sIn64_x2 = auto()
    Reverse16sIn64_x2 = auto()
    Reverse32sIn64_x2 = auto()
    Reverse1sIn8_x16 = auto()  # Reverse bits in each byte lane.
    # PERMUTING -- copy src bytes to dst,
    # as indexed by control vector bytes:
    # for i in 0 .. 15 . result[i] = argL[ argR[i] ]
    # argR[i] values may only be in the range 0 .. 15, else behaviour
    # is undefined.  That is, argR[i][7:4] must be zero.

    Perm8x16 = auto()
    Perm32x4 = auto()  # ditto, except argR values are restricted to 0 .. 3
    # PERMUTING with optional zeroing:
    # for i in 0 .. 15 . result[i] = if argR[i] bit 7 is set
    # then zero else argL[ argR[i] ]
    # argR[i][6:4] must be zero, else behaviour is undefined.

    PermOrZero8x16 = auto()

    # same, but Triop (argL consists of two 128-bit parts)
    # correct range for argR values is 0..31
    # (V128, V128, V128) -> V128
    # (ArgL_first, ArgL_second, ArgR) -> result

    Perm8x16x2 = auto()

    # MISC CONVERSION -- get high bits of each byte lane, a la
    # x86/amd64 pmovmskb

    GetMSBs8x16 = auto()  # V128 -> I16
    # Vector Reciprocal Estimate and Vector Reciprocal Square Root Estimate
    # See floating-point equivalents for details.

    RecipEst32Ux4 = auto()
    RSqrtEst32Ux4 = auto()

    # 128-bit multipy by 10 instruction, result is lower 128-bits

    MulI128by10 = auto()

    # 128-bit multipy by 10 instruction, result is carry out from the MSB

    MulI128by10Carry = auto()

    # 128-bit multipy by 10 instruction, result is lower 128-bits of the
    # source times 10 plus the carry in

    MulI128by10E = auto()

    # 128-bit multipy by 10 instruction, result is carry out from the MSB
    # of the source times 10 plus the carry in

    MulI128by10ECarry = auto()

    # 128-bit carry out from ((U64 * U64 -> U128) + (U64 * U64 -> U128))

    CarryOut2xMultU64Add128 = auto()

    # ------------------ 256-bit SIMD Integer. ------------------
    # Pack/unpack

    V256to64_0 = auto()  # V256 -> I64, extract least significant lane
    V256to64_1 = auto()
    V256to64_2 = auto()
    V256to64_3 = auto()  # V256 -> I64, extract most significant lane

    Pack64x4toV256 = auto()  # (I64,I64,I64,I64)->V256

    V256toV128_0 = auto()  # V256 -> V128, less significant lane
    V256toV128_1 = auto()  # V256 -> V128, more significant lane
    V128HLtoV256 = auto()  # (V128,V128)->V256, first arg is most signif

    AndV256 = auto()
    OrV256 = auto()
    XorV256 = auto()
    NotV256 = auto()

    # MISC (vector integer cmp != 0)

    CmpNEZ8x32 = auto()
    CmpNEZ16x16 = auto()
    CmpNEZ32x8 = auto()
    CmpNEZ64x4 = auto()

    Add8x32 = auto()
    Add16x16 = auto()
    Add32x8 = auto()
    Add64x4 = auto()
    Sub8x32 = auto()
    Sub16x16 = auto()
    Sub32x8 = auto()
    Sub64x4 = auto()

    CmpEQ8x32 = auto()
    CmpEQ16x16 = auto()
    CmpEQ32x8 = auto()
    CmpEQ64x4 = auto()
    CmpGT8Sx32 = auto()
    CmpGT16Sx16 = auto()
    CmpGT32Sx8 = auto()
    CmpGT64Sx4 = auto()

    ShlN16x16 = auto()
    ShlN32x8 = auto()
    ShlN64x4 = auto()
    ShrN16x16 = auto()
    ShrN32x8 = auto()
    ShrN64x4 = auto()
    SarN16x16 = auto()
    SarN32x8 = auto()

    Max8Sx32 = auto()
    Max16Sx16 = auto()
    Max32Sx8 = auto()
    Max8Ux32 = auto()
    Max16Ux16 = auto()
    Max32Ux8 = auto()
    Min8Sx32 = auto()
    Min16Sx16 = auto()
    Min32Sx8 = auto()
    Min8Ux32 = auto()
    Min16Ux16 = auto()
    Min32Ux8 = auto()

    Mul16x16 = auto()
    Mul32x8 = auto()
    MulHi16Ux16 = auto()
    MulHi16Sx16 = auto()

    QAdd8Ux32 = auto()
    QAdd16Ux16 = auto()
    QAdd8Sx32 = auto()
    QAdd16Sx16 = auto()
    QSub8Ux32 = auto()
    QSub16Ux16 = auto()
    QSub8Sx32 = auto()
    QSub16Sx16 = auto()

    Avg8Ux32 = auto()
    Avg16Ux16 = auto()

    Perm32x8 = auto()

    # (V128, V128) -> V128

    CipherV128 = auto()
    CipherLV128 = auto()
    CipherSV128 = auto()
    NCipherV128 = auto()
    NCipherLV128 = auto()

    # Hash instructions, Federal Information Processing Standards
    # Publication 180-3 Secure Hash Standard.
    # (V128, I8) -> V128; The I8 input arg is (ST | SIX), where ST and
    # SIX are fields from the insn. See ISA 2.07 description of
    # vshasigmad and vshasigmaw insns.

    SHA512 = auto()
    SHA256 = auto()

    # ------------------ 256-bit SIMD FP. ------------------
    # ternary :: IRRoundingMode(I32) x V256 x V256 -> V256

    Add64Fx4 = auto()
    Sub64Fx4 = auto()
    Mul64Fx4 = auto()
    Div64Fx4 = auto()
    Add32Fx8 = auto()
    Sub32Fx8 = auto()
    Mul32Fx8 = auto()
    Div32Fx8 = auto()

    I32StoF32x8 = auto()  # IRRoundingMode(I32) x V256 -> V256

    F32toI32Sx8 = auto()  # IRRoundingMode(I32) x V256 -> V256

    F32toF16x8 = auto()  # IRRoundingMode(I32) x V256 -> V128

    F16toF32x8 = auto()  # F16x8(==V128) -> F32x8(==V256)

    Sqrt32Fx8 = auto()
    Sqrt64Fx4 = auto()
    RSqrtEst32Fx8 = auto()
    RecipEst32Fx8 = auto()

    Max32Fx8 = auto()
    Min32Fx8 = auto()
    Max64Fx4 = auto()
    Min64Fx4 = auto()
    Rotx32 = auto()
    Rotx64 = auto()
    LAST = auto()  # must be the last enumerator
