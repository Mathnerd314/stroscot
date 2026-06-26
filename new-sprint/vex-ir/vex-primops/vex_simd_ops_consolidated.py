# Consolidated VEX SIMD-related Op declarations
# Scope: 128-bit SIMD FP, pack/unpack, 128-bit SIMD integer,
#        256-bit SIMD integer, and 256-bit SIMD FP.
# Notes:
# - Names follow the public IROp enum spellings without the Iop_ prefix.
# - Rotx32 / Rotx64 are inferred as unary V256 -> V256 from section placement.

# ===========================================================================
# 128-BIT SIMD FP
# ===========================================================================

# --- 16x8 vector FP ---

# unary :: IRRoundingMode(I32) x V128 -> V128
op_Sqrt16Fx8 = Op("Sqrt16Fx8", (I32, V128), V128)

# binary :: IRRoundingMode(I32) x V128 x V128 -> V128
op_Add16Fx8 = Op("Add16Fx8", (I32, V128, V128), V128)
op_Sub16Fx8 = Op("Sub16Fx8", (I32, V128, V128), V128)

# binary
op_CmpLT16Fx8 = Op("CmpLT16Fx8", (V128, V128), V128)
op_CmpLE16Fx8 = Op("CmpLE16Fx8", (V128, V128), V128)
op_CmpEQ16Fx8 = Op("CmpEQ16Fx8", (V128, V128), V128)

# unary
op_Abs16Fx8 = Op("Abs16Fx8", (V128,), V128)
op_Neg16Fx8 = Op("Neg16Fx8", (V128,), V128)

# --- 32x4 vector FP ---

# binary :: IRRoundingMode(I32) x V128 x V128 -> V128
op_Add32Fx4 = Op("Add32Fx4", (I32, V128, V128), V128)
op_Sub32Fx4 = Op("Sub32Fx4", (I32, V128, V128), V128)
op_Mul32Fx4 = Op("Mul32Fx4", (I32, V128, V128), V128)
op_Div32Fx4 = Op("Div32Fx4", (I32, V128, V128), V128)

# binary
op_Max32Fx4 = Op("Max32Fx4", (V128, V128), V128)
op_Min32Fx4 = Op("Min32Fx4", (V128, V128), V128)
op_Add32Fx2 = Op("Add32Fx2", (V128, V128), V128)
op_Sub32Fx2 = Op("Sub32Fx2", (V128, V128), V128)

op_CmpEQ32Fx4 = Op("CmpEQ32Fx4", (V128, V128), V128)
op_CmpLT32Fx4 = Op("CmpLT32Fx4", (V128, V128), V128)
op_CmpLE32Fx4 = Op("CmpLE32Fx4", (V128, V128), V128)
op_CmpUN32Fx4 = Op("CmpUN32Fx4", (V128, V128), V128)
op_CmpGT32Fx4 = Op("CmpGT32Fx4", (V128, V128), V128)
op_CmpGE32Fx4 = Op("CmpGE32Fx4", (V128, V128), V128)

op_PwMax32Fx4 = Op("PwMax32Fx4", (V128, V128), V128)
op_PwMin32Fx4 = Op("PwMin32Fx4", (V128, V128), V128)

# unary
op_Abs32Fx4 = Op("Abs32Fx4", (V128,), V128)
op_Neg32Fx4 = Op("Neg32Fx4", (V128,), V128)

# unary :: IRRoundingMode(I32) x V128 -> V128
op_Sqrt32Fx4 = Op("Sqrt32Fx4", (I32, V128), V128)

op_RecipEst32Fx4  = Op("RecipEst32Fx4",  (V128,), V128)
op_RecipStep32Fx4 = Op("RecipStep32Fx4", (V128, V128), V128)
op_RSqrtEst32Fx4  = Op("RSqrtEst32Fx4",  (V128,), V128)
op_Scale2_32Fx4   = Op("Scale2_32Fx4",   (V128, V128), V128)
op_Log2_32Fx4     = Op("Log2_32Fx4",     (V128,), V128)
op_Exp2_32Fx4     = Op("Exp2_32Fx4",     (V128,), V128)
op_RSqrtStep32Fx4 = Op("RSqrtStep32Fx4", (V128, V128), V128)

# --- Int to/from FP conversion ---

op_I32StoF32x4 = Op("I32StoF32x4", (I32, V128), V128)
op_F32toI32Sx4 = Op("F32toI32Sx4", (I32, V128), V128)

op_F32toI32Ux4_RZ  = Op("F32toI32Ux4_RZ",  (V128,), V128)
op_F32toI32Sx4_RZ  = Op("F32toI32Sx4_RZ",  (V128,), V128)
op_QF32toI32Ux4_RZ = Op("QF32toI32Ux4_RZ", (V128,), V128)
op_QF32toI32Sx4_RZ = Op("QF32toI32Sx4_RZ", (V128,), V128)

op_RoundF32x4_RM = Op("RoundF32x4_RM", (V128,), V128)
op_RoundF32x4_RP = Op("RoundF32x4_RP", (V128,), V128)
op_RoundF32x4_RN = Op("RoundF32x4_RN", (V128,), V128)
op_RoundF32x4_RZ = Op("RoundF32x4_RZ", (V128,), V128)

op_F32ToFixed32Ux4_RZ = Op("F32ToFixed32Ux4_RZ", (V128, I8), V128)
op_F32ToFixed32Sx4_RZ = Op("F32ToFixed32Sx4_RZ", (V128, I8), V128)
op_Fixed32UToF32x4_RN = Op("Fixed32UToF32x4_RN", (V128, I8), V128)
op_Fixed32SToF32x4_RN = Op("Fixed32SToF32x4_RN", (V128, I8), V128)

# --- Single to/from half conversion ---
op_F32toF16x4 = Op("F32toF16x4", (I32, V128), I64)
op_F16toF32x4 = Op("F16toF32x4", (I64,), V128)

# --- Double to/from half conversion ---
op_F16toF64x2 = Op("F16toF64x2", (I32,), V128)

# Values from two registers converted in smaller type and put in one
op_F32x4_2toQ16x8 = Op("F32x4_2toQ16x8", (I32, V128, V128), V128)

# --- 32x4 lowest-lane-only scalar FP ---
op_Add32F0x4 = Op("Add32F0x4", (V128, V128), V128)
op_Sub32F0x4 = Op("Sub32F0x4", (V128, V128), V128)
op_Mul32F0x4 = Op("Mul32F0x4", (V128, V128), V128)
op_Div32F0x4 = Op("Div32F0x4", (V128, V128), V128)
op_Max32F0x4 = Op("Max32F0x4", (V128, V128), V128)
op_Min32F0x4 = Op("Min32F0x4", (V128, V128), V128)

op_CmpEQ32F0x4 = Op("CmpEQ32F0x4", (V128, V128), V128)
op_CmpLT32F0x4 = Op("CmpLT32F0x4", (V128, V128), V128)
op_CmpLE32F0x4 = Op("CmpLE32F0x4", (V128, V128), V128)
op_CmpUN32F0x4 = Op("CmpUN32F0x4", (V128, V128), V128)

op_RecipEst32F0x4 = Op("RecipEst32F0x4", (V128,), V128)
op_Sqrt32F0x4     = Op("Sqrt32F0x4",     (V128,), V128)
op_RSqrtEst32F0x4 = Op("RSqrtEst32F0x4", (V128,), V128)

# --- 64x2 vector FP ---

# binary :: IRRoundingMode(I32) x V128 x V128 -> V128
op_Add64Fx2 = Op("Add64Fx2", (I32, V128, V128), V128)
op_Sub64Fx2 = Op("Sub64Fx2", (I32, V128, V128), V128)
op_Mul64Fx2 = Op("Mul64Fx2", (I32, V128, V128), V128)
op_Div64Fx2 = Op("Div64Fx2", (I32, V128, V128), V128)

# binary
op_Max64Fx2   = Op("Max64Fx2",   (V128, V128), V128)
op_Min64Fx2   = Op("Min64Fx2",   (V128, V128), V128)
op_CmpEQ64Fx2 = Op("CmpEQ64Fx2", (V128, V128), V128)
op_CmpLT64Fx2 = Op("CmpLT64Fx2", (V128, V128), V128)
op_CmpLE64Fx2 = Op("CmpLE64Fx2", (V128, V128), V128)
op_CmpUN64Fx2 = Op("CmpUN64Fx2", (V128, V128), V128)

# unary
op_Abs64Fx2 = Op("Abs64Fx2", (V128,), V128)
op_Neg64Fx2 = Op("Neg64Fx2", (V128,), V128)

# unary :: IRRoundingMode(I32) x V128 -> V128
op_Sqrt64Fx2 = Op("Sqrt64Fx2", (I32, V128), V128)

op_Scale2_64Fx2   = Op("Scale2_64Fx2",   (V128, V128), V128)
op_Log2_64Fx2     = Op("Log2_64Fx2",     (V128,), V128)
op_RecipEst64Fx2  = Op("RecipEst64Fx2",  (V128,), V128)
op_RecipStep64Fx2 = Op("RecipStep64Fx2", (V128, V128), V128)
op_RSqrtEst64Fx2  = Op("RSqrtEst64Fx2",  (V128,), V128)
op_RSqrtStep64Fx2 = Op("RSqrtStep64Fx2", (V128, V128), V128)

op_F64x2_2toQ32x4 = Op("F64x2_2toQ32x4", (I32, V128, V128), V128)

# --- 64x2 lowest-lane-only scalar FP ---
op_Add64F0x2 = Op("Add64F0x2", (V128, V128), V128)
op_Sub64F0x2 = Op("Sub64F0x2", (V128, V128), V128)
op_Mul64F0x2 = Op("Mul64F0x2", (V128, V128), V128)
op_Div64F0x2 = Op("Div64F0x2", (V128, V128), V128)
op_Max64F0x2 = Op("Max64F0x2", (V128, V128), V128)
op_Min64F0x2 = Op("Min64F0x2", (V128, V128), V128)

op_CmpEQ64F0x2 = Op("CmpEQ64F0x2", (V128, V128), V128)
op_CmpLT64F0x2 = Op("CmpLT64F0x2", (V128, V128), V128)
op_CmpLE64F0x2 = Op("CmpLE64F0x2", (V128, V128), V128)
op_CmpUN64F0x2 = Op("CmpUN64F0x2", (V128, V128), V128)

op_Sqrt64F0x2 = Op("Sqrt64F0x2", (V128,), V128)

# ===========================================================================
# PACK / UNPACK
# ===========================================================================

op_V128to64   = Op("V128to64",   (V128,), I64)
op_V128HIto64 = Op("V128HIto64", (V128,), I64)
op_64HLtoV128 = Op("64HLtoV128", (I64, I64), V128)

op_64UtoV128   = Op("64UtoV128",   (I64,), V128)
op_SetV128lo64 = Op("SetV128lo64", (V128, I64), V128)

op_ZeroHI64ofV128  = Op("ZeroHI64ofV128",  (V128,), V128)
op_ZeroHI96ofV128  = Op("ZeroHI96ofV128",  (V128,), V128)
op_ZeroHI112ofV128 = Op("ZeroHI112ofV128", (V128,), V128)
op_ZeroHI120ofV128 = Op("ZeroHI120ofV128", (V128,), V128)

op_32UtoV128   = Op("32UtoV128",   (I32,), V128)
op_V128to32    = Op("V128to32",    (V128,), I32)
op_SetV128lo32 = Op("SetV128lo32", (V128, I32), V128)

# ===========================================================================
# 128-BIT SIMD INTEGER
# ===========================================================================

# Bitwise
op_NotV128 = Op("NotV128", (V128,), V128)
op_AndV128 = Op("AndV128", (V128, V128), V128)
op_OrV128  = Op("OrV128",  (V128, V128), V128)
op_XorV128 = Op("XorV128", (V128, V128), V128)

# Vector shift
op_ShlV128 = Op("ShlV128", (V128, I8), V128)
op_ShrV128 = Op("ShrV128", (V128, I8), V128)
op_SarV128 = Op("SarV128", (V128, I8), V128)

# Misc cmp != 0
op_CmpNEZ8x16  = Op("CmpNEZ8x16",  (V128,), V128)
op_CmpNEZ16x8  = Op("CmpNEZ16x8",  (V128,), V128)
op_CmpNEZ32x4  = Op("CmpNEZ32x4",  (V128,), V128)
op_CmpNEZ64x2  = Op("CmpNEZ64x2",  (V128,), V128)
op_CmpNEZ128x1 = Op("CmpNEZ128x1", (V128,), V128)

# Addition
op_Add8x16  = Op("Add8x16",  (V128, V128), V128)
op_Add16x8  = Op("Add16x8",  (V128, V128), V128)
op_Add32x4  = Op("Add32x4",  (V128, V128), V128)
op_Add64x2  = Op("Add64x2",  (V128, V128), V128)
op_Add128x1 = Op("Add128x1", (V128, V128), V128)

op_QAdd8Ux16 = Op("QAdd8Ux16", (V128, V128), V128)
op_QAdd16Ux8 = Op("QAdd16Ux8", (V128, V128), V128)
op_QAdd32Ux4 = Op("QAdd32Ux4", (V128, V128), V128)
op_QAdd64Ux2 = Op("QAdd64Ux2", (V128, V128), V128)

op_QAdd8Sx16 = Op("QAdd8Sx16", (V128, V128), V128)
op_QAdd16Sx8 = Op("QAdd16Sx8", (V128, V128), V128)
op_QAdd32Sx4 = Op("QAdd32Sx4", (V128, V128), V128)
op_QAdd64Sx2 = Op("QAdd64Sx2", (V128, V128), V128)

op_QAddExtUSsatSS8x16  = Op("QAddExtUSsatSS8x16",  (V128, V128), V128)
op_QAddExtUSsatSS16x8  = Op("QAddExtUSsatSS16x8",  (V128, V128), V128)
op_QAddExtUSsatSS32x4  = Op("QAddExtUSsatSS32x4",  (V128, V128), V128)
op_QAddExtUSsatSS64x2  = Op("QAddExtUSsatSS64x2",  (V128, V128), V128)

op_QAddExtSUsatUU8x16  = Op("QAddExtSUsatUU8x16",  (V128, V128), V128)
op_QAddExtSUsatUU16x8  = Op("QAddExtSUsatUU16x8",  (V128, V128), V128)
op_QAddExtSUsatUU32x4  = Op("QAddExtSUsatUU32x4",  (V128, V128), V128)
op_QAddExtSUsatUU64x2  = Op("QAddExtSUsatUU64x2",  (V128, V128), V128)

# Subtraction
op_Sub8x16  = Op("Sub8x16",  (V128, V128), V128)
op_Sub16x8  = Op("Sub16x8",  (V128, V128), V128)
op_Sub32x4  = Op("Sub32x4",  (V128, V128), V128)
op_Sub64x2  = Op("Sub64x2",  (V128, V128), V128)
op_Sub128x1 = Op("Sub128x1", (V128, V128), V128)

op_QSub8Ux16 = Op("QSub8Ux16", (V128, V128), V128)
op_QSub16Ux8 = Op("QSub16Ux8", (V128, V128), V128)
op_QSub32Ux4 = Op("QSub32Ux4", (V128, V128), V128)
op_QSub64Ux2 = Op("QSub64Ux2", (V128, V128), V128)

op_QSub8Sx16 = Op("QSub8Sx16", (V128, V128), V128)
op_QSub16Sx8 = Op("QSub16Sx8", (V128, V128), V128)
op_QSub32Sx4 = Op("QSub32Sx4", (V128, V128), V128)
op_QSub64Sx2 = Op("QSub64Sx2", (V128, V128), V128)

# Multiplication
op_Mul8x16    = Op("Mul8x16",    (V128, V128), V128)
op_Mul16x8    = Op("Mul16x8",    (V128, V128), V128)
op_Mul32x4    = Op("Mul32x4",    (V128, V128), V128)
op_MulHi8Ux16 = Op("MulHi8Ux16", (V128, V128), V128)
op_MulHi16Ux8 = Op("MulHi16Ux8", (V128, V128), V128)
op_MulHi32Ux4 = Op("MulHi32Ux4", (V128, V128), V128)
op_MulHi8Sx16 = Op("MulHi8Sx16", (V128, V128), V128)
op_MulHi16Sx8 = Op("MulHi16Sx8", (V128, V128), V128)
op_MulHi32Sx4 = Op("MulHi32Sx4", (V128, V128), V128)

op_MullEven8Ux16  = Op("MullEven8Ux16",  (V128, V128), V128)
op_MullEven16Ux8  = Op("MullEven16Ux8",  (V128, V128), V128)
op_MullEven32Ux4  = Op("MullEven32Ux4",  (V128, V128), V128)
op_MullEven8Sx16  = Op("MullEven8Sx16",  (V128, V128), V128)
op_MullEven16Sx8  = Op("MullEven16Sx8",  (V128, V128), V128)
op_MullEven32Sx4  = Op("MullEven32Sx4",  (V128, V128), V128)

op_Mull8Ux8  = Op("Mull8Ux8",  (I64, I64), V128)
op_Mull8Sx8  = Op("Mull8Sx8",  (I64, I64), V128)
op_Mull16Ux4 = Op("Mull16Ux4", (I64, I64), V128)
op_Mull16Sx4 = Op("Mull16Sx4", (I64, I64), V128)
op_Mull32Ux2 = Op("Mull32Ux2", (I64, I64), V128)
op_Mull32Sx2 = Op("Mull32Sx2", (I64, I64), V128)

op_QDMull16Sx4 = Op("QDMull16Sx4", (I64, I64), V128)
op_QDMull32Sx2 = Op("QDMull32Sx2", (I64, I64), V128)

op_QDMulHi16Sx8  = Op("QDMulHi16Sx8",  (V128, V128), V128)
op_QDMulHi32Sx4  = Op("QDMulHi32Sx4",  (V128, V128), V128)
op_QRDMulHi16Sx8 = Op("QRDMulHi16Sx8", (V128, V128), V128)
op_QRDMulHi32Sx4 = Op("QRDMulHi32Sx4", (V128, V128), V128)

op_PolynomialMul8x16    = Op("PolynomialMul8x16",    (V128, V128), V128)
op_PolynomialMull8x8    = Op("PolynomialMull8x8",    (I64, I64), V128)
op_PolynomialMulAdd8x16 = Op("PolynomialMulAdd8x16", (V128, V128), V128)
op_PolynomialMulAdd16x8 = Op("PolynomialMulAdd16x8", (V128, V128), V128)
op_PolynomialMulAdd32x4 = Op("PolynomialMulAdd32x4", (V128, V128), V128)
op_PolynomialMulAdd64x2 = Op("PolynomialMulAdd64x2", (V128, V128), V128)

# Pairwise
op_PwAdd8x16  = Op("PwAdd8x16",  (V128, V128), V128)
op_PwAdd16x8  = Op("PwAdd16x8",  (V128, V128), V128)
op_PwAdd32x4  = Op("PwAdd32x4",  (V128, V128), V128)
op_PwAdd32Fx2 = Op("PwAdd32Fx2", (V128, V128), V128)

op_PwAddL8Ux16  = Op("PwAddL8Ux16",  (V128,), V128)
op_PwAddL16Ux8  = Op("PwAddL16Ux8",  (V128,), V128)
op_PwAddL32Ux4  = Op("PwAddL32Ux4",  (V128,), V128)
op_PwAddL64Ux2  = Op("PwAddL64Ux2",  (V128,), V128)
op_PwAddL8Sx16  = Op("PwAddL8Sx16",  (V128,), V128)
op_PwAddL16Sx8  = Op("PwAddL16Sx8",  (V128,), V128)
op_PwAddL32Sx4  = Op("PwAddL32Sx4",  (V128,), V128)

op_PwExtUSMulQAdd8x16 = Op("PwExtUSMulQAdd8x16", (V128, V128), V128)
op_PwBitMtxXpose64x2  = Op("PwBitMtxXpose64x2",  (V128,), V128)

# Absolute value
op_Abs8x16 = Op("Abs8x16", (V128,), V128)
op_Abs16x8 = Op("Abs16x8", (V128,), V128)
op_Abs32x4 = Op("Abs32x4", (V128,), V128)
op_Abs64x2 = Op("Abs64x2", (V128,), V128)

# Averaging
op_Avg8Ux16  = Op("Avg8Ux16",  (V128, V128), V128)
op_Avg16Ux8  = Op("Avg16Ux8",  (V128, V128), V128)
op_Avg32Ux4  = Op("Avg32Ux4",  (V128, V128), V128)
op_Avg64Ux2  = Op("Avg64Ux2",  (V128, V128), V128)
op_Avg8Sx16  = Op("Avg8Sx16",  (V128, V128), V128)
op_Avg16Sx8  = Op("Avg16Sx8",  (V128, V128), V128)
op_Avg32Sx4  = Op("Avg32Sx4",  (V128, V128), V128)
op_Avg64Sx2  = Op("Avg64Sx2",  (V128, V128), V128)

# Min/max
op_Max8Sx16  = Op("Max8Sx16",  (V128, V128), V128)
op_Max16Sx8  = Op("Max16Sx8",  (V128, V128), V128)
op_Max32Sx4  = Op("Max32Sx4",  (V128, V128), V128)
op_Max64Sx2  = Op("Max64Sx2",  (V128, V128), V128)
op_Max8Ux16  = Op("Max8Ux16",  (V128, V128), V128)
op_Max16Ux8  = Op("Max16Ux8",  (V128, V128), V128)
op_Max32Ux4  = Op("Max32Ux4",  (V128, V128), V128)
op_Max64Ux2  = Op("Max64Ux2",  (V128, V128), V128)
op_Min8Sx16  = Op("Min8Sx16",  (V128, V128), V128)
op_Min16Sx8  = Op("Min16Sx8",  (V128, V128), V128)
op_Min32Sx4  = Op("Min32Sx4",  (V128, V128), V128)
op_Min64Sx2  = Op("Min64Sx2",  (V128, V128), V128)
op_Min8Ux16  = Op("Min8Ux16",  (V128, V128), V128)
op_Min16Ux8  = Op("Min16Ux8",  (V128, V128), V128)
op_Min32Ux4  = Op("Min32Ux4",  (V128, V128), V128)
op_Min64Ux2  = Op("Min64Ux2",  (V128, V128), V128)

# Comparison
op_CmpEQ8x16   = Op("CmpEQ8x16",   (V128, V128), V128)
op_CmpEQ16x8   = Op("CmpEQ16x8",   (V128, V128), V128)
op_CmpEQ32x4   = Op("CmpEQ32x4",   (V128, V128), V128)
op_CmpEQ64x2   = Op("CmpEQ64x2",   (V128, V128), V128)
op_CmpGT8Sx16  = Op("CmpGT8Sx16",  (V128, V128), V128)
op_CmpGT16Sx8  = Op("CmpGT16Sx8",  (V128, V128), V128)
op_CmpGT32Sx4  = Op("CmpGT32Sx4",  (V128, V128), V128)
op_CmpGT64Sx2  = Op("CmpGT64Sx2",  (V128, V128), V128)
op_CmpGT8Ux16  = Op("CmpGT8Ux16",  (V128, V128), V128)
op_CmpGT16Ux8  = Op("CmpGT16Ux8",  (V128, V128), V128)
op_CmpGT32Ux4  = Op("CmpGT32Ux4",  (V128, V128), V128)
op_CmpGT64Ux2  = Op("CmpGT64Ux2",  (V128, V128), V128)

# Count
op_Cnt8x16 = Op("Cnt8x16", (V128,), V128)
op_Clz8x16 = Op("Clz8x16", (V128,), V128)
op_Clz16x8 = Op("Clz16x8", (V128,), V128)
op_Clz32x4 = Op("Clz32x4", (V128,), V128)
op_Cls8x16 = Op("Cls8x16", (V128,), V128)
op_Cls16x8 = Op("Cls16x8", (V128,), V128)
op_Cls32x4 = Op("Cls32x4", (V128,), V128)

# Vector x scalar shift
op_ShlN8x16 = Op("ShlN8x16", (V128, I8), V128)
op_ShlN16x8 = Op("ShlN16x8", (V128, I8), V128)
op_ShlN32x4 = Op("ShlN32x4", (V128, I8), V128)
op_ShlN64x2 = Op("ShlN64x2", (V128, I8), V128)
op_ShrN8x16 = Op("ShrN8x16", (V128, I8), V128)
op_ShrN16x8 = Op("ShrN16x8", (V128, I8), V128)
op_ShrN32x4 = Op("ShrN32x4", (V128, I8), V128)
op_ShrN64x2 = Op("ShrN64x2", (V128, I8), V128)
op_SarN8x16 = Op("SarN8x16", (V128, I8), V128)
op_SarN16x8 = Op("SarN16x8", (V128, I8), V128)
op_SarN32x4 = Op("SarN32x4", (V128, I8), V128)
op_SarN64x2 = Op("SarN64x2", (V128, I8), V128)

# Vector x vector shift / rotate
op_Shl8x16 = Op("Shl8x16", (V128, V128), V128)
op_Shl16x8 = Op("Shl16x8", (V128, V128), V128)
op_Shl32x4 = Op("Shl32x4", (V128, V128), V128)
op_Shl64x2 = Op("Shl64x2", (V128, V128), V128)
op_Shr8x16 = Op("Shr8x16", (V128, V128), V128)
op_Shr16x8 = Op("Shr16x8", (V128, V128), V128)
op_Shr32x4 = Op("Shr32x4", (V128, V128), V128)
op_Shr64x2 = Op("Shr64x2", (V128, V128), V128)
op_Sar8x16 = Op("Sar8x16", (V128, V128), V128)
op_Sar16x8 = Op("Sar16x8", (V128, V128), V128)
op_Sar32x4 = Op("Sar32x4", (V128, V128), V128)
op_Sar64x2 = Op("Sar64x2", (V128, V128), V128)
op_Sal8x16 = Op("Sal8x16", (V128, V128), V128)
op_Sal16x8 = Op("Sal16x8", (V128, V128), V128)
op_Sal32x4 = Op("Sal32x4", (V128, V128), V128)
op_Sal64x2 = Op("Sal64x2", (V128, V128), V128)
op_Rol8x16 = Op("Rol8x16", (V128, V128), V128)
op_Rol16x8 = Op("Rol16x8", (V128, V128), V128)
op_Rol32x4 = Op("Rol32x4", (V128, V128), V128)
op_Rol64x2 = Op("Rol64x2", (V128, V128), V128)

# Vector x vector saturating shift
op_QShl8x16 = Op("QShl8x16", (V128, V128), V128)
op_QShl16x8 = Op("QShl16x8", (V128, V128), V128)
op_QShl32x4 = Op("QShl32x4", (V128, V128), V128)
op_QShl64x2 = Op("QShl64x2", (V128, V128), V128)
op_QSal8x16 = Op("QSal8x16", (V128, V128), V128)
op_QSal16x8 = Op("QSal16x8", (V128, V128), V128)
op_QSal32x4 = Op("QSal32x4", (V128, V128), V128)
op_QSal64x2 = Op("QSal64x2", (V128, V128), V128)

# Vector x integer saturating shift
op_QShlNsatSU8x16 = Op("QShlNsatSU8x16", (V128, I8), V128)
op_QShlNsatSU16x8 = Op("QShlNsatSU16x8", (V128, I8), V128)
op_QShlNsatSU32x4 = Op("QShlNsatSU32x4", (V128, I8), V128)
op_QShlNsatSU64x2 = Op("QShlNsatSU64x2", (V128, I8), V128)
op_QShlNsatUU8x16 = Op("QShlNsatUU8x16", (V128, I8), V128)
op_QShlNsatUU16x8 = Op("QShlNsatUU16x8", (V128, I8), V128)
op_QShlNsatUU32x4 = Op("QShlNsatUU32x4", (V128, I8), V128)
op_QShlNsatUU64x2 = Op("QShlNsatUU64x2", (V128, I8), V128)
op_QShlNsatSS8x16 = Op("QShlNsatSS8x16", (V128, I8), V128)
op_QShlNsatSS16x8 = Op("QShlNsatSS16x8", (V128, I8), V128)
op_QShlNsatSS32x4 = Op("QShlNsatSS32x4", (V128, I8), V128)
op_QShlNsatSS64x2 = Op("QShlNsatSS64x2", (V128, I8), V128)

# Vector x vector bidirectional saturating / rounding shift
op_QandUQsh8x16  = Op("QandUQsh8x16",  (V128, V128), V256)
op_QandUQsh16x8  = Op("QandUQsh16x8",  (V128, V128), V256)
op_QandUQsh32x4  = Op("QandUQsh32x4",  (V128, V128), V256)
op_QandUQsh64x2  = Op("QandUQsh64x2",  (V128, V128), V256)
op_QandSQsh8x16  = Op("QandSQsh8x16",  (V128, V128), V256)
op_QandSQsh16x8  = Op("QandSQsh16x8",  (V128, V128), V256)
op_QandSQsh32x4  = Op("QandSQsh32x4",  (V128, V128), V256)
op_QandSQsh64x2  = Op("QandSQsh64x2",  (V128, V128), V256)
op_QandUQRsh8x16 = Op("QandUQRsh8x16", (V128, V128), V256)
op_QandUQRsh16x8 = Op("QandUQRsh16x8", (V128, V128), V256)
op_QandUQRsh32x4 = Op("QandUQRsh32x4", (V128, V128), V256)
op_QandUQRsh64x2 = Op("QandUQRsh64x2", (V128, V128), V256)
op_QandSQRsh8x16 = Op("QandSQRsh8x16", (V128, V128), V256)
op_QandSQRsh16x8 = Op("QandSQRsh16x8", (V128, V128), V256)
op_QandSQRsh32x4 = Op("QandSQRsh32x4", (V128, V128), V256)
op_QandSQRsh64x2 = Op("QandSQRsh64x2", (V128, V128), V256)

# Vector x vector bidirectional / rounding shift
op_Sh8Sx16  = Op("Sh8Sx16",  (V128, V128), V128)
op_Sh16Sx8  = Op("Sh16Sx8",  (V128, V128), V128)
op_Sh32Sx4  = Op("Sh32Sx4",  (V128, V128), V128)
op_Sh64Sx2  = Op("Sh64Sx2",  (V128, V128), V128)
op_Sh8Ux16  = Op("Sh8Ux16",  (V128, V128), V128)
op_Sh16Ux8  = Op("Sh16Ux8",  (V128, V128), V128)
op_Sh32Ux4  = Op("Sh32Ux4",  (V128, V128), V128)
op_Sh64Ux2  = Op("Sh64Ux2",  (V128, V128), V128)
op_Rsh8Sx16 = Op("Rsh8Sx16", (V128, V128), V128)
op_Rsh16Sx8 = Op("Rsh16Sx8", (V128, V128), V128)
op_Rsh32Sx4 = Op("Rsh32Sx4", (V128, V128), V128)
op_Rsh64Sx2 = Op("Rsh64Sx2", (V128, V128), V128)
op_Rsh8Ux16 = Op("Rsh8Ux16", (V128, V128), V128)
op_Rsh16Ux8 = Op("Rsh16Ux8", (V128, V128), V128)
op_Rsh32Ux4 = Op("Rsh32Ux4", (V128, V128), V128)
op_Rsh64Ux2 = Op("Rsh64Ux2", (V128, V128), V128)

# Vector x scalar saturating / rounding narrowing shift right
op_QandQShrNnarrow16Uto8Ux8   = Op("QandQShrNnarrow16Uto8Ux8",   (V128, I8), V128)
op_QandQShrNnarrow32Uto16Ux4  = Op("QandQShrNnarrow32Uto16Ux4",  (V128, I8), V128)
op_QandQShrNnarrow64Uto32Ux2  = Op("QandQShrNnarrow64Uto32Ux2",  (V128, I8), V128)
op_QandQSarNnarrow16Sto8Sx8   = Op("QandQSarNnarrow16Sto8Sx8",   (V128, I8), V128)
op_QandQSarNnarrow32Sto16Sx4  = Op("QandQSarNnarrow32Sto16Sx4",  (V128, I8), V128)
op_QandQSarNnarrow64Sto32Sx2  = Op("QandQSarNnarrow64Sto32Sx2",  (V128, I8), V128)
op_QandQSarNnarrow16Sto8Ux8   = Op("QandQSarNnarrow16Sto8Ux8",   (V128, I8), V128)
op_QandQSarNnarrow32Sto16Ux4  = Op("QandQSarNnarrow32Sto16Ux4",  (V128, I8), V128)
op_QandQSarNnarrow64Sto32Ux2  = Op("QandQSarNnarrow64Sto32Ux2",  (V128, I8), V128)
op_QandQRShrNnarrow16Uto8Ux8  = Op("QandQRShrNnarrow16Uto8Ux8",  (V128, I8), V128)
op_QandQRShrNnarrow32Uto16Ux4 = Op("QandQRShrNnarrow32Uto16Ux4", (V128, I8), V128)
op_QandQRShrNnarrow64Uto32Ux2 = Op("QandQRShrNnarrow64Uto32Ux2", (V128, I8), V128)
op_QandQRSarNnarrow16Sto8Sx8  = Op("QandQRSarNnarrow16Sto8Sx8",  (V128, I8), V128)
op_QandQRSarNnarrow32Sto16Sx4 = Op("QandQRSarNnarrow32Sto16Sx4", (V128, I8), V128)
op_QandQRSarNnarrow64Sto32Sx2 = Op("QandQRSarNnarrow64Sto32Sx2", (V128, I8), V128)
op_QandQRSarNnarrow16Sto8Ux8  = Op("QandQRSarNnarrow16Sto8Ux8",  (V128, I8), V128)
op_QandQRSarNnarrow32Sto16Ux4 = Op("QandQRSarNnarrow32Sto16Ux4", (V128, I8), V128)
op_QandQRSarNnarrow64Sto32Ux2 = Op("QandQRSarNnarrow64Sto32Ux2", (V128, I8), V128)

# ===========================================================================
# 256-BIT SIMD INTEGER
# ===========================================================================

# Pack / unpack
op_V256to64_0 = Op("V256to64_0", (V256,), I64)
op_V256to64_1 = Op("V256to64_1", (V256,), I64)
op_V256to64_2 = Op("V256to64_2", (V256,), I64)
op_V256to64_3 = Op("V256to64_3", (V256,), I64)

op_64x4toV256   = Op("64x4toV256",   (I64, I64, I64, I64), V256)
op_V256toV128_0 = Op("V256toV128_0", (V256,), V128)
op_V256toV128_1 = Op("V256toV128_1", (V256,), V128)
op_V128HLtoV256 = Op("V128HLtoV256", (V128, V128), V256)

# Bitwise
op_AndV256 = Op("AndV256", (V256, V256), V256)
op_OrV256  = Op("OrV256",  (V256, V256), V256)
op_XorV256 = Op("XorV256", (V256, V256), V256)
op_NotV256 = Op("NotV256", (V256,), V256)

# Misc cmp != 0
op_CmpNEZ8x32  = Op("CmpNEZ8x32",  (V256,), V256)
op_CmpNEZ16x16 = Op("CmpNEZ16x16", (V256,), V256)
op_CmpNEZ32x8  = Op("CmpNEZ32x8",  (V256,), V256)
op_CmpNEZ64x4  = Op("CmpNEZ64x4",  (V256,), V256)

# Add / sub
op_Add8x32  = Op("Add8x32",  (V256, V256), V256)
op_Add16x16 = Op("Add16x16", (V256, V256), V256)
op_Add32x8  = Op("Add32x8",  (V256, V256), V256)
op_Add64x4  = Op("Add64x4",  (V256, V256), V256)
op_Sub8x32  = Op("Sub8x32",  (V256, V256), V256)
op_Sub16x16 = Op("Sub16x16", (V256, V256), V256)
op_Sub32x8  = Op("Sub32x8",  (V256, V256), V256)
op_Sub64x4  = Op("Sub64x4",  (V256, V256), V256)

# Comparisons
op_CmpEQ8x32   = Op("CmpEQ8x32",   (V256, V256), V256)
op_CmpEQ16x16  = Op("CmpEQ16x16",  (V256, V256), V256)
op_CmpEQ32x8   = Op("CmpEQ32x8",   (V256, V256), V256)
op_CmpEQ64x4   = Op("CmpEQ64x4",   (V256, V256), V256)
op_CmpGT8Sx32  = Op("CmpGT8Sx32",  (V256, V256), V256)
op_CmpGT16Sx16 = Op("CmpGT16Sx16", (V256, V256), V256)
op_CmpGT32Sx8  = Op("CmpGT32Sx8",  (V256, V256), V256)
op_CmpGT64Sx4  = Op("CmpGT64Sx4",  (V256, V256), V256)

# Scalar shifts
op_ShlN16x16 = Op("ShlN16x16", (V256, I8), V256)
op_ShlN32x8  = Op("ShlN32x8",  (V256, I8), V256)
op_ShlN64x4  = Op("ShlN64x4",  (V256, I8), V256)
op_ShrN16x16 = Op("ShrN16x16", (V256, I8), V256)
op_ShrN32x8  = Op("ShrN32x8",  (V256, I8), V256)
op_ShrN64x4  = Op("ShrN64x4",  (V256, I8), V256)
op_SarN16x16 = Op("SarN16x16", (V256, I8), V256)
op_SarN32x8  = Op("SarN32x8",  (V256, I8), V256)

# Min / max
op_Max8Sx32  = Op("Max8Sx32",  (V256, V256), V256)
op_Max16Sx16 = Op("Max16Sx16", (V256, V256), V256)
op_Max32Sx8  = Op("Max32Sx8",  (V256, V256), V256)
op_Max8Ux32  = Op("Max8Ux32",  (V256, V256), V256)
op_Max16Ux16 = Op("Max16Ux16", (V256, V256), V256)
op_Max32Ux8  = Op("Max32Ux8",  (V256, V256), V256)
op_Min8Sx32  = Op("Min8Sx32",  (V256, V256), V256)
op_Min16Sx16 = Op("Min16Sx16", (V256, V256), V256)
op_Min32Sx8  = Op("Min32Sx8",  (V256, V256), V256)
op_Min8Ux32  = Op("Min8Ux32",  (V256, V256), V256)
op_Min16Ux16 = Op("Min16Ux16", (V256, V256), V256)
op_Min32Ux8  = Op("Min32Ux8",  (V256, V256), V256)

# Multiply
op_Mul16x16    = Op("Mul16x16",    (V256, V256), V256)
op_Mul32x8     = Op("Mul32x8",     (V256, V256), V256)
op_MulHi16Ux16 = Op("MulHi16Ux16", (V256, V256), V256)
op_MulHi16Sx16 = Op("MulHi16Sx16", (V256, V256), V256)

# Saturating add / sub
op_QAdd8Ux32  = Op("QAdd8Ux32",  (V256, V256), V256)
op_QAdd16Ux16 = Op("QAdd16Ux16", (V256, V256), V256)
op_QAdd8Sx32  = Op("QAdd8Sx32",  (V256, V256), V256)
op_QAdd16Sx16 = Op("QAdd16Sx16", (V256, V256), V256)
op_QSub8Ux32  = Op("QSub8Ux32",  (V256, V256), V256)
op_QSub16Ux16 = Op("QSub16Ux16", (V256, V256), V256)
op_QSub8Sx32  = Op("QSub8Sx32",  (V256, V256), V256)
op_QSub16Sx16 = Op("QSub16Sx16", (V256, V256), V256)

# Average
op_Avg8Ux32  = Op("Avg8Ux32",  (V256, V256), V256)
op_Avg16Ux16 = Op("Avg16Ux16", (V256, V256), V256)

# Permute
op_Perm32x8 = Op("Perm32x8", (V256, V256), V256)

# Crypto / hash
op_CipherV128   = Op("CipherV128",   (V128, V128), V128)
op_CipherLV128  = Op("CipherLV128",  (V128, V128), V128)
op_CipherSV128  = Op("CipherSV128",  (V128, V128), V128)
op_NCipherV128  = Op("NCipherV128",  (V128, V128), V128)
op_NCipherLV128 = Op("NCipherLV128", (V128, V128), V128)
op_SHA512       = Op("SHA512",       (V128, I8), V128)
op_SHA256       = Op("SHA256",       (V128, I8), V128)

# ===========================================================================
# 256-BIT SIMD FP
# ===========================================================================

op_Add64Fx4 = Op("Add64Fx4", (I32, V256, V256), V256)
op_Sub64Fx4 = Op("Sub64Fx4", (I32, V256, V256), V256)
op_Mul64Fx4 = Op("Mul64Fx4", (I32, V256, V256), V256)
op_Div64Fx4 = Op("Div64Fx4", (I32, V256, V256), V256)

op_Add32Fx8 = Op("Add32Fx8", (I32, V256, V256), V256)
op_Sub32Fx8 = Op("Sub32Fx8", (I32, V256, V256), V256)
op_Mul32Fx8 = Op("Mul32Fx8", (I32, V256, V256), V256)
op_Div32Fx8 = Op("Div32Fx8", (I32, V256, V256), V256)

op_I32StoF32x8 = Op("I32StoF32x8", (I32, V256), V256)
op_F32toI32Sx8 = Op("F32toI32Sx8", (I32, V256), V256)

op_F32toF16x8 = Op("F32toF16x8", (I32, V256), V128)
op_F16toF32x8 = Op("F16toF32x8", (V128,), V256)

op_Sqrt32Fx8     = Op("Sqrt32Fx8",     (I32, V256), V256)
op_Sqrt64Fx4     = Op("Sqrt64Fx4",     (I32, V256), V256)
op_RSqrtEst32Fx8 = Op("RSqrtEst32Fx8", (V256,), V256)
op_RecipEst32Fx8 = Op("RecipEst32Fx8", (V256,), V256)

op_Max32Fx8 = Op("Max32Fx8", (V256, V256), V256)
op_Min32Fx8 = Op("Min32Fx8", (V256, V256), V256)
op_Max64Fx4 = Op("Max64Fx4", (V256, V256), V256)
op_Min64Fx4 = Op("Min64Fx4", (V256, V256), V256)

op_Rotx32 = Op("Rotx32", (V256,), V256)
op_Rotx64 = Op("Rotx64", (V256,), V256)
