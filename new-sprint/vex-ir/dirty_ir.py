from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Generic, Type, TypeVar

from ir_types import (
    Polarity, Side,
    InstantiatedRule, RuleSchema,
    array_minus_key_slots,
    Formula, SidedFormula,
    TypedSequent,
)
from core_ir import AdmissibleRule, OpaqueType, FlatType, FlatOperation, Box

# ── Pre-defined primitive types ───────────────────────────────────────────────

Bool    = FlatType(label_type=bool,  name="Bool",    polarity=Polarity.POS) # I1/U1 in Valgrind, treated as an opaque type in the IR for uniformity
Int8   = FlatType(label_type=int,   name="Int8",   polarity=Polarity.POS)
Int16   = FlatType(label_type=int,   name="Int16",   polarity=Polarity.POS)
Int32   = FlatType(label_type=int,   name="Int32",   polarity=Polarity.POS)
Int64   = FlatType(label_type=int,   name="Int64",   polarity=Polarity.POS)
Int128   = FlatType(label_type=int,   name="Int128",   polarity=Polarity.POS)
Float16 = FlatType(label_type=float, name="Float16", polarity=Polarity.POS)
Float32 = FlatType(label_type=float, name="Float32", polarity=Polarity.POS)
Float64 = FlatType(label_type=float, name="Float64", polarity=Polarity.POS)
Float128 = FlatType(label_type=float, name="Float128", polarity=Polarity.POS)
Int16   = FlatType(label_type=int,   name="Int16",   polarity=Polarity.POS)
Int32   = FlatType(label_type=int,   name="Int32",   polarity=Polarity.POS)
Int64   = FlatType(label_type=int,   name="Int64",   polarity=Polarity.POS)
Vec128 = FlatType(label_type=bytes, name="Vec128", polarity=Polarity.POS) # 128-bit vector type, represented as bytes
Vec256 = FlatType(label_type=bytes, name="Vec256", polarity=Polarity.POS) # 256-bit vector type, represented as bytes
Ptr = FlatType(label_type=int,   name="Ptr",     polarity=Polarity.POS) # Opaque pointer type, represented as an integer address

# ── Typed flat operations ─────────────────────────────────────────────────────
# Admissible by pure case analysis: Break each arg, then Build the result.


AddI32 = FlatOperation(name="add_i32", args=(Int32, Int32), result=Int32)
SubI32 = FlatOperation(name="sub_i32", args=(Int32, Int32), result=Int32)
MulI32 = FlatOperation(name="mul_i32", args=(Int32, Int32), result=Int32)
DivI32 = FlatOperation(name="div_i32", args=(Int32, Int32), result=Int32)
RemI32 = FlatOperation(name="rem_i32", args=(Int32, Int32), result=Int32)
NegI32 = FlatOperation(name="neg_i32", args=(Int32,),       result=Int32)
AndI32 = FlatOperation(name="and_i32", args=(Int32, Int32), result=Int32)
OrI32  = FlatOperation(name="or_i32",  args=(Int32, Int32), result=Int32)
XorI32 = FlatOperation(name="xor_i32", args=(Int32, Int32), result=Int32)
NotI32 = FlatOperation(name="not_i32", args=(Int32,),       result=Int32)
ShlI32 = FlatOperation(name="shl_i32", args=(Int32, Int32), result=Int32)
ShrI32 = FlatOperation(name="shr_i32", args=(Int32, Int32), result=Int32)  # arithmetic right shift
EqI32  = FlatOperation(name="eq_i32",  args=(Int32, Int32), result=Bool)
LtI32  = FlatOperation(name="lt_i32",  args=(Int32, Int32), result=Bool)
LeI32  = FlatOperation(name="le_i32",  args=(Int32, Int32), result=Bool)


# ── Constant introduction ───────────────────────────────────────────────

@dataclass(frozen=True)
class Const(AdmissibleRule):
    """Introduce a literal value (case) of an OpaqueType.

    Equivalent to Build on the corresponding case of the theoretical
    JumboFormula for that type.
    """
    principal: OpaqueType
    case_label: Any

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        case_sides = self.principal.get_case_type(self.case_label)

        assert len(instance.key_slots_conclusion) == 1
        side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert side == self.principal.active_side

        assert len(instance.premises) == len(case_sides)
        for i, (expected_side, _) in enumerate(case_sides):
            prem_prop = instance.premises[i]
            assert len(instance.key_slots_premises[i]) == 1
            prem_prop_side = prem_prop.formulas[instance.key_slots_premises[i][0]]
            assert prem_prop_side == expected_side

        prem_combined: list[Side] = []
        for i, prem_prop in enumerate(instance.premises):
            prem_combined.extend(array_minus_key_slots(prem_prop.formulas, instance.key_slots_premises[i]))
        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conclusion_others == tuple(prem_combined)

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        case_formulas = self.principal.get_case_type(self.case_label)

        assert len(instance.key_slots_conclusion) == 1
        side, jf = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert (side, jf) == (self.principal.active_side, self.principal)

        assert len(instance.premises) == len(case_formulas)
        for i, (expected_side, expected_f) in enumerate(case_formulas):
            prem_prop = instance.premises[i]
            assert len(instance.key_slots_premises[i]) == 1
            p_side, p_f = prem_prop.formulas[instance.key_slots_premises[i][0]]
            assert (p_side, p_f) == (expected_side, expected_f)

        prem_combined: list[SidedFormula] = []
        for i, prem_prop in enumerate(instance.premises):
            prem_combined.extend(array_minus_key_slots(prem_prop.formulas, instance.key_slots_premises[i]))
        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conclusion_others == tuple(prem_combined)


# ── Control flow ──────────────────────────────────────────────────────────────

@dataclass(frozen=True)
class IfBool(AdmissibleRule):
    """Conditional branch on a Bool value (typed — checks formula identity).

    Sequent shape:
        then_branch : Γ ⊢ Δ    else_branch : Γ ⊢ Δ
        ─────────────────────────────────────────────
        cond : Bool LEFT,  Γ ⊢ Δ

    Admissible as Break on the (non-opaque) two-case Bool JumboFormula.
    """
    name = "if_bool"

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.key_slots_conclusion) == 1
        side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert side == Side.LEFT

        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        for i, prem_prop in enumerate(instance.premises):
            prem_others = array_minus_key_slots(prem_prop.formulas, instance.key_slots_premises[i])
            assert conclusion_others == tuple(prem_others), f"premise {i} context mismatch"

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.key_slots_conclusion) == 1
        side, jf = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert (side, jf) == (Side.LEFT, Bool)

        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        for i, prem_prop in enumerate(instance.premises):
            prem_others = array_minus_key_slots(prem_prop.formulas, instance.key_slots_premises[i])
            assert conclusion_others == tuple(prem_others), f"premise {i} context mismatch"


# ── Exponential admissible rules ────────────────────────────────────────

@dataclass(frozen=True)
class WeakPromotion(AdmissibleRule):
    polarity: Polarity
    principle_formula_index: int

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        l = len(premise.formulas)
        assert l == len(instance.key_slots_premises[0])
        assert l == len(instance.key_slots_conclusion)
        assert l == len(instance.conclusion.formulas)

        active_side  = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        passive_side = active_side.flip()
        for i, side in enumerate(premise.formulas):
            conclusion_side = instance.conclusion.formulas[instance.key_slots_conclusion[i]]
            if i == self.principle_formula_index:
                assert conclusion_side == active_side, (
                    f"Principal formula in bottom of WeakPromotion must be on side "
                    f"{active_side}, found {conclusion_side}"
                )
                assert conclusion_side == side
            else:
                assert conclusion_side == passive_side, (
                    f"Passthrough formula in bottom of WeakPromotion must be on side "
                    f"{passive_side}, found {conclusion_side}"
                )
                assert conclusion_side == side

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        l = len(premise.formulas)
        assert l == len(instance.key_slots_premises[0])
        assert l == len(instance.key_slots_conclusion)
        assert l == len(instance.conclusion.formulas)

        active_side  = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        passive_side = active_side.flip()
        for i, (side, f) in enumerate(premise.formulas):
            conc_side, conc_f = instance.conclusion.formulas[instance.key_slots_conclusion[i]]
            if i == self.principle_formula_index:
                assert isinstance(conc_f, Box)
                assert conc_f.polarity == self.polarity
                assert conc_f.sub == f
                assert conc_side == active_side, (
                    f"Principal formula in bottom of WeakPromotion must be on side "
                    f"{active_side}, found {conc_side}"
                )
                assert conc_side == side
            else:
                assert isinstance(conc_f, Box)
                assert conc_f.sub == f
                assert conc_side == passive_side, (
                    f"Passthrough formula in bottom of WeakPromotion must be on side "
                    f"{passive_side}, found {conc_side}"
                )
                assert conc_side == side


@dataclass(frozen=True)
class Digging(AdmissibleRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == 1
        assert len(instance.key_slots_conclusion) == 1

        premise_side    = premise.formulas[instance.key_slots_premises[0][0]]
        conclusion_side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert conclusion_side == premise_side
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conclusion_side == expected, (
            f"Principal formula in bottom of Digging must be on side {expected}, found {conclusion_side}"
        )

        premise_others    = array_minus_key_slots(premise.formulas, instance.key_slots_premises[0])
        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conclusion_others == premise_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == 1
        assert len(instance.key_slots_conclusion) == 1

        premise_side, premise_f    = premise.formulas[instance.key_slots_premises[0][0]]
        conc_side, conc_f = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert isinstance(premise_f, Box)
        assert isinstance(conc_f, Box)
        assert premise_f.polarity == self.polarity
        assert conc_f.sub == premise_f
        assert conc_side == premise_side
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conc_side == expected, (
            f"Principal formula in bottom of Digging must be on side {expected}, found {conc_side}"
        )

        prem_others    = array_minus_key_slots(premise.formulas, instance.key_slots_premises[0])
        conc_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conc_others == prem_others


@dataclass(frozen=True)
class Absorption(AdmissibleRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == 2
        assert len(instance.key_slots_conclusion) == 1

        conc_side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conc_side == expected, (
            f"Principal formula in bottom of Absorption must be on side {expected}, found {conc_side}"
        )

        prem_side0 = premise.formulas[instance.key_slots_premises[0][0]]
        assert prem_side0 == expected, (
            f"First principal formula in top of Absorption must be on side {expected}, found {prem_side0}"
        )
        prem_side1 = premise.formulas[instance.key_slots_premises[0][1]]
        assert prem_side1 == expected, (
            f"Second principal formula in top of Absorption must be on side {expected}, found {prem_side1}"
        )

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == 2
        assert len(instance.key_slots_conclusion) == 1

        conclusion_side, conclusion_f = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert isinstance(conclusion_f, Box)
        assert conclusion_f.polarity == self.polarity
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conclusion_side == expected, (
            f"Principal formula in bottom of Absorption must be on side {expected}, found {conclusion_side}"
        )

        premise_side0, premise_f0 = premise.formulas[instance.key_slots_premises[0][0]]
        assert premise_side0 == expected
        assert premise_f0 == conclusion_f.sub, (
            f"First principal in top of Absorption must be subformula of bottom, "
            f"expected {conclusion_f.sub}, found {premise_f0}"
        )

        premise_side1, premise_f1 = premise.formulas[instance.key_slots_premises[0][1]]
        assert premise_side1 == expected
        assert premise_f1 == conclusion_f, (
            f"Second principal in top of Absorption must equal bottom formula, "
            f"expected {conclusion_f}, found {premise_f1}"
        )


@dataclass(frozen=True)
class Multiplexing(AdmissibleRule):
    polarity: Polarity
    count: int = 2

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == self.count
        assert len(instance.key_slots_conclusion) == 1

        conclusion_side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conclusion_side == expected, (
            f"Principal formula in bottom of Multiplexing must be on side {expected}, found {conclusion_side}"
        )

        for i in range(self.count):
            premise_side = premise.formulas[instance.key_slots_premises[0][i]]
            assert premise_side == expected, (
                f"Principal formula in top of Multiplexing must be on side {expected}, found {premise_side}"
            )

        prem_others = array_minus_key_slots(premise.formulas, instance.key_slots_premises[0])
        conc_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conc_others == prem_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == self.count
        assert len(instance.key_slots_conclusion) == 1

        conclusion_side, conclusion_f = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert isinstance(conclusion_f, Box)
        assert conclusion_f.polarity == self.polarity
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conclusion_side == expected, (
            f"Principal formula in bottom of Multiplexing must be on side {expected}, found {conclusion_side}"
        )

        for i in range(self.count):
            premise_side, premise_f = premise.formulas[instance.key_slots_premises[0][i]]
            assert premise_side == expected
            assert premise_f == conclusion_f.sub, (
                f"Principal formula in top of Multiplexing must be subformula of bottom, "
                f"expected {conclusion_f.sub}, found {premise_f}"
            )

        prem_others = array_minus_key_slots(premise.formulas, instance.key_slots_premises[0])
        conc_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conc_others == prem_others
