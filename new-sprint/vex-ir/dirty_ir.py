from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Generic, Type, TypeVar

from typed_ir import (
    Polarity, Side,
    InstantiatedRule, RuleSchema,
    array_minus_key_slots,
    Formula, Bang, SidedFormula,
    TypedSequent,
)

# ══════════════════════════════════════════════════════════════════════════════
# Typed Opaque Types
#
# An OpaqueType is a Formula whose case structure exists in theory but is too
# large (or infinite) to enumerate explicitly.  The canonical example is Int32:
# a positive type with 2^32 cases, one per bit-pattern.
#
# Invariant (maintained by convention, not enforced in code):
#   For every OpaqueType T there EXISTS a (possibly infinite / impractical)
#   JumboFormula J such that T ≅ J in the core logic.  We simply never
#   materialise J.
#
# Consequences:
#   - Build rules CAN appear on OpaqueType values (we know which case/value).
#   - Break rules are FORBIDDEN: we cannot enumerate all 2^32 premises.
#   - Structural rules (Identity, Cut) still apply normally.
# ══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class OpaqueType(Formula):
    """Typed opaque primitive type.

    Parameters
    ----------
    name:
        Human-readable type name, e.g. ``"Int32"``, ``"Float64"``.
    polarity:
        POS → active on the right (values are produced / returned).
        NEG → active on the left (values are consumed / demanded).
    """
    name: str

    def get_case_type(self, case_label: Any) -> tuple[SidedFormula, ...]:
        """Return the (Side, Formula) sub-slots for a given case label.

        For an OpaqueType this is theoretical — subclasses override as needed.
        """
        raise NotImplementedError

    def __str__(self) -> str:
        pol = "+" if self.polarity == Polarity.POS else "-"
        return f"{self.name}^{pol}"


T = TypeVar("T")


@dataclass(frozen=True)
class FlatType(OpaqueType, Generic[T]):
    """Typed flat type: every case produces no sub-slots."""
    label_type: Type[T]

    def get_case_type(self, case_label: T) -> tuple[SidedFormula, ...]:
        return ()


# ── Pre-defined primitive types ───────────────────────────────────────────────

Int32   = FlatType(label_type=int,   name="Int32",   polarity=Polarity.POS)
Int64   = FlatType(label_type=int,   name="Int64",   polarity=Polarity.POS)
Float32 = FlatType(label_type=float, name="Float32", polarity=Polarity.POS)
Float64 = FlatType(label_type=float, name="Float64", polarity=Polarity.POS)
# Bool is treated as opaque for uniformity; an explicit pass converts it to a
# two-case JumboFormula when Break is needed.
Bool    = FlatType(label_type=bool,  name="Bool",    polarity=Polarity.POS)

# ══════════════════════════════════════════════════════════════════════════════
# Admissible Rules
#
# An admissible rule is a RuleSchema derivable from the core rules in
# principle, but whose derivation is NOT spelled out here.  It is justified
# by a prose argument and validated by a custom check_shape and check_type.
#
# Structural invariant
# --------------------
# - CoreRule       : tag for the rules above.  Never subclassed here.
# - AdmissibleRule : base class for all rules defined in this module or
#                    downstream (dirty_ir.py, etc.).
#
# At runtime, isinstance(r, AdmissibleRule) reliably identifies "dirty" rules,
# enabling warnings, audits, or future proof-checking passes.
#
# Adding a new admissible rule
# ----------------------------
# 1. Subclass AdmissibleRule.
# 2. Write a comment justifying admissibility; add any extra fields needed.
# 3. Override check_shape and check_type using assert-based checking (same convention as core rules).
# ══════════════════════════════════════════════════════════════════════════════

class AdmissibleRule(RuleSchema):
    """Base class for all admissible (derived) rule schemas."""

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        raise NotImplementedError(
            f"AdmissibleRule subclass {type(self).__name__} must implement check_shape."
        )

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        raise NotImplementedError(
            f"AdmissibleRule subclass {type(self).__name__} must implement check_type."
        )

def _assert_opaque(f: Formula, t: OpaqueType, label: str) -> None:
    assert f == t, f"Expected {t} for {label}, got {f}"


# ── Typed flat operations ─────────────────────────────────────────────────────
# Admissible by pure case analysis: Break each arg, then Build the result.

@dataclass(frozen=True)
class FlatOperation(AdmissibleRule):
    name: str
    
    # these are part of the rule name and not erased
    args: tuple[FlatType, ...]
    result: FlatType

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 0, "FlatOperation has no premises"
        assert instance.bottom.count_left  == len(self.args), (
            f"Expected {len(self.args)} inputs, got {instance.bottom.count_left}"
        )
        assert instance.bottom.count_right == 1, (
            f"Expected 1 output, got {instance.bottom.count_right}"
        )

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.tops) == 0, "FlatOperation has no premises"
        lefts  = instance.bottom.left
        rights = instance.bottom.right
        assert len(lefts)  == len(self.args), (
            f"Expected {len(self.args)} inputs, got {len(lefts)}"
        )
        assert len(rights) == 1, f"Expected 1 output, got {len(rights)}"
        for i, arg in enumerate(self.args):
            _assert_opaque(lefts[i][1], arg, f"input {i}")
        _assert_opaque(rights[0][1], self.result, "output")

    def __repr__(self) -> str:
        return f"<FlatOp {self.name}: {self.args} -> {self.result}>"


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

        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == self.principal.active_side

        assert len(instance.tops) == len(case_sides)
        for i, (expected_side, _) in enumerate(case_sides):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == 1
            t_side = top.formulas[instance.key_slots_tops[i][0]]
            assert t_side == expected_side

        top_combined: list[Side] = []
        for i, top in enumerate(instance.tops):
            top_combined.extend(array_minus_key_slots(top.formulas, instance.key_slots_tops[i]))
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == tuple(top_combined)

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        case_formulas = self.principal.get_case_type(self.case_label)

        assert len(instance.key_slots_bottom) == 1
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (self.principal.active_side, self.principal)

        assert len(instance.tops) == len(case_formulas)
        for i, (expected_side, expected_f) in enumerate(case_formulas):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == 1
            t_side, t_f = top.formulas[instance.key_slots_tops[i][0]]
            assert (t_side, t_f) == (expected_side, expected_f)

        top_combined: list[SidedFormula] = []
        for i, top in enumerate(instance.tops):
            top_combined.extend(array_minus_key_slots(top.formulas, instance.key_slots_tops[i]))
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == tuple(top_combined)


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
        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == Side.LEFT

        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            assert bottom_others == tuple(top_others), f"top {i} context mismatch"

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.key_slots_bottom) == 1
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (Side.LEFT, Bool)

        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            assert bottom_others == tuple(top_others), f"top {i} context mismatch"


# ── Exponential admissible rules ────────────────────────────────────────

@dataclass(frozen=True)
class WeakPromotion(AdmissibleRule):
    polarity: Polarity
    principle_formula_index: int

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        l = len(top.formulas)
        assert l == len(instance.key_slots_tops[0])
        assert l == len(instance.key_slots_bottom)
        assert l == len(instance.bottom.formulas)

        active_side  = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        passive_side = active_side.flip()
        for i, side in enumerate(top.formulas):
            bottom_side = instance.bottom.formulas[instance.key_slots_bottom[i]]
            if i == self.principle_formula_index:
                assert bottom_side == active_side, (
                    f"Principal formula in bottom of WeakPromotion must be on side "
                    f"{active_side}, found {bottom_side}"
                )
                assert bottom_side == side
            else:
                assert bottom_side == passive_side, (
                    f"Passthrough formula in bottom of WeakPromotion must be on side "
                    f"{passive_side}, found {bottom_side}"
                )
                assert bottom_side == side

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        l = len(top.formulas)
        assert l == len(instance.key_slots_tops[0])
        assert l == len(instance.key_slots_bottom)
        assert l == len(instance.bottom.formulas)

        active_side  = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        passive_side = active_side.flip()
        for i, (side, f) in enumerate(top.formulas):
            bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[i]]
            if i == self.principle_formula_index:
                assert isinstance(bottom_f, Bang)
                assert bottom_f.polarity == self.polarity
                assert bottom_f.sub == f
                assert bottom_side == active_side, (
                    f"Principal formula in bottom of WeakPromotion must be on side "
                    f"{active_side}, found {bottom_side}"
                )
                assert bottom_side == side
            else:
                assert isinstance(bottom_f, Bang)
                assert bottom_f.sub == f
                assert bottom_side == passive_side, (
                    f"Passthrough formula in bottom of WeakPromotion must be on side "
                    f"{passive_side}, found {bottom_side}"
                )
                assert bottom_side == side


@dataclass(frozen=True)
class Digging(AdmissibleRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 1
        assert len(instance.key_slots_bottom) == 1

        top_side    = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert bottom_side == top_side
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Digging must be on side {expected}, found {bottom_side}"
        )

        top_others    = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 1
        assert len(instance.key_slots_bottom) == 1

        top_side,    top_f    = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(top_f, Bang)
        assert isinstance(bottom_f, Bang)
        assert top_f.polarity == self.polarity
        assert bottom_f.sub == top_f
        assert bottom_side == top_side
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Digging must be on side {expected}, found {bottom_side}"
        )

        top_others    = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others


@dataclass(frozen=True)
class Absorption(AdmissibleRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 2
        assert len(instance.key_slots_bottom) == 1

        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Absorption must be on side {expected}, found {bottom_side}"
        )

        top_side0 = top.formulas[instance.key_slots_tops[0][0]]
        assert top_side0 == expected, (
            f"First principal formula in top of Absorption must be on side {expected}, found {top_side0}"
        )
        top_side1 = top.formulas[instance.key_slots_tops[0][1]]
        assert top_side1 == expected, (
            f"Second principal formula in top of Absorption must be on side {expected}, found {top_side1}"
        )

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 2
        assert len(instance.key_slots_bottom) == 1

        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Absorption must be on side {expected}, found {bottom_side}"
        )

        top_side0, top_f0 = top.formulas[instance.key_slots_tops[0][0]]
        assert top_side0 == expected
        assert top_f0 == bottom_f.sub, (
            f"First principal in top of Absorption must be subformula of bottom, "
            f"expected {bottom_f.sub}, found {top_f0}"
        )

        top_side1, top_f1 = top.formulas[instance.key_slots_tops[0][1]]
        assert top_side1 == expected
        assert top_f1 == bottom_f, (
            f"Second principal in top of Absorption must equal bottom formula, "
            f"expected {bottom_f}, found {top_f1}"
        )


@dataclass(frozen=True)
class Multiplexing(AdmissibleRule):
    polarity: Polarity
    count: int = 2

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == self.count
        assert len(instance.key_slots_bottom) == 1

        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Multiplexing must be on side {expected}, found {bottom_side}"
        )

        for i in range(self.count):
            top_side = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected, (
                f"Principal formula in top of Multiplexing must be on side {expected}, found {top_side}"
            )

        top_others    = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == self.count
        assert len(instance.key_slots_bottom) == 1

        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        expected = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Multiplexing must be on side {expected}, found {bottom_side}"
        )

        for i in range(self.count):
            top_side, top_f = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected
            assert top_f == bottom_f.sub, (
                f"Principal formula in top of Multiplexing must be subformula of bottom, "
                f"expected {bottom_f.sub}, found {top_f}"
            )

        top_others    = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others
