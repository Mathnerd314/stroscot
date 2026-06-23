from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Callable, Generic, Optional, TYPE_CHECKING, Type, TypeVar

if TYPE_CHECKING:
    from typed_ir import InstantiatedRule

from typed_ir import (
    Bang, Formula, JumboFormula, Polarity, Side, Sequent, SidedFormula,
    RuleSchema, InstantiatedRule, array_minus_key_slots
)

# ══════════════════════════════════════════════════════════════════════════════
# Opaque Jumbo Types
#
# An OpaqueType is a JumboFormula-like type whose case structure exists in
# theory but is too large (or infinite) to enumerate explicitly.  The canonical
# example is Int32: a positive type with 2^32 cases, one per bit-pattern.
#
# Invariant (maintained by convention, not enforced in code):
#   For every OpaqueType T there EXISTS a (possibly infinite / impractical)
#   JumboFormula J such that T ≅ J in the core logic.  We simply never
#   materialise J.
#
# Consequences:
#   - Build rules CAN appear on OpaqueType values (we produce one; we know
#     which "case" / value we are building).
#   - Break rules are FORBIDDEN: we cannot enumerate all 2^32 premises.
#   - Structural rules (Identity, Cut) still apply normally.
# ══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class OpaqueType(Formula):
    """An opaque primitive type.

    Parameters
    ----------
    name:
        Human-readable type name, e.g. ``"Int32"``, ``"Float64"``.
    polarity:
        POS  → active on the right (values are produced / returned).
        NEG  → active on the left  (values are consumed / demanded).

    Theoretically an OpaqueType has a finite case structure just like a JumboType,
    but explicit enumeration is impractical and it is instead treated as a function from cases to formulas.
    For example, Int32 has 2^32 cases, one per bit-pattern.
    """
    name: str
    polarity: Polarity

    @property
    def active_side(self) -> Side:
        return Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT

    @property
    def passive_side(self) -> Side:
        return self.active_side.flip()
    
    def get_case_formula(self, case_label: Any) -> tuple[SidedFormula, ...]:
        """Return the formula corresponding to a given case label.

        For an OpaqueType, this is a theoretical operation: we cannot enumerate
        all cases, but we can still refer to a specific case by label.
        """
        raise NotImplementedError

    def __str__(self) -> str:
        pol = "+" if self.polarity == Polarity.POS else "-"
        return f"{self.name}^{pol}"


# ── Pre-defined primitive types ───────────────────────────────────────────────

T = TypeVar('T')

@dataclass(frozen=True)
class FlatType(OpaqueType, Generic[T]):
    """A flat type with no internal structure."""
    label_type: Type[T]
    # nb: we want to specify polarity=pos for flat types,
    # but python has issues with specifying a default value for a field in a frozen dataclass that is inherited from a base class.
    # So we allow NEG for completeness, a flat type is an "or" of units so this corresponds to an "and" of bottoms
    def get_case_formula(self, case_label: T) -> tuple[SidedFormula, ...]:
        # Flat types have no sub-premises; every case is just empty.
        return ()

Int32  = FlatType(label_type=int, name="Int32", polarity=Polarity.POS)
Int64  = FlatType(label_type=int, name="Int64", polarity=Polarity.POS)
Float32 = FlatType(label_type=float, name="Float32", polarity=Polarity.POS)
Float64 = FlatType(label_type=float, name="Float64", polarity=Polarity.POS)
Bool   = FlatType(label_type=bool, name="Bool", polarity=Polarity.POS) # we treat it as opaque for uniformity, explicit pass converts to two-case JumboFormula.


# ══════════════════════════════════════════════════════════════════════════════
# Admissible Rules
#
# An admissible rule is a RuleSchema that is derivable from the core rules in
# principle, but whose derivation is NOT spelled out in this code.  It is
# instead justified by a prose argument (the `justification` field) and
# validated by a custom `check_shape` implementation supplied by the subclass.
#
# Structural invariant
# --------------------
# - CoreRule        : tag for rules defined in typed_ir.py (Identity, Cut,
#                     Build, Break, Promotion, Dereliction, Weakening,
#                     Contraction).  Never subclassed here.
# - AdmissibleRule  : base class for all rules defined in THIS module.
#
# The split is purely *documentary* — at runtime, isinstance(r, AdmissibleRule)
# reliably identifies "dirty" rules, enabling warnings, audits, or future
# proof-checking passes.
#
# Adding a new admissible rule
# ----------------------------
# 1. Subclass AdmissibleRule.
# 2. Fill in `name`, write a comment justifying admissibility, and optionally add any extra fields needed to parameterize the rule.
# 3. Override `check_shape(self, instance)` using normal assert-based checking
#    (same convention as the core rules in typed_ir.py).
# 4. Optionally register a pretty-printer entry in dirty_connectives.py.
#
# See examples.
#
# ══════════════════════════════════════════════════════════════════════════════

class AdmissibleRule(RuleSchema):
    """Base class for all admissible (derived) rule schemas.

    Every subclass must supply:
    - ``name``          (str)  — short identifier used in pretty-printing
    - ``check_shape``          — instance-level shape checker
    """
    def check_shape(self, instance: InstantiatedRule) -> None:
        raise NotImplementedError(
            f"AdmissibleRule subclass {type(self).__name__} must implement check_shape."
        )

# Ops on flat types are always admissible just by pure case analysis:
# break arg1, break arg2, etc., until no args left, then Build.
# No special justification needed.
# Note that to actually implement the operation this way, we would need to choose an evaluation order of the arguments (e.g. left-to-right),
# and specify the function as a huge table mapping each combination of argument values to the result.
# but as an admissible rule we only care about the shape of the sequent and the evaluation order is irrelevant.

def _assert_opaque(f: Formula, t: OpaqueType, label: str) -> None:
    assert f == t, f"Expected {t} for {label}, got {f}"

@dataclass(frozen=True)
class FlatOperation(AdmissibleRule):
    name: str
    args: tuple[FlatType, ...]
    result: FlatType

    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 0, "Arithmetic rules have no premises"
        lefts  = instance.bottom.left
        rights = instance.bottom.right
        assert len(lefts)  == len(self.args), f"Expected {len(self.args)} inputs, got {len(lefts)}"
        assert len(rights) == 1, f"Expected 1 output, got {len(rights)}"
        for i, arg in enumerate(self.args):
            _assert_opaque(lefts[i], arg, f"input {i}")
        _assert_opaque(rights[0], self.result, "output")

    def __repr__(self) -> str:
        return f"<FlatOperation {self.name} {self.args} -> {self.result}>"

AddI32 = FlatOperation(name="add_i32", args=(Int32, Int32), result=Int32)
SubI32 = FlatOperation(name="sub_i32", args=(Int32, Int32), result=Int32)
MulI32 = FlatOperation(name="mul_i32", args=(Int32, Int32), result=Int32)
DivI32 = FlatOperation(name="div_i32", args=(Int32, Int32), result=Int32) # partial — undefined behaviour on division by zero (caller's responsibility)
RemI32 = FlatOperation(name="rem_i32", args=(Int32, Int32), result=Int32) # partial — undefined behaviour on division by zero (caller's responsibility)
NegI32 = FlatOperation(name="neg_i32", args=(Int32,), result=Int32)
AndI32 = FlatOperation(name="and_i32", args=(Int32, Int32), result=Int32)
OrI32  = FlatOperation(name="or_i32",  args=(Int32, Int32), result=Int32)
XorI32 = FlatOperation(name="xor_i32", args=(Int32, Int32), result=Int32)
NotI32 = FlatOperation(name="not_i32", args=(Int32,), result=Int32)
ShlI32 = FlatOperation(name="shl_i32", args=(Int32, Int32), result=Int32)
ShrI32 = FlatOperation(name="shr_i32", args=(Int32, Int32), result=Int32) # arithmetic right shift (sign-extending)
EqI32  = FlatOperation(name="eq_i32",  args=(Int32, Int32), result=Bool)
LtI32  = FlatOperation(name="lt_i32",  args=(Int32, Int32), result=Bool)
LeI32  = FlatOperation(name="le_i32",  args=(Int32, Int32), result=Bool)

# ── Constant introduction ─────────────────────────────────────────────────────

@dataclass(frozen=True)
class Const(AdmissibleRule):
    """Introduce a literal value (case) of an OpaqueType into the sequent. Equviakent to Build on the corresponding case of the theoretical JumboFormula."""
    principal: OpaqueType
    case_label: Any
    def check_shape(self, instance: InstantiatedRule) -> None:
        case_formulas = self.principal.get_case_formula(self.case_label)
        
        # check bottom - one jumbo formula on the active side
        assert len(instance.key_slots_bottom) == 1
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (self.principal.active_side, self.principal)
        
        # check top - one top per each subformula of the case, with the correct sides
        assert len(instance.tops) == len(case_formulas)
        for i, (side, f) in enumerate(case_formulas):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == 1
            t_side, t_f = top.formulas[instance.key_slots_tops[i][0]]
            assert (side, f) == (t_side, t_f)
            
        # check context preservation - all other formulas in the bottom must be present in each top
        top_combined = []
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            top_combined.extend(top_others)

        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == tuple(top_combined)

# ══════════════════════════════════════════════════════════════════════════════
# Control-flow — admissible rules operating on opaque Bool type
# ══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class IfBool(AdmissibleRule):
    """Conditional branch on a Bool value.

    Sequent shape:
        then_val : Gamma ⊢ Delta  else_val : Gamma ⊢ Delta
        ---------------------------
        cond : Bool, left : Gamma ⊢ result : Delta

    where Gamma and Delta are arbitrary contexts of formulas (possibly empty).

    This is admissible as Break on the (non-opaque) Bool type.
    This shows we don't really need JumboFormula at all, we could in principle
    implement everything using custom opaque types and admissible rules.
    """
    name = "if_bool"
    branch_type: Formula = field(default_factory=lambda: Int32)

    def check_shape(self, instance: InstantiatedRule) -> None:
        # check bottom - Bool on the passive side (left)
        assert len(instance.key_slots_bottom) == 1
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (Side.LEFT, Bool)

        # check context preservation - all other formulas in the bottom must be present in each top
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            assert bottom_others == tuple(top_others), f"top {i} context mismatch"

# ==══════════════════════════════════════════════════════════════════════════════
# Additional admissible rules for the Bang type
# ==══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class WeakPromotion(AdmissibleRule):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        l = len(top.formulas)
        # all slots must be marked as key slots and match 1-1 between top and bottom
        assert l == len(instance.key_slots_tops[0])
        assert l == len(instance.key_slots_bottom)
        assert l == len(instance.bottom.formulas)
        
        # all formulas are promoted to bang-wrapped, but only principal formula is on its active side and the rest are passthrough formulas on the opposite side
        num_principal_formulas = 0
        for i, (side, f) in enumerate(top.formulas):
            if isinstance(instance.bottom.formulas[0], Bang) and instance.bottom.formulas[0].sub == f:
                bang = instance.bottom.formulas[0]
                # check whether passthrough or principal formula
                if bang.polarity == Polarity.POS and side == Side.LEFT or bang.polarity == Polarity.NEG and side == Side.RIGHT:
                    # passive bang formula
                    continue
                elif bang.polarity == self.polarity and side == (Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT):
                    # principal formula
                    num_principal_formulas += 1
            else:
                raise AssertionError(f"Formula {f} at slot {i} in top does not match expected shape for WeakPromotion")
        
        assert num_principal_formulas == 1, f"Expected exactly one principal formula in top for WeakPromotion, found {num_principal_formulas}"

@dataclass(frozen=True)
class Digging(AdmissibleRule):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 1
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the top must be the subformula of the principal formula in the bottom, with the correct side
        top_side, top_f = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(top_f, Bang)
        assert isinstance(bottom_f, Bang)
        assert top_f.polarity == self.polarity
        assert bottom_f.sub == top_f
        assert bottom_side == top_side
        expected_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_side, f"Principal formula in bottom of Dereliction must be on side {expected_side}, found {bottom_side}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

@dataclass(frozen=True)
class Absorption(AdmissibleRule):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 2
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the bottom must be the "contracted" version of the principal formula in the top, with the correct side
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Absorption must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the top is one dereliction and one exact match
        top_side, top_f = top.formulas[instance.key_slots_tops[0][0]]
        assert top_side == expected_bottom_side, f"Principal formula in top of Absorption must be on side {expected_bottom_side}, found {top_side}"
        assert top_f == bottom_f.sub, f"Principal formula in top of Absorption must be the subformula of the principal formula in bottom, expected {bottom_f.sub}, found {top_f}"

        top_side, top_f = top.formulas[instance.key_slots_tops[0][1]]
        assert top_side == expected_bottom_side, f"Principal formula in top of Absorption must be on side {expected_bottom_side}, found {top_side}"
        assert top_f == bottom_f, f"Principal formula in top of Absorption must be the same as the principal formula in bottom, expected {bottom_f}, found {top_f}"


@dataclass(frozen=True)
class Multiplexing(AdmissibleRule):
    polarity: Polarity
    count: int = 2  # number of copies to contract into (default 2 for binary contraction)
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == self.count
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the bottom must be the "contracted" version of the principal formula in the top, with the correct side
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Multiplexing must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the each instance of the top formula matches the bottom formula
        for i in range(self.count):
            top_side, top_f = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected_bottom_side, f"Principal formula in top of Multiplexing must be on side {expected_bottom_side}, found {top_side}"
            assert top_f == bottom_f.sub, f"Principal formula in top of Multiplexing must be the subformula of the principal formula in bottom, expected {bottom_f.sub}, found {top_f}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others
