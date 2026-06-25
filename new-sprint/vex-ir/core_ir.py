from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Generic, Literal, Type, TypeVar, Optional
from ir_types import (
    Formula,
    Polarity,
    Side,
    Sequent,
    SidedFormula,
    InstantiatedRule,
    Slot,
    RuleSchema,
    array_minus_key_slots,
    slot_side,
)

@dataclass(frozen=True)
class Atom(Formula):
    name: str


@dataclass(frozen=True)
class Case(Generic[Slot]):
    """One case of a JumboFormula, with formula-annotated sub-slots."""
    label: Any
    formulas: Sequent[Slot]
    
    def __init__(self, label: Any, formulas: Any) -> None:
        object.__setattr__(self, "label", label)
        if isinstance(formulas, Sequent):
            object.__setattr__(self, "formulas", formulas)
        elif isinstance(formulas, tuple):
            object.__setattr__(self, "formulas", Sequent(formulas=formulas))
        else:
            raise TypeError(f"Invalid type for formulas: {type(formulas)}")
    
    def erase(self) -> Case[Side]:
        """Project to the erased version (drop Formula annotations)."""
        return Case(label=self.label, formulas=Sequent(formulas=tuple(slot_side(s) for s in self.formulas.formulas)))

@dataclass(frozen=True)
class JumboFormula(Formula,Generic[Slot]):
    """A connective with a fully-enumerated case structure and formula annotations."""
    cases: tuple[Case[Slot], ...]

    def erase(self) -> JumboFormula[Side]:
        """Project to the erased version (drop Formula annotations)."""
        erased_cases = tuple(
            case.erase() for case in self.cases
        )
        return JumboFormula[Side](polarity=self.polarity, cases=erased_cases)

    def is_cartesian(self: JumboFormula[SidedFormula]) -> bool:
        return self.polarity == Polarity.POS and \
            all(f.is_cartesian() if side == Side.LEFT else f.is_cocartesian() for case in self.cases for side, f in case.formulas.formulas)

    def is_cocartesian(self: JumboFormula[SidedFormula]) -> bool:
        return self.polarity == Polarity.NEG and \
            all(f.is_cartesian() if side == Side.LEFT else f.is_cocartesian() for case in self.cases for side, f in case.formulas.formulas)

@dataclass(frozen=True)
class Box(Formula):
    """!A — exponential modality."""
    polarity: Polarity
    sub: Formula
    modality: Optional[str] = None  # for distinguishing different flavours of !

    def is_cartesian(self) -> bool:
        return self.polarity == Polarity.POS
    
    def is_cocartesian(self) -> bool:
        return self.polarity == Polarity.NEG

# ══════════════════════════════════════════════════════════════════════════════
# Opaque Types
#
# An OpaqueType is a Formula corresponding to a (possibly infinite / impractical)
# JumboFormula whose case structure exists but is too large (or infinite) to
# enumerate explicitly.  The canonical example is Int32:
# a positive type with 2^32 cases, one per bit-pattern. We store the case structure
# in get_case_type, as a function, but we simply never materialise the full JumboFormula.
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
    """Typed flat type: every case produces no sub-slots.
    
    Positive flat types are cartesian (sums of units); negative flat types are cocartesian (product of bottoms)."""
    label_type: Type[T]

    def get_case_type(self, case_label: T) -> tuple[SidedFormula, ...]:
        return ()

    def is_cartesian(self) -> bool:
        return self.polarity == Polarity.POS
    
    def is_cocartesian(self) -> bool:
        return self.polarity == Polarity.NEG


# ══════════════════════════════════════════════════════════════════════════════
# Typed Rule Schemas
#
# Each rule delegates its structural (side-only) checks to the corresponding
# erased rule, then adds formula-identity checks on top.
# This is the key layering: erased_ir owns the shape logic; typed_ir owns the
# formula-identity refinements.
# ══════════════════════════════════════════════════════════════════════════════

# ── Structural rules ──────────────────────────────────────────────────────────

class CoreRule(RuleSchema):
    pass

# A |- A
@dataclass(frozen=True)
class Identity(CoreRule):
    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert instance.premises == ()
        assert instance.key_slots_conclusion == (0, 1)
        assert instance.conclusion == Sequent((Side.LEFT, Side.RIGHT))

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        f = instance.conclusion.formulas[0][1]
        assert instance.conclusion == Sequent(((Side.LEFT, f), (Side.RIGHT, f)))

# Γ ⊢ A, Δ  and  Θ, A ⊢ Λ  =>  Γ, Θ ⊢ Δ, Λ
@dataclass(frozen=True)
class Cut(CoreRule):
    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 2
        premise0, premise1 = instance.premises
        assert len(instance.key_slots_premises) == 2
        assert len(instance.key_slots_conclusion) == 0
        assert Side.RIGHT == premise0.formulas[instance.key_slots_premises[0][0]]
        assert Side.LEFT  == premise1.formulas[instance.key_slots_premises[1][0]]
        gamma_delta = array_minus_key_slots(premise0.formulas, instance.key_slots_premises[0])
        theta_lam   = array_minus_key_slots(premise1.formulas, instance.key_slots_premises[1])
        assert instance.conclusion.formulas == gamma_delta + theta_lam

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        premise0, premise1 = instance.premises
        a = premise0.formulas[instance.key_slots_premises[0][0]][1]
        assert (Side.RIGHT, a) == premise0.formulas[instance.key_slots_premises[0][0]]
        assert (Side.LEFT,  a) == premise1.formulas[instance.key_slots_premises[1][0]]


# ── Jumbo rules ───────────────────────────────────────────────────────────────

@dataclass(frozen=True)
class Build(CoreRule):
    """Introduce a JumboFormula on its active side.

    case_index picks the disjunct branch; always 0 for single-case connectives.
    Produces exactly 1 premise per sub-slot of the chosen case.
    """
    principal: JumboFormula[SidedFormula]
    case_index: int = 0

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        case = self.principal.cases[self.case_index]

        assert len(instance.key_slots_conclusion) == 1
        side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert side == self.principal.active_side

        assert len(instance.premises) == len(case.formulas.formulas)
        for i, (expected_side, expected_f) in enumerate(case.formulas.formulas):
            premise = instance.premises[i]
            assert len(instance.key_slots_premises[i]) == 1
            premise_side = premise.formulas[instance.key_slots_premises[i][0]]
            assert premise_side == expected_side

        premises_combined: list[Side] = []
        for i, premise in enumerate(instance.premises):
            premises_combined.extend(array_minus_key_slots(premise.formulas, instance.key_slots_premises[i]))
        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conclusion_others == tuple(premises_combined)

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        case = self.principal.cases[self.case_index]

        side, jf = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert (side, jf) == (self.principal.active_side, self.principal)

        for i, (expected_side, expected_f) in enumerate(case.formulas.formulas):
            premise = instance.premises[i]
            premise_side, premise_f = premise.formulas[instance.key_slots_premises[i][0]]
            assert (premise_side, premise_f) == (expected_side, expected_f)


@dataclass(frozen=True)
class Break(CoreRule):
    """Eliminate a JumboFormula from its passive side.

    Produces one premise per case (all branches handled).
    No choice — fully invertible.
    """
    principal: JumboFormula[SidedFormula]

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.key_slots_conclusion) == 1
        side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert side == self.principal.passive_side

        assert len(instance.premises) == len(self.principal.cases)
        for i, case in enumerate(self.principal.cases):
            premise = instance.premises[i]
            assert len(instance.key_slots_premises[i]) == len(case.formulas.formulas)
            for j, (expected_side, _expected_f) in enumerate(case.formulas.formulas):
                premise_side = premise.formulas[instance.key_slots_premises[i][j]]
                assert premise_side == expected_side

        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        for i, premise in enumerate(instance.premises):
            premise_others = array_minus_key_slots(premise.formulas, instance.key_slots_premises[i])
            assert conclusion_others == tuple(premise_others)

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        side, jf = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert (side, jf) == (self.principal.passive_side, self.principal)

        for i, case in enumerate(self.principal.cases):
            premise = instance.premises[i]
            for j, (expected_side, expected_f) in enumerate(case.formulas.formulas):
                premise_side, premise_f = premise.formulas[instance.key_slots_premises[i][j]]
                assert (premise_side, premise_f) == (expected_side, expected_f)


# ── Exponential rules ─────────────────────────────────────────────────────────

@dataclass(frozen=True)
class Promotion(CoreRule):
    polarity: Polarity
    principle_formula_index: int

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        l = len(premise.formulas)
        assert l == len(instance.key_slots_premises[0])
        assert l == len(instance.key_slots_conclusion)
        assert l == len(instance.conclusion.formulas)

        expected_principal_side = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        for i, side in enumerate(premise.formulas):
            conclusion_side = instance.conclusion.formulas[instance.key_slots_conclusion[i]]
            if i == self.principle_formula_index:
                assert conclusion_side == expected_principal_side, (
                    f"Principal formula in bottom of Promotion must be on side "
                    f"{expected_principal_side}, found {conclusion_side}"
                )
                assert conclusion_side == side, (
                    f"Principal formula in top of Promotion must be on the same side "
                    f"as principal in bottom, found {side} vs {conclusion_side}"
                )
            else:
                assert side == conclusion_side

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        premise = instance.premises[0]
        expected_principal_side = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        for i, (side, f) in enumerate(premise.formulas):
            if i == self.principle_formula_index:
                conclusion_side, conclusion_f = instance.conclusion.formulas[instance.key_slots_conclusion[i]]
                assert isinstance(conclusion_f, Box)
                assert conclusion_f.polarity == self.polarity
                assert conclusion_f.sub == f
                assert conclusion_side == expected_principal_side
            else:
                assert isinstance(f, Box)
                assert (f.polarity == Polarity.POS and side == Side.LEFT) or (f.polarity == Polarity.NEG and side == Side.RIGHT)
                assert (side, f) == instance.conclusion.formulas[i]


@dataclass(frozen=True)
class Dereliction(CoreRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 1
        assert len(instance.key_slots_premises[0]) == 1
        assert len(instance.key_slots_conclusion) == 1

        premise_side    = premise.formulas[instance.key_slots_premises[0][0]]
        conclusion_side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conclusion_side == expected, (
            f"Principal formula in bottom of Dereliction must be on side {expected}, found {conclusion_side}"
        )
        assert conclusion_side == premise_side, (
            f"Principal formula in top of Dereliction must match bottom side, "
            f"found {premise_side} vs {conclusion_side}"
        )

        premise_others    = array_minus_key_slots(premise.formulas, instance.key_slots_premises[0])
        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conclusion_others == premise_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        premise = instance.premises[0]
        premise_side,    premise_f    = premise.formulas[instance.key_slots_premises[0][0]]
        conclusion_side, conclusion_f = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert isinstance(conclusion_f, Box)
        assert conclusion_f.polarity == self.polarity
        assert conclusion_f.sub == premise_f
        assert conclusion_side == premise_side


@dataclass(frozen=True)
class Weakening(CoreRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 1
        premise = instance.premises[0]
        assert len(instance.key_slots_premises) == 0
        assert len(instance.key_slots_conclusion) == 1

        conclusion_side = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert conclusion_side == expected, (
            f"Principal formula in bottom of Weakening must be on side {expected}, found {conclusion_side}"
        )

        conclusion_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert conclusion_others == premise.formulas

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        conclusion_side, conclusion_f = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert isinstance(conclusion_f, Box)
        assert conclusion_f.polarity == self.polarity


@dataclass(frozen=True)
class Contraction(CoreRule):
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
            f"Principal formula in bottom of Contraction must be on side {expected}, found {conclusion_side}"
        )

        for i in range(self.count):
            top_side = premise.formulas[instance.key_slots_premises[0][i]]
            assert top_side == expected, (
                f"Principal formula in top of Contraction must be on side {expected}, found {top_side}"
            )

        top_others    = array_minus_key_slots(premise.formulas, instance.key_slots_premises[0])
        bottom_others = array_minus_key_slots(instance.conclusion.formulas, instance.key_slots_conclusion)
        assert bottom_others == top_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        premise = instance.premises[0]
        conclusion_side, conclusion_f = instance.conclusion.formulas[instance.key_slots_conclusion[0]]
        assert isinstance(conclusion_f, Box)
        assert conclusion_f.polarity == self.polarity
        for i in range(self.count):
            premise_side, premise_formula = premise.formulas[instance.key_slots_premises[0][i]]
            assert premise_formula == conclusion_f, (
                f"Principal formula in top of Contraction must equal bottom, "
                f"expected {conclusion_f}, found {premise_formula}"
            )

# ══════════════════════════════════════════════════════════════════════════════
# Admissible Rules
#
# An admissible rule is a RuleSchema derivable from the core rules in
# principle, but whose derivation is NOT spelled out here.  It is justified
# by a prose argument and validated by a custom check_shape and check_type.
#
# Structural invariant
# --------------------
# - CoreRule       : tag for the rules above.  Never subclassed outside of here.
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

@dataclass(frozen=True)
class FlatOperation(AdmissibleRule):
    name: str
    
    # these are part of the rule name and not erased
    args: tuple[FlatType, ...]
    result: FlatType

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.premises) == 0, "FlatOperation has no premises"
        assert instance.conclusion.count_antecedents  == len(self.args), (
            f"Expected {len(self.args)} inputs, got {instance.conclusion.count_antecedents}"
        )
        assert instance.conclusion.count_succedents == 1, (
            f"Expected 1 output, got {instance.conclusion.count_succedents}"
        )

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        assert len(instance.premises) == 0, "FlatOperation has no premises"
        lefts  = instance.conclusion.antecedents
        rights = instance.conclusion.succedents
        assert len(lefts)  == len(self.args), (
            f"Expected {len(self.args)} inputs, got {len(lefts)}"
        )
        assert len(rights) == 1, f"Expected 1 output, got {len(rights)}"
        for i, arg in enumerate(self.args):
            _assert_opaque(lefts[i][1], arg, f"input {i}")
        _assert_opaque(rights[0][1], self.result, "output")

    def __repr__(self) -> str:
        return f"<FlatOp {self.name}: {self.args} -> {self.result}>"

