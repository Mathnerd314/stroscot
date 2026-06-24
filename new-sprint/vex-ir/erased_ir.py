from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any
from typed_ir import (
    Polarity, Side
)

@dataclass(frozen=True)
class Sequent:
    formulas: tuple[Side, ...]

    def on(self, side: Side) -> int:
        return len([s for s in self.formulas if s == side])

    @property
    def left(self)  -> int: return self.on(Side.LEFT)

    @property
    def right(self) -> int: return self.on(Side.RIGHT)

    def __str__(self) -> str:
        return f"{self.left} ⊢ {self.right}"

@dataclass(frozen=True)
class Case:
    label: Any
    formulas: tuple[Side ...]

@dataclass(frozen=True)
class ErasedJumboFormula():
    polarity: Polarity
    cases:    tuple[Case, ...]

    @property
    def active_side(self) -> Side:
        return Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT

    @property
    def passive_side(self) -> Side:
        return self.active_side.flip()

# ══════════════════════════════════════════════════════════════════════════════
# Inference Rule Schemas
# An inference rule schema is the information in parentheses in the derivation tree, i.e. the "reason" for the inference step.
# ══════════════════════════════════════════════════════════════════════════════

def array_minus_key_slots(array: tuple[Side, ...], key_slots: tuple[int, ...]) -> tuple[Side, ...]:
    """Return the formulas in array that are NOT in the key slots."""
    return tuple(array[i] for i in range(len(array)) if i not in key_slots)

class RuleSchema:
    def check_shape(self, instance: InstantiatedRule) -> None:
        """Check that the given instantiated rule has the correct shape for this schema."""
        raise NotImplementedError

class CoreRule(RuleSchema):
    pass

## Structural rules

# A |- A
@dataclass(frozen=True)
class Identity(CoreRule):
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert instance.tops == ()
        assert instance.key_slots_bottom == (0, 1)  # the two key slots are the only two formulas in the bottom sequent
        assert instance.bottom == Sequent((Side.LEFT, Side.RIGHT))

# Γ ⊢ A, Δ and Θ, A ⊢ Λ  =>  Γ, Θ ⊢ Δ, Λ
@dataclass(frozen=True)
class Cut(CoreRule):
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 2
        top0, top1 = instance.tops
        assert len(instance.key_slots_tops) == 2
        assert len(instance.key_slots_bottom) == 0
        assert Side.RIGHT == top0.formulas[instance.key_slots_tops[0][0]]
        assert Side.LEFT == top1.formulas[instance.key_slots_tops[1][0]]
        # context union check - check that the contexts are exactly the same minus the cut formula
        gamma_delta = array_minus_key_slots(top0.formulas, instance.key_slots_tops[0])
        theta_lam = array_minus_key_slots(top1.formulas, instance.key_slots_tops[1])
        assert instance.bottom.formulas == gamma_delta + theta_lam

# exchange rule is managed in Derivation (permutations of the formulas in the sequent), so no explicit rule schema is needed

## Jumbo rules
@dataclass(frozen=True)
class Build(CoreRule):
    """Introduce a JumboFormula on its active side.
    case_index picks the disjunct branch; always 0 for single-case connectives.
    Produces exactly 1 premise."""
    principal: ErasedJumboFormula
    case_index: int = 0
    
    def check_shape(self, instance: InstantiatedRule) -> None:
        case = self.principal.cases[self.case_index]
        # check bottom - one jumbo formula on the active side
        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == self.principal.active_side
        
        # check top - one top per each subformula of the case, with the correct sides
        assert len(instance.tops) == len(case.formulas)
        for i, side in enumerate(case.formulas):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == 1
            t_side = top.formulas[instance.key_slots_tops[i][0]]
            assert side == t_side
            
        # check context preservation - all other formulas in the bottom must be present in each top
        top_combined = []
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            top_combined.extend(top_others)

        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == tuple(top_combined)

@dataclass(frozen=True)
class Break(CoreRule):
    """Eliminate a JumboFormula from its passive side.
    Produces one premise per case (all branches handled).
    No choice — fully invertible."""
    principal: ErasedJumboFormula

    def check_shape(self, instance: InstantiatedRule) -> None:
        # check bottom - one jumbo formula on the passive side
        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == self.principal.passive_side

        # check top - one top per case, with the correct sides
        assert len(instance.tops) == len(self.principal.cases)
        for i, case in enumerate(self.principal.cases):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == case.formulas.__len__()  # all subformulas of the case are principal in the top
            for j, side in enumerate(case.formulas):
                t_side = top.formulas[instance.key_slots_tops[i][j]]
                assert t_side == side

        # check context preservation - all other formulas in the bottom must be present in each top
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            assert bottom_others == tuple(top_others)

## Exponential rules

@dataclass(frozen=True)
class Promotion(CoreRule):
    polarity: Polarity
    principle_formula_index: int  # index of the principal formula in the top/bottom sequent key slots (which must be all slots)
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        l = len(top.formulas)
        # all slots must be marked as key slots and match 1-1 between top and bottom
        assert l == len(instance.key_slots_tops[0])
        assert l == len(instance.key_slots_bottom)
        assert l == len(instance.bottom.formulas)
        
        # all formulas must be bang-wrapped and with the correct polarity, except for the principal formula
        num_principal_formulas = 0
        for i, side in enumerate(top.formulas):
            if i == self.principle_formula_index:
                # principal formula must be the subformula of the principal formula in the bottom, with the correct side
                bottom_side = instance.bottom.formulas[instance.key_slots_bottom[i]]
                expected_bottom_side = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
                assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Promotion must be on side {expected_bottom_side}, found {bottom_side}"
                assert bottom_side == side, f"Principal formula in top of Promotion must be on the same side as the principal formula in bottom, found {side} vs {bottom_side}"
            else:
                # passthrough bang formula
                assert side == instance.bottom.formulas[i]

@dataclass(frozen=True)
class Dereliction(CoreRule):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 1
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the top must be the subformula of the principal formula in the bottom, with the correct side
        top_side = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Dereliction must be on side {expected_bottom_side}, found {bottom_side}"
        assert bottom_side == top_side, f"Principal formula in top of Dereliction must be on the same side as the principal formula in bottom, found {top_side} vs {bottom_side}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

@dataclass(frozen=True)
class Weakening(CoreRule):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 0
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the bottom must have correct shape and side
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Weakening must be on side {expected_bottom_side}, found {bottom_side}"
        
        # context preservation - must be the only new formula compared to the top
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top.formulas

@dataclass(frozen=True)
class Contraction(CoreRule):
    polarity: Polarity
    count: int = 2  # number of copies to contract into (default 2 for binary contraction)
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == self.count
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the bottom must be the "contracted" version of the principal formula in the top, with the correct side
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Contraction must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the each instance of the top formula matches the bottom formula
        for i in range(self.count):
            top_side = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected_bottom_side, f"Principal formula in top of Contraction must be on side {expected_bottom_side}, found {top_side}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

# ══════════════════════════════════════════════════════════════════════════════
# Derivation Tree
# ══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class InstantiatedRule:
    """A rule schema applied to concrete sequent contexts.

    tops    — the premise sequents (in order); length = rule.expected_premises()
    bottom  — the conclusion sequent
    rule    — the rule that generated this instance (for display / analysis)

    Invariant: the shape of tops/bottom must be consistent with rule —
    enforced by the factory methods below, not the constructor.
    """
    rule:   RuleSchema
    tops:   tuple[Sequent, ...]
    key_slots_tops: tuple[tuple[int, ...], ...] # marks the principal formula(s) in the top sequents
    bottom: Sequent
    key_slots_bottom: tuple[int, ...] # marks the principal formula(s) in the bottom sequent
    
    def validate(self) -> None:
        self.rule.check_shape(self)

def apply_perm(perm: tuple[int, ...], seq: Sequent) -> Sequent:
    """perm[j] = k means: output position j comes from input position k."""
    if len(perm) != len(seq.formulas):
        raise ValueError(f"Permutation length {len(perm)} != sequent length {len(seq.formulas)}")
    if sorted(perm) != list(range(len(perm))):
        raise ValueError(f"Not a valid permutation: {perm}")
    return Sequent(tuple(seq.formulas[k] for k in perm))

@dataclass
class Derivation:
    """A node in the proof tree.

    instantiated_rule  — fully concrete rule application at this node
    premises           — sub-derivations, one per top in instantiated_rule.tops
    perm_tops          — optional permutation of premises to match the order of tops in instantiated_rule.tops 

    Matching invariant (checked in __post_init__):
      premises[i].instantiated_rule.bottom == self.instantiated_rule.tops[i]
    """
    instantiated_rule: InstantiatedRule
    premises:          list[Derivation] = field(default_factory=list)
    perm_tops:         tuple[tuple[int, ...], ...]   = field(default=()) # we fill this in __post_init__ if not provided

    def __post_init__(self) -> None:
        self.instantiated_rule.validate()
        tops = self.instantiated_rule.tops
        if len(self.premises) != len(tops):
            raise ValueError(
                f"Expected {len(tops)} premises, got {len(self.premises)}"
            )
        if self.perm_tops == (): # default: identity
            self.perm_tops = tuple(tuple(range(len(t.formulas))) for t in tops)
        if len(self.perm_tops) != len(tops):
            raise ValueError(
                f"Expected {len(tops)} permutations, got {len(self.perm_tops)}"
            )
        for i, (prem, top, perm) in enumerate(zip(self.premises, tops, self.perm_tops)):
            expected = apply_perm(perm, top)
            if prem.instantiated_rule.bottom != expected:
                raise ValueError(
                    f"Premise {i} conclusion {prem.instantiated_rule.bottom} "
                    f"does not match required top {expected} "
                    f"which is {top} after applying permutation {perm}"
                )

    @property
    def conclusion(self) -> Sequent:
        return self.instantiated_rule.bottom

    @property
    def is_leaf(self) -> bool:
        return not self.premises

    def depth(self) -> int:
        return 0 if self.is_leaf else 1 + max(p.depth() for p in self.premises)

    def size(self) -> int:
        return 1 + sum(p.size() for p in self.premises)

    def pretty(self, indent: int = 0) -> str:
        pad   = "  " * indent
        lines = [p.pretty(indent + 1) for p in self.premises]
        perms = ""
        if self.perm_tops and any(
            list(p) != list(range(len(p))) for p in self.perm_tops
        ):
            perms = f" perm={self.perm_tops}"
        lines.append(f"{pad}[{self.instantiated_rule.rule}]{perms}  {self.conclusion}")
        return "\n".join(lines)

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

@dataclass(frozen=True)
class FlatOperation(AdmissibleRule):
    name: str
    args: int

    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 0, "Arithmetic rules have no premises"
        lefts  = instance.bottom.left
        rights = instance.bottom.right
        assert lefts  == self.args, f"Expected {self.args} inputs, got {lefts}"
        assert rights == 1, f"Expected 1 output, got {rights}"

    def __repr__(self) -> str:
        return f"<FlatOperation {self.name} {self.args}>"

AddI32 = FlatOperation(name="add_i32", args=2)
SubI32 = FlatOperation(name="sub_i32", args=2)
MulI32 = FlatOperation(name="mul_i32", args=2)
DivI32 = FlatOperation(name="div_i32", args=2) # partial — undefined behaviour on division by zero (caller's responsibility)
RemI32 = FlatOperation(name="rem_i32", args=2) # partial — undefined behaviour on division by zero (caller's responsibility)
NegI32 = FlatOperation(name="neg_i32", args=1)
AndI32 = FlatOperation(name="and_i32", args=2)
OrI32  = FlatOperation(name="or_i32",  args=2)
XorI32 = FlatOperation(name="xor_i32", args=2)
NotI32 = FlatOperation(name="not_i32", args=1)
ShlI32 = FlatOperation(name="shl_i32", args=2)
ShrI32 = FlatOperation(name="shr_i32", args=2) # arithmetic right shift (sign-extending)
EqI32  = FlatOperation(name="eq_i32",  args=2)
LtI32  = FlatOperation(name="lt_i32",  args=2)
LeI32  = FlatOperation(name="le_i32",  args=2)

# ── Constant introduction ─────────────────────────────────────────────────────

@dataclass(frozen=True)
class OpaqueType():
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
    
    def get_case_formula(self, case_label: Any) -> tuple[Side, ...]:
        """Return the formula corresponding to a given case label.

        For an OpaqueType, this is a theoretical operation: we cannot enumerate
        all cases, but we can still refer to a specific case by label.
        """
        raise NotImplementedError

    def __str__(self) -> str:
        pol = "+" if self.polarity == Polarity.POS else "-"
        return f"{self.name}^{pol}"


@dataclass(frozen=True)
class Const(AdmissibleRule):
    """Introduce a literal value (case) of an OpaqueType into the sequent. Equviakent to Build on the corresponding case of the theoretical JumboFormula."""
    principal: OpaqueType
    case_label: Any
    def check_shape(self, instance: InstantiatedRule) -> None:
        case_formulas = self.principal.get_case_formula(self.case_label)
        
        # check bottom - one jumbo formula on the active side
        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == self.principal.active_side
        
        # check top - one top per each subformula of the case, with the correct sides
        assert len(instance.tops) == len(case_formulas)
        for i, side in enumerate(case_formulas):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == 1
            t_side = top.formulas[instance.key_slots_tops[i][0]]
            assert side == t_side
            
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
    def check_shape(self, instance: InstantiatedRule) -> None:
        # check bottom - Bool on the passive side (left)
        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == Side.LEFT

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
    principle_formula_index: int  # index of the principal formula in the top/bottom sequent key slots (which must be all slots)
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
        for i, side in enumerate(top.formulas):
            if i == self.principle_formula_index:
                # principal formula must be on its active side
                bottom_side = instance.bottom.formulas[instance.key_slots_bottom[i]]
                expected_bottom_side = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
                assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Promotion must be on side {expected_bottom_side}, found {bottom_side}"
                assert bottom_side == side, f"Principal formula in top of Promotion must be on the same side as the principal formula in bottom, found {side} vs {bottom_side}"
            else:
                # passthrough bang formula must be on its passive side
                bottom_side = instance.bottom.formulas[instance.key_slots_bottom[i]]
                expected_bottom_side = (Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT).flip()
                assert bottom_side == expected_bottom_side, f"Passthrough formula in bottom of Promotion must be on side {expected_bottom_side}, found {bottom_side}"
                assert bottom_side == side, f"Passthrough formula in top of Promotion must be on the same side as the passthrough formula in bottom, found {side} vs {bottom_side}"


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
        top_side = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
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
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Absorption must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the top is one dereliction and one exact match
        top_side = top.formulas[instance.key_slots_tops[0][0]]
        assert top_side == expected_bottom_side, f"Principal formula in top of Absorption must be on side {expected_bottom_side}, found {top_side}"

        top_side = top.formulas[instance.key_slots_tops[0][1]]
        assert top_side == expected_bottom_side, f"Principal formula in top of Absorption must be on side {expected_bottom_side}, found {top_side}"


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
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Multiplexing must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the each instance of the top formula matches the bottom formula
        for i in range(self.count):
            top_side = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected_bottom_side, f"Principal formula in top of Multiplexing must be on side {expected_bottom_side}, found {top_side}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others


