from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional, Union

# ══════════════════════════════════════════════════════════════════════════════
# Polarity & Side
# ══════════════════════════════════════════════════════════════════════════════

class Polarity(Enum):
    POS = "+"
    NEG = "-"

    def flip(self) -> Polarity:
        return Polarity.NEG if self == Polarity.POS else Polarity.POS


class Side(Enum):
    LEFT  = "L"
    RIGHT = "R"

    def flip(self) -> Side:
        return Side.RIGHT if self == Side.LEFT else Side.LEFT


# ══════════════════════════════════════════════════════════════════════════════
# Formulas
# ══════════════════════════════════════════════════════════════════════════════


class Formula():
    pass

@dataclass(frozen=True)
class Atom(Formula):
    name: str

@dataclass(frozen=True)
class Case:
    label: str
    formulas: tuple[SidedFormula, ...]

@dataclass(frozen=True)
class JumboFormula(Formula):
    polarity: Polarity
    cases:    tuple[Case, ...]

    @property
    def active_side(self) -> Side:
        return Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT

    @property
    def passive_side(self) -> Side:
        return self.active_side.flip()


@dataclass(frozen=True)
class Bang(Formula):
    """!A  — exponential modality."""
    polarity: Polarity
    sub: Formula
    modality: Optional[str] = None # for distinguishing different "flavors" of !, if needed

# ══════════════════════════════════════════════════════════════════════════════
# Sequent  Γ ⊢ Δ  as a flat list of (Side, Formula)
# ══════════════════════════════════════════════════════════════════════════════

SidedFormula = tuple[Side, Formula]

@dataclass(frozen=True)
class Sequent:
    formulas: tuple[SidedFormula, ...]

    def on(self, side: Side) -> tuple[Formula, ...]:
        return tuple(f for s, f in self.formulas if s == side)

    @property
    def left(self)  -> tuple[Formula, ...]: return self.on(Side.LEFT)

    @property
    def right(self) -> tuple[Formula, ...]: return self.on(Side.RIGHT)

    def with_formula(self, sf: SidedFormula) -> Sequent:
        return Sequent(self.formulas + (sf,))

    def without_formula(self, sf: SidedFormula) -> Sequent:
        fs = list(self.formulas)
        fs.remove(sf)
        return Sequent(tuple(fs))

    def replace(self, old: SidedFormula, new: tuple[SidedFormula, ...]) -> Sequent:
        fs = list(self.formulas)
        i  = fs.index(old)
        fs[i:i+1] = list(new)
        return Sequent(tuple(fs))

    def __str__(self) -> str:
        left_str  = ", ".join(str(f) for s, f in self.formulas if s == Side.LEFT)
        right_str = ", ".join(str(f) for s, f in self.formulas if s == Side.RIGHT)
        return f"{left_str} ⊢ {right_str}"

# ══════════════════════════════════════════════════════════════════════════════
# Inference Rule Schemas
# An inference rule schema is the information in parentheses in the derivation tree, i.e. the "reason" for the inference step.
# ══════════════════════════════════════════════════════════════════════════════

def array_minus_key_slots(array: tuple[SidedFormula, ...], key_slots: tuple[int, ...]) -> tuple[SidedFormula, ...]:
    """Return the formulas in array that are NOT in the key slots."""
    return tuple(array[i] for i in range(len(array)) if i not in key_slots)

class RuleSchema:
    def check_shape(self, instance: InstantiatedRule) -> None:
        """Check that the given instantiated rule has the correct shape for this schema."""
        raise NotImplementedError

## Structural rules

# A |- A
@dataclass(frozen=True)
class Identity(RuleSchema):
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert instance.tops == ()
        assert instance.key_slots_bottom == (0, 1)  # the two key slots are the only two formulas in the bottom sequent
        f = instance.bottom.left[0]  # the single formula on the left
        assert instance.bottom == Sequent(((Side.LEFT, f), (Side.RIGHT, f)))

# Γ ⊢ A, Δ and Θ, A ⊢ Λ  =>  Γ, Θ ⊢ Δ, Λ
@dataclass(frozen=True)
class Cut(RuleSchema):
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 2
        top0, top1 = instance.tops
        assert len(instance.key_slots_tops) == 2
        assert len(instance.key_slots_bottom) == 0
        a = instance.tops[0].formulas[instance.key_slots_tops[0][0]][1]  # the cut formula
        assert (Side.RIGHT, a) == top0.formulas[instance.key_slots_tops[0][0]]
        assert (Side.LEFT,  a) == top1.formulas[instance.key_slots_tops[1][0]]
        # context union check - check that the contexts are exactly the same minus the cut formula
        gamma_delta = array_minus_key_slots(top0.formulas, instance.key_slots_tops[0])
        theta_lam = array_minus_key_slots(top1.formulas, instance.key_slots_tops[1])
        assert instance.bottom.formulas == gamma_delta + theta_lam

# exchange rule is managed in Derivation (permutations of the formulas in the sequent), so no explicit rule schema is needed

## Jumbo rules
@dataclass(frozen=True)
class Build(RuleSchema):
    """Introduce a JumboFormula on its active side.
    case_index picks the disjunct branch; always 0 for single-case connectives.
    Produces exactly 1 premise."""
    principal: JumboFormula
    case_index: int = 0
    
    def check_shape(self, instance: InstantiatedRule) -> None:
        case = self.principal.cases[self.case_index]
        
        # check bottom - one jumbo formula on the active side
        assert len(instance.key_slots_bottom) == 1
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (self.principal.active_side, self.principal)
        
        # check top - one top per each subformula of the case, with the correct sides
        assert len(instance.tops) == len(case.formulas)
        for i, (side, f) in enumerate(case.formulas):
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

@dataclass(frozen=True)
class Break(RuleSchema):
    """Eliminate a JumboFormula from its passive side.
    Produces one premise per case (all branches handled).
    No choice — fully invertible."""
    principal: JumboFormula

    def check_shape(self, instance: InstantiatedRule) -> None:
        # check bottom - one jumbo formula on the passive side
        assert len(instance.key_slots_bottom) == 1
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (self.principal.passive_side, self.principal)

        # check top - one top per case, with the correct sides
        assert len(instance.tops) == len(self.principal.cases)
        for i, case in enumerate(self.principal.cases):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == case.formulas.__len__()  # all subformulas of the case are principal in the top
            for j, (side, f) in enumerate(case.formulas):
                t_side, t_f = top.formulas[instance.key_slots_tops[i][j]]
                assert (t_side, t_f) == (side, f)

        # check context preservation - all other formulas in the bottom must be present in each top
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            assert bottom_others == tuple(top_others)

## Exponential rules

@dataclass(frozen=True)
class Promotion(RuleSchema):
    polarity: Polarity
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
        for i, (side, f) in enumerate(top.formulas):
            if isinstance(f, Bang) and (f.polarity == Polarity.POS and side == Side.LEFT or
                                        f.polarity == Polarity.NEG and side == Side.RIGHT) and (side, f) == instance.bottom.formulas[i]:
                # passthrough bang formula
                continue
            elif isinstance(instance.bottom.formulas[0], Bang) and instance.bottom.formulas[0].sub == f:
                # principal formula
                assert instance.bottom.formulas[0].polarity == self.polarity
                assert side == (Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT)
                num_principal_formulas += 1
            else:
                raise AssertionError(f"Formula {f} at slot {i} in top does not match expected shape for Promotion")
        
        assert num_principal_formulas == 1, f"Expected exactly one principal formula in top for Promotion, found {num_principal_formulas}"
        

@dataclass(frozen=True)
class Dereliction(RuleSchema):
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
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        assert bottom_f.sub == top_f
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Dereliction must be on side {expected_bottom_side}, found {bottom_side}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

@dataclass(frozen=True)
class Weakening(RuleSchema):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 0
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the bottom must have correct shape and side
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Weakening must be on side {expected_bottom_side}, found {bottom_side}"
        
        # context preservation - must be the only new formula compared to the top
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top.formulas

@dataclass(frozen=True)
class Contraction(RuleSchema):
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
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Contraction must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the each instance of the top formula matches the bottom formula
        for i in range(self.count):
            top_side, top_f = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected_bottom_side, f"Principal formula in top of Contraction must be on side {expected_bottom_side}, found {top_side}"
            assert top_f == bottom_f, f"Principal formula in top of Contraction must be the same as the principal formula in bottom, expected {bottom_f}, found {top_f}"
        
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
