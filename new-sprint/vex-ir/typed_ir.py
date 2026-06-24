from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Generic, TypeVar, Optional

# ══════════════════════════════════════════════════════════════════════════════
# Polarity & Side
# ══════════════════════════════════════════════════════════════════════════════

class Polarity(Enum):
    POS = "+"
    NEG = "-"

    def flip(self) -> Polarity:
        return Polarity.NEG if self == Polarity.POS else Polarity.POS

    def __str__(self) -> str:
        return self.value

class Side(Enum):
    LEFT = "L"
    RIGHT = "R"

    def flip(self) -> Side:
        return Side.RIGHT if self == Side.LEFT else Side.LEFT
    
    def __str__(self) -> str:
        return self.value

# ══════════════════════════════════════════════════════════════════════════════
# Slot is the type of a single entry in a sequent's formula list.
#   Erased IR:  Slot = Side
#   Typed IR:   Slot = tuple[Side, Formula]
#
# All structural logic (index arithmetic, context preservation, permutations)
# lives here once.  Rule schemas that need to inspect formula identity live in
# their respective IR modules.
# ══════════════════════════════════════════════════════════════════════════════

Slot = TypeVar("Slot")

def slot_side(slot: Any) -> Side:
    """Return the Side of a single sequent slot."""
    if isinstance(slot, Side):
        return slot
    elif isinstance(slot, tuple) and len(slot) == 2 and isinstance(slot[0], Side):
        return slot[0]
    else:
        raise TypeError(f"Cannot determine side of slot {slot}")

def array_minus_key_slots(
    array: tuple[Any, ...], key_slots: tuple[int, ...]
) -> tuple[Any, ...]:
    """Return elements of *array* whose indices are NOT in *key_slots*."""
    ks = set(key_slots)
    return tuple(array[i] for i in range(len(array)) if i not in ks)

# ══════════════════════════════════════════════════════════════════════════════
# Formulas
# ══════════════════════════════════════════════════════════════════════════════

@dataclass(frozen=True)
class Formula:
    polarity: Polarity

    @property
    def active_side(self) -> Side:
        return Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT

    @property
    def passive_side(self) -> Side:
        return self.active_side.flip()

SidedFormula = tuple[Side, Formula]

@dataclass(frozen=True)
class Atom(Formula):
    name: str


@dataclass(frozen=True)
class Case(Generic[Slot]):
    """One case of a JumboFormula, with formula-annotated sub-slots."""
    label: Any
    formulas: tuple[Slot, ...]
    
    def erase(self) -> Case[Side]:
        """Project to the erased version (drop Formula annotations)."""
        return Case(label=self.label, formulas=tuple(slot_side(s) for s in self.formulas)) # type: ignore

@dataclass(frozen=True)
class JumboFormula(Formula,Generic[Slot]):
    """A connective with a fully-enumerated case structure and formula annotations."""
    cases: tuple[Case[Slot], ...]

    def erase(self) -> JumboFormula[Side]:
        """Project to the erased version (drop Formula annotations)."""
        erased_cases = tuple(
            Case(label=c.label, formulas=tuple(slot_side(s) for s in c.formulas)) # type: ignore
            for c in self.cases
        )
        return JumboFormula[Side](polarity=self.polarity, cases=erased_cases)

@dataclass(frozen=True)
class Bang(Formula):
    """!A — exponential modality."""
    polarity: Polarity
    sub: Formula
    modality: Optional[str] = None  # for distinguishing different flavours of !


# ══════════════════════════════════════════════════════════════════════════════=
# Sequent  Γ ⊢ Δ  as a flat list of slots
# ══════════════════════════════=════════════════════════════════════════════════

@dataclass(frozen=True)
class Sequent(Generic[Slot]):
    formulas: tuple[Slot, ...]
    
    def on(self, side: Side) -> tuple[Slot, ...]:
        return tuple(s for s in self.formulas if slot_side(s) == side)

    @property
    def left(self) -> tuple[Slot, ...]:
        return self.on(Side.LEFT)

    @property
    def right(self) -> tuple[Slot, ...]:
        return self.on(Side.RIGHT)

    @property
    def count_left(self) -> int:
        return sum(1 for s in self.formulas if slot_side(s) == Side.LEFT)

    @property
    def count_right(self) -> int:
        return sum(1 for s in self.formulas if slot_side(s) == Side.RIGHT)

    def __str__(self) -> str:
        if not self.formulas:
            return "⊢"
        elif isinstance(self.formulas[0], Side):
            return f"{self.left} ⊢ {self.right}"
        elif isinstance(self.formulas[0], tuple) and len(self.formulas[0]) == 2:
            left_str  = ", ".join(str(f) for s, f in self.formulas if s == Side.LEFT) # type: ignore
            right_str = ", ".join(str(f) for s, f in self.formulas if s == Side.RIGHT) # type: ignore
            return f"{left_str} ⊢ {right_str}"
        else:
            return f"Sequent({self.formulas})"
        
    def erase(self) -> Sequent[Side]:
        """Project to the erased version (drop Formula annotations)."""
        return Sequent(tuple(slot_side(s) for s in self.formulas))

# Sequent whose slots are plain Side values (no formula information).
ErasedSequent = Sequent[Side]
# Sequent whose slots are (Side, Formula) pairs (full formula information).
TypedSequent = Sequent[SidedFormula]

def apply_perm(perm: tuple[int, ...], seq: Sequent) -> Sequent:
    """perm[j] = k means: output position j comes from input position k."""
    if len(perm) != len(seq.formulas):
        raise ValueError(f"Permutation length {len(perm)} != sequent length {len(seq.formulas)}")
    if sorted(perm) != list(range(len(perm))):
        raise ValueError(f"Not a valid permutation: {perm}")
    return Sequent(tuple(seq.formulas[k] for k in perm))

class RuleSchema:
    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        """Check that the given instantiated rule has the correct shape for this schema."""
        raise NotImplementedError

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        """Check that the given instantiated rule has the correct type for this schema."""
        raise NotImplementedError
    
    def check_shape_and_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        """Check that the given instantiated rule has the correct shape and type for this schema."""
        self.check_shape(instance.erase())
        self.check_type(instance)

@dataclass(frozen=True)
class InstantiatedRule(Generic[Slot]):
    """A rule schema applied to concrete sequent contexts.

    tops              — the premise sequents (in order)
    bottom            — the conclusion sequent
    rule              — the rule that generated this instance
    key_slots_tops    — marks the principal formula(s) in each top sequent
    key_slots_bottom  — marks the principal formula(s) in the bottom sequent

    Invariant: shape of tops/bottom must be consistent with rule —
    enforced by rule.check_shape, not the constructor.
    """
    rule: RuleSchema
    tops: tuple[Sequent[Slot], ...]
    key_slots_tops: tuple[tuple[int, ...], ...]
    bottom: Sequent[Slot]
    key_slots_bottom: tuple[int, ...]

    def validate(self) -> None:
        if isinstance(self.bottom.formulas[0], tuple) and len(self.bottom.formulas[0]) == 2:
            # Typed IR
            self.rule.check_shape_and_type(self)  # type: ignore
        else:
            # Erased IR
            self.rule.check_shape(self) # type: ignore

    def erase(self: InstantiatedRule[Any]) -> InstantiatedRule[Side]:
        """Return a copy of *ir* with all TypedSequents replaced by ErasedSequents."""
        return InstantiatedRule(
            rule=self.rule,
            tops=tuple(t.erase() for t in self.tops),
            key_slots_tops=self.key_slots_tops,
            bottom=self.bottom.erase(),
            key_slots_bottom=self.key_slots_bottom,
        )

@dataclass
class Derivation(Generic[Slot]):
    """A node in the proof tree.

    instantiated_rule — fully concrete rule application at this node
    premises          — sub-derivations, one per top in instantiated_rule.tops
    perm_tops         — permutation applied to each top before matching premises
                        (identity by default; filled in __post_init__)

    Matching invariant (checked in __post_init__):
        apply_perm(perm_tops[i], instantiated_rule.tops[i])
            == premises[i].instantiated_rule.bottom
    """
    instantiated_rule: InstantiatedRule[Slot]
    premises: list[Derivation[Slot]] = field(default_factory=list)
    perm_tops: tuple[tuple[int, ...], ...] = field(default=())

    def __post_init__(self) -> None:
        self.instantiated_rule.validate()
        tops = self.instantiated_rule.tops
        if len(self.premises) != len(tops):
            raise ValueError(f"Expected {len(tops)} premises, got {len(self.premises)}")
        if self.perm_tops == ():
            self.perm_tops = tuple(tuple(range(len(t.formulas))) for t in tops)
        if len(self.perm_tops) != len(tops):
            raise ValueError(f"Expected {len(tops)} permutations, got {len(self.perm_tops)}")
        for i, (prem, top, perm) in enumerate(zip(self.premises, tops, self.perm_tops)):
            expected = apply_perm(perm, top)
            if prem.instantiated_rule.bottom != expected:
                raise ValueError(
                    f"Premise {i} conclusion {prem.instantiated_rule.bottom} "
                    f"does not match required top {expected} "
                    f"(which is {top} after applying permutation {perm})"
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
        pad = "  " * indent
        lines = [p.pretty(indent + 1) for p in self.premises]
        perms = ""
        if self.perm_tops and any(list(p) != list(range(len(p))) for p in self.perm_tops):
            perms = f" perm={self.perm_tops}"
        lines.append(f"{pad}[{self.instantiated_rule.rule}]{perms} {self.conclusion}")
        return "\n".join(lines)

    def erase(self: Derivation[Any]) -> Derivation[Side]:
        """Recursively erase all TypedSequents in a Derivation tree."""
        return Derivation(
            instantiated_rule=self.instantiated_rule.erase(),
            premises=[p.erase() for p in self.premises],
            perm_tops=self.perm_tops,
        )


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
        assert instance.tops == ()
        assert instance.key_slots_bottom == (0, 1)
        assert instance.bottom == Sequent((Side.LEFT, Side.RIGHT))

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        f = instance.bottom.formulas[0][1]
        assert instance.bottom == Sequent(((Side.LEFT, f), (Side.RIGHT, f)))

# Γ ⊢ A, Δ  and  Θ, A ⊢ Λ  =>  Γ, Θ ⊢ Δ, Λ
@dataclass(frozen=True)
class Cut(CoreRule):
    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 2
        top0, top1 = instance.tops
        assert len(instance.key_slots_tops) == 2
        assert len(instance.key_slots_bottom) == 0
        assert Side.RIGHT == top0.formulas[instance.key_slots_tops[0][0]]
        assert Side.LEFT  == top1.formulas[instance.key_slots_tops[1][0]]
        gamma_delta = array_minus_key_slots(top0.formulas, instance.key_slots_tops[0])
        theta_lam   = array_minus_key_slots(top1.formulas, instance.key_slots_tops[1])
        assert instance.bottom.formulas == gamma_delta + theta_lam

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        top0, top1 = instance.tops
        a = top0.formulas[instance.key_slots_tops[0][0]][1]
        assert (Side.RIGHT, a) == top0.formulas[instance.key_slots_tops[0][0]]
        assert (Side.LEFT,  a) == top1.formulas[instance.key_slots_tops[1][0]]


# ── Jumbo rules ───────────────────────────────────────────────────────────────

@dataclass(frozen=True)
class Build(CoreRule):
    """Introduce a JumboFormula on its active side.

    case_index picks the disjunct branch; always 0 for single-case connectives.
    Produces exactly 1 premise per sub-slot of the chosen case.
    """
    principal: JumboFormula
    case_index: int = 0

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        case = self.principal.cases[self.case_index]

        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == self.principal.active_side

        assert len(instance.tops) == len(case.formulas)
        for i, (expected_side, expected_f) in enumerate(case.formulas):
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
        case = self.principal.cases[self.case_index]

        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (self.principal.active_side, self.principal)

        for i, (expected_side, expected_f) in enumerate(case.formulas):
            top = instance.tops[i]
            t_side, t_f = top.formulas[instance.key_slots_tops[i][0]]
            assert (t_side, t_f) == (expected_side, expected_f)


@dataclass(frozen=True)
class Break(CoreRule):
    """Eliminate a JumboFormula from its passive side.

    Produces one premise per case (all branches handled).
    No choice — fully invertible.
    """
    principal: JumboFormula

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.key_slots_bottom) == 1
        side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert side == self.principal.passive_side

        assert len(instance.tops) == len(self.principal.cases)
        for i, case in enumerate(self.principal.cases):
            top = instance.tops[i]
            assert len(instance.key_slots_tops[i]) == len(case.formulas)
            for j, (expected_side, expected_f) in enumerate(case.formulas):
                t_side = top.formulas[instance.key_slots_tops[i][j]]
                assert t_side == expected_side

        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        for i, top in enumerate(instance.tops):
            top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[i])
            assert bottom_others == tuple(top_others)

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        side, jf = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert (side, jf) == (self.principal.passive_side, self.principal)

        for i, case in enumerate(self.principal.cases):
            top = instance.tops[i]
            for j, (expected_side, expected_f) in enumerate(case.formulas):
                t_side, t_f = top.formulas[instance.key_slots_tops[i][j]]
                assert (t_side, t_f) == (expected_side, expected_f)


# ── Exponential rules ─────────────────────────────────────────────────────────

@dataclass(frozen=True)
class Promotion(CoreRule):
    polarity: Polarity
    principle_formula_index: int

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        l = len(top.formulas)
        assert l == len(instance.key_slots_tops[0])
        assert l == len(instance.key_slots_bottom)
        assert l == len(instance.bottom.formulas)

        expected_principal_side = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        for i, side in enumerate(top.formulas):
            bottom_side = instance.bottom.formulas[instance.key_slots_bottom[i]]
            if i == self.principle_formula_index:
                assert bottom_side == expected_principal_side, (
                    f"Principal formula in bottom of Promotion must be on side "
                    f"{expected_principal_side}, found {bottom_side}"
                )
                assert bottom_side == side, (
                    f"Principal formula in top of Promotion must be on the same side "
                    f"as principal in bottom, found {side} vs {bottom_side}"
                )
            else:
                assert side == bottom_side

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        top = instance.tops[0]
        expected_principal_side = Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT
        for i, (side, f) in enumerate(top.formulas):
            if i == self.principle_formula_index:
                bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[i]]
                assert isinstance(bottom_f, Bang)
                assert bottom_f.polarity == self.polarity
                assert bottom_f.sub == f
                assert bottom_side == expected_principal_side
            else:
                assert isinstance(f, Bang)
                assert (f.polarity == Polarity.POS and side == Side.LEFT) or (f.polarity == Polarity.NEG and side == Side.RIGHT)
                assert (side, f) == instance.bottom.formulas[i]


@dataclass(frozen=True)
class Dereliction(CoreRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 1
        assert len(instance.key_slots_bottom) == 1

        top_side    = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Dereliction must be on side {expected}, found {bottom_side}"
        )
        assert bottom_side == top_side, (
            f"Principal formula in top of Dereliction must match bottom side, "
            f"found {top_side} vs {bottom_side}"
        )

        top_others    = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        top = instance.tops[0]
        top_side,    top_f    = top.formulas[instance.key_slots_tops[0][0]]
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        assert bottom_f.sub == top_f
        assert bottom_side == top_side


@dataclass(frozen=True)
class Weakening(CoreRule):
    polarity: Polarity

    def check_shape(self, instance: InstantiatedRule[Side]) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 0
        assert len(instance.key_slots_bottom) == 1

        bottom_side = instance.bottom.formulas[instance.key_slots_bottom[0]]
        expected    = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected, (
            f"Principal formula in bottom of Weakening must be on side {expected}, found {bottom_side}"
        )

        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top.formulas

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity


@dataclass(frozen=True)
class Contraction(CoreRule):
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
            f"Principal formula in bottom of Contraction must be on side {expected}, found {bottom_side}"
        )

        for i in range(self.count):
            top_side = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected, (
                f"Principal formula in top of Contraction must be on side {expected}, found {top_side}"
            )

        top_others    = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

    def check_type(self, instance: InstantiatedRule[SidedFormula]) -> None:
        top = instance.tops[0]
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        for i in range(self.count):
            top_side, top_f = top.formulas[instance.key_slots_tops[0][i]]
            assert top_f == bottom_f, (
                f"Principal formula in top of Contraction must equal bottom, "
                f"expected {bottom_f}, found {top_f}"
            )

