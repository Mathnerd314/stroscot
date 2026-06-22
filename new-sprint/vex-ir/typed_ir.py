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


# ══════════════════════════════════════════════════════════════════════════════
# Inference Rule Schemas
# An inference rule schema is the information in parentheses in the derivation tree, i.e. the "reason" for the inference step.
# ══════════════════════════════════════════════════════════════════════════════

class RuleSchema:
    pass

## Structural rules

# A |- A
@dataclass(frozen=True)
class Identity(RuleSchema):
    pass

# A , A |- B  =>  A |- B
@dataclass(frozen=True)
class Cut(RuleSchema):
    pass

## Jumbo rules
@dataclass(frozen=True)
class Build(RuleSchema):
    """Introduce a JumboFormula on its active side.
    case_index picks the disjunct branch; always 0 for single-case connectives.
    Produces exactly 1 premise."""
    principal: JumboFormula
    case_index: int = 0

@dataclass(frozen=True)
class Break(RuleSchema):
    """Eliminate a JumboFormula from its passive side.
    Produces one premise per case (all branches handled).
    No choice — fully invertible."""
    principal: JumboFormula

## Exponential rules

@dataclass(frozen=True)
class Promotion(RuleSchema):
    polarity: Polarity

@dataclass(frozen=True)
class Dereliction(RuleSchema):
    polarity: Polarity

@dataclass(frozen=True)
class Weakening(RuleSchema):
    polarity: Polarity

@dataclass(frozen=True)
class Contraction(RuleSchema):
    polarity: Polarity
    count: int = 2  # number of copies to contract into (default 2 for binary contraction)

@dataclass(frozen=True)
class WeakPromotion(RuleSchema):
    polarity: Polarity

@dataclass(frozen=True)
class Digging(RuleSchema):
    polarity: Polarity

@dataclass(frozen=True)
class Absorption(RuleSchema):
    polarity: Polarity

@dataclass(frozen=True)
class Multiplexing(RuleSchema):
    polarity: Polarity
    count: int = 2  # number of copies to contract into (default 2 for binary contraction)

# ══════════════════════════════════════════════════════════════════════════════
# Derivation Tree
# ══════════════════════════════════════════════════════════════════════════════

@dataclass
class Derivation:
    conclusion: Sequent
    rule:       RuleSchema
    premises:   list[Derivation] = field(default_factory=list)

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
        lines.append(f"{pad}[{_fmt_rule(self.rule)}]  {self.conclusion}")
        return "\n".join(lines)