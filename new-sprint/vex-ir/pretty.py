
from __future__ import annotations
from dataclasses import dataclass
from typing import Callable
from ir_types import Polarity, Side, Formula
from core_ir import JumboFormula, Case, Atom, Box

L, R = Side.LEFT, Side.RIGHT
POS, NEG = Polarity.POS, Polarity.NEG


# ── Pattern helpers ───────────────────────────────────────────────────────────

def any_formula(_) -> bool:
    return True

def is_jf(pol: Polarity) -> Callable[[Formula], bool]:
    return lambda f: isinstance(f, JumboFormula) and f.polarity == pol

def cases_match(jf: JumboFormula, pattern: tuple) -> bool:
    """Match jf.cases against a tuple of (label, [(side, pred_or_value), ...])."""
    if len(jf.cases) != len(pattern):
        return False
    for case, (label, fmls) in zip(jf.cases, pattern):
        if case.label != label or len(case.formulas.formulas) != len(fmls):
            return False
        for (side, f), (exp_side, exp_f) in zip(case.formulas.formulas, fmls):
            if side != exp_side:
                return False
            if not (exp_f(f) if callable(exp_f) else f == exp_f):
                return False
    return True


# ── Rule registry ─────────────────────────────────────────────────────────────

@dataclass
class PrintRule:
    name:      str
    predicate: Callable[[Formula], bool]
    renderer:  Callable[[Formula], str]

_rules: list[PrintRule] = []

def register(name: str,
             predicate: Callable[[Formula], bool],
             renderer:  Callable[[Formula], str],
             *, prepend: bool = False) -> None:
    """Add a print rule. prepend=True gives it priority over existing rules."""
    rule = PrintRule(name, predicate, renderer)
    _rules.insert(0 if prepend else len(_rules), rule)

def fmt(f: Formula) -> str:
    for rule in _rules:
        if rule.predicate(f):
            return rule.renderer(f)
    return _fallback(f)


# ── Generic fallback ──────────────────────────────────────────────────────────

def _fmt_case(case: Case, polarity: Polarity) -> str:
    sep    = " - " if polarity == POS else " ⊸ "
    inputs  = ", ".join(fmt(x) for s, x in case.formulas.formulas if s == L)
    outputs = ", ".join(fmt(x) for s, x in case.formulas.formulas if s == R)
    return f"({case.label}, [{inputs}]{sep}[{outputs}])"

def _fallback(f: Formula) -> str:
    if isinstance(f, Atom):
        return f.name
    if isinstance(f, Box):
        sym = "!" if f.polarity == POS else "?"
        mod = f"[{f.modality}]" if f.modality else ""
        return f"{sym}{mod}{fmt(f.sub)}"
    if isinstance(f, JumboFormula):
        cases_str = "[" + ", ".join(_fmt_case(c, f.polarity) for c in f.cases) + "]"
        return f"𝕁^{f.polarity.value} {cases_str}"
    return repr(f)
