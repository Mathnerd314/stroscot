# Pretty-printer registrations for dirty_ir.py types and admissible rules.
# Import this module to activate the rules; they prepend themselves over
# the fallback so more-specific matches win.
from __future__ import annotations
from pretty import fmt, register
from ir_types import Polarity
from dirty_ir import (
    OpaqueType, FlatType, FlatOperation,
    Const, IfBool,
    WeakPromotion, Digging, Absorption, Multiplexing,
    Int32, Int64, Float32, Float64, Bool,
)

# ── OpaqueType / FlatType ─────────────────────────────────────────────────────

# Generic fallback for any OpaqueType: render as "Name^+" or "Name^-"
register(
    "OpaqueType",
    lambda f: isinstance(f, OpaqueType),
    lambda f: str(f),   # OpaqueType.__str__ already returns "Name^pol"
    prepend=True,
)

# Concrete flat primitives — register by name so they render without the "^+"
for _ft in (Int32, Int64, Float32, Float64, Bool):
    _name = _ft.name
    register(
        _name,
        lambda f, ft=_ft: f is ft,
        lambda f, n=_name: n,
        prepend=True,
    )

# ── AdmissibleRule pretty-printing (for Derivation.pretty()) ──────────────────
# These are formula-level renderers; rule-level rendering lives in each
# rule's __repr__ / name attribute. Sequent slots use fmt() on formulas,
# so all we need here are the formula-level hooks already handled above.

# IfBool is a rule, not a formula, so no formula printer needed.
# FlatOperation instances carry their own __repr__.
# Nothing more needed at the formula level for the remaining rule classes.