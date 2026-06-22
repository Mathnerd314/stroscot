from connectives import *
from pretty import fmt, register, is_jf, cases_match, any_formula
from typed_ir import Atom, Bang, Polarity, JumboFormula, Case, Side

L, R = Side.LEFT, Side.RIGHT
POS, NEG = Polarity.POS, Polarity.NEG
A = Atom("A"); B = Atom("B"); C = Atom("C")

tests = [
    ("F",             F),
    ("One",           One),
    ("Top",           Top),
    ("Bot",           Bot),
    ("A ∨ B",         Plus(A, B)),
    ("A ⊗ B",         Tensor(A, B)),
    ("A ⅋ B",         Par(A, B)),
    ("A → B",         Lollipop(A, B)),
    ("A ∧ B",         With(A, B)),
    ("A ↔ B",         Equiv(A, B)),
    ("+¬A",           PosNeg(A)),
    ("-¬A",           NegNeg(A)),
    ("↑A",            UpShift(A)),
    ("↓A",            DownShift(A)),
    ("A^⊗3",          TensorPower(A, 3)),
    ("A^⅋4",          ParPower(A, 4)),
    ("nested",        Lollipop(Tensor(A, B), With(A, C))),
    ("!A",            Bang(POS, Lollipop(A, B))),
    ("?(A∨B)",        Bang(NEG, Plus(A, B))),
    ("unknown",       JumboFormula(POS, (Case("#x", ((L, A), (R, B))), Case("#y", ())))),
]

for label, f in tests:
    print(f"  {label:<12}  =>  {fmt(f)}")

# Extension example: Bool
_ = any_formula
register("Bool",
    lambda f: is_jf(POS)(f) and len(f.cases)==2
              and f.cases[0].label=="#t" and len(f.cases[0].formulas)==0
              and f.cases[1].label=="#f" and len(f.cases[1].formulas)==0,
    lambda f: "Bool",
    prepend=True,
)
Bool = JumboFormula(POS, (Case("#t", ()), Case("#f", ())))
print()
print(f"  Bool         =>  {fmt(Bool)}")
print(f"  Bool∨A       =>  {fmt(Plus(Bool, A))}")