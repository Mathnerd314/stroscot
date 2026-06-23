
from __future__ import annotations
from typed_ir import Polarity, Side, JumboFormula, Case, Atom, Formula
from pretty import fmt, register, cases_match, is_jf, any_formula, _fallback

L, R = Side.LEFT, Side.RIGHT
POS, NEG = Polarity.POS, Polarity.NEG


# ══════════════════════════════════════════════════════════════════════════════
# Connective constructors
# ══════════════════════════════════════════════════════════════════════════════


# | Operator  | Polarity | Cases                                         |
# |-----------|----------|-----------------------------------------------|
# | F (False) | POS      | `[]`                                          |
# | 1 (One)   | POS      | `[(#s, [])]`                                  |
# | A ∨ B     | POS      | `[(#l,[(L,A)]), (#r,[(L,B)])]`                |
# | A ⊗ B     | POS      | `[(#s,[(L,A),(L,B)])]`                        |
# | A^⊗n      | POS      | `[(#s,[(L,A)×n])]`                            |
# | +¬A       | POS      | `[(#s,[(R,A)])]`                              |
# | ↑A        | POS      | `[(#s,[(L,A)])]`                              |
# | T (Top)   | NEG      | `[]`                                          |
# | ⊥ (Bot)   | NEG      | `[(#s,[])]`                                   |
# | A ∧ B     | NEG      | `[(#l,[(R,A)]), (#r,[(R,B)])]`                |
# | A ⅋ B     | NEG      | `[(#s,[(R,A),(R,B)])]`                        |
# | A^⅋n      | NEG      | `[(#s,[(R,A)×n])]`                            |
# | A → B     | NEG      | `[(#f,[(L,A),(R,B)])]`                        |
# | A ↔ B     | NEG      | `[(#l,[(L,A),(R,B)]), (#r,[(L,B),(R,A)])]`    |
# | -¬A       | NEG      | `[(#s,[(L,A)])]`                              |
# | ↓A        | NEG      | `[(#s,[(R,A)])]`                              |

F            = JumboFormula(POS, ())
One          = JumboFormula(POS, (Case("#s", ()),))
Top          = JumboFormula(NEG, ())
Bot          = JumboFormula(NEG, (Case("#s", ()),))

def Plus(A, B):        return JumboFormula(POS, (Case("#l", ((L,A),)), Case("#r", ((L,B),))))
def Tensor(A, B):      return JumboFormula(POS, (Case("#s", ((L,A),(L,B))),))
def TensorPower(A, n): return JumboFormula(POS, (Case("#s", tuple((L,A) for _ in range(n))),))
def PosNeg(A):         return JumboFormula(POS, (Case("#s", ((R,A),)),))
def UpShift(A):        return JumboFormula(POS, (Case("#s", ((L,A),)),))
def With(A, B):        return JumboFormula(NEG, (Case("#l", ((R,A),)), Case("#r", ((R,B),))))
def Par(A, B):         return JumboFormula(NEG, (Case("#s", ((R,A),(R,B))),))
def ParPower(A, n):    return JumboFormula(NEG, (Case("#s", tuple((R,A) for _ in range(n))),))
def Lollipop(A, B):    return JumboFormula(NEG, (Case("#f", ((L,A),(R,B))),))
def Equiv(A, B):       return JumboFormula(NEG, (Case("#l",((L,A),(R,B))), Case("#r",((L,B),(R,A)))))
def NegNeg(A):         return JumboFormula(NEG, (Case("#s", ((L,A),)),))
def DownShift(A):      return JumboFormula(NEG, (Case("#s", ((R,A),)),))


# ══════════════════════════════════════════════════════════════════════════════
# Print rules  (registered lowest-to-highest specificity so more specific
# rules added later via prepend=True win automatically)
# ══════════════════════════════════════════════════════════════════════════════

_ = any_formula   # shorthand wildcard

# Atoms and bangs — lowest priority, caught by fallback anyway but explicit here
register("Atom", lambda f: isinstance(f, Atom),                  _fallback)
register("Bang", lambda f: not isinstance(f, (Atom, JumboFormula)), _fallback)

# Nullary
register("F",   lambda f: is_jf(POS)(f) and f.cases == (),       lambda f: "F")
register("One", lambda f: is_jf(POS)(f) and cases_match(f, (("#s", ()),)),  lambda f: "1")
register("Top", lambda f: is_jf(NEG)(f) and f.cases == (),       lambda f: "T")
register("Bot", lambda f: is_jf(NEG)(f) and cases_match(f, (("#s", ()),)),  lambda f: "⊥")

# Unary — registered before binary so the single-subformula patterns don't
# accidentally shadow; ordering within unary doesn't matter since predicates are disjoint
register("PosNeg",    lambda f: is_jf(POS)(f) and cases_match(f,(("#s",((R,_),)),)),
                      lambda f: f"+¬{fmt(f.cases[0].formulas[0][1])}")
register("UpShift",   lambda f: is_jf(POS)(f) and cases_match(f,(("#s",((L,_),)),)),
                      lambda f: f"↑{fmt(f.cases[0].formulas[0][1])}")
register("NegNeg",    lambda f: is_jf(NEG)(f) and cases_match(f,(("#s",((L,_),)),)),
                      lambda f: f"-¬{fmt(f.cases[0].formulas[0][1])}")
register("DownShift", lambda f: is_jf(NEG)(f) and cases_match(f,(("#s",((R,_),)),)),
                      lambda f: f"↓{fmt(f.cases[0].formulas[0][1])}")

# Powers (n >= 3; n=2 caught by binary rules below)
def _is_tensor_power(f):
    if not (is_jf(POS)(f) and len(f.cases)==1 and f.cases[0].label=="#s"): return False
    fmls = f.cases[0].formulas
    return len(fmls)>=3 and all(s==L for s,_ in fmls) and len({x for _,x in fmls})==1

def _is_par_power(f):
    if not (is_jf(NEG)(f) and len(f.cases)==1 and f.cases[0].label=="#s"): return False
    fmls = f.cases[0].formulas
    return len(fmls)>=3 and all(s==R for s,_ in fmls) and len({x for _,x in fmls})==1

register("TensorPower", _is_tensor_power,
         lambda f: f"{fmt(f.cases[0].formulas[0][1])}^⊗{len(f.cases[0].formulas)}")
register("ParPower",    _is_par_power,
         lambda f: f"{fmt(f.cases[0].formulas[0][1])}^⅋{len(f.cases[0].formulas)}")

# Binary single-case
register("Tensor",   lambda f: is_jf(POS)(f) and cases_match(f,(("#s",((L,_),(L,_))),)),
                     lambda f: f"({fmt(f.cases[0].formulas[0][1])} ⊗ {fmt(f.cases[0].formulas[1][1])})")
register("Par",      lambda f: is_jf(NEG)(f) and cases_match(f,(("#s",((R,_),(R,_))),)),
                     lambda f: f"({fmt(f.cases[0].formulas[0][1])} ⅋ {fmt(f.cases[0].formulas[1][1])})")
register("Lollipop", lambda f: is_jf(NEG)(f) and cases_match(f,(("#f",((L,_),(R,_))),)),
                     lambda f: f"({fmt(f.cases[0].formulas[0][1])} → {fmt(f.cases[0].formulas[1][1])})")

# Binary two-case
register("Plus",  lambda f: is_jf(POS)(f) and cases_match(f,(("#l",((L,_),)),("#r",((L,_),)))),
                  lambda f: f"({fmt(f.cases[0].formulas[0][1])} ∨ {fmt(f.cases[1].formulas[0][1])})")
register("With",  lambda f: is_jf(NEG)(f) and cases_match(f,(("#l",((R,_),)),("#r",((R,_),)))),
                  lambda f: f"({fmt(f.cases[0].formulas[0][1])} ∧ {fmt(f.cases[1].formulas[0][1])})")
register("Equiv", lambda f: is_jf(NEG)(f) and cases_match(f,(("#l",((L,_),(R,_))),("#r",((L,_),(R,_))))),
                  lambda f: f"({fmt(f.cases[0].formulas[0][1])} ↔ {fmt(f.cases[1].formulas[0][1])})")
