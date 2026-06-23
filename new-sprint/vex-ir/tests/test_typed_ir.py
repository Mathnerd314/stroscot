
import pytest
from typed_ir import (
    Polarity, Side, Atom, Case, JumboFormula, Bang, Sequent,
    Identity, Cut, Build, Break,
    Promotion, Dereliction, Weakening, Contraction,
    InstantiatedRule, Derivation,
)

L, R = Side.LEFT, Side.RIGHT
POS, NEG = Polarity.POS, Polarity.NEG

A = Atom("A")
B = Atom("B")
C = Atom("C")

one      = JumboFormula(POS, (Case("#s", ()),))
tensor   = JumboFormula(POS, (Case("#s", ((L, A), (L, B))),))
plus     = JumboFormula(POS, (Case("#l", ((L, A),)), Case("#r", ((L, B),))))
with_    = JumboFormula(NEG, (Case("#l", ((R, A),)), Case("#r", ((R, B),))))
lollipop = JumboFormula(NEG, (Case("#f", ((L, A), (R, B))),))
bang_A   = Bang(POS, A)

def make_identity(f):
    ir = InstantiatedRule(
        rule=Identity(), tops=(), key_slots_tops=(),
        bottom=Sequent(((L, f), (R, f))),
        key_slots_bottom=(0, 1),
    )
    return Derivation(ir)

# ── Identity ──────────────────────────────────────────────────────────────────

def test_identity_atom():
    d = make_identity(A)
    assert d.conclusion == Sequent(((L, A), (R, A)))
    assert d.is_leaf

def test_identity_jumbo():
    make_identity(tensor)

def test_identity_wrong_shape():
    ir = InstantiatedRule(
        rule=Identity(), tops=(), key_slots_tops=(),
        bottom=Sequent(((L, A), (R, B))),
        key_slots_bottom=(0, 1),
    )
    with pytest.raises(AssertionError):
        Derivation(ir)

# ── Cut ───────────────────────────────────────────────────────────────────────

def test_cut_basic():
    ir = InstantiatedRule(
        rule=Cut(),
        tops=(Sequent(((R, A),)), Sequent(((L, A), (R, B)))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, B),)),
        key_slots_bottom=(),
    )
    ir.validate()

def test_cut_wrong_formula():
    ir = InstantiatedRule(
        rule=Cut(),
        tops=(Sequent(((R, A),)), Sequent(((L, B), (R, B)))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, B),)),
        key_slots_bottom=(),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── Build ─────────────────────────────────────────────────────────────────────

def test_build_one():
    # One: 0 subformulas, 0 tops, bottom = (R, one)
    ir = InstantiatedRule(
        rule=Build(principal=one, case_index=0),
        tops=(),
        key_slots_tops=(),
        bottom=Sequent(((R, one),)),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_build_tensor_no_context():
    ir = InstantiatedRule(
        rule=Build(principal=tensor, case_index=0),
        tops=(Sequent(((L, A),)), Sequent(((L, B),))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, tensor),)),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_build_tensor_with_shared_context():
    ir = InstantiatedRule(
        rule=Build(principal=tensor, case_index=0),
        tops=(Sequent(((L, A), (R, C))), Sequent(((L, B), (R, C)))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, tensor), (R, C), (R, C))),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_build_tensor_wrong_side():
    ir = InstantiatedRule(
        rule=Build(principal=tensor, case_index=0),
        tops=(Sequent(((L, A),)), Sequent(((L, B),))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((L, tensor),)),
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

def test_build_plus_left():
    ir = InstantiatedRule(
        rule=Build(principal=plus, case_index=0),
        tops=(Sequent(((L, A),)),),
        key_slots_tops=((0,),),
        bottom=Sequent(((R, plus),)),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_build_plus_right():
    ir = InstantiatedRule(
        rule=Build(principal=plus, case_index=1),
        tops=(Sequent(((L, B),)),),
        key_slots_tops=((0,),),
        bottom=Sequent(((R, plus),)),
        key_slots_bottom=(0,),
    )
    ir.validate()

# ── Break ─────────────────────────────────────────────────────────────────────

def test_break_with():
    ir = InstantiatedRule(
        rule=Break(principal=with_),
        tops=(Sequent(((R, A),)), Sequent(((R, B),))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, with_),)),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_break_with_with_context():
    ir = InstantiatedRule(
        rule=Break(principal=with_),
        tops=(Sequent(((R, A), (L, C))), Sequent(((R, B), (L, C)))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, with_), (L, C))),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_break_lollipop():
    ir = InstantiatedRule(
        rule=Break(principal=lollipop),
        tops=(Sequent(((L, A), (R, B))),),
        key_slots_tops=((0, 1),),
        bottom=Sequent(((R, lollipop),)),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_break_wrong_side():
    ir = InstantiatedRule(
        rule=Break(principal=with_),
        tops=(Sequent(((R, A),)), Sequent(((R, B),))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((L, with_),)),
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── Exponentials ──────────────────────────────────────────────────────────────

def test_weakening():
    ir = InstantiatedRule(
        rule=Weakening(POS),
        tops=(Sequent(((R, B),)),),
        key_slots_tops=(),
        bottom=Sequent(((L, bang_A), (R, B))),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_weakening_wrong_polarity():
    ir = InstantiatedRule(
        rule=Weakening(POS),
        tops=(Sequent(((R, B),)),),
        key_slots_tops=(),
        bottom=Sequent(((L, Bang(NEG, A)), (R, B))),
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

def test_dereliction():
    ir = InstantiatedRule(
        rule=Dereliction(POS),
        tops=(Sequent(((L, A), (R, B))),),
        key_slots_tops=((0,),),
        bottom=Sequent(((L, bang_A), (R, B))),
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_contraction():
    ir = InstantiatedRule(
        rule=Contraction(POS, count=2),
        tops=(Sequent(((L, bang_A), (L, bang_A), (R, B))),),
        key_slots_tops=((0, 1),),
        bottom=Sequent(((L, bang_A), (R, B))),
        key_slots_bottom=(0,),
    )
    ir.validate()

# ── Derivation tree wiring ────────────────────────────────────────────────────

def test_derivation_premise_count_mismatch():
    id_A = make_identity(A)
    with pytest.raises(ValueError):
        Derivation(id_A.instantiated_rule, premises=[id_A])

def test_derivation_conclusion_mismatch():
    id_B = make_identity(B)
    top0 = Sequent(((R, A),))
    top1 = Sequent(((L, A), (R, B)))
    ir_cut = InstantiatedRule(
        rule=Cut(),
        tops=(top0, top1),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, B),)),
        key_slots_bottom=(),
    )
    with pytest.raises((AssertionError, ValueError)):
        Derivation(ir_cut, premises=[id_B, id_B])

def test_derivation_depth_and_size():
    id_A = make_identity(A)
    assert id_A.depth() == 0
    assert id_A.size()  == 1

def test_derivation_pretty_runs():
    # Sequent has no __str__ so pretty() uses repr — just check it doesn't crash
    # and contains the rule class name
    d = make_identity(A)
    out = d.pretty()
    assert "Identity" in out

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
