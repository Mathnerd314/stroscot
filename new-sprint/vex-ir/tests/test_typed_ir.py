import pytest
from typed_ir import (
    Polarity,
    RuleDerivation,
    Side,
    Atom,
    Case,
    JumboFormula,
    Bang,
    Sequent,
    Identity,
    Cut,
    Build,
    Break,
    Promotion,
    Dereliction,
    SidedFormula,
    Weakening,
    Contraction,
    InstantiatedRule,
    Node,
    BasicBlock,
    compute_key_slot_wires,
)

L, R = Side.LEFT, Side.RIGHT
POS, NEG = Polarity.POS, Polarity.NEG

A = Atom(POS, "A")
B = Atom(POS, "B")
C = Atom(POS, "C")

one = JumboFormula[SidedFormula](POS, (Case("#s", Sequent(())),))
tensor = JumboFormula[SidedFormula](POS, (Case("#s", Sequent(((L, A), (L, B)))),))
plus = JumboFormula[SidedFormula](
    POS, (Case("#l", Sequent(((L, A),))), Case("#r", Sequent(((L, B),))))
)
with_ = JumboFormula[SidedFormula](
    NEG, (Case("#l", Sequent(((R, A),))), Case("#r", Sequent(((R, B),))))
)
lollipop = JumboFormula[SidedFormula](NEG, (Case("#f", Sequent(((L, A), (R, B)))),))
bang_A = Bang(POS, A)
bang_B = Bang(POS, B)


def mk_rule_derivation(ir, premises: list[Node]) -> RuleDerivation:
    return RuleDerivation(
        instantiated_rule=ir,
        premises=premises,
        perm_tops=tuple(() for _ in premises),
        node_id=id(ir),
    )


def make_identity(f) -> RuleDerivation:
    ir = InstantiatedRule(
        rule=Identity(),
        tops=(),
        key_slots_tops=(),
        bottom=Sequent(((L, f), (R, f))),
        key_slots_bottom=(0, 1),
    )
    return mk_rule_derivation(ir, [])


# ── Identity ──────────────────────────────────────────────────────────────────


def test_identity_atom():
    d = make_identity(A)
    assert d.conclusion == Sequent(((L, A), (R, A)))
    assert d.is_leaf


def test_identity_jumbo():
    make_identity(tensor)


def test_identity_wrong_shape():
    ir = InstantiatedRule(
        rule=Identity(),
        tops=(),
        key_slots_tops=(),
        bottom=Sequent(((L, A), (R, B))),
        key_slots_bottom=(0, 1),
    )
    with pytest.raises(AssertionError):
        mk_rule_derivation(ir, [])


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


def test_promotion():
    ir = InstantiatedRule(
        rule=Promotion(POS, principle_formula_index=1),
        tops=(Sequent(((L, bang_A), (R, B))),),
        key_slots_tops=((0, 1),),
        bottom=Sequent(((L, bang_A), (R, bang_B))),
        key_slots_bottom=(0, 1),
    )
    ir.validate()


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
        mk_rule_derivation(id_A.instantiated_rule, [id_A]).validate()

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
        RuleDerivation(instantiated_rule=ir_cut, premises=[id_B, id_B],perm_tops=(),node_id=None).validate()

def test_derivation_depth_and_size():
    id_A = make_identity(A)
    assert id_A.depth() == 0
    assert id_A.size() == 1


def test_derivation_pretty_runs():
    # Sequent has no __str__ so pretty() uses repr — just check it doesn't crash
    # and contains the rule class name
    d = make_identity(A)
    out = d.pretty()
    assert "Identity" in out


def test_compute_key_slot_wires_identity_leaf_empty():
    d = make_identity(A)
    wire_map = compute_key_slot_wires(d)
    assert d.node_id in wire_map
    assert wire_map[d.node_id] == ()


def test_compute_key_slot_wires_cut_to_identity_destinations():
    id_A = make_identity(A)
    id_B = make_identity(B)

    ir_cut = InstantiatedRule(
        rule=Cut(),
        tops=(Sequent(((R, A),)), Sequent(((L, A), (R, B)))),
        key_slots_tops=((0,), (0,)),
        bottom=Sequent(((R, B),)),
        key_slots_bottom=(),
    )
    cut = RuleDerivation(instantiated_rule=ir_cut, premises=[id_A, id_B], perm_tops=(), node_id=id(ir_cut))

    wire_map = compute_key_slot_wires(cut)
    cut_wires = wire_map[cut.node_id]
    assert len(cut_wires) == 2

    # top0 key reaches id_A at structural slot 0.
    w0 = cut_wires[0]
    assert w0.source.node_id == cut.node_id
    assert w0.source.top_index == 0
    assert w0.source.key_slot == 0
    assert w0.source.formal_slot == 0
    assert w0.target.node_id == id_A.node_id
    assert w0.target.key_slot == 0
    assert w0.target.formal_slot == 0

    # top1 key (L,A) reaches id_B bottom key index 0 (L,B) by slot position
    # (the analysis is structural and does not inspect formula equality)
    w1 = cut_wires[1]
    assert w1.source.node_id == cut.node_id
    assert w1.source.top_index == 1
    assert w1.source.key_slot == 0
    assert w1.source.formal_slot == 0
    assert w1.target.node_id == id_B.node_id
    assert w1.target.key_slot == 0
    assert w1.target.formal_slot == 0


def test_compute_key_slot_wires_basic_block_all_slots_key():
    id_A = make_identity(A)
    bb = BasicBlock(
        label="bb0",
        premises=[id_A],
        perm_tops=((1, 0),),
        node_id=123,
    )

    wire_map = compute_key_slot_wires(bb)
    bb_wires = wire_map[bb.node_id]
    assert len(bb_wires) == 2

    # BasicBlock top formal slot 0 maps to child bottom slot 1 due to perm (1,0)
    assert bb_wires[0].source.top_index == 0
    assert bb_wires[0].source.formal_slot == 0
    assert bb_wires[0].target.node_id == id_A.node_id
    assert bb_wires[0].target.formal_slot == 1

    # BasicBlock top formal slot 1 maps to child bottom slot 0
    assert bb_wires[1].source.top_index == 0
    assert bb_wires[1].source.formal_slot == 1
    assert bb_wires[1].target.node_id == id_A.node_id
    assert bb_wires[1].target.formal_slot == 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
