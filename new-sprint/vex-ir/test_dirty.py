import pytest
from typed_ir import (
    Atom, Bang, Polarity, Side, Sequent,
    Identity, Cut,
    InstantiatedRule, Derivation,
)
from dirty_ir import (
    OpaqueType, FlatType, FlatOperation,
    Int32, Int64, Float32, Float64, Bool,
    AddI32, SubI32, MulI32, NegI32, EqI32, LtI32,
    Const, IfBool,
    WeakPromotion, Digging, Absorption, Multiplexing,
    AdmissibleRule,
)

L, R = Side.LEFT, Side.RIGHT
POS, NEG = Polarity.POS, Polarity.NEG

A = Atom("A")
B = Atom("B")
bang_A = Bang(POS, A)

# ── Helpers ───────────────────────────────────────────────────────────────────

def make_flat_op(rule, *args, result):
    """Build a valid bottom sequent for a FlatOperation and call check_shape."""
    lefts = tuple((L, t) for t in args)
    bottom = Sequent(lefts + ((R, result),))
    ir = InstantiatedRule(
        rule=rule,
        tops=(),
        key_slots_tops=(),
        bottom=bottom,
        key_slots_bottom=(),
    )
    ir.validate()
    return ir

# ── isinstance tag ────────────────────────────────────────────────────────────

def test_admissible_rule_tag():
    assert isinstance(AddI32, AdmissibleRule)
    assert isinstance(EqI32, AdmissibleRule)

# ── OpaqueType / FlatType ─────────────────────────────────────────────────────

def test_flat_type_str():
    assert str(Int32) == "Int32^+"
    assert str(Float64) == "Float64^+"

def test_flat_type_get_case_formula():
    assert Int32.get_case_formula(0) == ()
    assert Bool.get_case_formula(True) == ()

def test_opaque_active_side():
    assert Int32.active_side == R
    custom_neg = OpaqueType(name="Foo", polarity=NEG)
    assert custom_neg.active_side == L

# ── FlatOperation ─────────────────────────────────────────────────────────────

def test_add_i32_valid():
    make_flat_op(AddI32, Int32, Int32, result=Int32)

def test_neg_i32_valid():
    make_flat_op(NegI32, Int32, result=Int32)

def test_eq_i32_produces_bool():
    make_flat_op(EqI32, Int32, Int32, result=Bool)

def test_flat_op_wrong_arg_type():
    bottom = Sequent(((L, Int64), (L, Int32), (R, Int32)))
    ir = InstantiatedRule(
        rule=AddI32, tops=(), key_slots_tops=(),
        bottom=bottom, key_slots_bottom=(),
    )
    with pytest.raises(AssertionError):
        ir.validate()

def test_flat_op_wrong_arity():
    bottom = Sequent(((L, Int32), (R, Int32)))
    ir = InstantiatedRule(
        rule=AddI32, tops=(), key_slots_tops=(),
        bottom=bottom, key_slots_bottom=(),
    )
    with pytest.raises(AssertionError):
        ir.validate()

def test_flat_op_has_premises():
    bottom = Sequent(((L, Int32), (L, Int32), (R, Int32)))
    ir = InstantiatedRule(
        rule=AddI32,
        tops=(Sequent(((R, Int32),)),),
        key_slots_tops=((0,),),
        bottom=bottom,
        key_slots_bottom=(),
    )
    with pytest.raises(AssertionError):
        ir.validate()

def test_flat_op_repr():
    assert "add_i32" in repr(AddI32)
    assert "Int32" in repr(AddI32)

# ── Const ─────────────────────────────────────────────────────────────────────

def test_const_flat_type():
    # FlatType.get_case_formula returns () so Const has no premises
    rule = Const(principal=Int32, case_label=42)
    bottom = Sequent(((R, Int32),))
    ir = InstantiatedRule(
        rule=rule, tops=(), key_slots_tops=(),
        bottom=bottom, key_slots_bottom=(0,),
    )
    ir.validate()

def test_const_wrong_formula():
    rule = Const(principal=Int32, case_label=0)
    bottom = Sequent(((R, Float32),))
    ir = InstantiatedRule(
        rule=rule, tops=(), key_slots_tops=(),
        bottom=bottom, key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── IfBool ────────────────────────────────────────────────────────────────────

def test_if_bool_basic():
    # cond : Bool ⊢ result : Int32  (two branch tops, context empty)
    rule = IfBool(branch_type=Int32)
    then_top = Sequent(((R, Int32),))
    else_top = Sequent(((R, Int32),))
    bottom   = Sequent(((L, Bool), (R, Int32)))
    ir = InstantiatedRule(
        rule=rule,
        tops=(then_top, else_top),
        key_slots_tops=((), ()),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_if_bool_wrong_cond_type():
    rule = IfBool(branch_type=Int32)
    then_top = Sequent(((R, Int32),))
    else_top = Sequent(((R, Int32),))
    bottom   = Sequent(((L, Int32), (R, Int32)))  # Int32 where Bool expected
    ir = InstantiatedRule(
        rule=rule,
        tops=(then_top, else_top),
        key_slots_tops=((), ()),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── Digging ───────────────────────────────────────────────────────────────────

def test_digging_pos():
    # !!A on left  →  !A on left  (POS)
    bang_bang_A = Bang(POS, bang_A)
    top    = Sequent(((L, bang_A),))
    bottom = Sequent(((L, bang_bang_A),))
    ir = InstantiatedRule(
        rule=Digging(POS),
        tops=(top,),
        key_slots_tops=((0,),),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_digging_wrong_polarity():
    bang_bang_A = Bang(POS, bang_A)
    top    = Sequent(((L, bang_A),))
    bottom = Sequent(((L, bang_bang_A),))
    ir = InstantiatedRule(
        rule=Digging(NEG),   # wrong polarity
        tops=(top,),
        key_slots_tops=((0,),),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── Absorption ────────────────────────────────────────────────────────────────

def test_absorption_pos():
    # !A, A  ⊢  ...   → !A ⊢ ...  (dereliction + identity collapsed)
    top    = Sequent(((L, A), (L, bang_A)))
    bottom = Sequent(((L, bang_A),))
    ir = InstantiatedRule(
        rule=Absorption(POS),
        tops=(top,),
        key_slots_tops=((0, 1),),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_absorption_wrong_slots():
    top    = Sequent(((L, A),))   # only one formula — can't hold two key slots
    bottom = Sequent(((L, bang_A),))
    ir = InstantiatedRule(
        rule=Absorption(POS),
        tops=(top,),
        key_slots_tops=((0,),),   # only one slot instead of two
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── Multiplexing ──────────────────────────────────────────────────────────────

def test_multiplexing_pos_count2():
    top    = Sequent(((L, A), (L, A)))
    bottom = Sequent(((L, bang_A),))
    ir = InstantiatedRule(
        rule=Multiplexing(POS, count=2),
        tops=(top,),
        key_slots_tops=((0, 1),),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    ir.validate()

def test_multiplexing_count_mismatch():
    top    = Sequent(((L, A), (L, A)))
    bottom = Sequent(((L, bang_A),))
    ir = InstantiatedRule(
        rule=Multiplexing(POS, count=3),  # says 3 but only 2 key slots
        tops=(top,),
        key_slots_tops=((0, 1),),
        bottom=bottom,
        key_slots_bottom=(0,),
    )
    with pytest.raises(AssertionError):
        ir.validate()

# ── Pretty-printer smoke tests ────────────────────────────────────────────────

def test_pretty_printer_opaque():
    import dirty_connectives   # registers rules
    from pretty import fmt
    assert fmt(Int32) == "Int32"
    assert fmt(Bool)  == "Bool"
    assert fmt(Float64) == "Float64"

def test_pretty_printer_opaque_generic():
    import dirty_connectives   # registers rules
    from pretty import fmt
    custom = OpaqueType(name="MyType", polarity=POS)
    assert fmt(custom) == "MyType^+"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])