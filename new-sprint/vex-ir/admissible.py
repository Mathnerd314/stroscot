
@dataclass(frozen=True)
class WeakPromotion(RuleSchema):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        l = len(top.formulas)
        # all slots must be marked as key slots and match 1-1 between top and bottom
        assert l == len(instance.key_slots_tops[0])
        assert l == len(instance.key_slots_bottom)
        assert l == len(instance.bottom.formulas)
        
        # all formulas are promoted to bang-wrapped, but only principal formula is on its active side and the rest are passthrough formulas on the opposite side
        num_principal_formulas = 0
        for i, (side, f) in enumerate(top.formulas):
            if isinstance(instance.bottom.formulas[0], Bang) and instance.bottom.formulas[0].sub == f:
                bang = instance.bottom.formulas[0]
                # check whether passthrough or principal formula
                if bang.polarity == Polarity.POS and side == Side.LEFT or bang.polarity == Polarity.NEG and side == Side.RIGHT:
                    # passive bang formula
                    continue
                elif bang.polarity == self.polarity and side == (Side.RIGHT if self.polarity == Polarity.POS else Side.LEFT):
                    # principal formula
                    num_principal_formulas += 1
            else:
                raise AssertionError(f"Formula {f} at slot {i} in top does not match expected shape for WeakPromotion")
        
        assert num_principal_formulas == 1, f"Expected exactly one principal formula in top for WeakPromotion, found {num_principal_formulas}"

@dataclass(frozen=True)
class Digging(RuleSchema):
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
        assert isinstance(top_f, Bang)
        assert top_f.polarity == self.polarity
        assert top_f.sub == bottom_f
        assert bottom_side == top_side
        expected_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_side, f"Principal formula in bottom of Dereliction must be on side {expected_side}, found {bottom_side}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

@dataclass(frozen=True)
class Absorption(RuleSchema):
    polarity: Polarity
    def check_shape(self, instance: InstantiatedRule) -> None:
        assert len(instance.tops) == 1
        top = instance.tops[0]
        assert len(instance.key_slots_tops) == 1
        assert len(instance.key_slots_tops[0]) == 2
        assert len(instance.key_slots_bottom) == 1
        
        # the principal formula in the bottom must be the "contracted" version of the principal formula in the top, with the correct side
        bottom_side, bottom_f = instance.bottom.formulas[instance.key_slots_bottom[0]]
        assert isinstance(bottom_f, Bang)
        assert bottom_f.polarity == self.polarity
        expected_bottom_side = Side.LEFT if self.polarity == Polarity.POS else Side.RIGHT
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Absorption must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the top is one dereliction and one exact match
        top_side, top_f = top.formulas[instance.key_slots_tops[0][0]]
        assert top_side == expected_bottom_side, f"Principal formula in top of Absorption must be on side {expected_bottom_side}, found {top_side}"
        assert top_f == bottom_f.sub, f"Principal formula in top of Absorption must be the subformula of the principal formula in bottom, expected {bottom_f.sub}, found {top_f}"

        top_side, top_f = top.formulas[instance.key_slots_tops[0][1]]
        assert top_side == expected_bottom_side, f"Principal formula in top of Absorption must be on side {expected_bottom_side}, found {top_side}"
        assert top_f == bottom_f, f"Principal formula in top of Absorption must be the same as the principal formula in bottom, expected {bottom_f}, found {top_f}"


@dataclass(frozen=True)
class Multiplexing(RuleSchema):
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
        assert bottom_side == expected_bottom_side, f"Principal formula in bottom of Multiplexing must be on side {expected_bottom_side}, found {bottom_side}"
        
        # check that the each instance of the top formula matches the bottom formula
        for i in range(self.count):
            top_side, top_f = top.formulas[instance.key_slots_tops[0][i]]
            assert top_side == expected_bottom_side, f"Principal formula in top of Multiplexing must be on side {expected_bottom_side}, found {top_side}"
            assert top_f == bottom_f.sub, f"Principal formula in top of Multiplexing must be the subformula of the principal formula in bottom, expected {bottom_f.sub}, found {top_f}"
        
        # context preservation - all other formulas in the bottom must be present in the top
        top_others = array_minus_key_slots(top.formulas, instance.key_slots_tops[0])
        bottom_others = array_minus_key_slots(instance.bottom.formulas, instance.key_slots_bottom)
        assert bottom_others == top_others

def test_digging():
    bang_bang_A = Bang(POS, bang_A)
    ir = InstantiatedRule(
        rule=Digging(POS),
        tops=(Sequent(((L, bang_bang_A), (R, B))),),
        key_slots_tops=((0,),),
        bottom=Sequent(((L, bang_A), (R, B))),
        key_slots_bottom=(0,),
    )
    ir.validate()

