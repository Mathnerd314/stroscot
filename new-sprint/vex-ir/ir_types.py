from __future__ import annotations
from collections import defaultdict
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Generic, Literal, Type, TypeVar, Optional

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

    def is_cartesian(self) -> bool:
        return False
    def is_cocartesian(self) -> bool:
        return False

SidedFormula = tuple[Side, Formula]

# ══════════════════════════════════════════════════════════════════════════════=
# Sequent  Γ ⊢ Δ  as a flat list of slots
# ══════════════════════════════=════════════════════════════════════════════════

@dataclass(frozen=True)
class Sequent(Generic[Slot]):
    formulas: tuple[Slot, ...]
    
    def on(self, side: Side) -> tuple[Slot, ...]:
        return tuple(s for s in self.formulas if slot_side(s) == side)

    @property
    def antecedents(self) -> tuple[Slot, ...]:
        return self.on(Side.LEFT)

    @property
    def succedents(self) -> tuple[Slot, ...]:
        return self.on(Side.RIGHT)

    def count_antecedents(self) -> int:
        return sum(1 for s in self.formulas if slot_side(s) == Side.LEFT)

    def count_succedents(self) -> int:
        return sum(1 for s in self.formulas if slot_side(s) == Side.RIGHT)

    def __str__(self) -> str:
        if not self.formulas:
            return "⊢"
        elif isinstance(self.formulas[0], Side):
            return f"{self.count_antecedents()} ⊢ {self.count_succedents()}"
        elif isinstance(self.formulas[0], tuple) and len(self.formulas[0]) == 2:
            fs: tuple[SidedFormula, ...] = self.formulas # type: ignore
            left_str  = ", ".join(str(f) for f in self.antecedents)
            right_str = ", ".join(str(f) for f in self.succedents)
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

    premises          — the premise sequents (in order)
    conclusion        — the conclusion sequent
    rule              — the rule that generated this instance
    key_slots_premises    — marks the principal formula(s) in each premise sequent
    key_slots_conclusion  — marks the principal formula(s) in the conclusion sequent

    Invariant: shape of premises/conclusion must be consistent with rule —
    enforced by rule.validate, not the constructor.
    """
    rule: RuleSchema
    premises: tuple[Sequent[Slot], ...]
    key_slots_premises: tuple[tuple[int, ...], ...]
    conclusion: Sequent[Slot]
    key_slots_conclusion: tuple[int, ...]
    
    def __init__(self, rule: RuleSchema, premises: tuple[Sequent[Slot], ...], key_slots_premises: tuple[tuple[int, ...], ...], conclusion: Any, key_slots_conclusion: tuple[int, ...]):
        object.__setattr__(self, "rule", rule)
        object.__setattr__(self, "premises", premises)
        object.__setattr__(self, "key_slots_premises", key_slots_premises)
        if isinstance(conclusion, Sequent):
            object.__setattr__(self, "conclusion", conclusion)
        elif isinstance(conclusion, tuple):
            object.__setattr__(self, "conclusion", Sequent(conclusion))
        else:
            raise TypeError(f"conclusion must be a Sequent or tuple, got {type(conclusion)}")
        object.__setattr__(self, "key_slots_conclusion", key_slots_conclusion)

    def validate(self) -> None:
        if isinstance(self.conclusion.formulas[0], tuple) and len(self.conclusion.formulas[0]) == 2:
            # Typed IR
            self.rule.check_shape_and_type(self) # type: ignore
        else:
            # Erased IR
            self.rule.check_shape(self) # type: ignore

    def erase(self: InstantiatedRule[Any]) -> InstantiatedRule[Side]:
        """Return a copy of *ir* with all TypedSequents replaced by ErasedSequents."""
        return InstantiatedRule(
            rule=self.rule,
            premises=tuple(t.erase() for t in self.premises),
            key_slots_premises=self.key_slots_premises,
            conclusion=self.conclusion.erase(),
            key_slots_conclusion=self.key_slots_conclusion,
        )

NodeId = int

@dataclass
class Node:
    """A node in the proof tree.

    premises          — sub-derivations/nodes, one per premise in instantiated_rule.premises
    perm_premises     — permutation applied to each premise before matching premise slots
                        (identity by default; filled in __post_init__)
    """
    premises: list[Node]
    perm_premises: tuple[tuple[int, ...], ...]
    node_id: NodeId
        
    def __post_init__(self):
        self.node_id = id(self)  # Use the built-in id() for uniqueness

    # Abstract methods that must be implemented in subclasses

    @property
    def conclusion(self) -> Sequent:
        raise NotImplementedError("Node.conclusion must be implemented in subclasses")

    def pretty(self, indent: int = 0) -> str:
        raise NotImplementedError("Node.pretty must be implemented in subclasses")

    def premises_key_slots(self) -> tuple[tuple[int, ...], ...]:
        """Return key-slot indices for each premise sequent."""
        raise NotImplementedError("Node.premises_key_slots must be implemented in subclasses")

    def conclusion_key_slots(self) -> tuple[int, ...]:
        """Return key-slot indices for the conclusion sequent."""
        raise NotImplementedError("Node.conclusion_key_slots must be implemented in subclasses")

    def non_key_conclusion_to_premise(self) -> dict[int, tuple[int, int]]:
        """Map conclusion non-key slot index -> (premise_index, premise_slot_index)."""
        raise NotImplementedError("Node.non_key_conclusion_to_premise must be implemented in subclasses")

    # Helper methods

    @property
    def is_leaf(self) -> bool:
        return not self.premises

    def depth(self) -> int:
        return 0 if self.is_leaf else 1 + max(p.depth() for p in self.premises)

    def size(self) -> int:
        return 1 + sum(p.size() for p in self.premises)

    def premise_slot_to_child_conclusion_slot(self, top_index: int, top_slot: int) -> int:
        """Map this node's premise slot position to the corresponding child conclusion slot."""
        if top_index >= len(self.perm_premises):
            raise ValueError(
                f"Node {self.node_id} has no permutation for top index {top_index}"
            )
        inv_perm = _invert_perm(self.perm_premises[top_index])
        if top_slot >= len(inv_perm):
            raise ValueError(
                f"Top slot {top_slot} out of range for top index {top_index} on node {self.node_id}"
            )
        return inv_perm[top_slot]

@dataclass
class RuleDerivation(Node, Generic[Slot]):
    """A node in the proof tree.

    instantiated_rule — fully concrete rule application at this node
    premises          — sub-derivations, one per premise in instantiated_rule.premises
    perm_premises     — permutation applied to each premise before matching slots
                        (identity by default; filled in __post_init__)

    Matching invariant (checked in __post_init__):
        apply_perm(perm_premises[i], instantiated_rule.premises[i])
            == premises[i].instantiated_rule.conclusion
    """
    instantiated_rule: InstantiatedRule[Slot]

    def __post_init__(self) -> None:
        self.instantiated_rule.validate()
        rule_premises = self.instantiated_rule.premises
        if len(self.premises) != len(rule_premises):
            raise ValueError(f"Expected {len(rule_premises)} premises, got {len(self.premises)}")
        if self.perm_premises == ():
            self.perm_premises = tuple(tuple(range(len(t.formulas))) for t in rule_premises)
        if len(self.perm_premises) != len(rule_premises):
            raise ValueError(f"Expected {len(rule_premises)} permutations, got {len(self.perm_premises)}")
        
    def validate(self) -> None:
        rule_premises = self.instantiated_rule.premises
        for i, (prem, rule_prem, perm) in enumerate(zip(self.premises, rule_premises, self.perm_premises)):
            expected = apply_perm(perm, rule_prem)
            if prem.conclusion != expected:
                raise ValueError(
                    f"Premise {i} conclusion {prem.conclusion} "
                    f"does not match required top {expected} "
                    f"(which is {rule_prem} after applying permutation {perm})"
                )

    @property
    def conclusion(self) -> Sequent:
        return self.instantiated_rule.conclusion

    def pretty(self, indent: int = 0) -> str:
        pad = "  " * indent
        lines = [p.pretty(indent + 1) for p in self.premises]
        perms = ""
        if self.perm_premises and any(list(p) != list(range(len(p))) for p in self.perm_premises):
            perms = f" perm={self.perm_premises}"
        lines.append(f"{pad}[{self.instantiated_rule.rule}]{perms} {self.conclusion}")
        return "\n".join(lines)

    def premises_key_slots(self) -> tuple[tuple[int, ...], ...]:
        return self.instantiated_rule.key_slots_premises

    def conclusion_key_slots(self) -> tuple[int, ...]:
        return self.instantiated_rule.key_slots_conclusion

    def non_key_conclusion_to_premise(self) -> dict[int, tuple[int, int]]:
        key_conclusion_slots = set(self.instantiated_rule.key_slots_conclusion)
        key_premise_slots = tuple(set(ks) for ks in self.instantiated_rule.key_slots_premises)

        conclusion_non_keys = [
            i for i in range(len(self.instantiated_rule.conclusion.formulas)) if i not in key_conclusion_slots
        ]
        premise_non_keys: list[tuple[int, int]] = []
        for prem_index, prem in enumerate(self.instantiated_rule.premises):
            premise_non_keys.extend(
                (prem_index, slot_index)
                for slot_index in range(len(prem.formulas))
                if slot_index not in key_premise_slots[prem_index]
            )

        if len(conclusion_non_keys) != len(premise_non_keys):
            raise ValueError(
                f"RuleDerivation {self.node_id} has {len(conclusion_non_keys)} bottom non-key slots "
                f"but {len(premise_non_keys)} top non-key slots"
            )

        return dict(zip(conclusion_non_keys, premise_non_keys))


@dataclass
class BasicBlock(Node):
    label: str

    def __post_init__(self) -> None:
        if len(self.premises) != 1:
            raise ValueError(f"Expected 1 premise, got {len(self.premises)}")
        if len(self.perm_premises) != 1:
            raise ValueError(f"Expected 1 permutation, got {len(self.perm_premises)}")

    @property
    def conclusion(self) -> Sequent:
        return apply_perm(self.perm_premises[0], self.premises[0].conclusion)

    def pretty(self, indent: int = 0) -> str:
        pad = "  " * indent
        perms = ""
        if self.perm_premises and any(list(p) != list(range(len(p))) for p in self.perm_premises):
            perms = f" perm={self.perm_premises}"
        return f"{pad}[BB {self.label}]{perms} {self.conclusion}"

    def premises_key_slots(self) -> tuple[tuple[int, ...], ...]:
        if not self.perm_premises:
            return ()
        return (tuple(range(len(self.perm_premises[0]))),)

    def conclusion_key_slots(self) -> tuple[int, ...]:
        return tuple(range(len(self.conclusion.formulas)))

@dataclass
class Reference(Node):
    label: str
    conclusion: Sequent
    premises = []
    perm_tops = ()

    def __post_init__(self) -> None:
        if len(self.premises) != 0:
            raise ValueError(f"Expected 0 premises, got {len(self.premises)}")
        if len(self.perm_premises) != 0:
            raise ValueError(f"Expected 0 permutations, got {len(self.perm_premises)}")

    def pretty(self, indent: int = 0) -> str:
        pad = "  " * indent
        return f"{pad}[Ref {self.label}] {self.conclusion}"

    def premises_key_slots(self) -> tuple[tuple[int, ...], ...]:
        return ()

    def conclusion_key_slots(self) -> tuple[int, ...]:
        return tuple(range(len(self.conclusion.formulas)))


@dataclass(frozen=True)
class SlotRef:
    """Reference to a key slot on a node boundary.

    premise_index is None for conclusion slots, otherwise it selects which premise sequent.
    """

    node_id: NodeId
    formal_slot: int
    key_slot: int
    premise_index: Optional[int] = None
    
    def is_conclusion(self) -> bool:
        return self.premise_index is None


@dataclass(frozen=True)
class Wire:
    """Collapsed path from one premise key slot through non-key slots to a final conclusion key slot."""

    source: SlotRef # premise slot on the rootward (ancestor) node
    target: SlotRef # conclusion slot on the leafward (descendant) node

def _invert_perm(perm: tuple[int, ...]) -> tuple[int, ...]:
    if sorted(perm) != list(range(len(perm))):
        raise ValueError(f"Not a valid permutation: {perm}")
    inv = [0] * len(perm)
    for out_idx, in_idx in enumerate(perm):
        inv[in_idx] = out_idx
    return tuple(inv)


def _trace_conclusion_slot_to_key_destination(
    node: Node,
    conclusion_slot: int,
    path: set[tuple[NodeId, int]],
) -> SlotRef:
    marker = (node.node_id, conclusion_slot)
    if marker in path:
        raise ValueError(f"Cycle detected while tracing slot path at {marker}")
    path.add(marker)

    conclusion_key_slots = node.conclusion_key_slots()
    if conclusion_slot in conclusion_key_slots:
        return SlotRef(
            node_id=node.node_id,
            formal_slot=conclusion_slot,
            key_slot=conclusion_key_slots.index(conclusion_slot),
            premise_index=None,
        )

    non_key_map = node.non_key_conclusion_to_premise()
    if conclusion_slot not in non_key_map:
        raise ValueError(
            f"Bottom slot {conclusion_slot} on node {node.node_id} is neither key nor pass-through"
        )

    premise_index, premise_slot = non_key_map[conclusion_slot]
    if premise_index >= len(node.premises):
        raise ValueError(
            f"Node {node.node_id} has no premise for top index {premise_index}"
        )
    child = node.premises[premise_index]
    child_conclusion_slot = node.premise_slot_to_child_conclusion_slot(premise_index, premise_slot)
    return _trace_conclusion_slot_to_key_destination(child, child_conclusion_slot, path)


def _collect_nodes(root: Node) -> dict[NodeId, Node]:
    nodes: dict[NodeId, Node] = {}
    stack = [root]
    while stack:
        node = stack.pop()
        if node.node_id in nodes:
            continue
        nodes[node.node_id] = node
        stack.extend(node.premises)
    return nodes


WireMap = dict[NodeId, list[Wire]] # Maps node_id to a list of wires for that node's key slots.

@dataclass(frozen=True)
class BiWireMap:
    """A pair of WireMaps: one for sources, one for targets."""
    source_to_wire: WireMap # Maps node_id to a list of wires for that node's premise key slots.
    target_to_wire: WireMap # Maps node_id to a list of wires for that node's conclusion key slots.

def compute_key_slot_wires(root: Node) -> BiWireMap:
    """Compute a collapsed key-slot flow map for every node in a derivation tree.

    For each node, each key slot on each premise sequent is traced through edge
    permutations and pass-through non-key slots until a key slot on some conclusion
    boundary is reached.
    """

    nodes = _collect_nodes(root)
    source_to_wire: WireMap = defaultdict(list)
    target_to_wire: WireMap = defaultdict(list)

    for node in nodes.values():
        top_keys = node.premises_key_slots()
        wires: list[Wire] = []

        for top_index, key_slots in enumerate(top_keys):
            if top_index >= len(node.premises):
                raise ValueError(
                    f"Node {node.node_id} has key slots for missing top index {top_index}"
                )
            for key_slot_index, top_slot in enumerate(key_slots):
                child_bottom_slot = node.premise_slot_to_child_conclusion_slot(top_index, top_slot)
                destination = _trace_conclusion_slot_to_key_destination(
                    node.premises[top_index],
                    child_bottom_slot,
                    path={(node.node_id, -1)},
                )
                source = SlotRef(
                    node_id=node.node_id,
                    formal_slot=top_slot,
                    key_slot=key_slot_index,
                    premise_index=top_index,
                )
                wire = Wire(source=source, target=destination)
                source_to_wire[node.node_id].append(wire)
                target_to_wire[destination.node_id].append(wire)

    return BiWireMap(source_to_wire=source_to_wire, target_to_wire=target_to_wire)

