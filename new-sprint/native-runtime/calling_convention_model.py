"""
Calling Convention Predicate Model and Location-Effect Typology

Revised to match the "LocationEffects" + four slot kinds model described in
Calling-conventions.md.

Core ideas:
- A callee is modeled as a state transformer f: S_in -> S_out over an abstract
  machine state S (registers, stack, flags, etc.).
- Each abstract location has a "location effect" at the call boundary, chosen
  from 4 possibilities based on entry-dependence and value-preservation:
    - IndepPreserved   (IP)
    - DepPreserved     (DP)
    - IndepClobbered   (IC)
    - DepClobbered     (DC)
- On top of these effects, we define 4 kinds of slots describing how logical
  arguments and return values map to locations:
    - Argument slots (entry-dependent inputs)
    - Convention slots (entry-dependent, structurally determined inputs)
    - Return slots (clobbering outputs consumed by the caller)
    - Scratch slots (clobbering outputs ignored by the caller)

This file provides only data structures and helpers; it does not implement
analysis. An analyzer can populate these objects; a trampoline/JIT layer can
consume them.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Dict, List, Optional, Sequence


# --- Location and LocationEffects -----------------------------------------------------------

class LocationEffect(Enum):
    """Effect of a location at the call boundary.

    The classification is the Cartesian product:
    - Entry dependence: Dep vs Indep
    - Output effect: Preserved vs Clobbered

    Abbreviations:
    - IP: IndepPreserved
    - DP: DepPreserved
    - IC: IndepClobbered
    - DC: DepClobbered
    """

    INDEP_PRESERVED = auto()  # IP: value passes through, block doesn't care
    DEP_PRESERVED = auto()    # DP: block reads value, returns it intact
    INDEP_CLOBBERED = auto()  # IC: block writes new value, ignores old
    DEP_CLOBBERED = auto()    # DC: block reads then overwrites in-place

@dataclass(frozen=True)
class BitRange:
    """Inclusive half-open bit range [start, end).

    Used to model partial-register or SIMD sub-lanes.
    """

    start: int
    end: int

    def width(self) -> int:
        return self.end - self.start

@dataclass(frozen=True)
class Location:
    """Abstract machine location.

    - name: human-readable identifier (e.g., "RAX", "XMM0", "[SP+8]").
    - kind: category ("gpr", "simd", "stack", "flag", "segment", "fpu", ...).
    - subrange: optional BitRange for partial-register effects.
    """

    name: str
    kind: str
    subrange: Optional[BitRange] = None

    def __str__(self) -> str:
        if self.subrange is None:
            return self.name
        return f"{self.name}[{self.subrange.start}:{self.subrange.end}]"

@dataclass
class LocationEffectEntry:
    """Location paired with its effect classification."""

    location: Location
    effect: LocationEffect

@dataclass
class LocationEffects:
    """Aggregate classification over all relevant locations.

    This corresponds to the "LocationEffects" object in the prose document.
    """

    entries: List[LocationEffectEntry] = field(default_factory=list)

    def add(self, location: Location, effect: LocationEffect) -> None:
        self.entries.append(LocationEffectEntry(location=location, effect=effect))

    def by_effect(self, effect: LocationEffect) -> List[LocationEffectEntry]:
        return [e for e in self.entries if e.effect is effect]

    def get(self, location: Location) -> Optional[LocationEffect]:
        for e in self.entries:
            if e.location == location:
                return e.effect
        return None

# --- Slot kinds: arguments, convention, returns, scratch (SlotSet) ------------------------

class SlotKind(Enum):
    """Logical role of a slot in the calling convention.

    - ARGUMENT: entry-dependent input data supplied by the caller.
    - CONVENTION: entry-dependent but structurally determined input
      (e.g., stack pointer, DF=0, SysV variadic AL, segment bases).
    - RETURN: clobbering output that the caller will read as a result.
    - SCRATCH: clobbering output that the caller ignores (caller-saved).
    """

    ARGUMENT = auto()
    CONVENTION = auto()
    RETURN = auto()
    SCRATCH = auto()

@dataclass
class Slot:
    """A logical slot mapping to one or more locations.

    index: logical ordering index within its category (arg0, arg1, ret0, ...).
    kind: one of SlotKind.
    locations: one or more Locations participating in the slot.
    name: optional human-readable identifier.

    The caller/trampoline can walk slots ordered by (kind, index) or just by
    index within each kind, depending on the marshalling policy.
    """

    index: int
    kind: SlotKind
    locations: List[Location]
    name: Optional[str] = None

@dataclass
class SlotSet:
    """Collection of all slots for a calling convention.

    This provides convenience filters by SlotKind.
    """

    slots: List[Slot] = field(default_factory=list)

    def add_slot(
        self,
        index: int,
        kind: SlotKind,
        locations: Sequence[Location],
        name: Optional[str] = None,
    ) -> Slot:
        slot = Slot(index=index, kind=kind, locations=list(locations), name=name)
        self.slots.append(slot)
        return slot

    def of_kind(self, kind: SlotKind) -> List[Slot]:
        return [s for s in self.slots if s.kind is kind]

    def arguments(self) -> List[Slot]:
        return self.of_kind(SlotKind.ARGUMENT)

    def conventions(self) -> List[Slot]:
        return self.of_kind(SlotKind.CONVENTION)

    def returns(self) -> List[Slot]:
        return self.of_kind(SlotKind.RETURN)

    def scratches(self) -> List[Slot]:
        return self.of_kind(SlotKind.SCRATCH)

# --- Additional invariants and constraints ------------------------------------------------

# --- Stack and frame invariants -------------------------------------------------

@dataclass
class StackDeltaInvariant:
    """Describes how the stack pointer changes across the call.

    Typically the stack pointer location itself will have a DepClobbered effect
    (the callee reads it to find its frame and writes a new value in prologue
    and epilogue), but we may detect an additional invariant:
        SP_out = SP_in + fixed_delta
    when fixed_delta is not None and may_vary is False.
    """

    sp_location: Location
    fixed_delta: Optional[int] = None

@dataclass
class StackRegionPolicy:
    """Semantic partitioning of memory relative to SP_in / CFA.

    Captures red zones, shadow space, and basic caller/callee ownership.
    """

    has_red_zone: bool = False
    red_zone_size: int = 0

    has_shadow_space: bool = False
    shadow_space_size: int = 0

@dataclass
class CFAInvariant:
    """Canonical Frame Address invariant for unwinding.

    - cfa_register: typically the caller's SP at call-site.
    - expression: opaque representation of a DWARF-like CFA expression.
    """

    cfa_register: Location
    expression: Optional[Any] = None

# --- Relational predicates and architectural invariants ------------------------

@dataclass
class HardwareControlFlowConstraint:
    has_ibt_endbr: bool = False

@dataclass
class HardwareStackInvariant:
    """Invariants for non-memory hardware stacks (e.g., x87 FPU stack)."""

    stack_location: Location
    depth_delta: int = 0

@dataclass
class FrameTransferContract:
    """Description of tail-call / frame-transfer behavior.

    When enabled, the callee may transfer ownership of the return address and
    part or all of the caller's frame to another block instead of returning
    normally.
    """

    enabled: bool = False
    tail_callee_id: Optional[str] = None

@dataclass
class HeapUsageSummary:
    """Coarse-grained heap usage relevant to the calling contract."""

    pointer_args: List[Location] = field(default_factory=list)
    may_allocate: bool = False


# --- Top-level CallingConvention model -----------------------------------------
@dataclass
class CallingConvention:
    """Unified description of a callee's calling contract.

    - location_effects: effect classification of each relevant location.
    - slots: argument/convention/return/scratch slots with stable ordering.
    - stack_delta / stack_regions: stack-pointer and ownership semantics.
    - cfa: unwinding metadata.
    - relational_preds / hw_cf_constraints / hidden_state / hw_stacks /
      frame_transfer / heap_usage: additional architectural constraints.
    """

    name: str

    location_effects: LocationEffects = field(default_factory=LocationEffects)
    slots: SlotSet = field(default_factory=SlotSet)

    stack_delta: Optional[StackDeltaInvariant] = None
    stack_regions: StackRegionPolicy = field(default_factory=StackRegionPolicy)

    cfa: Optional[CFAInvariant] = None

    hw_cf_constraints: HardwareControlFlowConstraint = field(
        default_factory=HardwareControlFlowConstraint
    )
    hw_stacks: List[HardwareStackInvariant] = field(default_factory=list)
    frame_transfer: FrameTransferContract = field(default_factory=FrameTransferContract)
    heap_usage: HeapUsageSummary = field(default_factory=HeapUsageSummary)

    # --- Convenience helpers ----------------------------------------------------

    def add_location(
        self,
        name: str,
        kind: str,
        effect: LocationEffect,
        subrange: Optional[BitRange] = None,
    ) -> Location:
        loc = Location(name=name, kind=kind, subrange=subrange)
        self.location_effects.add(loc, effect)
        return loc

    def add_slot(
        self,
        index: int,
        kind: SlotKind,
        locations: Sequence[Location],
        name: Optional[str] = None,
    ) -> Slot:
        return self.slots.add_slot(index=index, kind=kind, locations=locations, name=name)
    
    def print_summary(self) -> None:
        print(f"CallingConvention: {self.name}")
        print("Location Effects:")
        for entry in self.location_effects.entries:
            print(f"  {entry.location} : {entry.effect.name}")
        print("Slots:")
        sorted_slots = sorted(self.slots.slots, key=lambda s: (s.kind.value, s.index))
        for slot in sorted_slots:
            loc_names = ', '.join(str(loc) for loc in slot.locations)
            print(f"  [{slot.kind.name}] index={slot.index}, locations=[{loc_names}], name={slot.name}")
        if self.stack_delta:
            print(f"Stack Delta: SP location={self.stack_delta.sp_location}, fixed_delta={self.stack_delta.fixed_delta}")
        if self.stack_regions.has_red_zone:
            print(f"Red Zone: size={self.stack_regions.red_zone_size}")
        if self.stack_regions.has_shadow_space:
            print(f"Shadow Space: size={self.stack_regions.shadow_space_size}")
        if self.cfa:
            print(f"CFA: register={self.cfa.cfa_register}, expression={self.cfa.expression}")
        print(f"Hardware Control Flow Constraints: has_ibt_endbr={self.hw_cf_constraints.has_ibt_endbr}")
        if self.hw_stacks:
            for hw_stack in self.hw_stacks:
                print(f"Hardware Stack: location={hw_stack.stack_location}, depth_delta={hw_stack.depth_delta}")
        print(f"Frame Transfer: enabled={self.frame_transfer.enabled}, tail_callee_id={self.frame_transfer.tail_callee_id}")
        if self.heap_usage.pointer_args:
            pointer_arg_names = ', '.join(str(loc) for loc in self.heap_usage.pointer_args)
            print(f"Heap Usage: pointer_args=[{pointer_arg_names}], may_allocate={self.heap_usage.may_allocate}")

# --- Example builder: partial SysV AMD64 model ---------------------------------


def build_sysv_amd64_example() -> CallingConvention:
    """Illustrative example of a calling convention for a symbol with a System V AMD64 ABI.

    Shows how to populate the new LocationEffect + Slot model.
    """

    cc = CallingConvention(name="sysv_amd64_example")

    # Integer argument registers: RDI, RSI, RDX, RCX, R8, R9
    int_arg_regs = [
        cc.add_location("RDI", "gpr", LocationEffect.DEP_PRESERVED),
        cc.add_location("RSI", "gpr", LocationEffect.DEP_PRESERVED),
        cc.add_location("RDX", "gpr", LocationEffect.DEP_PRESERVED),
        cc.add_location("RCX", "gpr", LocationEffect.DEP_PRESERVED),
        cc.add_location("R8", "gpr", LocationEffect.DEP_PRESERVED),
        cc.add_location("R9", "gpr", LocationEffect.DEP_PRESERVED),
    ]

    # Return value in RAX: block overwrites RAX with result, independent of its
    # previous value.
    rax = cc.add_location("RAX", "gpr", LocationEffect.INDEP_CLOBBERED)

    # Stack pointer: typically DepClobbered (uses entry SP to find frame,
    # then writes new SP). Here we assume SP_out == SP_in (boundary view).
    rsp = cc.add_location("RSP", "sp", LocationEffect.DEP_CLOBBERED)
    cc.stack_delta = StackDeltaInvariant(sp_location=rsp, fixed_delta=0)

    # Red zone of 128 bytes below RSP.
    cc.stack_regions.has_red_zone = True
    cc.stack_regions.red_zone_size = 128

    # Argument slots map logical args to the integer arg registers.
    for i, reg in enumerate(int_arg_regs):
        cc.add_slot(index=i, kind=SlotKind.ARGUMENT, locations=[reg], name=f"arg{i}")

    # Return slot for the primary scalar return in RAX.
    cc.add_slot(index=0, kind=SlotKind.RETURN, locations=[rax], name="retval")

    # SysV variadic AL convention slot: AL is a DepPreserved or DepClobbered
    # location depending on how you model it; we mark it DepPreserved here and
    # encode the actual semantic relation via a RelationalPredicate.
    al = cc.add_location("AL", "gpr", LocationEffect.DEP_PRESERVED)
    cc.add_slot(index=0, kind=SlotKind.CONVENTION, locations=[al], name="variadic_al")
    # Direction flag DF: modeled as preserved with convention-specified input DF_in = 0.
    df = cc.add_location("DF", "flag", LocationEffect.DEP_PRESERVED)
    cc.add_slot(index=0, kind=SlotKind.CONVENTION, locations=[df], name="direction_flag")

    return cc


if __name__ == "__main__":
    example = build_sysv_amd64_example()
    example.print_summary()