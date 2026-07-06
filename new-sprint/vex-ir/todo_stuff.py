ProgramTy = OpaqueScalar("Program")
ProgramTy = OpaquePropType("Program", cartesian=False)

def dirty_call_opcode(arg_types: list, result_ty: PropType) -> OpcodeSignature:
    return make_opcode(OpcodeKind.J_POS_RIGHT, "DirtyCall",
                       [PtrType] + arg_types + [FnTy(result_ty, ProgramTy)], [ProgramTy])

def print_opcode(ty: PropType = None) -> OpcodeSignature:
    return make_opcode(OpcodeKind.J_POS_RIGHT, "Print", [ty or Int64, ProgramTy], [ProgramTy])

def exit_opcode() -> OpcodeSignature:
    return make_opcode(OpcodeKind.J_POS_RIGHT, "Exit", [], [ProgramTy])

def load_opcode(ty: PropType) -> OpcodeSignature:
    return make_opcode(OpcodeKind.J_POS_RIGHT, f"Load[{ty}]",
                       [PtrType, ProgramTy], [ty, ProgramTy])

def store_opcode(ty: PropType) -> OpcodeSignature:
    return make_opcode(OpcodeKind.J_POS_RIGHT, f"Store[{ty}]",
                       [PtrType, ty, ProgramTy], [ProgramTy])

# ═══════════════════════════════════════════════════════════════
# 11.  MEMORY MODEL OVERLAY
# ═══════════════════════════════════════════════════════════════

class MemOrder(Enum):
    READ_FROM       = "rf"
    COHERENCE_ORDER = "co"
    SYNCS_WITH      = "sw"

@dataclass
class MemOrderEdge:
    rel:           MemOrder
    from_ref:      SlotRef
    to_ref:        SlotRef
    pointer_class: str = "default"
    def __repr__(self):
        return f"MemOrd({self.rel.value}: {self.from_ref}→{self.to_ref} [{self.pointer_class}])"


# ═══════════════════════════════════════════════════════════════
# 13.  BUILDER
# ═══════════════════════════════════════════════════════════════

class PBBuilder:
    def __init__(self, name: str, gamma: list = None, delta: list = None):
        self._pb = make_block(name, gamma or [], delta or [])
        self._prog_head = None

    @property
    def pb(self) -> ProofBlock: return self._pb

    def _connect(self, from_node, from_slot, to_node, to_slot) -> Wire:
        prop_ty = from_node.output_type(from_slot)
        w = Wire(from_ref=SlotRef(from_node.id, from_slot),
                 to_ref  =SlotRef(to_node.id,   to_slot),
                 prop_ty =prop_ty)
        from_node.outputs[from_slot].append(w)
        to_node.inputs[to_slot] = w
        return w

    def _add(self, node: Node) -> Node:
        self._pb.add_node(node)
        node.parent = self._pb.def_node.id
        self._pb.def_node.children.append(node.id)
        return node

    def const(self, ty: PropType, value: Any) -> Node:
        return self._add(Node(opcode=const_opcode(ty, value)))

    def binop(self, op: OpcodeSignature, lhs: Node, rhs: Node,
              ls: int = 0, rs: int = 0) -> Node:
        n = self._add(Node(opcode=op))
        self._connect(lhs, ls, n, 0); self._connect(rhs, rs, n, 1); return n

    def promote(self, node: Node, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        n = self._add(Node(opcode=promotion(ty))); self._connect(node, slot, n, 0); return n

    def derelict(self, node: Node, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        assert isinstance(ty, BangPos)
        n = self._add(Node(opcode=dereliction_pos(ty.inner)))
        self._connect(node, slot, n, 0); return n

    def weaken(self, node: Node, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        assert isinstance(ty, BangPos)
        n = self._add(Node(opcode=weakening_pos(ty.inner)))
        self._connect(node, slot, n, 0); return n

    def contract(self, node: Node, n_copies: int = 2, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        assert isinstance(ty, BangPos)
        cn = self._add(Node(opcode=contraction_pos(ty.inner, n_copies)))
        self._connect(node, slot, cn, 0); return cn

    def coerce(self, node: Node, dst_ty: PropType, slot: int = 0) -> Node:
        src_ty = node.output_type(slot)
        n = self._add(Node(opcode=bang_coerce(src_ty, dst_ty)))
        self._connect(node, slot, n, 0); return n

    def construct(self, ty: JPositive, case_idx: int,
                  left_args: list) -> Node:
        op = jpos_right(ty, case_idx)
        n = self._add(Node(opcode=op))
        for i, (src, ss) in enumerate(left_args):
            self._connect(src, ss, n, i)
        return n

    def match(self, ty: JPositive, scrutinee: Node, slot: int = 0) -> Node:
        n = self._add(Node(opcode=jpos_left(ty)))
        self._connect(scrutinee, slot, n, 0); return n

    def exit(self) -> Node:
        n = self._add(Node(opcode=exit_opcode()))
        self._prog_head = n; return n

    def print_val(self, val: Node, val_slot: int = 0) -> Node:
        assert self._prog_head is not None
        n = self._add(Node(opcode=print_opcode(val.output_type(val_slot))))
        self._connect(val, val_slot, n, 0)
        self._connect(self._prog_head, 0, n, 1)
        self._prog_head = n; return n

    def use(self, target_pb: ProofBlock, arg_nodes: list = None) -> Node:
        gamma = target_pb.gamma_types(); delta = target_pb.delta_types()
        op = use_opcode(target_pb.name, gamma, delta)
        n = self._add(Node(opcode=op))
        for i, (src, ss) in enumerate(arg_nodes or []):
            self._connect(src, ss, n, i)
        return n

    def build(self) -> ProofBlock: return self._pb


# ═══════════════════════════════════════════════════════════════
# 14.  OPTIMIZATION PASSES
# ═══════════════════════════════════════════════════════════════

class OptPass:
    name = "base"
    def run(self, pb: ProofBlock) -> int: raise NotImplementedError

class ConstantFolding(OptPass):
    name = "constant_folding"
    _ops = {"add": int.__add__, "sub": int.__sub__,
            "mul": int.__mul__, "idiv": lambda a,b: a//b if b else None}

    def _val(self, n): return n.opcode.params.get("value") if "value" in n.opcode.params else None

    def run(self, pb: ProofBlock) -> int:
        rewrites = 0
        for node in list(pb.nodes.values()):
            op = node.opcode.params.get("arith")
            if op not in self._ops: continue
            lw, rw = node.inputs[0], node.inputs[1]
            if not lw or not rw: continue
            ln = pb.get_node(lw.from_ref.node); rn = pb.get_node(rw.from_ref.node)
            if not ln or not rn: continue
            lv = self._val(ln); rv = self._val(rn)
            if lv is None or rv is None: continue
            result = self._ops[op](lv, rv)
            if result is None: continue
            ty = node.output_type(0)
            inner = ty.inner if isinstance(ty, BangPos) else ty
            folded = pb.add_node(Node(opcode=const_opcode(inner, result)))
            for w in list(node.outputs[0]):
                consumer = pb.get_node(w.to_ref.node)
                if consumer:
                    nw = Wire(from_ref=SlotRef(folded.id, 0), to_ref=w.to_ref, prop_ty=w.prop_ty)
                    folded.outputs[0].append(nw); consumer.inputs[w.to_ref.slot] = nw
            del pb.nodes[node.id]; rewrites += 1
        return rewrites

class CSE(OptPass):
    name = "cse"
    def _key(self, n):
        v = n.opcode.params.get("value")
        if v is not None: return ("const", v, str(n.output_type(0)))
        a = n.opcode.params.get("arith")
        if a:
            lw, rw = n.inputs[0], n.inputs[1]
            if lw and rw: return (a, lw.from_ref.node, lw.from_ref.slot, rw.from_ref.node, rw.from_ref.slot)
        return None

    def run(self, pb: ProofBlock) -> int:
        rewrites, seen = 0, {}
        for node in list(pb.topological_order()):
            key = self._key(node)
            if key is None: continue
            if key in seen:
                can = seen[key]
                for w in list(node.outputs[0]):
                    c = pb.get_node(w.to_ref.node)
                    if c:
                        nw = Wire(from_ref=SlotRef(can.id,0), to_ref=w.to_ref, prop_ty=w.prop_ty)
                        can.outputs[0].append(nw); c.inputs[w.to_ref.slot] = nw
                del pb.nodes[node.id]; rewrites += 1
            else: seen[key] = node
        return rewrites

class DCE(OptPass):
    name = "dce"
    def run(self, pb: ProofBlock) -> int:
        rewrites, changed = 0, True
        while changed:
            changed = False
            for nid, node in list(pb.nodes.items()):
                if nid == pb.def_node.id: continue
                if any(t == ProgramTy for t in node.opcode.output_types()): continue
                if node.opcode.kind in (OpcodeKind.J_NEG_LEFT, OpcodeKind.USE, OpcodeKind.DEF): continue
                if all(len(outs) == 0 for outs in node.outputs):
                    del pb.nodes[nid]; rewrites += 1; changed = True
        return rewrites

class StructuralFoldOptimizer:
    def __init__(self): self.passes = [ConstantFolding(), CSE(), DCE()]
    def optimize_pb(self, pb: ProofBlock) -> dict:
        totals, changed = {}, True
        while changed:
            changed = False
            for p in self.passes:
                n = p.run(pb); totals[p.name] = totals.get(p.name,0) + n
                if n > 0: changed = True
        return totals
    def optimize_program(self, prog: StroscotProgram) -> dict:
        totals = {}
        for pb in prog.proof_blocks.values():
            for k, v in self.optimize_pb(pb).items():
                totals[k] = totals.get(k,0) + v
        return totals


@dataclass
class StroscotProgram:
    proof_blocks:    dict = field(default_factory=dict)   # dict[NodeId, ProofBlock]
    mem_order_edges: list = field(default_factory=list)
    entry:           Optional[NodeId] = None

    def add_block(self, pb: ProofBlock) -> ProofBlock:
        self.proof_blocks[pb.id] = pb; return pb

    def successors(self, pb_id: NodeId) -> list:
        pb = self.proof_blocks.get(pb_id)
        if not pb: return []
        return [n.opcode.params["block_id"] for n in pb.use_leaves()]

    def __repr__(self):
        return f"StroscotProgram({len(self.proof_blocks)} blocks)"

# ═══════════════════════════════════════════════════════════════
# 13.  BUILDER
# ═══════════════════════════════════════════════════════════════

class PBBuilder:
    def __init__(self, name: str, gamma: list = None, delta: list = None):
        self._pb = make_block(name, gamma or [], delta or [])
        self._prog_head = None

    @property
    def pb(self) -> ProofBlock: return self._pb

    def _connect(self, from_node, from_slot, to_node, to_slot) -> Wire:
        prop_ty = from_node.output_type(from_slot)
        w = Wire(from_ref=SlotRef(from_node.id, from_slot),
                 to_ref  =SlotRef(to_node.id,   to_slot),
                 prop_ty =prop_ty)
        from_node.outputs[from_slot].append(w)
        to_node.inputs[to_slot] = w
        return w

    def _add(self, node: Node) -> Node:
        self._pb.add_node(node)
        node.parent = self._pb.def_node.id
        self._pb.def_node.children.append(node.id)
        return node

    def const(self, ty: PropType, value: Any) -> Node:
        return self._add(Node(opcode=const_opcode(ty, value)))

    def binop(self, op: OpcodeSignature, lhs: Node, rhs: Node,
              ls: int = 0, rs: int = 0) -> Node:
        n = self._add(Node(opcode=op))
        self._connect(lhs, ls, n, 0); self._connect(rhs, rs, n, 1); return n

    def promote(self, node: Node, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        n = self._add(Node(opcode=promotion(ty))); self._connect(node, slot, n, 0); return n

    def derelict(self, node: Node, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        assert isinstance(ty, BangPos)
        n = self._add(Node(opcode=dereliction_pos(ty.inner)))
        self._connect(node, slot, n, 0); return n

    def weaken(self, node: Node, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        assert isinstance(ty, BangPos)
        n = self._add(Node(opcode=weakening_pos(ty.inner)))
        self._connect(node, slot, n, 0); return n

    def contract(self, node: Node, n_copies: int = 2, slot: int = 0) -> Node:
        ty = node.output_type(slot)
        assert isinstance(ty, BangPos)
        cn = self._add(Node(opcode=contraction_pos(ty.inner, n_copies)))
        self._connect(node, slot, cn, 0); return cn

    def coerce(self, node: Node, dst_ty: PropType, slot: int = 0) -> Node:
        src_ty = node.output_type(slot)
        n = self._add(Node(opcode=bang_coerce(src_ty, dst_ty)))
        self._connect(node, slot, n, 0); return n

    def construct(self, ty: JPositive, case_idx: int,
                  left_args: list) -> Node:
        op = jpos_right(ty, case_idx)
        n = self._add(Node(opcode=op))
        for i, (src, ss) in enumerate(left_args):
            self._connect(src, ss, n, i)
        return n

    def match(self, ty: JPositive, scrutinee: Node, slot: int = 0) -> Node:
        n = self._add(Node(opcode=jpos_left(ty)))
        self._connect(scrutinee, slot, n, 0); return n

    def exit(self) -> Node:
        n = self._add(Node(opcode=exit_opcode()))
        self._prog_head = n; return n

    def print_val(self, val: Node, val_slot: int = 0) -> Node:
        assert self._prog_head is not None
        n = self._add(Node(opcode=print_opcode(val.output_type(val_slot))))
        self._connect(val, val_slot, n, 0)
        self._connect(self._prog_head, 0, n, 1)
        self._prog_head = n; return n

    def use(self, target_pb: ProofBlock, arg_nodes: list = None) -> Node:
        gamma = target_pb.gamma_types(); delta = target_pb.delta_types()
        op = use_opcode(target_pb.name, gamma, delta)
        n = self._add(Node(opcode=op))
        for i, (src, ss) in enumerate(arg_nodes or []):
            self._connect(src, ss, n, i)
        return n

    def build(self) -> ProofBlock: return self._pb


# ═══════════════════════════════════════════════════════════════
# 14.  OPTIMIZATION PASSES
# ═══════════════════════════════════════════════════════════════

class OptPass:
    name = "base"
    def run(self, pb: ProofBlock) -> int: raise NotImplementedError

class ConstantFolding(OptPass):
    name = "constant_folding"
    _ops = {"add": int.__add__, "sub": int.__sub__,
            "mul": int.__mul__, "idiv": lambda a,b: a//b if b else None}

    def _val(self, n): return n.opcode.params.get("value") if "value" in n.opcode.params else None

    def run(self, pb: ProofBlock) -> int:
        rewrites = 0
        for node in list(pb.nodes.values()):
            op = node.opcode.params.get("arith")
            if op not in self._ops: continue
            lw, rw = node.inputs[0], node.inputs[1]
            if not lw or not rw: continue
            ln = pb.get_node(lw.from_ref.node); rn = pb.get_node(rw.from_ref.node)
            if not ln or not rn: continue
            lv = self._val(ln); rv = self._val(rn)
            if lv is None or rv is None: continue
            result = self._ops[op](lv, rv)
            if result is None: continue
            ty = node.output_type(0)
            inner = ty.inner if isinstance(ty, BangPos) else ty
            folded = pb.add_node(Node(opcode=const_opcode(inner, result)))
            for w in list(node.outputs[0]):
                consumer = pb.get_node(w.to_ref.node)
                if consumer:
                    nw = Wire(from_ref=SlotRef(folded.id, 0), to_ref=w.to_ref, prop_ty=w.prop_ty)
                    folded.outputs[0].append(nw); consumer.inputs[w.to_ref.slot] = nw
            del pb.nodes[node.id]; rewrites += 1
        return rewrites

class CSE(OptPass):
    name = "cse"
    def _key(self, n):
        v = n.opcode.params.get("value")
        if v is not None: return ("const", v, str(n.output_type(0)))
        a = n.opcode.params.get("arith")
        if a:
            lw, rw = n.inputs[0], n.inputs[1]
            if lw and rw: return (a, lw.from_ref.node, lw.from_ref.slot, rw.from_ref.node, rw.from_ref.slot)
        return None

    def run(self, pb: ProofBlock) -> int:
        rewrites, seen = 0, {}
        for node in list(pb.topological_order()):
            key = self._key(node)
            if key is None: continue
            if key in seen:
                can = seen[key]
                for w in list(node.outputs[0]):
                    c = pb.get_node(w.to_ref.node)
                    if c:
                        nw = Wire(from_ref=SlotRef(can.id,0), to_ref=w.to_ref, prop_ty=w.prop_ty)
                        can.outputs[0].append(nw); c.inputs[w.to_ref.slot] = nw
                del pb.nodes[node.id]; rewrites += 1
            else: seen[key] = node
        return rewrites

class DCE(OptPass):
    name = "dce"
    def run(self, pb: ProofBlock) -> int:
        rewrites, changed = 0, True
        while changed:
            changed = False
            for nid, node in list(pb.nodes.items()):
                if nid == pb.def_node.id: continue
                if any(t == ProgramTy for t in node.opcode.output_types()): continue
                if node.opcode.kind in (OpcodeKind.J_NEG_LEFT, OpcodeKind.USE, OpcodeKind.DEF): continue
                if all(len(outs) == 0 for outs in node.outputs):
                    del pb.nodes[nid]; rewrites += 1; changed = True
        return rewrites

class StructuralFoldOptimizer:
    def __init__(self): self.passes = [ConstantFolding(), CSE(), DCE()]
    def optimize_pb(self, pb: ProofBlock) -> dict:
        totals, changed = {}, True
        while changed:
            changed = False
            for p in self.passes:
                n = p.run(pb); totals[p.name] = totals.get(p.name,0) + n
                if n > 0: changed = True
        return totals
    def optimize_program(self, prog: StroscotProgram) -> dict:
        totals = {}
        for pb in prog.proof_blocks.values():
            for k, v in self.optimize_pb(pb).items():
                totals[k] = totals.get(k,0) + v
        return totals


@dataclass(frozen=True)
class ProgramType(PropType):
    """Linear effect-tree type. Flows upward to root. Never destructed inside IR."""
    def __repr__(self): return "Program"

@dataclass(frozen=True)
class FnPtrType(PropType):
    def __repr__(self): return "FnPtr"


DIRTY_CALL = OpcodeSignature(
    kind=OpcodeKind.J_POS_RIGHT, name="DirtyCall",
    inputs=[FnPtrType(), BangPositive(IntType(64)),
            JNegative("k", (IntType(64),), (ProgramType(),))],
    outputs=[ProgramType()], key_slots=[0])

PRINT_OP = OpcodeSignature(
    kind=OpcodeKind.J_POS_RIGHT, name="Print",
    inputs=[BangPositive(IntType(64)), ProgramType()],
    outputs=[ProgramType()], key_slots=[0])

EXIT_OP = OpcodeSignature(
    kind=OpcodeKind.J_POS_RIGHT, name="Exit",
    inputs=[], outputs=[ProgramType()], key_slots=[])

# ═══════════════════════════════════════════════════════════════
# 7.  MEMORY MODEL OVERLAY
# ═══════════════════════════════════════════════════════════════

class MemOrder(Enum):
    READ_FROM       = "rf"   # read observed this write
    COHERENCE_ORDER = "co"   # write-write modification order
    SYNCS_WITH      = "sw"   # release/acquire pairing

@dataclass
class MemOrderEdge:
    """
    Layer-2 cross-thread ordering annotation.
    Not carried on typed wires — a separate graph overlay.
    Derived relations: fr = rf⁻¹;co   hb = tc(sw ∪ po)
    """
    rel:           MemOrder
    from_ref:      SlotRef
    to_ref:        SlotRef
    pointer_class: str = "default"

    def __repr__(self):
        return (f"MemOrd({self.rel.value}: "
                f"{self.from_ref.node[:6]} → {self.to_ref.node[:6]} "
                f"[{self.pointer_class}])")


# ═══════════════════════════════════════════════════════════════
# 8.  STROSCOT PROGRAM
# ═══════════════════════════════════════════════════════════════

@dataclass
class StroscotProgram:
    proof_blocks:    dict[NodeId, ProofBlock] = field(default_factory=dict)
    block_edges:     list[BlockEdge]          = field(default_factory=list)
    mem_order_edges: list[MemOrderEdge]       = field(default_factory=list)
    entry_pb:        Optional[NodeId]         = None

    def add_block(self, pb: ProofBlock) -> ProofBlock:
        self.proof_blocks[pb.id] = pb
        return pb

    def add_edge(self, be: BlockEdge) -> BlockEdge:
        self.block_edges.append(be)
        return be

    def successors(self, pb_id: NodeId) -> list[NodeId]:
        return [e.target_pb for e in self.block_edges if e.source_pb == pb_id]

    def predecessors(self, pb_id: NodeId) -> list[NodeId]:
        return [e.source_pb for e in self.block_edges if e.target_pb == pb_id]

    def __repr__(self):
        return (f"StroscotProgram({len(self.proof_blocks)} blocks, "
                f"{len(self.block_edges)} edges)")


# ═══════════════════════════════════════════════════════════════
# 9.  BUILDER API
# ═══════════════════════════════════════════════════════════════

class PBBuilder:
    def __init__(self, name: str = "", gamma: list[PropType] = None, delta: list[PropType] = None):
        self.pb = ProofBlock(name=name, gamma_types=gamma or [], delta_types=delta or [])

    def wire(self, from_node: Node, from_slot: int, to_node: Node, to_slot: int) -> Wire:
        prop_ty = from_node.output_type(from_slot)
        w = Wire(from_ref=SlotRef(from_node.id, from_slot, Side.RIGHT),
                 to_ref  =SlotRef(to_node.id,   to_slot,   Side.LEFT),
                 prop_ty =prop_ty)
        from_node.outputs[from_slot].append(w)
        to_node.inputs[to_slot] = w
        return w

    def const_int(self, value: int) -> Node:
        n = Node(opcode=CONST_INT, const_value=value)
        return self.pb.add_node(n)

    def binop(self, op: OpcodeSignature, lhs: Node, rhs: Node) -> Node:
        n = Node(opcode=op)
        self.pb.add_node(n)
        self.wire(lhs, 0, n, 0)
        self.wire(rhs, 0, n, 1)
        return n

    def branch(self, cond: Node) -> Node:
        n = Node(opcode=BRANCH)
        self.pb.add_node(n)
        self.wire(cond, 0, n, 0)
        return n

    def exit_node(self) -> Node:
        return self.pb.add_node(Node(opcode=EXIT_OP))

    def print_node(self, val: Node, cont: Node) -> Node:
        n = Node(opcode=PRINT_OP)
        self.pb.add_node(n)
        self.wire(val,  0, n, 0)
        self.wire(cont, 0, n, 1)
        return n

    def build(self) -> ProofBlock:
        return self.pb


# ═══════════════════════════════════════════════════════════════
# 10.  OPTIMIZATION PASSES  (Structural Fold Optimization)
# ═══════════════════════════════════════════════════════════════

class OptimizationPass:
    name = "base"
    def run(self, pb: ProofBlock) -> int:
        raise NotImplementedError


class ConstantFolding(OptimizationPass):
    name = "constant_folding"
    _ops = {"add": lambda a,b: a+b, "sub": lambda a,b: a-b,
            "mul": lambda a,b: a*b, "idiv": lambda a,b: a//b if b else None}

    def _const(self, node: Node) -> Optional[int]:
        return node.const_value if node.opcode.name == "const_int" else None

    def run(self, pb: ProofBlock) -> int:
        rewrites = 0
        for node in list(pb.nodes.values()):
            if node.opcode.name not in self._ops:
                continue
            lw, rw = node.inputs[0], node.inputs[1]
            if not lw or not rw:
                continue
            ln = pb.get_node(lw.from_ref.node)
            rn = pb.get_node(rw.from_ref.node)
            if not ln or not rn:
                continue
            lv, rv = self._const(ln), self._const(rn)
            if lv is None or rv is None:
                continue
            result = self._ops[node.opcode.name](lv, rv)
            if result is None:
                continue
            folded = pb.add_node(Node(opcode=CONST_INT, const_value=result))
            for out_wire in list(node.outputs[0]):
                consumer = pb.get_node(out_wire.to_ref.node)
                if consumer:
                    nw = Wire(from_ref=SlotRef(folded.id, 0, Side.RIGHT),
                              to_ref=out_wire.to_ref, prop_ty=out_wire.prop_ty)
                    folded.outputs[0].append(nw)
                    consumer.inputs[out_wire.to_ref.slot] = nw
            del pb.nodes[node.id]
            rewrites += 1
        return rewrites


class CommonSubexpressionElimination(OptimizationPass):
    name = "cse"

    def _key(self, node: Node) -> Optional[tuple]:
        if node.opcode.name == "const_int":
            return ("const_int", node.const_value)
        if node.opcode.name in ("add","sub","mul","idiv"):
            lw, rw = node.inputs[0], node.inputs[1]
            if lw and rw:
                return (node.opcode.name, lw.from_ref.node, rw.from_ref.node)
        return None

    def run(self, pb: ProofBlock) -> int:
        rewrites, seen = 0, {}
        for node in list(pb.topological_order()):
            key = self._key(node)
            if key is None:
                continue
            if key in seen:
                canonical = seen[key]
                for out_wire in list(node.outputs[0]):
                    consumer = pb.get_node(out_wire.to_ref.node)
                    if consumer:
                        nw = Wire(from_ref=SlotRef(canonical.id, 0, Side.RIGHT),
                                  to_ref=out_wire.to_ref, prop_ty=out_wire.prop_ty)
                        canonical.outputs[0].append(nw)
                        consumer.inputs[out_wire.to_ref.slot] = nw
                del pb.nodes[node.id]
                rewrites += 1
            else:
                seen[key] = node
        return rewrites


class DeadCodeElimination(OptimizationPass):
    name = "dce"

    def run(self, pb: ProofBlock) -> int:
        rewrites, changed = 0, True
        while changed:
            changed = False
            for nid, node in list(pb.nodes.items()):
                if any(isinstance(t, ProgramType) for t in node.opcode.outputs):
                    continue
                if node.opcode.kind == OpcodeKind.J_NEG_LEFT:
                    continue
                if all(len(outs) == 0 for outs in node.outputs):
                    del pb.nodes[nid]
                    rewrites += 1
                    changed = True
        return rewrites


class StructuralFoldOptimizer:
    """Fixed-point driver for all intra-PB cut-elimination passes."""
    def __init__(self):
        self.passes = [ConstantFolding(), CommonSubexpressionElimination(), DeadCodeElimination()]

    def optimize_pb(self, pb: ProofBlock) -> dict[str, int]:
        totals: dict[str, int] = {}
        changed = True
        while changed:
            changed = False
            for p in self.passes:
                n = p.run(pb)
                totals[p.name] = totals.get(p.name, 0) + n
                if n > 0:
                    changed = True
        return totals

    def optimize_program(self, prog: StroscotProgram) -> dict[str, int]:
        totals: dict[str, int] = {}
        for pb in prog.proof_blocks.values():
            for k, v in self.optimize_pb(pb).items():
                totals[k] = totals.get(k, 0) + v
        return totals


# ═══════════════════════════════════════════════════════════════
# 11.  INSTRUCTION REORDER  (Program-wire swap, §IX)
# ═══════════════════════════════════════════════════════════════

class InstructionReorder:
    @staticmethod
    def can_reorder(a: Node, b: Node, mem_edges: list[MemOrderEdge]) -> bool:
        """True iff no Layer-2 MemOrderEdge exists between a and b."""
        a_ids = {a.id}
        b_ids = {b.id}
        for e in mem_edges:
            if (e.from_ref.node in a_ids and e.to_ref.node in b_ids) or \
               (e.from_ref.node in b_ids and e.to_ref.node in a_ids):
                return False
        return True
