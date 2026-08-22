from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Optional
import pyvex
import vex_ir as tv

class UnsupportedVEX(ValueError): pass

T={"Ity_I1":tv.Bool,"Ity_I8":tv.Int8,"Ity_I16":tv.Int16,"Ity_I32":tv.Int32,"Ity_I64":tv.Int64,"Ity_F32":tv.Float32,"Ity_F64":tv.Float64,"Ity_V128":tv.Vec128,"Ity_V256":tv.Vec256}
def tag(x): return getattr(x,"tag",type(x).__name__)

class VexTranslator:
    """Translate a pyvex IRSB to your vex_ir operations plus an SSA value graph.

    `vex_ir.py` supplies rule/type objects, not a proof-term AST. LoweredBlock is
    the small missing carrier: values are outputs of your Const/FlatOperation
    rules; effects explicitly thread GuestState and Memory tokens.
    """
    def __init__(self): self.reset()
    def reset(self):
        self.n=0
        self.values=[]
        self.binds=[]
        self.effects=[]
        self.events=[]
        self.temps={}
        self.tyenv=()
        self.source_tyenv=None
    def ty(self,x):
        try:return T[str(x)]
        except KeyError as e: raise UnsupportedVEX(f"No vex_ir FlatType for {x}") from e
    def i32(self,x): return self.const(tv.Int32,int(x))
    def end(self,x): return self.const(tv.IREndness,str(x))
    def result_ty(self,e):
        for name in ("ty","retty"):
            if hasattr(e,name): return self.ty(getattr(e,name))
        try:return self.ty(e.result_type(self.source_tyenv))
        except Exception as err: raise UnsupportedVEX(f"Cannot determine result type of {e!r}") from err
    def expr(self,e):
        k=tag(e)
        if k=="Iex_Const": return self.con(e.con)
        if k=="Iex_RdTmp":
            if e.tmp not in self.temps: raise UnsupportedVEX(f"Read before write: t{e.tmp}")
            return self.temps[e.tmp]
        if k=="Iex_Get": return self.apply(tv.Get(self.ty(e.ty)),self.guest,self.i32(e.offset))
        if k=="Iex_GetI": return self.apply(tv.GetI(self.ty(e.ty)),self.guest,self.const(tv.IRRegArray,e.descr),self.expr(e.ix),self.i32(e.bias))
        if k=="Iex_Load": return self.apply(tv.Load(self.ty(e.ty)),self.memory,self.expr(e.addr),self.end(e.end))
        if k=="Iex_ITE": return self.apply(tv.ITE(self.result_ty(e)),self.expr(e.cond),self.expr(e.iftrue),self.expr(e.iffalse))
        if k in {"Iex_Unop","Iex_Binop","Iex_Triop","Iex_Qop"}:
            a=tuple(self.expr(x) for x in e.args); return self.apply(tv.IrOp(str(e.op),tuple(x.ty for x in a),self.result_ty(e)),*a)
        if k=="Iex_CCall":
            a=tuple(self.expr(x) for x in e.args); return self.apply(tv.PureCall(tuple(x.ty for x in a),self.ty(e.retty)),self.const(tv.IRCallee,e.callee),*a)
        raise UnsupportedVEX(f"Unsupported expression {k}")
    def con(self,c):
        ty=self.ty(c.type); v=c.value; k=tag(c)
        rules={"Ico_F32i":tv.Const_F32i,"Ico_F64i":tv.Const_F64i,"Ico_V128":tv.Const_V128,"Ico_V256":tv.Const_V256}
        return self.const(ty,bool(v) if ty is tv.Bool else v,rules[k](int(v)) if k in rules else None)
    def stmt(self,s):
        k=tag(s)
        if k=="Ist_NoOp": self.events.append(ProgramEvent(tv.IRStmt_NoOp(),(),s)); return None
        if k=="Ist_IMark": self.events.append(ProgramEvent(tv.IRStmt_IMark(s.addr,s.len,s.delta),(),s)); return None
        if k=="Ist_WrTmp":
            v=self.expr(s.data)
            if v.ty!=self.tyenv[s.tmp]: raise UnsupportedVEX(f"Type mismatch binding t{s.tmp}")
            self.temps[s.tmp]=v; self.binds.append(BindTemp(s.tmp,v)); return None
        if k=="Ist_Put":
            v=self.expr(s.data); self.guest=self.effect(tv.Put(v.ty),self.guest,self.i32(s.offset),v,source=s); return None
        if k=="Ist_PutI":
            v=self.expr(s.data); self.guest=self.effect(tv.PutI(v.ty),self.guest,self.const(tv.IRRegArray,s.descr),self.expr(s.ix),self.i32(s.bias),v,source=s); return None
        if k=="Ist_Store":
            v=self.expr(s.data); self.memory=self.effect(tv.Store(v.ty),self.memory,self.expr(s.addr),v,self.end(s.end),source=s); return None
        if k=="Ist_StoreG":
            v=self.expr(s.data); self.memory=self.effect(tv.StoreG(v.ty),self.memory,self.expr(s.addr),v,self.expr(s.guard),self.end(s.end),source=s); return None
        if k=="Ist_LoadG":
            a=self.expr(s.alt); v=self.apply(tv.LoadG(a.ty),self.memory,self.expr(s.addr),a,self.expr(s.guard),self.const(tv.IRLoadGOp,str(s.cvt)),self.end(s.end)); self.temps[s.dst]=v; self.binds.append(BindTemp(s.dst,v)); return None
        if k=="Ist_Exit": return Exit(self.expr(s.guard),self.expr(s.dst),str(s.jumpkind),int(s.offsIP))
        if k=="Ist_AbiHint": self.events.append(ProgramEvent(tv.IRStmt_AbiHint(s.len),(self.expr(s.base),self.expr(s.nia)),s)); return None
        if k=="Ist_MBE": self.events.append(ProgramEvent(tv.IRStmt_MBE(str(s.event)),(),s)); return None
        if k in {"Ist_CAS","Ist_LLSC","Ist_Dirty"}: raise UnsupportedVEX(f"{k} needs a continuation/proof-term lowering")
        raise UnsupportedVEX(f"Unsupported statement {k}")
    def translate_block(self,b):
        self.reset(); self.source_tyenv=b.tyenv; self.tyenv=tuple(self.ty(t) for t in b.tyenv.types); self.guest=ValueRef("guest_state_0",tv.GuestState); self.memory=ValueRef("memory_0",tv.Memory); xs=[]
        for s in b.statements:
            x=self.stmt(s)
            if x is not None: xs.append(x)
        xs.append(Exit(None,self.expr(b.next),str(b.jumpkind),None,True))
        return LoweredBlock(b.addr,b.size,str(b.jumpkind),self.tyenv,(ValueRef("guest_state_0",tv.GuestState),ValueRef("memory_0",tv.Memory)),tuple(self.values),tuple(self.binds),tuple(self.effects),tuple(self.events),tuple(xs),self.guest,self.memory)
