- don't care about performance. at all. simple, braindead solutions.
- speed speed speed (of development). get those tests passing as quickly as possible. no tests? use pytest
- syntax follows thought, think clearly

so I have some differences
- I don't include register assignment in the machine - it's in the calling convention which is stored in the call node
- I don't plan to implement codegen first, I plan to implement an interpreter which walks the instructions and executes them - some ops are inspired by specific machine ops but semantically they're all pure
- I don't understand why you have instruction selection (Machine.instSelect), why do you need to replace nodes with machine-specific versions, it doesn't seem to add information - or couldn't you simply put this in the IR - but I have a completely different codegen pipeline planned so it's not too important

but that's my design - I have an instruction which performs the multiplication, I have an instruction which generates the wraparound bit, and then I'm like, I want to do this as one multiply instruction and a flag check. And so my thought was, the bottom up rewrite generator sounds smart.

    @Override public String op() { return "div"; }
    @Override public RegMask regmap(int i) { return riscv.RMASK; }
    @Override public RegMask outregmap() { return riscv.WMASK; }
    @Override public void encoding( Encoding enc ) { riscv.r_type(enc,this,4,1);  }
    @Override public void asm(CodeGen code, SB sb) {
        sb.p(code.reg(this)).p(" = ").p(code.reg(in(1))).p(" / ").p(code.reg(in(2)));
    }
    @Override public String op() { return "divf"; }
    @Override public RegMask regmap(int i) { return riscv.FMASK; }
    @Override public RegMask outregmap() { return riscv.FMASK; }
    @Override public void encoding( Encoding enc ) { riscv.rf_type(enc,this,riscv.RM.RUP,13); }
    // Default on double precision for now(64 bits)
    // General form: "fdiv.d  rd = src1 / src2
}
public class DivFARM extends MachConcreteNode implements MachNode {
    DivFARM( Node divf) { super(divf); }
    @Override public RegMask regmap(int i) { return arm.DMASK; }
    @Override public RegMask outregmap() {  return arm.DMASK; }
    // FDIV (scalar)
    @Override public void encoding( Encoding enc ) { arm.f_scalar(enc,this,arm.OPF_DIV); }
    // General form: "VDIF =  dst /= src"
public class DivARM extends MachConcreteNode implements MachNode {
    DivARM( Node div ) { super(div); }
    @Override public RegMask regmap(int i) { return arm.RMASK; }
    @Override public RegMask outregmap() { return arm.WMASK; }
    // SDIV
    @Override public void encoding( Encoding enc ) { arm.madd(enc,this,arm.OP_DIV,3); }
    // General form: "div  dst /= src"