ok, so this is the next-in-progress goal: "implement lifter from machine code (x86) to IR - use pyvex library"
and so implementing this requires two things, it requires using the pyvex lifter, which you have to install and setup and kind of understand what it is doing. And then it also requires understanding my Stroscot IR, and how the VEX IR is encoded in it

so notes:

- the VEX types and how I want them represented in the IR are documented in new-sprint/vex-ir/vex-primops/vex-ir-summary.md
    - there is a JSON file in vex-primops/vex-ops-description.json extracted directly from the C VEX IR files, this may be more useful than trying to figure out PyVex APIs for dealing with the IrOp type
- the pyvex API is (barely) documented in https://api.angr.io/projects/pyvex/en/latest/ - I have found it is often more informative to directly read the source code at https://github.com/angr/pyvex, there are also some notes in the vex-ir-summary file
- in theory installing pyvex should be as easy as `pip install pyvex`
- examples of how to define IR operations are in core_ir.py and dirty_ir.py and examples of how to create them are in the tests
- you should make a new folder vex-lift within vex-ir, I think, to contain the lifter and new vex IR Python types
- some old diagrams of how the Stroscot IR looks are in https://mathnerd314.github.io/stroscot/Commentary/Implementation/Reduction-Example.html, the reduction example is not really correct based on how I am encoding the IR now but the general tree and graph structure at the beginning is still accurate
- start by taking the example lifted prgoram from https://decompilation.wiki/fundamentals/cfg-recovery/lifting/#example-lifted-program and seeing if you can decompile it with PyVex, then figure out how to encode it in the Stroscot IR. this is equivalent to this C program:
```
int main(int argc) {
    int t;
    if (argc > 2) t = 0;
    else          t = 1;
    return t;
}
```

if it is too hard then maybe try running trampoline.py from ../native-runtime and using the snippets there, they are just straight-line things with no control flow.