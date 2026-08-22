import angr, monkeyhex
proj = angr.Project('./test')
print(proj.loader)
irsb = proj.factory.block(proj.entry).vex
irsb.pp()
proj.factory.block(0x403fd8).vex.pp()
