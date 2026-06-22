# ADR: Implement a VEX-like Intermediate Representation from Scratch

**Status:** Proposed  
**Deciders:** mathnerd314159265  
**Date:** 2026-06-19  
**Context:** New code generator for a dynamic binary translator (Python, x86 initial target)

---

## Context and Problem Statement

A new dynamic binary translator requires an intermediate representation (IR) to sit between the lifter (raw machine code → IR) and the code generator (IR → target machine code). The IR must be instruction-precise, side-effect-explicit, and suitable for analysis passes ported or adapted from the angr ecosystem.

The central question is whether to adopt an existing IR library (primarily pyvex/libVEX) or to implement a clean VEX-like IR from scratch in Python.

---

## Decision Drivers

- **Instruction precision.** The IR must capture all aspects of a machine instruction: operands, result, flags, PC update, and side effects, in a way that is complete and unambiguous.
- **Analysis suitability.** Existing angr analysis passes (pyvex-based) should be adaptable with moderate effort.
- **Python-first.** The project is written in Python; compiled dependencies are acceptable only if they expose a clean Python API.
- **Dynamic translation context.** The IR was designed to serve a code generator, not just a static analyzer, so the "gestalt" of VEX (designed for dynamic translators) is relevant.
- **Unison/LLVM integration.** The code generator is based on Unison, which uses LLVM's Linear SSA (LSSA) form; alignment with full SSA will aid this integration.
- **Practical maintainability.** The chosen IR must not block forward progress on AVX-512 and other modern instructions.

---

## Considered Options

| Option | Summary |
|--------|---------|
| **A. Use pyvex / angr VEX as-is** | Python bindings to the frozen 2015 libVEX fork |
| **B. Use stock Valgrind libVEX (via new Python bindings)** | Wrap the upstream C library with fresh bindings |
| **C. Implement a VEX-like IR from scratch in Python** | Pure-Python IR that mirrors VEX's design principles |
| **D. Use LLVM IR (via llvmlite or MLIR)** | Lift directly to LLVM IR or MLIR dialects |
| **E. Use Ghidra p-code (via PyGhidra / Pyhidra)** | Extract Ghidra's RTL via the Java API from Python |

---

## Decision Outcome

**Chosen option: C — Implement a VEX-like IR from scratch in Python.**

The design of VEX is the gold standard for instruction-precise, analysis-friendly IRs for dynamic translators. However, the pyvex fork is stale (forked 2015, AVX-512 unresolved as of 2026), Valgrind upstream is conservatively maintained with the same limitations, and the Python binding layer in pyvex is small enough (~7k Python LOC) that it does not represent a prohibitive implementation cost. A fresh implementation allows fixing the known structural shortcomings of VEX (simple optimizer, not-full SSA, AVX-512, precise-exception observability) without inheriting a decade of accumulated C glue.

---

## Detailed Analysis of Options

### Option A: Use pyvex / angr VEX as-is

**Pros**
- Zero lift cost: pyvex already provides Python objects for every VEX IR construct.
- Large body of angr analysis passes immediately available.

**Cons**
- libVEX is a 2015 fork of Valgrind that is not synchronized upstream. AVX-512 support is absent; the relevant bug (KDE #383010) has patches but has not been merged into either Valgrind or angr/vex as of 2026.
- AVX-512 has been officially marked "very low priority" by the pyvex maintainers, making it a structural dead end for production use on modern x86 hardware.
- The C layer (~265k LOC for all architectures in the full VEX tree) is opaque and hard to extend safely from Python.
- angr analysis passes are coupled to pyvex's IR object model; adapting them to a custom IR requires the same porting work regardless of whether pyvex is the starting point.
- The moment you write a custom angr pass against pyvex's object model, you have created the first unit of debt. Analysis passes are deeply coupled to the IR object model by definition — that's the whole point of writing them. Every `isinstance(stmt, pyvex.stmt.Put)` or `stmt.offset` or `expr.op` in your pass is a coupling point that needs to change. And passes, by their nature, tend to proliferate — you write one, it works, you write another. Each pass is a discrete chunk of IR-coupled code that needs to be ported at the flag day. You won't be porting one pass, you'll be porting N passes, where N grew while you weren't paying attention.

**Verdict:** Not viable - long-term lack of AVX-512 will bite, and effort on a short prototype will just compound debt.

---

### Option B: Use stock Valgrind libVEX via new bindings

**Pros**
- Tracks the most-maintained VEX codebase.
- Preserves the full precision of the original IR design.

**Cons**
- AVX-512 is also absent in upstream Valgrind; the same patch backlog applies.
- Julian Seward (VEX creator) is no longer the primary maintainer; Mark Wielaard (current maintainer) has a conservative maintenance posture. The 2017 FOSDEM talk identifying structural issues (precise exceptions, simple optimizer, not-full SSA) remains essentially current as of 2026.
- Writing robust Python bindings to a C99+GNU-extensions library is non-trivial, requires platform-specific compilation, and re-creates the exact maintenance surface that pyvex already exists to avoid.

**Verdict:** Worse than Option A (same AVX-512 problem, more integration work).

---

### Option C: Implement a VEX-like IR from scratch in Python (CHOSEN)

**Pros**
- The Python IR object model is demonstrably small: pyvex's Python layer is ~7k LOC, suggesting a clean-room IR core is similarly sized.
- Full control over the design, allowing structural improvements over VEX: full SSA (LSSA compatible with Unison), better optimizer, first-class AVX-512, clean precise-exception handling.
- No C build dependency: pure Python, optionally with a compiled lifter (e.g., a thin wrapper around a maintained x86 decoder).
- angr passes localize their IR coupling; adapting them to a fresh IR with the same statement/expression taxonomy is straightforward.
- Right now, before you've written a single line of pass code, the IR design cost is at its absolute minimum — you have full design freedom, no legacy to carry, and nothing to port. A week from now, after writing one pass, that's no longer true.

**Cons**
- The x86 lifter (instruction → IR) must be written or borrowed. `guest_x86_toIR.c` is ~16k LOC in the VEX tree; however, a prototype only needs the subset of x86 instructions actually used, with stub/TODO paths for the rest. LLM-assisted translation of the instruction → IR mappings is practical for this subset.
- The "anal retentive" flag and side-effect semantics that make VEX valuable must be replicated carefully. The risk is subtle semantic errors in less-common instructions.

**Verdict:** Correct long-term choice. Prototype scope (x86 subset + stubs) makes near-term cost manageable.

---

### Option D: Use LLVM IR (llvmlite / MLIR)

**Pros**
- Excellent SSA support and a large optimization ecosystem.
- Direct integration with Unison, which is LLVM-based.

**Cons**
- LLVM IR is a compiler IR, not a binary analysis IR. It does not natively represent instruction-level side effects: flags, partial registers (AH/AL), ring-0 instructions (LGDT, CLI, STI), precise-exception state, or memory ordering primitives.
- "Lifting binary code to LLVM cleanly is a pain" — the angr team's own assessment, still accurate in 2026.
- LLVM IR's infinite register model and lack of machine state (CPL, EFLAGS) make it unsuitable as the *primary* IR for a dynamic translator. It can serve as a downstream target but not as the analysis-facing IR.

**Verdict:** Wrong abstraction level for the primary IR. Could serve as a second-level IR for the optimizer/code-generator tier.

---

### Option E: Use Ghidra p-code (via PyGhidra / Pyhidra)

**Pros**
- Over 100 supported architectures.
- Active development by NSA; modern instructions added regularly.
- Accessible from Python 3 via PyGhidra (JPype) or Ghidrathon (Jep).

**Cons**
- p-code is an RTL (Register Transfer Language) designed for decompilation, not dynamic translation. It is less instruction-precise than VEX: flags are not lazily thunked, and micro-op detail is lower.
- The Java bridge (JPype/Jep) introduces significant overhead and complexity for a performance-sensitive dynamic translator hot path.
- Ghidra's primary design goal is human-assisted reverse engineering, not programmatic IR emission at runtime.

**Verdict:** Useful for architecture reference material and as a cross-check, but not a suitable primary IR.

---

## Key Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Flag semantics subtle errors | Medium | High | Direct port of VEX's cc_op/cc_dep thunk tables for the first prototype; fuzz against pyvex output |
| x86 decoder coverage gaps | High | Medium | Start with stub/TODO paths; use a maintained x86 disassembler (iced-x86) as the front end |
| Precise exceptions structural complexity | Medium | Medium | Focus on during IR design |
| angr pass adaptation cost underestimated | Low | Medium | angr explicitly localizes VEX coupling; IR object taxonomy is stable |
| Performance of pure Python IR | Low | Low | Will be addressed later; correctness matters more than throughput here |

---

## Positive Consequences

- No dependency on frozen or poorly-maintained C libraries.
- AVX-512 and future instruction sets are first-class concerns from the start.
- IR design is directly controlled, enabling precise-exception handling and full SSA to be built in cleanly.
- The implementation is readable and auditable by anyone familiar with Python, rather than requiring knowledge of C99+GNU-extensions.

## Negative Consequences

- Instruction semantics must be carefully replicated; bugs in flag behavior or memory ordering are possible and subtle.
- Total coverage of x86 (including the long tail of rarely-used instructions) will take significant time beyond Phase 1.
- No automatic inheritance of future Valgrind bug fixes.

---

## References

- Valgrind VEX IR header (`libvex_ir.h`) — canonical IR taxonomy
- angr/pyvex GitHub repository (~9k LOC total, ~7k Python LOC)
- FOSDEM 2017: "VEX: Where next for Valgrind's dynamic instrumentation infrastructure?" — Julian Seward
- KDE Bug #383010 — AVX-512 support in Valgrind/VEX (open, patches unmerged, 2026)
- Braun et al. 2013 — "Simple and Efficient Construction of Static Single Assignment Form"
- Unison Manual — LSSA form and code generator integration
- angr FAQ — rationale for choosing VEX over LLVM IR, TCG, BAP, REIL
