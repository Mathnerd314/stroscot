This is an excellent framing. Here is an expanded research-grounded treatment of each dimension, drawing in sharper details and open problems.

***

## Startup Time

The research consensus is clear: a fast-path interpreter or cached/AOT code is the right strategy. The core insight is that a JIT's job at startup is simply *not to get in the way*. Copy-and-patch — used in CPython 3.13 — generates code two orders of magnitude faster than LLVM -O0, making it practical as a startup tier that runs faster than a pure interpreter without any real compilation budget. [en.wikipedia](https://en.wikipedia.org/wiki/Copy-and-patch)

Formally verified JITs (e.g., the CompCert-derived JIT work) note that interpretation is the natural first tier precisely because it requires zero upfront compilation; the JIT layer only fires after observed behavior gives it something to work with.  GraalVM Native Image shows the logical extreme: compile everything offline, pay zero JIT cost at startup, and accept that you've frozen all optimization decisions. [arxiv](https://arxiv.org/pdf/2212.03129.pdf)

The open research question here is **profile-guided warm-up sharing**: Oracle Labs research proposed a remote compilation server that shares compiled code across VMs, so a "cold" VM can inherit a "warm" profile from a sibling that already ran the same program. [labs.oracle](https://labs.oracle.com/pls/apex/f?p=LABS%3A0%3A100123841340494%3AAPPLICATION_PROCESS%3DGETDOC_INLINE%3A%3A%3ADOC_ID%3A4000)

***

## Warm-Up Time

Your framing is accurate. The fundamental variable is *when the first useful native code appears*:

- **Copy-and-patch and baseline method JITs** produce native code on first invocation or after a very low threshold (sometimes 1–2 calls). Druid (2025) shows that an auto-generated baseline JIT runs **2× faster than the interpreter** with minimal warm-up investment and only needs changes at ~60 call sites. [arxiv](http://arxiv.org/pdf/2502.20543.pdf)
- **Two-level RPython (adaptive RPython)** achieves baseline JIT at 1.77× interpreter speed quickly, then continues warming toward tracing-quality code over time — a staged approach where every tier improves on the previous. [arxiv](http://arxiv.org/pdf/2201.09268.pdf)
- **Tracing JITs** require loop trip-count thresholds (~100–1,000 iterations) before tracing begins, then additional time to compile the trace. This makes them categorically unsuitable for programs that don't loop heavily. [pypy](https://www.pypy.org/posts/2025/01/musings-tracing.html)
- **ML-predictive speculation (True-JIT)** hides this cost by compiling *before* the threshold is reached based on learned patterns, effectively moving compilation off the critical path. [uksystems](https://uksystems.org/workshop/2025/pdfs/paper5.pdf)

The key research insight is that **warm-up and tier design are inseparable**: the question isn't "how fast does the JIT warm up?" but "what does each tier hand off to the next, and when?"

***

## Steady-State Throughput

The tradeoff with warm-up is real and well-documented. The 2017 "Warmup Blows Hot and Cold" paper showed that fewer than half of VM/benchmark pairs ever reach a *stable* steady state — many oscillate or never converge — which means "steady-state throughput" is often an idealization. [arxiv](https://arxiv.org/pdf/1602.00602.pdf)

When a stable steady state *is* reached:

- **Partial evaluation (offline or JIT-driven)** produces the best throughput by effectively compiling the interpreter away. The 2024 "Partial Evaluation, Whole-Program Compilation" paper derives compiled code directly from a single interpreter source of truth, reducing the semantic gap between spec and compiled output. [arxiv](http://arxiv.org/pdf/2411.10559.pdf)
- **Tracing JITs** achieve outstanding loop throughput but get stuck when the hot path is method-call-heavy or polymorphic — the trace tree degenerates. Meta-hybrid designs recover this by falling back to method compilation for those units. [arxiv](https://arxiv.org/pdf/2011.03516.pdf)
- **JIT heuristic bugs** are a real and underappreciated cause of steady-state regression. The 2026 OOPSLA paper on JIT performance bugs identifies cases where misguided heuristics (over-inlining, wrong tier selection) permanently prevent a function from reaching its optimal tier. [arxiv](https://arxiv.org/html/2603.06551)
- **Deegen (2024)** — a VM generator for dynamic languages — produces a baseline JIT that is 360% faster than PUC Lua and only 33% slower than LuaJIT's optimizing JIT at steady state, with negligible startup delay, suggesting that well-designed tiering can close most of the gap between "quick" and "optimizing" JITs. [arxiv](https://arxiv.org/html/2411.11469v2)

***

## Compilation Latency

Compilation latency and warm-up are coupled but distinct: warm-up is the user-visible delay, latency is the per-compilation-unit cost that produces it.

- **Copy-and-patch** is explicitly designed to minimize latency: it matches pre-generated code stencils to bytecode and patches addresses, requiring no IR construction, no register allocator, no optimization passes. It is "orders of magnitude faster" than LLVM per compile unit. [news.ycombinator](https://news.ycombinator.com/item?id=38769874)
- **Tracing** produces bursty latency: nothing, then a sudden large pause when a trace is recorded and compiled. The PyPy team's warm-up improvements (2016) cut trace compilation time by 2.5× via a more efficient trace IR representation. [pypy](https://www.pypy.org/posts/2016/04/warmup-improvements-more-efficient-7082900097299909512.html)
- **Full optimizing JIT (e.g., Graal, TurboFan)** has the highest per-unit latency but produces the best code. The Rotor optimizing JIT paper showed that adding three optimization passes made the JIT only 1.4–1.9× slower to compile than a baseline JIT but produced code 6–10× faster at runtime — a strong latency/quality tradeoff. [arxiv](http://arxiv.org/pdf/2411.09391.pdf)
- **True-JIT's predictive speculation** is the most novel angle: it learns compilation sequences from past runs and pre-compiles speculatively, hiding latency by moving it off the hot path. [uksystems](https://uksystems.org/workshop/2025/pdfs/paper5.pdf)

***

## Compilation Cost

Cost (total resource consumption: CPU, memory, energy) is more elastic than latency and depends heavily on system design.

- **Multi-core parallelism** is the obvious lever: background compilation threads decouple compilation cost from execution latency. The key open question is the scheduling policy — when to compile, at what tier, on which core. [dl.acm](https://dl.acm.org/doi/10.1145/3313808.3313818)
- **Code cache pressure** grows with the number of active versions; eviction policies determine whether compilation cost is amortized or repeated.
- **ML-guided JITs (JavART)** reduce compilation cost by replacing expensive optimization passes with a lightweight model that makes "good enough" decisions cheaply. [dl.acm](https://dl.acm.org/doi/10.1145/3720418)
- **Workload shape matters enormously**: a server with steady long-running loops can absorb high compilation cost; a CLI tool or real-time system (Julia's RT concerns, embedded Lua) cannot. [discourse.julialang](https://discourse.julialang.org/t/jit-issues-relevant-to-realtime-applications/67736)
- The most underexplored axis is **energy cost**: V8's Maglev was explicitly motivated partly by reducing power consumption, a dimension most academic JIT papers ignore entirely. [v8](https://v8.dev/blog/maglev)

***

## Code Quality

Your hierarchy is well-supported by the literature:

1. **Static AOT / offline partial evaluation**: best quality, no runtime pressure, can run full optimization suites. CompCert-class verified compilers sit here. [cacm.acm](https://cacm.acm.org/research/formal-verification-of-a-realistic-compiler/)
2. **Offline partial evaluation derived from interpreter (2024 paper)**: derives compiled code from the interpreter spec, achieving near-AOT quality with semantic fidelity guarantees. [arxiv](http://arxiv.org/pdf/2411.10559.pdf)
3. **Optimizing JIT (Graal, TurboFan C2, LuaJIT)**: close to AOT on hot paths, with the advantage of runtime profiling enabling speculative optimizations that AOT cannot make.
4. **Tracing JIT**: excellent for reducible loop-dominated code, poor for method-call-heavy or polymorphic code. Does not build a full CFG, so whole-function optimizations (e.g., global register allocation) are largely absent. [pypy](https://www.pypy.org/posts/2025/01/musings-tracing.html)
5. **Baseline method JIT / copy-and-patch**: faster than an interpreter but not much more — essentially avoids bytecode dispatch overhead without doing real optimization. Wikipedia describes copy-and-patch as "quick-and-dirty" but notes it "can approach partially optimized code" in favorable cases. [en.wikipedia](https://en.wikipedia.org/wiki/Copy-and-patch)
6. **Threaded code**: little better than interpreted bytecode in quality; its value is lower dispatch overhead rather than better code generation.
7. **Meta-hybrid**: the key insight is that code quality is *per-unit*, so a hot loop can be traced to excellent quality while cold or method-heavy code gets a cheaper tier. [arxiv](http://arxiv.org/pdf/2201.09268.pdf)

***

## Bug-Finding and Verified JITs

The correctness/performance tradeoff in verified JITs is better than most practitioners assume. The CompCert-derived JIT work shows that formal verification of native code generation is achievable, and that the overhead is mostly in **metadata and proof bookkeeping** rather than runtime execution cost. [inria.hal](https://inria.hal.science/hal-03882598/document)

The core limitation is **prover expressiveness**: most verification frameworks can handle straight-line optimization and simple deoptimization, but struggle with speculative optimizations, aggressive inlining across deoptimization points, and escape analysis. The POPL 2021 paper on formally verified speculation/deoptimization is one of the few that tackles the hard case — and it required significant proof engineering to handle the frame rule across code and data simultaneously. [janvitek](https://janvitek.org/pubs/popl21.pdf)

A practical middle ground is **differential testing and compilation-space exploration**: rather than proving correctness, you exhaustively explore the space of compilation decisions and compare outputs. The Artemis (SOSP 2023) paper introduced this as a scalable alternative to formal proof for finding JIT miscompilation bugs.  The 2026 performance-bug paper complements this with differential *performance* testing, catching cases where the JIT is correct but slow due to bad heuristics. [connglli.github](https://connglli.github.io/pdfs/artemis_sosp23.pdf)

The research direction worth watching is **verified deoptimization + unverified fast path**: verify only the correctness of the fallback mechanism, leave the fast path unverified but tested heavily. This keeps proof obligations tractable while ensuring the system can always safely recover from a wrong optimization. [arxiv](https://arxiv.org/pdf/2212.03129.pdf)