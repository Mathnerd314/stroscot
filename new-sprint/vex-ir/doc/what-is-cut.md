# What "Cut" Means in the Stroscot IR
### A one-page explanation for compiler engineers

---

## 1. Start from what you already know

In a conventional IR, a function call is one asymmetric node:

```
   f -----\
   a1 -----\
   a2 ------> [ CALL ] -----> r
              apply f(a1,a2)
```

The callee, the arguments, and the return value are all baked into a single opcode. The sequent-calculus view *decomposes* this into two independent halves that are then plugged together:

```
   FUN f --------------------\
                              >== CUT
   call-context(a1,a2,-,r) --/
```

- **Producer**: the function value `f`.
- **Consumer / context**: values `a1`, `a2` to feed as arguments, and a continuation `r` to hand back the result to."

**Cut is the name for the composition point where a producer and a matching consumer are plugged together.** It is not a new opcode — it is more fundamental. It is what happens whenever one piece of IR's output feeds another piece's input.

---

## 2. Cut is not "control" in the CFG sense

It is tempting to read this producer/consumer split as "data vs. control flow," but that overloads a word compiler engineers already use for branches and dominance. More precisely:

| Term                        | What it means here                                   |
|------------------------------|-------------------------------------------------------|
| CFG control edge             | reachability — did execution get here at all?         |
| Cut                          | composition — this producer's output *is* this consumer's input |

In this IR, reachability is already implied by linearity: if a value exists on a wire, its producer necessarily ran. There is no separate reachability token. (That's why `discard`/`drop` is necessary) So "cut" should be read as **typed composition**, not as a rebranding of control flow.

---

## 3. Cut is where reduction can happen; everything else is static structure

The rule tree, the typed wires, the opcodes — all of this is just *structure*, a description of what the program is built from. Nothing about that structure moves by itself.

**Reduction only happens at a cut**: a point where a constructor (something that builds a value) is directly wired to a matching eliminator (something that consumes that same value). When that pairing exists, the optimizer can locally simplify — fold constants, inline, fuse a construct-then-destruct pair, drop unreachable branches, etc. This is what "cut elimination" means: finding these constructor/eliminator pairings and reducing them, working inward from the sub-derivation ("witness") of each compound opcode.

```
   construct(x) ---CUT---> destruct(...)
          |                     |
          +--- reducible! ------+
          local rewrite replaces both nodes
          with the simplified result
```

So the compiler-friendly summary is:

> **The IR stores code as typed structure. `Cut` marks the live reduction boundaries inside that structure — the places where two fragments meet and can be simplified. Everything that is not a cut is just scaffolding: the shape that holds the program together between reduction points.**

This is why optimizations are organized *around cuts* rather than around every node uniformly — cuts are where interaction actually happens.

---

## 4. Why `DirtyCall` is different from a cut

`DirtyCall` represents an FFI call. It looks like a conventional IR "call," which invites confusion with the cut picture above. But `DirtyCall` plays a different role entirely:

- It is a **constructor** of the `Program` data type — an ordinary ADT value:

  ```haskell
  DirtyCall :: FnPtr -> [Arg] -> (Result -> Program) -> Program
  v = DirtyCall fn args k
  ```

- As an IR rule, it is a normal opcode with fixed input slots (`fn`, args, callback `k`) and one output slot (`Program`). It builds a value; it does not execute anything. Execution only happens later, when some external runtime interpreter pattern-matches on the resulting `Program` value.

- Inside the IR, `Program` values flow **upward only** — they are built, not destructed. So a `DirtyCall` node does not normally sit at a live cut the way a pure `add`-then-`use` redex does. It is *data representing an effect*: a piece of the effect tree describing "what to do," not a computation the optimizer is expected to reduce away.

**In one line:**

> `DirtyCall` is static: a data constructor describing an effectful action.
> `Cut` is dynamic but pure: the composition point where computation actually reduces.

They are different categories, not competing ways to spell "function call."

---

## 5. Why you would not want a "pure call" shaped like `DirtyCall`

If pure function application were encoded the same opaque way as `DirtyCall` — as a single black-box constructor swallowing its callee and continuation — the optimizer would lose the producer/consumer boundary that cut elimination depends on. Pure computation benefits from being exposed as an ordinary cut (so it can be normalized, inlined, fused); effectful operations benefit from being explicit, non-reducible data (so their ordering and observability stay intact). Collapsing the two into one opaque shape sacrifices whichever property mattered for that case.

---

## 6. The one-paragraph version, if you only get one shot

* Cut is not an opcode — it's the point where one piece of code's output plugs into another piece's input, the way a `let`-bound value feeds into its use.
* The rest of the IR — nodes, wires, opcodes — is just the static shape of the program. Cut is where that shape becomes *reducible*: wherever a construct and its matching destruct meet, the optimizer can simplify.
* `DirtyCall` isn't a cut; it's a plain data constructor describing an effect, built once and left alone until an interpreter runs it. Cut is for computation you want to normalize away; explicit `Program` constructors like `DirtyCall` are for effects you want to keep visible and ordered."