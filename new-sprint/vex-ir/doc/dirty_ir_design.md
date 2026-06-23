# Dirty IR — Opaque Types and Admissible Rules

**Module:** `dirty_ir.py`  
**Depends on:** `typed_ir.py` (frozen — do not modify)  
**Language:** Python 3.10+  
**Logic basis:** Linear Logic (Girard 1987) — core rules in `typed_ir.py`

---

## 1. Overview

`dirty_ir.py` extends the core sequent calculus IR with two features that
are logically sound but computationally impractical to represent in fully
expanded form:

1. **Opaque types** — primitive types (e.g. `Int32`) whose case structure
   exists in the theory but is too large to enumerate.
2. **Admissible rules** — derived inference rules whose core-rule expansion
   is justified by prose argument rather than being written out in code.

Together these features make the IR practically usable as a compiler IR while
preserving the *invariant that every construct is, in principle, admissible in
the core logic*.

The files `typed_ir.py`, `connectives.py`, `pretty.py` are **frozen** — all
extensions go in `dirty_ir.py`.

---

## 2. Opaque Types

### 2.1 Motivation

A `JumboFormula` stores its case structure explicitly as a `tuple[Case, ...]`.
This works for `Bool` (2 cases), `Option[A]` (2 cases), and any finite
connective with a manageable number of cases.  It does not work for `Int32`,
which has 2³² cases — one per 32-bit bit-pattern.

An **opaque type** is a `Formula` subclass that names a type without storing
its cases:

```python
@dataclass(frozen=True)
class OpaqueType(Formula):
    name:        str
    polarity:    Polarity
    description: str = ""
```

### 2.2 Relationship to JumboFormula

Every `OpaqueType` T corresponds to some (possibly enormous) `JumboFormula` J
in the theory.  This correspondence is:

- **Documented** — the `description` field and `justification` strings on
  associated rules explain it.
- **Asserted** — we treat it as a metatheoretic axiom, not a proven theorem in
  this codebase.
- **Not enforced** — if you introduce an `OpaqueType` with no plausible
  JumboFormula expansion, the code will not catch it.

`OpaqueType` intentionally does **not** subclass `JumboFormula`.  This means:

| Operation | `JumboFormula` | `OpaqueType` |
|-----------|---------------|-------------|
| `Build`   | ✓ allowed      | ✓ allowed (via admissible `Const` rule) |
| `Break`   | ✓ allowed      | ✗ **type error** — `Break.principal` must be a `JumboFormula` |
| `Identity`| ✓ allowed      | ✓ allowed (structural rule, formula-agnostic) |
| `Cut`     | ✓ allowed      | ✓ allowed (structural rule, formula-agnostic) |

Making `Break(principal=<opaque>)` a type error (not just a runtime assertion
failure) is a deliberate design choice: it catches mistakes early and makes the
restriction visible at the class level.

### 2.3 Predefined primitive types

The `FlatType` specifies a J-type with no premise substructure. Flat types include all the traditional "machine" types. The name is taken from Haskell literature, where types with no substructure form flat CPO domains.

| Name       | Polarity | Description |
|------------|----------|-------------|
| `Int32`    | POS      | 32-bit two's-complement integer |
| `Int64`    | POS      | 64-bit two's-complement integer |
| `Float32`  | POS      | IEEE 754 single-precision float |
| `Float64`  | POS      | IEEE 754 double-precision float |
| `Bool`     | POS      | Boolean; theoretically a 2-case JumboFormula |

`Bool` is listed as opaque for uniformity even though its JumboFormula expansion is trivial and could be written explicitly - it gives a trivial example of how to do control flow with opaque types.

---

## 3. Admissible Rules

### 3.1 Core vs. Admissible

The rules in `typed_ir.py` are the **core** rules of the logic — each is
necessary, none is redundant, and together they are complete for linear logic.
An **admissible rule** is one that can in principle be derived from the core
rules, even if we do not write down the derivation.

```
Core rules        typed_ir.py      Identity, Cut, Build, Break,
                                   Promotion, Dereliction,
                                   Weakening, Contraction

Admissible rules  dirty_ir.py      AddI32, MulI32, ConstI32, …
```

The split is **documentary**, not operational.  At runtime, both families
produce `InstantiatedRule` objects validated by `check_shape`.  You can detect
admissible rules with `isinstance(rule, AdmissibleRule)`.

### 3.2 The AdmissibleRule base class

```python
class AdmissibleRule(RuleSchema):
    name:          str   # short identifier for pretty-printing
    justification: str   # prose argument for admissibility

    def check_shape(self, instance: InstantiatedRule) -> None: ...
```

Every subclass must:
1. Set `name` and `justification` as class variables.
2. Override `check_shape` using `assert`-based checking (same convention as
   core rules).

The `justification` string is the *only* place in this codebase where the
connection to the core logic is recorded.  It is therefore the single most
important field to fill in carefully.  A missing or vacuous justification is
an admission that the rule may not actually be admissible.

### 3.3 Adding a new admissible rule

```python
@dataclass(frozen=True)
class MyOp(AdmissibleRule):
    name = "my_op"
    justification = (
        "Admissible because … (reference to the core-rule derivation sketch)."
    )

    def check_shape(self, instance: InstantiatedRule) -> None:
        # inspect instance.tops, instance.bottom, instance.key_slots_*
        assert len(instance.tops) == 0        # leaf rule (no premises)
        assert len(instance.bottom.left) == 2  # two inputs consumed
        assert len(instance.bottom.right) == 1 # one output produced
        assert instance.bottom.left[0]  == SomeOpaqueType
        assert instance.bottom.right[0] == SomeOpaqueType
```

Rules with parameters (e.g. `ConstI32.value`) should be `@dataclass(frozen=True)`
so that two rules with the same parameters compare equal and hash the same.

### 3.4 Sequent shape conventions for arithmetic rules

All current arithmetic rules are **flat rules** (zero premises).  The bottom
sequent encodes the operation as a linear function:

```
x : T, y : T  ⊢  z : T        (binary op)
x : T          ⊢  z : T        (unary op)
x : T, y : T  ⊢  b : Bool      (comparison)
```

Inputs appear on the **left** side of the sequent (consumed resources);
outputs appear on the **right** (produced resources).  This is the standard
linear-logic convention.

---

## 4. Predefined Admissible Rules

### 4.1 Integer arithmetic (Int32)

| Rule      | Signature                              | Notes |
|-----------|----------------------------------------|-------|
| `AddI32`  | Int32, Int32 ⊢ Int32                  | wrapping addition |
| `SubI32`  | Int32, Int32 ⊢ Int32                  | wrapping subtraction |
| `MulI32`  | Int32, Int32 ⊢ Int32                  | wrapping multiplication |
| `DivI32`  | Int32, Int32 ⊢ Int32                  | truncating division; UB on zero |
| `RemI32`  | Int32, Int32 ⊢ Int32                  | remainder |
| `NegI32`  | Int32 ⊢ Int32                         | two's-complement negation |
| `AndI32`  | Int32, Int32 ⊢ Int32                  | bitwise AND |
| `OrI32`   | Int32, Int32 ⊢ Int32                  | bitwise OR |
| `XorI32`  | Int32, Int32 ⊢ Int32                  | bitwise XOR |
| `NotI32`  | Int32 ⊢ Int32                         | bitwise NOT |
| `ShlI32`  | Int32, Int32 ⊢ Int32                  | left shift |
| `ShrI32`  | Int32, Int32 ⊢ Int32                  | arithmetic right shift |
| `EqI32`   | Int32, Int32 ⊢ Bool                   | equality |
| `LtI32`   | Int32, Int32 ⊢ Bool                   | signed less-than |
| `LeI32`   | Int32, Int32 ⊢ Bool                   | signed less-than-or-equal |

### 4.2 Constant

The constant rule is an adapted version of `Build`; it allows introducing a literal. Unlike `Break`, `Build` poses no computational issues.

### 4.2 Control flow

| Rule      | Signature                                    | Notes |
|-----------|----------------------------------------------|-------|
| `IfBool`  | Bool ⊢ A                                     | conditional select; `branch_type` param |

This demonstrates that admissible rules can involve subpremises; they are not simply limited to leaf nodes.

---

## 5. Safety Properties and Failure Modes

### 5.1 What the code guarantees

- Every `InstantiatedRule` of an `AdmissibleRule` subclass has passed
  `check_shape`.  The sequent shape is correct.
- `Break` cannot be applied to an `OpaqueType` — this is a static type error.
- `isinstance(rule, AdmissibleRule)` reliably identifies "dirty" rules for
  auditing, pretty-printing, or future verification passes.

### 5.2 What the code does NOT guarantee

- That the justification for admissibility is correct.  A malicious or careless programmer
  can write a nonsense "admissible rule" and the code will accept it.
- That `check_shape` is tight enough.  A rule could pass its own shape checker
  while still not being admissible (e.g. by being too permissive about types).
- That the metatheoretic axiom "OpaqueType T ≅ some JumboFormula" holds.
  This is an assertion by naming, not a proof.

These are acceptable tradeoffs for a practical IR.  If formal verification is
ever desired, the justification comments provide a roadmap for what needs to
be proved, and the `check_shape` methods encode the expected sequent shape for
every rule.

---

## 6. File Structure

| File                  | Contents |
|-----------------------|---------|
| `typed_ir.py`         | **Frozen.** Core formula types, sequent, core rule schemas, derivation tree |
| `connectives.py`      | **Frozen.** Standard connective constructor pretty-printing |
| `pretty.py`           | **Frozen.** Pretty-printer registry |
| `dirty_ir.py`         | `OpaqueType`, `AdmissibleRule`, predefined opaque types and arithmetic rules |
| `dirty_connectives.py`| (to be written) Pretty-printer registrations for opaque types and admissible rules |
| `test_dirty.py`       | (to be written) Unit tests for shape checkers |

---

## 7. Design Decision Index

| Decision | Chosen | Rejected | Reason |
|----------|--------|----------|--------|
| OpaqueType inheritance | Separate class, NOT a JumboFormula subclass | Subclass of JumboFormula | Makes `Break(opaque)` a static type error rather than a runtime assert |
| Core / admissible split | `isinstance(r, AdmissibleRule)` tag | Separate registries or flags | Minimal overhead; works with existing `RuleSchema` hierarchy |
| Justification field | Free-text `str` on the class | No field; comments only | Tooling can enumerate and audit justifications programmatically |
| Arithmetic as zero-premise rules | Leaf `InstantiatedRule` with no tops | Separate `Opcode` class | Uniform with the rest of the IR; derivation tree nodes look the same |
| UB (e.g. div-by-zero) | Documented in `justification`; no runtime check | Trap or proof obligation | Matches target semantics (C/LLVM); proof obligation can be added later |
