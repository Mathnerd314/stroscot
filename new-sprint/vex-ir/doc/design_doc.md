# Sequent Calculus Derivation Tree — Core Logic Design Documentation

**Project:** `typed_ir.py` / `connectives.py` / `pretty.py`  
**Language:** Python 3.10+  
**Logic:** Linear Logic (Girard 1987), two-sided sequent calculus

---

## 1. Overview

This document records the design of a Python representation of sequent calculus derivation trees for linear logic. The system has three layers:

1. **Formulas** — a recursive algebraic data type for linear-logic propositions
2. **Instantiated Rules** — fully concrete rule applications with explicit sequent contexts
3. **Derivation Trees** — graphs of instantiated rules connected via premise/conclusion matching

---

## 2. Formulas

### 2.1 Base Types

```python
class Formula:            # base class
class Atom(Formula):      # propositional variable, e.g. Atom('A')
class JumboFormula(Formula):   # any standard connective (see §2.2)
class Bang(Formula):      # exponential modality !A / ?A
```

All formula classes are `frozen=True` dataclasses — hashable and structurally equal.

### 2.2 JumboFormula

All standard connectives (⊗, ⅋, ∨, ∧, →, ¬, shifts, 0, 1, ⊤, ⊥) are encoded
uniformly as a **polarized case schema**:

```python
@dataclass(frozen=True)
class Case:
    label:    str                       # e.g. '#s', '#l', '#r', '#f'
    formulas: tuple[SidedFormula, ...]  # (Side, Formula) pairs

@dataclass(frozen=True)
class JumboFormula(Formula):
    polarity: Polarity    # POS (+) or NEG (-)
    cases:    tuple[Case, ...]
```

**Side convention within a Case:**
- `LEFT` = input (consumed from context; left of ⊸)
- `RIGHT` = output (produced into context; right of ⊸)

**Polarity convention:**
- `POS` connectives are active on the RIGHT (build = right-intro, break = left-elim)
- `NEG` connectives are active on the LEFT  (build = left-intro,  break = right-elim)

### 2.3 Standard Connectives as JumboFormula Instances

See `connectives.py`. 16 connectives defined. Key structural symmetries confirmed by inspection:
- `PosNeg(A)` and `DownShift(A)` share case structure `(#s,[(R,A)])` — differ only in polarity
- `NegNeg(A)` and `UpShift(A)` share case structure `(#s,[(L,A)])` — differ only in polarity
- `One` / `Bot` and `F` / `Top` are pairwise case-identical, differing only in polarity
- `UpShift` and `DownShift` differ in **both** polarity and side tag on A and cannot be collapsed
- Polarity cannot be dropped: it determines which side is active, which drives build vs. break

### 2.4 Bang (Exponentials)

```python
@dataclass(frozen=True)
class Bang(Formula):
    polarity: Polarity       # POS = !A, NEG = ?A
    sub:      Formula
    modality: Optional[str]  # for non-standard flavors of !
```

It is a theorem in the linear logic literature that exponentials cannot be expressed as finite combinations of standard connectives. Therefore exponentials are separate from JumboFormula with separate rules involving context duplication, context discarding, and whole-rule side conditions (see §4.3).

The modality field arises from the fact that exponentials are not "canonical" - we cannot prove the theorem BangA ↔ BangB if we have two distinct bang types. This means Bang requires special handling - sometimes we want the expressiveness of additional modalities, and other times it is convenient to ignore or erase it. In contrast, if we had two JumboFormula constructors, say JumboFormulaA and JumboFormulaB, we could prove the theorem JumboFormulaA ↔ JumboFormulaB for every instance of cases and polarity, and a modality field would be logically redundant.

---

## 3. Sequent

```python
SidedFormula = tuple[Side, Formula]

@dataclass(frozen=True)
class Sequent:
    formulas: tuple[SidedFormula, ...]
```

**Design decision: flat `(Side, Formula)` list** rather than two separate tuples `(Γ, Δ)`.

The flat list was chosen because rule application logic operates on the sequent holistically — `replace(old, new)` uniformly swaps a principal formula for its subformulas regardless of side. The traditional split view is available via `.left` and `.right` properties.

Key methods:
- `on(side)` — filter by side
- `replace(old, new)` — swap one sided-formula for a tuple (used by rule checking)
- `with_formula(sf)` / `without_formula(sf)` — non-destructive builders

---

## 4. Rule Schemas

Rule schemas are the **templates** defining the shape of a rule without concrete contexts.

```python
class RuleSchema:
    def check_shape(self, instance: InstantiatedRule) -> None: ...
```

### 4.1 Structural Rules

| Schema     | Premises | Description                               |
|------------|----------|-------------------------------------------|
| `Identity` | 0        | A ⊢ A                                     |
| `Cut`      | 2        | Γ⊢A,Δ and Θ,A⊢Λ  →  Γ,Θ⊢Δ,Λ             |

### 4.2 Jumbo Rules

| Schema  | Premises      | Description                                             |
|---------|---------------|---------------------------------------------------------|
| `Build` | `len(case.formulas)` | Introduce a JumboFormula on its active side; `case_index` selects the branch |
| `Break` | `len(cases)`  | Eliminate a JumboFormula from its passive side; one premise per case |

The Build/Break duality is the core of the formalism: every standard connective is handled by exactly these two generic rules, parameterized by the `JumboFormula`. There is no per-connective case analysis anywhere in the rule checker.

**Context preservation** for Build: context formulas are split multiplicatively across the tops (each top gets a disjoint portion), with shared additive context appearing in every top. The `check_shape` asserts that concatenating the non-key formulas across all tops equals the non-key formulas in the bottom.

### 4.3 Exponential Rules

| Schema         | Premises | Description                                           |
|----------------|----------|-------------------------------------------------------|
| `Weakening`    | 1        | Ignore input !A/?A                                    |
| `Dereliction`  | 1        | Strip bang wrapper, use bare subformula once          |
| `Contraction`  | 1        | Split one !A/?A into `count` copies                   |
| `Promotion`    | 1        | Introduce !A; all context must be bang-wrapped        |

Exponentials are a **third family** alongside structural and jumbo rules. They cannot be encoded as JumboFormula cases because:
1. Contraction/Multiplexing duplicate formulas — no case structure can express this
2. Weakening introduces a formula from nothing but with a non-trivial subformula
3. Promotion has a global side condition on the entire sequent context

---

## 5. InstantiatedRule

The instantiated rule is the key intermediate layer between rule schema and tree node.

```python
@dataclass(frozen=True)
class InstantiatedRule:
    rule:             RuleSchema
    tops:             tuple[Sequent, ...]
    key_slots_tops:   tuple[tuple[int,...],...]  # principal formula indices in each top
    bottom:           Sequent
    key_slots_bottom: tuple[int, ...]            # principal formula indices in bottom

    def validate(self) -> None:
        self.rule.check_shape(self)
```

**Why key slots?** `check_shape` needs to distinguish principal formulas (being introduced/eliminated) from context formulas (passively preserved). Key slots encode this explicitly rather than requiring the checker to infer it.

**Three levels of structure:**

```
RuleSchema          the template ('cut eliminates a formula')
    |
    v  instantiated with Sequents + key_slots
InstantiatedRule    one concrete rule application
    |
    v  connected via premises + perm_tops
Derivation          the full proof tree
```

---

## 6. Derivation Tree

```python
@dataclass
class Derivation:
    instantiated_rule: InstantiatedRule
    premises:          list[Derivation]
    perm_tops:         tuple[tuple[int,...], ...]  # optional during initialization, omitted / () → identity
```

### 6.1 Matching Invariant

The core correctness property, checked in `__post_init__`:

```
apply_perm(perm_tops[i], tops[i]) == premises[i].instantiated_rule.bottom
```

For the default case (identity permutation) this reduces to strict equality:

```
tops[i] == premises[i].instantiated_rule.bottom
```

### 6.2 Permutation Layer

**Design decision:** Rather than introducing an explicit exchange rule in the proof, permutations on context formulas are stored inline in `perm_tops`.

```python
def apply_perm(perm: tuple[int, ...], seq: Sequent) -> Sequent:
    # perm[j] = k: output position j comes from input position k
    return Sequent(tuple(seq.formulas[k] for k in perm))
```

`perm_tops` defaults to `None` → identity permutations for full backward compatibility. Non-identity permutations are printed in `pretty()` output; identity permutations are suppressed.

### 6.3 Structural Properties

| Property    | Definition                                 |
|-------------|---------------------------------------------|
| `is_leaf`   | `not self.premises`                        |
| `depth()`   | 0 for leaves; `1 + max(child.depth())`     |
| `size()`    | `1 + sum(child.size())`                    |
| `conclusion`| `instantiated_rule.bottom`                 |

---

## 7. Pretty Printer

A lightweight BURG-style printer using a priority-ordered rule list (`pretty.py`).

```python
def register(name, predicate, renderer, *, prepend=False): ...
def fmt(f: Formula) -> str: ...
```

Rules are `(predicate, renderer)` pairs tried in insertion order; first match wins. `prepend=True` inserts at index 0, giving unconditional priority. Unrecognized `JumboFormula` falls back to raw `J^+/- [...]` notation.

**Adding a new abbreviation:**
```python
register('Bool',
    lambda f: is_jf(POS)(f) and len(f.cases)==2
              and f.cases[0].label=='#t' and len(f.cases[0].formulas)==0
              and f.cases[1].label=='#f' and len(f.cases[1].formulas)==0,
    lambda f: 'Bool',
    prepend=True,
)
```

The `cases_match(jf, pattern)` helper accepts exact values or callable predicates (use `any_formula` as wildcard) for concise structural patterns.

---

## 8. File Structure

| File            | Contents                                                              |
|-----------------|-----------------------------------------------------------------------|
| `typed_ir.py`   | `Polarity`, `Side`, `Formula`, `Atom`, `Case`, `JumboFormula`, `Bang`, `Sequent`, core `RuleSchema` subclasses, `InstantiatedRule`, `Derivation`, `apply_perm` |
| `connectives.py`| Connective constructor functions + print rule registrations           |
| `pretty.py`     | Print registry: `register`, `fmt`, `cases_match`, `is_jf`, `any_formula`, `_fallback` |
| `test_all.py`   | 28 unit tests covering all rule schemas, `apply_perm`, derivation wiring |

---

## 9. Design Decision Index

| Decision | Chosen | Rejected | Reason |
|----------|--------|----------|--------|
| Sequent representation | Flat `(Side, Formula)` list | Two tuples `(Γ, Δ)` | Rule application is uniform replace; `.left`/`.right` recover split view |
| Formula equality | `frozen=True` dataclass | Mutable class | Hashable, structurally equal, safe as dict/set keys |
| Connective encoding | `JumboFormula` case schema | Per-connective classes | Uniform Build/Break; no per-connective case analysis in rule checker |
| Exponentials | Separate `Bang` + rule classes | Encode as JumboFormula | Cannot express duplication or global side conditions in case structure |
| Rule hierarchy | Class hierarchy with abstract `check_shape` | `Union` type alias | Shared base enables `isinstance` grouping; abstract method enforces completeness |
| Validation location | `Derivation.__post_init__` | `InstantiatedRule.__init__` | Keeps IR as plain record; validates at tree-construction time |
| Exchange/permutation | Inline `perm_tops` field | Explicit exchange rule | Avoids cluttering proof tree with trivial exchange steps |
| Print priority | Ordered list, `prepend=True` | Cost-based BURG | Sufficient; trivial to extend with one `register()` call |