# IR Glossary

## Polarity

Every node in the IR carries a **polarity**: either **positive** or **negative**.

- A **positive** node is oriented toward data: it either constructs a value or pattern-matches one.
- A **negative** node is oriented toward control: it either constructs a continuation or pattern-matches one.

Polarity is an intrinsic property of the node's type. It is not assigned by position. Basic blocks and references inherit polarity from the rule they are associated with.

*Typical case:* the vast majority of connectives you will encounter are positive. Negative connectives exist and are fully supported, but are rare in practice.

***

## Sequent Sides: Antecedent and Succedent

Every rule has a **type**, which is a sequent. A sequent has two zones separated by `⊢`:

- The **antecedent** is the zone to the left of `⊢`. It holds propositions being consumed.
- The **succedent** is the zone to the right of `⊢`. It holds propositions being produced.

The words "left" and "right" are reserved exclusively as shorthand for antecedent and succedent when the sequent notation is in view. They are never used for spatial directions in the graph or tree.

The usage of "premise" and "conclusion" to name the antecedent and succedent zones of a sequent is retired, always use antecedent and succedent.

*Central example:* in `A, B ⊢ C`, propositions `A` and `B` are antecedent propositions ("on the left") and `C` is the succedent proposition ("on the right"). They are not called premises or conclusions.

***

## Polarity × Side

These two Boolean flags combine to give four operational roles:

| | Antecedent (left) | Succedent (right) |
|---|---|---|
| **Positive** | Match data | Build data |
| **Negative** | Build a continuation | Match a continuation |

***

## Packed and Unpacked Sequents

A sequent can appear in two forms:

- An **unpacked** sequent is an entry point — a labeled definition site, like a `def` block in Python or a function address in C. It can have any number of antecedent and succedent propositions. It does not exist as a runtime value; it is a named location in the derivation.
- A **packed** sequent is a first-class value. It has exactly one succedent proposition, which packages the entire sequent into a single transferable object. You can pass it around, store it, and later unpack it.

Packing an unpacked sequent is a straightforward, explicit operation using a jumbo type. In contrast, unpacking is more complex; 
an arbitary sequent may have multiple ways of expanding its derivation tree, leading to unpacking not being well-defined. However, the inverse of packing is well-defined and this is the sense we typically use.

*Edge case:* a sequent with one antecedent proposition is the dual of a value and follows similar rules.

***

## Build and Break

These are the two fundamental rule forms:

- A **build** rule constructs a value. For a positive connective, build appears on the succedent side. For a negative connective, build appears on the antecedent side.
- A **break** rule destructs a value by pattern matching. For a positive connective, break appears on the antecedent side. For a negative connective, break appears on the succedent side.

Build and break are always exact duals under polarity-flip + side-swap.

***

## Connective Types

### Jumbo

The **jumbo** connective is the general sum-of-products connective. It encodes an arbitrary number of variants, each with an arbitrary number of fields. It subsumes all standard connectives (tensor, with, plus, their duals) as special cases. A jumbo build rule selects one variant and provides all its fields. A jumbo break rule provides a handler for every variant and binds the fields of the matched one.

*Adjacent terms:* people variously call instances of this concept enums, algebraic data types, or variants. Those terms all point to roughly the same cluster but carry language-specific baggage. "Jumbo" is used here to avoid inheriting those connotations and to stay close to Levy's original formulation.

### Flat

A **flat** type is a jumbo connective whose variants carry no fields — only tags. Flat types are exactly the machine primitive types: integers, booleans, floats, characters. Their proof structure is trivial: a build rule names a value; a break rule switches on it. There is no recursive substructure.

*Etymology:* from the Haskell notion of a flat complete partial order — a type whose only subvalue relationship is `⊥ ≤ x` for every `x`. The bottom semantics are not used here, but the structural flatness is what matters.

### Box

The **box** type is the exponential connective, written `!A` in linear logic. It marks a proposition as unrestricted: it may be used any number of times (including zero), unlike ordinary linear propositions which must be used exactly once. Its full operational role in this IR is not yet fixed, but the type is present and structurally supported.

*Note on naming:* the connective is also called "bang" after its symbol `!`. "Box" is preferred here because it names the *modal* character of the type (a boxed value is wrapped in a mode that changes its usage rules) rather than its notation.

***

## Tree Directions: Rootward and Leafward

The derivation tree has a root and leaves. To describe movement through the tree unambiguously, regardless of how it is drawn on the page:

- **Rootward** means toward the root of the derivation tree — in the direction of the final sequent, the overall type of the whole rule.
- **Leafward** means toward the leaves — in the direction of the premises, the sub-derivations that feed a rule.

These terms are orientation-independent. Whether the tree is drawn top-down, bottom-up, or left-to-right on any given diagram, rootward and leafward remain fixed.

We addiitonaly incorporate standard tree terminology:
- A **child** of a rule R is any rule immediately leafward of R — a rule whose conclusion directly feeds one of rule R's premises.
- The **parent** of a rule R is the single rule immediately rootward of R — the rule whose leafward premise sequent rule R's conclusion sequent directly fills.

Every rule has exactly one parent, except the root rule which has none. A rule may have any number of children, including zero (in which case it is a leaf).

*Relationship to rootward/leafward:* rootward and leafward describe direction of travel across any number of steps; parent and child describe exactly one step. "The root is rootward of every node" is correct; "the root is the parent of every node" is not.

"Ancestor" and "Descendant" are also available for multi-step rootward and leafward relationships respectively — a rule's **ancestors** are all rules rootward of it, and its **descendants** are all rules leafward of it.

In this glossary, **premise** refers exclusively to a child sequent of a rule — one of the leafward inputs at the rule level — and **conclusion** refers exclusively to the rootward sequent that a rule derives. Conflating the sequent-level and rule-level meanings of these words is the primary source of confusion they cause, so the fix is strict scoping rather than full retirement.

***

## Proposition Threading: Sources and Targets

A proposition in a rootward sequent of a rule can be **threaded** through the rule to a corresponding slot in the leafward sequent. This relationship is:

- A **source** is the proposition slot in the leafward half of the ancestor sequent.
- A **target** is the corresponding proposition slot in the rootward half of the descendant sequent.

Sources and targets describe a general form of data flow through the whole graph.

***

***

# Retired Words and Why They Were Retired

## "Top" and "Bottom" (for tree direction)

*Retired in favor of:* **leafward** and **rootward**

These are retired because they are spatial metaphors that depend on drawing convention. In standard sequent calculus presentations, the root is drawn at the *bottom* and the leaves at the *top* — the opposite of a typical tree diagram. In a control flow graph, "top" means the entry point and "bottom" means the next instruction. These two conventions directly contradict each other. Any reader moving between sequent calculus literature and control flow graph literature will read "top" and "bottom" differently depending on which frame they are in. Since this IR straddles both worlds, the terms are unworkable.

## "Up" and "Down" (for tree direction)

*Retired in favor of:* **leafward** and **rootward**

Same problem as "top" and "bottom", but more acute because "up" and "down" also describe literal page direction. A reader might interpret "go up" as "move your eyes toward the top of the page" (spatial), "go toward the leaves" (sequent calculus convention), or "go toward the root" (typical tree convention). All three readings are natural in different contexts. There is no consistent interpretation.

## "Premise" and "Conclusion" (for sequent parts)

*Retired in favor of:* **leafward** and **rootward** (for tree direction); **antecedent** and **succedent** (for sequent sides)

Within a sequent, the antecedent is sometimes called the "premise" and the succedent the "conclusion" — this is standard Gentzen terminology, but this usage is retired. Instead we use these terms solely in a derivation tree: the sub-derivations feeding a rule are called its "premises" and the sequent it derives is called its "conclusion." Both usages are standard, but they operate at different levels (within a sequent vs. between rules), and this IR constantly requires you to talk about both levels simultaneously. Using the same words for both causes persistent confusion. "Antecedent/succedent" handles the within-sequent use; "leafward/rootward" handles the between-rule use.

## "Left" and "Right" (for control flow or tree direction)

*Retained only for sequent sides; retired elsewhere*

In sequent notation, left and right unambiguously refer to the antecedent and succedent zones. However, "left" and "right" also appear as spatial descriptions of how the graph is drawn, and as descriptions of which argument of a cut is the "left" operand vs. the "right" operand. These spatial and operational uses are retired. If you need to describe graph layout, use explicit layout vocabulary. If you need to describe cut operand order, name them by their roles (e.g., "the value argument" and "the continuation argument") rather than by position.

## "Proof" and "Refutation" (for packed sequents)

*Retired in favor of:* **packed**

In linear logic, a completed derivation with a single succedent proposition is called a proof of that proposition, and dually a single antecedent is a refutation. These are accurate technical terms, but they carry strong philosophical and academic connotations — a "proof" sounds like a finished mathematical argument, not a runtime value being passed to a function. For a compiler IR audience, these connotations are misleading. "Packed" focuses on the operational fact (the sequent has been wrapped into a single transferable value) without implying anything about logical validity or mathematical completeness.

## "Bang" (for the exponential connective)

*Retired in favor of:* **box**

"Bang" is the name of the symbol `!`, not a description of the type's behavior. It gives a reader no inferential purchase on what the type does. "Box" comes from modal logic (the `□` operator) and conveys the key property: a boxed value is placed in a distinct modal context that relaxes the linearity constraint. This is a more informative handle, and it connects to a broader literature on modal type theory that provides useful intuitions about what the exponential is for.