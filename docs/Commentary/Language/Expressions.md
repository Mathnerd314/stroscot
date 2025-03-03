# Expressions

An expression represents an element within the [universe of discourse](https://en.wikipedia.org/wiki/Domain_of_discourse). As Stroscot aims to maximize functionality, the goal is to have an unfettered universe of discourse in which all applications of words are understood according to their common conditions and meanings. Expressions are immutable (as [Rich Hickey says](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/PersistentDataStructure/00.11.36.jpg)). In terms of memory management, expressions are just data - they can be copied freely, and discarded once they will no longer be used. Expressions thus have a canonical serialized representation. For convenience we assume two serialization forms: a text (UTF-8) representation, for human interaction, and also a distinct in-memory representation that is perhaps more compact.

## Bare symbols

Bare symbols are perhaps the simplest expression. Bare symbols are simply a sequence of characters. Vaguely like [Raku](https://docs.raku.org/language/syntax#Identifiers), an ordinary bare symbol is a Unicode word, and an extended symbol uses an escape sequence `@"sym str"` to allow spaces etc. following the syntax for a string. An extended symbol matches a corresponding ordinary symbol if that form exists, i.e. `null == @"null"`. It is a bit tricky to write a bare symbol in proper Stroscot as most symbols are understood to be qualified.

One may ask: if we have jumbos, then why do we need symbols? The answer is that we need a base case - jumbos build expressions out of expressions, but if there are no expressions to start with then we cannot build anything.

## Logical proofs

According to the Curry-Howard isomorphism, proofs are programs and propositions are types. We use the logic described in {ref}`Reference/Logic:Logic` - sequent calculus derivations trees ("proofs") are expressions in the core language.

### Slots

Stroscot is unityped. So we want to erase the propositions - it should be valid to cut-and-paste any expression into any other expression. Like replacing any derivation tree with a bare symbol, for example. But we cannot quite erase the whole sequent - a sequent like $A \vdash B$ is used to define a lambda expression and so $A$ acts like a "binder", the `x` in `\x. x`. So I have compromised by preserving the "slots" of the sequent - a sequent like $A,B \vdash C,D$ is erased to $Slot,Slot \vdash Slot,Slot$, written more concisely as `2-2`. So each expression is identified by its number of left and right slots.

The main reason to use slots is alpha renaming. In concrete syntax, the binder is specified using variables (symbols), like a lambda expression `\x. x x`. But, technically, renaming the variables should not change the meaning of the binder (alpha equivalence). So we cannot represent a lambda like `Lam "x" (App "x" "x")` because it gives too much information - it exposes the variable name, which under alpha equivalence cannot be exposed. The only way to accomplish this is higher-order syntax - the binder should be a special part of the language, rather than just a convention, and use some sort of nameless representation, like `\<1 slot>. 1 1`. This sort of de Brujin numbering is exactly what slots accomplish - they are in this gray area between syntactic and semantic.

Now there is a concern: the use of slots means our language actually is not unityped, because the slots differentiate terms. But, we can use a jumbo build derivation to wrap any expression as a single-slot derivation. This is sort of like how we can box unboxed types in Haskell - it doesn't actually change the fact that we need to deal with slot counts in the reduction logic, it just hides it. Certainly, it would be nicer if Haskell had no unboxed types, and no syntactic restrictions on using unboxed types, but it does. And in :cite\`jonesUnboxedValuesFirst1991\` there is a pretty decent argument for including them: they are "almost" first-class, and because they come in a variety of shapes and sizes, they can be optimized efficiently, without the pointer indirection usually associated with a box. In fact {cite}`bolingbrokeTypesAreCalling2009` argues that arities should be exposed in the core for exactly this reason.

Also, I think this sort of information is present even in a unityped language like Python - one can see on inspection that `\x. x` takes one argument, and one needs to know that information to reduce `(\x. x) 1` to `1`. Now it is true with term rewriting that an expression like `function 1` also can reduce, but the body `x` in the lambda has an `x` in scope that a bare symbol `function` does not - it is not simply a bare symbol `x`. The slots formalize this sort of reasoning precisely.

### Jumbos

Per the logic, there are four "Jumbo" rules, J-R, J-L, J+R, J+L. Ignoring polarity and side for now, really there only two rules, "break" J-R/J+L and "build" J-L/J+R. And if we stretch our definition of each rule, we can encompass term rewriting in the semantics.

#### Terms

```
tree (combine (leaf 1 2) (leaf 1 2))
(++++) a b
(*) (some (weird thing)) 12
1 2
```

In the logic, the Jumbo "build" rule takes a tag `i` and then a collection of derivation trees. This is very similar to a term in term rewriting which takes a symbol and a collection of sub-terms. Also, the tag can be generalized to any set. Therefore, we allow the Jumbo build rule to combine any expression and then any collection of expressions, as long as the slots match up. In particular, the "tag" expression must have exactly one slot (a proof), and then for each expression in the collection of expressions, the jumbo rule must specify exactly one "active"/"target" slot that is consumed by the build expression.

Terms subsume algebraic data types. Whereas an ADT value is a symbol applied to other values, with restrictions on the types of those values, a term is "untyped" and has no restrictions on its constructor or arguments.

Term application also includes the other function syntax - keyword arguments, output arguments, and so on - so is more than just positional application.

#### Rewriting systems

There is also the Jumbo "break" rule. This contains a set of cases over possible "tags" and specifies for each case a derivation with a set of active slots that become bound. The active slots can vary between cases but the non-active slots must have the same arities for all cases. Going back to the tags, we can have an infinite set of tags and then an infinite set of cases, so the break rule really specifies a function from tags to derivation trees (expressions).

But then we can recall the isomorphism between terms-of-terms and terms with larger tags, like `a b c` vs. `a_b_c`. And once we allow matching on the sub-terms of the "build" expression, it makes sense to allow matching on any type of expression rather than just build-of-build. So then a break rule is a (mathematical) function from expressions to expressions of a specific slot size, and the semantics of a Cut between a build and a break is that it applies the break function to the build expression and then patches up the non-active slots of the result.

Now naturally an arbitrary mathematical function is hard to represent in a programming language. We have a spectrum of break rules of different power:

- One-level break rule: This functions like in the logic and only inspects the tag. This is the easiest to translate to machine code.
- Multi-level break rule: This uses the terms-of-terms isomorphism to allow matching arbitrary first-order terms. A little more powerful but basically syntax sugar.
- Term rewriting system: This uses the reduction-to-normal-form relation of a term rewriting system. It encompasses multi-level break rules but the transitive closure and conditional rules allow more complex functions to be specified. It is quite natural to allow non-determinism at this point, i.e. a mathematical relation rather than a function.
- Logical relation: This specifies the relation as a mathematical formula, e.g. in first-order or second-order logic. This can express some relations that a TRS cannot, but is hard to execute.

### Structural rules

#### Identity

Identity is a bit weird because on the one hand (with unityping) every identity is the same derivation, $Slot \vdash Slot$, but on the other hand every occurrence of the rule is unique and if we mix up the mapping of the identity slots with another identity rule's occurrence then we're screwed. But as long as we preserve the nesting structure of expressions we're fine.

#### Cut

Cut is a bit tricky because of course there is cut-elimination which can eliminate most cuts. But then because we are working in a unityped language we can also cut non-cut-eliminable things, and then cut isn't eliminable because it's invalid. But then with rewriting it is potentially valid. So in the end the cut elimination theorem isn't too important except that it makes the substitution calculus convergent.

#### Exponentials

There are the exponential rules. It is a bit weird to have these in a unityped language - we can generally identify exponentials even after erasing types from the relevant uses of explicit promotion/dereliction rules, and arguably we could adopt classical logic where the use of contraction/weakening is unrestricted. But I have preserved it nonetheless:

- The unityping allows us to consider "nonsensical" derivations like derelicting a jumbo rule - such a cut doesn't reduce under the basic substitution calculus, but it can be pattern-matched in a rewriting system and thus can be assigned some meaning.
- Restricting the applicability of contraction/weakening in the substitution calculus seems like it will make it easier to do TRS pattern-matching, per {cite}`smithOptimalSharingGraphs2017`. Also, in the non-rewriting portion of the core, it seems like it will be possible to identify boxes with memory allocations and avoid allocating a node for each expression.
- General reasons for preferring linear logic to classical logic, as specified in {ref}`Commentary/Language/Logic:Logic style`.
- The promotion rule is actually distinguished, in that a cut with weakening/contraction must operate on a promotion rule and not arbitrary expressions.

As far as the mechanics, I think the promotion, dereliction, weaking, and contraction rules are pretty clear. I am not sure about including the admissible rules; I will leave them out for now, for simplicity, and if there is a performance bottleneck where there are a lot of trivial cuts that could be avoided by admissible rules, then I will consider adding them in.

#### Exchange

Another issue is tracing the flow of slots from each rule to the next. I am thinking something like we just assign symbolic names to each slot. It would also be possible to make some sort of de Brujin-type syntax.

### Type nodes

There are a fair number of nodes that don't affect the structure of proofs at all - they have the same number of slots in/out and just change the types. These we treat as a group, really they don't affect the proof structure and we can just cancel them out where they occur. Besides implementing higher-order logic I am not sure they are necessary for anything. But, when we move to higher-order logic and want to search for proofs that prove a given proposition, then it is important to have these rules. Although we could just model them as a jumbo term `forall x A`, it does solve a few problems to use proper constructors, like how to make cut-elimination do type substitution.

#### Substitution

Substitution maps slots to slots, and then there is the extra $t \vdash t$ which is not even written in most presentations. There is a mechanical substitution operation, which finds the occurrences of the identity $x \vdash x$ and grafts $t \vdash t$ to replace those, but we already have symbols and symbol substitution which makes more sense in an unityped language.

#### Quantifiers

These also have type-level substitution involved, but it is more explicit in that we can recover the type `t` from the structure of the proof. It is basically trivial to add them, the only concern is that these constructors are unnecessary in most cases.

#### Sets

Sets are similarly trivial, the set definition just unpacks.

#### Definitions

And also definitions, they are type-level and similarly trivial at the proof level.

- Use/def: We must have a global map from identifiers to definitions, and use nodes then list these identifiers

## SCC

An SCC value is a strongly connected component of expressions that reduce to each other but not anything else. For example `\(\x. x)(\x. x)` is an SCC because it reduces to itself and not anything else. It is a bit strange as a value though because there may be multiple ways of writing it; for example with the rewrite system `a = b; b = a`, both `a` and `b` refer to the same SCC. Because of this strangeness SCCs are specially handled by the execution engine.

Another way to handle SCCs is to just not allow them to reduce to anything in the SCC, I am unsure about this. The approach of letting every expression represent an SCC rather than itself is on firmer theoretical footing.

## Infinite terms

According to the infinite term reduction semantics, we may get an infinite term, defined as a Cauchy sequence of finite terms or simply a Cauchy sequence of terms. If there are no reduction of this infinite term then it is a value. For example with a definition `x = [1,...x]` we may get the infinite list `x = [1,1,1,...]`. To avoid issues with nontermination, infinite terms also need special handling by the execution engine.

There are two basic representations of infinite terms:

- regular terms - these are terms where only a finite set of subtrees occur. So we can model the term as a set of equations, like `x = 1 : y; y = 2 : x` for the list `[1,2,1,2,...]`.
- isolated terms - the intuition I have for these is that they are like Mathematica's `Root` or `RootInterval` expressions. They identify a unique term, have some pre-computed approximation (upper/lower bounds), and provide a method to compute it further to arbitrary precision. So when we reduce an expression, we get a set of isolated terms. The key is that they are isolated/unique so we don't have to worry about nondeterminism or an expression representing multiple values.

## Logical types

As well as proofs-as-programs, there are also propositions-as-types. And with dependent types, types are values. So we must have all of the following:

- jumbo connectives of lists of two-sided sequent types
- the exponential connective
- the forall/exists connectives
- definition and set connectives

I think though that these can all be modeled naturally as jumbo terms - we just need special symbols to use as tags.

## Values

Ideally, values are normal forms, meaning they are not subject to evaluation, reduction, or computation. For example, "1/3" and "let x=3 in 1/x" may be equivalent under reduction, but only "1/3" is considered a value because the second can be further reduced to the first. WHNF is not sufficient to ensure a value, e.g. `[1,undefined]` reduces to `undefined` hence is not a value. In practice, reduction rules are dependent on context - therefore, we loosely consider all expressions as values. This is safe in Stroscot because per our term-reduction semantics every expression reduces to at least one value. If we know that an expression is not ambiguous, i.e. it reduces only to one value/normal form, we can naturally confuse writing the normal form with writing the expression that reduces to that normal form.
