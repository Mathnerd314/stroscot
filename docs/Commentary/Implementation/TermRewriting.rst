Term rewriting
##############

Cycle detection
===============

So the first strategy is to prove the system is acyclic. Per :cite:`ketemaViciousCirclesRewriting2005` this includes orthogonal weakly head normalizing higher-order TRSs.

For general-purpose detection there are SCC computation algorithms; Wikipedia has a `list <https://en.wikipedia.org/wiki/Strongly_connected_component#Algorithms>`__. The DFS algorithms seem most appropriate as they can naturally be maintained during the reduction graph search; finding the normal forms of a term essentially already is a DFS. Kosaraju's algorithm is not appropriate as computing the transpose / converse of the reduction relation is not easy. Comparing Tarjan and the path-based algorithms, Tarjan uses a second index (pointer) while the path-based uses a stack. The stack manipulation of the path-based algorithm is simpler to understand than the invariants of Tarjan; the Wikipedia page for Tarjan is constantly vandalized with people who do not understand it. So I would say the path-based algorithm is better.

For associativity and commutativity there are special unification algorithms, where we represent terms as lists or bags rather than trees. There are some PhD theses and so on for this, Maude has references. I would say these are optimizations and for now acyclic detection plus general-purpose cycle handling is sufficient.

Nondeterminism
==============

This sounds a bit tricky to implement but it is not too bad. We can split into two steps:

* Find a value: Evaluate the expression to a value, any value including exceptional values. Because of the meaningless term reduction, every expression will evaluate to some kind of value. The search should be biased towards finding non-exception values but it does not need to be perfect, for example there are reduction strategies such as parallel outermost that are guaranteed to be normalizing for some classes of TRS. This is where cycle detection and infinite value handling come in.
* Non-determinism check: We can analyze confluence and the reduction sequence of the value to see if the expression can evaluate to anything else. If there are no other values or all other values are exceptions, we are done; otherwise, we handle the non-determinism appropriately, such as replacing an exception with a non-exception, erroring on multiple non-exception values at the top-level, collecting and returning the multiple values if inside a logical combinator such as ``allOf``, or skipping the non-determinism check entirely for ``oneOf``.

Check for non-exception values: Here is where we have to analyze all reduction sequences for the possibility of avoiding exceptions. , so if these apply and the expression evaluates to an exception we can rule out non-exception values. Also,
* Nondeterminism check: If , collect all of the possible values;  skip the check; if at the top-level, analyze confluence to see if the expression can evaluate to multiple distinct non-exception values, and error if multiple values.
* Find exceptional value:  There may be multiple exceptional values but we don't care; the implementation is just allowed to pick one arbitrarily.

The common notions of an ARS carry over to infinitary reductions: :cite:`endrullisInfinitaryTermRewriting2014`

* transitive reduction: irreflexive kernel of reduction closure
* normal form: irreducible term
* strongly normalizing (terminating): every infinite reduction sequence has a limit
* nonterminating reduction: infinite reduction sequence with no limit or that does not reduce to its limit
* weakly normalizing (normalizing): every term has a reduction to a normal form
* confluence: if t reduces to t1 and t2, then there is a common term s such that t1 and t2 reduce to s.
* Church-Rosser: if t1 is equivalent to t2, then there is a common term s such that t1 and t2 reduce to s.
* normal form property w.r.t. reduction:: if u reduces to t and s, and s is a normal form, then t reduces to s
* normal form property: if t is equivalent to s and s is a normal form, then t reduces to s
* unique normalization w.r.t. reduction: if t reduces to t1 and t2, and t1, t2 are normal forms, then t1=t2
* unique normalization: if t1 is equivalent to t2, and t1, t2 are normal forms, then t1=t2

However common theorems such as Newman's lemma do not, so it is not clear how useful these are.
