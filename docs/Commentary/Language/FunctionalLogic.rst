Functional logic
################

Curry, Verse, Flix, Oz, ... all functional logic languages.

Values. A value v is either a variable x or a head-normal form hnf. A variable counts as a value because an expression may evaluate to an as-yet-unknown logical variable.
A head-normal form hnf is a conventional value.
Expressions e include values v and other constructs:

* sequencing 𝑒𝑞; e. An "expression or equation" 𝑒𝑞 is either an ordinary (possibly failing) expression e, or an equation v = e; the syntax ensures that equations can only occur to the left of a “; ”.
* ∃x. e - existential, introduces logical variable
* fail - yield no values
* choice e1 || e2 - yield multiple values (e1 followed by e2)
* one{e} - if e fails, fails, otherwise returns the first of the values yielded by e
* all{e} - reifies choices as a tuple; n choices mean tuple of length n

A program 𝑝 ::= one{e} is a closed expression e (freevars(𝑒) = ∅) taking the first result. If the expression fails, the program fails.

Rewrite rules:

* ∃x1 x2 ··· xn. e means ∃x1. ∃x2. ···∃xn. e
* x := e1; e2 means ∃x. x = e1; e2
* ⟨e1, ···, en⟩ means x1 := e1; ···; xn := en; ⟨x1, ···, xn⟩ where xi are fresh (again skip rebinding values)
* e1 = e2 means‡ x := e1; x = e2; x where x is fresh (skip if e1 is a value v and the equation is not the last in a sequence)
* if (∃x1 ···xn. e1) then e2 else e3 means (one{(∃x1 ···xn. e1; 𝜆⟨⟩. e2) || (𝜆⟨⟩. e3)})⟨⟩

Note: In the rules marked with a superscript 𝛼, use 𝛼-conversion to satisfy the side condition

A multi-equation pattern match such as::

  function pat1 = body1
  function pat2 = body2

desugars to

.. code-block:: none

  function = λp.((∃x1 ··· xn. p = pat1; body1) || (∃x1 ··· xn. p = pat2; body2))

Primops and literals:

* Hnfs include integer constants and primitive operators +, >
* e1 + e2 means add⟨e1, e2⟩
* e1 > e2 means gt⟨e1, e2⟩
* app-add add⟨k1, k2⟩ −→ k3 where 𝑘3 = 𝑘1 + 𝑘2
* app-gt gt⟨k1, k2⟩ −→ k1 if 𝑘1 > 𝑘2
* app-gt-fail gt⟨k1, k2⟩ −→ fail if 𝑘1 ⩽ 𝑘2
* u-lit k1 = k2; e −→ e if 𝑘1 = 𝑘2

Lambdas:

* A head-normal form hnf includes a lambda 𝜆x. e.
* Expressions e include applications v1 v2
* e1 e2 means f := e1; x := e2; f x, where f,x are fresh (skip rebinding values)
* 𝜆⟨x1, ···, xn⟩. e means 𝜆p. ∃x1 ··· xn. p = ⟨x1, ···, xn⟩; e p fresh, n ⩾ 0
* app-beta𝛼 (𝜆x. e) (v) −→ e{v/x} if 𝑥 ∉ fvs(v)
* u-lambda a=b is stuck if a or b is a lambda

Tuples:

* A head-normal form includes a tuple ⟨v1, ···, vn⟩.
* app-tup ⟨v0, ···, vn⟩(v) −→ ∃x. x = v; (x = 0; v0) || ··· || (x = n; vn) fresh x ∉ fvs(v, v0, ···, vn)
* app-tup-0 ⟨⟩(v) −→ fail
* u-tup ⟨v1, ···, vn⟩ = ⟨v′1, ···, v′n⟩; e −→ v1 = v′1; ···; vn = v′n; e
* all-choice all{v1 || ··· || vn } −→ ⟨v1, ···, vn⟩
* all-value all{v} −→ ⟨v⟩
* all-fail all{fail} −→ ⟨⟩

Failure:

* u-fail hnf1 = hnf2; e −→ fail if no unification
* u-occurs x = V [ x ]; e −→ fail if V ≠ □ (i.e., all but x=x fail)
* fail-elim 𝑋 [ fail] −→ fail
* one-fail one{fail} −→ fail
* choose-r fail || e −→ e
* choose-l e || fail −→ e

Existential:

* exi-elim ∃x. e −→ e if x ∉ fvs(e)
* eqn-elim ∃x. 𝑋 [ x = v; e ] −→ 𝑋 [ e ] if x ∉ fvs(𝑋 [ e ]) and v ≠ V [ x ]
* exi-float𝛼 𝑋 [ ∃x. e ] −→ ∃x. 𝑋 [ e ] if 𝑥 ∉ fvs(𝑋 )
* exi-swap ∃x. ∃y. e −→ ∃y. ∃x. e

Equality:

* subst 𝑋 [ x = v; e ] −→ (𝑋 {v/x}) [ x = v; e{v/x} ] if v ≠ V [ x ]
* hnf-swap hnf = v; e −→ v = hnf ; e
* var-swap y = x; e −→ x = y; e if x ≺ y

Sequences:

* seq-swap 𝑒𝑞; x = v; e −→ x = v; 𝑒𝑞; e unless (𝑒𝑞 is y = v′ and y ⪯ x)
* val-elim v; e −→ e
* seq-assoc (𝑒𝑞; e1); e2 −→ 𝑒𝑞; (e1; e2)
* eqn-float v = (𝑒𝑞; e1); e2 −→ 𝑒𝑞; (v = e1; e2)

Choice:

* one-value one{v} −→ v
* one-choice one{v || e} −→ v
* choose-assoc (e1 || e2) || e3 −→ e1 || (e2 || e3)
* choose SX [𝐶𝑋 [ e1 || e2 ] ] −→ SX [𝐶𝑋 [ e1 ] || 𝐶𝑋 [ e2 ] ]

