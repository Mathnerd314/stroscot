Delimited continuations
#######################

The API for delimited continuations is:

-  **newPrompt** --- creates a fresh prompt, distinct from all other
   prompts. It's just an object that we use as an identifier, basically.
   Some systems simply use symbols for prompts.
-  **pushPrompt** p e --- pushes prompt p on the stack, and executes
   expression e in this new context. This *delimits* the stack, so we
   can later capture a delimited continuation up to this part of the
   program.
-  **withSubCont** p f --- aborts (unwinds) up to and including the
   prompt p, and calls the function f with a single argument k
   representing the delimited continuation from the call to withSubCont
   up to but not including the prompt. This *captures* a delimited
   continuation, analogous to how call/cc captures an undelimited
   continuation. k can then be applied like any other function.
-  **pushSubCont** k v ---  evaluates its first subexpression to yield a subcontinuation, then evaluates its second subexpression in a continuation that composes the subcontinuation with the current continuation.

This is based on :cite:`dyvbigMonadicFrameworkDelimited2007`.

An example from the paper is the following:

::

   p = newPrompt;
   2 + pushPrompt p
         if withSubCont p
              (\k. pushSubCont k False + pushSubCont k True)
           then 3 else 4
   # result: 2+4+3=9

First we call newPrompt to obtain a fresh prompt and give it a name, p,
for the prompt. Then we have the addition of 2 to a second expression,
that pushPrompt's p on the stack, i.e. delimits the stack at that point.
The if is called inside the new context delimited by p. The if's test
expression is a withSubCont p, so it calls the function with k bound to
the rest of the computation between the withSubCont and the pushPrompt,
which, by the magic of delimited control, is equivalent to the function
λb. if b then 3 else 4. We then call it once with False and once with
True as argument, and we add the two results, giving us 7. The result of
the pushPrompt is thus 7, and we return to the addition of 2, giving us
9 as the final result.

Since continuations are the mother of all monads, they can easily implement effects, state, I/O, etc.

For example, State:

::

  state = newPrompt
  get = withSubCont state (\k s -> pushSubCont k s s)
  put s = withSubCont state (\k _ -> pushSubCont k () s)
  run s e = pushPrompt state e (,) s

  # example
  run {
    x = get
    put (x + 1)
    x
  } 1

Implementation
==============

Implementing delimited continuations relies on transforming the program.

First we define a name supply for prompts:

::

   n = 0
   newPrompt = n++

Then withSubCont propagates upwards until is finds a pushPrompt:

::
   (withSubCont p k) e = withSubCont p (\x. k (x e))
   v (withSubCont p k) = withSubCont p (\x. k (v x))
   \x.(withSubCont p k) -- keep evaluating
   pushPrompt q (withSubCont p f) | p /= q = withSubCont p (\x. pushPrompt q (f x))
   pushPrompt p (e [withSubCont p f]) = f (\y. e y)
   pushPrompt p v = v

A naked withSubCont results in the "Prompt not found" exception.

pushSubCont can be safely ignored as a no-op type conversion.

Undelimited continuations
=========================

.. code-block:: none

   call allocates a frame object
      "the locals" (name -> object bindings)
      a little evaluation stack for holding temps and dynamic
         block-nesting info
      the offset to the current bytecode instruction, relative to the
         start of the code object's fixed (immutable) bytecode vector

   When a subroutine returns, it decrefs the frame and then the frame typically
   goes away; if it returns because of an exception, though, traceback objects
   may keep the frame alive.

   GENERATORS

   Generators add two new abstract operations, "suspend" and "resume".  When a
   generator suspends, it's exactly like a return today except we simply
   decline to decref the frame.  That's it!  The locals, and where we are in
   the computation, aren't thrown away.  A "resume" then consists of
   restarting the frame at its next bytecode instruction, with the retained
   frame's locals and eval stack just as they were.

   Some generator properties:

   + They're asymmetric:  "suspend" is something only a generator can do, and
   "resume" something only its caller can do (this does not preclude a
   generator from being "the caller" wrt to some other generator, though, and
   indeed that's very useful in practice).

   + A generator always returns control directly to its caller, at the point
   the caller invoked the generator.  And upon resumption, a generator always
   picks up where it left off.

   + Because a generator remembers where it is and what its locals are, its
   state and "what to do next" don't have to be encoded in global data
   structures then decoded from scratch upon entry.  That is, whenever you
   build a little (or large!) state machine to figure out "what to do next"
   from a collection of persistent flags and state vrbls, chances are good
   there's a simple algorithm dying to break free of that clutter <wink>.

   COROUTINES

   Coroutines add only one new abstract operation, "transfer".  They're fully
   symmetric so can get away with only one.  "transfer" names a coroutine to
   transfer to, and gives a value to deliver to it (there are variations, but
   this one is common & most useful).  When A transfers to B, it acts like a
   generator "suspend" wrt A and like a generator "resume" wrt B.  So A
   remembers where it is, and what its locals etc are, and B gets restarted
   from the point *it* last transfered to someone else.

   Coroutines grew up in simulation languages because they're an achingly
   natural way to model independent objects that interact with feedback.  There
   each object (which may itself be a complex system of other stuff) is written
   as an infinite loop, transferring control to other objects when it has
   something to tell them, and transferred to by other objects when they have
   something to tell it.

   A Unix pipeline "A | B | C | D" doesn't exploit the full power but is
   suggestive.  A may be written as

   while 1:
      x = compute my next output
      B.transfer(x)     # resume B with my output

   B as

   while 1:
      x = A.transfer()  # resume A to get my input
      y = compute something from x and my own history
      C.transfer(y)     # resume C with my output

   C as

   while 1:
      x = B.transfer()  # resume B to get my input
      y = compute something from x and my own history
      D.transfer(y)     # resume D with my output

   and D as

   while 1:
      x = C.transfer()  # resume C to get my input
      y = compute something from x and my own history
      print y

   If e.g. C collapses pairs of values from B, it can be written instead as

   while 1:
      # get a pair of B's
      x = B.transfer()
      y = B.transfer()
      z = f(x, y, whatever)
      D.transfer(z)     # resume D with my output

   It's a local modification to C:  B doesn't know and shouldn't need to know.
   This keeps complex algorithms manageable as things evolve.

   Initialization and shutdown can be delicate, but once the pipe is set up it
   doesn't even matter which of {A, B, C, D} first gets control!  You can view
   A as pushing results through the pipe, or D as pulling them, or whatever.
   In reality they're all equal partners.

   Why these are so much harder to implement than generators:  "transfer"
   *names* who next gets control, while generators always return to their
   (unnamed) caller.  So a generator simply "pops the stack" when it suspends,
   while coroutine flow need not  be (and typically isn't) stack-like.

   In Python this is currently a coroutine-killer, because the C stack gets
   intertwined.  So if coroutine A merely calls (in the regular sense) function
   F, and F tries to transfer to coroutine B, the info needed to resume A
   includes the chunk of the C stack between A and F.  And that's why the
   Python coroutine implementation I referenced earlier uses threads under the
   covers (where capturing pieces of the C stack isn't a problem).

   Early versions of coroutines didn't allow for this, though!  At first
   coroutines could only transfer *directly* to other coroutines, and as soon
   as a coroutine made "a regular call" transfers were prohibited until the
   call returned (unless the called function kicked off a brand new collection
   of coroutines, which could then transfer among themselves -- making the
   distinction leads to convoluted rules, so modern practice is to generalize
   from the start).

   Then the current state of each coroutine was contained in a single frame,
   and it's really no harder to implement than generators.  Knuth seems to have
   this restricted flavor of coroutine in mind when he describes generator
   behavior as "semi-coroutine".

   CONTINUATIONS

   Given the pedagogical structure so far, you're primed to view continuations
   as an enhancement of coroutines.  And that's exactly what will get you
   nowhere <wink>.  Continuations aren't more elaborate than coroutines,
   they're simpler.  Indeed, they're simpler than generators, and even simpler
   than "a regular call"!  That's what makes them so confusing at first:
   they're a different *basis* for *all* call-like behavior.  Generators and
   coroutines are variations on what you already know; continuations challenge
   your fundamental view of the universe.

   Legend has it they were discovered when theorists were trying to find a
   solid reason for why goto statements suck:  the growth of "denotational
   semantics" (DS) boomed at the same time "structured programming" took off.
   The former is a solid & fruitful approach to formally specifying the
   semantics of programming languages, built on the lambda calculus (and so
   dear to the Lisp/Scheme community -- this all ties together, of course
   <wink>).

   The early hope was that goto statements would prove to present intractable
   problems for formal specification, and then "that's why they suck:  we can't
   even sort them out on paper, let alone in practice".

   But in one of God's cleverer tricks on the programming world <tee hee>, the
   semantics of goto turned out to be trivial:  at a branch point, you can go
   one of two ways.  Represent one of those ways by a function f that computes
   what happens if you branch one way, and the other way by a function g.  Then
   an if+goto simply picks one of f or g as "the continuation" of the program,
   depending on whether the "if" condition is true or false.  And a plain goto
   simply replaces the current continuation with a different one (representing
   what happens at the branch target) unconditionally.  So goto turned out to
   be simpler (from the DS view) than even an assignment stmt!

   I've often suspected theorists were *surprised* (and maybe appalled <0.7
   wink>) when the language folks went on to *implement* the continuation idea.
   Don't really know, but suppose it doesn't matter anyway.  The fact is we're
   stuck with them now <wink>.

   In theory a continuation is a function that computes "the rest of the
   program", or "its future".  And it really is like a supercharged goto!  It's
   the formal DS basis for all control flow, from goto stmts to exception
   handling, subsuming vanilla call flow, recursion, generators, coroutines,
   backtracking, and even loops along the way.

   To a certain frame of mind (like Sam's, and Christian is temporarily under
   his evil influence <wink>), this relentless uniformity & consistency of
   approach is very appealing.  Guido tends to like his implementations to
   mirror his surface semantics, though, and if he has ten constructs they're
   likely to be implemented ten ways.  View that as a preview of future battles
   that have barely been hinted at so far <0.3 wink>.

   Anyway, in implementation terms a continuation "is like" what a coroutine
   would be if you could capture its resumption state at any point (even
   without the coroutine's knowledge!) and assign that state to a vrbl.  So we
   could say it adds an abstract operation "capture", which essentially
   captures the program counter, call stack, and local (in Python terms) "block
   stack" at its point of invocation, and packages all that into a first-class
   "continuation object".  IOW, a building block on top of which a generator's
   suspend, and the suspend half of a coroutine transfer, can be built.  In a
   pure vision, there's no difference at all between a regular return and the
   "resume" half of a coroutine transfer:  both amount to no more than picking
   some continuation to evaluate next.

   A continuation can be captured anywhere (even in the middle of an
   expression), and any continuation can be invoked at will from anywhere else.
   Note that "invoking a continuation" is *not* like "a call", though:  it's
   abandoning the current continuation, *replacing* it with another one.  In
   formal DS this isn't formally true (it's still "a call" -- a function
   application), but in practice it's a call that never returns to its caller
   so the implementation takes a shortcut.

   Like a goto, this is as low-level as it gets, and even hard-core
   continuation fans don't use them directly except as a means to implement
   better-behaved abstractions.

   As to whether continuations have "volatile state", I'm not sure what that
   was asking.  If a given continuation is invoked more than once (which is
   something that's deliberately done when e.g. implementing backtracking
   searches), then changes made to the locals by the first invocation are
   visible to the second (& so on), so maybe <wink> the answer is "yes".  It's
   more accurate to think of a continuation as being immutable, though:  it
   holds a reference to the structure that implements name bindings, but does
   not copy (save or restore) the bindings.

   Quick example, given:

   (define continuation 0)

   (define (test)
   (let ((i 0))
      (call/cc (lambda (k)
                  (set! continuation k)))
      (set! i (+ i 1))
      i))

   That's like the Python:

   def test():
      i = 0
      global continuation
      continuation = magic to resume at the start of the next line
      i = i + 1
      return i

   Then (this is interactive output from a Scheme shell):

   > (test) ; Python "test()"
   1
   > (continuation) ; Python "continuation()"
   2
   > (continuation)
   3
   > (define thisguy continuation) ; Python "thisguy = continuation"
   > (test)
   1
   > (continuation)
   2
   > (thisguy)
   4
   >

   too-simple-to-be-obvious?-ly y'rs  - tim


