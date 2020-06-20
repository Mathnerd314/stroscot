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

This is based on :cite:`dyvbigMonadicFrameworkDelimited2007` except
that there is no **pushSubCont** because applying a continuation is automatic.

An example from the paper is the following:

::

   p = newPrompt;
   2 + pushPrompt p
         if withSubCont p
              (\k. k False + k True)
           then 3 else 4
   # result: 9

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
