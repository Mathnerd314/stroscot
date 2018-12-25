Implementing delimited continuations relies on transforming the program. 

First we define a name supply for prompts:
```
n = 0
newPrompt = n++
```

Then withSubCont propagates upwards:
```
(withSubCont p k) e = withSubCont p (\x. k (x e))
v (withSubCont p k) = withSubCont p (\x. k (v x))
\x.(withSubCont p k) -- keep evaluating
pushPrompt q (withSubCont p f) | p /= q = withSubCont p (\x. pushPrompt q (f x))
```
Finally it meets a matching pushPrompt:
```
pushPrompt p (e [withSubCont p f]) = f (\y. e y)
pushPrompt p v = v
```
The first is if withSubCont appears, the second is if it doesn't. 

And a naked withSubCont results in the "Prompt not found" exception.

pushSubCont can be safely ignored as a no-op type conversion.
