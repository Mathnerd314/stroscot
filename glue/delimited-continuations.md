Implementing delimited continuations relies on transforming the program. 

First we define a name supply for prompts:
```
n = 0
newPrompt = n++
```

Then there's one rewrite rule:
```
pushPrompt p (e [withSubCont p f]) = f (\y. e y)
pushPrompt p v = v
```
The first is if withSubCont appears, the second is if it doesn't. A naked withSubCont results in the "Prompt not found" exception.

pushSubCont can be safely ignored as a no-op type conversion.
