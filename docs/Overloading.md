Stroscot supports typical pattern matching and predicate dispatch:
```
f x y = 1
f 1 y = 2
f x 2 | x == 1 = 3
```
The cases are not ordered, instead they are matched most-specific to least-specific. Specificity is defined by an SMT solver, i.e. SAT(a & not b) and UNSAT (not a & b) means that a is less specific than b.

If there is a tie regarding specificity, e.g. we were to modify the last case:
```
f x y = 1
f 1 y = 2
f x 2 = 3

f 1 2
# Error: rule conflict
```
Then it is an error.
