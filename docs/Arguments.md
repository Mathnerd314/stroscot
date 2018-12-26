Stroscot supports
* keyword arguments
```
foo w x y z = z - x / y * w

v = foo (y:2) (x:4) (w:1) (z:0)
# 0-4/2*1
v == foo {x:4,y:2,w:1,z:0}
# true
```
* positional arguments
```
v == foo 1 4 2 0
# true
```
* Mixtures of the above
```
v == foo {z:0} {w:1} 4 2
# true
```
* implicit (dynamically-scoped) arguments
```
bar = foo + 2
baz = bar {x:4,y:2}

v + 2 == baz {z:0,w:1}
# true
v + 2 == baz 1 _ _ 0
# true
```

Explicit argument passing cannot replace our implicit variable example, because in general the author of the system cannot know what all the parameters will be. Imagine that bar is part of the standard library, a complex function. The variables x and y do not exist in the standard library; it is part of the user's code. To use explicit argument passing would require adding a new argument to bar, but this would break anyone else using the library. Not to mention that just one intervening function is rare and we'd probably need to modify 20 or 30 functions in a bad case.

A typical usage example is logging; the decision of whether to enable logging is generally a flag defined close to the top level, but each use site is scattered in the code. An implicit loglevel argument replaces the global variable that is often used.

Using positional arguments inhibits passing positional arguments implicitly:
```
bar = foo + 2
baz a = bar {x:4,y:2} - a

v + 2 == baz 0 {z:0,w:1}
# true
v + 2 == baz 1 _ _ 0
# Error: too many arguments to baz
```
Similarly keyword arguments inhibit passing down that keyword implicitly:
```
a k = 1
b k = k + a

b {k:2}
# Error: no definition for k given to a
```
A proper definition for b would simply omit k:
```
a k = 1
b = k + a

b {k:2}
# 3
```
For functions with no positional arguments, positions are assigned implicitly left-to-right:
```
a = x / y + y
a 4 1
# 5
```
Atoms that are in lexical scope are not assigned positions, hence (/) and (+) are not implicit positional arguments for a in the example above. But they are implicit keyword arguments:
```
a = x / y + y
a {(+):(-)} 4 1
# 4/1-1=3
```
The namespace scoping mechanism protects against accidental use in large projects.
