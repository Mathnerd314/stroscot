Types are hard. Academics have spent decades in search of the perfect type system, often not even paying attention to the work of others.


Types are implemented in Stroscot as partial functions; if the argument can't be cast to the specified type then it won't reduce.
```
i8 4
# binary 00000100
i8 129
# i8 129
```
Primitive types include ADTs, bitstring patterns, and pointers.
