import Data.List.Ordered (minus, union, unionAll)

-- Generators/streams:

fromThenTo n m
| n > m = []
| otherwise = n:(fromThenTo (n+1) m)

fromThen n = n:(fromThen (n+1))

-- Sieve of Eratosthenes
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

-- composition, like Unix pipes.
composition = take 10 primes

{-
Hamming numbers example (quoted from "A theory of nondeterminism, parallelism,
and concurrency" by M. Broy, "Theoretical Computer Science" vol 45, pp1-61,
example on page 9.
A program is required which generates the infinite stream of all numbers
greater than 1 of the form 2^i*3^j*4^k in ascending order.
To me, this is nicer than other solutions (eg Dijkstra. "A discipline of
programming" P129) I have seen or could construct myself.
-}
streammult n s = (n * head s) : streammult n (tail s)
merge s1 s2 =
  if head s1 <= head s2
then head s1 : merge (tail s1) s2
else head s2 : merge s1 (tail s2)

s1 = streammult 5 (1 : s1)
s2 = merge (streammult 3 (1 & s2)) s1
s3 = merge (streammult 2 (1 & s3)) s2

{-
Oh!! BTW, lazy lists also allows the programmer to code things that s/he would
need 'static' variables for in 'C' -- like a random number generator -- since
there's no place for the 'seed' in a purely applicative order program. A lazy
program could define randoms() as follows to supply an infinite list of random
numbers...
-}
my_randoms = randoms start_seed -- start_seed is constant
randoms n = nh : randoms nh
  where
    nh = some_seed_hasher n

{-
Where clauses
This Miranda definition expresses a well-known algorithm for multiplying
without using multiplication (except multiplication and division by 2, which
are easy in binary).

mult x n = x*n

The "where" part is a convenient notation, saving some typing and
clarifying the structure (by making explicit the fact that the same value y is
calculated for both the 2nd and 3rd lines). But if y is evaluated eagerly, then
this function as written diverges. Therefore for an eager implementation extra
information is needed to say which cases y is calculated for.

My point was that this kind of solution involves a minute bit of extra
scheduling responsibility to say that the where/let clause should only be
evaluated in the last two cases, whereas the lazy solution can leave that to
the compiler and run-time system.

-}
mult :: Int->Nat->Int
mult x n
| n == 0 = 0
| n > 0 && n mod 2 == 0 = y
| otherwise = y+x
where y = 2*(mult x (n div 2))

{-
Circular programs.
Richard Bird wrote a paper about this (Acta Informatica 21, 239-250 (1984)).

Example: Suppose you have a list of natural numbers and you want to
subtract the least element of the list from all elements. In a lazy
language, you can do it all in one list traversal.
The first argument of "norm" is like an inherited attribute,
it has no value yet when "norm" is called from "normalize".
But when "norm" comes back, it returns (synthesized attribute)
the changed list and the minimum of the list
and the local declaration in "normalize" puts the two attributes together.
This only works, because the subtractions (x-m) in the result of
"norm" are delayed, their result is not needed for "norm" to proceed.
-}
normalize [] = []
normalize ls = res
  where (m,res) = norm m ls
norm m [x] = (x,[x-m])
norm m (x:xs) = (min x m',x-m : res)
  where (m',res) = norm m xs


accumulate f a [] = a
accumulate f a (b::x) = accumulate f (f a b) x
reduce f a [] = a
reduce f a (b::x) = f b (reduce f a x)

x appendR y = reduce cons y x
x appendA y = revonto y (rev x)
  where
  rev x = revonto [] x
  revonto y x = accumulate consonto y x
  consonto y a = a :: y

{-
accumulate is fine in a non-lazy language, but for a lazy language
it has the serious flaw that it returns no result until it has seen its
entire argument. On the other hand,
reduce is capable of yielding a result without first seeing the entire argument.
That means that appendR's behavior will be quite different in a lazy language from
appendA. x appendA omega = omega for all x (where omega is the
`undefined' or `nonterminating' value).
-}

myand False x = False
myand x False = False
myand True True = True

test = myand undef False == False
{-
This test fails in Haskell.
As a programmer, I think of the equations I write down as equations
and not as nested case-expressions, and I would like to be able to
manipulate them as I usually manipulate equations.
In other words: I would like the semantics to keep the promises
the syntax makes.
-}
