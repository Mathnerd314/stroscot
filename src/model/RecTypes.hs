-- goal: type (\x. x (\w. w) x x) (\y. (\x. x x) (y z))

{-
newtype S a = S { s :: (T a -> T a) -> S a -> S a -> a}
newtype T a = T { t :: T a -> S a -> S a -> a}

i = \w -> w
f2 = \x -> s x i x x

o = \x -> t x x

g2 z = \y -> o (y z)

test z = f2 (S (g2 z))

i :: p -> p
f2 :: S a -> a
o :: T a -> S a -> S a -> a
g2 :: T a -> (T a -> T a) -> S a -> S a -> a -- T a -> S a
test :: T a -> a
-}

-- goal: type o $ \h. o (h I), o = \x. x x, I = \x. x

newtype T = T { t :: T -> T }

o = \x -> t x x
i = \x -> x
f = \h -> o (t h (T i))
m = o . T $ f

o :: T -> T
i :: p -> p
f :: T -> T
m :: T
