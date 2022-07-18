{-
I = λx.x
K = λx.λy.x
ω = λx.x x
test = (λzy.y(zI)(zK)) ω
-}

data X a = X { unX :: X a -> a }
i :: p -> p
i = \x -> x
k :: p1 -> p2 -> p1
k = \x y -> x
ω :: X a -> a
ω = \x -> unX x x
test = (\z y -> y (z (X i)) (z (X k))) ω

test2 :: ((a -> a) -> (d -> c -> e -> c) -> b) -> b
test2 = \y -> y (\x0 -> x0) (\y0 -> (\x -> (\y1 -> x)))
-- λ y . y (λ x . x) (λ y . λ x . λ y . x)

t3 = test2 (\x y -> (x 3, y 1 "y" 2))

(test {x -> {y -> (pair (x 3) (((y "a") "b") "c")) } })