module Example where

{-}
foo 0 = 1
foo n =
  let a = \x -> (foo (n - 1)) in
  a 2 + a 3

main = print (foo 3)
-}

dbl g w = g (g w)
y h = dbl (\w -> h (h w))
x = dbl y
