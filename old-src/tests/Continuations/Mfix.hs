{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, RankNTypes #-}

import Data.Maybe

mfix f = case fmap f (f undefined) of
  Nothing -> Nothing
  Just _ -> return (fix (unJust . f))



{-
It might or might not be possible to implement mfix as a function (lambda / letrec mixture)
that works for all continuation types.
`This implementation <https://stackoverflow.com/questions/25827227/why-cant-there-be-an-instance-of-monadfix-for-the-continuation-monad>`__
looks wrong, it uses ``let a=a`` so will diverge too often.

None of the purported proofs of impossibility look too convincing
I don't care about types, I just want to know if there's any untyped lambda that works.

return x = \k -> k x
m >>= h = \k -> m (\v -> h v k)
fmap f m = \c -> m (\r -> c (f r))

mfix (return . h)
= mfix (\b -> return (h b))
= mfix (\b k -> k (h b))
= \k -> k (let a = h a in a)
= \k -> k (fix h)
= return (fix h)

mfix (\x -> a >>= f x)
= mfix (\x k -> a (\v -> f x v k))
= \k -> a (\v -> mfix (\x -> f x v) k)
= \k -> a (\v -> (\y -> mfix (\x -> f x y)) v k)
= a >>= \y -> mfix (\x -> f x y)

mfix (fmap h . f)
 = mfix (\x -> fmap h (f x))
 = mfix (\x c -> f x (\r -> c (h r)))
 = \c -> mfix (f . h) (\r -> c (h r))
 = fmap h (mfix (f . h))

mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)

Goal: search for a GHC core term for mfix that satisfies the rules
it's only lambdas, so the choices are:
  = Var	  Id
  | App   (Expr b) (Expr b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)

We can make all bindings top-level by changing
  \x -> let a = f x in g a
  to
  let a = \x -> f x in \x -> g (a x)
Furthermore we can pull out all nested lambdas that are not at the beginning:
  \x -> a (\y -> b x)
  to
  let
    f = \x -> \y -> b
  in \x -> a (f x)
Writing x = \a -> \b -> ... as x a b, we have our structure:
  [(Id,[Id],Expr)]
  Expr = Id | App Expr Expr

mfix (\b k -> k (h b)) = \k -> k (let a = h a in a)
mfix (\x k -> a (\v -> f x v k)) = \k -> a (\v -> mfix (\x -> f x y) v k)
mfix (\x c -> f x (\r -> c (h r))) = \c -> mfix (f . h) (\r -> c (h r))
mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)

mfix d k =
  case () of
    () | d x c == c (h x) ->  k r
    () | d x c == a (fliplx x c) -> a dv
    () | d x c == f x (\r -> c (g r)) -> mfix fg dr
    () | d x c == mfix (o x) c -> mfix (\x -> o x x) k
  where
    x = undefined
    c = undefined
    h _ = undefined
    a q = undefined
    l x v c = undefined
    f = undefined
    g = undefined
    o = undefined

    r = h r
    dv v = mfix (flipl v) k
    fliplx x c v = l x v c
    flipl v x = l x v
    fg x = f (g x)
    dr r = k (g r)

-}

