{-# LANGUAGE NoMonomorphismRestriction, UndecidableInstances #-}
import GHC.Generics
import Control.Monad.Fix

instance MonadFix U1 where
  mfix _ = U1

instance (MonadFix f, MonadFix g, Monad (f :+: g)) => MonadFix (f :+: g) where
  mfix f = case f undefined of
   L1 _ -> L1 $ mfix (\x -> case f x of L1 y -> y)
   R1 _ -> R1 $ mfix (\x -> case f x of R1 y -> y)

instance (Functor f, Functor g) => Applicative (f :+: g) where
instance (Functor f, Functor g) => Monad (f :+: g) where

f, f2, f3 :: [Integer] -> Maybe [Integer]
f = const Nothing
f2 = \x -> Just (take 100 (1:x))
f3 = Just
test f = mfix f == to1 (mfix (from1 . f))
main = print [test f, test f2, test f3]
