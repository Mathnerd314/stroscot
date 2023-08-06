
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE NoMonomorphismRestriction, UndecidableInstances #-}
import GHC.Generics
import Control.Monad.Fix
import Control.Applicative
import Data.Kind(Type)

instance MonadFix U1 where
  mfix _ = U1

instance (MonadFix f, MonadFix g, Monad (f :+: g)) => MonadFix (f :+: g) where
  mfix f = case f undefined of
   L1 _ -> L1 $ mfix (\x -> case f x of L1 y -> y)
   R1 _ -> R1 $ mfix (\x -> case f x of R1 y -> y)

instance (Functor f, Applicative g) => Applicative (f :+: g) where
  pure = R1 . pure

instance (Monad f, Monad g) => Monad (f :+: g) where
  L1 l >>= f = L1 $ l >>= (\x -> case f x of L1 y -> y)
  R1 r >>= g = R1 $ r >>= (\x -> case g x of R1 y -> y)

type    Generically1 :: forall k. (k -> Type) -> (k -> Type)
newtype Generically1 f a where
  Generically1 :: forall {k} f a. f a -> Generically1 @k f a

-- | @since 4.18.0.0
instance (Generic1 f, Eq (Rep1 f a)) => Eq (Generically1 f a) where
   Generically1 x == Generically1 y = from1 x == from1 y
   Generically1 x /= Generically1 y = from1 x /= from1 y

-- | @since 4.18.0.0
instance (Generic1 f, Ord (Rep1 f a)) => Ord (Generically1 f a) where
   Generically1 x `compare` Generically1 y = from1 x `compare` from1 y

-- | @since 4.17.0.0
instance (Generic1 f, Functor (Rep1 f)) => Functor (Generically1 f) where
  fmap :: (a -> a') -> (Generically1 f a -> Generically1 f a')
  fmap f (Generically1 as) = Generically1
    (to1 (fmap f (from1 as)))

  (<$) :: a -> Generically1 f b -> Generically1 f a
  a <$ Generically1 as = Generically1
    (to1 (a <$ from1 as))

-- | @since 4.17.0.0
instance (Generic1 f, Applicative (Rep1 f)) => Applicative (Generically1 f) where
  pure :: a -> Generically1 f a
  pure a = Generically1
    (to1 (pure a))

  (<*>) :: Generically1 f (a1 -> a2) -> Generically1 f a1 -> Generically1 f a2
  Generically1 fs <*> Generically1 as = Generically1
    (to1 (from1 fs <*> from1 as))

  liftA2 :: (a1 -> a2 -> a3)
         -> (Generically1 f a1 -> Generically1 f a2 -> Generically1 f a3)
  liftA2 (·) (Generically1 as) (Generically1 bs) = Generically1
    (to1 (liftA2 (·) (from1 as) (from1 bs)))

-- | @since 4.17.0.0
instance (Generic1 f, Alternative (Rep1 f)) => Alternative (Generically1 f) where
  empty :: Generically1 f a
  empty = Generically1
    (to1 empty)

  (<|>) :: Generically1 f a -> Generically1 f a -> Generically1 f a
  Generically1 as1 <|> Generically1 as2 = Generically1
    (to1 (from1 as1 <|> from1 as2))

instance (Generic1 f, Monad (Rep1 f)) => Monad (Generically1 f) where
  (Generically1 a) >>= f = Generically1 . to1 $ from1 a >>= (\x -> case f x of Generically1 a -> from1 a)

f, f2, f3 :: [Integer] -> Maybe [Integer]
f = const Nothing
f2 = \x -> Just (take 100 (1:x))
f3 = Just
test f = mfix f == to1 (mfix (from1 . f))
main = print [test f, test f2, test f3]
