{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections, RankNTypes #-}

import Control.Monad.Cont
import Control.Monad.Fix
import Data.Functor((<&>))

mfixx mfix2 f = ContT (\ka -> mfixing (\a -> runContT (f a) ka<&>(,a)))
  where mfixing f = fst <$> mfix2 (\ ~(_,a) -> f a )


newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b
  }

instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity (\k -> m (\x -> k (f x)))


instance Applicative (Codensity f) where
  pure x = Codensity (\k -> k x)
  Codensity f <*> Codensity g = Codensity (\bfr -> f (\ab -> g (\x -> bfr (ab x))))


instance Monad (Codensity f) where
  return = pure
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

instance MonadFix m => MonadFix (Codensity m) where
  mfix f = Codensity (\ka -> mfixing (\a -> runCodensity (f a) ka<&>(,a)))
    where mfixing f = fst <$> mfix (\ ~(_,a) -> f a )

mfixC :: (a -> (a -> r) -> r) -> (a -> r) -> r
mfixC f ka = fst $ fix (\ ~(_,a) -> (f a ka,a))
