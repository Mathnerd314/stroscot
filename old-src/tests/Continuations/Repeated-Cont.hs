{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, NoImplicitPrelude #-}

import Prelude(($),Either(..))

newtype Codensity f a = Codensity { runCodensity :: forall r. (a -> f r) -> f r }

{-
Codensity Maybe Bool
forall r. (Bool -> Maybe r) -> Maybe r

t = \_ -> Nothing
t = \a -> a True
t = \a -> a False

Codensity (Codensity Maybe) Bool
forall r. (Bool -> Codensity Maybe r) -> Codensity Maybe r
forall r s.
  (Bool -> forall t. (r -> Maybe t) -> Maybe t) -> (r -> Maybe s) -> Maybe s

t = \_ _ -> Nothing
t = \a b -> b (a True (const Nothing))

-}

liftCD :: Codensity m a -> Codensity (Codensity m) a
-- forall r. (a -> m r) -> m r
-- -> forall r s. (forall t. a -> (r -> m t) -> m t) -> (r -> m s) -> m s
liftCD m = Codensity $ \k -> Codensity $ \c -> runCodensity m (\x -> runCodensity (k x) c)

lowerCD :: Codensity (Codensity m) a -> Codensity m a
lowerCD a = runCodensity a (\x -> Codensity $ \c -> c x)

{-

liftCD (lowerCD f) == f?

lowerCD f = f (\x c -> c x)
liftCD (lowerCD f) =
  \k c -> f (\x c -> c x) (\x -> k x c)

Example: Codensity (Codensity Maybe) Bool, t = \a b -> b (a False (const Nothing))
liftCD (lowerCD f) =
  \k c -> (\x -> k x c) ((\x c -> c x) False (const Nothing))
  \k c -> (\x -> k x c) (const Nothing False)
  \k c -> (\x -> k x c) Nothing
  \k c -> k Nothing c

Conclusion: not equal

-}


newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
return :: a -> ContT r m a
return x = ContT $ \c -> c x
(>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
m >>= k = ContT $ \c -> runContT m (\x -> runContT (k x) c)

liftCT :: ContT r m a -> ContT s (ContT r m) a
-- liftCT :: ((a -> m r) -> m r) -> (a -> (s -> m r) -> m r) -> (s -> m r) -> m r
liftCT m = ContT $ \k -> ContT $ \c -> runContT m (\x -> runContT (k x) c)

lowerCT :: ContT a (ContT r m) a -> ContT r m a
lowerCT a = runContT a (\x -> ContT $ \c -> c x)

squashCT :: ContT s (ContT r m) a -> ContT r m (Either (a, s -> m r) s)
squashCT a = ContT $ \e -> runContT (runContT a (\c -> ContT $ \d -> e (Left (c,d)))) (\k -> e (Right k))

{-
(a -> (s -> m r) -> m r) -> (s -> m r) -> m r
(Either (a, s -> m r) s -> m r) -> m r
ContT r m (Either (a, s -> m r) s)
-}