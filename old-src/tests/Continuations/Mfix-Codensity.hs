{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns -fdefer-typed-holes #-}
{-# LANGUAGE TupleSections, RankNTypes #-}

import Control.Monad.Cont
import Control.Monad.Fix
import Data.Functor((<&>))
import qualified Data.IORef as IORef
import GHC.IO.Unsafe ( unsafeDupableInterleaveIO )

newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b
  }


instance MonadTrans Codensity where
  lift :: Monad m => m a -> Codensity m a
  lift m = Codensity (m >>=)

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity a = runCodensity a return

{-
  return x = \c -> c x
  m >>= k = \c -> m (\x -> k x c)
  fmap f m = \c -> m (\k -> c (f k))
-}
instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity (\k -> m (\x -> k (f x)))
instance Applicative (Codensity f) where
  pure x = Codensity (\k -> k x)
  Codensity f <*> Codensity g = Codensity (\bfr -> f (\ab -> g (\x -> bfr (ab x))))
instance Monad (Codensity f) where
  return = pure
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

-- potential MonadFix instances from https://github.com/ekmett/kan-extensions/issues/64
-- erkokValueRecursionMonadic2002 argues that no plausible definition exists for ContT, based on type inhabitants, but ContT is a different type

mmtl_mfix :: (MonadFix m) => (a -> Codensity m a) -> Codensity m a
mmtl_mfix f = lift (mfix (lowerCodensity . f))

io_mfix :: MonadIO m => (a -> Codensity m a) -> Codensity m a
io_mfix f = Codensity $ \k -> do
  promise <- liftIO $ IORef.newIORef (error "result eval'd too early")
  ans <- liftIO $ unsafeDupableInterleaveIO (IORef.readIORef promise)
  runCodensity (f ans) $ \a -> do
    liftIO $ IORef.writeIORef promise a
    k a

state_mfix :: MonadFix m => (a -> Codensity m a) -> Codensity m a
state_mfix f = Codensity (\ka -> mfixing (\a -> runCodensity (f a) ka<&>(,a)))
    where mfixing f = fst <$> mfix (\ ~(_,a) -> f a )


mfixC :: (a -> (a -> r) -> r) -> (a -> r) -> r
mfixC f ka = fst $ fix (\ ~(_,a) -> (f a ka,a))

mfixx mfix2 f = ContT (\ka -> mfixing (\a -> runContT (f a) ka<&>(,a)))
  where mfixing f = fst <$> mfix2 (\ ~(_,a) -> f a )

data Fudget a = Val a
  | Put Char (Fudget a)
  | Get (Char -> Fudget a)

instance Functor Fudget
instance Applicative Fudget where
  pure = Val

instance Monad Fudget where
  (>>=) :: Fudget t -> (t -> Fudget a) -> Fudget a
  (>>=) (Val a) f = f a
  (>>=) (Put c m) f = Put c (m >>= f)
  (>>=) (Get h) f = Get (\c -> h c >>= f )


fudget_mfix :: (a -> Fudget a) -> Fudget a
fudget_mfix f = case f undefined of
  Val _ -> fix (f . unVal )
  Put c _ -> Put c (fudget_mfix (unPut . f ))
  Get _ -> Get (\c -> fudget_mfix (unGet c . f ))

unVal (Val a) = a
unPut (Put _ m) = m
unGet c (Get h) = h c

cfudget_mfix ::  (a -> Codensity Fudget a) -> Codensity Fudget a
cfudget_mfix f = lift $ fudget_mfix (\a -> lowerCodensity (f a))

cfudget_mfix_expanded ::  (a -> Codensity Fudget a) -> Codensity Fudget a
cfudget_mfix_expanded f = Codensity $ \k ->
  let uf = \a -> runCodensity (f a) in
  case uf undefined Val of
    Val _ -> k $ fix (\a -> unVal $ uf a Val)
    Put c _ -> Put c (fudget_mfix (unPut . ($Val) . uf ) >>= k)
    Get _ -> Get (\c -> fudget_mfix (unGet c . ($Val) . uf ) >>= k)

real_mfix :: (a -> Codensity Fudget a) -> Codensity Fudget a
real_mfix f = Codensity _

test_mfix :: (a -> Codensity Fudget a) -> Codensity Fudget a
test_mfix = cfudget_mfix_expanded

runFudget :: Fudget a -> String -> (String, a, String)
runFudget (Val a) inp = (" ", a, inp)
runFudget (Put c m) inp = let (o, a, r ) = runFudget m inp in ('!' : c : o, a, r )
runFudget (Get f ) [] = error "trying to Get from an empty stream!"
runFudget (Get f ) (c:cs) = let (o, a, r ) = runFudget (f c) cs in ('?' : c : o, a, r )

runCFudget :: Codensity Fudget a -> String -> (String, a, String)
runCFudget f = runFudget (lowerCodensity f)

e32 :: [Integer] -> Fudget [Integer]
e32 = \xs -> Put 'a' (Val (1 : xs))

t32 :: (String, [Integer], String)
t32 = (\(a,b,c) -> (a,take 10 b,c)) $ runCFudget (test_mfix (\a -> lift (e32 a))) "z"
-- ("!a ", repeat 1, "z")

e33 :: [Char] -> Fudget [Char]
e33 = \cs -> Get (\c -> Val (c : cs))

t33 :: (String, String, String)
t33 = (\(a,b,c) -> (a,take 10 b,c)) $ runCFudget (test_mfix (\a -> lift (e33 a))) "z"
-- ("?z ", repeat 'z', "")

e35 :: Char -> Fudget Char
e35 = \c -> Put c (Val 'a')

t35 :: (Char, String, String)
t35 = (\(a,b,c) -> (b,c,a)) $ runCFudget (test_mfix (\a -> lift (e35 a))) ""
-- ("!" ++ undefined, 'a', "") reordered to ('a', "", "!" ++ undefined)


{-

the laws for mfix are:
  mfix x c2 = case x of
    -- purity
    exists h. (\k c -> c (h k)) -> c2 (fix h)
    -- left shrinking
    exists f a. (\x c -> a (\l -> f x l c)) -> a (\l -> mfix (\x -> f x l) c2)
    -- sliding (h is strict)
    exists f h. (\x c -> f x (\k -> c (h k))) -> mfix (f . h) (\k -> c2 (h k))
    -- nesting
    exists f. (\x -> mfix (\y -> f x y)) -> mfix (\x -> f x x) c2

-}