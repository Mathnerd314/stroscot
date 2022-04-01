{-# LANGUAGE RankNTypes #-}
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Codensity

data TestException = TestException String
    deriving (Show, Eq)

instance Exception TestException


instance MonadCatch m => MonadCatch (ContT r m) where
  catch a handler = ContT $ \c ->
    runContT a c `catch` \e -> runContT (handler e) c

instance MonadMask m => MonadMask (ContT r m) where
  mask f = ContT $ \c ->
    mask $ \restore ->
      runContT (f (q restore)) c
    where
      q :: (forall a. m a -> m a) -> ContT r m a -> ContT r m a
      q r x = ContT $ \c -> r $ runContT x c

  uninterruptibleMask f = ContT $ \c ->
    uninterruptibleMask $ \restore ->
      runContT (f (q restore)) c
    where
      q :: (forall a. m a -> m a) -> ContT r m a -> ContT r m a
      q r x = ContT $ \c -> r $ runContT x c

  generalBracket acquire release use = mask $ \unmasked -> do
    resource <- acquire
    b <- unmasked (use resource) `catch` \e -> do
      _ <- release resource (ExitCaseException e)
      throwM e
    c <- release resource (ExitCaseSuccess b)
    return (b, c)

bracket_' :: MonadCatch m
          => m a  -- ^ computation to run first (\"acquire resource\")
          -> m b  -- ^ computation to run last when successful (\"release resource\")
          -> m b  -- ^ computation to run last when an exception occurs
          -> m c  -- ^ computation to run in-between
          -> m c  -- returns the value from the in-between computation
bracket_' before after afterEx thing = do
   _ <- before
   r <- thing `onException` afterEx
   _ <- after
   return r

f :: ContT () IO String
f = do
     resetT $ bracket_' (say "acquired") (say "released-successful") (say "released-exception") (say "executed")
     say "Hello!"
     -- throwM (TestException "error")
     return "success"
   where
     say = liftIO . putStrLn

main = runContT f print
