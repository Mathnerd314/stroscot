{-# LANGUAGE RecordWildCards, TupleSections #-}

module Pool where

import Control.Exception
import System.Time.Extra
import Data.Either.Extra
import Control.Monad.IO.Class
import General.Fence
import Control.Concurrent.Extra
import System.Time.Extra
import Control.Exception
import Control.Monad.Extra
import qualified Data.HashMap.Strict as Map
import Data.IORef.Extra
import System.Random


---------------------------------------------------------------------

data S = S
  {alive :: !Bool -- True until there's an exception or the pool stops, after which don't spawn more tasks
  ,threads :: !(Map.HashMap ThreadId (Barrier ())) -- IMPORTANT: Must be strict or we leak thread stacks
  ,threadsCount :: {-# UNPACK #-} !Int -- Set.size threads, but in O(1), used to check if all threads have finished
  ,threadsMax :: {-# UNPACK #-} !Int -- high water mark of Set.size threads (accounting only)
  ,threadsSum :: {-# UNPACK #-} !Int -- number of threads we have been through (accounting only)
  }

emptyS :: S
emptyS = S Nothing Map.empty 0 0 0

-- | The thread pool keeps a list of active threads.
--   If any thread throws an exception, this exception is rethrown in a timely manner to all the other threads to cancel them,
--   and no new threads are started. If all threads finish the pool is done.
data Pool = Pool !(Var S)
                 !(Barrier (Maybe SomeException, Int, Int)) -- Barrier to signal that we are finished, (e, max, sum)

-- | Spawn a new thread in the pool. Returns a barrier that ends when the thread ends, or Nothing if the pool is stopped.
addPool' :: Pool -> IO () -> IO (Maybe (Barrier ()))
addPool' pool@(Pool var done) act = modifyVar var $ \s -> case alive s of
  False -> pure (s, Nothing)
  True -> do
    bar <- newBarrier
    t <- mask_ $ forkIOWithUnmask $ \unmask -> do
      res <- try $ unmask act
      signalBarrier bar ()
      t <- myThreadId
      join $ modifyVar var $ \s -> case alive s of
        False -> pure (s, pure ()) -- silently exit if pool is stopped
        True -> case res of
          Left e -> pure $ (s{alive = False}, cleanup t s e)
          Right () -> pure $
                      (s {threads = Map.delete t $ threads s, threadsCount = threadsCount s - 1}, pure ())
    pure . (,Just bar) $ s{threads = Map.insert t bar $ threads s, threadsCount = threadsCount s + 1,
            threadsMax = threadsMax s `max` (threadsCount s + 1), threadsSum = threadsSum s + 1}

addPool :: Pool -> IO () -> IO ()
addPool pool act = void $ addPool' pool act

-- | Quit the pool. This throws an error if the pool is already stopped,
-- and returns False if other threads are executing.
quitPool :: Pool -> IO Bool
quitPool pool@(Pool var done) = do
  t <- myThreadId
  join $ modifyVar var $ \s -> case null (Map.delete t $ threads s) of
    False -> pure (s, pure False)
    True | alive s -> pure $ (s{alive = False}, signalBarrier done (Nothing, threadsMax s, threadsSum s) >> pure True)
         | otherwise -> pure (s, throwIO $ AssertionFailed "Pool is already stopped")

data ThreadKilledDueTo = ThreadKilledDueTo SomeException
    deriving Show

instance Exception ThreadKilledDueTo where
    toException   = asyncExceptionToException
    fromException = asyncExceptionFromException

-- | If someone kills our thread, make sure we kill the other threads. There should be no other threads in a clean exit.
cleanup t s e = do
  -- if a thread is in a masked action, killing it may take some time, so kill them in parallel
  bars <- foldrWithKey f (pure []) (Map.delete t $ threads s)
    where
      f bar tid bars_act = do
        bars <- bars_act
        forkIO $ throwTo tid (ThreadKilledDueTo e)
        pure (bar:bars)
  mapM_ waitBarrier bars
  signalBarrier done (Just e, threadsMax s, threadsSum s)

-- | Run all the tasks in the pool.
--   If any thread throws an exception, the exception will be reraised.
--   Returns once 'quitPool' is called.
runPool :: Bool -> Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool act = do
  s <- newVar emptyS
  done <- newBarrier
  let pool = Pool s done
  addPool pool $ act pool
  waitBarrier done
