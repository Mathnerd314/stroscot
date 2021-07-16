{-# LANGUAGE RecordWildCards, TupleSections #-}

module Pool where

import Control.Exception
import System.Time.Extra
import Data.Either.Extra
import Control.Monad.IO.Class
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
emptyS = S True Map.empty 0 0 0

-- | The thread pool keeps a list of active threads.
--   If any thread throws an exception, this exception is rethrown in a timely manner to all the other threads to cancel them,
--   and no new threads are started. If all threads finish the pool is done.
data Pool = Pool !(Var S)
                 !((Maybe SomeException, Int, Int) -> IO ())
                    -- Function to signal that we are finished, (e, max, sum)
                    -- Only called when alive transitions from True to False

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
          Left e -> pure $ (s{alive = False}, cleanup pool t s e)
          Right () | threadsCount s == 1 -> -- last thread to stop quits the pool
            pure $
              (s{alive = False, threads = Map.delete t $ threads s, threadsCount = threadsCount s - 1},
              done (Nothing, threadsMax s, threadsSum s))
          Right () -> pure $
                      (s {threads = Map.delete t $ threads s, threadsCount = threadsCount s - 1}, pure ())
    pure . (,Just bar) $ s{threads = Map.insert t bar $ threads s, threadsCount = threadsCount s + 1,
            threadsMax = threadsMax s `max` (threadsCount s + 1), threadsSum = threadsSum s + 1}

addPool :: Pool -> IO () -> IO ()
addPool pool act = void $ addPool' pool act

data ThreadKilledDueTo = ThreadKilledDueTo SomeException
    deriving Show

instance Exception ThreadKilledDueTo where
    toException   = asyncExceptionToException
    fromException = asyncExceptionFromException

-- | If someone kills our thread, make sure we kill the other threads.
cleanup (Pool var done) t s e = do
  let
    f tid bar bars_act = do
      bars <- bars_act
      -- if a thread is in a masked action, killing it may take some time, so kill them in parallel
      forkIO $ throwTo tid (ThreadKilledDueTo e)
      pure (bar:bars)
  bars <- Map.foldrWithKey f (pure []) (Map.delete t $ threads s)
  mapM_ waitBarrier bars
  done (Just e, threadsMax s, threadsSum s)

-- | Run all the tasks in the pool.
--   If any thread throws an exception, the exception will be rethrown to all other threads in the pool.
--   Once all threads have terminated (either gracefully or due to the exceptions), the second argument is called
--   from the last thread in the pool.
runPool :: (Pool -> IO ()) -> ((Maybe SomeException, Int, Int) -> IO ()) -> IO () -- run all tasks in the pool
runPool act done = do
  s <- newVar emptyS
  let pool = Pool s done
  addPool pool $ act pool
