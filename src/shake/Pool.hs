{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Shake.Internal.Core.Pool(
    addPoolWait, actionFenceSteal, actionFenceRequeue,
    actionAlwaysRequeue, actionAlwaysRequeuePriority,
    addPoolWait_,
    actionFenceRequeueBy
    ) where

import Control.Exception
import General.Pool
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import System.Time.Extra
import Data.Either.Extra
import Control.Monad.IO.Class
import General.Fence

{-# LANGUAGE TupleSections #-}

-- | Thread pool implementation. The three names correspond to the following
module General.Pool(
    Pool, runPool,
    addPool, PoolPriority(..),
    increasePool, keepAlivePool
    ) where

import Control.Concurrent.Extra
import System.Time.Extra
import Control.Exception
import Control.Monad.Extra
import General.Timing
import General.Thread
import qualified Data.Heap as Heap
import qualified Data.HashSet as Set
import Data.IORef.Extra
import System.Random


---------------------------------------------------------------------
-- THREAD POOL

{-
Must keep a list of active threads, so can raise exceptions in a timely manner
If any worker throws an exception, must signal to all the other workers
-}

data S = S
    {alive :: !Bool -- True until there's an exception, after which don't spawn more tasks
    ,threads :: !(Set.HashSet Thread) -- IMPORTANT: Must be strict or we leak thread stacks
    ,threadsLimit :: {-# UNPACK #-} !Int -- user supplied thread limit, Set.size threads <= threadsLimit
    ,threadsCount :: {-# UNPACK #-} !Int -- Set.size threads, but in O(1)
    ,threadsMax :: {-# UNPACK #-} !Int -- high water mark of Set.size threads (accounting only)
    ,threadsSum :: {-# UNPACK #-} !Int -- number of threads we have been through (accounting only)
    ,rand :: IO Int -- operation to give us the next random Int
    ,todo :: !(Heap.Heap (Heap.Entry (PoolPriority, Int) (IO ()))) -- operations waiting a thread
    }


emptyS :: Int -> Bool -> IO S
emptyS n deterministic = do
    rand <- if not deterministic then pure randomIO else do
        ref <- newIORef 0
        -- no need to be thread-safe - if two threads race they were basically the same time anyway
        pure $ do i <- readIORef ref; writeIORef' ref (i+1); pure i
    pure $ S True Set.empty n 0 0 0 rand Heap.empty


data Pool = Pool
    !(Var S) -- Current state, 'alive' = False to say we are aborting
    !(Barrier (Either SomeException S)) -- Barrier to signal that we are finished


withPool :: Pool -> (S -> IO (S, IO ())) -> IO ()
withPool (Pool var _) f = join $ modifyVar var $ \s ->
    if alive s then f s else pure (s, pure ())

withPool_ :: Pool -> (S -> IO S) -> IO ()
withPool_ pool act = withPool pool $ fmap (, pure()) . act


worker :: Pool -> IO ()
worker pool = withPool pool $ \s -> pure $ case Heap.uncons $ todo s of
    Nothing -> (s, pure ())
    Just (Heap.Entry _ now, todo2) -> (s{todo = todo2}, now >> worker pool)

-- | Given a pool, and a function that breaks the S invariants, restore them.
--   They are only allowed to touch threadsLimit or todo.
--   Assumes only requires spawning a most one job (e.g. can't increase the pool by more than one at a time)
step :: Pool -> (S -> IO S) -> IO ()
-- mask_ is so we don't spawn and not record it
step pool@(Pool _ done) op = mask_ $ withPool_ pool $ \s -> do
    s <- op s
    case Heap.uncons $ todo s of
        Just (Heap.Entry _ now, todo2) | threadsCount s < threadsLimit s -> do
            -- spawn a new worker
            t <- newThreadFinally (now >> worker pool) $ \t res -> case res of
                Left e -> withPool_ pool $ \s -> do
                    signalBarrier done $ Left e
                    pure (remThread t s){alive = False}
                Right _ ->
                    step pool $ pure . remThread t
            pure (addThread t s){todo = todo2}
        Nothing | threadsCount s == 0 -> do
            signalBarrier done $ Right s
            pure s{alive = False}
        _ -> pure s
    where
        addThread t s = s{threads = Set.insert t $ threads s, threadsCount = threadsCount s + 1
                         ,threadsSum = threadsSum s + 1, threadsMax = threadsMax s `max` (threadsCount s + 1)}
        remThread t s = s{threads = Set.delete t $ threads s, threadsCount = threadsCount s - 1}


-- | Add a new task to the pool. See the top of the module for the relative ordering
--   and semantics.
addPool :: PoolPriority -> Pool -> IO a -> IO ()
addPool priority pool act = step pool $ \s -> do
    i <- rand s
    pure s{todo = Heap.insert (Heap.Entry (priority, i) $ void act) $ todo s}


-- | Temporarily increase the pool by 1 thread. Call the cleanup action to restore the value.
--   After calling cleanup you should requeue onto a new thread.
increasePool :: Pool -> IO (IO ())
increasePool pool = do
    step pool $ \s -> pure s{threadsLimit = threadsLimit s + 1}
    pure $ step pool $ \s -> pure s{threadsLimit = threadsLimit s - 1}


-- | Make sure the pool cannot run out of tasks (and thus everything finishes) until after the cancel is called.
--   Ensures that a pool that will requeue in time doesn't go idle.
keepAlivePool :: Pool -> IO (IO ())
keepAlivePool pool = do
    bar <- newBarrier
    addPool PoolResume pool $ do
        cancel <- increasePool pool
        waitBarrier bar
        cancel
    pure $ signalBarrier bar ()


-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
runPool :: Bool -> Int -> (Pool -> IO ()) -> IO () -- run all tasks in the pool
runPool deterministic n act = do
    s <- newVar =<< emptyS n deterministic
    done <- newBarrier
    let pool = Pool s done

    -- if someone kills our thread, make sure we kill our child threads
    let cleanup = join $ modifyVar s $ \s -> pure (s{alive=False}, stopThreads $ Set.toList $ threads s)

    let ghc10793 = do
            -- if this thread dies because it is blocked on an MVar there's a chance we have
            -- a better error in the done barrier, and GHC raised the exception wrongly, see:
            -- https://ghc.haskell.org/trac/ghc/ticket/10793
            sleep 1 -- give it a little bit of time for the finally to run
                    -- no big deal, since the blocked indefinitely takes a while to fire anyway
            res <- waitBarrierMaybe done
            case res of
                Just (Left e) -> throwIO e
                _ -> throwIO BlockedIndefinitelyOnMVar
    flip finally cleanup $ handle (\BlockedIndefinitelyOnMVar -> ghc10793) $ do
        addPool PoolStart pool $ act pool
        res <- waitBarrier done
        case res of
            Left e -> throwIO e
            Right s -> addTiming $ "Pool finished (" ++ show (threadsSum s) ++ " threads, " ++ show (threadsMax s) ++ " max)"

priority x = if isLeft x then PoolException else PoolResume


-- | Enqueue an Action into the pool and return a Fence to wait for it.
--   Returns the value along with how long it spent executing.
addPoolWait :: PoolPriority -> Action a -> Action (Fence IO (Either SomeException (Seconds, a)))
addPoolWait pri act = do
    ro@Global{..} <- Action getRO
    rw <- Action getRW
    liftIO $ do
        fence <- newFence
        let act2 = do offset <- liftIO offsetTime; res <- act; offset <- liftIO offset; pure (offset, res)
        addPool pri globalPool $ runAction ro rw act2 $ signalFence fence
        pure fence

-- | Like 'addPoolWait' but doesn't provide a fence to wait for it - a fire and forget version.
--   Warning: If Action throws an exception, it would be lost, so must be executed with try. Seconds are not tracked.
addPoolWait_ :: PoolPriority -> Action a -> Action ()
addPoolWait_ pri act = do
    ro@Global{..} <- Action getRO
    rw <- Action getRW
    liftIO $ addPool pri globalPool $ runAction ro rw act $ \_ -> pure ()


actionFenceSteal :: Fence IO (Either SomeException a) -> Action (Seconds, a)
actionFenceSteal fence = do
    res <- liftIO $ testFence fence
    case res of
        Just (Left e) -> Action $ throwRAW e
        Just (Right v) -> pure (0, v)
        Nothing -> Action $ captureRAW $ \continue -> do
            offset <- offsetTime
            waitFence fence $ \v -> do
                offset <- offset
                continue $ (offset,) <$> v


actionFenceRequeue :: Fence IO (Either SomeException b) -> Action (Seconds, b)
actionFenceRequeue = actionFenceRequeueBy id

actionFenceRequeueBy :: (a -> Either SomeException b) -> Fence IO a -> Action (Seconds, b)
actionFenceRequeueBy op fence = Action $ do
    res <- liftIO $ testFence fence
    case fmap op res of
        Just (Left e) -> throwRAW e
        Just (Right v) -> pure (0, v)
        Nothing -> do
            Global{..} <- getRO
            offset <- liftIO offsetTime
            captureRAW $ \continue -> waitFence fence $ \v -> do
                let v2 = op v
                addPool (priority v2) globalPool $ do
                    offset <- offset
                    continue $ (offset,) <$> v2


actionAlwaysRequeue :: Either SomeException a -> Action (Seconds, a)
actionAlwaysRequeue res = actionAlwaysRequeuePriority (priority res) res

actionAlwaysRequeuePriority :: PoolPriority -> Either SomeException a -> Action (Seconds, a)
actionAlwaysRequeuePriority pri res = Action $ do
    Global{..} <- getRO
    offset <- liftIO offsetTime
    captureRAW $ \continue ->
        addPool pri globalPool $ do
            offset <- offset
            continue $ (offset,) <$> res
