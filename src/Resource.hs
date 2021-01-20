{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Shake.Internal.Resource(
    Resource, newResourceIO, newThrottleIO, withResource
    ) where

import Data.Function
import System.IO.Unsafe
import Control.Concurrent.Extra
import General.Fence
import Control.Exception.Extra
import Data.Tuple.Extra
import Data.IORef
import Control.Monad.Extra
import General.Bilist
import General.Pool
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Pool
import Control.Monad.IO.Class
import System.Time.Extra

---------------------------------------------------------------------
-- FINITE RESOURCES
-- * 'Development.Shake.newResource' creates a finite resource, stopping too many actions running
--   simultaneously.

data Finite = Finite
    {finiteAvailable :: !Int
        -- ^ number of currently available resources
    ,finiteWaiting :: Bilist (Int, Fence IO ())
        -- ^ queue of people with how much they want and the action when it is allocated to them
    ,rand :: IO Int -- operation to give us the next random Int
    ,todo :: !(Heap.Heap (Heap.Entry PoolPriority (Int, Fence IO ()))) -- operations waiting a thread
    }

type PoolPriority = (Priority, Int) -- priority, random position

--   priority levels (highest to lowest):
--
-- * 'PrioException' - things that probably result in a build error, so kick them off quickly.
-- * 'PrioResume' - things that started, blocked, and may have open resources in their closure.
-- * 'PrioStart' - rules that haven't yet started.
-- * 'PrioBatch' - rules that might batch if other rules start first.
data Priority
    = PrioException
    | PrioResume
    | PrioStart
    | PrioBatch
    | PrioDeprioritize Double
      deriving (Eq,Ord)

newResourceIO :: String -> Int -> IO Resource
newResourceIO name mx = do
    rand <- if not deterministic then pure randomIO else do
        ref <- newIORef 0
        -- no need to be thread-safe - if two threads race they were basically the same time anyway
        pure $ do i <- readIORef ref; writeIORef' ref (i+1); pure i
    newVar $ Finite mx rand Heap.empty


addAct = do
    i <- rand s
    Heap.insert (Heap.Entry (priority, i) $ void act
remAct = case Heap.uncons $ todo s of Just (Heap.Entry _ now, todo2) -> (now, todo2)


-- | Run an action which uses part of a finite resource. For more details see 'Resource'.
withResource :: Resource -> Int -> Action a -> Action a
withResource var want act = do
    fence <- modifyVar var $ \x@Finite{..} ->
        if want <= finiteAvailable then
            pure (x{finiteAvailable = finiteAvailable - want}, Nothing)
        else do
            fence <- newFence
            pure (x{finiteWaiting = finiteWaiting `snoc` (want, fence)}, Just fence)
    when fence $ do
        (offset, ()) <- actionFenceRequeueBy Right fence
        Action $ modifyRW $ addDiscount offset
    act `finally` do
      let
        f (Finite i (uncons -> Just ((wi,wa),ws)))
            | wi <= i = second (signalFence wa () >>) $ f $ Finite (i-wi) ws
            | otherwise = first (add (wi,wa)) $ f $ Finite i ws
        f (Finite i _) = (Finite i mempty, pure ())
        add a s = s{finiteWaiting = a `cons` finiteWaiting s}
      join $ modifyVar var $ \x -> pure $ f x{finiteAvailable = finiteAvailable x + want}

addPool priority pool act =
    mask_ $ join $ modifyVar var $ \s ->
    if not (alive s) then pure (s, pure ())
    else fmap (, pure ()) $ do
        t <- newThreadFinally (now >> worker pool) $ \t res -> case res of
            Left e -> withPool_ pool $ \s -> do
                signalBarrier done $ Left e
                pure (remThread t s){alive = False}
            Right _ ->
                step pool $ pure . remThread t
        pure (addThread t s){todo = todo2}
        where
            addThread t s = s{threads = Set.insert t $ threads s, threadsCount = threadsCount s + 1
                            ,threadsSum = threadsSum s + 1, threadsMax = threadsMax s `max` (threadsCount s + 1)}
            remThread t s = s{threads = Set.delete t $ threads s, threadsCount = threadsCount s - 1}

---------------------------------------------------------------------
-- THROTTLE RESOURCES

data Throttle
      -- | Some number of resources are available
    = ThrottleAvailable !Int
      -- | Some users are blocked (non-empty), plus an action to call once we go back to Available
    | ThrottleWaiting (IO ()) (Bilist (Int, Fence IO ()))


-- * 'Development.Shake.newThrottle' creates a throttled resource, stopping too many actions running
--   over a short time period.
newThrottleIO :: String -> Int -> Double -> IO Resource
newThrottleIO name count period = do
    when (count < 0) $
        errorIO $ "You cannot create a throttle named " ++ name ++ " with a negative quantity, you used " ++ show count
    key <- resourceId
    var <- newVar $ ThrottleAvailable count
    pure var

withResource :: Resource -> Int -> Action a -> Action a
withResource var i act = do
    modifyVar var $ \case
        ThrottleAvailable i
            | i >= want -> pure (ThrottleAvailable $ i - want, Nothing)
            | otherwise -> do
                stop <- keepAlivePool pool
                fence <- newFence
                pure (ThrottleWaiting stop $ (want - i, fence) `cons` mempty, Just fence)
        ThrottleWaiting stop xs -> do
            fence <- newFence
            pure (ThrottleWaiting stop $ xs `snoc` (want, fence), Just fence)
    when fence $ do
        (offset, ()) <- actionFenceRequeueBy Right fence
        Action $ modifyRW $ addDiscount offset
    act `finally` do
        let
            -- call a function after a certain delay
            waiter :: Seconds -> IO () -> IO ()
            waiter period act = void $ forkIO $ do
                sleep period
                act

            f stop i (uncons -> Just ((wi,wa),ws))
                | i >= wi = second (signalFence wa () >>) $ f stop (i-wi) ws
                | otherwise = (ThrottleWaiting stop $ (wi-i,wa) `cons` ws, pure ())
            f stop i _ = (ThrottleAvailable i, stop)

        waiter period $ join $ modifyVar var $ \x -> pure $ case x of
            ThrottleAvailable i -> (ThrottleAvailable $ i+n, pure ())
            ThrottleWaiting stop xs -> f stop n xs
