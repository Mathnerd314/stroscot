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



-- | Run an action which uses part of a finite resource. For more details see 'Resource'.
withResource :: Resource -> Int -> Action a -> Action a
withResource r i act = do
    acquireResource r globalPool i
    when fence -> do
        (offset, ()) <- actionFenceRequeueBy Right fence
        Action $ modifyRW $ addDiscount offset
    act `finally` do
    liftIO $ releaseResource r globalPool i


---------------------------------------------------------------------
-- FINITE RESOURCES
-- * 'Development.Shake.newResource' creates a finite resource, stopping too many actions running
--   simultaneously.

data Finite = Finite
    {finiteAvailable :: !Int
        -- ^ number of currently available resources
    ,finiteWaiting :: Bilist (Int, Fence IO ())
        -- ^ queue of people with how much they want and the action when it is allocated to them
    }

newResourceIO :: String -> Int -> IO Resource
newResourceIO name mx = newVar $ Finite mx mempty

acquire :: Var Finite -> Pool -> Int -> IO (Maybe (Fence IO ()))
acquire var _ want
    | want < 0 = errorIO $ "You cannot acquire a negative quantity of " ++ shw ++ ", requested " ++ show want
    | want > mx = errorIO $ "You cannot acquire more than " ++ show mx ++ " of " ++ shw ++ ", requested " ++ show want
    | otherwise = modifyVar var $ \x@Finite{..} ->
        if want <= finiteAvailable then
            pure (x{finiteAvailable = finiteAvailable - want}, Nothing)
        else do
            fence <- newFence
            pure (x{finiteWaiting = finiteWaiting `snoc` (want, fence)}, Just fence)

release :: Var Finite -> Pool -> Int -> IO ()
release var _ i = join $ modifyVar var $ \x -> pure $ f x{finiteAvailable = finiteAvailable x + i}
    where
        f (Finite i (uncons -> Just ((wi,wa),ws)))
            | wi <= i = second (signalFence wa () >>) $ f $ Finite (i-wi) ws
            | otherwise = first (add (wi,wa)) $ f $ Finite i ws
        f (Finite i _) = (Finite i mempty, pure ())
        add a s = s{finiteWaiting = a `cons` finiteWaiting s}



---------------------------------------------------------------------
-- THROTTLE RESOURCES


-- call a function after a certain delay
waiter :: Seconds -> IO () -> IO ()
waiter period act = void $ forkIO $ do
    sleep period
    act


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
    pure $ Resource key shw (acquire var) (release var)
    where
        shw = "Throttle " ++ name

        acquire :: Var Throttle -> Pool -> Int -> IO (Maybe (Fence IO ()))
        acquire var pool want
            | want < 0 = errorIO $ "You cannot acquire a negative quantity of " ++ shw ++ ", requested " ++ show want
            | want > count = errorIO $ "You cannot acquire more than " ++ show count ++ " of " ++ shw ++ ", requested " ++ show want
            | otherwise = modifyVar var $ \case
                ThrottleAvailable i
                    | i >= want -> pure (ThrottleAvailable $ i - want, Nothing)
                    | otherwise -> do
                        stop <- keepAlivePool pool
                        fence <- newFence
                        pure (ThrottleWaiting stop $ (want - i, fence) `cons` mempty, Just fence)
                ThrottleWaiting stop xs -> do
                    fence <- newFence
                    pure (ThrottleWaiting stop $ xs `snoc` (want, fence), Just fence)

        release :: Var Throttle -> Pool -> Int -> IO ()
        release var _ n = waiter period $ join $ modifyVar var $ \x -> pure $ case x of
                ThrottleAvailable i -> (ThrottleAvailable $ i+n, pure ())
                ThrottleWaiting stop xs -> f stop n xs
            where
                f stop i (uncons -> Just ((wi,wa),ws))
                    | i >= wi = second (signalFence wa () >>) $ f stop (i-wi) ws
                    | otherwise = (ThrottleWaiting stop $ (wi-i,wa) `cons` ws, pure ())
                f stop i _ = (ThrottleAvailable i, stop)
