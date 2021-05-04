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


-- | A Double with its Ord instance flipped, so that higher priorities are pulled first from the minimum-biased priority queue
newtype Priority = Priority Double
    deriving (Eq)

instance Ord Priority where
    compare (Priority x) (Priority y) = case compare x y of
      LT -> GT
      GT -> LT
      EQ -> EQ
    (Priority x) <  (Priority y) = x > y
    (Priority x) <= (Priority y) = x >= y
    (Priority x) >= (Priority y) = x <= y
    (Priority x) >  (Priority y) = x < y

data SystemPriority = PrioException | PrioResume | PrioStart | PrioBatch

defaultPriority :: SystemPriority -> Priority
defaultPriority Pr

-- | things that probably result in a build error, so kick them off quickly.
prioException = Priority 4
-- | things that started, blocked, and may have open resources in their closure.
prioResume = Priority 3
-- | rules that haven't yet started.
prioStart = Priority 2
-- | rules that might batch if other rules start first.
prioBatch = Priority 1



type PoolPriority = (Priority, Int) -- ^ priority, random position

-- | A type representing a limited resource which the build system should conserve. 'newFinite' and 'newThrottle' create resources. These resources are used with 'withResource'.
data Resource = Resource
    {resourceOrd :: !Int
        -- ^ Key used for Eq/Ord operations.
    ,resourceName :: !String
        -- ^ String used for Show / error messages
    ,resourceI :: !ResourceI
    }

instance Show Resource where show = "<Resource " ++ resourceName ++ " #" ++ resourceOrd ++ ">"
instance Eq Resource where (==) = (==) `on` resourceOrd
instance Ord Resource where compare = compare `on` resourceOrd

data ResourceI
  = Finite { resourceVar :: !(ResourceVar ()), finiteCapacity :: !Int }
  | Throttle { resourceVar :: !(ResourceVar (TimeSpec,Bool)), throttleCount :: !Int, throttlePeriod :: !TimeSpec } -- var is last_reset, recheck thread running

type E = Heap.Entry PoolPriority (Map Resource Int, IO ())

data ResourceVar f = ResourceVar {
  waiting :: !(Heap.Heap E),
    -- ^ priority queue of pending operations with how much they want and the action when it is allocated to them
  available :: !Int,
  extra :: !f
}

-- | Create a resource with a given name and availability type.
newResource :: String -> ResourceI -> M Resource
newResource s i = do
  key <- resourceId
  pure $ Resource key s i

newResourceVar :: Int -> f -> Var (ResourceVar f)
newResourceVar mx e = newVar $ ResourceVar [] Heap.empty mx mx e

-- | Creates a finite resource, stopping too many actions running
--   simultaneously. The capacity is the maximum number of units that may be used concurrently.
newFinite :: String -> Int -> M Resource
newFinite s mx = do
    when (mx <= 0) $
        errorIO $ "You cannot create a finite resource named " ++ name ++ " with a non-positive capacity, you used " ++ show mx
    var <- newResourceVar mx ()
    newResource s (Finite var mx)

-- | Creates a throttled resource, stopping too many actions running
--   over a short time period.
newThrottle :: String -> Int -> TimeSpec -> IO Resource
newThrottle name count period = do
    when (count <= 0) $
        errorIO $ "You cannot create a throttle named " ++ name ++ " with a non-positive quantity, you used " ++ show count
    t <- now
    var <- newResourceVar count t
    newResource s (Throttle var count period)

checkError (r,want) =
  case resourceI r of
    Finite v cap ->
      | want < 0 = errorIO $ "You cannot acquire a negative quantity of " ++ show r ++ ", requested " ++ show want
      | want > cap = errorIO $ "You cannot acquire more than " ++ show cap ++ " of " ++ show r ++ ", requested " ++ show want
      | otherwise -> pure ()
    Throttle var count period ->
      | want < 0 = errorIO $ "You cannot acquire a negative quantity of " ++ show r ++ ", requested " ++ show want
      | want > count = errorIO $ "You cannot acquire more than " ++ show count ++ " of " ++ show r ++ ", requested " ++ show want
      | otherwise -> pure ()

canPreempt want available queue | want <= 0 = True
canPreempt want available queue | want > available = False
canPreempt want available queue = case uncons queue of
  Just (e,es) | e <= ent ->
    canPreempt want (available - (Map.lookup r . fst . Heap.payload $ e)) es
  _ -> want <= available

acquire pool ent [] suc = pure suc
acquire pool ent ((r,want):rs) suc | Finite var cap <- resourceI r = modifyVar var $ \x@ResourceVar{..} ->
      acquireSucceeded <- acquire pool ent rs (suc && canPreempt want available waiting)
      (,acquireSucceeded) <$>
        if acquireSucceeded then
          pure x{available = available - want}
        else pure x{waiting = Heap.insert ent waiting}
acquire pool ent ((r,want):rs) suc | t@(Throttle var count period) <- resourceI r = modifyVar var $ \x_pre -> do
      x@ResourceVar{..} <- maybeResetThrottle t x_pre
      acquireSucceeded <- acquire pool ent rs (suc && canPreempt want available waiting)
      (,acquireSucceeded) <$>
        if acquireSucceeded then
          pure x{available=available - want)}
        else do
          let (last_reset,running) = extra
          unless running $ do
            -- spawn thread to run waiting tasks when limit resets
            t <- now
            waiter pool (period - (now - last_reset)) $ recheckThrottle
          pure x{waiting = Heap.insert ent waiting,extra=(last_reset,True)}

-- | Add an action which uses some nonempty multiset of resources to the waiting queue with the given priority, and run (in the pool) the top queued actions if their resources are available. The action should only do "productive" work, otherwise the resource may be held while the action is waiting on something. This encourages an acquire-work-release-block-acquire pattern that the Action monad implements.
withResourcePool :: Pool -> Map Resource Int -> Priority -> IO () -> M ()
withResourcePool pool wants priority act = do
  let l = Map.toList wants
  mapM_ checkError l
  i <- resourceRand
  let e = Heap.Entry (priority, i) (wants,act)
  acquireSucceeded <- acquire pool e l True
  when acquireSucceeded $ addPool pool $ act `finally` release pool e l
  where
    getNext x@Finite{..} =
      case Heap.uncons finiteWaiting of
        Nothing -> (x, [])
        Just (Heap.Entry _ (want,act), t)
          | want > finiteAvailable -> (x, [])
          | otherwise -> case getNext x{finiteAvailable = finiteAvailable - want, finiteWaiting = t} of
                          (x', xs) -> (x', (act,want) : xs)
    runNext x = let (x', xs) = getNext x in
      pure $ (x', case xs of
        [] -> pure ()
        t:ts -> do
          mapM_ (\(act,want) -> addPool $ act `finally` addBack want) ts
          t `finally` addBack want)
    addBack want = join $ modifyVar var $ \x@Finite{..} ->
        runNext x{finiteAvailable = finiteAvailable x + want}

---------------------------------------------------------------------
-- THROTTLE RESOURCES

withThrottle :: Var Finite -> Int -> Action a -> Action a
withThrottle var i act = do
  modifyVar var $ \case
    ThrottleAvailable i
      | i >= want -> pure (ThrottleAvailable $ i - want, Nothing)
      | otherwise -> do
        fence <- newFence
        pure (ThrottleWaiting $ (want - i, fence) `cons` mempty, Just fence)
    ThrottleWaiting xs -> do
      fence <- newFence
      pure (ThrottleWaiting $ xs `snoc` (want, fence), Just fence)
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

      f i (uncons -> Just ((wi,wa),ws))
        | i >= wi = second (signalFence wa () >>) $ f (i-wi) ws
        | otherwise = ThrottleWaiting $ (wi-i,wa) `cons` ws
      f i _ = ThrottleAvailable i

    waiter period $ join $ modifyVar var $ \x -> pure $ case x of
      ThrottleAvailable i -> ThrottleAvailable $ i+n
      ThrottleWaiting stop xs -> f stop n xs


-- | Run an action which uses part of a finite resource. For more details see 'Resource'.
--   You cannot depend on a rule (e.g. 'need') while a resource is held.
withResource :: Resource -> Int -> Action a -> Action a
withResource r i act = do
    Global{..} <- Action getRO
    liftIO $ globalDiagnostic $ pure $ show r ++ " waiting to acquire " ++ show i

    fence <- liftIO $ acquireResource r globalPool i
    whenJust fence $ \fence -> do
        (offset, ()) <- actionFenceRequeueBy Right fence
        Action $ modifyRW $ addDiscount offset

    liftIO $ globalDiagnostic $ pure $ show r ++ " running with " ++ show i
    Action $ fromAction (blockApply ("Within withResource using " ++ show r) act) `finallyRAW` do
        liftIO $ releaseResource r globalPool i
        liftIO $ globalDiagnostic $ pure $ show r ++ " released " ++ show i



-- | A version of 'Development.Shake.newResource' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newResource' instead.
newResourceIO :: String -> Int -> IO Resource
newResourceIO name mx = do
    where
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

-- https://gitlab.haskell.org/ghc/ghc/-/issues/7087
maxDelay :: Integer
maxDelay = fromIntegral $ ((maxBound :: Int) `div` 10^7 - 1) * 10^4

-- | Sleep for the given number of microseconds
sleep :: Integer -> IO ()
sleep s | s < 0 = pure ()
        | s > maxDelay = do
          threadDelay maxDelay
          sleep $ s - maxDelay
        | otherwise = threadDelay s

-- call a function after a certain delay
waiter :: Pool -> TimeSpec -> IO () -> IO ()
waiter pool period act = addPool $ do
    sleep (toNanoSecs period `div` 1000)
    act

-- | A version of 'Development.Shake.newThrottle' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newThrottle' instead.
newThrottleIO :: String -> Int -> Double -> IO Resource
newThrottleIO name count period =
    key <- resourceId
    var <- newVar $ ThrottleAvailable count
    pure $ Resource key shw (acquire var) (release var)
    where
        shw = "Throttle " ++ name

        acquire :: Var Throttle -> Pool -> Int -> IO (Maybe (Fence IO ()))
        acquire var pool want =

        release :: Var Throttle -> Pool -> Int -> IO ()
        release var _ n = waiter period $ join $ modifyVar var $ \x -> pure $ case x of
                ThrottleAvailable i -> (ThrottleAvailable $ i+n, pure ())
                ThrottleWaiting stop xs -> f stop n xs
            where
                f stop i (uncons -> Just ((wi,wa),ws))
                    | i >= wi = second (signalFence wa () >>) $ f stop (i-wi) ws
                    | otherwise = (ThrottleWaiting stop $ (wi-i,wa) `cons` ws, pure ())
                f stop i _ = (ThrottleAvailable i, stop)

now = getTime Monotonic

maybeResetThrottle Throttle{..} r@ResourceVar{available=(num_calls,last_reset)} = do
  n <- now
  let elapsed = n - last_reset
  let period_remaining = period - elapsed
  -- If the time window has elapsed then reset.
  pure $ if period_remaining <= 0 then r{available=(0,n)} else r

Each global resource is equipped with a FIFO
resource queue queue(r). The resource queue stores
tasks which are waiting for or executing on the resource.

When a task τi attempts to lock a set of resources R
using lock (R), The scheduler checks if τi is ready. If so, then the task is scheduled and becomes executing.


Otherwise, τi becomes waiting. It is inserted into the resource queues
of all global resources in R. Moreover, this insertion
is atomic, meaning that no other task can be inserted
into any of the resource queues in R before τi has been
inserted into all queues in R.

Other tasks may not consume the resources where τi is at the head of the queue.
τi waits until enough of the segments currently using r have completed and released
a sufficient number of units of r for τi to move into the ready state.

When a task τi releases a set of resources R using
unlock (R), it is removed from the resource queues of
all global resources in R.
If after removing a task from a resource queue
the queue is not empty, then the scheduler checks if the
head task is ready. If so, then the task is scheduled and
becomes executing.