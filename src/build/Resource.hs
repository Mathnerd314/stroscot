{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Resource where

import Data.Function
import Data.List.Extra
import Data.Maybe(fromJust)
import Unsafe.Coerce
import Control.Concurrent.Extra
import Control.Exception.Extra
import Data.Tuple.Extra
import Data.IORef
import Control.Monad.Extra
import Pool
import Control.Monad.IO.Class
import System.Clock
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Heap as Heap
import Data.Map(Map)
import Data.Set(Set)
import Data.Heap(Heap)
import GHC.Exts(Any)
import Types
import Control.Monad.State
import Control.Monad.IO.Unlift
import GHC.Stack

type Seconds = Integer

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

-- In shake: Exception > Resume > Start > Batch > Deprioritize
-- batch runs with Batch
-- * this is terribly non-deterministic, as the batches change each run depending on which files are modified
-- * better is to collect the files in a list and then split the list into batches
-- * if the file list is generated sequentially, then the batches can be spawned as soon as each is complete
-- * the behavior on rebuild can be minimized with a complex rule that disentangles the dependencies
-- initial tasks and building a new key uses Start
-- parallel runs tasks with Resume
-- reschedule runs with Deprioritize
-- the stuff after a callCC runs with Exception or Resume depending on whether the actions threw an exception
-- the logic seems to be that resumed resources may be holding resources so finishing them first is better
-- Cot's scheduler is different enough that benchmarking it again seems necessary

type PoolPriority = (Priority, Int, Int) -- ^ priority, random position, sequential unique ID
{-
how our threading model works:
let's say 4 threads max
initial task A starts, acquires 1 thread
A spawns 7 tasks BCDEFGH using 1 thread
BCD acquire threads and spawn with addPool
EFGH fail to acquire and add themselves to the queue
A finishes and its thread executes the highest-priority among EFGH (say EFGH is the order)
B finishes and its thread executes F
C finishes and its thread executes G
D finishes and its thread executes H
so we've had 4 threads running continuously
then all threads finish up and quit.
-}
-- | A type representing a limited resource which the build system should conserve. 'newFinite' and 'newThrottle' create resources. These resources are used with 'withResource'.
data Resource = Resource
    {resourceOrd :: !Int
        -- ^ Key used for Eq/Ord operations.
    ,resourceName :: !String
        -- ^ String used for Show / error messages
    ,resourceI :: !ResourceI
    }

instance Show Resource where
  show Resource{..} = "<Resource " ++ resourceName ++ " #" ++ show resourceOrd ++ ">"
instance Eq Resource where (==) = (==) `on` resourceOrd
instance Ord Resource where compare = compare `on` resourceOrd

data ResourceI
  = Finite { resourceVarF :: !(Var (ResourceVar ())), finiteCapacity :: !Int }
  | Throttle { resourceVarT :: !(Var (ResourceVar (Seconds,Bool))), -- ^ var extra is last_reset, waiter thread running
              throttleMax :: !Int, throttleGain :: !Int, throttlePeriod :: !Seconds }
              -- maximum tokens, tokens per period, period length

isFinite :: Resource -> Bool
isFinite r | Finite{} <- resourceI r = True
           | otherwise = False

toAnyFinite :: ResourceVar () -> Any
toAnyFinite = unsafeCoerce
fromAnyFinite :: Any -> ResourceVar ()
fromAnyFinite = unsafeCoerce
toAnyThrottle :: ResourceVar (Seconds,Bool) -> Any
toAnyThrottle = unsafeCoerce
fromAnyThrottle :: Any -> ResourceVar (Seconds,Bool)
fromAnyThrottle = unsafeCoerce

-- | Priority, how much the action wants, and the action when it is allocated to them
type E = Heap.Entry PoolPriority (Map Resource Int, IO ())

wanted :: E -> Map Resource Int
wanted e = fst . Heap.payload $ e
action :: E -> IO ()
action e = snd . Heap.payload $ e
priority :: E -> PoolPriority
priority e = Heap.priority e

data ResourceVar f = ResourceVar {
  waiting :: !(Set.Set E),
    -- ^ Each resource is equipped with a priority queue that stores
    --   tasks which are waiting for the resource.
    --   We use a Set though as its split operation is more useful.
  available :: !Int,
  extra :: !f
}

-- | Create a resource with a given name and availability type.
newResource :: String -> ResourceI -> M Resource
newResource s i = do
  key <- resourceId
  pure $ Resource key s i

newResourceVar :: Int -> f -> IO (Var (ResourceVar f))
newResourceVar mx e = newVar $ ResourceVar Set.empty mx e

-- | Creates a finite resource, stopping too many actions running
--   simultaneously. The capacity is the maximum number of units that may be used concurrently.
newFinite :: String -> Int -> M Resource
newFinite name mx = do
    when (mx <= 0) $
        liftIO . errorIO $ "You cannot create a finite resource named " ++ name ++ " with a non-positive capacity, you used " ++ show mx
    var <- liftIO $ newResourceVar mx ()
    newResource name (Finite var mx)

-- | Creates a throttled resource, stopping too many actions running
--   over a short time period.
newThrottle :: String -> Int -> Int -> Seconds -> M Resource
newThrottle name count gain period = do
    when (count <= 0) $
        liftIO . errorIO $ "You cannot create a throttle named " ++ name ++ " with a non-positive quantity, you used " ++ show count
    t <- liftIO now
    var <- liftIO $ newResourceVar count (t,False)
    newResource name (Throttle var count gain period)

-- | Checks for out-of-bounds resource requests
checkError (r,want) =
  case resourceI r of
    Finite v cap
      | want <= 0 -> errorIO $ "You cannot acquire a non-positive quantity of " ++ show r ++ ", requested " ++ show want
      | want > cap -> errorIO $ "You cannot acquire more than " ++ show cap ++ " of " ++ show r ++ ", requested " ++ show want
      | otherwise -> pure ()
    Throttle var count gain period
      | want <= 0 -> errorIO $ "You cannot acquire a non-positive quantity of " ++ show r ++ ", requested " ++ show want
      | want > count -> errorIO $ "You cannot acquire more than " ++ show count ++ " of " ++ show r ++ ", requested " ++ show want
      | otherwise -> pure ()

-- | Locks some resources. The resources must be in ascending order to prevent deadlocks, and similarly calls cannot be nested.
withVarsT :: [Resource] -> StateT (Map Resource Any) IO a -> StateT (Map Resource Any) IO a
withVarsT [] f = f
withVarsT (r:rs) f = do
  mp <- get
  case r `Map.member` mp of
    True -> withVarsT rs f
    False -> StateT $ \mp_o -> case resourceI r of
      Finite var cap -> modifyVar var $ \x -> do
        let mp = Map.insert r (toAnyFinite x) mp_o
        (a, mp') <- runStateT (withVarsT rs f) mp
        pure (fromAnyFinite . fromJust $ Map.lookup r mp', (a, Map.delete r mp'))
      Throttle var count gain period -> modifyVar var $ \x -> do
        let mp = Map.insert r (toAnyThrottle x) mp_o
        (a, mp') <- runStateT (withVarsT rs f) mp
        pure (fromAnyThrottle . fromJust $ Map.lookup r mp', (a, Map.delete r mp'))

withVars ::  [Resource] -> StateT (Map Resource Any) IO a -> IO a
withVars rs f = evalStateT (withVarsT rs f) Map.empty

lookupVar :: Resource -> StateT (Map Resource Any) IO Any
lookupVar r = do
  mp <- get
  pure (fromJust $ Map.lookup r mp)

-- | Run an action which uses some multiset of resources. If the resources are available the action is run in the thread pool,
--   otherwise it is added to the waiting queue with the given priority. In either case the function returns quickly.
--   The action should only do "productive" work, otherwise the resource may be held while the action is waiting on something.
--   The Action monad implements an acquire-work-release-block-acquire pattern that releases resources while blocking.
spawnWithResource :: Map Resource Int -> Priority -> IO () -> M ()
spawnWithResource mp prio act = do
  pool <- globalPool
  spawnWithResourceInner (addPool pool) mp prio act

-- | Similar to 'withResource' but runs the worker directly.
--   Used for saving a thread when running the initial task.
spawnWithResourceWorker :: Map Resource Int -> Priority -> IO () -> M ()
spawnWithResourceWorker = spawnWithResourceInner id

-- | inner method to share code between withResource and withResourceWorker
spawnWithResourceInner :: (IO () -> IO ()) -> Map Resource Int -> Priority -> IO () -> M ()
spawnWithResourceInner spawn wants priority act = do
  pool <- globalPool
  mapM_ (liftIO . checkError) (Map.toList wants)
  i <- resourceRand
  tid <- taskId
  let e = Heap.Entry (priority, i, tid) (wants,act)
  acquireSucceeded <- liftIO $ acquire pool e
  when acquireSucceeded $ liftIO $ spawn $ act `finally` release pool wants

-- | Check if ent can obtain sufficient resources.
--   It may not consume the resources of any higher-priority tasks,
--   but can preempt any lower-priority requests waiting for resources.
canPreempt :: Resource -> E -> Int -> Int -> Set E -> Bool
canPreempt r ent want available queue | want > available = False
canPreempt r ent want available queue = do
  let lt = Set.takeWhileAntitone (<= ent) queue
  let f _ Nothing = Nothing
      f e b = do
        avail <- b
        let avail' = avail - (fromJust . Map.lookup r $ wanted e)
        pureIf (avail' >= 0) avail'
  case Set.foldr f (Just (available - want)) lt of
    Nothing -> False
    Just _ -> True

canPreemptM :: Resource -> E -> Int -> StateT (Map Resource Any) IO Bool
canPreemptM r ent want = do
    v <- lookupVar r
    pure $ case resourceI r of
      Finite{} -> let ResourceVar{..} = fromAnyFinite v in canPreempt r ent want available waiting
      Throttle{} -> let ResourceVar{..} = fromAnyThrottle v in canPreempt r ent want available waiting
        -- don't bother with checking the throttle period,
        -- the waiter thread will handle it if necessary as soon as it is scheduled

newtype AllM m = AllM (m Bool)
instance Monad m => Semigroup (AllM m) where
  AllM x <> AllM y = AllM $ do
    b <- x
    case b of
      False -> pure False
      True -> y
instance Monad m => Monoid (AllM m) where
  mempty = AllM $ pure True

-- | Checks if the task is ready to run (can obtain its wanted resources).
checkPreempt :: E -> StateT (Map Resource Any) IO Bool
checkPreempt ent = case Map.foldMapWithKey (\r want -> AllM $ canPreemptM r ent want) (wanted ent) of
  AllM x -> x

-- | Checks if the task is ready to run (can obtain its wanted resources).
--   If so, the resources are reserved and True is returned.
--   Otherwise the task is inserted into each resource's queue and False is returned.
--   This action locks all of the relevant resource queues while running.
acquire :: Pool -> E -> IO Bool
acquire pool ent = let rs = wanted ent in withVars (Map.keys rs) $ do
  acquireSucceeded <- checkPreempt ent
  forM_ (Map.toList rs) $ \(r,want) -> do
    v <- lookupVar r
    updated <- case resourceI r of
      Finite{} -> do
        let x@ResourceVar{..} = fromAnyFinite v
        pure . toAnyFinite $
          if acquireSucceeded then
            x{available = available - want}
          else x{waiting = Set.insert ent waiting}
      Throttle{..} -> do
        let x@ResourceVar{..} = fromAnyThrottle v
        toAnyThrottle <$>
          if acquireSucceeded then
            pure x{available = available - want}
          else do
            -- spawn thread to run waiting tasks when limit resets, unless already spawned
            let (last_reset,running) = extra
            liftIO $ unless running $ do
              t <- now
              addPool pool $ do
                sleep (throttlePeriod - (t - last_reset))
                recheckThrottle pool r
            pure x{waiting = Set.insert ent waiting,extra=(last_reset,True)}
    modify $ Map.insert r updated
  pure acquireSucceeded

-- Determine if there are any tasks that can be newly run (freed from the queue) due to released units.
-- example: queue [3,2,1,4] going from 4 to 7 units available.
-- the 3 task is runnable but not newly, 2,1 are newly runnable, 4 is not runnable (insufficient units).
newlyRunnable :: Resource -> Int -> Int -> [E] -> [E]
newlyRunnable r newAvailable oldAvailable queue
  | newAvailable <= oldAvailable = []
  | newAvailable <= 0 = []
  | otherwise =
      case queue of
        [] -> [] -- no more tasks to check
        e:es -> let want = fromJust . Map.lookup r $ wanted e in
          if want <= oldAvailable then
            -- resource runnable but blocked on something else, not newly runnable
            newlyRunnable r (newAvailable - want) (oldAvailable - want) es
          else if want <= newAvailable then
            -- newly runnable
            e : newlyRunnable r (newAvailable - want) (oldAvailable - want) es
          else
            -- not runnable at all, all further resources blocked
            []

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs [] = []
mergePairs [ls] = [ls]
mergePairs (x:y:ls) = (merge x y):mergePairs ls

mergeLists :: Ord a => [[a]] -> [a]
mergeLists [] = []
mergeLists [x] = x
mergeLists ls = mergeLists $ mergePairs ls

-- | Release the resources held after running a task. (Only relevant to finite resources)
release pool rs = do
  requestsToCheckL <- withVars (Map.keys rs) $ forM (Map.toList rs) $ \(r,want) -> do
    case resourceI r of
      Finite{} -> do
        v <- lookupVar r
        let x@ResourceVar{..} = fromAnyFinite v
        modify $ Map.insert r (toAnyFinite $ x{available =available + want})
        pure $ newlyRunnable r (available+want) available (Set.toAscList waiting)
      Throttle{} -> pure [] -- handled in releaseThrottle on the waiter thread
  let requestsToCheck = mergeLists requestsToCheckL
  reqs <- recheckRequests requestsToCheck pure
  case reqs of
    [] -> pure ()
    x:xs -> do
      forM_ xs $ \x -> addPool pool $ action x `finally` release pool (wanted x)
      action x `finally` release pool (wanted x)

-- | Check if the given tasks are ready to execute.
--   The ones that ready are removed from the resource queues and returned, and the resources are reserved.
--   The rest are left in the queues.
--   f is used to run an action with all the resources held
recheckRequests :: [E] -> ([E] -> StateT (Map Resource Any) IO a) -> IO a
recheckRequests requestsToCheck f = do
  let allResources = Set.toAscList . Set.unions $ map (Map.keysSet . fst . Heap.payload) requestsToCheck
  withVars allResources $ foldr tryTake (pure []) requestsToCheck >>= f
    where
      tryTake :: E -> StateT (Map Resource Any) IO [E] -> StateT (Map Resource Any) IO [E]
      tryTake request rest = do
        acquireSucceeded <- checkPreempt request
        if not acquireSucceeded then rest else do
          forM_ (Map.toList $ wanted request) $ \(r,want) -> do
            v <- lookupVar r
            updated <- case resourceI r of
              Finite{} -> do
                let x@ResourceVar{..} = fromAnyFinite v
                pure . toAnyFinite $ x{available = available - want, waiting = Set.delete request waiting}
              Throttle{} -> do
                let x@ResourceVar{..} = fromAnyThrottle v
                pure . toAnyThrottle $ x{available = available - want, waiting = Set.delete request waiting}
            modify $ Map.insert r updated
          (request :) <$> rest


recheckThrottle :: Pool -> Resource -> IO ()
recheckThrottle pool r = do
  let Throttle{..} = resourceI r
  (requestsToCheck, nextCheckWait) <- modifyVar resourceVarT $
    \v@ResourceVar{..} -> do
      let (last_reset,running) = extra
      assertIO running
      n <- now
      let periods_elapsed = fromIntegral $ (n - last_reset) `div` throttlePeriod
      let newAvailable = throttleMax `min` (available + periods_elapsed * throttleGain)
      let nR = newlyRunnable r newAvailable available (Set.toAscList waiting)
      let (var,nextCheckWait) = if periods_elapsed > 0
                                  then
                                    (v{available=newAvailable,extra=(n,True)}, throttlePeriod)
                                  else
                                    (v, last_reset + throttlePeriod - n)
      pure (v,(nR,nextCheckWait))
  (reqs, running) <- recheckRequests requestsToCheck $ \reqs -> do
    x@ResourceVar{extra=(last_reset,True),..} <- fromAnyThrottle <$> lookupVar r
    let running = not $ Set.null waiting
    modify $ Map.insert r (toAnyThrottle $ x{extra=(last_reset,running)})
    pure (reqs,running)
  forM_ reqs  $ \x -> addPool pool $ action x `finally` release pool (wanted x)
  if running then do
    sleep nextCheckWait
    recheckThrottle pool r
  else
    pure ()

---------------------------------------------------------------------
-- THROTTLE TIMING HELPERS

-- number from https://gitlab.haskell.org/ghc/ghc/-/issues/7087
-- in microseconds
maxDelay :: Int
maxDelay = ((maxBound :: Int) `div` 10^7 - 1) * 10^4

maxDelayT :: Seconds
maxDelayT = fromIntegral maxDelay * 1000

-- | Sleep for the given number of nanoseconds
sleep :: Seconds -> IO ()
sleep s
  | s <= 0 = pure () -- threadDelay 0 does putMVar takeMvar, basically nothing
  | s > maxDelayT = do
    threadDelay maxDelay
    sleep $ s - maxDelayT
  | s < 1000 = yield -- TODO: is this a short delay?
  | otherwise = threadDelay (fromIntegral $ s `div` 1000)

now :: IO Seconds
now = toNanoSecs <$> getTime Monotonic

-- | An 'IO' action that when evaluated calls 'assert' in the 'IO' monad and throws an 'AssertionFailed' exception if the argument is 'False'.
--
-- > catch (errorIO "Hello") (\(ErrorCall x) -> pure x) == pure "Hello"
-- > seq (errorIO "foo") (print 1) == print 1
assertIO :: Partial => Bool -> IO ()
assertIO x = withFrozenCallStack $ evaluate $ assert x ()