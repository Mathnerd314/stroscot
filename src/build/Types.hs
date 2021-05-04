{-# LANGUAGE
EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, DerivingStrategies, StandaloneDeriving,
UndecidableInstances, DataKinds, FlexibleInstances,  ScopedTypeVariables, DeriveFunctor, DeriveTraversable
#-}
module Types where

import Data.ByteString
import Data.Store
import Data.Store.TH
import Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock

-- a memory location, filename, etc. that can be looked up
type DatumName = ByteString
type DatumValue = ByteString
-- the entry point of a thunk, where a computation can be recorded or replayed.
-- A specific thunk label is entered at most once per run.
-- To run a computation multiple times it must be aliased under different labels.
type ThunkName = ByteString
type LockId = ByteString

data Source
  = Input
  | GeneratedBy ThunkName
      -- if the value is damaged it can be reconstructed by running this label

data ValueRecord = ValueRecord
  { value :: DatumValue
  , source :: Source
  , damaged :: Bool
    -- cached computation of whether diskState == value
  }

data ThunkRecord = ThunkRecord
 {
   readSet :: [(DatumName,DatumValue)], -- ^ sorted in ascending order
   writeSet :: [(DatumName,DatumValue)],
   syncPrimitive :: SyncPrimitive ThunkName
 }

data SyncPrimitive l
  -- | A pair (waitlist,thunks) in the first list means to spawn a thread that waits for all the thunks in
  --   the waitlist to conclude and then runs the thunks. This allows constructing arbitrary DAGs of orderings.
  --   If an action in the waitlist throws an exception, then the actions are cancelled.
  --   The second argument is a list of thunks to wait for before marking the current thunk as finished.
  --   This allows spawning threads, is an iterated version of the synchronization primitive of Shake's need and apply
  --   The one-element version Sequence [[a,b,c]] is like pthread_create
  -- | like Sequence but it doesn't start execution of the first set of actions, it only waits for them (pthread_join)
  = Exec [([ThunkName],[l])] [l]
  -- | Acquire a lock
  | Acquire LockId [l]
  -- | Release a lock
  | Release LockId [l]
  -- | Thunk can also terminate abruptly if they encounter an exception.
  --   Exception records are always out-of-date.
  | Exception String
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

makeStore ''ThunkRecord
makeStore ''SyncPrimitive

-- | Graceful termination - stop the thread.
die = Exec [] []

-- | Global operations, these need to be thread-safe
data Global = Global
  { state :: HashMap DatumName ValueRecord
    -- shake uses HashMap DatumName Word32 + MutableArray (Maybe ValueRecord), assigning index id's sequentially from 0
    -- it seems to be to speed up storing stacks/dependency lists.
    -- Here, we don't have stacks, but compressing the read/write sets might be worth it.
    -- we need to benchmark: HashTable, HashSet, and combinations with MutableArray.
    -- also we might want split storage, e.g. version keys stored separately from the rest as
    -- version keys won't change.
  , diskState :: HashMap DatumName DatumValue
      -- to cache the actual file state so we don't hash it multiple times per run
  , thunkList :: HashMap ThunkName ThunkState
  -- auxiliary data structure to aid in implementing the synchronization primitives for running thunks
  , locks :: HashMap LockId Lock
  , execThunk :: ThunkName -> IO ()
  , resourceRandAction :: IO Int -- operation to give us the next random Int (sequential if deterministic)
  , resourceCounterAction :: IO Int -- operation to number resources for Eq/Ord operations
  }

-- | Return a number generator. The Bool is True if it's deterministic.
getResourceRand :: Bool -> IO (IO Int)
{- when random-1.2.x is in stackage (https://github.com/haskell/random/issues/103)
getResourceRand False = do
  g <- initStdGen >>= newIOGenM
  pure $ uniformM g
-}
getResourceRand False = pure randomIO
getResourceRand True = do
  ref <- newIORef 0
  -- no need to be thread-safe - if two threads race they were basically the same time anyway
  pure $ do i <- readIORef ref; writeIORef' ref (i+1); pure i

getResourceCounter :: IO (IO Int)
getResourceCounter = do
    ref <- newIORef 0
    pure $ atomicModifyIORef' ref $ \i -> let j = i + 1 in (j, j)

type M = ReaderT Global IO

resourceId :: M Int
resourceId = do
  rC <- reader resourceCounterAction
  liftIO $ rC

resourceRand :: M Int
  rR <- reader resourceRandAction
  liftIO $ rR
