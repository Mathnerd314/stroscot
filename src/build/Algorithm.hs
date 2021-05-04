{-# LANGUAGE
EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, DerivingStrategies, StandaloneDeriving,
UndecidableInstances, DataKinds, FlexibleInstances,  ScopedTypeVariables, RecordWildCards, LambdaCase,
PackageImports, OverloadedLabels, DeriveGeneric
#-}
import Types
import Database
import Control.Monad(void,unless)
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Lock( Lock )
import qualified Control.Concurrent.Lock as Lock
import Data.Foldable(forM_)
import Data.IORef

global :: Global
global = undefined

getLock l = pure $ HM.lookup l (locks global)
setMem l s = void $ pure $ HM.insert l s (thunkList global)
getThunkState l = pure $ case HM.lookup l (thunkList global) of
  Nothing -> Uninitialized []
  Just s -> s

data ThunkState
  -- | A thunk starts uninitialized, with no entry. This state is if it has a waiter.
  = Uninitialized { waiters :: [IO ()] }
  -- | 'Loaded' thunks include thunks that are cleanly restored from a previous run
  --   as well as damaged thunks where not all writes can be restored (due to missing data etc.)
  --   The damage is only relevant for giving an error when reading thunks so we store that information in 'reconstructDamaged'
  | Loaded { waiters :: [IO ()], record :: ThunkRecord }
  -- | Starting execution of a thunk will create thunks in the running state. Running is either a thread in the body of the code,
  -- or else an IO () inside a child's waitlist
  | Running { waiters :: [IO ()] }
  -- | A thunk finishes if it dies and once all children have finished
  | Finished { record :: ThunkRecord }

-- the way to execute thunks is fixed throughout the build system
-- for example in iThreads it is a register-saving/restoring routine + address jump + tracing
-- but here we use M () and record actions manually
-- the IO () is required to write its ThunkRecord to its thunk state using the next operation (even if it encounters an exception)
recordThunk l trec = do
  let runFinish = do
        oldstate <- setMem l (Finished trec)
        run (waiters oldstate) where
          run [] = pure ()
          run (w:ws) = w >> run ws
      runNext l = execAfter l runFinish

  case syncPrimitive trec of
    Exception _ -> runFinish
    Exec seqs wait_finish -> do
      waitFor wait_finish runFinish
      forM_ seqs $ \(wait,run) -> waitFor wait $ mapM_ execOrReplayThunk run
    Acquire lock_id l -> do
      lock <- getLock lock_id
      Lock.acquire lock
      runNext l
    Release lock_id l -> do
      lock <- getLock lock_id
      Lock.release lock
      runNext l

waitFor :: [ThunkName] -> IO () -> IO ()
waitFor [] continue = continue
waitFor (l:ls) continue = let c = waitFor ls continue in
  getThunkState l >>= \case
    Finished _ -> c
    Running w -> setMem l $ Running (c:w)
    Uninitialized w -> setMem l $ Running (c:w)

execAfter :: [ThunkName] -> IO () -> IO ()
execAfter ls continue = do
  waitFor ls continue
  mapM_ execOrReplayThunk ls

----------------------------

execOrReplayThunk l = do
  getThunkState l >>= \case
    Uninitialized w -> do
      setMem l (Running w)
      addPool $ findRecordOrExec l
    _ -> pure () -- already running

data ReadResult = Clean | Dirty
  deriving (Show,Eq,Ord)

data DamageResult = DamagedReExec | Valid

findRecordOrExec l = do
  p <- lookupPreviousRuns
  checkAll p
    where
      checkAll [] = reallyExec l -- no valid execution traces
      checkAll (r:rs) = checkReads rs r (readSet r)

      checkReads rs r [] = do
        -- check if the thunk is damaged
        c <- checkDamaged l (writeSet r)
        case c of
          DamagedReExec -> checkAll rs
          Valid -> do
            -- valid thunk, replay writes and finalize
            mapM_ (replayWrite l) (writeSet r)
            recordThunk l r

      checkReads rs r (ri : ris) = do
        checkRead ri >>= \case
          Clean -> checkReads rs r ris
          Dirty -> checkAll rs

checkRead (rk,rv) = do
  -- first check the computed state
  cv <- lookup rk (state global)
  case cv of
    Just cv -> compareValues rk rv cv
    Nothing -> do
      -- not computed, so it must be an input
      dv <- getDiskValue rk
      compareValues rk rv dv

replayWrite l (wk,wv) = do
  wv_real <- getDiskValue wk
  set state wk $ ValueRecord
    { value = wv
    , damaged = wv_real /= wv
    }

data F = F { recordRead :: (DatumName,DatumValue) -> IO (),
            recordWrite :: (DatumName,DatumValue) -> IO ()}

makeThunkRecord act = do
  read <- newIORef []
  write <- newIORef []
  sync <- act $ F
   { recordRead = \r -> unless (r `elem` write) $ modifyIORef read (r:)
   , recordWrite = \r -> modifyIORef read (r:)
   }
  ThunkRecord <$> readIORef read <*> readIORef write <*> return sync
