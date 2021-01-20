-- a memory location, filename, etc. that can be looked up
type Key = ByteString
type Value = ByteString
-- the entry point of a thunk, where a computation can be recorded or replayed.
-- A specific thunk label is entered at most once per run.
-- To run a computation multiple times it must be aliased under different labels.
type Label = ByteString

data ThunkRecord = ThunkRecord
 {
   readSet :: [(Key,Value)],
   writeSet :: [(Key,Value)],
   syncPrimitive :: SyncPrimitive
 }

data SyncPrimitive
  -- | Run the list of actions concurrently.
  --   If any action throws an exception at any time, then all other actions are 'cancel'led.
  = Concurrently [Label]
  -- | like concurrently but resumes execution at the second argument once the first argument's thunks are exhausted
  --   this is basically the synchronization primitive of Shake's need and apply
  | ExecAfter [Label] Label
  -- | like ExecAfter but it doesn't start execution of the actions, it only waits for them (deadlock city)
  | WaitFor [Label] Label
  -- | Graceful termination - Stop the thread. We could use Concurrently [] but this is clearer
  | Die
  -- | Thunk can also terminate abruptly if they encounter an exception.
  -- Exception records are always out-of-date.
  | Exception SomeException

-- Global operations, these need to be thread-safe
data Global = Global
  { state :: HashMap Key ValueRecord
    -- shake uses HashMap Key Word32 + MutableArray (Maybe ValueRecord), assigning index id's sequentially from 0
    -- it seems to be to speed up storing stacks/dependency lists.
    -- Here, we don't have stacks, but compressing the read/write sets might be worth it.
    -- we need to benchmark: HashTable, HashSet, and combinations with MutableArray.
    -- also we might want split storage, e.g. version keys stored separately from the rest as
    -- version keys won't change.
  , diskState :: HashMap Key Value
      -- to cache the actual file state so we don't hash it multiple times per run
  , thunkList :: HashMap Label ThunkState
  -- auxiliary data structure to aid in implementing the synchronization primitives for running thunks
  , execThunk :: Label -> IO ()
    -- the way to execute thunks is fixed throughout the run
    -- for example in iThreads it is a register-saving/restoring routine + address jump + tracing
    -- but here we use M () and record actions manually
    -- the IO () is required to write its ThunkRecord to its thunk state using the next operation (even if it encounters an exception)
  , recordThunk :: Label -> ThunkRecord -> IO ()
  }

data ValueRecord = ValueRecord
  { value :: Value
  , reconstructDamaged :: Maybe Label
      -- if the value is damaged it can be reconstructed by running this label, otherwise Nothing
  }

data ThunkState
  -- | A thunk starts uninitialized; normally there is no entry, but WaitFor will create an uninitialized entry
  -- todo: is this needed?
  = Uninitialized { waiters :: [IO ()] }
  -- | 'Loaded' thunks include thunks that are cleanly restored from a previous run
  --   as well as damaged thunks where not all writes can be restored (due to missing data etc.)
  --   The damage is only relevant for giving an error when reading thunks so we store that information in 'reconstructDamaged'
  | Loaded { waiters :: [IO ()], record :: ThunkRecord }
  -- | ExecAfter will create thunks in the running state. Running is either a thread in the body of the code,
  -- or else an IO () inside a child's waitlist
  | Running { waiters :: [IO ()] }
  -- | A thunk finishes if it dies and once all children have finished
  | Finished { record :: ThunkRecord }

recordThunk l trec = do
  let runFinish = do
      oldstate <- setMem l (Finished trec)
      run (waiters oldstate) where
        run [] = pure ()
        run (w:ws) = w >> run ws

  case syncPrimitive trec of
    Die -> runFinish
    Exception _ -> runFinish
    WaitFor ls l -> waitFor ls $ do
      addWaiter l runFinish
      execOrReplayThunk l
    Concurrently ls -> execAfter ls runFinish
    ExecAfter ls l -> execAfter ls $ do
      addWaiter l runFinish
      execOrReplayThunk l

addWaiter :: Label -> IO () -> IO ()
addWaiter l continue = getThunkState l >>= \case
    Finished _ -> continue
    Running w -> setMem k $ Running [w,continue]
    Uninitialized w -> setMem k $ Running [w,continue]

waitFor :: [Label] -> IO () -> IO ()
waitFor [] continue = continue
waitFor (l:ls) continue = addWaiter l (waitFor ls continue)

execAfter :: [Label] -> IO () -> IO ()
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
    , reconstructDamaged = when (wv_real /= wv) l
    }

Algorithm 3: Subroutines for the initial run algorithm
initThread(t)
begin
α ← 0; // Initializes thunk counter (α) to zero
∀i ∈ {1, ..., T } : Ct [i] ← 0; // t’s clock set to zero
end
startThunk()
begin
Ct [t] ← α; // Update thread clock
∀i ∈ {1, ..., T } : Lt [α].C[i] ← Ct [i]; // Update thunk clock
Lt [α].R/W ← ∅; // Initialize read/write sets to empty set
end
onMemoryAccess()
begin
if load then
Lt[α].R ← Lt [α].R ∪ {memory-address}; // Read
else
Lt[α].W ← Lt [α].W ∪ {memory-address}; // Write
end
end
endThunk()
begin
memo (Lt[α].W ) ← content(Lt [α].W ); // Globals & heap
memo (Lt[α].Stack) ←content(Stack);
memo (Lt[α].Reg) ←content(CPU Registers);
end
onSynchronization(s)
begin
switch Syncronization type do
case release(s):
// Update s’s clock to hold max of its and t’s clocks
∀i ∈ {1, ..., T } : Cs [i] ← max(Cs[i], Ct [i]);
sync(s); // Perform the synchronization
case acquire(s):
sync(s); // Perform the synchronization
// Update t’s clock to hold max of its and s’s clocks
∀i ∈ {1, ..., T } : Ct [i] ← max(Cs[i], Ct [i]);
end
end

Algorithm 4: The incremental run algorithm
Data: Shared dirty set M ← { modified pages } and Lt
∀s ∈ S, ∀i ∈ {1, ..., T } : Cs [i] ← 0; // All sync clocks set to 0
executeThread(t)
begin
initThread(t); // Same as initial run algorithm
while (t has not terminated and isValid(Lt[α]) ) do
// Thread t is valid
await (isEnabled(Lt [α]) or ! isValid(Lt [α]) );
if (isEnabled(Lt[α]) then
resolveValid(Lt [α]);
Ct [t] ← α; // Update thread clock
α ← α + 1; // Increment thunk counter
end
end
// The thread has terminated or a thunk has been invalidated
L0 t ← Lt ; // Make a temp copy for missing writes
while (t has not terminated or α < |L0 t |) do
// Thread t is invalid
if (α < |L0 t|) then
M ← M ∪ L0 t[α].W ; // Add missing writes
Ct [t] ← α; // Update thread clock
end
if (t has not terminated) then
resolveInvalid(Lt [α]);
end
α ← α + 1; // Increment thunk counter
end
// The thread has terminated
end

Algorithm 5: Subroutines for the incremental run algo-
rithm
isEnabled(Lt[α])
begin
if (∀i ∈ {1, ..., T } \ {t} : (Ci [i] > Lt [α].C[i])) then
// All thunks happened-before are resolved
return (isValid(Lt[α])); // check if it’s valid
end
returnfalse;
end
isValid(Lt [α])
begin
if ((Lt [α].R ∩ M ) = ∅) then
return true; // Read set does not intersects with dirty set
end
returnfalse;
end
resolveInvalid(Lt[α])
begin
startThunk(); // Same as initial run algorithm
repeat
Execute instruction of t;
if (instruction is load or store) then
onMemoryAccess(); // Same as initial run algorithm
end
until t invokes synchronization primitive;
M ← M ∪ Lt[α].W ; // Add the new writes
endThunk(); // Same as initial run algorithm
onSynchronization(s); // Same as initial run algorithm
end
resolveValid(Lt [α])
begin
address space ← memo(Lt [α].W ); // Globals and heap
stack ← memo(Lt[α].Stack);
CPU registers ← memo(Lt[α].Reg); // Also adjusts PC
onSynchronization(s); // Same as initial run algorithm
end
