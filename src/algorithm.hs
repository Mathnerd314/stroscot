-- a memory location, filename, etc. that can be looked up
type Key = ByteString
-- the entry point of a thunk, where a computation can be recorded or replayed
type Label = ByteString

data ThunkRecord = ThunkRecord
 {
   readSet :: [Key],
   writeSet :: [Key],
   happensBefore :: [Label],
   happensAfter :: [Label]
 }

executeThunk :: Label -> M ThunkRecord

-- Shake uses lots of tiny threads, that need ~2kb stack.
-- GHC's thread stacks all start out at 1Kb, then grow in 32Kb chunks (by default).
-- These 32Kb chunks eat up all the space despite being only slightly occupied, almost 400Mb for 10k threads.
-- The solution is probably to rethink Pool, perhaps using a continuation monad to pause tasks without requiring a running thread.

data M a where
    Fmap :: (a -> b) -> M a -> M b

    Pure :: a -> M a
    LiftIO :: IO a -> M a

    Ap :: M (a -> b) -> M a -> M b
    Next :: M a -> M b -> M b
    Bind :: M a -> (a -> M b) -> M b

    Void :: M a -> M ()

    Read :: Key -> M ByteString
    Write :: !Key -> !ByteString -> M ()

    HappensBefore :: Label -> M ()

    -- | Run the list of @IO@ actions concurrently, and return the results.
    --   If any action throws an exception at any time, then all other actions are
    --   'cancel'led, and the exception is rethrown.
    Concurrently :: [M a] -> M [a]
    -- todo: concurrently from https://hackage.haskell.org/package/async-2.2.2/docs/src/Control.Concurrent.Async.html#local-6989586621679035295
    -- or parallel from https://hackage.haskell.org/package/parallel-io-0.3.3/docs/src/Control-Concurrent-ParallelIO-Local.html#parallel


    -- | Create a finite resource, given a name (for error messages) and a quantity of the resource that exists.
    --   Shake will ensure that actions using the same finite resource do not execute in parallel.
    --   The main example is threads, only 
    NewResource :: String -> Int -> M Resource
    NewThrottle :: String -> Int -> Double -> M Resource
    WithResources :: [(Resource, Int)] -> Action a -> Action a

    -- record a message
    Traced :: String -> M a -> M a

    PutWhen :: Verbosity -> String -> Action ()
    WithVerbosity :: Verbosity -> Action a -> Action a


-- shake has a rules monad with a few operations for defining rules:
-- rule, versioned, priority, alternatives. also action and rulesio to run things at the beginning.

data ActionADT a where
    ActionBoom :: Bool -> Action a -> IO b -> Action a
    RunAfter :: IO () -> Action ()
    Apply :: Rule key value => [key] -> Action [value]
    ApplyKeyValue :: [Key] -> Action [Value]

    BlockApply :: String -> Action a -> Action a
    UnsafeExtraThread :: Action a -> Action a
    UnsafeIgnoreDependencies :: Action a -> Action a

-- our filesystem access tracer needs configurable options,
-- to ignore files (to allow benign impurities), and/or to error on files (to enforce a static policy)

forever (threadDelay maxBound)
concurrently :: [IO a] -> IO a
concurrently actions =

-- | Memoize an IO action which is recursive
memoRec :: (Eq a, Hashable a, MonadIO m) => ((a -> m b) -> a -> m b) -> m (a -> m b)
memoRec f = do
    var <- liftIO $ newVar Map.empty
    let go x =
            join $ liftIO $ modifyVar var $ \mp -> case Map.lookup x mp of
                Just bar -> pure (mp, liftIO $ waitBarrier bar)
                Nothing -> do
                    bar <- newBarrier
                    pure (Map.insert x bar mp, do v <- f go x; liftIO $ signalBarrier bar v; pure v)
    pure go

state :: HashMap Key Value
-- shake uses HashMap Key Word32 + MutableArray (Maybe Value), assigning index id's sequentially from 0
-- it seems to be to speed up storing stacks/dependency lists.
-- Here, we don't have stacks, but compressing the key lists might be worth it.

applyKeyValue :: [Key] -> Action [Value]
applyKeyValue ks = do
  is <- nubOrd $ ks
  wait <- forM is $ liftIO $ getKeyValueFromId database i >>= \case
    Ready r -> Now $ Right r
    Failed e _ -> Now $ Left e
    Running (NoShow w) r -> do
      let w2 v = w v >> continue v
      setMem database i k $ Running (NoShow w2) r
    Loaded r -> Later $ \continue -> do
      addPool (if isLeft x then PoolException else PoolResume) globalPool $
      setIdKeyStatus global database i k (Running (NoShow continue) r)
      liftIO $ addPool PoolStart globalPool $
        res <- builtinRun k (fmap result r) mode
        globalRuleFinished k
        producesCheck
      \case
            Left e ->
                continue . Left . toException =<< shakeException global stack e
            Right (RunResult{..}, Local{..})
                | runChanged == ChangedNothing || runChanged == ChangedStore, Just r <- r ->
                    continue $ Right $ RunResult runChanged runStore (r{result = mkResult runValue runStore})
                | otherwise -> do
                    dur <- time
                    let (cr, c) | Just r <- r, runChanged == ChangedRecomputeSame = (ChangedRecomputeSame, changed r)
                                | otherwise = (ChangedRecomputeDiff, globalStep)
                    continue $ Right $ RunResult cr runStore Result
                        {result = mkResult runValue runStore
                        ,changed = c
                        ,built = globalStep
                        ,depends = nubDepends $ reverse localDepends
                        ,execution = doubleToFloat $ dur - localDiscount
                        ,traces = reverse localTraces}
            where
                mkResult value store = (value, if globalOneShot then BS.empty else store)

                    let val = fmap runValue res
                    res <- liftIO $ getKeyValueFromId database i
                    w <- case res of
                        Just (_, Running (NoShow w) _) -> pure w
                        -- We used to be able to hit here, but we fixed it by ensuring the thread pool workers are all
                        -- dead _before_ any exception bubbles up
                        _ -> throwM $ errorInternal $ "expected Waiting but got " ++ maybe "nothing" (statusType . snd) res ++ ", key " ++ show k
                    setIdKeyStatus global database i k $ either mkError Ready val
                    w val
                case res of
                    Right RunResult{..} | runChanged /= ChangedNothing -> setDisk database i k $ Loaded runValue{result=runStore}
                    _ -> pure ()
    Action $ modifyRW $ \s -> s{localDepends = Depends is : localDepends s}


runKey
    :: Global
    -> Stack  -- Given the current stack with the key added on
    -> Key -- The key to build
    -> Maybe (Result BS.ByteString) -- A previous result, or Nothing if never been built before
    -> RunMode -- True if any of the children were dirty
    -> Capture (Either SomeException (RunResult (Result (Value, BS_Store))))
        -- Either an error, or a (the produced files, the result).
runKey global@Global{globalOptions=ShakeOptions{..},..} stack k r mode continue = do






Algorithm 1: Basic algorithm for the incremental run
dirty-set ← {changed input};
executeThread(t)
forall sub-computations in thread t do
// Check a sub-computation’s validity in happens-before order
if (read-set ∩ dirty-set) then
– recompute the sub-computation
– add the write-set to the dirty-set
else
– skip execution of the sub-computation
– write memoized value of the write-set to address space
end
end

Algorithm 2: The initial run algorithm
/* Let S be the set of synchronization objects and T be the number
of threads in the system. */
∀s ∈ S, ∀i ∈ {1, ..., T } : Cs [i] ← 0; // All sync clocks set to zero
executeThread(t)
begin
initThread(t);
while t has not terminated do
startThunk(); // Start new thunk
repeat
Execute instruction of t;
if (instruction is load or store) then
onMemoryAccess();
end
until t invokes synchronization primitive;
endThunk(); // Memoize the end state of thunk
α ← α + 1; // Increment thunk counter
// Let s denote invoked synchronization primitive
onSynchronization(s);
end
end

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
