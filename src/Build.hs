  type Task f k v =
    { fetch :: k -> f v
    , key :: k } -> f v

-- | This scheduler builds keys recursively: to build a key it executes the
-- associated task, discovering its dependencies on the fly, and if one of the
-- dependencies is dirty, the task is suspended until the dependency is rebuilt.
-- It stores the set of keys that have already been built as part of the state
-- to avoid executing the same task twice.
suspending :: forall i k v. Ord k => Scheduler Monad i i k v
suspending rebuilder tasks target store = fst $ execState (fetch target) (store, Set.empty)
  where
    fetch :: k -> State (Store i k v, Set k) v
    fetch key = do
        done <- gets snd
        case tasks key of
            Just task | key `Set.notMember` done -> do
                value <- gets (getValue key . fst)
                let newTask :: Task (MonadState i) k v
                    newTask = rebuilder key value task
                newValue <- liftRun newTask fetch
                modify $ \(s, d) -> (updateValue key value newValue s, Set.insert key d)
                return newValue
            _ -> gets (getValue key . fst) -- fetch the existing value



-- | This rebuilder uses modification time to decide whether a key is dirty and
-- needs to be rebuilt. Used by Make.
modTimeRebuilder :: Ord k => Rebuilder Applicative (MakeInfo k) k v
modTimeRebuilder key value task = Task $ \fetch -> do
    (now, modTimes) <- get
    let dirty = case Map.lookup key modTimes of
            Nothing -> True
            time -> any (\d -> Map.lookup d modTimes > time) (dependencies task)
    if not dirty
    then return value
    else do
        put (now + 1, Map.insert key now modTimes)
        run task fetch


------------------------------ Constructive traces -----------------------------
-- | This rebuilder relies on constructive traces.
ctRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (CT k v) k v
ctRebuilder key value task = Task $ \fetch -> do
    cachedValues <- constructCT key (fmap hash . fetch) =<< get
    if value `elem` cachedValues
    then return value -- The current value has been verified, let's keep it
    else case cachedValues of
        (cachedValue:_) -> return cachedValue -- Any cached value will do
        _ -> do -- No cached values, need to run the task
            (newValue, deps) <- track task fetch
            modify $ recordCT key newValue [ (k, hash v) | (k, v) <- deps ]
            return newValue
            
-- | A model of Cloud Shake: a monadic build system that uses constructive
-- traces to check if a key is up to date as well as for caching build results.
cloudShake :: (Ord k, Hashable v) => Build Monad (CT k v) k v
cloudShake = suspending ctRebuilder
