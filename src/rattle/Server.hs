{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, NamedFieldPuns #-}

module Development.Rattle.Server(
    Rattle, withRattle, Run(..),
    addCmdOptions
    ) where

import Control.Monad.Extra
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import General.Pool
import Development.Rattle.Types
import Development.Rattle.UI
import Development.Rattle.Shared
import Development.Rattle.Hash
import Development.Rattle.Hazards
import Development.Rattle.CmdOption
import Control.Exception.Extra
import Control.Concurrent.Extra
import General.Extra
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath
import System.FilePattern
import System.IO.Extra
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra
import General.FileName
import General.FileInfo
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as BS
import Data.Ord
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import Prelude


-- | Basic options for configuring rattle.
data RattleOptions = RattleOptions
    {rattleFiles :: FilePath -- ^ Where all my shared files go
    ,rattleSpeculate :: Maybe String -- ^ Should I speculate? Under which key?
    ,rattleMachine :: String -- ^ Key to store run#
    ,rattleShare :: Bool -- ^ Should I share files from the cache
    ,rattleProcesses :: Int -- ^ Number of simulateous processes
    ,rattleCmdOptions :: [C.CmdOption] -- ^ Extra options added to every command line
    ,rattleNamedDirs :: [(String, FilePath)] -- ^ Named directories, e.g. (PWD, .)
    ,rattleUI :: Maybe RattleUI -- ^ Nothing for auto detect
    ,rattleForward :: Bool -- ^ Support forward style stuff
    } deriving Show


-- | Default 'RattleOptions' value.
rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") "m1" True 0 [] [("PWD",".")] Nothing False


rattleOptionsExplicit :: RattleOptions -> IO RattleOptions
rattleOptionsExplicit = fixProcessorCount >=> fixNamedDirs
    where
        fixProcessorCount o
            | rattleProcesses o /= 0 = pure o
            | otherwise = do p <- getProcessorCount; pure o{rattleProcesses=p}

        fixNamedDirs o = do
            xs <- sequence [(a,) . addTrailingPathSeparator <$> canonicalizePath b | (a,b) <- rattleNamedDirs o]
            -- sort so all prefixes come last, so we get the most specific match
            pure o{rattleNamedDirs = sortOn (Down . snd) xs}


shorten :: [(String, String)] -> FileName -> FileName
shorten [] = id
shorten named = \x -> fromMaybe x $ firstJust (f x) named2
    where
        named2 = [(BSC.pack $ "$" ++ a ++ [pathSeparator], BSC.pack $ addTrailingPathSeparator b) | (a,b) <- named]
        f x (name,dir) = do rest <- BSC.stripPrefix dir $ fileNameToByteString x; pure $ byteStringToFileName $ name <> rest

expand :: [(String, String)] -> FileName -> FileName
expand [] = id
expand named = \x -> case BSC.uncons $ fileNameToByteString x of
    Just ('$', x)
        | (x1, x2) <- BSC.break isPathSeparator x
        , not $ BSC.null x2
        , Just v <- Map.lookup x1 named2
        -> byteStringToFileName $ v <> x2
    _ -> x
    where
        named2 = Map.fromList [(BSC.pack a, BSC.pack b) | (a,b) <- named]

-- | Type of actions to run. Executed using 'rattle'.
newtype Run a = Run {fromRun :: ReaderT Rattle IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance a ~ () => C.CmdArguments (Run a) where
    cmdArguments (C.CmdArgument x) = do
        let (opts, args) = partitionEithers x
        r <- Run ask
        liftIO $ cmdRattle r opts args



data S = S
    {started :: Map.HashMap Cmd (NoShow (IO ()))
        -- ^ Things that have got to running - if you find a duplicate just run the IO
        --   to wait for it.
    ,running :: [(Seconds, Cmd, Maybe (Touch FileName))]
        -- ^ Things currently running, with the time they started,
        --    and an amalgamation of their previous Trace (if we have any)
    ,hazard :: HazardSet
        -- ^ Things that have been read or written, at what time, and by which command
        --   Used to detect hazards.
        --   Read is recorded as soon as it can, Write as late as it can, as that increases hazards.
    ,pending :: [(Seconds, Cmd, Trace (FileName, ModTime, Hash))]
        -- ^ Things that have completed, and would like to get recorded, but have to wait
        --   to confirm they didn't cause hazards
    ,required :: [Cmd]
        -- ^ Things what were required by the user calling cmdRattle, not added due to speculation.
        --   Will be the 'speculate' list next time around.
    ,speculatable :: [(Cmd, Touch FileName)]
        -- ^ Things that were used in the last speculation with this name
    ,speculateNext :: Maybe Cmd
        -- ^ If I was to speculate, which would I do. A cached value computed from specutable, started, running and hazard
    } deriving Show

data Problem
    = Finished
    | Hazard Hazard

throwProblem :: Problem -> IO a
throwProblem Finished = fail "Finished, but still trying to do stuff"
throwProblem (Hazard h) = throwIO h

data Rattle = Rattle
    {options :: RattleOptions
    ,runIndex :: !RunIndex -- ^ Run# we are on
    ,state :: Var (Either Problem S)
    ,timer :: IO Seconds
    ,speculated :: IORef Bool
    ,speculatableWrites :: Set.HashSet FileName
    ,pool :: Pool
    ,ui :: UI
    ,shared :: Shared
    ,shortener :: FileName -> FileName
    }

addCmdOptions :: [C.CmdOption] -> Rattle -> Rattle
addCmdOptions new r@Rattle{options=o@RattleOptions{rattleCmdOptions=old}} =
    r{options = o{rattleCmdOptions = old ++ new}}


withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = withUI rattleUI (pure "Running") $ \ui -> withShared rattleFiles rattleShare $ \shared -> do
    options@RattleOptions{..} <- rattleOptionsExplicit options
    -- make sure we have a thread for the speculation too
    withNumCapabilities (rattleProcesses + 1) $ do

        when (rattleProcesses > 1 && not rtsSupportsBoundThreads) $
            putStrLn "WARNING: Running with multiple threads but not compiled with -threaded"

        let expander = expand rattleNamedDirs
        let shortener = shorten rattleNamedDirs
        speculatable <- if rattleProcesses <= 1 then pure [] else do
            speculatable <- maybe (pure []) (getSpeculate shared) rattleSpeculate
            fmap (takeWhile (not . null . snd)) $ -- don't speculate on things we have no traces for
                forM speculatable $ \x -> do
                    traces <- unsafeInterleaveIO (getCmdTraces shared x)
                    pure (x, normalizeTouch $ foldMap (fmap (expander . fst3) . tTouch) traces)
        let speculatableWrites = Set.fromList $ concatMap (tWrite . snd) speculatable
        speculated <- newIORef False

        runIndex <- nextRun shared rattleMachine
        timer <- offsetTime
        let s0 = S Map.empty [] emptyHazardSet [] [] speculatable Nothing
        state <- newVar $ Right $ ensureS s0

        let saveSpeculate state =
                whenJust rattleSpeculate $ \name ->
                    whenRightM (readVar state) $ \v ->
                        setSpeculate shared name $ reverse $ required v

        -- first try and run it
        let attempt1 = runPool True rattleProcesses $ \pool -> do
                let r = Rattle{..}
                runSpeculate r
                (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)
        attempt1 `catch` \(h :: Hazard) -> do
            b <- readIORef speculated
            if not (recoverableHazard h || restartableHazard h) then throwIO h else do
                -- if we speculated, and we failed with a hazard, try again
                putStrLn "Warning: Speculation lead to a hazard, retrying without speculation"
                print h
                state <- newVar $ Right s0{speculatable=[]}
                runPool True rattleProcesses $ \pool -> do
                    let r = Rattle{..}
                    (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)

-- Kick off the speculation pool worker thread
runSpeculate :: Rattle -> IO ()
runSpeculate rattle@Rattle{..} = when (rattleProcesses options > 1) $
    addPool PoolSpeculate pool $ do
        run <- modifyS rattle $ \case
            s@S{speculateNext=Just cmd} -> do
                writeIORef speculated True
                cmdRattleStarted rattle cmd s ["speculative"]
            _ -> pure (Right Nothing, pure ())
        -- run the command but ignore all errors, if there are real errors
        -- whoever reruns them will bump into them
        ignore run

modifyS :: Rattle -> (S -> IO (Either Problem (Maybe S), IO a)) -> IO (IO a)
modifyS rattle@Rattle{..} act = modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> do
        (res, cont) <- act s
        case res of
            Left e -> pure (Left e, cont)
            Right Nothing -> pure (Right s, cont)
            Right (Just s) -> pure (Right $ ensureS s, runSpeculate rattle >> cont)

ensureS :: S -> S
ensureS = fillInNext . reduceSpeculate
    where
        -- often we drain the front of the speculatable list repeatedly, so do that once
        reduceSpeculate s = s{speculatable = dropWhile (\(c, _) -> c `Map.member` started s) $ speculatable s}
        fillInNext s = s{speculateNext = calculateSpeculateNext s}


-- speculate on a process iff it is the first process in speculate that:
-- 1) we have some parallelism free
-- 2) it is the first eligible in the list
-- 3) not already been started
-- 4) no read/write conflicts with anything completed
-- 5) no read conflicts with anything running or any earlier speculation
calculateSpeculateNext :: S -> Maybe Cmd
calculateSpeculateNext S{speculatable, running, started, hazard}
    -- I have things to speculate, and I know exactly what is running
    | not $ null speculatable, Just xs <- mapM thd3 running = step (newTouchSet xs) speculatable
    | otherwise = Nothing
    where
        -- Note the TouchSet.tsWrite has been filtered to speculatableWrites
        -- which is sufficient because we only check values of tWrite from speculatable
        step :: TouchSet -> [(Cmd, Touch FileName)] -> Maybe Cmd
        step _ [] = Nothing
        step rw ((x,_):xs)
            | x `Map.member` started = step rw xs -- do not update the rw, since its already covered
        step rw ((x, t@Touch{..}):xs)
            | not $ any (\v -> v `Set.member` tsRead rw || v `Set.member` tsWrite rw || seenHazardSet v hazard) tWrite
                -- if anyone I write has ever been read or written, or might be by an ongoing thing, that would be bad
            , not $ any (`Set.member` tsWrite rw) tRead
                -- if anyone I read might be being written right now, that would be bad
                = Just x
            | otherwise
                = step (addTouchSet rw t) xs


cmdRattle :: Rattle -> [C.CmdOption] -> [String] -> IO ()
cmdRattle rattle opts args = cmdRattleRequired rattle $ mkCmd (rattleCmdOptions (options rattle) ++ opts) args

cmdRattleRequired :: Rattle -> Cmd -> IO ()
cmdRattleRequired rattle@Rattle{..} cmd = addPoolWait PoolRequired pool $ do
    modifyVar_ state $ pure . fmap (\s -> s{required = cmd : required s})
    cmdRattleStart rattle cmd

cmdRattleStart :: Rattle -> Cmd -> IO ()
cmdRattleStart rattle cmd = join $ modifyS rattle $ \s ->
    cmdRattleStarted rattle cmd s []

cmdRattleStarted :: Rattle -> Cmd -> S -> [String] -> IO (Either Problem (Maybe S), IO ())
cmdRattleStarted rattle@Rattle{..} cmd s msgs = do
    start <- timer
    case Map.lookup cmd (started s) of
        Just (NoShow wait) -> pure (Right Nothing, wait)
        Nothing -> do
            hist <- unsafeInterleaveIO $ map (fmap (\(f,mt,h) -> (expand (rattleNamedDirs options) f, mt, h))) <$> getCmdTraces shared cmd
            go <- once $ cmdRattleRun rattle cmd start hist msgs

            -- we only speculate on the very last one
            -- and we only care about reads which might be speculated as writes
            let trimReads t = t{tRead = filter (`Set.member` speculatableWrites) $ tRead t}
            let specHist = if null hist then Nothing else Just $ trimReads $ fmap fst3 $ tTouch $ last hist

            s <- pure s{running = (start, cmd, specHist) : running s}
            s <- pure s{started = Map.insert cmd (NoShow go) $ started s}
            pure (Right $ Just s, go)


-- either fetch it from the cache or run it)
cmdRattleRun :: Rattle -> Cmd -> Seconds -> [Trace (FileName, ModTime, Hash)] -> [String] -> IO ()
cmdRattleRun rattle@Rattle{..} cmd@(Cmd _ opts args) startTimestamp hist msgs = do
    let forwardOpt = rattleForward options
    let match (fp, mt, h) = (== Just h) <$> (if forwardOpt then hashFileForwardIfStale else hashFileIfStale) fp mt h
    histRead <- filterM (allM match . tRead . tTouch) hist
    histBoth <- filterM (allM match . tWrite . tTouch) histRead
    case histBoth of
        t:_ ->
            -- we have something consistent at this point, no work to do
            -- technically we aren't writing to the tWrite part of the trace, but if we don't include that
            -- skipping can turn write/write hazards into read/write hazards
            cmdRattleFinished rattle startTimestamp cmd t False
        [] -> do
            -- lets see if any histRead's are also available in the cache
            fetcher <- memoIO $ getFile shared
            let fetch (fp, mt, h) = do v <- fetcher h; case v of Nothing -> pure Nothing; Just op -> pure $ Just $ op fp
            download <- if not (rattleShare options)
                then pure Nothing
                else firstJustM (\t -> fmap (t,) <$> allMaybeM fetch (tWrite $ tTouch t)) histRead
            case download of
                Just (t, download) -> do
                    display ["copying"] $ sequence_ download
                    cmdRattleFinished rattle startTimestamp cmd t False
                Nothing -> do
                    start <- timer
                    (opts2, c) <- display [] $ cmdRattleRaw ui opts args
                    stop <- timer
                    touch <- fsaTrace c
                    when forwardOpt $
                        checkHashForwardConsistency touch
                    let pats = matchMany [((), x) | Ignored xs <- opts2, x <- xs]
                    --let hasTrailingPathSeparator x = if BS.null x then False else isPathSeparator $ BS.last x
                    let hasTrailingPathSeparatorBS = maybe False (isPathSeparator . snd) . BS.unsnoc
                    let skip x = BS.isPrefixOf slashDev (fileNameToByteString x) ||
                                 hasTrailingPathSeparatorBS (fileNameToByteString x) ||
                                 pats [((),fileNameToString x)] /= []
                    let f hasher xs = mapMaybeM (\x -> fmap (\(mt,h) -> (x,mt,h)) <$> hasher x) $ filter (not . skip) xs
                    let reads = concat [x | Read x <- opts2]
                    let writes = concat [x | Write x <- opts2]
                    let plus real fake = if null fake then real else nubOrd $ real ++ map fileNameFromString fake
                    touch <- Touch
                        <$> f (if forwardOpt then hashFileForward else hashFile) (tRead touch `plus` reads)
                        <*> f hashFile (tWrite touch `plus` writes)
                    touch <- if forwardOpt then generateHashForwards cmd [x | HashNonDeterministic xs <- opts2, x <- xs] touch else pure touch
                    when (rattleShare options) $
                        forM_ (tWrite touch) $ \(fp, mt, h) ->
                            setFile shared fp h ((== Just h) <$> hashFileIfStale fp mt h)
                    cmdRattleFinished rattle startTimestamp cmd (Trace runIndex start stop touch) True
    where
        display :: [String] -> IO a -> IO a
        display msgs2 = addUI ui (headDef cmdline overrides) (unwords $ msgs ++ msgs2)
        overrides = [x | C.Traced x <- opts] ++ [x | C.UserCommand x <- opts]
        cmdline = unwords $ ["cd " ++ x ++ " &&" | C.Cwd x <- opts] ++ args

slashDev :: BS.ByteString
slashDev = BS.pack "/dev/"

cmdRattleRaw :: UI -> [C.CmdOption] -> [String] -> IO ([CmdOption2], [C.FSATrace BS.ByteString])
cmdRattleRaw ui opts args = do
    (opts, opts2) <- pure $ partitionEithers $ map fromCmdOption opts
    case [x | WriteFile x <- opts2] of
        [] -> do
            let optsUI = if isControlledUI ui then [C.EchoStdout False,C.EchoStderr False] else []
            res <- C.cmd (opts ++ optsUI) args
            pure (opts2, res)
        files -> do
            forM_ files $ \file -> do
                createDirectoryIfMissing True $ takeDirectory file
                writeFileUTF8 file $ concat args
            pure (opts2, map (C.FSAWrite . UTF8.fromString) files)

checkHashForwardConsistency :: Touch FileName -> IO ()
checkHashForwardConsistency Touch{..} = do
    -- check that anyone who is writing forwarding hashes is writing the actual file
    let sources = mapMaybe fromHashForward tWrite
    let bad = sources \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the forwarding file, but not the source: " ++ show bad

    -- and anyone writing to a file with a hash also updates it
    forwards <- filterM doesFileNameExist $ mapMaybe toHashForward tWrite
    let bad = forwards \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the source file which has a forwarding hash, but didn't touch the hash: " ++ show bad


-- | If you have been asked to generate a forwarding hash for writes
generateHashForwards :: Cmd -> [FilePattern] -> Touch (FileName, ModTime, Hash) -> IO (Touch (FileName, ModTime, Hash))
generateHashForwards cmd ms t = do
    let match = matchMany $ map ((),) ms
    let (normal, forward) = partition (\(x, _, _) -> isJust (toHashForward x) && null (match [((), fileNameToString x)])) $ tWrite t
    let Hash hash = hashString $ show (cmd, tRead t, normal)
    let hhash = hashHash $ Hash hash
    forward <- forM forward $ \(x,mt,_) -> do
        let Just x2 = toHashForward x -- checked this is OK earlier
        BS.writeFile (fileNameToString x2) hash
        pure (x2, mt,hhash)
    pure t{tWrite = tWrite t ++ forward}

-- | I finished running a command
cmdRattleFinished :: Rattle -> Seconds -> Cmd -> Trace (FileName, ModTime, Hash) -> Bool -> IO ()
cmdRattleFinished rattle@Rattle{..} start cmd trace@Trace{..} save = join $ modifyS rattle $ \s -> do
    -- update all the invariants
    stop <- timer
    s <- pure s{running = filter ((/= start) . fst3) $ running s}
    s <- pure s{pending = [(stop, cmd, trace) | save] ++ pending s}

    -- look for hazards
    -- push writes to the end, and reads to the start, because reads before writes is the problem
    case addHazardSet (required s) (hazard s) start stop cmd $ fmap fst3 tTouch of
        (ps@(p:_), _) -> pure (Left $ Hazard p, print ps >> throwIO p)
        ([], hazard2) -> do
            s <- pure s{hazard = hazard2}

            -- move people out of pending if they have survived long enough
            maxTimestamp <- timer
            let earliest = minimum $ maxTimestamp : map fst3 (running s)
            (safe, pending) <- pure $ partition (\x -> fst3 x < earliest) $ pending s
            s <- pure s{pending = pending}
            pure (Right $ Just s, forM_ safe $ \(_,c,t) -> addCmdTrace shared c $ fmap (\(f,mt,h) -> (shortener f, mt,h)) t)
