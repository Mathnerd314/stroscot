Thunk "T1.a" $ do
  y <- Read "y"
  let y' = y + 1
  Write "y" y'
  Write "z" y'
Thunk "T2.a" $ do
  x <- Read "x"
  let x' = x + 1
  Write "x" x'
Thunk "T2.b" $ do
  x <- Read "x"
  z <- Read "z"
  let y = 2*x + z
  Write "y" y


bad = do
  Thunk "old" do
    Write "a"
    Write "b"
    ExecAfter ["y"] "x"
  Thunk "x" do
    Read "a"
    Die
  Thunk "y" do
    Read "b"
    Write "b"
    Die

-- record a message
Traced :: String -> M a -> M a
PutWhen :: Verbosity -> String -> Action ()
WithVerbosity :: Verbosity -> Action a -> Action a

BracketEx :: Bool -> IO a -> (a -> IO b) -> (a -> Action c) -> Action
            -- runOnSuccess alloc free act

data M a where
  Pure :: a -> M a
  LiftIO :: IO a -> M a
  Fmap :: (a -> b) -> M a -> M b
  Ap :: M (a -> b) -> M a -> M b
  Next :: M a -> M b -> M b
  Bind :: M a -> (a -> M b) -> M b

  Void :: M a -> M ()

  Read :: Key -> M Value
  Write :: !Key -> !Value -> M ()

  HappensBefore :: Label -> M ()
  RunAfter :: Label -> M () -> M ()

  -- todo: concurrently from https://hackage.haskell.org/package/async-2.2.2/docs/src/Control.Concurrent.Async.html#local-6989586621679035295
  -- or parallel from https://hackage.haskell.org/package/parallel-io-0.3.3/docs/src/Control-Concurrent-ParallelIO-Local.html#parallel

   happensBefore :: [Label],
   happensAfter :: [Label]


-- our filesystem access tracer needs configurable options,
-- to ignore files (to allow benign impurities), and/or to error on files (to enforce a static policy)


data Rules a where
    Thunk :: Label -> M a -> M (Thunk a)
    Get :: Thunk a -> a

    Rule :: (Label -> Maybe (M ())) -> M ()
    Alternatives :: M a -> M a -- use first matching rule instead of raising ambiguity error
    Priority :: Double -> M a -> M a -- use higher priority instead of raising ambiguity error
    Versioned :: Int -> M a -> M a -- indicate version for the rules

    -- | Create a finite resource, given a name (for error messages) and a quantity of the resource that exists.
    --   Shake will ensure that actions using the same finite resource do not execute in parallel.
    --   The main example is threads, it doesn't make sense to build 200 files in parallel on an 8-core system
    NewResource :: String -> Int -> M Resource
    NewThrottle :: String -> Int -> Double -> M Resource

    runInit, runCleanup :: Action () -> M ()

WithResources :: [(Resource, Int)] -> M () -> M ()
-- Shake uses lots of tiny threads, that need ~2kb stack.
-- GHC's thread stacks all start out at 1Kb, then grow in 32Kb chunks (by default).
-- These 32Kb chunks eat up all the space despite being only slightly occupied, almost 400Mb for 10k threads.
-- Shake's solution is to use a continuation monad to pause tasks without requiring a running thread.
-- really though, we just need to avoid starting threads. So all tasks start in the main thread and it's their responsibility to fork/yield.

-- https://github.com/ndmitchell/rattle/tree/master/test/Test/Example
-- | Try replicating the build scripts of fsatrace from:
--   https://github.com/jacereda/fsatrace/blob/master/Makefile
--   (note the two unix.mk and win.mk files)
--
--   We don't deal with the 32bit Windows build since our CI doesn't have a 32bit compiler
module Test.Example.FSATrace(main) where

import System.Info.Extra
import Development.Rattle
import Development.Shake.FilePath
import Test.Type


main :: IO ()
main = testGit "https://github.com/jacereda/fsatrace" $ do
    let plat = if isWindows then "win" else "unix"

    let srcs = ["src/fsatrace.c", "src/"++plat++"/proc.c", "src/"++plat++"/shm.c"] ++
               if isWindows then ["src/win/inject.c","src/win/dbg.c"] else []
    let sosrcs = ["src/unix/fsatraceso.c","src/emit.c","src/unix/shm.c","src/unix/proc.c"]
    let dllsrcs = map ("src/win/" ++) (words "fsatracedll.c inject.c patch.c hooks.c shm.c handle.c utf8.c dbg.c") ++ ["src/emit.c"]

    let cflags = "-g -std=c99 -Wall -O2 -fomit-frame-pointer -fno-stack-protector -MMD -DIS32=0"
    let cppflags | isWindows = "-D_WIN32_WINNT=0x600 -isysteminclude/ddk"
                 | otherwise = "-D_GNU_SOURCE -D_DEFAULT_SOURCE=1"

    let ldflags = [] :: [String]
    let ldlibs = if isWindows then "-lntdll -lpsapi" else "-ldl -lrt"
    let ldobjs = ["CRT_noglob.o" | isWindows && False]

    forP_ (srcs ++ if isWindows then dllsrcs else sosrcs) $ \x ->
        cmd "gcc -c" ["-fPIC" | not isWindows] cppflags cflags x "-o" (x -<.> "o")
    let os = map (-<.> "o")

    -- we use the leading _'s on fsatrace stuff so it doesn't conflict with the version
    -- we are using to do tracing ourselves

    if isWindows then
        cmd "gcc -shared" ldflags (os dllsrcs) "-o _fsatrace64.dll" ldlibs
    else
        cmd "gcc -shared" ldflags (os sosrcs) "-o _fsatrace.so" ldlibs

    cmd "gcc" ldflags ldobjs (os srcs) ldlibs "-o" ("_fsatrace" <.> exe)
