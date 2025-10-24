module Test.Resources(main) where

import Test.Tasty
import Test.Tasty.HUnit

import Pool
import Types
import Resource
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Heap as Heap
import Data.Map(Map)
import Data.Set(Set)
import Data.Heap(Heap)
import Control.Concurrent.Extra
import Control.Exception.Extra
import Data.Traversable
import Data.IORef
import Control.Monad.Reader
import Control.Monad
import Data.List.Extra
import Data.Functor((<&>))
import Control.Monad.IO.Unlift

-- See #474, we should never be running pool actions masked
assertUnmasked :: HasCallStack => Assertion
assertUnmasked = do
  s <- getMaskingState
  assertBool "Unmasked masking state" $ s == Unmasked

gM :: TestName -> (TestName -> (Pool -> IO Global) -> TestTree) -> TestTree
gM name test = testGroup name $
  [False,True] <&> \deterministic ->
    test ((if deterministic then "" else "non-") ++ "deterministic") $ \pool -> do
      rRand <- getResourceRand deterministic
      rId <- getCounter
      tId <- getCounter
      pure $ Global {
        state = undefined,
        diskState = undefined,
        thunkList = undefined,
        locks = undefined,
        execThunk = undefined,
        resourceRandAction = rRand,
        resourceIdAction = rId,
        taskIdAction = tId,
        globalPoolV = pool
      }

nprio = Priority 0

main = defaultMain $ localOption (mkTimeout 1) $ testGroup "Tests"
  [ testCase "Pool terminates promptly when exceptions are thrown" $ do
    good <- newVar True
    started <- newBarrier
    result <- newBarrier
    runPool (\pool -> do
        addPool pool $ do
          waitBarrier started
          throwIO Underflow
        addPool pool $ do
          signalBarrier started ()
          sleep 10
          modifyVar_ good $ const $ pure False)
      (signalBarrier result)
    -- the pool finishes once all threads die
    (ex, max, sum) <- waitBarrier result
    fromException <$> ex @?= Just (Just Underflow)
    (max,sum) @?= (2, 2)
    readVar good @? "Must be true"

  , testCase "nested spawning works" $ do
    done <- newBarrier
    died <- newBarrier
    runPool (\pool ->
      addPool pool $
        addPool pool $
          signalBarrier done ()
      ) (signalBarrier died)
    waitBarrier done
    res <- waitBarrier died
    res @?= (Nothing, 2, 2)

  , testCase "killing a thread stops the pool and propagates the right exception" $ do
    -- https://code.google.com/archive/p/ndmitchell/issues/545
    thread <- newBarrier
    died <- newBarrier
    doneMain <- newBarrier
    doneSub <- newBarrier
    runPool (\pool -> (do
      t <- myThreadId
      addPool pool $
        (do
          throwTo t Overflow
          sleep 10) `catch` signalBarrier doneSub
      sleep 10
      ) `catch` signalBarrier doneMain) (signalBarrier died)
    (ex, max, sum) <- waitBarrier died
    show ex @?= "arithmetic overflow"
    (max, sum) @?= (2, 2)
    subE <- waitBarrier doneSub
    mainE <- waitBarrier doneMain
    show subE @?= "ThreadKilledDueTo arithmetic overflow"
    show mainE @?= "arithmetic overflow"

  , testCase "a thread pool waits for all threads before it returns" $ do
    var <- newVar False
    died <- newBarrier
    runPool (\pool -> do
      started <- newBarrier
      addPool pool $ (do signalBarrier started (); sleep 10) `finally` (do sleep 1; writeVar var True)
      addPool pool $ do waitBarrier started; throw Overflow
      ) (signalBarrier died)
    _ <- waitBarrier died
    readVar var >>= (@?= True)

  , gM "Resource Ord and Show" $ \n mkGlobal -> testCase n $ mkGlobal undefined >>= \g -> flip runReaderT g $ do
    r1 <- newFinite "test" 2
    r2 <- newThrottle "special" 67 1 2
    liftIO $ assertBool "Resources should have a good ordering" $ r1 < r2
    liftIO $ assertBool "Resource should contain their name when shown" $ "special" `isInfixOf` show r2

  , gM "Finite cap" $ \n mkGlobal -> testGroup n $
    [1..6] <&> \cap -> testCase ("Cap " ++ show cap) $ do
      -- check that it aims for exactly the limit
      inside <- newIORef (0, 0, 0) -- (current, maximum, ran count)
      result <- liftIO $ newBarrier

      runPool (\pool -> mkGlobal pool >>= \g -> flip runReaderT g $ do
        r <- newFinite "test" cap
        let mp = Map.fromList [(r,1)]

        replicateM_ 5 $ spawnWithResource mp nprio $ do
          i <- atomicModifyIORef inside $ \(i,m,cnt) ->
            let newI = i+1; new = (newI,newI `max` m, cnt+1)
            in (new, newI)
          assertBool "Too many resources in use at one time" $ i <= cap
          sleep 0.1
          atomicModifyIORef inside $ \(i,m,cnt) -> ((i-1,m,cnt), ())
        ) (signalBarrier result)
      ret <- waitBarrier result
      ret @?= (Nothing, min cap 5, min cap 5)
      var <- readIORef inside
      var @?= (0, min cap 5, 5)

  , gM "Finite Priority" $ \n mkGlobal -> testCase n $ do
    -- check high priority stuff runs first
    res <- newVar []
    result <- newBarrier
    runPool (\pool -> mkGlobal pool >>= \g -> flip runReaderT g $ do
      r <- newFinite "test" 1
      let mp = Map.fromList [(r,1)]
      let note c = modifyVar_ res $ pure . (c:)
      -- deliberately in a random order
      forM_ [1,5,2,3,4,6] $ \prio -> spawnWithResource mp (Priority prio) $ note prio
      ) (signalBarrier result)

    _ <- waitBarrier result
    readVar res >>= (@?= [2,3,4,5,6,1]) -- the first executes immediately

  , gM "Things can still run while a Finite is held" $ \n mkGlobal -> testCase n $ do
    done <- newIORef 0
    result <- newBarrier
    runPool (\pool -> mkGlobal pool >>= \g -> flip runReaderT g $ do
      lock <- newFinite "lock" 1
      spawnWithResource (Map.fromList [(lock,1)]) nprio $ sleep 0.5
      forM_ [1..10] $ \_ -> spawnWithResource Map.empty nprio $ atomicModifyIORef done $ \i -> (i+1,())
      spawnWithResource (Map.fromList [(lock,1)]) nprio $ do
        done <- readIORef done
        assertBool "Non-blocked finished" $ done == 10
      ) (signalBarrier result)

    _ <- waitBarrier result
    pure ()

  , localOption (mkTimeout 1.5) $ gM "Throttle works properly" $ \n mkGlobal -> testCase n $ do
    count <- newIORef 0
    start <- now
    result <- newBarrier
    runPool (\pool -> mkGlobal pool >>= \g -> flip runReaderT g $ do
      res <- newThrottle "throttle" 2 1 0.4
      forM_ (zip [0..] [1,1,2,1,2]) $ \(n,i) -> do
        spawnWithResource (Map.fromList [(res,i)]) (Priority $ negate n) $ do
          old <- liftIO $ atomicModifyIORef count $ \i -> (i+1,i)
          when (n == 2) $ liftIO $ sleep 0.2
      ) (signalBarrier result)

    _ <- waitBarrier result
    old <- readIORef count
    assertBool "All executed" $ old == 5
    end <- now
    let duration = end - start
    assertBool "Expected throttle duration" $ duration >= 1.2 && duration < 1.5
  ]

-- benchmark for testing thread performance, see https://github.com/ndmitchell/shake/pull/751
benchPool = do
    start <- now
    withNumCapabilities 4 $ do
      runPool (\pool ->
        replicateM_ 200000 $ addPool pool $ pure ()) (\_ -> do
      end <- now
      print $ end - start
      )

