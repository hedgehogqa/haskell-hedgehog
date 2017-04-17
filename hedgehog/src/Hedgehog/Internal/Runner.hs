{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Hedgehog.Internal.Runner (
  -- * Running Individual Properties
    check
  , recheck

  -- * Running Groups of Properties
  , RunnerConfig(..)
  , checkConcurrent
  , checkSequential
  , checkGroup

  -- * Internal
  , checkReport
  , checkConsoleRegion
  , checkNamed
  ) where

import           Control.Concurrent.Async (forConcurrently)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.TMVar as TMVar
import           Control.Monad (when)
import           Control.Monad.Catch (MonadMask(..), MonadCatch(..), catchAll, bracket)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified GHC.Conc as Conc

import           Hedgehog.Gen (runGen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Property (Group(..), GroupName(..))
import           Hedgehog.Internal.Property (Property(..), PropertyConfig(..), PropertyName(..))
import           Hedgehog.Internal.Property (ShrinkLimit, withTests)
import           Hedgehog.Internal.Property (Test, Log(..), Failure(..), runTest)
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import           Hedgehog.Range (Size)

import           Language.Haskell.TH.Lift (deriveLift)

import           System.Console.Regions (ConsoleRegion, RegionLayout(..), LiftRegion)
import qualified System.Console.Regions as Console
import           System.Environment (lookupEnv)

import           Text.Read (readMaybe)


-- | Configuration for a property test run.
--
data RunnerConfig =
  RunnerConfig {
      -- | The number of property tests to run concurrently. 'Nothing' means
      --   use one worker per processor.
      runnerWorkers :: !(Maybe Int)
    } deriving (Eq, Ord, Show)

findM :: Monad m => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] ->
      return def
    x0 : xs ->
      p x0 >>= \m ->
        case m of
          Nothing ->
            findM xs def p
          Just x ->
            return x

isFailure :: Node m (Maybe (Either x a, b)) -> Bool
isFailure = \case
  Node (Just (Left _, _)) _ ->
    True
  _ ->
    False

takeSmallest ::
     MonadIO m
  => Size
  -> Seed
  -> ShrinkCount
  -> ShrinkLimit
  -> (Progress -> m ())
  -> Node m (Maybe (Either Failure (), [Log]))
  -> m Result
takeSmallest size seed shrinks slimit updateUI = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Left (Failure loc err mdiff) -> do
        let
          failure =
            mkFailure size seed shrinks loc err mdiff w

        updateUI $ Shrinking failure

        if shrinks >= fromIntegral slimit then
          -- if we've hit the shrink limit, don't shrink any further
          pure $ Failed failure
        else
          findM xs (Failed failure) $ \m -> do
            o <- runTree m
            if isFailure o then
              Just <$> takeSmallest size seed (shrinks + 1) slimit updateUI o
            else
              return Nothing

      Right () ->
        return OK

checkReport ::
     forall m.
     MonadIO m
  => MonadCatch m
  => PropertyConfig
  -> Size
  -> Seed
  -> Test m ()
  -> (Report Progress -> m ())
  -> m (Report Result)
checkReport cfg size0 seed0 test0 updateUI =
  let
    test =
      catchAll test0 (fail . show)

    loop :: TestCount -> DiscardCount -> Size -> Seed -> m (Report Result)
    loop !tests !discards !size !seed = do
      updateUI $ Report tests discards Running

      if size > 99 then
        -- size has reached limit, reset to 0
        loop tests discards 0 seed

      else if tests >= fromIntegral (propertyTestLimit cfg) then
        -- we've hit the test limit, test was successful
        pure $ Report tests discards OK

      else if discards >= fromIntegral (propertyDiscardLimit cfg) then
        -- we've hit the discard limit, give up
        pure $ Report tests discards GaveUp

      else
        case Seed.split seed of
          (s0, s1) -> do
            node@(Node x _) <-
              runTree . Gen.runDiscardEffect $ runGen size s0 (runTest test)
            case x of
              Nothing ->
                loop tests (discards + 1) (size + 1) s1

              Just (Left _, _) ->
                let
                  mkReport =
                    Report (tests + 1) discards
                in
                  fmap mkReport $
                    takeSmallest
                      size
                      seed
                      0
                      (propertyShrinkLimit cfg)
                      (updateUI . mkReport)
                      node

              Just (Right (), _) ->
                loop (tests + 1) discards (size + 1) s1
  in
    loop 0 0 size0 seed0

checkConsoleRegion ::
     MonadIO m
  => ConsoleRegion
  -> Maybe PropertyName
  -> Size
  -> Seed
  -> Property
  -> m (Report Result)
checkConsoleRegion region name size seed prop =
  liftIO $ do
    result <-
      checkReport (propertyConfig prop) size seed (propertyTest prop) $ \status ->
        Console.setConsoleRegion region =<<
          renderProgress name status

    Console.setConsoleRegion region =<<
      renderResult name result

    pure result

checkNamed :: MonadIO m => ConsoleRegion -> Maybe PropertyName -> Property -> m Bool
checkNamed region name prop = do
  seed <- liftIO Seed.random
  result <- checkConsoleRegion region name 0 seed prop
  pure $
    reportStatus result == OK

-- | Check a property.
--
check :: MonadIO m => Property -> m Bool
check prop =
  liftIO . displayRegion $ \region ->
    checkNamed region Nothing prop

-- | Check a property using a specific size and seed.
--
recheck :: MonadIO m => Size -> Seed -> Property -> m ()
recheck size seed prop0 = do
  let prop = withTests 1 prop0
  _ <- liftIO . displayRegion $ \region ->
    checkConsoleRegion region Nothing size seed prop
  pure ()

-- | Check a group of properties using the specified runner config.
--
checkGroup :: MonadIO m => RunnerConfig -> Group -> m Bool
checkGroup config (Group group props) =
  liftIO $ do
    n <- maybe getNumWorkers pure (runnerWorkers config)

    -- ensure few spare capabilities for concurrent-output, it's likely that
    -- our tests will saturate all the capabilities they're given.
    updateNumCapabilities (n + 2)

    putStrLn $ "━━━ " ++ unGroupName group ++ " ━━━"
    Console.displayConsoleRegions $ do
      remaining <- Console.openConsoleRegion Linear

      let
        start (TasksRemaining tasks) _ix (name, prop) =
          liftIO $ do
            ppremaining <- renderRemaining $ PropertyCount tasks

            atomically $ do
              region <- Console.openConsoleRegion Linear
              moveToBottom remaining

              if tasks == 0 then
                Console.closeConsoleRegion remaining
              else
                Console.setConsoleRegion remaining ppremaining

              pure (name, prop, region)

        finalize (_name, _prop, region) =
          finishRegion region

      runTasks n props start finalize $ \(name, prop, region) ->
        checkNamed region (Just name) prop

-- | Check a group of properties sequentially.
--
--   Using Template Haskell for property discovery:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkSequential $$(discover)
--
--   With manually specified properties:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkSequential $ Group "Test.Example" [
-- >       ("prop_reverse", prop_reverse)
-- >     ]
--
--
checkSequential :: MonadIO m => Group -> m Bool
checkSequential =
  checkGroup
    RunnerConfig {
        runnerWorkers =
          Just 1
      }

-- | Check a group of properties concurrently.
--
--   Using Template Haskell for property discovery:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkConcurrent $$(discover)
--
--   With manually specified properties:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkConcurrent $ Group "Test.Example" [
-- >       ("prop_reverse", prop_reverse)
-- >     ]
--
checkConcurrent :: MonadIO m => Group -> m Bool
checkConcurrent =
  checkGroup
    RunnerConfig {
        runnerWorkers =
          Nothing
      }

------------------------------------------------------------------------
-- quick and dirty task queue

newtype TaskIndex =
  TaskIndex Int
  deriving (Eq, Ord, Enum, Num)

newtype TasksRemaining =
  TasksRemaining Int

dequeueMVar ::
     MVar [(TaskIndex, a)]
  -> (TasksRemaining -> TaskIndex -> a -> IO b)
  -> IO (Maybe (TaskIndex, b))
dequeueMVar mvar start =
  MVar.modifyMVar mvar $ \case
    [] ->
      pure ([], Nothing)
    (ix, x) : xs -> do
      y <- start (TasksRemaining $ length xs) ix x
      pure (xs, Just (ix, y))

runTasks ::
     Int
  -> [a]
  -> (TasksRemaining -> TaskIndex -> a -> IO b)
  -> (b -> IO ())
  -> (b -> IO Bool)
  -> IO Bool
runTasks n tasks start finalize runTask = do
  qvar <- MVar.newMVar (zip [0..] tasks)
  fvar <- MVar.newMVar (-1, Map.empty)

  let
    worker r0 = do
      mx <- dequeueMVar qvar start
      case mx of
        Nothing ->
          pure r0
        Just (ix, x) -> do
          r <- runTask x
          finalizeTask fvar ix (finalize x)
          worker (r0 && r)

  -- FIXME ensure all workers have finished running
  fmap and . forConcurrently [1..max 1 n] $ \_ix ->
    worker True

runActiveFinalizers ::
     MonadIO m
  => MVar (TaskIndex, Map TaskIndex (IO ()))
  -> m ()
runActiveFinalizers mvar =
  liftIO $ do
    again <-
      MVar.modifyMVar mvar $ \original@(minIx, finalizers0) ->
        case Map.minViewWithKey finalizers0 of
          Nothing ->
            pure (original, False)

          Just ((ix, finalize), finalizers) ->
            if ix == minIx + 1 then do
              finalize
              pure ((ix, finalizers), True)
            else
              pure (original, False)

    when again $
      runActiveFinalizers mvar

finalizeTask ::
     MonadIO m
  => MVar (TaskIndex, Map TaskIndex (IO ()))
  -> TaskIndex
  -> IO ()
  -> m ()
finalizeTask mvar ix finalize = do
  liftIO . MVar.modifyMVar_ mvar $ \(minIx, finalizers) ->
    pure (minIx, Map.insert ix finalize finalizers)
  runActiveFinalizers mvar

------------------------------------------------------------------------
-- concurrent-output utils

displayRegion ::
     MonadIO m
  => MonadMask m
  => LiftRegion m
  => (ConsoleRegion -> m a)
  -> m a
displayRegion =
  Console.displayConsoleRegions .
  bracket (Console.openConsoleRegion Linear) finishRegion

moveToBottom :: ConsoleRegion -> STM ()
moveToBottom region = do
  mxs <- TMVar.tryTakeTMVar Console.regionList
  case mxs of
    Nothing ->
      pure ()
    Just xs0 ->
      let
        xs1 =
          filter (/= region) xs0
      in
        TMVar.putTMVar Console.regionList (region : xs1)

finishRegion :: MonadIO m => ConsoleRegion -> m ()
finishRegion region =
  liftIO . atomically $ do
    content <- Console.getConsoleRegion region
    Console.finishConsoleRegion region content

-- | Update the number of capabilities but never set it lower than it already
--   is.
--
updateNumCapabilities :: Int -> IO ()
updateNumCapabilities n = do
  ncaps <- Conc.getNumCapabilities
  Conc.setNumCapabilities (max n ncaps)

getNumWorkers :: IO Int
getNumWorkers = do
  menv <- (readMaybe =<<) <$> lookupEnv "HEDGEHOG_WORKERS"
  case menv of
    Nothing ->
      Conc.getNumProcessors
    Just env ->
      pure env

------------------------------------------------------------------------
-- FIXME Replace with DeriveLift when we drop 7.10 support.

$(deriveLift ''RunnerConfig)
