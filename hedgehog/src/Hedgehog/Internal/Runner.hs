{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hedgehog.Internal.Runner (
  -- * Running Individual Properties
    check
  , recheck

  -- * Running Groups of Properties
  , RunnerConfig(..)
  , checkParallel
  , checkSequential
  , checkGroup

  -- * Internal
  , checkReport
  , checkRegion
  , checkNamed
  ) where

import           Control.Concurrent.STM (TVar, atomically)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Catch (MonadCatch(..), catchAll)
import           Control.Monad.IO.Class (MonadIO(..))

import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Gen (evalGenT)
import           Hedgehog.Internal.Prelude
import           Hedgehog.Internal.Property (DiscardCount(..), ShrinkCount(..))
import           Hedgehog.Internal.Property (Group(..), GroupName(..))
import           Hedgehog.Internal.Property (Journal(..), Coverage(..), CoverCount(..))
import           Hedgehog.Internal.Property (Property(..), PropertyConfig(..), PropertyName(..))
import           Hedgehog.Internal.Property (PropertyT(..), Failure(..), runTestT)
import           Hedgehog.Internal.Property (ShrinkLimit, ShrinkRetries, withTests)
import           Hedgehog.Internal.Property (TerminationCriteria(..))
import           Hedgehog.Internal.Property (TestCount(..), PropertyCount(..))
import           Hedgehog.Internal.Property (confidenceSuccess, confidenceFailure)
import           Hedgehog.Internal.Property (coverageSuccess, journalCoverage)
import           Hedgehog.Internal.Property (defaultMinTests)
import           Hedgehog.Internal.Queue
import           Hedgehog.Internal.Region
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (TreeT(..), NodeT(..))
import           Hedgehog.Range (Size)

import           Language.Haskell.TH.Syntax (Lift)

#if mingw32_HOST_OS
import           System.IO (hSetEncoding, stdout, stderr, utf8)
#endif

-- | Configuration for a property test run.
--
data RunnerConfig =
  RunnerConfig {
      -- | The number of property tests to run concurrently. 'Nothing' means
      --   use one worker per processor.
      runnerWorkers :: !(Maybe WorkerCount)

      -- | Whether to use colored output or not. 'Nothing' means detect from
      --   the environment.
    , runnerColor :: !(Maybe UseColor)

      -- | How verbose to be in the runner output. 'Nothing' means detect from
      --   the environment.
    , runnerVerbosity :: !(Maybe Verbosity)
    } deriving (Eq, Ord, Show, Lift)

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

isFailure :: NodeT m (Maybe (Either x a, b)) -> Bool
isFailure = \case
  NodeT (Just (Left _, _)) _ ->
    True
  _ ->
    False

isSuccess :: NodeT m (Maybe (Either x a, b)) -> Bool
isSuccess =
  not . isFailure

runTreeN ::
     Monad m
  => ShrinkRetries
  -> TreeT m (Maybe (Either x a, b))
  -> m (NodeT m (Maybe (Either x a, b)))
runTreeN n m = do
  o <- runTreeT m
  if n > 0 && isSuccess o then
    runTreeN (n - 1) m
  else
    pure o

takeSmallest ::
     MonadIO m
  => Size
  -> Seed
  -> ShrinkCount
  -> ShrinkLimit
  -> ShrinkRetries
  -> (Progress -> m ())
  -> NodeT m (Maybe (Either Failure (), Journal))
  -> m Result
takeSmallest size seed shrinks slimit retries updateUI = \case
  NodeT Nothing _ ->
    pure GaveUp

  NodeT (Just (x, (Journal logs))) xs ->
    case x of
      Left (Failure loc err mdiff) -> do
        let
          failure =
            mkFailure size seed shrinks Nothing loc err mdiff (reverse logs)

        updateUI $ Shrinking failure

        if shrinks >= fromIntegral slimit then
          -- if we've hit the shrink limit, don't shrink any further
          pure $ Failed failure
        else
          findM xs (Failed failure) $ \m -> do
            o <- runTreeN retries m
            if isFailure o then
              Just <$> takeSmallest size seed (shrinks + 1) slimit retries updateUI o
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
  -> PropertyT m ()
  -> (Report Progress -> m ())
  -> m (Report Result)
checkReport cfg size0 seed0 test0 updateUI =
  let
    test =
      catchAll test0 (fail . show)

    terminationCriteria =
      propertyTerminationCriteria cfg

    (confidence, minTests) =
      case terminationCriteria of
        EarlyTermination c t -> (Just c, t)
        NoEarlyTermination c t -> (Just c, t)
        NoConfidenceTermination t -> (Nothing, t)

    successVerified count coverage =
      count `mod` 100 == 0 &&
      -- If the user wants a statistically significant result, this function
      -- will run a confidence check. Otherwise, it will default to checking
      -- the percentage of encountered labels
      maybe False (\c -> confidenceSuccess count c coverage) confidence

    failureVerified count coverage =
      -- Will be true if we can statistically verify that our coverage was
      -- inadequate.
      -- Testing only on 100s to minimise repeated measurement statistical
      -- errors.
      count `mod` 100 == 0 &&
      maybe False (\c -> confidenceFailure count c coverage) confidence

    loop ::
         TestCount
      -> DiscardCount
      -> Size
      -> Seed
      -> Coverage CoverCount
      -> m (Report Result)
    loop !tests !discards !size !seed !coverage0 = do
      updateUI $ Report tests discards coverage0 Running

      let
        coverageReached =
          successVerified tests coverage0

        coverageUnreachable =
          failureVerified tests coverage0

        enoughTestsRun =
          case terminationCriteria of
            EarlyTermination _ _ ->
              tests >= fromIntegral defaultMinTests &&
                (coverageReached || coverageUnreachable)
            NoEarlyTermination _ _ ->
              tests >= fromIntegral minTests
            NoConfidenceTermination _ ->
              tests >= fromIntegral minTests

        labelsCovered =
          coverageSuccess tests coverage0

        successReport =
          Report tests discards coverage0 OK

        failureReport message =
          Report tests discards coverage0 . Failed $ mkFailure
            size
            seed
            0
            (Just coverage0)
            Nothing
            message
            Nothing
            []

        confidenceReport =
          if coverageReached && labelsCovered then
            successReport
          else
            failureReport $
              "Test coverage cannot be reached after " <> show tests <> " tests"

      if size > 99 then
        -- size has reached limit, reset to 0
        loop tests discards 0 seed coverage0

      else if enoughTestsRun then
        -- at this point, we know that enough tests have been run in order to
        -- make a decision on if this was a successful run or not
        --
        -- If we have early termination, then we need to check coverageReached / coverageUnreachable
        pure $ case terminationCriteria of
          EarlyTermination _ _ -> confidenceReport
          NoEarlyTermination _ _ -> confidenceReport
          NoConfidenceTermination _ ->
            if labelsCovered then
              successReport
            else
              failureReport $
                "Labels not sufficently covered after " <> show tests <> " tests"

      else if discards >= fromIntegral (propertyDiscardLimit cfg) then
        -- we've hit the discard limit, give up
        pure $ Report tests discards coverage0 GaveUp

      else
        case Seed.split seed of
          (s0, s1) -> do
            node@(NodeT x _) <-
              runTreeT . evalGenT size s0 . runTestT $ unPropertyT test
            case x of
              Nothing ->
                loop tests (discards + 1) (size + 1) s1 coverage0

              Just (Left _, _) ->
                let
                  mkReport =
                    Report (tests + 1) discards coverage0
                in
                  fmap mkReport $
                    takeSmallest
                      size
                      seed
                      0
                      (propertyShrinkLimit cfg)
                      (propertyShrinkRetries cfg)
                      (updateUI . mkReport)
                      node

              Just (Right (), journal) ->
                let
                  coverage =
                    journalCoverage journal <> coverage0
                in
                  loop (tests + 1) discards (size + 1) s1 coverage
  in
    loop 0 0 size0 seed0 mempty

checkRegion ::
     MonadIO m
  => Region
  -> UseColor
  -> Maybe PropertyName
  -> Size
  -> Seed
  -> Property
  -> m (Report Result)
checkRegion region color name size seed prop =
  liftIO $ do
    result <-
      checkReport (propertyConfig prop) size seed (propertyTest prop) $ \progress -> do
        ppprogress <- renderProgress color name progress
        case reportStatus progress of
          Running ->
            setRegion region ppprogress
          Shrinking _ ->
            openRegion region ppprogress

    ppresult <- renderResult color name result
    case reportStatus result of
      Failed _ ->
        openRegion region ppresult
      GaveUp ->
        openRegion region ppresult
      OK ->
        setRegion region ppresult

    pure result

checkNamed ::
     MonadIO m
  => Region
  -> UseColor
  -> Maybe PropertyName
  -> Property
  -> m (Report Result)
checkNamed region color name prop = do
  seed <- liftIO Seed.random
  checkRegion region color name 0 seed prop

-- | Check a property.
--
check :: MonadIO m => Property -> m Bool
check prop = do
  color <- detectColor
  liftIO . displayRegion $ \region ->
    (== OK) . reportStatus <$> checkNamed region color Nothing prop

-- | Check a property using a specific size and seed.
--
recheck :: MonadIO m => Size -> Seed -> Property -> m ()
recheck size seed prop0 = do
  color <- detectColor
  let prop = withTests 1 prop0
  _ <- liftIO . displayRegion $ \region ->
    checkRegion region color Nothing size seed prop
  pure ()

-- | Check a group of properties using the specified runner config.
--
checkGroup :: MonadIO m => RunnerConfig -> Group -> m Bool
checkGroup config (Group group props) =
  liftIO $ do
    n <- resolveWorkers (runnerWorkers config)

    -- ensure few spare capabilities for concurrent-output, it's likely that
    -- our tests will saturate all the capabilities they're given.
    updateNumCapabilities (n + 2)

#if mingw32_HOST_OS
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
#endif

    putStrLn $ "━━━ " ++ unGroupName group ++ " ━━━"

    verbosity <- resolveVerbosity (runnerVerbosity config)
    color <- resolveColor (runnerColor config)
    summary <- checkGroupWith n verbosity color props

    pure $
      summaryFailed summary == 0 &&
      summaryGaveUp summary == 0

updateSummary :: Region -> TVar Summary -> UseColor -> (Summary -> Summary) -> IO ()
updateSummary sregion svar color f = do
  summary <- atomically (TVar.modifyTVar' svar f >> TVar.readTVar svar)
  setRegion sregion =<< renderSummary color summary

checkGroupWith ::
     WorkerCount
  -> Verbosity
  -> UseColor
  -> [(PropertyName, Property)]
  -> IO Summary
checkGroupWith n verbosity color props =
  displayRegion $ \sregion -> do
    svar <- atomically . TVar.newTVar $ mempty { summaryWaiting = PropertyCount (length props) }

    let
      start (TasksRemaining tasks) _ix (name, prop) =
        liftIO $ do
          updateSummary sregion svar color $ \x -> x {
              summaryWaiting =
                PropertyCount tasks
            , summaryRunning =
                summaryRunning x + 1
            }

          atomically $ do
            region <-
              case verbosity of
                Quiet ->
                  newEmptyRegion
                Normal ->
                  newOpenRegion

            moveToBottom sregion

            pure (name, prop, region)

      finish (_name, _prop, _region) =
        updateSummary sregion svar color $ \x -> x {
            summaryRunning =
              summaryRunning x - 1
          }

      finalize (_name, _prop, region) =
        finishRegion region

    summary <-
      fmap (mconcat . fmap (fromResult . reportStatus)) $
        runTasks n props start finish finalize $ \(name, prop, region) -> do
          result <- checkNamed region color (Just name) prop
          updateSummary sregion svar color
            (<> fromResult (reportStatus result))
          pure result

    updateSummary sregion svar color (const summary)
    pure summary

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
      , runnerColor =
          Nothing
      , runnerVerbosity =
          Nothing
      }

-- | Check a group of properties in parallel.
--
--   /Warning: although this check function runs tests faster than/
--   /'checkSequential', it should be noted that it may cause problems with/
--   /properties that are not self-contained. For example, if you have a group/
--   /of tests which all use the same database table, you may find that they/
--   /interfere with each other when being run in parallel./
--
--   Using Template Haskell for property discovery:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkParallel $$(discover)
--
--   With manually specified properties:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkParallel $ Group "Test.Example" [
-- >       ("prop_reverse", prop_reverse)
-- >     ]
--
checkParallel :: MonadIO m => Group -> m Bool
checkParallel =
  checkGroup
    RunnerConfig {
        runnerWorkers =
          Nothing
      , runnerColor =
          Nothing
      , runnerVerbosity =
          Nothing
      }
