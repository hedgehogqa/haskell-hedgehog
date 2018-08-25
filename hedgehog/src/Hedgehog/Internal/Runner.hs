{-# OPTIONS_HADDOCK not-home #-}
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
import           Control.Monad (void)

import           Data.Semigroup ((<>))

import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Gen (runGenT, runDiscardEffect)
import           Hedgehog.Internal.Property (Group(..), GroupName(..))
import           Hedgehog.Internal.Property (Property(..), PropertyConfig(..), PropertyName(..))
import           Hedgehog.Internal.Property (ShrinkLimit, ShrinkRetries, withTests)
import           Hedgehog.Internal.Property (PropertyT(..), Log(..), Failure(..), runTestT)
import           Hedgehog.Internal.Queue
import           Hedgehog.Internal.Region
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import           Hedgehog.Range (Size)

import           Language.Haskell.TH.Lift (deriveLift)


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

isSuccess :: Node m (Maybe (Either x a, b)) -> Bool
isSuccess =
  not . isFailure

runTreeN ::
     Monad m
  => ShrinkRetries
  -> Tree m (Maybe (Either x a, b))
  -> m (Node m (Maybe (Either x a, b)))
runTreeN n m = do
  o <- runTree m
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
  -> Node m (Maybe (Either Failure (), [Log]))
  -> m Result
takeSmallest size seed shrinks slimit retries updateUI = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Left (Failure loc err mdiff) -> do
        let
          failure =
            mkFailure size seed shrinks loc err mdiff (reverse w)

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
              runTree . runDiscardEffect $ runGenT size s0 . runTestT $ unPropertyT test
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
                      (propertyShrinkRetries cfg)
                      (updateUI . mkReport)
                      node

              Just (Right (), _) ->
                loop (tests + 1) discards (size + 1) s1
  in
    loop 0 0 size0 seed0

checkRegion ::
     MonadIO m
  => Region
  -> Maybe UseColor
  -> Maybe PropertyName
  -> Size
  -> Seed
  -> Property a
  -> m (Report Result)
checkRegion region mcolor name size seed prop =
  liftIO $ do
    result <-
      checkReport (propertyConfig prop) size seed (void $ propertyTest prop) $ \progress -> do
        ppprogress <- renderProgress mcolor name progress
        case reportStatus progress of
          Running ->
            setRegion region ppprogress
          Shrinking _ ->
            openRegion region ppprogress

    ppresult <- renderResult mcolor name result
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
  -> Maybe UseColor
  -> Maybe PropertyName
  -> Property a
  -> m (Report Result)
checkNamed region mcolor name prop = do
  seed <- liftIO Seed.random
  checkRegion region mcolor name 0 seed prop

-- | Check a property.
--
check :: MonadIO m => Property a -> m Bool
check prop =
  liftIO . displayRegion $ \region ->
    (== OK) . reportStatus <$> checkNamed region Nothing Nothing prop

-- | Check a property using a specific size and seed.
--
recheck :: MonadIO m => Size -> Seed -> Property a -> m ()
recheck size seed prop0 = do
  let prop = withTests 1 prop0
  _ <- liftIO . displayRegion $ \region ->
    checkRegion region Nothing Nothing size seed prop
  pure ()

-- | Check a group of properties using the specified runner config.
--
checkGroup :: MonadIO m => RunnerConfig -> Group a -> m Bool
checkGroup config (Group group props) =
  liftIO $ do
    n <- resolveWorkers (runnerWorkers config)

    -- ensure few spare capabilities for concurrent-output, it's likely that
    -- our tests will saturate all the capabilities they're given.
    updateNumCapabilities (n + 2)

    putStrLn $ "━━━ " ++ unGroupName group ++ " ━━━"

    verbosity <- resolveVerbosity (runnerVerbosity config)
    summary <- checkGroupWith n verbosity (runnerColor config) props

    pure $
      summaryFailed summary == 0 &&
      summaryGaveUp summary == 0

updateSummary :: Region -> TVar Summary -> Maybe UseColor -> (Summary -> Summary) -> IO ()
updateSummary sregion svar mcolor f = do
  summary <- atomically (TVar.modifyTVar' svar f >> TVar.readTVar svar)
  setRegion sregion =<< renderSummary mcolor summary

checkGroupWith ::
     WorkerCount
  -> Verbosity
  -> Maybe UseColor
  -> [(PropertyName, Property a)]
  -> IO Summary
checkGroupWith n verbosity mcolor props =
  displayRegion $ \sregion -> do
    svar <- atomically . TVar.newTVar $ mempty { summaryWaiting = PropertyCount (length props) }

    let
      start (TasksRemaining tasks) _ix (name, prop) =
        liftIO $ do
          updateSummary sregion svar mcolor $ \x -> x {
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
        updateSummary sregion svar mcolor $ \x -> x {
            summaryRunning =
              summaryRunning x - 1
          }

      finalize (_name, _prop, region) =
        finishRegion region

    summary <-
      fmap (mconcat . fmap (fromResult . reportStatus)) $
        runTasks n props start finish finalize $ \(name, prop, region) -> do
          result <- checkNamed region mcolor (Just name) prop
          updateSummary sregion svar mcolor
            (<> fromResult (reportStatus result))
          pure result

    updateSummary sregion svar mcolor (const summary)
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
checkSequential :: MonadIO m => Group a -> m Bool
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
checkParallel :: MonadIO m => Group a -> m Bool
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

------------------------------------------------------------------------
-- FIXME Replace with DeriveLift when we drop 7.10 support.

$(deriveLift ''RunnerConfig)
