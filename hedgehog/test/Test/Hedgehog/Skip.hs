{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Hedgehog.Skip where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Config as Config
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Internal.Runner as Runner
import           Hedgehog.Internal.Report (Report(..), Result(..), FailureReport(..))

-- | We use this property to help test skipping. It keeps a log of every time it
--   runs in the 'IORef' it's passed.
--
--   It ignores its seed. It fails at size 2. When it shrinks, it initially
--   shrinks to something that will pass, and then to something that will fail.
skipTestProperty :: IORef [(Size, Int, Bool)] -> Property
skipTestProperty logRef =
  withTests 5 . property $ do
    val@(_, _, shouldPass) <- forAll $ do
      curSize <- Gen.sized pure
      (shouldPass, nShrinks) <-
        (,)
          <$> Gen.shrink (\b -> if b then [] else [True]) (pure $ curSize /= 2)
          <*> Gen.shrink (\n -> reverse [0 .. n-1]) (pure 3)
      pure (curSize, nShrinks, shouldPass)

    liftIO $ IORef.modifyIORef' logRef (val :)
    assert shouldPass

checkProp :: MonadIO m => Property -> m (Report Result)
checkProp prop = do
  seed <- Config.resolveSeed Nothing
  liftIO $ Runner.checkReport (Property.propertyConfig prop)
                              0
                              seed
                              (Property.propertyTest prop)
                              (const $ pure ())

prop_SkipNothing :: Property
prop_SkipNothing =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop = Property.withSkip Property.SkipNothing $ skipTestProperty logRef

    report <- checkProp prop
    case reportStatus report of
      Failed f -> do
        failureShrinks f === 3
        failureShrinkPath f === Property.ShrinkPath [1, 1, 1]

      _ ->
        failure

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs ===
      [ (0, 3, True)
      , (1, 3, True)
      , (2, 3, False)
      , (2, 3, True)
      , (2, 2, False)
      , (2, 2, True)
      , (2, 1, False)
      , (2, 1, True)
      , (2, 0, False)
      , (2, 0, True)
      ]

prop_SkipToFailingTest :: Property
prop_SkipToFailingTest =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop = Property.withSkip (Property.SkipToTest 3) $ skipTestProperty logRef

    report <- checkProp prop
    case reportStatus report of
      Failed f -> do
        failureShrinks f === 3
        failureShrinkPath f === Property.ShrinkPath [1, 1, 1]

      _ ->
        failure

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs ===
      [ (2, 3, False)
      , (2, 3, True)
      , (2, 2, False)
      , (2, 2, True)
      , (2, 1, False)
      , (2, 1, True)
      , (2, 0, False)
      , (2, 0, True)
      ]

prop_SkipPastFailingTest :: Property
prop_SkipPastFailingTest =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop = Property.withSkip (Property.SkipToTest 4) $ skipTestProperty logRef

    report <- checkProp prop
    reportStatus report === OK

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs === [(3, 3, True), (4, 3, True)]

prop_SkipToNoShrink :: Property
prop_SkipToNoShrink =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop = Property.withSkip (Property.SkipToShrink 3 $ Property.ShrinkPath [])
        $ skipTestProperty logRef

    report <- checkProp prop
    case reportStatus report of
      Failed f -> do
        failureShrinks f === 0
        failureShrinkPath f === Property.ShrinkPath []

      _ ->
        failure

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs === [(2, 3, False)]

prop_SkipToFailingShrink :: Property
prop_SkipToFailingShrink =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop = Property.withSkip (Property.SkipToShrink 3 $ Property.ShrinkPath [1, 1])
        $ skipTestProperty logRef

    report <- checkProp prop
    case reportStatus report of
      Failed f -> do
        failureShrinks f === 2
        failureShrinkPath f === Property.ShrinkPath [1, 1]

      _ ->
        failure

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs === [(2, 3, False), (2, 2, False), (2, 1, False)]

prop_SkipToPassingShrink :: Property
prop_SkipToPassingShrink =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      -- ShrinkPath is stored in reverse order of what you'd expect.
      prop = Property.withSkip (Property.SkipToShrink 3 $ Property.ShrinkPath [0, 1])
        $ skipTestProperty logRef

    report <- checkProp prop
    reportStatus report === OK

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs === [(2, 3, False), (2, 2, False), (2, 2, True)]

tests :: IO Bool
tests =
  checkParallel $$(discover)
