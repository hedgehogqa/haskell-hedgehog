{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Hedgehog.Skip where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Foldable (for_)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Config as Config
import           Hedgehog.Internal.Property (Skip(..), ShrinkPath(..), skipCompress, skipDecompress)
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Internal.Runner as Runner
import           Hedgehog.Internal.Report (Report(..), Result(..), FailureReport(..))

-- | We use this property to help test skipping. It keeps a log of every time it
--   runs in the 'IORef' it's passed.
--
--   It ignores its seed. The third test fails. When it shrinks, it initially
--   shrinks to something that will pass, and then to something that will fail.
--
skipTestProperty :: IORef [(Size, Int, Bool)] -> Property
skipTestProperty logRef =
  withTests 5 . property $ do
    val@(curSize, _, shouldPass) <- forAll $ do
      -- With 5 tests, size goes 0, 24, 48, 72, 96.
      curSize <- Gen.sized $ pure . (`div` 24)
      (shouldPass, nShrinks) <-
        (,)
          <$> Gen.shrink (\b -> if b then [] else [True]) (pure $ curSize /= 2)
          <*> Gen.shrink (\n -> reverse [0 .. n-1]) (pure 3)
      pure (curSize, nShrinks, shouldPass)

    -- Fail coverage to make sure we disable it when shrinking.
    cover 100 "Not 4" (curSize /= 4)

    liftIO $ IORef.modifyIORef' logRef (val :)
    assert shouldPass

checkProp :: MonadIO m => Property -> m (Report Result)
checkProp prop = do
  seed <- Config.resolveSeed Nothing
  liftIO $ Runner.checkReport
    (Property.propertyConfig prop)
    seed
    (Property.propertyTest prop)
    (const $ pure ())

prop_SkipNothing :: Property
prop_SkipNothing =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop =
        withSkip "" $ skipTestProperty logRef

    report <- checkProp prop
    case reportStatus report of
      Failed f -> do
        failureShrinks f === 3
        failureShrinkPath f === ShrinkPath [1, 1, 1]

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
      prop =
        withSkip "3" $ skipTestProperty logRef

    report <- checkProp prop
    case reportStatus report of
      Failed f -> do
        failureShrinks f === 3
        failureShrinkPath f === ShrinkPath [1, 1, 1]

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
      prop =
        withSkip "4" $ skipTestProperty logRef

    report <- checkProp prop
    reportStatus report === OK

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs === [(3, 3, True), (4, 3, True)]

prop_SkipToNoShrink :: Property
prop_SkipToNoShrink =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []
    let
      prop =
        withSkip "3:" $ skipTestProperty logRef

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
      prop =
        withSkip "3:b2" $ skipTestProperty logRef

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
      prop =
        withSkip "3:bA" $ skipTestProperty logRef

    report <- checkProp prop
    reportStatus report === OK

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs === [(2, 3, False), (2, 2, False), (2, 2, True)]

genSkip :: Gen Skip
genSkip =
  let
    range =
      Range.linear 0 100

    genTestCount =
      Property.TestCount <$> Gen.int range

    genShrinkPath =
      Property.ShrinkPath <$> Gen.list range (Gen.int range)
  in
    Gen.choice
      [ pure SkipNothing
      , SkipToTest <$> genTestCount
      , SkipToShrink <$> genTestCount <*> genShrinkPath
      ]

-- | Test that `skipCompress` and `skipDecompress` roundtrip.
prop_compressSkip :: Property
prop_compressSkip =
  property $ do
    skip <- forAll genSkip
    tripping skip Property.skipCompress Property.skipDecompress

-- | Demonstrate some example compressions.
--
--   In general it's probably fine for compressions to change between hedgehog
--   versions. There's not much reason to share them or save them long-term. So
--   breaking this test isn't necessarily a problem, if it's done deliberately.
--
--   But it's useful to have examples, to avoid accidentally changing the
--   compression format and to demonstrate edge cases.
prop_compressDecompressExamples :: Property
prop_compressDecompressExamples =
  withTests 1 . property $ do
    let
      -- Each test case has a Skip, the result of compressing it, and some other
      -- strings that would decompress to the same Skip.
      testCases =
        [ (SkipNothing, "", [])
        , (SkipToTest 3, "3", ["03", "003"])
        , (SkipToTest 197, "197", ["0197", "00197"])
        , ( SkipToShrink 5 $ Property.ShrinkPath [2, 3, 0]
          , "5:cDa"
          , ["5:CdA", "05:c1b0D1A1"]
          )
        , ( SkipToShrink 21 $ Property.ShrinkPath [5, 3, 27, 27, 26]
          , "21:fDbb2BA"
          , ["21:fDbbBBba"]
          )
        ]

    for_ testCases $ \(skip, compressed, otherCompressions) -> do
      skipCompress skip === compressed
      for_ (compressed : otherCompressions) $ \c ->
        skipDecompress c === Just skip

tests :: IO Bool
tests =
  checkParallel $$(discover)
