{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Hedgehog.Skip where

import           Control.Monad (when)
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
--   It ignores its seed. It discards at size 1 and fails at size 2. When it
--   shrinks, it initially shrinks to something that will pass, and then to
--   something that will fail.
--
skipTestProperty :: IORef [(Size, Int, Bool, Bool)] -> Property
skipTestProperty logRef =
  withTests 5 . property $ do
    val@(curSize, _, shouldDiscard, shouldPass) <- forAll $ do
      curSize <- Gen.sized pure
      (shouldDiscard, shouldPass, nShrinks) <-
        (,,)
          <$> pure (curSize == 1)
          <*> Gen.shrink (\b -> if b then [] else [True]) (pure $ curSize /= 2)
          <*> Gen.shrink (\n -> reverse [0 .. n-1]) (pure 3)
      pure (curSize, nShrinks, shouldDiscard, shouldPass)

    -- Fail coverage to make sure we disable it when shrinking.
    cover 100 "Not 4" (curSize /= 4)

    liftIO $ IORef.modifyIORef' logRef (val :)
    when shouldDiscard discard
    assert shouldPass

checkProp :: MonadIO m => Property -> m (Report Result)
checkProp prop = do
  seed <- Config.resolveSeed Nothing
  liftIO $ Runner.checkReport
    (Property.propertyConfig prop)
    0
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

      _ -> do
        annotateShow report
        failure

    logs <- liftIO $ reverse <$> IORef.readIORef logRef
    logs ===
      [ (0, 3, False, True)
      , (1, 3, True, True)
      , (2, 3, False, False)
      , (2, 3, False, True)
      , (2, 2, False, False)
      , (2, 2, False, True)
      , (2, 1, False, False)
      , (2, 1, False, True)
      , (2, 0, False, False)
      , (2, 0, False, True)
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
      [ (2, 3, False, False)
      , (2, 3, False, True)
      , (2, 2, False, False)
      , (2, 2, False, True)
      , (2, 1, False, False)
      , (2, 1, False, True)
      , (2, 0, False, False)
      , (2, 0, False, True)
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
    logs === [(3, 3, False, True), (4, 3, False, True)]

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
    logs === [(2, 3, False, False)]

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
    logs === [(2, 3, False, False), (2, 2, False, False), (2, 1, False, False)]

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
    logs === [(2, 3, False, False), (2, 2, False, False), (2, 2, False, True)]

prop_SkipToReportedShrink :: Property
prop_SkipToReportedShrink =
  withTests 1 . property $ do
    logRef <- liftIO $ IORef.newIORef []

    report1 <- checkProp $ skipTestProperty logRef
    failure1 <- case reportStatus report1 of
      Failed f -> pure f
      _ -> do
        annotateShow report1
        failure

    let
      skip = SkipToShrink (reportTests report1)
                          (reportDiscards report1)
                          (failureShrinkPath failure1)


    report2 <- checkProp $ withSkip skip $ skipTestProperty logRef
    failure2 <- case reportStatus report2 of
      Failed f -> pure f
      _ -> do
        annotateShow report2
        failure

    failure1 === failure2

    reportTests report1 === 2
    reportTests report2 === 2
    reportDiscards report1 === 1
    reportDiscards report2 === 1

genSkip :: Gen Skip
genSkip =
  let
    range =
      Range.linear 0 100

    genTestCount =
      Property.TestCount <$> Gen.int range

    genDiscardCount =
      Property.DiscardCount <$> Gen.int range

    genShrinkPath =
      Property.ShrinkPath <$> Gen.list range (Gen.int range)
  in
    Gen.choice
      [ pure SkipNothing
      , SkipToTest <$> genTestCount <*> genDiscardCount
      , SkipToShrink <$> genTestCount <*> genDiscardCount <*> genShrinkPath
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
        , (SkipToTest 3 0, "3", ["03", "003", "3/0", "03/00"])
        , (SkipToTest 197 1, "197/1", ["0197/1", "00197/01"])
        , ( SkipToShrink 5 0 $ Property.ShrinkPath [2, 3, 0]
          , "5:cDa"
          , ["5:CdA", "05:c1b0D1A1"]
          )
        , ( SkipToShrink 21 3 $ Property.ShrinkPath [5, 3, 27, 27, 26]
          , "21/3:fDbb2BA"
          , ["21/3:fDbbBBba"]
          )
        ]

    for_ testCases $ \(skip, compressed, otherCompressions) -> do
      skipCompress skip === compressed
      for_ (compressed : otherCompressions) $ \c ->
        skipDecompress c === Just skip

tests :: IO Bool
tests =
  checkParallel $$(discover)
