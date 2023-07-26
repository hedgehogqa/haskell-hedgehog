{-# LANGUAGE TemplateHaskell #-}

module Test.Hedgehog.Size where

import           Control.Monad                  ( void, when )
import           Control.Monad.IO.Class         ( MonadIO(..) )

import           Data.Foldable                  ( for_ )
import qualified Data.IORef                    as IORef

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Internal.Config      as Config
import qualified Hedgehog.Internal.Property    as Property
import           Hedgehog.Internal.Report       ( Report(..)
                                                , Result(..)
                                                )
import qualified Hedgehog.Internal.Runner      as Runner

checkProp :: MonadIO m => Property -> m (Report Result)
checkProp prop = do
  seed <- Config.resolveSeed Nothing
  liftIO $ Runner.checkReport (Property.propertyConfig prop)
                              seed
                              (Property.propertyTest prop)
                              (const $ pure ())

checkGrowth ::
  MonadIO m => (Property -> Property) -> [Size] -> m [Size]
checkGrowth applyTerminationCriteria discardOn = do
  logRef <- liftIO $ IORef.newIORef []

  void $ checkProp $ applyTerminationCriteria $ property $ do
    curSize <- forAll $ Gen.sized pure
    liftIO $ IORef.modifyIORef' logRef (curSize :)
    when (curSize `elem` discardOn) discard

  liftIO $ reverse <$> IORef.readIORef logRef

data GrowthTest =
  GrowthTest
    TestLimit -- ^ number of tests to run
    [Size]    -- ^ which sizes should be discarded
    [Size]    -- ^ the expected sizes run at (including ones discarded) for
              --   NoConfidenceTermination and NoEarlyTermination
    [Size]    -- ^ the expected sizes run at (including ones discarded) for
              --   EarlyTermination

growthTests :: [GrowthTest]
growthTests =
  [ GrowthTest 1   [] [0]                          [0 .. 99]
  , GrowthTest 5   [] [0, 24 .. 96]                [0 .. 99]
  , GrowthTest 10  [] [0, 11 .. 99]                [0 .. 99]
  , GrowthTest 101 [] ([0 .. 99] ++ [0])           [0 .. 99]
  , GrowthTest 105 [] ([0 .. 99] ++ [0, 24 .. 96]) [0 .. 99]
  , GrowthTest 5   [24] (concat [[0], replicate 10 24, [25, 48, 72, 96]])
                        (concat [[0 .. 23], replicate 10 24, [25], [25 .. 99]])
  , let discards = concat [ replicate 10 96
                          , replicate 10 97
                          , replicate 10 98
                          , replicate 70 99 -- discard limit is 100
                          ]
    in GrowthTest 5 [96 .. 99] ([0, 24 .. 72] ++ discards)
                               ([0 .. 95] ++ discards)
  ]

prop_GrowthTest :: Property
prop_GrowthTest =
  withTests 1 . property $ do
    for_ growthTests $
      \(GrowthTest testLimit discardOn expected1 expected2) -> do
        let noConfidenceTerm = withTests testLimit
        sizes1 <- checkGrowth noConfidenceTerm discardOn
        sizes1 === expected1

        let noEarlyTerm = withConfidence 1000 . noConfidenceTerm
        sizes2 <- checkGrowth noEarlyTerm discardOn
        sizes2 === expected1

        let earlyTerm = verifiedTermination . noEarlyTerm
        sizes3 <- checkGrowth earlyTerm discardOn
        sizes3 === expected2

tests :: IO Bool
tests =
  checkParallel $$(discover)
