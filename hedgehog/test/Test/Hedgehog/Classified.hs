{-# LANGUAGE TemplateHaskell #-}
module Test.Hedgehog.Classified (
    tests
  ) where

import           Data.Foldable (for_)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_check_classifiers :: Property
prop_check_classifiers =
  withTests 1 . property $ do
    for_ [1 :: Int ..100] $ \a -> do
      classify (a < 50) "small number"
      classify (a >= 50) "big number"
      success

prop_check_coverage :: Property
prop_check_coverage =
  withTests 1 . property $ do
    forAll (Gen.int $ Range.linear 1 100) >>= \number -> do
      cover 50 (number < 50) "small number"
      cover 50 (number >= 50) "big number"
      success

prop_check_coverage_many_tests :: Property
prop_check_coverage_many_tests =
  withTests 100 . property $
    forAll Gen.bool >>= \match -> do
      cover 30 match "True"
      cover 30 (not match) "False"
      success

tests :: IO Bool
tests =
  checkParallel $$(discover)
