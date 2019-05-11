{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.Confidence where

import           Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as Gen

------------------------------------------------------------------------
-- Example 0: This test will certify that it is impossible to get 100%
--            coverage for the property label "number == 1"
--
prop_without_confidence :: Property
prop_without_confidence =
  withConfidence (10^9) . withTests 100 . property $ do
    number <- forAll (Gen.int $ Range.linear 1 10)
    cover 100 "number == 1" $ number == 1

------------------------------------------------------------------------
tests :: IO Bool
tests =
  checkSequential $$(discover)
