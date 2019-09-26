{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.Confidence where

import           Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as Gen

------------------------------------------------------------------------
-- Example 0: This test will certify that it is impossible to get 60%
--            coverage for the property label "number == 1"
--
--            Note that it will abort running once it knows its task is
--            impossible - it will not run 1000000 tests
--
prop_without_confidence :: Property
prop_without_confidence =
  terminateEarly . withConfidence (10^9) . withTests 1000000 . property $ do
    number <- forAll (Gen.int $ Range.constant 1 2)
    cover 60 "number == 1" $ number == 1

------------------------------------------------------------------------
tests :: IO Bool
tests =
  checkSequential $$(discover)
