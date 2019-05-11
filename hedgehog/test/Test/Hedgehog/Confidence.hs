{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Hedgehog.Confidence where

import           Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as Gen

prop_with_confidence :: Property
prop_with_confidence =
  withConfidence (10^9) . withTests 10000 . property $ do
    number <- forAll (Gen.int $ Range.linear 1 10)
    cover 30 "number == 1" $ number == 1

tests :: IO Bool
tests =
  checkParallel $$(discover)
