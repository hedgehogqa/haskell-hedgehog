--
-- An example of using QuickCheck generators inside Hedgehog.
--
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.QuickCheck where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range


prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll (Gen.arbitrary :: Gen [Int])
    reverse (reverse xs) === xs

prop_reverse_mixed :: Property
prop_reverse_mixed =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.arbitrary :: Gen Char)
    reverse (reverse xs) === xs

tests :: IO Bool
tests =
  checkParallel $$(discover)
