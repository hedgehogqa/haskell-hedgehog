{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Test.Example.Exception where

import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Prelude hiding (maximum, filter)

filter :: Ord a => (a -> Bool) -> [a] -> [a]
filter _ (_ : _ : _ : _ : _) = error "too many!"
filter _ [] = []
filter p (x : xs) =
  if p x then
    x : filter p xs
  else
    filter p xs

maximum :: Ord a => [a] -> a
maximum (x : xs) = x `max` maximum xs

prop_equals :: Property
prop_equals =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
    maximum xs === List.maximum xs

prop_assert :: Property
prop_assert =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
    assert $
      filter (== 0) xs == []

prop_property_exception :: Property
prop_property_exception =
  property $ do
    _xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
    _x <- error "got an error"
    assert True

return []
tests :: IO Bool
tests =
  checkSequential $$(discover)
