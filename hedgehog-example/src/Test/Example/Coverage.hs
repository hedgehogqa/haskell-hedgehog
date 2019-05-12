{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.Coverage (
    tests
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


prop_label :: Property
prop_label =
  withTests 101 . property $ do
    match <- forAll Gen.bool
    evalIO $ threadDelay 10000
    if match then
      label "True"
    else
      label "False"

data Bucket =
  Bucket !Int
  deriving (Eq, Show)

-- Good for testing progress bar boundary conditions.
prop_occupy :: Property
prop_occupy =
  property $ do
    size <- forAll $ Gen.sized pure
    if size == 1 then
      label "wall st"
    else
      label "occupy"

-- Good for testing progress bar boundary conditions.
prop_world_cup_titles_2018 :: Property
prop_world_cup_titles_2018 =
  withTests 5 . property $ do
    size <- forAll $ Gen.sized pure
    flip classify (size < 5) "brazil"
    flip classify (size < 4) "italy"
    flip classify (size < 4) "germany"
    flip classify (size < 2) "uruguay"
    flip classify (size < 2) "france"
    flip classify (size < 2) "argentina"
    flip classify (size < 1) "spain"
    flip classify (size < 1) "england"
    flip classify (size < 0) "australia"
    flip classify (size < 0) "greece"

prop_collect :: Property
prop_collect =
  withTests 101 . property $ do
    x <- forAll .
      fmap Bucket . Gen.int $ Range.linear 1 9
    evalIO $ threadDelay 10000
    collect x

prop_classify :: Property
prop_classify =
  withTests 100 . property $ do
    a <- forAll $ Gen.int (Range.constant 0 100)
    classify "small number" $ a < 50
    classify "big number" $ a >= 50

prop_cover_number :: Property
prop_cover_number =
  withTests 101 . property $ do
    number <- forAll (Gen.int $ Range.linear 1 100)
    evalIO $ threadDelay 20000
    cover 50 "small number" $ number < 10
    cover 15 "medium number" $ number >= 20
    cover 5 "big number" $ number >= 70
    when (number > 10) $ label ">10 number"

prop_cover_bool :: Property
prop_cover_bool =
  withTests 101 . property $ do
    match <- forAll Gen.bool
    cover 30 "True" match
    cover 30 "False" $ not match

tests :: IO Bool
tests =
  checkParallel $$(discover)
