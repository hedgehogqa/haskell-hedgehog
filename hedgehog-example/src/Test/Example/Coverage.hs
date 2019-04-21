{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.Coverage (
    tests
  ) where

import           Control.Concurrent (threadDelay)

import           Data.Foldable (for_)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_classify :: Property
prop_classify =
  withTests 1 . property $ do
    for_ [1 :: Int ..100] $ \a -> do
      classify "small number" $ a < 50
      classify "big number" $ a >= 50

prop_cover_number :: Property
prop_cover_number =
  property $ do
    number <- forAll (Gen.int $ Range.linear 1 100)
    evalIO $ threadDelay 20000
    cover 50 "small number" $ number < 50
    cover 50 "medium number" $ number >= 20
    cover 50 "big number" $ number >= 50

prop_cover_bool :: Property
prop_cover_bool =
  property $ do
    match <- forAll Gen.bool
    cover 30 "True" match
    cover 30 "False" $ not match

tests :: IO Bool
tests =
  checkParallel $$(discover)
