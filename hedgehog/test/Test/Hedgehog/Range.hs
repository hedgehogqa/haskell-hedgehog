{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Hedgehog.Range where

import           Data.Word (Word64)

import           Hedgehog
import qualified Hedgehog.Range as Range

-- | Ensure exponential range doesn't overflow at the upper bound.
--
prop_exponential_overflow :: Property
prop_exponential_overflow = withTests 1 . property $ do
  maxBound === Range.upperBound 99 (Range.exponentialBounded :: Range Word64)

tests :: IO Bool
tests =
  checkParallel $$(discover)
