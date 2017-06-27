{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hedgehog.Range (
  -- * Size
    Size(..)

  -- * Range
  , Range
  , origin
  , bounds
  , lowerBound
  , upperBound

  -- * Constant
  , singleton
  , constant
  , constantFrom
  , constantBounded

  -- * Linear
  , linear
  , linearFrom
  , linearFrac
  , linearFracFrom
  , linearBounded

  -- * Exponential
  , exponential
  , exponentialFrom
  , exponentialBounded
  , exponentialFloat
  , exponentialFloatFrom
  ) where

import           Hedgehog.Internal.Range
