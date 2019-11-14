{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hedgehog.Internal.TestCount where

import          Prelude

-- | The number of tests a property ran successfully.
--
newtype TestCount =
  TestCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)
