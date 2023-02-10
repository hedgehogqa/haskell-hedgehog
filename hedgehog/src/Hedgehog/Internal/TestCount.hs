{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hedgehog.Internal.TestCount where

import          Prelude
import          Language.Haskell.TH.Syntax (Lift)

-- | The number of tests a property ran successfully.
--
newtype TestCount =
  TestCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)
