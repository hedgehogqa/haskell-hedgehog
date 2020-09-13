{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hedgehog.Internal.State.Name
  ( Name (..)
  ) where

-- | Symbolic variable names.
--
newtype Name =
  Name Int
  deriving (Eq, Ord, Num)

instance Show Name where
  showsPrec p (Name x) =
    showsPrec p x
