{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE RankNTypes #-}
module Hedgehog.Internal.HTraversable (
    HTraversable(..)
  ) where


-- | Higher-order traversable functors.
--
-- This is used internally to make symbolic variables concrete given an 'Environment'.
--
class HTraversable t where
  htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
