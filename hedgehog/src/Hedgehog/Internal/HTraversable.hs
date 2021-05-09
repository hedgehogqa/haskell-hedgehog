{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE RankNTypes #-}
module Hedgehog.Internal.HTraversable (
    HTraversable(..)
  ) where

import Hedgehog.Internal.Barbie (TraversableB)

-- | Higher-order traversable functors.
--
-- /Deprecated in favor of 'TraversableB' which can be derived using "GHC.Generics"/
--
class HTraversable t where
  htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)

{-#
  DEPRECATED HTraversable
  "Replace with Hedgehog.TraversableB (defined in Data.Functor.Barbie) which can be derived automatically using GHC.Generics"
#-}
