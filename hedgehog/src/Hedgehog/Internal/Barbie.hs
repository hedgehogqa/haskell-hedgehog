{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}

-- | For compatibility across different barbie versions.
--
module Hedgehog.Internal.Barbie (
    FunctorB(..)
  , TraversableB(..)
  ) where

-- Hide CPP in here instead of the State.hs file.

#if MIN_VERSION_barbies(2,0,0)
import           Data.Functor.Barbie (FunctorB(..), TraversableB(..))
#else
import           Data.Barbie (FunctorB(..), TraversableB(..))
#endif
