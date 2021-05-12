{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}

-- | For compatibility across different versions of the @barbie@ package.
--
module Hedgehog.Internal.Barbie (
    FunctorB(..)
  , TraversableB(..)
  , Rec(..)
  ) where

-- Hide CPP in here instead of the State.hs file.

#if MIN_VERSION_barbies(2,0,0)
import           Data.Functor.Barbie (FunctorB(..), TraversableB(..), Rec(..))
#else
import           Data.Barbie (FunctorB(..), TraversableB(..), Rec(..))
#endif
