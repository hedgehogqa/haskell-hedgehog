{-# OPTIONS_HADDOCK not-home #-}
module Hedgehog.Internal.Exception (
    tryAll
  , tryEvaluate
  ) where

import           Control.Exception (Exception(..), AsyncException, SomeException(..), evaluate)
import           Control.Exception.Safe (MonadCatch, catch, throwM)

import           System.IO.Unsafe (unsafePerformIO)


tryAll :: MonadCatch m => m a -> m (Either SomeException a)
tryAll m =
  catch (fmap Right m) $ \exception ->
    case fromException exception :: Maybe AsyncException of
      Nothing ->
        pure $ Left exception
      Just async ->
        throwM async

tryEvaluate :: a -> Either SomeException a
tryEvaluate x =
  unsafePerformIO (tryAll (evaluate x))
