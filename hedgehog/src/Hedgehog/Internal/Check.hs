{-# LANGUAGE NoImplicitPrelude #-}

module Hedgehog.Internal.Check
  ( check
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Prelude
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Region
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Runner (checkReport)
import           Hedgehog.Range (Size)

checkRegion ::
     MonadIO m
  => Region
  -> UseColor
  -> Maybe PropertyName
  -> Size
  -> Seed
  -> Property
  -> m (Report Result)
checkRegion region color name size seed prop =
  liftIO $ do
    result <-
      checkReport (propertyConfig prop) size seed (propertyTest prop) $ \progress -> do
        ppprogress <- renderProgress color name progress
        case reportStatus progress of
          Running ->
            setRegion region ppprogress
          Shrinking _ ->
            openRegion region ppprogress

    ppresult <- renderResult color name result
    case reportStatus result of
      Failed _ ->
        openRegion region ppresult
      GaveUp ->
        openRegion region ppresult
      OK ->
        setRegion region ppresult

    pure result

checkNamed ::
     MonadIO m
  => Region
  -> UseColor
  -> Maybe PropertyName
  -> Maybe Seed
  -> Property
  -> m (Report Result)
checkNamed region color name mseed prop = do
  seed <- resolveSeed mseed
  checkRegion region color name 0 seed prop

-- | Check a property.
--
check :: MonadIO m => Property -> m Bool
check prop = do
  color <- detectColor
  liftIO . displayRegion $ \region ->
    (== OK) . reportStatus <$> checkNamed region color Nothing Nothing prop
