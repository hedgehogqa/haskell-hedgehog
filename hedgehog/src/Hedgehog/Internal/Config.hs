{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Hedgehog.Internal.Config (
    UseColor(..)
  , resolveColor

  , Seed(..)
  , resolveSeed

  , Verbosity(..)
  , resolveVerbosity

  , WorkerCount(..)
  , resolveWorkers

  , Skip(..)
  , resolveSkip

  , detectMark
  , detectColor
  , detectSeed
  , detectVerbosity
  , detectWorkers
  , detectSkip
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as Text

import qualified GHC.Conc as Conc

import           Hedgehog.Internal.Seed (Seed(..))
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Property (Skip(..), TestCount(..), shrinkPathDecompress)

import           Language.Haskell.TH.Syntax (Lift)

import           System.Console.ANSI (hSupportsANSI)
import           System.Environment (lookupEnv)
import           System.IO (stdout)

import           Text.Read (readMaybe)


-- | Whether to render output using ANSI colors or not.
--
data UseColor =
    DisableColor
    -- ^ Disable ANSI colors in report output.
  | EnableColor
    -- ^ Enable ANSI colors in report output.
    deriving (Eq, Ord, Show, Lift)

-- | How verbose should the report output be.
--
data Verbosity =
    Quiet
    -- ^ Only display the summary of the test run.
  | Normal
    -- ^ Display each property as it is running, as well as the summary.
    deriving (Eq, Ord, Show, Lift)

-- | The number of workers to use when running properties in parallel.
--
newtype WorkerCount =
  WorkerCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

detectMark :: MonadIO m => m Bool
detectMark = do
  user <- liftIO $ lookupEnv "USER"
  pure $ user == Just "mth"

lookupBool :: MonadIO m => String -> m (Maybe Bool)
lookupBool key =
  liftIO $ do
    menv <- lookupEnv key
    case menv of
      Just "0" ->
        pure $ Just False
      Just "no" ->
        pure $ Just False
      Just "false" ->
        pure $ Just False

      Just "1" ->
        pure $ Just True
      Just "yes" ->
        pure $ Just True
      Just "true" ->
        pure $ Just True

      _ ->
        pure Nothing

detectColor :: MonadIO m => m UseColor
detectColor =
  liftIO $ do
    ok <- lookupBool "HEDGEHOG_COLOR"
    case ok of
      Just False ->
        pure DisableColor

      Just True ->
        pure EnableColor

      Nothing -> do
        mth <- detectMark
        if mth then
          pure DisableColor -- avoid getting fired :)
        else do
          enable <- hSupportsANSI stdout
          if enable then
            pure EnableColor
          else
            pure DisableColor

splitOn :: String -> String -> [String]
splitOn needle haystack =
  fmap Text.unpack $ Text.splitOn (Text.pack needle) (Text.pack haystack)

parseSeed :: String -> Maybe Seed
parseSeed env =
  case splitOn " " env of
    [value, gamma] ->
      Seed <$> readMaybe value <*> readMaybe gamma
    _ ->
      Nothing

detectSeed :: MonadIO m => m Seed
detectSeed =
  liftIO $ do
    menv <- lookupEnv "HEDGEHOG_SEED"
    case parseSeed =<< menv of
      Nothing ->
        Seed.random
      Just seed ->
        pure seed

detectVerbosity :: MonadIO m => m Verbosity
detectVerbosity =
  liftIO $ do
    menv <- (readMaybe =<<) <$> lookupEnv "HEDGEHOG_VERBOSITY"
    case menv of
      Just (0 :: Int) ->
        pure Quiet

      Just (1 :: Int) ->
        pure Normal

      _ -> do
        mth <- detectMark
        if mth then
          pure Quiet
        else
          pure Normal

detectWorkers :: MonadIO m => m WorkerCount
detectWorkers = do
  liftIO $ do
    menv <- (readMaybe =<<) <$> lookupEnv "HEDGEHOG_WORKERS"
    case menv of
      Nothing ->
        WorkerCount <$> Conc.getNumProcessors
      Just env ->
        pure $ WorkerCount env

detectSkip :: MonadIO m => m Skip
detectSkip =
  liftIO $ do
    mSkipToTest1 <-
      liftIO $ fmap (TestCount . read) <$> lookupEnv "HEDGEHOG_SKIP_TO_TEST"

    mSkipToShrink1 <- liftIO $
      fmap shrinkPathDecompress <$> lookupEnv "HEDGEHOG_SKIP_TO_SHRINK"

    case (mSkipToTest1, mSkipToShrink1) of
      (Nothing, Nothing) ->
        pure SkipNothing
      (Just t, Nothing) ->
        pure $ SkipToTest t
      (Nothing, Just (Just (t, s))) ->
        pure $ SkipToShrink t s
      (Nothing, Just Nothing) ->
        error "could not read shrink path"
      (Just _, Just _) ->
        error "Cannot skip to both test and shrink"

resolveColor :: MonadIO m => Maybe UseColor -> m UseColor
resolveColor = \case
  Nothing ->
    detectColor
  Just x ->
    pure x

resolveSeed :: MonadIO m => Maybe Seed -> m Seed
resolveSeed = \case
  Nothing ->
    detectSeed
  Just x ->
    pure x

resolveVerbosity :: MonadIO m => Maybe Verbosity -> m Verbosity
resolveVerbosity = \case
  Nothing ->
    detectVerbosity
  Just x ->
    pure x

resolveWorkers :: MonadIO m => Maybe WorkerCount -> m WorkerCount
resolveWorkers = \case
  Nothing ->
    detectWorkers
  Just x ->
    pure x

resolveSkip :: MonadIO m => Maybe Skip -> m Skip
resolveSkip = \case
  Nothing ->
    detectSkip
  Just x ->
    pure x
