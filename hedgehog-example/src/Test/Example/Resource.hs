{-# LANGUAGE OverloadedStrings #-}

module Test.Example.Resource where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Monad.Trans.Resource (runResourceT)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.Exit
import           System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import           System.Process

newtype ProcessFailed =
  ProcessFailed Int
  deriving (Show)

unixSort :: MonadIO m => FilePath -> FilePath -> ExceptT ProcessFailed m ()
unixSort input output = do
  let
    process =
      (proc "sort" ["-o", output, input]) {
          env = Just [("LC_COLLATE", "C")]
        }

  (_, _, _, h) <- liftIO $ createProcess process
  code <- liftIO $ waitForProcess h

  case code of
    ExitSuccess ->
      pure ()
    ExitFailure x ->
      throwE $ ProcessFailed x

prop_unix_sort :: Property
prop_unix_sort =
  property . hoist runResourceT $ do
    values0 <- forAll $
      Gen.list (Range.linear 0 100) $
      Gen.string (Range.constant 1 5) Gen.alpha

    (_, dir) <- Temp.createTempDirectory Nothing "prop_dir"

    let input = dir </> "input"
        output = dir </> "output"

    liftIO $ writeFile input (unlines values0)
    evalExceptT $ unixSort input output
    values <- liftIO . fmap lines $ readFile output

    values0 === values

tests :: IO Bool
tests =
  checkSequential $ Group "Test.Example.Resource" [
      ("prop_unix_sort", prop_unix_sort)
    ]
