module Hedgehog.Main (
  -- * Running tests
    defaultMain
  ) where

import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

-- | An entry point that can be used as a main function.
--
defaultMain :: [IO Bool] -> IO ()
defaultMain tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  result <- and <$> sequence tests
  unless result
    exitFailure
