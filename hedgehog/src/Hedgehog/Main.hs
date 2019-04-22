{-# LANGUAGE CPP #-}
module Hedgehog.Main (
  -- * Running tests
    defaultMain
  ) where

import           Control.Monad (unless)

import           System.Exit (exitFailure)

#if mingw32_HOST_OS
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, stderr, utf8)
#else
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
#endif

-- | An entry point that can be used as a main function.
--
defaultMain :: [IO Bool] -> IO ()
defaultMain tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  result <- and <$> sequence tests
  unless result
    exitFailure
