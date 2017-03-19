import           Control.Monad (unless)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)
import           System.Exit (exitFailure)

import qualified Hedgehog.Example as Hedgehog.Example

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _expect_fail <- Hedgehog.Example.tests

  results <- sequence [
    ]

  unless (and results) $
    exitFailure
