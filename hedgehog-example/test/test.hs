import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Example.Basic as Test.Example.Basic
import qualified Test.Example.Exception as Test.Example.Exception
import qualified Test.Example.References as Test.Example.References
import qualified Test.Example.Registry as Test.Example.Registry
import qualified Test.Example.Resource as Test.Example.Resource
import qualified Test.Example.Roundtrip as Test.Example.Roundtrip
import qualified Test.Example.STLC as Test.Example.STLC

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
      Test.Example.Basic.tests
    , Test.Example.Exception.tests
    , Test.Example.References.tests
    , Test.Example.Registry.tests
    , Test.Example.Resource.tests
    , Test.Example.Roundtrip.tests
    , Test.Example.STLC.tests
    ]

  --
  -- Normally we would exit with failure when tests fail using something like:
  --
  --   Control.Monad.unless (and results) $
  --     System.Exit.exitFailure
  --
  -- But this project is designed to actually show test errors as an example so
  -- we don't want it to break CI.
  --

  pure ()
