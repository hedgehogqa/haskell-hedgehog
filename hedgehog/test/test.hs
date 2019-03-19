import           Hedgehog (runTests)

import qualified Test.Hedgehog.Seed
import qualified Test.Hedgehog.Text


main :: IO ()
main =
  runTests [
      Test.Hedgehog.Text.tests
    , Test.Hedgehog.Seed.tests
    ]
