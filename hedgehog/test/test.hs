import           Hedgehog (runTests)

import qualified Test.Hedgehog.Seed
import qualified Test.Hedgehog.Text
import qualified Test.Hedgehog.Classified


main :: IO ()
main =
  runTests [
      Test.Hedgehog.Text.tests
    , Test.Hedgehog.Seed.tests
    , Test.Hedgehog.Classified.tests
    ]
