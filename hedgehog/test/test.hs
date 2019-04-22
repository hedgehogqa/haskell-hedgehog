import           Hedgehog.Main (defaultMain)

import qualified Test.Hedgehog.Seed
import qualified Test.Hedgehog.Text


main :: IO ()
main =
  defaultMain [
      Test.Hedgehog.Text.tests
    , Test.Hedgehog.Seed.tests
    ]
