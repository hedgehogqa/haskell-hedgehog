import           Hedgehog.Main (defaultMain)

import qualified Test.Hedgehog.Seed
import qualified Test.Hedgehog.Text
import qualified Test.Hedgehog.Zip


main :: IO ()
main =
  defaultMain [
      Test.Hedgehog.Text.tests
    , Test.Hedgehog.Seed.tests
    , Test.Hedgehog.Zip.tests
    ]
