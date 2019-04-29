import           Hedgehog.Main (defaultMain)

import qualified Test.Hedgehog.Applicative
import qualified Test.Hedgehog.Filter
import qualified Test.Hedgehog.Seed
import qualified Test.Hedgehog.Text
import qualified Test.Hedgehog.Zip


main :: IO ()
main =
  defaultMain [
      Test.Hedgehog.Applicative.tests
    , Test.Hedgehog.Filter.tests
    , Test.Hedgehog.Seed.tests
    , Test.Hedgehog.Text.tests
    , Test.Hedgehog.Zip.tests
    ]
