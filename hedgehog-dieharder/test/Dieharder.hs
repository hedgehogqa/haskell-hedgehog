import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import           System.IO (stdout)

import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed

main :: IO ()
main =
  Builder.hPutBuilder stdout . go =<< Seed.random
  where
    go :: Seed -> Builder
    go s0 =
      case Seed.nextWord64 s0 of
        (v0, s1) -> Builder.word64LE v0 `mappend` go s1
