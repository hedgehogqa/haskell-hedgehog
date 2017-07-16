-- | Use QuickCheck generators and 'QuickCheck.Arbitrary' instances with Hedgehog.
--
module Hedgehog.Gen.QuickCheck (
    arbitrary
  , quickcheck
  ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as QuickCheck
import qualified Test.QuickCheck.Random as QuickCheck


seedQCGen :: MonadGen m => m QuickCheck.QCGen
seedQCGen =
  QuickCheck.mkQCGen <$> Gen.lift (Gen.integral_ Range.constantBounded)

-- | Create a Hedgehog 'Gen' from a QuickCheck 'QuickCheck.Gen'.
--
--   By default the 'Gen' created will not have any shrinking, you can use
--   @Gen.@'Gen.shrink' if you have a shrink function which you would like to
--   apply.
--
quickcheck :: MonadGen m => QuickCheck.Gen a -> m a
quickcheck (QuickCheck.MkGen gen) =
  Gen.sized $ \size -> do
    qcg <- seedQCGen
    pure $
      gen qcg (fromIntegral size)

-- | Create a Hedgehog 'Gen' from a QuickCheck 'QuickCheck.Arbitrary' instance.
--
--   The Arbitrary's 'QuickCheck.shrink' function is used to provide shrinking
--   for the Hedgehog 'Gen'.
--
arbitrary :: (QuickCheck.Arbitrary a, MonadGen m) => m a
arbitrary =
  Gen.shrink QuickCheck.shrink $
    quickcheck QuickCheck.arbitrary
