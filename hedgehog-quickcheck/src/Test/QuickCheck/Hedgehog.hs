-- | Use Hedgehog generators with QuickCheck.
--
module Test.QuickCheck.Hedgehog (
    hedgehog
  ) where

import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Functor.Identity (runIdentity)

import           Hedgehog
import           Hedgehog.Internal.Gen (runGenT)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree(..), Node(..))

import qualified Test.QuickCheck as QuickCheck


genSeed :: QuickCheck.Gen Seed
genSeed =
  Seed.from <$> QuickCheck.arbitraryBoundedIntegral

-- | Create a QuickCheck 'QuickCheck.Gen' from a Hedgehog 'Gen'.
--
--   /Note that this conversion does not preserve shrinking. There is currently/
--   /no way to use Hedgehog's shrinking capability inside QuickCheck./
--
hedgehog :: Gen a -> QuickCheck.Gen a
hedgehog gen =
  let
    loop n =
      if n <= 0 then
        QuickCheck.discard
      else do
        seed <- genSeed
        size <- QuickCheck.sized (pure . fromIntegral)
        case runIdentity . runMaybeT . runTree $ runGenT size seed gen of
          Nothing ->
            loop (n - 1)
          Just x ->
            pure $ nodeValue x
  in
    loop (100 :: Int)
