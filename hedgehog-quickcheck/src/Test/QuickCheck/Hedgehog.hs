-- | Use Hedgehog generators with QuickCheck.
--
module Test.QuickCheck.Hedgehog (
    hedgehog

  , forAll
  , forAllShow
  ) where

import           Hedgehog (Gen, Seed)
import           Hedgehog.Internal.Show (showPretty)
import           Hedgehog.Internal.Gen (evalGen)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree, treeValue, treeChildren)

import qualified Test.QuickCheck as QuickCheck

import           Test.QuickCheck.Gen.Unsafe (promote)
import           Test.QuickCheck.Property (Testable(..), Property(..), Prop(..), Rose(..))
import           Test.QuickCheck.Property (joinRose, counterexample)

genSeed :: QuickCheck.Gen Seed
genSeed =
  Seed.from <$> QuickCheck.arbitraryBoundedIntegral

-- | Create a QuickCheck 'QuickCheck.Gen' from a Hedgehog 'Gen'.
--
--   /Note that this conversion does not preserve shrinking. To preserve/
--   /shrinking use the exported 'forAll' and 'forAllShow' functions rather/
--   /than going via a 'QuickCheck.Gen'./
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
        case evalGen size seed gen of
          Nothing ->
            loop (n - 1)
          Just x ->
            pure $ treeValue x
  in
    loop (100 :: Int)


-- | Use a Hedgehog 'Gen' in a QuickCheck 'QuickCheck.Property', using the
--   show typeclass for displaying errors.
--
--   This operation preserves Hedgehog's shrinking capabilities.
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> QuickCheck.Property
forAll =
  forAllShow showPretty


-- | Use a Hedgehog 'Gen' in a QuickCheck 'QuickCheck.Property', but provide a custom render function
--   for displaying counterexamples.
--
--   This operation preserves Hedgehog's shrinking capabilities.
forAllShow :: Testable prop => (a -> String) -> Gen a -> (a -> prop) -> QuickCheck.Property
forAllShow render gen pf =
  QuickCheck.again $
    MkProperty $
      let
        loop n =
          if n <= 0 then
            QuickCheck.discard
          else do
            seed <- genSeed
            size <- QuickCheck.sized (pure . fromIntegral)
            case evalGen size seed gen of
              Nothing ->
                loop (n - 1)
              Just tree ->
                shrinking tree $ \x ->
                  counterexample (render x) $
                  pf x

      in
        loop (100 :: Int)

-- | Use an existing 'Tree' to exercise a given property.
shrinking :: Testable prop => Tree a -> (a -> prop) -> QuickCheck.Gen Prop
shrinking tree pf =
  let
    props x =
      MkRose
        (unProperty . QuickCheck.property . pf $ treeValue x)
        (fmap props $ treeChildren x)
  in
    fmap (MkProp . joinRose . fmap unProp) $
      promote (props tree)
