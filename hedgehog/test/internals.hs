module Main where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Functor.Classes
import Hedgehog.Internal.Gen
import Hedgehog.Internal.Range
import Hedgehog.Internal.Seed
import Hedgehog.Internal.Tree
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain instances

instances ::  TestTree
instances = testGroup "Instances"
  [ testGroup "Tree" $ testBatch <$>
      [ applicative      (undefined :: Tree Maybe (Bool, Char, Int))
      , monad            (undefined :: Tree Maybe (Bool, Char, Int))
      , monadApplicative (undefined :: Tree (Either Bool) (Char, Int))
      ]
  , testGroup "Node" $ testBatch <$>
      [ applicative      (undefined :: Node Maybe (Bool, Char, Int))
      , monad            (undefined :: Node Maybe (Bool, Char, Int))
      , monadApplicative (undefined :: Node (Either Bool) (Char, Int))
      ]
  , ignoreTest $ testGroup "GenT" $ testBatch <$>
      [ applicative      (undefined :: GenT Maybe (Bool, Char, Int))
      , monad            (undefined :: GenT Maybe (Bool, Char, Int))
      , monadApplicative (undefined :: GenT (Either Bool) (Char, Int))
      ]
  ]
  where
    testBatch = uncurry testProperties

------------------------------------------------------------------------
-- Orphan instances

-- Tree

instance (Eq1 m, Eq a) => EqProp (Tree m a) where
  (=-=) = eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (Tree m a) where
  arbitrary = Tree <$> arbitrary1

-- Node

instance (Eq1 m, Eq a) => EqProp (Node m a) where
  (=-=) = eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (Node m a) where
  arbitrary = do
    n <- choose (0, 2)
    liftA2 Node arbitrary (vector n)

-- GenT

instance Arbitrary Size where
  arbitrary = Size <$> arbitrary

instance CoArbitrary Size where
  coarbitrary = coarbitraryIntegral

instance Arbitrary Seed where
  arbitrary = liftA2 Seed arbitrary ((\n -> 2 * n + 1) <$> arbitrary)

instance CoArbitrary Seed where
  coarbitrary (Seed v g) = coarbitrary (v, g)

instance (Arbitrary1 m) => Arbitrary1 (MaybeT m) where
  liftArbitrary = fmap MaybeT . liftArbitrary . liftArbitrary

instance Show (GenT m a) where
  show _ = "GenT { unGen = <function> }"

instance (Eq1 m, Eq a) => EqProp (GenT m a) where
  GenT f0 =-= GenT f1 = property $ liftA2 (=-=) f0 f1

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (GenT m a) where
  arbitrary = GenT <$> arbitrary
