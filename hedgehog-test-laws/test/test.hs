{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.Maybe (MaybeT(..))

import           Data.Functor.Classes (Eq1(..))

import           Hedgehog.Internal.Gen (GenT(..))
import           Hedgehog.Internal.Range (Size(..))
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.Tree (TreeT(..), NodeT(..))
import           Hedgehog.Internal.TestCount (TestCount(..))

import           Test.QuickCheck (Arbitrary(..), Arbitrary1(..), CoArbitrary(..))
import           Test.QuickCheck (choose, vector, coarbitraryIntegral, property)

import           Test.QuickCheck (arbitrary1)
import           Test.QuickCheck.Checkers (EqProp(..), eq)
import           Test.QuickCheck.Classes (applicative, monad, monadApplicative)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.ExpectedFailure (ignoreTest)
import           Test.Tasty.QuickCheck (testProperties)

main :: IO ()
main =
  defaultMain instances

instances ::  TestTree
instances =
  let
    testBatch =
      uncurry testProperties
  in
    testGroup "Instances" [
      testGroup "TreeT" $
        testBatch <$> [
            applicative (undefined :: TreeT Maybe (Bool, Char, Int))
          , monad (undefined :: TreeT Maybe (Bool, Char, Int))
          , monadApplicative (undefined :: TreeT (Either Bool) (Char, Int))
          ]
    , testGroup "NodeT" $
        testBatch <$> [
            applicative (undefined :: NodeT Maybe (Bool, Char, Int))
          , monad (undefined :: NodeT Maybe (Bool, Char, Int))
          , monadApplicative (undefined :: NodeT (Either Bool) (Char, Int))
          ]
    , testGroup "GenT" $
        ignoreTest . testBatch <$> [
            applicative (undefined :: GenT Maybe (Bool, Char, Int))
          , monad (undefined :: GenT Maybe (Bool, Char, Int))
          , monadApplicative (undefined :: GenT (Either Bool) (Char, Int))
          ]
    ]

------------------------------------------------------------------------
-- Orphan instances

-- Tree

instance (Eq1 m, Eq a) => EqProp (TreeT m a) where
  (=-=) =
    eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (TreeT m a) where
  arbitrary =
    TreeT <$> arbitrary1

-- Node

instance (Eq1 m, Eq a) => EqProp (NodeT m a) where
  (=-=) = eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (NodeT m a) where
  arbitrary = do
    n <- choose (0, 2)
    liftA2 NodeT arbitrary (vector n)

-- GenT

instance Arbitrary TestCount where
  arbitrary = TestCount <$> arbitrary

instance CoArbitrary TestCount where
  coarbitrary = coarbitraryIntegral

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
  show _ = "GenT { unGenT = <function> }"

instance (Eq1 m, Eq a) => EqProp (GenT m a) where
  GenT f0 =-= GenT f1 = property $ liftA2 (=-=) f0 f1

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (GenT m a) where
  arbitrary = GenT <$> arbitrary
