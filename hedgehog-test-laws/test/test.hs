{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.Maybe (MaybeT(..))

import           Data.Functor.Classes (Eq1(..))

import           Hedgehog.Internal.Gen (GenT(..))
import           Hedgehog.Internal.Range (Size(..))
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.Tree (Tree(..), Node(..))

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
      testGroup "Tree" $
        testBatch <$> [
            applicative (undefined :: Tree Maybe (Bool, Char, Int))
          , monad (undefined :: Tree Maybe (Bool, Char, Int))
          , monadApplicative (undefined :: Tree (Either Bool) (Char, Int))
          ]
    , testGroup "Node" $
        testBatch <$> [
            applicative (undefined :: Node Maybe (Bool, Char, Int))
          , monad (undefined :: Node Maybe (Bool, Char, Int))
          , monadApplicative (undefined :: Node (Either Bool) (Char, Int))
          ]
    , ignoreTest . testGroup "GenT" $
        testBatch <$> [
            applicative (undefined :: GenT Maybe (Bool, Char, Int))
          , monad (undefined :: GenT Maybe (Bool, Char, Int))
          , monadApplicative (undefined :: GenT (Either Bool) (Char, Int))
          ]
    ]

------------------------------------------------------------------------
-- Orphan instances

-- Tree

instance (Eq1 m, Eq a) => EqProp (Tree m a) where
  (=-=) =
    eq

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (Tree m a) where
  arbitrary =
    Tree <$> arbitrary1

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
