{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative (liftA2)

import           Data.Functor.Classes (Eq1(..))

import           Hedgehog.Internal.Tree (TreeT(..), NodeT(..))

import           Test.QuickCheck (Arbitrary(..), Arbitrary1(..))
import           Test.QuickCheck (choose, vector)

import           Test.QuickCheck (arbitrary1)
import           Test.QuickCheck.Checkers (EqProp(..), eq)
import           Test.QuickCheck.Classes (applicative, monad, monadApplicative)
import           Test.Tasty (TestTree, defaultMain, testGroup)
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
