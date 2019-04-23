{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hedgehog.Zip where

import           Control.Monad.Zip (mzip)

import           Data.Maybe (fromJust)
import           Data.Foldable (toList)

import           Hedgehog
import qualified Hedgehog.Range as Range

import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.Tree (Tree)
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import qualified Hedgehog.Internal.Tree as Tree
import qualified Hedgehog.Internal.Shrink as Shrink


mkTree :: Int -> Tree Int
mkTree n =
  Tree.expand (Shrink.towards 0) (pure n)

mkGen :: Int -> Gen Int
mkGen =
  Gen.liftTreeT . mkTree

prop_gen_applicative :: Property
prop_gen_applicative =
  property $ do
    let
      treeApplicative n m =
        (,) <$> mkTree n <*> mkTree m

      treeZip n m =
        mzip (mkTree n) (mkTree m)

      genApplicative n m =
        fromJust .
        Gen.runGen 0 (Seed 0 0) $
          (,) <$> mkGen n <*> mkGen m

      count00 =
        length .
        filter (== (0,0)) .
        toList

      render :: HasCallStack => Tree (Int, Int) -> PropertyT IO ()
      render x =
        withFrozenCallStack $ do
          annotate . Tree.render $ fmap show x
          annotate $ "---"
          annotate $ "count (0,0) = " ++ show (count00 x)

    n <- forAll $ Gen.int (Range.constant 1 5)
    m <- forAll $ Gen.int (Range.constant 1 5)

    render $ genApplicative n m
    render $ treeZip n m
    render $ treeApplicative n m

    genApplicative n m === treeZip n m
    genApplicative n m /== treeApplicative n m

    success

tests :: IO Bool
tests =
  checkParallel $$(discover)
