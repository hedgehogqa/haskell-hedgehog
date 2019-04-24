{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.List where

import           Control.Monad.Zip (mzip)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Class as Writer
import           Control.Monad.Morph (MFunctor(..))

import           Data.Maybe (fromJust)
import           Data.Foldable (toList)
import           Data.Monoid (Sum(..))
import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Range as Range

import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import           Hedgehog.Internal.Tree (Tree)
import qualified Hedgehog.Internal.Tree as Tree


genInt :: MonadGen m => m Int
genInt =
  Gen.int (Range.constant 0 2)

genList :: MonadGen m => m a -> m [a]
genList =
  Gen.list (Range.constant 0 2)

prop_list :: Property
prop_list =
  property $ do
    let
      cond :: [Int] -> Bool
      cond xs =
        all (>= length xs) xs

      renderTree :: Tree [Int] -> String
      renderTree =
        Tree.render .
        fmap show .
        fmap (\xs -> (cond xs, xs))

    ts <- forAllWith renderTree (Gen.tree $ genList genInt)
    xs0 <- forAll (Gen.liftTree ts)

    assert (cond xs0)

newtype Index =
  Index Int
  deriving (Eq, Num, Show)

genStateInt :: (MonadGen m, State.MonadState Index m) => m (Index, Int)
genStateInt = do
  x <- genInt
  State.modify (+ 1)
  index <- State.get
  pure (index, x)

prop_state_list :: Property
prop_state_list =
  property $ do
    let
      cond :: [(a, Int)] -> Bool
      cond xs =
        all ((>= length xs) . snd) xs

      renderTree :: Tree [(Index, Int)] -> String
      renderTree =
        Tree.render .
        fmap show .
        fmap (\xs -> (cond xs, xs))

    ts <- forAllWith renderTree (Gen.tree $ hoist (`Lazy.evalStateT` 0) $ genList genStateInt)
    xs0 <- forAll (Gen.liftTree ts)

    assert (cond xs0)

data Log =
    List Int
  | Int Int
    deriving (Show)

genWriterList :: (MonadGen m, Writer.MonadWriter [Log] m) => m a -> m [a]
genWriterList gen = do
  xs <- Gen.list (Range.constant 0 2) gen
  Writer.tell [List (length xs)]
  pure xs

genWriterInt :: (MonadGen m, Writer.MonadWriter [Log] m) => m Int
genWriterInt = do
  x <- genInt
  Writer.tell [Int x]
  pure x

renderLog :: [Log] -> String
renderLog xs =
  concat . flip fmap xs $ \case
    List n ->
      " L" ++ show n
    Int n ->
      show n

prop_writer_list :: Property
prop_writer_list =
  property $ do
    let
      cond :: [Int] -> Bool
      cond xs =
        all ((>= length xs)) xs

      renderTree =
        Tree.render .
        fmap (\(ns, ws) -> show (cond ns, ns) ++ " (" ++ renderLog ws ++ ")")

    ts <- forAllWith renderTree (Gen.tree . Lazy.runWriterT $ genWriterList genWriterInt)
    (xs0, _) <- forAll (Gen.liftTree ts)

    assert (cond xs0)


tests :: IO Bool
tests =
  checkParallel $$(discover)
