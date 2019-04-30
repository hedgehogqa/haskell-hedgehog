{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Test.Hedgehog.Applicative where

import           Control.Monad.Morph (hoist)
import           Control.Monad.State.Class (MonadState(..), modify)
import qualified Control.Monad.Trans.State.Lazy as Lazy

import           Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Map as Map

import           Hedgehog hiding (Command, Var)
import qualified Hedgehog.Range as Range

import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Tree as Tree


newtype Var =
  Var Int
  deriving (Eq, Ord, Show)

data Command =
    Add
  | Remove
    deriving (Eq, Ord, Show)

data a :<- b =
  a :<- b
  deriving (Eq, Ord, Show)

takeVar :: a :<- b -> a
takeVar (var :<- _) =
  var

genVar :: (MonadState Int m, MonadGen m) => m Var
genVar = do
  modify (+1)
  Var <$> get

genCommand :: MonadGen m => m Command
genCommand =
  Gen.element [Add, Remove]

genCommands :: (MonadState Int m, MonadGen m) => m [Var :<- Command]
genCommands =
  Gen.list (Range.constant 0 3) $ do
    var <- genVar
    cmd <- genCommand
    pure $
      var :<- cmd

-- | Uncomment to observe invalid Applicative behaviour
--
--   /This actually also works, if you comment out the ApplicativeDo above./
--
xprop_StateT_inside :: Property
xprop_StateT_inside =
  propVars $ hoist (`Lazy.evalStateT` 0) genCommands

prop_StateT_outside :: Property
prop_StateT_outside =
  propVars . (`Lazy.evalStateT` 0) $ distributeT genCommands

propVars :: Gen [Var :<- Command] -> Property
propVars gen =
  property $ do
    let

    tree <-
      forAllWith (Tree.render . fmap show . Tree.prune 3) $
        Gen.toTree gen

    let
      noDuplicates xs =
        let
          sorted =
            List.sort xs

          unique =
            Map.elems (Map.fromList (fmap (\x -> (takeVar x, x)) xs))

          varsEq ys zs =
            fmap takeVar ys ==
            fmap takeVar zs
        in
          diff sorted varsEq unique

    traverse_ noDuplicates tree

tests :: IO Bool
tests =
  checkParallel $$(discover)
