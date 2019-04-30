{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Hedgehog.Filter where

import           Data.Foldable (toList)
import qualified Data.Set as Set

import           Hedgehog
import qualified Hedgehog.Range as Range

import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.Tree (NodeT(..))
import qualified Hedgehog.Internal.Tree as Tree

-- | Prevent this bug from returning:
--
--   https://stackoverflow.com/questions/54412108/why-the-does-this-shrink-tree-looks-the-way-it-does-when-using-filter
--
--   I'm trying to understand what is the effect that filter has in the shrink
--   tree of a generator when using _integrated shrinking_.
--
--   Consider the following function:
--
-- @
--   {-# LANGUAGE OverloadedStrings #-}
--
--   import Hedgehog
--   import qualified Hedgehog.Gen as Gen
--
--   genChar:: Gen Char
--   genChar =
--     Gen.filter (`elem` ("x" :: String)) (Gen.element "yx")
--
-- @
--
--   When a print the shrink tree:
--
-- @
--   >>>  Gen.printTree genChar
-- @
--
--   I'd get shrink trees that look as follow:
--
-- @
--   'x'
--    └╼'x'
--       └╼'x'
--          └╼'x'
--                  ...
--
--                      └╼<discard>
-- @
--
--   This is, a very deep tree containing only @x@'s, and a @discard@ at the
--   end.
--
prop_filter_repetition :: Property
prop_filter_repetition =
  property $ do
    let
      genChar:: Gen Char
      genChar =
        Gen.filter (`elem` ("x" :: String)) (Gen.element "yx")

    tree <- forAllWith (Tree.render . fmap show . Tree.prune 10) (Gen.toTree genChar)
    Tree.depth tree === 1

prop_filter_even :: Property
prop_filter_even =
  property $ do
    let
      genEven :: Gen Int
      genEven =
        Gen.filter even (Gen.int (Range.constant 0 8))

    tree <- forAllWith (Tree.render . fmap show . Tree.prune 5) (Gen.toTree genEven)

    let
      NodeT x _ =
        Tree.runTree tree

      required =
        Set.fromList (filter even [0..x])

      actual =
        Set.fromList (toList tree)

      missing =
        required `Set.difference` actual

    annotateShow missing
    required === actual

tests :: IO Bool
tests =
  checkParallel $$(discover)
