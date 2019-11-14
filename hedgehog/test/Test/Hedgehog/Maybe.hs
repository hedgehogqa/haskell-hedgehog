{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hedgehog.Maybe where

import           Data.Foldable (toList)

import           Hedgehog

import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import           Hedgehog.Internal.Tree (Tree)
import qualified Hedgehog.Internal.Tree as Tree


mkTree :: Int -> Tree Int
mkTree n =
  Tree.expand (Shrink.towards 0) (pure n)

showOdd :: Int -> Maybe String
showOdd n =
  if n `mod` 2 == 0 then
    Nothing
  else
    Just (show n)

render :: (HasCallStack, Show a) => Tree a -> PropertyT IO ()
render x =
  withFrozenCallStack $ do
    annotate . Tree.render $ fmap show x

prop_mapMaybe :: Property
prop_mapMaybe =
  withTests 1 . property $ do
    let original = mkTree 5
    case Tree.mapMaybe showOdd original of
      Nothing ->
        failure
      Just mapped -> do
        render original
        render mapped
        ["5" , "3" , "1" , "1" , "3" , "1"] === toList mapped

tests :: IO Bool
tests =
  checkParallel $$(discover)
