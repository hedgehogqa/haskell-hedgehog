module Text (
    textProperties
  ) where

import           Data.Foldable (elem)
import           Data.Typeable (Typeable)

import           Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

genSize :: Monad m
        => G.Gen m Size
genSize =
  Size <$> G.enumBounded

genSeed :: Monad m
        => G.Gen m Seed
genSeed =
  Seed <$> G.enumBounded <*> oddGen
    where
      oddGen = mkOdd <$> G.enumBounded
      mkOdd x
        | x == 0 = 1
        | odd x = abs x
        | otherwise = pred (abs x)

genPred :: Monad m
        => G.Gen m Int
genPred =
  G.int (R.constant 0 11)

showAppendProp :: (Typeable a, Show a)
               => G.Gen IO a
               -> Property
showAppendProp g =
  let
    genString =
      G.string (R.constant 0 100) (G.enum 'a' 'z')
  in
    property $ do
      p <- forAll genPred
      a <- forAll g
      r <- forAll genString
      s <- forAll genString
      showsPrec p a r ++ s  === showsPrec p a (r ++ s)

readShowProp :: (Eq a, Typeable a, Show a, Read a)
             => G.Gen IO a
             -> Property
readShowProp g = property $ do
  p <- forAll genPred
  a <- forAll g
  elem (a, "") (readsPrec p (showsPrec p a "")) === True

textProperties :: [Property]
textProperties =
  [ showAppendProp genSize
  , readShowProp genSize
  , showAppendProp genSeed
  , readShowProp genSeed
  ]
