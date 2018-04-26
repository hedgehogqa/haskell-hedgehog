{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hedgehog.Text where

import           Data.Int (Int64)
import           Data.Typeable (Typeable)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Text.Read (readEither)


genSize :: Gen Size
genSize =
  Size <$> Gen.enumBounded

genOdd :: Gen Int64
genOdd =
  let
    mkOdd x =
      if odd x then
        x
      else
        pred x
  in
    mkOdd <$> Gen.int64 (Range.constant 1 maxBound)

genSeed :: Gen Seed
genSeed =
  Seed <$> Gen.word64 Range.constantBounded <*> fmap fromIntegral genOdd

genPrecedence :: Gen Int
genPrecedence =
  Gen.int (Range.constant 0 11)

genString :: Gen String
genString =
  Gen.string (Range.constant 0 100) Gen.alpha

checkShowAppend :: (Typeable a, Show a) => Gen a -> Property
checkShowAppend gen =
  property $ do
    prec <- forAll genPrecedence
    x <- forAll gen
    xsuffix <- forAll genString
    ysuffix <- forAll genString
    showsPrec prec x xsuffix ++ ysuffix  === showsPrec prec x (xsuffix ++ ysuffix)

trippingReadShow :: (Eq a, Typeable a, Show a, Read a) => Gen a -> Property
trippingReadShow gen =
  property $ do
    prec <- forAll genPrecedence
    x <- forAll gen
    tripping x (\z -> showsPrec prec z "") readEither

prop_show_append_size :: Property
prop_show_append_size =
  checkShowAppend genSize

prop_tripping_append_size :: Property
prop_tripping_append_size =
  trippingReadShow genSize

prop_show_append_seed :: Property
prop_show_append_seed =
  checkShowAppend genSeed

prop_tripping_append_seed :: Property
prop_tripping_append_seed =
  trippingReadShow genSeed

tests :: IO Bool
tests =
  checkParallel $$(discover)
