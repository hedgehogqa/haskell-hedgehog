{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Hedgehog.Example where

import           Control.Monad (guard)

import           Hedgehog.Gen (Gen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Property
import qualified Hedgehog.Range as Range
import           Hedgehog.TH

------------------------------------------------------------------------
-- Example 0

prop_success :: Monad m => Property m ()
prop_success =
  success

prop_discard :: Monad m => Property m ()
prop_discard =
  discard

prop_failure :: Monad m => Property m ()
prop_failure =
  failure

{-
prop_commented_out_properties_do_not_run :: Monad m => Property m ()
prop_commented_out_properties_do_not_run =
  pure ()
-}

------------------------------------------------------------------------
-- Example 1

-- Try 'check prop_foo' to see what happens
prop_foo :: Monad m => Property m ()
prop_foo = do
  x <- forAll $ Gen.enum 'a' 'z'
  y <- forAll $
    Gen.choice [
        Gen.integral (Range.linear 0 1000)
      , Gen.integral (Range.linear 0 1000)
      ]

  guard (y `mod` 2 == (1 :: Int))

  ensure $
    y < 87 && x <= 'r'

------------------------------------------------------------------------
-- Example 2

newtype Product =
  Product String
  deriving (Eq, Ord, Show)

newtype USD =
  USD Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data Item =
  Item Product USD
  deriving (Eq, Ord, Show)

newtype Order =
  Order [Item]
  deriving (Eq, Ord, Show)

merge :: Order -> Order -> Order
merge (Order xs) (Order ys) =
  Order $ xs ++ ys ++
    if any ((< 50) . price) xs &&
       any ((> 50) . price) xs &&
       any ((> 1050) . price) ys then
      [Item (Product "processing") (USD 1)]
    else
      []

price :: Item -> USD
price (Item _ x) =
  x

total :: Order -> USD
total (Order xs) =
  sum $ fmap price xs

cheap :: Monad m => Gen m Item
cheap =
  Item
    <$> (Product <$> Gen.element ["sandwich", "noodles"])
    <*> (USD <$> Gen.integral (Range.constant 5 10))

expensive :: Monad m => Gen m Item
expensive =
  Item
    <$> (Product <$> Gen.element ["oculus", "vive"])
    <*> (USD <$> Gen.integral (Range.linear 1000 2000))

order :: Monad m => Gen m Item -> Gen m Order
order gen =
  Order <$> Gen.list (Range.linear 0 50) gen

-- | Fails with:
--
-- @
-- λ check prop_total
-- *** Failed! Falsifiable (after 4 tests and 3 shrinks):
-- cheap :: Order
-- cheap =
--   Order []
--
-- expensive :: Order
-- expensive =
--   Order [ Item (Product "oculus") (USD 1000) ]
--
-- === Not Equal ===
-- USD 1001
-- USD 1000
-- @
--
prop_total :: Monad m => Property m ()
prop_total = do
  x <- forAll (order $ Gen.choice [cheap, expensive])
  y <- forAll (order expensive)
  total (merge x y) === total x + total y


tests :: IO Bool
tests =
  $$(checkAll)
