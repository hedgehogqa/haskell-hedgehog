{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.Basic where

import           Control.Monad (guard)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

------------------------------------------------------------------------
-- Example 0

prop_success :: Property
prop_success =
  property success

prop_discard :: Property
prop_discard =
  property discard

prop_failure :: Property
prop_failure =
  property failure

{-
prop_commented_out_properties_do_not_run :: Property
prop_commented_out_properties_do_not_run =
  pure ()
-}

------------------------------------------------------------------------
-- Example 1

prop_test_limit :: Property
prop_test_limit =
  withTests 10000 . property $
    success

prop_discard_limit :: Property
prop_discard_limit =
  withDiscards 5000 . property $
    discard

prop_shrink_limit :: Property
prop_shrink_limit =
  withShrinks 0 . property $ do
    x <- forAll $ Gen.enum 'a' 'z'
    assert $
      x == 'z'

------------------------------------------------------------------------
-- Example 2

-- Try 'check prop_foo' to see what happens
prop_foo :: Property
prop_foo =
  property $ do
    x <- forAll $ Gen.enum 'a' 'z'
    y <- forAll $
      Gen.choice [
          Gen.integral (Range.linear 0 1000)
        , Gen.integral (Range.linear 0 1000)
        ]

    guard (y `mod` 2 == (1 :: Int))

    assert $
      y < 87 && x <= 'r'

------------------------------------------------------------------------
-- Example 3

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
prop_total :: Property
prop_total =
  property $ do
    x <- forAll (order $ Gen.choice [cheap, expensive])
    y <- forAll (order expensive)
    total (merge x y) === total x + total y

------------------------------------------------------------------------
-- Example 4 - Hutton's Razor

data Exp =
    Lit !Int
  | Add !Exp !Exp
    deriving (Eq, Ord, Show)

evalExp :: Exp -> Int
evalExp = \case
  Lit x ->
    x
  Add x y ->
    evalExp x + evalExp y

--
-- The subterm combinators (Gen.subterm, Gen.subtermM, Gen.subterm2,
-- Gen.subterm2M, etc) allow a generator to shrink to one of its sub-terms.
--
-- In the example below, the Add expression can shrink to either of its
-- sub-terms:
--

genExp1 :: Monad m => Gen m Exp
genExp1 =
  Gen.recursive Gen.choice [
      Lit <$> Gen.int (Range.linear 0 10000)
    ] [
      Gen.subterm2 genExp1 genExp1 Add
    ]

prop_hutton_1 :: Property
prop_hutton_1 =
  property $ do
    x <- forAll genExp1
    case x of
      Add (Add _ _) _ ->
        assert (evalExp x < 100)
      _ ->
        success

--
-- Gen.shrink is a more general way to add shrinks to a generator.
--
-- Here we use it to replace an expression with the literal it evaluates to:
--

shrinkExp2 :: Exp -> [Exp]
shrinkExp2 = \case
  Lit _ ->
    []
  Add x y ->
    [Lit (evalExp (Add x y))]

genExp2 :: Monad m => Gen m Exp
genExp2 =
  Gen.shrink shrinkExp2 $
  Gen.recursive Gen.choice [
      Lit <$> Gen.int (Range.linear 0 10000)
    ] [
      Gen.subterm2 genExp2 genExp2 Add
    ]

prop_hutton_2 :: Property
prop_hutton_2 =
  property $ do
    x <- forAll genExp2
    case x of
      Add (Add _ _) _ ->
        assert (evalExp x < 100)
      _ ->
        success

------------------------------------------------------------------------
-- Example 5 - Diff Record

data SomeRecord =
  SomeRecord {
      someInt :: Int
    , someBool :: Bool
    , someDouble :: Double
    , someList :: [(Int, String)]
    } deriving (Eq, Show)

genRecord :: Monad m => Gen m SomeRecord
genRecord =
  SomeRecord
    <$> Gen.int (Range.linearFrom 0 (-1000) 1000)
    <*> Gen.bool
    <*> Gen.double (Range.linearFrac 7.2 15.9)
    <*> Gen.list (Range.linear 5 100)
          ((,)
            <$> Gen.int (Range.constant 0 10)
            <*> Gen.string (Range.constant 2 4) Gen.alpha)

prop_record :: Property
prop_record =
  property $ do
    x <- forAll genRecord
    y <- forAll genRecord
    x === y

------------------------------------------------------------------------

tests :: IO Bool
tests =
  $$(checkAll)
