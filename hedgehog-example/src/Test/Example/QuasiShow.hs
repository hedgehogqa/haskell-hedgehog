{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.QuasiShow where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Language.Haskell.TH (Q, Exp)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax (Lift(..))


newtype Foo =
  Foo {
      unFoo :: String
    } deriving (Eq, Ord, Lift)

-- demo pretty-show supports quasi-quote syntax
instance Show Foo where
  showsPrec _ x =
    showString ("[foo|" ++ unFoo x ++ "|]")

data SomeData =
  SomeData {
      someFoo1 :: Foo
    , someFoo2 :: Foo
    } deriving (Eq, Ord, Show)

genFoo :: Gen Foo
genFoo =
  Foo <$> Gen.element ["kick", "flip", "flap"]

prop_foo :: Property
prop_foo =
  property $ do
    x <- forAll (SomeData <$> genFoo <*> genFoo)
    x === SomeData (Foo "quasi") (Foo "quoted")
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--  │ ━━━ Failed (- lhs) (+ rhs) ━━━
--  │ - SomeData { someFoo1 = [foo|kick|] , someFoo2 = [foo|kick|] }
--  │ + SomeData { someFoo1 = [foo|quasi|] , someFoo2 = [foo|quoted|] }

tests :: IO Bool
tests =
  checkParallel $$(discover)


------------------------------------------------------------------------
-- Can't actually use this in the same file we define it,
-- but this is what the quasi-quoter would look like.

mkQQ :: ([Char] -> Q Exp) -> QuasiQuoter
mkQQ f =
  let
    no c =
      const (fail ("This QQ produces expressions, not " ++ c))
  in
    QuasiQuoter f (no "patterns") (no "types") (no "declarations")

foo :: QuasiQuoter
foo =
  mkQQ (\str -> lift (Foo str))
