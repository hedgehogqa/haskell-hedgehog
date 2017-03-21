-- |
-- This module includes almost everything you need to get started writing
-- property tests with Hedgehog.
--
-- It is designed to be used alongside "Hedgehog.Gen" and "Hedgehog.Range",
-- which should be imported qualified. You also need to enable Template Haskell
-- so the Hedgehog test runner can find your properties.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import           Hedgehog
-- > import qualified Hedgehog.Gen as Gen
-- > import qualified Hedgehog.Range as Range
--
-- Once you have your imports set up, you can write a simple property:
--
-- > prop_reverse :: Monad m => Property m ()
-- > prop_reverse =
-- >   xs <- given $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
-- >   reverse (reverse xs) === xs
--
-- And add the Template Haskell splice which will run your properies:
--
-- > tests :: IO Bool
-- > tests =
-- >   $$(checkAll)
--
-- You can then load the module in GHCi, and run it:
--
-- >>> tests
-- ━━━ Test.Example ━━━
--   ✓ prop_reverse passed 100 tests.
--
module Hedgehog (
    Property
  , Gen
  , Range
  , Seed(..)
  , Size(..)

  -- * Construction
  , forAll
  , info
  , success
  , discard
  , failure
  , assert
  , (===)

  -- * Validation
  , check
  , checkAll
  , recheck
  ) where

import           Hedgehog.Gen (Gen)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Property (assert, (===))
import           Hedgehog.Property (discard, failure, success)
import           Hedgehog.Property (forAll, info)
import           Hedgehog.Property (Property)
import           Hedgehog.Range (Range, Size(..))
import           Hedgehog.Runner (check, recheck)
import           Hedgehog.TH (checkAll)
