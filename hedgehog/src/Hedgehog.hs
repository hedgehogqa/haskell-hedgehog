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
-- > prop_reverse :: Property
-- > prop_reverse =
-- >   property $ do
-- >     xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
-- >     reverse (reverse xs) === xs
--
-- And add the Template Haskell splice which will run your properies:
--
-- > tests :: IO Bool
-- > tests =
-- >   $$(checkConcurrent)
--
-- You can then load the module in GHCi, and run it:
--
-- >>> tests
-- ━━━ Test.Example ━━━
--   ✓ prop_reverse passed 100 tests.
--
module Hedgehog (
    Property
  , Test
  , TestLimit
  , DiscardLimit
  , ShrinkLimit

  , Gen
  , Range
  , Size(..)
  , Seed(..)

  -- * Property
  , property
  , withTests
  , withDiscards
  , withShrinks

  , check
  , checkSequential
  , checkConcurrent
  , recheck

  -- * Test
  , forAll
  , info
  , success
  , discard
  , failure
  , assert
  , (===)

  , liftEither
  , liftExceptT
  , withResourceT

  , tripping
  ) where

import           Hedgehog.Gen (Gen)
import           Hedgehog.Internal.Property (assert, (===))
import           Hedgehog.Internal.Property (discard, failure, success)
import           Hedgehog.Internal.Property (DiscardLimit, withDiscards)
import           Hedgehog.Internal.Property (forAll, info)
import           Hedgehog.Internal.Property (liftEither, liftExceptT, withResourceT)
import           Hedgehog.Internal.Property (Property)
import           Hedgehog.Internal.Property (ShrinkLimit, withShrinks)
import           Hedgehog.Internal.Property (Test, property)
import           Hedgehog.Internal.Property (TestLimit, withTests)
import           Hedgehog.Internal.Runner (check, recheck)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.TH (checkSequential, checkConcurrent)
import           Hedgehog.Internal.Tripping (tripping)
import           Hedgehog.Range (Range, Size(..))
