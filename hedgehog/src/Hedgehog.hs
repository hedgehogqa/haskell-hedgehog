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
-- >     xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
-- >     reverse (reverse xs) === xs
--
-- And add the Template Haskell splice which will discover your properties:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkParallel $$(discover)
--
-- If you prefer to avoid macros, you can specify the group of properties to
-- run manually instead:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkParallel $ Group "Test.Example" [
-- >       ("prop_reverse", prop_reverse)
-- >     ]
--
-- You can then load the module in GHCi, and run it:
--
-- > λ tests
-- > ━━━ Test.Example ━━━
-- >   ✓ prop_reverse passed 100 tests.
--
module Hedgehog (
    Group(..)
  , GroupName
  , Property
  , PropertyName
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
  , recheck

  , discover
  , checkParallel
  , checkSequential

  -- * Test
  , forAll
  , forAllWith
  , annotate
  , annotateShow
  , footnote
  , footnoteShow
  , success
  , discard
  , failure
  , assert
  , (===)

  , liftEither
  , liftExceptT
  , withExceptT
  , withResourceT

  , tripping

  -- * Transformers
  , distribute
  ) where

import           Hedgehog.Gen (Gen)
import           Hedgehog.Internal.Distributive (distribute)
import           Hedgehog.Internal.Property (annotate, annotateShow)
import           Hedgehog.Internal.Property (assert, (===))
import           Hedgehog.Internal.Property (discard, failure, success)
import           Hedgehog.Internal.Property (DiscardLimit, withDiscards)
import           Hedgehog.Internal.Property (footnote, footnoteShow)
import           Hedgehog.Internal.Property (forAll, forAllWith)
import           Hedgehog.Internal.Property (liftEither, liftExceptT)
import           Hedgehog.Internal.Property (Property, PropertyName, Group(..), GroupName)
import           Hedgehog.Internal.Property (ShrinkLimit, withShrinks)
import           Hedgehog.Internal.Property (Test, property)
import           Hedgehog.Internal.Property (TestLimit, withTests)
import           Hedgehog.Internal.Property (withExceptT, withResourceT)
import           Hedgehog.Internal.Runner (check, recheck, checkSequential, checkParallel)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.TH (discover)
import           Hedgehog.Internal.Tripping (tripping)
import           Hedgehog.Range (Range, Size(..))
