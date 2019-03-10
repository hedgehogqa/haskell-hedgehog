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
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
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
  -- * Properties
    Property
  , PropertyT
  , Group(..)
  , PropertyName
  , GroupName

  , property
  , test

  , forAll
  , forAllWith
  , discard

  , check
  , recheck

  , discover
  , discoverPrefix
  , checkParallel
  , checkSequential

  , withTests
  , TestLimit

  , withDiscards
  , DiscardLimit

  , withShrinks
  , ShrinkLimit

  , withRetries
  , ShrinkRetries

  -- * Generating Test Data
  , Gen
  , GenT
  , MonadGen(..)

  , Range
  , Size(..)
  , Seed(..)

  -- * Tests
  , Test
  , TestT
  , MonadTest(..)
  , annotate
  , annotateShow
  , footnote
  , footnoteShow
  , success
  , failure
  , assert
  , (===)
  , (/==)
  , tripping

  , eval
  , evalM
  , evalIO
  , evalEither
  , evalExceptT

  -- * State Machine Tests
  , Command(..)
  , Callback(..)
  , Action
  , Sequential(..)
  , Parallel(..)
  , executeSequential
  , executeParallel

  , Var(..)
  , concrete
  , opaque

  , Symbolic
  , Concrete(..)
  , Opaque(..)

  -- * Transformers
  , distribute

  -- * Functors
  , HTraversable(..)

  , Eq1
  , eq1

  , Ord1
  , compare1

  , Show1
  , showsPrec1
  ) where

import           Data.Functor.Classes (Eq1, eq1, Ord1, compare1, Show1, showsPrec1)

import           Hedgehog.Internal.Distributive (Distributive(..))
import           Hedgehog.Internal.Gen (Gen, GenT, MonadGen(..))
import           Hedgehog.Internal.HTraversable (HTraversable(..))
import           Hedgehog.Internal.Opaque (Opaque(..))
import           Hedgehog.Internal.Property (annotate, annotateShow)
import           Hedgehog.Internal.Property (assert, (===), (/==))
import           Hedgehog.Internal.Property (discard, failure, success)
import           Hedgehog.Internal.Property (DiscardLimit, withDiscards)
import           Hedgehog.Internal.Property (eval, evalM, evalIO)
import           Hedgehog.Internal.Property (evalEither, evalExceptT)
import           Hedgehog.Internal.Property (footnote, footnoteShow)
import           Hedgehog.Internal.Property (forAll, forAllWith)
import           Hedgehog.Internal.Property (MonadTest(..))
import           Hedgehog.Internal.Property (Property, PropertyT, PropertyName)
import           Hedgehog.Internal.Property (Group(..), GroupName)
import           Hedgehog.Internal.Property (ShrinkLimit, withShrinks)
import           Hedgehog.Internal.Property (ShrinkRetries, withRetries)
import           Hedgehog.Internal.Property (Test, TestT, property, test)
import           Hedgehog.Internal.Property (TestLimit, withTests)
import           Hedgehog.Internal.Range (Range, Size(..))
import           Hedgehog.Internal.Runner (check, recheck, checkSequential, checkParallel)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.State (Command(..), Callback(..))
import           Hedgehog.Internal.State (Action, Sequential(..), Parallel(..))
import           Hedgehog.Internal.State (executeSequential, executeParallel)
import           Hedgehog.Internal.State (Var(..), Symbolic, Concrete(..), concrete, opaque)
import           Hedgehog.Internal.TH (discover, discoverPrefix)
import           Hedgehog.Internal.Tripping (tripping)
