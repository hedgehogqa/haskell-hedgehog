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
  , recheckAt

  , discover
  , discoverPrefix
  , checkParallel
  , checkSequential

  , Confidence
  , verifiedTermination
  , withConfidence

  , withTests
  , TestLimit

  , withDiscards
  , DiscardLimit

  , withShrinks
  , ShrinkLimit

  , withRetries
  , ShrinkRetries

  , withSkip
  , Skip

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
  , diff
  , (===)
  , (/==)
  , tripping

  , eval
  , evalNF
  , evalM
  , evalIO
  , evalEither
  , evalEitherM
  , evalExceptT
  , evalMaybe
  , evalMaybeM

  -- * Coverage
  , LabelName
  , classify
  , cover
  , label
  , collect

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
  , distributeT

  -- * Functors
  -- $functors
  , FunctorB(..)
  , TraversableB(..)
  , Rec(..)

  , Eq1
  , eq1

  , Ord1
  , compare1

  , Show1
  , showsPrec1

  -- * Deprecated
  , HTraversable(..)
  ) where

import           Data.Functor.Classes (Eq1, eq1, Ord1, compare1, Show1, showsPrec1)

import           Hedgehog.Internal.Barbie (FunctorB(..), TraversableB(..), Rec(..))
import           Hedgehog.Internal.Distributive (distributeT)
import           Hedgehog.Internal.Gen (Gen, GenT, MonadGen(..))
import           Hedgehog.Internal.HTraversable (HTraversable(..))
import           Hedgehog.Internal.Opaque (Opaque(..))
import           Hedgehog.Internal.Property (annotate, annotateShow)
import           Hedgehog.Internal.Property (assert, diff, (===), (/==))
import           Hedgehog.Internal.Property (classify, cover)
import           Hedgehog.Internal.Property (discard, failure, success)
import           Hedgehog.Internal.Property (DiscardLimit, withDiscards)
import           Hedgehog.Internal.Property (eval, evalNF, evalM, evalIO)
import           Hedgehog.Internal.Property (evalEither, evalEitherM, evalExceptT, evalMaybe, evalMaybeM)
import           Hedgehog.Internal.Property (footnote, footnoteShow)
import           Hedgehog.Internal.Property (forAll, forAllWith)
import           Hedgehog.Internal.Property (LabelName, MonadTest(..))
import           Hedgehog.Internal.Property (Property, PropertyT, PropertyName)
import           Hedgehog.Internal.Property (Group(..), GroupName)
import           Hedgehog.Internal.Property (Confidence, verifiedTermination, withConfidence)
import           Hedgehog.Internal.Property (ShrinkLimit, withShrinks)
import           Hedgehog.Internal.Property (ShrinkRetries, withRetries)
import           Hedgehog.Internal.Property (Skip, withSkip)
import           Hedgehog.Internal.Property (Test, TestT, property, test)
import           Hedgehog.Internal.Property (TestLimit, withTests)
import           Hedgehog.Internal.Property (collect, label)
import           Hedgehog.Internal.Range (Range, Size(..))
import           Hedgehog.Internal.Runner (check, recheck, recheckAt, checkSequential, checkParallel)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.State (Command(..), Callback(..))
import           Hedgehog.Internal.State (Action, Sequential(..), Parallel(..))
import           Hedgehog.Internal.State (executeSequential, executeParallel)
import           Hedgehog.Internal.State (Var(..), Symbolic, Concrete(..), concrete, opaque)
import           Hedgehog.Internal.TH (discover, discoverPrefix)
import           Hedgehog.Internal.Tripping (tripping)


-- $functors
--
-- 'FunctorB' and 'TraversableB' must be implemented for all 'Command' @input@ types.
--
-- This is most easily achieved using `DeriveGeneric`:
--
-- @
-- data Register v =
--   Register Name (Var Pid v)
--   deriving (Eq, Show, Generic)
--
-- instance FunctorB Register
-- instance TraversableB Register
--
-- newtype Unregister (v :: * -> *) =
--   Unregister Name
--   deriving (Eq, Show, Generic)
--
-- instance FunctorB Unregister
-- instance TraversableB Unregister
-- @
--
-- `DeriveAnyClass` and `DerivingStrategies` allow a more compact syntax:
--
-- @
-- data Register v =
--   Register Name (Var Pid v)
--   deriving (Eq, Show, Generic, FunctorB, TraversableB)
--
-- newtype Unregister (v :: * -> *) =
--   Unregister Name
--   deriving (Eq, Show, Generic)
--   deriving anyclass (FunctorB, TraversableB)
-- @
--
