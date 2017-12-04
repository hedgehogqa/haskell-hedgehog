{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Distributive
module Hedgehog.Internal.Property (
  -- * Property
    Property(..)
  , PropertyT(..)
  , PropertyName(..)
  , PropertyConfig(..)
  , TestLimit(..)
  , DiscardLimit(..)
  , ShrinkLimit(..)
  , ShrinkRetries(..)
  , withTests
  , withDiscards
  , withShrinks
  , withRetries
  , property
  , test
  , forAll
  , forAllT
  , forAllWith
  , forAllWithT
  , discard

  -- * Group
  , Group(..)
  , GroupName(..)

  -- * TestT
  , MonadTest(..)
  , Test
  , TestT(..)
  , Log(..)
  , Failure(..)
  , Diff(..)
  , annotate
  , annotateShow
  , footnote
  , footnoteShow
  , failure
  , success
  , assert
  , (===)

  , eval
  , evalM
  , evalIO
  , evalEither
  , evalExceptT

  -- * Internal
  -- $internal
  , defaultConfig
  , mapConfig
  , failDiff
  , failException
  , failWith
  , writeLog

  , mkTest
  , mkTestT
  , runTest
  , runTestT
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import           Control.Monad.Catch (SomeException(..), displayException)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Cont (ContT)
import           Control.Monad.Trans.Control (ComposeSt, defaultLiftBaseWith, defaultRestoreM)
import           Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..))
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (MonadResource(..))
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import qualified Data.Char as Char
import           Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import           Data.Semigroup (Semigroup)
import           Data.String (IsString)
import           Data.Typeable (typeOf)

import           Hedgehog.Internal.Distributive
import           Hedgehog.Internal.Exception
import           Hedgehog.Internal.Gen (Gen, GenT)
import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source

import           Language.Haskell.TH.Lift (deriveLift)

------------------------------------------------------------------------

-- | A property test, along with some configurable limits like how many times
--   to run the test.
--
data Property =
  Property {
      propertyConfig :: !PropertyConfig
    , propertyTest :: PropertyT IO ()
    }

-- | The property monad transformer allows both the generation of test inputs
--   and the assertion of expectations.
--
newtype PropertyT m a =
  PropertyT {
      unPropertyT :: TestT (GenT m) a
    } deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadBase b
    , MonadThrow
    , MonadCatch
    , MonadReader r
    , MonadState s
    , MonadError e
    )

-- | A test monad allows the assertion of expectations.
--
type Test =
  TestT Identity

-- | A test monad transformer allows the assertion of expectations.
--
newtype TestT m a =
  TestT {
      unTest :: ExceptT Failure (Lazy.WriterT [Log] m) a
    } deriving (
      Functor
    , Applicative
    , MonadIO
    , MonadBase b
    , MonadThrow
    , MonadCatch
    , MonadReader r
    , MonadState s
    )

-- | The name of a property.
--
--   Can be constructed using `OverloadedStrings`:
--
-- @
--   "apples" :: PropertyName
-- @
--
newtype PropertyName =
  PropertyName {
      unPropertyName :: String
    } deriving (Eq, Ord, Show, IsString, Semigroup)

-- | Configuration for a property test.
--
data PropertyConfig =
  PropertyConfig {
      propertyTestLimit :: !TestLimit
    , propertyDiscardLimit :: !DiscardLimit
    , propertyShrinkLimit :: !ShrinkLimit
    , propertyShrinkRetries :: !ShrinkRetries
    } deriving (Eq, Ord, Show)

-- | The number of successful tests that need to be run before a property test
--   is considered successful.
--
--   Can be constructed using numeric literals:
--
-- @
--   200 :: TestLimit
-- @
--
newtype TestLimit =
  TestLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of discards to allow before giving up.
--
--   Can be constructed using numeric literals:
--
-- @
--   10000 :: DiscardLimit
-- @
--
--
newtype DiscardLimit =
  DiscardLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of shrinks to try before giving up on shrinking.
--
--   Can be constructed using numeric literals:
--
-- @
--   1000 :: ShrinkLimit
-- @
--
newtype ShrinkLimit =
  ShrinkLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of times to re-run a test during shrinking. This is useful if
--   you are testing something which fails non-deterministically and you want to
--   increase the change of getting a good shrink.
--
--   If you are doing parallel state machine testing, you should probably set
--   shrink retries to something like @10@. This will mean that during
--   shrinking, a parallel test case requires 10 successful runs before it is
--   passes and we try a different shrink.
--
--   Can be constructed using numeric literals:
--
-- @
--   0 :: ShrinkRetries
-- @
--
newtype ShrinkRetries =
  ShrinkRetries Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | A named collection of property tests.
--
data Group =
  Group {
      groupName :: !GroupName
    , groupProperties :: ![(PropertyName, Property)]
    }

-- | The name of a group of properties.
--
--   Can be constructed using `OverloadedStrings`:
--
-- @
--   "fruit" :: GroupName
-- @
--
newtype GroupName =
  GroupName {
      unGroupName :: String
    } deriving (Eq, Ord, Show, IsString, Semigroup)

--
-- FIXME This whole Log/Failure thing could be a lot more structured to allow
-- FIXME for richer user controlled error messages, think Doc. Ideally we'd
-- FIXME allow user's to create their own diffs anywhere.
--

-- | Log messages which are recorded during a test run.
--
data Log =
    Annotation (Maybe Span) String
  | Footnote String
    deriving (Eq, Show)

-- | Details on where and why a test failed.
--
data Failure =
  Failure (Maybe Span) String (Maybe Diff)
  deriving (Eq, Show)

-- | The difference between some expected and actual value.
--
data Diff =
  Diff {
      diffPrefix :: String
    , diffRemoved :: String
    , diffInfix :: String
    , diffAdded :: String
    , diffSuffix :: String
    , diffValue :: ValueDiff
    } deriving (Eq, Show)

------------------------------------------------------------------------
-- TestT

instance Monad m => Monad (TestT m) where
  return =
    TestT . return

  (>>=) m k =
    TestT $
      unTest m >>=
      unTest . k

  fail err =
    TestT . ExceptT . pure . Left $ Failure Nothing err Nothing

instance MonadTrans TestT where
  lift =
    TestT . lift . lift

instance MFunctor TestT where
  hoist f =
    TestT . hoist (hoist f) . unTest

instance Distributive TestT where
  type Transformer t TestT m = (
      Transformer t (Lazy.WriterT [Log]) m
    , Transformer t (ExceptT Failure) (Lazy.WriterT [Log] m)
    )

  distribute =
    hoist TestT .
    distribute .
    hoist distribute .
    unTest

instance PrimMonad m => PrimMonad (TestT m) where
  type PrimState (TestT m) =
    PrimState m
  primitive =
    lift . primitive

-- FIXME instance MonadWriter w m => MonadWriter w (TestT m)

instance MonadError e m => MonadError e (TestT m) where
  throwError =
    lift . throwError
  catchError m onErr =
    TestT . ExceptT $
      (runExceptT $ unTest m) `catchError`
      (runExceptT . unTest . onErr)

instance MonadResource m => MonadResource (TestT m) where
  liftResourceT =
    lift . liftResourceT

instance MonadTransControl TestT where
  type StT TestT a =
    (Either Failure a, [Log])

  liftWith f =
    mkTestT . fmap (, []) . fmap Right $ f $ runTestT

  restoreT =
    mkTestT

instance MonadBaseControl b m => MonadBaseControl b (TestT m) where
  type StM (TestT m) a =
    ComposeSt TestT m a

  liftBaseWith =
    defaultLiftBaseWith

  restoreM =
    defaultRestoreM

class Monad m => MonadTest m where
  liftTest :: Test a -> m a

instance Monad m => MonadTest (TestT m) where
  liftTest =
    hoist (pure . runIdentity)

instance MonadTest m => MonadTest (IdentityT m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (MaybeT m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (ExceptT x m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (ReaderT r m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (Lazy.StateT s m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (Strict.StateT s m) where
  liftTest =
    lift . liftTest

instance (MonadTest m, Monoid w) => MonadTest (Lazy.WriterT w m) where
  liftTest =
    lift . liftTest

instance (MonadTest m, Monoid w) => MonadTest (Strict.WriterT w m) where
  liftTest =
    lift . liftTest

instance (MonadTest m, Monoid w) => MonadTest (Lazy.RWST r w s m) where
  liftTest =
    lift . liftTest

instance (MonadTest m, Monoid w) => MonadTest (Strict.RWST r w s m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (ContT r m) where
  liftTest =
    lift . liftTest

instance MonadTest m => MonadTest (ResourceT m) where
  liftTest =
    lift . liftTest

mkTestT :: m (Either Failure a, [Log]) -> TestT m a
mkTestT =
  TestT . ExceptT . Lazy.WriterT

mkTest :: (Either Failure a, [Log]) -> Test a
mkTest =
  mkTestT . Identity

runTestT :: TestT m a -> m (Either Failure a, [Log])
runTestT =
  Lazy.runWriterT . runExceptT . unTest

runTest :: Test a -> (Either Failure a, [Log])
runTest =
  runIdentity . runTestT

-- | Log some information which might be relevant to a potential test failure.
--
writeLog :: MonadTest m => Log -> m ()
writeLog x =
  liftTest $ mkTest (pure (), [x])

-- | Fail the test with an error message, useful for building other failure
--   combinators.
--
failWith :: (MonadTest m, HasCallStack) => Maybe Diff -> String -> m a
failWith diff msg =
  liftTest $ mkTest (Left $ Failure (getCaller callStack) msg diff, [])

-- | Annotates the source code with a message that might be useful for
--   debugging a test failure.
--
annotate :: (MonadTest m, HasCallStack) => String -> m ()
annotate x = do
  writeLog $ Annotation (getCaller callStack) x

-- | Annotates the source code with a value that might be useful for
--   debugging a test failure.
--
annotateShow :: (MonadTest m, Show a, HasCallStack) => a -> m ()
annotateShow x = do
  withFrozenCallStack $ annotate (showPretty x)

-- | Logs a message to be displayed as additional information in the footer of
--   the failure report.
--
footnote :: MonadTest m => String -> m ()
footnote =
  writeLog . Footnote

-- | Logs a value to be displayed as additional information in the footer of
--   the failure report.
--
footnoteShow :: (MonadTest m, Show a) => a -> m ()
footnoteShow =
  writeLog . Footnote . showPretty

-- | Fails with an error which shows the difference between two values.
--
failDiff :: (MonadTest m, Show a, Show b, HasCallStack) => a -> b -> m ()
failDiff x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      withFrozenCallStack $
        failWith Nothing $ unlines [
            "━━━ Not Equal ━━━"
          , showPretty x
          , showPretty y
          ]
    Just diff ->
      withFrozenCallStack $
        failWith (Just $ Diff "Failed (" "- lhs" "=/=" "+ rhs" ")" diff) ""

-- | Fails with an error which renders the type of an exception and its error
--   message.
--
failException :: (MonadTest m, HasCallStack) => SomeException -> m a
failException (SomeException x) =
  withFrozenCallStack $
    failWith Nothing $ unlines [
        "━━━ Exception: " ++ show (typeOf x) ++ " ━━━"
      , List.dropWhileEnd Char.isSpace (displayException x)
      ]

-- | Causes a test to fail.
--
failure :: (MonadTest m, HasCallStack) => m a
failure =
  withFrozenCallStack $ failWith Nothing ""

-- | Another name for @pure ()@.
--
success :: MonadTest m => m ()
success =
  pure ()

-- | Fails the test if the condition provided is 'False'.
--
assert :: (MonadTest m, HasCallStack) => Bool -> m ()
assert b = do
  ok <- withFrozenCallStack $ eval b
  if ok then
    success
  else
    withFrozenCallStack failure

infix 4 ===

-- | Fails the test if the two arguments provided are not equal.
--
(===) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(===) x y = do
  ok <- withFrozenCallStack $ eval (x == y)
  if ok then
    success
  else
    withFrozenCallStack $ failDiff x y

-- | Fails the test if the value throws an exception when evaluated to weak
--   head normal form (WHNF).
--
eval :: (MonadTest m, HasCallStack) => a -> m a
eval x =
  either (withFrozenCallStack failException) pure (tryEvaluate x)

-- | Fails the test if the action throws an exception.
--
--   /The benefit of using this over simply letting the exception bubble up is/
--   /that the location of the closest 'evalM' will be shown in the output./
--
evalM :: (MonadTest m, MonadCatch m, HasCallStack) => m a -> m a
evalM m =
  either (withFrozenCallStack failException) pure =<< tryAll m

-- | Fails the test if the 'IO' action throws an exception.
--
--   /The benefit of using this over 'liftIO' is that the location of the/
--   /exception will be shown in the output./
--
evalIO :: (MonadTest m, MonadIO m, HasCallStack) => IO a -> m a
evalIO m =
  either (withFrozenCallStack failException) pure =<< liftIO (tryAll m)

-- | Fails the test if the 'Either' is 'Left', otherwise returns the value in
--   the 'Right'.
--
evalEither :: (MonadTest m, Show x, HasCallStack) => Either x a -> m a
evalEither = \case
  Left x ->
    withFrozenCallStack $ failWith Nothing $ showPretty x
  Right x ->
    pure x

-- | Fails the test if the 'ExceptT' is 'Left', otherwise returns the value in
--   the 'Right'.
--
evalExceptT :: (MonadTest m, Show x, HasCallStack) => ExceptT x m a -> m a
evalExceptT m =
  withFrozenCallStack evalEither =<< runExceptT m

------------------------------------------------------------------------
-- PropertyT

instance MonadTrans PropertyT where
  lift =
    PropertyT . lift . lift

instance MFunctor PropertyT where
  hoist f =
    PropertyT . hoist (hoist f) . unPropertyT

instance Distributive PropertyT where
  type Transformer t PropertyT m = (
      Transformer t GenT m
    , Transformer t TestT (GenT m)
    )

  distribute =
    hoist PropertyT .
    distribute .
    hoist distribute .
    unPropertyT

instance PrimMonad m => PrimMonad (PropertyT m) where
  type PrimState (PropertyT m) =
    PrimState m
  primitive =
    lift . primitive

---- FIXME instance MonadWriter w m => MonadWriter w (PropertyT m)

instance Monad m => MonadTest (PropertyT m) where
  liftTest =
    PropertyT . hoist (pure . runIdentity)

instance MonadPlus m => MonadPlus (PropertyT m) where
  mzero =
    discard

  mplus (PropertyT x) (PropertyT y) =
    PropertyT . mkTestT $
      mplus (runTestT x) (runTestT y)

instance MonadPlus m => Alternative (PropertyT m) where
  empty =
    mzero
  (<|>) =
    mplus

-- | Generates a random input for the test by running the provided generator.
--
--   /This is a the same as 'forAllT' but allows the user to provide a custom/
--   /rendering function. This is useful for values which don't have a/
--   /'Show' instance./
--
forAllWithT :: (Monad m, HasCallStack) => (a -> String) -> GenT m a -> PropertyT m a
forAllWithT render gen = do
  x <- PropertyT $ lift gen
  withFrozenCallStack $ annotate (render x)
  return x

-- | Generates a random input for the test by running the provided generator.
--
--   /This is a the same as 'forAll' but allows the user to provide a custom/
--   /rendering function. This is useful for values which don't have a/
--   /'Show' instance./
--
forAllWith :: (Monad m, HasCallStack) => (a -> String) -> Gen a -> PropertyT m a
forAllWith render gen =
  withFrozenCallStack $ forAllWithT render $ Gen.lift gen

-- | Generates a random input for the test by running the provided generator.
--
forAllT :: (Monad m, Show a, HasCallStack) => GenT m a -> PropertyT m a
forAllT gen =
  withFrozenCallStack $ forAllWithT showPretty gen

-- | Generates a random input for the test by running the provided generator.
--
forAll :: (Monad m, Show a, HasCallStack) => Gen a -> PropertyT m a
forAll gen =
  withFrozenCallStack $ forAllWith showPretty gen

-- | Discards the current test entirely.
--
discard :: Monad m => PropertyT m a
discard =
  PropertyT $ lift Gen.discard

-- | Lift a test in to a property.
--
--   Because both 'TestT' and 'PropertyT' have 'MonadTest' instances, this
--   function is not often required. It can however be useful for writing
--   functions directly in 'TestT' and thus gaining a 'MonadTransControl'
--   instance at the expense of not being able to generate additional inputs
--   using 'forAll'.
--
--   One use case for this is writing tests which use 'ResourceT':
--
-- @
--   property $ do
--     n <- forAll $ Gen.int64 Range.linearBounded
--     test . runResourceT $ do
--       -- test with resource usage here
-- @
--
test :: Monad m => TestT m a -> PropertyT m a
test =
  PropertyT . hoist lift

------------------------------------------------------------------------
-- Property

-- | The default configuration for a property test.
--
defaultConfig :: PropertyConfig
defaultConfig =
  PropertyConfig {
      propertyTestLimit =
        100
    , propertyDiscardLimit =
        100
    , propertyShrinkLimit =
        1000
    , propertyShrinkRetries =
        0
    }

-- | Map a config modification function over a property.
--
mapConfig :: (PropertyConfig -> PropertyConfig) -> Property -> Property
mapConfig f (Property cfg t) =
  Property (f cfg) t

-- | Set the number times a property should be executed before it is considered
--   successful.
--
--   If you have a test that does not involve any generators and thus does not
--   need to run repeatedly, you can use @withTests 1@ to define a property that
--   will only be checked once.
--
withTests :: TestLimit -> Property -> Property
withTests n =
  mapConfig $ \config -> config { propertyTestLimit = n }

-- | Set the number times a property is allowed to discard before the test
--   runner gives up.
--
withDiscards :: DiscardLimit -> Property -> Property
withDiscards n =
  mapConfig $ \config -> config { propertyDiscardLimit = n }

-- | Set the number times a property is allowed to shrink before the test
--   runner gives up and prints the counterexample.
--
withShrinks :: ShrinkLimit -> Property -> Property
withShrinks n =
  mapConfig $ \config -> config { propertyShrinkLimit = n }

-- | Set the number times a property will be executed for each shrink before
--   the test runner gives up and tries a different shrink. See 'ShrinkRetries'
--   for more information.
--
withRetries :: ShrinkRetries -> Property -> Property
withRetries n =
  mapConfig $ \config -> config { propertyShrinkRetries = n }

-- | Creates a property with the default configuration.
--
property :: HasCallStack => PropertyT IO () -> Property
property m =
  Property defaultConfig $
    withFrozenCallStack (evalM m)

------------------------------------------------------------------------
-- FIXME Replace with DeriveLift when we drop 7.10 support.

$(deriveLift ''GroupName)
$(deriveLift ''PropertyName)
$(deriveLift ''PropertyConfig)
$(deriveLift ''TestLimit)
$(deriveLift ''DiscardLimit)
$(deriveLift ''ShrinkLimit)
$(deriveLift ''ShrinkRetries)

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
