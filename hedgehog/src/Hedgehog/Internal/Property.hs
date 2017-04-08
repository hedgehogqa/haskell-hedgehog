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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
module Hedgehog.Internal.Property (
  -- * Property
    Property(..)
  , GroupName(..)
  , PropertyName(..)
  , PropertyConfig(..)
  , TestLimit(..)
  , DiscardLimit(..)
  , ShrinkLimit(..)
  , property
  , withTests
  , withDiscards
  , withShrinks

  -- * Test
  , Test(..)
  , Log(..)
  , Failure(..)
  , Diff(..)
  , forAll
  , info
  , discard
  , failure
  , success
  , assert
  , (===)

  , liftEither
  , liftExceptT
  , withResourceT

  -- * Internal
  -- $internal
  , defaultConfig
  , mapConfig
  , failWith
  , writeLog
  , runTest
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Resource (MonadResource(..), MonadResourceBase)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..))
import           Control.Monad.Writer.Class (MonadWriter(..))

import           Data.Typeable (Typeable, TypeRep, typeOf)

import           Hedgehog.Gen (Gen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source

import           Language.Haskell.TH.Lift (deriveLift)

------------------------------------------------------------------------

-- | A property test to check, along with some configurable limits like how
--   many times to run the test.
--
data Property =
  Property {
      propertyConfig :: !PropertyConfig
    , propertyTest :: Test IO ()
    }

-- | A property test.
--
newtype Test m a =
  Test {
      unTest :: ExceptT Failure (WriterT [Log] (Gen m)) a
    } deriving (Functor, Applicative)

-- | The name of a group of properties.
--
newtype GroupName =
  GroupName {
      unGroupName :: String
    } deriving (Eq, Ord, Show)

-- | The name of a property.
--
newtype PropertyName =
  PropertyName {
      unPropertyName :: String
    } deriving (Eq, Ord, Show)

-- | Configuration for a property test.
--
data PropertyConfig =
  PropertyConfig {
      propertyTestLimit :: !TestLimit
    , propertyDiscardLimit :: !DiscardLimit
    , propertyShrinkLimit :: !ShrinkLimit
    } deriving (Eq, Ord, Show)

-- | The number of successful tests that need to be run before a property test
--   is considered successful.
--
newtype TestLimit =
  TestLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of shrinks to try before giving up on shrinking.
--
newtype ShrinkLimit =
  ShrinkLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of discards to allow before giving up.
--
newtype DiscardLimit =
  DiscardLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

--
-- FIXME This whole Log/Failure thing could be a lot more structured to allow
-- FIXME for richer user controlled error messages, think Doc. Ideally we'd
-- FIXME allow user's to create their own diffs anywhere.
--

-- | Log messages which are recorded during a test run.
--
data Log =
    Info String
  | Input (Maybe Span) TypeRep String
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

instance Monad m => Monad (Test m) where
  return =
    Test . return

  (>>=) m k =
    Test $
      unTest m >>=
      unTest . k

  fail err =
    Test . ExceptT . pure . Left $ Failure Nothing err Nothing

instance Monad m => MonadPlus (Test m) where
  mzero =
    discard

  mplus x y =
    Test . ExceptT . WriterT $
      mplus (runTest x) (runTest y)

instance Monad m => Alternative (Test m) where
  empty =
    mzero
  (<|>) =
    mplus

instance MonadTrans Test where
  lift =
    Test . lift . lift . lift

instance MFunctor Test where
  hoist f =
    Test . hoist (hoist (hoist f)) . unTest

instance PrimMonad m => PrimMonad (Test m) where
  type PrimState (Test m) =
    PrimState m
  primitive =
    lift . primitive

instance MonadIO m => MonadIO (Test m) where
  liftIO =
    lift . liftIO

instance MonadBase b m => MonadBase b (Test m) where
  liftBase =
    lift . liftBase

instance MonadThrow m => MonadThrow (Test m) where
  throwM =
    lift . throwM

instance MonadCatch m => MonadCatch (Test m) where
  catch m onErr =
    Test $
      (unTest m) `catch`
      (unTest . onErr)

instance MonadReader r m => MonadReader r (Test m) where
  ask =
    lift ask
  local f m =
    Test $
      local f (unTest m)

instance MonadState s m => MonadState s (Test m) where
  get =
    lift get
  put =
    lift . put
  state =
    lift . state

-- FIXME instance MonadWriter Test

instance MonadError e m => MonadError e (Test m) where
  throwError =
    lift . throwError
  catchError m onErr =
    Test . ExceptT $
      (runExceptT $ unTest m) `catchError`
      (runExceptT . unTest . onErr)

instance MonadResource m => MonadResource (Test m) where
  liftResourceT =
    lift . liftResourceT

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
    }

-- | Map a config modification function over a property.
--
mapConfig :: (PropertyConfig -> PropertyConfig) -> Property -> Property
mapConfig f (Property cfg t) =
  Property (f cfg) t

-- | Set the number times a property should be executed before it is considered
--   successful.
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

-- | Creates a property to check.
--
property :: Test IO () -> Property
property =
  Property defaultConfig

------------------------------------------------------------------------
-- Test

runTest :: Test m a -> Gen m (Either Failure a, [Log])
runTest =
  runWriterT . runExceptT . unTest

writeLog :: Monad m => Log -> Test m ()
writeLog =
  Test . lift . tell . pure

-- | Generates a random input for the test by running the provided generator.
--
forAll :: (Monad m, Show a, Typeable a, HasCallStack) => Gen m a -> Test m a
forAll gen = do
  x <- Test . lift $ lift gen
  writeLog $ Input (getCaller callStack) (typeOf x) (showPretty x)
  return x

-- | Logs an information message to be displayed if the test fails.
--
info :: Monad m => String -> Test m ()
info =
  writeLog . Info

-- | Discards a test entirely.
--
discard :: Monad m => Test m a
discard =
  Test . lift $ lift Gen.discard

-- | Fail with an error message, useful for building other failure combinators.
--
failWith :: (Monad m, HasCallStack) => Maybe Diff -> String -> Test m a
failWith diff msg =
  Test . ExceptT . pure . Left $ Failure (getCaller callStack) msg diff

-- | Causes a test to fail.
--
failure :: (Monad m, HasCallStack) => Test m a
failure =
  withFrozenCallStack $ failWith Nothing ""

-- | Another name for @pure ()@.
--
success :: Monad m => Test m ()
success =
  Test $ pure ()

-- | Fails the test if the condition provided is 'False'.
--
assert :: (Monad m, HasCallStack) => Bool -> Test m ()
assert b =
  if b then
    success
  else do
    withFrozenCallStack failure

infix 4 ===

-- | Fails the test if the two arguments provided are not equal.
--
(===) :: (Monad m, Eq a, Show a, HasCallStack) => a -> a -> Test m ()
(===) x y =
  if x == y then
    success
  else
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

-- | Fails the test if the 'Either' is 'Left', otherwise returns the value in
--   the 'Right'.
--
liftEither :: (Monad m, Show x, HasCallStack) => Either x a -> Test m a
liftEither = \case
  Left x -> do
    withFrozenCallStack $ failWith Nothing $ showPretty x
  Right x ->
    pure x

-- | Fails the test if the 'ExceptT' is 'Left', otherwise returns the value in
--   the 'Right'.
--
liftExceptT :: (Monad m, Show x, HasCallStack) => ExceptT x m a -> Test m a
liftExceptT m = do
  withFrozenCallStack liftEither =<< lift (runExceptT m)

-- | Run a computation which requires resource acquisition / release.
--
--   /Note that if you 'Control.Monad.Trans.Resource.allocate' anything before/
--   /a 'forAll' you will likely encounter unexpected behaviour, due to the way/
--   /'ResourceT' interacts with the control flow introduced by shrinking./
--
withResourceT :: MonadResourceBase m => Test (ResourceT m) a -> Test m a
withResourceT =
  hoist runResourceT

------------------------------------------------------------------------
-- FIXME Replace with DeriveLift when we drop 7.10 support.

$(deriveLift ''GroupName)
$(deriveLift ''PropertyName)
$(deriveLift ''PropertyConfig)
$(deriveLift ''TestLimit)
$(deriveLift ''ShrinkLimit)
$(deriveLift ''DiscardLimit)

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
