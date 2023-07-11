{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
  , TestCount(..)
  , DiscardLimit(..)
  , DiscardCount(..)
  , ShrinkLimit(..)
  , ShrinkCount(..)
  , Skip(..)
  , ShrinkPath(..)
  , ShrinkRetries(..)
  , withTests
  , withDiscards
  , withShrinks
  , withRetries
  , withSkip
  , property
  , test
  , forAll
  , forAllT
  , forAllWith
  , forAllWithT
  , defaultMinTests
  , discard
  , skipCompress
  , shrinkPathCompress
  , skipDecompress
  , shrinkPathDecompress

  -- * Group
  , Group(..)
  , GroupName(..)
  , PropertyCount(..)

  -- * TestT
  , MonadTest(..)
  , Test
  , TestT(..)
  , Log(..)
  , Journal(..)
  , Failure(..)
  , Diff(..)
  , annotate
  , annotateShow
  , footnote
  , footnoteShow
  , failure
  , success
  , assert
  , diff
  , (===)
  , (/==)

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
  , Coverage(..)
  , Label(..)
  , LabelName(..)
  , cover
  , classify
  , label
  , collect
  , coverPercentage
  , labelCovered
  , coverageSuccess
  , coverageFailures
  , journalCoverage

  , Cover(..)
  , CoverCount(..)
  , CoverPercentage(..)
  , toCoverCount

  -- * Confidence
  , Confidence(..)
  , TerminationCriteria(..)
  , confidenceSuccess
  , confidenceFailure
  , withConfidence
  , verifiedTermination
  , defaultConfidence

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

  , wilsonBounds
  ) where

import           Control.Applicative (Alternative(..))
import           Control.DeepSeq (NFData, rnf)
import           Control.Exception.Safe (MonadThrow, MonadCatch)
import           Control.Exception.Safe (SomeException(..), displayException)
import           Control.Monad (MonadPlus(..), (<=<))
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Error.Class (MonadError(..))
import qualified Control.Monad.Fail as Fail
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
import           Data.Functor (($>))
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Number.Erf (invnormcdf)
import qualified Data.List as List
import           Data.String (IsString(..))
import           Data.Ratio ((%))
import           Data.Typeable (typeOf)

import           Hedgehog.Internal.Distributive
import           Hedgehog.Internal.Exception
import           Hedgehog.Internal.Gen (Gen, GenT)
import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.Prelude
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source

import           Language.Haskell.TH.Syntax (Lift)

import qualified Numeric

import           Text.Read (readMaybe)

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
-- NOTE: Move this to the deriving list above when we drop 7.10
deriving instance MonadResource m => MonadResource (PropertyT m)

-- NOTE: Move this to the deriving list above when we drop 8.0
#if __GLASGOW_HASKELL__ >= 802
deriving instance MonadBaseControl b m => MonadBaseControl b (PropertyT m)
#else
instance MonadBaseControl b m => MonadBaseControl b (PropertyT m) where
  type StM (PropertyT m) a = StM (TestT (GenT m)) a
  liftBaseWith f = PropertyT $ liftBaseWith $ \rib -> f (rib . unPropertyT)
  restoreM = PropertyT . restoreM
#endif

-- | A test monad allows the assertion of expectations.
--
type Test =
  TestT Identity

-- | A test monad transformer allows the assertion of expectations.
--
newtype TestT m a =
  TestT {
      unTest :: ExceptT Failure (Lazy.WriterT Journal m) a
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
--   Should be constructed using `OverloadedStrings`:
--
-- @
--   "apples" :: PropertyName
-- @
--
newtype PropertyName =
  PropertyName {
      unPropertyName :: String
    } deriving (Eq, Ord, Show, IsString, Semigroup, Lift)

-- | The acceptable occurrence of false positives
--
--   Example, @Confidence 10^9@ would mean that you'd accept a false positive
--   for 1 in 10^9 tests.
newtype Confidence =
  Confidence {
    unConfidence :: Int64
  } deriving (Eq, Ord, Show, Num, Lift)

-- | Configuration for a property test.
--
data PropertyConfig =
  PropertyConfig {
      propertyDiscardLimit :: !DiscardLimit
    , propertyShrinkLimit :: !ShrinkLimit
    , propertyShrinkRetries :: !ShrinkRetries
    , propertyTerminationCriteria :: !TerminationCriteria

    -- | If this is 'Nothing', we take the Skip from the environment variable
    --   @HEDGEHOG_SKIP@.
    , propertySkip :: Maybe Skip
    } deriving (Eq, Ord, Show, Lift)

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
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

-- | The number of tests a property ran successfully.
--
newtype TestCount =
  TestCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

-- | The number of tests a property had to discard.
--
newtype DiscardCount =
  DiscardCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

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
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

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
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

-- | The numbers of times a property was able to shrink after a failing test.
--
newtype ShrinkCount =
  ShrinkCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Where to start running a property's tests.
--
data Skip =
  -- | Don't skip anything.
  --
    SkipNothing

  -- | Skip to a specific test number. If it fails, shrink as normal. If it
  --   passes, move on to the next test. Coverage checks are disabled.
  --
  --   We also need to count discards, since failing "after 7 tests" points at a
  --   different generated value than failing "after 7 tests and 5 discards".
  --
  | SkipToTest TestCount DiscardCount

  -- | Skip to a specific test number and shrink state. If it fails, stop
  --   without shrinking further. If it passes, the property will pass without
  --   running any more tests.
  --
  --   Due to implementation details, all intermediate shrink states - those on
  --   the direct path from the original test input to the target state - will
  --   be tested too, and their results discarded.
  --
  | SkipToShrink TestCount DiscardCount ShrinkPath
  deriving (Eq, Ord, Show, Lift)

-- | We use this instance to support usage like
--
-- @
--   withSkip "3:aB"
-- @
--
--   It throws an error if the input is not a valid compressed 'Skip'.
--
instance IsString Skip where
  fromString s =
    case skipDecompress s of
      Nothing ->
        error $ "fromString: Not a valid Skip: " ++ s
      Just skip ->
        skip

-- | The path taken to reach a shrink state.
--
newtype ShrinkPath =
  ShrinkPath [Int]
  deriving (Eq, Ord, Show, Lift)

-- | Compress a Skip into a hopefully-short alphanumeric string.
--
--   The bit that might be long is the 'ShrinkPath' in 'SkipToShrink'. For that,
--   we encode the path components in base 26, alternating between uppercase and
--   lowercase alphabets to distinguish list elements. Additionally when we have
--   runs of equal components, we use the normal base 10 encoding to indicate
--   the length.
--
--   This gives something which is hopefully quite short, but a human can
--   roughly interpret it by eyeball.
--
skipCompress :: Skip -> String
skipCompress =
  let
    showTD (TestCount t) (DiscardCount d) =
      show t ++ (if d == 0 then "" else "/" ++ show d)
  in \case
    SkipNothing ->
      ""
    SkipToTest t d->
      showTD t d
    SkipToShrink t d sp ->
      showTD t d ++ ":" ++ shrinkPathCompress sp

-- | Compress a 'ShrinkPath' into a hopefully-short alphanumeric string.
--
--   We encode the path components in base 26, alternating between uppercase and
--   lowercase alphabets to distinguish list elements. Additionally when we have
--   runs of equal components, we use the normal base 10 encoding to indicate
--   the length.
shrinkPathCompress :: ShrinkPath -> String
shrinkPathCompress (ShrinkPath sp) =
  let
    groups = List.map (\l -> (head l, length l)) $ List.group sp
  in
    (mconcat
      $ zipWith
          (\alphabet (loc, count) ->
              Numeric.showIntAtBase 26 (alphabet !!) loc
              <> if count == 1 then mempty else shows count
          )
          (cycle [['a'..'z'], ['A'..'Z']])
          groups
    )
      ""

-- | Decompress a 'Skip'.
--
--   This satisfies
--
-- @
--   skipDecompress (skipCompress a) == Just a
-- @
--
skipDecompress :: String -> Maybe Skip
skipDecompress str =
  if null str then
    Just SkipNothing
  else do
    let
      (tcDcStr, spStr)
        = span (/= ':') str

      (tcStr, dcStr)
        = span (/= '/') tcDcStr

    tc <- TestCount <$> readMaybe tcStr
    dc <- DiscardCount <$> if null dcStr
      then Just 0
      else readMaybe (drop 1 dcStr)

    if null spStr then
      Just $ SkipToTest tc dc
    else do
      sp <- shrinkPathDecompress $ drop 1 spStr
      Just $ SkipToShrink tc dc sp

-- | Decompress a 'ShrinkPath'.
--
--   This satisfies
--
-- @
--   shrinkPathDecompress (shrinkPathCompress a) == Just a
-- @
shrinkPathDecompress :: String -> Maybe ShrinkPath
shrinkPathDecompress str =
  let
    isDigit c = '0' <= c && c <= '9'
    isLower c = 'a' <= c && c <= 'z'
    isUpper c = 'A' <= c && c <= 'Z'
    classifyChar c = (isDigit c, isLower c, isUpper c)

    readSNum "" = []
    readSNum s@(c1:_) =
      if isDigit c1 then
        Numeric.readInt 10 isDigit (\c -> fromEnum c - fromEnum '0') s
      else if isLower c1 then
        Numeric.readInt 26 isLower (\c -> fromEnum c - fromEnum 'a') s
      else if isUpper c1 then
        Numeric.readInt 26 isUpper (\c -> fromEnum c - fromEnum 'A') s
      else
        []

    readNumMaybe s =
      case readSNum s of
        [(num, "")] -> Just num
        _ -> Nothing

    spGroups :: [(Maybe Int, Maybe Int)] =
      let
        go [] =
          []
        go (c1:cs) =
          let
            (hd, tl1) =
              span (\c -> classifyChar c == classifyChar c1) cs
            (digs, tl2) =
              span isDigit tl1
          in
            ( readNumMaybe (c1:hd)
            , readNumMaybe $ if null digs then "1" else digs
            )
            : go tl2
      in
        go str
  in do
    sp <- concat <$>
      traverse (\(mNum, mCount) -> replicate <$> mCount <*> mNum) spGroups
    Just $ ShrinkPath sp

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
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Lift)

-- | A named collection of property tests.
--
data Group =
  Group {
      groupName :: !GroupName
    , groupProperties :: ![(PropertyName, Property)]
    }

-- | The name of a group of properties.
--
--   Should be constructed using `OverloadedStrings`:
--
-- @
--   "fruit" :: GroupName
-- @
--
newtype GroupName =
  GroupName {
      unGroupName :: String
    } deriving (Eq, Ord, Show, IsString, Semigroup, Lift)

-- | The number of properties in a group.
--
newtype PropertyCount =
  PropertyCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data TerminationCriteria =
    EarlyTermination Confidence TestLimit
  | NoEarlyTermination Confidence TestLimit
  | NoConfidenceTermination TestLimit
  deriving (Eq, Ord, Show, Lift)

--
-- FIXME This whole Log/Failure thing could be a lot more structured to allow
-- FIXME for richer user controlled error messages, think Doc. Ideally we'd
-- FIXME allow user's to crete their own diffs anywhere.
--

-- | Log messages which are recorded during a test run.
--
data Log =
    Annotation (Maybe Span) String
  | Footnote String
  | Label (Label Cover)
    deriving (Eq, Show)

-- | A record containing the details of a test run.
newtype Journal =
  Journal {
      journalLogs :: [Log]
    } deriving (Eq, Show, Semigroup, Monoid)

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

-- | Whether a test is covered by a classifier, and therefore belongs to a
--   'Class'.
--
data Cover =
    NoCover
  | Cover
    deriving (Eq, Ord, Show)

-- | The total number of tests which are covered by a classifier.
--
--   Can be constructed using numeric literals:
--
-- @
--   30 :: CoverCount
-- @
--
newtype CoverCount =
  CoverCount {
      unCoverCount :: Int
    } deriving (Eq, Ord, Show, Num)

-- | The relative number of tests which are covered by a classifier.
--
--   Can be constructed using numeric literals:
--
-- @
--   30 :: CoverPercentage
-- @
--
newtype CoverPercentage =
  CoverPercentage {
      unCoverPercentage :: Double
    } deriving (Eq, Ord, Show, Num, Fractional)

-- | The name of a classifier.
--
--   Should be constructed using `OverloadedStrings`:
--
-- @
--   "apples" :: LabelName
-- @
--
newtype LabelName =
  LabelName {
      unLabelName :: String
    } deriving (Eq, Monoid, Ord, Semigroup, Show, IsString)

-- | The extent to which a test is covered by a classifier.
--
--   /When a classifier's coverage does not exceed the required minimum, the/
--   /test will be failed./
--
data Label a =
  MkLabel {
      labelName :: !LabelName
    , labelLocation :: !(Maybe Span)
    , labelMinimum :: !CoverPercentage
    , labelAnnotation :: !a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

-- | The extent to which all classifiers cover a test.
--
--   /When a given classification's coverage does not exceed the required/
--   /minimum, the test will be failed./
--
newtype Coverage a =
  Coverage {
      coverageLabels :: Map LabelName (Label a)
    } deriving (Eq, Show, Functor, Foldable, Traversable)

------------------------------------------------------------------------
-- TestT

instance Monad m => Monad (TestT m) where
  return =
    pure

  (>>=) m k =
    TestT $
      unTest m >>=
      unTest . k

instance Monad m => MonadFail (TestT m) where
  fail err =
    TestT . ExceptT . pure . Left $ Failure Nothing err Nothing

instance MonadTrans TestT where
  lift =
    TestT . lift . lift

instance MFunctor TestT where
  hoist f =
    TestT . hoist (hoist f) . unTest

instance MonadTransDistributive TestT where
  type Transformer t TestT m = (
      Transformer t (Lazy.WriterT Journal) m
    , Transformer t (ExceptT Failure) (Lazy.WriterT Journal m)
    )

  distributeT =
    hoist TestT .
    distributeT .
    hoist distributeT .
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
    (Either Failure a, Journal)

  liftWith f =
    mkTestT . fmap (, mempty) . fmap Right $ f $ runTestT

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

mkTestT :: m (Either Failure a, Journal) -> TestT m a
mkTestT =
  TestT . ExceptT . Lazy.WriterT

mkTest :: (Either Failure a, Journal) -> Test a
mkTest =
  mkTestT . Identity

runTestT :: TestT m a -> m (Either Failure a, Journal)
runTestT =
  Lazy.runWriterT . runExceptT . unTest

runTest :: Test a -> (Either Failure a, Journal)
runTest =
  runIdentity . runTestT

-- | Log some information which might be relevant to a potential test failure.
--
writeLog :: MonadTest m => Log -> m ()
writeLog x =
  liftTest $ mkTest (pure (), (Journal [x]))

-- | Fail the test with an error message, useful for building other failure
--   combinators.
--
failWith :: (MonadTest m, HasCallStack) => Maybe Diff -> String -> m a
failWith mdiff msg =
  liftTest $ mkTest (Left $ Failure (getCaller callStack) msg mdiff, mempty)

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

-- | Fails with an error that shows the difference between two values.
failDiff :: (MonadTest m, Show a, Show b, HasCallStack) => a -> b -> m ()
failDiff x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      withFrozenCallStack $
        failWith Nothing $
        unlines $ [
            "Failed"
          , "━━ lhs ━━"
          , showPretty x
          , "━━ rhs ━━"
          , showPretty y
          ]

    Just vdiff@(ValueSame _) ->
      withFrozenCallStack $
        failWith (Just $
          Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff) ""

    Just vdiff ->
      withFrozenCallStack $
        failWith (Just $
          Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff) ""

-- | Fails with an error which renders the type of an exception and its error
--   message.
--
failException :: (MonadTest m, HasCallStack) => SomeException -> m a
failException x =
  withFrozenCallStack $
    failExceptionWith [] x

-- | Fails with an error which renders the given messages, the type of an exception,
--   and its error message.
--
failExceptionWith :: (MonadTest m, HasCallStack) => [String] -> SomeException -> m a
failExceptionWith messages (SomeException x) =
  withFrozenCallStack
    failWith Nothing $ unlines $ messages <> [
        "━━━ Exception (" ++ show (typeOf x) ++ ") ━━━"
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

-- | Fails the test and shows a git-like diff if the comparison operation
--   evaluates to 'False' when applied to its arguments.
--
--   The comparison function is the second argument, which may be
--   counter-intuitive to Haskell programmers. However, it allows operators to
--   be written infix for easy reading:
--
-- @
--   diff y (<) 87
--   diff x (<=) 'r'
-- @
--
--   This function behaves like the unix @diff@ tool, which gives a 0 exit
--   code if the compared files are identical, or a 1 exit code code
--   otherwise. Like unix @diff@, if the arguments fail the comparison, a
--   /diff is shown.
--
diff :: (MonadTest m, Show a, Show b, HasCallStack) => a -> (a -> b -> Bool) -> b -> m ()
diff x op y = do
  ok <- withFrozenCallStack $ eval (x `op` y)
  if ok then
    success
  else
    withFrozenCallStack $ failDiff x y

infix 4 ===

-- | Fails the test if the two arguments provided are not equal.
--
(===) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(===) x y =
  withFrozenCallStack $
    diff x (==) y

infix 4 /==

-- | Fails the test if the two arguments provided are equal.
--
(/==) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(/==) x y =
  withFrozenCallStack $
    diff x (/=) y

-- | Fails the test if the value throws an exception when evaluated to weak
--   head normal form (WHNF).
--
eval :: (MonadTest m, HasCallStack) => a -> m a
eval x =
  either (withFrozenCallStack failException) pure (tryEvaluate x)

-- | Fails the test if the value throws an exception when evaluated to
--   normal form (NF).
--
evalNF :: (MonadTest m, NFData a, HasCallStack) => a -> m a
evalNF x =
  let
    messages =
      ["━━━ Value could not be evaluated to normal form ━━━"]
  in
    either (withFrozenCallStack (failExceptionWith messages)) pure (tryEvaluate (rnf x)) $> x

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

-- | Fails the test if the action throws an exception, or if the
--   'Either' is 'Left', otherwise returns the value in the 'Right'.
--
evalEitherM :: (MonadTest m, Show x, MonadCatch m, HasCallStack) => m (Either x a) -> m a
evalEitherM =
  withFrozenCallStack $ evalEither <=< evalM

-- | Fails the test if the 'ExceptT' is 'Left', otherwise returns the value in
--   the 'Right'.
--
evalExceptT :: (MonadTest m, Show x, HasCallStack) => ExceptT x m a -> m a
evalExceptT m =
  withFrozenCallStack evalEither =<< runExceptT m

-- | Fails the test if the 'Maybe' is 'Nothing', otherwise returns the value in
--   the 'Just'.
--
evalMaybe :: (MonadTest m, HasCallStack) => Maybe a -> m a
evalMaybe = \case
  Nothing ->
    withFrozenCallStack $ failWith Nothing "the value was Nothing"
  Just x ->
    pure x

-- | Fails the test if the action throws an exception, or if the
--   'Maybe' is 'Nothing', otherwise returns the value in the 'Just'.
--
evalMaybeM :: (MonadTest m, MonadCatch m, HasCallStack) => m (Maybe a) -> m a
evalMaybeM =
  withFrozenCallStack $ evalMaybe <=< evalM

------------------------------------------------------------------------
-- PropertyT

instance MonadTrans PropertyT where
  lift =
    PropertyT . lift . lift

instance Monad m => MonadFail (PropertyT m) where
  fail err =
    PropertyT (Fail.fail err)

instance MFunctor PropertyT where
  hoist f =
    PropertyT . hoist (hoist f) . unPropertyT

instance MonadTransDistributive PropertyT where
  type Transformer t PropertyT m = (
      Transformer t GenT m
    , Transformer t TestT (GenT m)
    )

  distributeT =
    hoist PropertyT .
    distributeT .
    hoist distributeT .
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
  withFrozenCallStack $ forAllWithT render $ Gen.generalize gen

-- | Generates a random input for the test by running the provided generator.
--
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
  PropertyT $ lift (Gen.generalize Gen.discard)

-- | Lift a test in to a property.
--
--   Because both 'TestT' and 'PropertyT' have 'MonadTest' instances, this
--   function is not often required. It can however be useful for writing
--   functions directly in 'TestT' and thus gaining a 'MonadTransControl'
--   instance at the expense of not being able to generate additional inputs
--   using 'forAll'.
--
--   An example where this is useful is parallel state machine testing, as
--   'Hedgehog.Internal.State.executeParallel' requires 'MonadBaseControl' 'IO'
--   in order to be able to spawn threads in 'MonadTest'.
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
      propertyDiscardLimit =
        100
    , propertyShrinkLimit =
        1000
    , propertyShrinkRetries =
        0
    , propertyTerminationCriteria =
        NoConfidenceTermination defaultMinTests
    , propertySkip =
        Nothing
    }

-- | The minimum amount of tests to run for a 'Property'
--
defaultMinTests :: TestLimit
defaultMinTests = 100

-- | The default confidence allows one false positive in 10^9 tests
--
defaultConfidence :: Confidence
defaultConfidence = 10 ^ (9 :: Int)

-- | Map a config modification function over a property.
--
mapConfig :: (PropertyConfig -> PropertyConfig) -> Property -> Property
mapConfig f (Property cfg t) =
  Property (f cfg) t

-- | Make sure that the result is statistically significant in accordance to
--   the passed 'Confidence'
--
withConfidence :: Confidence -> Property -> Property
withConfidence c =
  let
    setConfidence = \case
      NoEarlyTermination _ tests -> NoEarlyTermination c tests
      NoConfidenceTermination tests -> NoEarlyTermination c tests
      EarlyTermination _ tests -> EarlyTermination c tests
  in
    mapConfig $ \config@PropertyConfig{..} ->
      config
        { propertyTerminationCriteria =
            setConfidence propertyTerminationCriteria
        }

verifiedTermination :: Property -> Property
verifiedTermination =
  mapConfig $ \config@PropertyConfig{..} ->
    let
      newTerminationCriteria = case propertyTerminationCriteria of
        NoEarlyTermination c tests -> EarlyTermination c tests
        NoConfidenceTermination tests -> EarlyTermination defaultConfidence tests
        EarlyTermination c tests -> EarlyTermination c tests
    in
      config { propertyTerminationCriteria = newTerminationCriteria }

-- | Set the number of times a property should be executed before it is considered
--   successful.
--
--   If you have a test that does not involve any generators and thus does not
--   need to run repeatedly, you can use @withTests 1@ to define a property that
--   will only be checked once.
--
withTests :: TestLimit -> Property -> Property
withTests n =
  let
    setTestLimit tests = \case
      NoEarlyTermination c _ -> NoEarlyTermination c tests
      NoConfidenceTermination _ -> NoConfidenceTermination tests
      EarlyTermination c _ -> EarlyTermination c tests
  in
    mapConfig $ \config@PropertyConfig{..} ->
      config { propertyTerminationCriteria = setTestLimit n propertyTerminationCriteria }

-- | Set the number of times a property is allowed to discard before the test
--   runner gives up.
--
withDiscards :: DiscardLimit -> Property -> Property
withDiscards n =
  mapConfig $ \config -> config { propertyDiscardLimit = n }

-- | Set the number of times a property is allowed to shrink before the test
--   runner gives up and prints the counterexample.
--
withShrinks :: ShrinkLimit -> Property -> Property
withShrinks n =
  mapConfig $ \config -> config { propertyShrinkLimit = n }

-- | Set the number of times a property will be executed for each shrink before
--   the test runner gives up and tries a different shrink. See 'ShrinkRetries'
--   for more information.
--
withRetries :: ShrinkRetries -> Property -> Property
withRetries n =
  mapConfig $ \config -> config { propertyShrinkRetries = n }

-- | Set the target that a property will skip to before it starts to run.
--
withSkip :: Skip -> Property -> Property
withSkip s =
  mapConfig $ \config -> config { propertySkip = Just s }

-- | Creates a property with the default configuration.
--
property :: HasCallStack => PropertyT IO () -> Property
property m =
  Property defaultConfig $
    withFrozenCallStack (evalM m)

------------------------------------------------------------------------
-- Coverage

instance Semigroup Cover where
  (<>) NoCover NoCover =
    NoCover
  (<>) _ _ =
    Cover

instance Monoid Cover where
  mempty =
    NoCover
  mappend =
    (<>)

instance Semigroup CoverCount where
  (<>) (CoverCount n0) (CoverCount n1) =
    CoverCount (n0 + n1)

instance Monoid CoverCount where
  mempty =
    CoverCount 0
  mappend =
    (<>)

toCoverCount :: Cover -> CoverCount
toCoverCount = \case
  NoCover ->
    CoverCount 0
  Cover ->
    CoverCount 1

-- | This semigroup is right biased. The name, location and percentage from the
--   rightmost `Label` will be kept. This shouldn't be a problem since the
--   library doesn't allow setting multiple classes with the same 'ClassifierName'.
instance Semigroup a => Semigroup (Label a) where
  (<>) (MkLabel _ _ _ m0) (MkLabel name location percentage m1) =
    MkLabel name location percentage (m0 <> m1)

instance Semigroup a => Semigroup (Coverage a) where
  (<>) (Coverage c0) (Coverage c1) =
    Coverage $
      Map.foldrWithKey (Map.insertWith (<>)) c0 c1

instance (Semigroup a, Monoid a) => Monoid (Coverage a) where
  mempty =
    Coverage mempty
  mappend =
    (<>)

coverPercentage :: TestCount -> CoverCount -> CoverPercentage
coverPercentage (TestCount tests) (CoverCount count) =
  let
    percentage :: Double
    percentage =
      fromIntegral count / fromIntegral tests * 100

    thousandths :: Int
    thousandths =
      round $ percentage * 10
  in
    CoverPercentage (fromIntegral thousandths / 10)

labelCovered :: TestCount -> Label CoverCount -> Bool
labelCovered tests (MkLabel _ _ minimum_ population) =
  coverPercentage tests population >= minimum_

-- | All labels are covered
coverageSuccess :: TestCount -> Coverage CoverCount -> Bool
coverageSuccess tests =
  null . coverageFailures tests

coverageFailures :: TestCount -> Coverage CoverCount -> [Label CoverCount]
coverageFailures tests (Coverage kvs) =
  List.filter (not . labelCovered tests) (Map.elems kvs)

-- | Is true when the test coverage satisfies the specified 'Confidence'
--   contstraint for all 'Coverage CoverCount's
confidenceSuccess :: TestCount -> Confidence -> Coverage CoverCount -> Bool
confidenceSuccess tests confidence =
  let
    assertLow :: Label CoverCount -> Bool
    assertLow coverCount@MkLabel{..} =
      fst (boundsForLabel tests confidence coverCount)
        >= unCoverPercentage labelMinimum / 100.0
  in
    and . fmap assertLow . Map.elems . coverageLabels

-- | Is true when there exists a label that is sure to have failed according to
--   the 'Confidence' constraint
confidenceFailure :: TestCount -> Confidence -> Coverage CoverCount -> Bool
confidenceFailure tests confidence =
  let
    assertHigh :: Label CoverCount -> Bool
    assertHigh coverCount@MkLabel{..} =
      snd (boundsForLabel tests confidence coverCount)
        < (unCoverPercentage labelMinimum / 100.0)
  in
    or . fmap assertHigh . Map.elems . coverageLabels

boundsForLabel :: TestCount -> Confidence -> Label CoverCount -> (Double, Double)
boundsForLabel tests confidence MkLabel{..} =
  wilsonBounds
    (fromIntegral $ unCoverCount labelAnnotation)
    (fromIntegral tests)
    (1 / fromIntegral (unConfidence confidence))

-- In order to get an accurate measurement with small sample sizes, we're
-- using the Wilson score interval
-- (<https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval
-- wikipedia>) instead of a normal approximation interval.
wilsonBounds :: Integer -> Integer -> Double -> (Double, Double)
wilsonBounds positives count acceptance =
  let
    p =
      fromRational $ positives % count
    n =
      fromIntegral count
    z =
      invnormcdf $ 1 - acceptance / 2

    midpoint =
      p + z * z / (2 * n)

    offset =
      z / (1 + z ** 2 / n) * sqrt (p * (1 - p) / n + z ** 2 / (4 * n ** 2))

    denominator =
      1 + z * z / n

    low =
      (midpoint - offset) / denominator

    high =
      (midpoint + offset) / denominator
  in
    (low, high)

fromLabel :: Label a -> Coverage a
fromLabel x =
  Coverage $
    Map.singleton (labelName x) x

unionsCoverage :: Semigroup a => [Coverage a] -> Coverage a
unionsCoverage =
  Coverage .
  Map.unionsWith (<>) .
  fmap coverageLabels

journalCoverage :: Journal -> Coverage CoverCount
journalCoverage (Journal logs) =
  fmap toCoverCount .
  unionsCoverage $ do
    Label x <- logs
    pure (fromLabel x)

-- | Require a certain percentage of the tests to be covered by the
--   classifier.
--
-- @
--    prop_with_coverage :: Property
--    prop_with_coverage =
--      property $ do
--        match <- forAll Gen.bool
--        cover 30 \"True\" $ match
--        cover 30 \"False\" $ not match
-- @
--
--   The example above requires a minimum of 30% coverage for both
--   classifiers. If these requirements are not met, it will fail the test.
--
cover :: (MonadTest m, HasCallStack) => CoverPercentage -> LabelName -> Bool -> m ()
cover minimum_ name covered =
  let
    cover_ =
      if covered then
        Cover
      else
        NoCover
  in
    writeLog . Label $
      MkLabel name (getCaller callStack) minimum_ cover_

-- | Records the proportion of tests which satisfy a given condition.
--
-- @
--    prop_with_classifier :: Property
--    prop_with_classifier =
--      property $ do
--        xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--        for_ xs $ \\x -> do
--          classify "newborns" $ x == 0
--          classify "children" $ x > 0 && x < 13
--          classify "teens" $ x > 12 && x < 20
-- @
classify :: (MonadTest m, HasCallStack) => LabelName -> Bool -> m ()
classify name covered =
  withFrozenCallStack $
    cover 0 name covered

-- | Add a label for each test run. It produces a table showing the percentage
--   of test runs that produced each label.
--
label :: (MonadTest m, HasCallStack) => LabelName -> m ()
label name =
  withFrozenCallStack $
    cover 0 name True

-- | Like 'label', but uses 'Show' to render its argument for display.
--
collect :: (MonadTest m, Show a, HasCallStack) => a -> m ()
collect x =
  withFrozenCallStack $
    cover 0 (LabelName (show x)) True

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
