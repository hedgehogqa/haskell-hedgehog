{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hedgehog.Property (
  -- * Property
    Property(..)
  , Log(..)
  , Name(..)
  , forAll
  , info
  , discard
  , failure
  , success
  , assert
  , (===)

  -- * Reporting
  , Report(..)
  , Status(..)
  , ShrinkCount
  , SuccessCount
  , DiscardCount
  , check
  , recheck
  , report

  -- * Internal
  -- $internal
  -- ** Property
  , writeLog
  , runProperty
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..), tell)

import           Data.String (IsString(..))
import           Data.Typeable (Typeable, TypeRep, typeOf)

import           Hedgehog.Gen (Gen, runGen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import           Hedgehog.Range (Size)

import           Text.Show.Pretty (ppShow)

------------------------------------------------------------------------

newtype Property m a =
  Property {
      unProperty :: MaybeT (WriterT [Log] (Gen m)) a
    } deriving (Functor, Applicative, Monad)

data Log =
    ForAll Name TypeRep String
  | Info String
    deriving (Eq, Ord, Show)

newtype Name =
  Name String
  deriving (Eq, Ord, Show)

instance Monad m => MonadPlus (Property m) where
  mzero =
    discard

  mplus x y =
    Property . MaybeT . WriterT $
      mplus (runProperty x) (runProperty y)

instance Monad m => Alternative (Property m) where
  empty =
    mzero
  (<|>) =
    mplus

instance MonadTrans Property where
  lift =
    Property . lift . lift . lift

instance MonadIO m => MonadIO (Property m) where
  liftIO =
    Property . liftIO

instance IsString Name where
  fromString =
    Name

runProperty :: Property m a -> Gen m (Maybe a, [Log])
runProperty =
  runWriterT . runMaybeT . unProperty

writeLog :: Monad m => Log -> Property m ()
writeLog =
  Property . lift . tell . pure

forAll :: (Show a, Typeable a, Monad m) => Name -> Gen m a -> Property m a
forAll name gen = do
  x <- Property . lift $ lift gen
  writeLog $ ForAll name (typeOf x) (ppShow x)
  return x

info :: Monad m => String -> Property m ()
info =
  writeLog . Info

discard :: Monad m => Property m a
discard =
  Property . lift $ lift Gen.discard

failure :: Monad m => Property m a
failure =
  Property . MaybeT $ pure Nothing

success :: Monad m => Property m ()
success =
  Property $ pure ()

assert :: Monad m => Bool -> Property m ()
assert b =
  if b then
    success
  else
    failure

infix 4 ===

(===) :: (Monad m, Eq a, Show a) => a -> a -> Property m ()
(===) x y =
  if x == y then
    success
  else do
    info "=== Not Equal ==="
    info (ppShow x)
    info (ppShow y)
    failure

------------------------------------------------------------------------
-- Reporting

-- | The numbers of times a property was able to shrink after a failing test.
--
newtype ShrinkCount =
  ShrinkCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of tests a property ran successfully.
--
newtype SuccessCount =
  SuccessCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of tests a property had to discard.
--
newtype DiscardCount =
  DiscardCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The status of a property test run.
--
--   In the case of a failure it provides the seed used for the test, the
--   number of shrinks, and the execution log.
--
data Status =
    Failed Size Seed ShrinkCount [Log]
  | GaveUp
  | OK
    deriving (Show)

-- | The report from a property test run.
--
data Report =
  Report {
      reportTests :: SuccessCount
    , reportDiscards :: DiscardCount
    , reportStatus :: Status
    } deriving (Show)

findM :: Monad m => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] ->
      return def
    x0 : xs ->
      p x0 >>= \m ->
        case m of
          Nothing ->
            findM xs def p
          Just x ->
            return x

isFailure :: Node m (Maybe (Maybe a, b)) -> Bool
isFailure = \case
  Node (Just (Nothing, _)) _ ->
    True
  _ ->
    False

takeSmallest :: MonadIO m => Size -> Seed -> ShrinkCount -> Node m (Maybe (Maybe (), [Log])) -> m Status
takeSmallest size seed (ShrinkCount n) = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Nothing -> do
        --liftIO $ putStrLn "*** Shrinking"
        --liftIO . putStr . unlines $ fmap renderLog w
        findM xs (Failed size seed (ShrinkCount n) w) $ \m -> do
          o <- runTree m
          if isFailure o then
            Just <$> takeSmallest size seed (ShrinkCount $ n + 1) o
          else
            return Nothing

      Just () ->
        return OK

report :: forall m. MonadIO m => SuccessCount -> Size -> Seed -> Property m () -> m Report
report n size0 seed0 prop =
  let
    loop :: SuccessCount -> DiscardCount -> Size -> Seed -> m Report
    loop !successes !discards !size !seed =
      if size > 99 then
        loop successes discards 0 seed
      else if successes == n then
        pure $ Report successes discards OK
      else if discards >= 100 then
        pure $ Report successes discards GaveUp
      else
        case Seed.split seed of
          (s0, s1) -> do
            node@(Node x _) <-
              runTree . Gen.runDiscardEffect $ runGen size s0 (runProperty prop)
            case x of
              Nothing ->
                loop successes (discards + 1) (size + 1) s1

              Just (Nothing, _) ->
                Report successes discards <$> takeSmallest size seed (ShrinkCount 0) node

              Just (Just (), _) ->
                loop (successes + 1) discards (size + 1) s1
  in
    loop 0 0 size0 seed0

check :: MonadIO m => Property m () -> m ()
check prop = do
  seed <- liftIO Seed.random
  printReport =<< report 100 0 seed prop

recheck :: MonadIO m => Size -> Seed -> Property m () -> m ()
recheck size seed prop =
  printReport =<< report 1 size seed prop

------------------------------------------------------------------------
-- Rendering

printReport :: MonadIO m => Report -> m ()
printReport (Report successes discards status) =
  case status of
    Failed size seed shrinks msgs -> do
      liftIO . putStr . unlines $
        [ "*** Failed! Falsifiable (after " ++
            renderSuccessCount successes ++
            renderShrinkDiscard shrinks discards ++
            ")"
        ] ++ fmap renderLog msgs ++ [
          ""
        , "This failure can be reproduced by running:"
        , "> recheck " ++
            showsPrec 11 size "" ++
            " " ++
            showsPrec 11 seed "" ++
            " <property>"
        ]
    GaveUp ->
      liftIO . putStrLn $
        "*** Gave up after " ++
        renderDiscardCount discards ++
        ", passed " ++
        renderSuccessCount successes ++
        "."
    OK ->
      liftIO . putStrLn $
        "+++ OK, passed " ++
        renderSuccessCount successes ++
        "."

renderSuccessCount :: SuccessCount -> String
renderSuccessCount = \case
  SuccessCount 1 ->
    "1 successful test"
  SuccessCount n ->
    show n ++ " successful tests"

renderDiscardCount :: DiscardCount -> String
renderDiscardCount = \case
  DiscardCount 1 ->
    "1 discard"
  DiscardCount n ->
    show n ++ " discards"

renderShrinkCount :: ShrinkCount -> String
renderShrinkCount = \case
  ShrinkCount 1 ->
    "1 shrink"
  ShrinkCount n ->
    show n ++ " shrinks"

renderShrinkDiscard :: ShrinkCount -> DiscardCount -> String
renderShrinkDiscard s d =
  case (s, d) of
    (0, 0) ->
      ""
    (0, _) ->
      " and " ++ renderDiscardCount d
    (_, 0) ->
      " and " ++ renderShrinkCount s
    (_, _) ->
      ", " ++ renderShrinkCount s ++ " and " ++ renderDiscardCount d

renderLog :: Log -> String
renderLog = \case
  ForAll (Name name) typ x ->
    name ++ " :: " ++ show typ ++ "\n" ++
    name ++ " =\n" ++
    unlines (fmap ("  " ++) $ lines x)
  Info msg ->
    msg
