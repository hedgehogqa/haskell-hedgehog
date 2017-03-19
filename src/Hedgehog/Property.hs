{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hedgehog.Property (
  -- * Property
    Property(..)
  , Log(..)
  , Failure(..)
  , forAll
  , info
  , discard
  , failure
  , success
  , ensure
  , (===)

  -- * Internal
  -- $internal
  , writeLog
  , runProperty
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..), tell)

import           Data.Typeable (Typeable, TypeRep, typeOf)

import           Hedgehog.Gen (Gen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Source

import           Text.Show.Pretty (ppShow)

------------------------------------------------------------------------

newtype Property m a =
  Property {
      unProperty :: ExceptT Failure (WriterT [Log] (Gen m)) a
    } deriving (Functor, Applicative, Monad)

data Log =
    Info String
  | Input (Maybe Span) TypeRep String
    deriving (Eq, Show)

data Failure =
  Failure (Maybe Span)
  deriving (Eq, Ord, Show)

instance Monad m => MonadPlus (Property m) where
  mzero =
    discard

  mplus x y =
    Property . ExceptT . WriterT $
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

runProperty :: Property m a -> Gen m (Either Failure a, [Log])
runProperty =
  runWriterT . runExceptT . unProperty

writeLog :: Monad m => Log -> Property m ()
writeLog =
  Property . lift . tell . pure

forAll :: (Monad m, Show a, Typeable a, HasCallStack) => Gen m a -> Property m a
forAll gen = do
  x <- Property . lift $ lift gen
  writeLog $ Input (getCaller callStack) (typeOf x) (ppShow x)
  return x

info :: Monad m => String -> Property m ()
info =
  writeLog . Info

discard :: Monad m => Property m a
discard =
  Property . lift $ lift Gen.discard

failure :: (Monad m, HasCallStack) => Property m a
failure =
  Property . ExceptT . pure . Left . Failure $ getCaller callStack

success :: Monad m => Property m ()
success =
  Property $ pure ()

ensure :: (Monad m, HasCallStack) => Bool -> Property m ()
ensure b =
  if b then
    success
  else do
    withFrozenCallStack failure

infix 4 ===

(===) :: (Monad m, Eq a, Show a, HasCallStack) => a -> a -> Property m ()
(===) x y =
  if x == y then
    success
  else do
    info "━━━ Not Equal ━━━"
    info (ppShow x)
    info (ppShow y)
    withFrozenCallStack failure
