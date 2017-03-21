{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
module Hedgehog.Property (
  -- * Property
    Property(..)
  , Log(..)
  , Failure(..)
  , given
  , info
  , discard
  , failure
  , success
  , assert
  , (===)

  -- * Internal
  -- $internal
  , writeLog
  , runProperty
  ) where

import           Control.Monad.Trans.Cont (ContT)

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
import           Control.Monad.Trans.Resource (MonadResource(..))
import           Control.Monad.Trans.Writer.Lazy (WriterT(..))
import           Control.Monad.Writer.Class (MonadWriter(..))

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

instance MFunctor Property where
  hoist f =
    Property . hoist (hoist (hoist f)) . unProperty

instance PrimMonad m => PrimMonad (Property m) where
  type PrimState (Property m) =
    PrimState m
  primitive =
    lift . primitive

instance MonadIO m => MonadIO (Property m) where
  liftIO =
    lift . liftIO

instance MonadBase b m => MonadBase b (Property m) where
  liftBase =
    lift . liftBase

instance MonadThrow m => MonadThrow (Property m) where
  throwM =
    lift . throwM

instance MonadCatch m => MonadCatch (Property m) where
  catch m onErr =
    Property $
      (unProperty m) `catch`
      (unProperty . onErr)

instance MonadReader r m => MonadReader r (Property m) where
  ask =
    lift ask
  local f m =
    Property $
      local f (unProperty m)

instance MonadState s m => MonadState s (Property m) where
  get =
    lift get
  put =
    lift . put
  state =
    lift . state

-- FIXME Implement MonadWriter

instance MonadError e m => MonadError e (Property m) where
  throwError =
    lift . throwError
  catchError m onErr =
    Property . ExceptT $
      (runExceptT $ unProperty m) `catchError`
      (runExceptT . unProperty . onErr)

instance MonadResource m => MonadResource (Property m) where
  liftResourceT =
    lift . liftResourceT

------------------------------------------------------------------------

runProperty :: Property m a -> Gen m (Either Failure a, [Log])
runProperty =
  runWriterT . runExceptT . unProperty

writeLog :: Monad m => Log -> Property m ()
writeLog =
  Property . lift . tell . pure

given :: (Monad m, Show a, Typeable a, HasCallStack) => Gen m a -> Property m a
given gen = do
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

assert :: (Monad m, HasCallStack) => Bool -> Property m ()
assert b =
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
