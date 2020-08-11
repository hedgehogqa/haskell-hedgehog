{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Hedgehog.Internal.Distributive (
    MonadTransDistributive(..)
  ) where

import           Control.Monad (join)
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Identity (IdentityT(..))
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import           Data.Kind (Type)
import           GHC.Exts (Constraint)

------------------------------------------------------------------------
-- * MonadTransDistributive

class MonadTransDistributive g where
  type Transformer
    (f :: (Type -> Type) -> Type -> Type)
    (g :: (Type -> Type) -> Type -> Type)
    (m :: Type -> Type) :: Constraint

  type Transformer f g m = (
      Monad m
    , Monad (f m)
    , Monad (g m)
    , Monad (f (g m))
    , MonadTrans f
    , MFunctor f
    )

  -- | Distribute one monad transformer over another.
  --
  distributeT :: Transformer f g m => g (f m) a -> f (g m) a

instance MonadTransDistributive IdentityT where
  distributeT m =
    lift . IdentityT . pure =<< hoist lift (runIdentityT m)

instance MonadTransDistributive MaybeT where
  distributeT m =
    lift . MaybeT . pure =<< hoist lift (runMaybeT m)

instance MonadTransDistributive (ExceptT x) where
  distributeT m =
    lift . ExceptT . pure =<< hoist lift (runExceptT m)

instance MonadTransDistributive (ReaderT r) where
  distributeT m =
    join . lift . ReaderT $ \r ->
      pure . hoist lift $ runReaderT m r

instance Monoid w => MonadTransDistributive (Lazy.WriterT w) where
  distributeT m =
    lift . Lazy.WriterT . pure =<< hoist lift (Lazy.runWriterT m)

instance Monoid w => MonadTransDistributive (Strict.WriterT w) where
  distributeT m = do
    lift . Strict.WriterT . pure =<< hoist lift (Strict.runWriterT m)

instance MonadTransDistributive (Lazy.StateT s) where
  distributeT m = do
    s       <- lift Lazy.get
    (a, s') <- hoist lift (Lazy.runStateT m s)
    lift (Lazy.put s')
    return a

instance MonadTransDistributive (Strict.StateT s) where
  distributeT m = do
    s       <- lift Strict.get
    (a, s') <- hoist lift (Strict.runStateT m s)
    lift (Strict.put s')
    return a

instance Monoid w => MonadTransDistributive (Lazy.RWST r w s) where
  distributeT m = do
    -- ask and get combined
    (r, s0)    <- lift . Lazy.RWST $ \r s -> return ((r, s), s, mempty)
    (a, s1, w) <- hoist lift (Lazy.runRWST m r s0)
    -- tell and put combined
    lift $ Lazy.RWST $ \_ _ -> return (a, s1, w)

instance Monoid w => MonadTransDistributive (Strict.RWST r w s) where
  distributeT m = do
    -- ask and get combined
    (r, s0)    <- lift . Strict.RWST $ \r s -> return ((r, s), s, mempty)
    (a, s1, w) <- hoist lift (Strict.runRWST m r s0)
    -- tell and put combined
    lift $ Strict.RWST $ \_ _ -> return (a, s1, w)
