{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Hedgehog.Internal.Distributive (
    MonadTransDistributive(..)
  , MonadTransJuggle(..)
  ) where

import           Control.Monad (join)
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control (MonadTransControl(..))
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

import           Data.Proxy (Proxy(..))
import           Data.Bifunctor (Bifunctor(..))

import           GHC.Exts (Constraint)

-- NOTE: Replace use of Proxy with TypeApplications when we drop 7.10 support.

------------------------------------------------------------------------
-- * MonadTransDistributive

class MonadTransDistributive g where
  type Transformer
    (f :: (* -> *) -> * -> *)
    (g :: (* -> *) -> * -> *)
    (m :: * -> *) :: Constraint

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
  type Transformer f (Lazy.StateT s) m = (
      Monad m
    , Monad (f m)
    , Monad (Lazy.StateT s m)
    , Monad (f (Lazy.StateT s m))
    , MonadTrans f
    , MonadTransControl f
    , MonadTransJuggle f
    , MFunctor f
    )

  distributeT :: forall f m a.
       Transformer f (Lazy.StateT s) m
    => Lazy.StateT s (f m) a
    -> f (Lazy.StateT s m) a
  distributeT m =
    join $ liftWith $ \run ->
      let
        restoreStateT :: s -> StT f (a, s) -> (f (Lazy.StateT s m) a, s)
        restoreStateT s = do
          first (restoreT . pure) . juggleState @f @a Proxy Proxy s
      in
        Lazy.StateT $ \s -> do
          fmap (restoreStateT s) . run $ Lazy.runStateT m s

instance MonadTransDistributive (Strict.StateT s) where
  type Transformer f (Strict.StateT s) m = (
      Monad m
    , Monad (f m)
    , Monad (Strict.StateT s m)
    , Monad (f (Strict.StateT s m))
    , MonadTrans f
    , MonadTransControl f
    , MonadTransJuggle f
    , MFunctor f
    )

  distributeT :: forall f m a.
       Transformer f (Strict.StateT s) m
    => Strict.StateT s (f m) a
    -> f (Strict.StateT s m) a
  distributeT m =
    join $ liftWith $ \run ->
      let
        restoreStateT :: s -> StT f (a, s) -> (f (Strict.StateT s m) a, s)
        restoreStateT s = do
          first (restoreT . pure) . juggleState @f @a Proxy Proxy s
      in
        Strict.StateT $ \s -> do
          fmap (restoreStateT s) . run $ Strict.runStateT m s

instance Monoid w => MonadTransDistributive (Lazy.RWST r w s) where
  type Transformer f (Lazy.RWST r w s) m = (
      Monad m
    , Monad (f m)
    , Monad (Lazy.RWST r w s m)
    , Monad (f (Lazy.RWST r w s m))
    , MonadTrans f
    , MonadTransControl f
    , MonadTransJuggle f
    , MFunctor f
    )

  distributeT :: forall f m a.
       Transformer f (Lazy.RWST r w s) m
    => Lazy.RWST r w s (f m) a
    -> f (Lazy.RWST r w s m) a
  distributeT m =
    join $ liftWith $ \run ->
      let
        restoreRWST :: s -> StT f (a, s, w) -> (f (Lazy.RWST r w s m) a, s, w)
        restoreRWST s = do
          first3 (restoreT . pure) . juggleRWS @f @w @a Proxy Proxy Proxy s
      in
        Lazy.RWST $ \r s -> do
          fmap (restoreRWST s) . run $ Lazy.runRWST m r s

instance Monoid w => MonadTransDistributive (Strict.RWST r w s) where
  type Transformer f (Strict.RWST r w s) m = (
      Monad m
    , Monad (f m)
    , Monad (Strict.RWST r w s m)
    , Monad (f (Strict.RWST r w s m))
    , MonadTrans f
    , MonadTransControl f
    , MonadTransJuggle f
    , MFunctor f
    )

  distributeT :: forall f m a.
       Transformer f (Strict.RWST r w s) m
    => Strict.RWST r w s (f m) a
    -> f (Strict.RWST r w s m) a
  distributeT m =
    join $ liftWith $ \run ->
      let
        restoreRWST :: s -> StT f (a, s, w) -> (f (Strict.RWST r w s m) a, s, w)
        restoreRWST s = do
          first3 (restoreT . pure) . juggleRWS @f @w @a Proxy Proxy Proxy s
      in
        Strict.RWST $ \r s -> do
          fmap (restoreRWST s) . run $ Strict.runRWST m r s

------------------------------------------------------------------------
-- * MonadTransJuggle

first3 :: (a -> b) -> (a, s, w) -> (b, s, w)
first3 f (a, s, w) =
  (f a, s, w)

unpack3 :: (a, s, w) -> (a, (s, w))
unpack3 (a, s, w) =
  (a, (s, w))

juggleRWS :: forall t w a s.
     (MonadTransJuggle t, Monoid w)
  => Proxy t
  -> Proxy w
  -> Proxy a
  -> s
  -> StT t (a, s, w)
  -> (StT t a, s, w)
juggleRWS _ _ _ s0 st0 =
  let
    (st, (s, w)) =
      juggleState @t @a Proxy Proxy (s0, mempty) $
        mapStT @t @(a, s, w) Proxy Proxy unpack3 st0
  in
    (st, s, w)

class MonadTransControl t => MonadTransJuggle (t :: (* -> *) -> * -> *) where
  mapStT :: Proxy t -> Proxy a -> (a -> b) -> StT t a -> StT t b
  juggleState :: Proxy t -> Proxy a -> s -> StT t (a, s) -> (StT t a, s)

instance MonadTransJuggle MaybeT where
  mapStT _ _ =
    fmap

  juggleState _ _ s0 = \case
    Nothing ->
      (Nothing, s0)
    Just (x, s) ->
      (Just x, s)

instance MonadTransJuggle (ExceptT x) where
  mapStT _ _ =
    fmap

  juggleState _ _ s0 = \case
    Left x ->
      (Left x, s0)
    Right (x, s) ->
      (Right x, s)

instance MonadTransJuggle (ReaderT r) where
  mapStT _ _ =
    id

  juggleState _ _ _ (x, s) =
    (x, s)

instance Monoid w => MonadTransJuggle (Lazy.WriterT w) where
  mapStT _ _ =
    first

  juggleState _ _ _ ((x, s), w) =
    ((x, w), s)

instance Monoid w => MonadTransJuggle (Strict.WriterT w) where
  mapStT _ _ =
    first

  juggleState _ _ _ ((x, s), w) =
    ((x, w), s)

instance MonadTransJuggle (Lazy.StateT s) where
  mapStT _ _ =
    first

  juggleState _ _ _ ((x, s0), s1) =
    ((x, s1), s0)

instance MonadTransJuggle (Strict.StateT s) where
  mapStT _ _ =
    first

  juggleState _ _ _ ((x, s0), s1) =
    ((x, s1), s0)

instance Monoid w => MonadTransJuggle (Lazy.RWST r w s) where
  mapStT _ _ =
    first3

  juggleState _ _ _ ((x, s0), s1, w) =
    ((x, s1, w), s0)

instance Monoid w => MonadTransJuggle (Strict.RWST r w s) where
  mapStT _ _ =
    first3

  juggleState _ _ _ ((x, s0), s1, w) =
    ((x, s1, w), s0)
