{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Hedgehog.Internal.Distributive (
    Distributive(..)
  ) where

import           Control.Monad (join)
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.Writer (WriterT(..))

import           GHC.Exts (Constraint)


class Distributive g where
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

  distribute :: Transformer f g m => g (f m) a -> f (g m) a

instance Distributive MaybeT where
  distribute x =
    lift . MaybeT . pure =<< hoist lift (runMaybeT x)

instance Distributive (ExceptT x) where
  distribute x =
    lift . ExceptT . pure =<< hoist lift (runExceptT x)

instance Monoid w => Distributive (WriterT w) where
  distribute x =
    lift . WriterT . pure =<< hoist lift (runWriterT x)

instance Distributive (ReaderT r) where
  distribute x =
    join . lift . ReaderT $ \r ->
      pure . hoist lift $ runReaderT x r
