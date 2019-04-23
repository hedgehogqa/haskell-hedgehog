{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
module Hedgehog.Internal.Tree (
    Tree
  , TreeT(..)
  , runTree

  , Node
  , NodeT(..)
  , fromNodeT

  , unfold
  , unfoldForest

  , expand
  , prune

  , filter

  , render
  , renderT
  ) where

import           Control.Applicative (Alternative(..), liftA2)
import           Control.Monad (MonadPlus(..), join)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(..), MonadCatch(..), Exception)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..), MMonad(..))
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Resource (MonadResource(..))
import           Control.Monad.Writer.Class (MonadWriter(..))

import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Classes (Eq1(..))
#if MIN_VERSION_base(4,9,0)
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
#endif
import           Data.Foldable (Foldable(..))
import           Data.Maybe (mapMaybe)

import           Hedgehog.Internal.Distributive

import           Prelude hiding (filter)

------------------------------------------------------------------------

-- | A rose tree.
--
type Tree =
  TreeT Identity

-- | An effectful tree, each node in the tree can have an effect before it is
--   produced.
--
newtype TreeT m a =
  TreeT {
      runTreeT :: m (NodeT m a)
    }

-- | A node in a rose tree.
--
type Node =
  NodeT Identity

-- | A node in an effectful tree, as well as its unevaluated children.
--
data NodeT m a =
  NodeT {
      nodeValue :: a
    , nodeChildren :: [TreeT m a]
    } deriving (Eq)

-- | Extracts the 'Node' from a 'Tree'.
--
runTree :: Tree a -> Node a
runTree =
  runIdentity . runTreeT

-- | Create a 'TreeT' from a 'NodeT'
--
fromNodeT :: Applicative m => NodeT m a -> TreeT m a
fromNodeT =
  TreeT . pure

-- | Create a tree from a value and an unfolding function.
--
unfold :: Monad m => (a -> [a]) -> a -> TreeT m a
unfold f x =
  TreeT . pure $
    NodeT x (unfoldForest f x)

-- | Create a forest from a value and an unfolding function.
--
unfoldForest :: Monad m => (a -> [a]) -> a -> [TreeT m a]
unfoldForest f =
  fmap (unfold f) . f

-- | Expand a tree using an unfolding function.
--
expand :: Monad m => (a -> [a]) -> TreeT m a -> TreeT m a
expand f m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    pure . NodeT x $
      fmap (expand f) xs ++ unfoldForest f x

-- | Throw away a tree's children.
--
prune :: Monad m => TreeT m a -> TreeT m a
prune m =
  TreeT $ do
    NodeT x _ <- runTreeT m
    pure $ NodeT x []

-- | Returns a tree containing only elements that match the predicate.
--
filter :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filter p m =
  let
    NodeT x xs =
      runTree m
  in
    if p x then
      Just . TreeT . pure . NodeT x $
        mapMaybe (filter p) xs
    else
      Nothing

------------------------------------------------------------------------

instance Foldable Tree where
  foldMap f (TreeT mx) =
    foldMap f (runIdentity mx)

instance Foldable Node where
  foldMap f (NodeT x xs) =
    f x <> mconcat (fmap (foldMap f) xs)

------------------------------------------------------------------------
-- NodeT/TreeT instances

instance (Eq1 m, Eq a) => Eq (TreeT m a) where
  TreeT m0 == TreeT m1 =
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
    liftEq (==) m0 m1
#else
    eq1 m0 m1
#endif

instance Functor m => Functor (NodeT m) where
  fmap f (NodeT x xs) =
    NodeT (f x) (fmap (fmap f) xs)

instance Functor m => Functor (TreeT m) where
  fmap f =
    TreeT . fmap (fmap f) . runTreeT

instance Applicative m => Applicative (NodeT m) where
  pure x =
    NodeT x []
  (<*>) (NodeT ab tabs) na@(NodeT a tas) =
    NodeT (ab a) $
      map (<*> (fromNodeT na)) tabs ++ map (fmap ab) tas

instance Applicative m => Applicative (TreeT m) where
  pure =
    TreeT . pure . pure
  (<*>) (TreeT mab) (TreeT ma) =
    TreeT $
      liftA2 (<*>) mab ma

instance Monad m => Monad (NodeT m) where
  return =
    pure

  (>>=) (NodeT x xs) k =
    case k x of
      NodeT y ys ->
        NodeT y $
          fmap (TreeT . fmap (>>= k) . runTreeT) xs ++ ys

instance Monad m => Monad (TreeT m) where
  return =
    pure

  (>>=) m k =
    TreeT $ do
      NodeT x xs <- runTreeT m
      NodeT y ys <- runTreeT (k x)
      pure . NodeT y $
        fmap (>>= k) xs ++ ys

instance Alternative m => Alternative (TreeT m) where
  empty =
    TreeT empty
  (<|>) x y =
    TreeT (runTreeT x <|> runTreeT y)

instance MonadPlus m => MonadPlus (TreeT m) where
  mzero =
    TreeT mzero
  mplus x y =
    TreeT (runTreeT x `mplus` runTreeT y)

instance MonadTrans TreeT where
  lift f =
    TreeT $
      fmap (\x -> NodeT x []) f

instance MFunctor NodeT where
  hoist f (NodeT x xs) =
    NodeT x (fmap (hoist f) xs)

instance MFunctor TreeT where
  hoist f (TreeT m) =
    TreeT . f $ fmap (hoist f) m

embedNodeT :: Monad m => (t (NodeT t b) -> TreeT m (NodeT t b)) -> NodeT t b -> NodeT m b
embedNodeT f (NodeT x xs) =
  NodeT x (fmap (embedTreeT f) xs)

embedTreeT :: Monad m => (t (NodeT t b) -> TreeT m (NodeT t b)) -> TreeT t b -> TreeT m b
embedTreeT f (TreeT m) =
  TreeT . pure . embedNodeT f =<< f m

instance MMonad TreeT where
  embed f m =
    embedTreeT f m

distributeNodeT :: Transformer t TreeT m => NodeT (t m) a -> t (TreeT m) a
distributeNodeT (NodeT x xs) =
  join . lift . fromNodeT . NodeT (pure x) $
    fmap (pure . distributeTreeT) xs

distributeTreeT :: Transformer t TreeT m => TreeT (t m) a -> t (TreeT m) a
distributeTreeT x =
  distributeNodeT =<< hoist lift (runTreeT x)

instance Distributive TreeT where
  distribute =
    distributeTreeT

instance PrimMonad m => PrimMonad (TreeT m) where
  type PrimState (TreeT m) =
    PrimState m
  primitive =
    lift . primitive

instance MonadIO m => MonadIO (TreeT m) where
  liftIO =
    lift . liftIO

instance MonadBase b m => MonadBase b (TreeT m) where
  liftBase =
    lift . liftBase

instance MonadThrow m => MonadThrow (TreeT m) where
  throwM =
    lift . throwM

handleNodeT :: (Exception e, MonadCatch m) => (e -> TreeT m a) -> NodeT m a -> NodeT m a
handleNodeT onErr (NodeT x xs) =
  NodeT x $
    fmap (handleTreeT onErr) xs

handleTreeT :: (Exception e, MonadCatch m) => (e -> TreeT m a) -> TreeT m a -> TreeT m a
handleTreeT onErr m =
  TreeT . fmap (handleNodeT onErr) $
    catch (runTreeT m) (runTreeT . onErr)

instance MonadCatch m => MonadCatch (TreeT m) where
  catch =
    flip handleTreeT

localNodeT :: MonadReader r m => (r -> r) -> NodeT m a -> NodeT m a
localNodeT f (NodeT x xs) =
  NodeT x $
    fmap (localTreeT f) xs

localTreeT :: MonadReader r m => (r -> r) -> TreeT m a -> TreeT m a
localTreeT f (TreeT m) =
  TreeT $
    pure . localNodeT f =<< local f m

instance MonadReader r m => MonadReader r (TreeT m) where
  ask =
    lift ask
  local =
    localTreeT

instance MonadState s m => MonadState s (TreeT m) where
  get =
    lift get
  put =
    lift . put
  state =
    lift . state

listenNodeT :: MonadWriter w m => w -> NodeT m a -> NodeT m (a, w)
listenNodeT w (NodeT x xs) =
  NodeT (x, w) $
    fmap (listenTreeT w) xs

listenTreeT :: MonadWriter w m => w -> TreeT m a -> TreeT m (a, w)
listenTreeT w0 (TreeT m) =
  TreeT $ do
    (x, w) <- listen m
    pure $ listenNodeT (mappend w0 w) x

-- FIXME This just throws away the writer modification function.
passNodeT :: MonadWriter w m => NodeT m (a, w -> w) -> NodeT m a
passNodeT (NodeT (x, _) xs) =
  NodeT x $
    fmap passTreeT xs

passTreeT :: MonadWriter w m => TreeT m (a, w -> w) -> TreeT m a
passTreeT (TreeT m) =
  TreeT $
    pure . passNodeT =<< m

instance MonadWriter w m => MonadWriter w (TreeT m) where
  writer =
    lift . writer
  tell =
    lift . tell
  listen =
    listenTreeT mempty
  pass =
    passTreeT

handleErrorNodeT :: MonadError e m => (e -> TreeT m a) -> NodeT m a -> NodeT m a
handleErrorNodeT onErr (NodeT x xs) =
  NodeT x $
    fmap (handleErrorTreeT onErr) xs

handleErrorTreeT :: MonadError e m => (e -> TreeT m a) -> TreeT m a -> TreeT m a
handleErrorTreeT onErr m =
  TreeT . fmap (handleErrorNodeT onErr) $
    catchError (runTreeT m) (runTreeT . onErr)

instance MonadError e m => MonadError e (TreeT m) where
  throwError =
    lift . throwError
  catchError =
    flip handleErrorTreeT

instance MonadResource m => MonadResource (TreeT m) where
  liftResourceT =
    lift . liftResourceT

------------------------------------------------------------------------
-- Show/Show1 instances

#if MIN_VERSION_base(4,9,0)
instance (Show1 m, Show a) => Show (NodeT m a) where
  showsPrec =
    showsPrec1

instance (Show1 m, Show a) => Show (TreeT m a) where
  showsPrec =
    showsPrec1

instance Show1 m => Show1 (NodeT m) where
  liftShowsPrec sp sl d (NodeT x xs) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsBinaryWith sp sp2 "NodeT" d x xs

instance Show1 m => Show1 (TreeT m) where
  liftShowsPrec sp sl d (TreeT m) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsUnaryWith sp2 "TreeT" d m
#endif

------------------------------------------------------------------------
-- Pretty Printing

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeTLines :: Monad m => TreeT m String -> m [String]
renderTreeTLines (TreeT m) = do
  NodeT x xs0 <- m
  xs <- renderForestLines xs0
  pure $
    lines (renderNodeT x) ++ xs

renderNodeT :: String -> String
renderNodeT xs =
  case xs of
    [_] ->
      ' ' : xs
    _ ->
      xs

renderForestLines :: Monad m => [TreeT m String] -> m [String]
renderForestLines xs0 =
  let
    shift hd other =
      zipWith (++) (hd : repeat other)
  in
    case xs0 of
      [] ->
        pure []

      [x] -> do
        s <- renderTreeTLines x
        pure $
          shift " └╼" "   " s

      x : xs -> do
        s <- renderTreeTLines x
        ss <- renderForestLines xs
        pure $
          shift " ├╼" " │ " s ++ ss

-- | Render a tree of strings.
--
render :: Tree String -> String
render =
  runIdentity . renderT

-- | Render a tree of strings, note that this forces all the delayed effects in
--   the tree.
--
renderT :: Monad m => TreeT m String -> m String
renderT =
  fmap unlines . renderTreeTLines
