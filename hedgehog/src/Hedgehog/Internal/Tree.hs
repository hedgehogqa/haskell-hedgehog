{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
module Hedgehog.Internal.Tree (
    Tree(..)
  , Node(..)

  , fromNode

  , unfold
  , unfoldForest

  , expand
  , prune

  , render
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
import           Data.Functor.Classes (Eq1(..))

#if MIN_VERSION_base(4,9,0)
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
#endif

import           Hedgehog.Internal.Distributive

------------------------------------------------------------------------

-- | An effectful tree, each node in the tree can have an effect before it is
--   produced.
--
newtype Tree m a =
  Tree {
      runTree :: m (Node m a)
    }

-- | A node in an effectful tree, as well as its unevaluated children.
--
data Node m a =
  Node {
      nodeValue :: a
    , nodeChildren :: [Tree m a]
    } deriving Eq

-- | Create a 'Tree' from a 'Node'
--
fromNode :: Applicative m => Node m a -> Tree m a
fromNode =
  Tree . pure

-- | Create a tree from a value and an unfolding function.
--
unfold :: Monad m => (a -> [a]) -> a -> Tree m a
unfold f x =
  Tree . pure $
    Node x (unfoldForest f x)

-- | Create a forest from a value and an unfolding function.
--
unfoldForest :: Monad m => (a -> [a]) -> a -> [Tree m a]
unfoldForest f =
  fmap (unfold f) . f

-- | Expand a tree using an unfolding function.
--
expand :: Monad m => (a -> [a]) -> Tree m a -> Tree m a
expand f m =
  Tree $ do
    Node x xs <- runTree m
    pure . Node x $
      fmap (expand f) xs ++ unfoldForest f x

-- | Throw away a tree's children.
--
prune :: Monad m => Tree m a -> Tree m a
prune m =
  Tree $ do
    Node x _ <- runTree m
    pure $ Node x []

------------------------------------------------------------------------
-- Node/Tree instances

instance (Eq1 m, Eq a) => Eq (Tree m a) where
  Tree m0 == Tree m1 =
#if MIN_VERSION_base(4,9,0)
    liftEq (==) m0 m1
#else
    eq1 m0 m1
#endif

instance Functor m => Functor (Node m) where
  fmap f (Node x xs) =
    Node (f x) (fmap (fmap f) xs)

instance Functor m => Functor (Tree m) where
  fmap f =
    Tree . fmap (fmap f) . runTree

instance Applicative m => Applicative (Node m) where
  pure x =
    Node x []
  (<*>) (Node ab tabs) na@(Node a tas) =
    Node (ab a) $
      map (<*> (fromNode na)) tabs ++ map (fmap ab) tas

instance Applicative m => Applicative (Tree m) where
  pure =
    Tree . pure . pure
  (<*>) (Tree mab) (Tree ma) =
    Tree $
      liftA2 (<*>) mab ma

instance Monad m => Monad (Node m) where
  return =
    pure

  (>>=) (Node x xs) k =
    case k x of
      Node y ys ->
        Node y $
          fmap (Tree . fmap (>>= k) . runTree) xs ++ ys

instance Monad m => Monad (Tree m) where
  return =
    pure

  (>>=) m k =
    Tree $ do
      Node x xs <- runTree m
      Node y ys <- runTree (k x)
      pure . Node y $
        fmap (>>= k) xs ++ ys

instance MonadPlus m => Alternative (Tree m) where
  empty =
    mzero
  (<|>) =
    mplus

instance MonadPlus m => MonadPlus (Tree m) where
  mzero =
    Tree mzero
  mplus x y =
    Tree (runTree x `mplus` runTree y)

instance MonadTrans Tree where
  lift m =
    Tree $ do
      x <- m
      pure (Node x [])

instance MFunctor Node where
  hoist f (Node x xs) =
    Node x (fmap (hoist f) xs)

instance MFunctor Tree where
  hoist f (Tree m) =
    Tree . f $ fmap (hoist f) m

embedNode :: Monad m => (t (Node t b) -> Tree m (Node t b)) -> Node t b -> Node m b
embedNode f (Node x xs) =
  Node x (fmap (embedTree f) xs)

embedTree :: Monad m => (t (Node t b) -> Tree m (Node t b)) -> Tree t b -> Tree m b
embedTree f (Tree m) =
  Tree . pure . embedNode f =<< f m

instance MMonad Tree where
  embed f m =
    embedTree f m

distributeNode :: Transformer t Tree m => Node (t m) a -> t (Tree m) a
distributeNode (Node x xs) =
  join . lift . fromNode . Node (pure x) $
    fmap (pure . distributeTree) xs

distributeTree :: Transformer t Tree m => Tree (t m) a -> t (Tree m) a
distributeTree x =
  distributeNode =<< hoist lift (runTree x)

instance Distributive Tree where
  distribute =
    distributeTree

instance PrimMonad m => PrimMonad (Tree m) where
  type PrimState (Tree m) =
    PrimState m
  primitive =
    lift . primitive

instance MonadIO m => MonadIO (Tree m) where
  liftIO =
    lift . liftIO

instance MonadBase b m => MonadBase b (Tree m) where
  liftBase =
    lift . liftBase

instance MonadThrow m => MonadThrow (Tree m) where
  throwM =
    lift . throwM

handleNode :: (Exception e, MonadCatch m) => (e -> Tree m a) -> Node m a -> Node m a
handleNode onErr (Node x xs) =
  Node x $
    fmap (handleTree onErr) xs

handleTree :: (Exception e, MonadCatch m) => (e -> Tree m a) -> Tree m a -> Tree m a
handleTree onErr m =
  Tree . fmap (handleNode onErr) $
    catch (runTree m) (runTree . onErr)

instance MonadCatch m => MonadCatch (Tree m) where
  catch =
    flip handleTree

localNode :: MonadReader r m => (r -> r) -> Node m a -> Node m a
localNode f (Node x xs) =
  Node x $
    fmap (localTree f) xs

localTree :: MonadReader r m => (r -> r) -> Tree m a -> Tree m a
localTree f (Tree m) =
  Tree $
    pure . localNode f =<< local f m

instance MonadReader r m => MonadReader r (Tree m) where
  ask =
    lift ask
  local =
    localTree

instance MonadState s m => MonadState s (Tree m) where
  get =
    lift get
  put =
    lift . put
  state =
    lift . state

listenNode :: MonadWriter w m => w -> Node m a -> Node m (a, w)
listenNode w (Node x xs) =
  Node (x, w) $
    fmap (listenTree w) xs

listenTree :: MonadWriter w m => w -> Tree m a -> Tree m (a, w)
listenTree w0 (Tree m) =
  Tree $ do
    (x, w) <- listen m
    pure $ listenNode (mappend w0 w) x

-- FIXME This just throws away the writer modification function.
passNode :: MonadWriter w m => Node m (a, w -> w) -> Node m a
passNode (Node (x, _) xs) =
  Node x $
    fmap passTree xs

passTree :: MonadWriter w m => Tree m (a, w -> w) -> Tree m a
passTree (Tree m) =
  Tree $
    pure . passNode =<< m

instance MonadWriter w m => MonadWriter w (Tree m) where
  writer =
    lift . writer
  tell =
    lift . tell
  listen =
    listenTree mempty
  pass =
    passTree

handleErrorNode :: MonadError e m => (e -> Tree m a) -> Node m a -> Node m a
handleErrorNode onErr (Node x xs) =
  Node x $
    fmap (handleErrorTree onErr) xs

handleErrorTree :: MonadError e m => (e -> Tree m a) -> Tree m a -> Tree m a
handleErrorTree onErr m =
  Tree . fmap (handleErrorNode onErr) $
    catchError (runTree m) (runTree . onErr)

instance MonadError e m => MonadError e (Tree m) where
  throwError =
    lift . throwError
  catchError =
    flip handleErrorTree

instance MonadResource m => MonadResource (Tree m) where
  liftResourceT =
    lift . liftResourceT

------------------------------------------------------------------------
-- Show/Show1 instances

#if MIN_VERSION_base(4,9,0)
instance (Show1 m, Show a) => Show (Node m a) where
  showsPrec =
    showsPrec1

instance (Show1 m, Show a) => Show (Tree m a) where
  showsPrec =
    showsPrec1

instance Show1 m => Show1 (Node m) where
  liftShowsPrec sp sl d (Node x xs) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsBinaryWith sp sp2 "Node" d x xs

instance Show1 m => Show1 (Tree m) where
  liftShowsPrec sp sl d (Tree m) =
    let
      sp1 =
        liftShowsPrec sp sl

      sl1 =
        liftShowList sp sl

      sp2 =
        liftShowsPrec sp1 sl1
    in
      showsUnaryWith sp2 "Tree" d m
#endif

------------------------------------------------------------------------
-- Pretty Printing

--
-- Rendering implementation based on the one from containers/Data.Tree
--

renderTreeLines :: Monad m => Tree m String -> m [String]
renderTreeLines (Tree m) = do
  Node x xs0 <- m
  xs <- renderForestLines xs0
  pure $
    lines (renderNode x) ++ xs

renderNode :: String -> String
renderNode xs =
  case xs of
    [_] ->
      ' ' : xs
    _ ->
      xs

renderForestLines :: Monad m => [Tree m String] -> m [String]
renderForestLines xs0 =
  let
    shift hd other =
      zipWith (++) (hd : repeat other)
  in
    case xs0 of
      [] ->
        pure []

      [x] -> do
        s <- renderTreeLines x
        pure $
          shift " └╼" "   " s

      x : xs -> do
        s <- renderTreeLines x
        ss <- renderForestLines xs
        pure $
          shift " ├╼" " │ " s ++ ss

-- | Render a tree of strings, note that this forces all the delayed effects in
--   the tree.
render :: Monad m => Tree m String -> m String
render =
  fmap unlines . renderTreeLines
