{-# LANGUAGE CPP #-}
module Hedgehog.Internal.Tree (
    Tree(..)
  , Node(..)

  , unfold
  , unfoldForest

  , expand
  , prune

  , render
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), ap)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (MonadTrans(..))

#if MIN_VERSION_base(4,9,0)
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
#endif

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
    }

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
-- Node instances

instance Functor m => Functor (Node m) where
  fmap f (Node x xs) =
    Node (f x) (fmap (fmap f) xs)

instance Monad m => Applicative (Node m) where
  pure =
    return
  (<*>) =
    ap

instance Monad m => Monad (Node m) where
  return x =
    Node x []

  (>>=) (Node x xs) k =
    case k x of
      Node y ys ->
        Node y $
          fmap (Tree . fmap (>>= k) . runTree) xs ++ ys

------------------------------------------------------------------------
-- Tree instances

instance Functor m => Functor (Tree m) where
  fmap f =
    Tree . fmap (fmap f) . runTree

instance Monad m => Applicative (Tree m) where
  pure =
    return
  (<*>) =
    ap

instance Monad m => Monad (Tree m) where
  return x =
    Tree . pure $ Node x []

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

instance MFunctor Tree where
  hoist f (Tree m) =
    let
      hoistOutcome (Node x xs) =
        Node x (fmap (hoist f) xs)
    in
      Tree . f $ fmap hoistOutcome m

instance MonadIO m => MonadIO (Tree m) where
  liftIO =
    lift . liftIO

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
