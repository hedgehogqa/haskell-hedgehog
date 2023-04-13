{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
module Hedgehog.Internal.Tree (
    Tree
  , pattern Tree
  , TreeT(..)
  , runTree
  , mapTreeT
  , treeValue
  , treeChildren

  , Node
  , pattern Node
  , NodeT(..)
  , fromNodeT

  , unfold
  , unfoldForest

  , expand
  , prune

  , catMaybes
  , filter
  , mapMaybe
  , filterMaybeT
  , mapMaybeMaybeT
  , filterT
  , consChild
  , mapMaybeT
  , depth
  , interleave

  , render
  , renderT
  ) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif
import           Control.Applicative (Alternative(..))
import           Control.Exception.Safe (Exception)
import           Control.Monad (MonadPlus(..), guard, join)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch))
import           Control.Monad.Trans.Control ()
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..), MMonad(..), generalize)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Resource (MonadResource(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Control.Monad.Zip (MonadZip(..))

import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Classes (Eq1(..))
import           Data.Functor.Classes (Show1(..), showsPrec1)
import           Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import           Hedgehog.Internal.Distributive
import           Control.Monad.Trans.Control (MonadBaseControl (..))

import           Prelude hiding (filter)

------------------------------------------------------------------------

-- | A rose tree.
--
type Tree =
  TreeT Identity

-- | Pattern to ease construction / deconstruction of pure trees.
--
pattern Tree :: NodeT Identity a -> Tree a
pattern Tree node =
  TreeT (Identity node)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Tree #-}
#endif

-- | An effectful tree, each node in the tree can have an effect before it is
--   produced.
--
newtype TreeT m a =
  TreeT {
      runTreeT :: m (NodeT m a)
    }

instance MonadBaseControl b m => MonadBaseControl b (TreeT m) where
  type StM (TreeT m) a = StM m (NodeT m a)
  liftBaseWith f = TreeT $ liftBaseWith (\g -> pure <$> f (g . runTreeT))
  restoreM = TreeT . restoreM

-- | A node in a rose tree.
--
type Node =
  NodeT Identity
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Node #-}
#endif

-- | Pattern to ease construction / deconstruction of pure nodes.
--
pattern Node :: a -> [Tree a] -> Node a
pattern Node x xs =
  NodeT x xs

-- | A node in an effectful tree, as well as its unevaluated children.
--
data NodeT m a =
  NodeT {
      -- | The value at this 'NodeT' in the 'TreeT'.
      nodeValue :: a

      -- | The children of this 'NodeT'.
    , nodeChildren :: [TreeT m a]
    } deriving (Eq)

-- | Extracts the 'Node' from a 'Tree'.
--
runTree :: Tree a -> Node a
runTree =
  runIdentity . runTreeT

-- | Map between 'TreeT' computations.
--
mapTreeT :: (m (NodeT m a) -> m (NodeT m a)) -> TreeT m a -> TreeT m a
mapTreeT f =
  TreeT . f . runTreeT

-- | Create a 'TreeT' from a 'NodeT'
--
fromNodeT :: Applicative m => NodeT m a -> TreeT m a
fromNodeT =
  TreeT . pure

-- | The value at the root of the 'Tree'.
--
treeValue :: Tree a -> a
treeValue =
  nodeValue . runTree

-- | The children of the 'Tree'.
--
treeChildren :: Tree a -> [Tree a]
treeChildren =
  nodeChildren . runTree

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

-- | Throw away all but the top @n@ levels of a tree's children.
--
--   /@prune 0@ will throw away all of a tree's children./
--
prune :: Monad m => Int -> TreeT m a -> TreeT m a
prune n m =
  if n <= 0 then
    TreeT $ do
      NodeT x _ <- runTreeT m
      pure $ NodeT x []
  else
    TreeT $ do
      NodeT x xs0 <- runTreeT m
      pure . NodeT x $
        fmap (prune (n - 1)) xs0

-- | Returns the depth of the deepest leaf node in the tree.
--
depth :: Tree a -> Int
depth m =
  let
    NodeT _ xs =
      runTree m

    n =
      if null xs then
        0
      else
        maximum (fmap depth xs)
  in
    1 + n

-- | Takes a tree of 'Maybe's and returns a tree of all the 'Just' values.
--
--   If the root of the tree is 'Nothing' then 'Nothing' is returned.
--
catMaybes :: Tree (Maybe a) -> Maybe (Tree a)
catMaybes m =
  let
    NodeT mx mxs =
      runTree m
  in
    case mx of
      Nothing -> do
        case Maybe.mapMaybe catMaybes mxs of
          [] ->
            Nothing
          Tree (NodeT x xs0) : xs1 ->
            Just . Tree $
              Node x (xs0 ++ xs1)
      Just x ->
        Just . Tree $
          Node x (Maybe.mapMaybe catMaybes mxs)

fromPred :: (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)

-- | Returns a tree containing only elements that match the predicate.
--
--   If the root of the tree does not match the predicate then 'Nothing' is
--   returned.
--
filter :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filter p = mapMaybe (fromPred p)

mapMaybe :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybe p =
  catMaybes .
  runTreeMaybeT .
  mapMaybeMaybeT p .
  hoist lift

runTreeMaybeT :: Monad m => TreeT (MaybeT m) a -> TreeT m (Maybe a)
runTreeMaybeT =
  runMaybeT .
  distributeT

-- | Returns a tree containing only elements that match the predicate.
--
--   If the root of the tree does not match the predicate then 'Nothing' is
--   returned.
--
filterMaybeT :: (a -> Bool) -> TreeT (MaybeT Identity) a -> TreeT (MaybeT Identity) a
filterMaybeT p = mapMaybeMaybeT (fromPred p)

mapMaybeMaybeT :: (a -> Maybe b) -> TreeT (MaybeT Identity) a -> TreeT (MaybeT Identity) b
mapMaybeMaybeT p t =
  case runTreeMaybeT t of
    Tree (Node Nothing _) ->
      TreeT . MaybeT . Identity $ Nothing
    Tree (Node (Just x) xs) ->
      case p x of
        Nothing -> TreeT . MaybeT . Identity $ Nothing
        Just x' ->
          hoist generalize $
            Tree . Node x' $
              concatMap (flattenTree p) xs

flattenTree :: (a -> Maybe b) -> Tree (Maybe a) -> [Tree b]
flattenTree p (Tree (Node mx mxs0)) =
  let
    mxs =
      concatMap (flattenTree p) mxs0
  in
    case mx of
      Nothing -> mxs
      Just x ->
        case p x of
          Just x' ->
            [Tree (Node x' mxs)]
          Nothing ->
            mxs

-- | Returns a tree containing only elements that match the predicate.
--
--   When an element does not match the predicate its node is replaced with
--   'empty'.
--
filterT :: (Monad m, Alternative m) => (a -> Bool) -> TreeT m a -> TreeT m a
filterT p =
  mapMaybeT (fromPred p)

mapMaybeT :: (Monad m, Alternative m) => (a -> Maybe b) -> TreeT m a -> TreeT m b
mapMaybeT p m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    case p x of
      Just x' ->
        pure $
          NodeT x' (fmap (mapMaybeT p) xs)
      Nothing ->
        empty

consChild :: (Monad m) => a -> TreeT m a -> TreeT m a
consChild a m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    pure $
      NodeT x $
        pure a : xs

------------------------------------------------------------------------

-- | All ways a list can be split
--
-- > splits [1,2,3]
-- > ==
-- > [ ([], 1, [2, 3])
--   , ([1], 2, [3])
--   , ([1, 2], 3, [])
--   ]
--
splits :: [a] -> [([a], a, [a])]
splits xs0 =
  let
    go (front : fronts) (x : xs) =
      (front, x, xs) : go fronts xs
    go _ _ =
      []
  in
    go (List.inits xs0) xs0

-- | @removes n@ computes all ways we can remove chunks of size @n@ from a list
--
-- Examples
--
-- > removes 1 [1..3] == [[2,3],[1,3],[1,2]]
-- > removes 2 [1..4] == [[3,4],[1,2]]
-- > removes 2 [1..5] == [[3,4,5],[1,2,5],[1,2,3,4]]
-- > removes 3 [1..5] == [[4,5],[1,2,3]]
--
-- Note that the last chunk we delete might have fewer elements than @n@.
removes :: forall a. Int -> [a] -> [[a]]
removes k = \xs -> go xs
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = xs2 : map (xs1 ++) (go xs2)
      where
        (xs1, xs2) = splitAt k xs

dropSome :: Monad m => [NodeT m a] -> [TreeT m [a]]
dropSome ts = do
  n   <- takeWhile (> 0) $ iterate (`div` 2) (length ts)
  ts' <- removes n ts
  pure . TreeT . pure $ interleave ts'

shrinkOne :: Monad m => [NodeT m a] -> [TreeT m [a]]
shrinkOne ts = do
  (xs, y0, zs) <- splits ts
  y1 <- nodeChildren y0
  pure . TreeT $ do
    y2 <- runTreeT y1
    pure $
      interleave (xs ++ [y2] ++ zs)

interleave :: forall m a. Monad m => [NodeT m a] -> NodeT m [a]
interleave ts =
  NodeT (fmap nodeValue ts) $
    concat [
        dropSome ts
      , shrinkOne ts
      ]

------------------------------------------------------------------------

instance Foldable Tree where
  foldMap f (TreeT mx) =
    foldMap f (runIdentity mx)

instance Foldable Node where
  foldMap f (NodeT x xs) =
    f x `mappend` mconcat (fmap (foldMap f) xs)

instance Traversable Tree where
  traverse f (TreeT mx) =
    TreeT <$> traverse (traverse f) mx

instance Traversable Node where
  traverse f (NodeT x xs) =
    NodeT <$> f x <*> traverse (traverse f) xs

------------------------------------------------------------------------
-- NodeT/TreeT instances

instance (Eq1 m, Eq a) => Eq (TreeT m a) where
  TreeT m0 == TreeT m1 =
    liftEq (==) m0 m1

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

zipTreeT :: forall f a b. Applicative f => TreeT f a -> TreeT f b -> TreeT f (a, b)
zipTreeT l0@(TreeT left) r0@(TreeT right) =
  TreeT $
    let
      zipNodeT :: NodeT f a -> NodeT f b -> NodeT f (a, b)
      zipNodeT (NodeT a ls) (NodeT b rs) =
          NodeT (a, b) $
            concat [
                [zipTreeT l1 r0 | l1 <- ls]
              , [zipTreeT l0 r1 | r1 <- rs]
              ]
    in
      zipNodeT <$> left <*> right

instance Monad m => MonadZip (TreeT m) where
  mzip =
    zipTreeT

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

instance MonadTransDistributive TreeT where
  distributeT =
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
