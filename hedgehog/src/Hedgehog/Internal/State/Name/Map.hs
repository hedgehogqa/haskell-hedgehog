{-# language ScopedTypeVariables #-}
module Hedgehog.Internal.State.Name.Map
  ( NMap
  , empty
  , insert
  , lookup
  , maxViewWithKey
  , union
  , unions
  , null
  , isSubmapOf
  ) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Coerce (coerce)
import           Hedgehog.Internal.State.Name (Name (..))
import           Data.Foldable (foldl')
import           Prelude hiding (lookup, null)

newtype NMap a = NMap { unNMap :: IntMap a }

instance Show a => Show (NMap a) where
  showsPrec p = showsPrec p . unNMap

empty :: NMap a
empty = NMap IM.empty

insert :: Name -> a -> NMap a -> NMap a
insert (Name n) a (NMap m) = NMap (IM.insert n a m)

lookup :: Name -> NMap a -> Maybe a
lookup (Name n) (NMap m) = IM.lookup n m

maxViewWithKey :: forall a. NMap a -> Maybe ((Name, a), NMap a)
maxViewWithKey = coerce (IM.maxViewWithKey :: IntMap a -> Maybe ((Int, a), IntMap a))

union :: NMap a -> NMap a -> NMap a
union (NMap m) (NMap n) = NMap $ IM.union m n

unions :: Foldable f => f (NMap a) -> NMap a
unions = foldl' union empty

null :: NMap a -> Bool
null = IM.null . unNMap

isSubmapOf :: Eq a => NMap a -> NMap a -> Bool
isSubmapOf (NMap m) (NMap n) = IM.isSubmapOf m n
