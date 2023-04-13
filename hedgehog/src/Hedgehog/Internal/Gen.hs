{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingVia #-}
#endif

module Hedgehog.Internal.Gen (
  -- * Transformer
    Gen
  , GenT(..)
  , MonadGen(..)

  -- * Combinators
  , generalize

  -- ** Shrinking
  , shrink
  , prune

  -- ** Size
  , small
  , scale
  , resize
  , sized

  -- ** Integral
  , integral
  , integral_

  , int
  , int8
  , int16
  , int32
  , int64

  , word
  , word8
  , word16
  , word32
  , word64

  -- ** Floating-point
  , realFloat
  , realFrac_
  , float
  , double

  -- ** Enumeration
  , enum
  , enumBounded
  , bool
  , bool_

  -- ** Characters
  , binit
  , octit
  , digit
  , hexit
  , lower
  , upper
  , alpha
  , alphaNum
  , ascii
  , latin1
  , unicode
  , unicodeAll

  -- ** Strings
  , string
  , text
  , utf8
  , bytes

  -- ** Choice
  , constant
  , element
  , element_
  , choice
  , frequency
  , recursive

  -- ** Conditional
  , discard
  , ensure
  , filter
  , mapMaybe
  , filterT
  , mapMaybeT
  , just
  , justT

  -- ** Collections
  , maybe
  , either
  , either_
  , list
  , seq
  , nonEmpty
  , set
  , map

  -- ** Subterms
  , freeze
  , subterm
  , subtermM
  , subterm2
  , subtermM2
  , subterm3
  , subtermM3

  -- ** Combinations & Permutations
  , subsequence
  , subset
  , shuffle
  , shuffleSeq

  -- * Sampling Generators
  , sample
  , print
  , printTree
  , printWith
  , printTreeWith
  , renderTree

  -- * Internal
  -- $internal

  -- ** Transfomer
  , runGenT
  , evalGen
  , evalGenT
  , mapGenT
  , generate
  , toTree
  , toTreeMaybeT
  , fromTree
  , fromTreeT
  , fromTreeMaybeT
  , runDiscardEffect
  , runDiscardEffectT

  -- ** Size
  , golden

  -- ** Shrinking
  , atLeast

  -- ** Characters
  , isSurrogate
  , isNoncharacter

  -- ** Subterms
  , Vec(..)
  , Nat(..)
  , subtermMVec
  ) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif
import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), filterM, guard, replicateM, join)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch))
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..), MMonad(..))
import qualified Control.Monad.Morph as Morph
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Identity (IdentityT(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.Resource (MonadResource(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Control.Monad.Zip (MonadZip(..))

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import           Data.Foldable (for_, toList)
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Semigroup as Semigroup
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word8, Word16, Word32, Word64)

import           Hedgehog.Internal.Distributive (MonadTransDistributive(..))
import           Hedgehog.Internal.Prelude hiding (either, maybe, seq)
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Tree (Tree, TreeT(..), NodeT(..))
import qualified Hedgehog.Internal.Tree as Tree
import           Hedgehog.Range (Size, Range)
import qualified Hedgehog.Range as Range

#if __GLASGOW_HASKELL__ < 808
import qualified Control.Monad.Fail as Fail
#endif
#if __GLASGOW_HASKELL__ < 806
import           Data.Coerce (coerce)
#endif

------------------------------------------------------------------------
-- Generator transformer

-- | Generator for random values of @a@.
--
type Gen =
  GenT Identity

-- | Monad transformer which can generate random values of @a@.
--
newtype GenT m a =
  GenT {
      unGenT :: Size -> Seed -> TreeT (MaybeT m) a
    }

-- | Runs a generator, producing its shrink tree.
--
runGenT :: Size -> Seed -> GenT m a -> TreeT (MaybeT m) a
runGenT size seed (GenT m) =
  m size seed

-- | Run a generator, producing its shrink tree.
--
--   'Nothing' means discarded, 'Just' means we have a value.
--
evalGen :: Size -> Seed -> Gen a -> Maybe (Tree a)
evalGen size seed =
  Tree.mapMaybe id .
  evalGenT size seed

-- | Runs a generator, producing its shrink tree.
--
evalGenT :: Monad m => Size -> Seed -> GenT m a -> TreeT m (Maybe a)
evalGenT size seed =
  runDiscardEffectT .
  runGenT size seed

-- | Map over a generator's shrink tree.
--
mapGenT :: (TreeT (MaybeT m) a -> TreeT (MaybeT n) b) -> GenT m a -> GenT n b
mapGenT f gen =
  GenT $ \size seed ->
    f (runGenT size seed gen)

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
fromTree :: MonadGen m => Tree a -> m a
fromTree =
  fromTreeT .
  hoist (Morph.generalize)

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
fromTreeT :: MonadGen m => TreeT (GenBase m) a -> m a
fromTreeT x =
  fromTreeMaybeT $
    hoist (MaybeT . fmap Just) x

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
fromTreeMaybeT :: MonadGen m => TreeT (MaybeT (GenBase m)) a -> m a
fromTreeMaybeT x =
  fromGenT . GenT $ \_ _ ->
    x

-- | Observe a generator's shrink tree.
--
toTree :: forall m a. (MonadGen m, GenBase m ~ Identity) => m a -> m (Tree a)
toTree =
  withGenT $ mapGenT (Maybe.maybe empty pure . runDiscardEffect)

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
toTreeMaybeT :: MonadGen m => m a -> m (TreeT (MaybeT (GenBase m)) a)
toTreeMaybeT =
  withGenT $ mapGenT pure

-- | Lazily run the discard effects through the tree and reify it a
--   @Maybe (Tree a)@.
--
--   'Nothing' means discarded, 'Just' means we have a value.
--
--   Discards in the child nodes of the tree are simply removed.
--
runDiscardEffect :: TreeT (MaybeT Identity) a -> Maybe (Tree a)
runDiscardEffect =
  Tree.mapMaybe id .
  runDiscardEffectT

-- | Run the discard effects through the tree and reify them as 'Maybe' values
--   at the nodes.
--
--   'Nothing' means discarded, 'Just' means we have a value.
--
runDiscardEffectT :: Monad m => TreeT (MaybeT m) a -> TreeT m (Maybe a)
runDiscardEffectT =
  runMaybeT .
  distributeT

-- | Lift a @Gen / GenT Identity@ in to a @Monad m => GenT m@
--
generalize :: Monad m => Gen a -> GenT m a
generalize =
  hoist Morph.generalize

------------------------------------------------------------------------
-- MonadGen

-- | Class of monads which can generate input data for tests.
--
class (Monad m, Monad (GenBase m)) => MonadGen m where
  type GenBase m :: (Type -> Type)

  -- | Extract a 'GenT' from a  'MonadGen'.
  --
  toGenT :: m a -> GenT (GenBase m) a

  -- | Lift a 'GenT' in to a 'MonadGen'.
  --
  fromGenT :: GenT (GenBase m) a -> m a

-- | Transform a 'MonadGen' as a 'GenT'.
--
withGenT :: (MonadGen m, MonadGen n) => (GenT (GenBase m) a -> GenT (GenBase n) b) -> m a -> n b
withGenT f =
  fromGenT . f . toGenT

instance Monad m => MonadGen (GenT m) where
  -- | The type of the transformer stack's base 'Monad'.
  --
  type GenBase (GenT m) =
    m

  -- | Convert a 'MonadGen' to a 'GenT'.
  --
  toGenT =
    id

  -- | Convert a 'GenT' to a 'MonadGen'.
  --
  fromGenT =
    id

instance MonadGen m => MonadGen (IdentityT m) where
  type GenBase (IdentityT m) =
    IdentityT (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance MonadGen m => MonadGen (MaybeT m) where
  type GenBase (MaybeT m) =
    MaybeT (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance MonadGen m => MonadGen (ExceptT x m) where
  type GenBase (ExceptT x m) =
    ExceptT x (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance MonadGen m => MonadGen (ReaderT r m) where
  type GenBase (ReaderT r m) =
    ReaderT r (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance MonadGen m => MonadGen (Lazy.StateT r m) where
  type GenBase (Lazy.StateT r m) =
    Lazy.StateT r (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance MonadGen m => MonadGen (Strict.StateT r m) where
  type GenBase (Strict.StateT r m) =
    Strict.StateT r (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance (MonadGen m, Monoid w) => MonadGen (Lazy.WriterT w m) where
  type GenBase (Lazy.WriterT w m) =
    Lazy.WriterT w (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

instance (MonadGen m, Monoid w) => MonadGen (Strict.WriterT w m) where
  type GenBase (Strict.WriterT w m) =
    Strict.WriterT w (GenBase m)

  toGenT =
    distributeT . hoist toGenT

  fromGenT =
    hoist fromGenT . distributeT

------------------------------------------------------------------------
-- GenT instances

instance (Monad m, Semigroup a) => Semigroup (GenT m a) where
  (<>) =
    liftA2 (Semigroup.<>)

instance (
  Monad m, Monoid a
#if !MIN_VERSION_base(4,11,0)
  , Semigroup a
#endif
         ) => Monoid (GenT m a) where
#if !MIN_VERSION_base(4,11,0)
  mappend = (Semigroup.<>)
#endif

  mempty =
    return mempty

instance Functor m => Functor (GenT m) where
  fmap f gen =
    GenT $ \seed size ->
      fmap f (runGenT seed size gen)

--
-- implementation: parallel shrinking
--
instance Monad m => Applicative (GenT m) where
  pure =
    fromTreeMaybeT . pure

  (<*>) f m =
    GenT $ \ size seed ->
      case Seed.split seed of
        (sf, sm) ->
          uncurry ($) <$>
            runGenT size sf f `mzip`
            runGenT size sm m

--
-- implementation: satisfies law (ap = <*>)
--
--instance Monad m => Applicative (GenT m) where
--  pure =
--    fromTreeMaybeT . pure
--  (<*>) f m =
--    GenT $ \ size seed ->
--      case Seed.split seed of
--        (sf, sm) ->
--          runGenT size sf f <*>
--          runGenT size sm m

instance Monad m => Monad (GenT m) where
  return =
    pure

  (>>=) m k =
    GenT $ \size seed ->
      case Seed.split seed of
        (sk, sm) ->
          runGenT size sk . k =<<
          runGenT size sm m

#if __GLASGOW_HASKELL__ < 808
  fail =
    Fail.fail
#endif

instance Monad m => MonadFail (GenT m) where
  fail =
    error

instance Monad m => Alternative (GenT m) where
  empty =
    mzero

  (<|>) =
    mplus

instance Monad m => MonadPlus (GenT m) where
  mzero =
    fromTreeMaybeT mzero

  mplus x y =
    GenT $ \size seed ->
      case Seed.split seed of
        (sx, sy) ->
          runGenT size sx x `mplus`
          runGenT size sy y

instance MonadTrans GenT where
  lift =
    fromTreeMaybeT . lift . lift

instance MFunctor GenT where
  hoist f =
    mapGenT (hoist (hoist f))

embedMaybeT ::
     MonadTrans t
  => Monad n
  => Monad (t (MaybeT n))
  => (forall a. m a -> t (MaybeT n) a)
  -> MaybeT m b
  -> t (MaybeT n) b
embedMaybeT f m =
  lift . MaybeT . pure =<< f (runMaybeT m)

embedTreeMaybeT ::
     Monad n
  => (forall a. m a -> TreeT (MaybeT n) a)
  -> TreeT (MaybeT m) b
  -> TreeT (MaybeT n) b
embedTreeMaybeT f tree_ =
  embed (embedMaybeT f) tree_

embedGenT ::
     Monad n
  => (forall a. m a -> GenT n a)
  -> GenT m b
  -> GenT n b
embedGenT f gen =
  GenT $ \size seed ->
    case Seed.split seed of
      (sf, sg) ->
        (runGenT size sf . f) `embedTreeMaybeT`
        (runGenT size sg gen)

instance MMonad GenT where
  embed =
    embedGenT

distributeGenT :: Transformer t GenT m => GenT (t m) a -> t (GenT m) a
distributeGenT x =
  join . lift . GenT $ \size seed ->
    pure . hoist fromTreeMaybeT . distributeT . hoist distributeT $ runGenT size seed x

instance MonadTransDistributive GenT where
  type Transformer t GenT m = (
      Monad (t (GenT m))
    , Transformer t MaybeT m
    , Transformer t TreeT (MaybeT m)
    )

  distributeT =
    distributeGenT

instance PrimMonad m => PrimMonad (GenT m) where
  type PrimState (GenT m) =
    PrimState m

  primitive =
    lift . primitive

instance MonadIO m => MonadIO (GenT m) where
  liftIO =
    lift . liftIO

instance MonadBase b m => MonadBase b (GenT m) where
  liftBase =
    lift . liftBase

#if __GLASGOW_HASKELL__ >= 806
deriving via (ReaderT Size (ReaderT Seed (TreeT (MaybeT m))))
  instance MonadBaseControl b m => MonadBaseControl b (GenT m)
#else
instance MonadBaseControl b m => MonadBaseControl b (GenT m) where
  type StM (GenT m) a = StM (GloopT m) a
  liftBaseWith g = gloopToGen $ liftBaseWith $ \q -> g (\gen -> q (genToGloop gen))
  restoreM = gloopToGen . restoreM

type GloopT m = ReaderT Size (ReaderT Seed (TreeT (MaybeT m)))

gloopToGen :: GloopT m a -> GenT m a
gloopToGen = coerce

genToGloop :: GenT m a -> GloopT m a
genToGloop = coerce
#endif

instance MonadThrow m => MonadThrow (GenT m) where
  throwM =
    lift . throwM

instance MonadCatch m => MonadCatch (GenT m) where
  catch m onErr =
    GenT $ \size seed ->
      case Seed.split seed of
        (sm, se) ->
          (runGenT size sm m) `catch`
          (runGenT size se . onErr)

instance MonadReader r m => MonadReader r (GenT m) where
  ask =
    lift ask
  local f m =
    mapGenT (local f) m

instance MonadState s m => MonadState s (GenT m) where
  get =
    lift get
  put =
    lift . put
  state =
    lift . state

instance MonadWriter w m => MonadWriter w (GenT m) where
  writer =
    lift . writer
  tell =
    lift . tell
  listen m =
    GenT $ \size seed ->
      listen $ runGenT size seed m
  pass m =
    GenT $ \size seed ->
      pass $ runGenT size seed m

instance MonadError e m => MonadError e (GenT m) where
  throwError =
    lift . throwError
  catchError m onErr =
    GenT $ \size seed ->
      case Seed.split seed of
        (sm, se) ->
          (runGenT size sm m) `catchError`
          (runGenT size se . onErr)

instance MonadResource m => MonadResource (GenT m) where
  liftResourceT =
    lift . liftResourceT

------------------------------------------------------------------------
-- Combinators

-- | Generate a value with no shrinks from a 'Size' and a 'Seed'.
--
generate :: MonadGen m => (Size -> Seed -> a) -> m a
generate f =
  fromGenT . GenT $ \size seed ->
    pure (f size seed)

------------------------------------------------------------------------
-- Combinators - Shrinking

-- | Apply a shrinking function to a generator.
--
--   This will give the generator additional shrinking options, while keeping
--   the existing shrinks intact.
--
shrink :: MonadGen m => (a -> [a]) -> m a -> m a
shrink f =
  withGenT $ mapGenT (Tree.expand f)

-- | Throw away a generator's shrink tree.
--
prune :: MonadGen m => m a -> m a
prune =
  withGenT $ mapGenT (Tree.prune 0)

------------------------------------------------------------------------
-- Combinators - Size

-- | Construct a generator that depends on the size parameter.
--
sized :: MonadGen m => (Size -> m a) -> m a
sized f = do
  f =<< generate (\size _ -> size)

-- | Override the size parameter. Returns a generator which uses the given size
--   instead of the runtime-size parameter.
--
resize :: MonadGen m => Size -> m a -> m a
resize size gen =
  scale (const size) gen

-- | Adjust the size parameter by transforming it with the given function.
--
scale :: MonadGen m => (Size -> Size) -> m a -> m a
scale f =
  withGenT $ \gen ->
    GenT $ \size0 seed ->
      let
        size =
          f size0
      in
        if size < 0 then
          error "Hedgehog.Gen.scale: negative size"
        else
          runGenT size seed gen

-- | Make a generator smaller by scaling its size parameter.
--
small :: MonadGen m => m a -> m a
small =
  scale golden

-- | Scale a size using the golden ratio.
--
--   > golden x = x / φ
--   > golden x = x / 1.61803..
--
golden :: Size -> Size
golden x =
  round (fromIntegral x * 0.61803398875 :: Double)

------------------------------------------------------------------------
-- Combinators - Integral

-- | Generates a random integral number in the given @[inclusive,inclusive]@ range.
--
--   When the generator tries to shrink, it will shrink towards the
--   'Range.origin' of the specified 'Range'.
--
--   For example, the following generator will produce a number between @1970@
--   and @2100@, but will shrink towards @2000@:
--
-- @
-- integral (Range.'Range.constantFrom' 2000 1970 2100) :: 'Gen' 'Int'
-- @
--
--   Some sample outputs from this generator might look like:
--
--   > === Outcome ===
--   > 1973
--   > === Shrinks ===
--   > 2000
--   > 1987
--   > 1980
--   > 1976
--   > 1974
--
--   > === Outcome ===
--   > 2061
--   > === Shrinks ===
--   > 2000
--   > 2031
--   > 2046
--   > 2054
--   > 2058
--   > 2060
--
integral :: forall m a. (MonadGen m, Integral a) => Range a -> m a
integral range =
  -- https://github.com/hedgehogqa/haskell-hedgehog/pull/413/files
  let
    origin_ =
      Range.origin range

    binarySearchTree bottom top =
      Tree.Tree $
        let
          shrinks =
            Shrink.towards bottom top
          children =
            zipWith binarySearchTree shrinks (drop 1 shrinks)
        in
          Tree.NodeT top children

    createTree root =
      if root == origin_ then
        pure root
      else
        hoist Morph.generalize $
          Tree.consChild origin_ $
            binarySearchTree origin_ root

  in
    fromGenT . GenT $ \size seed ->
      createTree $ integralHelper range size seed

-- | Generates a random integral number in the [inclusive,inclusive] range.
--
--   /This generator does not shrink./
--
integral_ :: (MonadGen m, Integral a) => Range a -> m a
integral_ =
  generate . integralHelper


-- | Generates a random integral value from a range.
integralHelper :: (Integral a, Num c) => Range a -> Size -> Seed -> c
integralHelper range size seed =
  let
    (x, y) =
      Range.bounds size range
  in
    fromInteger . fst $
      Seed.nextInteger (toInteger x) (toInteger y) seed


-- | Generates a random machine integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int :: MonadGen m => Range Int -> m Int
int =
  integral

-- | Generates a random 8-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int8 :: MonadGen m => Range Int8 -> m Int8
int8 =
  integral

-- | Generates a random 16-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int16 :: MonadGen m => Range Int16 -> m Int16
int16 =
  integral

-- | Generates a random 32-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int32 :: MonadGen m => Range Int32 -> m Int32
int32 =
  integral

-- | Generates a random 64-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int64 :: MonadGen m => Range Int64 -> m Int64
int64 =
  integral

-- | Generates a random machine word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word :: MonadGen m => Range Word -> m Word
word =
  integral

-- | Generates a random byte in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word8 :: MonadGen m => Range Word8 -> m Word8
word8 =
  integral

-- | Generates a random 16-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word16 :: MonadGen m => Range Word16 -> m Word16
word16 =
  integral

-- | Generates a random 32-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word32 :: MonadGen m => Range Word32 -> m Word32
word32 =
  integral

-- | Generates a random 64-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word64 :: MonadGen m => Range Word64 -> m Word64
word64 =
  integral

------------------------------------------------------------------------
-- Combinators - Fractional / Floating-Point

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This generator works the same as 'integral', but for floating point numbers./
--
realFloat :: (MonadGen m, RealFloat a) => Range a -> m a
realFloat range =
  shrink (Shrink.towardsFloat $ Range.origin range) (realFrac_ range)

-- | Generates a random fractional number in the [inclusive,exclusive) range.
--
--   /This generator does not shrink./
--
realFrac_ :: (MonadGen m, RealFrac a) => Range a -> m a
realFrac_ range =
  generate $ \size seed ->
    let
      (x, y) =
        Range.bounds size range
    in
      realToFrac . fst $
        Seed.nextDouble (realToFrac x) (realToFrac y) seed

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This is a specialization of 'realFloat', offered for convenience./
--
float :: MonadGen m => Range Float -> m Float
float =
 realFloat

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This is a specialization of 'realFloat', offered for convenience./
--
double :: MonadGen m => Range Double -> m Double
double =
 realFloat

------------------------------------------------------------------------
-- Combinators - Enumeration

-- | Generates an element from an enumeration.
--
--   This generator shrinks towards the first argument.
--
--   For example:
--
-- @
-- enum \'a' \'z' :: 'Gen' 'Char'
-- @
--
enum :: (MonadGen m, Enum a) => a -> a -> m a
enum lo hi =
  fmap toEnum . integral $
    Range.constant (fromEnum lo) (fromEnum hi)

-- | Generates a random value from a bounded enumeration.
--
--   This generator shrinks towards 'minBound'.
--
--   For example:
--
-- @
-- enumBounded :: 'Gen' 'Bool'
-- @
--
--   /This is implemented in terms of the 'Enum' class, and thus may be/
--   /partial for integral types larger than 'Int', e.g. 'Word64'./
enumBounded :: (MonadGen m, Enum a, Bounded a) => m a
enumBounded =
  enum minBound maxBound

-- | Generates a random boolean.
--
--   This generator shrinks to 'False'.
--
--   /This is a specialization of 'enumBounded', offered for convenience./
--
bool :: MonadGen m => m Bool
bool =
  enumBounded

-- | Generates a random boolean.
--
--   /This generator does not shrink./
--
bool_ :: MonadGen m => m Bool
bool_ =
  generate $ \_ seed ->
    (/= 0) . fst $ Seed.nextInteger 0 1 seed

------------------------------------------------------------------------
-- Combinators - Characters

-- | Generates an ASCII binit: @'0'..'1'@
--
binit :: MonadGen m => m Char
binit =
  enum '0' '1'

-- | Generates an ASCII octit: @'0'..'7'@
--
octit :: MonadGen m => m Char
octit =
  enum '0' '7'

-- | Generates an ASCII digit: @'0'..'9'@
--
digit :: MonadGen m => m Char
digit =
  enum '0' '9'

-- | Generates an ASCII hexit: @'0'..'9', \'a\'..\'f\', \'A\'..\'F\'@
--
hexit :: MonadGen m => m Char
hexit =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "0123456789aAbBcCdDeEfF"

-- | Generates an ASCII lowercase letter: @\'a\'..\'z\'@
--
lower :: MonadGen m => m Char
lower =
  enum 'a' 'z'

-- | Generates an ASCII uppercase letter: @\'A\'..\'Z\'@
--
upper :: MonadGen m => m Char
upper =
  enum 'A' 'Z'

-- | Generates an ASCII letter: @\'a\'..\'z\', \'A\'..\'Z\'@
--
alpha :: MonadGen m => m Char
alpha =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Generates an ASCII letter or digit: @\'a\'..\'z\', \'A\'..\'Z\', \'0\'..\'9\'@
--
alphaNum :: MonadGen m => m Char
alphaNum =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

-- | Generates an ASCII character: @'\0'..'\127'@
--
ascii :: MonadGen m => m Char
ascii =
  enum '\0' '\127'

-- | Generates a Latin-1 character: @'\0'..'\255'@
--
latin1 :: MonadGen m => m Char
latin1 =
  enum '\0' '\255'

-- | Generates a Unicode character, excluding noncharacters and invalid standalone surrogates:
--   @'\0'..'\1114111' (excluding '\55296'..'\57343', '\65534', '\65535')@
--
unicode :: (MonadGen m) => m Char
unicode =
  let
    s1 =
      (55296, enum '\0' '\55295')
    s2 =
      (8190, enum '\57344' '\65533')
    s3 =
      (1048576, enum '\65536' '\1114111')
  in
    frequency [s1, s2, s3]

-- | Generates a Unicode character, including noncharacters and invalid standalone surrogates:
--   @'\0'..'\1114111'@
--
unicodeAll :: MonadGen m => m Char
unicodeAll =
  enumBounded

-- | Check if a character is in the surrogate category.
--
isSurrogate :: Char -> Bool
isSurrogate x =
  x >= '\55296' && x <= '\57343'

-- | Check if a character is one of the noncharacters '\65534', '\65535'.
--
isNoncharacter :: Char -> Bool
isNoncharacter x =
  x == '\65534' || x == '\65535'

------------------------------------------------------------------------
-- Combinators - Strings

-- | Generates a string using 'Range' to determine the length.
--
--   /This is a specialization of 'list', offered for convenience./
--
string :: MonadGen m => Range Int -> m Char -> m String
string =
  list

-- | Generates a string using 'Range' to determine the length.
--
text :: MonadGen m => Range Int -> m Char -> m Text
text range =
  fmap Text.pack . string range

-- | Generates a UTF-8 encoded string, using 'Range' to determine the length.
--
utf8 :: MonadGen m => Range Int -> m Char -> m ByteString
utf8 range =
  fmap Text.encodeUtf8 . text range

-- | Generates a random 'ByteString', using 'Range' to determine the
--   length.
--
bytes :: MonadGen m => Range Int -> m ByteString
bytes range =
  fmap ByteString.pack $
  choice [
      list range . word8 $
        Range.constant
          (fromIntegral $ Char.ord 'a')
          (fromIntegral $ Char.ord 'z')

    , list range . word8 $
        Range.constant minBound maxBound
    ]

------------------------------------------------------------------------
-- Combinators - Choice

-- | Trivial generator that always produces the same element.
--
--   /This is another name for 'pure' \/ 'return'./
constant :: MonadGen m => a -> m a
constant =
  pure

-- | Randomly selects one of the elements in the list.
--
--   This generator shrinks towards the first element in the list.
--
--   /The input list must be non-empty./
--
element :: (Foldable f, MonadGen m) => f a -> m a
element fa = case toList fa of
  [] ->
    error "Hedgehog.Gen.element: used with empty Foldable"
  xs -> do
    n <- integral $ Range.constant 0 (length xs - 1)
    pure $ xs !! n

-- | Randomly selects one of the elements in the list.
--
--   This generator does not shrink the choice of element.
--
--   /The input list must be non-empty./
--
element_ :: MonadGen m => [a] -> m a
element_ = \case
  [] ->
    error "Hedgehog.Gen.element: used with empty list"
  xs -> do
    n <- integral_ $ Range.constant 0 (length xs - 1)
    pure $ xs !! n

-- | Randomly selects one of the generators in the list.
--
--   This generator shrinks towards the first generator in the list.
--
--   /The input list must be non-empty./
--
choice :: MonadGen m => [m a] -> m a
choice = \case
  [] ->
    error "Hedgehog.Gen.choice: used with empty list"
  xs -> do
    n <- integral $ Range.constant 0 (length xs - 1)
    xs !! n

-- | Uses a weighted distribution to randomly select one of the generators in
--   the list.
--
--   This generator shrinks towards the first generator in the list.
--
--   /The input list must be non-empty./
--
frequency :: MonadGen m => [(Int, m a)] -> m a
frequency = \case
  [] ->
    error "Hedgehog.Gen.frequency: used with empty list"
  xs0 -> do
    let
      pick n = \case
        [] ->
          error "Hedgehog.Gen.frequency/pick: used with empty list"
        (k, x) : xs ->
          if n <= k then
            x
          else
            pick (n - k) xs

      iis =
        scanl1 (+) (fmap fst xs0)

      total =
        sum (fmap fst xs0)

    n <- shrink (\n -> takeWhile (< n) iis) $ integral_ $ Range.constant 1 total
    pick n xs0

-- | Modifies combinators which choose from a list of generators, like 'choice'
--   or 'frequency', so that they can be used in recursive scenarios.
--
--   This combinator modifies its target to select one of the generators in
--   either the non-recursive or the recursive list. When a selection is made
--   from the recursive list, the 'Size' is halved. When the 'Size' gets to one
--   or less, selections are no longer made from the recursive list, this
--   ensures termination.
--
--   A good example of where this might be useful is abstract syntax trees:
--
-- @
-- data Expr =
--     Var String
--   | Lam String Expr
--   | App Expr Expr
--
-- -- Assuming we have a name generator
-- genName :: 'MonadGen' m => m String
--
-- -- We can write a generator for expressions
-- genExpr :: 'MonadGen' m => m Expr
-- genExpr =
--   Gen.'recursive' Gen.'choice' [
--       -- non-recursive generators
--       Var '<$>' genName
--     ] [
--       -- recursive generators
--       Gen.'subtermM' genExpr (\x -> Lam '<$>' genName '<*>' pure x)
--     , Gen.'subterm2' genExpr genExpr App
--     ]
-- @
--
--   If we wrote the above example using only 'choice', it is likely that it
--   would fail to terminate. This is because for every call to @genExpr@,
--   there is a 2 in 3 chance that we will recurse again.
--
recursive :: MonadGen m => ([m a] -> m a) -> [m a] -> [m a] -> m a
recursive f nonrec rec =
  sized $ \n ->
    if n <= 1 then
      f nonrec
    else
      f $ nonrec ++ fmap small rec

------------------------------------------------------------------------
-- Combinators - Conditional

-- | Discards the whole generator.
--
discard :: MonadGen m => m a
discard =
  fromGenT empty

-- | Discards the generator if the generated value does not satisfy the
--   predicate.
--
ensure :: MonadGen m => (a -> Bool) -> m a -> m a
ensure p gen = do
  x <- gen
  if p x then
    pure x
  else
    discard

fromPred :: (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)

-- | Generates a value that satisfies a predicate.
--
--   Shrinks of the generated value will also satisfy the predicate. From the
--   original generator's shrink tree, any values that fail the predicate will
--   be removed, but any subsequent shrinks that satisfy it will be retained.
--   Compared to 'filter', shrinking may be slower but will be optimal.
--
--   It's possible that the predicate will never pass, or will only pass at a
--   larger size than we're currently running at. To avoid looping forever, we
--   limit the number of retries, and grow the size with each retry. If we retry
--   too many times then the whole generator is discarded.
--
filter :: (MonadGen m, GenBase m ~ Identity) => (a -> Bool) -> m a -> m a
filter p =
  mapMaybe (fromPred p)

-- | Generates a value which is the result of the given function returning a
--   'Just'.
--
--   The original generator's shrink tree will be retained, with values
--   returning 'Nothing' removed. Subsequent shrinks of those values will be
--   retained. Compared to 'mapMaybeT', shrinking may be slower but will be
--   optimal.
--
--   It's possible that the function will never return 'Just', or will only do
--   so a larger size than we're currently running at. To avoid looping forever,
--   we limit the number of retries, and grow the size with each retry. If we
--   retry too many times then the whole generator is discarded.
--
mapMaybe :: (MonadGen m, GenBase m ~ Identity) => (a -> Maybe b) -> m a -> m b
mapMaybe p gen0 =
  let
    try k =
      if k > 100 then
        discard
      else do
        (x, gen) <- freeze $ scale (2 * k +) gen0

        case p x of
          Just _ ->
            withGenT (mapGenT (Tree.mapMaybeMaybeT p)) gen
          Nothing ->
            try (k + 1)
  in
    try 0

-- | Generates a value that satisfies a predicate.
--
--   Shrinks of the generated value will also satisfy the predicate. From the
--   original generator's shrink tree, any values that fail the predicate will
--   be removed, along with their subsequent shrinks. Compared to 'filter',
--   shrinking may be faster but may also be less optimal.
--
--   The type is also more general, because the shrink behavior from 'filter'
--   would force the entire shrink tree to be evaluated when applied to an
--   impure tree.
--
--   This is essentially:
--
-- @
--   filterT p gen = 'mfilter' p gen '<|>' filterT p gen
-- @
--
--   But that could loop forever, if the predicate will never pass or will only
--   pass at a larger size than we're currently running at. We differ from the
--   above in keeping some state to avoid that. We limit the number of retries,
--   and grow the size with each retry. If we retry too many times then the
--   whole generator is discarded.
--
filterT :: MonadGen m => (a -> Bool) -> m a -> m a
filterT p =
  mapMaybeT (fromPred p)

-- | Generates a value which is the result of the given function returning a
--   'Just'.
--
--   The original generator's shrink tree will be retained, with values
--   returning 'Nothing' removed. Subsequent shrinks of those values will be
--   retained. Compared to 'mapMaybeT', shrinking may be slower but will be
--   optimal.
--
--   The type is also more general, because the shrink behavior from 'mapMaybe'
--   would force the entire shrink tree to be evaluated when applied to an
--   impure tree.
--
--   It's possible that the function will never return 'Just', or will only do
--   so a larger size than we're currently running at. To avoid looping forever,
--   we limit the number of retries, and grow the size with each retry. If we
--   retry too many times then the whole generator is discarded.
--
mapMaybeT :: MonadGen m => (a -> Maybe b) -> m a -> m b
mapMaybeT p gen0 =
  let
    try k =
      if k > 100 then
        discard
      else do
        (x, gen) <- freeze $ scale (2 * k +) gen0

        case p x of
          Just _ ->
            withGenT (mapGenT (Tree.mapMaybeT p)) gen
          Nothing ->
            try (k + 1)
  in
    try 0

-- | Runs a 'Maybe' generator until it produces a 'Just'.
--
--   /This is implemented using 'filter' and has the same caveats./
--
just :: (MonadGen m, GenBase m ~ Identity) => m (Maybe a) -> m a
just g = do
  mx <- filter Maybe.isJust g
  case mx of
    Just x ->
      pure x
    Nothing ->
      error "Hedgehog.Gen.just: internal error, unexpected Nothing"

-- | Runs a 'Maybe' generator until it produces a 'Just'.
--
--   /This is implemented using 'filter' and has the same caveats./
--
justT :: MonadGen m => m (Maybe a) -> m a
justT g = do
  mx <- filterT Maybe.isJust g
  case mx of
    Just x ->
      pure x
    Nothing ->
      error "Hedgehog.Gen.just: internal error, unexpected Nothing"

------------------------------------------------------------------------
-- Combinators - Collections

-- | Generates a 'Nothing' some of the time.
--
maybe :: MonadGen m => m a -> m (Maybe a)
maybe gen =
  sized $ \n ->
    frequency [
        (2, pure Nothing)
      , (1 + fromIntegral n, Just <$> gen)
      ]

-- | Generates either an 'a' or a 'b'.
--
--   As the size grows, this generator generates @Right@s more often than @Left@s.
--
either :: MonadGen m => m a -> m b -> m (Either a b)
either genA genB =
  sized $ \n ->
    frequency [
        (2, Left <$> genA)
      , (1 + fromIntegral n, Right <$> genB)
      ]

-- | Generates either an 'a' or a 'b', without bias.
--
--   This generator generates as many @Right@s as it does @Left@s.
--
either_ :: MonadGen m => m a -> m b -> m (Either a b)
either_ genA genB =
    choice [
      Left <$> genA
    , Right <$> genB
    ]

-- | Generates a list using a 'Range' to determine the length.
--
list :: MonadGen m => Range Int -> m a -> m [a]
list range gen =
  let
     interleave =
       (interleaveTreeT . nodeValue =<<)
  in
    sized $ \size ->
      ensure (atLeast $ Range.lowerBound size range) .
      withGenT (mapGenT (TreeT . interleave . runTreeT)) $ do
        n <- integral_ range
        replicateM n (toTreeMaybeT gen)

interleaveTreeT :: Monad m => [TreeT m a] -> m (NodeT m [a])
interleaveTreeT =
  fmap Tree.interleave . traverse runTreeT

-- | Generates a seq using a 'Range' to determine the length.
--
seq :: MonadGen m => Range Int -> m a -> m (Seq a)
seq range gen =
  Seq.fromList <$> list range gen

-- | Generates a non-empty list using a 'Range' to determine the length.
--
nonEmpty :: MonadGen m => Range Int -> m a -> m (NonEmpty a)
nonEmpty range gen = do
  xs <- list (fmap (max 1) range) gen
  case xs of
    [] ->
      error "Hedgehog.Gen.nonEmpty: internal error, generated empty list"
    _ ->
      pure $ NonEmpty.fromList xs

-- | Generates a set using a 'Range' to determine the length.
--
--   /This may fail to generate anything if the element generator/
--   /cannot produce a large enough number of unique items to satify/
--   /the required set size./
--
set :: (MonadGen m, Ord a) => Range Int -> m a -> m (Set a)
set range gen =
  fmap Map.keysSet . map range $ fmap (, ()) gen

-- | Generates a map using a 'Range' to determine the length.
--
--   /This may fail to generate anything if the keys produced by the/
--   /generator do not account for a large enough number of unique/
--   /items to satify the required map size./
--
map :: (MonadGen m, Ord k) => Range Int -> m (k, v) -> m (Map k v)
map range gen =
  sized $ \size ->
    ensure ((>= Range.lowerBound size range) . Map.size) .
    fmap Map.fromList .
    (sequence =<<) .
    shrink Shrink.list $ do
      k <- integral_ range
      uniqueByKey k gen

-- | Generate exactly 'n' unique generators.
--
uniqueByKey :: (MonadGen m, Ord k) => Int -> m (k, v) -> m [m (k, v)]
uniqueByKey n gen =
  let
    try k xs0 =
      if k > 100 then
        discard
      else
        replicateM n (freeze gen) >>= \kvs ->
        case uniqueInsert n xs0 (fmap (first fst) kvs) of
          Left xs ->
            pure $ Map.elems xs
          Right xs ->
            try (k + 1) xs
  in
    try (0 :: Int) Map.empty

uniqueInsert :: Ord k => Int -> Map k v -> [(k, v)] -> Either (Map k v) (Map k v)
uniqueInsert n xs kvs0 =
  if Map.size xs >= n then
    Left xs
  else
    case kvs0 of
      [] ->
        Right xs
      (k, v) : kvs ->
        uniqueInsert n (Map.insertWith (\x _ -> x) k v xs) kvs

-- | Check that list contains at least a certain number of elements.
--
atLeast :: Int -> [a] -> Bool
atLeast n =
  if n == 0 then
    const True
  else
    not . null . drop (n - 1)

------------------------------------------------------------------------
-- Combinators - Subterms

data Subterms n a =
    One a
  | All (Vec n a)
    deriving (Functor, Foldable, Traversable)

data Nat =
    Z
  | S Nat

data Vec n a where
  Nil :: Vec 'Z a
  (:.) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :.

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)
deriving instance Traversable (Vec n)

-- | Freeze the size and seed used by a generator, so we can inspect the value
--   which it will produce.
--
--   This is used for implementing `list` and `subtermMVec`. It allows us to
--   shrink the list itself before trying to shrink the values inside the list.
--
freeze :: MonadGen m => m a -> m (a, m a)
freeze =
  withGenT $ \gen ->
    GenT $ \size seed -> do
      mx <- lift . lift . runMaybeT . runTreeT $ runGenT size seed gen
      case mx of
        Nothing ->
          empty
        Just (NodeT x xs) ->
          pure (x, fromGenT . fromTreeMaybeT . Tree.fromNodeT $ NodeT x xs)

shrinkSubterms :: Subterms n a -> [Subterms n a]
shrinkSubterms = \case
  One _ ->
    []
  All xs ->
    fmap One $ toList xs

genSubterms :: MonadGen m => Vec n (m a) -> m (Subterms n a)
genSubterms =
  (sequence =<<) .
  shrink shrinkSubterms .
  fmap All .
  mapM (fmap snd . freeze)

fromSubterms :: Applicative m => (Vec n a -> m a) -> Subterms n a -> m a
fromSubterms f = \case
  One x ->
    pure x
  All xs ->
    f xs

-- | Constructs a generator from a number of sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermMVec :: MonadGen m => Vec n (m a) -> (Vec n a -> m a) -> m a
subtermMVec gs f =
  fromSubterms f =<< genSubterms gs

-- | Constructs a generator from a sub-term generator.
--
--   /Shrinks to the sub-term if possible./
--
subtermM :: MonadGen m => m a -> (a -> m a) -> m a
subtermM gx f =
  subtermMVec (gx :. Nil) $ \(x :. Nil) ->
    f x

-- | Constructs a generator from a sub-term generator.
--
--   /Shrinks to the sub-term if possible./
--
subterm :: MonadGen m => m a -> (a -> a) -> m a
subterm gx f =
  subtermM gx $ \x ->
    pure (f x)

-- | Constructs a generator from two sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermM2 :: MonadGen m => m a -> m a -> (a -> a -> m a) -> m a
subtermM2 gx gy f =
  subtermMVec (gx :. gy :. Nil) $ \(x :. y :. Nil) ->
    f x y

-- | Constructs a generator from two sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subterm2 :: MonadGen m => m a -> m a -> (a -> a -> a) -> m a
subterm2 gx gy f =
  subtermM2 gx gy $ \x y ->
    pure (f x y)

-- | Constructs a generator from three sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermM3 :: MonadGen m => m a -> m a -> m a -> (a -> a -> a -> m a) -> m a
subtermM3 gx gy gz f =
  subtermMVec (gx :. gy :. gz :. Nil) $ \(x :. y :. z :. Nil) ->
    f x y z

-- | Constructs a generator from three sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subterm3 :: MonadGen m => m a -> m a -> m a -> (a -> a -> a -> a) -> m a
subterm3 gx gy gz f =
  subtermM3 gx gy gz $ \x y z ->
    pure (f x y z)

------------------------------------------------------------------------
-- Combinators - Combinations & Permutations

-- | Generates a random subsequence of a list.
--
-- For example:
--
-- @
-- Gen.print (Gen.subsequence [1..5])
-- @
--
--   > === Outcome ===
--   > [1,2,4]
--   > === Shrinks ===
--   > []
--   > [2,4]
--   > [1,4]
--   > [1,2]
--
subsequence :: MonadGen m => [a] -> m [a]
subsequence xs =
  shrink Shrink.list $ filterM (const bool_) xs

-- | Generates a random subset of a set.
--
--  /This shrinks towards the empty set./
--
subset :: MonadGen m => Set a -> m (Set a)
-- Set.fromDistinctAscList has an unchecked precondition that the list
-- must be strictly ascending. This precondition is satisfied because
-- Set.toAscList produces a strictly ascending list, and the 'subsequence'
-- generator only removes elements from the list; it never adds or
-- rearranges elements, so the strictly ascending property is undisturbed.
subset =
  fmap Set.fromDistinctAscList . subsequence . Set.toAscList

-- | Generates a random permutation of a list.
--
--   /This shrinks towards the order of the list being identical to the input/
--   /list./
--
shuffle :: MonadGen m => [a] -> m [a]
-- We shuffle sequences instead of lists to make extracting an arbitrary
-- element logarithmic instead of linear, and to make length calculation
-- constant-time instead of linear. We could probably do better, but
-- this is at least reasonably quick.
shuffle = fmap toList . shuffleSeq . Seq.fromList

-- | Generates a random permutation of a sequence.
--
--   /This shrinks towards the order of the sequence being identical to the input/
--   /sequence./
--
shuffleSeq :: MonadGen m => Seq a -> m (Seq a)
shuffleSeq xs =
  if null xs then
    pure Seq.empty
  else do
    n <- integral $ Range.constant 0 (length xs - 1)
#if MIN_VERSION_containers(0,5,8)
    -- Data.Sequence should offer a version of deleteAt that returns the
    -- deleted element, but it does not currently do so. Lookup followed
    -- by deletion seems likely faster than splitting and then appending,
    -- but I haven't actually tested that. It's certainly easier to see
    -- what's going on.
    case Seq.lookup n xs of
      Just y ->
        (y Seq.<|) <$> shuffleSeq (Seq.deleteAt n xs)
      Nothing ->
        error "Hedgehog.Gen.shuffleSeq: internal error, lookup in empty sequence"
#else
    case Seq.splitAt n xs of
      (beginning, end) ->
        case Seq.viewl end of
          y Seq.:< end' ->
            (y Seq.<|) <$> shuffleSeq (beginning Seq.>< end')
          Seq.EmptyL ->
            error "Hedgehog.Gen.shuffleSeq: internal error, lookup in empty sequence"
#endif

------------------------------------------------------------------------
-- Sampling

-- | Generate a sample from a generator.
--
-- This function is useful for examining a 'Gen' in GHCi or other contexts.
-- It is not appropriate for use in a test suite directly. You will only
-- get a single sample from this function, and it will not give you
-- a property test. The seed is random, so the test is not deterministic.
--
-- If you only want a single test to run, then use @'withTests' 1@:
--
-- @
-- prop_OnlyRunOnce :: Property
-- prop_OnlyRunOnce =
--   'withTests' 1 $ 'property' $ do
--     i <- Gen.int
--     i /== 0
-- @
sample :: MonadIO m => Gen a -> m a
sample gen =
  liftIO $
    let
      loop n =
        if n <= 0 then
          error "Hedgehog.Gen.sample: too many discards, could not generate a sample"
        else do
          seed <- Seed.random
          case evalGen 30 seed gen of
            Nothing ->
              loop (n - 1)
            Just x ->
              pure $ Tree.treeValue x
    in
      loop (100 :: Int)

-- | Run a generator with a random seed and print the outcome, and the first
--   level of shrinks.
--
-- @
-- Gen.print (Gen.'enum' \'a\' \'f\')
-- @
--
--   > === Outcome ===
--   > 'd'
--   > === Shrinks ===
--   > 'a'
--   > 'b'
--   > 'c'
--
print :: (MonadIO m, Show a) => Gen a -> m ()
print gen = do
  seed <- liftIO Seed.random
  printWith 30 seed gen

-- | Print the value produced by a generator, and the first level of shrinks,
--   for the given size and seed.
--
--   Use 'print' to generate a value from a random seed.
--
printWith :: (MonadIO m, Show a) => Size -> Seed -> Gen a -> m ()
printWith size seed gen =
  liftIO $ do
    case evalGen size seed gen of
      Nothing -> do
        putStrLn "=== Outcome ==="
        putStrLn "<discard>"

      Just tree_ -> do
        let
          NodeT x ss =
            runIdentity (runTreeT tree_)

        putStrLn "=== Outcome ==="
        putStrLn (show x)
        putStrLn "=== Shrinks ==="

        for_ ss $ \s ->
          let
            NodeT y _ =
              runIdentity $ runTreeT s
          in
            putStrLn (show y)

-- | Run a generator with a random seed and print the resulting shrink tree.
--
-- @
-- Gen.printTree (Gen.'enum' \'a\' \'f\')
-- @
--
--   > 'd'
--   >  ├╼'a'
--   >  ├╼'b'
--   >  │  └╼'a'
--   >  └╼'c'
--   >     ├╼'a'
--   >     └╼'b'
--   >        └╼'a'
--
--   /This may not terminate when the tree is very large./
--
printTree :: (MonadIO m, Show a) => Gen a -> m ()
printTree gen = do
  seed <- liftIO Seed.random
  printTreeWith 30 seed gen

-- | Print the shrink tree produced by a generator, for the given size and
--   seed.
--
--   Use 'printTree' to generate a value from a random seed.
--
printTreeWith :: (MonadIO m, Show a) => Size -> Seed -> Gen a -> m ()
printTreeWith size seed gen = do
  liftIO . putStr $
    renderTree size seed gen

-- | Render the shrink tree produced by a generator, for the given size and
--   seed.
--
renderTree :: Show a => Size -> Seed -> Gen a -> String
renderTree size seed gen =
  case evalGen size seed gen of
    Nothing ->
      "<discard>"
    Just x ->
      Tree.render (fmap show x)

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
