{-# OPTIONS_HADDOCK not-home #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- MonadBase
module Hedgehog.Internal.Gen (
  -- * Transformer
    Gen
  , GenT(..)
  , MonadGen(..)

  -- * Combinators
  , lift

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
  , choice
  , frequency
  , recursive

  -- ** Conditional
  , discard
  , ensure
  , filter
  , just

  -- ** Collections
  , maybe
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
  , shuffle

  -- * Sampling Generators
  , sample
  , print
  , printTree
  , printWith
  , printTreeWith

  -- * Internal
  -- $internal

  -- ** Transfomer
  , runGenT
  , mapGenT
  , generate
  , liftTree
  , runDiscardEffect

  -- ** Size
  , golden

  -- ** Shrinking
  , atLeast

  -- ** Characters
  , isSurrogate

  -- ** Subterms
  , Vec(..)
  , Nat(..)
  , subtermMVec

  -- ** Sampling
  , renderNodes
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), filterM, replicateM, ap, join)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..), MMonad(..), generalize)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.Except (ExceptT(..), mapExceptT)
import           Control.Monad.Trans.Identity (IdentityT(..), mapIdentityT)
import           Control.Monad.Trans.Maybe (MaybeT(..), mapMaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import           Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import           Control.Monad.Trans.Resource (MonadResource(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Control.Monad.Writer.Class (MonadWriter(..))

import           Data.Bifunctor (first, second)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import           Data.Foldable (for_, toList)
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word8, Word16, Word32, Word64)

import           Hedgehog.Internal.Distributive (Distributive(..))
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import qualified Hedgehog.Internal.Tree as Tree
import           Hedgehog.Range (Size, Range)
import qualified Hedgehog.Range as Range

import           Prelude hiding (filter, print, maybe, map, seq)


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
      unGen :: Size -> Seed -> Tree (MaybeT m) a
    }

-- | Runs a generator, producing its shrink tree.
--
runGenT :: Size -> Seed -> GenT m a -> Tree (MaybeT m) a
runGenT size seed (GenT m) =
  m size seed

-- | Map over a generator's shrink tree.
--
mapGenT :: (Tree (MaybeT m) a -> Tree (MaybeT n) b) -> GenT m a -> GenT n b
mapGenT f gen =
  GenT $ \size seed ->
    f (runGenT size seed gen)

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
liftTree :: Tree (MaybeT m) a -> GenT m a
liftTree x =
  GenT (\_ _ -> x)

-- | Run the discard effects through the tree and reify them as 'Maybe' values
--   at the nodes. 'Nothing' means discarded, 'Just' means we have a value.
--
runDiscardEffect :: Monad m => Tree (MaybeT m) a -> Tree m (Maybe a)
runDiscardEffect =
  runMaybeT . distribute

------------------------------------------------------------------------
-- MonadGen

-- | Class of monads which can generate input data for tests.
--
--   /The functions on this class can, and should, be used without their @Gen@/
--   /suffix by importing "Hedgehog.Gen" qualified./
--
class Monad m => MonadGen m where
  -- | See @Gen.@'Hedgehog.Gen.lift'
  --
  liftGen :: Gen a -> m a

  -- | See @Gen.@'Hedgehog.Gen.shrink'
  --
  shrinkGen :: (a -> [a]) -> m a -> m a

  -- | See @Gen.@'Hedgehog.Gen.prune'
  --
  pruneGen :: m a -> m a

  -- | See @Gen.@'Hedgehog.Gen.scale'
  --
  scaleGen :: (Size -> Size) -> m a -> m a

  -- | See @Gen.@'Hedgehog.Gen.freeze'
  --
  freezeGen :: m a -> m (a, m a)

instance Monad m => MonadGen (GenT m) where
  liftGen gen =
    hoist generalize gen

  shrinkGen =
    mapGenT . Tree.expand

  pruneGen =
    mapGenT Tree.prune

  scaleGen f gen =
    GenT $ \size0 seed ->
      let
        size =
          f size0
      in
        if size < 0 then
          error "Hedgehog.Gen.scale: negative size"
        else
          runGenT size seed gen

  freezeGen gen =
    GenT $ \size seed -> do
      mx <- Trans.lift . Trans.lift . runMaybeT . runTree $ runGenT size seed gen
      case mx of
        Nothing ->
          mzero
        Just (Node x xs) ->
          pure (x, liftTree . Tree.fromNode $ Node x xs)

instance MonadGen m => MonadGen (IdentityT m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    mapIdentityT (shrink f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen =
    mapIdentityT $
      fmap (second Trans.lift) . freeze

shrinkMaybe :: (a -> [a]) -> Maybe a -> [Maybe a]
shrinkMaybe f = \case
  Nothing ->
    pure Nothing
  Just x ->
    fmap Just (f x)

shrinkEither :: (a -> [a]) -> Either x a -> [Either x a]
shrinkEither f = \case
  Left x ->
    pure $ Left x
  Right x ->
    fmap Right (f x)

shrink2 :: (a -> [a]) -> (a, b) -> [(a, b)]
shrink2 f (x, y) =
  fmap (, y) (f x)

shrink3 :: (a -> [a]) -> (a, b, c) -> [(a, b, c)]
shrink3 f (x, y, z) =
  fmap (, y, z) (f x)

instance MonadGen m => MonadGen (MaybeT m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    mapMaybeT $
      shrink (shrinkMaybe f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen =
    mapMaybeT $ \m0 -> do
      (mx, m) <- freeze m0
      pure $ fmap (, MaybeT m) mx

instance MonadGen m => MonadGen (ExceptT x m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    mapExceptT $
      shrink (shrinkEither f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen =
    mapExceptT $ \m0 -> do
      (mx, m) <- freeze m0
      pure $ fmap (, ExceptT m) mx

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    mapReaderT (shrink f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen =
    mapReaderT $
      fmap (second Trans.lift) . freeze

instance MonadGen m => MonadGen (Lazy.StateT s m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    Lazy.mapStateT $
      shrink (shrink2 f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen m0 =
    Lazy.StateT $ \s0 -> do
      ((x, s), m) <- freeze (Lazy.runStateT m0 s0)
      pure ((x, Lazy.StateT (const m)), s)

instance MonadGen m => MonadGen (Strict.StateT s m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    Strict.mapStateT $
      shrink (shrink2 f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen m0 =
    Strict.StateT $ \s0 -> do
      ((x, s), m) <- freeze (Strict.runStateT m0 s0)
      pure ((x, Strict.StateT (const m)), s)

instance (MonadGen m, Monoid w) => MonadGen (Lazy.WriterT w m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    Lazy.mapWriterT $
      shrink (shrink2 f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen m0 =
    Lazy.WriterT $ do
      ((x, w), m) <- freeze (Lazy.runWriterT m0)
      pure ((x, Lazy.WriterT m), w)

instance (MonadGen m, Monoid w) => MonadGen (Strict.WriterT w m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    Strict.mapWriterT $
      shrink (shrink2 f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen m0 =
    Strict.WriterT $ do
      ((x, w), m) <- freeze (Strict.runWriterT m0)
      pure ((x, Strict.WriterT m), w)

instance (MonadGen m, Monoid w) => MonadGen (Lazy.RWST r w s m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    Lazy.mapRWST $
      shrink (shrink3 f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen m0 =
    Lazy.RWST $ \r s0 -> do
      ((x, s, w), m) <- freeze (Lazy.runRWST m0 r s0)
      pure ((x, Lazy.RWST (\_ _ -> m)), s, w)

instance (MonadGen m, Monoid w) => MonadGen (Strict.RWST r w s m) where
  liftGen =
    Trans.lift . liftGen

  shrinkGen f =
    Strict.mapRWST $
      shrink (shrink3 f)

  pruneGen =
    hoist prune

  scaleGen f =
    hoist (scale f)

  freezeGen m0 =
    Strict.RWST $ \r s0 -> do
      ((x, s, w), m) <- freeze (Strict.runRWST m0 r s0)
      pure ((x, Strict.RWST (\_ _ -> m)), s, w)

------------------------------------------------------------------------
-- GenT instances

instance Functor m => Functor (GenT m) where
  fmap f gen =
    GenT $ \seed size ->
      fmap f (runGenT seed size gen)

instance Monad m => Applicative (GenT m) where
  pure =
    return
  (<*>) =
    ap

instance Monad m => Monad (GenT m) where
  return =
    liftTree . pure

  (>>=) m k =
    GenT $ \size seed ->
      case Seed.split seed of
        (sk, sm) ->
          runGenT size sk . k =<<
          runGenT size sm m

instance Monad m => Alternative (GenT m) where
  empty =
    mzero
  (<|>) =
    mplus

instance Monad m => MonadPlus (GenT m) where
  mzero =
    liftTree mzero

  mplus x y =
    GenT $ \size seed ->
      case Seed.split seed of
        (sx, sy) ->
          runGenT size sx x `mplus`
          runGenT size sy y

instance MonadTrans GenT where
  lift =
    liftTree . Trans.lift . Trans.lift

instance MFunctor GenT where
  hoist f =
    mapGenT (hoist (hoist f))

embedMaybe ::
     MonadTrans t
  => Monad n
  => Monad (t (MaybeT n))
  => (forall a. m a -> t (MaybeT n) a)
  -> MaybeT m b
  -> t (MaybeT n) b
embedMaybe f m =
  Trans.lift . MaybeT . pure =<< f (runMaybeT m)

embedTree :: Monad n => (forall a. m a -> Tree (MaybeT n) a) -> Tree (MaybeT m) b -> Tree (MaybeT n) b
embedTree f tree =
  embed (embedMaybe f) tree

embedGen :: Monad n => (forall a. m a -> GenT n a) -> GenT m b -> GenT n b
embedGen f gen =
  GenT $ \size seed ->
    case Seed.split seed of
      (sf, sg) ->
        (runGenT size sf . f) `embedTree`
        (runGenT size sg gen)

instance MMonad GenT where
  embed =
    embedGen

distributeGen :: Transformer t GenT m => GenT (t m) a -> t (GenT m) a
distributeGen x =
  join . Trans.lift . GenT $ \size seed ->
    pure . hoist liftTree . distribute . hoist distribute $ runGenT size seed x

instance Distributive GenT where
  type Transformer t GenT m = (
      Monad (t (GenT m))
    , Transformer t MaybeT m
    , Transformer t Tree (MaybeT m)
    )

  distribute =
    distributeGen

instance PrimMonad m => PrimMonad (GenT m) where
  type PrimState (GenT m) =
    PrimState m
  primitive =
    Trans.lift . primitive

instance MonadIO m => MonadIO (GenT m) where
  liftIO =
    Trans.lift . liftIO

instance MonadBase b m => MonadBase b (GenT m) where
  liftBase =
    Trans.lift . liftBase

instance MonadThrow m => MonadThrow (GenT m) where
  throwM =
    Trans.lift . throwM

instance MonadCatch m => MonadCatch (GenT m) where
  catch m onErr =
    GenT $ \size seed ->
      case Seed.split seed of
        (sm, se) ->
          (runGenT size sm m) `catch`
          (runGenT size se . onErr)

instance MonadReader r m => MonadReader r (GenT m) where
  ask =
    Trans.lift ask
  local f m =
    mapGenT (local f) m

instance MonadState s m => MonadState s (GenT m) where
  get =
    Trans.lift get
  put =
    Trans.lift . put
  state =
    Trans.lift . state

instance MonadWriter w m => MonadWriter w (GenT m) where
  writer =
    Trans.lift . writer
  tell =
    Trans.lift . tell
  listen =
    mapGenT listen
  pass =
    mapGenT pass

instance MonadError e m => MonadError e (GenT m) where
  throwError =
    Trans.lift . throwError
  catchError m onErr =
    GenT $ \size seed ->
      case Seed.split seed of
        (sm, se) ->
          (runGenT size sm m) `catchError`
          (runGenT size se . onErr)

instance MonadResource m => MonadResource (GenT m) where
  liftResourceT =
    Trans.lift . liftResourceT

------------------------------------------------------------------------
-- Combinators

-- | Lift a vanilla 'Gen' in to a 'MonadGen'.
--
lift :: MonadGen m => Gen a -> m a
lift =
  liftGen

-- | Generate a value with no shrinks from a 'Size' and a 'Seed'.
--
generate :: MonadGen m => (Size -> Seed -> a) -> m a
generate f =
  liftGen . GenT $ \size seed ->
    pure (f size seed)

------------------------------------------------------------------------
-- Combinators - Shrinking

-- | Apply a shrinking function to a generator.
--
--   This will give the generator additional shrinking options, while keeping
--   the existing shrinks intact.
--
shrink :: MonadGen m => (a -> [a]) -> m a -> m a
shrink =
  shrinkGen

-- | Throw away a generator's shrink tree.
--
prune :: MonadGen m => m a -> m a
prune =
  pruneGen

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
scale =
  scaleGen

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
integral :: (MonadGen m, Integral a) => Range a -> m a
integral range =
  shrink (Shrink.towards $ Range.origin range) (integral_ range)

-- | Generates a random integral number in the [inclusive,inclusive] range.
--
--   /This generator does not shrink./
--
integral_ :: (MonadGen m, Integral a) => Range a -> m a
integral_ range =
  generate $ \size seed ->
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
--   This generator works the same as 'integral', but for floating point numbers.
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

-- | Generates a Unicode character, excluding invalid standalone surrogates:
--   @'\0'..'\1114111' (excluding '\55296'..'\57343')@
--
unicode :: MonadGen m => m Char
unicode =
  filter (not . isSurrogate) unicodeAll

-- | Generates a Unicode character, including invalid standalone surrogates:
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
element :: MonadGen m => [a] -> m a
element = \case
  [] ->
    error "Hedgehog.Gen.element: used with empty list"
  xs -> do
    n <- integral $ Range.constant 0 (length xs - 1)
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

      total =
        sum (fmap fst xs0)

    n <- integral $ Range.constant 1 total
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
  liftGen mzero

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

-- | Generates a value that satisfies a predicate.
--
--   This is essentially:
--
-- @
-- filter p gen = 'mfilter' p gen '<|>' filter p gen
-- @
--
--   It differs from the above in that we keep some state to avoid looping
--   forever. If we trigger these limits then the whole generator is discarded.
--
filter :: MonadGen m => (a -> Bool) -> m a -> m a
filter p gen =
  let
    try k =
      if k > 100 then
        discard
      else do
        x <- scale (2 * k +) gen
        if p x then
          pure x
        else
          try (k + 1)
  in
    try 0

-- | Runs a 'Maybe' generator until it produces a 'Just'.
--
--   This is implemented using 'filter' and has the same caveats.
--
just :: MonadGen m => m (Maybe a) -> m a
just g = do
  mx <- filter Maybe.isJust g
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

-- | Generates a list using a 'Range' to determine the length.
--
list :: MonadGen m => Range Int -> m a -> m [a]
list range gen =
  sized $ \size ->
    (traverse snd =<<) .
    ensure (atLeast $ Range.lowerBound size range) .
    shrink Shrink.list $ do
      k <- integral_ range
      replicateM k (freeze gen)

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
  freezeGen

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
subsequence :: MonadGen m => [a] -> m [a]
subsequence xs =
  shrink Shrink.list $ filterM (const bool_) xs

-- | Generates a random permutation of a list.
--
--   This shrinks towards the order of the list being identical to the input
--   list.
--
shuffle :: MonadGen m => [a] -> m [a]
shuffle = \case
  [] ->
    pure []
  xs0 -> do
    n <- integral $ Range.constant 0 (length xs0 - 1)
    case splitAt n xs0 of
      (xs, y : ys) ->
        (y :) <$> shuffle (xs ++ ys)
      (_, []) ->
        error "Hedgehog.Gen.shuffle: internal error, split generated empty list"

------------------------------------------------------------------------
-- Sampling

-- | Generate a sample from a generator.
--
sample :: MonadIO m => Gen a -> m a
sample gen =
  liftIO $
    let
      loop n =
        if n <= 0 then
          error "Hedgehog.Gen.sample: too many discards, could not generate a sample"
        else do
          seed <- Seed.random
          case runIdentity . runMaybeT . runTree $ runGenT 30 seed gen of
            Nothing ->
              loop (n - 1)
            Just x ->
              pure $ nodeValue x
    in
      loop (100 :: Int)

-- | Print the value produced by a generator, and the first level of shrinks,
--   for the given size and seed.
--
--   Use 'print' to generate a value from a random seed.
--
printWith :: (MonadIO m, Show a) => Size -> Seed -> Gen a -> m ()
printWith size seed gen =
  liftIO $ do
    let
      Node x ss =
        runIdentity . runTree $ renderNodes size seed gen

    putStrLn "=== Outcome ==="
    putStrLn x
    putStrLn "=== Shrinks ==="

    for_ ss $ \s ->
      let
        Node y _ =
          runIdentity $ runTree s
      in
        putStrLn y

-- | Print the shrink tree produced by a generator, for the given size and
--   seed.
--
--   Use 'printTree' to generate a value from a random seed.
--
printTreeWith :: (MonadIO m, Show a) => Size -> Seed -> Gen a -> m ()
printTreeWith size seed gen = do
  liftIO . putStr . runIdentity . Tree.render $ renderNodes size seed gen

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

-- | Render a generator as a tree of strings.
--
renderNodes :: (Monad m, Show a) => Size -> Seed -> Gen a -> Tree m String
renderNodes size seed =
  fmap (Maybe.maybe "<discard>" show) . runDiscardEffect . runGenT size seed . lift

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
