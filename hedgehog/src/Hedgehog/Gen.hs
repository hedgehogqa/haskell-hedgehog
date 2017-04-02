{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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
module Hedgehog.Gen (
  -- * Transformer
    Gen(..)

  -- * Combinators
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

  -- ** Strings
  , string
  , text
  , utf8
  , bytes

  -- ** Choice
  , element
  , choice
  , frequency
  , recursive

  -- ** Conditional
  , discard
  , filter
  , just

  -- ** Collections
  , maybe
  , list
  , nonEmpty
  , set
  , map

  -- ** Subterms
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
  , runGen
  , mapGen
  , generate
  , liftTree
  , runDiscardEffect

  -- ** Size
  , golden

  -- ** Shrinking
  , atLeast

  -- ** Subterms
  , Vec(..)
  , Nat(..)
  , subtermMVec
  , freeze

  -- ** Sampling
  , renderNodes
  ) where

import           Control.Applicative (Alternative(..))
import           Control.Monad (MonadPlus(..), mfilter, filterM, replicateM, ap)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..), MMonad(..))
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Resource (MonadResource(..))
import           Control.Monad.Writer.Class (MonadWriter(..))

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import           Data.Foldable (for_, toList)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word8, Word16, Word32, Word64)

import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import qualified Hedgehog.Internal.Tree as Tree
import           Hedgehog.Range (Size, Range)
import qualified Hedgehog.Range as Range

import           Prelude hiding (filter, print, maybe, map)


------------------------------------------------------------------------
-- Generator transformer

-- | Generator for random values of @a@.
--
newtype Gen m a =
  Gen {
      unGen :: Size -> Seed -> Tree (MaybeT m) a
    }

-- | Runs a generator, producing its shrink tree.
--
runGen :: Size -> Seed -> Gen m a -> Tree (MaybeT m) a
runGen size seed (Gen m) =
  m size seed

-- | Map over a generator's shrink tree.
--
mapGen :: (Tree (MaybeT m) a -> Tree (MaybeT n) b) -> Gen m a -> Gen n b
mapGen f gen =
  Gen $ \size seed ->
    f (runGen size seed gen)

-- | Generate a value with no shrinks from a 'Size' and a 'Seed'.
--
generate :: Monad m => (Size -> Seed -> a) -> Gen m a
generate f =
  Gen $ \size seed ->
    pure (f size seed)

-- | Freeze the size and seed used by a generator, so we can inspect the value
--   which it will produce.
--
--   This is used for implementing `list` and `subtermMVec`. It allows us to
--   shrink the list itself before trying to shrink the values inside the list.
--
freeze :: Monad m => Gen m a -> Gen m (a, Gen m a)
freeze gen =
  Gen $ \size seed -> do
    mx <- lift . lift . runMaybeT . runTree $ runGen size seed gen
    case mx of
      Nothing ->
        mzero
      Just (Node x xs) ->
        pure (x, liftTree . Tree . pure $ Node x xs)

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
liftTree :: Tree (MaybeT m) a -> Gen m a
liftTree x =
  Gen (\_ _ -> x)

-- | Run the discard effects through the tree and reify them as 'Maybe' values
--   at the nodes. 'Nothing' means discarded, 'Just' means we have a value.
--
runDiscardEffect :: Monad m => Tree (MaybeT m) a -> Tree m (Maybe a)
runDiscardEffect s = do
  Tree $ do
    mx <- runMaybeT $ runTree s
    case mx of
      Nothing ->
        pure $ Node Nothing []
      Just (Node x xs) ->
        pure $ Node (Just x) (fmap runDiscardEffect xs)

------------------------------------------------------------------------
-- Gen instances

instance Functor m => Functor (Gen m) where
  fmap f gen =
    Gen $ \seed size ->
      fmap f (runGen seed size gen)

instance Monad m => Applicative (Gen m) where
  pure =
    return
  (<*>) =
    ap

instance Monad m => Monad (Gen m) where
  return =
    liftTree . pure

  (>>=) m k =
    Gen $ \size seed ->
      case Seed.split seed of
        (sk, sm) ->
          runGen size sk . k =<<
          runGen size sm m

instance Monad m => Alternative (Gen m) where
  empty =
    mzero
  (<|>) =
    mplus

instance Monad m => MonadPlus (Gen m) where
  mzero =
    liftTree mzero

  mplus x y =
    Gen $ \size seed ->
      case Seed.split seed of
        (sx, sy) ->
          runGen size sx x `mplus`
          runGen size sy y

instance MonadTrans Gen where
  lift =
    liftTree . lift . lift

instance MFunctor Gen where
  hoist f =
    mapGen (hoist (hoist f))

embedMaybe ::
  MonadTrans t =>
  Monad n =>
  Monad (t (MaybeT n)) =>
  (forall a. m a -> t (MaybeT n) a) ->
  MaybeT m b ->
  t (MaybeT n) b
embedMaybe f m =
  lift . MaybeT . pure =<< f (runMaybeT m)

embedTree :: Monad n => (forall a. m a -> Tree (MaybeT n) a) -> Tree (MaybeT m) b -> Tree (MaybeT n) b
embedTree f tree =
  embed (embedMaybe f) tree

embedGen :: Monad n => (forall a. m a -> Gen n a) -> Gen m b -> Gen n b
embedGen f gen =
  Gen $ \size seed ->
    case Seed.split seed of
      (sf, sg) ->
        (runGen size sf . f) `embedTree`
        (runGen size sg gen)

instance MMonad Gen where
  embed =
    embedGen

instance PrimMonad m => PrimMonad (Gen m) where
  type PrimState (Gen m) =
    PrimState m
  primitive =
    lift . primitive

instance MonadIO m => MonadIO (Gen m) where
  liftIO =
    lift . liftIO

instance MonadBase b m => MonadBase b (Gen m) where
  liftBase =
    lift . liftBase

instance MonadThrow m => MonadThrow (Gen m) where
  throwM =
    lift . throwM

instance MonadCatch m => MonadCatch (Gen m) where
  catch m onErr =
    Gen $ \size seed ->
      case Seed.split seed of
        (sm, se) ->
          (runGen size sm m) `catch`
          (runGen size se . onErr)

instance MonadReader r m => MonadReader r (Gen m) where
  ask =
    lift ask
  local f m =
    mapGen (local f) m

instance MonadState s m => MonadState s (Gen m) where
  get =
    lift get
  put =
    lift . put
  state =
    lift . state

instance MonadWriter w m => MonadWriter w (Gen m) where
  writer =
    lift . writer
  tell =
    lift . tell
  listen =
    mapGen listen
  pass =
    mapGen pass

instance MonadError e m => MonadError e (Gen m) where
  throwError =
    lift . throwError
  catchError m onErr =
    Gen $ \size seed ->
      case Seed.split seed of
        (sm, se) ->
          (runGen size sm m) `catchError`
          (runGen size se . onErr)

instance MonadResource m => MonadResource (Gen m) where
  liftResourceT =
    lift . liftResourceT

------------------------------------------------------------------------
-- Shrinking

-- | Apply a shrinking function to a generator.
--
--   This will give the generator additional shrinking options, while keeping
--   the existing shrinks intact.
--
shrink :: Monad m => (a -> [a]) -> Gen m a -> Gen m a
shrink =
  mapGen . Tree.expand

-- | Throw away a generator's shrink tree.
--
prune :: Monad m => Gen m a -> Gen m a
prune =
  mapGen Tree.prune

------------------------------------------------------------------------
-- Combinators - Size

-- | Construct a generator that depends on the size parameter.
--
sized :: (Size -> Gen m a) -> Gen m a
sized f =
  Gen $ \size seed ->
    runGen size seed (f size)

-- | Override the size parameter. Returns a generator which uses the given size
--   instead of the runtime-size parameter.
--
resize :: Size -> Gen m a -> Gen m a
resize size gen =
  if size < 0 then
    error "Hedgehog.Random.resize: negative size"
  else
    Gen $ \_ seed ->
      runGen size seed gen

-- | Adjust the size parameter by transforming it with the given function.
--
scale :: (Size -> Size) -> Gen m a -> Gen m a
scale f gen =
  sized $ \n ->
    resize (f n) gen

-- | Make a generator smaller by scaling its size parameter.
--
small :: Gen m a -> Gen m a
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
integral :: (Monad m, Integral a) => Range a -> Gen m a
integral range =
  shrink (Shrink.towards $ Range.origin range) (integral_ range)

-- | Generates a random integral number in the [inclusive,inclusive] range.
--
--   /This generator does not shrink./
--
integral_ :: (Monad m, Integral a) => Range a -> Gen m a
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
int :: Monad m => Range Int -> Gen m Int
int =
  integral

-- | Generates a random 8-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int8 :: Monad m => Range Int8 -> Gen m Int8
int8 =
  integral

-- | Generates a random 16-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int16 :: Monad m => Range Int16 -> Gen m Int16
int16 =
  integral

-- | Generates a random 32-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int32 :: Monad m => Range Int32 -> Gen m Int32
int32 =
  integral

-- | Generates a random 64-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int64 :: Monad m => Range Int64 -> Gen m Int64
int64 =
  integral

-- | Generates a random machine word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word :: Monad m => Range Word -> Gen m Word
word =
  integral

-- | Generates a random byte in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word8 :: Monad m => Range Word8 -> Gen m Word8
word8 =
  integral

-- | Generates a random 16-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word16 :: Monad m => Range Word16 -> Gen m Word16
word16 =
  integral

-- | Generates a random 32-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word32 :: Monad m => Range Word32 -> Gen m Word32
word32 =
  integral

-- | Generates a random 64-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word64 :: Monad m => Range Word64 -> Gen m Word64
word64 =
  integral

------------------------------------------------------------------------
-- Combinators - Fractional / Floating-Point

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   This generator works the same as 'integral', but for floating point numbers.
--
realFloat :: (Monad m, RealFloat a) => Range a -> Gen m a
realFloat range =
  shrink (Shrink.towardsFloat $ Range.origin range) (realFrac_ range)

-- | Generates a random fractional number in the [inclusive,exclusive) range.
--
--   /This generator does not shrink./
--
realFrac_ :: (Monad m, RealFrac a) => Range a -> Gen m a
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
float :: Monad m => Range Float -> Gen m Float
float =
 realFloat

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This is a specialization of 'realFloat', offered for convenience./
--
double :: Monad m => Range Double -> Gen m Double
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
enum :: (Monad m, Enum a) => a -> a -> Gen m a
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
enumBounded :: (Monad m, Enum a, Bounded a) => Gen m a
enumBounded =
  enum minBound maxBound

-- | Generates a random boolean.
--
--   This generator shrinks to 'False'.
--
--   /This is a specialization of 'enumBounded', offered for convenience./
--
bool :: Monad m => Gen m Bool
bool =
  enumBounded

-- | Generates a random boolean.
--
--   /This generator does not shrink./
--
bool_ :: Monad m => Gen m Bool
bool_ =
  generate $ \_ seed ->
    (/= 0) . fst $ Seed.nextInteger 0 1 seed

------------------------------------------------------------------------
-- Combinators - Characters

-- | Generates an ASCII binit: @'0'..'1'@
--
binit :: Monad m => Gen m Char
binit =
  enum '0' '1'

-- | Generates an ASCII octit: @'0'..'7'@
--
octit :: Monad m => Gen m Char
octit =
  enum '0' '7'

-- | Generates an ASCII digit: @'0'..'9'@
--
digit :: Monad m => Gen m Char
digit =
  enum '0' '9'

-- | Generates an ASCII hexit: @'0'..'9', \'a\'..\'f\', \'A\'..\'F\'@
--
hexit :: Monad m => Gen m Char
hexit =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "0123456789aAbBcCdDeEfF"

-- | Generates an ASCII lowercase letter: @\'a\'..\'z\'@
--
lower :: Monad m => Gen m Char
lower =
  enum 'a' 'z'

-- | Generates an ASCII uppercase letter: @\'A\'..\'Z\'@
--
upper :: Monad m => Gen m Char
upper =
  enum 'A' 'Z'

-- | Generates an ASCII letter: @\'a\'..\'z\', \'A\'..\'Z\'@
--
alpha :: Monad m => Gen m Char
alpha =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Generates an ASCII letter or digit: @\'a\'..\'z\', \'A\'..\'Z\', \'0\'..\'9\'@
--
alphaNum :: Monad m => Gen m Char
alphaNum =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

------------------------------------------------------------------------
-- Combinators - Strings

-- | Generates a string using 'Range' to determine the length.
--
--   /This is a specialization of 'list', offered for convenience./
--
string :: Monad m => Range Int -> Gen m Char -> Gen m String
string =
  list

-- | Generates a string using 'Range' to determine the length.
--
text :: Monad m => Range Int -> Gen m Char -> Gen m Text
text range =
  fmap Text.pack . string range

-- | Generates a UTF-8 encoded string, using 'Range' to determine the length.
--
utf8 :: Monad m => Range Int -> Gen m Char -> Gen m ByteString
utf8 range =
  fmap Text.encodeUtf8 . text range

-- | Generates a random 'ByteString', using 'Range' to determine the
--   length.
--
bytes :: Monad m => Range Int -> Gen m ByteString
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

-- | Randomly selects one of the elements in the list.
--
--   This generator shrinks towards the first element in the list.
--
--   /The input list must be non-empty./
--
element :: Monad m => [a] -> Gen m a
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
choice :: Monad m => [Gen m a] -> Gen m a
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
frequency :: Monad m => [(Int, Gen m a)] -> Gen m a
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
-- genName :: 'Monad' m => 'Gen' m String
--
-- -- We can write a generator for expressions
-- genExpr :: 'Monad' m => 'Gen' m Expr
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
recursive :: ([Gen m a] -> Gen m a) -> [Gen m a] -> [Gen m a] -> Gen m a
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
--   /This is another name for 'empty' \/ 'mzero'./
--
discard :: Monad m => Gen m a
discard =
  mzero

-- | Generates a value that satisfies a predicate.
--
--   This is essentially:
--
-- @
-- filter p gen = 'mfilter' p gen '<|>' filter p gen
-- @
--
--   It differs from the above in that we keep some state to avoid looping
--   forever. If we trigger these limits then the whole whole generator is
--   discarded.
--
filter :: Monad m => (a -> Bool) -> Gen m a -> Gen m a
filter p gen =
  let
    try k =
      if k > 100 then
        empty
      else
        mfilter p (scale (2 * k +) gen) <|> try (k + 1)
  in
    try 0

-- | Runs a 'Maybe' generator until it produces a 'Just'.
--
--   This is implemented using 'filter' and has the same caveats.
--
just :: Monad m => Gen m (Maybe a) -> Gen m a
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
maybe :: Monad m => Gen m a -> Gen m (Maybe a)
maybe gen = do
  sized $ \n ->
    frequency [
        (2, pure Nothing)
      , (1 + fromIntegral n, Just <$> gen)
      ]

-- | Generates a list using a 'Range' to determine the length.
--
list :: Monad m => Range Int -> Gen m a -> Gen m [a]
list range gen =
  sized $ \size ->
    (traverse snd =<<) .
    mfilter (atLeast $ Range.lowerBound size range) .
    shrink Shrink.list $ do
      k <- integral_ range
      replicateM k (freeze gen)

-- | Generates a non-empty list using a 'Range' to determine the length.
--
nonEmpty :: Monad m => Range Int -> Gen m a -> Gen m (NonEmpty a)
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
set :: (Monad m, Ord a) => Range Int -> Gen m a -> Gen m (Set a)
set range gen =
  fmap Map.keysSet . map range $ fmap (, ()) gen

-- | Generates a map using a 'Range' to determine the length.
--
--   /This may fail to generate anything if the keys produced by the/
--   /generator do not account for a large enough number of unique/
--   /items to satify the required map size./
--
map :: (Monad m, Ord k) => Range Int -> Gen m (k, v) -> Gen m (Map k v)
map range gen =
  sized $ \size ->
    mfilter ((>= Range.lowerBound size range) . Map.size) .
    fmap Map.fromList .
    (sequence =<<) .
    shrink Shrink.list $ do
      k <- integral_ range
      uniqueByKey k gen

-- | Generate exactly 'n' unique generators.
--
uniqueByKey :: (Monad m, Ord k) => Int -> Gen m (k, v) -> Gen m [Gen m (k, v)]
uniqueByKey n gen =
  let
    try k xs0 =
      if k > 100 then
        mzero
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

shrinkSubterms :: Subterms n a -> [Subterms n a]
shrinkSubterms = \case
  One _ ->
    []
  All xs ->
    fmap One $ toList xs

genSubterms :: Monad m => Vec n (Gen m a) -> Gen m (Subterms n a)
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
subtermMVec :: Monad m => Vec n (Gen m a) -> (Vec n a -> Gen m a) -> Gen m a
subtermMVec gs f =
  fromSubterms f =<< genSubterms gs

-- | Constructs a generator from a sub-term generator.
--
--   /Shrinks to the sub-term if possible./
--
subtermM :: Monad m => Gen m a -> (a -> Gen m a) -> Gen m a
subtermM gx f =
  subtermMVec (gx :. Nil) $ \(x :. Nil) ->
    f x

-- | Constructs a generator from a sub-term generator.
--
--   /Shrinks to the sub-term if possible./
--
subterm :: Monad m => Gen m a -> (a -> a) -> Gen m a
subterm gx f =
  subtermM gx $ \x ->
    pure (f x)

-- | Constructs a generator from two sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermM2 :: Monad m => Gen m a -> Gen m a -> (a -> a -> Gen m a) -> Gen m a
subtermM2 gx gy f =
  subtermMVec (gx :. gy :. Nil) $ \(x :. y :. Nil) ->
    f x y

-- | Constructs a generator from two sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subterm2 :: Monad m => Gen m a -> Gen m a -> (a -> a -> a) -> Gen m a
subterm2 gx gy f =
  subtermM2 gx gy $ \x y ->
    pure (f x y)

-- | Constructs a generator from three sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermM3 :: Monad m => Gen m a -> Gen m a -> Gen m a -> (a -> a -> a -> Gen m a) -> Gen m a
subtermM3 gx gy gz f =
  subtermMVec (gx :. gy :. gz :. Nil) $ \(x :. y :. z :. Nil) ->
    f x y z

-- | Constructs a generator from three sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subterm3 :: Monad m => Gen m a -> Gen m a -> Gen m a -> (a -> a -> a -> a) -> Gen m a
subterm3 gx gy gz f =
  subtermM3 gx gy gz $ \x y z ->
    pure (f x y z)

------------------------------------------------------------------------
-- Combinators - Combinations & Permutations

-- | Generates a random subsequence of a list.
--
subsequence :: Monad m => [a] -> Gen m [a]
subsequence xs =
  shrink Shrink.list $ filterM (const bool_) xs

-- | Generates a random permutation of a list.
--
--   This shrinks towards the order of the list being identical to the input
--   list.
--
shuffle :: Monad m => [a] -> Gen m [a]
shuffle = \case
  [] ->
    pure []
  xs0 -> do
    n <- integral $ Range.constant 0 (length xs0 - 1)
    case splitAt n xs0 of
      (xs, y : ys) ->
        (y :) <$> shuffle (xs ++ ys)
      (_, []) ->
        error "Hedgehog.shuffle: internal error, split generated empty list"

------------------------------------------------------------------------
-- Sampling

-- | Generate a random sample of data from the a generator.
--
sample :: MonadIO m => Gen m a -> m [a]
sample gen =
  fmap (fmap nodeValue . Maybe.catMaybes) .
  replicateM 10 $ do
    seed <- liftIO $ Seed.random
    runMaybeT . runTree $ runGen 30 seed gen

-- | Print the value produced by a generator, and the first level of shrinks,
--   for the given size and seed.
--
--   Use 'print' to generate a value from a random seed.
--
printWith :: (MonadIO m, Show a) => Size -> Seed -> Gen m a -> m ()
printWith size seed gen = do
  Node x ss <- runTree $ renderNodes size seed gen
  liftIO $ putStrLn "=== Outcome ==="
  liftIO $ putStrLn x
  liftIO $ putStrLn "=== Shrinks ==="
  for_ ss $ \s -> do
    Node y _ <- runTree s
    liftIO $ putStrLn y

-- | Print the shrink tree produced by a generator, for the given size and
--   seed.
--
--   Use 'printTree' to generate a value from a random seed.
--
printTreeWith :: (MonadIO m, Show a) => Size -> Seed -> Gen m a -> m ()
printTreeWith size seed gen = do
  s <- Tree.render $ renderNodes size seed gen
  liftIO $ putStr s

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
print :: (MonadIO m, Show a) => Gen m a -> m ()
print gen = do
  seed <- liftIO $ Seed.random
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
printTree :: (MonadIO m, Show a) => Gen m a -> m ()
printTree gen = do
  seed <- liftIO $ Seed.random
  printTreeWith 30 seed gen

-- | Render a generator as a tree of strings.
--
renderNodes :: (Monad m, Show a) => Size -> Seed -> Gen m a -> Tree m String
renderNodes size seed =
  fmap (Maybe.maybe "<discard>" show) . runDiscardEffect . runGen size seed

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
