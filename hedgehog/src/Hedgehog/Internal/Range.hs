{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hedgehog.Internal.Range (
  -- * Size
    Size(..)

  -- * Range
  , Range(..)
  , origin
  , bounds
  , lowerBound
  , upperBound

  -- * Constant
  , singleton
  , constant
  , constantFrom
  , constantBounded

  -- * Linear
  , linear
  , linearFrom
  , linearFrac
  , linearFracFrom
  , linearBounded

  -- * Exponential
  , exponential
  , exponentialFrom
  , exponentialBounded
  , exponentialFloat
  , exponentialFloatFrom

  -- * Internal
  -- $internal
  , clamp
  , scaleLinear
  , scaleLinearFrac
  , scaleExponential
  , scaleExponentialFloat
  ) where

import           Data.Bifunctor (bimap)

import           Prelude hiding (minimum, maximum)

-- $setup
-- >>> import Data.Int (Int8)
-- >>> let x = 3

-- | Tests are parameterized by the size of the randomly-generated data. The
--   meaning of a 'Size' value depends on the particular generator used, but
--   it must always be a number between 0 and 99 inclusive.
--
newtype Size =
  Size {
      unSize :: Int
    } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show Size where
  showsPrec p (Size x) =
    showParen (p > 10) $
      showString "Size " .
      showsPrec 11 x

instance Read Size where
  readsPrec p =
    readParen (p > 10) $ \r0 -> do
      ("Size", r1) <- lex r0
      (s, r2) <- readsPrec 11 r1
      pure (Size s, r2)

-- | A range describes the bounds of a number to generate, which may or may not
--   be dependent on a 'Size'.
--
--   The constructor takes an origin between the lower and upper bound, and a
--   function from 'Size' to bounds.  As the size goes towards @0@, the values
--   go towards the origin.
--
data Range a =
  Range !a (Size -> (a, a))

instance Functor Range where
  fmap f (Range z g) =
    Range (f z) $ \sz ->
      bimap f f (g sz)

-- | Get the origin of a range. This might be the mid-point or the lower bound,
--   depending on what the range represents.
--
--   The 'bounds' of a range are scaled around this value when using the
--   'linear' family of combinators.
--
--   When using a 'Range' to generate numbers, the shrinking function will
--   shrink towards the origin.
--
origin :: Range a -> a
origin (Range z _) =
  z

-- | Get the extents of a range, for a given size.
--
bounds :: Size -> Range a -> (a, a)
bounds sz (Range _ f) =
  f sz

-- | Get the lower bound of a range for the given size.
--
lowerBound :: Ord a => Size -> Range a -> a
lowerBound sz range =
  let
    (x, y) =
      bounds sz range
  in
    min x y

-- | Get the upper bound of a range for the given size.
--
upperBound :: Ord a => Size -> Range a -> a
upperBound sz range =
  let
    (x, y) =
      bounds sz range
  in
    max x y

-- | Construct a range which represents a constant single value.
--
--   >>> bounds x $ singleton 5
--   (5,5)
--
--   >>> origin $ singleton 5
--   5
--
singleton :: a -> Range a
singleton x =
  Range x $ \_ -> (x, x)

-- | Construct a range which is unaffected by the size parameter.
--
--   A range from @0@ to @10@, with the origin at @0@:
--
--   >>> bounds x $ constant 0 10
--   (0,10)
--
--   >>> origin $ constant 0 10
--   0
--
constant :: a -> a -> Range a
constant x y =
  constantFrom x x y

-- | Construct a range which is unaffected by the size parameter with a origin
--   point which may differ from the bounds.
--
--   A range from @-10@ to @10@, with the origin at @0@:
--
--   >>> bounds x $ constantFrom 0 (-10) 10
--   (-10,10)
--
--   >>> origin $ constantFrom 0 (-10) 10
--   0
--
--   A range from @1970@ to @2100@, with the origin at @2000@:
--
--   >>> bounds x $ constantFrom 2000 1970 2100
--   (1970,2100)
--
--   >>> origin $ constantFrom 2000 1970 2100
--   2000
--
constantFrom ::
     a -- ^ Origin (the value produced when the size parameter is 0).
  -> a -- ^ Lower bound (the bottom of the range when the size parameter is 99).
  -> a -- ^ Upper bound (the top of the range when the size parameter is 99).
  -> Range a
constantFrom z x y =
  Range z $ \_ -> (x, y)

-- | Construct a range which is unaffected by the size parameter using the full
--   range of a data type.
--
--   A range from @-128@ to @127@, with the origin at @0@:
--
--   >>> bounds x (constantBounded :: Range Int8)
--   (-128,127)
--
--   >>> origin (constantBounded :: Range Int8)
--   0
--
constantBounded :: (Bounded a, Num a) => Range a
constantBounded =
  constantFrom 0 minBound maxBound

-- | Construct a range which scales the second bound relative to the size
--   parameter.
--
--   >>> bounds 0 $ linear 0 10
--   (0,0)
--
--   >>> bounds 50 $ linear 0 10
--   (0,5)
--
--   >>> bounds 99 $ linear 0 10
--   (0,10)
--
linear :: Integral a => a -> a -> Range a
linear x y =
  linearFrom x x y

-- | Construct a range which scales the bounds relative to the size parameter.
--
--   >>> bounds 0 $ linearFrom 0 (-10) 10
--   (0,0)
--
--   >>> bounds 50 $ linearFrom 0 (-10) 20
--   (-5,10)
--
--   >>> bounds 99 $ linearFrom 0 (-10) 20
--   (-10,20)
--
linearFrom :: Integral a
  => a -- ^ Origin (the value produced when the size parameter is 0).
  -> a -- ^ Lower bound (the bottom of the range when the size parameter is 99).
  -> a -- ^ Upper bound (the top of the range when the size parameter is 99).
  -> Range a
linearFrom z x y =
  Range z $ \sz ->
    let
      x_sized =
        clamp x y $ scaleLinear sz z x

      y_sized =
        clamp x y $ scaleLinear sz z y
    in
      (x_sized, y_sized)

-- | Construct a range which is scaled relative to the size parameter and uses
--   the full range of a data type.
--
--   >>> bounds 0 (linearBounded :: Range Int8)
--   (0,0)
--
--   >>> bounds 50 (linearBounded :: Range Int8)
--   (-64,64)
--
--   >>> bounds 99 (linearBounded :: Range Int8)
--   (-128,127)
--
linearBounded :: (Bounded a, Integral a) => Range a
linearBounded =
  linearFrom 0 minBound maxBound

-- | Construct a range which scales the second bound relative to the size
--   parameter.
--
--   /This works the same as 'linear', but for fractional values./
--
linearFrac :: (Fractional a, Ord a) => a -> a -> Range a
linearFrac x y =
  linearFracFrom x x y

-- | Construct a range which scales the bounds relative to the size parameter.
--
--   /This works the same as 'linearFrom', but for fractional values./
--
linearFracFrom :: (Fractional a, Ord a) => a -> a -> a -> Range a
linearFracFrom z x y =
  Range z $ \sz ->
    let
      x_sized =
        clamp x y $ scaleLinearFrac sz z x

      y_sized =
        clamp x y $ scaleLinearFrac sz z y
    in
      (x_sized, y_sized)

-- | Truncate a value so it stays within some range.
--
--   >>> clamp 5 10 15
--   10
--
--   >>> clamp 5 10 0
--   5
--
clamp :: Ord a => a -> a -> a -> a
clamp x y n =
  if x > y then
    min x (max y n)
  else
    min y (max x n)

-- | Scale an integral linearly with the size parameter.
--
scaleLinear :: Integral a => Size -> a -> a -> a
scaleLinear sz0 z0 n0 =
  let
    sz =
      max 0 (min 99 sz0)

    z =
      toInteger z0

    n =
      toInteger n0

    -- @rng@ has magnitude 1 bigger than the biggest diff
    -- i.e. it specifies the range the diff can be in [0,rng)
    -- with the upper bound being exclusive.
    rng =
      n - z + signum (n - z)

    diff =
      (rng * fromIntegral sz) `quot` 100
  in
    fromInteger $ z + diff

-- | Scale a fractional number linearly with the size parameter.
--
scaleLinearFrac :: Fractional a => Size -> a -> a -> a
scaleLinearFrac sz0 z n =
  let
    sz =
      max 0 (min 99 sz0)

    diff =
      (n - z) * (fromIntegral sz / 99)
  in
    z + diff

-- | Construct a range which scales the second bound exponentially relative to
--   the size parameter.
--
--   >>> bounds 0 $ exponential 1 512
--   (1,1)
--
--   >>> bounds 11 $ exponential 1 512
--   (1,2)
--
--   >>> bounds 22 $ exponential 1 512
--   (1,4)
--
--   >>> bounds 77 $ exponential 1 512
--   (1,128)
--
--   >>> bounds 88 $ exponential 1 512
--   (1,256)
--
--   >>> bounds 99 $ exponential 1 512
--   (1,512)
--
exponential :: Integral a => a -> a -> Range a
exponential x y =
  exponentialFrom x x y

-- | Construct a range which scales the bounds exponentially relative to the
-- size parameter.
--
--   >>> bounds 0 $ exponentialFrom 0 (-128) 512
--   (0,0)
--
--   >>> bounds 25 $ exponentialFrom 0 (-128) 512
--   (-2,4)
--
--   >>> bounds 50 $ exponentialFrom 0 (-128) 512
--   (-11,22)
--
--   >>> bounds 75 $ exponentialFrom 0 (-128) 512
--   (-39,112)
--
--   >>> bounds 99 $ exponentialFrom x (-128) 512
--   (-128,512)
--
exponentialFrom :: Integral a
  => a -- ^ Origin (the value produced when the size parameter is 0).
  -> a -- ^ Lower bound (the bottom of the range when the size parameter is 99).
  -> a -- ^ Upper bound (the top of the range when the size parameter is 99).
  -> Range a
exponentialFrom z x y =
  Range z $ \sz ->
    let
      sized_x =
        clamp x y $ scaleExponential sz z x

      sized_y =
        clamp x y $ scaleExponential sz z y
    in
      (sized_x, sized_y)

-- | Construct a range which is scaled exponentially relative to the size
--   parameter and uses the full range of a data type.
--
--   >>> bounds 0 (exponentialBounded :: Range Int8)
--   (0,0)
--
--   >>> bounds 50 (exponentialBounded :: Range Int8)
--   (-11,11)
--
--   >>> bounds 99 (exponentialBounded :: Range Int8)
--   (-128,127)
--
exponentialBounded :: (Bounded a, Integral a) => Range a
exponentialBounded =
  exponentialFrom 0 minBound maxBound

-- | Construct a range which scales the second bound exponentially relative to
--   the size parameter.
--
--   /This works the same as 'exponential', but for floating-point values./
--
--   >>> bounds 0 $ exponentialFloat 0 10
--   (0.0,0.0)
--
--   >>> bounds 50 $ exponentialFloat 0 10
--   (0.0,2.357035250656098)
--
--   >>> bounds 99 $ exponentialFloat 0 10
--   (0.0,10.0)
--
exponentialFloat :: (Floating a, Ord a) => a -> a -> Range a
exponentialFloat x y =
  exponentialFloatFrom x x y

-- | Construct a range which scales the bounds exponentially relative to the
--   size parameter.
--
--   /This works the same as 'exponentialFrom', but for floating-point values./
--
--   >>> bounds 0 $ exponentialFloatFrom 0 (-10) 20
--   (0.0,0.0)
--
--   >>> bounds 50 $ exponentialFloatFrom 0 (-10) 20
--   (-2.357035250656098,3.6535836249197002)
--
--   >>> bounds 99 $ exponentialFloatFrom x (-10) 20
--   (-10.0,20.0)
--
exponentialFloatFrom :: (Floating a, Ord a) => a -> a -> a -> Range a
exponentialFloatFrom z x y =
  Range z $ \sz ->
    let
      sized_x =
        clamp x y $ scaleExponentialFloat sz z x

      sized_y =
        clamp x y $ scaleExponentialFloat sz z y
    in
      (sized_x, sized_y)

-- | Scale an integral exponentially with the size parameter.
--
scaleExponential :: Integral a => Size -> a -> a -> a
scaleExponential sz z0 n0 =
  let
    z =
      fromIntegral z0

    n =
      fromIntegral n0
  in
    round (scaleExponentialFloat sz z n :: Double)

-- | Scale a floating-point number exponentially with the size parameter.
--
scaleExponentialFloat :: Floating a => Size -> a -> a -> a
scaleExponentialFloat sz0 z n =
  let
    sz =
      clamp 0 99 sz0

    diff =
      (((abs (n - z) + 1) ** (realToFrac sz / 99)) - 1) * signum (n - z)
  in
    z + diff


------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
