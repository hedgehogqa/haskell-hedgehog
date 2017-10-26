{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
-- |
-- This is a port of "Fast Splittable Pseudorandom Number Generators" by Steele
-- et. al. [1].
--
-- The paper's algorithm provides decent randomness for most purposes but
-- sacrifices cryptographic-quality randomness in favor of speed.  The original
-- implementation is tested with DieHarder and BigCrush; see the paper for
-- details.
--
-- This implementation, originally from [2], is a port from the paper.
--
-- It also takes in to account the SplittableRandom.java source code in OpenJDK
-- v8u40-b25 as well as splittable_random.ml in Jane Street's standard library
-- overlay (kernel) v113.33.03, and Random.fs in FsCheck v3.
--
-- Other than the choice of initial seed for 'from' this port should be
-- faithful. Currently, we have not rerun the DieHarder, or BigCrush tests on
-- this implementation.
--
-- 1. Guy L. Steele, Jr., Doug Lea, Christine H. Flood
--    Fast splittable pseudorandom number generators
--    Comm ACM, 49(10), Oct 2014, pp453-472.
--
-- 2. Nikos Baxevanis
--    https://github.com/moodmosaic/SplitMix/blob/master/SplitMix.hs
--
module Hedgehog.Internal.Seed (
    Seed(..)
  , random
  , from
  , split
  , nextInteger
  , nextDouble

  -- * Internal
  -- $internal
  , goldenGamma
  , nextInt64
  , nextInt32
  , mix64
  , mix64variant13
  , mix32
  , mixGamma
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Bifunctor (first)
import           Data.Bits ((.|.), xor, shiftR, popCount)
import           Data.Int (Int32, Int64)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           System.IO.Unsafe (unsafePerformIO)
import           System.Random (RandomGen)
import qualified System.Random as Random

-- | A splittable random number generator.
--
data Seed =
  Seed {
      seedValue :: !Int64
    , seedGamma :: !Int64 -- ^ must be an odd number
    } deriving (Eq, Ord)

instance Show Seed where
  showsPrec p (Seed v g) =
    showParen (p > 10) $
      showString "Seed " .
      showsPrec 11 v .
      showChar ' ' .
      showsPrec 11 g

instance Read Seed where
  readsPrec p =
    readParen (p > 10) $ \r0 -> do
      ("Seed", r1) <- lex r0
      (v, r2) <- readsPrec 11 r1
      (g, r3) <- readsPrec 11 r2
      pure (Seed v g, r3)

global :: IORef Seed
global =
  unsafePerformIO $ do
    -- FIXME use /dev/urandom on posix
    seconds <- getPOSIXTime
    IORef.newIORef $ from (round (seconds * 1000))
{-# NOINLINE global #-}

-- | Create a random 'Seed' using an effectful source of randomness.
--
random :: MonadIO m => m Seed
random =
  liftIO $ IORef.atomicModifyIORef' global split

-- | Create a 'Seed' using an 'Int64'.
--
from :: Int64 -> Seed
from x =
  Seed x goldenGamma

-- | A predefined gamma value's needed for initializing the "root" instances of
--   'Seed'. That is, instances not produced by splitting an already existing
--   instance.
--
--   We choose: the odd integer closest to @2^64/φ@, where @φ = (1 + √5)/2@ is
--   the golden ratio.
--
goldenGamma :: Int64
goldenGamma =
  -7046029254386353131

-- | Get the next value in the SplitMix sequence.
--
next :: Seed -> (Int64, Seed)
next (Seed v0 g) =
  let
    v = v0 + g
  in
    (v, Seed v g)

-- | Splits a random number generator in to two.
--
split :: Seed -> (Seed, Seed)
split s0 =
  let
    (v0, s1) = next s0
    (g0, s2) = next s1
  in
    (s2, Seed (mix64 v0) (mixGamma g0))

-- | Generate a random 'Int64'.
--
nextInt64 :: Seed -> (Int64, Seed)
nextInt64 s0 =
  let
    (v0, s1) = next s0
  in
    (mix64 v0, s1)

-- | Generate a random 'Int32'.
--
nextInt32 :: Seed -> (Int32, Seed)
nextInt32 s0 =
  let
    (v0, s1) = next s0
  in
    (mix32 v0, s1)

-- | Generate a random 'Integer' in the [inclusive,inclusive] range.
--
nextInteger :: Integer -> Integer -> Seed -> (Integer, Seed)
nextInteger lo hi =
  Random.randomR (lo, hi)

-- | Generate a random 'Double' in the [inclusive,exclusive) range.
--
nextDouble :: Double -> Double -> Seed -> (Double, Seed)
nextDouble lo hi =
  Random.randomR (lo, hi)

mix64 :: Int64 -> Int64
mix64 x =
  let
    y = (x `xor` (x `shiftR` 33)) * (-49064778989728563)
    z = (y `xor` (y `shiftR` 33)) * (-4265267296055464877)
  in
    z `xor` (z `shiftR` 33)

mix32 :: Int64 -> Int32
mix32 x =
  let
    y = (x `xor` (x `shiftR` 33)) * (-49064778989728563)
    z = (y `xor` (y `shiftR` 33)) * (-4265267296055464877)
  in
    fromIntegral (z `shiftR` 32)

mix64variant13 :: Int64 -> Int64
mix64variant13 x =
  let
    y = (x `xor` (x `shiftR` 30)) * (-4658895280553007687)
    z = (y `xor` (y `shiftR` 27)) * (-7723592293110705685)
  in
    z `xor` (z `shiftR` 31)

mixGamma :: Int64 -> Int64
mixGamma x =
  let
    y = mix64variant13 x .|. 1
    n = popCount $ y `xor` (y `shiftR` 1)
  in
    if n >= 24 then
      y `xor` (-6148914691236517206)
    else
      y

------------------------------------------------------------------------
-- RandomGen instances

#include "MachDeps.h"

#if (SIZEOF_HSINT == 8)
instance RandomGen Seed where
  next =
    first fromIntegral . nextInt64
  genRange _ =
    (fromIntegral (minBound :: Int64), fromIntegral (maxBound :: Int64))
  split =
    split
#else
instance RandomGen Seed where
  next =
    first fromIntegral . nextInt32
  genRange _ =
    (fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32))
  split =
    split
#endif

------------------------------------------------------------------------
-- Internal

-- $internal
--
-- These functions are exported in case you need them in a pinch, but are not
-- part of the public API and may change at any time, even as part of a minor
-- update.
