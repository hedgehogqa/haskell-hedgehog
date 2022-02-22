module Hedgehog.Gen (
  -- ** Shrinking
    shrink
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
  , filter
  , filterT
  , mapMaybe
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

  -- ** Abstract State Machine
  , sequential
  , parallel

  -- * Sampling Generators
  , sample
  , print
  , printTree
  , printWith
  , printTreeWith
  ) where

import           Hedgehog.Internal.Gen
import           Hedgehog.Internal.State (sequential, parallel)

import           Prelude hiding (either, filter, print, maybe, map, seq)
