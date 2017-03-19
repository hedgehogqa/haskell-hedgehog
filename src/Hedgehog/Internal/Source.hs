{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Hedgehog.Internal.Source (
    LineNo(..)
  , ColumnNo(..)
  , Span(..)
  , getCaller

  -- * Re-exports from 'GHC.Stack'
  , CallStack
  , HasCallStack
  , callStack
  , withFrozenCallStack
  ) where

#if MIN_VERSION_base(4,9,0)
import GHC.Stack (CallStack, HasCallStack, SrcLoc(..))
import GHC.Stack (callStack, getCallStack, withFrozenCallStack)
#else
import GHC.Exts (Constraint)
#endif

newtype LineNo =
  LineNo {
      unLineNo :: Int
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

newtype ColumnNo =
  ColumnNo {
      unColumnNo :: Int
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

data Span =
  Span {
      spanFile :: !FilePath
    , spanStartLine :: !LineNo
    , spanStartColumn :: !ColumnNo
    , spanEndLine :: !LineNo
    , spanEndColumn :: !ColumnNo
    } deriving (Eq, Ord)

#if !MIN_VERSION_base(4,9,0)
type family HasCallStack :: Constraint where
  HasCallStack = ()

data CallStack =
  CallStack
  deriving (Show)

callStack :: HasCallStack => CallStack
callStack =
  CallStack

withFrozenCallStack :: HasCallStack => (HasCallStack => a) -> a
withFrozenCallStack x =
  x
#endif

getCaller :: CallStack -> Maybe Span
#if MIN_VERSION_base(4,9,0)
getCaller stack =
  case getCallStack stack of
    [] ->
      Nothing
    (_, x) : _ ->
      Just $ Span
        (srcLocFile x)
        (fromIntegral $ srcLocStartLine x)
        (fromIntegral $ srcLocStartCol x)
        (fromIntegral $ srcLocEndLine x)
        (fromIntegral $ srcLocEndCol x)
#else
getCaller _ =
  Nothing
#endif

------------------------------------------------------------------------
-- Show instances

instance Show Span where
  showsPrec p (Span file sl sc el ec) =
    showParen (p > 10) $
      showString "Span " .
      showsPrec 11 file .
      showChar ' ' .
      showsPrec 11 sl .
      showChar ' ' .
      showsPrec 11 sc .
      showChar ' ' .
      showsPrec 11 el .
      showChar ' ' .
      showsPrec 11 ec

instance Show LineNo where
  showsPrec p (LineNo x) =
    showParen (p > 10) $
      showString "LineNo " .
      showsPrec 11 x

instance Show ColumnNo where
  showsPrec p (ColumnNo x) =
    showParen (p > 10) $
      showString "ColumnNo " .
      showsPrec 11 x
