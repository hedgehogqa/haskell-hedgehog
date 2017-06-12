module Hedgehog.Internal.Exception (
    TypedException(..)
  , tryAll
  ) where

import           Control.Exception (Exception(..), AsyncException, SomeException(..))
import           Control.Monad.Catch (MonadCatch(..), throwM)

import           Data.Typeable (typeOf)


-- | Newtype for 'SomeException' with a 'Show' instance that only contains
--   valid Haskell 98 tokens and also includes the type of the exception.
--
--   For example, when catching the exception thrown by @fail "foo" :: IO ()@
--   and calling show:
--
-- @
--   IOException "user error (foo)"
-- @
--
--   Having access to the type can be useful when trying to track down the
--   source of an exception.
--
newtype TypedException =
  TypedException SomeException

instance Show TypedException where
  showsPrec p (TypedException (SomeException x)) =
    showParen (p > 10) $
      showsPrec 11 (typeOf x) .
      showChar ' ' .
      showsPrec 11 (displayException x)

tryAll :: MonadCatch m => m a -> m (Either TypedException a)
tryAll m =
  catch (fmap Right m) $ \exception ->
    case fromException exception :: Maybe AsyncException of
      Nothing ->
        pure . Left $ TypedException exception
      Just async ->
        throwM async
