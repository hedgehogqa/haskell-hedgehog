module Hedgehog.Internal.Predicate
  ( fromPred
  ) where

import Control.Monad (guard)

-- | Convert a Boolean predicate to something suitable for use
-- with @mapMaybe@-like functions. The @wizards@ package calls this
-- @ensure@. @protolude@ and @relude@ generalize from 'Maybe' to
-- arbitrary 'Control.Applicative.Alternative' and call it @guarded@.
fromPred :: (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)
