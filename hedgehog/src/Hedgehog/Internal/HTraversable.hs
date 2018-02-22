{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE RankNTypes #-}
module Hedgehog.Internal.HTraversable (
    HTraversable(..)
  ) where


-- | Higher-order traversable functors.
--
-- This is used internally to make symbolic variables concrete given an 'Environment'.
--
-- The type checker will ask you to write 'HTraversable' instances for your action types.  If you do
-- the trivial thing:
--
-- @
-- data YourAction (v :: * -> *) = YourAction (Var YourType v)
--   deriving (Eq, Show)
--
-- instance HTraversable YourAction where
--   htraverse _ (YourAction var) = pure (YourAction var)  -- wrong!
-- @
--
-- The type checker will complain that it @couldn't match type ‘g’ with ‘h’@.  To fix this, you need
-- to 'htraverse' into your structure like so:
--
-- @
-- instance HTraversable YourAction where
--   htraverse f (YourAction var) = YourACtion <$> htraverse f var  -- correct!
-- @
--
class HTraversable t where
  htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
