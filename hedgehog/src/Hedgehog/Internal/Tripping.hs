module Hedgehog.Internal.Tripping (
    tripping
  ) where

import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source


-- | Test that a pair of render / parse functions are compatible.
--
tripping ::
     HasCallStack
  => Applicative f
  => Monad m
  => Show (f a)
  => Eq (f a)
  => a
  -> (a -> b)
  -> (b -> f a)
  -> Test m ()
tripping x render parse =
  let
    mx =
      pure x

    my =
      (parse . render) x
  in
    if mx == my then
      success
    else
      case valueDiff <$> mkValue mx <*> mkValue my of
        Nothing ->
          withFrozenCallStack $
            failWith Nothing $ unlines [
                "━━━ Original ━━━"
              , showPretty mx
              , "━━━ Roundtrip ━━━"
              , showPretty my
              ]

        Just diff ->
          withFrozenCallStack $
              failWith
                (Just $ Diff "Failed (" "- Original" "/" "+ Roundtrip" ")" diff)
                ""
