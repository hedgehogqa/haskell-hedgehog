module Hedgehog.Internal.Tripping (
    tripping
  ) where

import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source


-- | Test that a pair of encode / decode functions are compatible.
--
tripping ::
     (MonadTest m, Applicative f, Show b, Show (f a), Eq (f a), HasCallStack)
  => a
  -> (a -> b)
  -> (b -> f a)
  -> m ()
tripping x encode decode =
  let
    mx =
      pure x

    i =
      encode x

    my =
      decode i
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
              , "━━━ Intermediate ━━━"
              , showPretty i
              , "━━━ Roundtrip ━━━"
              , showPretty my
              ]

        Just diff ->
          withFrozenCallStack $
            failWith
              (Just $ Diff "━━━ " "- Original" "/" "+ Roundtrip" " ━━━" diff) $
              unlines [
                  "━━━ Intermediate ━━━"
                , showPretty i
                ]
