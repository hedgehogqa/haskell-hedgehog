{-# LANGUAGE OverloadedStrings #-}
module Test.Hedgehog.Seed (
    tests
  ) where

import           Hedgehog
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Property (PropertyName(..))

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Hedgehog.Seed" $
    -- | Verify that SplitMix avoids pathological γ-values, as discussed by
    --   Melissa E. O'Neill in the post with title Bugs in SplitMix(es), at
    --   http://www.pcg-random.org/posts/bugs-in-splitmix.html
    --
    --   See also:
    --   https://github.com/hedgehogqa/haskell-hedgehog/issues/191
    --
    do
    (expected, actual) <-
      [
        ( Seed 15210016002011668638 12297829382473034411
        , Seed.from 0x61c8864680b583eb
        )
      , ( Seed 11409286845259996466 12297829382473034411
        , Seed.from 0xf8364607e9c949bd
        )
      , ( Seed 1931727433621677744 12297829382473034411
        , Seed.from 0x88e48f4fcc823718
        )
      , ( Seed 307741759840609752 12297829382473034411
        , Seed.from 0x7f83ab8da2e71dd1
        )
      , ( Seed 8606169619657412120 12297829382473034413
        , Seed.from 0x7957d809e827ff4c
        )
      , ( Seed 13651108307767328632 12297829382473034413
        , Seed.from 0xf8d059aee4c53639
        )
      , ( Seed 125750466559701114 12297829382473034413
        , Seed.from 0x9cd9f015db4e58b7
        )
      , ( Seed 6781260234005250507 12297829382473034413
        , Seed.from 0xf4077b0dbebc73c0
        )
      , ( Seed 15306535823716590088 12297829382473034405
        , Seed.from 0x305cb877109d0686
        )
      , ( Seed 7344074043290227165 12297829382473034405
        , Seed.from 0x359e58eeafebd527
        )
      , ( Seed 9920554987610416076 12297829382473034405
        , Seed.from 0xbeb721c511b0da6d
        )
      , ( Seed 3341781972484278810 12297829382473034405
        , Seed.from 0x86466fd0fcc363a6
        )
      , ( Seed 12360157267739240775 12297829382473034421
        , Seed.from 0xefee3e7b93db3075
        )
      , ( Seed 600595566262245170 12297829382473034421
        , Seed.from 0x79629ee76aa83059
        )
      , ( Seed 1471112649570176389 12297829382473034421
        , Seed.from 0x05d507d05e785673
        )
      , ( Seed 8100917074368564322 12297829382473034421
        , Seed.from 0x76442b62dddf926c
        )
      ]
    return
      ( PropertyName $ "prop_avoid_pathological_gamma_values → " ++ show actual ++ ","
      , withTests 1 . property $ do
          footnote $ "expected: " ++ show expected
          footnote $ "actual: " ++ show actual
          expected === actual
      )
