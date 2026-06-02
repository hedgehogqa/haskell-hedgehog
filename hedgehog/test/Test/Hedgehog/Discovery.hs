{-# LANGUAGE TemplateHaskell #-}
module Test.Hedgehog.Discovery where

import qualified Data.Map as Map

import           Hedgehog
import           Hedgehog.Internal.Discovery (findProperties)
import           Hedgehog.Internal.Property (unPropertyName)

-- | A @"{-"@ appearing inside a string literal must not be treated as the
--   start of a block comment, otherwise every declaration after it is hidden
--   from property discovery.
--
--   https://github.com/hedgehogqa/haskell-hedgehog/issues/570
--
prop_string_with_comment_token_is_not_a_comment :: Property
prop_string_with_comment_token_is_not_a_comment =
  withTests 1 . property $ do
    let
      source =
        unlines
          [ "module Test where"
          , ""
          , "brace = \"{-\""
          , ""
          , "prop_after = ()"
          ]

      found =
        fmap unPropertyName . Map.keys $ findProperties "prop_" "Test.hs" source

    found === ["prop_after"]

tests :: IO Bool
tests =
  checkParallel $$(discover)
