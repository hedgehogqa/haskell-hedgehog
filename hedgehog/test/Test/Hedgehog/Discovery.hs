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

-- | A double quote inside a character literal must not open a string,
--   otherwise the next real string flips the classifier inside-out and a
--   @{-@ in that string hides the declarations that follow it.
--
prop_char_literal_quote_is_not_a_string :: Property
prop_char_literal_quote_is_not_a_string =
  withTests 1 . property $
    discovered
      [ "module Test where"
      , ""
      , "quote = '\"'"
      , ""
      , "banner = \"{- inside a string\""
      , ""
      , "prop_after = ()"
      ] === ["prop_after"]

-- | An escaped quote must not end the string early, otherwise the @{-@
--   after it opens a block comment.
--
prop_escaped_quote_does_not_end_string :: Property
prop_escaped_quote_does_not_end_string =
  withTests 1 . property $
    discovered
      [ "module Test where"
      , ""
      , "banner = \"\\\" {- still inside\""
      , ""
      , "prop_after = ()"
      ] === ["prop_after"]

-- | A @--@ inside a string literal is not a line comment.
--
prop_line_comment_token_in_string_is_code :: Property
prop_line_comment_token_in_string_is_code =
  withTests 1 . property $
    discovered
      [ "module Test where"
      , ""
      , "dashes = \"-- {-\""
      , ""
      , "prop_after = ()"
      ] === ["prop_after"]

-- | Block comment tokens inside a string literal must not change the
--   comment nesting level.
--
prop_block_comment_tokens_in_string_are_code :: Property
prop_block_comment_tokens_in_string_are_code =
  withTests 1 . property $
    discovered
      [ "module Test where"
      , ""
      , "block = \"{- -}\""
      , ""
      , "banner = \"{- inside a string\""
      , ""
      , "prop_after = ()"
      ] === ["prop_after"]

discovered :: [String] -> [String]
discovered =
  fmap unPropertyName . Map.keys . findProperties "prop_" "Test.hs" . unlines

tests :: IO Bool
tests =
  checkParallel $$(discover)
