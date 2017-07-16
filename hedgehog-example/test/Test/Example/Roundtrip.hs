--
-- See the original post for a description of this example:
--
--   http://teh.id.au/posts/2017/06/07/round-trip-property
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.Roundtrip where

import           Control.Monad (void, guard)
import           Control.Applicative (Alternative(..))

import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Text.Parsec (ParseError)
import qualified Text.Parsec as Parsec
import           Text.Parsec.Text (Parser)
import           Text.Printf (printf)
import           Text.Read (readMaybe)


data Token =
    TInt Int
  | TFloat Double
  | TAssign
  | TPlus
  | TPlusEq
  | TVar Text
  | TLParen
  | TRParen
  | TIncrement
  | TSeparator
    deriving (Eq, Ord, Show)

renderToken :: Token -> Text
renderToken t =
  case t of
    TInt x ->
      Text.pack (show x)
    TFloat d ->
      Text.pack (printf "%6f" d)
    TAssign ->
      ":="
    TPlus ->
      "+"
    TPlusEq ->
      "+="
    TVar x ->
      x
    TLParen ->
      "("
    TRParen ->
      ")"
    TIncrement ->
      "++"
    TSeparator ->
      ";"

pretty :: [Token] -> Text
pretty =
  Text.intercalate " " . fmap renderToken

whitespace :: Parser ()
whitespace =
  void (many Parsec.space)

parseInt :: Parser Int
parseInt = do
  xyz <- Parsec.many1 Parsec.digit
  case readMaybe xyz of
    Just x ->
      pure x
    Nothing ->
      empty

parseDouble :: Parser Double
parseDouble = do
  a <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '.'
  b <- Parsec.many1 Parsec.digit
  guard (length b <= 6)
  case readMaybe (a ++ "." ++ b) of
    Just x ->
      pure x
    Nothing ->
      empty

parseVar :: Parser Text
parseVar =
  fmap Text.pack (Parsec.many1 Parsec.letter)

parseToken :: Parser Token
parseToken =
  Parsec.choice [
      TFloat <$> Parsec.try parseDouble
    , TInt <$> parseInt
    , TVar <$> parseVar
    , Parsec.try (Parsec.string "++") *> pure TIncrement
    , Parsec.try (Parsec.string "+=") *> pure TPlusEq
    , Parsec.string ":=" *> pure TAssign
    , Parsec.string "+" *> pure TPlus
    , Parsec.string "(" *> pure TLParen
    , Parsec.string ")" *> pure TRParen
    , Parsec.string ";" *> pure TSeparator
    ]

parseTokens :: Parser [Token]
parseTokens = do
  whitespace
  Parsec.many (parseToken <* whitespace)

parse :: Text -> Either ParseError [Token]
parse =
  -- You almost always want to parse until EOF when using Parsec!
  Parsec.parse (parseTokens <* Parsec.eof) ""

------------------------------------------------------------------------

genTokens :: Gen [Token]
genTokens =
  Gen.list (Range.linear 0 100) genToken

round6 :: Double -> Double
round6 x =
  fromInteger (round (x * (10 ^ (6 :: Int)))) / 10.0 ^^ (6 :: Int)

genToken :: Gen Token
genToken =
  Gen.choice [
      TInt <$> Gen.int (Range.linear 0 maxBound)
    , TFloat . round6 <$> Gen.double (Range.exponentialFloat 0.0 9223372036854775807.9)
    , pure TAssign
    , pure TPlus
    , pure TPlusEq
    , TVar <$> Gen.text (Range.linear 1 20) Gen.alpha
    , pure TLParen
    , pure TRParen
    , pure TIncrement
    , pure TSeparator
    ]

prop_trip :: Property
prop_trip =
  withTests 1000 . property $ do
    toks <- forAll genTokens
    tripping toks pretty parse

tests :: IO Bool
tests =
  checkParallel $$(discover)
