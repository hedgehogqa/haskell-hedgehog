{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Hedgehog.Internal.Show (
    Name
  , Value(..)
  , ValueDiff(..)
  , LineDiff(..)

  , mkValue
  , showPretty

  , valueDiff
  , lineDiff
  , toLineDiff

  , renderValue
  , renderValueDiff
  , renderLineDiff

  , takeLeft
  , takeRight
  ) where

import           Data.Bifunctor (second)

import           Text.Show.Pretty (Value(..), Name, reify, valToStr, ppShow)


data ValueDiff =
    ValueCon Name [ValueDiff]
  | ValueRec Name [(Name, ValueDiff)]
  | ValueTuple [ValueDiff]
  | ValueList [ValueDiff]
  | ValueSame Value
  | ValueDiff Value Value
    deriving (Eq, Show)

data LineDiff =
    LineSame String
  | LineRemoved String
  | LineAdded String
    deriving (Eq, Show)

data DocDiff =
    DocSame Int String
  | DocRemoved Int String
  | DocAdded Int String
  | DocOpen Int String
  | DocItem Int String [DocDiff]
  | DocClose Int String
    deriving (Eq, Show)

renderValue :: Value -> String
renderValue =
  valToStr

renderValueDiff :: ValueDiff -> String
renderValueDiff =
  unlines .
  fmap renderLineDiff .
  toLineDiff

renderLineDiff :: LineDiff -> String
renderLineDiff = \case
  LineSame x ->
    "  " ++ x
  LineRemoved x ->
    "- " ++ x
  LineAdded x ->
    "+ " ++ x

mkValue :: Show a => a -> Maybe Value
mkValue =
  reify

showPretty :: Show a => a -> String
showPretty =
  ppShow

lineDiff :: Value -> Value -> [LineDiff]
lineDiff x y =
  toLineDiff $ valueDiff x y

toLineDiff :: ValueDiff -> [LineDiff]
toLineDiff =
  concatMap (mkLineDiff 0 "") .
  collapseOpen .
  dropLeadingSep .
  mkDocDiff 0

valueDiff :: Value -> Value -> ValueDiff
valueDiff x y =
  if x == y then
    ValueSame x
  else
    case (x, y) of
      (Con nx xs, Con ny ys)
        | nx == ny
        , length xs == length ys
        ->
          ValueCon nx (zipWith valueDiff xs ys)

      (Rec nx nxs, Rec ny nys)
        | nx == ny
        , fmap fst nxs == fmap fst nys
        , ns <- fmap fst nxs
        , xs <- fmap snd nxs
        , ys <- fmap snd nys
        ->
          ValueRec nx (zip ns (zipWith valueDiff xs ys))

      (Tuple xs, Tuple ys)
        | length xs == length ys
        ->
          ValueTuple (zipWith valueDiff xs ys)

      (List xs, List ys)
        | length xs == length ys
        ->
          ValueList (zipWith valueDiff xs ys)

      _ ->
        ValueDiff x y

takeLeft :: ValueDiff -> Value
takeLeft = \case
  ValueCon n xs ->
    Con n (fmap takeLeft xs)
  ValueRec n nxs ->
    Rec n (fmap (second takeLeft) nxs)
  ValueTuple xs ->
    Tuple (fmap takeLeft xs)
  ValueList xs ->
    List (fmap takeLeft xs)
  ValueSame x ->
    x
  ValueDiff x _ ->
    x

takeRight :: ValueDiff -> Value
takeRight = \case
  ValueCon n xs ->
    Con n (fmap takeRight xs)
  ValueRec n nxs ->
    Rec n (fmap (second takeRight) nxs)
  ValueTuple xs ->
    Tuple (fmap takeRight xs)
  ValueList xs ->
    List (fmap takeRight xs)
  ValueSame x ->
    x
  ValueDiff _ x ->
    x

mkLineDiff :: Int -> String -> DocDiff -> [LineDiff]
mkLineDiff indent0 prefix0 diff =
  let
    mkLinePrefix indent =
      spaces indent0 ++ prefix0 ++ spaces indent

    mkLineIndent indent =
      indent0 + length prefix0 + indent
  in
    case diff of
      DocSame indent x ->
        [LineSame $ mkLinePrefix indent ++ x]

      DocRemoved indent x ->
        [LineRemoved $ mkLinePrefix indent ++ x]

      DocAdded indent x ->
        [LineAdded $ mkLinePrefix indent ++ x]

      DocOpen indent x ->
        [LineSame $ mkLinePrefix indent ++ x]

      DocItem _ _ [] ->
        []

      DocItem indent prefix (x@DocRemoved{} : y@DocAdded{} : xs) ->
        mkLineDiff (mkLineIndent indent) prefix x ++
        mkLineDiff (mkLineIndent indent) prefix y ++
        concatMap (mkLineDiff (mkLineIndent (indent + length prefix)) "") xs

      DocItem indent prefix (x : xs) ->
        mkLineDiff (mkLineIndent indent) prefix x ++
        concatMap (mkLineDiff (mkLineIndent (indent + length prefix)) "") xs

      DocClose indent x ->
        [LineSame $ spaces (mkLineIndent indent) ++ x]

spaces :: Int -> String
spaces indent =
  replicate indent ' '

collapseOpen :: [DocDiff] -> [DocDiff]
collapseOpen = \case
  DocSame indent line : DocOpen _ bra : xs ->
    DocSame indent (line ++ " " ++ bra) : collapseOpen xs
  DocItem indent prefix xs : ys ->
    DocItem indent prefix (collapseOpen xs) : collapseOpen ys
  x : xs ->
    x : collapseOpen xs
  [] ->
    []

dropLeadingSep :: [DocDiff] -> [DocDiff]
dropLeadingSep = \case
  DocOpen oindent bra : DocItem indent prefix xs : ys ->
    DocOpen oindent bra : DocItem (indent + length prefix) "" (dropLeadingSep xs) : dropLeadingSep ys
  DocItem indent prefix xs : ys ->
    DocItem indent prefix (dropLeadingSep xs) : dropLeadingSep ys
  x : xs ->
    x : dropLeadingSep xs
  [] ->
    []

mkDocDiff :: Int -> ValueDiff -> [DocDiff]
mkDocDiff indent = \case
  ValueSame x ->
    same indent (renderValue x)

  diff
    | x <- takeLeft diff
    , y <- takeRight diff
    , oneLiner x
    , oneLiner y
    ->
      removed indent (renderValue x) ++
      added indent (renderValue y)

  ValueCon n xs ->
    same indent n ++
    concatMap (mkDocDiff (indent + 2)) xs

  ValueRec n nxs ->
    same indent n ++
    [DocOpen indent "{"] ++
    fmap (\(name, x) -> DocItem (indent + 2) ", " (same 0 (name ++ " =") ++ mkDocDiff 2 x)) nxs ++
    [DocClose (indent + 2) "}"]

  ValueTuple xs ->
    [DocOpen indent "("] ++
    fmap (DocItem indent ", " . mkDocDiff 0) xs ++
    [DocClose indent ")"]

  ValueList xs ->
    [DocOpen indent "["] ++
    fmap (DocItem indent ", " . mkDocDiff 0) xs ++
    [DocClose indent "]"]

  ValueDiff x y ->
    removed indent (renderValue x) ++
    added indent (renderValue y)

oneLiner :: Value -> Bool
oneLiner x =
  case lines (renderValue x) of
    _ : _ : _ ->
      False
    _ ->
      True

same :: Int -> String -> [DocDiff]
same indent =
  fmap (DocSame indent) . lines

removed :: Int -> String -> [DocDiff]
removed indent =
  fmap (DocRemoved indent) . lines

added :: Int -> String -> [DocDiff]
added indent =
  fmap (DocAdded indent) . lines
