{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hedgehog.Internal.Report (
  -- * Report
    Summary(..)
  , Report(..)
  , Progress(..)
  , Result(..)
  , FailureReport(..)
  , FailedAnnotation(..)

  , Style(..)
  , Markup(..)

  , renderProgress
  , renderResult
  , renderSummary
  , renderDoc

  , ppProgress
  , ppResult
  , ppSummary

  , fromResult
  , mkFailure
  ) where

import           Control.Monad (zipWithM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))

import           Data.Bifunctor (bimap, first, second)
import qualified Data.Char as Char
import           Data.Either (partitionEithers)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, catMaybes)
import           Data.Traversable (for)

import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Discovery (Pos(..), Position(..))
import qualified Hedgehog.Internal.Discovery as Discovery
import           Hedgehog.Internal.Prelude
import           Hedgehog.Internal.Property (CoverCount(..), CoverPercentage(..))
import           Hedgehog.Internal.Property (Coverage(..), Label(..), LabelName(..))
import           Hedgehog.Internal.Property (PropertyName(..), Log(..), Diff(..))
import           Hedgehog.Internal.Property (ShrinkCount(..), PropertyCount(..))
import           Hedgehog.Internal.Property (TestCount(..), DiscardCount(..))
import           Hedgehog.Internal.Property (coverPercentage, coverageFailures)
import           Hedgehog.Internal.Property (labelCovered)

import           Hedgehog.Internal.Seed (Seed)
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source
import           Hedgehog.Range (Size)

import           System.Console.ANSI (ColorIntensity(..), Color(..))
import           System.Console.ANSI (ConsoleLayer(..), ConsoleIntensity(..))
import           System.Console.ANSI (SGR(..), setSGRCode)
import           System.Directory (makeRelativeToCurrentDirectory)

#if mingw32_HOST_OS
import           System.IO (hSetEncoding, stdout, stderr, utf8)
#endif

import           Text.PrettyPrint.Annotated.WL (Doc, (<#>), (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL
import           Text.Printf (printf)

------------------------------------------------------------------------
-- Data

data FailedAnnotation =
  FailedAnnotation {
      failedSpan :: !(Maybe Span)
    , failedValue :: !String
    } deriving (Eq, Show)

data FailureReport =
  FailureReport {
      failureSize :: !Size
    , failureSeed :: !Seed
    , failureShrinks :: !ShrinkCount
    , failureCoverage :: !(Maybe (Coverage CoverCount))
    , failureAnnotations :: ![FailedAnnotation]
    , failureLocation :: !(Maybe Span)
    , failureMessage :: !String
    , failureDiff :: !(Maybe Diff)
    , failureFootnotes :: ![String]
    } deriving (Eq, Show)

-- | The status of a running property test.
--
data Progress =
    Running
  | Shrinking !FailureReport
    deriving (Eq, Show)

-- | The status of a completed property test.
--
--   In the case of a failure it provides the seed used for the test, the
--   number of shrinks, and the execution log.
--
data Result =
    Failed !FailureReport
  | GaveUp
  | OK
    deriving (Eq, Show)

-- | A report on a running or completed property test.
--
data Report a =
  Report {
      reportTests :: !TestCount
    , reportDiscards :: !DiscardCount
    , reportCoverage :: !(Coverage CoverCount)
    , reportStatus :: !a
    } deriving (Show, Functor, Foldable, Traversable)

-- | A summary of all the properties executed.
--
data Summary =
  Summary {
      summaryWaiting :: !PropertyCount
    , summaryRunning :: !PropertyCount
    , summaryFailed :: !PropertyCount
    , summaryGaveUp :: !PropertyCount
    , summaryOK :: !PropertyCount
    } deriving (Show)

instance Monoid Summary where
  mempty =
    Summary 0 0 0 0 0
  mappend (Summary x1 x2 x3 x4 x5) (Summary y1 y2 y3 y4 y5) =
    Summary
      (x1 + y1)
      (x2 + y2)
      (x3 + y3)
      (x4 + y4)
      (x5 + y5)

instance Semigroup Summary where
  (<>) = mappend

-- | Construct a summary from a single result.
--
fromResult :: Result -> Summary
fromResult = \case
  Failed _ ->
    mempty { summaryFailed = 1 }
  GaveUp ->
    mempty { summaryGaveUp = 1 }
  OK ->
    mempty { summaryOK = 1 }

summaryCompleted :: Summary -> PropertyCount
summaryCompleted (Summary _ _ x3 x4 x5) =
  x3 + x4 + x5

summaryTotal :: Summary -> PropertyCount
summaryTotal (Summary x1 x2 x3 x4 x5) =
  x1 + x2 + x3 + x4 + x5

------------------------------------------------------------------------
-- Pretty Printing Helpers

data Line a =
  Line {
      _lineAnnotation :: !a
    , lineNumber :: !LineNo
    , _lineSource :: !String
    } deriving (Eq, Ord, Show, Functor)

data Declaration a =
  Declaration {
      declarationFile :: !FilePath
    , declarationLine :: !LineNo
    , _declarationName :: !String
    , declarationSource :: !(Map LineNo (Line a))
    } deriving (Eq, Ord, Show, Functor)

data Style =
    StyleDefault
  | StyleAnnotation
  | StyleFailure
    deriving (Eq, Ord, Show)

data Markup =
    WaitingIcon
  | WaitingHeader
  | RunningIcon
  | RunningHeader
  | ShrinkingIcon
  | ShrinkingHeader
  | FailedIcon
  | FailedText
  | GaveUpIcon
  | GaveUpText
  | SuccessIcon
  | SuccessText
  | CoverageIcon
  | CoverageText
  | CoverageFill
  | DeclarationLocation
  | StyledLineNo !Style
  | StyledBorder !Style
  | StyledSource !Style
  | AnnotationGutter
  | AnnotationValue
  | FailureArrows
  | FailureGutter
  | FailureMessage
  | DiffPrefix
  | DiffInfix
  | DiffSuffix
  | DiffSame
  | DiffRemoved
  | DiffAdded
  | ReproduceHeader
  | ReproduceGutter
  | ReproduceSource
    deriving (Eq, Ord, Show)

instance Semigroup Style where
  (<>) x y =
    case (x, y) of
      (StyleFailure, _) ->
        StyleFailure
      (_, StyleFailure) ->
        StyleFailure
      (StyleAnnotation, _) ->
        StyleAnnotation
      (_, StyleAnnotation) ->
        StyleAnnotation
      (StyleDefault, _) ->
        StyleDefault

------------------------------------------------------------------------

takeAnnotation :: Log -> Maybe FailedAnnotation
takeAnnotation = \case
  Annotation loc val ->
    Just $ FailedAnnotation loc val
  _ ->
    Nothing

takeFootnote :: Log -> Maybe String
takeFootnote = \case
  Footnote x ->
    Just x
  _ ->
    Nothing

mkFailure ::
     Size
  -> Seed
  -> ShrinkCount
  -> Maybe (Coverage CoverCount)
  -> Maybe Span
  -> String
  -> Maybe Diff
  -> [Log]
  -> FailureReport
mkFailure size seed shrinks mcoverage location message diff logs =
  let
    inputs =
      mapMaybe takeAnnotation logs

    footnotes =
      mapMaybe takeFootnote logs
  in
    FailureReport size seed shrinks mcoverage inputs location message diff footnotes

------------------------------------------------------------------------
-- Pretty Printing

ppShow :: Show x => x -> Doc a
ppShow = -- unfortunate naming clash
  WL.text . show

markup :: Markup -> Doc Markup -> Doc Markup
markup =
  WL.annotate

gutter :: Markup -> Doc Markup -> Doc Markup
gutter m x =
  markup m ">" <+> x

icon :: Markup -> Char -> Doc Markup -> Doc Markup
icon m i x =
  markup m (WL.char i) <+> x

ppTestCount :: TestCount -> Doc a
ppTestCount = \case
  TestCount 1 ->
    "1 test"
  TestCount n ->
    ppShow n <+> "tests"

ppDiscardCount :: DiscardCount -> Doc a
ppDiscardCount = \case
  DiscardCount 1 ->
    "1 discard"
  DiscardCount n ->
    ppShow n <+> "discards"

ppShrinkCount :: ShrinkCount -> Doc a
ppShrinkCount = \case
  ShrinkCount 1 ->
    "1 shrink"
  ShrinkCount n ->
    ppShow n <+> "shrinks"

ppRawPropertyCount :: PropertyCount -> Doc a
ppRawPropertyCount (PropertyCount n) =
  ppShow n

ppWithDiscardCount :: DiscardCount -> Doc Markup
ppWithDiscardCount = \case
  DiscardCount 0 ->
    mempty
  n ->
    " with" <+> ppDiscardCount n

ppShrinkDiscard :: ShrinkCount -> DiscardCount -> Doc Markup
ppShrinkDiscard s d =
  case (s, d) of
    (0, 0) ->
      ""
    (0, _) ->
      " and" <+> ppDiscardCount d
    (_, 0) ->
      " and" <+> ppShrinkCount s
    (_, _) ->
      "," <+> ppShrinkCount s <+> "and" <+> ppDiscardCount d

mapSource :: (Map LineNo (Line a) -> Map LineNo (Line a)) -> Declaration a -> Declaration a
mapSource f decl =
  decl {
      declarationSource =
        f (declarationSource decl)
    }

-- | The span of non-whitespace characters for the line.
--
--   The result is @[inclusive, exclusive)@.
--
lineSpan :: Line a -> (ColumnNo, ColumnNo)
lineSpan (Line _ _ x0) =
  let
    (pre, x1) =
      span Char.isSpace x0

    (_, x2) =
      span Char.isSpace (reverse x1)

    start =
      length pre

    end =
      start + length x2
  in
    (fromIntegral start, fromIntegral end)

takeLines :: Span -> Declaration a -> Map LineNo (Line a)
takeLines sloc =
  fst . Map.split (spanEndLine sloc + 1) .
  snd . Map.split (spanStartLine sloc - 1) .
  declarationSource

readDeclaration :: MonadIO m => Span -> m (Maybe (Declaration ()))
readDeclaration sloc =
  runMaybeT $ do
    path <- liftIO . makeRelativeToCurrentDirectory $ spanFile sloc

    (name, Pos (Position _ line0 _) src) <- MaybeT $
      Discovery.readDeclaration path (spanEndLine sloc)

    let
      line =
        fromIntegral line0

    pure . Declaration path line name .
      Map.fromList .
      zip [line..] .
      zipWith (Line ()) [line..] $
      lines src


defaultStyle :: Declaration a -> Declaration (Style, [(Style, Doc Markup)])
defaultStyle =
  fmap $ const (StyleDefault, [])

lastLineSpan :: Monad m => Span -> Declaration a -> MaybeT m (ColumnNo, ColumnNo)
lastLineSpan sloc decl =
  case reverse . Map.elems $ takeLines sloc decl of
    [] ->
      MaybeT $ pure Nothing
    x : _ ->
      pure $
        lineSpan x

ppFailedInputTypedArgument :: Int -> FailedAnnotation -> Doc Markup
ppFailedInputTypedArgument ix (FailedAnnotation _ val) =
  WL.vsep [
      WL.text "forAll" <> ppShow ix <+> "="
    , WL.indent 2 . WL.vsep . fmap (markup AnnotationValue . WL.text) $ lines val
    ]

ppFailedInputDeclaration ::
     MonadIO m
  => FailedAnnotation
  -> m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailedInputDeclaration (FailedAnnotation msloc val) =
  runMaybeT $ do
    sloc <- MaybeT $ pure msloc
    decl <- fmap defaultStyle . MaybeT $ readDeclaration sloc
    startCol <- fromIntegral . fst <$> lastLineSpan sloc decl

    let
      ppValLine =
        WL.indent startCol .
          (markup AnnotationGutter (WL.text "│ ") <>) .
          markup AnnotationValue .
          WL.text

      valDocs =
        fmap ((StyleAnnotation, ) . ppValLine) $
        List.lines val

      startLine =
        fromIntegral $ spanStartLine sloc

      endLine =
        fromIntegral $ spanEndLine sloc

      styleInput kvs =
        foldr (Map.adjust . fmap . first $ const StyleAnnotation) kvs [startLine..endLine]

      insertDoc =
        Map.adjust (fmap . second $ const valDocs) endLine

    pure $
      mapSource (styleInput . insertDoc) decl

ppFailedInput ::
     MonadIO m
  => Int
  -> FailedAnnotation
  -> m (Either (Doc Markup) (Declaration (Style, [(Style, Doc Markup)])))
ppFailedInput ix input = do
  mdecl <- ppFailedInputDeclaration input
  case mdecl of
    Nothing ->
      pure . Left $ ppFailedInputTypedArgument ix input
    Just decl ->
      pure $ Right decl

ppLineDiff :: LineDiff -> Doc Markup
ppLineDiff = \case
  LineSame x ->
    markup DiffSame $
      "  " <> WL.text x

  LineRemoved x ->
    markup DiffRemoved $
      "- " <> WL.text x

  LineAdded x ->
    markup DiffAdded $
      "+ " <> WL.text x

ppDiff :: Diff -> [Doc Markup]
ppDiff (Diff prefix removed infix_ added suffix diff) = [
    markup DiffPrefix (WL.text prefix) <>
    markup DiffRemoved (WL.text removed) <>
    markup DiffInfix (WL.text infix_) <>
    markup DiffAdded (WL.text added) <>
    markup DiffSuffix (WL.text suffix)
  ] ++ fmap ppLineDiff (toLineDiff diff)

ppFailureLocation ::
     MonadIO m
  => [Doc Markup]
  -> Maybe Diff
  -> Span
  -> m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailureLocation msgs mdiff sloc =
  runMaybeT $ do
    decl <- fmap defaultStyle . MaybeT $ readDeclaration sloc
    (startCol, endCol) <- bimap fromIntegral fromIntegral <$> lastLineSpan sloc decl

    let
      arrowDoc =
        WL.indent startCol $
          markup FailureArrows (WL.text (replicate (endCol - startCol) '^'))

      ppFailure x =
        WL.indent startCol $
          markup FailureGutter (WL.text "│ ") <> x

      msgDocs =
        fmap ((StyleFailure, ) . ppFailure . markup FailureMessage) msgs

      diffDocs =
        case mdiff of
          Nothing ->
            []
          Just diff ->
            fmap ((StyleFailure, ) . ppFailure) (ppDiff diff)

      docs =
        [(StyleFailure, arrowDoc)] ++ msgDocs ++ diffDocs

      startLine =
        spanStartLine sloc

      endLine =
        spanEndLine sloc

      styleFailure kvs =
        foldr (Map.adjust . fmap . first $ const StyleFailure) kvs [startLine..endLine]

      insertDoc =
        Map.adjust (fmap . second $ const docs) endLine

    pure $
      mapSource (styleFailure . insertDoc) decl

ppDeclaration :: Declaration (Style, [(Style, Doc Markup)]) -> Doc Markup
ppDeclaration decl =
  case Map.maxView $ declarationSource decl of
    Nothing ->
      mempty
    Just (lastLine, _) ->
      let
        ppLocation =
          WL.indent (digits + 1) $
            markup (StyledBorder StyleDefault) "┏━━" <+>
            markup DeclarationLocation (WL.text (declarationFile decl)) <+>
            markup (StyledBorder StyleDefault) "━━━"

        digits =
          length . show . unLineNo $ lineNumber lastLine

        ppLineNo =
          WL.text . printf ("%" <> show digits <> "d") . unLineNo

        ppEmptyNo =
          WL.text $ replicate digits ' '

        ppSource style n src =
          markup (StyledLineNo style) (ppLineNo n) <+>
          markup (StyledBorder style) "┃" <+>
          markup (StyledSource style) (WL.text src)

        ppAnnot (style, doc) =
          markup (StyledLineNo style) ppEmptyNo <+>
          markup (StyledBorder style) "┃" <+>
          doc

        ppLines = do
          Line (style, xs) n src <- Map.elems $ declarationSource decl
          ppSource style n src : fmap ppAnnot xs
      in
        WL.vsep (ppLocation : ppLines)

ppReproduce :: Maybe PropertyName -> Size -> Seed -> Doc Markup
ppReproduce name size seed =
  WL.vsep [
      markup ReproduceHeader
        "This failure can be reproduced by running:"
    , gutter ReproduceGutter . markup ReproduceSource $
        "recheck" <+>
        WL.text (showsPrec 11 size "") <+>
        WL.text (showsPrec 11 seed "") <+>
        maybe "<property>" (WL.text . unPropertyName) name
    ]

mergeLine :: Semigroup a => Line a -> Line a -> Line a
mergeLine (Line x no src) (Line y _ _) =
  Line (x <> y) no src

mergeDeclaration :: Semigroup a => Declaration a -> Declaration a -> Declaration a
mergeDeclaration (Declaration file line name src0) (Declaration _ _ _ src1) =
  Declaration file line name $
  Map.unionWith mergeLine src0 src1

mergeDeclarations :: Semigroup a => [Declaration a] -> [Declaration a]
mergeDeclarations =
  Map.elems .
  Map.fromListWith mergeDeclaration .
  fmap (\d -> ((declarationFile d, declarationLine d), d))

ppTextLines :: String -> [Doc Markup]
ppTextLines =
  fmap WL.text . List.lines

ppFailureReport :: MonadIO m => Maybe PropertyName -> TestCount -> FailureReport -> m [Doc Markup]
ppFailureReport name tests (FailureReport size seed _ mcoverage inputs0 mlocation0 msg mdiff msgs0) = do
  (msgs1, mlocation) <-
    case mlocation0 of
      Nothing ->
        -- Move the failure message to the end section if we have
        -- no source location.
        let
          msgs1 =
            msgs0 ++
            (if null msg then [] else [msg])

          docs =
            concatMap ppTextLines msgs1 ++
            maybe [] ppDiff mdiff
        in
          pure (docs, Nothing)

      Just location0 ->
        fmap (concatMap ppTextLines msgs0,) $
          ppFailureLocation (fmap WL.text $ List.lines msg) mdiff location0

  coverageLocations <-
    case mcoverage of
      Nothing ->
        pure []
      Just coverage ->
        for (coverageFailures tests coverage) $ \(MkLabel _ mclocation _ count) ->
          case mclocation of
            Nothing ->
              pure Nothing
            Just clocation ->
              let
                coverageMsg =
                  WL.cat [
                      "Failed ("
                    , WL.annotate CoverageText $
                        ppCoverPercentage (coverPercentage tests count) <> " coverage"
                    , ")"
                    ]
              in
                ppFailureLocation [coverageMsg] Nothing clocation

  (args, idecls) <- fmap partitionEithers $ zipWithM ppFailedInput [0..] inputs0

  let
    decls =
      mergeDeclarations .
      catMaybes $
        mlocation : coverageLocations <> fmap pure idecls

    with xs f =
      if null xs then
        []
      else
        [f xs]

    whenSome f xs =
      if null xs then
        xs
      else
        f xs

    bottom =
      maybe [ppReproduce name size seed] (const []) mcoverage

  pure .
    whenSome (mempty :) .
    whenSome (++ [mempty]) .
    WL.punctuate WL.line .
    fmap (WL.vsep . fmap (WL.indent 2)) .
    fmap (id :: [Doc Markup] -> [Doc Markup]) .
    List.filter (not . null) $
    concat [
      with args $
        WL.punctuate WL.line
    , with decls $
        WL.punctuate WL.line . fmap ppDeclaration
    , with msgs1 $
        id
    , with bottom $
        id
    ]

ppName :: Maybe PropertyName -> Doc a
ppName = \case
  Nothing ->
    "<interactive>"
  Just (PropertyName name) ->
    WL.text name

ppProgress :: MonadIO m => Maybe PropertyName -> Report Progress -> m (Doc Markup)
ppProgress name (Report tests discards coverage status) =
  case status of
    Running ->
      pure . WL.vsep $ [
          icon RunningIcon '●' . WL.annotate RunningHeader $
            ppName name <+>
            "passed" <+>
            ppTestCount tests <>
            ppWithDiscardCount discards <+>
            "(running)"
        ] ++
        ppCoverage tests coverage

    Shrinking failure ->
      pure . icon ShrinkingIcon '↯' . WL.annotate ShrinkingHeader $
        ppName name <+>
        "failed" <+> ppFailedAtLocation (failureLocation failure) <#>
        "after" <+>
        ppTestCount tests <>
        ppShrinkDiscard (failureShrinks failure) discards <+>
        "(shrinking)"

ppResult :: MonadIO m => Maybe PropertyName -> Report Result -> m (Doc Markup)
ppResult name (Report tests discards coverage result) = do
  case result of
    Failed failure -> do
      pfailure <- ppFailureReport name tests failure
      pure . WL.vsep $ [
          icon FailedIcon '✗' . WL.align . WL.annotate FailedText $
            ppName name <+>
            "failed" <+> ppFailedAtLocation (failureLocation failure) <#>
            "after" <+>
            ppTestCount tests <>
            ppShrinkDiscard (failureShrinks failure) discards <>
            "."
        ] ++
        ppCoverage tests coverage ++
        pfailure

    GaveUp ->
      pure . WL.vsep $ [
          icon GaveUpIcon '⚐' . WL.annotate GaveUpText $
            ppName name <+>
            "gave up after" <+>
            ppDiscardCount discards <>
            ", passed" <+>
            ppTestCount tests <>
            "."
        ] ++
        ppCoverage tests coverage

    OK ->
      pure . WL.vsep $ [
          icon SuccessIcon '✓' . WL.annotate SuccessText $
            ppName name <+>
            "passed" <+>
            ppTestCount tests <>
            "."
        ] ++
        ppCoverage tests coverage

ppFailedAtLocation :: Maybe Span -> Doc Markup
ppFailedAtLocation = \case
  Just x ->
    "at" <+>
    WL.text (spanFile x) <> ":" <>
    WL.pretty (unLineNo (spanStartLine x)) <> ":" <>
    WL.pretty (unColumnNo (spanStartColumn x))
  Nothing ->
    mempty

ppCoverage :: TestCount -> Coverage CoverCount -> [Doc Markup]
ppCoverage tests x =
  if Map.null (coverageLabels x) then
    mempty
  else
    fmap (ppLabel tests (coverageWidth tests x)) .
    List.sortOn labelLocation $
    Map.elems (coverageLabels x)

data ColumnWidth =
  ColumnWidth {
      widthPercentage :: !Int
    , widthMinimum :: !Int
    , widthName :: !Int
    , _widthNameFail :: !Int
    }

instance Semigroup ColumnWidth where
  (<>) (ColumnWidth p0 m0 n0 f0) (ColumnWidth p1 m1 n1 f1) =
    ColumnWidth
      (max p0 p1)
      (max m0 m1)
      (max n0 n1)
      (max f0 f1)

instance Monoid ColumnWidth where
  mempty =
    ColumnWidth 0 0 0 0
  mappend =
    (<>)

coverageWidth :: TestCount -> Coverage CoverCount -> ColumnWidth
coverageWidth tests (Coverage labels) =
  foldMap (labelWidth tests) labels

labelWidth :: TestCount -> Label CoverCount -> ColumnWidth
labelWidth tests x =
  let
    percentage =
      length .
      renderCoverPercentage .
      coverPercentage tests $
      labelAnnotation x

    minimum_ =
      if labelMinimum x == 0 then
        0
      else
        length .
        renderCoverPercentage $
        labelMinimum x

    name =
      length .
      unLabelName $
      labelName x

    nameFail =
      if labelCovered tests x then
        0
      else
        name
  in
    ColumnWidth percentage minimum_ name nameFail

ppLeftPad :: Int -> Doc a -> Doc a
ppLeftPad n doc =
  let
    ndoc =
      length (show doc)

    pad =
      WL.text $
        List.replicate (n - ndoc) ' '
  in
    pad <> doc

ppLabel :: TestCount -> ColumnWidth -> Label CoverCount -> Doc Markup
ppLabel tests w x@(MkLabel name _ minimum_ count) =
  let
    covered =
      labelCovered tests x

    ltext =
      if not covered then
        WL.annotate CoverageText
      else
        id

    lborder =
      WL.annotate (StyledBorder StyleDefault)

    licon =
      if not covered then
        WL.annotate CoverageText "⚠ "
      else
        "  "

    lname =
      WL.fill (widthName w) (ppLabelName name)

    wminimum =
      ppLeftPad (widthMinimum w) $
        ppCoverPercentage minimum_

    wcover i =
      ppLeftPad (widthPercentage w + length i) $
        WL.text i <>
        ppCoverPercentage (coverPercentage tests count)

    lminimum =
      if widthMinimum w == 0 then
        mempty
      else if not covered then
        " ✗ " <> wminimum
      else if minimum_ == 0 then
        "   " <> ppLeftPad (widthMinimum w) ""
      else
        " ✓ " <> wminimum

    lcover =
      if widthMinimum w == 0 then
        wcover ""
      else if not covered then
        wcover ""
      else if minimum_ == 0 then
        wcover ""
      else
        wcover ""
  in
    WL.hcat [
        licon
      , ltext lname
      , lborder " "
      , ltext lcover
      , lborder " "
      , ltext $ ppCoverBar (coverPercentage tests count) minimum_
      , lborder "" -- "│"
      , ltext lminimum
      ]

ppLabelName :: LabelName -> Doc a
ppLabelName (LabelName name) =
  WL.text name

ppCoverPercentage :: CoverPercentage -> Doc Markup
ppCoverPercentage =
  WL.text . renderCoverPercentage

ppCoverBar :: CoverPercentage -> CoverPercentage -> Doc Markup
ppCoverBar (CoverPercentage percentage) (CoverPercentage minimum_) =
  let
    barWidth :: Int
    barWidth =
      20

    coverageRatio :: Double
    coverageRatio =
      percentage / 100.0

    coverageWidth_ :: Int
    coverageWidth_ =
      floor $
        coverageRatio * fromIntegral barWidth

    minimumRatio :: Double
    minimumRatio =
      minimum_ / 100.0

    minimumWidth :: Int
    minimumWidth =
      floor $
        minimumRatio * fromIntegral barWidth

    index :: [a] -> Int
    index xs =
      floor $
        ((coverageRatio * fromIntegral barWidth) - fromIntegral coverageWidth_) *
        fromIntegral (length xs)

    part xs =
      xs !! index xs

    fillWidth =
      barWidth - coverageWidth_ - 1

    fillErrorWidth =
      max 0 (minimumWidth - coverageWidth_ - 1)

    fillSurplusWidth =
      fillWidth - fillErrorWidth

    bar :: (Char, [Char]) -> Doc Markup
    bar (full, parts) =
      WL.hcat [
        WL.text $ replicate coverageWidth_ full
      , if fillWidth >= 0 then
          if index parts == 0 then
            if fillErrorWidth > 0 then
              WL.annotate FailedText $ WL.text [part parts]
            else
              WL.annotate CoverageFill $ WL.text [part parts]
          else
            WL.text [part parts]
        else
          ""
      , WL.annotate FailedText . WL.text $
          replicate fillErrorWidth (head parts)
      , WL.annotate CoverageFill . WL.text $
          replicate fillSurplusWidth (head parts)
      --
      -- Uncomment when debugging:
      --
      -- , WL.annotate CoverageFill . WL.text $
      --        " " ++ show barWidth
      --     ++ " " ++ show coverageWidth_
      --     ++ " " ++ show minimumWidth
      --     ++ " " ++ "/"
      --     ++ " " ++ show fillErrorWidth
      --     ++ " " ++ "+"
      --     ++ " " ++ show fillSurplusWidth
      --     ++ " " ++ "="
      --     ++ " " ++ show fillWidth
      ]
  in
    bar ('█', ['·', '▏', '▎', '▍', '▌', '▋', '▊', '▉'])

    -- FIXME Maybe this should be configurable?
    -- Alternative histogram bars:
    --bar ('⣿', ['·', '⡀', '⡄', '⡆', '⡇', '⣇', '⣧', '⣷'])
    --bar ('⣿', ['⢕', '⡀', '⣀', '⣄', '⣤', '⣦', '⣶', '⣷'])
    --bar ('⣿', ['⢕', '⡵', '⢗', '⣗', '⣟'])
    --bar ('⣿', [' ', '⡵', '⢗', '⣗', '⣟'])
    --bar ('█', ['░','▓'])
    --bar ('█', ['░'])

renderCoverPercentage :: CoverPercentage -> String
renderCoverPercentage (CoverPercentage percentage) =
  printf "%.0f" percentage <> "%"

ppWhenNonZero :: Doc a -> PropertyCount -> Maybe (Doc a)
ppWhenNonZero suffix n =
  if n <= 0 then
    Nothing
  else
    Just $ ppRawPropertyCount n <+> suffix

annotateSummary :: Summary -> Doc Markup -> Doc Markup
annotateSummary summary =
  if summaryFailed summary > 0 then
    icon FailedIcon '✗' . WL.annotate FailedText
  else if summaryGaveUp summary > 0 then
    icon GaveUpIcon '⚐' . WL.annotate GaveUpText
  else if summaryWaiting summary > 0 || summaryRunning summary > 0 then
    icon WaitingIcon '○' . WL.annotate WaitingHeader
  else
    icon SuccessIcon '✓' . WL.annotate SuccessText

ppSummary :: MonadIO m => Summary -> m (Doc Markup)
ppSummary summary =
  let
    complete =
      summaryCompleted summary == summaryTotal summary

    prefix end =
      if complete then
        mempty
      else
        ppRawPropertyCount (summaryCompleted summary) <>
        "/" <>
        ppRawPropertyCount (summaryTotal summary) <+>
        "complete" <> end

    addPrefix xs =
      if null xs then
        prefix mempty : []
      else
        prefix ": " : xs

    suffix =
      if complete then
        "."
      else
        " (running)"
  in
    pure .
      annotateSummary summary .
      (<> suffix) .
      WL.hcat .
      addPrefix .
      WL.punctuate ", " $
      catMaybes [
          ppWhenNonZero "failed" (summaryFailed summary)
        , ppWhenNonZero "gave up" (summaryGaveUp summary)
        , if complete then
            ppWhenNonZero "succeeded" (summaryOK summary)
          else
            Nothing
        ]

renderDoc :: MonadIO m => UseColor -> Doc Markup -> m String
renderDoc color doc = do
  let
    dull =
      SetColor Foreground Dull

    vivid =
      SetColor Foreground Vivid

    bold =
      SetConsoleIntensity BoldIntensity

    start = \case
      WaitingIcon ->
        setSGRCode []
      WaitingHeader ->
        setSGRCode []
      RunningIcon ->
        setSGRCode []
      RunningHeader ->
        setSGRCode []
      ShrinkingIcon ->
        setSGRCode [vivid Red]
      ShrinkingHeader ->
        setSGRCode [vivid Red]
      FailedIcon ->
        setSGRCode [vivid Red]
      FailedText ->
        setSGRCode [vivid Red]
      GaveUpIcon ->
        setSGRCode [dull Yellow]
      GaveUpText ->
        setSGRCode [dull Yellow]
      SuccessIcon ->
        setSGRCode [dull Green]
      SuccessText ->
        setSGRCode [dull Green]
      CoverageIcon ->
        setSGRCode [dull Yellow]
      CoverageText ->
        setSGRCode [dull Yellow]
      CoverageFill ->
        setSGRCode [vivid Black]

      DeclarationLocation ->
        setSGRCode []

      StyledLineNo StyleDefault ->
        setSGRCode []
      StyledSource StyleDefault ->
        setSGRCode []
      StyledBorder StyleDefault ->
        setSGRCode []

      StyledLineNo StyleAnnotation ->
        setSGRCode [dull Magenta]
      StyledSource StyleAnnotation ->
        setSGRCode []
      StyledBorder StyleAnnotation ->
        setSGRCode []
      AnnotationGutter ->
        setSGRCode [dull Magenta]
      AnnotationValue ->
        setSGRCode [dull Magenta]

      StyledLineNo StyleFailure ->
        setSGRCode [vivid Red]
      StyledSource StyleFailure ->
        setSGRCode [vivid Red, bold]
      StyledBorder StyleFailure ->
        setSGRCode []
      FailureArrows ->
        setSGRCode [vivid Red]
      FailureMessage ->
        setSGRCode []
      FailureGutter ->
        setSGRCode []

      DiffPrefix ->
        setSGRCode []
      DiffInfix ->
        setSGRCode []
      DiffSuffix ->
        setSGRCode []
      DiffSame ->
        setSGRCode []
      DiffRemoved ->
        setSGRCode [dull Red]
      DiffAdded ->
        setSGRCode [dull Green]

      ReproduceHeader ->
        setSGRCode []
      ReproduceGutter ->
        setSGRCode []
      ReproduceSource ->
        setSGRCode []

    end _ =
      setSGRCode [Reset]

  let
    display =
      case color of
        EnableColor ->
          WL.displayDecorated start end id
        DisableColor ->
          WL.display

#if mingw32_HOST_OS
  liftIO $ do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
#endif

  pure .
    display .
    WL.renderSmart 100 $
    WL.indent 2 doc

renderProgress :: MonadIO m => UseColor -> Maybe PropertyName -> Report Progress -> m String
renderProgress color name x =
  renderDoc color =<< ppProgress name x

renderResult :: MonadIO m => UseColor -> Maybe PropertyName -> Report Result -> m String
renderResult color name x =
  renderDoc color =<< ppResult name x

renderSummary :: MonadIO m => UseColor -> Summary -> m String
renderSummary color x =
  renderDoc color =<< ppSummary x
