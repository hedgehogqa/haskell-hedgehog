{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Hedgehog.Internal.Report (
  -- * Report
    Report(..)
  , Progress(..)
  , Result(..)
  , FailureReport(..)
  , FailedInput(..)

  , ShrinkCount(..)
  , TestCount(..)
  , DiscardCount(..)

  , Style(..)
  , Markup(..)

  , renderProgress
  , renderResult
  , renderDoc

  , ppProgress
  , ppResult

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
import           Data.Semigroup (Semigroup(..))
import           Data.Typeable (TypeRep)

import           Hedgehog.Internal.Discovery (Pos(..), Position(..))
import qualified Hedgehog.Internal.Discovery as Discovery
import           Hedgehog.Internal.Property (PropertyName(..), Log(..), Diff(..))
import           Hedgehog.Internal.Seed (Seed)
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source
import           Hedgehog.Range (Size)

import           System.Console.ANSI (ColorIntensity(..), Color(..))
import           System.Console.ANSI (ConsoleLayer(..), ConsoleIntensity(..))
import           System.Console.ANSI (SGR(..), setSGRCode, hSupportsANSI)
import           System.Directory (makeRelativeToCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.IO (stdout)

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL
import           Text.Printf (printf)

------------------------------------------------------------------------
-- Data

-- | The numbers of times a property was able to shrink after a failing test.
--
newtype ShrinkCount =
  ShrinkCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of tests a property ran successfully.
--
newtype TestCount =
  TestCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of tests a property had to discard.
--
newtype DiscardCount =
  DiscardCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data FailedInput =
  FailedInput {
      failedSpan :: !(Maybe Span)
    , failedType :: !TypeRep
    , failedValue :: !String
    } deriving (Eq, Show)

data FailureReport =
  FailureReport {
      failureSize :: !Size
    , failureSeed :: !Seed
    , failureShrinks :: !ShrinkCount
    , failureInputs :: ![FailedInput]
    , failureLocation :: !(Maybe Span)
    , failureMessage :: !String
    , failureDiff :: !(Maybe Diff)
    , failureMessages :: ![String]
    } deriving (Eq, Show)

-- | The status of a running property test.
--
data Progress =
    Waiting
  | Running
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
    , reportStatus :: !a
    } deriving (Show, Functor, Foldable, Traversable)

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
    StyleInfo
  | StyleInput
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
  | FailedHeader
  | GaveUpIcon
  | GaveUpHeader
  | SuccessIcon
  | SuccessHeader
  | DeclarationLocation
  | StyledLineNo !Style
  | StyledBorder !Style
  | StyledSource !Style
  | InputGutter
  | InputValue
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
      (StyleInput, _) ->
        StyleInput
      (_, StyleInput) ->
        StyleInput
      (StyleInfo, _) ->
        StyleInfo

------------------------------------------------------------------------

takeInput :: Log -> Maybe FailedInput
takeInput = \case
  Input loc typ val ->
    Just $ FailedInput loc typ val
  _ ->
    Nothing

takeInfo :: Log -> Maybe String
takeInfo = \case
  Info x ->
    Just x
  _ ->
    Nothing

mkFailure ::
     Size
  -> Seed
  -> ShrinkCount
  -> Maybe Span
  -> String
  -> Maybe Diff
  -> [Log]
  -> FailureReport
mkFailure size seed shrinks location message diff logs =
  let
    inputs =
      mapMaybe takeInput logs

    info =
      mapMaybe takeInfo logs
  in
    FailureReport size seed shrinks inputs location message diff info

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
  fmap $ const (StyleInfo, [])

lastLineSpan :: Monad m => Span -> Declaration a -> MaybeT m (ColumnNo, ColumnNo)
lastLineSpan sloc decl =
  case reverse . Map.elems $ takeLines sloc decl of
    [] ->
      MaybeT $ pure Nothing
    x : _ ->
      pure $
        lineSpan x

ppFailedInputTypedArgument :: Int -> FailedInput -> Doc Markup
ppFailedInputTypedArgument ix (FailedInput _ typ val) =
  WL.vsep [
      WL.text "forAll" <> ppShow ix <+> "::" <+> ppShow typ
    , WL.text "forAll" <> ppShow ix <+> "="
    , WL.indent 2 . WL.vsep . fmap (markup InputValue . WL.text) $ lines val
    ]

ppFailedInputDeclaration ::
     MonadIO m
  => FailedInput
  -> m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailedInputDeclaration (FailedInput msloc _ val) =
  runMaybeT $ do
    sloc <- MaybeT $ pure msloc
    decl <- fmap defaultStyle . MaybeT $ readDeclaration sloc
    startCol <- fromIntegral . fst <$> lastLineSpan sloc decl

    let
      ppValLine =
        WL.indent startCol .
          (markup InputGutter (WL.text "│ ") <>) .
          markup InputValue .
          WL.text

      valDocs =
        fmap ((StyleInput, ) . ppValLine) $
        List.lines val

      startLine =
        fromIntegral $ spanStartLine sloc

      endLine =
        fromIntegral $ spanEndLine sloc

      styleInput kvs =
        foldr (Map.adjust . fmap . first $ const StyleInput) kvs [startLine..endLine]

      insertDoc =
        Map.adjust (fmap . second $ const valDocs) endLine

    pure $
      mapSource (styleInput . insertDoc) decl

ppFailedInput ::
     MonadIO m
  => Int
  -> FailedInput
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
    markup DiffRemoved (WL.text removed) <+>
    markup DiffInfix (WL.text infix_) <+>
    markup DiffAdded (WL.text added) <>
    markup DiffSuffix (WL.text suffix)
  ] ++ fmap ppLineDiff (toLineDiff diff)

ppFailureLocation ::
     MonadIO m
  => String
  -> Maybe Diff
  -> Span
  -> m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailureLocation msg mdiff sloc =
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
        fmap ((StyleFailure, ) . ppFailure . markup FailureMessage . WL.text) (List.lines msg)

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
            markup (StyledBorder StyleInfo) "┏━━" <+>
            markup DeclarationLocation (WL.text (declarationFile decl)) <+>
            markup (StyledBorder StyleInfo) "━━━"

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

ppFailureReport :: MonadIO m => Maybe PropertyName -> FailureReport -> m (Doc Markup)
ppFailureReport name (FailureReport size seed _ inputs0 mlocation0 msg mdiff msgs0) = do
  (msgs, mlocation) <-
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
        (concatMap ppTextLines msgs0,)
          <$> ppFailureLocation msg mdiff location0

  (args, idecls) <- partitionEithers <$> zipWithM ppFailedInput [0..] inputs0

  let
    decls =
      mergeDeclarations .
      catMaybes $
        mlocation : fmap pure idecls

    with xs f =
      if null xs then
        []
      else
        [f xs]

  pure . WL.indent 2 . WL.vsep . WL.punctuate WL.line $ concat [
      with args $
        WL.vsep . WL.punctuate WL.line
    , with decls $
        WL.vsep . WL.punctuate WL.line . fmap ppDeclaration
    , with msgs $
        WL.vsep
    , [ppReproduce name size seed]
    ]

ppName :: Maybe PropertyName -> Doc a
ppName = \case
  Nothing ->
    "<interactive>"
  Just (PropertyName name) ->
    WL.text name

ppProgress :: MonadIO m => Maybe PropertyName -> Report Progress -> m (Doc Markup)
ppProgress name (Report tests discards status) =
  case status of
    Waiting ->
      pure . icon WaitingIcon '○' . WL.annotate WaitingHeader $
        ppName name

    Running ->
      pure . icon RunningIcon '●' . WL.annotate RunningHeader $
        ppName name <+>
        "passed" <+>
        ppTestCount tests <>
        ppWithDiscardCount discards <+>
        "(running)"

    Shrinking failure ->
      pure . icon ShrinkingIcon '↯' . WL.annotate ShrinkingHeader $
        ppName name <+>
        "failed after" <+>
        ppTestCount tests <>
        ppShrinkDiscard (failureShrinks failure) discards <+>
        "(shrinking)"

ppResult :: MonadIO m => Maybe PropertyName -> Report Result -> m (Doc Markup)
ppResult name (Report tests discards result) =
  case result of
    Failed failure -> do
      pfailure <- ppFailureReport name failure
      pure . WL.vsep $ [
          icon FailedIcon '✗' . WL.annotate FailedHeader $
            ppName name <+>
            "failed after" <+>
            ppTestCount tests <>
            ppShrinkDiscard (failureShrinks failure) discards <>
            "."
        , mempty
        , pfailure
        , mempty
        ]

    GaveUp ->
      pure . icon GaveUpIcon '⚐' . WL.annotate GaveUpHeader $
        ppName name <+>
        "gave up after" <+>
        ppDiscardCount discards <>
        ", passed" <+>
        ppTestCount tests <>
        "."

    OK ->
      pure . icon SuccessIcon '✓' . WL.annotate SuccessHeader $
        ppName name <+>
        "passed" <+>
        ppTestCount tests <>
        "."

useColor :: MonadIO m => m Bool
useColor =
  liftIO $ do
    menv <- lookupEnv "HEDGEHOG_COLOR"
    case menv of
      Just "0" ->
        pure False
      Just "no" ->
        pure False
      Just "false" ->
        pure False

      Just "1" ->
        pure True
      Just "yes" ->
        pure True
      Just "true" ->
        pure True

      Just _ ->
        hSupportsANSI stdout
      Nothing ->
        hSupportsANSI stdout

renderDoc :: MonadIO m => Doc Markup -> m String
renderDoc doc = do
  color <- useColor

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
      FailedHeader ->
        setSGRCode [vivid Red]
      GaveUpIcon ->
        setSGRCode [dull Yellow]
      GaveUpHeader ->
        setSGRCode [dull Yellow]
      SuccessIcon ->
        setSGRCode [dull Green]
      SuccessHeader ->
        setSGRCode [dull Green]

      DeclarationLocation ->
        setSGRCode []

      StyledLineNo StyleInfo ->
        setSGRCode []
      StyledSource StyleInfo ->
        setSGRCode []
      StyledBorder StyleInfo ->
        setSGRCode []

      StyledLineNo StyleInput ->
        setSGRCode [dull Magenta]
      StyledSource StyleInput ->
        setSGRCode []
      StyledBorder StyleInput ->
        setSGRCode []
      InputGutter ->
        setSGRCode [dull Magenta]
      InputValue ->
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

    display =
      if color then
        WL.displayDecorated start end id
      else
        WL.display

  pure .
    display .
    WL.renderSmart 100 $
    WL.indent 2 doc

renderProgress :: MonadIO m => Maybe PropertyName -> Report Progress -> m String
renderProgress name x = do
  renderDoc =<< ppProgress name x

renderResult :: MonadIO m => Maybe PropertyName -> Report Result -> m String
renderResult name x = do
  renderDoc =<< ppResult name x
