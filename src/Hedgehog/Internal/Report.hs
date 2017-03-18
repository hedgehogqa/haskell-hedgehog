{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Hedgehog.Internal.Report (
  -- * Report
    Report(..)
  , Status(..)
  , Failure(..)
  , FailedInput(..)

  , ShrinkCount
  , SuccessCount
  , DiscardCount

  , Style(..)
  , Markup(..)

  , printReport
  , ppReport

  , mkFailure
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))

import           Data.Bifunctor (bimap, first, second)
import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, catMaybes)
import           Data.Semigroup (Semigroup(..))
import           Data.Typeable (TypeRep)

import           GHC.Stack (SrcLoc(..))

import           Hedgehog.Internal.Discovery (Pos(..), Position(..))
import qualified Hedgehog.Internal.Discovery as Discovery
import           Hedgehog.Internal.Seed (Seed)
import           Hedgehog.Property (Log(..))
import           Hedgehog.Range (Size)

import           System.Console.ANSI (ColorIntensity(..), Color(..))
import           System.Console.ANSI (ConsoleLayer(..), ConsoleIntensity(..))
import           System.Console.ANSI (SGR(..), setSGRCode, hSupportsANSI)
import           System.Environment (getArgs)
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
newtype SuccessCount =
  SuccessCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of tests a property had to discard.
--
newtype DiscardCount =
  DiscardCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data FailedInput =
  FailedInput {
      failedSrcLoc :: !SrcLoc
    , failedType :: !TypeRep
    , failedValue :: !String
    } deriving (Eq, Show)

data Failure =
  Failure {
      failureSize :: !Size
    , failureSeed :: !Seed
    , failureShrinks :: !ShrinkCount
    , failureInputs :: ![FailedInput]
    , failureLocation :: !SrcLoc
    , failureInfo :: ![String]
    } deriving (Eq, Show)

-- | The status of a property test run.
--
--   In the case of a failure it provides the seed used for the test, the
--   number of shrinks, and the execution log.
--
data Status =
    Failed !Failure
  | GaveUp
  | OK
    deriving (Eq, Show)

-- | The reporty from a property test run.
--
data Report =
  Report {
      reportTests :: !SuccessCount
    , reportDiscards :: !DiscardCount
    , reportStatus :: !Status
    } deriving (Show)

------------------------------------------------------------------------
-- Pretty Printing Helpers

newtype LineNo =
  LineNo {
      unLineNo :: Int
    } deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype ColumnNo =
  ColumnNo {
      _unColumnNo :: Int
    } deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

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
    FailedIcon
  | FailedHeader
  | GaveUpIcon
  | GaveUpHeader
  | SuccessIcon
  | SuccessHeader
  | DeclarationLocation
  | StyledLineNo !Style
  | StyledBorder !Style
  | StyledSource !Style
  | InputValue
  | FailureArrows
  | FailureMessage
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

mkFailure :: Size -> Seed -> ShrinkCount -> SrcLoc -> [Log] -> Failure
mkFailure size seed shrinks location logs =
  let
    inputs =
      mapMaybe takeInput logs

    info =
      mapMaybe takeInfo logs
  in
    Failure size seed shrinks inputs location info

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

ppSuccessCount :: SuccessCount -> Doc a
ppSuccessCount = \case
  SuccessCount 1 ->
    "1 test"
  SuccessCount n ->
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

takeLines :: SrcLoc -> Declaration a -> Map LineNo (Line a)
takeLines sloc =
  fst . Map.split (fromIntegral $ srcLocEndLine sloc + 1) .
  snd . Map.split (fromIntegral $ srcLocStartLine sloc - 1) .
  declarationSource

readDeclaration :: MonadIO m => SrcLoc -> m (Maybe (Declaration ()))
readDeclaration sloc =
  runMaybeT $ do
    (name, Pos (Position _ line0 _) src) <- MaybeT $
      Discovery.readDeclaration (srcLocFile sloc) (srcLocEndLine sloc)

    let
      path =
        srcLocFile sloc

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

lastLineSpan :: Monad m => SrcLoc -> Declaration a -> MaybeT m (ColumnNo, ColumnNo)
lastLineSpan sloc decl =
  case reverse . Map.elems $ takeLines sloc decl of
    [] ->
      MaybeT $ pure Nothing
    x : _ ->
      pure $
        lineSpan x

ppFailedInput ::
  MonadIO m =>
  FailedInput ->
  m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailedInput (FailedInput sloc _ val) =
  runMaybeT $ do
    decl <- fmap defaultStyle . MaybeT $ readDeclaration sloc
    startCol <- fromIntegral . fst <$> lastLineSpan sloc decl

    let
      code =
        WL.indent startCol .
        markup InputValue .
        (WL.text "│ " <>) .
        WL.text

      docs =
        fmap ((StyleInput, ) . code) $
        List.lines val

      endLine =
        fromIntegral $ srcLocEndLine sloc

      --TODO do we even want this?
      --padding =
      -- -- We only add padding after inputs if the next line contains code
      -- case Map.lookup (endLine + 1) (declarationSource decl) of
      --   Nothing ->
      --     []
      --   Just line ->
      --     if all Char.isSpace (lineSource line) then
      --       []
      --     else
      --       [(StyleInput, mempty)]

      insertDoc =
        Map.adjust (fmap . second $ const docs) endLine

    pure $
      mapSource insertDoc decl

ppFailureLocation ::
  MonadIO m =>
  SrcLoc ->
  m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailureLocation sloc =
  runMaybeT $ do
    decl <- fmap defaultStyle . MaybeT $ readDeclaration sloc
    (startCol, endCol) <- bimap fromIntegral fromIntegral <$> lastLineSpan sloc decl

    let
      doc =
        WL.indent startCol $
          markup FailureArrows (WL.text (replicate (endCol - startCol) '^')) <+>
          markup FailureMessage (WL.text "proposition was falsifiable!")

      startLine =
        fromIntegral $ srcLocStartLine sloc

      endLine =
        fromIntegral $ srcLocEndLine sloc

      styleFailure kvs =
        foldr (Map.adjust . fmap . first $ const StyleFailure) kvs [startLine..endLine]

      insertDoc =
        Map.adjust (fmap . second $ const [(StyleFailure, doc)]) endLine

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

ppReproduce :: Maybe String -> Size -> Seed -> Doc Markup
ppReproduce name size seed =
  WL.vsep [
      markup ReproduceHeader
        "This failure can be reproduced by running:"
    , gutter ReproduceGutter . markup ReproduceSource $
        "recheck" <+>
        WL.text (showsPrec 11 size "") <+>
        WL.text (showsPrec 11 seed "") <+>
        maybe "<property>" WL.text name
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

ppFailure :: MonadIO m => Maybe String -> Failure -> m (Doc Markup)
ppFailure name (Failure size seed _ inputs0 location0 msgs) = do
  mlocation <- ppFailureLocation location0
  minputs <- traverse ppFailedInput inputs0

  let
    decls =
      mergeDeclarations .
      catMaybes $
        mlocation : minputs

  pure . WL.indent 2 . WL.vsep . WL.punctuate WL.line $ [
      WL.vsep . WL.punctuate WL.line $ fmap ppDeclaration decls
    ] <> (if null msgs then [] else [WL.vsep $ fmap WL.text msgs]) <> [
      ppReproduce name size seed
    ]

ppName :: Maybe String -> Doc a
ppName = \case
  Nothing ->
    "<interactive>"
  Just name ->
    WL.text name

ppReport :: MonadIO m => Maybe String -> Report -> m (Doc Markup)
ppReport name (Report successes discards status) =
  case status of
    Failed failure -> do
      pfailure <- ppFailure name failure
      pure . WL.vsep $ [
          icon FailedIcon '✗' . WL.annotate FailedHeader $
            ppName name <+>
            "failed after" <+>
            ppSuccessCount successes <>
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
        ppSuccessCount successes <>
        "."
    OK ->
      pure . icon SuccessIcon '✓' . WL.annotate SuccessHeader $
        ppName name <+>
        "passed" <+>
        ppSuccessCount successes <>
        "."

useColor :: MonadIO m => m Bool
useColor = do
  -- FIXME This is pretty savage, not sure how else to provide this while
  -- FIXME maintain a seamless experience. Maybe we should just go for a
  -- FIXME HEDGEHOG_COLOR environment variable instead.
  args <- liftIO getArgs
  if elem "--color" args then
    pure True
  else if elem "--no-color" args then
    pure False
  else
    liftIO $ hSupportsANSI stdout

printReport :: MonadIO m => Maybe String -> Report -> m ()
printReport name x = do
  doc <- ppReport name x
  color <- useColor

  let
    dull =
      SetColor Foreground Dull

    vivid =
      SetColor Foreground Vivid

    bold =
      SetConsoleIntensity BoldIntensity

    start = \case
      FailedIcon ->
        setSGRCode [dull Red]
      FailedHeader ->
        setSGRCode [vivid Red]
      GaveUpIcon ->
        setSGRCode [dull Yellow]
      GaveUpHeader ->
        setSGRCode [vivid Yellow]
      SuccessIcon ->
        setSGRCode [dull Green]
      SuccessHeader ->
        setSGRCode [vivid Green]

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
      InputValue ->
        setSGRCode [dull Magenta]

      StyledLineNo StyleFailure ->
        setSGRCode [vivid Red]
      StyledSource StyleFailure ->
        setSGRCode [vivid Red, bold]
      StyledBorder StyleFailure ->
        setSGRCode [vivid Red]
      FailureArrows ->
        setSGRCode [vivid Red]
      FailureMessage ->
        setSGRCode [vivid Red]

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

  liftIO .
    putStrLn .
    display .
    WL.renderSmart 100 $
    WL.indent 2 doc
