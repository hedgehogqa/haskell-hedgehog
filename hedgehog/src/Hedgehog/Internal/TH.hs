{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Hedgehog.Internal.TH (
    TExpQ
  , checkAll
  , runCheckAll
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import           Data.Traversable (for)

import           Hedgehog.Internal.Discovery
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Runner

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

type TExpQ a =
  Q (TExp a)

-- | Check all the properties in a file.
--
-- > tests :: IO Bool
-- > tests =
-- >   $$(checkAll)
--
checkAll :: TExpQ (IO Bool)
checkAll = do
  file <- getCurrentFile
  properties <- Map.toList <$> runIO (readProperties file)

  let
    startLine =
      Ord.comparing $
        posLine .
        posPostion .
        propertySource .
        snd

    names =
      fmap (mkNamedProperty . fst) $
      List.sortBy startLine properties

  [|| runCheckAll $$(moduleName) $$(listTE names) ||]

mkNamedProperty :: PropertyName -> TExpQ (String, Property)
mkNamedProperty name@(PropertyName x) = do
  [|| (x, $$(unsafeProperty name)) ||]

unsafeProperty :: PropertyName -> TExpQ Property
unsafeProperty =
  unsafeTExpCoerce . pure . VarE . mkName . unPropertyName

listTE :: [TExpQ a] -> TExpQ [a]
listTE xs = do
  unsafeTExpCoerce . pure . ListE =<< traverse unTypeQ xs

moduleName :: TExpQ String
moduleName = do
  loc <- loc_module <$> location
  [|| loc ||]

getCurrentFile :: Q FilePath
getCurrentFile =
  loc_filename <$> location

runCheckAll :: String -> [(String, Property)] -> IO Bool
runCheckAll modName ps = do
  putStrLn $ "━━━ " ++ modName ++ " ━━━"
  fmap and . for ps $ \(name, p) -> do
    checkNamed (Just name) p
