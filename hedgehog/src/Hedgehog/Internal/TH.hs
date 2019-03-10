{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Hedgehog.Internal.TH (
    TExpQ
  , discover
  , discoverPrefix
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord

import           Hedgehog.Internal.Discovery
import           Hedgehog.Internal.Property

import           Language.Haskell.TH (Exp(..), Q, TExp, location, runIO)
import           Language.Haskell.TH.Syntax (Loc(..), mkName, unTypeQ, unsafeTExpCoerce)

type TExpQ a =
  Q (TExp a)

-- | Discover all the properties in a module.
--
--   Functions starting with `prop_` are assumed to be properties.
--
discover :: TExpQ Group
discover = discoverPrefix "prop_"

discoverPrefix :: String -> TExpQ Group
discoverPrefix prefix = do
  file <- getCurrentFile
  properties <- Map.toList <$> runIO (readProperties prefix file)

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

  [|| Group $$(moduleName) $$(listTE names) ||]

mkNamedProperty :: PropertyName -> TExpQ (PropertyName, Property)
mkNamedProperty name = do
  [|| (name, $$(unsafeProperty name)) ||]

unsafeProperty :: PropertyName -> TExpQ Property
unsafeProperty =
  unsafeTExpCoerce . pure . VarE . mkName . unPropertyName

listTE :: [TExpQ a] -> TExpQ [a]
listTE xs = do
  unsafeTExpCoerce . pure . ListE =<< traverse unTypeQ xs

moduleName :: TExpQ GroupName
moduleName = do
  loc <- GroupName . loc_module <$> location
  [|| loc ||]

getCurrentFile :: Q FilePath
getCurrentFile =
  loc_filename <$> location
