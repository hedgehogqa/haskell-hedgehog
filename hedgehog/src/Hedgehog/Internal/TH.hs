{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
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

import           Language.Haskell.TH (Exp(..), Q, location, runIO
#if MIN_VERSION_template_haskell(2,17,0)
  , CodeQ, joinCode, unTypeCode, unsafeCodeCoerce
#endif
  )
import           Language.Haskell.TH.Syntax (Loc(..), mkName
#if !MIN_VERSION_template_haskell(2,17,0)
  , TExp, unsafeTExpCoerce, unTypeQ
#endif
  )

#if MIN_VERSION_template_haskell(2,17,0)
type TExpQ a = CodeQ a
#else
-- Originally `Code` is a more polymorphic newtype wrapper, but for this module
-- we can get away with just making it a type alias.
type TExpQ a = Q (TExp a)
joinCode :: Q (TExpQ a) -> TExpQ a
joinCode = (>>= id)
unsafeCodeCoerce :: Q Exp -> TExpQ a
unsafeCodeCoerce = unsafeTExpCoerce
unTypeCode ::  TExpQ a -> Q Exp
unTypeCode = unTypeQ
#endif

-- | Discover all the properties in a module.
--
--   Functions starting with `prop_` are assumed to be properties.
--
discover :: TExpQ Group
discover = discoverPrefix "prop_"

discoverPrefix :: String -> TExpQ Group
discoverPrefix prefix = joinCode $ do
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

  return [|| Group $$(moduleName) $$(listTE names) ||]

mkNamedProperty :: PropertyName -> TExpQ (PropertyName, Property)
mkNamedProperty name =
  [|| (name, $$(unsafeProperty name)) ||]

unsafeProperty :: PropertyName -> TExpQ Property
unsafeProperty =
  unsafeCodeCoerce . pure . VarE . mkName . unPropertyName

listTE :: [TExpQ a] -> TExpQ [a]
listTE xs =
  unsafeCodeCoerce $ pure . ListE =<< traverse unTypeCode xs

moduleName :: TExpQ GroupName
moduleName = joinCode $ do
  loc <- GroupName . loc_module <$> location
  return [|| loc ||]

getCurrentFile :: Q FilePath
getCurrentFile =
  loc_filename <$> location
