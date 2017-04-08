{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Hedgehog.Internal.TH (
    TExpQ
  , checkSequential
  , checkConcurrent
  , checkWith
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord

import           Hedgehog.Internal.Discovery
import           Hedgehog.Internal.Property
import           Hedgehog.Internal.Runner

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

type TExpQ a =
  Q (TExp a)

-- | Check all the properties in a file sequentially.
--
-- > tests :: IO Bool
-- > tests =
-- >   $$(checkSequential)
--
checkSequential :: TExpQ (IO Bool)
checkSequential =
  checkWith $
    RunnerConfig {
        runnerWorkers =
          Just 1
      }

-- | Check all the properties in a file concurrently.
--
-- > tests :: IO Bool
-- > tests =
-- >   $$(checkConcurrent)
--
checkConcurrent :: TExpQ (IO Bool)
checkConcurrent =
  checkWith $
    RunnerConfig {
        runnerWorkers =
          Nothing
      }

-- | Check all the properties in a file.
--
checkWith :: RunnerConfig -> TExpQ (IO Bool)
checkWith config = do
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

  [|| checkGroupWith config $$(moduleName) $$(listTE names) ||]

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
