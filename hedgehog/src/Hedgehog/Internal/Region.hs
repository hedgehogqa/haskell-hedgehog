module Hedgehog.Internal.Region (
    Region(..)
  , newEmptyRegion
  , newRegion
  , forceRegion
  , setRegion
  , displayRegions
  , displayRegion
  , moveToBottom
  , finishRegion
  ) where

import           Control.Concurrent.STM (STM, TVar, atomically)
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Catch (MonadMask(..), bracket)
import           Control.Monad.IO.Class (MonadIO(..))

import           System.Console.Regions (ConsoleRegion, RegionLayout(..), LiftRegion(..))
import qualified System.Console.Regions as Console


newtype Region =
  Region {
      unRegion :: TVar (Maybe ConsoleRegion)
    }

newEmptyRegion :: LiftRegion m => m Region
newEmptyRegion =
  liftRegion $ do
    ref <- TVar.newTVar Nothing
    pure $ Region ref

newRegion :: LiftRegion m => m Region
newRegion =
  liftRegion $ do
    region <- Console.openConsoleRegion Linear
    ref <- TVar.newTVar $ Just region
    pure $ Region ref

forceRegion :: LiftRegion m => Region -> String -> m ()
forceRegion (Region var) content =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Nothing -> do
        region <- Console.openConsoleRegion Linear
        TVar.writeTVar var $ Just region
        Console.setConsoleRegion region content

      Just region ->
        Console.setConsoleRegion region content

setRegion :: LiftRegion m => Region -> String -> m ()
setRegion (Region var) content =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Nothing -> do
        pure ()

      Just region ->
        Console.setConsoleRegion region content

displayRegions :: (MonadIO m, MonadMask m) => m a -> m a
displayRegions io = do
  liftIO . atomically $ do
    -- clear old regions
    mxs <- TMVar.tryTakeTMVar Console.regionList
    case mxs of
      Nothing ->
        pure ()
      Just _xs ->
        TMVar.putTMVar Console.regionList []

  Console.displayConsoleRegions io

displayRegion ::
     MonadIO m
  => MonadMask m
  => LiftRegion m
  => (Region -> m a)
  -> m a
displayRegion =
  displayRegions . bracket newRegion finishRegion

moveToBottom :: ConsoleRegion -> STM ()
moveToBottom region = do
  mxs <- TMVar.tryTakeTMVar Console.regionList
  case mxs of
    Nothing ->
      pure ()
    Just xs0 ->
      let
        xs1 =
          filter (/= region) xs0
      in
        TMVar.putTMVar Console.regionList (region : xs1)

finishRegion :: LiftRegion m => Region -> m ()
finishRegion (Region var) =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Nothing ->
        pure ()

      Just region -> do
        content <- Console.getConsoleRegion region
        Console.finishConsoleRegion region content
