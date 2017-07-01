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

import           Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Catch (MonadMask(..), bracket)
import           Control.Monad.IO.Class (MonadIO(..))

import           System.Console.Regions (ConsoleRegion, RegionLayout(..), LiftRegion(..))
import qualified System.Console.Regions as Console


data Content =
    Empty
  | Open ConsoleRegion
  | Closed

newtype Region =
  Region {
      unRegion :: TVar Content
    }

newEmptyRegion :: LiftRegion m => m Region
newEmptyRegion =
  liftRegion $ do
    ref <- TVar.newTVar Empty
    pure $ Region ref

newRegion :: LiftRegion m => m Region
newRegion =
  liftRegion $ do
    region <- Console.openConsoleRegion Linear
    ref <- TVar.newTVar $ Open region
    pure $ Region ref

forceRegion :: LiftRegion m => Region -> String -> m ()
forceRegion (Region var) content =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Empty -> do
        region <- Console.openConsoleRegion Linear
        TVar.writeTVar var $ Open region
        Console.setConsoleRegion region content

      Open region ->
        Console.setConsoleRegion region content

      Closed ->
        pure ()

setRegion :: LiftRegion m => Region -> String -> m ()
setRegion (Region var) content =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Empty ->
        pure ()

      Open region ->
        Console.setConsoleRegion region content

      Closed ->
        pure ()

displayRegions :: (MonadIO m, MonadMask m) => m a -> m a
displayRegions io =
  Console.displayConsoleRegions io

displayRegion ::
     MonadIO m
  => MonadMask m
  => LiftRegion m
  => (Region -> m a)
  -> m a
displayRegion =
  displayRegions . bracket newRegion finishRegion

moveToBottom :: Region -> STM ()
moveToBottom (Region var) =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Empty ->
        pure ()

      Open region -> do
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

      Closed ->
        pure ()

finishRegion :: LiftRegion m => Region -> m ()
finishRegion (Region var) =
  liftRegion $ do
    mregion <- TVar.readTVar var
    case mregion of
      Empty -> do
        TVar.writeTVar var Closed

      Open region -> do
        content <- Console.getConsoleRegion region
        Console.finishConsoleRegion region content
        TVar.writeTVar var Closed

      Closed ->
        pure ()
