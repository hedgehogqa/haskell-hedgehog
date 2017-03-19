{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hedgehog.Runner (
  -- * Runner
    check
  , checkNamed
  , recheck

  -- * Internal
  , checkReport
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           GHC.Stack (SrcLoc(..))

import           Hedgehog.Gen (runGen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import           Hedgehog.Property (Property, Log(..), runProperty)
import           Hedgehog.Range (Size)

------------------------------------------------------------------------

findM :: Monad m => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] ->
      return def
    x0 : xs ->
      p x0 >>= \m ->
        case m of
          Nothing ->
            findM xs def p
          Just x ->
            return x

isFailure :: Node m (Maybe (Either x a, b)) -> Bool
isFailure = \case
  Node (Just (Left _, _)) _ ->
    True
  _ ->
    False

takeSmallest ::
  MonadIO m =>
  Size ->
  Seed ->
  ShrinkCount ->
  Node m (Maybe (Either SrcLoc (), [Log])) -> m Status
takeSmallest size seed shrinks = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Left loc -> do
        --liftIO $ putStrLn "*** Shrinking"
        --liftIO . putStr . unlines $ fmap ppLog w
        findM xs (Failed $ mkFailure size seed shrinks loc w) $ \m -> do
          o <- runTree m
          if isFailure o then
            Just <$> takeSmallest size seed (shrinks + 1) o
          else
            return Nothing

      Right () ->
        return OK

checkReport :: forall m. MonadIO m => SuccessCount -> Size -> Seed -> Property m () -> m Report
checkReport n size0 seed0 prop =
  let
    loop :: SuccessCount -> DiscardCount -> Size -> Seed -> m Report
    loop !successes !discards !size !seed =
      if size > 99 then
        loop successes discards 0 seed
      else if successes == n then
        pure $ Report successes discards OK
      else if discards >= 100 then
        pure $ Report successes discards GaveUp
      else
        case Seed.split seed of
          (s0, s1) -> do
            node@(Node x _) <-
              runTree . Gen.runDiscardEffect $ runGen size s0 (runProperty prop)
            case x of
              Nothing ->
                loop successes (discards + 1) (size + 1) s1

              Just (Left _, _) ->
                Report successes discards <$> takeSmallest size seed 0 node

              Just (Right (), _) ->
                loop (successes + 1) discards (size + 1) s1
  in
    loop 0 0 size0 seed0

checkNamed :: MonadIO m => Maybe String -> Property m () -> m Bool
checkNamed name prop = do
  seed <- liftIO Seed.random
  x <- checkReport 100 0 seed prop
  printReport name x
  pure $
    reportStatus x == OK

check :: MonadIO m => Property m () -> m Bool
check =
  checkNamed Nothing

recheck :: MonadIO m => Size -> Seed -> Property m () -> m ()
recheck size seed prop =
  printReport Nothing =<< checkReport 1 size seed prop
