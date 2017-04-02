{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hedgehog.Internal.Runner (
  -- * Runner
    check
  , checkNamed
  , recheck

  -- * Internal
  , checkReport
  ) where

import           Control.Monad.Catch (MonadCatch(..), catchAll)
import           Control.Monad.IO.Class (MonadIO(..))

import           Hedgehog.Gen (runGen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Report
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Tree (Tree(..), Node(..))
import           Hedgehog.Internal.Property (Test, Log(..), Failure(..), runTest)
import           Hedgehog.Internal.Property (Property(..), Config(..))
import           Hedgehog.Internal.Property (ShrinkLimit, withTests)
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
  ShrinkLimit ->
  Node m (Maybe (Either Failure (), [Log])) ->
  m Status
takeSmallest size seed shrinks slimit = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Left (Failure loc err) ->
        let
          status =
            Failed $ mkFailure size seed shrinks loc err w
        in
          if shrinks >= fromIntegral slimit then
            -- if we've hit the shrink limit, don't shrink any further
            pure status
          else
            findM xs status $ \m -> do
              o <- runTree m
              if isFailure o then
                Just <$> takeSmallest size seed (shrinks + 1) slimit o
              else
                return Nothing

      Right () ->
        return OK

checkReport ::
  forall m.
  MonadIO m =>
  MonadCatch m =>
  Config ->
  Size ->
  Seed ->
  Test m () ->
  m Report
checkReport cfg size0 seed0 test0 =
  let
    test =
      catchAll test0 (fail . show)

    loop :: TestCount -> DiscardCount -> Size -> Seed -> m Report
    loop !tests !discards !size !seed =
      if size > 99 then
        -- size has reached limit, reset to 0
        loop tests discards 0 seed

      else if tests >= fromIntegral (configTestLimit cfg) then
        -- we've hit the test limit, test was successful
        pure $ Report tests discards OK

      else if discards >= fromIntegral (configDiscardLimit cfg) then
        -- we've hit the discard limit, give up
        pure $ Report tests discards GaveUp

      else
        case Seed.split seed of
          (s0, s1) -> do
            node@(Node x _) <-
              runTree . Gen.runDiscardEffect $ runGen size s0 (runTest test)
            case x of
              Nothing ->
                loop tests (discards + 1) (size + 1) s1

              Just (Left _, _) ->
                Report (tests + 1) discards
                  <$> takeSmallest size seed 0 (configShrinkLimit cfg) node

              Just (Right (), _) ->
                loop (tests + 1) discards (size + 1) s1
  in
    loop 0 0 size0 seed0

checkNamed :: MonadIO m => Maybe String -> Property -> m Bool
checkNamed name prop = do
  seed <- liftIO Seed.random
  report <- liftIO $ checkReport (propertyConfig prop) 0 seed (propertyTest prop)
  printReport name report
  pure $
    reportStatus report == OK

-- | Check a property.
--
check :: MonadIO m => Property -> m Bool
check =
  checkNamed Nothing

-- | Check a property using a specific size and seed.
--
recheck :: MonadIO m => Size -> Seed -> Property -> m ()
recheck size seed prop0 = do
  let prop = withTests 1 prop0
  report <- liftIO $ checkReport (propertyConfig prop) size seed (propertyTest prop)
  printReport Nothing report
