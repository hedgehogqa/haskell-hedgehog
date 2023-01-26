{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Hedgehog.State where

import           Control.Applicative (Const(..))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.IORef as IORef
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Config as Config
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Internal.Runner as Runner
import qualified Hedgehog.Range as Range

-- | Test that 'commandAMkInput' works as expected when shrinking.
--
-- We create a state machine that always generates two actions. Initially, one
-- will have the number 5 as input and put it in state. The other will have
-- (True, 5) as input. It checks the number is less than 5. Since it's not, we
-- start shrinking.
--
-- We shrink the first action initially, through 4,3,2,1,0. Each of these
-- changes the input to the second action, even though we're not shrinking that,
-- because `commandMkInput` looks at the state. The second action passes with
-- each of these.
--
-- So then we shrink the second action, to (False, 5). That fails again, so we
-- go back to shrinking the first one. All of those shrinks pass again.
--
-- We log the list of inputs to the second action, and after running this state
-- machine (and ignoring its result) we check that this list is correct.
--
-- This depends on the order shrinks are performed in state machines. Hopefully
-- it won't be too fragile.
prop_mkInput :: Property
prop_mkInput =
  withTests 1 . property $ do
    actionListsRef <- liftIO $ IORef.newIORef []
    let
      prop = property $ do
        actions <- forAll $ Gen.sequential
          (Range.linear 2 2)
          (Const Nothing)
          [ let
              commandGen = \case
                Const Nothing ->
                  Just $ Const <$> Gen.shrink (\n -> reverse [0..n-1])
                                              (pure (5 :: Int))
                Const (Just _) -> Nothing
              commandExecute _ = pure ()
              commandCallbacks =
                [Update $ \_ (Const input) _ -> Const $ Just input]
            in
              Command { .. }
          , let
              commandAGen = \case
                Const Nothing ->
                  Nothing
                Const (Just _) ->
                  Just $ Gen.shrink (\b -> if b then [False] else [])
                                    (pure True)
              commandAMkInput (Const st) inputB = case st of
                Nothing ->
                  Nothing
                Just stateN ->
                  Just $ Const (stateN, inputB)
              commandAExecute (Const (stateN, inputB)) = liftIO $ do
                IORef.modifyIORef' actionListsRef ((stateN, inputB) :)
              commandACallbacks =
                [Ensure $ \_ _ (Const (stateN, _)) _ -> diff stateN (<) 5]
            in
              CommandA { .. }
          ]
        executeSequential (Const Nothing) actions

    -- We could simply use `check` here, but that prints its output to the test
    -- logs.
    seed <- Config.resolveSeed Nothing
    void $ liftIO $ Runner.checkReport (Property.propertyConfig prop)
                                       seed
                                       (Property.propertyTest prop)
                                       (const $ pure ())

    actionLists <- liftIO $ reverse <$> IORef.readIORef actionListsRef
    actionLists === ((, True) <$> [5,4..0]) ++ ((, False) <$> [5,4..0])

tests :: IO Bool
tests =
  checkParallel $$(discover)
