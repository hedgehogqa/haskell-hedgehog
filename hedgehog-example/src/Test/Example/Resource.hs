{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Test.Example.Resource where

import           Control.Exception.Lifted (bracket, bracket_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.Directory
import           System.Exit
import           System.FilePath ((</>))
import           System.Process

import qualified "temporary"           System.IO.Temp as Temp
import qualified "temporary-resourcet" System.IO.Temp as TempResourceT

newtype ProcessFailed =
  ProcessFailed Int
  deriving (Show)

unixSort :: MonadIO m => FilePath -> FilePath -> ExceptT ProcessFailed m ()
unixSort input output = do
  let
    process =
      (proc "sort" ["-o", output, input]) {
          env = Just [("LC_COLLATE", "C")]
        }

  (_, _, _, h) <- liftIO $ createProcess process
  code <- liftIO $ waitForProcess h

  case code of
    ExitSuccess ->
      pure ()
    ExitFailure x ->
      throwE $ ProcessFailed x

prop_unix_sort :: Property
prop_unix_sort =
  property . hoist runResourceT $ do
    values0 <- forAll $
      Gen.list (Range.linear 0 100) $
      Gen.string (Range.constant 1 5) Gen.alpha

    (_, dir) <- TempResourceT.createTempDirectory Nothing "prop_dir"

    let input = dir </> "input"
        output = dir </> "output"

    liftIO $ writeFile input (unlines values0)
    evalExceptT $ unixSort input output
    values <- liftIO . fmap lines $ readFile output

    values0 === values

--
-- Run this test to get an idea of how and when actions are executed during
-- shrinking.
--
-- You should see that the following cases are possible:
--
-- @
--   -- normal test run
--   runResourceT {
--     forAll:before
--     forAll:after
--     inner:hoist {
--       inner:action
--     }
--   }
-- @
--
-- @
--   -- during shrinking
--   runResourceT {
--     forAll:after
--     inner:hoist {
--       inner:action
--     }
--   }
-- @
--
-- @
--   -- during shrinking (discard)
--   runResourceT {
--   }
-- @
--
prop_logged_unix_sort :: IORef [String] -> Property
prop_logged_unix_sort ref =
  property . hoist (bracketLog ref 0 "runResourceT") . hoist runResourceT $ do
    normalLog ref 2 "forAll:before"

    values0 <- forAll $
      Gen.list (Range.constant 1 2) $
      Gen.string (Range.constant 1 2) Gen.alpha

    normalLog ref 2 "forAll:after"

    hoist (bracketLog ref 2 "inner:hoist") $ do
      normalLog ref 4 "inner:action"

      (_, dir) <- TempResourceT.createTempDirectory Nothing "prop_dir"

      let input = dir </> "input"
          output = dir </> "output"

      liftIO $ writeFile input (unlines values0)
      evalExceptT $ unixSort input output
      values <- liftIO . fmap lines $ readFile output

      values0 === values

prop_logged_unix_sort_bracket :: IORef [String] -> Property
prop_logged_unix_sort_bracket ref =
  property .
  hoist (bracketLog ref 0 "b:property") .
  bracket_
    (normalLog ref 0 "b:property:acquire")
    (normalLog ref 0 "b:property:release") $ do
    normalLog ref 2 "b:forAll:before"

    values0 <- forAll $
      Gen.list (Range.constant 1 2) $
      Gen.string (Range.constant 1 2) Gen.alpha

    normalLog ref 2 "b:forAll:after"

    hoist (bracketLog ref 2 "b:inner:hoist") $ do
      normalLog ref 4 "b:inner:action"

      tmpdir <- liftIO getTemporaryDirectory
      bracket
        (liftIO $ Temp.createTempDirectory tmpdir "prop_foo")
        (liftIO . removeDirectoryRecursive) $ \dir -> do

        let input = dir </> "input"
            output = dir </> "output"

        liftIO $ writeFile input (unlines values0)
        evalExceptT $ unixSort input output
        values <- liftIO . fmap lines $ readFile output

        values0 === values

bracketLog :: MonadIO m => IORef [String] -> Int -> String -> m a -> m a
bracketLog ref indent x io = do
  liftIO $ IORef.modifyIORef ref (++ [replicate indent ' ' ++ x ++ " {"])
  z <- io
  liftIO $ IORef.modifyIORef ref (++ [replicate indent ' ' ++ "}"])
  pure z

normalLog :: MonadIO m => IORef [String] -> Int -> String -> m ()
normalLog ref indent x = do
  liftIO $ IORef.modifyIORef ref (++ [replicate indent ' ' ++ x])

joinBlocks :: [String] -> [String]
joinBlocks xs0 =
  case List.span (/= "}") xs0 of
    (xs, []) -> xs
    (xs, x : ys) ->  concat (List.intersperse "\n" (xs ++ [x])) : joinBlocks ys


tests :: IO Bool
tests = do
  ref <- IORef.newIORef []

  result <-
    checkSequential $ Group "Test.Example.Resource" [
        ("prop_unix_sort", prop_logged_unix_sort ref)
      , ("prop_logged_unix_sort", prop_logged_unix_sort ref)
      , ("prop_logged_unix_sort_bracket", prop_logged_unix_sort_bracket ref)
      ]

  -- Print only unique blocks, remove 'List.nub' to see the complete log.
  liftIO $
    putStr .
    unlines .
    List.nub .
    joinBlocks =<<
    IORef.readIORef ref

  pure result
