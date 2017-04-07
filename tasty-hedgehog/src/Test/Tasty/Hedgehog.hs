{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Tasty.Hedgehog (
    testProperty
  ) where

import Control.Monad (when)
import Data.Typeable
import Data.Proxy
import Data.List
import Data.Maybe

import Test.Tasty.Providers
import Test.Tasty.Options

import Hedgehog
import Hedgehog.Internal.Property
import Hedgehog.Internal.Runner
import Hedgehog.Internal.Report
import Hedgehog.Internal.Seed as Seed

newtype HP = HP Property
  deriving (Typeable)

-- | Create a 'Test' for a Hedgehog property
testProperty :: TestName -> Property ->TestTree
testProperty name prop = singleTest name (HP prop)

newtype HedgehogReplay = HedgehogReplay (Maybe (Size, Seed))
  deriving (Typeable)

instance IsOption HedgehogReplay where
  defaultValue = HedgehogReplay Nothing
  parseValue v = HedgehogReplay . Just <$> replay
    -- Reads a replay token in the form "{size} {seed}"
    where replay = (,) <$> safeRead (intercalate " " seed) <*> safeRead (concat size)
          (size, seed) = splitAt 1 $ words v
  optionName = return "hedgehog-replay"
  optionHelp = return "Replay token to use for replaying a previous test run"

newtype HedgehogShowReplay = HedgehogShowReplay Bool
  deriving (Typeable)

instance IsOption HedgehogShowReplay where
  defaultValue = HedgehogShowReplay True
  parseValue = fmap HedgehogShowReplay . safeRead
  optionName = return "hedgehog-show-replay"
  optionHelp = return "Show a replay token for replaying tests"

newtype HedgehogVerbose = HedgehogVerbose Bool
  deriving (Typeable)

instance IsOption HedgehogVerbose where
  defaultValue = HedgehogVerbose False
  parseValue = fmap HedgehogVerbose . safeRead
  optionName = return "hedgehog-verbose"
  optionHelp = return "Show the generated Hedgehog test cases"
  optionCLParser = flagCLParser Nothing (HedgehogVerbose True)

newtype HedgehogTestLimit = HedgehogTestLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogTestLimit where
  defaultValue = 100
  parseValue = fmap HedgehogTestLimit . safeRead
  optionName = return "hedgehog-tests"
  optionHelp = return "Number of successful test cases required before Hedgehog will pass a test"

newtype HedgehogDiscardLimit = HedgehogDiscardLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogDiscardLimit where
  defaultValue = 100
  parseValue = fmap HedgehogDiscardLimit . safeRead
  optionName = return "hedgehog-discards"
  optionHelp = return "Number of discarded cases allowed before Hedgehog will fail a test"

newtype HedgehogShrinkLimit = HedgehogShrinkLimit Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Typeable)

instance IsOption HedgehogShrinkLimit where
  defaultValue = 100
  parseValue = fmap HedgehogShrinkLimit . safeRead
  optionName = return "hedgehog-shrinks"
  optionHelp = return "Number of shrinks allowed before Hedgehog will fail a test"

instance IsTest HP where
  testOptions =
    return [ Option (Proxy :: Proxy HedgehogReplay)
           , Option (Proxy :: Proxy HedgehogShowReplay)
           , Option (Proxy :: Proxy HedgehogVerbose)
           , Option (Proxy :: Proxy HedgehogTestLimit)
           , Option (Proxy :: Proxy HedgehogDiscardLimit)
           , Option (Proxy :: Proxy HedgehogShrinkLimit)
           ]

  run opts (HP (Property _ test)) _ = do
    let
      HedgehogReplay         replay = lookupOption opts
      HedgehogShowReplay showReplay = lookupOption opts
      HedgehogVerbose       verbose = lookupOption opts
      HedgehogTestLimit       tests = lookupOption opts
      HedgehogDiscardLimit discards = lookupOption opts
      HedgehogShrinkLimit   shrinks = lookupOption opts
      config = Config (TestLimit tests) (DiscardLimit discards) (ShrinkLimit shrinks)

    randSeed <- Seed.random
    let
      size = maybe 0 fst replay
      seed = maybe randSeed snd replay

    report <- checkReport config size seed test
    when verbose $
      printReport Nothing report

    let
      -- TODO non-verbose failure output
      output = ""
      replayOutput =
        if showReplay
        then "Use \"--hedgehog-replay " ++ show size ++ " " ++ show seed ++ "\" to reproduce."
        else ""

    return $ if reportStatus report == OK
             then testPassed ""
             else testFailed $ output ++ "\n" ++ replayOutput
