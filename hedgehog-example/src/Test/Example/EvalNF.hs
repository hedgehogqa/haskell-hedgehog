{-# LANGUAGE TemplateHaskell #-}
module Test.Example.EvalNF where

import           Data.Functor (void)

import           Hedgehog

prop_evalNF_success :: Property
prop_evalNF_success =
  property $
    void $
      evalNF [""]

prop_evalNF_failure :: Property
prop_evalNF_failure =
  property $
    void $
      evalNF ["", undefined]

------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$(discover)
