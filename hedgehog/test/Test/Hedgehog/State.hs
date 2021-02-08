{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Test.Hedgehog.State (tests) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef (IORef, readIORef, atomicModifyIORef', newIORef)
import           Data.Kind (Type)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data State = State (IORef Int)

createState :: IO State
createState = State <$> newIORef 0

counterValue :: State -> IO Int
counterValue (State ref) = readIORef ref

incrementState :: State -> IO ()
incrementState (State ref) = atomicModifyIORef' ref (\x -> (x+1,()))

decrementState :: State -> IO ()
decrementState (State ref) = atomicModifyIORef' ref (\x -> (x-1,()))

data Model (v :: Type -> Type) = Model Int

data Increment (v :: Type -> Type) = Increment deriving Show
instance HTraversable Increment where
  htraverse _ Increment = pure Increment

data Decrement (v :: Type -> Type) = Decrement deriving Show
instance HTraversable Decrement where
  htraverse _ Decrement = pure Decrement

data GetCounter (v :: Type -> Type) = GetCounter deriving Show
instance HTraversable GetCounter where
  htraverse _ GetCounter = pure GetCounter

cIncrement :: forall gen m. (MonadGen gen, MonadTest m, MonadIO m)
           => State
           -> Command gen m Model
cIncrement s = Command gen exec cbs
  where
    gen :: Model Symbolic -> Maybe (gen (Increment Symbolic))
    gen _ = Just (pure Increment)
    exec :: Increment Concrete -> m ()
    exec _ = evalIO (incrementState s)
    cbs = [ Update $ \(Model value) _i _o -> Model (value + 1)
          ]

cDecrement :: forall gen m. (MonadGen gen, MonadTest m, MonadIO m)
           => State
           -> Command gen m Model
cDecrement s = Command gen exec cbs
  where
    gen :: Model Symbolic -> Maybe (gen (Decrement Symbolic))
    gen _ = Just (pure Decrement)
    exec :: Decrement Concrete -> m ()
    exec _ = evalIO (decrementState s)
    cbs = [ Update $ \(Model value) _i _o -> Model (value - 1)
          ]

cGetCounter :: forall gen m. (MonadGen gen, MonadTest m, MonadIO m)
           => State
           -> Command gen m Model
cGetCounter s = Command gen exec cbs
  where
    gen :: Model Symbolic -> Maybe (gen (GetCounter Symbolic))
    gen _ = Just (pure GetCounter)
    exec :: GetCounter Concrete -> m Int
    exec _ = evalIO (counterValue s)
    cbs = [ Ensure $ \_oldState (Model modelValue) _i retrievedValue -> modelValue === retrievedValue
          ]

commandsGen :: (MonadGen gen, MonadTest m, MonadIO m)
            => State
            -> gen (Command gen m Model)
commandsGen s = Gen.frequency $
  zipWith
    (\freq cmd -> (freq, pure (cmd s)))
    [1, 1, 2]
    [cIncrement, cDecrement, cGetCounter]

prop_commands_gen :: Property
prop_commands_gen =
  property $ do
    state <- evalIO createState
    let initialModel = Model 0
    actions <- forAll $ Gen.sequential' (Range.linear 1 10) initialModel (commandsGen state)
    executeSequential initialModel actions

tests :: IO Bool
tests =
  checkParallel $$(discover)
