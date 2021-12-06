--
-- Translated from https://github.com/advancedtelematic/quickcheck-state-machine/blob/7e3056d493ad430cfacd62da7878955e80fd296f/example/src/MutableReference.hs
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.References where

import           Control.Monad.IO.Class (MonadIO(..))
import           GHC.Generics (Generic)

import           Data.Bifunctor (second)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.List as List
import           Data.Kind (Type)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


------------------------------------------------------------------------
-- State

data State v =
  State {
      stateRefs :: [(Var (Opaque (IORef Int)) v, Int)]
    } deriving (Eq, Show)

initialState :: State v
initialState =
  State []

------------------------------------------------------------------------
-- NewRef

data NewRef (v :: Type -> Type) =
  NewRef
  deriving (Eq, Show, Generic)

-- This would be more nicely done with DerivingStrategies anyclass but
-- it's not supported in GHC 8.0, in your own app you have more options.
instance FunctorB NewRef
instance TraversableB NewRef

newRef :: (Monad n, MonadIO m) => Command n m State
newRef =
  let
    gen _s =
      Just $
        pure NewRef

    execute _i =
      fmap Opaque . liftIO $ IORef.newIORef 0
  in
    Command gen execute [
        Update $ \(State xs) _i o ->
          State $
            xs ++ [(o, 0)]
      ]

------------------------------------------------------------------------
-- ReadRef

data ReadRef v =
  ReadRef (Var (Opaque (IORef Int)) v)
  deriving (Eq, Show, Generic)

instance FunctorB ReadRef
instance TraversableB ReadRef

readRef :: (MonadGen n, MonadIO m, MonadTest m) => Command n m State
readRef =
  let
    gen s =
      case stateRefs s of
        [] ->
          Nothing
        xs ->
          Just $
            ReadRef <$> Gen.element (fmap fst xs)

    execute (ReadRef ref) =
      liftIO $ IORef.readIORef (opaque ref)
  in
    Command gen execute [
        Require $ \(State xs) (ReadRef ref) ->
          elem ref $ fmap fst xs

      , Ensure $ \_s0 s (ReadRef ref) o ->
          lookup ref (stateRefs s) === Just o
      ]

------------------------------------------------------------------------
-- WriteRef

data WriteRef v =
  WriteRef (Var (Opaque (IORef Int)) v) Int
  deriving (Eq, Show, Generic)

instance FunctorB WriteRef
instance TraversableB WriteRef

writeRef :: (MonadGen n, MonadIO m) => Command n m State
writeRef =
  let
    gen s =
      case stateRefs s of
        [] ->
          Nothing
        xs ->
          Just $
            WriteRef
              <$> Gen.element (fmap fst xs)
              <*> Gen.int (Range.linear 0 100)

    execute (WriteRef ref x) =
      liftIO $ IORef.writeIORef (opaque ref) x
  in
    Command gen execute [
        Require $ \(State xs) (WriteRef ref _) ->
          elem ref $ fmap fst xs

      , Update $ \(State xs) (WriteRef ref x) _o ->
          State $
            (ref, x) : filter ((/= ref) . fst) xs
      ]

------------------------------------------------------------------------
-- IncRef

data IncRef v =
  IncRef (Var (Opaque (IORef Int)) v)
  deriving (Eq, Show, Generic)

instance FunctorB IncRef
instance TraversableB IncRef

incRef :: (MonadGen n, MonadIO m) => Int -> Command n m State
incRef n =
  let
    gen s =
      case stateRefs s of
        [] ->
          Nothing
        xs ->
          Just $
            IncRef <$> Gen.element (fmap fst xs)

    execute (IncRef ref) = do
      x <- liftIO $ IORef.readIORef (opaque ref)
      liftIO $ IORef.writeIORef (opaque ref) (x + n) -- deliberate bug
  in
    Command gen execute [
        Require $ \(State xs) (IncRef ref) ->
          elem ref $ fmap fst xs

      , Update $ \(State xs) (IncRef ref) _o ->
          State $
            let
              (xs1, xs2) =
                List.partition ((== ref) . fst) xs
            in
              fmap (second (+1)) xs1 ++ xs2
      ]

------------------------------------------------------------------------

prop_references_sequential :: Property
prop_references_sequential =
  property $ do
    actions <- forAll $
      Gen.sequential (Range.linear 1 100) initialState [
          newRef
        , readRef
        , writeRef
        , incRef 2
        ]

    executeSequential initialState actions

prop_references_parallel :: Property
prop_references_parallel =
  withTests 1000 . withRetries 10 . property $ do
    actions <- forAll $
      Gen.parallel (Range.linear 1 50) (Range.linear 1 10) initialState [
          newRef
        , readRef
        , writeRef
        , incRef 1
        ]

    test $
      executeParallel initialState actions

------------------------------------------------------------------------

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
