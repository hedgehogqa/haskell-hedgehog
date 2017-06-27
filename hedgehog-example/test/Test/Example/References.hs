--
-- Translated from https://github.com/advancedtelematic/quickcheck-state-machine/blob/7e3056d493ad430cfacd62da7878955e80fd296f/example/src/MutableReference.hs
--
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.References where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Bifunctor (second)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


------------------------------------------------------------------------
-- State

data Ref v =
  Ref (v (Opaque (IORef Int)))

data State v =
  State {
      stateRefs :: [(Ref v, Int)]
    } deriving (Eq, Show)

initialState :: State v
initialState =
  State []

instance Eq1 v => Eq (Ref v) where
  (==) (Ref x) (Ref y) =
    eq1 x y

instance Show1 v => Show (Ref v) where
  showsPrec p (Ref x) =
    showParen (p >= 11) $
      showString "Ref " .
      showsPrec1 11 x

instance HTraversable Ref where
  htraverse f (Ref v) =
    fmap Ref (f v)

------------------------------------------------------------------------
-- NewRef

data NewRef (v :: * -> *) =
  NewRef
  deriving (Eq, Show)

instance HTraversable NewRef where
  htraverse _ NewRef =
    pure NewRef

newRef :: Monad m => Command m IO State
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
            xs ++ [(Ref o, 0)]
      ]

------------------------------------------------------------------------
-- ReadRef

data ReadRef v =
  ReadRef (Ref v)
  deriving (Eq, Show)

instance HTraversable ReadRef where
  htraverse f (ReadRef ref) =
    ReadRef <$> htraverse f ref

readRef :: Monad m => Command m IO State
readRef =
  let
    gen s =
      case stateRefs s of
        [] ->
          Nothing
        xs ->
          Just $
            ReadRef <$> Gen.element (fmap fst xs)

    execute (ReadRef (Ref (Concrete (Opaque ref)))) =
      liftIO $ IORef.readIORef ref
  in
    Command gen execute [
        Require $ \(State xs) (ReadRef ref) ->
          elem ref $ fmap fst xs

      , Ensure $ \s (ReadRef ref) o ->
          lookup ref (stateRefs s) === Just o
      ]

------------------------------------------------------------------------
-- WriteRef

data WriteRef v =
  WriteRef (Ref v) Int
  deriving (Eq, Show)

instance HTraversable WriteRef where
  htraverse f (WriteRef ref x) =
    WriteRef <$> htraverse f ref <*> pure x

writeRef :: Monad m => Command m IO State
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

    execute (WriteRef (Ref (Concrete (Opaque ref))) x) =
      liftIO $ IORef.writeIORef ref x
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
  IncRef (Ref v)
  deriving (Eq, Show)

instance HTraversable IncRef where
  htraverse f (IncRef ref) =
    IncRef <$> htraverse f ref

incRef :: Monad m => Command m IO State
incRef =
  let
    gen s =
      case stateRefs s of
        [] ->
          Nothing
        xs ->
          Just $
            IncRef <$> Gen.element (fmap fst xs)

    execute (IncRef (Ref (Concrete (Opaque ref)))) = do
      x <- liftIO $ IORef.readIORef ref
      liftIO $ IORef.writeIORef ref (x + 2) -- deliberate bug
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

prop_references :: Property
prop_references =
  property $ do
    actions <- forAll $
      Gen.actions (Range.linear 1 100) initialState [newRef, readRef, writeRef, incRef]

    executeSequential initialState actions

------------------------------------------------------------------------

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
