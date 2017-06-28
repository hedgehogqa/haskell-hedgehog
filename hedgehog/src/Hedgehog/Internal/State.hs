{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Hedgehog.Internal.State (
  -- * Variables
    Var(..)
  , Symbolic(..)
  , Concrete(..)

  -- * Environment
  , Environment(..)
  , EnvironmentError(..)
  , emptyEnvironment
  , insertConcrete
  , reifyDynamic
  , reifyEnvironment
  , reify

  -- * Commands
  , Command(..)
  , Callback(..)
  , commandGenOK

  -- * Actions
  , Action(..)
  , takeVariables
  , variablesOK
  , dropInvalid
  , action
  , actions
  , execute
  , executeSequential
  ) where

import           Control.Monad (when, foldM_)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Morph (hoist)
import           Control.Monad.State.Class (get, put, modify)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, execState, evalStateT)

import           Data.Dynamic (Dynamic, toDyn, fromDynamic, dynTypeRep)
import           Data.Foldable (traverse_)
import           Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsPrec1)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Typeable (Typeable, TypeRep, Proxy(..), typeRep)

import           Hedgehog.Internal.Gen (Gen)
import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.HTraversable (HTraversable(..))
import           Hedgehog.Internal.Property (Test, liftEither, withCatch, success)
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
import           Hedgehog.Internal.Range (Range)


-- | Symbolic variable names.
--
newtype Var =
  Var Int
  deriving (Eq, Ord, Show, Num)

-- | Symbolic values.
--
data Symbolic a where
  Symbolic :: Typeable a => Var -> Symbolic a

deriving instance Eq (Symbolic a)
deriving instance Ord (Symbolic a)

instance Show (Symbolic a) where
  showsPrec p (Symbolic x) =
    showsPrec p x

instance Show1 Symbolic where
  liftShowsPrec _ _ p (Symbolic x) =
    showsPrec p x

instance Eq1 Symbolic where
  liftEq _ (Symbolic x) (Symbolic y) =
    x == y

instance Ord1 Symbolic where
  liftCompare _ (Symbolic x) (Symbolic y) =
    compare x y

-- | Concrete values.
--
newtype Concrete a where
  Concrete :: a -> Concrete a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Concrete a) where
  showsPrec =
    showsPrec1

instance Show1 Concrete where
  liftShowsPrec sp _ p (Concrete x) =
    sp p x

instance Eq1 Concrete where
  liftEq eq (Concrete x) (Concrete y) =
    eq x y

instance Ord1 Concrete where
  liftCompare comp (Concrete x) (Concrete y) =
    comp x y

------------------------------------------------------------------------
-- Symbolic Environment

-- | A mapping of symbolic values to concrete values.
--
newtype Environment =
  Environment {
      unEnvironment :: Map Var Dynamic
    } deriving (Show)

-- | Environment errors.
--
data EnvironmentError =
    EnvironmentValueNotFound !Var
  | EnvironmentTypeError !TypeRep !TypeRep
    deriving (Eq, Ord, Show)

-- | Create an empty environment.
--
emptyEnvironment :: Environment
emptyEnvironment =
  Environment Map.empty

-- | Insert a symbolic / concrete pairing in to the environment.
--
insertConcrete :: Symbolic a -> Concrete a -> Environment -> Environment
insertConcrete (Symbolic k) (Concrete v) =
  Environment . Map.insert k (toDyn v) . unEnvironment

-- | Cast a 'Dynamic' in to a concrete value.
--
reifyDynamic :: forall a. Typeable a => Dynamic -> Either EnvironmentError (Concrete a)
reifyDynamic dyn =
  case fromDynamic dyn of
    Nothing ->
      Left $ EnvironmentTypeError (typeRep (Proxy :: Proxy a)) (dynTypeRep dyn)
    Just x ->
      Right $ Concrete x

-- | Turns an environment in to a function for looking up a concrete value from
--   a symbolic one.
--
reifyEnvironment :: Environment -> (forall a. Symbolic a -> Either EnvironmentError (Concrete a))
reifyEnvironment (Environment vars) (Symbolic n) =
  case Map.lookup n vars of
    Nothing ->
      Left $ EnvironmentValueNotFound n
    Just dyn ->
      reifyDynamic dyn

-- | Convert a symbolic structure to a concrete one, using the provided environment.
--
reify :: HTraversable t => Environment -> t Symbolic -> Either EnvironmentError (t Concrete)
reify vars =
  htraverse (reifyEnvironment vars)

------------------------------------------------------------------------
-- Callbacks

-- | Optional command configuration.
--
data Callback input output m state =
  -- | A pre-condition for a command that must be verified before the command
  --   can be executed. This is mainly used during shrinking to ensure that it
  --   is still OK to run a command despite the fact that some previously
  --   executed commands may have been removed from the sequence.
  --
    Require (state Symbolic -> input Symbolic -> Bool)

  -- | Updates the model state, given the input and output of the command. Note
  --   that this function is polymorphic in the type of values. This is because
  --   it must work over 'Symbolic' values when we are generating actions, and
  --   'Concrete' values when we are executing them.
  --
  | Update (forall v. Ord1 v => state v -> input v -> v output -> state v)

  -- | A post-condition for a command that must be verified for the command to
  --   be considered a success.
  --
  | Ensure (state Concrete -> input Concrete -> output -> Test m ())

callbackRequire1 ::
     state Symbolic
  -> input Symbolic
  -> Callback input output m state
  -> Bool
callbackRequire1 s i = \case
  Require f ->
    f s i
  Update _ ->
    True
  Ensure _ ->
    True

callbackUpdate1 ::
     Ord1 v
  => state v
  -> input v
  -> v output
  -> Callback input output m state
  -> state v
callbackUpdate1 s i o = \case
  Require _ ->
    s
  Update f ->
    f s i o
  Ensure _ ->
    s

callbackEnsure1 ::
     Monad m
  => state Concrete
  -> input Concrete
  -> output
  -> Callback input output m state
  -> Test m ()
callbackEnsure1 s i o = \case
  Require _ ->
    success
  Update _ ->
    success
  Ensure f ->
    f s i o

callbackRequire ::
     [Callback input output m state]
  -> state Symbolic
  -> input Symbolic
  -> Bool
callbackRequire callbacks s i =
  all (callbackRequire1 s i) callbacks

callbackUpdate ::
     Ord1 v
  => [Callback input output m state]
  -> state v
  -> input v
  -> v output
  -> state v
callbackUpdate callbacks s0 i o =
  foldl (\s -> callbackUpdate1 s i o) s0 callbacks

callbackEnsure ::
     Monad m
  => [Callback input output m state]
  -> state Concrete
  -> input Concrete
  -> output
  -> Test m ()
callbackEnsure callbacks s i o =
  traverse_ (callbackEnsure1 s i o) callbacks

------------------------------------------------------------------------

-- | The specification for the expected behaviour of an 'Action'.
--
data Command n m (state :: (* -> *) -> *) =
  forall input output.
  (HTraversable input, Show (input Symbolic), Typeable output) =>
  Command {
    -- | A generator which provides random arguments for a command. If the
    --   command cannot be executed in the current state, it should return
    --   'Nothing'.
    --
      commandGen ::
        state Symbolic -> Maybe (Gen n (input Symbolic))

    -- | Executes a command using the arguments generated by 'commandGen'.
    --
    , commandExecute ::
        input Concrete -> Test m output

    -- | A set of callbacks which provide optional command configuration such
    --   as pre-condtions, post-conditions and state updates.
    --
    , commandCallbacks ::
        [Callback input output m state]
    }

-- | Checks that input for a command can be executed in the given state.
--
commandGenOK :: Command n m state -> state Symbolic -> Bool
commandGenOK (Command inputGen _ _) state =
  Maybe.isJust (inputGen state)

-- | An instantiation of a 'Command' which can be executed, and its effect
--   evaluated.
--
data Action m (state :: (* -> *) -> *) =
  forall input output.
  (HTraversable input, Show (input Symbolic)) =>
  Action {
      actionInput ::
        input Symbolic

    , actionOutput ::
        Symbolic output

    , actionExecute ::
        input Concrete -> Test m output

    , actionRequire ::
        state Symbolic -> input Symbolic -> Bool

    , actionUpdate ::
        forall v. Ord1 v => state v -> input v -> v output -> state v

    , actionEnsure ::
        state Concrete -> input Concrete -> output -> Test m ()
    }

instance Show (Action m state) where
  showsPrec p (Action input output _ _ _ _) =
    showParen (p > 10) $
      showsPrec 11 output .
      showString " :<- " .
      showsPrec 11 input

-- | Extract the variable name and the type from a symbolic value.
--
takeSymbolic :: forall a. Symbolic a -> (Var, TypeRep)
takeSymbolic (Symbolic var) =
  (var, typeRep (Proxy :: Proxy a))

-- | Insert a symbolic variable in to a map of variables to types.
--
insertSymbolic :: Symbolic a -> Map Var TypeRep -> Map Var TypeRep
insertSymbolic s =
  let
    (var, typ) =
      takeSymbolic s
  in
    Map.insert var typ

-- | Collects all the symbolic values in a data structure and produces a set of
--   all the variables they refer to.
--
takeVariables :: forall t. HTraversable t => t Symbolic -> Map Var TypeRep
takeVariables xs =
  let
    go x = do
      modify (insertSymbolic x)
      pure x
  in
    flip execState Map.empty $ htraverse go xs

-- | Checks that the symbolic values in the data structure refer only to the
--   variables in the provided set, and that they are of the correct type.
--
variablesOK :: HTraversable t => t Symbolic -> Map Var TypeRep -> Bool
variablesOK xs allowed =
  let
    vars =
      takeVariables xs
  in
    Map.null (vars `Map.difference` allowed) &&
    and (Map.intersectionWith (==) vars allowed)

-- | Drops invalid actions from the sequence.
--
dropInvalid :: (forall v. state v) -> [Action m state] -> [Action m state]
dropInvalid initial =
  let
    loop step@(Action input output _execute require update _ensure) = do
      ((state0, vars0), steps0) <- get

      when (require state0 input && variablesOK input vars0) $
        let
          state =
            update state0 input output

          vars =
            insertSymbolic output vars0

          steps =
            steps0 ++ [step]
        in
          put ((state, vars), steps)
  in
    snd . flip execState ((initial, Map.empty), []) . traverse_ loop

-- | Generates a single action from a set of possible commands.
--
action ::
     (Monad n, Monad m)
  => [Command n m state]
  -> Gen (StateT (state Symbolic, Var) n) (Action m state)
action commands =
  Gen.just $ do
    (state, var) <- get

    Command mgenInput exec callbacks <-
      Gen.element $ filter (\c -> commandGenOK c state) commands

    input <-
      case mgenInput state of
        Nothing ->
          error "genCommand: internal error, tried to use generator with invalid state."
        Just g ->
          hoist lift g

    if not $ callbackRequire callbacks state input then
      pure Nothing

    else do
      let
        output =
          Symbolic var

      put (callbackUpdate callbacks state input output, var + 1)

      pure . Just $
        Action input output exec
          (callbackRequire callbacks)
          (callbackUpdate callbacks)
          (callbackEnsure callbacks)

-- | Generates a sequence of actions from an initial model state and set of commands.
--
actions ::
     (Monad n, Monad m)
  => Range Int
  -> (forall v. state v)
  -> [Command n m state]
  -> Gen n [Action m state]
actions range initial =
  fmap (dropInvalid initial) .
  Gen.shrink Shrink.list .
  hoist (flip evalStateT (initial, 0)) .
  Gen.list range .
  action

-- | Executes a single action in the given evironment.
--
execute ::
     (HasCallStack, Monad m)
  => (state Concrete, Environment)
  -> Action m state
  -> Test m (state Concrete, Environment)
execute (state0, env0) (Action sinput soutput exec _require update ensure) =
  withFrozenCallStack $ do
    input <- liftEither $ reify env0 sinput
    output <- exec input

    let
      coutput =
        Concrete output

      state =
        update state0 input coutput

      env =
        insertConcrete soutput coutput env0

    ensure state input output

    pure (state, env)

-- | Executes a list of actions sequentially, verifying that all
--   post-conditions are met and no exceptions are thrown.
--
--   To generate a sequence of actions to execute, see the
--   'Hedgehog.Gen.actions' combinator in the "Hedgehog.Gen" module.
--
executeSequential ::
     forall m state.
     (HasCallStack, MonadCatch m)
  => (forall v. state v)
  -> [Action m state]
  -> Test m ()
executeSequential initial commands =
  withFrozenCallStack $
    withCatch (foldM_ execute (initial, emptyEnvironment) commands)
