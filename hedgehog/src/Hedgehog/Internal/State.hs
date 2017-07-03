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
  , concrete
  , opaque

  , Concrete(..)
  , Symbolic(..)
  , Name(..)

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
import           Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import           Data.Functor.Classes (eq1, compare1, showsPrec1)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Typeable (Typeable, TypeRep, Proxy(..), typeRep)

import           Hedgehog.Internal.Gen (Gen)
import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.HTraversable (HTraversable(..))
import           Hedgehog.Internal.Opaque (Opaque(..))
import           Hedgehog.Internal.Property (MonadTest, evalEither, evalM, success)
import           Hedgehog.Internal.Range (Range)
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)


-- | Symbolic variable names.
--
newtype Name =
  Name Int
  deriving (Eq, Ord, Num)

instance Show Name where
  showsPrec p (Name x) =
    showsPrec p x

-- | Symbolic values.
--
data Symbolic a where
  Symbolic :: Typeable a => Name -> Symbolic a

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

-- | Variables are the potential or actual result of executing an action. They
--   are parameterised by either `Symbolic` or `Concrete` depending on the
--   phase of the test.
--
--   `Symbolic` variables are the potential results of actions. These are used
--   when generating the sequence of actions to execute. They allow actions
--   which occur later in the sequence to make use of the result of an action
--   which came earlier in the sequence.
--
--   `Concrete` variables are the actual results of actions. These are used
--   during test execution. They provide access to the actual runtime value of
--   a variable.
--
--   The state update `Callback` for a command needs to be polymorphic in the
--   type of variable because it is used in both the generation and the
--   execution phase.
--
data Var a v =
  Var (v a)

-- | Take the value from a concrete variable.
--
concrete :: Var a Concrete -> a
concrete (Var (Concrete x)) =
  x

-- | Take the value from an opaque concrete variable.
--
opaque :: Var (Opaque a) Concrete -> a
opaque (Var (Concrete (Opaque x))) =
  x

instance (Eq a, Eq1 v) => Eq (Var a v) where
  (==) (Var x) (Var y) =
    eq1 x y

instance (Ord a, Ord1 v) => Ord (Var a v) where
  compare (Var x) (Var y) =
    compare1 x y

instance (Show a, Show1 v) => Show (Var a v) where
  showsPrec p (Var x) =
    showParen (p >= 11) $
      showString "Var " .
      showsPrec1 11 x

instance HTraversable (Var a) where
  htraverse f (Var v) =
    fmap Var (f v)

------------------------------------------------------------------------
-- Symbolic Environment

-- | A mapping of symbolic values to concrete values.
--
newtype Environment =
  Environment {
      unEnvironment :: Map Name Dynamic
    } deriving (Show)

-- | Environment errors.
--
data EnvironmentError =
    EnvironmentValueNotFound !Name
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
  | Update (forall v. Ord1 v => state v -> input v -> Var output v -> state v)

  -- | A post-condition for a command that must be verified for the command to
  --   be considered a success.
  --
  --   This callback receives the state prior to execution as the first
  --   argument, and the state after execution as the second argument.
  --
  | Ensure (state Concrete -> state Concrete -> input Concrete -> output -> m ())

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
  -> Var output v
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
     MonadTest m
  => state Concrete
  -> state Concrete
  -> input Concrete
  -> output
  -> Callback input output m state
  -> m ()
callbackEnsure1 s0 s i o = \case
  Require _ ->
    success
  Update _ ->
    success
  Ensure f ->
    f s0 s i o

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
  -> Var output v
  -> state v
callbackUpdate callbacks s0 i o =
  foldl (\s -> callbackUpdate1 s i o) s0 callbacks

callbackEnsure ::
     MonadTest m
  => [Callback input output m state]
  -> state Concrete
  -> state Concrete
  -> input Concrete
  -> output
  -> m ()
callbackEnsure callbacks s0 s i o =
  traverse_ (callbackEnsure1 s0 s i o) callbacks

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
        input Concrete -> m output

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
        input Concrete -> m output

    , actionRequire ::
        state Symbolic -> input Symbolic -> Bool

    , actionUpdate ::
        forall v. Ord1 v => state v -> input v -> Var output v -> state v

    , actionEnsure ::
        state Concrete -> state Concrete -> input Concrete -> output -> m ()
    }

instance Show (Action m state) where
  showsPrec p (Action input (Symbolic (Name output)) _ _ _ _) =
    showParen (p > 10) $
      showString "Var " .
      showsPrec 11 output .
      showString " :<- " .
      showsPrec 11 input

-- | Extract the variable name and the type from a symbolic value.
--
takeSymbolic :: forall a. Symbolic a -> (Name, TypeRep)
takeSymbolic (Symbolic name) =
  (name, typeRep (Proxy :: Proxy a))

-- | Insert a symbolic variable in to a map of variables to types.
--
insertSymbolic :: Symbolic a -> Map Name TypeRep -> Map Name TypeRep
insertSymbolic s =
  let
    (name, typ) =
      takeSymbolic s
  in
    Map.insert name typ

-- | Collects all the symbolic values in a data structure and produces a set of
--   all the variables they refer to.
--
takeVariables :: forall t. HTraversable t => t Symbolic -> Map Name TypeRep
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
variablesOK :: HTraversable t => t Symbolic -> Map Name TypeRep -> Bool
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
            update state0 input (Var output)

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
     (Monad n, MonadTest m)
  => [Command n m state]
  -> Gen (StateT (state Symbolic, Name) n) (Action m state)
action commands =
  Gen.just $ do
    (state, name) <- get

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
          Symbolic name

      put (callbackUpdate callbacks state input (Var output), name + 1)

      pure . Just $
        Action input output exec
          (callbackRequire callbacks)
          (callbackUpdate callbacks)
          (callbackEnsure callbacks)

-- | Generates a sequence of actions from an initial model state and set of commands.
--
actions ::
     (Monad n, MonadTest m)
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
     (HasCallStack, MonadTest m)
  => (state Concrete, Environment)
  -> Action m state
  -> m (state Concrete, Environment)
execute (state0, env0) (Action sinput soutput exec _require update ensure) =
  withFrozenCallStack $ do
    input <- evalEither $ reify env0 sinput
    output <- exec input

    let
      coutput =
        Concrete output

      state =
        update state0 input (Var coutput)

      env =
        insertConcrete soutput coutput env0

    ensure state0 state input output

    pure (state, env)

-- | Executes a list of actions sequentially, verifying that all
--   post-conditions are met and no exceptions are thrown.
--
--   To generate a sequence of actions to execute, see the
--   'Hedgehog.Gen.actions' combinator in the "Hedgehog.Gen" module.
--
executeSequential ::
     forall m state.
     (HasCallStack, MonadTest m, MonadCatch m)
  => (forall v. state v)
  -> [Action m state]
  -> m ()
executeSequential initial commands =
  withFrozenCallStack $
    evalM (foldM_ execute (initial, emptyEnvironment) commands)
