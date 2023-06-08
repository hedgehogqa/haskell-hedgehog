{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , Sequential(..)
  , Parallel(..)
  , takeVariables
  , variablesOK
  , dropInvalid
  , action
  , sequential
  , parallel
  , executeSequential
  , executeParallel
  ) where

import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Exception.Safe (MonadCatch)
import           Control.Monad (foldM, foldM_)
import           Control.Monad.State.Class (MonadState, get, put, modify)
import           Control.Monad.Morph (MFunctor(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.State (State, runState, execState)
import           Control.Monad.Trans.State (StateT(..), evalStateT, runStateT)

import           Data.Dynamic (Dynamic, toDyn, fromDynamic, dynTypeRep)
import           Data.Foldable (traverse_)
import           Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import           Data.Functor.Classes (eq1, compare1, showsPrec1)
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Typeable (Typeable, TypeRep, Proxy(..), typeRep)

import           Hedgehog.Internal.Barbie (FunctorB(..), TraversableB(..))
import           Hedgehog.Internal.Distributive (distributeT)
import           Hedgehog.Internal.Gen (MonadGen, GenT, GenBase)
import qualified Hedgehog.Internal.Gen as Gen
import           Hedgehog.Internal.Opaque (Opaque(..))
import           Hedgehog.Internal.Property (MonadTest(..), Test, evalEither, evalM, success, runTest, failWith, annotate)
import           Hedgehog.Internal.Range (Range)
import           Hedgehog.Internal.Show (showPretty)
import           Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)


-- | Symbolic variable names.
--
newtype Name =
  Name Int
  deriving (Eq, Ord, Num)

instance Show Name where
  showsPrec p (Name x) =
    showsPrec p x

-- | Symbolic values: Because hedgehog generates actions in a separate phase
--   before execution, you will sometimes need to refer to the result of a
--   previous action in a generator without knowing the value of the result
--   (e.g., to get the ID of a previously-created user).
--
--   Symbolic variables provide a token to stand in for the actual variables at
--   generation time (and in 'Require'/'Update' callbacks). At execution time,
--   real values are available, so your execute actions work on 'Concrete'
--   variables.
--
--   See also: 'Command', 'Var'
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

-- | Concrete values: At test-execution time, 'Symbolic' values from generation
--   are replaced with 'Concrete' values from performing actions. This type
--   gives us something of the same kind as 'Symbolic' to pass as a type
--   argument to 'Var'.
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
--   The order of arguments makes 'Var' 'FunctorB' and 'TraversableB', which is
--   how 'Symbolic' values are turned into 'Concrete' ones.
--
newtype Var a v =
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

instance FunctorB (Var a) where
  bmap f (Var v) =
    Var (f v)

instance TraversableB (Var a) where
  btraverse f (Var v) =
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

unionsEnvironment :: [Environment] -> Environment
unionsEnvironment =
  Environment . Map.unions . fmap unEnvironment

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
reify :: TraversableB t => Environment -> t Symbolic -> Either EnvironmentError (t Concrete)
reify vars =
  btraverse (reifyEnvironment vars)

------------------------------------------------------------------------
-- Callbacks

-- | Optional command configuration.
--
data Callback input output state =
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
  | Ensure (state Concrete -> state Concrete -> input Concrete -> output -> Test ())

callbackRequire1 ::
     state Symbolic
  -> input Symbolic
  -> Callback input output state
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
  -> Callback input output state
  -> state v
callbackUpdate1 s i o = \case
  Require _ ->
    s
  Update f ->
    f s i o
  Ensure _ ->
    s

callbackEnsure1 ::
     state Concrete
  -> state Concrete
  -> input Concrete
  -> output
  -> Callback input output state
  -> Test ()
callbackEnsure1 s0 s i o = \case
  Require _ ->
    success
  Update _ ->
    success
  Ensure f ->
    f s0 s i o

callbackRequire ::
     [Callback input output state]
  -> state Symbolic
  -> input Symbolic
  -> Bool
callbackRequire callbacks s i =
  all (callbackRequire1 s i) callbacks

callbackUpdate ::
     Ord1 v
  => [Callback input output state]
  -> state v
  -> input v
  -> Var output v
  -> state v
callbackUpdate callbacks s0 i o =
  foldl (\s -> callbackUpdate1 s i o) s0 callbacks

callbackEnsure ::
     [Callback input output state]
  -> state Concrete
  -> state Concrete
  -> input Concrete
  -> output
  -> Test ()
callbackEnsure callbacks s0 s i o =
  traverse_ (callbackEnsure1 s0 s i o) callbacks

------------------------------------------------------------------------

-- | The specification for the expected behaviour of an
-- 'Action'. These are used to generate sequences of actions to test.
--
-- This is the main type you will use when writing state machine
-- tests. @gen@ is usually an instance of 'MonadGen', and @m@ is usually
-- an instance of 'MonadTest'. These constraints appear when you pass
-- your 'Command' list to 'sequential' or 'parallel'.
--
data Command gen m (state :: (Type -> Type) -> Type) =
  forall input output.
  (TraversableB input, Show (input Symbolic), Show output, Typeable output) =>
  Command {
    -- | A generator which provides random arguments for a command. If the
    --   command cannot be executed in the current state, it should return
    --   'Nothing'.
    --
      commandGen ::
        state Symbolic -> Maybe (gen (input Symbolic))

    -- | Executes a command using the arguments generated by 'commandGen'.
    --
    , commandExecute ::
        input Concrete -> m output

    -- | A set of callbacks which provide optional command configuration such
    --   as pre-condtions, post-conditions and state updates.
    --
    , commandCallbacks ::
        [Callback input output state]
    }

-- | Checks that input for a command can be executed in the given state.
--
commandGenOK :: Command gen m state -> state Symbolic -> Bool
commandGenOK (Command inputGen _ _) state =
  Maybe.isJust (inputGen state)

-- | An instantiation of a 'Command' which can be executed, and its effect
--   evaluated.
--
data Action m (state :: (Type -> Type) -> Type) =
  forall input output.
  (TraversableB input, Show (input Symbolic), Show output) =>
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
        state Concrete -> state Concrete -> input Concrete -> output -> Test ()
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
takeVariables :: forall t. TraversableB t => t Symbolic -> Map Name TypeRep
takeVariables xs =
  let
    go x = do
      modify (insertSymbolic x)
      pure x
  in
    flip execState Map.empty $ btraverse go xs

-- | Checks that the symbolic values in the data structure refer only to the
--   variables in the provided set, and that they are of the correct type.
--
variablesOK :: TraversableB t => t Symbolic -> Map Name TypeRep -> Bool
variablesOK xs allowed =
  let
    vars =
      takeVariables xs
  in
    Map.null (vars `Map.difference` allowed) &&
    and (Map.intersectionWith (==) vars allowed)

data Context state =
  Context {
      contextState :: state Symbolic
    , _contextVars :: Map Name TypeRep
    }

mkContext :: state Symbolic -> Context state
mkContext initial =
  Context initial Map.empty

contextUpdate :: MonadState (Context state) m => state Symbolic -> m ()
contextUpdate state = do
  Context _ vars <- get
  put $ Context state vars

contextNewVar :: (MonadState (Context state) m, Typeable a) => m (Symbolic a)
contextNewVar = do
  Context state vars <- get

  let
    var =
      case Map.maxViewWithKey vars of
        Nothing ->
          Symbolic 0
        Just ((name, _), _) ->
          Symbolic (name + 1)

  put $ Context state (insertSymbolic var vars)
  pure var

-- | Drops invalid actions from the sequence.
--
dropInvalid :: [Action m state] -> State (Context state) [Action m state]
dropInvalid =
  let
    loop step@(Action input output _execute require update _ensure) = do
      Context state0 vars0 <- get

      if require state0 input && variablesOK input vars0 then do
        let
          state =
            update state0 input (Var output)

          vars =
            insertSymbolic output vars0

        put $ Context state vars
        pure $ Just step
      else
        pure Nothing
  in
    fmap Maybe.catMaybes . traverse loop

-- | Generates a single action from a set of possible commands.
--
action ::
     (MonadGen gen, MonadTest m)
  => [Command gen m state]
  -> GenT (StateT (Context state) (GenBase gen)) (Action m state)
action commands =
  Gen.justT $ do
    Context state0 _ <- get

    Command mgenInput exec callbacks <-
      Gen.element_ $ filter (\c -> commandGenOK c state0) commands

    -- If we shrink the input, we still want to use the same output. Otherwise
    -- any actions using this output as part of their input will be dropped. But
    -- the existing output is still in the context, so `contextNewVar` will
    -- create a new one. To avoid that, we generate the output before the input.
    output <- contextNewVar

    input <-
      case mgenInput state0 of
        Nothing ->
          error "genCommand: internal error, tried to use generator with invalid state."
        Just gen ->
          hoist lift $ Gen.toGenT gen

    if not $ callbackRequire callbacks state0 input then
      pure Nothing

    else do
      contextUpdate $
        callbackUpdate callbacks state0 input (Var output)

      pure . Just $
        Action input output exec
          (callbackRequire callbacks)
          (callbackUpdate callbacks)
          (callbackEnsure callbacks)

genActions ::
     (MonadGen gen, MonadTest m)
  => Range Int
  -> [Command gen m state]
  -> Context state
  -> gen ([Action m state], Context state)
genActions range commands ctx = do
  xs <- Gen.fromGenT . (`evalStateT` ctx) . distributeT $ Gen.list range (action commands)
  pure $
    dropInvalid xs `runState` ctx

-- | A sequence of actions to execute.
--
newtype Sequential m state =
  Sequential {
      -- | The sequence of actions.
      sequentialActions :: [Action m state]
    }

renderAction :: Action m state -> [String]
renderAction (Action input (Symbolic (Name output)) _ _ _ _) =
  let
    prefix0 =
      "Var " ++ show output ++ " = "

    prefix =
      replicate (length prefix0) ' '
  in
    case lines (showPretty input) of
      [] ->
        [prefix0 ++ "?"]
      x : xs ->
        (prefix0 ++ x) :
        fmap (prefix ++) xs

renderActionResult :: Environment -> Action m state -> [String]
renderActionResult env (Action _ output@(Symbolic (Name name)) _ _ _ _) =
  let
    prefix0 =
      "Var " ++ show name ++ " = "

    prefix =
      replicate (length prefix0) ' '

    unfound = \case
      EnvironmentValueNotFound _
        -> "<<not found in environment>>"
      EnvironmentTypeError _ _
        -> "<<type representation in environment unexpected>>"

    actual =
      either unfound showPretty
        $ reifyEnvironment env output

  in
    case lines actual of
      [] ->
        [prefix0 ++ "?"]
      x : xs ->
        (prefix0 ++ x) :
        fmap (prefix ++) xs

-- FIXME we should not abuse Show to get nice output for actions
instance Show (Sequential m state) where
  show (Sequential xs) =
    unlines $ concatMap renderAction xs

-- | Generates a sequence of actions from an initial model state and set of commands.
--
sequential ::
     (MonadGen gen, MonadTest m)
  => Range Int
  -> (forall v. state v)
  -> [Command gen m state]
  -> gen (Sequential m state)
sequential range initial commands =
  fmap (Sequential . fst) $
    genActions range commands (mkContext initial)

-- | A sequential prefix of actions to execute, with two branches to execute in parallel.
--
data Parallel m state =
  Parallel {
      -- | The sequential prefix.
      parallelPrefix :: [Action m state]

      -- | The first branch.
    , parallelBranch1 :: [Action m state]

      -- | The second branch.
    , parallelBranch2 :: [Action m state]
    }

-- FIXME we should not abuse Show to get nice output for actions
instance Show (Parallel m state) where
  show =
    renderParallel renderAction

renderParallel :: (Action m state -> [String]) -> Parallel m state -> String
renderParallel render (Parallel pre xs ys) =
  unlines $ concat [
      ["━━━ Prefix ━━━"]
    , concatMap render pre
    , ["", "━━━ Branch 1 ━━━"]
    , concatMap render xs
    , ["", "━━━ Branch 2 ━━━"]
    , concatMap render ys
    ]


-- | Given the initial model state and set of commands, generates prefix
--   actions to be run sequentially, followed by two branches to be run in
--   parallel.
--
parallel ::
     (MonadGen gen, MonadTest m)
  => Range Int
  -> Range Int
  -> (forall v. state v)
  -> [Command gen m state]
  -> gen (Parallel m state)
parallel prefixN parallelN initial commands = do
  (prefix, ctx0) <- genActions prefixN commands (mkContext initial)
  (branch1, ctx1) <- genActions parallelN commands ctx0
  (branch2, _ctx2) <- genActions parallelN commands ctx1 { contextState = contextState ctx0 }

  pure $ Parallel prefix branch1 branch2

data ActionCheck state =
  ActionCheck {
      checkUpdate :: state Concrete -> state Concrete
    , checkEnsure :: state Concrete -> state Concrete -> Test ()
    }

execute :: (MonadTest m, HasCallStack) => Action m state -> StateT Environment m (ActionCheck state)
execute (Action sinput soutput exec _require update ensure) =
  withFrozenCallStack $ do
    env0 <- get
    input <- evalEither $ reify env0 sinput
    output <- lift $ exec input

    let
      coutput =
        Concrete output

      env =
        insertConcrete soutput coutput env0

    put env

    pure $
      ActionCheck
        (\s0 -> update s0 input (Var coutput))
        (\s0 s -> ensure s0 s input output)

-- | Executes a single action in the given evironment.
--
executeUpdateEnsure ::
     (MonadTest m, HasCallStack)
  => (state Concrete, Environment)
  -> Action m state
  -> m (state Concrete, Environment)
executeUpdateEnsure (state0, env0) (Action sinput soutput exec _require update ensure) =
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

    liftTest $ ensure state0 state input output

    pure (state, env)

-- | Executes a list of actions sequentially, verifying that all
--   post-conditions are met and no exceptions are thrown.
--
--   To generate a sequence of actions to execute, see the
--   'Hedgehog.Gen.sequential' combinator in the "Hedgehog.Gen" module.
--
executeSequential ::
     (MonadTest m, MonadCatch m, HasCallStack)
  => (forall v. state v)
  -> Sequential m state
  -> m ()
executeSequential initial (Sequential xs) =
  withFrozenCallStack $ evalM $
    foldM_ executeUpdateEnsure (initial, emptyEnvironment) xs

successful :: Test () -> Bool
successful x =
  case runTest x of
    (Left _, _) ->
      False
    (Right _, _) ->
      True

interleave :: [a] -> [a] -> [[a]]
interleave xs00 ys00 =
  case (xs00, ys00) of
    ([], []) ->
      []
    (xs, []) ->
      [xs]
    ([], ys) ->
      [ys]
    (xs0@(x:xs), ys0@(y:ys)) ->
      [ x : zs | zs <- interleave xs ys0 ] ++
      [ y : zs | zs <- interleave xs0 ys ]

checkActions :: state Concrete -> [ActionCheck state] -> Test ()
checkActions s0 = \case
  [] ->
    pure ()
  x : xs -> do
    let
      s =
        checkUpdate x s0

    checkEnsure x s0 s
    checkActions s xs

linearize :: MonadTest m => state Concrete -> [ActionCheck state] -> [ActionCheck state] -> m ()
linearize initial branch1 branch2 =
  withFrozenCallStack $
    let
      ok =
        any successful .
        fmap (checkActions initial) $
        interleave branch1 branch2
    in
      if ok then
        pure ()
      else
        failWith Nothing "no valid interleaving"


-- | Executes the prefix actions sequentially, then executes the two branches
--   in parallel, verifying that no exceptions are thrown and that there is at
--   least one sequential interleaving where all the post-conditions are met.
--
--   To generate parallel actions to execute, see the 'Hedgehog.Gen.parallel'
--   combinator in the "Hedgehog.Gen" module.
--
executeParallel ::
     (MonadTest m, MonadCatch m, MonadBaseControl IO m, HasCallStack)
  => (forall v. state v)
  -> Parallel m state
  -> m ()
executeParallel initial p@(Parallel prefix branch1 branch2) =
  withFrozenCallStack $ evalM $ do
    (s0, env0) <- foldM executeUpdateEnsure (initial, emptyEnvironment) prefix

    ((xs, env1), (ys, env2)) <-
      Async.concurrently
        (runStateT (traverse execute branch1) env0)
        (runStateT (traverse execute branch2) env0)

    let
      env = unionsEnvironment [env0, env1, env2]

    annotate $ renderParallel (renderActionResult env) p
    linearize s0 xs ys
