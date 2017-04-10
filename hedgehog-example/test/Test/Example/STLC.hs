{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.STLC where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

------------------------------------------------------------------------
-- A simply-typed lambda calculus with ints, bools, and strings

data Type =
    TBool
  | TInt
  | TString
  | TArrow Type Type
    deriving (Eq, Ord, Show)

data Expr =
    EBool Bool
  | EInt Int
  | EString Text
  | EVar Text
  | ELam Text Type Expr
  | EApp Expr Expr
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

-- | Evaluate to weak head normal form.
eval :: Expr -> Expr
eval expr =
  case expr of
    EBool _ ->
      expr
    EInt _ ->
      expr
    EString _ ->
      expr
    EVar _ ->
      expr
    ELam _ _ _ ->
      expr
    EApp f g ->
      case eval f of
        ELam x _t e ->
          eval (subst x g e)
        h ->
          EApp h g

subst :: Text -> Expr -> Expr -> Expr
subst x y expr =
  case expr of
    EBool _ ->
      expr
    EInt _ ->
      expr
    EString _ ->
      expr
    EVar z ->
      if x == z then
        y
      else
        expr
    ELam n t g ->
      if n == x then
        ELam n t g
      else
        ELam n t (subst x y g)
    EApp f g ->
      EApp (subst x y f) (subst x y g)

-- | Collect all the free variables in an 'Expr'.
free :: Expr -> Set Text
free =
  free' mempty mempty

free' :: Set Text -> Set Text -> Expr -> Set Text
free' binds frees expr =
  case expr of
    EBool _ ->
      frees
    EInt _ ->
      frees
    EString _ ->
      frees
    EVar x ->
      if Set.member x binds then
        frees
      else
        Set.insert x frees
    ELam x _t y ->
      free' (Set.insert x binds) frees y
    EApp f g ->
      free' binds frees f <> free' binds frees g

------------------------------------------------------------------------

data TypeError =
    Mismatch Type Type
  | FreeVariable Text
  | ExpectedArrow Type
  deriving (Eq, Ord, Show)

-- | Typecheck some expression.
typecheck :: Expr -> Either TypeError Type
typecheck =
  typecheck' mempty

typecheck' :: Map Text Type -> Expr -> Either TypeError Type
typecheck' env expr =
  case expr of
    EBool _ ->
      pure TBool

    EInt _ ->
      pure TInt

    EString _ ->
      pure TString

    EVar x ->
      maybe (Left (FreeVariable x)) pure (Map.lookup x env)

    ELam x t y ->
      TArrow t <$> typecheck' (Map.insert x t env) y

    EApp f g -> do
      tf <- typecheck' env f
      tg <- typecheck' env g
      case tf of
        TArrow ta tb ->
          if ta == tg then
            pure tb
--          pure ta {- uncomment for bugs -}
          else
            Left (Mismatch ta tg)
        _ ->
          Left (ExpectedArrow tf)

------------------------------------------------------------------------

genType :: Monad m => Gen m Type
genType =
  Gen.recursive Gen.choice [
      pure TBool
    , pure TInt
    , pure TString
    ] [
      TArrow <$> genType <*> genType
    ]

------------------------------------------------------------------------

genWellTypedExpr :: Monad m => Type -> Gen m Expr
genWellTypedExpr want =
  hoist (generalize . flip runReaderT mempty) (genWellTypedExpr' want)

genWellTypedExpr' :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedExpr' want =
  Gen.shrink shrinkExpr $
  Gen.recursive Gen.choice [
      genWellTypedExpr'' want
    ] [
      genWellTypedPath want <|> genWellTypedApp want
    , genWellTypedApp want
    ]

shrinkExpr :: Expr -> [Expr]
shrinkExpr expr =
  case expr of
    EApp f g ->
      case eval f of
        ELam x _ e ->
          [eval (subst x g e)]
        _ ->
          []
    _ ->
      []

genWellTypedExpr'' :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedExpr'' want =
  case want of
    TBool ->
      EBool <$> Gen.element [True, False]
    TInt ->
      EInt <$> Gen.int (Range.linear 0 10000)
    TString ->
      EString <$> Gen.text (Range.linear 0 25) Gen.lower
    TArrow t1 t2 -> do
      x <- Gen.text (Range.linear 1 25) Gen.lower
      ELam x t1 <$> local (insertVar x t1) (genWellTypedExpr' t2)

insertVar :: Text -> Type -> Map Type [Expr] -> Map Type [Expr]
insertVar n typ =
  Map.insertWith (<>) typ [EVar n] .
  fmap (filter (/= EVar n))

genWellTypedApp :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedApp want = do
  tg <- genKnownTypeMaybe
  eg <- genWellTypedExpr' tg
  let tf = TArrow tg want
  ef <- genWellTypedExpr' tf
  pure (EApp ef eg)

-- | This tries to look up a known expression of the desired type from the env.
-- It does not always succeed, throwing `empty` when unavailable.
genWellTypedPath :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedPath want = do
  paths <- ask
  case fromMaybe [] (Map.lookup want paths) of
    [] ->
      empty
    es ->
      Gen.element es

genKnownTypeMaybe :: Gen (Reader (Map Type [Expr])) Type
genKnownTypeMaybe = do
  known <- ask
  if Map.null known then
    genType
  else
    Gen.frequency [
        (2, Gen.element $ Map.keys known)
      , (1, genType)
      ]

------------------------------------------------------------------------

-- Generates a term that is ill-typed at some point.
genIllTypedExpr :: Monad m => Gen m Expr
genIllTypedExpr = do
  be <- genIllTypedApp
  Gen.recursive Gen.choice [
      -- Don't grow - just dish up the broken expr
      pure be
    ] [
      -- Grow a reasonable app expression around the error
      do tg <- genType
         tf <- genType
         let ta = TArrow tg tf
         ea <- genWellTypedExpr ta
         pure (EApp ea be)
    ]

-- Generates a term that is ill-typed at the very top.
genIllTypedApp :: Monad m => Gen m Expr
genIllTypedApp = do
  t1 <- genType
  t2 <- genType
  t3 <- genType
  guard (t1 /= t2)
  f <- genWellTypedExpr t3
  g <- genWellTypedExpr t2
  x <- Gen.text (Range.linear 1 25) Gen.lower
  pure $ EApp (ELam x t1 f) g

------------------------------------------------------------------------

prop_welltyped :: Property
prop_welltyped =
  property $ do
    ty <- forAll genType
    ex <- forAll (genWellTypedExpr ty)
    typecheck ex === pure ty

prop_illtyped :: Property
prop_illtyped =
  property $ do
    ex <- forAll genIllTypedExpr
    _t <- liftEither (typecheck ex)
    success

prop_consistent :: Property
prop_consistent =
  property $ do
    ty <- forAll genType
    ex <- forAll (genWellTypedExpr ty)
    typecheck (eval ex) === pure ty

prop_idempotent :: Property
prop_idempotent =
  property $ do
    ty <- forAll genType
    ex <- forAll (genWellTypedExpr ty)
    eval (eval ex) === eval ex

------------------------------------------------------------------------
-- These are just for testing the concurrent test runner

prop_idempotent1 :: Property
prop_idempotent1 =
  prop_idempotent

prop_idempotent2 :: Property
prop_idempotent2 =
  prop_idempotent

prop_idempotent3 :: Property
prop_idempotent3 =
  prop_idempotent

prop_idempotent4 :: Property
prop_idempotent4 =
  prop_idempotent

prop_idempotent5 :: Property
prop_idempotent5 =
  prop_idempotent

prop_idempotent6 :: Property
prop_idempotent6 =
  prop_idempotent

------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkConcurrent $$(discover)
