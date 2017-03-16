{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Example.STLC where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader

import           Data.Either
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           Hedgehog
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

import           Prelude


-- -----------------------------------------------------------------------------
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

-- -----------------------------------------------------------------------------

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
      if x == z then y else expr
    ELam f t g ->
      -- so incredibly wrong
      ELam f t (subst x y g)
      -- if f == x then ELam undefined t (subst f undefined g) else ELam f t (subst x y g)
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
      if S.member x binds then frees else S.insert x frees
    ELam x _t y ->
      free' (S.insert x binds) frees y
    EApp f g ->
      free' binds frees f <> free' binds frees g

-- -----------------------------------------------------------------------------

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
      maybe (Left (FreeVariable x)) pure (M.lookup x env)

    ELam x t y ->
      TArrow t <$> typecheck' (M.insert x t env) y

    EApp f g -> do
      tf <- typecheck' env f
      tg <- typecheck' env g
      case tf of
        TArrow ta tb ->
          if ta == tg
            then pure tb
--            then pure ta
            else Left (Mismatch ta tg)
        _ ->
          Left (ExpectedArrow tf)

-- -----------------------------------------------------------------------------

genType :: Monad m => Gen m Type
genType =
  HG.recursive HG.choice [
      pure TBool
    , pure TInt
    , pure TString
    ] [
      TArrow <$> genType <*> genType
    ]

-- -----------------------------------------------------------------------------

genWellTypedExpr :: Monad m => Type -> Gen m Expr
genWellTypedExpr want =
  hoist (generalize . flip runReaderT mempty) (genWellTypedExpr' want)

genWellTypedExpr' :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedExpr' want =
  HG.recursive HG.choice [
      genWellTypedExpr'' want
    ] [
      genWellTypedPath want <|> genWellTypedApp want
    , genWellTypedApp want
    ]

genWellTypedExpr'' :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedExpr'' want =
  case want of
    TBool ->
      EBool <$> HG.element [True, False]
    TInt ->
      EInt <$> HG.int (HR.linear 0 10000)
    TString ->
      EString <$> HG.text (HR.linear 0 25) (HG.enum 'a' 'z')
    TArrow t1 t2 -> do
      x <- HG.text (HR.linear 1 25) (HG.enum 'a' 'z')
      ELam x t1 <$> local (M.insertWith (<>) t1 [EVar x]) (genWellTypedExpr' t2)

genWellTypedApp :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedApp want = do
  tg <- genType
  eg <- genWellTypedExpr' tg
  let tf = TArrow tg want
  ef <- genWellTypedExpr' tf
  pure (EApp ef eg)

-- | This tries to look up a known expression of the desired type from the env.
-- It does not always succeed, throwing `empty` when unavailable.
genWellTypedPath :: Type -> Gen (Reader (Map Type [Expr])) Expr
genWellTypedPath want = do
  paths <- lift ask
  case M.lookup want paths of
    Just es ->
      HG.element es
    Nothing ->
      empty

-- -----------------------------------------------------------------------------

genIllTypedExpr :: Monad m => Gen m Expr
genIllTypedExpr =
  genIllTypedApp

genIllTypedApp :: Monad m => Gen m Expr
genIllTypedApp = do
  t1 <- genType
  t2 <- genType
  t3 <- genType
  guard (t1 /= t2)
  f <- genWellTypedExpr t3
  g <- genWellTypedExpr t2
  x <- HG.text (HR.linear 1 25) (HG.enum 'a' 'z')
  pure $ EApp (ELam x t1 f) g

-- -----------------------------------------------------------------------------

prop_welltyped :: Monad m => Property m ()
prop_welltyped = do
  ty <- forAll genType
  ex <- forAll (genWellTypedExpr ty)
  typecheck ex === pure ty

prop_illtyped :: Monad m => Property m ()
prop_illtyped = do
  ex <- forAll genIllTypedExpr
  -- fix counterexample here
  assert $ isLeft (typecheck ex)

prop_consistent :: Monad m => Property m ()
prop_consistent = do
  ty <- forAll genType
  ex <- forAll (genWellTypedExpr ty)
  typecheck (eval ex) === pure ty

prop_idempotent :: Monad m => Property m ()
prop_idempotent = do
  ty <- forAll genType
  ex <- forAll (genWellTypedExpr ty)
  eval (eval ex) === eval ex

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  $$(checkAll)
