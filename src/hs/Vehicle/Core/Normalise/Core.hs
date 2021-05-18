{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Normalise.Core
  ( NormError (..)
  , MonadNorm
  , NormExpr
  , NormType
  , NormDecl
  , NormProg
  , pattern EOp0
  , pattern EOp1
  , pattern EOp2
  , pattern EOp3
  , mkBool
  ) where

import Control.Monad.Trans.Except ()
import Vehicle.Core.AST
import Vehicle.Core.Abs (ExprName)

-- |Errors thrown during normalisation
data NormError
  = MissingDefFunType ExprName -- TODO: should be a type error?
  | MalformedLambdaError       -- TODO: should be a type error?
  | EmptyQuantifierDomain Provenance

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- |Some useful type synonyms
type NormExpr ann = Expr DeBruijn Builtin ann
type NormType ann = Type DeBruijn Builtin ann
type NormDecl ann = Decl DeBruijn Builtin ann
type NormProg ann = Prog DeBruijn Builtin ann

-- TODO: migrate to module with builtins

-- |Pattern synonyms to help matching during normalisation. Perhaps these are useful elsewhere and should be lifted?
pattern EOp0 op ann0 = ECon ann0 op
pattern EOp1 op e1 ann0 ann1 = EApp ann1 (EOp0 op ann0) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2 = EApp ann2 (EOp1 op e1 ann0 ann1) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3 = EApp ann3 (EOp2 op e1 e2 ann0 ann1 ann2) e3

-- TODO: migrate to different module?

mkBool :: Bool -> ann 'EXPR -> NormExpr ann
mkBool True  ann = EOp0 ETrue  ann
mkBool False ann = EOp0 EFalse ann
