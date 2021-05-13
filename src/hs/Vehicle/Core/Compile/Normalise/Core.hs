{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module Vehicle.Core.Compile.Normalise.Core
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

import Vehicle.Core.AST.Builtin (BuiltinOp(..), Builtin(..))
import Vehicle.Prelude (Position)
import Vehicle.Core.AST ( Sort(..), Tree (..), Expr, Type, Decl, Prog)
import Vehicle.Core.AST.DeBruijn (DeBruijn(..))
import Control.Monad.Except (MonadError)
import Vehicle.Core.Abs (ExprName)

-- |Errors thrown during normalisation
data NormError
  = MissingDefFunType ExprName -- TODO: should be a type error?
  | MalformedLambdaError       -- TODO: should be a type error?
  | EmptyQuantifierDomain Position

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- |Some useful type synonyms
type NormExpr ann = Expr DeBruijn Builtin ann
type NormType ann = Type DeBruijn Builtin ann
type NormDecl ann = Decl DeBruijn Builtin ann
type NormProg ann = Prog DeBruijn Builtin ann

-- TODO: migrate to module with builtins

-- |Pattern synonyms to help matching during normalisation. Perhaps these are useful elsewhere and should be lifted?
pattern EOp0 op ann0 pos = ECon ann0 (Builtin pos op)
pattern EOp1 op e1 ann0 ann1 pos = EApp ann1 (EOp0 op ann0 pos) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2 pos = EApp ann2 (EOp1 op e1 ann0 ann1 pos) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3 pos  = EApp ann3 (EOp2 op e1 e2 ann0 ann1 ann2 pos) e3

-- TODO: migrate to different module?

mkBool :: Bool -> ann 'EXPR -> Position -> NormExpr ann
mkBool True ann pos = EOp0 ETrue ann pos
mkBool False ann pos = EOp0 EFalse ann pos
