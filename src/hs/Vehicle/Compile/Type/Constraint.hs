{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Constraint where

import Prelude hiding (pi)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.MetaSet
import Vehicle.Compile.Simplify

--------------------------------------------------------------------------------
-- Unification definitions

data BaseConstraint
  -- | Represents that the two contained expressions should be equal.
  = Unify UnificationPair
  -- | Represents that the provided type must have the required functionality
  | Meta `Has` CheckedExpr
  deriving Show

instance Simplify BaseConstraint where
  simplifyReader (Unify (e1, e2)) = do
    e1' <- simplifyReader e1
    e2' <- simplifyReader e2
    return $ Unify (e1', e2')

  simplifyReader (m `Has` e) = do
    e' <- simplifyReader e
    return $ m `Has` e'

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

type BlockingMetas = MetaSet


{-
instance PrettyDescopedLang BaseConstraint where
  prettyDescopedLang target ctx (Unify (e1, e2)) = prettyDescopedLang target ctx e1 <+> "~" <+> prettyDescopedLang target ctx e2
  prettyDescopedLang target ctx (m `Has` e)      = pretty m <+> "~" <+> prettyDescopedLang target ctx e
-}
data ConstraintContext = ConstraintContext
  { _prov            :: Provenance       -- The origin of the constraint
  , blockingMetas    :: BlockingMetas    -- The set of metas blocking progress on this constraint, if known
  , varContext       :: VariableCtx      -- The current declaration context (needed for normalisation)
  }

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext p _ _) = p

data Constraint = Constraint ConstraintContext BaseConstraint

instance Show Constraint where
  show (Constraint _ c) = show c

variableContext :: Constraint -> VariableCtx
variableContext (Constraint ctx _) = varContext ctx

declContext :: ConstraintContext -> DeclCtx
declContext = declCtx . varContext

boundContext :: Constraint -> BoundCtx
boundContext = boundCtx . variableContext

instance HasProvenance Constraint where
  provenanceOf (Constraint ctx _) = provenanceOf ctx

instance Simplify Constraint where
  simplifyReader (Constraint ctx c) = Constraint ctx <$> simplifyReader c