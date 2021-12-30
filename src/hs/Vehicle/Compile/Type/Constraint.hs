module Vehicle.Compile.Type.Constraint where

import Prelude hiding (pi)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.MetaSet

--------------------------------------------------------------------------------
-- Unification definitions

data BaseConstraint
  -- | Represents that the two contained expressions should be equal.
  = Unify UnificationPair
  -- | Represents that the provided type must have the required functionality
  | Meta `Has` CheckedExpr
  deriving Show


-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

type BlockingMetas = MetaSet

data ConstraintContext = ConstraintContext
  { _prov            :: Provenance       -- The origin of the constraint
  , blockingMetas    :: BlockingMetas    -- The set of metas blocking progress on this constraint, if known
  , varContext       :: VariableCtx      -- The current declaration context (needed for normalisation)
  }

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext p _ _) = p

data Constraint = Constraint ConstraintContext BaseConstraint

baseConstraint :: Constraint -> BaseConstraint
baseConstraint (Constraint _ c) = c

instance Show Constraint where
  show c = show (baseConstraint c)

variableContext :: Constraint -> VariableCtx
variableContext (Constraint ctx _) = varContext ctx

declContext :: Constraint -> DeclCtx
declContext = declCtx . variableContext

boundContext :: Constraint -> BoundCtx
boundContext = boundCtx . variableContext

instance HasProvenance Constraint where
  provenanceOf (Constraint ctx _) = provenanceOf ctx