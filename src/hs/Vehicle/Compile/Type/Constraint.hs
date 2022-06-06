module Vehicle.Compile.Type.Constraint where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.MetaSet

--------------------------------------------------------------------------------
-- Unification definitions

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

newtype UnificationConstraint = Unify UnificationPair
  deriving (Show)

data TypeClassConstraint = Meta `Has` CheckedExpr
  deriving (Show)

type PolarityConstraint = CheckedExpr

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

type BlockingMetas = MetaSet

data ConstraintContext = ConstraintContext
  { _prov            :: Provenance       -- The origin of the constraint
  , blockedBy        :: BlockingMetas    -- The set of metas blocking progress on this constraint, if known
  , varContext       :: VariableCtx      -- The current declaration context (needed for normalisation)
  }

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext p _ _) = p

data Constraint
  -- | Represents that the two contained expressions should be equal.
  = UC ConstraintContext UnificationConstraint
  -- | Represents that the provided type must have the required functionality
  | TC ConstraintContext TypeClassConstraint
  -- | Represents a constraint on boolean polarities
  | PC ConstraintContext PolarityConstraint

instance Show Constraint where
  show (UC _ c) = show c
  show (TC _ c) = show c
  show (PC _ c) = show c

constraintContext :: Constraint -> ConstraintContext
constraintContext (UC ctx _) = ctx
constraintContext (TC ctx _) = ctx
constraintContext (PC ctx _) = ctx

variableContext :: Constraint -> VariableCtx
variableContext = varContext . constraintContext

declContext :: Constraint -> DeclCtx
declContext = declCtx . variableContext

boundContext :: Constraint -> BoundCtx
boundContext = boundCtx . variableContext

instance HasProvenance Constraint where
  provenanceOf = provenanceOf . constraintContext

isUnificationConstraint :: Constraint -> Bool
isUnificationConstraint UC{} = True
isUnificationConstraint _    = False

isAuxiliaryConstraint :: Constraint -> Bool
isAuxiliaryConstraint PC{} = True
isAuxiliaryConstraint _    = False

getTypeClassConstraint :: Constraint
                       -> Maybe (TypeClassConstraint, ConstraintContext)
getTypeClassConstraint (TC ctx c) = Just (c, ctx)
getTypeClassConstraint _          = Nothing

getAuxiliaryConstraint :: Constraint
                       -> Maybe (CheckedExpr, ConstraintContext)
getAuxiliaryConstraint (PC ctx c) = Just (c, ctx)
getAuxiliaryConstraint _          = Nothing