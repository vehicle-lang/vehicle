module Vehicle.Compile.Type.Constraint where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.MetaSet
import Vehicle.Compile.Type.VariableContext

--------------------------------------------------------------------------------
-- Constraint contexts

data ConstraintContext = ConstraintContext
  { _prov            :: Provenance        -- The origin of the constraint
  , blockedBy        :: BlockingMetas     -- The set of metas blocking progress on this constraint, if known
  , varContext       :: TypingVariableCtx -- The current declaration context (needed for normalisation)
  } deriving (Show)

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext p _ _) = p

instance HasBoundCtx ConstraintContext where
  boundContextOf = boundContextOf . varContext

--------------------------------------------------------------------------------
-- Unification constraints

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

newtype UnificationConstraint = Unify UnificationPair
  deriving (Show)

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint = Meta `Has` CheckedExpr
  deriving (Show)

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

type BlockingMetas = MetaSet

--------------------------------------------------------------------------------
-- Constraint

data Constraint
  -- | Represents that the two contained expressions should be equal.
  = UC ConstraintContext UnificationConstraint
  -- | Represents that the provided type must have the required functionality
  | TC ConstraintContext TypeClassConstraint

instance Show Constraint where
  show (UC _ c) = show c
  show (TC _ c) = show c

constraintContext :: Constraint -> ConstraintContext
constraintContext (UC ctx _) = ctx
constraintContext (TC ctx _) = ctx

variableContext :: Constraint -> TypingVariableCtx
variableContext = varContext . constraintContext

declContext :: Constraint -> TypingDeclCtx
declContext = declCtx . variableContext

boundContext :: Constraint -> TypingBoundCtx
boundContext = boundCtx . variableContext

instance HasBoundCtx Constraint where
  boundContextOf = boundContextOf . variableContext

instance HasProvenance Constraint where
  provenanceOf = provenanceOf . constraintContext

isTypeClassConstraint :: Constraint -> Bool
isTypeClassConstraint TC{} = True
isTypeClassConstraint _    = False

isAuxiliaryTypeClassConstraint :: Constraint -> Bool
isAuxiliaryTypeClassConstraint = \case
  TC _ (_ `Has` (BuiltinTypeClass _ tc _)) -> isAuxiliaryTypeClass tc
  _                                        -> False

isNonAuxiliaryTypeClassConstraint :: Constraint -> Bool
isNonAuxiliaryTypeClassConstraint = \case
  TC _ (_ `Has` (BuiltinTypeClass _ tc _)) -> not (isAuxiliaryTypeClass tc)
  _                                        -> False

isUnificationConstraint :: Constraint -> Bool
isUnificationConstraint UC{} = True
isUnificationConstraint _    = False

getTypeClassConstraint :: Constraint
                       -> Maybe (TypeClassConstraint, ConstraintContext)
getTypeClassConstraint (TC ctx c) = Just (c, ctx)
getTypeClassConstraint _          = Nothing