module Vehicle.Compile.Type.Constraint where

import Data.List.NonEmpty (NonEmpty)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.VariableContext

--------------------------------------------------------------------------------
-- Constraint types

data ConstraintGroup
  = TypeGroup
  | PolarityGroup
  | LinearityGroup
  deriving (Show, Eq)


typeClassGroup :: TypeClass -> ConstraintGroup
typeClassGroup tc = case tc of
    HasEq{}                 -> TypeGroup
    HasOrd{}                -> TypeGroup
    HasNot                  -> TypeGroup
    HasAnd                  -> TypeGroup
    HasOr                   -> TypeGroup
    HasImplies              -> TypeGroup
    HasQuantifier{}         -> TypeGroup
    HasAdd                  -> TypeGroup
    HasSub                  -> TypeGroup
    HasMul                  -> TypeGroup
    HasDiv                  -> TypeGroup
    HasNeg                  -> TypeGroup
    HasFold                 -> TypeGroup
    HasQuantifierIn{}       -> TypeGroup
    HasNatLits{}            -> TypeGroup
    HasRatLits              -> TypeGroup
    HasVecLits{}            -> TypeGroup
    AlmostEqualConstraint{} -> TypeGroup
    NatInDomainConstraint{} -> TypeGroup

    MulLinearity  -> LinearityGroup
    MaxLinearity  -> LinearityGroup

    NegPolarity{}      -> PolarityGroup
    AddPolarity{}      -> PolarityGroup
    EqPolarity{}       -> PolarityGroup
    ImpliesPolarity{}  -> PolarityGroup
    MaxPolarity{}      -> PolarityGroup

isAuxiliaryTypeClass :: TypeClass -> Bool
isAuxiliaryTypeClass tc = do
  let group = typeClassGroup tc
  group == PolarityGroup || group == LinearityGroup

--------------------------------------------------------------------------------
-- Constraint contexts

data ConstraintContext = ConstraintContext
  { _prov            :: Provenance        -- The origin of the constraint
  , blockedBy        :: BlockingMetas     -- The set of metas blocking progress on this constraint, if known
  , varContext       :: TypingVariableCtx -- The current declaration context (needed for normalisation)
  , group            :: ConstraintGroup
  } deriving (Show)

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext p _ _ _) = p

instance HasBoundCtx ConstraintContext where
  boundContextOf = boundContextOf . varContext

blockCtxOn :: ConstraintContext -> MetaSet -> ConstraintContext
blockCtxOn (ConstraintContext p _ ctx group) metas = ConstraintContext p metas ctx group

-- | Create a new fresh copy of the context for a new constraint
copyContext :: ConstraintContext -> ConstraintContext
copyContext (ConstraintContext p _ ctx group) = ConstraintContext p mempty ctx group

--------------------------------------------------------------------------------
-- Unification constraints

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

newtype UnificationConstraint = Unify UnificationPair
  deriving (Show)

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint = Has Meta TypeClass (NonEmpty CheckedArg)
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
  TC _ (Has _ tc _) -> isAuxiliaryTypeClass tc
  _                 -> False

isNonAuxiliaryTypeClassConstraint :: Constraint -> Bool
isNonAuxiliaryTypeClassConstraint = \case
  TC _ (Has _ tc _) -> not (isAuxiliaryTypeClass tc)
  _                 -> False

isUnificationConstraint :: Constraint -> Bool
isUnificationConstraint UC{} = True
isUnificationConstraint _    = False

getTypeClassConstraint :: Constraint
                       -> Maybe (TypeClassConstraint, ConstraintContext)
getTypeClassConstraint (TC ctx c) = Just (c, ctx)
getTypeClassConstraint _          = Nothing

blockConstraintOn :: Constraint -> MetaSet -> Constraint
blockConstraintOn (UC ctx c) metas = UC (blockCtxOn ctx metas) c
blockConstraintOn (TC ctx c) metas = TC (blockCtxOn ctx metas) c

isUnblockedBy :: MetaSet -> Constraint -> Bool
isUnblockedBy solvedMetas c = do
  let blockingMetas = blockedBy (constraintContext c)
  MetaSet.null blockingMetas || not (MetaSet.disjoint solvedMetas blockingMetas)