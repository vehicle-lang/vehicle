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
    HasIf{}                 -> TypeGroup
    AlmostEqualConstraint{} -> TypeGroup
    NatInDomainConstraint{} -> TypeGroup

    LinearityTypeClass{}    -> LinearityGroup
    PolarityTypeClass{}     -> PolarityGroup

isAuxiliaryTypeClass :: TypeClass -> Bool
isAuxiliaryTypeClass tc = do
  let group = typeClassGroup tc
  group == PolarityGroup || group == LinearityGroup

--------------------------------------------------------------------------------
-- Constraint contexts

type BlockingMetas = MetaSet

data ConstraintContext = ConstraintContext
  { originalProvenance :: Provenance
  -- ^ The origin of the constraint
  , creationProvenance :: Provenance
  -- ^ Where the constraint was instantiated
  , blockedBy          :: BlockingMetas
  -- ^ The set of metas blocking progress on this constraint, if known
  , boundContext       :: TypingBoundCtx
  -- ^ TODO reduce this to just `TypingBoundCtx`
  -- (At the moment the full context is needed for normalisation but should
  -- be able to get that from TCM)
  , group              :: ConstraintGroup
  } deriving (Show)

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext _ creationProvenance _ _ _) = creationProvenance

instance HasBoundCtx ConstraintContext where
  boundContextOf = boundContextOf . boundContext

blockCtxOn :: MetaSet -> ConstraintContext -> ConstraintContext
blockCtxOn metas (ConstraintContext originProv creationProv _ ctx group) =
  ConstraintContext originProv creationProv metas ctx group

-- | Create a new fresh copy of the context for a new constraint
copyContext :: ConstraintContext -> ConstraintContext
copyContext (ConstraintContext originProv creationProv _ ctx group) =
  ConstraintContext originProv creationProv mempty ctx group

--------------------------------------------------------------------------------
-- Unification constraints

-- | A constraint representing that a pair of expressions should be equal
data UnificationConstraint = Unify CheckedExpr CheckedExpr
  deriving (Show)

type instance WithContext UnificationConstraint =
  Contextualised UnificationConstraint ConstraintContext

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint = Has MetaID TypeClass (NonEmpty CheckedArg)
  deriving (Show)

type instance WithContext TypeClassConstraint =
  Contextualised TypeClassConstraint ConstraintContext

--------------------------------------------------------------------------------
-- Constraint

data Constraint
  -- | Represents that the two contained expressions should be equal.
  = UnificationConstraint UnificationConstraint
  -- | Represents that the provided type must have the required functionality
  | TypeClassConstraint TypeClassConstraint
  deriving (Show)

type instance WithContext Constraint =
  Contextualised Constraint ConstraintContext

{-
instance HasBoundCtx Constraint where
  boundContextOf = boundContextOf . constraintContext

instance HasProvenance Constraint where
  provenanceOf = provenanceOf . constraintContext
-}
getTypeClassConstraint :: WithContext Constraint -> Maybe (WithContext TypeClassConstraint)
getTypeClassConstraint (WithContext constraint ctx) = case constraint of
  TypeClassConstraint tc -> Just (WithContext tc ctx)
  _                      -> Nothing

isAuxiliaryTypeClassConstraint :: TypeClassConstraint -> Bool
isAuxiliaryTypeClassConstraint (Has _ tc _) = isAuxiliaryTypeClass tc
{-
isNonAuxiliaryTypeClassConstraint :: Constraint -> Bool
isNonAuxiliaryTypeClassConstraint = \case
  TC _ (Has _ tc _) -> not (isAuxiliaryTypeClass tc)
  _                 -> False

isUnificationConstraint :: Constraint -> Bool
isUnificationConstraint UC{} = True
isUnificationConstraint _    = False
-}

blockConstraintOn :: WithContext Constraint
                  -> MetaSet
                  -> WithContext Constraint
blockConstraintOn (WithContext c ctx) metas = WithContext c (blockCtxOn metas ctx)

isUnblockedBy :: MetaSet -> WithContext Constraint -> Bool
isUnblockedBy solvedMetas c = do
  let blockingMetas = blockedBy (contextOf c)
  MetaSet.null blockingMetas || not (MetaSet.disjoint solvedMetas blockingMetas)
