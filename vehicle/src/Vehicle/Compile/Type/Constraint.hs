module Vehicle.Compile.Type.Constraint
  ( ConstraintGroup (..),
    typeClassGroup,
    isAuxiliaryTypeClass,
    ConstraintOrigin (..),
    ConstraintContext (..),
    UnificationConstraint (..),
    TypeClassConstraint (..),
    tcNormExpr,
    InstanceGoal (..),
    goalExpr,
    InstanceCandidate (..),
    Constraint (..),
    extendConstraintBoundCtx,
    separateConstraints,
    getTypeClassConstraint,
    isAuxiliaryTypeClassConstraint,
    BlockingStatus,
    unknownBlockingStatus,
    blockConstraintOn,
    isBlocked,
    constraintIsBlocked,
    copyContext,
    contextDBLevel,
    ConstraintProgress (..),
  )
where

import Data.Bifunctor (Bifunctor (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.VariableContext
import Vehicle.Expr.DeBruijn (DBLevel (..))
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Constraint types

data ConstraintGroup
  = TypeGroup
  | PolarityGroup
  | LinearityGroup
  deriving (Show, Eq)

typeClassGroup :: TypeClass -> ConstraintGroup
typeClassGroup tc = case tc of
  HasEq {} -> TypeGroup
  HasOrd {} -> TypeGroup
  HasNot -> TypeGroup
  HasAnd -> TypeGroup
  HasOr -> TypeGroup
  HasImplies -> TypeGroup
  HasQuantifier {} -> TypeGroup
  HasAdd -> TypeGroup
  HasSub -> TypeGroup
  HasMul -> TypeGroup
  HasDiv -> TypeGroup
  HasNeg -> TypeGroup
  HasMap -> TypeGroup
  HasFold -> TypeGroup
  HasQuantifierIn {} -> TypeGroup
  HasNatLits {} -> TypeGroup
  HasRatLits -> TypeGroup
  HasVecLits {} -> TypeGroup
  HasIf {} -> TypeGroup
  AlmostEqualConstraint {} -> TypeGroup
  NatInDomainConstraint {} -> TypeGroup
  LinearityTypeClass {} -> LinearityGroup
  PolarityTypeClass {} -> PolarityGroup

isAuxiliaryTypeClass :: TypeClass -> Bool
isAuxiliaryTypeClass tc = do
  let group = typeClassGroup tc
  group == PolarityGroup || group == LinearityGroup

--------------------------------------------------------------------------------
-- Constraint origins

data ConstraintOrigin
  = CheckingExprType CheckedExpr CheckedType CheckedType
  | CheckingBinderType (Maybe Name) CheckedType CheckedType
  | CheckingTypeClass CheckedExpr [UncheckedArg]
  | CheckingAuxiliary
  deriving (Show)

--------------------------------------------------------------------------------
-- Blocking status

-- | Denotes whether a constraint is blocked and if so what metas it is blocked
-- on.
newtype BlockingStatus = BlockingStatus (Maybe MetaSet)
  deriving (Show)

instance Pretty BlockingStatus where
  pretty (BlockingStatus status) = case status of
    Nothing -> ""
    Just v -> "blockedBy:" <+> pretty v

unknownBlockingStatus :: BlockingStatus
unknownBlockingStatus = BlockingStatus Nothing

isStillBlocked :: MetaSet -> BlockingStatus -> Bool
isStillBlocked solvedMetas (BlockingStatus status) =
  -- If unknown then not blocked, otherwise blocked if none of the blocking
  -- metas are solved.
  maybe False (MetaSet.disjoint solvedMetas) status

--------------------------------------------------------------------------------
-- Constraint contexts

data ConstraintContext = ConstraintContext
  { -- | The original provenance of the constraint
    originalProvenance :: Provenance,
    -- | The origin of the constraint.
    origin :: ConstraintOrigin,
    -- | Where the constraint was instantiated
    creationProvenance :: Provenance,
    -- | The set of metas blocking progress on this constraint.
    -- If |Nothing| then the set is unknown.
    blockedBy :: BlockingStatus,
    -- | TODO reduce this to just `TypingBoundCtx`
    -- (At the moment the full context is needed for normalisation but should
    -- be able to get that from TCM)
    boundContext :: TypingBoundCtx,
    group :: ConstraintGroup
  }
  deriving (Show)

instance Pretty ConstraintContext where
  pretty ctx = pretty (blockedBy ctx)

-- <+> "<boundCtx=" <> pretty (length (boundContext ctx)) <> ">"

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext _ _ creationProvenance _ _ _) = creationProvenance

instance HasBoundCtx ConstraintContext where
  boundContextOf = boundContextOf . boundContext

blockCtxOn :: MetaSet -> ConstraintContext -> ConstraintContext
blockCtxOn metas (ConstraintContext originProv originalConstraint creationProv _ ctx group) =
  let status = BlockingStatus (Just metas)
   in ConstraintContext originProv originalConstraint creationProv status ctx group

-- | Create a new fresh copy of the context for a new constraint
copyContext :: ConstraintContext -> ConstraintContext
copyContext (ConstraintContext originProv originalConstraint creationProv _ ctx group) =
  ConstraintContext originProv originalConstraint creationProv unknownBlockingStatus ctx group

extendConstraintBoundCtx :: ConstraintContext -> CheckedTelescope -> ConstraintContext
extendConstraintBoundCtx ConstraintContext {..} telescope =
  ConstraintContext
    { boundContext = fmap mkTypingBoundCtxEntry telescope ++ boundContext,
      ..
    }

contextDBLevel :: ConstraintContext -> DBLevel
contextDBLevel = DBLevel . length . boundContext

--------------------------------------------------------------------------------
-- Unification constraints

-- | A constraint representing that a pair of expressions should be equal
data UnificationConstraint = Unify NormExpr NormExpr
  deriving (Show)

type instance
  WithContext UnificationConstraint =
    Contextualised UnificationConstraint ConstraintContext

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint = Has MetaID TypeClass Spine
  deriving (Show)

tcNormExpr :: TypeClassConstraint -> NormExpr
tcNormExpr (Has _ tc spine) = VConstructor (TypeClass tc) spine

type instance
  WithContext TypeClassConstraint =
    Contextualised TypeClassConstraint ConstraintContext

--------------------------------------------------------------------------------
-- Instance constraints

data InstanceGoal = InstanceGoal
  { goalTelescope :: CheckedTelescope,
    goalHead :: TypeClass,
    goalSpine :: Spine
  }
  deriving (Show)

goalExpr :: InstanceGoal -> NormExpr
goalExpr InstanceGoal {..} = VConstructor (TypeClass goalHead) goalSpine

data InstanceCandidate = InstanceCandidate
  { candidateContext :: TypingBoundCtx,
    candidateExpr :: CheckedExpr,
    candidateSolution :: CheckedExpr
  }

--------------------------------------------------------------------------------
-- Constraint

data Constraint
  = -- | Represents that the two contained expressions should be equal.
    UnificationConstraint UnificationConstraint
  | -- | Represents that the provided type must have the required functionality
    TypeClassConstraint TypeClassConstraint
  deriving (Show)

type instance
  WithContext Constraint =
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
  _ -> Nothing

isAuxiliaryTypeClassConstraint :: TypeClassConstraint -> Bool
isAuxiliaryTypeClassConstraint (Has _ tc _) = isAuxiliaryTypeClass tc

separateConstraints :: [WithContext Constraint] -> ([WithContext UnificationConstraint], [WithContext TypeClassConstraint])
separateConstraints [] = ([], [])
separateConstraints (WithContext c ctx : cs) = case c of
  UnificationConstraint uc -> first (WithContext uc ctx :) (separateConstraints cs)
  TypeClassConstraint tc -> second (WithContext tc ctx :) (separateConstraints cs)

{-
isNonAuxiliaryTypeClassConstraint :: Constraint -> Bool
isNonAuxiliaryTypeClassConstraint = \case
  TC _ (Has _ tc _) -> not (isAuxiliaryTypeClass tc)
  _                 -> False

isUnificationConstraint :: Constraint -> Bool
isUnificationConstraint UC{} = True
isUnificationConstraint _    = False
-}

blockConstraintOn ::
  Contextualised c ConstraintContext ->
  MetaSet ->
  Contextualised c ConstraintContext
blockConstraintOn (WithContext c ctx) metas = WithContext c (blockCtxOn metas ctx)

isBlocked :: MetaSet -> ConstraintContext -> Bool
isBlocked solvedMetas ctx = isStillBlocked solvedMetas (blockedBy ctx)

constraintIsBlocked :: MetaSet -> Contextualised c ConstraintContext -> Bool
constraintIsBlocked solvedMetas c = isBlocked solvedMetas (contextOf c)

--------------------------------------------------------------------------------
-- Progress in solving meta-variable constraints

data ConstraintProgress
  = Stuck MetaSet
  | Progress [WithContext Constraint]
  deriving (Show)

instance Semigroup ConstraintProgress where
  Stuck m1 <> Stuck m2 = Stuck (m1 <> m2)
  Stuck {} <> x@Progress {} = x
  x@Progress {} <> Stuck {} = x
  Progress r1 <> Progress r2 = Progress (r1 <> r2)
