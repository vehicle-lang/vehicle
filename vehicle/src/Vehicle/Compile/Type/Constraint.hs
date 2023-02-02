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
import Vehicle.Expr.DeBruijn (DBArg, DBExpr, DBLevel (..), DBTelescope, DBType)
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

data ConstraintOrigin builtin
  = CheckingExprType (DBExpr builtin) (DBType builtin) (DBType builtin)
  | CheckingBinderType (Maybe Name) (DBType builtin) (DBType builtin)
  | CheckingTypeClass (DBExpr builtin) [DBArg builtin]
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

data ConstraintContext builtin = ConstraintContext
  { -- | The original provenance of the constraint
    originalProvenance :: Provenance,
    -- | The origin of the constraint.
    origin :: ConstraintOrigin builtin,
    -- | Where the constraint was instantiated
    creationProvenance :: Provenance,
    -- | The set of metas blocking progress on this constraint.
    -- If |Nothing| then the set is unknown.
    blockedBy :: BlockingStatus,
    -- | TODO reduce this to just `TypingBoundCtx`
    -- (At the moment the full context is needed for normalisation but should
    -- be able to get that from TCM)
    boundContext :: TypingBoundCtx builtin
  }
  deriving (Show)

instance Pretty (ConstraintContext builtin) where
  pretty ctx = pretty (blockedBy ctx)

-- <+> "<boundCtx=" <> pretty (length (boundContext ctx)) <> ">"

instance HasProvenance (ConstraintContext builtin) where
  provenanceOf (ConstraintContext _ _ creationProvenance _ _) = creationProvenance

instance HasBoundCtx (ConstraintContext builtin) where
  boundContextOf = boundContextOf . boundContext

blockCtxOn :: MetaSet -> ConstraintContext builtin -> ConstraintContext builtin
blockCtxOn metas (ConstraintContext originProv originalConstraint creationProv _ ctx) =
  let status = BlockingStatus (Just metas)
   in ConstraintContext originProv originalConstraint creationProv status ctx

-- | Create a new fresh copy of the context for a new constraint
copyContext :: ConstraintContext builtin -> ConstraintContext builtin
copyContext (ConstraintContext originProv originalConstraint creationProv _ ctx) =
  ConstraintContext originProv originalConstraint creationProv unknownBlockingStatus ctx

extendConstraintBoundCtx :: ConstraintContext builtin -> DBTelescope builtin -> ConstraintContext builtin
extendConstraintBoundCtx ConstraintContext {..} telescope =
  ConstraintContext
    { boundContext = fmap mkTypingBoundCtxEntry telescope ++ boundContext,
      ..
    }

contextDBLevel :: ConstraintContext builtin -> DBLevel
contextDBLevel = DBLevel . length . boundContext

--------------------------------------------------------------------------------
-- Unification constraints

-- | A constraint representing that a pair of expressions should be equal
data UnificationConstraint builtin = Unify (NormExpr builtin) (NormExpr builtin)
  deriving (Show)

type instance
  WithContext (UnificationConstraint builtin) =
    Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint builtin = Has MetaID builtin (Spine builtin)
  deriving (Show)

tcNormExpr :: TypeClassConstraint builtin -> NormExpr builtin
tcNormExpr (Has _ tc spine) = VBuiltin tc spine

type instance
  WithContext (TypeClassConstraint builtin) =
    Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin)

--------------------------------------------------------------------------------
-- Instance constraints

data InstanceGoal = InstanceGoal
  { goalTelescope :: DBTelescope Builtin,
    goalHead :: TypeClass,
    goalSpine :: Spine Builtin
  }
  deriving (Show)

goalExpr :: InstanceGoal -> NormExpr Builtin
goalExpr InstanceGoal {..} = VBuiltin (Constructor (TypeClass goalHead)) goalSpine

data InstanceCandidate = InstanceCandidate
  { candidateContext :: TypingBoundCtx Builtin,
    candidateExpr :: DBExpr Builtin,
    candidateSolution :: DBExpr Builtin
  }

--------------------------------------------------------------------------------
-- Constraint

data Constraint builtin
  = -- | Represents that the two contained expressions should be equal.
    UnificationConstraint (UnificationConstraint builtin)
  | -- | Represents that the provided type must have the required functionality
    TypeClassConstraint (TypeClassConstraint builtin)
  deriving (Show)

type instance
  WithContext (Constraint builtin) =
    Contextualised (Constraint builtin) (ConstraintContext builtin)

{-
instance HasBoundCtx Constraint where
  boundContextOf = boundContextOf . constraintContext

instance HasProvenance Constraint where
  provenanceOf = provenanceOf . constraintContext
-}
getTypeClassConstraint :: WithContext (Constraint builtin) -> Maybe (WithContext (TypeClassConstraint builtin))
getTypeClassConstraint (WithContext constraint ctx) = case constraint of
  TypeClassConstraint tc -> Just (WithContext tc ctx)
  _ -> Nothing

separateConstraints :: [WithContext (Constraint builtin)] -> ([WithContext (UnificationConstraint builtin)], [WithContext (TypeClassConstraint builtin)])
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
  Contextualised c (ConstraintContext builtin) ->
  MetaSet ->
  Contextualised c (ConstraintContext builtin)
blockConstraintOn (WithContext c ctx) metas = WithContext c (blockCtxOn metas ctx)

isBlocked :: MetaSet -> ConstraintContext builtin -> Bool
isBlocked solvedMetas ctx = isStillBlocked solvedMetas (blockedBy ctx)

constraintIsBlocked :: MetaSet -> Contextualised c (ConstraintContext builtin) -> Bool
constraintIsBlocked solvedMetas c = isBlocked solvedMetas (contextOf c)

--------------------------------------------------------------------------------
-- Progress in solving meta-variable constraints

data ConstraintProgress builtin
  = Stuck MetaSet
  | Progress [WithContext (Constraint builtin)]
  deriving (Show)

instance Semigroup (ConstraintProgress builtin) where
  Stuck m1 <> Stuck m2 = Stuck (m1 <> m2)
  Stuck {} <> x@Progress {} = x
  x@Progress {} <> Stuck {} = x
  Progress r1 <> Progress r2 = Progress (r1 <> r2)
