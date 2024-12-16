{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Core where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map (findWithDefault, lookup)
import Data.Hashable (Hashable)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Meta variable substitution

type MetaSubstitution builtin = MetaMap (GluedExpr builtin)

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

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

type ConstraintID = Int

data ConstraintContext builtin = ConstraintContext
  { -- | The id for the constraint, used primarily for logging purposes.
    constraintID :: ConstraintID,
    -- | The original provenance of the constraint
    originalProvenance :: Provenance,
    -- | Where the constraint was instantiated
    creationProvenance :: Provenance,
    -- | The set of metas blocking progress on this constraint.
    -- If |Nothing| then the set is unknown.
    blockedBy :: BlockingStatus,
    -- | The set of bound variables in scope at the point the constraint was generated.
    boundContext :: BoundCtx (Type builtin)
  }
  deriving (Show)

instance Pretty (ConstraintContext builtin) where
  pretty ctx = pretty (blockedBy ctx)

-- <+> "<boundCtx=" <> pretty (length (boundContext ctx)) <> ">"

instance HasProvenance (ConstraintContext builtin) where
  provenanceOf (ConstraintContext _ _ creationProvenance _ _) = creationProvenance

instance HasBoundCtx (ConstraintContext builtin) (Type builtin) where
  boundContextOf = boundContext

blockCtxOn :: MetaSet -> ConstraintContext builtin -> ConstraintContext builtin
blockCtxOn metas (ConstraintContext cid originProv creationProv _ ctx) =
  let status = BlockingStatus (Just metas)
   in ConstraintContext cid originProv creationProv status ctx

updateConstraintBoundCtx ::
  ConstraintContext builtin ->
  (BoundCtx (Type builtin) -> BoundCtx (Type builtin)) ->
  ConstraintContext builtin
updateConstraintBoundCtx ConstraintContext {..} updateFn =
  ConstraintContext {boundContext = updateFn boundContext, ..}

setConstraintBoundCtx ::
  ConstraintContext builtin ->
  BoundCtx (Type builtin) ->
  ConstraintContext builtin
setConstraintBoundCtx ctx v = updateConstraintBoundCtx ctx (const v)

contextDBLevel :: ConstraintContext builtin -> Lv
contextDBLevel = Lv . length . boundContext

--------------------------------------------------------------------------------
-- Application constraints

-- | A constraint that says when the unchecked arguments to the function are correctly
-- augmented with the suitable missing implicits and instance arguments, then it has
-- the current expected type.
data ArgInsertionProblem builtin = ArgInsertionProblem
  { originalFun :: Expr builtin,
    originalArgs :: [Arg builtin],
    originalType :: Type builtin,
    checkedArgs :: [Arg builtin],
    currentExpectedType :: Type builtin,
    uncheckedArgs :: [Arg builtin]
  }
  deriving (Show)

data ApplicationConstraint builtin = InferArgs
  { typeSolutionMeta :: MetaID,
    exprSolutionMeta :: MetaID,
    argInsertionProblem :: ArgInsertionProblem builtin
  }
  deriving (Show)

solutionSoFar :: ArgInsertionProblem builtin -> Expr builtin
solutionSoFar ArgInsertionProblem {..} = normAppList originalFun (reverse checkedArgs)

type instance
  WithContext (ApplicationConstraint builtin) =
    Contextualised (ApplicationConstraint builtin) (ConstraintContext builtin)

--------------------------------------------------------------------------------
-- Instance constraints

data InstanceArgOrigin builtin = ArgOrigin
  { checkedInstanceOp :: Expr builtin,
    checkedInstanceOpArgs :: [Arg builtin],
    checkedInstanceOpType :: Type builtin,
    checkedInstanceType :: Type builtin
  }
  deriving (Show)

data InstanceTypeRestrictionOrigin builtin = TypeRestrictionOrigin
  { freeEnv :: FreeEnv builtin,
    restrictedDeclProv :: DeclProvenance,
    restrictedDeclSort :: RestrictedDecl,
    restrictedDeclType :: Type builtin
  }
  deriving (Show)

data InstanceConstraintOrigin builtin
  = InstanceArgOrigin (InstanceArgOrigin builtin)
  | InstanceTypeRestrictionOrigin (InstanceTypeRestrictionOrigin builtin)
  deriving (Show)

data InstanceConstraint builtin = Resolve
  { instanceOrigin :: InstanceConstraintOrigin builtin,
    instanceSolutionMeta :: MetaID,
    instanceRelevance :: Relevance,
    instanceGoal :: Value builtin
  }
  deriving (Show)

type instance
  WithContext (InstanceConstraint builtin) =
    Contextualised (InstanceConstraint builtin) (ConstraintContext builtin)

data InstanceCandidate builtin = InstanceCandidate
  { candidateExpr :: Expr builtin,
    candidateSolution :: Expr builtin,
    defaultInstance :: Bool
  }
  deriving (Show)

type instance
  WithContext (InstanceCandidate builtin) =
    Contextualised (InstanceCandidate builtin) (BoundCtx (Type builtin))

data InstanceGoal builtin = InstanceGoal
  { goalTelescope :: Telescope builtin,
    goalHead :: builtin,
    goalSpine :: Spine builtin
  }
  deriving (Show)

type InstanceConstraintInfo builtin =
  ( ConstraintContext builtin,
    InstanceConstraintOrigin builtin
  )

type InstanceSearchDepth = Int

-- | Stores the list of instance candidates currently in scope.
-- We use a HashMap rather than an ordinary Map as not all builtins may be
-- totally ordered (e.g. PolarityBuiltin and LinearityBuiltin)
data InstanceDatabase builtin = InstanceDatabase
  { instances :: HashMap builtin [InstanceCandidate builtin],
    defaultInstances :: HashMap builtin (InstanceCandidate builtin)
  }

emptyInstanceDatabase :: (Hashable builtin) => InstanceDatabase builtin
emptyInstanceDatabase = InstanceDatabase mempty mempty

lookupInstances :: (Hashable builtin) => InstanceDatabase builtin -> InstanceGoal builtin -> [InstanceCandidate builtin]
lookupInstances database goal = Map.findWithDefault [] (goalHead goal) (instances database)

lookupDefaultInstance :: (Hashable builtin) => InstanceDatabase builtin -> InstanceGoal builtin -> Maybe (InstanceCandidate builtin)
lookupDefaultInstance database goal = Map.lookup (goalHead goal) (defaultInstances database)

--------------------------------------------------------------------------------
-- Unification constraints

data CheckingExprType builtin = CheckingExpr
  { checkedExpr :: Expr builtin,
    checkedExprExpectedType :: Type builtin,
    checkedExprActualType :: Expr builtin
  }
  deriving (Show)

data CheckingBinderType builtin = CheckingBinder
  { checkedBinderName :: Maybe Name,
    checkedBinderExpectedType :: Type builtin,
    checkedBinderActualType :: Type builtin
  }
  deriving (Show)

data UnificationConstraintOrigin builtin
  = CheckingExprType (CheckingExprType builtin)
  | CheckingBinderType (CheckingBinderType builtin)
  | CheckingInstanceType (InstanceConstraintOrigin builtin)
  deriving (Show)

-- | A constraint representing that a pair of expressions should be equal
data UnificationConstraint builtin
  = Unify
      (UnificationConstraintOrigin builtin)
      (Value builtin)
      (Value builtin)
  deriving (Show)

type instance
  WithContext (UnificationConstraint builtin) =
    Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)

--------------------------------------------------------------------------------
-- Constraint

data Constraint builtin
  = -- | Represents that the two contained expressions should be equal.
    UnificationConstraint (UnificationConstraint builtin)
  | -- | Represents that the provided type must have the required functionality
    InstanceConstraint (InstanceConstraint builtin)
  | -- | Represents an implicit/instance application argument insertion problem
    ApplicationConstraint (ApplicationConstraint builtin)
  deriving (Show)

type instance
  WithContext (Constraint builtin) =
    Contextualised (Constraint builtin) (ConstraintContext builtin)

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
-- Restrictions on decl types
--------------------------------------------------------------------------------
-- The set of declarations that have type restrictions

data RestrictedDecl
  = RestrictedParameter ParameterSort
  | RestrictedProperty
  | RestrictedNetwork
  | RestrictedDataset
  deriving (Show)

instance Pretty RestrictedDecl where
  pretty = \case
    RestrictedParameter s -> pretty (ParameterDef s)
    RestrictedProperty {} -> "@property"
    RestrictedNetwork {} -> pretty NetworkDef
    RestrictedDataset {} -> pretty DatasetDef
