{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Core where

import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta.Map (MetaMap (..))
import Vehicle.Compile.Type.Meta.Set (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

type Imports types = [GluedProg types]

--------------------------------------------------------------------------------

-- | Errors in bidirectional type-checking
data TypingError types
  = MissingExplicitArgument (TypingBoundCtx types) (NormalisableBinder types) (NormalisableArg types)
  | FunctionTypeMismatch (TypingBoundCtx types) (NormalisableExpr types) [NormalisableArg types] (NormalisableExpr types) [NormalisableArg types]
  | FailedUnification (NonEmpty (WithContext (UnificationConstraint types)))
  | UnsolvableConstraints (NonEmpty (WithContext (Constraint types)))

instance Pretty (TypingError types) where
  pretty = \case
    MissingExplicitArgument {} -> "MissingExplicitArgument"
    FunctionTypeMismatch {} -> "FunctionTypeMismatch"
    FailedUnification {} -> "FailedUnification"
    UnsolvableConstraints {} -> "UnsolvableConstraints"

--------------------------------------------------------------------------------
-- Typing declaration context

data TypingDeclCtxEntry types = TypingDeclCtxEntry
  { declAnns :: [Annotation],
    declType :: NormalisableType types,
    declBody :: Maybe (GluedExpr types)
  }

type TypingDeclCtx types = DeclCtx (TypingDeclCtxEntry types)

mkTypingDeclCtxEntry :: GluedDecl types -> TypingDeclCtxEntry types
mkTypingDeclCtxEntry decl =
  TypingDeclCtxEntry
    { declAnns = annotationsOf decl,
      declType = unnormalised $ typeOf decl,
      declBody = bodyOf decl
    }

addToTypingDeclCtx :: GluedDecl types -> TypingDeclCtx types -> TypingDeclCtx types
addToTypingDeclCtx decl = Map.insert (identifierOf decl) (mkTypingDeclCtxEntry decl)

--------------------------------------------------------------------------------
-- Typing declaration context

data NormDeclCtxEntry types = NormDeclCtxEntry
  { declExpr :: Value types,
    declAnns :: [Annotation]
  }

type NormDeclCtx types = DeclCtx (NormDeclCtxEntry types)

typingDeclCtxToNormDeclCtx :: TypingDeclCtx types -> NormDeclCtx types
typingDeclCtxToNormDeclCtx = Map.mapMaybe $ \TypingDeclCtxEntry {..} ->
  fmap
    ( \body ->
        NormDeclCtxEntry
          { declExpr = normalised body,
            declAnns = declAnns
          }
    )
    declBody

--------------------------------------------------------------------------------
-- Meta variable substitution

type MetaSubstitution types = MetaMap (GluedExpr types)

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry types =
  ( Maybe Name,
    NormalisableType types
  )

mkTypingBoundCtxEntry :: NormalisableBinder types -> TypingBoundCtxEntry types
mkTypingBoundCtxEntry binder = (nameOf binder, binderType binder)

type TypingBoundCtx types = BoundCtx (TypingBoundCtxEntry types)

instance HasBoundCtx (TypingBoundCtx types) where
  boundContextOf = map fst

typingBoundContextToEnv :: TypingBoundCtx types -> Env types
typingBoundContextToEnv ctx = do
  let levels = reverse (fmap Lv [0 .. length ctx - 1])
  zipWith (\level (n, _) -> (n, VBoundVar level [])) levels ctx

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Constraint origins

data ConstraintOrigin types
  = CheckingExprType (NormalisableExpr types) (NormalisableType types) (NormalisableType types)
  | CheckingBinderType (Maybe Name) (NormalisableType types) (NormalisableType types)
  | CheckingTypeClass (NormalisableExpr types) [NormalisableArg types] types [NormalisableArg types]
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

type ConstraintID = Int

data ConstraintContext types = ConstraintContext
  { -- | The id for the constraint, used primarily for logging purposes.
    constraintID :: ConstraintID,
    -- | The original provenance of the constraint
    originalProvenance :: Provenance,
    -- | The origin of the constraint.
    origin :: ConstraintOrigin types,
    -- | Where the constraint was instantiated
    creationProvenance :: Provenance,
    -- | The set of metas blocking progress on this constraint.
    -- If |Nothing| then the set is unknown.
    blockedBy :: BlockingStatus,
    -- | TODO reduce this to just `TypingBoundCtx`
    -- (At the moment the full context is needed for normalisation but should
    -- be able to get that from TCM)
    boundContext :: TypingBoundCtx types
  }
  deriving (Show)

instance Pretty (ConstraintContext types) where
  pretty ctx = pretty (blockedBy ctx)

-- <+> "<boundCtx=" <> pretty (length (boundContext ctx)) <> ">"

instance HasProvenance (ConstraintContext types) where
  provenanceOf (ConstraintContext _ _ _ creationProvenance _ _) = creationProvenance

instance HasBoundCtx (ConstraintContext types) where
  boundContextOf = boundContextOf . boundContext

blockCtxOn :: MetaSet -> ConstraintContext types -> ConstraintContext types
blockCtxOn metas (ConstraintContext cid originProv originalConstraint creationProv _ ctx) =
  let status = BlockingStatus (Just metas)
   in ConstraintContext cid originProv originalConstraint creationProv status ctx

extendConstraintBoundCtx :: ConstraintContext types -> NormalisableTelescope types -> ConstraintContext types
extendConstraintBoundCtx ConstraintContext {..} telescope =
  ConstraintContext
    { boundContext = fmap mkTypingBoundCtxEntry telescope ++ boundContext,
      ..
    }

contextDBLevel :: ConstraintContext types -> Lv
contextDBLevel = Lv . length . boundContext

--------------------------------------------------------------------------------
-- Unification constraints

-- | A constraint representing that a pair of expressions should be equal
data UnificationConstraint types = Unify (Value types) (Value types)
  deriving (Show)

type instance
  WithContext (UnificationConstraint types) =
    Contextualised (UnificationConstraint types) (ConstraintContext types)

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint types = Has MetaID types (ExplicitSpine types)
  deriving (Show)

tcNormExpr :: TypeClassConstraint types -> Value types
tcNormExpr (Has _ tc spine) = VBuiltin (CType tc) spine

type instance
  WithContext (TypeClassConstraint types) =
    Contextualised (TypeClassConstraint types) (ConstraintContext types)

--------------------------------------------------------------------------------
-- Constraint

data Constraint types
  = -- | Represents that the two contained expressions should be equal.
    UnificationConstraint (UnificationConstraint types)
  | -- | Represents that the provided type must have the required functionality
    TypeClassConstraint (TypeClassConstraint types)
  deriving (Show)

type instance
  WithContext (Constraint types) =
    Contextualised (Constraint types) (ConstraintContext types)

getTypeClassConstraint :: WithContext (Constraint types) -> Maybe (WithContext (TypeClassConstraint types))
getTypeClassConstraint (WithContext constraint ctx) = case constraint of
  TypeClassConstraint tc -> Just (WithContext tc ctx)
  _ -> Nothing

separateConstraints :: [WithContext (Constraint types)] -> ([WithContext (UnificationConstraint types)], [WithContext (TypeClassConstraint types)])
separateConstraints [] = ([], [])
separateConstraints (WithContext c ctx : cs) = case c of
  UnificationConstraint uc -> first (WithContext uc ctx :) (separateConstraints cs)
  TypeClassConstraint tc -> second (WithContext tc ctx :) (separateConstraints cs)

blockConstraintOn ::
  Contextualised c (ConstraintContext types) ->
  MetaSet ->
  Contextualised c (ConstraintContext types)
blockConstraintOn (WithContext c ctx) metas = WithContext c (blockCtxOn metas ctx)

isBlocked :: MetaSet -> ConstraintContext types -> Bool
isBlocked solvedMetas ctx = isStillBlocked solvedMetas (blockedBy ctx)

constraintIsBlocked :: MetaSet -> Contextualised c (ConstraintContext types) -> Bool
constraintIsBlocked solvedMetas c = isBlocked solvedMetas (contextOf c)

--------------------------------------------------------------------------------
-- Progress in solving meta-variable constraints

data ConstraintProgress types
  = Stuck MetaSet
  | Progress [WithContext (Constraint types)]
  deriving (Show)

instance Semigroup (ConstraintProgress types) where
  Stuck m1 <> Stuck m2 = Stuck (m1 <> m2)
  Stuck {} <> x@Progress {} = x
  x@Progress {} <> Stuck {} = x
  Progress r1 <> Progress r2 = Progress (r1 <> r2)

--------------------------------------------------------------------------------
-- Class for typable builtins

class (Eq types) => PrintableBuiltin types where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  convertBuiltin ::
    Provenance ->
    types ->
    Expr var Builtin

  isTypeClassOp :: types -> Bool

isTypeClassOperation :: (PrintableBuiltin types) => NormalisableBuiltin types -> Bool
isTypeClassOperation = \case
  CType t -> isTypeClassOp t
  _ -> False
