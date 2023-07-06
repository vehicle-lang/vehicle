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
import Vehicle.Expr.Normalised

type Imports builtin = [GluedProg builtin]

--------------------------------------------------------------------------------

-- | Errors in bidirectional type-checking
data TypingError builtin
  = MissingExplicitArgument (TypingBoundCtx builtin) (Binder Ix builtin) (Arg Ix builtin)
  | FunctionTypeMismatch (TypingBoundCtx builtin) (Expr Ix builtin) [Arg Ix builtin] (Expr Ix builtin) [Arg Ix builtin]
  | FailedUnification (NonEmpty (WithContext (UnificationConstraint builtin)))
  | UnsolvableConstraints (NonEmpty (WithContext (Constraint builtin)))

instance Pretty (TypingError builtin) where
  pretty = \case
    MissingExplicitArgument {} -> "MissingExplicitArgument"
    FunctionTypeMismatch {} -> "FunctionTypeMismatch"
    FailedUnification {} -> "FailedUnification"
    UnsolvableConstraints {} -> "UnsolvableConstraints"

--------------------------------------------------------------------------------
-- Typing declaration context

data TypingDeclCtxEntry builtin = TypingDeclCtxEntry
  { declAnns :: [Annotation],
    declType :: GluedType builtin,
    declBody :: Maybe (GluedExpr builtin)
  }

type TypingDeclCtx builtin = DeclCtx (TypingDeclCtxEntry builtin)

mkTypingDeclCtxEntry :: GluedDecl builtin -> TypingDeclCtxEntry builtin
mkTypingDeclCtxEntry decl =
  TypingDeclCtxEntry
    { declAnns = annotationsOf decl,
      declType = typeOf decl,
      declBody = bodyOf decl
    }

addToTypingDeclCtx :: GluedDecl builtin -> TypingDeclCtx builtin -> TypingDeclCtx builtin
addToTypingDeclCtx decl = Map.insert (identifierOf decl) (mkTypingDeclCtxEntry decl)

--------------------------------------------------------------------------------
-- Typing declaration context

data NormDeclCtxEntry builtin = NormDeclCtxEntry
  { declExpr :: Value builtin,
    declAnns :: [Annotation],
    declArity :: Int
  }

type NormDeclCtx builtin = DeclCtx (NormDeclCtxEntry builtin)

typingDeclCtxToNormDeclCtx :: TypingDeclCtx builtin -> NormDeclCtx builtin
typingDeclCtxToNormDeclCtx = Map.mapMaybe $ \TypingDeclCtxEntry {..} ->
  fmap
    ( \body ->
        NormDeclCtxEntry
          { declExpr = normalised body,
            declAnns = declAnns,
            declArity = arity (normalised declType)
          }
    )
    declBody

--------------------------------------------------------------------------------
-- Meta variable substitution

type MetaSubstitution builtin = MetaMap (GluedExpr builtin)

--------------------------------------------------------------------------------
-- Bound variable context

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type TypingBoundCtxEntry builtin =
  ( Binder Ix builtin
  )

mkTypingBoundCtxEntry :: Binder Ix builtin -> TypingBoundCtxEntry builtin
mkTypingBoundCtxEntry binder = binder

type TypingBoundCtx builtin = BoundCtx (TypingBoundCtxEntry builtin)

instance HasBoundCtx (TypingBoundCtx builtin) where
  boundContextOf = map nameOf

typingBoundContextToEnv :: TypingBoundCtx builtin -> Env builtin
typingBoundContextToEnv ctx = do
  let levels = reverse (fmap Lv [0 .. length ctx - 1])
  zipWith (\level binder -> (nameOf binder, VBoundVar level [])) levels ctx

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Constraint origins

data ConstraintOrigin builtin
  = CheckingExprType (Expr Ix builtin) (Type Ix builtin) (Type Ix builtin)
  | CheckingBinderType (Maybe Name) (Type Ix builtin) (Type Ix builtin)
  | CheckingTypeClass (Expr Ix builtin) [Arg Ix builtin] (Type Ix builtin)
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

data ConstraintContext builtin = ConstraintContext
  { -- | The id for the constraint, used primarily for logging purposes.
    constraintID :: ConstraintID,
    -- | The original provenance of the constraint
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
  provenanceOf (ConstraintContext _ _ _ creationProvenance _ _) = creationProvenance

instance HasBoundCtx (ConstraintContext builtin) where
  boundContextOf = boundContextOf . boundContext

blockCtxOn :: MetaSet -> ConstraintContext builtin -> ConstraintContext builtin
blockCtxOn metas (ConstraintContext cid originProv originalConstraint creationProv _ ctx) =
  let status = BlockingStatus (Just metas)
   in ConstraintContext cid originProv originalConstraint creationProv status ctx

extendConstraintBoundCtx :: ConstraintContext builtin -> Telescope Ix builtin -> ConstraintContext builtin
extendConstraintBoundCtx ConstraintContext {..} telescope =
  ConstraintContext
    { boundContext = fmap mkTypingBoundCtxEntry telescope ++ boundContext,
      ..
    }

contextDBLevel :: ConstraintContext builtin -> Lv
contextDBLevel = Lv . length . boundContext

--------------------------------------------------------------------------------
-- Unification constraints

-- | A constraint representing that a pair of expressions should be equal
data UnificationConstraint builtin = Unify (Value builtin) (Value builtin)
  deriving (Show)

type instance
  WithContext (UnificationConstraint builtin) =
    Contextualised (UnificationConstraint builtin) (ConstraintContext builtin)

--------------------------------------------------------------------------------
-- Type-class constraints

data TypeClassConstraint builtin = Has MetaID (Value builtin)
  deriving (Show)

type instance
  WithContext (TypeClassConstraint builtin) =
    Contextualised (TypeClassConstraint builtin) (ConstraintContext builtin)

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

getTypeClassConstraint :: WithContext (Constraint builtin) -> Maybe (WithContext (TypeClassConstraint builtin))
getTypeClassConstraint (WithContext constraint ctx) = case constraint of
  TypeClassConstraint tc -> Just (WithContext tc ctx)
  _ -> Nothing

separateConstraints :: [WithContext (Constraint builtin)] -> ([WithContext (UnificationConstraint builtin)], [WithContext (TypeClassConstraint builtin)])
separateConstraints [] = ([], [])
separateConstraints (WithContext c ctx : cs) = case c of
  UnificationConstraint uc -> first (WithContext uc ctx :) (separateConstraints cs)
  TypeClassConstraint tc -> second (WithContext tc ctx :) (separateConstraints cs)

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
