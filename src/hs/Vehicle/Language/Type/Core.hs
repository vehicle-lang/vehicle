{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Language.Type.Core where

import Prelude hiding (pi)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Language.Type.MetaSet
import Vehicle.Language.Type.MetaSubstitution hiding (map)
import Vehicle.Language.Simplify

--------------------------------------------------------------------------------
-- Context definitions

-- | The names and types of the expression variables that are in currently in scope,
-- indexed into via De Bruijn expressions.
type BoundCtx = [(Name, CheckedExpr)]

instance IsBoundCtx BoundCtx where
  ctxNames b = map fst b

-- | The declarations that are currently in scope, indexed into via their names.
-- The first component is the type, and the second one the expression (if not
-- a postulate-style declaration).
type DeclCtx = Map Identifier (CheckedExpr, Maybe CheckedExpr)

instance Pretty DeclCtx where
  pretty = pretty . show

-- | Combined context
data VariableCtx = VariableCtx
  { boundCtx :: BoundCtx
  , declCtx  :: DeclCtx
  }

emptyVariableCtx :: VariableCtx
emptyVariableCtx = VariableCtx mempty mempty

--------------------------------------------------------------------------------
-- Unification definitions

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

--------------------------------------------------------------------------------
-- Constraints

type BlockingMetas = MetaSet

data BaseConstraint
  -- | Represents that the two contained expressions should be equal.
  = Unify UnificationPair
  -- | Represents that the provided type must have the required functionality
  | Meta `Has` CheckedExpr

instance Simplify BaseConstraint where
  simplify (Unify (e1, e2)) = do
    e1' <- simplify e1
    e2' <- simplify e2
    return $ Unify (e1', e2')

  simplify (m `Has` e) = do
    e' <- simplify e
    return $ m `Has` e'

instance PrettyLang BaseConstraint where
  prettyLang target (Unify (e1, e2)) = prettyLang target e1 <+> "~" <+> prettyLang target e2
  prettyLang target (m `Has` e)      = pretty m <+> "~" <+> prettyLang target e

instance PrettyDescopedLang BaseConstraint where
  prettyDescopedLang target ctx (Unify (e1, e2)) = prettyDescopedLang target ctx e1 <+> "~" <+> prettyDescopedLang target ctx e2
  prettyDescopedLang target ctx (m `Has` e)      = pretty m <+> "~" <+> prettyDescopedLang target ctx e

data ConstraintContext = ConstraintContext
  { _prov            :: Provenance       -- The origin of the constraint
  , blockingMetas    :: BlockingMetas    -- The set of metas blocking progress on this constraint, if known
  , varContext       :: VariableCtx      -- The current declaration context (needed for normalisation)
  }

instance HasProvenance ConstraintContext where
  provenanceOf (ConstraintContext p _ _) = p

data Constraint = Constraint ConstraintContext BaseConstraint

variableContext :: Constraint -> VariableCtx
variableContext (Constraint ctx _) = varContext ctx

declContext :: ConstraintContext -> DeclCtx
declContext = declCtx . varContext

boundContext :: Constraint -> BoundCtx
boundContext = boundCtx . variableContext

instance HasProvenance Constraint where
  provenanceOf (Constraint ctx _) = provenanceOf ctx

instance Simplify Constraint where
  simplify (Constraint ctx c) = Constraint ctx <$> simplify c

instance PrettyLang Constraint where
  prettyLang target (Constraint _ c) = prettyLang target c

instance PrettyDescopedLang Constraint where
  prettyDescopedLang target ctx (Constraint _ c) = prettyDescopedLang target ctx c

--------------------------------------------------------------------------------
-- Meta-variable definitions

-- | The meta-variables and constraints relating the variables currently in scope.
data MetaCtx = MetaCtx
  { nextMeta            :: Int
  , currentSubstitution :: MetaSubstitution
  , constraints         :: [Constraint]
  }

instance Pretty MetaCtx where
  pretty MetaCtx{..} = "{" <> line <>
    "nextMeta"            <+> "=" <+> pretty nextMeta                   <> line <>
    "currentSubstitution" <+> "=" <+> prettyVerbose currentSubstitution <> line <>
    "constraints"         <+> "=" <+> prettyVerbose constraints         <> line <>
    "}"

emptyMetaCtx :: MetaCtx
emptyMetaCtx = MetaCtx
  { nextMeta               = 0
  , currentSubstitution    = mempty
  , constraints            = mempty
  }

--------------------------------------------------------------------------------
-- Errors

-- | Errors thrown during type checking
data TypingError
  = UnresolvedHole
    Provenance              -- The location of the hole
    Symbol                  -- The name of the hole
  | Mismatch
    Provenance              -- The location of the mismatch.
    BoundCtx                -- The context at the time of the failure
    CheckedExpr             -- The possible inferred types.
    CheckedExpr             -- The expected type.
  | FailedConstraints
    (NonEmpty Constraint)
  | UnsolvedConstraints
    (NonEmpty Constraint)
  | MissingExplicitArg
    BoundCtx                -- The context at the time of the failure
    UncheckedArg            -- The non-explicit argument
    CheckedExpr             -- Expected type of the argument

instance MeaningfulError TypingError where
  details (Mismatch p ctx candidate expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> prettyFriendlyDescope ctx expected <+>
                   "but inferred type" <+> prettyFriendlyDescope ctx candidate
    , fix        = "unknown"
    }

  details (UnresolvedHole p name) = UError $ UserError
    { provenance = p
    , problem    = "the type of" <+> squotes (pretty name) <+> "could not be resolved"
    , fix        = "unknown"
    }

  details (FailedConstraints cs) = let constraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = provenanceOf constraint
    , problem    = "Could not solve the constraint:" <+>
                      prettyFriendlyDescope (boundContext constraint) constraint
    , fix        = "Check your types"
    }

  details (UnsolvedConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = provenanceOf firstConstraint
    , problem    = "unsolved constraint " <+>
                      prettyFriendlyDescope (boundContext firstConstraint) firstConstraint
    , fix        = "Try adding more type annotations"
    }

  details (MissingExplicitArg ctx arg argType) = UError $ UserError
    { provenance = provenanceOf arg
    , problem    = "expected an" <+> pretty Explicit <+> "argument of type" <+>
                   argTypeDoc <+> "but instead found" <+>
                   pretty (visibilityOf arg) <+> "argument" <+> squotes (prettyFriendlyDescope ctx (argExpr arg))
    , fix        = "Try inserting an argument of type" <+> argTypeDoc
    } where argTypeDoc = prettyFriendlyDescope ctx argType