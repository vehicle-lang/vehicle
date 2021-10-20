{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Language.Compile.Type.Core where

import Prelude hiding (pi)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Language.MetaSet
import Vehicle.Language.MetaSubstitution hiding (map)
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
  prettyCore (Unify (e1, e2)) = prettyCore e1 <+> "~" <+> prettyCore e2
  prettyCore (m `Has` e)      = pretty m <+> "~" <+> prettyCore e

  prettyFrontend ctx (Unify (e1, e2)) = prettyFrontend ctx e1 <+> "~" <+> prettyFrontend ctx e2
  prettyFrontend ctx (m `Has` e)      = pretty m <+> "~" <+> prettyFrontend ctx e

data ConstraintContext = ConstraintContext
  { _prov            :: Provenance       -- The origin of the constraint
  , blockingMetas    :: BlockingMetas    -- The set of metas blocking progress on this constraint, if known
  , varContext       :: VariableCtx      -- The current declaration context (needed for normalisation)
  }

instance HasProvenance ConstraintContext where
  prov (ConstraintContext p _ _) = p

data Constraint = Constraint ConstraintContext BaseConstraint

variableContext :: Constraint -> VariableCtx
variableContext (Constraint ctx _) = varContext ctx

declContext :: ConstraintContext -> DeclCtx
declContext = declCtx . varContext

boundContext :: Constraint -> BoundCtx
boundContext = boundCtx . variableContext

instance HasProvenance Constraint where
  prov (Constraint ctx _) = prov ctx

instance Simplify Constraint where
  simplify (Constraint ctx c) = Constraint ctx <$> simplify c

instance PrettyLang Constraint where
  prettyCore         (Constraint _ c) = prettyCore c
  prettyFrontend ctx (Constraint _ c) = prettyFrontend ctx c

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
    UncheckedArg  -- The non-explicit argument
    CheckedExpr   -- Expected type

instance MeaningfulError TypingError where
  details (Mismatch p ctx candidate expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> prettyFriendly ctx expected <+>
                   "but inferred type" <+> prettyFriendly ctx candidate
    , fix        = "unknown"
    }

  details (UnresolvedHole p name) = UError $ UserError
    { provenance = p
    , problem    = "the type of" <+> squotes (pretty name) <+> "could not be resolved"
    , fix        = "unknown"
    }

  details (FailedConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = prov firstConstraint
    , problem    = "Could not solve the constraint:" <+>
                      prettyFriendly (boundContext firstConstraint) firstConstraint
    , fix        = "Check your types"
    }

  details (UnsolvedConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = prov firstConstraint
    , problem    = "unsolved constraint " <+>
                      prettyFriendly (boundContext firstConstraint) firstConstraint
    , fix        = "Try adding more type annotations"
    }
