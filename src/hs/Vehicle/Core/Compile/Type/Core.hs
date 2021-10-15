{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Core.Compile.Type.Core where

import Prelude hiding (pi)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Print (PrettyWithConfig(..), prettyVerbose)
import Vehicle.Core.Print.Friendly (PrettyFriendly(..))
import Vehicle.Core.MetaSet
import Vehicle.Core.MetaSubstitution hiding (map)

--------------------------------------------------------------------------------
-- Context definitions

-- | The names and types of the expression variables that are in currently in scope,
-- indexed into via De Bruijn expressions.
type BoundCtx = [(Name, CheckedExpr)]

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

instance PrettyWithConfig BaseConstraint where
  pretty' (Unify (e1, e2)) = do
    e1' <- pretty' e1
    e2' <- pretty' e2
    return $ e1' <+> "~" <+> e2'

  pretty' (m `Has` e) = do
    e' <- pretty' e
    return $ pretty m <+> "~" <+> e'

instance PrettyFriendly BaseConstraint where
  prettyFriendly ctx (Unify (e1, e2)) =
    prettyFriendly ctx e1 <+> "~" <+> prettyFriendly ctx e2

  prettyFriendly ctx (m `Has` e) =
    pretty m <+> "~" <+> prettyFriendly ctx e

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

instance PrettyWithConfig Constraint where
  pretty' (Constraint _ c) = pretty' c

instance PrettyFriendly Constraint where
  prettyFriendly ctx (Constraint _ c) = prettyFriendly ctx c

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
