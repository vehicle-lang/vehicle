{-# OPTIONS_GHC -Wno-orphans #-}
module Vehicle.Core.Compile.Type.Core where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Prelude hiding (pi)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Prettyprinter

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Print.Frontend (prettyFrontend)

--------------------------------------------------------------------------------
-- Context definitions

-- | The names and types of the expression variables that are in currently in scope,
-- indexed into via De Bruijn expressions.
type BoundCtx = [(Name, CheckedExpr)]

--------------------------------------------------------------------------------
-- Unification definitions

-- The context in which a unification occurs.
data UnificationContext = UnificationContext
  { sharedContext :: [Name]        -- The context before any unification starts
  , splitContext :: [(Name, Name)] -- The context after unification.
  } deriving (Show)

addBinderToCtx :: CheckedBinder -> CheckedBinder -> UnificationContext -> UnificationContext
addBinderToCtx b1 b2 = addNameToCtx (binderName b1) (binderName b2)

addNameToCtx :: Name -> Name -> UnificationContext -> UnificationContext
addNameToCtx n1 n2 UnificationContext{..} = UnificationContext
  { sharedContext = sharedContext
  , splitContext = (n1, n2) : splitContext
  }

-- | A pair of expressions should be equal
type UnificationPair = (CheckedExpr, CheckedExpr)

-- | A sequence of attempts at unification
type UnificationHistory = [UnificationPair]

-- | Represents that the two contained expressions should be equal.
data UnificationConstraint = Unify
  { unifyProv     :: Provenance         -- The location of the code that gave rise to the constraint
  , unifyCtxt     :: UnificationContext -- The context of the constraint is being solved in
  , unifHistory   :: UnificationHistory -- The history, i.e. unification path that has lead to this.
  , unifBlockedOn :: MetaSet            -- The meta-variables that the constraint is blocked on
  , unifExprs     :: UnificationPair    -- The expressions to unify
  }
  deriving Show

instance HasProvenance UnificationConstraint where
  prov (Unify p _ _ _ _) = p

instance Pretty UnificationConstraint where
  pretty (Unify _ _ _ metas (e1, e2)) =
    pretty metas <+> prettyFrontend e1 <+> "~" <+> prettyFrontend e2

makeConstraint :: Provenance -> [(Name, CheckedExpr)] -> CheckedExpr -> CheckedExpr -> UnificationConstraint
makeConstraint p ctx e1 e2 = Unify p constraintCtx [] mempty (e1, e2)
  where constraintCtx = UnificationContext (map fst ctx) []

--------------------------------------------------------------------------------
-- Type-class constraint definition

data TypeClassConstraint = Meta `Has` CheckedExpr
  deriving Show

instance Pretty TypeClassConstraint where
  pretty (m `Has` e) = "?" <> pretty m <+> "~" <+> pretty e

instance HasProvenance TypeClassConstraint where
  prov (_m `Has` e) = prov e

--------------------------------------------------------------------------------
-- Meta-variable definitions

type MetaSet = IntSet

type MetaSubstitution = IntMap CheckedExpr

instance Pretty MetaSubstitution where
  pretty msubst =
    "{" <+> align (group
      (concatWith (\x y -> x <> ";" <> line <> y)
        (fmap (\(i, t') -> "?" <> pretty i <+> ":=" <+> pretty t') (IntMap.toAscList msubst))
       <> softline <> "}"))

instance Pretty MetaSet where
  pretty = pretty . IntSet.toList


-- | The meta-variables and constraints relating the variables currently in scope.
data MetaCtx = MetaCtx
  { nextMeta               :: Meta
  , currentSubstitution    :: MetaSubstitution
  , unificationConstraints :: [UnificationConstraint]
  , typeClassConstraints   :: [TypeClassConstraint]
  }

instance Pretty MetaCtx where
  pretty MetaCtx{..} = "{" <> line <>
    "nextMeta" <+> "=" <+> pretty nextMeta <> line <>
    "unificationConstraints" <+> "=" <+> pretty unificationConstraints <> line <>
    "typeClassConstraints" <+> "=" <+> pretty typeClassConstraints <> line <>
    "}"

emptyMetaCtx :: MetaCtx
emptyMetaCtx = MetaCtx
  { nextMeta               = 0
  , currentSubstitution    = mempty
  , unificationConstraints = mempty
  , typeClassConstraints   = mempty
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
    CheckedExpr             -- The possible inferred types.
    CheckedExpr             -- The expected type.
  | UnsupportedOperation
    Provenance              -- The location of the unsupported operation.
    Text                    -- A description of the unsupported operation.
  | UnificationFailure
    UnificationConstraint
  | UnsolvedUnificationConstraints
    (NonEmpty UnificationConstraint)
  | UnsolvedTypeClassConstraints
    (NonEmpty TypeClassConstraint)

instance MeaningfulError TypingError where
  details (Mismatch p candidate expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> pretty expected <+>
                   "but inferred type" <+> pretty candidate
    , fix        = "unknown"
    }

  details (UnsupportedOperation p t) = UError $ UserError
    { provenance = p
    , problem    = "type-checking of" <+> squotes (pretty t) <+> "not currently supported"
    , fix        = "unknown"
    }

  details (UnresolvedHole p name) = UError $ UserError
    { provenance = p
    , problem    = "the type of" <+> squotes (pretty name) <+> "could not be resolved"
    , fix        = "unknown"
    }

  details (UnificationFailure constraint) = UError $ UserError
    { provenance = prov constraint
    , problem    = "Could not solve the unification constraint:" <+> pretty constraint
    , fix        = "Try adding more type annotations"
    }

  details (UnsolvedUnificationConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = prov firstConstraint
    , problem    = "unsolved constraint " <+> pretty firstConstraint
    , fix        = "unknown"
    }

  details (UnsolvedTypeClassConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = prov firstConstraint
    , problem    = "unsolved constraint " <+> pretty firstConstraint
    , fix        = "unknown"
    }
