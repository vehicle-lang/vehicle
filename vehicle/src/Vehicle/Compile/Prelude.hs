module Vehicle.Compile.Prelude
  ( module X
  , module Vehicle.Compile.Prelude
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import Vehicle.Compile.Dependency.Graph as X
import Vehicle.Compile.Prelude.Contexts as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.PositionTree (PositionTree)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Patterns as X
import Vehicle.Prelude as X
import Vehicle.Resource as X
import Vehicle.Syntax.AST as X
import Vehicle.Expr.Normalised (GluedExpr)

--------------------------------------------------------------------------------
-- Type synonyms


-- * Types pre type-checking

type UncheckedBinding = DBBinding
type UncheckedVar     = DBVar

type UncheckedBinder = DBBinder
type UncheckedArg    = DBArg
type UncheckedExpr   = DBExpr
type UncheckedType   = DBExpr
type UncheckedDecl   = DBDecl
type UncheckedProg   = DBProg

-- * Types post type-checking

type CheckedBinding = DBBinding
type CheckedVar     = DBVar

type CheckedBinder = DBBinder
type CheckedArg    = DBArg
type CheckedExpr   = DBExpr
type CheckedType   = CheckedExpr
type CheckedDecl   = DBDecl
type CheckedProg   = DBProg

type CheckedCoDBExpr   = CoDBExpr
type CheckedCoDBArg    = CoDBArg
type CheckedCoDBBinder = CoDBBinder

-- * Type of annotations attached to the AST that are output by the compiler
type OutputBinding = NamedBinding
type OutputVar     = Name

type OutputBinder = Binder OutputBinding OutputVar
type OutputArg    = Arg    OutputBinding OutputVar
type OutputExpr   = Expr   OutputBinding OutputVar
type OutputDecl   = Decl   OutputBinding OutputVar
type OutputProg   = Prog   OutputBinding OutputVar

-- | De Bruijn expressions that have had the missing names supplied.
type SuppliedDBExpr   = Expr   NamedBinding DBVar
type SuppliedDBArg    = Arg    NamedBinding DBVar
type SuppliedDBBinder = Binder NamedBinding DBVar
type SuppliedDBProg   = Prog   NamedBinding DBVar
type SuppliedDBDecl   = Decl   NamedBinding DBVar

-- | An expression paired with a position tree represting positions within it.
-- Currently used mainly for pretty printing position trees.
data PositionsInExpr = PositionsInExpr CheckedCoDBExpr PositionTree
  deriving Show

type DeclProvenance = (Identifier, Provenance)

--------------------------------------------------------------------------------
-- Typed expressions

type ImportedModules = [TypedProg]

-- | A typed-expression. Outside of the type-checker, the contents of this
-- should not be inspected directly but instead use
newtype TypedExpr = TypedExpr
  { glued :: GluedExpr
  -- |^ Stores the both the unnormalised and normalised expression, WITH
  -- auxiliary annotations.
  } deriving (Generic)

instance ToJSON   TypedExpr
instance FromJSON TypedExpr

type TypedDecl = GenericDecl TypedExpr
type TypedProg = GenericProg TypedExpr

--------------------------------------------------------------------------------
-- Logging

logCompilerPass :: MonadLogger m => LoggingLevel -> Doc a -> m b -> m b
logCompilerPass level passName performPass = do
  logDebug level $ "Starting" <+> passName
  incrCallDepth
  result <- performPass
  decrCallDepth
  logDebug level $ "Finished" <+> passName <> line
  return result

logCompilerSection :: MonadLogger m => LoggingLevel -> Doc a -> m b -> m b
logCompilerSection level sectionName performPass = do
  logDebug level sectionName
  incrCallDepth
  result <- performPass
  decrCallDepth
  logDebug level ""
  return result

logCompilerPassOutput :: MonadLogger m => Doc a -> m ()
logCompilerPassOutput result = do
  logDebug MidDetail "Result:"
  incrCallDepth
  logDebug MidDetail result
  decrCallDepth

--------------------------------------------------------------------------------
-- Property annotations

-- | A marker for how a declaration is used as part of a quantified property
-- and therefore needs to be lifted to the type-level when being exported, or
-- whether it is only used unquantified and therefore needs to be computable.
data PropertyInfo
  = PropertyInfo Linearity Polarity
  deriving (Show, Eq, Generic)

instance NFData   PropertyInfo
instance ToJSON   PropertyInfo
instance FromJSON PropertyInfo

instance Pretty PropertyInfo where
  pretty (PropertyInfo lin pol) = pretty lin <+> pretty pol

--------------------------------------------------------------------------------
-- Other

data Contextualised object context = WithContext
  { objectIn  :: object
  , contextOf :: context
  } deriving (Show)

type family WithContext a


class HasType expr typ | expr -> typ where
  typeOf :: expr -> typ

instance HasType (GenericBinder binder expr) expr where
  typeOf = binderType

instance HasType (GenericDecl expr) expr where
  typeOf = \case
    DefResource  _ _ _ t   -> t
    DefFunction  _ _ _ t _ -> t
    DefPostulate _ _ t     -> t
