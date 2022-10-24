
module Vehicle.Compile.Prelude
  ( module X
  , module Vehicle.Compile.Prelude
  ) where

import Data.Set (Set)
import Data.Map (Map)

import Vehicle.Prelude as X
import Vehicle.Language.AST as X
import Vehicle.Resource as X
import Vehicle.Compile.Prelude.Patterns as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Compile.Prelude.Contexts as X
import Vehicle.Compile.Prelude.DependencyGraph as X

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the AST after parsing
-- before being analysed by the compiler

type InputBinding = Maybe NamedBinding
type InputVar     = Name

type InputArg       = Arg    InputBinding InputVar
type InputBinder    = Binder InputBinding InputVar
type InputExpr      = Expr   InputBinding InputVar
type InputDecl      = Decl   InputBinding InputVar
type InputProg      = Prog   InputBinding InputVar

-- * Types pre type-checking

type UncheckedBinding = DBBinding
type UncheckedVar     = DBVar

type UncheckedBinder = DBBinder
type UncheckedArg    = DBArg
type UncheckedExpr   = DBExpr
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
type SuppliedDBProg   = Prog   NamedBinding DBVar
type SuppliedDBDecl   = Decl   NamedBinding DBVar
type SuppliedDBExpr   = Expr   NamedBinding DBVar
type SuppliedDBArg    = Arg    NamedBinding DBVar
type SuppliedDBBinder = Binder NamedBinding DBVar

-- | An expression paired with a position tree represting positions within it.
-- Currently used mainly for pretty printing position trees.
data PositionsInExpr = PositionsInExpr CheckedCoDBExpr PositionTree
  deriving Show

type DeclProvenance = (Identifier, Provenance)

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
-- Other

type UncheckedPropertyContext = Set Identifier
type PropertyContext = Map Identifier PropertyInfo