
module Vehicle.Compile.Prelude
  ( module X
  , module Vehicle.Compile.Prelude
  ) where

import Vehicle.Prelude as X
import Vehicle.Backend.Prelude (Backend)
import Vehicle.Language.AST as X
import Vehicle.Resource as X
import Vehicle.Compile.Prelude.Patterns as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Compile.Prelude.Contexts as X

--------------------------------------------------------------------------------
-- Compilation

data CompileOptions = CompileOptions
  { target            :: Backend
  , specificationFile :: FilePath
  , outputFile        :: Maybe FilePath
  , networkLocations  :: NetworkLocations
  , datasetLocations  :: DatasetLocations
  , parameterValues   :: ParameterValues
  , modulePrefix      :: Maybe String
  , proofCache        :: Maybe FilePath
  } deriving (Show)

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the AST after parsing
-- before being analysed by the compiler

type InputBinding = Maybe NamedBinding
type InputVar     = NamedVar

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
type OutputVar     = NamedVar

type OutputBinder = Binder OutputBinding OutputVar
type OutputArg    = Arg    OutputBinding OutputVar
type OutputExpr   = Expr   OutputBinding OutputVar
type OutputDecl   = Decl   OutputBinding OutputVar
type OutputProg   = Prog   OutputBinding OutputVar

-- | An expression paired with a position tree represting positions within it.
-- Currently used mainly for pretty printing position trees.
data PositionsInExpr = PositionsInExpr CheckedCoDBExpr PositionTree
  deriving Show

--------------------------------------------------------------------------------
-- Logging

logCompilerPass :: MonadLogger m => DebugLevel -> Doc a -> m b -> m b
logCompilerPass level passName performPass = do
  logDebug level $ "Starting" <+> passName
  incrCallDepth
  result <- performPass
  decrCallDepth
  logDebug level $ "Finished" <+> passName <> line
  return result

logCompilerPassOutput :: MonadLogger m => Doc a -> m ()
logCompilerPassOutput result = do
  logDebug MidDetail "Result:"
  incrCallDepth
  logDebug MidDetail result
  decrCallDepth

--------------------------------------------------------------------------------
-- Other

type DeclProvenance = (Identifier, Provenance)