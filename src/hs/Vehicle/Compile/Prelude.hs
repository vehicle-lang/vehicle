
module Vehicle.Compile.Prelude
  ( module X
  , module Vehicle.Compile.Prelude
  ) where

import Data.Map (Map)

import Vehicle.Prelude as X
import Vehicle.Backend.Prelude (Backend)
import Vehicle.Language.AST as X
import Vehicle.Resource as X
import Vehicle.Language.Provenance as X

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
type InputAnn     = Provenance

type InputArg       = Arg    InputBinding InputVar InputAnn
type InputBinder    = Binder InputBinding InputVar InputAnn
type InputExpr      = Expr   InputBinding InputVar InputAnn
type InputDecl      = Decl   InputBinding InputVar InputAnn
type InputProg      = Prog   InputBinding InputVar InputAnn

-- * Types pre type-checking

type UncheckedBinding = DBBinding
type UncheckedVar     = DBVar
type UncheckedAnn     = Provenance

type UncheckedBinder = DBBinder UncheckedAnn
type UncheckedArg    = DBArg    UncheckedAnn
type UncheckedExpr   = DBExpr   UncheckedAnn
type UncheckedDecl   = DBDecl   UncheckedAnn
type UncheckedProg   = DBProg   UncheckedAnn

-- * Types post type-checking

type CheckedBinding = DBBinding
type CheckedVar     = DBVar
type CheckedAnn     = Provenance

type CheckedBinder = DBBinder  CheckedAnn
type CheckedArg    = DBArg     CheckedAnn
type CheckedExpr   = DBExpr    CheckedAnn
type CheckedDecl   = DBDecl    CheckedAnn
type CheckedProg   = DBProg    CheckedAnn

-- * Type of annotations attached to the AST that are output by the compiler

type OutputBinding = NamedBinding
type OutputVar     = NamedVar
type OutputAnn     = Provenance

type OutputBinder = Binder OutputBinding OutputVar OutputAnn
type OutputArg    = Arg    OutputBinding OutputVar OutputAnn
type OutputExpr   = Expr   OutputBinding OutputVar OutputAnn
type OutputDecl   = Decl   OutputBinding OutputVar OutputAnn
type OutputProg   = Prog   OutputBinding OutputVar OutputAnn

type CheckedCoDBExpr   = CoDBExpr CheckedAnn
type CheckedCoDBArg    = CoDBArg CheckedAnn
type CheckedCoDBBinder = CoDBBinder CheckedAnn

-- | An expression paired with a position tree represting positions within it.
-- Currently used mainly for pretty printing position trees.
data PositionsInExpr = PositionsInExpr CheckedCoDBExpr PositionTree
  deriving Show

--------------------------------------------------------------------------------
-- Contexts

-- | The names, types and values if known of the variables that are in
-- currently in scope, indexed into via De Bruijn expressions.
type BoundCtx = [(DBBinding, CheckedExpr, Maybe CheckedExpr)]

instance IsBoundCtx BoundCtx where
  ctxNames = map (\(n, _, _) -> n)

-- | The declarations that are currently in scope, indexed into via their names.
-- The first component is the type, and the second one the expression (if not
-- a postulate-style declaration).
type DeclCtx = Map Identifier (CheckedExpr, Maybe CheckedExpr)

-- | Combined context
data VariableCtx = VariableCtx
  { boundCtx :: BoundCtx
  , declCtx  :: DeclCtx
  }

emptyVariableCtx :: VariableCtx
emptyVariableCtx = VariableCtx mempty mempty

class IsBoundCtx a where
  ctxNames :: a -> [DBBinding]

instance IsBoundCtx [DBBinding] where
  ctxNames = id

instance IsBoundCtx [Symbol] where
  ctxNames = map Just

--------------------------------------------------------------------------------
-- Logging

logCompilerPass :: MonadLogger m => Doc a -> m b -> m b
logCompilerPass passName performPass = do
  logDebug MinDetail $ "Beginning" <+> passName
  incrCallDepth
  result <- performPass
  decrCallDepth
  logDebug MinDetail $ "Finished" <+> passName <> line
  return result

logCompilerPassOutput :: MonadLogger m => Doc a -> m ()
logCompilerPassOutput result = do
  logDebug MidDetail "Result:"
  incrCallDepth
  logDebug MidDetail result
  decrCallDepth