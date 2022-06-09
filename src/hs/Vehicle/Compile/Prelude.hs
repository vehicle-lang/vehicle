
module Vehicle.Compile.Prelude
  ( module X
  , module Vehicle.Compile.Prelude
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

import Vehicle.Prelude as X
import Vehicle.Backend.Prelude (Backend)
import Vehicle.Language.AST as X
import Vehicle.Resource as X
import Control.Monad.Reader

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

getDeclCtx :: MonadReader VariableCtx m => m DeclCtx
getDeclCtx = asks declCtx

addToDeclCtx :: MonadReader VariableCtx m => CheckedDecl -> m a -> m a
addToDeclCtx decl = local addDecl
  where
    declName = identifierOf decl
    declType = typeOf decl
    declBody = bodyOf decl

    addDecl :: VariableCtx -> VariableCtx
    addDecl VariableCtx{..} = VariableCtx
      { declCtx = Map.insert declName (declType, declBody) declCtx
      , ..
      }

getBoundCtx :: MonadReader VariableCtx m => m BoundCtx
getBoundCtx = asks boundCtx

getVariableCtx :: MonadReader VariableCtx m => m VariableCtx
getVariableCtx = ask

addToBoundCtx :: MonadReader VariableCtx m => DBBinding -> CheckedExpr -> Maybe CheckedExpr -> m a -> m a
addToBoundCtx n t e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{ boundCtx = (n, t, e) : boundCtx, ..}

--------------------------------------------------------------------------------
-- Logging

logCompilerPass :: MonadLogger m => Doc a -> m b -> m b
logCompilerPass passName performPass = do
  logDebug MinDetail $ "Starting" <+> passName
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