module Vehicle.Syntax.AST.Module where

import Control.DeepSeq (NFData)
import Data.Foldable (traverse_)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Decl (GenericDecl)
import Vehicle.Syntax.AST.Name (ModulePath)

--------------------------------------------------------------------------------
-- Module

newtype ImportStatement = ImportStatement
  { importPath :: ModulePath
  }
  deriving (Show, Generic)

instance NFData ImportStatement

instance Serialize ImportStatement

--------------------------------------------------------------------------------
-- Module

-- | A module is a list of declarations in a given namespace.
data GenericModule expr = Module
  { -- | Import statements
    moduleImports :: [ImportStatement],
    -- | List of declarations.
    moduleDeclarations :: [GenericDecl expr]
  }
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericModule expr)

instance (Serialize expr) => Serialize (GenericModule expr)

mapModuleDecls ::
  (GenericDecl expr1 -> GenericDecl expr2) ->
  GenericModule expr1 ->
  GenericModule expr2
mapModuleDecls f (Module imports ds) =
  Module imports $ fmap f ds

traverseModuleDecls ::
  (Monad m) =>
  (GenericDecl expr1 -> m (GenericDecl expr2)) ->
  GenericModule expr1 ->
  m (GenericModule expr2)
traverseModuleDecls f (Module imports ds) =
  Module imports <$> traverse f ds

traverseDecls_ ::
  (Monad m) =>
  (GenericDecl expr1 -> m b) ->
  GenericModule expr1 ->
  m ()
traverseDecls_ f (Module _imports ds) =
  traverse_ f ds

filterDecls ::
  (GenericDecl expr -> Bool) ->
  GenericModule expr ->
  GenericModule expr
filterDecls f (Module imports ds) =
  Module imports (filter f ds)
