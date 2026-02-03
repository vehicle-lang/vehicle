module Vehicle.Data.AST.Prog where

import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- FlattenedProgram

newtype GenericProg expr = Main
  { programDeclarations :: [GenericDecl expr]
  }
  deriving (Foldable, Functor, Traversable)

type Prog builtin = GenericProg (Expr builtin)

traverseDecls ::
  (Monad m) =>
  (GenericDecl expr1 -> m (GenericDecl expr2)) ->
  GenericProg expr1 ->
  m (GenericProg expr2)
traverseDecls f (Main ds) =
  Main <$> traverse f ds
