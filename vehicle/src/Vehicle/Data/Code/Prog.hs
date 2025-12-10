module Vehicle.Data.Code.Prog where

import Vehicle.Data.Code.Expr
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
