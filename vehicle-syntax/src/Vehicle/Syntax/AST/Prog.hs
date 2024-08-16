module Vehicle.Syntax.AST.Prog where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Decl (GenericDecl)
import Vehicle.Syntax.AST.Expr (Expr)

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype GenericProg expr
  = -- | List of declarations.
    Main [GenericDecl expr]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

type Prog var builtin = GenericProg (Expr var builtin)

instance (NFData expr) => NFData (GenericProg expr)

instance (Serialize expr) => Serialize (GenericProg expr)

traverseDecls ::
  (Monad m) =>
  (GenericDecl expr1 -> m (GenericDecl expr2)) ->
  GenericProg expr1 ->
  m (GenericProg expr2)
traverseDecls f (Main ds) = Main <$> traverse f ds

filterDecls ::
  (GenericDecl expr -> Bool) ->
  GenericProg expr ->
  GenericProg expr
filterDecls f (Main ds) = Main (filter f ds)
