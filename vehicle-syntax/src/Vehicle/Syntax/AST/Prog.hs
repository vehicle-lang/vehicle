module Vehicle.Syntax.AST.Prog where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Vehicle.Syntax.AST.Decl (GenericDecl)

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype GenericProg expr
  = Main [GenericDecl expr] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData expr => NFData (GenericProg expr)

traverseDecls :: Monad m
              => (GenericDecl expr1 -> m (GenericDecl expr2))
              -> GenericProg expr1
              -> m (GenericProg expr2)
traverseDecls f (Main ds) = Main <$> traverse f ds
