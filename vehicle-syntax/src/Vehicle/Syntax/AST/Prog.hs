module Vehicle.Syntax.AST.Prog where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Decl (GenericDecl)

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype GenericProg expr
  = -- | List of declarations.
    Main [GenericDecl expr]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData expr => NFData (GenericProg expr)

instance ToJSON expr => ToJSON (GenericProg expr)

instance Serialize expr => Serialize (GenericProg expr)

traverseDecls ::
  Monad m =>
  (GenericDecl expr1 -> m (GenericDecl expr2)) ->
  GenericProg expr1 ->
  m (GenericProg expr2)
traverseDecls f (Main ds) = Main <$> traverse f ds
