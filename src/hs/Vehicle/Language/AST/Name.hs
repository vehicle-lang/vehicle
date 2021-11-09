module Vehicle.Language.AST.Name where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core

--------------------------------------------------------------------------------
-- Names

data Name
  = User Symbol  -- User-generated name
  | Machine      -- Automatically generated name
  deriving (Eq, Ord, Show, Generic)

instance NFData Name

instance Pretty Name where
  pretty (User symbol) = pretty symbol
  pretty Machine       = "Machine"

--------------------------------------------------------------------------------
-- Type class

class HasName a where
  nameOf :: a -> Name

freshNames :: [Symbol]
freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]

instance HasName (Binder Name var ann) where
  nameOf (Binder _ _ _ name _) = name

--------------------------------------------------------------------------------
-- Names

-- An expression that uses named variables for both binders and variables.
type NamedBinder ann = Binder Name Name ann
type NamedArg    ann = Arg    Name Name ann
type NamedExpr   ann = Expr   Name Name ann
type NamedDecl   ann = Decl   Name Name ann
type NamedProg   ann = Prog   Name Name ann
