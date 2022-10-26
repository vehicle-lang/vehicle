module Vehicle.Language.AST.Name where

import Data.Text (pack)

import Vehicle.Language.AST.Core
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Definition

-- | Bindings when using the named representation of the AST.
type NamedBinding = Name

-- An expression that uses named variables for both binders and variables.
type NamedBinder = Binder NamedBinding Name
type NamedArg    = Arg    NamedBinding Name
type NamedExpr   = Expr   NamedBinding Name
type NamedDecl   = Decl   NamedBinding Name
type NamedProg   = Prog   NamedBinding Name

--------------------------------------------------------------------------------
-- Type class

class HasName a name where
  nameOf :: a -> name

freshNames :: [Name]
freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]

instance HasName (Binder binder var) binder where
  nameOf (Binder _ _ _ name _) = name

instance HasName Identifier Name where
  nameOf (Identifier name) = name
