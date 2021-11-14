module Vehicle.Language.AST.Name where

import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core

--------------------------------------------------------------------------------
-- Type class

class HasName a name where
  nameOf :: a -> name

freshNames :: [Symbol]
freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]

instance HasName (Binder binder var ann) binder where
  nameOf (Binder _ _ name _) = name

--------------------------------------------------------------------------------
-- Names

-- An expression that uses named variables for both binders and variables.
type NamedBinder ann = Binder Symbol Symbol ann
type NamedArg    ann = Arg    Symbol Symbol ann
type NamedExpr   ann = Expr   Symbol Symbol ann
type NamedDecl   ann = Decl   Symbol Symbol ann
type NamedProg   ann = Prog   Symbol Symbol ann
