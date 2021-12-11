module Vehicle.Language.AST.Name where

import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core

--------------------------------------------------------------------------------
-- Definition

-- |Type of bindings when using the named representation of the AST.
type NamedBinding = Symbol
-- |Type of variables when using named representation of the AST.
type NamedVar     = Symbol

-- An expression that uses named variables for both binders and variables.
type NamedBinder ann = Binder NamedBinding NamedVar ann
type NamedArg    ann = Arg    NamedBinding NamedVar ann
type NamedExpr   ann = Expr   NamedBinding NamedVar ann
type NamedDecl   ann = Decl   NamedBinding NamedVar ann
type NamedProg   ann = Prog   NamedBinding NamedVar ann

--------------------------------------------------------------------------------
-- Type class

class HasName a name where
  nameOf :: a -> name

freshNames :: [Symbol]
freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]

instance HasName (Binder binder var ann) binder where
  nameOf (Binder _ _ name _) = name