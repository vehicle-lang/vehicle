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
type NamedBinder = Binder NamedBinding NamedVar
type NamedArg    = Arg    NamedBinding NamedVar
type NamedExpr   = Expr   NamedBinding NamedVar
type NamedDecl   = Decl   NamedBinding NamedVar
type NamedProg   = Prog   NamedBinding NamedVar

--------------------------------------------------------------------------------
-- Type class

class HasName a name where
  nameOf :: a -> name

freshNames :: [Symbol]
freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]

instance HasName (Binder binder var) binder where
  nameOf (Binder _ _ name _) = name

instance HasName Identifier Symbol where
  nameOf (Identifier name) = name