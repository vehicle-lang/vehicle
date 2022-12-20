{-# LANGUAGE CPP #-}

module Vehicle.Syntax.AST.Instances.NoThunks where

#if nothunks
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.AST.Binder
import Vehicle.Syntax.AST.Builtin
import Vehicle.Syntax.AST.Builtin.Core
import Vehicle.Syntax.AST.Builtin.Linearity
import Vehicle.Syntax.AST.Builtin.Polarity
import Vehicle.Syntax.AST.Builtin.TypeClass
import Vehicle.Syntax.AST.Decl
import Vehicle.Syntax.AST.Expr
import Vehicle.Syntax.AST.Meta
import Vehicle.Syntax.AST.Name
import Vehicle.Syntax.AST.Prog
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Relevance
import Vehicle.Syntax.AST.Visibility

import NoThunks.Class (NoThunks)

-- Vehicle.Syntax.AST.Builtin.Core
instance NoThunks FunctionPosition
instance NoThunks EqualityOp
instance NoThunks EqualityDomain
instance NoThunks OrderOp
instance NoThunks OrderDomain
instance NoThunks Quantifier

-- Vehicle.Syntax.AST.Builtin.Linearity
instance NoThunks LinearityProvenance
instance NoThunks Linearity
instance NoThunks LinearityTypeClass

-- Vehicle.Syntax.AST.Builtin.Polarity
instance NoThunks PolarityProvenance
instance NoThunks Polarity
instance NoThunks PolarityTypeClass

-- Vehicle.Syntax.AST.Builtin.TypeClass
instance NoThunks TypeClass
instance NoThunks TypeClassOp

-- Vehicle.Syntax.AST.Arg
instance NoThunks expr => NoThunks (GenericArg expr)

-- Vehicle.Syntax.AST.Binder
instance NoThunks BinderNamingForm
instance NoThunks BinderForm
instance (NoThunks binder, NoThunks expr) => NoThunks (GenericBinder binder expr)

-- Vehicle.Syntax.AST.Builtin
instance NoThunks BuiltinConstructor
instance NoThunks NegDomain
instance NoThunks AddDomain
instance NoThunks SubDomain
instance NoThunks MulDomain
instance NoThunks DivDomain
instance NoThunks FromNatDomain
instance NoThunks FromRatDomain
instance NoThunks FromVecDomain
instance NoThunks FoldDomain
instance NoThunks MapDomain
instance NoThunks Builtin
instance NoThunks Resource

-- Vehicle.Syntax.AST.Decl
instance NoThunks expr => NoThunks (GenericDecl expr)

-- Vehicle.Syntax.AST.Expr
instance NoThunks Universe
instance NoThunks Literal
instance (NoThunks binder, NoThunks var) => NoThunks (Expr binder var)

-- Vehicle.Syntax.AST.Meta
instance NoThunks MetaID

-- Vehicle.Syntax.AST.Name
instance NoThunks Module
instance NoThunks Identifier

-- Vehicle.Syntax.AST.Prog
instance NoThunks expr => NoThunks (GenericProg expr)

-- Vehicle.Syntax.AST.Provenance
instance NoThunks Position
instance NoThunks Range
instance NoThunks Owner
instance NoThunks Origin
instance NoThunks Provenance

-- Vehicle.Syntax.AST.Relevance
instance NoThunks Relevance

-- Vehicle.Syntax.AST.Visibility
instance NoThunks Visibility
#endif
