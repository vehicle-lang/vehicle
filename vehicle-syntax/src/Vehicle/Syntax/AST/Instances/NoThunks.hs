{-# LANGUAGE CPP #-}

module Vehicle.Syntax.AST.Instances.NoThunks where

#ifdef nothunks
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.AST.Binder
import Vehicle.Syntax.AST.Decl
import Vehicle.Syntax.AST.Expr
import Vehicle.Syntax.AST.Name
import Vehicle.Syntax.AST.Prog
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.AST.Relevance
import Vehicle.Syntax.AST.Visibility
import Vehicle.Syntax.Builtin

import NoThunks.Class (NoThunks)

-- Vehicle.Syntax.Builtin.Core
instance NoThunks FunctionPosition
instance NoThunks EqualityOp
instance NoThunks EqualityDomain
instance NoThunks OrderOp
instance NoThunks OrderDomain
instance NoThunks Quantifier

-- Now Vehicle.Data.Builtin.Linearity.Core
-- instance NoThunks LinearityProvenance
-- instance NoThunks Linearity
-- instance NoThunks LinearityTypeClass

-- Now Vehicle.Data.Builtin.Polarity.Core
-- instance NoThunks PolarityProvenance
-- instance NoThunks Polarity
-- instance NoThunks PolarityTypeClass

-- Vehicle.Syntax.Builtin.TypeClass
instance NoThunks TypeClass
instance NoThunks TypeClassOp

-- Vehicle.Syntax.AST.Arg
instance NoThunks expr => NoThunks (GenericArg expr)

-- Vehicle.Syntax.AST.Binder
instance NoThunks BinderNamingForm
instance NoThunks BinderDisplayForm
instance (NoThunks expr) => NoThunks (GenericBinder expr)

-- Vehicle.Syntax.Builtin
instance NoThunks BuiltinConstructor
instance NoThunks BuiltinFunction
instance NoThunks BuiltinType
instance NoThunks NegDomain
instance NoThunks AddDomain
instance NoThunks SubDomain
instance NoThunks MulDomain
instance NoThunks DivDomain
instance NoThunks FromNatDomain
instance NoThunks FromRatDomain
instance NoThunks Builtin

-- Vehicle.Syntax.AST.Decl
instance NoThunks expr => NoThunks (GenericDecl expr)
instance NoThunks ParameterSort
instance NoThunks DefAbstractSort
instance NoThunks Annotation

-- Vehicle.Syntax.AST.Expr
instance NoThunks UniverseLevel
instance (NoThunks var, NoThunks builtin) => NoThunks (Expr builtin)

-- Vehicle.Data.Meta
instance NoThunks MetaID

-- Vehicle.Syntax.AST.Name
instance NoThunks Module
instance NoThunks ModulePath
instance NoThunks Identifier

-- Vehicle.Syntax.AST.Prog
instance NoThunks expr => NoThunks (GenericProg expr)

-- Vehicle.Syntax.AST.Provenance
instance NoThunks Position
instance NoThunks Range
instance NoThunks Provenance

-- Vehicle.Syntax.AST.Relevance
instance NoThunks Relevance

-- Vehicle.Syntax.AST.Visibility
instance NoThunks Visibility
#endif
