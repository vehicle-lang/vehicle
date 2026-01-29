{-# LANGUAGE CPP #-}

module Vehicle.Data.AST.Instances.NoThunks where

#ifdef nothunks
import Vehicle.Data.AST.Arg
import Vehicle.Data.AST.Binder
import Vehicle.Data.AST.Decl
import Vehicle.Data.AST.Expr.Desugared
import Vehicle.Data.AST.Name
import Vehicle.Data.AST.Record
import Vehicle.Data.AST.Module
import Vehicle.Data.AST.Provenance
import Vehicle.Data.AST.Relevance
import Vehicle.Data.AST.Visibility
import Vehicle.Data.Tensor
import Vehicle.Data.Builtin.Standard

import NoThunks.Class (NoThunks)

-- Vehicle.Data.Builtin.Core.Core
instance NoThunks FunctionPosition
instance NoThunks ComparisonOp
instance NoThunks Quantifier

-- Now Vehicle.Data.Builtin.Linearity
-- instance NoThunks LinearityProof
-- instance NoThunks Linearity
-- instance NoThunks LinearityTypeClass

-- Now Vehicle.Data.Builtin.Polarity
-- instance NoThunks PolarityProvenance
-- instance NoThunks Polarity
-- instance NoThunks PolarityTypeClass

-- Vehicle.Data.Builtin.Core.TypeClass
instance NoThunks TypeClass
instance NoThunks TypeClassOp

-- Vehicle.Data.AST.Arg
instance NoThunks expr => NoThunks (GenericArg expr)

-- Vehicle.Data.AST.Binder
instance NoThunks BinderNamingForm
instance NoThunks BinderDisplayForm
instance (NoThunks expr) => NoThunks (GenericBinder expr)

-- Vehicle.Data.Builtin.Core
instance NoThunks BuiltinConstructor
instance NoThunks BuiltinFunction
instance NoThunks BuiltinType
instance NoThunks BuiltinCast
instance NoThunks DerivedFunction
instance NoThunks NegDomain
instance NoThunks AddDomain
instance NoThunks SubDomain
instance NoThunks MulDomain
instance NoThunks DivDomain
instance NoThunks MinDomain
instance NoThunks MaxDomain
instance NoThunks FromNatDomain
instance NoThunks FromRatDomain
instance NoThunks Builtin

-- Vehicle.Data.Tensor
instance NoThunks (Tensor Int)
instance NoThunks (Tensor Bool)
instance NoThunks (Tensor Rational)

-- Vehicle.Data.AST.Decl
instance NoThunks expr => NoThunks (GenericDecl expr)
instance NoThunks DefAbstractSort
instance NoThunks ParameterSort
instance NoThunks DefFunctionSort
instance NoThunks DefRecordSort
instance NoThunks FunctionDeclAnnotation

-- Vehicle.Data.AST.Expr.Desugared
instance NoThunks Expr

-- Vehicle.Data.AST.Name
instance NoThunks ModulePath
instance NoThunks Identifier

-- Vehicle.Data.AST.Module
instance NoThunks ImportStatement
instance NoThunks expr => NoThunks (GenericModule expr)

-- Vehicle.Data.AST.Provenance
instance NoThunks Position
instance NoThunks Range
instance NoThunks Provenance

-- Vehicle.Data.AST.Relevance
instance NoThunks Relevance

-- Vehicle.Data.AST.Record
instance NoThunks FieldName

-- Vehicle.Data.AST.Visibility
instance NoThunks Visibility
#endif
