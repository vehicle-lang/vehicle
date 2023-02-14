{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core where

import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Instance constraints

type MonadInstance m =
  (TCM Builtin m)

-- | Function signature for constraints solved by instance search.
type InstanceSolver =
  forall m.
  MonadInstance m =>
  StandardConstraintContext ->
  MetaID ->
  StandardSpine ->
  m ()

type TypeClassProgress = Either MetaSet ([WithContext (Constraint Builtin)], StandardNormExpr)

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
type TypeClassSolver =
  forall m.
  MonadInstance m =>
  WithContext (TypeClassConstraint Builtin) ->
  [StandardNormType] ->
  m TypeClassProgress

-- | Function signature for auxiliary constraints solved by type class resolution.
type AuxiliaryTypeClassSolver =
  forall m.
  MonadInstance m =>
  WithContext (TypeClassConstraint Builtin) ->
  [StandardNormType] ->
  m StandardConstraintProgress

mkVAnnBoolType :: StandardNormExpr -> StandardNormExpr -> StandardNormExpr
mkVAnnBoolType lin pol = VConstructor Bool [IrrelevantImplicitArg mempty lin, IrrelevantImplicitArg mempty pol]

mkVAnnRatType :: StandardNormExpr -> StandardNormExpr
mkVAnnRatType lin = VConstructor Rat [IrrelevantImplicitArg mempty lin]

mkVIndexType :: StandardNormExpr -> StandardNormExpr
mkVIndexType size = VConstructor Index [ExplicitArg mempty size]

mkVListType :: StandardNormType -> StandardNormExpr
mkVListType tElem = VConstructor List [ExplicitArg mempty tElem]

mkVVecType :: StandardNormType -> StandardNormExpr -> StandardNormExpr
mkVVecType tElem dim = VConstructor Vector [ExplicitArg mempty tElem, ExplicitArg mempty dim]

isBoolType :: StandardNormExpr -> Bool
isBoolType (VConstructor Bool _) = True
isBoolType _ = False

isIndexType :: StandardNormExpr -> Bool
isIndexType (VConstructor Index _) = True
isIndexType _ = False

isNatType :: StandardNormExpr -> Bool
isNatType (VConstructor Nat _) = True
isNatType _ = False

isIntType :: StandardNormExpr -> Bool
isIntType (VConstructor Int _) = True
isIntType _ = False

isRatType :: StandardNormExpr -> Bool
isRatType (VConstructor Rat _) = True
isRatType _ = False

isListType :: StandardNormExpr -> Bool
isListType (VConstructor List _) = True
isListType _ = False

isVectorType :: StandardNormExpr -> Bool
isVectorType (VConstructor Vector _) = True
isVectorType _ = False

isBoundVar :: StandardNormExpr -> Bool
isBoundVar VBoundVar {} = True
isBoundVar _ = False

isAuxTypeClassConstraint :: TypeClassConstraint Builtin -> Bool
isAuxTypeClassConstraint (Has _ b _) = case b of
  Constructor (TypeClass tc) -> isAuxiliaryTypeClass tc
  _ -> False

getTypeClass :: MonadCompile m => Builtin -> m TypeClass
getTypeClass = \case
  Constructor (TypeClass tc) -> return tc
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
