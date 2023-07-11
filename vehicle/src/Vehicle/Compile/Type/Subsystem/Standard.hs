{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard
  ( module Core,
  )
where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.AnnotationRestrictions
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core (relevanceOfTypeClass)
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceSolver
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
import Vehicle.Compile.Type.Subsystem.Standard.Core as Core
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Sugar

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

instance Normalisable StandardBuiltinType where
  evalBuiltin _ l spine = return $ VBuiltin l spine
  isValue = return True
  isTypeClassOp = \case
    StandardTypeClassOp {} -> True
    _ -> False
  forceBuiltin _ _ _ _ = return (Nothing, mempty)

instance TypableBuiltin StandardBuiltin where
  convertFromStandardTypes p1 p2 t args = return $ normAppList p1 (Builtin p2 t) args
  useDependentMetas _ = True
  typeBuiltin = typeStandardBuiltin
  restrictNetworkType = restrictStandardNetworkType
  restrictDatasetType = restrictStandardDatasetType
  restrictParameterType = restrictStandardParameterType
  restrictPropertyType = restrictStandardPropertyType
  handleTypingError = handleStandardTypingError
  typeClassRelevancy = relevanceOfTypeClass
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults

instance FoldableBuiltin StandardBuiltin where
  getQuant = \case
    QuantifierTCExpr p q binder body -> Just (p, q, binder, body)
    _ -> Nothing
