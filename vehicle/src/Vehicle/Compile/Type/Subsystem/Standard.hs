{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard
  ( module Core,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.AnnotationRestrictions
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceSolver
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
import Vehicle.Compile.Type.Subsystem.Standard.Core as Core
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Expr.Normalisable
import Vehicle.Syntax.Sugar

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

instance TypableBuiltin StandardBuiltinType where
  convertFromStandardTypes p t args = return $ normAppList p (Builtin p (CType t)) args
  useDependentMetas _ = True
  typeBuiltin = typeStandardBuiltin
  restrictNetworkType = restrictStandardNetworkType
  restrictDatasetType = restrictStandardDatasetType
  restrictParameterType = restrictStandardParameterType
  restrictInferableParameterType = restrictStandardInferableParameterType
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
