{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard
  ( module Core,
  )
where

import Vehicle.Compile.Type.Auxiliary
import Vehicle.Compile.Type.Subsystem
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core (isAuxTypeClassConstraint)
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceSolver
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults (generateConstraintUsingDefaults)
import Vehicle.Compile.Type.Subsystem.Standard.Core as Core
import Vehicle.Compile.Type.Subsystem.Standard.Normalisation ()
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Compile.Type.Subsystem.Standard.TypeResource
import Vehicle.Syntax.AST (Builtin)

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

-- TODO Separate builtins from syntactic sugar
--
-- TODO Pass in the right number of arguments ensuring all literals
instance TypableBuiltin Builtin where
  typeBuiltin = typeStandardBuiltin
  typeLiteral = standardTypeOfLiteral
  typeVectorLiteral = standardTypeOfVectorLiteral
  typeResource = checkResourceType
  isAuxiliaryTypeClassConstraint = isAuxTypeClassConstraint
  insertHolesForAuxAnnotations = insertHolesForAuxiliaryAnnotations
  handleTypingError = handleStandardTypingError
  typeClassRelevancy = relevanceOfTypeClass
  solveInstance = solveInstanceConstraint
  getPropertyInfo = getStandardPropertyInfo
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints
  generateDefaultConstraint = generateConstraintUsingDefaults
