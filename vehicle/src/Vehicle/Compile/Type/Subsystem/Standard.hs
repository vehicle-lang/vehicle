{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard
  ( module Core,
    convertToTypingBuiltins,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core (malformedConstraintError)
import Vehicle.Compile.Type.Constraint.IndexSolver
import Vehicle.Compile.Type.Constraint.InstanceDefaultSolver
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.AnnotationRestrictions
import Vehicle.Compile.Type.Subsystem.Standard.Core as Core
import Vehicle.Compile.Type.Subsystem.Standard.InstanceDefaults ()
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.Normalised
import Prelude hiding (pi)

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

instance TypableBuiltin Builtin where
  typeBuiltin = typeStandardBuiltin

instance TypableBuiltin StandardTypingBuiltin where
  typeBuiltin = typeStandardTypingBuiltin

instance HasTypeSystem StandardTypingBuiltin where
  convertFromStandardBuiltins = convertToTypingBuiltins
  useDependentMetas _ = True
  restrictNetworkType = restrictStandardNetworkType
  restrictDatasetType = restrictStandardDatasetType
  restrictParameterType = restrictStandardParameterType
  restrictPropertyType = restrictStandardPropertyType
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults

convertToTypingBuiltins :: (MonadCompile m) => BuiltinUpdate m Ix Builtin StandardTypingBuiltin
convertToTypingBuiltins p1 p2 t args = return $ normAppList p1 (Builtin p2 (StandardBuiltin t)) args

solveInstanceConstraint ::
  (TCM StandardTypingBuiltin m) =>
  InstanceCandidateDatabase StandardTypingBuiltin ->
  WithContext (InstanceConstraint StandardTypingBuiltin) ->
  m ()
solveInstanceConstraint database constraint@(WithContext (Resolve _ _ _ goal) _) = do
  case goal of
    VBuiltin (StandardBuiltin NatInDomainConstraint) _ -> solveIndexConstraint constraint
    VBuiltin {} -> resolveInstance database constraint
    _ -> malformedConstraintError constraint
