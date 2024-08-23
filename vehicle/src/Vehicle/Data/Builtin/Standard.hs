{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard
  ( module Core,
    module Vehicle.Data.Builtin.Standard,
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
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard.AnnotationRestrictions
import Vehicle.Data.Builtin.Standard.Core as Core
import Vehicle.Data.Builtin.Standard.Eval ()
import Vehicle.Data.Builtin.Standard.InstanceDefaults ()
import Vehicle.Data.Builtin.Standard.Type
import Vehicle.Data.Expr.Value
import Prelude hiding (pi)

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

instance TypableBuiltin Builtin where
  typeBuiltin = typeStandardBuiltin

instance HasTypeSystem Builtin where
  convertFromStandardBuiltins = convertToTypingBuiltins
  useDependentMetas _ = True
  restrictNetworkType = restrictStandardNetworkType
  restrictDatasetType = restrictStandardDatasetType
  restrictParameterType = restrictStandardParameterType
  restrictPropertyType = restrictStandardPropertyType
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults

convertToTypingBuiltins :: (MonadCompile m) => BuiltinUpdate m Ix Builtin Builtin
convertToTypingBuiltins p t args = return $ normAppList (Builtin p t) args

solveInstanceConstraint ::
  (TCM Builtin m) =>
  InstanceCandidateDatabase Builtin ->
  WithContext (InstanceConstraint Builtin) ->
  m ()
solveInstanceConstraint database constraint@(WithContext (Resolve _ _ _ goal) _) = do
  case goal of
    VBuiltin NatInDomainConstraint _ -> solveIndexConstraint constraint
    VBuiltin {} -> resolveInstance database constraint
    _ -> malformedConstraintError constraint
