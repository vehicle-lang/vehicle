{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard
  ( module Core,
  )
where

import Data.HashMap.Strict qualified as Map
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core (malformedConstraintError)
import Vehicle.Compile.Type.Constraint.IndexSolver
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.AnnotationRestrictions
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassSolver (solveTypeClassConstraint)
import Vehicle.Compile.Type.Subsystem.Standard.Core as Core
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Expr.Normalised
import Prelude hiding (pi)

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

instance TypableBuiltin StandardBuiltin where
  convertFromStandardTypes p1 p2 t args = return $ normAppList p1 (Builtin p2 t) args
  useDependentMetas _ = True
  typeBuiltin = typeStandardBuiltin
  restrictNetworkType = restrictStandardNetworkType
  restrictDatasetType = restrictStandardDatasetType
  restrictParameterType = restrictStandardParameterType
  restrictPropertyType = restrictStandardPropertyType
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults

solveInstanceConstraint ::
  (TCM StandardBuiltin m) =>
  InstanceCandidateDatabase StandardBuiltin ->
  WithContext (InstanceConstraint StandardBuiltin) ->
  m ()
solveInstanceConstraint database constraint@(WithContext (Has _ _ goal) _) = do
  case goal of
    VBuiltin NatInDomainConstraint _ -> solveIndexConstraint constraint
    VBuiltin tc _ -> case Map.lookup tc database of
      Just {} -> resolveInstance database constraint
      Nothing -> solveTypeClassConstraint constraint
    _ -> malformedConstraintError constraint
