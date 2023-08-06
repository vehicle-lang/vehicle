{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard
  ( module Core,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.HashMap.Strict as HashMap
import Data.Monoid (Endo (..), appEndo)
import Data.Text (pack)
import Vehicle.Compile.Error (CompileError (..), MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core (malformedConstraintError)
import Vehicle.Compile.Type.Constraint.IndexSolver
import Vehicle.Compile.Type.Constraint.InstanceSolver
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.AnnotationRestrictions
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.InstanceBuiltins
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassDefaults
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.TypeClassSolver (solveTypeClassConstraint)
import Vehicle.Compile.Type.Subsystem.Standard.Core as Core
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Expr.DSL
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
  handleTypingError = handleStandardTypingError
  solveInstance = solveInstanceConstraint
  addAuxiliaryInputOutputConstraints = return
  generateDefaultConstraint = addNewConstraintUsingDefaults

handleStandardTypingError :: (MonadCompile m) => TypingError StandardBuiltin -> m a
handleStandardTypingError = \case
  MissingExplicitArgument boundCtx expectedBinder actualArg ->
    throwError $ MissingExplicitArg (boundContextOf boundCtx) actualArg (typeOf expectedBinder)
  FunctionTypeMismatch boundCtx fun originalArgs nonPiType args -> do
    let p = provenanceOf fun
    let mkRes =
          [ Endo $ \tRes -> pi Nothing (visibilityOf arg) (relevanceOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
            | (i, arg) <- zip [0 :: Int ..] args
          ]
    let expectedType = fromDSL p (appEndo (mconcat mkRes) (tHole "res"))
    let currentFun = normAppList p fun (take (length args) originalArgs)
    throwError $ FunTypeMismatch p (boundContextOf boundCtx) currentFun nonPiType expectedType
  FailedUnification constraints ->
    throwError (FailedUnificationConstraints constraints)
  FailedInstanceSearch ctx goal candidates ->
    throwError (FailedInstanceConstraint ctx goal candidates)
  UnsolvableConstraints constraints ->
    throwError $ UnsolvedConstraints constraints
  FailedIndexConstraintTooBig ctx m n ->
    throwError $ FailedNatLitConstraintTooBig ctx m n
  FailedIndexConstraintUnknown ctx e t ->
    throwError $ FailedNatLitConstraintUnknown ctx e t

solveInstanceConstraint ::
  (TCM StandardBuiltin m) =>
  WithContext (InstanceConstraint StandardBuiltin) ->
  m ()
solveInstanceConstraint constraint@(WithContext (Has _ _ goal) _) = do
  case goal of
    VBuiltin NatInDomainConstraint _ -> solveIndexConstraint constraint
    VBuiltin tc _ -> case HashMap.lookup tc builtinInstances of
      Just {} -> resolveInstance builtinInstances constraint
      Nothing -> solveTypeClassConstraint constraint
    _ -> malformedConstraintError constraint
