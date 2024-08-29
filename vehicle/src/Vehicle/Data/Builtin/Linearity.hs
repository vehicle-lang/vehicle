{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Linearity
  ( module Core,
  )
where

import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class (TypableBuiltin (typeBuiltin))
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Data.Builtin.Linearity.AnnotationRestrictions (assertConstantLinearity, checkNetworkType)
import Vehicle.Data.Builtin.Linearity.Core as Core
import Vehicle.Data.Builtin.Linearity.Eval ()
import Vehicle.Data.Builtin.Linearity.LinearitySolver
import Vehicle.Data.Builtin.Linearity.Type
import Vehicle.Data.Code.Value
import Vehicle.Syntax.Builtin hiding (Builtin (..))
import Vehicle.Syntax.Builtin qualified as S

instance TypableBuiltin LinearityBuiltin where
  typeBuiltin = typeLinearityBuiltin

instance HasTypeSystem LinearityBuiltin where
  convertFromStandardBuiltins = convertToLinearityTypes
  useDependentMetas _ = False
  restrictNetworkType = checkNetworkType
  restrictDatasetType = assertConstantLinearity
  restrictParameterType = const assertConstantLinearity
  restrictPropertyType _ _ = return ()
  solveInstance = solveLinearityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (LinearityRelation . FunctionLinearity)
  generateDefaultConstraint = const $ return False

freshLinearityMeta :: (MonadTypeChecker LinearityBuiltin m) => Provenance -> m (GluedExpr LinearityBuiltin)
freshLinearityMeta p = freshMetaExpr p (TypeUniverse p 0) mempty

convertToLinearityTypes ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  BuiltinUpdate m S.Builtin LinearityBuiltin
convertToLinearityTypes p b args = case b of
  S.BuiltinFunction f -> return $ normAppList (Builtin p (BuiltinFunction f)) args
  S.BuiltinConstructor c -> return $ normAppList (Builtin p (BuiltinConstructor c)) args
  S.BuiltinType s -> case s of
    Unit -> return $ Builtin p $ Linearity Constant
    Bool -> unnormalised <$> freshLinearityMeta p
    Index -> unnormalised <$> freshLinearityMeta p
    Nat -> unnormalised <$> freshLinearityMeta p
    Rat -> unnormalised <$> freshLinearityMeta p
    List -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "List"
    Vector -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "Vector"
  S.TypeClass {} -> monomorphisationError "TypeClass"
  S.TypeClassOp {} -> monomorphisationError "TypeClassOp"
  S.NatInDomainConstraint -> monomorphisationError "IndexConstraints"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of" <+> squotes name <+> "s but found" <+> prettyVerbose args
