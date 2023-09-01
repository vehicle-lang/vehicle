{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Queries.Error.Linearity
  ( module Core,
  )
where

import Vehicle.Backend.Queries.Error.Linearity.AnnotationRestrictions (assertConstantLinearity, checkNetworkType)
import Vehicle.Backend.Queries.Error.Linearity.Core as Core
import Vehicle.Backend.Queries.Error.Linearity.LinearitySolver
import Vehicle.Backend.Queries.Error.Linearity.Type
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
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
  BuiltinUpdate m Ix S.Builtin LinearityBuiltin
convertToLinearityTypes p1 p2 b args = case b of
  S.BuiltinFunction f -> return $ normAppList p1 (Builtin p2 (BuiltinFunction f)) args
  S.BuiltinConstructor c -> return $ normAppList p1 (Builtin p2 (BuiltinConstructor c)) args
  S.BuiltinType s -> case s of
    Unit -> return $ Builtin p2 $ Linearity Constant
    Bool -> unnormalised <$> freshLinearityMeta p2
    Index -> unnormalised <$> freshLinearityMeta p2
    Nat -> unnormalised <$> freshLinearityMeta p2
    Int -> unnormalised <$> freshLinearityMeta p2
    Rat -> unnormalised <$> freshLinearityMeta p2
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
