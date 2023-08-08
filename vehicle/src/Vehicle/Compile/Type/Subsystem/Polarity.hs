{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Polarity
  ( module Core,
  )
where

import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class (freshMeta)
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Compile.Type.Subsystem.Polarity.AnnotationRestrictions (assertUnquantifiedPolarity, checkNetworkType)
import Vehicle.Compile.Type.Subsystem.Polarity.Core
import Vehicle.Compile.Type.Subsystem.Polarity.Core as Core hiding (BuiltinFunction)
import Vehicle.Compile.Type.Subsystem.Polarity.PolaritySolver
import Vehicle.Compile.Type.Subsystem.Polarity.Type
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Builtin hiding (Builtin (..))
import Vehicle.Syntax.Builtin qualified as S

instance PrintableBuiltin PolarityBuiltin where
  convertBuiltin = convertFromPolarityTypes

instance TypableBuiltin PolarityBuiltin where
  convertFromStandardTypes = convertToPolarityTypes
  useDependentMetas _ = False
  typeBuiltin = typePolarityBuiltin
  restrictNetworkType = checkNetworkType
  restrictDatasetType = assertUnquantifiedPolarity
  restrictParameterType = const assertUnquantifiedPolarity
  restrictPropertyType _ _ = return ()
  solveInstance = solvePolarityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (PolarityRelation . FunctionPolarity)
  generateDefaultConstraint = const $ return False

-------------------------------------------------------------------------------
-- Conversion

convertFromPolarityTypes :: Provenance -> PolarityBuiltin -> Expr var S.Builtin
convertFromPolarityTypes p = \case
  BuiltinConstructor c -> Builtin p (S.BuiltinConstructor c)
  BuiltinFunction f -> Builtin p (S.BuiltinFunction f)
  b -> FreeVar p $ Identifier StdLib (layoutAsText $ pretty b)

freshPolarityMeta :: (MonadTypeChecker PolarityBuiltin m) => Provenance -> m (GluedExpr PolarityBuiltin)
freshPolarityMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty

convertToPolarityTypes ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  BuiltinUpdate m Ix S.Builtin PolarityBuiltin
convertToPolarityTypes p1 p2 b args = case b of
  S.BuiltinConstructor c -> return $ normAppList p1 (Builtin p2 (BuiltinConstructor c)) args
  S.BuiltinFunction f -> return $ normAppList p1 (Builtin p2 (BuiltinFunction f)) args
  S.BuiltinType s -> case s of
    Unit -> return $ PolarityExpr p2 Unquantified
    Bool -> unnormalised <$> freshPolarityMeta p2
    Index -> return $ PolarityExpr p2 Unquantified
    Nat -> return $ PolarityExpr p2 Unquantified
    Int -> return $ PolarityExpr p2 Unquantified
    Rat -> unnormalised <$> freshPolarityMeta p2
    List -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "List"
    Vector -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "Vector"
  S.TypeClass {} ->
    monomorphisationError "TypeClass"
  S.TypeClassOp {} ->
    compilerDeveloperError "Type class operations should have been resolved before converting to other type systems"
  S.NatInDomainConstraint -> monomorphisationError "TypeClass"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of partially applied" <+> name <+> "types but found" <+> prettyVerbose args
