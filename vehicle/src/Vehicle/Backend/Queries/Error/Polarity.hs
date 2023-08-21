{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Queries.Error.Polarity
  ( module Core,
  )
where

import Vehicle.Backend.Queries.Error.Polarity.AnnotationRestrictions (assertUnquantifiedPolarity, checkNetworkType)
import Vehicle.Backend.Queries.Error.Polarity.Core
import Vehicle.Backend.Queries.Error.Polarity.Core as Core hiding (BuiltinFunction)
import Vehicle.Backend.Queries.Error.Polarity.PolaritySolver
import Vehicle.Backend.Queries.Error.Polarity.Type
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Builtin hiding (Builtin (..))
import Vehicle.Syntax.Builtin qualified as S

instance PrintableBuiltin PolarityBuiltin where
  convertBuiltin = convertFromPolarityTypes
  isCoercion = const False

instance TypableBuiltin PolarityBuiltin where
  typeBuiltin = typePolarityBuiltin

instance HasTypeSystem PolarityBuiltin where
  convertFromStandardBuiltins = convertToPolarityTypes
  useDependentMetas _ = False
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
freshPolarityMeta p = freshMetaExpr p (TypeUniverse p 0) mempty

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
  S.TypeClass {} -> monomorphisationError "TypeClass"
  S.TypeClassOp {} -> monomorphisationError "TypeClassOp"
  S.NatInDomainConstraint -> monomorphisationError "IndexConstraints"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of partially applied" <+> name <+> "types but found" <+> prettyVerbose args
