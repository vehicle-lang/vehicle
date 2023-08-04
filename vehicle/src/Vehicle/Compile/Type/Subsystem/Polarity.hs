{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Polarity
  ( module Core,
  )
where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Compile.Type.Subsystem.Polarity.AnnotationRestrictions (assertUnquantifiedPolarity, checkNetworkType)
import Vehicle.Compile.Type.Subsystem.Polarity.Core as Core
import Vehicle.Compile.Type.Subsystem.Polarity.PolaritySolver
import Vehicle.Compile.Type.Subsystem.Polarity.Type
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised (Value (..))
import Vehicle.Prelude
import Vehicle.Syntax.AST

instance PrintableBuiltin PolarityType where
  convertBuiltin = convertFromPolarityTypes

instance Normalisable PolarityType where
  evalBuiltin _ l spine = return $ VBuiltin l spine
  isValue = return True
  isTypeClassOp = const False
  forceBuiltin _ _ _ _ = return (Nothing, mempty)

instance TypableBuiltin PolarityBuiltin where
  convertFromStandardTypes = convertToPolarityTypes
  useDependentMetas _ = False
  typeBuiltin = typePolarityBuiltin
  restrictNetworkType = checkNetworkType
  restrictDatasetType = assertUnquantifiedPolarity
  restrictParameterType = const assertUnquantifiedPolarity
  restrictPropertyType _ _ = return ()
  handleTypingError = handlePolarityTypingError
  typeClassRelevancy = const $ return Relevant
  solveInstance = solvePolarityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (CType . PolarityTypeClass . FunctionPolarity)
  generateDefaultConstraint = const $ return False

-------------------------------------------------------------------------------
-- Conversion

convertFromPolarityTypes :: Provenance -> PolarityType -> Expr var Builtin
convertFromPolarityTypes p b = FreeVar p $ Identifier StdLib (layoutAsText $ pretty b)
