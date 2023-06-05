{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Polarity
  ( module Core,
  )
where

import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Compile.Type.Subsystem.Polarity.AnnotationRestrictions (assertUnquantifiedPolarity, checkNetworkType)
import Vehicle.Compile.Type.Subsystem.Polarity.Core as Core
import Vehicle.Compile.Type.Subsystem.Polarity.PolaritySolver
import Vehicle.Compile.Type.Subsystem.Polarity.Type
import Vehicle.Prelude
import Vehicle.Syntax.AST

instance PrintableBuiltin PolarityType where
  convertBuiltin = convertFromPolarityTypes
  isTypeClassOp = const False

instance TypableBuiltin PolarityType where
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
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (PolarityTypeClass . FunctionPolarity)
  generateDefaultConstraint = const $ return False

-------------------------------------------------------------------------------
-- Conversion

convertFromPolarityTypes :: Provenance -> PolarityType -> Expr var Builtin
convertFromPolarityTypes p b = FreeVar p $ Identifier StdLib (layoutAsText $ pretty b)
