{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Linearity
  ( module Core,
  )
where

import Vehicle.Compile.Normalise.Builtin (Normalisable (..))
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.InputOutputInsertion
import Vehicle.Compile.Type.Subsystem.Linearity.AnnotationRestrictions (assertConstantLinearity, checkNetworkType)
import Vehicle.Compile.Type.Subsystem.Linearity.Core as Core
import Vehicle.Compile.Type.Subsystem.Linearity.LinearitySolver
import Vehicle.Compile.Type.Subsystem.Linearity.Type
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised (Value (..))
import Vehicle.Prelude
import Vehicle.Syntax.AST

instance PrintableBuiltin LinearityType where
  convertBuiltin = convertFromLinearityTypes

instance Normalisable LinearityType where
  evalBuiltin _ l spine = return $ VBuiltin l spine
  isValue = return True
  isTypeClassOp = const False
  forceBuiltin _ _ _ _ = return (Nothing, mempty)

instance TypableBuiltin LinearityBuiltin where
  convertFromStandardTypes = convertToLinearityTypes
  useDependentMetas _ = False
  typeBuiltin = typeLinearityBuiltin
  restrictNetworkType = checkNetworkType
  restrictDatasetType = assertConstantLinearity
  restrictParameterType = const assertConstantLinearity
  restrictPropertyType _ _ = return ()
  handleTypingError = handleLinearityTypingError
  typeClassRelevancy = const $ return Relevant
  solveInstance = solveLinearityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (CType . LinearityTypeClass . FunctionLinearity)
  generateDefaultConstraint = const $ return False

-------------------------------------------------------------------------------
-- Conversion

convertFromLinearityTypes :: Provenance -> LinearityType -> Expr var Builtin
convertFromLinearityTypes p b = FreeVar p $ Identifier StdLib (layoutAsText $ pretty b)
