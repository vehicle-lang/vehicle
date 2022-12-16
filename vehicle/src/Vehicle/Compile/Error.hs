module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, throwError)
import Data.List.NonEmpty (NonEmpty)
import Prettyprinter (list)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
  ( GluedExpr,
    GluedType,
    NormBinder,
    NormExpr,
    NormType,
  )
import Vehicle.Syntax.Parse (ParseError)
import Vehicle.Verify.Core (VerifierIdentifier)

--------------------------------------------------------------------------------
-- Compilation monad

type MonadCompile m =
  ( MonadLogger m,
    MonadError CompileError m
  )

--------------------------------------------------------------------------------
-- Compilation errors

data CompileError
  = DevError (Doc ())
  | -- Parse errors
    ParseError ParseError
  | -- Errors thrown by scope checking.
    UnboundName Provenance Name
  | DuplicateName Provenance Name Identifier
  | -- Errors thrown while type checking
    UnresolvedHole
      Provenance -- The location of the hole
      Name -- The name of the hole
  | TypeMismatch
      Provenance -- The location of the mismatch.
      [DBBinding] -- The context at the time of the failure
      CheckedType -- The possible inferred types.
      CheckedType -- The expected type.
  | UnsolvedConstraints
      (NonEmpty (WithContext Constraint))
  | UnsolvedMetas
      (NonEmpty (MetaID, Provenance))
  | MissingExplicitArg
      [DBBinding] -- The context at the time of the failure
      UncheckedArg -- The non-explicit argument
      CheckedType -- Expected type of the argument
  | FailedUnificationConstraints
      (NonEmpty (WithContext UnificationConstraint))
  | FailedEqConstraint ConstraintContext NormType NormType EqualityOp
  | FailedOrdConstraint ConstraintContext NormType NormType OrderOp
  | FailedBuiltinConstraintArgument ConstraintContext Builtin NormType [Builtin] Int Int
  | FailedBuiltinConstraintResult ConstraintContext Builtin NormType [Builtin]
  | FailedNotConstraint ConstraintContext NormType
  | FailedBoolOp2Constraint ConstraintContext NormType NormType Builtin
  | FailedQuantifierConstraintDomain ConstraintContext NormType Quantifier
  | FailedQuantifierConstraintBody ConstraintContext NormType Quantifier
  | FailedArithOp2Constraint ConstraintContext NormType NormType Builtin
  | FailedFoldConstraintContainer ConstraintContext NormType
  | FailedQuantInConstraintContainer ConstraintContext NormType Quantifier
  | FailedNatLitConstraint ConstraintContext Int NormType
  | FailedNatLitConstraintTooBig ConstraintContext Int Int
  | FailedNatLitConstraintUnknown ConstraintContext Int NormType
  | FailedIntLitConstraint ConstraintContext NormType
  | FailedRatLitConstraint ConstraintContext NormType
  | FailedConLitConstraint ConstraintContext NormType
  | QuantifiedIfCondition ConstraintContext
  | NonLinearIfCondition ConstraintContext
  | -- Resource typing errors
    ResourceNotProvided DeclProvenance Resource
  | ResourceIOError DeclProvenance Resource IOException
  | UnsupportedResourceFormat DeclProvenance Resource String
  | UnableToParseResource DeclProvenance Resource String
  | NetworkTypeIsNotAFunction DeclProvenance GluedType
  | NetworkTypeIsNotOverTensors DeclProvenance GluedType NormType InputOrOutput
  | NetworkTypeHasNonExplicitArguments DeclProvenance GluedType NormBinder
  | NetworkTypeHasVariableSizeTensor DeclProvenance GluedType NormExpr InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance GluedType Identifier InputOrOutput
  | NetworkTypeHasUnsupportedElementType DeclProvenance GluedType NormType InputOrOutput
  | DatasetTypeUnsupportedContainer DeclProvenance GluedType
  | DatasetTypeUnsupportedElement DeclProvenance GluedType NormType
  | DatasetVariableSizeTensor DeclProvenance GluedType NormExpr
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int [Int] [Int]
  | DatasetDimensionsMismatch DeclProvenance FilePath GluedExpr [Int]
  | DatasetTypeMismatch DeclProvenance FilePath GluedType NormType NormType
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | ParameterTypeUnsupported DeclProvenance GluedType
  | ParameterTypeVariableSizeIndex DeclProvenance GluedType
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinConstructor
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterTypeUnsupported DeclProvenance GluedType
  | InferableParameterContradictory Identifier (DeclProvenance, Resource, Int) (DeclProvenance, Resource, Int)
  | InferableParameterUninferrable DeclProvenance
  | PropertyTypeUnsupported DeclProvenance GluedType
  | -- Backend errors
    NoPropertiesFound
  | UnsupportedResource Backend Identifier Provenance Resource
  | UnsupportedInequality Backend Identifier Provenance
  | UnsupportedPolymorphicEquality Backend Provenance Name
  | UnsupportedNonMagicVariable Backend Provenance Name
  | NoNetworkUsedInProperty Backend Provenance Identifier
  | UnsupportedVariableType VerifierIdentifier Identifier Provenance Name CheckedType [Builtin]
  | UnsupportedAlternatingQuantifiers Backend DeclProvenance Quantifier Provenance PolarityProvenance
  | UnsupportedNonLinearConstraint Backend DeclProvenance Provenance LinearityProvenance LinearityProvenance
  | UnsupportedNegatedOperation DifferentiableLogic DeclProvenance Provenance CheckedExpr
  deriving (Show)

--------------------------------------------------------------------------------
-- Some useful developer errors

unexpectedExpr :: Doc a -> Doc a -> Doc a
unexpectedExpr pass name =
  "encountered unexpected expression"
    <+> squotes name
    <+> "during"
    <+> pass <> "."

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: MonadError CompileError m => Doc () -> m b
compilerDeveloperError message = throwError $ DevError message

unexpectedExprError :: MonadError CompileError m => Doc () -> Doc () -> m b
unexpectedExprError pass name = compilerDeveloperError $ unexpectedExpr pass name

normalisationError :: MonadError CompileError m => Doc () -> Doc () -> m b
normalisationError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "We should have normalised this out."

unexpectedTypeInExprError :: MonadError CompileError m => Doc () -> Doc () -> m b
unexpectedTypeInExprError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "We should not be processing types."

illTypedError :: MonadError CompileError m => Doc () -> Doc () -> m b
illTypedError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "This is ill-typed."

visibilityError :: MonadError CompileError m => Doc () -> Doc () -> m b
visibilityError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "Should not be present as explicit arguments"

-- | Throw this when you encounter a case that should have been resolved during
-- type-checking, e.g. holes or metas.
resolutionError :: MonadError CompileError m => Doc () -> Doc () -> m b
resolutionError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "We should have resolved this during type-checking."

caseError :: MonadError CompileError m => Doc () -> Doc () -> [Doc ()] -> m b
caseError pass name cases =
  compilerDeveloperError $
    unexpectedExpr pass name
      <+> "This should already have been caught by the"
      <+> "following cases:"
      <+> list cases

internalScopingError :: MonadError CompileError m => Doc () -> Identifier -> m b
internalScopingError pass ident =
  compilerDeveloperError $
    "Internal scoping error during"
      <+> pass <> ":"
      <+> "declaration"
      <+> quotePretty ident
      <+> "not found in scope..."
