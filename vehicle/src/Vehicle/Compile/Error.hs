module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Trans.Except (ExceptT)
import Data.List.NonEmpty (NonEmpty)
import Prettyprinter (list)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Linearity.Core
import Vehicle.Compile.Type.Subsystem.Polarity.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable (NormalisableArg)
import Vehicle.Syntax.Parse (ParseError)
import Vehicle.Verify.Core (QueryFormatID)

--------------------------------------------------------------------------------
-- Compilation monad

type MonadCompile m =
  ( MonadLogger m,
    MonadError CompileError m
  )

runCompileMonadSilently :: Doc x -> ExceptT CompileError SilentLogger a -> a
runCompileMonadSilently action v = do
  let r = runSilentLogger $ runExceptT v
  case r of
    Left err -> developerError $ "Error thrown while" <+> action <> ":" <+> pretty (show err)
    Right s -> s

--------------------------------------------------------------------------------
-- Compilation errors

data CompileError
  = DevError (Doc ())
  | -- Parse errors
    ParseError ParseError
  | -- Command line option errors
    InvalidPrunedName Name
  | -- Errors thrown by scope checking.
    UnboundName Provenance Name
  | DeclarationDeclarationShadowing Provenance Name Identifier
  | DeclarationBoundShadowing Provenance Name
  | -- Errors thrown while type checking
    UnresolvedHole Provenance Name
  | FunTypeMismatch
      Provenance -- The location of the mismatch.
      BoundDBCtx -- The context at the time of the failure
      StandardExpr -- The function being typed
      StandardType -- The possible inferred types.
      StandardType -- The expected type.
  | MissingExplicitArg
      BoundDBCtx -- The context at the time of the failure
      (NormalisableArg StandardBuiltinType) -- The non-explicit argument
      StandardType -- Expected type of the argument
  | UnsolvedConstraints (NonEmpty (WithContext StandardConstraint))
  | UnsolvedMetas (NonEmpty (MetaID, Provenance))
  | FailedUnificationConstraints (NonEmpty (WithContext StandardUnificationConstraint))
  | FailedEqConstraint StandardConstraintContext StandardNormType StandardNormType EqualityOp
  | FailedOrdConstraint StandardConstraintContext StandardNormType StandardNormType OrderOp
  | FailedBuiltinConstraintArgument StandardConstraintContext TypeClassOp StandardNormType [UnAnnDoc] Int Int
  | FailedBuiltinConstraintResult StandardConstraintContext StandardBuiltin StandardNormType [UnAnnDoc]
  | FailedNotConstraint StandardConstraintContext StandardNormType
  | FailedBoolOp2Constraint StandardConstraintContext StandardNormType StandardNormType StandardBuiltin
  | FailedQuantifierConstraintDomain StandardConstraintContext StandardNormType Quantifier
  | FailedQuantifierConstraintBody StandardConstraintContext StandardNormType Quantifier
  | FailedArithOp2Constraint StandardConstraintContext StandardNormType StandardNormType StandardBuiltin
  | FailedFoldConstraintContainer StandardConstraintContext StandardNormType
  | FailedQuantInConstraintContainer StandardConstraintContext StandardNormType Quantifier
  | FailedNatLitConstraint StandardConstraintContext Int StandardNormType
  | FailedNatLitConstraintTooBig StandardConstraintContext Int Int
  | FailedNatLitConstraintUnknown StandardConstraintContext StandardNormExpr StandardNormType
  | FailedIntLitConstraint StandardConstraintContext StandardNormType
  | FailedRatLitConstraint StandardConstraintContext StandardNormType
  | FailedConLitConstraint StandardConstraintContext StandardNormType
  | FailedInstanceConstraint StandardConstraintContext InstanceGoal [WithContext InstanceCandidate]
  | QuantifiedIfCondition PolarityConstraintContext
  | NonLinearIfCondition LinearityConstraintContext
  | -- Resource typing errors
    ResourceNotProvided DeclProvenance ExternalResource
  | ResourceIOError DeclProvenance ExternalResource IOException
  | UnsupportedResourceFormat DeclProvenance ExternalResource String
  | UnableToParseResource DeclProvenance ExternalResource String
  | NetworkTypeIsNotAFunction DeclProvenance StandardGluedType
  | NetworkTypeIsNotOverTensors DeclProvenance StandardGluedType StandardNormType InputOrOutput
  | NetworkTypeHasNonExplicitArguments DeclProvenance StandardGluedType StandardNormBinder
  | NetworkTypeHasVariableSizeTensor DeclProvenance StandardGluedType StandardNormType InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance StandardGluedType Identifier InputOrOutput
  | NetworkTypeHasUnsupportedElementType DeclProvenance StandardGluedType StandardNormType InputOrOutput
  | DatasetTypeUnsupportedContainer DeclProvenance StandardGluedType
  | DatasetTypeUnsupportedElement DeclProvenance StandardGluedType StandardNormType
  | DatasetVariableSizeTensor DeclProvenance StandardGluedType StandardNormType
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int TensorDimensions TensorDimensions
  | DatasetDimensionsMismatch DeclProvenance FilePath StandardGluedExpr TensorDimensions
  | DatasetTypeMismatch DeclProvenance FilePath StandardGluedType StandardNormType StandardNormType
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | ParameterTypeUnsupported DeclProvenance StandardGluedType
  | ParameterTypeVariableSizeIndex DeclProvenance StandardGluedType
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinType
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterTypeUnsupported DeclProvenance StandardGluedType
  | InferableParameterContradictory Identifier (DeclProvenance, ExternalResource, Int) (DeclProvenance, ExternalResource, Int)
  | InferableParameterUninferrable DeclProvenance
  | PropertyTypeUnsupported DeclProvenance StandardGluedType
  | -- Backend errors
    NoPropertiesFound
  | UnsupportedInequality QueryFormatID DeclProvenance
  | UnsupportedPolymorphicEquality ITP Provenance Name
  | NoNetworkUsedInProperty DeclProvenance
  | UnsupportedVariableType QueryFormatID Identifier Provenance Name StandardNormType StandardNormType [Builtin]
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance Quantifier Provenance PolarityProvenance
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance Provenance LinearityProvenance LinearityProvenance
  | UnsupportedNegatedOperation DifferentiableLogic Provenance (Expr Name StandardBuiltin)
  | DuplicateQuantifierNames DeclProvenance Name
  deriving (Show)

--------------------------------------------------------------------------------
-- Some useful developer errors

unexpectedExpr :: Doc a -> Doc a -> Doc a
unexpectedExpr pass name =
  "encountered unexpected expression"
    <+> squotes name
    <+> "during"
    <+> pass
    <> "."

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: (MonadError CompileError m) => Doc () -> m b
compilerDeveloperError message = throwError $ DevError message

unexpectedExprError :: (MonadError CompileError m) => Doc () -> Doc () -> m b
unexpectedExprError pass name = compilerDeveloperError $ unexpectedExpr pass name

normalisationError :: (MonadError CompileError m) => Doc () -> Doc () -> m b
normalisationError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "We should have normalised this out."

unexpectedTypeInExprError :: (MonadError CompileError m) => Doc () -> Doc () -> m b
unexpectedTypeInExprError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "We should not be processing types."

illTypedError :: (MonadError CompileError m) => Doc () -> Doc () -> m b
illTypedError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "This is ill-typed."

visibilityError :: (MonadError CompileError m) => Doc () -> Doc () -> m b
visibilityError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "Should not be present as explicit arguments"

-- | Throw this when you encounter a case that should have been resolved during
-- type-checking, e.g. holes or metas.
resolutionError :: (MonadError CompileError m) => Doc () -> Doc () -> m b
resolutionError pass name =
  compilerDeveloperError $
    unexpectedExpr pass name <+> "We should have resolved this during type-checking."

caseError :: (MonadError CompileError m) => Doc () -> Doc () -> [Doc ()] -> m b
caseError pass name cases =
  compilerDeveloperError $
    unexpectedExpr pass name
      <+> "This should already have been caught by the"
      <+> "following cases:"
      <+> list cases

internalScopingError :: (MonadError CompileError m) => Doc () -> Identifier -> m b
internalScopingError pass ident =
  compilerDeveloperError $
    "Internal scoping error during"
      <+> pass
      <> ":"
        <+> "declaration"
        <+> quotePretty ident
        <+> "not found in scope..."
