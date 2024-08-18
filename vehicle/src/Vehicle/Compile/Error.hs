{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, throwError)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Void (Void)
import Prettyprinter (list)
import Vehicle.Backend.LossFunction.Core (DifferentiableLogicField)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Data.Builtin.Interface (HasStandardData, PrintableBuiltin)
import Vehicle.Data.Builtin.Linearity.Core
import Vehicle.Data.Builtin.Polarity.Core
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.DeBruijn
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.QuantifiedVariable (UnderConstrainedVariableStatus, UserRationalVariable)
import Vehicle.Syntax.Parse (ParseError, ParseLocation)
import Vehicle.Verify.QueryFormat.Core

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
    ParseError ParseLocation ParseError
  | -- Errors thrown by scope checking.
    UnboundName Provenance Name
  | DeclarationDeclarationShadowing Provenance Name Identifier
  | DeclarationBoundShadowing Provenance Name
  | MissingPrunedName Name
  | -- Type checking errors
    UnresolvedHole Provenance Name
  | forall builtin.
    (PrintableBuiltin builtin, Show builtin, HasStandardData builtin) =>
    TypingError (TypingError builtin)
  | UnsolvedMetas (NonEmpty (MetaID, Provenance))
  | RelevantUseOfIrrelevantVariable Provenance Name
  | -- Resource loading errors
    ResourceNotProvided DeclProvenance ExternalResource
  | ResourceIOError DeclProvenance ExternalResource IOException
  | UnsupportedResourceFormat DeclProvenance ExternalResource String
  | UnableToParseResource DeclProvenance ExternalResource String
  | -- Unsupported networks
    NetworkTypeHasVariableSizeTensor DeclProvenance (GluedType Builtin) (WHNFType Builtin) InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance (GluedType Builtin) Identifier InputOrOutput
  | NetworkTypeIsNotAFunction DeclProvenance (GluedType Builtin)
  | NetworkTypeIsNotOverTensors DeclProvenance (GluedType Builtin) (WHNFType Builtin) InputOrOutput
  | NetworkTypeHasNonExplicitArguments DeclProvenance (GluedType Builtin) (WHNFBinder Builtin)
  | NetworkTypeHasUnsupportedElementType DeclProvenance (GluedType Builtin) (WHNFType Builtin) InputOrOutput
  | -- Unsupported datasets
    DatasetTypeUnsupportedContainer DeclProvenance (GluedType Builtin)
  | DatasetTypeUnsupportedElement DeclProvenance (GluedType Builtin) (WHNFType Builtin)
  | DatasetVariableSizeTensor DeclProvenance (GluedType Builtin) (WHNFType Builtin)
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int TensorShape TensorShape
  | DatasetDimensionsMismatch DeclProvenance FilePath (GluedExpr Builtin) TensorShape
  | DatasetTypeMismatch DeclProvenance FilePath (GluedType Builtin) (WHNFType Builtin) (Doc Void)
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | -- Unsupported parameters
    ParameterTypeUnsupported DeclProvenance (GluedType Builtin)
  | ParameterTypeVariableSizeIndex DeclProvenance (GluedType Builtin)
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinType
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterTypeUnsupported DeclProvenance (GluedType Builtin)
  | InferableParameterContradictory Identifier (DeclProvenance, ExternalResource, Int) (DeclProvenance, ExternalResource, Int)
  | InferableParameterUninferrable DeclProvenance
  | -- Unsupported properties
    PropertyTypeUnsupported DeclProvenance (GluedType Builtin)
  | NoPropertiesFound
  | -- Verification backend errors
    UnsupportedVariableType DeclProvenance Provenance Name (WHNFType Builtin) (WHNFType Builtin) [Builtin]
  | HigherOrderVectors DeclProvenance NamedBoundCtx (NFType TensorBuiltin) (NFType TensorBuiltin)
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Either CompileError (Quantifier, Provenance, PolarityProvenance))
  | DuplicateQuantifierNames DeclProvenance Name
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Either CompileError NonLinearitySource)
  | -- Loss backend errors
    UnsupportedIfOperation (Either DeclProvenance DifferentiableLogicField) Provenance
  | NoQuantifierDomainFound DeclProvenance (GenericBinder ()) (Maybe [(UserRationalVariable, UnderConstrainedVariableStatus)])
  | -- ITP backend errors
    UnsupportedPolymorphicEquality ITP Provenance Name
  | -- Other
    UnsupportedInequality QueryFormatID DeclProvenance
  | QuantifiedIfCondition (ConstraintContext PolarityBuiltin)

deriving instance Show CompileError

--------------------------------------------------------------------------------
-- Some useful developer errors

unexpectedExpr :: Doc a -> Doc a -> Doc a
unexpectedExpr pass name =
  "encountered unexpected expression:"
    <> line
    <> indent 2 name
    <> line
    <> "during"
      <+> pass
    <> "."

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: (MonadError CompileError m) => Doc () -> m b
compilerDeveloperError message = throwError $ DevError message

unexpectedExprError :: Doc () -> Doc () -> a
unexpectedExprError pass name = developerError $ unexpectedExpr pass name

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

visibilityError :: Doc () -> Doc () -> Doc () -> m b
visibilityError pass fun args =
  developerError $
    unexpectedExpr pass args <+> "Does not match function's visibility:" <+> fun

-- | Throw this when you encounter a case that should have been resolved during
-- type-checking, e.g. holes or metas.
resolutionError :: Doc () -> Doc () -> m b
resolutionError pass name =
  developerError $
    unexpectedExpr pass name <+> "We should have resolved this during type-checking."

caseError :: (MonadError CompileError m) => Doc () -> Doc () -> [Doc ()] -> m a
caseError pass name cases =
  compilerDeveloperError $
    unexpectedExpr pass name
      <+> "This should already have been caught by the"
      <+> "following cases:"
      <+> list cases

internalScopingError :: Doc () -> Identifier -> a
internalScopingError pass ident =
  developerError $
    "Internal scoping error during"
      <+> pass
      <> ":"
        <+> "declaration"
        <+> quotePretty ident
        <+> "not found in scope..."

outOfBoundsError :: Doc () -> GenericBoundCtx a -> Ix -> b
outOfBoundsError pass ctx i =
  developerError $
    "Internal scoping error during"
      <+> pass
      <> ":"
        <+> "the bound context of length"
        <+> quotePretty (length ctx)
        <+> "is smaller than the found DB index"
        <+> pretty i

-- | Looks up the declaration associated the provided `Identifier`, throwing
-- an error if that identifier is out of scope.
lookupInFreeCtx ::
  (MonadLogger m) =>
  Doc () ->
  Identifier ->
  GenericFreeCtx a ->
  m a
lookupInFreeCtx pass ident ctx = case Map.lookup ident ctx of
  Nothing -> internalScopingError pass ident
  Just x -> return x

-- | Looks up the value associated with the variable given the provided `Lv`, throwing
-- an error if that level is out of scope.
lookupLvInBoundCtx ::
  Doc () ->
  Lv ->
  GenericBoundCtx a ->
  a
lookupLvInBoundCtx pass lv ctx = case lookupLv ctx lv of
  Nothing -> outOfBoundsError pass ctx (dbLevelToIndex (Lv $ length ctx) lv)
  Just x -> x

-- | Looks up the value associated with the variable given the provided `Ix`, throwing
-- an error if that index is out of scope.
lookupIxInBoundCtx ::
  Doc () ->
  Ix ->
  GenericBoundCtx a ->
  a
lookupIxInBoundCtx pass ix ctx = case lookupIx ctx ix of
  Nothing -> outOfBoundsError pass ctx ix
  Just x -> x
