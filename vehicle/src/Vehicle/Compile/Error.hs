{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Trans.Except (ExceptT)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Prettyprinter (list)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Queries.Error.Linearity.Core
import Vehicle.Backend.Queries.Error.Polarity.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.BuiltinInterface (HasStandardData, PrintableBuiltin)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Parse (ParseError)
import Vehicle.Verify.QueryFormat.Core

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
  | forall builtin.
    (PrintableBuiltin builtin, Show builtin, HasStandardData builtin) =>
    TypingError (TypingError builtin)
  | UnsolvedMetas (NonEmpty (MetaID, Provenance))
  | RelevantUseOfIrrelevantVariable Provenance Name
  | -- Resource typing errors
    ResourceNotProvided DeclProvenance ExternalResource
  | ResourceIOError DeclProvenance ExternalResource IOException
  | UnsupportedResourceFormat DeclProvenance ExternalResource String
  | UnableToParseResource DeclProvenance ExternalResource String
  | NetworkTypeIsNotAFunction DeclProvenance (GluedType StandardTypingBuiltin)
  | NetworkTypeIsNotOverTensors DeclProvenance (GluedType StandardTypingBuiltin) (VType StandardTypingBuiltin) InputOrOutput
  | NetworkTypeHasNonExplicitArguments DeclProvenance (GluedType StandardTypingBuiltin) (VBinder StandardTypingBuiltin)
  | NetworkTypeHasUnsupportedElementType DeclProvenance (GluedType StandardTypingBuiltin) (VType StandardTypingBuiltin) InputOrOutput
  | DatasetTypeUnsupportedContainer DeclProvenance (GluedType StandardTypingBuiltin)
  | DatasetTypeUnsupportedElement DeclProvenance (GluedType StandardTypingBuiltin) (VType StandardTypingBuiltin)
  | DatasetVariableSizeTensor DeclProvenance (GluedType Builtin) (VType Builtin)
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int TensorDimensions TensorDimensions
  | DatasetDimensionsMismatch DeclProvenance FilePath (GluedExpr Builtin) TensorDimensions
  | DatasetTypeMismatch DeclProvenance FilePath (GluedType Builtin) (VType Builtin) (VType Builtin)
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | ParameterTypeUnsupported DeclProvenance (GluedType StandardTypingBuiltin)
  | ParameterTypeVariableSizeIndex DeclProvenance (GluedType Builtin)
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinType
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterTypeUnsupported DeclProvenance (GluedType StandardTypingBuiltin)
  | InferableParameterContradictory Identifier (DeclProvenance, ExternalResource, Int) (DeclProvenance, ExternalResource, Int)
  | InferableParameterUninferrable DeclProvenance
  | PropertyTypeUnsupported DeclProvenance (GluedType StandardTypingBuiltin)
  | -- Unsupported networks
    NetworkTypeHasVariableSizeTensor DeclProvenance (GluedType Builtin) (VType Builtin) InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance (GluedType Builtin) Identifier InputOrOutput
  | -- Backend errors
    NoPropertiesFound
  | UnsupportedInequality QueryFormatID DeclProvenance
  | UnsupportedPolymorphicEquality ITP Provenance Name
  | NoNetworkUsedInProperty DeclProvenance
  | UnsupportedVariableType QueryFormatID Identifier Provenance Name (VType Builtin) (VType Builtin) [Builtin]
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Either CompileError (Quantifier, Provenance, PolarityProvenance))
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Either CompileError NonLinearitySource)
  | UnsupportedNegatedOperation DifferentiableLogicID Provenance
  | UnsupportedIfOperation DeclProvenance Provenance
  | DuplicateQuantifierNames DeclProvenance Name
  | QuantifiedIfCondition (ConstraintContext PolarityBuiltin)

deriving instance Show CompileError

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

outOfBoundsError :: (MonadError CompileError m) => Doc () -> GenericBoundCtx a -> Ix -> m b
outOfBoundsError pass ctx i =
  compilerDeveloperError $
    "Internal scoping error during"
      <+> pass
      <> ":"
        <+> "the bound context of length"
        <+> quotePretty (length ctx)
        <+> "is smaller than the found DB index"
        <+> pretty i

-- | Looks up the declaration associated the provided `Identifier`, throwing
-- an error if that identifier is out of scope.
lookupInDeclCtx ::
  (MonadError CompileError m) =>
  Doc () ->
  Identifier ->
  GenericFreeCtx a ->
  m a
lookupInDeclCtx pass ident ctx = case Map.lookup ident ctx of
  Nothing -> internalScopingError pass ident
  Just x -> return x

-- | Looks up the value associated with the variable given the provided `Lv`, throwing
-- an error if that level is out of scope.
lookupLvInBoundCtx ::
  (MonadError CompileError m) =>
  Doc () ->
  Lv ->
  GenericBoundCtx a ->
  m a
lookupLvInBoundCtx pass lv ctx = case lookupLv ctx lv of
  Nothing -> outOfBoundsError pass ctx (dbLevelToIndex (Lv $ length ctx) lv)
  Just x -> return x

-- | Looks up the value associated with the variable given the provided `Ix`, throwing
-- an error if that index is out of scope.
lookupIxInBoundCtx ::
  (MonadError CompileError m) =>
  Doc () ->
  Ix ->
  GenericBoundCtx a ->
  m a
lookupIxInBoundCtx pass ix ctx = case lookupIx ctx ix of
  Nothing -> outOfBoundsError pass ctx ix
  Just x -> return x
