{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, throwError)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Typeable (Proxy)
import Data.Void (Void)
import Prettyprinter (list)
import Vehicle.Backend.LossFunction.Core (BooleanDifferentiableLogicField, TensorDifferentiableLogicField)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyFriendly)
import Vehicle.Compile.Print.Builtin
import Vehicle.Compile.Type.Core
import Vehicle.Data.Assertion (UnderConstrainedVariableStatus)
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Syntax.Parse (ParseError, ParseLocation)
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Compilation monad

type MonadCompile m =
  ( MonadLogger m,
    MonadError CompileError m
  )

--------------------------------------------------------------------------------
-- Typing errors

data MissingExplicitArgError builtin = MissingExplicitArgError
  { _ctx :: NamedBoundCtx,
    explicitBinder :: Binder builtin,
    nonExplicitArg :: Arg builtin
  }
  deriving (Show)

data RelevantUseOfIrrelevantVariableError builtin = RelevantUseOfIrrelevantVariableError
  { _proxy :: Proxy builtin,
    _provenance :: Provenance,
    irrelevantVariableName :: Name
  }
  deriving (Show)

data FunctionTypeMismatchError builtin = FunctionTypeMismatchError
  { _ctx :: NamedBoundCtx,
    originalFunction :: Expr builtin,
    currentExpectedType :: Expr builtin,
    currentUncheckedArgs :: [Arg builtin]
  }
  deriving (Show)

newtype FailedUnificationConstraintsError builtin = FailedUnificationConstraintsError
  { failedConstraints :: NonEmpty (WithContext (UnificationConstraint builtin))
  }
  deriving (Show)

data FailedInstanceConstraintError builtin = FailedInstanceConstraintError
  { _freeEnv :: FreeEnv builtin,
    failedConstraint :: WithContext (InstanceConstraint builtin),
    exploredCandidates :: [(WithContext (InstanceCandidate builtin), UnAnnDoc)]
  }
  deriving (Show)

-- | Errors thrown during type-checking
data TypingError builtin
  = MissingExplicitArg (MissingExplicitArgError builtin)
  | FunctionTypeMismatch (FunctionTypeMismatchError builtin)
  | RelevantUseOfIrrelevantVariable (RelevantUseOfIrrelevantVariableError builtin)
  | FailedUnificationConstraints (FailedUnificationConstraintsError builtin)
  | FailedInstanceConstraint (FailedInstanceConstraintError builtin)
  | FailedIndexConstraintTooBig (ConstraintContext builtin) Int Int
  | FailedIndexConstraintUnknown (ConstraintContext builtin) (Value builtin) (VType builtin)
  | UnsolvedConstraints (NonEmpty (WithContext (Constraint builtin)))
  | UnsolvedMetas (Proxy builtin) (NonEmpty (MetaID, Provenance))
  deriving (Show)

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
    forall builtin.
    (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin, Show builtin) =>
    TypingError (TypingError builtin)
  | -- Resource loading errors
    ResourceNotProvided DeclProvenance ExternalResource
  | ResourceIOError DeclProvenance ExternalResource IOException
  | UnsupportedResourceFormat DeclProvenance ExternalResource String
  | UnableToParseResource DeclProvenance ExternalResource String
  | -- Unsupported networks
    NetworkTypeHasVariableSizeTensor DeclProvenance (GluedType Builtin) (VType Builtin) InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance (GluedType Builtin) Identifier InputOrOutput
  | -- Unsupported datasets
    DatasetVariableSizeTensor DeclProvenance (GluedType Builtin) (VType Builtin)
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int TensorShape TensorShape
  | DatasetDimensionsMismatch DeclProvenance FilePath (GluedExpr Builtin) TensorShape
  | DatasetTypeMismatch DeclProvenance FilePath (GluedType Builtin) (VType Builtin) (Doc Void)
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | -- Unsupported parameters
    ParameterTypeVariableSizeIndex DeclProvenance (GluedType Builtin)
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinType
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterContradictory Identifier (DeclProvenance, ExternalResource, Int) (DeclProvenance, ExternalResource, Int)
  | InferableParameterUninferrable DeclProvenance
  | -- Unsupported properties
    NoPropertiesFound
  | -- Verification backend errors
    forall builtin.
    (PrintableBuiltin builtin, Eq builtin, PrettyFriendly (Contextualised (VType builtin) NamedBoundCtx)) =>
    UnsupportedVariableType DeclProvenance Provenance Name (VType builtin) (VType builtin) [builtin]
  | HigherOrderVectors DeclProvenance NamedBoundCtx (VType TensorBuiltin) (VType TensorBuiltin)
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Either CompileError (Quantifier, Provenance, PolarityProvenance))
  | DuplicateQuantifierNames DeclProvenance Name
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Either CompileError NonLinearitySource)
  | -- Loss backend errors
    UnsupportedLossOperation DeclProvenance Provenance (Doc Void)
  | UnsupportedHigherOrderTensorCode DeclProvenance NamedBoundCtx (Value Builtin) NamedBoundCtx (Value TensorBuiltin)
  | UnableToLiftLogicFieldToTensors DifferentiableLogicID TensorDifferentiableLogicField (BooleanDifferentiableLogicField, Value Builtin) NamedBoundCtx (Value Builtin)
  | NoQuantifierDomainFound DeclProvenance (GenericBinder ()) (Maybe [(Name, UnderConstrainedVariableStatus)])
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

--------------------------------------------------------------------------------
-- The final error type

-- | Errors from external code that we have no control over.
--  These may be either user or developer errors but in general we
--  can't distinguish between the two.
newtype ExternalError = ExternalError Text

-- | Errors that are the user's responsibility to fix.
data UserError = UserError
  { provenance :: Provenance,
    problem :: UnAnnDoc,
    fix :: Maybe UnAnnDoc
  }

data VehicleError
  = UError UserError
  | EError ExternalError
  | DError (Doc ())

instance Pretty VehicleError where
  pretty (UError (UserError p prob probFix)) =
    unAnnotate $
      "Error in"
        <+> pretty p
        <> ":"
          <+> prob
        <> maybe "" (\fix -> line <> fixText fix) probFix
  pretty (EError (ExternalError text)) = pretty text
  pretty (DError text) = unAnnotate text

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t
