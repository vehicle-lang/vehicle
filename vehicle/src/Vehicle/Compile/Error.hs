{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error
  ( VehicleUserError (..),
    VehicleError,
    CompileError (..),
    ParseError (..),
    TypingError (..),
    MultiPropertyTraveralError (..),
    RecordMatch (..),
    MissingExplicitArgError (..),
    RelevantUseOfIrrelevantVariableError (..),
    FunctionTypeMismatchError (..),
    FailedInstanceConstraintError (..),
    FailedUnificationConstraintsError (..),
    MissingResource,
    UninferableParameter,
    UnboundedIndices,
    ParseLocation,
    MonadCompile,
    BlockingReason (..),
    compilerDeveloperError,
    unsupportedTensorLikeQuantifier,
  )
where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (ToJSON, genericToJSON)
import Data.Aeson.Types (ToJSON (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Typeable (Proxy)
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkIOType, NetworkName)
import Vehicle.Compile.Type.Core
import Vehicle.Data.Bound (UnboundedIndices)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue (ForcedValue, Thunk, ThunkWithMetas, UnforcedBinder, UnforcedType, UnforcedTypeWithMetas)
import Vehicle.Data.Code.ForcedValue qualified as Forced
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Compilation monad

type MonadCompile m =
  ( MonadLogger m,
    MonadError CompileError m
  )

--------------------------------------------------------------------------------
-- Scoping errors

data RecordMatch = RecordMatch
  { sharedFields :: [FieldName],
    mispellings :: [(FieldName, FieldName)],
    missingFields :: [FieldName],
    extraFields :: [FieldName]
  }
  deriving (Show)

instance Pretty RecordMatch where
  pretty = pretty . show

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

data FailedUnificationConstraintsError builtin = FailedUnificationConstraintsError
  { _freeCtx :: FreeCtx builtin,
    failedConstraints :: NonEmpty (WithContext (UnificationConstraint builtin))
  }
  deriving (Show)

data FailedInstanceConstraintError builtin = FailedInstanceConstraintError
  { _freeCtx :: FreeCtx builtin,
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
  | FailedIndexConstraintUnknown (ConstraintContext builtin) (ThunkWithMetas builtin) (UnforcedTypeWithMetas builtin)
  | UnsolvedConstraints (NonEmpty (WithContext (Constraint builtin)))
  | UnsolvedMetas (Proxy builtin) (NonEmpty (MetaID, Provenance))
  | InvalidInstanceHead DeclProvenance (Expr builtin)
  | NonTypeClassInstanceHead (Proxy builtin) DeclProvenance Identifier
  deriving (Show)

--------------------------------------------------------------------------------
-- MultiPropertyTraveralError

data MultiPropertyTraveralError
  = UnsupportedVectorDimension (Thunk Builtin)
  | UnsupportedVectorValue (ForcedValue Builtin)
  | UnsupportedTensorDimensions (Thunk Builtin)
  | UnreducableTensorValue (ForcedValue Builtin)
  | UnreducableType (Thunk Builtin)
  deriving (Show)

type MissingResource = (ExternalResource, DeclProvenance)

type UninferableParameter = DeclProvenance

--------------------------------------------------------------------------------
-- Sugaring error

data ParseError
  = -- Parse errors
    RawParseError String
  | FunctionWithMismatchedNames Provenance Identifier Identifier
  | -- Annotations
    UnannotatedAbstractDef Provenance Identifier
  | MultiplyAnnotatedDef Provenance Identifier (Doc Void) (Doc Void)
  | TypeDefWithAnnotation Provenance Identifier (Doc Void)
  | FunctionDefWithRecordAnnotation Provenance Identifier (Doc Void)
  | RecordDefWithFunctionAnnotation Provenance Identifier (Doc Void)
  | AbstractDefWithNonAbstractAnnotation Provenance Identifier (Doc Void)
  | NonAbstractDefWithAbstractAnnotation Provenance Identifier (Doc Void)
  | AnnotationWithNoDef Provenance Name
  | -- Annotation options
    InvalidAnnotationOption Provenance Name Name [Name]
  | InvalidAnnotationOptionValue Provenance Name Text
  | MissingAnnotationOption Provenance Text Name
  | DuplicateAnnotationOption Provenance Text Name
  | -- Other
    UnknownBuiltin Provenance Text
  | MissingVariables Provenance Name
  | UnchainableComparisons Provenance ComparisonOp ComparisonOp
  | UnknownSupportsOperation Provenance String
  deriving (Show)

--------------------------------------------------------------------------------
-- Compilation errors

type ParseLocation = (ModulePath, FilePath)

data CompileError
  = DevError UnAnnDoc
  | -- Parse errors
    ParseError ParseLocation ParseError
  | -- Scoping errors.
    UnboundName Provenance Name [Name]
  | UnboundRecordAccessor Provenance Name [Name]
  | DeclarationDeclarationShadowing Provenance (Either FieldName Name) Identifier
  | DeclarationBoundShadowing Provenance Name
  | MissingRequestedDeclarations (NonEmpty Name)
  | UnmatchedRecord Provenance [FieldName] (Maybe (Identifier, RecordMatch))
  | -- Type checking errors
    forall builtin.
    (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin, Show builtin) =>
    TypingError (TypingError builtin)
  | -- Resource loading errors
    ResourcesNotProvided (NonEmpty MissingResource)
  | ResourceIOError DeclProvenance ExternalResource IOException
  | UnsupportedResourceFormat DeclProvenance ExternalResource String
  | UnableToParseResource DeclProvenance ExternalResource String
  | -- Unsupported networks
    NetworkTypeHasVariableSizeTensor DeclProvenance (Forced.GluedType Builtin) (UnforcedType Builtin) InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance (Forced.GluedType Builtin) Identifier InputOrOutput
  | -- Unsupported datasets
    DatasetVariableSizeTensor DeclProvenance (Forced.GluedType Builtin) (Thunk Builtin)
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int Int
  | DatasetDimensionsMismatch DeclProvenance FilePath (Forced.GluedType Builtin) TensorShape
  | DatasetTypeVariableSizeIndex DeclProvenance (Forced.GluedType Builtin) (ForcedValue Builtin)
  | DatasetTypeMismatch DeclProvenance FilePath (Forced.GluedType Builtin) (Thunk Builtin) (Doc Void)
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | -- Unsupported parameters
    ParameterTypeVariableSizeIndex DeclProvenance (Forced.GluedType Builtin) (ForcedValue Builtin)
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinType
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterContradictory Identifier (DeclProvenance, ExternalResource, Int) (DeclProvenance, ExternalResource, Int)
  | InferableParametersUninferrable (NonEmpty UninferableParameter)
  | -- Unsupported tensor record
    ZeroFieldTensorLike DeclProvenance
  | -- Query backend
    NoPropertiesFound
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Either CompileError (Quantifier, Provenance, PolarityProvenance))
  | DuplicateQuantifierNames DeclProvenance Name
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Either CompileError NonLinearityProof)
  | UnsupportedMultipleNetworkApplications QueryFormatID DeclProvenance CompleteNamedBoundCtx [(NetworkName, Thunk Builtin)]
  | VariableSizeTensorQuantification DeclProvenance NamedBoundCtx (UnforcedBinder Builtin) (UnforcedType Builtin)
  | MultiPropertyTraveralError DeclProvenance MultiPropertyTraveralError
  | UnboundedNetworkInputVariables DeclProvenance CompleteNamedBoundCtx (NonEmpty (NetworkName, NetworkIOType, Thunk Builtin, [Lv], UnboundedIndices))
  | QuantifiedIfCondition (ConstraintContext PolarityBuiltin)
  | -- Loss backend errors
    UnknownDifferentiableLogic Name [Name]
  | UnreducableDifferentiableLogic DeclProvenance
  | UnsupportedLossOperation DeclProvenance (Maybe Provenance) (Doc Void)
  | UnsupportedIfLossOperation Provenance
  | UnorderableDifferentiableLogic DeclProvenance (Thunk Builtin) (Either BlockingReason (ForcedValue Builtin))
  | BackwardsDifferentiableLogic DeclProvenance (Thunk Builtin)
  | UnableToLiftQuantifiersInProperty DeclProvenance
  | QuantifierWithNoGradients Provenance (Binder Builtin)
  | -- ITP backend errors
    UnimplementedFeature Provenance (Doc Void)
  | -- Other
    UnsupportedInequality QueryFormatID DeclProvenance

deriving instance Show CompileError

data BlockingReason
  = BlockingNetwork Identifier
  | BlockingDatasetOrParameter Identifier
  | BlockingVar Lv

deriving instance Show BlockingReason

--------------------------------------------------------------------------------
-- Some useful developer errors

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: (MonadError CompileError m) => UnAnnDoc -> m b
compilerDeveloperError message = throwError $ DevError message

--------------------------------------------------------------------------------
-- The final error type

-- | Errors that are the user's responsibility to fix.
data VehicleUserError a = VehicleUserError
  { provenance :: Maybe Provenance,
    problem :: a,
    fix :: Maybe a
  }
  deriving (Generic)

type VehicleError = VehicleUserError UnAnnDoc

instance Pretty (VehicleUserError UnAnnDoc) where
  pretty VehicleUserError {..} =
    unAnnotate $
      "Error"
        <> maybe "" (\p -> " in" <+> pretty p) provenance
        <> ":"
          <+> problem
        <> maybe "" (\f -> line <> "Fix:" <+> f) fix

instance ToJSON (VehicleUserError UnAnnDoc) where
  toJSON VehicleUserError {..} =
    genericToJSON jsonOptions $
      VehicleUserError
        { provenance = provenance,
          problem = layoutAsText problem,
          fix = fmap layoutAsText fix
        }

-- developer error for unsupported tensorLike quantification
unsupportedTensorLikeQuantifier :: forall b. (HasCallStack) => b
unsupportedTensorLikeQuantifier = developerError "Quantification over TensorLikes is unsupported."
