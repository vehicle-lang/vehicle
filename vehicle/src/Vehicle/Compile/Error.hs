{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error
  ( VehicleUserError (..),
    VehicleError,
    FrontendError (..),
    BackendError (..),
    ParseError (..),
    ScopeError (..),
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
    MonadFrontend,
    MonadBackend,
    BlockingReason (..),
    Diagnosis (..),
    unsupportedTensorLikeQuantifier,
  )
where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError)
import Data.Aeson (ToJSON, genericToJSON)
import Data.Aeson.Types (ToJSON (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
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
import Vehicle.Data.Code.ForcedValue (ForcedValue, Thunk, UnforcedBinder, UnforcedType)
import Vehicle.Data.Code.ForcedValue qualified as Forced
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Compilation monads

-- | The monad in which the front end of the compiler (parsing, scope-checking
-- and type-checking) is run.
type MonadFrontend m =
  ( MonadLogger m,
    MonadError FrontendError m
  )

-- | The monad in which the back end of the compiler is run.
type MonadBackend m =
  ( MonadLogger m,
    MonadError BackendError m
  )

--------------------------------------------------------------------------------
-- Parsing error

-- | Errors thrown during parsing
data ParseError
  = RawParseError String
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
  | MultipleVariablesInSingleBinder Provenance
  | UnchainableComparisons Provenance ComparisonOp ComparisonOp
  | UnknownSupportsOperation Provenance String
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Scoping errors

data RecordMatch = RecordMatch
  { sharedFields :: [FieldName],
    mispellings :: [(FieldName, FieldName)],
    missingFields :: [FieldName],
    extraFields :: [FieldName]
  }
  deriving stock (Show)

instance Pretty RecordMatch where
  pretty = pretty . show

-- | Errors thrown during scope checking
data ScopeError
  = UnboundName Provenance Name [Name]
  | UnboundRecordAccessor Provenance Name [Name]
  | DeclarationDeclarationShadowing Provenance (Either FieldName Name) Identifier
  | DeclarationBoundShadowing Provenance Name
  | UnmatchedRecord Provenance [FieldName] (Maybe (Identifier, RecordMatch))
  | -- Unsupported @tensor records
    ZeroFieldTensorLike DeclProvenance
  | ParameterisedTensorLike DeclProvenance
  deriving stock (Show)

--------------------------------------------------------------------------------
-- MultiPropertyTraveralError

data MultiPropertyTraveralError
  = UnsupportedVectorDimension (Thunk Builtin)
  | UnsupportedVectorValue (ForcedValue Builtin)
  | UnsupportedTensorDimensions (Thunk Builtin)
  | UnreducableTensorValue (ForcedValue Builtin)
  | UnreducableType (Thunk Builtin)
  deriving stock (Show)

type MissingResource = (ExternalResource, DeclProvenance)

type UninferableParameter = DeclProvenance

--------------------------------------------------------------------------------
-- Compilation errors

type ParseLocation = (ModulePath, FilePath)

-- | Errors thrown by the front end of the compiler, i.e. parsing,
-- scope-checking and type-checking.
data FrontendError
  = -- Parse errors
    ParseError ParseLocation ParseError
  | -- Scoping errors
    ScopeError ScopeError
  | -- Type checking errors. The type-checker can report multiple errors at
    -- once, and so this carries a non-empty list of them.
    forall builtin.
    (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin, Show builtin) =>
    TypingErrors (NonEmpty (TypingError builtin))
  | -- Declaration filtering errors
    MissingRequestedDeclarations (NonEmpty Name)

deriving stock instance Show FrontendError

-- | Errors thrown by the back end of the compiler.
data BackendError
  = -- Resource loading errors
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
  | -- Query backend
    NoPropertiesFound
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Diagnosis (Quantifier, Provenance, PolarityProvenance))
  | DuplicateQuantifierNames DeclProvenance Name
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Diagnosis NonLinearityProof)
  | UnsupportedMultipleNetworkApplications QueryFormatID DeclProvenance CompleteNamedBoundCtx [(NetworkName, Thunk Builtin)]
  | VariableSizeTensorQuantification DeclProvenance NamedBoundCtx (UnforcedBinder Builtin) (UnforcedType Builtin)
  | MultiPropertyTraveralError DeclProvenance MultiPropertyTraveralError
  | UnboundedNetworkInputVariables DeclProvenance CompleteNamedBoundCtx (NonEmpty (NetworkName, NetworkIOType, Thunk Builtin, [Lv], UnboundedIndices))
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

deriving stock instance Show BackendError

-- | The result of attempting to locate the original source of an error that
-- was detected during compilation, but whose root cause can only be found by
-- re-analysing the original program (e.g. with a type-checking subsystem).
data Diagnosis a
  = -- | No diagnosis has been attempted yet.
    Undiagnosed
  | -- | Diagnosis was attempted but itself failed with the given error.
    DiagnosisFailed FrontendError
  | -- | Diagnosis succeeded and found the original source.
    Diagnosed a
  deriving (Show)

data BlockingReason
  = BlockingNetwork Identifier
  | BlockingDatasetOrParameter Identifier
  | BlockingVar Lv

deriving stock instance Show BlockingReason

--------------------------------------------------------------------------------
-- The final error type

-- | Errors that are the user's responsibility to fix.
data VehicleUserError a = VehicleUserError
  { provenance :: Maybe Provenance,
    problem :: a,
    fix :: Maybe a
  }
  deriving stock (Generic)

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
