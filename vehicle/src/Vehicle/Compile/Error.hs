{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error
  ( VehicleError (..),
    CompileError (..),
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
    MonadCompile,
    compilerDeveloperError,
  )
where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (ToJSON (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.These (These)
import Data.Typeable (Proxy)
import Data.Void (Void)
import GHC.Generics (Generic)
import Vehicle.Backend.Loss.Logics (BooleanDifferentiableLogicField, TensorDifferentiableLogicField)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkName)
import Vehicle.Compile.Type.Core
import Vehicle.Data.Bound (UnboundedIndices)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, TensorShape)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Syntax.Parse (ParseError, ParseLocation)
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
  { _freeEnv :: FreeEnv builtin,
    failedConstraints :: NonEmpty (WithContext (UnificationConstraint builtin))
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
-- MultiPropertyTraveralError

data MultiPropertyTraveralError
  = UnsupportedVectorDimension (Value Builtin)
  | UnsupportedVectorValue (Value Builtin)
  | UnsupportedTensorDimensions (Value Builtin)
  | UnreducableTensorValue (Value Builtin)
  | UnreducableType (VType Builtin)
  deriving (Show)

type MissingResource = (ExternalResource, DeclProvenance)

type UninferableParameter = DeclProvenance

--------------------------------------------------------------------------------
-- Compilation errors

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
    NetworkTypeHasVariableSizeTensor DeclProvenance (GluedType Builtin) (VType Builtin) InputOrOutput
  | NetworkTypeHasImplicitSizeTensor DeclProvenance (GluedType Builtin) Identifier InputOrOutput
  | -- Unsupported datasets
    DatasetVariableSizeTensor DeclProvenance (GluedType Builtin) (VType Builtin)
  | DatasetDimensionSizeMismatch DeclProvenance FilePath Int Int Int
  | DatasetDimensionsMismatch DeclProvenance FilePath (GluedExpr Builtin) TensorShape
  | DatasetTypeMismatch DeclProvenance FilePath (GluedType Builtin) (VType Builtin) (Doc Void)
  | DatasetInvalidIndex DeclProvenance FilePath Int Int
  | DatasetInvalidNat DeclProvenance FilePath Int
  | -- Unsupported parameters
    ParameterTypeVariableSizeIndex DeclProvenance (GluedType Builtin) (Value Builtin)
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable DeclProvenance String BuiltinType
  | ParameterValueInvalidIndex DeclProvenance Int Int
  | ParameterValueInvalidNat DeclProvenance Int
  | InferableParameterContradictory Identifier (DeclProvenance, ExternalResource, Int) (DeclProvenance, ExternalResource, Int)
  | InferableParametersUninferrable (NonEmpty UninferableParameter)
  | -- Query backend
    NoPropertiesFound
  | HigherOrderVectors DeclProvenance NamedBoundCtx (VType Builtin) (VType Builtin)
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Either CompileError (Quantifier, Provenance, PolarityProvenance))
  | DuplicateQuantifierNames DeclProvenance Name
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Either CompileError NonLinearityProof)
  | UnsupportedMultipleNetworkApplications QueryFormatID DeclProvenance CompleteNamedBoundCtx [(NetworkName, Value Builtin)]
  | VariableSizeTensorQuantification DeclProvenance NamedBoundCtx (VBinder Builtin) (VType Builtin)
  | MultiPropertyTraveralError DeclProvenance MultiPropertyTraveralError
  | UnboundedNetworkInputVariables DeclProvenance CompleteNamedBoundCtx (NonEmpty (NetworkName, Value Builtin, [Lv], UnboundedIndices))
  | -- Loss backend errors
    UnsupportedLossOperation DeclProvenance (Doc Void)
  | UnsupportedHigherOrderTensorCode DeclProvenance NamedBoundCtx (Value Builtin) NamedBoundCtx (Value Builtin)
  | UnableToLiftLogicFieldToTensors DifferentiableLogicID TensorDifferentiableLogicField (BooleanDifferentiableLogicField, Value Builtin) NamedBoundCtx (Value Builtin)
  | NoQuantifierDomainFound DeclProvenance (VBinder Builtin) (These (NonEmpty TensorIndices) (NonEmpty TensorIndices))
  | -- ITP backend errors
    UnsupportedPolymorphicEquality InteractiveTheoremProverID Provenance Name
  | UnusedMonomorphisableDeclaration Provenance Identifier
  | -- Other
    UnsupportedInequality QueryFormatID DeclProvenance
  | QuantifiedIfCondition (ConstraintContext PolarityBuiltin)

deriving instance Show CompileError

--------------------------------------------------------------------------------
-- Some useful developer errors

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: (MonadError CompileError m) => UnAnnDoc -> m b
compilerDeveloperError message = throwError $ DevError message

--------------------------------------------------------------------------------
-- The final error type

-- | Errors that are the user's responsibility to fix.
data VehicleError = VehicleError
  { provenance :: Maybe Provenance,
    problem :: UnAnnDoc,
    fix :: Maybe UnAnnDoc
  }

instance Pretty VehicleError where
  pretty VehicleError {..} =
    unAnnotate $
      "Error"
        <> maybe "" (\p -> " in" <+> pretty p) provenance
        <> ":"
          <+> problem
        <> maybe "" (\f -> line <> "Fix:" <+> f) fix

instance ToJSON VehicleError where
  toJSON = toJSON . toJSONError

toJSONError :: VehicleError -> JSONError
toJSONError VehicleError {..} =
  JSONError
    { provenance = provenance,
      problem = layoutAsText problem,
      fix = fmap layoutAsText fix
    }

data JSONError = JSONError
  { provenance :: Maybe Provenance,
    problem :: Text,
    fix :: Maybe Text
  }
  deriving (Generic)

instance ToJSON JSONError
