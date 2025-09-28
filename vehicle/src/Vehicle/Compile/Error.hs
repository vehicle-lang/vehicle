{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.These (These)
import Data.Typeable (Proxy)
import Data.Void (Void)
import GHC.Generics (Generic)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Vehicle.Backend.LossFunction.Core (BooleanDifferentiableLogicField, TensorDifferentiableLogicField)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, TensorShape)
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
-- Compilation errors

data CompileError
  = DevError (Doc ())
  | -- Parse errors
    ParseError ParseLocation ParseError
  | -- Errors thrown by scope checking.
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
    ResourceNotProvided DeclProvenance ExternalResource
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
  | InferableParameterUninferrable DeclProvenance
  | -- Unsupported properties
    NoPropertiesFound
  | HigherOrderVectors DeclProvenance NamedBoundCtx (VType Builtin) (VType Builtin)
  | UnsupportedAlternatingQuantifiers QueryFormatID DeclProvenance (Either CompileError (Quantifier, Provenance, PolarityProvenance))
  | DuplicateQuantifierNames DeclProvenance Name
  | UnsupportedNonLinearConstraint QueryFormatID DeclProvenance (Either CompileError NonLinearityProof)
  | UnsupportedMultipleNetworkApplications QueryFormatID DeclProvenance CompleteNamedBoundCtx [(Name, Value Builtin)]
  | VariableSizeTensorQuantification DeclProvenance NamedBoundCtx (VBinder Builtin) (VType Builtin)
  | -- Loss backend errors
    UnsupportedLossOperation DeclProvenance Provenance (Doc Void)
  | UnsupportedHigherOrderTensorCode DeclProvenance NamedBoundCtx (Value Builtin) NamedBoundCtx (Value Builtin)
  | UnableToLiftLogicFieldToTensors DifferentiableLogicID TensorDifferentiableLogicField (BooleanDifferentiableLogicField, Value Builtin) NamedBoundCtx (Value Builtin)
  | NoQuantifierDomainFound DeclProvenance (VBinder Builtin) (These (NonEmpty TensorIndices) (NonEmpty TensorIndices))
  | -- ITP backend errors
    UnsupportedPolymorphicEquality ITP Provenance Name
  | UnusedMonomorphisableDeclaration Provenance Identifier
  | -- Other
    UnsupportedInequality QueryFormatID DeclProvenance
  | QuantifiedIfCondition (ConstraintContext PolarityBuiltin)

deriving instance Show CompileError

--------------------------------------------------------------------------------
-- Some useful developer errors

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: (MonadError CompileError m) => Doc () -> m b
compilerDeveloperError message = throwError $ DevError message

--------------------------------------------------------------------------------
-- The final error type

-- | Errors from external code that we have no control over.
--  These may be either user or developer errors but in general we
--  can't distinguish between the two.
newtype ExternalError = ExternalError Text
  deriving (Generic)

instance ToJSON ExternalError

-- | Errors that are the user's responsibility to fix.
data UserError = UserError
  { provenance :: Provenance,
    problem :: UnAnnDoc,
    fix :: Maybe UnAnnDoc
  }

-- | Concrete instance for JSON serialization of UserError as
-- UnAnnDoc cannot be serialized directly.
instance ToJSON UserError where
  toJSON (UserError p prob probFix) =
    object
      [ "provenance" .= toJSON p,
        "problem" .= renderString (layoutPretty defaultLayoutOptions prob),
        "fix" .= maybe "" (renderString . layoutPretty defaultLayoutOptions) probFix
      ]

data VehicleError
  = UError UserError
  | EError ExternalError
  | DError (Doc ())
  deriving (Generic)

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

instance ToJSON VehicleError where
  toJSON vehicleError = case vehicleError of
    DError doc -> toJSON $ renderString $ layoutPretty defaultLayoutOptions doc
    EError eError -> toJSON eError
    UError uError -> toJSON uError

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t
