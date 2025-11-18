module Vehicle.Compile.Print.Error
  ( formatCompileError,
    prettyCompileError,
    logCompileError,
    multipleNetworkErrorMessages,
    errorInSubsystemMessage,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.These (mergeTheseWith)
import System.FilePath
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Print.Error.Property (propertyTraversalErrorDetails)
import Vehicle.Compile.Print.Error.Typing
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface (getDimsExprs)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic (TensorDifferentiableLogicField (..))
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Syntax.Parse (ParseError (..))
import Vehicle.Syntax.Tensor (TensorIndices)
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- User errors

logCompileError ::
  (MonadLogger m) =>
  ExceptT CompileError m a ->
  m (Either CompileError a)
logCompileError x = do
  e' <- runExceptT x
  case e' of
    Left err -> logDebug MinDetail (pretty (formatCompileError err))
    Right _ -> return ()
  return e'

prettyCompileError :: Bool -> CompileError -> Doc a
prettyCompileError outputAsJSON err = do
  let vehicleError = formatCompileError err
  if outputAsJSON
    then prettyAsJSON vehicleError
    else pretty vehicleError

--------------------------------------------------------------------------------
-- Meaningful error classes

formatCompileError :: CompileError -> VehicleError
formatCompileError = \case
  ----------------------
  -- Developer errors --
  ----------------------

  DevError text ->
    VehicleError
      { provenance = Nothing,
        problem = text,
        fix = Nothing
      }
  -------------
  -- Parsing --
  -------------

  ParseError _module parseError -> case parseError of
    RawParseError text ->
      VehicleError
        { provenance = Nothing,
          problem = pretty text,
          fix = Nothing
        }
    UnannotatedAbstractDef p name ->
      VehicleError
        { provenance = Just p,
          problem =
            "no definition provided for the declaration"
              <+> quotePretty name
              <> ".",
          fix =
            Just $
              "either provide a definition for"
                <+> quotePretty name
                <+> "or mark it as an external resource by adding an appropriate annotation, i.e."
                <+> pretty NetworkDef
                <> ","
                  <+> pretty DatasetDef
                  <+> "or"
                  <+> pretty (ParameterDef NonInferable)
                <> "."
        }
    MultiplyAnnotatedAbstractDef p name ann1 ann2 ->
      VehicleError
        { provenance = Just p,
          problem =
            "abstract declaration"
              <+> quotePretty name
              <+> "cannot simultaneously be annotated with both"
              <+> quotePretty ann1
              <+> "and"
              <+> quotePretty ann2
              <> ".",
          fix =
            Just "remove one of annotations."
        }
    AbstractDefWithNonAbstractAnnotation p name ann ->
      VehicleError
        { provenance = Just p,
          problem = "missing definition for" <+> pretty ann <+> quotePretty name <> ".",
          fix = Just $ "add a definition for" <+> quotePretty name <+> "."
        }
    NonAbstractDefWithAbstractAnnotation p name resource ->
      VehicleError
        { provenance = Just p,
          problem =
            "The declaration"
              <+> quotePretty name
              <+> "should not have a definition"
              <+> "as it has been marked with a"
              <+> quotePretty resource
              <+> "annotation.",
          fix =
            Just $
              "either remove the definition for"
                <+> quotePretty name
                <+> "or remove the"
                <+> quotePretty resource
                <+> "annotation."
        }
    AnnotationWithNoDef p name ->
      VehicleError
        { provenance = Just p,
          problem = "unattached annotation" <+> quotePretty name,
          fix = Just "either attach the annotation to a declaration or remove it entirely"
        }
    FunctionWithMismatchedNames p name1 name2 ->
      VehicleError
        { provenance = Just p,
          problem = "mismatch in function declaration names:" <+> quotePretty name1 <+> "and" <+> quotePretty name2 <> ".",
          fix = Just "ensure the function definition has the same name as the declaration it follows."
        }
    MissingVariables p symbol ->
      VehicleError
        { provenance = Just p,
          problem = "expected at least one variable name after" <+> quotePretty symbol,
          fix = Just $ "add one or more names after" <+> quotePretty symbol
        }
    UnchainableComparisons p prevOrder currentOrder ->
      VehicleError
        { provenance = Just p,
          problem =
            "cannot chain"
              <+> quotePretty prevOrder
              <+> "and"
              <+> quotePretty currentOrder,
          fix = Just "split chained orders into a conjunction"
        }
    InvalidAnnotationOption p annotationName parameterName suggestions ->
      VehicleError
        { provenance = Just p,
          problem =
            "unknown option"
              <+> quotePretty parameterName
              <+> "for"
              <+> quotePretty annotationName
              <+> "annotation.",
          fix =
            case suggestions of
              [] -> Nothing
              (s : _) -> Just $ "did you mean" <+> quotePretty s <> "?"
        }
    InvalidAnnotationOptionValue parameterName parameterValue ->
      VehicleError
        { provenance = Just $ provenanceOf parameterValue,
          problem =
            "unable to parse the value"
              <+> squotes (prettyFriendly parameterValue)
              <+> "for option"
              <+> quotePretty parameterName,
          fix = Nothing
        }
    DuplicateAnnotationOption p annotation name ->
      VehicleError
        { provenance = Just p,
          problem =
            "the annotation"
              <+> quotePretty annotation
              <+> "has multiple copies of the option"
              <+> quotePretty name,
          fix = Just $ "remove all but one of the instances of" <+> quotePretty name
        }
    MissingAnnotationOption p annotation name ->
      VehicleError
        { provenance = Just p,
          problem =
            "the annotation"
              <+> quotePretty annotation
              <+> "is missing the option"
              <+> quotePretty name,
          fix = Just $ "add a value for the option" <+> quotePretty name
        }
    UnknownBuiltin p symbol ->
      VehicleError
        { provenance = Just p,
          problem = "Unknown symbol" <+> quotePretty symbol,
          fix =
            Just $
              "Please consult the documentation for a description"
                <+> "of Vehicle syntax"
        }
  -------------
  -- Scoping --
  -------------
  MissingRequestedDeclarations names ->
    VehicleError
      { provenance = Nothing,
        -- TODO can use Levenschtein distance to search contexts/builtins
        problem =
          "Requested to compile the declaration"
            <+> quotePretty (NonEmpty.head names)
            <+> "but no declarations exist with that name in the specification.",
        fix =
          Just
            "check the spelling of the names and that the correct specification is being used."
      }
  UnboundName p name suggestions ->
    VehicleError
      { provenance = Just p,
        problem = "The name" <+> quotePretty name <+> "is not in scope",
        fix =
          if null suggestions
            then Nothing
            else Just $ "Did you mean to use one of the following:" <> line <> indent 2 (vsep (fmap pretty suggestions))
      }
  UnboundRecordAccessor p name suggestions ->
    VehicleError
      { provenance = Just p,
        problem = "No record with field" <+> quotePretty name <+> "in scope",
        fix =
          if null suggestions
            then Nothing
            else Just $ "Did you mean to use one of the following:" <> line <> indent 2 (vsep (fmap pretty suggestions))
      }
  DeclarationDeclarationShadowing p name _matching ->
    VehicleError
      { provenance = Just p,
        problem = "multiple" <+> typDoc <+> "with the name" <+> quotePretty name,
        fix = Just "remove or rename the duplicate definitions"
      }
    where
      typDoc = either (const "record fields declared") (const "declarations") name
  DeclarationBoundShadowing p name ->
    VehicleError
      { provenance = Just p,
        problem =
          "cannot re-use the name"
            <+> quotePretty name
            <+> "as a local variable because there is already a declaration with that name.",
        fix = Just "rename either the original declaration or this variable"
      }
  UnmatchedRecord p fields maybeBestMatch -> case maybeBestMatch of
    Nothing ->
      VehicleError
        { provenance = Just p,
          problem = "Unable to find a record declaration with matching fields.",
          fix = Just $ "declare a record with the fields:" <> lineIndent (vsep $ fmap pretty fields)
        }
    Just (ident, RecordMatch {..}) -> do
      VehicleError
        { provenance = Just p,
          problem = "Unable to find a record declaration with matching fields.",
          fix =
            Just $
              "it seems likely that the intension is to construct a"
                <+> quotePretty (nameOf ident)
                <+> "record."
                <+> "If so, then "
                <> suggestionDoc
        }
      where
        mispellingDoc = (["fix the mispellings:" <> lineIndent (vsep (fmap (\(actual, expected) -> pretty actual <+> "->" <+> pretty expected) mispellings)) | not (null mispellings)])
        missingDoc = (["add the missing fields:" <> lineIndent (vsep $ fmap pretty missingFields) | not (null missingFields)])
        extraDoc = (["remove the unneeded fields:" <> lineIndent (vsep $ fmap pretty extraFields) | not (null extraFields)])
        suggestionDocs = mispellingDoc <> missingDoc <> extraDoc
        suggestionDoc = concatWith (\a b -> a <> line <> "and" <+> b) suggestionDocs
  ------------
  -- Typing --
  ------------

  TypingError t -> typingErrorDetails t
  QuantifiedIfCondition ctx ->
    VehicleError
      { provenance = Just $ provenanceOf ctx,
        problem = "cannot currently use quantifiers in `if` conditions.",
        fix = Just $ implementationLimitation Nothing
      }
  UnsupportedTensorAnnotation p ->
    VehicleError
      { provenance = Just p,
        problem = "cannot use an empty record as a tensor.",
        fix = Just "remove the annotation or add fields to the record."
      }
  ---------------
  -- Resources --
  ---------------

  ResourcesNotProvided ((resourceType, (ident, p)) :| _) ->
    VehicleError
      { provenance = Just p,
        problem =
          "No"
            <+> entity
            <+> "was provided for the"
            <+> prettyResource resourceType ident
            <> ".",
        fix =
          Just $
            "provide it via the command line using"
              <+> squotes
                ( "--"
                    <> pretty resourceType
                      <+> pretty (nameOf ident :: Name)
                    <> ":"
                    <> var
                )
      }
    where
      (entity, var) = case resourceType of
        Parameter -> ("value", "VALUE")
        _ -> ("file", "FILEPATH")
  UnsupportedResourceFormat (ident, p) resourceType fileExtension ->
    VehicleError
      { provenance = Just p,
        problem =
          "The"
            <+> quotePretty fileExtension
            <+> "format of the file provided"
            <+> "for the"
            <+> prettyResource resourceType ident
            <+> "is not currently supported by Vehicle.",
        fix =
          Just $
            "use one of the supported formats"
              <+> pretty (supportedFileFormats resourceType)
              <+> ", or open an issue on Github ("
              <> githubIssues
              <> ") to discuss adding support."
      }
  ResourceIOError (ident, p) resourceType ioException ->
    VehicleError
      { provenance = Just p,
        problem =
          "The following exception occured when trying to read the file"
            <+> "provided for"
            <+> prettyResource resourceType ident
            <> ":"
            <> line
            <> indent 2 (pretty (show ioException)),
        fix = Nothing
      }
  UnableToParseResource (ident, p) resourceType value ->
    VehicleError
      { provenance = Just p,
        problem =
          "Unable to parse the"
            <+> entity
            <+> squotes (pretty value)
            <+> "provided for the"
            <+> prettyResource resourceType ident,
        fix = Nothing
      }
    where
      entity = if resourceType == Parameter then "value" else "file"

  -- Network errors
  NetworkTypeHasVariableSizeTensor (ident, _p) networkType tDim io ->
    VehicleError
      { provenance = Just $ provenanceOf networkType,
        problem =
          unsupportedAnnotationTypeDescription (pretty NetworkDef) ident networkType
            <+> "as the size of the"
            <+> pretty io
            <+> "tensor"
            <+> squotes (prettyFriendlyEmptyCtx tDim)
            <+> "is not a constant at compile time.",
        fix =
          Just $
            supportedNetworkTypeDescription
              <+> "ensure that the size of the"
              <+> pretty io
              <+> "tensor is constant."
      }
  NetworkTypeHasImplicitSizeTensor (ident, p) networkType implIdent _io ->
    VehicleError
      { provenance = Just p,
        problem =
          unsupportedAnnotationTypeDescription (pretty NetworkDef) ident networkType
            <+> "as the use of the inferable parameter"
            <+> quotePretty implIdent
            <+> "in the type of network declarations is not currently supported.",
        fix =
          Just $
            "instanstiate inferable parameter"
              <+> quotePretty implIdent
              <+> "to an explicit value"
      }
  -- Dataset errors
  DatasetVariableSizeTensor (ident, p) datasetType variableDim ->
    VehicleError
      { provenance = Just p,
        problem =
          unsupportedAnnotationTypeDescription (pretty (ParameterDef NonInferable)) ident datasetType
            <+> "as the dimension size"
            <> line
            <> indent 2 (prettyFriendlyEmptyCtx variableDim)
            <> line
            <> "is not a constant.",
        fix = Just "make sure the dimensions of the dataset are all constants."
      }
  DatasetDimensionsMismatch (ident, p) file expectedType actualDims ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the dimensions of"
            <+> prettyResource Dataset ident
            <> "."
            <> line
            <> "According to the specification it should be"
              <+> maybe "?" pretty (dimensionsOf (normalised expectedType))
            <> "-dimensional"
              <+> "but was actually found to be"
              <+> pretty (length actualDims)
            <> "-dimensional"
              <+> "when reading"
              <+> quotePretty file
            <> ".",
        fix = Just $ datasetDimensionsFix "dimensions" ident file
      }
    where
      dimensionsOf :: VType Builtin -> Maybe Int
      dimensionsOf t = case toTypeValue t of
        VRatTensorType dims -> dimLength dims
        VBoolTensorType dims -> dimLength dims
        VNatTensorType dims -> dimLength dims
        VIndexTensorType _ dims -> dimLength dims
        VListType tElem -> (+ 1) <$> dimensionsOf tElem
        VVectorType tElem _dims -> (+ 1) <$> dimensionsOf tElem
        _ -> Just 0

      dimLength :: Value Builtin -> Maybe Int
      dimLength dims = either (const Nothing) (Just . length) (getDimsExprs dims)
  DatasetDimensionSizeMismatch (ident, p) file expectedSize actualSize wrongDimensionIndex ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the size of"
            <+> prettyOrdinal "dimension" (wrongDimensionIndex + 1) Nothing
            <+> "of"
            <+> prettyResource Dataset ident
            <> "."
            <> line
            <> "According to the specification it should be"
              <+> quotePretty expectedSize
              <+> "but was actually found to be"
              <+> quotePretty actualSize
              <+> "when reading"
              <+> quotePretty file
            <> ".",
        fix = Just $ datasetDimensionsFix "dimensions" ident file
      }
  DatasetInvalidNat (ident, p) file v ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the type of elements of"
            <+> prettyResource Dataset ident
            <> "."
            <> line
            <> "Expected elements of type"
              <+> quotePretty NatType
              <+> "but found value"
              <+> quotePretty v
              <+> "when reading"
              <+> quotePretty file
            <> ".",
        fix = Just $ datasetDimensionsFix "type" ident file
      }
  DatasetInvalidIndex (ident, p) file v n ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the type of elements of"
            <+> prettyResource Dataset ident
            <> "."
            <> line
            <> "Expected elements of type"
              <+> squotes (pretty IndexType <+> pretty n)
              <+> parens ("i.e. values between 0 and" <+> pretty (n - 1) <+> "inclusive")
              <+> "but found value"
              <+> quotePretty v
              <+> "when reading"
              <+> quotePretty file
            <> ".",
        fix = Just $ datasetDimensionsFix "type" ident file
      }
  DatasetTypeMismatch (ident, p) file _datasetType expectedType actualType ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the type of elements of"
            <+> prettyResource Dataset ident
            <> "."
            <> line
            <> "Expected elements of type"
              <+> squotes (prettyFriendlyEmptyCtx expectedType)
              <+> "but found elements of type"
              <+> squotes actualType
              <+> "when reading"
              <+> quotePretty file
            <> ".",
        fix = Just $ datasetDimensionsFix "type" ident file
      }
  -- Parameter errors
  ParameterValueUnparsable (ident, p) value expectedType ->
    VehicleError
      { provenance = Just p,
        problem =
          "The value"
            <+> squotes (pretty value)
            <+> "provided for"
            <+> prettyResource Parameter ident
            <+> "could not be parsed as"
            <+> prettyBuiltinType expectedType
            <> ".",
        fix =
          Just $
            "either change the type of"
              <+> prettyIdentName ident
              <+> "in the specification or change the value provided."
      }
  ParameterTypeVariableSizeIndex (ident, p) parameterType size ->
    VehicleError
      { provenance = Just p,
        problem =
          unsupportedAnnotationTypeDescription (pretty (ParameterDef NonInferable)) ident parameterType
            <+> "as the size"
            <+> squotes (prettyFriendlyEmptyCtx size)
            <+> "for the"
            <+> pretty IndexType
            <+> "type is not a constant.",
        fix = Just "make sure the dimensions of the indices are all constants."
      }
  ParameterValueInvalidIndex (ident, p) value n ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the type of"
            <+> prettyResource Parameter ident
            <> "."
            <> line
            <> "Expected something of type"
              <+> squotes (pretty IndexType <+> pretty n)
              <+> "but was provided the value"
              <+> quotePretty value
            <> ".",
        fix =
          Just $
            "either change the size of the index or ensure the value"
              <+> "provided is in the range"
              <+> squotes ("0..." <> pretty (n - 1))
              <+> "(inclusive)."
      }
  ParameterValueInvalidNat (ident, p) value ->
    VehicleError
      { provenance = Just p,
        problem =
          "Mismatch in the type of"
            <+> prettyResource Parameter ident
            <> "."
            <> line
            <> "Expected something of type"
              <+> quotePretty NatType
              <+> "but was provided the value"
              <+> quotePretty value
            <> ".",
        fix =
          Just $
            "either change the type of"
              <+> prettyIdentName ident
              <+> "or ensure the value provided is non-negative."
      }
  ParameterTypeInferableParameterIndex (ident, p) _varIndent ->
    VehicleError
      { provenance = Just p,
        problem =
          "The use of an inferable parameter for the size of an"
            <+> pretty IndexType
            <+> "in the type of"
            <+> prettyResource Parameter ident
            <+> "is not currently supported.",
        fix =
          Just $
            "either replace the inferable parameter with a concrete value or"
              <+> "open an issue on the Github tracker to request support."
      }
  InferableParameterContradictory ident ((ident1, _p1), r1, v1) ((ident2, p2), r2, v2) ->
    VehicleError
      { provenance = Just p2,
        problem =
          "Found contradictory values for inferable parameter"
            <+> quotePretty ident
            <> "."
            <> "Inferred the value"
              <+> squotes (pretty v1)
              <+> "from"
              <+> prettyResource r1 ident1
            <> "but inferred the value"
              <+> squotes (pretty v2)
              <+> "from"
              <+> prettyResource r2 ident2
            <> ".",
        fix = Just "make sure the provided resources are consistent with each other."
      }
  InferableParametersUninferrable ((ident, p) :| _) ->
    VehicleError
      { provenance = Just p,
        problem =
          "Unable to infer the value of"
            <+> prettyResource Parameter ident
            <> ".",
        fix =
          Just $
            "For a parameter's value to be inferable, it must"
              <+> "be used as the dimension of a dataset"
              <+> "(networks will be supported later)."
      }
  --------------------
  -- Backend errors --
  --------------------
  MultiPropertyTraveralError prov err -> propertyTraversalErrorDetails prov err
  VariableSizeTensorQuantification (ident, _p) ctx binder dims ->
    VehicleError
      { provenance = Just $ provenanceOf binder,
        problem =
          "whilst compiling property"
            <+> quotePretty ident
            <+> "found the quantified variable"
            <+> quotePretty (nameOf binder)
            <+> "with dimensions"
            <+> prettyFriendly (WithContext dims ctx)
            <> "."
              <+> "This is not supported during verification as the dimensions are not constant.",
        fix =
          Just $
            "ensure that the dimensions of variable"
              <+> quotePretty (nameOf binder)
              <+> "are known at compile time."
      }
  UnsupportedAlternatingQuantifiers queryFormat (ident, p) cause ->
    VehicleError
      { provenance = Just p,
        problem =
          "The property"
            <+> prettyIdentName ident
            <+> "contains"
            <+> "alternating"
            <+> quotePretty Forall
            <+> "and"
            <+> quotePretty Exists
            <+> "quantifiers which is not supported by the"
            <+> pretty queryFormat
            <> "."
            <> line
            <> causeDoc,
        fix = Just "try simplifying the specification to avoid the alternating quantifiers."
      }
    where
      causeDoc :: Doc a
      causeDoc = case cause of
        Left err -> line <> errorInSubsystemMessage "locate the original source of the alternating quantifiers" err
        Right (q, pq, pp) ->
          "In particular:"
            <> line
            <> indent 2 (prettyPolarityProvenance pq q pp)
  UnsupportedNonLinearConstraint queryFormat (ident, p) cause ->
    VehicleError
      { provenance = Just p,
        problem =
          "The property"
            <+> prettyIdentName ident
            <+> "contains"
            <+> "a non-linear constraint which is not supported by the"
            <+> pretty queryFormat
            <> "."
            <> line
            <> causeDoc,
        fix =
          Just "try rewriting the specification to avoid the non-linearity."
      }
    where
      causeDoc :: Doc a
      causeDoc = case cause of
        Left err -> line <> errorInSubsystemMessage "locate the original source of the non-linearity" err
        Right source -> case source of
          LinearTimesLinear opProv lhs rhs ->
            "In particular the multiplication in"
              <+> pretty opProv
              <+> "involves"
              <> prettyLinearityProvenance lhs "left hand side of the multiplication"
              <> "and"
              <> prettyLinearityProvenance rhs "right hand side of the multiplication"
          DivideByLinear opProv rhs ->
            "In particular the division in"
              <+> pretty opProv
              <+> "involves"
              <> prettyLinearityProvenance rhs "denominator of the division"
          PowLinearBase opProv lhs ->
            "In particular the power in"
              <+> pretty opProv
              <+> "involves"
              <> prettyLinearityProvenance lhs "base of the power"
          PowLinearExponent opProv lhs ->
            "In particular the power in"
              <+> pretty opProv
              <+> "involves"
              <> prettyLinearityProvenance lhs "exponent of the power"
  UnsupportedInequality queryFormat (identifier, p) ->
    VehicleError
      { provenance = Just p,
        problem =
          "After compilation, property"
            <+> prettyIdentName identifier
            <+> "contains a `!=` which is not current supported by the"
            <+> pretty queryFormat
            <> ". ",
        fix = Just (implementationLimitation (Just 74))
      }
  UnsupportedPolymorphicEquality target p typeName ->
    VehicleError
      { provenance = Just p,
        problem =
          "The use of equality over the unknown type"
            <+> quotePretty typeName
            <+> "is not currently supported"
            <+> "when compiling to"
            <+> pretty target,
        fix =
          Just $
            "try avoiding it, otherwise open an issue on the"
              <+> "Vehicle issue tracker describing the use case."
      }
  NoPropertiesFound ->
    VehicleError
      { provenance = Nothing,
        problem = "No properties found in file.",
        fix = Just $ "an expression is labelled as a property by giving it type" <+> squotes (pretty BoolType) <+> "."
      }
  UnsupportedLossOperation (_, p) op ->
    VehicleError
      { provenance = Just p,
        problem =
          "Loss functions do not yet support compilation of"
            <+> squotes op
            <+> ".",
        fix = Nothing
      }
  DuplicateQuantifierNames (identifier, p) name ->
    VehicleError
      { provenance = Just p,
        problem =
          "The property"
            <+> quotePretty identifier
            <+> "contains multiple quantified variables with the name"
            <+> quotePretty name
            <> ".",
        fix = Just "change the specification so that all quantified variables have unique names"
      }
  HigherOrderVectors (ident, p) ctx vecTyp elemTyp ->
    VehicleError
      { provenance = Just p,
        problem =
          "The property"
            <+> quotePretty (nameOf ident)
            <+> "cannot be compiled to tensor code as it contains"
            <+> "the vector type:"
            <> line
              <+> indent 2 (prettyFriendly (WithContext vecTyp ctx))
            <> line
            <> "Vectors with elements of type" <+> squotes (prettyFriendly (WithContext elemTyp ctx)) <+> "cannot currently be compiled to loss functions",
        fix = Nothing
      }
  NoQuantifierDomainFound (ident, _p) binder maybeUnboundedVariables ->
    VehicleError
      { provenance = Just $ provenanceOf binder,
        problem =
          "The property"
            <+> quotePretty ident
            <+> "cannot be compiled to tensor code as the variable"
            <+> quotePretty (nameOf binder)
            <+> "is not properly bounded. In particular,"
            <+> missingBounds maybeUnboundedVariables,
        fix = Just "Add inequalities that restrict the value of the variable both below and above."
      }
    where

  UnsupportedHigherOrderTensorCode (ident, p) originalCtx originalExpr blockedCtx blockedExpr ->
    VehicleError
      { provenance = Just p,
        problem =
          "While compiling property"
            <+> quotePretty ident
            <+> "found the following expression cannot be efficiently compiled to tensors:"
            <> line
            <> indent 2 (prettyFriendly (WithContext originalExpr originalCtx))
            <> line
            <> "In particular the operation that Vehicle doesn't know how to lift to tensors is:"
            <> line
            <> indent 2 (prettyFriendly (WithContext blockedExpr blockedCtx)),
        fix = Nothing
      }
  UnableToLiftLogicFieldToTensors logicID _tensorField (boolField, value) ctx problematicValue ->
    VehicleError
      { provenance = Nothing,
        problem =
          "While compiling the logic"
            <+> quotePretty logicID
            <+> "unable to lift differentiable logic field"
            <> line
            <> indent 2 (quotePretty boolField <> ":" <+> prettyFriendlyEmptyCtx value)
            <> line
            <> "to a corresponding tensor operation."
              <+> "In particular, unable to lift"
            <> line
            <> indent 2 (prettyFriendly (WithContext problematicValue ctx)),
        fix = Nothing
      }
  UnusedMonomorphisableDeclaration p ident ->
    VehicleError
      { provenance = Just p,
        problem =
          "Unable to compile declaration"
            <+> quotePretty ident
            <+> "as it is both not used in any properties and the type is not precisely defined"
            <+> "and therefore unable to decide"
            <+> "whether or not it should be lifted to the type-level.",
        fix = Just "either remove the declaration, or add a type signature or use it in a property."
      }
  UnsupportedMultipleNetworkApplications queryFormat (_, p) ctx apps ->
    VehicleError
      { provenance = Just p,
        problem = multipleNetworkErrorMessages (pretty queryFormat) ctx apps,
        fix = Just "this is on our road map to fix with VNNLib 2.0, but please open an issue on the Issue tracker with your use-case."
      }
  UnboundedNetworkInputVariables (ident, p) ctx ((networkName, inputValue, userVariables, unboundedInputs) :| _) ->
    VehicleError
      { provenance = Just p,
        problem =
          "The property"
            <+> quotePretty ident
            <+> "cannot be compiled as cannot deduce lower and upper bounds for the input of"
            <+> lineIndent (prettyFriendly (WithContext (VFreeVar (Identifier (ModulePath [User]) networkName) [explicit inputValue]) ctx))
            <> line
            <> "In particular,"
              <+> missingBounds unboundedInputs
              <+> "for"
              <+> squotes (prettyFriendly (WithContext inputValue ctx)),
        fix =
          Just $
            "add additional inequalities that restrict the value of" <+> case userVariables of
              [v] -> "the quantified variable" <+> quotePretty v
              _ -> "the following quantified variables:" <+> hsep (fmap pretty userVariables)
      }
  UnknownDifferentiableLogic name possibleNames ->
    VehicleError
      { provenance = Nothing,
        problem =
          "Unable to find a differentiable logic named"
            <+> quotePretty name,
        fix =
          Just $
            "use one of the following available logics:"
              <> lineIndent (starredList (fmap pretty possibleNames))
              <> line
              <> "or declare your own logic called" <+> quotePretty name
              <> "."
      }
  UnreducableDifferentiableLogic (ident, p) ->
    VehicleError
      { provenance = Just p,
        problem =
          "Unable to compile differentiable logic"
            <+> quotePretty (nameOf ident)
            <> "."
            <> line
            <> "Its definition does not compute to a record literal.",
        fix =
          Just
            "declare the logic directly as a record literal."
      }
  UnorderableDifferentiableLogic (ident, p) value ->
    VehicleError
      { provenance = Just p,
        problem =
          "Unable to compile differentiable logic"
            <+> quotePretty (nameOf ident)
            <> "."
            <> line
            <> "Internally Vehicle computes the result of:" <+> lineIndent comp
            <> line
            <> "in order to work out whether the loss should be maximised or minimised."
            <> line
            <> "However, Vehicle was unable to establish the truth value of the result:"
              <+> lineIndent (prettyFriendlyEmptyCtx value),
        fix =
          Just $
            "ensure that the expression" <+> squotes comp <+> "evaluates to either `true` or `false`."
      }
    where
      comp = pretty TruthityElement <+> "<=" <+> pretty FalsityElement

datasetDimensionsFix :: Doc a -> Identifier -> FilePath -> Doc a
datasetDimensionsFix feature ident file =
  "change the"
    <+> feature
    <+> "of"
    <+> prettyIdentName ident
    <+> "in the specification"
    <+> "or check that"
    <+> quotePretty (takeFileName file)
    <+> "is in the format you were expecting."

errorInSubsystemMessage :: Doc a -> CompileError -> Doc a
errorInSubsystemMessage task err =
  "Unfortunately while trying to"
    <+> task
    <> ","
      <+> "the following error was encountered:"
    <> line
    <> indent 2 (pretty (formatCompileError err))
    <> line
    <> "Please report this as an issue on Github"
      <+> parens githubIssues
    <> line

githubIssues :: Doc a
githubIssues = "https://github.com/vehicle-lang/vehicle/issues/"

implementationLimitation :: Maybe Int -> Doc a
implementationLimitation issue =
  "This is a limitation of the current implementation rather than a fundamental problem."
    <+> case issue of
      Nothing -> "If you would like this to be fixed, please open an issue at" <+> squotes githubIssues
      Just issueNumber ->
        "If you would like this to be fixed, please comment at"
          <+> squotes (githubIssues <+> pretty issueNumber)
          <> "."

prettyResource :: ExternalResource -> Identifier -> Doc a
prettyResource resourceType ident =
  pretty resourceType <+> prettyIdentName ident

prettyBuiltinType :: BuiltinType -> Doc a
prettyBuiltinType t = article <+> squotes (pretty t)
  where
    article :: Doc a
    article = case t of
      IndexType -> "an"
      _ -> "a"

prettyQuantifierArticle :: Quantifier -> Doc a
prettyQuantifierArticle q =
  (if q == Forall then "a" else "an") <+> squotes (pretty q)

prettyPolarityProvenance :: Provenance -> Quantifier -> PolarityProvenance -> Doc a
prettyPolarityProvenance topQuantifierProv topQuantifier bottomQuantifierProvenance = do
  let bottomQuantifier = neg topQuantifier
  numberedList $ reverse (finalLine : go bottomQuantifier bottomQuantifierProvenance)
  where
    go :: Quantifier -> PolarityProvenance -> [Doc a]
    go q = \case
      QuantifierProvenance p ->
        ["the inner quantifier is the" <+> quotePretty q <+> "located in" <+> pretty p]
      NegateProvenance p pp ->
        transform p ("the" <+> quotePretty Not) : go (neg q) pp
      LHSImpliesProvenance p pp ->
        transform p ("being on the LHS of the" <+> quotePretty Implies) : go (neg q) pp
      PolFunctionProvenance p pp position ->
        surround p (prettyAuxiliaryFunctionProvenance position) : go q pp
      where
        surround p x =
          "which is" <+> x <+> "in" <+> pretty p

        transform p x =
          surround p ("turned into" <+> prettyQuantifierArticle q <+> "by" <+> x)

    finalLine :: Doc a
    finalLine =
      "which alternates with the outer"
        <+> quotePretty topQuantifier
        <+> "in"
        <+> pretty topQuantifierProv

prettyLinearityProvenance :: forall a. LinearityProof -> Doc a -> Doc a
prettyLinearityProvenance lp location =
  lineIndent (numberedList $ reverse (finalLine : go lp)) <> line
  where
    go :: LinearityProof -> [Doc a]
    go = \case
      QuantifiedVariableProvenance p v ->
        ["the quantified variable" <+> quotePretty v <+> "introduced in" <+> pretty p]
      NetworkOutputProvenance p networkName ->
        ["the output of network" <+> squotes (pretty networkName) <+> "in" <+> pretty p]
      LinFunctionProvenance p pp position ->
        (prettyAuxiliaryFunctionProvenance position <+> "in" <+> pretty p) : go pp

    finalLine :: Doc a
    finalLine = "which is used in the" <+> location

prettyAuxiliaryFunctionProvenance :: FunctionPosition -> Doc a
prettyAuxiliaryFunctionProvenance = \case
  FunctionInput n _ -> "which is used as an input to the function" <+> quotePretty n
  FunctionOutput n -> "which is returned as an output of the function" <+> quotePretty n

prettyOrdinal :: Doc b -> Int -> Maybe Int -> Doc b
prettyOrdinal object argNo argTotal
  | argTotal == Just 1 = "the" <+> object
  | argNo > 9 = object <+> pretty argNo
  | otherwise = "the" <+> prettyOrd argNo <+> object
  where
    prettyOrd :: Int -> Doc b
    prettyOrd = \case
      1 -> "first"
      2 -> "second"
      3 -> "third"
      4 -> "fourth"
      5 -> "fifth"
      6 -> "sixth"
      7 -> "seventh"
      8 -> "eighth"
      9 -> "ninth"
      _ -> developerError "Cannot convert ordinal"

supportedNetworkTypeDescription :: Doc a
supportedNetworkTypeDescription =
  "Only networks of the following types are allowed:"
    <> line
    <> indent 2 "Tensor Rat [a_1, ..., a_n] -> Tensor Rat [b_1, ..., b_n]"
    <> line
    <> "where 'a_i' and 'b_i' are all constants at compile time."

multipleNetworkErrorMessages :: Doc a -> CompleteNamedBoundCtx -> [(Name, Value Builtin)] -> Doc a
multipleNetworkErrorMessages verifier ctx networkNames = do
  let prettyApp (n, v) = pretty n <+> prettyFriendly (WithContext v ctx)
  "The"
    <+> verifier
    <+> "currently doesn't support properties that involve"
    <+> "multiple network applications."
    <> line
    <> "This property involves the following network applications:"
    <> line
    <> indent 2 (vsep $ fmap prettyApp networkNames)

missingBounds :: UnboundedIndices -> Doc a
missingBounds = mergeTheseWith (missingOneSidedBounds True) (missingOneSidedBounds False) (\u v -> u <+> "and" <+> v)

missingOneSidedBounds :: Bool -> NonEmpty TensorIndices -> Doc a
missingOneSidedBounds isLowerBound missingIndices =
  "missing" <+> (if isLowerBound then "lower" else "upper") <+> "bounds" <> case missingIndices of
    [[]] -> ""
    _ -> " for indices" <+> vsep (fmap pretty missingIndices)
