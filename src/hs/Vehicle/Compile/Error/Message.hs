module Vehicle.Compile.Error.Message
  ( UserError(..)
  , VehicleError(..)
  , MeaningfulError(..)
  , fromLoggedEitherIO
  , logCompileError
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Void ( Void )
import Data.Text ( Text, pack )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Foldable (fold)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Language.Print
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Resource

--------------------------------------------------------------------------------
-- User errors

-- |Errors that are the user's responsibility to fix.
data UserError = UserError
  { provenance :: Provenance
  , problem    :: Doc Void
  , fix        :: Maybe (Doc Void)
  }

-- |Errors from external code that we have no control over.
-- These may be either user or developer errors but in general we
-- can't distinguish between the two.
newtype ExternalError = ExternalError Text

data VehicleError
  = UError UserError
  | EError ExternalError

instance Pretty VehicleError where
  pretty (UError (UserError p prob probFix)) =
    unAnnotate $ "Error:" <+> appendProvenance prob p <>
      maybe "" (\fix -> line <> fixText fix) probFix

  pretty (EError (ExternalError text)) =
    pretty text

appendProvenance :: Doc ann -> Provenance -> Doc ann
appendProvenance doc p = doc <+> "(" <> pretty p <> ")"

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t

--------------------------------------------------------------------------------
-- IO

fromEitherIO :: MonadIO m => LoggingOptions -> Either CompileError a -> m a
fromEitherIO _              (Right x)  = return x
fromEitherIO loggingOptions (Left err) =
  fatalError loggingOptions $ pretty $ details err

fromLoggedEitherIO :: MonadIO m
                   => LoggingOptions
                   -> ExceptT CompileError (LoggerT m) a
                   -> m a
fromLoggedEitherIO loggingOptions x = do
  fromEitherIO loggingOptions =<< fromLoggedIO loggingOptions (logCompileError x)

logCompileError :: Monad m
                => ExceptT CompileError (LoggerT m) a
                -> LoggerT m (Either CompileError a)
logCompileError x = do
  e' <- runExceptT x
  case e' of
    Left err -> logDebug MinDetail (pretty (details err))
    Right _  -> return ()
  return e'

--------------------------------------------------------------------------------
-- Meaningful error classes

class MeaningfulError e where
  details :: e -> VehicleError

instance MeaningfulError CompileError where
  details = \case

    ----------------------
    -- Developer errors --
    ----------------------

    DevError text -> developerError text

    -------------
    -- Parsing --
    -------------

    BNFCParseError text -> EError $ ExternalError
      -- TODO need to revamp this error, BNFC must provide some more
      -- information than a simple string surely?
      (pack text)

    --------------------------
    -- Elaboration internal --
    --------------------------

    UnknownBuiltin tk -> UError $ UserError
      { provenance = tkProvenance tk
      , problem    = "Unknown symbol" <+> pretty (tkSymbol tk)
      , fix        = Just "Please consult the documentation for a description of Vehicle syntax"
      }

    MalformedPiBinder tk -> UError $ UserError
      { provenance = tkProvenance tk
      , problem    = "Malformed binder for Pi, expected a type but only found name" <+> pretty (tkSymbol tk)
      , fix        = Nothing
      }

    MalformedLamBinder expr -> UError $ UserError
      { provenance = provenanceOf expr
      , problem    = "Malformed binder for Lambda, expected a name but only found an expression" <+> prettyVerbose expr
      , fix        = Nothing
      }

    --------------------------
    -- Elaboration external --
    --------------------------

    MissingDefFunExpr p name -> UError $ UserError
      { provenance = p
      , problem    = "missing definition for the declaration" <+> squotes (pretty name)
      , fix        = Just $ "add a definition for the declaration, e.g."
                    <> line <> line
                    <> "addOne :: Int -> Int" <> line
                    <> "addOne x = x + 1     <-----   declaration definition"
      }

    DuplicateName p name -> UError $ UserError
      { provenance = fold p
      , problem    = "multiple definitions found with the name" <+> squotes (pretty name)
      , fix        = Just "remove or rename the duplicate definitions"
      }

    MissingVariables p symbol -> UError $ UserError
      { provenance = p
      , problem    = "expected at least one variable name after" <+> squotes (pretty symbol)
      , fix        = Just $ "add one or more names after" <+> squotes (pretty symbol)
      }

    UnchainableOrders p prevOrder currentOrder -> UError $ UserError
      { provenance = p
      , problem    = "cannot chain" <+> squotes (pretty prevOrder) <+>
                     "and" <+> squotes (pretty currentOrder)
      , fix        = Just "split chained orders into a conjunction"
      }

    -------------
    -- Scoping --
    -------------

    UnboundName name p -> UError $ UserError
      { provenance = p
      -- TODO can use Levenschtein distance to search contexts/builtins
      , problem    = "The name" <+> squotes (pretty name) <+> "is not in scope"
      , fix        = Nothing
      }

    ------------
    -- Typing --
    ------------

    TypeMismatch p ctx candidate expected -> UError $ UserError
      { provenance = p
      , problem    = "expected something of type" <+> prettyFriendlyDB nameCtx expected <+>
                    "but inferred type" <+> prettyFriendlyDB nameCtx candidate
      , fix        = Nothing
      } where nameCtx = ctxNames ctx

    UnresolvedHole p name -> UError $ UserError
      { provenance = p
      , problem    = "the type of" <+> squotes (pretty name) <+> "could not be resolved"
      , fix        = Nothing
      }

    FailedConstraints cs -> UError $ failedConstraintError nameCtx constraint
      where
        constraint = NonEmpty.head cs
        nameCtx = ctxNames (boundContext constraint)

    UnsolvedConstraints cs -> UError $ UserError
      { provenance = provenanceOf constraint
      , problem    = unsolvedConstraintError constraint nameCtx
      , fix        = Just "try adding more type annotations"
      }
      where
        constraint = NonEmpty.head cs
        nameCtx    = ctxNames (boundContext constraint)

    UnsolvedMetas ms -> UError $ UserError
      { provenance = p
      , problem    = "Unable to infer type of bound variable"
      , fix        = Just "add more type annotations"
      }
      where
        (_, p) = NonEmpty.head ms

    MissingExplicitArg ctx arg argType -> UError $ UserError
      { provenance = provenanceOf arg
      , problem    = "expected an" <+> pretty Explicit <+> "argument of type" <+>
                    argTypeDoc <+> "but instead found" <+>
                    pretty (visibilityOf arg) <+> "argument" <+> squotes argExprDoc
      , fix        = Just $ "try inserting an argument of type" <+> argTypeDoc
      }
      where
        nameCtx    = ctxNames ctx
        argExprDoc = prettyFriendlyDB nameCtx (argExpr arg)
        argTypeDoc = prettyFriendlyDB nameCtx argType

    ---------------
    -- Resources --
    ---------------

    ResourceNotProvided ident p resourceType -> UError $ UserError
      { provenance = p
      , problem    = "No" <+> entity <+> "was provided for the" <+>
                     prettyResource resourceType ident <> "."
      , fix        = Just $ "provide it via the command line using" <+>
                     squotes ("--" <> pretty resourceType <+> pretty ident <>
                     ":" <> var)
      }
      where
      (entity, var) = case resourceType of
        Parameter -> ("value", "VALUE")
        _         -> ("file", "FILEPATH")

    UnsupportedResourceFormat ident p resourceType fileExtension -> UError $ UserError
      { provenance = p
      , problem    = "The file provided for the" <+> prettyResource resourceType ident <+>
                     "is in a format (" <> pretty fileExtension <> ") not currently" <+>
                     "supported by Vehicle."
      , fix        = Just $ "use one of the supported formats" <+> pretty (supportedFileFormats resourceType) <+>
                     ", or open an issue on Github to discuss adding support."
      }

    ResourceIOError ident p resourceType ioException -> UError $ UserError
      { provenance = p
      , problem    = "The following exception occured when trying to read the file" <+>
                     "provided for" <+> prettyResource resourceType ident <> ":" <>
                     line <> indent 2 (pretty (show ioException))
      , fix        = Nothing
      }

    UnableToParseResource ident p resourceType value -> UError $ UserError
      { provenance = p
      , problem    = "Unable to parse the" <+> entity <+> squotes (pretty value) <+>
                     "provided for the" <+> prettyResource resourceType ident
      , fix        = Nothing
      } where entity = if resourceType == Parameter then "value" else "file"

    -- Network errors

    NetworkTypeIsNotAFunction ident networkType -> UError $ UserError
      { provenance = provenanceOf networkType
      , problem    = unsupportedResourceTypeDescription Network ident networkType <+> "as" <+>
                     squotes (prettyFriendly networkType) <+> "is not a function."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Provide both an input type and output type for your network."
      }

    NetworkTypeIsNotOverTensors ident fullType nonTensorType io -> UError $ UserError
      { provenance = provenanceOf nonTensorType
      , problem    = unsupportedResourceTypeDescription Network ident fullType <+>
                    "as the" <+> pretty io <+> squotes (prettyFriendly nonTensorType) <+>
                    "is not a tensor."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Ensure the" <+> pretty io <+> "of the network is a Tensor"
      }

    NetworkTypeHasNonExplicitArguments ident networkType binder -> UError $ UserError
      { provenance = provenanceOf binder
      , problem    = unsupportedResourceTypeDescription Network ident networkType <+> "as" <+>
                     squotes (prettyFriendly networkType) <+>
                     "contains a non-explicit argument" <+>
                     squotes (prettyFriendly binder) <> "."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Remove the non-explicit argument."
      }

    NetworkTypeHasUnsupportedElementType ident fullType elementType io -> UError $ UserError
      { provenance = provenanceOf elementType
      , problem    = unsupportedResourceTypeDescription Network ident fullType <+> "as" <+>
                     pretty io <+> "s of type" <+>
                     squotes (prettyFriendly elementType) <+>
                     "are not currently supported."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Ensure that the network" <+> pretty io <+> "uses" <+>
                     "supported types."
      }

    NetworkTypeHasMultidimensionalTensor ident fullType tensorType io -> UError $ UserError
      { provenance = provenanceOf tensorType
      , problem    = unsupportedResourceTypeDescription Network ident fullType <+> "as" <+>
                     "the" <+> pretty io <+>
                     squotes (prettyFriendly tensorType) <+> "is not a 1D tensor."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Ensure that the network" <+> pretty io <+> "is a 1D tensor."
      }

    NetworkTypeHasVariableSizeTensor ident fullType tDim io -> UError $ UserError
      { provenance = provenanceOf tDim
      , problem    = unsupportedResourceTypeDescription Network ident fullType <+> "as" <+>
                     "the size of the" <+> pretty io <+> "tensor" <+>
                     squotes (pretty $ show tDim) <+> "is not a constant."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "ensure that the size of the" <+> pretty io <+>
                     "tensor is constant."
      }

    -- Dataset errors

    DatasetTypeUnsupportedContainer ident p tCont -> UError $ UserError
      { provenance = p
      , problem    = squotes (prettyFriendly tCont) <+> "is not a valid type" <+>
                     "for the" <+> prettyResource Dataset ident <> "."
      , fix        = Just $ "change the type of" <+> squotes (pretty ident) <+>
                     "to either a" <+> squotes (pretty List) <+> "or" <+>
                     squotes (pretty Tensor) <> "."
      }

    DatasetTypeUnsupportedElement ident p tCont -> UError $ UserError
      { provenance = p
      , problem    = squotes (prettyFriendly tCont) <+> "is not a valid type" <+>
                     "for the elements of the" <+> prettyResource Dataset ident <> "."
      , fix        = Just $ "change the type to one of" <+> elementTypes <> "."
      } where elementTypes = pretty @[Builtin] ([Index] <> fmap NumericType [Nat, Int, Rat])

    DatasetVariableSizeTensor ident p tCont -> UError $ UserError
      { provenance = p
      , problem    = "A tensor with variable dimensions" <+> squotes (prettyFriendly tCont) <+>
                     "is not a supported type for the" <+> prettyResource Dataset ident <> "."
      , fix        = Just "make sure the dimensions of the dataset are all constants."
      }

    DatasetInvalidNat ident p v -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected elements of type" <+> squotes (prettyFriendly nat) <+>
                     "but found value" <+> squotes (pretty v)
      , fix        = Just $ "either remove the offending entries in the dataset or" <+>
                     "update the type of the dataset in the specification."
      } where (nat :: CheckedExpr) = NatType mempty

    DatasetInvalidIndex ident p v n -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected elements of type" <+> squotes (prettyFriendly index) <+>
                     "but found value" <+> squotes (pretty v)
      , fix        = Just $ "either remove the offending entries in the dataset or" <+>
                     "update the type of the dataset in the specification."
      }
      where (index :: CheckedExpr) = IndexType mempty (NatLiteralExpr mempty (NatType mempty) n)

    DatasetDimensionMismatch ident p expectedType actualDims -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected type to be" <+> prettyFriendlyDBClosed expectedType <+>
                     "but found dimensions" <+> pretty actualDims
      , fix        = Just "correct the dataset dimensions in the specification."
      }

    DatasetTypeMismatch ident p expectedType actualType -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected dataset elements to be of type" <+> prettyFriendly expectedType <+>
                     "but found elements of type" <+> pretty actualType
      , fix        = Just "correct the dataset type in the specification."
      }

    -- Parameter errors

    ParameterTypeUnsupported ident p expectedType -> UError $ UserError
      { provenance = p
      , problem    = unsupportedResourceTypeDescription Parameter ident expectedType <> "." <+>
                     supportedParameterTypeDescription
      , fix        = Just "change the parameter type in the specification."
      }

    ParameterValueUnparsable ident p expectedType value -> UError $ UserError
      { provenance = p
      , problem    = "Error while parsing the value provided for" <+>
                     prettyResource Parameter ident <> "." <+>
                     "Expected the value to be of type" <+>
                     squotes (prettyFriendly expectedType) <+>
                     "but found" <+> squotes (pretty value)
      , fix        = Just $ "either fix the type of the parameter in the" <+>
                     "specification or change the value provided."
      }

    ParameterTypeVariableSizeIndex ident p fullType -> UError $ UserError
      { provenance = p
      , problem    = "An Index with variable dimensions" <+> squotes (prettyFriendly fullType) <+>
                     "is not a supported type for the" <+> prettyResource Parameter ident <> "."
      , fix        = Just "make sure the dimensions of the indices are all constants."
      }

    --------------------
    -- Backend errors --
    --------------------

    UnsupportedResource target ident p resource ->
      let dType = squotes (pretty resource) in UError $ UserError
      { provenance = p
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a" <+> dType <+> "declaration which" <+>
                     "cannot be compiled."
      , fix        = Just $ "remove all" <+> dType <+> "declarations or switch to a" <+>
                     "different compilation target."
      }

    UnsupportedSequentialQuantifiers target ident p q pq pp -> UError $ UserError
      { provenance = p
      , problem    = "The property" <+> squotes (pretty ident) <+> "contains" <+>
                     "a sequence of quantifiers unsupported by" <+> pretty target <> "." <>
                     line <>
                     pretty target <+> "cannot verify properties that mix both" <+>
                     squotes (pretty Forall) <+> "and" <+> squotes (pretty Exists) <+>
                     "quantifiers." <>
                     line <>
                     "In particular the" <+> squotes (pretty q) <+> "at" <+>
                     pretty pq <+> "clashes with" <+>
                     prettyPolarityProvenance (neg q) pp
      , fix        = Just $ "if possible try reformulating" <+> squotes (pretty ident) <+>
                    "in terms of a single type of quantifier."
      }

    UnsupportedVariableType target ident p name t supportedTypes -> UError $ UserError
      { provenance = p
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a quantified variable" <+> squotes (pretty name) <+> "of type" <+>
                     squotes (prettyFriendlyDBClosed t) <+> "which is not currently supported" <+>
                     "when compiling to" <+> pretty target <> "."
      , fix        = Just $ "try switching the variable to one of the following supported types:" <+>
                     pretty supportedTypes
      }

    UnsupportedBuiltin target p builtin -> UError $ UserError
      { provenance = p
      , problem    = "Compilation of" <+> squotes (pretty builtin) <+> "to" <+>
                     pretty target <+> "is not currently supported."
      , fix        = Just $ "Try avoiding it, otherwise please open an issue on the" <+>
                     "Vehicle issue tracker."
      }

    UnsupportedInequality target identifier p -> UError $ UserError
      { provenance = p
      , problem    = "After compilation, property" <+> squotes (pretty identifier) <+>
                     "contains a `!=` which is not current supported by" <+>
                     pretty target <> ". See https://github.com/vehicle-lang/vehicle/issues/74" <+>
                     "for details."
      , fix        = Just "not easy, needs fixing upstream in Marabou."
      }

    UnsupportedPolymorphicEquality target p typeName -> UError $ UserError
      { provenance = p
      , problem    = "The use of equality over the unknown type" <+>
                     squotes (pretty typeName) <+> "is not currently supported" <+>
                     "when compiling to" <+> pretty target
      , fix        = Just $ "try avoiding it, otherwise open an issue on the" <+>
                     "Vehicle issue tracker describing the use case."
      }

    UnsupportedNonMagicVariable target p name -> UError $ UserError
      { provenance = p
      , problem    = "The variable" <+> squotes (pretty name) <+> "is not used as" <+>
                     "an input to a network, which is not currently supported" <+>
                     "by" <+> pretty target
      , fix        = Just $ "try reformulating the property, or else open an issue on the" <+>
                     "Vehicle issue tracker describing the use-case."
      }

    NonLinearConstraint target p ident _ v1 v2 -> UError $ UserError
      { provenance = p
      , problem    = "Property" <+> pretty ident <+> "contains a non-linear" <+>
                     "constraint (in particular constraint contains" <+>
                     squotes (prettyFriendly v1 <> "*" <> prettyFriendly v2) <+>
                     "which is not currently supported by" <+> pretty target
      , fix        = Just $ "try avoiding it, otherwise please open an issue on the" <+>
                     "Vehicle issue tracker."
      }

    NoPropertiesFound -> UError $ UserError
      { provenance = mempty
      , problem    = "No properties found in file."
      , fix        = Just $ "an expression is labelled as a property by giving it type" <+> squotes (pretty Bool) <+> "."
      }

    NoNetworkUsedInProperty target ann ident -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "After normalisation, the property" <+>
                     squotes (pretty ident) <+>
                     "does not contain any neural networks and" <+>
                     "therefore" <+> pretty target <+> "is the wrong compilation target"
      , fix        = Just "choose a different compilation target than VNNLib"
      }

unsupportedResourceTypeDescription :: ResourceType -> Identifier -> CheckedExpr -> Doc a
unsupportedResourceTypeDescription resource ident actualType =
  "The type" <+> squotes (prettyFriendlyDBClosed actualType) <+> "of" <+> pretty resource <+>
  squotes (pretty ident) <+> "is not currently supported"

supportedNetworkTypeDescription :: Doc a
supportedNetworkTypeDescription =
  let allowedElementTypes = prettyFriendlyDBClosed <$> allowedNetworkElementTypes in
  "Only networks of the following types are allowed:" <> line <>
  indent 2 "Tensor A [m] -> Tensor B [n]" <> line <>
  "where 'A' and 'B' are one of" <+> prettyFlatList allowedElementTypes <+>
  "and 'm' and 'n' are constants."

supportedParameterTypeDescription :: Doc a
supportedParameterTypeDescription =
  "Only parameters of the following types are allowed:" <> line <>
  indent 2 (
    "1." <+> "Bool"    <> line <>
    "2." <+> "Index n" <> line <>
    "3." <+> "Nat"     <> line <>
    "4." <+> "Int"     <> line <>
    "5." <+> "Rat" )

unsolvedConstraintError :: Constraint -> [DBBinding] -> Doc a
unsolvedConstraintError constraint ctx ="Typing error: not enough information to solve constraint" <+>
  case constraint of
    UC _ (Unify _)   ->  prettyFriendlyDB ctx constraint
    TC _ (_ `Has` t) ->  prettyFriendlyDB ctx t
    PC _ t           ->  prettyFriendlyDB ctx t

prettyResource :: ResourceType -> Identifier -> Doc a
prettyResource resourceType ident = pretty resourceType <+> squotes (pretty ident)

prettyPolarityProvenance :: Quantifier -> PolarityProvenance -> Doc a
prettyPolarityProvenance quantifier = \case
  QuantifierProvenance p ->
    "the" <+> squotes (pretty quantifier) <+> "at" <+> pretty p
  polProv ->
    prettyQuantifierArticle quantifier <+>
    "derived as follows:" <> line <>
    indent 2 (numberedList $ reverse (go quantifier polProv))
  where
    go :: Quantifier -> PolarityProvenance -> [Doc a]
    go q = \case
      QuantifierProvenance p ->
        ["the" <+> squotes (pretty q) <+> "is originally located at" <+> pretty p]
      LHSImpliesProvenance p pp ->
        "which is turned into" <+> prettyQuantifierArticle q <+>
        "by being on the LHS of the" <+> squotes (pretty (BooleanOp2 Impl)) <+>
        "at" <+> pretty p
        : go (neg q) pp
      NegationProvenance p pp ->
        "which is turned into" <+> prettyQuantifierArticle q <+>
        "by the" <+> squotes (pretty Not) <+> "at" <+> pretty p
        : go (neg q) pp

--------------------------------------------------------------------------------
-- Constraint error messages

failedConstraintError :: [DBBinding]
                      -> Constraint
                      -> UserError
failedConstraintError ctx c@(UC _ (Unify (t1, t2))) = UserError
  { provenance = provenanceOf c
  , problem    = "Type error:" <+>
                    prettyFriendlyDB ctx t1 <+> "!=" <+> prettyFriendlyDB ctx t2
  , fix        = Just "check your types"
  }
failedConstraintError ctx c@(TC _ (_ `Has` t)) = UserError
  { provenance = provenanceOf c
  , problem    = "Type error:" <+>
                    "Could not satisfy" <+> squotes (prettyFriendlyDB ctx t)
  , fix        = Just "check your types"
  }
failedConstraintError ctx (PC _ t) = developerError $
  "Should not have unsolved polarity constraint:" <+> squotes (prettyFriendlyDB ctx t)

{-
-- Some attempts more readable error messages. Need something more principled,
e.g. Jurrian's work.

    TC _ (_ `Has` (HasNatLitsUpToExpr _ n (IndexType _ (LiteralExpr _ _ v)))) -> UserError
      { provenance = p
      , problem    = "Type error: index" <+> pretty n <+> "is out of bounds when" <+>
                    "looking up value in tensor of size" <+> pretty v
      , fix        = Just "check your types"
      }

    TC _ (_ `Has` (HasNatLitsUpToExpr _ n (IndexType _ v))) -> UserError
      { provenance = p
      , problem    = "Type error: unknown if index" <+> pretty n <+> "is in bounds" <+>
                    "when looking up value in tensor of size" <+> prettyFriendlyDB ctx v
      , fix        = Just "check your types"
      }
-}