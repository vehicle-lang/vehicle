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
import Vehicle.Resource.NeuralNetwork

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

    -- Network type errors

    NetworkTypeIsNotAFunction ident networkType -> UError $ UserError
      { provenance = provenanceOf networkType
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     squotes (prettyFriendly networkType) <+> "is not a function."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Provide both an input type and output type for your network."
      }

    NetworkTypeHasNonExplicitArguments ident networkType binder -> UError $ UserError
      { provenance = provenanceOf binder
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     squotes (prettyFriendly networkType) <+>
                     "contains a non-explicit argument" <+>
                     squotes (prettyFriendly binder) <> "."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Remove the non-explicit argument."
      }

    NetworkTypeHasHeterogeneousInputTypes ident networkType t1 t2 -> UError $ UserError
      { provenance = provenanceOf t1 <> provenanceOf t2
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     squotes (prettyFriendly networkType) <+>
                     "contains heterogeneous input types" <+>
                     squotes (prettyFriendly t1) <+> "and" <+>
                     squotes (prettyFriendly t2) <+> "."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Ensure that all the inputs to" <+> squotes (pretty ident) <+>
                     "are the same type."
      }

    NetworkTypeHasUnsupportedElementType ident elementType io -> UError $ UserError
      { provenance = provenanceOf elementType
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     pretty io <+> "s of type" <+>
                     squotes (prettyFriendly elementType) <+>
                     "are not currently supported."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Ensure that the network" <+> pretty io <+> "uses" <+>
                     "supported types."
      }

    NetworkTypeHasMultidimensionalTensor ident t io -> UError $ UserError
      { provenance = provenanceOf t
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     "the" <+> pretty io <+>
                     squotes (prettyFriendly t) <+> "is not a 1D tensor."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "Ensure that the network" <+> pretty io <+> "is a 1D tensor."
      }

    NetworkTypeHasVariableSizeTensor ident tDim io -> UError $ UserError
      { provenance = provenanceOf tDim
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     "the size of the" <+> pretty io <+> "tensor" <+>
                     squotes (pretty $ show tDim) <+> "is not a constant."
      , fix        = Just $ supportedNetworkTypeDescription <+>
                     "ensure that the size of the" <+> pretty io <+>
                     "tensor is constant."
      }

    --------------------
    -- Backend errors --
    --------------------

    UnsupportedResource target ann ident resource ->
      let dType = squotes (pretty resource) in UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a" <+> dType <+> "declaration which" <+>
                     "cannot be compiled."
      , fix        = Just $ "remove all" <+> dType <+> "declarations or switch to a" <+>
                     "different compilation target."
      }

    UnsupportedQuantifierSequence target p ident q -> UError $ UserError
      { provenance = p
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a" <+> prettyQuant q <+> "quantifier after" <+>
                     "the opposite quantifier. Only homogeneous sequences of" <+>
                     prettyQuant Forall <+> "s or" <+> prettyQuant Exists <+>
                     "quantifiers are currently supported."
      , fix        = Just "if possible try reformulating your property in terms of a single quantifier type."
      } where prettyQuant quant = squotes (pretty (Quant quant))

    UnsupportedQuantifierPosition target p ident quantifier name -> UError $ UserError
      { provenance = p
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a non top-level quantifier" <+>
                     squotes (prettyQuant quantifier <+> pretty name) <+> "which is not" <+>
                     "currently supported when compiling to" <+> pretty target <> "."
      , fix        = Just "refactor your property to lift the quantifier to the top-level."
      } where prettyQuant quant = squotes (pretty (Quant quant))

    UnsupportedVariableType target ann ident name t supportedTypes -> UError $ UserError
      { provenance = provenanceOf ann
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

    UnsupportedInequality target p identifier -> UError $ UserError
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

unsupportedNetworkTypeDescription :: Identifier -> Doc a
unsupportedNetworkTypeDescription ident =
  "The type of" <+> pretty Network <+>
  squotes (pretty ident) <+> "is not currently supported"

supportedNetworkTypeDescription :: Doc a
supportedNetworkTypeDescription =
  "Only networks of type:" <> line <> indent 2 (
     "1." <+> "Tensor A [m] -> Tensor B [n]" <> line <>
     "2." <+> "A -> ... -> A -> B") <> line <>
  "are allowed, where A and B are one of" <+> prettyFlatList allowedNetworkElementTypes <+>
  "and" <+> squotes "m" <+> "and" <+> squotes "n" <+> "are constants."

unsolvedConstraintError :: Constraint -> [DBBinding] -> Doc a
unsolvedConstraintError constraint ctx ="Typing error: not enough information to solve constraint" <+>
  case constraint of
    UC _ (Unify _)   ->  prettyFriendlyDB ctx constraint
    TC _ (_ `Has` t) ->  prettyFriendlyDB ctx t

prettyResource :: ResourceType -> Identifier -> Doc a
prettyResource resourceType ident = pretty resourceType <+> squotes (pretty ident)

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

    DatasetInvalidContainerType ident p tCont -> UError $ UserError
      { provenance = p
      , problem    = squotes (prettyFriendly tCont) <+> "is not a valid type" <+>
                     "for the" <+> prettyResource Dataset ident <> "."
      , fix        = Just $ "change the type of" <+> squotes (pretty ident) <+>
                     "to either a" <+> squotes (pretty List) <+> "or" <+>
                     squotes (pretty Tensor) <> "."
      }

    DatasetInvalidElementType ident p tCont -> UError $ UserError
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
                     "update the type of the dataset in the Vehicle specification."
      } where (nat :: CheckedExpr) = NatType mempty

    DatasetInvalidIndex ident p v n -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected elements of type" <+> squotes (prettyFriendly fin) <+>
                     "but found value" <+> squotes (pretty v)
      , fix        = Just $ "either remove the offending entries in the dataset or" <+>
                     "update the type of the dataset in the Vehicle specification."
      }
      where (fin :: CheckedExpr) = IndexType mempty (NatLiteralExpr mempty (NatType mempty) n)

    DatasetDimensionMismatch ident p expectedDims actualDims -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected dimensions to be" <+> pretty expectedDims <+>
                     "but found dimensions" <+> pretty actualDims
      , fix        = Just "correct the dataset dimensions in the Vehicle specification."
      }

    DatasetTypeMismatch ident p expectedType actualType -> UError $ UserError
      { provenance = p
      , problem    = "Error while reading" <+> prettyResource Dataset ident <> "." <+>
                     "Expected dataset elements to be of type" <+> prettyFriendly expectedType <+>
                     "but found elements of type" <+> pretty actualType
      , fix        = Just "correct the dataset type in the Vehicle specification."
      }
-}