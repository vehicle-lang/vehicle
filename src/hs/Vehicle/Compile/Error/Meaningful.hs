

module Vehicle.Compile.Error.Meaningful where

import Data.Void ( Void )
import Data.Text ( Text, pack )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Foldable (fold)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- User errors

-- |Errors that are the user's responsibility to fix.
data UserError = UserError
  { provenance :: Provenance
  , problem    :: Doc Void
  , fix        :: Doc Void
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
    unAnnotate $ "Error:" <+> appendProvenance prob p <> line <> fixText probFix

  pretty (EError (ExternalError text)) =
    pretty text

instance Show VehicleError where
  show = layoutAsString . pretty

appendProvenance :: Doc ann -> Provenance -> Doc ann
appendProvenance doc p = doc <+> "(" <> pretty p <> ")"

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t

--------------------------------------------------------------------------------
-- Meaningful error classes

class MeaningfulError e where
  details :: e -> VehicleError

instance MeaningfulError CompileError where
  details = \case
    ParseError        e -> details e
    CoreElabError     e -> details e
    FrontendElabError e -> details e
    ScopeError        e -> details e
    TypeError         e -> details e
    NormError         e -> details e
    AgdaError         e -> details e
    SMTLibError       e -> details e

instance MeaningfulError ParseError where
  -- TODO need to revamp this error, BNFC must provide some more
  -- information than a simple string surely?
  details (BNFCParseError text) = EError $ ExternalError (pack text)

instance MeaningfulError CoreElabError where
  details (UnknownBuiltin tk) = UError $ UserError
    { problem    = "Unknown symbol" <+> pretty (tkSymbol tk)
    , provenance = tkProvenance tk
    , fix        = "Please consult the documentation for a description of Vehicle syntax"
    }

  details (MalformedPiBinder tk) = UError $ UserError
    { problem    = "Malformed binder for Pi, expected a type but only found name" <+> pretty (tkSymbol tk)
    , provenance = tkProvenance tk
    , fix        = "Unknown"
    }

  details (MalformedLamBinder expr) = UError $ UserError
    { problem    = "Malformed binder for Lambda, expected a name but only found an expression" <+> prettyVerbose expr
    , provenance = provenanceOf expr
    , fix        = "Unknown"
    }

instance MeaningfulError FrontendElabError where
  details (MissingDefFunType p name) = UError $ UserError
    { problem    = "missing type for the declaration" <+> squotes (pretty name)
    , provenance = p
    , fix        = "add a type for the declaration, e.g."
                   <> line <> line
                   <> "addOne :: Int -> Int    <-----   type declaration" <> line
                   <> "addOne x = x + 1"
    }

  details (MissingDefFunExpr p name) = UError $ UserError
    { problem    = "missing definition for the declaration" <+> squotes (pretty name)
    , provenance = p
    , fix        = "add a definition for the declaration, e.g."
                   <> line <> line
                   <> "addOne :: Int -> Int" <> line
                   <> "addOne x = x + 1     <-----   declaration definition"
    }

  details (DuplicateName p name) = UError $ UserError
    { problem    = "multiple definitions found with the name" <+> squotes (pretty name)
    , provenance = fold p
    , fix        = "remove or rename the duplicate definitions"
    }

  details (MissingVariables p symbol) = UError $ UserError
    { problem    = "expected at least one variable name after" <+> squotes (pretty symbol)
    , provenance = p
    , fix        = "add one or more names after" <+> squotes (pretty symbol)
    }

instance MeaningfulError ScopeError where
  details  (UnboundName name p) = UError $ UserError
    { problem    = "The name" <+> squotes (pretty name) <+> "is not in scope"
    , provenance = p
    -- TODO can use Levenschtein distance to search contexts/builtins
    , fix        = pretty ("Unknown" :: String)
    }

instance MeaningfulError TypeError where
  details (Mismatch p ctx candidate expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> prettyFriendlyDB nameCtx expected <+>
                   "but inferred type" <+> prettyFriendlyDB nameCtx candidate
    , fix        = "unknown"
    } where nameCtx = fmap fst ctx

  details (UnresolvedHole p name) = UError $ UserError
    { provenance = p
    , problem    = "the type of" <+> squotes (pretty name) <+> "could not be resolved"
    , fix        = "unknown"
    }

  details (FailedConstraints cs) = let constraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = provenanceOf constraint
    , problem    = "Could not solve the constraint:" <+> prettyFriendly constraint
    , fix        = "Check your types"
    }

  details (UnsolvedConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = provenanceOf firstConstraint
    , problem    = "unsolved constraint " <+> prettyFriendly firstConstraint
    , fix        = "Try adding more type annotations"
    }

  details (MissingExplicitArg ctx arg argType) = UError $ UserError
    { provenance = provenanceOf arg
    , problem    = "expected an" <+> pretty Explicit <+> "argument of type" <+>
                   argTypeDoc <+> "but instead found" <+>
                   pretty (visibilityOf arg) <+> "argument" <+> squotes argExprDoc
    , fix        = "Try inserting an argument of type" <+> argTypeDoc
    }
    where
      nameCtx    = fmap fst ctx
      argExprDoc = prettyFriendlyDB nameCtx (argExpr arg)
      argTypeDoc = prettyFriendlyDB nameCtx argType

instance MeaningfulError NormError where
  details (EmptyQuantifierDomain p) = UError $ UserError
    { problem    = "Quantifying over an empty domain"
    , provenance = p
    , fix        = "Check your definition of the domain"
    }

instance MeaningfulError AgdaError where
  details (CompilationUnsupported p operation) = UError $ UserError
    { provenance = p
    , problem    = "compilation of" <+> squotes operation <+> "is not supported"
    , fix        = "see user manual for details"
    }

  details (ContainerDimensionError p err) = UError $ UserError
    { provenance = p
    , problem    = problem
    , fix        = "see user manual for details"
    }
      where
        problem = case err of
          VariableTensorTypeDimensions ex ->
            "cannot compile a tensor with the variable dimensions" <+> prettyFriendly ex

          VariableTensorTypeDimension ex ->
            "cannot compile a tensor with the variable first dimension" <+> prettyFriendly ex

          EmptyTensorSize ->
            "cannot compile a zero-dimensional tensor"

          TensorIndexOutOfBounds size index ->
            "index" <+> pretty index <+> "is larger than the first dimension" <+> pretty size <+>
            "when trying to compile" <+> pretty At

instance MeaningfulError SMTLibError where
  details = \case
    UnsupportedDecl ann ident decType -> let dType = squotes (pretty decType) in UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found" <+>
                     "a" <+> dType <+> "declaration which cannot be compiled to SMTLib."
      , fix        = "Remove all" <+> dType <+> "declarations or switch to a" <+>
                     "different compilation target."
      }

    UnsupportedVariableType ann ident name t supportedTypes -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found" <+>
                     "a quantified variable" <+> squotes (pretty name) <+> "of type" <+>
                     squotes (prettyFriendlyDBClosed t) <+> "which is not currently supported" <+>
                     "when compiling to SMTLib."
      , fix        = "Try switching the variable to one of the following supported types:" <+>
                     pretty supportedTypes
      }

    UnsupportedQuantifierSequence ann ident -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found a mixed" <+>
                     "sequence of quantifiers which is not currently supported when compiling" <+>
                     "to SMTLib. All properties must either be a sequence of" <+>
                     squotes (pretty (Quant All)) <+> "s or" <+> squotes (pretty (Quant Any)) <+> "s"
      , fix        = "If possible try removing some quantifiers."
      }

    NonTopLevelQuantifier ann ident q n -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found a non-top" <+>
                     "level quantifier" <+> squotes (pretty (Quant q) <+> pretty n) <+>
                     "which is not currently supported when compiling to SMTLib."
      , fix        = "Lift all quantifiers to the top-level"
      }

    NoPropertiesFound -> UError $ UserError
      { provenance = mempty
      , problem    = "No properties found in file."
      , fix        = "An expression is labelled as a property by giving it type" <+> squotes (pretty Prop) <+> "."
      }

    -- VNNLib
    UnsupportedNetworkType ann ident t detailedError -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "Found a" <+> squotes (pretty Network) <+> "declaration" <+> squotes (pretty ident) <+>
                     "whose type" <+> squotes (prettyFriendlyDBClosed t) <+> "is not currently unsupported." <+>
                     "Currently only networks of type" <+> squotes "Tensor A [m] -> Tensor B [n]" <+>
                     "where" <+> squotes "m" <+> "and" <+> squotes "n" <+> "are integer literals are allowed." <+>
                     "In particular" <+> pretty detailedError <+> "."
      , fix        = "Change the network type."
      }

    NoNetworkUsedInProperty ann ident -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "After normalisation, the property" <+>
                     squotes (pretty ident) <+>
                     "does not contain any neural networks and" <+>
                     "therefore VNNLib is the wrong compilation target"
      , fix        = "Choose a different compilation target than VNNLib"
      }

