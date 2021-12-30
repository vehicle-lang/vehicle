module Vehicle.Compile.Error.Message where

import Data.Void ( Void )
import Data.Text ( Text, pack )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Foldable (fold)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Language.Print
import Vehicle.Compile.Type.Constraint (boundContext)
import Control.Monad.Except (ExceptT, runExceptT)
import System.Exit (exitFailure)

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
-- IO

fromEitherIO :: Either CompileError a -> IO a
fromEitherIO (Left err) = do print $ details err; exitFailure
fromEitherIO (Right x)  = return x

fromLoggedEitherIO :: LogFilePath -> ExceptT CompileError Logger a -> IO a
fromLoggedEitherIO logFile x = fromEitherIO =<< fromLoggedIO logFile (logCompileError x)

logCompileError :: ExceptT CompileError Logger a -> Logger (Either CompileError a)
logCompileError x = do
  e' <- runExceptT x
  case e' of
    Left err -> logDebug ("Error thrown:" <+> pretty (show err))
    Right _  -> return ()
  return e'

fromLoggedIO :: LogFilePath -> Logger a -> IO a
fromLoggedIO Nothing        logger = return $ discardLogger logger
fromLoggedIO (Just logFile) logger = flushLogs logFile logger

--------------------------------------------------------------------------------
-- Meaningful error classes

class MeaningfulError e where
  details :: e -> VehicleError

instance MeaningfulError CompileError where
  details = \case
    -- Parsing

    BNFCParseError text -> EError $ ExternalError
      -- TODO need to revamp this error, BNFC must provide some more
      -- information than a simple string surely?
      (pack text)

    -- Elaboration Core

    UnknownBuiltin tk -> UError $ UserError
      { problem    = "Unknown symbol" <+> pretty (tkSymbol tk)
      , provenance = tkProvenance tk
      , fix        = "Please consult the documentation for a description of Vehicle syntax"
      }

    MalformedPiBinder tk -> UError $ UserError
      { problem    = "Malformed binder for Pi, expected a type but only found name" <+> pretty (tkSymbol tk)
      , provenance = tkProvenance tk
      , fix        = "Unknown"
      }

    MalformedLamBinder expr -> UError $ UserError
      { problem    = "Malformed binder for Lambda, expected a name but only found an expression" <+> prettyVerbose expr
      , provenance = provenanceOf expr
      , fix        = "Unknown"
      }

    -- Elaboration Frontend

    MissingDefFunType p name -> UError $ UserError
      { problem    = "missing type for the declaration" <+> squotes (pretty name)
      , provenance = p
      , fix        = "add a type for the declaration, e.g."
                    <> line <> line
                    <> "addOne :: Int -> Int    <-----   type declaration" <> line
                    <> "addOne x = x + 1"
      }

    MissingDefFunExpr p name -> UError $ UserError
      { problem    = "missing definition for the declaration" <+> squotes (pretty name)
      , provenance = p
      , fix        = "add a definition for the declaration, e.g."
                    <> line <> line
                    <> "addOne :: Int -> Int" <> line
                    <> "addOne x = x + 1     <-----   declaration definition"
      }

    DuplicateName p name -> UError $ UserError
      { problem    = "multiple definitions found with the name" <+> squotes (pretty name)
      , provenance = fold p
      , fix        = "remove or rename the duplicate definitions"
      }

    MissingVariables p symbol -> UError $ UserError
      { problem    = "expected at least one variable name after" <+> squotes (pretty symbol)
      , provenance = p
      , fix        = "add one or more names after" <+> squotes (pretty symbol)
      }

    -- Scoping

    UnboundName name p -> UError $ UserError
      { problem    = "The name" <+> squotes (pretty name) <+> "is not in scope"
      , provenance = p
      -- TODO can use Levenschtein distance to search contexts/builtins
      , fix        = pretty ("Unknown" :: String)
      }

    -- Typing

    TypeMismatch p ctx candidate expected -> UError $ UserError
      { provenance = p
      , problem    = "expected something of type" <+> prettyFriendlyDB nameCtx expected <+>
                    "but inferred type" <+> prettyFriendlyDB nameCtx candidate
      , fix        = "unknown"
      } where nameCtx = ctxNames ctx

    UnresolvedHole p name -> UError $ UserError
      { provenance = p
      , problem    = "the type of" <+> squotes (pretty name) <+> "could not be resolved"
      , fix        = "unknown"
      }

    FailedConstraints cs -> UError $ UserError
      { provenance = provenanceOf constraint
      , problem    = "Could not solve the constraint:" <+> prettyFriendlyDB nameCtx constraint
      , fix        = "Check your types"
      }
      where
        constraint = NonEmpty.head cs
        nameCtx = ctxNames (boundContext constraint)

    UnsolvedConstraints cs -> UError $ UserError
      { provenance = provenanceOf constraint
      , problem    = "unsolved constraint " <+> prettyFriendlyDB nameCtx constraint
      , fix        = "Try adding more type annotations"
      }
      where
        constraint = NonEmpty.head cs
        nameCtx    = ctxNames (boundContext constraint)

    MissingExplicitArg ctx arg argType -> UError $ UserError
      { provenance = provenanceOf arg
      , problem    = "expected an" <+> pretty Explicit <+> "argument of type" <+>
                    argTypeDoc <+> "but instead found" <+>
                    pretty (visibilityOf arg) <+> "argument" <+> squotes argExprDoc
      , fix        = "Try inserting an argument of type" <+> argTypeDoc
      }
      where
        nameCtx    = ctxNames ctx
        argExprDoc = prettyFriendlyDB nameCtx (argExpr arg)
        argTypeDoc = prettyFriendlyDB nameCtx argType

    -- Network type errors

    NetworkTypeIsNotAFunction ident networkType -> UError $ UserError
      { provenance = provenanceOf networkType
      , problem    = unsupportedNetworkTypeDescription ident networkType <+> "as" <+>
                     "it is not a function type." <+> supportedNetworkTypeDescrption
      , fix        = "Provide both an input type and output type for your network."
      }

    NetworkTypeWithNonExplicitArguments ident networkType binder -> UError $ UserError
      { provenance = provenanceOf binder
      , problem    = unsupportedNetworkTypeDescription ident networkType <+> "as" <+>
                     "it contains a non-explicit argument" <+>
                     squotes (prettyFriendly binder) <> "." <+>
                     supportedNetworkTypeDescrption
      , fix        = "Remove the non-explicit argument."
      }

    NetworkTypeWithHeterogeneousInputTypes ident networkType t1 t2 -> UError $ UserError
      { provenance = provenanceOf t1 <> provenanceOf t2
      , problem    = unsupportedNetworkTypeDescription ident networkType <+> "as" <+>
                     "it contains heterogeneous input types" <+>
                     squotes (prettyFriendly t1) <+> "and" <+>
                     squotes (prettyFriendly t2) <+> "." <+>
                     supportedNetworkTypeDescrption
      , fix        = "Ensure that all the inputs to" <+> squotes (pretty ident) <+>
                     "are the same type."
      }

    NetworkTypeUnsupportedElementType ident networkType elementType io -> UError $ UserError
      { provenance = provenanceOf elementType
      , problem    = unsupportedNetworkTypeDescription ident networkType <+> "as" <+>
                     "the" <+> pretty io <+> "type" <+>
                     squotes (prettyFriendly elementType) <+>
                     "is not currently supported." <+>
                     supportedNetworkTypeDescrption
      , fix        = "Ensure that the network" <+> pretty io <+> "is a 1D tensor."
      }

    NetworkTypeHasMultidimensionalTensor ident networkType t io -> UError $ UserError
      { provenance = provenanceOf t
      , problem    = unsupportedNetworkTypeDescription ident networkType <+> "as" <+>
                     "the" <+> pretty io <+> "of the network is not a 1D tensor." <+>
                     supportedNetworkTypeDescrption
      , fix        = "Ensure that the network" <+> pretty io <+> "is a 1D tensor."
      }

    NetworkTypeHasVariableSizeTensor ident networkType tDim io -> UError $ UserError
      { provenance = provenanceOf tDim
      , problem    = unsupportedNetworkTypeDescription ident networkType <+> "as" <+>
                     squotes (pretty $ show tDim) <+> "is not a constant." <+>
                     supportedNetworkTypeDescrption
      , fix        = "Ensure that the size of the" <+> pretty io <+> "tensor is constant."
      }

    -- Backend errors
    LookupInVariableDimTensor target p dims -> UError $ UserError
      { provenance = p
      , problem    = "When targeting" <+> pretty target <+> ", cannot lookup a value in" <+>
                     "a tensor with the variable dimensions" <+> prettyFriendly dims
      , fix        = "see user manual for details"
      }

    LookupInEmptyTensor target p -> UError $ UserError
      { provenance = p
      , problem    = "When targeting" <+> pretty target <+> ", cannot lookup a value in" <+>
                     "a zero-dimensional tensor."
      , fix        = "see user manual for details"
      }

    TensorIndexOutOfBounds p size index -> UError $ UserError
      { provenance = p
      , problem    = "Looking up index" <+> pretty index <+>
                     "in a tensor whose first dimension is of size" <+> pretty size
      , fix        = "see user manual for details"
      }

    UnsupportedDecl target ann ident decType ->
      let dType = squotes (pretty decType) in UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a" <+> dType <+> "declaration which" <+>
                     "cannot be compiled."
      , fix        = "Remove all" <+> dType <+> "declarations or switch to a" <+>
                     "different compilation target."
      }

    UnsupportedQuantifierSequence target p ident q -> UError $ UserError
      { provenance = p
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a" <+> prettyQuant q <+> "quantifier after a" <+>
                     prettyQuant (neg q) <+> "quantifier. Only homogeneous sequences of" <+>
                     prettyQuant All <+> "s or" <+> prettyQuant Any <+>
                     "quantifiers are currently supported."
      , fix        = "If possible try reformulating your property in terms of a single quantifier type."
      } where prettyQuant quant = squotes (pretty (Quant quant))

    UnsupportedQuantifierPosition target p ident quantifier name -> UError $ UserError
      { provenance = p
      , problem    = "While compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a non top-level quantifier" <+>
                     squotes (prettyQuant quantifier <+> pretty name) <+> "which is not" <+>
                     "currently supported when compiling to" <+> pretty target <> "."
      , fix        = "Refactor your property to lift the quantifier to the top-level."
      } where prettyQuant quant = squotes (pretty (Quant quant))

    UnsupportedVariableType target ann ident name t supportedTypes -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "to" <+>
                     pretty target <+> "found a quantified variable" <+> squotes (pretty name) <+> "of type" <+>
                     squotes (prettyFriendlyDBClosed t) <+> "which is not currently supported" <+>
                     "when compiling to" <+> pretty target <> "."
      , fix        = "Try switching the variable to one of the following supported types:" <+>
                     pretty supportedTypes
      }

    UnsupportedBuiltin target p builtin -> UError $ UserError
      { provenance = p
      , problem    = "Compilation of" <+> squotes (pretty builtin) <+> "to" <+>
                     pretty target <+> "is not currently supported."
      , fix        = "Try avoiding it, otherwise please open an issue on the" <+>
                     "Vehicle issue tracker."
      }

    UnsupportedOrder target p quantifier order -> UError $ UserError
      { provenance = p
      , problem    = "The use of" <+> squotes (pretty actualOrder) <+> "inside of an" <+>
                     quantifierAdverb quantifier <+> "quantifier property" <+>
                     "is not currently supported by" <+> pretty target
      , fix        = "You can use" <+> squotes (pretty (counterpart actualOrder)) <+>
                     "instead, otherwise please open an issue on the" <+> pretty target <+>
                     "issue tracker to ask for support."
      } where actualOrder = if quantifier == All then counterpart order else order

    UnsupportedEquality target p quantifier eq -> UError $ UserError
      { provenance = p
      , problem    = "The use of" <+> squotes (pretty actualEq) <+> "inside of an" <+>
                     quantifierAdverb quantifier <+> "quantifier property" <+>
                     "is not currently supported by" <+> pretty target
      , fix        = "Instead of an equality you can try bounding the region using" <+>
                     squotes (pretty less) <+> "and" <+>  squotes (pretty greater) <+>
                     "instead, otherwise please open an issue on the" <+> pretty target <+>
                     "issue tracker to ask for support."
      } where
          actualEq = if quantifier == All then neg eq else eq
          (less, greater) = if quantifier == All then (Lt, Gt) else (Le, Ge)

    UnsupportedPolymorphicEquality target p typeName -> UError $ UserError
      { provenance = p
      , problem    = "The use of equality over the unknown type" <+>
                     squotes (pretty typeName) <+> "is not currently supported" <+>
                     "when compiling to" <+> pretty target
      , fix        = "Try avoiding it, otherwise please open an issue on the" <+>
                     "Vehicle issue tracker."
      }

    NonLinearConstraint target p ident v1 v2 -> UError $ UserError
      { provenance = p
      , problem    = "Property" <+> pretty ident <+> "contains a non-linear" <+>
                     "constraint (in particular constraint contains" <+>
                     squotes (prettyFriendly v1 <> "*" <> prettyFriendly v2) <+>
                     "which is not currently supported by" <+> pretty target
      , fix        = "Try avoiding it, otherwise please open an issue on the" <+>
                     "Vehicle issue tracker."
      }


    NoPropertiesFound -> UError $ UserError
      { provenance = mempty
      , problem    = "No properties found in file."
      , fix        = "An expression is labelled as a property by giving it type" <+> squotes (pretty Prop) <+> "."
      }

    NoNetworkUsedInProperty target ann ident -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "After normalisation, the property" <+>
                     squotes (pretty ident) <+>
                     "does not contain any neural networks and" <+>
                     "therefore" <+> pretty target <+> "is the wrong compilation target"
      , fix        = "Choose a different compilation target than VNNLib"
      }

unsupportedNetworkTypeDescription :: Identifier -> CheckedExpr -> Doc a
unsupportedNetworkTypeDescription ident t =
  "The type" <+> squotes (prettyFriendly t) <+> "of" <+> pretty Network <+>
  squotes (pretty ident) <+> "is not currently supported"

supportedNetworkTypeDescrption :: Doc a
supportedNetworkTypeDescrption =
  "Only networks of type:" <> line <> indent 2 (
     "1." <+> squotes "Tensor A [m] -> Tensor B [n]" <> line <>
     "2." <+> squotes "A -> ... -> A -> B") <> line <>
  "are allowed, where A and B are one of" <+> pretty allowedNetworkElementTypes <+>
  "and" <+> squotes "m" <+> "and" <+> squotes "n" <+> "are constants."