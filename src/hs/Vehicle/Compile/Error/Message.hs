module Vehicle.Compile.Error.Message
  ( UserError(..)
  , VehicleError(..)
  , MeaningfulError(..)
  , fromLoggedEitherIO
  , logCompileError
  ) where

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
import Vehicle.NeuralNetwork

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

appendProvenance :: Doc ann -> Provenance -> Doc ann
appendProvenance doc p = doc <+> "(" <> pretty p <> ")"

fixText :: Doc ann -> Doc ann
fixText t = "Fix:" <+> t

--------------------------------------------------------------------------------
-- IO

fromEitherIO :: LoggingOptions -> Either CompileError a -> IO a
fromEitherIO _              (Right x)  = return x
fromEitherIO loggingOptions (Left err) =
  fatalError loggingOptions $ pretty $ details err

fromLoggedEitherIO :: LoggingOptions -> ExceptT CompileError Logger a -> IO a
fromLoggedEitherIO loggingOptions x = do
  fromEitherIO loggingOptions =<< fromLoggedIO loggingOptions (logCompileError x)

logCompileError :: ExceptT CompileError Logger a -> Logger (Either CompileError a)
logCompileError x = do
  e' <- runExceptT x
  case e' of
    Left err -> logDebug (pretty (details err))
    Right _  -> return ()
  return e'

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

    -- Elaboration internal

    UnknownBuiltin tk -> UError $ UserError
      { provenance = tkProvenance tk
      , problem    = "Unknown symbol" <+> pretty (tkSymbol tk)
      , fix        = "Please consult the documentation for a description of Vehicle syntax"
      }

    MalformedPiBinder tk -> UError $ UserError
      { provenance = tkProvenance tk
      , problem    = "Malformed binder for Pi, expected a type but only found name" <+> pretty (tkSymbol tk)
      , fix        = "Unknown"
      }

    MalformedLamBinder expr -> UError $ UserError
      { provenance = provenanceOf expr
      , problem    = "Malformed binder for Lambda, expected a name but only found an expression" <+> prettyVerbose expr
      , fix        = "Unknown"
      }

    -- Elaboration external

    MissingDefFunExpr p name -> UError $ UserError
      { provenance = p
      , problem    = "missing definition for the declaration" <+> squotes (pretty name)
      , fix        = "add a definition for the declaration, e.g."
                    <> line <> line
                    <> "addOne :: Int -> Int" <> line
                    <> "addOne x = x + 1     <-----   declaration definition"
      }

    DuplicateName p name -> UError $ UserError
      { provenance = fold p
      , problem    = "multiple definitions found with the name" <+> squotes (pretty name)
      , fix        = "remove or rename the duplicate definitions"
      }

    MissingVariables p symbol -> UError $ UserError
      { provenance = p
      , problem    = "expected at least one variable name after" <+> squotes (pretty symbol)
      , fix        = "add one or more names after" <+> squotes (pretty symbol)
      }

    UnchainableOrders p prevOrder currentOrder -> UError $ UserError
      { provenance = p
      , problem    = "cannot chain" <+> squotes (pretty prevOrder) <+>
                     "and" <+> squotes (pretty currentOrder)
      , fix        = "split chained orders into a conjunction"
      }

    -- Scoping

    UnboundName name p -> UError $ UserError
      { provenance = p
      -- TODO can use Levenschtein distance to search contexts/builtins
      , problem    = "The name" <+> squotes (pretty name) <+> "is not in scope"
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
      , problem    = failedConstraintError constraint nameCtx
      , fix        = "Check your types"
      }
      where
        constraint = NonEmpty.head cs
        nameCtx = ctxNames (boundContext constraint)

    UnsolvedConstraints cs -> UError $ UserError
      { provenance = provenanceOf constraint
      , problem    = unsolvedConstraintError constraint nameCtx
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
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     squotes (prettyFriendly networkType) <+> "is not a function."
      , fix        = supportedNetworkTypeDescription <+>
                     "Provide both an input type and output type for your network."
      }

    NetworkTypeWithNonExplicitArguments ident networkType binder -> UError $ UserError
      { provenance = provenanceOf binder
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     squotes (prettyFriendly networkType) <+>
                     "contains a non-explicit argument" <+>
                     squotes (prettyFriendly binder) <> "."
      , fix        = supportedNetworkTypeDescription <+>
                     "Remove the non-explicit argument."
      }

    NetworkTypeWithHeterogeneousInputTypes ident networkType t1 t2 -> UError $ UserError
      { provenance = provenanceOf t1 <> provenanceOf t2
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     squotes (prettyFriendly networkType) <+>
                     "contains heterogeneous input types" <+>
                     squotes (prettyFriendly t1) <+> "and" <+>
                     squotes (prettyFriendly t2) <+> "."
      , fix        = supportedNetworkTypeDescription <+>
                     "Ensure that all the inputs to" <+> squotes (pretty ident) <+>
                     "are the same type."
      }

    NetworkTypeUnsupportedElementType ident elementType io -> UError $ UserError
      { provenance = provenanceOf elementType
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     pretty io <+> "s of type" <+>
                     squotes (prettyFriendly elementType) <+>
                     "are not currently supported."
      , fix        = supportedNetworkTypeDescription <+>
                     "Ensure that the network" <+> pretty io <+> "uses" <+>
                     "supported types."
      }

    NetworkTypeHasMultidimensionalTensor ident t io -> UError $ UserError
      { provenance = provenanceOf t
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     "the" <+> pretty io <+>
                     squotes (prettyFriendly t) <+> "is not a 1D tensor."
      , fix        = supportedNetworkTypeDescription <+>
                     "Ensure that the network" <+> pretty io <+> "is a 1D tensor."
      }

    NetworkTypeHasVariableSizeTensor ident tDim io -> UError $ UserError
      { provenance = provenanceOf tDim
      , problem    = unsupportedNetworkTypeDescription ident <+> "as" <+>
                     "the size of the" <+> pretty io <+> "tensor" <+>
                     squotes (pretty $ show tDim) <+> "is not a constant."
      , fix        = supportedNetworkTypeDescription <+>
                     "Ensure that the size of the" <+> pretty io <+> "tensor is constant."
      }

    -- Backend errors
    LookupInVariableDimTensor target p dims -> UError $ UserError
      { provenance = p
      , problem    = "When targeting" <+> pretty target <+> ", cannot lookup a value in" <+>
                     "a tensor with the variable dimensions" <+> prettyFriendly dims
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
      , fix        = "You can use" <+> squotes (pretty (flipStrictness actualOrder)) <+>
                     "instead, otherwise please open an issue on the" <+> pretty target <+>
                     "issue tracker to ask for support."
      } where actualOrder = if quantifier == All then flipStrictness order else order

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
      , fix        = "Try avoiding it, otherwise open an issue on the" <+>
                     "Vehicle issue tracker."
      }

    UnsupportedNonMagicVariable target p name -> UError $ UserError
      { provenance = p
      , problem    = "The variable" <+> squotes (pretty name) <+> "is not used as" <+>
                     "an input to a network, which is not currently supported" <+>
                     "by" <+> pretty target
      , fix        = "Try reformulating the property, or else open an issue on the" <+>
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
  case baseConstraint constraint of
    Unify _   ->  prettyFriendlyDB ctx constraint
    _ `Has` t ->  prettyFriendlyDB ctx t

failedConstraintError :: Constraint -> [DBBinding] -> Doc a
failedConstraintError constraint ctx = "Type error:" <+> case baseConstraint constraint of
  Unify (t1, t2) ->
    prettyFriendlyDB ctx t1 <+> "!=" <+> prettyFriendlyDB ctx t2

  _ `Has` (HasNatLitsUpToExpr _ n (FinType _ (LiteralExpr _ _ v))) ->
    "Index" <+> pretty n <+> "is out of bounds when looking up value in tensor of size" <+> pretty v

  _ `Has` (HasNatLitsUpToExpr _ n (FinType _ v)) ->
    "Unknown if index" <+> pretty n <+> "is in bounds when looking up value in tensor of size" <+> prettyFriendlyDB ctx v

  _ `Has` t -> "Could not satisfy" <+> squotes (prettyFriendlyDB ctx t)