
module Vehicle.Compile.Error where

import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Backend.Verifier

data CompileError
  = ParseError        ParseError
  | CoreElabError     CoreElabError
  | FrontendElabError FrontendElabError
  | ScopeError        ScopeError

--------------------------------------------------------------------------------
-- Parse errors

newtype ParseError
  = BNFCParseError String

class AsParseError e where
  _BNFCParseError :: String -> e

instance AsParseError ParseError where
  _BNFCParseError = BNFCParseError

instance AsParseError CompileError where
  _BNFCParseError = ParseError . BNFCParseError

--------------------------------------------------------------------------------
-- Elaboration errors

-- |Type of errors thrown during elaboration.
data CoreElabError
  = UnknownBuiltin     Token
  | MalformedPiBinder  Token
  | MalformedLamBinder InputExpr

class AsCoreElabError e where
  _UnknownBuiltin     :: Token     -> e
  _MalformedPiBinder  :: Token     -> e
  _MalformedLamBinder :: InputExpr -> e

instance AsCoreElabError CoreElabError where
  _UnknownBuiltin     = UnknownBuiltin
  _MalformedPiBinder  = MalformedPiBinder
  _MalformedLamBinder = MalformedLamBinder

instance AsCoreElabError CompileError where
  _UnknownBuiltin     tk = CoreElabError $ UnknownBuiltin     tk
  _MalformedPiBinder  tk = CoreElabError $ MalformedPiBinder  tk
  _MalformedLamBinder e  = CoreElabError $ MalformedLamBinder e

--------------------------------------------------------------------------------
-- Frontend elaboration

data FrontendElabError
  = MissingDefFunType    Provenance Symbol
  | MissingDefFunExpr    Provenance Symbol
  | DuplicateName        (NonEmpty Provenance) Symbol
  | MissingVariables     Provenance Symbol

class AsFrontendElabError e where
  _MissingDefFunType :: Provenance -> Symbol -> e
  _MissingDefFunExpr :: Provenance -> Symbol -> e
  _DuplicateName     :: NonEmpty Provenance -> Symbol -> e
  _MissingVariables  :: Provenance -> Symbol -> e

instance AsFrontendElabError FrontendElabError where
  _MissingDefFunType = MissingDefFunType
  _MissingDefFunExpr = MissingDefFunExpr
  _DuplicateName     = DuplicateName
  _MissingVariables  = MissingVariables

instance AsFrontendElabError CompileError where
  _MissingDefFunType p s = FrontendElabError $ MissingDefFunType p s
  _MissingDefFunExpr p s = FrontendElabError $ MissingDefFunExpr p s
  _DuplicateName     p s = FrontendElabError $ DuplicateName     p s
  _MissingVariables  p s = FrontendElabError $ MissingVariables  p s

--------------------------------------------------------------------------------
-- Scope checking

-- |Type of errors thrown by scope checking.
data ScopeError
  = UnboundName Symbol Provenance
  deriving Show

class AsScopeError e where
  _UnboundName :: Symbol -> Provenance -> e

instance AsScopeError ScopeError where
  _UnboundName = UnboundName

instance AsScopeError CompileError where
  _UnboundName s p = ScopeError $ UnboundName s p

--------------------------------------------------------------------------------
-- Type checking

-- | Errors thrown during type checking
data TypingError
  = UnresolvedHole
    Provenance              -- The location of the hole
    Symbol                  -- The name of the hole
  | Mismatch
    Provenance              -- The location of the mismatch.
    BoundCtx                -- The context at the time of the failure
    CheckedExpr             -- The possible inferred types.
    CheckedExpr             -- The expected type.
  | FailedConstraints
    (NonEmpty Constraint)
  | UnsolvedConstraints
    (NonEmpty Constraint)
  | MissingExplicitArg
    BoundCtx                -- The context at the time of the failure
    UncheckedArg            -- The non-explicit argument
    CheckedExpr             -- Expected type of the argument


--------------------------------------------------------------------------------
-- Normalisation

-- |Errors thrown during normalisation
newtype NormError
  = EmptyQuantifierDomain Provenance

--------------------------------------------------------------------------------
-- Agda errors

-- TODO fold into Agda error
data ContainerDimensionError
  = VariableTensorTypeDimensions OutputExpr
  | VariableTensorTypeDimension OutputExpr
  | EmptyTensorSize
  | TensorIndexOutOfBounds Int Int

-- * Type of errors that can be thrown during compilation
data AgdaError
  = CompilationUnsupported  Provenance (Doc Void)
  | ContainerDimensionError Provenance ContainerDimensionError

--------------------------------------------------------------------------------
-- SMTLib errors

-- | Reasons why we might not support the network type.
-- Options with `Bool` type equate
data UnsupportedNetworkType
  = NotAFunction
  | NotATensor             InputOrOutput
  | MultidimensionalTensor InputOrOutput
  | VariableSizeTensor     InputOrOutput
  | WrongTensorType        InputOrOutput

instance Pretty UnsupportedNetworkType where
  pretty = \case
    NotAFunction              -> "the network type is not a function"
    NotATensor io             -> "the" <+> pretty io <+> "of the network is not a tensor"
    MultidimensionalTensor io -> "the" <+> pretty io <+> "of the network is not a 1D tensor"
    VariableSizeTensor io     -> "the" <+> pretty io <+> "of the network is a tensor with a non-fixed dimension"
    WrongTensorType io        -> "the type of the" <+> pretty io <+> "tensor of the network is not supported"

data SMTLibError
  = UnsupportedDecl               Provenance Identifier DeclType
  | UnsupportedVariableType       CheckedAnn Identifier Symbol CheckedExpr [Builtin]
  | UnsupportedQuantifierSequence CheckedAnn Identifier
  | NonTopLevelQuantifier         CheckedAnn Identifier Quantifier Symbol
  | NoPropertiesFound
  -- VNNLib
  | UnsupportedNetworkType        CheckedAnn Identifier CheckedExpr UnsupportedNetworkType
  | NoNetworkUsedInProperty       CheckedAnn Identifier