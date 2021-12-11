
module Vehicle.Compile.Error where

import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Constraint

--------------------------------------------------------------------------------
-- Compilation errors

-- There's a bit of type-class magic going on in this file. We want to have a
-- general error type `CompilationError` that can be thrown at any stage of the
-- compilation pipeline, but we also want to be able to be have an specific
-- error class for each stage of the pipeline. Therefore for each individual
-- error `X`, e.g. `ParseError`, we also have a type-class `AsX`, e.g.
-- `AsParseError`, for which we have instances of both `X` and `CompileError`.
-- This allows each pipeline stage to be agnostic as to whether it's throwing
-- a `CompileError` or an `X` error. This is a lot more code in this file, but
-- hopefully the flexibility is worth it. Note that at the cost of greatly
-- increasing our dependency footprint, we could auto-generate this code with
-- prisms from the `lens` package.

data CompileError
  = ParseError                  ParseError
  | CoreElabError               CoreElabError
  | FrontendElabError           FrontendElabError
  | ScopeError                  ScopeError
  | TypeError                   TypeError
  | NormError                   NormError
  | NetworkStandardisationError NetworkStandardisationError
  | AgdaError                   AgdaError
  | SMTLibError                 SMTLibError
  deriving (Show)

--------------------------------------------------------------------------------
-- Parse errors

newtype ParseError
  = BNFCParseError String
  deriving (Show)

class AsParseError e where
  mkBNFCParseError :: String -> e

instance AsParseError ParseError where
  mkBNFCParseError = BNFCParseError

instance AsParseError CompileError where
  mkBNFCParseError = ParseError . BNFCParseError

--------------------------------------------------------------------------------
-- Elaboration errors

-- |Type of errors thrown during elaboration.
data CoreElabError
  = UnknownBuiltin     Token
  | MalformedPiBinder  Token
  | MalformedLamBinder InputExpr
  deriving (Show)

class AsCoreElabError e where
  mkUnknownBuiltin     :: Token     -> e
  mkMalformedPiBinder  :: Token     -> e
  mkMalformedLamBinder :: InputExpr -> e

instance AsCoreElabError CoreElabError where
  mkUnknownBuiltin     = UnknownBuiltin
  mkMalformedPiBinder  = MalformedPiBinder
  mkMalformedLamBinder = MalformedLamBinder

instance AsCoreElabError CompileError where
  mkUnknownBuiltin     tk = CoreElabError $ UnknownBuiltin     tk
  mkMalformedPiBinder  tk = CoreElabError $ MalformedPiBinder  tk
  mkMalformedLamBinder e  = CoreElabError $ MalformedLamBinder e

--------------------------------------------------------------------------------
-- Frontend elaboration

data FrontendElabError
  = MissingDefFunType    Provenance Symbol
  | MissingDefFunExpr    Provenance Symbol
  | DuplicateName        (NonEmpty Provenance) Symbol
  | MissingVariables     Provenance Symbol
  deriving (Show)

class AsFrontendElabError e where
  mkMissingDefFunType :: Provenance -> Symbol -> e
  mkMissingDefFunExpr :: Provenance -> Symbol -> e
  mkDuplicateName     :: NonEmpty Provenance -> Symbol -> e
  mkMissingVariables  :: Provenance -> Symbol -> e

instance AsFrontendElabError FrontendElabError where
  mkMissingDefFunType = MissingDefFunType
  mkMissingDefFunExpr = MissingDefFunExpr
  mkDuplicateName     = DuplicateName
  mkMissingVariables  = MissingVariables

instance AsFrontendElabError CompileError where
  mkMissingDefFunType p s = FrontendElabError $ MissingDefFunType p s
  mkMissingDefFunExpr p s = FrontendElabError $ MissingDefFunExpr p s
  mkDuplicateName     p s = FrontendElabError $ DuplicateName     p s
  mkMissingVariables  p s = FrontendElabError $ MissingVariables  p s

--------------------------------------------------------------------------------
-- Scope checking

-- |Type of errors thrown by scope checking.
data ScopeError
  = UnboundName Symbol Provenance
  deriving Show

class AsScopeError e where
  mkUnboundName :: Symbol -> Provenance -> e

instance AsScopeError ScopeError where
  mkUnboundName = UnboundName

instance AsScopeError CompileError where
  mkUnboundName s p = ScopeError $ UnboundName s p

--------------------------------------------------------------------------------
-- Type checking

-- | Errors thrown during type checking
data TypeError
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
  deriving (Show)

class AsTypeError e where
  mkUnresolvedHole      :: Provenance -> Symbol -> e
  mkMismatch            :: Provenance -> BoundCtx -> CheckedExpr -> CheckedExpr -> e
  mkFailedConstraints   :: NonEmpty Constraint -> e
  mkUnsolvedConstraints :: NonEmpty Constraint -> e
  mkMissingExplicitArg  :: BoundCtx -> UncheckedArg -> CheckedExpr -> e

instance AsTypeError TypeError where
  mkUnresolvedHole      = UnresolvedHole
  mkMismatch            = Mismatch
  mkFailedConstraints   = FailedConstraints
  mkUnsolvedConstraints = UnsolvedConstraints
  mkMissingExplicitArg  = MissingExplicitArg

instance AsTypeError CompileError where
  mkUnresolvedHole p s            = TypeError $ UnresolvedHole p s
  mkMismatch p ctx e1 e2          = TypeError $ Mismatch p ctx e1 e2
  mkFailedConstraints cs          = TypeError $ FailedConstraints cs
  mkUnsolvedConstraints cs        = TypeError $ UnsolvedConstraints cs
  mkMissingExplicitArg ctx e1 e2  = TypeError $ MissingExplicitArg ctx e1 e2

--------------------------------------------------------------------------------
-- Normalisation

-- |Errors thrown during normalisation
newtype NormError
  = EmptyQuantifierDomain Provenance
  deriving (Show)

class AsNormError e where
  mkEmptyQuantifierDomain :: Provenance -> e

instance AsNormError NormError where
  mkEmptyQuantifierDomain = EmptyQuantifierDomain

instance AsNormError CompileError where
  mkEmptyQuantifierDomain = NormError . EmptyQuantifierDomain

--------------------------------------------------------------------------------
-- Agda errors

-- TODO fold into Agda error
data ContainerDimensionError
  = VariableTensorTypeDimensions OutputExpr
  | VariableTensorTypeDimension OutputExpr
  | EmptyTensorSize
  | TensorIndexOutOfBounds Int Int
  deriving (Show)

-- * Type of errors that can be thrown during compilation
data AgdaError
  = CompilationUnsupported  Provenance (Doc Void)
  | ContainerDimensionError Provenance ContainerDimensionError
  deriving (Show)

class AsAgdaError e where
  mkCompilationUnsupported  :: Provenance -> Doc Void -> e
  mkContainerDimensionError :: Provenance -> ContainerDimensionError -> e

instance AsAgdaError AgdaError where
  mkCompilationUnsupported  = CompilationUnsupported
  mkContainerDimensionError = ContainerDimensionError

instance AsAgdaError CompileError where
  mkCompilationUnsupported  p doc = AgdaError $ CompilationUnsupported p doc
  mkContainerDimensionError p e   = AgdaError $ ContainerDimensionError p e

--------------------------------------------------------------------------------
-- Network standardisation error

data InputOrOutput
  = Input
  | Output
  deriving (Show, Eq)

-- | Reasons why we might not support the network type.
data UnsupportedNetworkType
  = NotAFunction
  | NonExplicitArguments   CheckedBinder
  | NonEqualArguments      CheckedExpr CheckedExpr
  | NotATensor             InputOrOutput
  | MultidimensionalTensor InputOrOutput
  | VariableSizeTensor     InputOrOutput
  | WrongTensorType        InputOrOutput
  deriving (Show)

data NetworkStandardisationError
  = UnsupportedNetworkType  CheckedAnn Identifier CheckedExpr [Builtin] UnsupportedNetworkType
  deriving (Show)

class AsNetworkStandardisationError e where
  mkUnsupportedNetworkType  :: CheckedAnn -> Identifier -> CheckedExpr -> [Builtin] -> UnsupportedNetworkType -> e

instance AsNetworkStandardisationError NetworkStandardisationError where
  mkUnsupportedNetworkType  = UnsupportedNetworkType

instance AsNetworkStandardisationError CompileError where
  mkUnsupportedNetworkType ann ident e allowed err =
    NetworkStandardisationError $ UnsupportedNetworkType ann ident e allowed err

--------------------------------------------------------------------------------
-- SMTLib errors

data SMTLibError
  = NoPropertiesFound
  | UnsupportedDecl               Provenance Identifier DeclType
  | UnsupportedVariableType       CheckedAnn Identifier Symbol CheckedExpr [Builtin]
  | UnsupportedQuantifierSequence CheckedAnn Identifier
  | NonTopLevelQuantifier         CheckedAnn Identifier Quantifier Symbol
  -- VNNLib
  | NoNetworkUsedInProperty       CheckedAnn Identifier
  deriving (Show)

class AsSMTLibError e where
  mkNoPropertiesFound             :: e
  mkUnsupportedDecl               :: Provenance -> Identifier -> DeclType -> e
  mkUnsupportedVariableType       :: CheckedAnn -> Identifier -> Symbol -> CheckedExpr -> [Builtin] -> e
  mkUnsupportedQuantifierSequence :: CheckedAnn -> Identifier ->  e
  mkNonTopLevelQuantifier         :: CheckedAnn -> Identifier -> Quantifier -> Symbol -> e
  mkNoNetworkUsedInProperty       :: CheckedAnn -> Identifier -> e

instance AsSMTLibError SMTLibError where
  mkNoPropertiesFound             = NoPropertiesFound
  mkUnsupportedDecl               = UnsupportedDecl
  mkUnsupportedVariableType       = UnsupportedVariableType
  mkUnsupportedQuantifierSequence = UnsupportedQuantifierSequence
  mkNonTopLevelQuantifier         = NonTopLevelQuantifier
  mkNoNetworkUsedInProperty       = NoNetworkUsedInProperty

instance AsSMTLibError CompileError where
  mkNoPropertiesFound                         = SMTLibError NoPropertiesFound
  mkUnsupportedDecl p ident t                 = SMTLibError $ UnsupportedDecl p ident t
  mkUnsupportedVariableType ann ident s e bs  = SMTLibError $ UnsupportedVariableType ann ident s e bs
  mkUnsupportedQuantifierSequence ann ident   = SMTLibError $ UnsupportedQuantifierSequence ann ident
  mkNonTopLevelQuantifier ann ident q s       = SMTLibError $ NonTopLevelQuantifier ann ident q s
  mkNoNetworkUsedInProperty ann ident         = SMTLibError $ NoNetworkUsedInProperty ann ident

--------------------------------------------------------------------------------
-- Some useful developer errors

unexpectedExprError :: Doc a -> Doc a
unexpectedExprError name =
  "encountered unexpected expression" <+> squotes name <+> "during compilation."

normalisationError :: Doc a -> b
normalisationError name = developerError $
  unexpectedExprError name <+> "We should have normalised this out."

typeError :: Doc a -> b
typeError name = developerError $
  unexpectedExprError name <+> "We should not be compiling types."

visibilityError :: Doc a -> b
visibilityError name = developerError $
  unexpectedExprError name <+> "Should not be present as explicit arguments"

resolutionError :: Doc a -> b
resolutionError name = developerError $
  unexpectedExprError name <+> "We should have resolved this during type-checking."
