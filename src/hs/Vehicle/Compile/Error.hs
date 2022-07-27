module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except ( MonadError, throwError )
import Data.List.NonEmpty (NonEmpty)
import Prettyprinter (list)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Backend.Prelude (Backend)

--------------------------------------------------------------------------------
-- Compilation monad

type MonadCompile m =
  ( MonadLogger m
  , MonadError CompileError m
  )

--------------------------------------------------------------------------------
-- Compilation errors

data CompileError
  = DevError (Doc ())

  -- Parse errors
  | BNFCParseError String

  -- Errors thrown when elaborating from the BNFC internal language
  | UnknownBuiltin     Token
  | MalformedPiBinder  Token
  | MalformedLamBinder InputExpr

  -- Errors thrown when elaborating from the BNFC external language
  | MissingDefFunExpr    Provenance Symbol
  | DuplicateName        (NonEmpty Provenance) Symbol
  | MissingVariables     Provenance Symbol
  | UnchainableOrders    Provenance OrderOp OrderOp

  -- Errors thrown by scope checking.
  | UnboundName Symbol Provenance

  -- Errors thrown while type checking
  | UnresolvedHole
    Provenance              -- The location of the hole
    Symbol                  -- The name of the hole
  | TypeMismatch
    Provenance              -- The location of the mismatch.
    [DBBinding]             -- The context at the time of the failure
    CheckedExpr             -- The possible inferred types.
    CheckedExpr             -- The expected type.
  | UnsolvedConstraints
    (NonEmpty Constraint)
  | UnsolvedMetas
    (NonEmpty (Meta, Provenance))
  | MissingExplicitArg
    [DBBinding]             -- The context at the time of the failure
    UncheckedArg            -- The non-explicit argument
    CheckedExpr             -- Expected type of the argument
  | FailedConstraints
    (NonEmpty Constraint)
  | FailedEqConstraint               ConstraintContext CheckedExpr CheckedExpr EqualityOp
  | FailedOrdConstraint              ConstraintContext CheckedExpr CheckedExpr OrderOp
  | FailedBuiltinConstraintArgument  ConstraintContext Builtin CheckedExpr [InputExpr] Int Int
  | FailedBuiltinConstraintResult    ConstraintContext Builtin CheckedExpr [InputExpr]
  | FailedNotConstraint              ConstraintContext CheckedExpr
  | FailedBoolOp2Constraint          ConstraintContext CheckedExpr CheckedExpr Builtin
  | FailedQuantifierConstraintDomain ConstraintContext CheckedExpr Quantifier
  | FailedQuantifierConstraintBody   ConstraintContext CheckedExpr Quantifier
  | FailedArithOp2Constraint         ConstraintContext CheckedExpr CheckedExpr Builtin
  | FailedFoldConstraintContainer    ConstraintContext CheckedExpr
  | FailedQuantInConstraintContainer ConstraintContext CheckedExpr Quantifier
  | FailedNatLitConstraint           ConstraintContext Int CheckedExpr
  | FailedNatLitConstraintTooBig     ConstraintContext Int Int
  | FailedNatLitConstraintUnknown    ConstraintContext Int CheckedExpr
  | FailedIntLitConstraint           ConstraintContext CheckedExpr
  | FailedRatLitConstraint           ConstraintContext CheckedExpr
  | FailedConLitConstraint           ConstraintContext CheckedExpr

  -- Resource typing errors
  | ResourceNotProvided       DeclProvenance ResourceType
  | ResourceIOError           DeclProvenance ResourceType IOException
  | UnsupportedResourceFormat DeclProvenance ResourceType String
  | UnableToParseResource     DeclProvenance ResourceType String

  | NetworkTypeIsNotAFunction              DeclProvenance CheckedExpr
  | NetworkTypeIsNotOverTensors            DeclProvenance CheckedExpr CheckedExpr InputOrOutput
  | NetworkTypeHasNonExplicitArguments     DeclProvenance CheckedExpr CheckedBinder
  | NetworkTypeHasVariableSizeTensor       DeclProvenance CheckedExpr CheckedExpr InputOrOutput
  | NetworkTypeHasImplicitSizeTensor       DeclProvenance Identifier InputOrOutput
  | NetworkTypeHasUnsupportedElementType   DeclProvenance CheckedExpr CheckedExpr InputOrOutput

  | DatasetTypeUnsupportedContainer DeclProvenance CheckedExpr
  | DatasetTypeUnsupportedElement   DeclProvenance CheckedExpr
  | DatasetVariableSizeTensor       DeclProvenance CheckedExpr
  | DatasetDimensionMismatch        DeclProvenance CheckedExpr [Int]
  | DatasetTypeMismatch             DeclProvenance CheckedExpr CheckedExpr
  | DatasetInvalidNat               DeclProvenance Int
  | DatasetInvalidIndex             DeclProvenance Int Int

  | ParameterTypeUnsupported        DeclProvenance CheckedExpr
  | ParameterTypeVariableSizeIndex  DeclProvenance CheckedExpr
  | ParameterTypeImplicitParamIndex DeclProvenance Identifier
  | ParameterValueUnparsable        DeclProvenance CheckedExpr String

  | ImplicitParameterTypeUnsupported DeclProvenance CheckedExpr
  | ImplicitParameterContradictory   Identifier (DeclProvenance, ResourceType, Int) (DeclProvenance, ResourceType, Int)

  -- Backend errors
  | NoPropertiesFound
  | UnsupportedResource              Backend Identifier Provenance ResourceType
  | UnsupportedSequentialQuantifiers Backend DeclProvenance Quantifier Provenance PolarityProvenance
  | UnsupportedVariableType          Backend Identifier Provenance Symbol CheckedExpr [Builtin]
  | UnsupportedInequality            Backend Identifier Provenance
  | UnsupportedPolymorphicEquality   Backend Provenance Symbol
  | UnsupportedBuiltin               Backend Provenance Builtin
  | UnsupportedNonMagicVariable      Backend Provenance Symbol
  | UnsupportedNonLinearConstraint   Backend DeclProvenance LinearityProvenance LinearityProvenance
  | NoNetworkUsedInProperty          Backend Provenance Identifier
  deriving (Show)

--------------------------------------------------------------------------------
-- Some useful developer errors

-- | Should be used in preference to `developerError` whenever in the error
-- monad, as unlike the latter this method does not prevent logging.
compilerDeveloperError :: MonadError CompileError m => Doc () -> m b
compilerDeveloperError message = throwError $ DevError message

unexpectedExpr :: Doc a -> Doc a -> Doc a
unexpectedExpr pass name =
  "encountered unexpected expression" <+> squotes name <+>
  "during" <+> pass <> "."

unexpectedExprError :: MonadError CompileError m => Doc () -> Doc () -> m b
unexpectedExprError pass name = compilerDeveloperError $ unexpectedExpr pass name

normalisationError :: MonadError CompileError m => Doc () -> Doc () -> m b
normalisationError pass name = compilerDeveloperError $
  unexpectedExpr pass name <+> "We should have normalised this out."

typeError :: MonadError CompileError m => Doc () -> Doc () -> m b
typeError pass name = compilerDeveloperError $
  unexpectedExpr pass name <+> "We should not be processing types."

visibilityError :: MonadError CompileError m => Doc () -> Doc () -> m b
visibilityError pass name = compilerDeveloperError $
  unexpectedExpr pass name <+> "Should not be present as explicit arguments"

-- | Throw this when you encounter a case that should have been resolved during
-- type-checking, e.g. holes or metas.
resolutionError :: MonadError CompileError m => Doc () -> Doc () -> m b
resolutionError pass name = compilerDeveloperError $
  unexpectedExpr pass name <+> "We should have resolved this during type-checking."

caseError :: MonadError CompileError m => Doc () -> Doc () -> [Doc ()] -> m b
caseError pass name cases = compilerDeveloperError $
  unexpectedExpr pass name <+> "This should already have been caught by the" <+>
  "following cases:" <+> list cases

allowedType :: Builtin -> InputExpr
allowedType = let p = mempty in \case
  Nat                  -> NatType p
  Int                  -> IntType p
  Rat                  -> RatType p
  Index                -> IndexType p (Var p "n")
  List                 -> ListType p (Var p "A")
  Tensor               -> TensorType p (Var p "A") (Var p "dims")
  b                    -> developerError
    $ "Builtin" <+> squotes (pretty b) <+> "not yet supported for allowed translation"

allowed :: [Builtin] -> [InputExpr]
allowed = fmap allowedType