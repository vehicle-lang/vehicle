module Vehicle.Compile.Error where

import Control.Exception (IOException)
import Control.Monad.Except ( MonadError, throwError )
import Data.List.NonEmpty (NonEmpty)
import Prettyprinter (list)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Verify.Core (VerifierIdentifier)
import Vehicle.Backend.Prelude

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
  | FunctionNotGivenBody         Provenance Symbol
  | PropertyNotGivenBody         Provenance Symbol
  | ResourceGivenBody            Provenance Symbol Symbol
  | AnnotationWithNoDeclaration  Provenance Symbol
  | FunctionWithMismatchedNames  Provenance Symbol Symbol
  | MissingVariables             Provenance Symbol
  | UnchainableOrders            Provenance OrderOp OrderOp
  | InvalidAnnotationOption      Provenance Symbol Symbol [Symbol]
  | InvalidAnnotationOptionValue Provenance Symbol Symbol

  -- Errors thrown by scope checking.
  | UnboundName Provenance Symbol
  | DuplicateName Provenance Symbol

  -- Errors thrown while type checking
  | UnresolvedHole
    Provenance              -- The location of the hole
    Symbol                  -- The name of the hole
  | TypeMismatch
    Provenance              -- The location of the mismatch.
    [DBBinding]             -- The context at the time of the failure
    CheckedType             -- The possible inferred types.
    CheckedType             -- The expected type.
  | UnsolvedConstraints
    (NonEmpty Constraint)
  | UnsolvedMetas
    (NonEmpty (Meta, Provenance))
  | MissingExplicitArg
    [DBBinding]             -- The context at the time of the failure
    UncheckedArg            -- The non-explicit argument
    CheckedType             -- Expected type of the argument
  | FailedConstraints
    (NonEmpty Constraint)
  | FailedEqConstraint               ConstraintContext CheckedType CheckedType EqualityOp
  | FailedOrdConstraint              ConstraintContext CheckedType CheckedType OrderOp
  | FailedBuiltinConstraintArgument  ConstraintContext Builtin CheckedType [InputExpr] Int Int
  | FailedBuiltinConstraintResult    ConstraintContext Builtin CheckedType [InputExpr]
  | FailedNotConstraint              ConstraintContext CheckedType
  | FailedBoolOp2Constraint          ConstraintContext CheckedType CheckedType Builtin
  | FailedQuantifierConstraintDomain ConstraintContext CheckedType Quantifier
  | FailedQuantifierConstraintBody   ConstraintContext CheckedType Quantifier
  | FailedArithOp2Constraint         ConstraintContext CheckedType CheckedType Builtin
  | FailedFoldConstraintContainer    ConstraintContext CheckedType
  | FailedQuantInConstraintContainer ConstraintContext CheckedType Quantifier
  | FailedNatLitConstraint           ConstraintContext Int CheckedType
  | FailedNatLitConstraintTooBig     ConstraintContext Int Int
  | FailedNatLitConstraintUnknown    ConstraintContext Int CheckedType
  | FailedIntLitConstraint           ConstraintContext CheckedType
  | FailedRatLitConstraint           ConstraintContext CheckedType
  | FailedConLitConstraint           ConstraintContext CheckedType

  | QuantifiedIfCondition ConstraintContext
  | NonLinearIfCondition  ConstraintContext

  -- Resource typing errors
  | ResourceNotProvided       DeclProvenance ResourceType
  | ResourceIOError           DeclProvenance ResourceType IOException
  | UnsupportedResourceFormat DeclProvenance ResourceType String
  | UnableToParseResource     DeclProvenance ResourceType String

  | NetworkTypeIsNotAFunction              DeclProvenance CheckedType
  | NetworkTypeIsNotOverTensors            DeclProvenance CheckedType CheckedType InputOrOutput
  | NetworkTypeHasNonExplicitArguments     DeclProvenance CheckedType CheckedBinder
  | NetworkTypeHasVariableSizeTensor       DeclProvenance CheckedType CheckedExpr InputOrOutput
  | NetworkTypeHasImplicitSizeTensor       DeclProvenance Identifier InputOrOutput
  | NetworkTypeHasUnsupportedElementType   DeclProvenance CheckedType CheckedType InputOrOutput

  | DatasetTypeUnsupportedContainer DeclProvenance CheckedType
  | DatasetTypeUnsupportedElement   DeclProvenance CheckedType
  | DatasetVariableSizeTensor       DeclProvenance CheckedExpr
  | DatasetDimensionSizeMismatch    DeclProvenance FilePath Int Int [Int] [Int]
  | DatasetDimensionsMismatch       DeclProvenance FilePath CheckedExpr [Int]
  | DatasetTypeMismatch             DeclProvenance FilePath CheckedType CheckedType
  | DatasetInvalidIndex             DeclProvenance FilePath Int Int
  | DatasetInvalidNat               DeclProvenance FilePath Int

  | ParameterTypeUnsupported             DeclProvenance CheckedType
  | ParameterTypeVariableSizeIndex       DeclProvenance CheckedExpr
  | ParameterTypeInferableParameterIndex DeclProvenance Identifier
  | ParameterValueUnparsable             DeclProvenance String Builtin
  | ParameterValueInvalidIndex           DeclProvenance Int Int
  | ParameterValueInvalidNat             DeclProvenance Int
  | InferableParameterTypeUnsupported    DeclProvenance CheckedType
  | InferableParameterContradictory      Identifier (DeclProvenance, ResourceType, Int) (DeclProvenance, ResourceType, Int)
  | InferableParameterUninferrable       DeclProvenance

  | PropertyTypeUnsupported         DeclProvenance CheckedType

  -- Backend errors
  | NoPropertiesFound
  | UnsupportedResource              Backend Identifier Provenance ResourceType
  | UnsupportedInequality            Backend Identifier Provenance
  | UnsupportedPolymorphicEquality   Backend Provenance Symbol
  | UnsupportedBuiltin               Backend Provenance Builtin
  | UnsupportedNonMagicVariable      Backend Provenance Symbol
  | NoNetworkUsedInProperty          Backend Provenance Identifier
  | UnsupportedVariableType           VerifierIdentifier Identifier Provenance Symbol CheckedType [Builtin]
  | UnsupportedAlternatingQuantifiers Backend DeclProvenance Quantifier Provenance PolarityProvenance
  | UnsupportedNonLinearConstraint   Backend DeclProvenance Provenance LinearityProvenance LinearityProvenance
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

unexpectedTypeInExprError :: MonadError CompileError m => Doc () -> Doc () -> m b
unexpectedTypeInExprError pass name = compilerDeveloperError $
  unexpectedExpr pass name <+> "We should not be processing types."

illTypedError :: MonadError CompileError m => Doc () -> Doc () -> m b
illTypedError pass name = compilerDeveloperError $
  unexpectedExpr pass name <+> "This is ill-typed."

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