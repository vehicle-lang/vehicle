{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Scoping where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core
import Vehicle.Compile.Sugar.Core
import Vehicle.Data.AST.Expr.Desugared qualified as D (Expr (..), normAppList)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Vehicle.Data.Tensor (pattern ZeroDimTensor)
import Vehicle.Libraries.StandardLibrary

instance ScopableBuiltin Builtin where
  generateAuxiliaryRecordDefinitions p ident sort telescope fields
    | isAnnotatedAsTensor sort = createTensorRecordConversionFunctions p ident telescope fields
    | otherwise = return [createRecordHasValidIOTypeInstance p ident fields]

instance DesugarableBuiltin Builtin where
  elabUnitLiteral p = D.Builtin p $ BuiltinConstructor UnitLiteral
  elabBoolLiteral p = D.Builtin p . BuiltinConstructor . BoolTensorLiteral . ZeroDimTensor
  elabNatLiteral p n = do
    let fromNat = D.Builtin p (TypeClassOp FromNatTC)
    D.normAppList fromNat $ fmap explicit [D.Builtin p $ BuiltinConstructor $ NatLiteral n]
  elabDecimalLiteral p r = do
    let fromRat = D.Builtin p (TypeClassOp FromRatTC)
    D.normAppList fromRat $ fmap explicit [D.Builtin p $ BuiltinConstructor $ RatTensorLiteral $ ZeroDimTensor r]

createRecordHasValidIOTypeInstance ::
  Provenance ->
  Identifier ->
  RecordFields Builtin ->
  Decl Builtin
createRecordHasValidIOTypeInstance p recordIdent fields = do
  -- For each record R we want to create a function that looks like:
  --
  --   @instance
  --   recordRHasValidNetworkIOType :
  --     {{HasValidNetworkFieldType t1}} ->
  --     ...
  --     {{HasValidNetworkFieldType tn}} ->
  --     HasValidNetworkIOType R
  --   recordRHasValidNetworkIOType = {}
  --
  -- ... where t1 through tn are the types of R's fields.

  -- Create the name
  let instanceName = Text.pack "record" <> nameOf recordIdent <> "HasValidNetworkIOType"
  let instanceIdent = Identifier (modulePath recordIdent) instanceName

  -- Create the type
  let convertFieldToConstraint f = freeVar validNetworkIOTypeIdent @@ [toDSL . snd $ f]
  let recordType' = freeVar validNetworkIOTypeIdent @@ [freeVar recordIdent]
  let instanceType = fromDSL mempty $ foldr (\field currentType -> convertFieldToConstraint field ~~~> currentType) recordType' fields

  -- Create the function body
  let recordType = fromDSL mempty recordType'
  let functionBody = Record p recordType []

  DefFunction p instanceIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) instanceType functionBody

createTensorRecordConversionFunctions ::
  (MonadCompile m) =>
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  RecordFields Builtin ->
  m [Decl Builtin]
createTensorRecordConversionFunctions p ident telescope fields = do
  unless (null telescope) $
    throwError $
      UnimplementedFeature p ("Annotating parameterised records with" <+> pretty AnnTensor)

  nonEmptyFields <- case fields of
    [] -> throwError $ ZeroFieldTensorLike (ident, p)
    f : fs -> return $ f :| fs

  -- We can't actually know the element and the field types at scope checking
  -- time because the user may be using type synonyms for the tensors, e.g.
  --
  --    @tensor
  --    record Input where
  --      { red   : Image
  --      , green : Image
  --      , blue  : Image
  --      }
  --
  -- but if we make holes for them, the type checker should be able to fill
  -- them in.
  let fieldElementType = hole
  let fieldDimensions = hole

  let recordToTensorDecl = createRecordToTensor p ident fieldElementType fieldDimensions nonEmptyFields
  let tensorToRecordDecl = createTensorToRecord p ident fieldElementType fieldDimensions nonEmptyFields
  let tensorLikeInstance = createTensorLikeInstance p ident fieldElementType fieldDimensions nonEmptyFields
  let validNetworkInstance = createValidNetworkIOInstance p ident
  let validQuantifierInstance = createTensorLikeHasQuantifierInstance p ident

  return
    [recordToTensorDecl, tensorToRecordDecl, tensorLikeInstance, validNetworkInstance, validQuantifierInstance]

createRecordToTensor ::
  Provenance ->
  Identifier ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  NonEmpty (GenericRecordField (Type Builtin)) ->
  Decl Builtin
createRecordToTensor p recordIdent fieldElementType fieldDimensions fields = do
  -- Create the name
  let functionName = Text.pack "_" <> nameOf recordIdent <> "ToTensor"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  -- Create the type
  let firstDimension = dim (length fields)
  let allDimensions = dimCons firstDimension fieldDimensions
  let recordType = freeVar recordIdent
  let functionType = fromDSL mempty $ recordType ~> tTensor fieldElementType allDimensions

  -- Create the body
  let functionBody = fromDSL mempty $ explLam "x" recordType $ \r -> do
        let tensorElements = fmap (\(fieldName, _) -> recordProj (freeVar recordIdent) r fieldName) fields
        stackTensor fieldElementType firstDimension fieldDimensions tensorElements

  DefFunction p functionIdent (FunctionDecl 1 Nothing) functionType functionBody

createTensorToRecord ::
  Provenance ->
  Identifier ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  NonEmpty (GenericRecordField (Type Builtin)) ->
  Decl Builtin
createTensorToRecord p recordIdent fieldElementType fieldDimensions fields = do
  -- Create the name
  let functionName = Text.pack "_" <> nameOf recordIdent <> "FromTensor"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  -- Create the type
  let firstDimension = dim (length fields)
  let recordType = freeVar recordIdent
  let tensorType = tTensor fieldElementType (dimCons firstDimension fieldDimensions)
  let functionType = fromDSL mempty $ tensorType ~> recordType

  let fieldNames = fmap fst (toList fields)
  let tensorIndices = fmap indexLit ([0 .. length fields - 1] :: [Int]) :: [DSLExpr Builtin]

  -- Create the body
  let functionBody = fromDSL mempty $ explLam "x" tensorType $ \tensor -> do
        let fieldContents = fmap (\index -> atTensor fieldElementType firstDimension fieldDimensions tensor index) tensorIndices
        record recordType (zip fieldNames fieldContents)

  DefFunction p functionIdent (FunctionDecl 1 Nothing) functionType functionBody

createTensorLikeInstance ::
  Provenance ->
  Identifier ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  NonEmpty (GenericRecordField (Type Builtin)) ->
  Decl Builtin
createTensorLikeInstance p recordIdent fieldElementType fieldDimensions fields = do
  -- Create ident for TensorLike typeclass
  let tensorLikeIdent = Identifier standardLibraryDefinitionsModulePath "TensorLike"

  -- Create record type
  let firstDimension = dim (length fields)
  let allDimensions = dimCons firstDimension fieldDimensions
  let recordType = fromDSL mempty $ freeVar tensorLikeIdent @@ [freeVar recordIdent, fieldElementType, allDimensions]

  -- Create record expression for the function body
  let toTensorFieldName = FieldName p "toTensor"
  let fromTensorFieldName = FieldName p "fromTensor"
  let toTensorIdent = Identifier (modulePath recordIdent) (Text.pack "_" <> nameOf recordIdent <> "ToTensor")
  let fromTensorIdent = Identifier (modulePath recordIdent) (Text.pack "_" <> nameOf recordIdent <> "FromTensor")
  let recordFields = [(toTensorFieldName, fromDSL mempty (freeVar toTensorIdent)), (fromTensorFieldName, fromDSL mempty (freeVar fromTensorIdent))]
  let functionBody = Record p recordType recordFields

  -- Create ident for the function
  let functionName = Text.pack "_" <> nameOf recordIdent <> "IsTensorLike"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

createValidNetworkIOInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createValidNetworkIOInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar validNetworkIOTypeIdent @@ [freeVar recordIdent]
  let functionBody = Record p recordType []

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasValidNetworkIOType"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

createTensorLikeHasQuantifierInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createTensorLikeHasQuantifierInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar hasQuantifierIdent @@ [freeVar recordIdent]

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasQuantifier"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  let forAllTCFieldName = FieldName p "forallTC"
  let existsTCFieldName = FieldName p "existsTC"

  let functionBody =
        Record
          p
          recordType
          [ (forAllTCFieldName, fromDSL mempty (builtinFunction (QuantifyTensorLike Forall))),
            (existsTCFieldName, fromDSL mempty (builtinFunction (QuantifyTensorLike Exists)))
          ]

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody
