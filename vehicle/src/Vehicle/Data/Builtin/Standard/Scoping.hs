{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Scoping where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (MonadExpandResources)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core
import Vehicle.Compile.Sugar.Core
import Vehicle.Data.AST.Expr.Desugared qualified as D (Expr (..), normAppList)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface (getDims)
import Vehicle.Data.Code.TypedView (TypeValue (VRatTensorType), toTypeValue)
import Vehicle.Data.DSL
import Vehicle.Data.Tensor (TensorShape, pattern ZeroDimTensor)
import Vehicle.Libraries.StandardLibrary

instance ScopableBuiltin Builtin where
  generateAuxiliaryRecordDefinitions p ident sort telescope fields
    | isAnnotatedAsTensor sort = createTensorRecordConversionFunctions p ident telescope fields
    | not (isStandardLibIdent ident) = return [createRecordHasValidIOTypeInstance p ident telescope fields]
    | otherwise = return []

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
  Telescope Builtin ->
  RecordFields Builtin ->
  Decl Builtin
createRecordHasValidIOTypeInstance p recordIdent telescope fields = do
  -- For each record R we want to create a function that looks like:
  --
  --   @instance
  --   recordRHasValidNetworkIOType :
  --     {{t1}} ->
  --     ...
  --     {{tn}} ->
  --     {{HasValidNetworkFieldType f1}} ->
  --     ...
  --     {{HasValidNetworkFieldType fn}} ->
  --     HasValidNetworkIOType (R t1 ... tn)
  --   recordRHasValidNetworkIOType t1 ... tn = {}
  --
  -- ... where t1 through tn are the types in the telescope,
  -- and f1 through fn are the types of R's fields.

  -- Create the name
  let instanceName = Text.pack "record" <> nameOf recordIdent <> "HasValidNetworkIOType"
  let instanceIdent = Identifier (modulePath recordIdent) instanceName

  let mkConstraint (_, fieldType) k = flip mkInstanceBinder Nothing $ normAppList target [argument]
        where
          target = FreeVar p validNetworkFieldTypeIdent
          argument = explicit (liftDBIndices (Lv k) fieldType)

  -- Construct both the telescope and the typeclass constraints
  let implicitTelescope = fmap (flip setBinderVisibility $ Implicit True) telescope
  let constraintBinders = zipWith mkConstraint fields [0 .. length fields]
  let binderList = implicitTelescope ++ constraintBinders

  -- Create the type arguments for the fully applied record type
  let mkArg (binder, ix) = argFromBinder binder (BoundVar p ix)
  let binderIndices = reverse $ fmap Ix [0 .. (length binderList - 1)]
  let args = fmap mkArg (zip telescope binderIndices)

  -- Create the applied record and result type
  let parameterisedRecordType = normAppList (FreeVar p recordIdent) args
  let resultType = fromDSL mempty $ freeVar validNetworkIOTypeIdent @@ [toDSL parameterisedRecordType]

  -- Create the function
  let functionType = foldr (Pi p) resultType binderList
  let functionBody = foldr (Lam p) (Record p resultType []) binderList
  let functionSort = FunctionDecl 1 (Just (AnnInstance Nothing))

  DefFunction p instanceIdent functionSort functionType functionBody

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
  let validNetworkInstance = createValidNetworkIOInstance p ident
  let validNetworkFieldInstance = createValidNetworkFieldInstance p ident
  let validQuantifierInstance = createTensorLikeHasQuantifierInstance p ident
  let validHasAddInstance = createTensorLikeArithmeticInstance p ident hasAddIdent "HasAdd" "addTC"
  let validHasSubInstance = createTensorLikeArithmeticInstance p ident hasSubIdent "HasSub" "subTC"
  let validHasMulInstance = createTensorLikeArithmeticInstance p ident hasMulIdent "HasMul" "mulTC"
  let validHasDivInstance = createTensorLikeArithmeticInstance p ident hasDivIdent "HasDiv" "divTC"
  let validHasComparisonInstance = createTensorLikeComparisonInstance p ident

  return
    [ recordToTensorDecl,
      tensorToRecordDecl,
      validNetworkInstance,
      validNetworkFieldInstance,
      validQuantifierInstance,
      validHasAddInstance,
      validHasSubInstance,
      validHasDivInstance,
      validHasMulInstance,
      validHasComparisonInstance
    ]

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

createValidNetworkFieldInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createValidNetworkFieldInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar validNetworkFieldTypeIdent @@ [freeVar recordIdent]
  let functionBody = Record p recordType []

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasValidNetworkFieldType"
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
          [ (forAllTCFieldName, fromDSL mempty (builtinFunction (QuantifyRecord Forall))),
            (existsTCFieldName, fromDSL mempty (builtinFunction (QuantifyRecord Exists)))
          ]

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

createTensorLikeArithmeticInstance ::
  Provenance ->
  Identifier ->
  Identifier -> -- standard library identifier for the typeclass, e.g. hasAddIdent
  Text -> -- name of typeclass in text, e.g HasAdd
  Text -> -- name of the field for the operation in text e.g. addTC
  Decl Builtin
createTensorLikeArithmeticInstance p recordIdent typeclassIdent typeclassName fieldName = do
  let recordType = freeVar recordIdent
  let fromTensor = toDSL $ constructFromTensorFreeVar recordIdent p
  let toTensor = toDSL $ constructToTensorFreeVar recordIdent p

  let typeclass = fromDSL mempty $ freeVar typeclassIdent @@ [recordType, recordType, recordType]
  let instanceName = Text.pack "_" <> nameOf recordIdent <> typeclassName
  let instanceIdent = Identifier (modulePath recordIdent) instanceName
  let fieldIdent = freeVar $ standardLibIdent fieldName

  let field = fromDSL mempty $ explLam "r1" recordType $ \r1 ->
        explLam "r2" recordType $ \r2 -> do
          let innerAddTC = fieldIdent @@ [toTensor @@ [r1], toTensor @@ [r2]]
          fromTensor @@ [innerAddTC]

  let body = Record p typeclass [(FieldName p fieldName, field)]
  DefFunction p instanceIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) typeclass body

createTensorLikeComparisonInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createTensorLikeComparisonInstance p recordIdent = do
  let recordType = freeVar recordIdent

  let toTensor = toDSL $ constructToTensorFreeVar recordIdent p

  let typeclass = fromDSL mempty $ freeVar hasComparisonIdent @@ [recordType, recordType]
  let instanceName = Text.pack "_" <> nameOf recordIdent <> "HasComparison"

  let instanceIdent = Identifier (modulePath recordIdent) instanceName

  let fieldText = ["leTC", "ltTC", "geTC", "gtTC", "eqTC", "neTC"]
  let fieldIdents = map (freeVar . standardLibIdent) fieldText
  let fieldValues = map (createComparisonField (freeVar recordIdent) toTensor) fieldIdents
  let fieldNames = map (FieldName p) fieldText
  let fields = zip fieldNames fieldValues

  let body = Record p typeclass fields
  DefFunction p instanceIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) typeclass body

createComparisonField ::
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  Expr Builtin
createComparisonField recordType toTensor fieldIdent = do
  fromDSL mempty $ explLam "r1" recordType $ \r1 ->
    explLam "r2" recordType $ \r2 -> fieldIdent @@ [toTensor @@ [r1], toTensor @@ [r2]]

-- -----------------------------------------------------------------------------------------------
-- Record/Tensorisable util functions
-- Not sure if these should go here or if they are at the right level of abstraction

getRecordDimsExpr ::
  forall m.
  (MonadError CompileError m) =>
  FreeCtxEntry Builtin ->
  m TensorShape
getRecordDimsExpr (DefRecord _ _ _ _ fields@((_n, typ) : _fs)) = do
  case toTypeValue typ of
    (VRatTensorType dims) -> do
      case getDims dims of
        Just d -> return $ length fields : d
        Nothing -> return [length fields]
    _ -> return [length fields]
getRecordDimsExpr _ = compilerDeveloperError "Record declaration is not of expected format."

getRecordProvenance ::
  (MonadError CompileError m) =>
  FreeCtxEntry Builtin ->
  m Provenance
getRecordProvenance (DefRecord p _ _ _ _) = return p
getRecordProvenance _ = compilerDeveloperError "Record declaration is not of expected format."

getRecordFieldNames ::
  forall m.
  (MonadExpandResources m) =>
  FreeCtxEntry Builtin ->
  m [Name]
getRecordFieldNames r = case r of
  DefRecord _p _ident _sort _telescope fields -> return $ map (\(field, _typ) -> nameOf field) fields
  _ -> compilerDeveloperError "Record declaration is not of expected format."

constructFromTensorFreeVar ::
  Identifier ->
  Provenance ->
  Expr Builtin
constructFromTensorFreeVar ident p =
  let name = Text.pack "_" <> identifierName ident <> "FromTensor"
   in FreeVar p (Identifier (modulePath ident) name)

constructToTensorFreeVar ::
  Identifier ->
  Provenance ->
  Expr Builtin
constructToTensorFreeVar ident p =
  let name = Text.pack "_" <> identifierName ident <> "ToTensor"
   in FreeVar p (Identifier (modulePath ident) name)
