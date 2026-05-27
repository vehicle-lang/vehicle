module Vehicle.Backend.Loss
  ( convertToLossTensors,
  )
where

import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.LogicCompilation (findAndCompileLogic)
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalDecl, normaliseClosure)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface.Patterns
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Bound.Context.Tensor (addNonTensorBinderToContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Set Name ->
  Prog Builtin ->
  m (Prog LossBuiltin)
convertToLossTensors logicID requestedDecls prog@(Main ds) =
  logCompilerSection2 MinDetail currentPass $ do
    logic <- findAndCompileLogic logicID prog
    runFreshFreeContextT (Proxy @Builtin) $ do
      Main <$> convertDecls logicID logic requestedDecls ds

--------------------------------------------------------------------------------
-- Program conversion

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logicID logic requestedDecls = \case
  [] -> return []
  decl : decls -> do
    normDecl <- evalDecl decl
    maybeLossDecl <- convertDecl logicID logic requestedDecls normDecl
    decls' <- addDeclEntryToContext normDecl $ convertDecls logicID logic requestedDecls decls
    return $ maybeToList maybeLossDecl ++ decls'

shouldEmit :: Set Name -> GenericDecl expr -> Bool
shouldEmit requestedDecls decl
  | Set.null requestedDecls = isPropertyDecl decl
  | otherwise = Set.member (nameOf (identifierOf decl)) requestedDecls

convertDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  VDecl Builtin ->
  m (Maybe (Decl LossBuiltin))
convertDecl logicID logic requestedDecls decl = do
  logCompilerSection2 MinDetail ("declaration" <+> quotePretty (identifierOf decl)) $ do
    runMonadLogicT logicID logic decl $ do
      case decl of
        DefAbstract p ident sort typ
          | isExternalResourceDecl decl -> Just <$> convertResourceDecl p ident sort typ
          | otherwise -> return Nothing
        DefFunction p ident ann typ expr
          | shouldEmit requestedDecls decl -> Just <$> convertValueDecl p ident ann typ expr
          | otherwise -> return Nothing
        DefRecord {} -> return Nothing

convertResourceDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefAbstractSort ->
  VType Builtin ->
  m (Decl LossBuiltin)
convertResourceDecl p ident sort typ = do
  -- Keep resource declarations, converting their type appropriately.
  -- TODO what about boolean parameters?
  typ' <- convertDeclType typ
  return $ DefAbstract p ident sort typ'

convertValueDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  VType Builtin ->
  Value Builtin ->
  m (Decl LossBuiltin)
convertValueDecl p ident ann typ value = do
  lossValue <- convertTypedValue p ident typ value
  lossType <- convertDeclType typ
  return $ DefFunction p ident ann lossType (unnormalise 0 lossValue)

convertDeclType :: (MonadLogic m) => VType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertType typ

convertTypedValue ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  VType Builtin ->
  Value Builtin ->
  m (Value LossBuiltin)
convertTypedValue p ident typ value = case toTypeValue typ of
  VPiType binder closure -> case value of
    VLam vBinder vClosure -> do
      vBinder' <- traverse convertType vBinder
      let convertBody v = do
            body <- normaliseClosure binder closure
            addNonTensorBinderToContext binder $ convertTypedValue p ident body v
      vClosure' <- convertClosure convertBody vBinder vClosure
      return $ VLam vBinder' vClosure'
    -- Non-Lam at a Pi position: walk past the binder and dispatch at the leaf.
    _ -> do
      body <- normaliseClosure binder closure
      addNonTensorBinderToContext binder $ convertTypedValue p ident body value
  VBoolType -> convertBoolTensorValue value
  VRatType -> convertRatTensor value
  VNatType -> convertNatValue value
  VIndexType _ -> convertIndexValue value
  VBoolTensorType _ -> convertBoolTensorValue value
  VRatTensorType _ -> convertRatTensor value
  VNatTensorType _ -> convertNatTensor value
  VIndexTensorType _ _ -> convertIndexTensor value
  VVectorType tElem _d -> convertVectorValue p ident tElem value
  VListType tElem -> convertListValue (convertTypedValue p ident tElem) value
  VUnitType {} -> unsupportedOperation "Unit-typed declarations"
  VFreeTypeVar {} -> unexpectedExprError currentPass "free type variable in decl type"
  VBoundTypeVar {} -> unexpectedExprError currentPass "bound type variable in decl type"

convertVectorValue ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  VType Builtin ->
  Value Builtin ->
  m (Value LossBuiltin)
convertVectorValue p ident elemType value = do
  let dims = getVectorDims elemType
  let convertElem = convertTypedValue p ident elemType
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset name -> return $ VFreeVar name []
    VVectorLiteral args -> convertVecLiteralArgs convertElem (IBoolType, dims) args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeachArgs convertElem (IBoolType, dims) args

getVectorDims :: VType Builtin -> VDims Builtin
getVectorDims typ = case toTypeValue typ of
  VBoolTensorType ds -> ds
  VRatTensorType ds -> ds
  VNatTensorType ds -> ds
  VIndexTensorType _ ds -> ds
  VVectorType t d -> IDimCons d (getVectorDims t)
  _ -> developerError "non-tensor element type in vector unwrap"

-- `LossCompilation.convertBoolTensor` throws on `VBoolTensorQuantifyRat`
-- because its callers nest inside contexts where quantifiers can't occur.
-- A top-level decl body can have them, so handle that one arm here.
convertBoolTensorValue :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertBoolTensorValue value = case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertBoolTensorValue args
  VBoolStackTensor args -> convertStackTensor convertBoolTensorValue args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertBoolTensorValue args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertBoolTensorValue args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertBoolTensorValue args
  VBoolTensorCompareNat args -> convertNatComparison args
  VBoolTensorCompareIndex args -> convertIndexComparison args
  VBoolTensorCompareRatPointwise args -> convertRatTensorPointwiseComparison args
  VBoolTensorCompareRatReduced args -> convertRatTensorReducedComparison args
  VBoolTensorQuantifyRat args -> compileQuantifier args
  VBoolTensorReduceAnd args -> convertReduceAnd =<< convertTensorReduction convertBoolTensorValue args
  VBoolTensorReduceOr args -> convertReduceOr =<< convertTensorReduction convertBoolTensorValue args
  VBoolTensorBoolIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertBoolTensorValue args
  VBoolTensorForeach args -> convertForeachTensor convertBoolTensorValue args
