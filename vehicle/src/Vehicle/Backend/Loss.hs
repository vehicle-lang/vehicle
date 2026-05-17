module Vehicle.Backend.Loss
  ( convertToLossTensors,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier, convertBoolTensor)
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
import Vehicle.Data.Code.Interface.Args (TensorOp1Args (..))
import Vehicle.Data.Code.Interface.Patterns
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Bound.Context.Tensor (addNonTensorBinderToContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Bool ->
  Set Name ->
  Prog Builtin ->
  m (Prog LossBuiltin, DifferentiableLogicImplementation)
convertToLossTensors logicID nativeDirection requestedDecls prog@(Main ds) =
  logCompilerSection2 MinDetail currentPass $ do
    logic <- findAndCompileLogic logicID prog
    converted <- runFreshFreeContextT (Proxy @Builtin) $ do
      Main <$> convertDecls logicID logic nativeDirection requestedDecls ds
    return (converted, logic)

--------------------------------------------------------------------------------
-- Program conversion

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Bool ->
  Set Name ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logicID logic nativeDirection requestedDecls = \case
  [] -> return []
  decl : decls -> do
    normDecl <- evalDecl decl
    maybeLossDecl <- convertDecl logicID logic nativeDirection requestedDecls normDecl
    decls' <- addDeclEntryToContext normDecl $ convertDecls logicID logic nativeDirection requestedDecls decls
    return $ maybeToList maybeLossDecl ++ decls'

convertDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Bool ->
  Set Name ->
  VDecl Builtin ->
  m (Maybe (Decl LossBuiltin))
convertDecl logicID logic nativeDirection requestedDecls decl = do
  logCompilerSection2 MinDetail ("declaration" <+> quotePretty (identifierOf decl)) $ do
    runMonadLogicT logicID logic decl $ do
      case decl of
        DefAbstract p ident sort typ
          | isExternalResourceDecl decl -> Just <$> convertResourceDecl p ident sort typ
          | otherwise -> return Nothing
        DefFunction p ident ann typ expr
          -- Handled by convertLogicDecl, not compiled as a loss output.
          | isLogicDecl decl -> return Nothing
          | isPropertyDecl decl || Set.member (nameOf ident) requestedDecls ->
              Just <$> convertOutputDecl p ident ann typ nativeDirection expr
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

convertOutputDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  VType Builtin ->
  Bool ->
  Value Builtin ->
  m (Decl LossBuiltin)
convertOutputDecl p ident ann typ nativeDirection value = do
  leafType <- stripPiTypes typ
  sourceMinimise <- getLogicDirection
  let shouldWrap = not nativeDirection && not sourceMinimise
  case toTypeValue leafType of
    VBoolTensorType {} -> emit leafType shouldWrap
    VBoolType -> emit leafType shouldWrap
    VRatTensorType {} -> emit leafType shouldWrap
    VRatType -> emit leafType shouldWrap
    VVectorType {} -> emit leafType shouldWrap
    _ ->
      throwError $
        UnimplementedFeature p $
          "compiling declaration"
            <+> quotePretty (nameOf ident)
            <+> "as a loss output (supported leaf types: `Bool`, `Real`, `Tensor Bool _`, `Tensor Real _`, `Vector _ _`)"
  where
    emit leafType wrap = do
      lossType <- convertDeclType typ
      lossValue <- convertFunction (convertMultiOutput wrap leafType) value
      let lossExpr = unnormalise 0 lossValue
      return $ DefFunction p ident ann lossType lossExpr

stripPiTypes :: (MonadLogic m) => VType Builtin -> m (VType Builtin)
stripPiTypes typ = case toTypeValue typ of
  VPiType binder closure -> do
    body <- normaliseClosure binder closure
    addNonTensorBinderToContext binder $ stripPiTypes body
  _ -> return typ

convertDeclType :: (MonadLogic m) => VType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertType typ

convertMultiOutput :: (MonadLogic m) => Bool -> VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertMultiOutput wrap typ value = case toTypeValue typ of
  VBoolTensorType ds -> wrapTensorProperty wrap ds value
  VBoolType -> wrapTensorProperty wrap IDimNil value
  VRatTensorType _ds -> convertRatTensor value
  VRatType -> convertRatTensor value
  VVectorType tElem _d -> convertVectorOutput wrap tElem value
  _ -> unexpectedExprError currentPass "unsupported leaf type"

wrapTensorProperty :: (MonadLogic m) => Bool -> VDims Builtin -> Value Builtin -> m (Value LossBuiltin)
wrapTensorProperty wrap ds value = do
  result <- convertBoolTensorOutput value
  if wrap
    then do
      lossDims <- convertDims ds
      convertNot (TensorOp1Args lossDims result)
    else return result

convertVectorOutput :: (MonadLogic m) => Bool -> VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertVectorOutput wrap typ value = do
  let dims = getVectorDims typ
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset ident -> return $ VFreeVar ident []
    VVectorLiteral args -> convertVecLiteralArgs (convertMultiOutput wrap typ) (IBoolType, dims) args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeachArgs (convertMultiOutput wrap typ) (IBoolType, dims) args

convertBoolTensorOutput :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertBoolTensorOutput value = case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertBoolTensorOutput args
  VBoolStackTensor args -> convertStackTensor convertBoolTensorOutput args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertBoolTensorOutput args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertBoolTensorOutput args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertBoolTensorOutput args
  VBoolTensorGlobally {} -> convertBoolTensor value
  VBoolTensorFinally {} -> convertBoolTensor value
  VBoolTensorUntil {} -> convertBoolTensor value
  VBoolTensorCompareNat args -> convertNatComparison args
  VBoolTensorCompareIndex args -> convertIndexComparison args
  VBoolTensorCompareRatPointwise args -> convertRatTensorPointwiseComparison args
  VBoolTensorCompareRatReduced args -> convertRatTensorReducedComparison args
  VBoolTensorQuantifyRat args -> compileQuantifier args
  VBoolTensorReduceAnd args -> convertReduceAnd =<< convertTensorReduction convertBoolTensorOutput args
  VBoolTensorReduceOr args -> convertReduceOr =<< convertTensorReduction convertBoolTensorOutput args
  VBoolTensorBoolIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertBoolTensorOutput args
  VBoolTensorForeach args -> convertForeachTensor convertBoolTensorOutput args

getVectorDims :: VType Builtin -> VDims Builtin
getVectorDims typ = case toTypeValue typ of
  VBoolTensorType ds -> ds
  VVectorType t d -> IDimCons d (getVectorDims t)
  _ -> developerError "Impossible property type"
