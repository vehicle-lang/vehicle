module Vehicle.Backend.Loss
  ( convertToLossTensors,
  )
where

import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier, convertBoolTensor)
import Vehicle.Backend.Loss.LogicCompilation (findAndCompileLogic)
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalDecl)
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
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Bool ->
  Prog Builtin ->
  m (Prog LossBuiltin, DifferentiableLogicImplementation)
convertToLossTensors logicID nativeDirection prog@(Main ds) =
  logCompilerSection2 MinDetail currentPass $ do
    logic <- findAndCompileLogic logicID prog
    converted <- runFreshFreeContextT (Proxy @Builtin) $ do
      Main <$> convertDecls logicID logic nativeDirection ds
    return (converted, logic)

--------------------------------------------------------------------------------
-- Program conversion

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Bool ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logicID logic nativeDirection = \case
  [] -> return []
  decl : decls -> do
    normDecl <- evalDecl decl
    maybeLossDecl <- convertDecl logicID logic nativeDirection normDecl
    decls' <- addDeclEntryToContext normDecl $ convertDecls logicID logic nativeDirection decls
    return $ maybeToList maybeLossDecl ++ decls'

convertDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Bool ->
  VDecl Builtin ->
  m (Maybe (Decl LossBuiltin))
convertDecl logicID logic nativeDirection decl = do
  logCompilerSection2 MinDetail ("declaration" <+> quotePretty (identifierOf decl)) $ do
    runMonadLogicT logicID logic decl $ do
      case decl of
        DefAbstract p ident sort typ
          | isExternalResourceDecl decl -> Just <$> convertResourceDecl p ident sort typ
          | otherwise -> return Nothing
        DefFunction p ident ann typ expr
          | isPropertyDecl decl -> Just <$> convertPropertyDecl p ident ann typ nativeDirection expr
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

convertPropertyDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  VType Builtin ->
  Bool ->
  Value Builtin ->
  m (Decl LossBuiltin)
convertPropertyDecl p ident ann typ nativeDirection value = do
  lossType <- convertDeclType typ
  -- If the logic's native semantics is "higher is better" (Maximise), wrap each
  -- leaf bool-tensor in `not` so the emitted body is always a minimisation
  -- target. The flag opts out so users can read the raw DL-native robustness.
  sourceMinimise <- getLogicDirection
  let shouldWrap = not nativeDirection && not sourceMinimise
  lossValue <- convertMultiProperty shouldWrap typ value
  let lossExpr = unnormalise 0 lossValue
  let lossTensorDecl = DefFunction p ident ann lossType lossExpr
  return lossTensorDecl

convertDeclType :: (MonadLogic m) => VType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertType typ

convertMultiProperty :: (MonadLogic m) => Bool -> VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertMultiProperty wrap typ = case toTypeValue typ of
  VBoolTensorType ds -> wrapTensorProperty wrap ds
  VVectorType tElem _d -> convertVectorProperty wrap tElem
  _ -> unexpectedExprError currentPass "Impossible property type"

wrapTensorProperty :: (MonadLogic m) => Bool -> VDims Builtin -> Value Builtin -> m (Value LossBuiltin)
wrapTensorProperty wrap ds value = do
  result <- convertTensorProperty value
  if wrap
    then do
      lossDims <- convertDims ds
      convertNot (TensorOp1Args lossDims result)
    else return result

convertVectorProperty :: (MonadLogic m) => Bool -> VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertVectorProperty wrap typ value = do
  let dims = getVectorDims typ
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset ident -> return $ VFreeVar ident []
    VVectorLiteral args -> convertVecLiteralArgs (convertMultiProperty wrap typ) (IBoolType, dims) args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeachArgs (convertMultiProperty wrap typ) (IBoolType, dims) args

convertTensorProperty :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertTensorProperty value = case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertTensorProperty args
  VBoolStackTensor args -> convertStackTensor convertTensorProperty args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertTensorProperty args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertTensorProperty args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertTensorProperty args
  VBoolTensorGlobally {} -> convertBoolTensor value
  VBoolTensorFinally {} -> convertBoolTensor value
  VBoolTensorUntil {} -> convertBoolTensor value
  VBoolTensorCompareNat args -> convertNatComparison args
  VBoolTensorCompareIndex args -> convertIndexComparison args
  VBoolTensorCompareRatPointwise args -> convertRatTensorPointwiseComparison args
  VBoolTensorCompareRatReduced args -> convertRatTensorReducedComparison args
  VBoolTensorQuantifyRat args -> compileQuantifier args
  VBoolTensorReduceAnd args -> convertReduceAnd =<< convertTensorReduction convertTensorProperty args
  VBoolTensorReduceOr args -> convertReduceOr =<< convertTensorReduction convertTensorProperty args
  VBoolTensorBoolIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertTensorProperty args
  VBoolTensorForeach args -> convertForeachTensor convertTensorProperty args

getVectorDims :: VType Builtin -> VDims Builtin
getVectorDims typ = case toTypeValue typ of
  VBoolTensorType ds -> ds
  VVectorType t d -> IDimCons d (getVectorDims t)
  _ -> developerError "Impossible property type"
