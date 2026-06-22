module Vehicle.Backend.Loss
  ( convertToLossTensors,
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.LogicCompilation (findAndCompileLogic)
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Backend.Loss.RecordCompilation qualified as RecordCompilation
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalDecl)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Bound.Context.Tensor (TensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..), addDeclEntryToContext, addDeclToContext, runFreshFreeContextT)

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Builtin ->
  m (Prog LossBuiltin)
convertToLossTensors logicID prog@(Main ds) =
  logCompilerSection2 MinDetail currentPass $ do
    logic <- findAndCompileLogic logicID prog
    runFreshFreeContextT (Proxy @Builtin) $ do
      runFreshFreeContextT (Proxy @LossBuiltin) $ do
        Main <$> convertDecls logicID logic ds

--------------------------------------------------------------------------------
-- Program conversion

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadFreeContext LossBuiltin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logicID logic = \case
  [] -> return []
  decl : decls -> do
    normDecl <- evalDecl decl
    maybeLossDecl <- convertDecl logicID logic normDecl
    decls' <-
      maybe id addDeclToContext maybeLossDecl $
        addDeclEntryToContext normDecl $
          convertDecls logicID logic decls
    return $ maybeToList maybeLossDecl ++ decls'

convertDecl ::
  forall m.
  (MonadCompile m, MonadFreeContext Builtin m, MonadFreeContext LossBuiltin m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  VDecl Builtin ->
  m (Maybe (Decl LossBuiltin))
convertDecl logicID logic decl = case decl of
  DefAbstract p ident sort typ
    | isAnnotatedAsExternalResource sort -> runConversion $ convertResourceDecl p ident sort typ
    | otherwise -> return Nothing
  DefFunction p ident ann typ expr
    | isAnnotatedAsProperty ann -> runConversion $ convertPropertyDecl p ident ann typ expr
    | otherwise -> return Nothing
  DefRecord p ident anns telescope fields
    | isAnnotatedAsTensor anns -> runConversion $ convertTensorRecordDecl p ident anns telescope fields
    | otherwise -> return Nothing
  where
    runConversion :: TensorBoundContextT (ReaderT LossCtx m) (Decl LossBuiltin) -> m (Maybe (Decl LossBuiltin))
    runConversion action = do
      logCompilerSection2 MidDetail ("translation of" <+> quotePretty (identifierOf decl)) $ do
        Just <$> runMonadLogicT logicID logic decl action

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

convertTensorRecordDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  Maybe DefRecordSort ->
  GenericTelescope (VType Builtin) ->
  GenericRecordFields (VType Builtin) ->
  m (Decl LossBuiltin)
convertTensorRecordDecl p ident anns telescope fields = do
  telescope' <- traverse (traverse convertDeclType) telescope
  fields' <- traverse (traverse convertDeclType) fields
  return $ DefRecord p ident anns telescope' fields'

convertPropertyDecl ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  VType Builtin ->
  Value Builtin ->
  m (Decl LossBuiltin)
convertPropertyDecl p ident ann typ value = do
  lossType <- convertDeclType typ
  lossValue <- convertMultiProperty typ value
  let lossExpr = unnormalise 0 lossValue
  let lossTensorDecl = DefFunction p ident ann lossType lossExpr
  return lossTensorDecl

convertDeclType :: (MonadLogic m) => VType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertType typ

convertMultiProperty :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertMultiProperty typ = case toTypeValue typ of
  VBoolTensorType _ds -> convertTensorProperty
  VVectorType tElem _d -> convertVectorProperty tElem
  _ -> unexpectedExprError currentPass "Impossible property type"

convertVectorProperty :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertVectorProperty typ value = do
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset ident -> return $ VFreeVar ident []
    VVectorLiteral args -> convertVecLiteral (convertMultiProperty typ) args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeach (convertMultiProperty typ) args

convertTensorProperty :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertTensorProperty value = case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertTensorProperty args
  VBoolStackTensor args -> convertStackTensor convertTensorProperty args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertTensorProperty args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertTensorProperty args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertTensorProperty args
  VBoolTensorCompareNat args -> convertNatComparison args
  VBoolTensorCompareIndex args -> convertIndexComparison args
  VBoolTensorCompareRatPointwise args -> convertRatTensorPointwiseComparison args
  VBoolTensorCompareRatReduced args -> convertRatTensorReducedComparison args
  VBoolTensorQuantifyRat args -> compileQuantifier args
  VBoolTensorQuantifyRecord (q, recordArgs) -> do
    flattenedArgs <- RecordCompilation.wrapQuantifyRecordForLoss recordArgs
    compileQuantifier (q, flattenedArgs)
  VBoolTensorReduceAnd args -> convertReduceAnd =<< convertTensorReduction convertTensorProperty args
  VBoolTensorReduceOr args -> convertReduceOr =<< convertTensorReduction convertTensorProperty args
  VBoolTensorIf args -> convertIf args
  VBoolTensorAt args -> convertAtTensor convertTensorProperty args
  VBoolTensorForeach args -> convertForeachTensor convertTensorProperty args
