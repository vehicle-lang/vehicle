module Vehicle.Backend.Loss
  ( convertToLossTensors,
  )
where

import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.Core (MonadLogic, runMonadLogicT)
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.Logics (CompiledDifferentiableLogic)
import Vehicle.Backend.Loss.LossCompilation
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseInEmptyEnv)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface.Patterns
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)

convertToLossTensors ::
  (MonadCompile m) =>
  CompiledDifferentiableLogic ->
  Prog Builtin ->
  m (Prog LossBuiltin)
convertToLossTensors logic (Main ds) =
  logCompilerSection2 MinDetail currentPass $
    runFreshFreeContextT (Proxy @Builtin) $
      Main <$> convertDecls logic ds

convertDecls ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  CompiledDifferentiableLogic ->
  [Decl Builtin] ->
  m [Decl LossBuiltin]
convertDecls logic = \case
  [] -> return []
  decl : decls -> do
    normDecl <- traverse normaliseInEmptyEnv decl
    maybeLossDecl <- convertDecl logic normDecl
    decls' <- addDeclEntryToContext (decl, normDecl) $ convertDecls logic decls
    return $ maybeToList maybeLossDecl ++ decls'

convertDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  CompiledDifferentiableLogic ->
  VDecl Builtin ->
  m (Maybe (Decl LossBuiltin))
convertDecl logic decl = do
  logCompilerSection2 MinDetail ("declaration" <+> quotePretty (identifierOf decl)) $ do
    runMonadLogicT logic (identifierOf decl, provenanceOf decl) $
      case decl of
        DefRecord {} -> return Nothing
        DefAbstract p ident sort typ -> do
          -- Keep resource declarations, converting their type appropriately.
          -- TODO what about boolean parameters?
          typ' <- convertDeclType typ
          return $ Just $ DefAbstract p ident sort typ'
        DefFunction p ident ann typ expr
          | AnnProperty `notElem` ann -> return Nothing
          | otherwise -> do
              typ' <- convertDeclType typ
              expr' <- convertPropertyExpr typ expr
              let lossTensorDecl = DefFunction p ident ann typ' expr'
              return $ Just lossTensorDecl

convertDeclType :: (MonadLogic m) => VType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertType typ

convertPropertyExpr :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Type LossBuiltin)
convertPropertyExpr typ value = do
  lossValue <- convertMultiProperty typ value
  let lossExpr = unnormalise 0 lossValue
  return lossExpr

convertMultiProperty :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertMultiProperty typ = case toTypeValue typ of
  VBoolTensorType _ds -> convertTensorProperty
  VVectorType tElem _d -> convertVectorProperty tElem
  _ -> unexpectedExprError currentPass "Impossible property type"

convertVectorProperty :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertVectorProperty typ value = do
  let dims = getVectorDims typ
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset ident -> return $ VFreeVar ident []
    VVectorLiteral args -> convertVecLiteralArgs (convertMultiProperty typ) (IBoolType, dims) args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeachArgs (convertMultiProperty typ) (IBoolType, dims) args

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

currentPass :: Doc a
currentPass = "loss compilation"
