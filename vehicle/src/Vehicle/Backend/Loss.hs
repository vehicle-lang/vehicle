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
import Vehicle.Backend.Loss.Domain (compileQuantifier)
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
import Vehicle.Data.Code.Interface.Patterns
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DifferentiableLogic
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
          | isPropertyDecl decl || Set.member (nameOf ident) requestedDecls ->
              Just <$> convertOutputDecl p ident ann typ expr
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
  Value Builtin ->
  m (Decl LossBuiltin)
convertOutputDecl p ident ann typ value = do
  checkLossOutputType p ident typ
  lossType <- convertDeclType typ
  lossValue <- convertMultiOutput typ value
  let lossExpr = unnormalise 0 lossValue
  let lossTensorDecl = DefFunction p ident ann lossType lossExpr
  return lossTensorDecl

checkLossOutputType :: (MonadLogic m) => Provenance -> Identifier -> VType Builtin -> m ()
checkLossOutputType p ident typ = case toTypeValue typ of
  VBoolTensorType _ -> return ()
  VRatTensorType _ -> return ()
  VVectorType tElem _ -> checkLossOutputType p ident tElem
  _ ->
    throwError $
      UnimplementedFeature p $
        "compiling declaration"
          <+> quotePretty (nameOf ident)
          <+> "as a loss output (supported types: `Tensor Bool _`, `Tensor Real _`, `Vector _ _`)"

convertDeclType :: (MonadLogic m) => VType Builtin -> m (Type LossBuiltin)
convertDeclType typ = unnormalise 0 <$> convertType typ

convertMultiOutput :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertMultiOutput typ = case toTypeValue typ of
  VBoolTensorType _ds -> convertBoolTensorOutput
  VRatTensorType _ds -> convertRatTensor
  VVectorType tElem _d -> convertVectorOutput tElem
  _ -> unexpectedExprError currentPass "Impossible property type"

convertVectorOutput :: (MonadLogic m) => VType Builtin -> Value Builtin -> m (Value LossBuiltin)
convertVectorOutput typ value = do
  let dims = getVectorDims typ
  case toVectorValue value of
    VVectorBoundVar lv spine -> convertBoundVar lv spine
    VVectorDataset ident -> return $ VFreeVar ident []
    VVectorLiteral args -> convertVecLiteralArgs (convertMultiOutput typ) (IBoolType, dims) args
    VVectorIf args -> convertIf args
    VVectorForeach args -> convertVecForeachArgs (convertMultiOutput typ) (IBoolType, dims) args

convertBoolTensorOutput :: (MonadLogic m) => Value Builtin -> m (Value LossBuiltin)
convertBoolTensorOutput value = case toBoolTensorValue value of
  VBoolTensorLiteral bs -> convertBoolTensorLiteral bs
  VBoolConstTensor args -> convertConstTensor convertBoolTensorOutput args
  VBoolStackTensor args -> convertStackTensor convertBoolTensorOutput args
  VBoolTensorAnd args -> convertAnd =<< convertTensorOp2 convertBoolTensorOutput args
  VBoolTensorOr args -> convertOr =<< convertTensorOp2 convertBoolTensorOutput args
  VBoolTensorNot args -> convertNot =<< convertTensorOp1 convertBoolTensorOutput args
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
