module Vehicle.Backend.LossSearch
  ( convertToSearchLoss,
    SearchProg (..),
    SearchDecl (..),
  )
where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.LiftQuantifier (LiftedData, QuantifierData, liftQuantifiers)
import Vehicle.Backend.Loss.LogicCompilation (findAndCompileLogic)
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Backend.LossTraining (convertMultiProperty)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot)
import Vehicle.Compile.Normalise.Builtin (elimImplies)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Compile.Unblock (noUnblocking, unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.BooleanExpr (DisjunctAll (..))
import Vehicle.Data.Code.ForcedValue (GenericClosure (..), GenericThunk (..), Thunk, emptyBoundEnv, extendClosureWithBound, namedBoundContextToEnv)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Tensor (TensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..), runFreshFreeContextT)
import Vehicle.Verify.Specification (Property, QuerySet (..), traverseProperty)

newtype SearchProg builtin = SearchMain
  { programDeclarations :: [SearchDecl builtin]
  }

data SearchDecl builtin
  = StandardDecl (Decl Builtin)
  | PropertyDecl Provenance Identifier (Property (Expr builtin))
  deriving (Show, Generic)

convertToSearchLoss ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Set Name ->
  Prog Builtin ->
  m (SearchProg LossBuiltin)
convertToSearchLoss logicID requestedDecls prog@(Main ds) = do
  logic <- logCompilerPass LossLogic $ findAndCompileLogic logicID prog

  runFreshFreeContextT (Proxy @Builtin) $ do
    runFreshFreeContextT (Proxy @LossBuiltin) $
      logCompilerPass Loss $ do
        SearchMain <$> convertDecls logicID logic requestedDecls ds

convertDecls ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadFreeContext LossBuiltin m
  ) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  [Decl Builtin] ->
  m [SearchDecl LossBuiltin]
convertDecls logicID logic requestedDecls = \case
  [] -> return []
  decl : decls -> do
    logDebug MaxDetail $ pretty $ identifierOf decl
    logDebugM MaxDetail $ do
      pretty . Map.keys <$> getFreeCtx (Proxy @Builtin)
    decl' <- convertDecl logicID logic requestedDecls decl
    decls' <- addDeclEntryToContext decl $ convertDecls logicID logic requestedDecls decls
    return (decl' : decls')

convertDecl ::
  forall m.
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadFreeContext LossBuiltin m
  ) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  Decl Builtin ->
  m (SearchDecl LossBuiltin)
convertDecl logicID logic requestedDecls decl = case decl of
  DefAbstract {} -> return $ StandardDecl decl
  DefRecord {} -> return $ StandardDecl decl
  DefFunction p ident ann _ expr ->
    if isAnnotatedAsProperty ann && nameOf decl `Set.member` requestedDecls
      then do
        let declProv = (ident, p)
        propertyLoweredNot <-
          runFreshNameBoundContextT $
            flip runReaderT declProv $
              applyDeMorgan (Unforced emptyBoundEnv expr)
        (propertyLifted, _) <-
          runFreshNameBoundContextT $
            flip runReaderT declProv $
              liftQuantifiers (propertyLoweredNot, 0)
        propertyExistential <- runReaderT (eliminateForall propertyLifted) declProv
        propertyLoss <- runConversion $ convertPropertyDecl propertyExistential
        logDebug MaxDetail $ prettyFriendlyEmptyCtx propertyLoss
        return $ PropertyDecl p ident propertyLoss
      else return $ StandardDecl decl
    where
      runConversion :: TensorBoundContextT (ReaderT LossCtx m) (Property (Expr LossBuiltin)) -> m (Property (Expr LossBuiltin))
      runConversion action = do
        logCompilerSection2 MidDetail ("translation of" <+> quotePretty (nameOf ident)) $ do
          runMonadLogicT logicID logic (ident, p) action

applyDeMorgan ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  ) =>
  Thunk Builtin ->
  m (Thunk Builtin)
applyDeMorgan value = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral _ -> return value
    VAnd args -> do
      args' <- traverseTensorOp2Args applyDeMorgan args
      return (Forced $ mkExpr accessAndTensor args')
    VOr args -> do
      args' <- traverseTensorOp2Args applyDeMorgan args
      return (Forced $ mkExpr accessOrTensor args')
    VNot args -> do
      errorOrResult <- runExceptT $ lowerNot noUnblocking args
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> return result
    VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      newBody <- addNameToContext binder $ do
        body' <- applyDeMorgan normBody
        lv' <- getBinderDepth
        return $ unnormalise lv' body'
      newEnv <- namedBoundContextToEnv <$> getNameContext
      return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newBody)))
    VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      newBody <- addNameToContext binder $ do
        body' <- applyDeMorgan normBody
        lv' <- getBinderDepth
        return $ unnormalise lv' body'
      newEnv <- namedBoundContextToEnv <$> getNameContext
      return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))
    VCompareIndex {} -> return value
    VCompareNat {} -> return value
    VCompareRatTensor {} -> return value
    VBoolIf args -> do
      unfolded <- unfoldIf args
      applyDeMorgan unfolded
    VImplies args -> do
      let unfolded = elimImplies args
      applyDeMorgan unfolded
    VBoolVectorAt {} -> unblock
    VBoolFoldList {} -> unblock
    VReduceAndTensor {} -> unblock
    VReduceOrTensor {} -> unblock
    VBoolTensorAt {} -> unblock
  where
    unblock = do
      errorOrResult <- runExceptT $ unblockBoolExpr noUnblocking value
      case errorOrResult of
        Left _ -> do
          declProv <- ask
          throwError $ UnableToLiftQuantifiersInProperty declProv
        Right result -> applyDeMorgan result

eliminateForall ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  Property LiftedData ->
  m (Property LiftedData)
eliminateForall = traverseProperty eliminateForallCheckAlternatingQuantifiers

-- | Throws an error if there are alternating quantifiers in the
-- property and makes all quantifiers existential
eliminateForallCheckAlternatingQuantifiers ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  LiftedData ->
  m LiftedData
eliminateForallCheckAlternatingQuantifiers (quantifiers, value, hasForall, hasExists) =
  if hasForall && hasExists
    then do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    else do
      newQuantifiers <- flipForall quantifiers
      return (newQuantifiers, value, False, True)

flipForall ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  [QuantifierData] ->
  m [QuantifierData]
flipForall quantifiers = case quantifiers of
  [] -> return []
  (_, dimsOrType, binder) : qs -> do
    newQuantifiers <- flipForall qs
    return ((Exists, dimsOrType, binder) : newQuantifiers)

convertPropertyDecl ::
  (MonadLogic m) =>
  Property LiftedData ->
  m (Property (Expr LossBuiltin))
convertPropertyDecl = \case
  NonTrivial expr -> do
    lossExpr <- traverse convertQuerySet expr
    return $ NonTrivial lossExpr
  Trivial _ -> developerError "Trivial property"

convertQuerySet ::
  (MonadLogic m) =>
  QuerySet LiftedData ->
  m (QuerySet (Expr LossBuiltin))
convertQuerySet querySet = case querySet of
  QuerySet negated (DisjunctAll [liftedData]) -> do
    expr <- runFreshNameBoundContextT $ reconstructExpr liftedData
    lossExprs <- convertExpr expr
    return $ QuerySet negated lossExprs
  _ -> developerError "Malformed query set"

reconstructExpr ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  ) =>
  LiftedData ->
  m (Thunk Builtin)
reconstructExpr (quantifiers, value, hasForall, hasExists) = case quantifiers of
  [] -> return value
  (quantifier, dimsOrType, binder) : qs -> do
    newBody <- addNameToContext binder $ do
      reconstructed <- reconstructExpr (qs, value, hasForall, hasExists)
      lv <- getBinderDepth
      return $ unnormalise lv reconstructed
    newEnv <- namedBoundContextToEnv <$> getNameContext
    case dimsOrType of
      Left (pDims, bDims) -> return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newBody)))
      Right typ -> return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))

convertExpr ::
  (MonadLogic m) =>
  Thunk Builtin ->
  m (DisjunctAll (Expr LossBuiltin))
convertExpr expr = do
  forcedValue <- forceThunk expr
  -- Separate VQuantifyRecord case not handled yet
  case toBoolValue forcedValue of
    VQuantifyRatTensor args -> do
      lossThunks <- compileQuantifier args
      let lossExprs = fmap (unnormalise 0) lossThunks -- convert each Thunk to Expr
      return lossExprs
    _ -> do
      lossExpr <- convertMultiProperty expr
      return $ DisjunctAll [lossExpr]

{-convertToSearchLoss ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Set Name ->
  LiftedProg ->
  m SearchProg
convertToSearchLoss logicID requestedDecls liftedProg = _

convertDecls ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadFreeContext LossBuiltin m
  ) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  LiftedProg ->
  m SearchProg
convertDecls logicID logic requestedDecls = \case
  [] -> return []
  decl : decls -> do
    decl' <- case decl of
      Left nonProperty -> do
        maybeLossNonProperty <- convertDecl logicID logic requestedDecls nonProperty
        return $ Left maybeLossNonProperty
      Right property -> do
        maybeLossProperty <- convertProperty logicID logic requestedDecls property
        return $ Right maybeLossProperty
    decls' <-

convertProperty ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadFreeContext LossBuiltin m,
    MonadLogic m
  ) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  (DeclProvenance, Property LiftedExpr) ->
  m (Maybe (DeclProvenance, Property (Expr LossBuiltin)))
convertProperty logicID logic requestedDecls (declProv, property)= case property of
  NonTrivial expr -> do
    if nameOf declProv `Set.member` requestedDecls
      then do
        newExpr <- traverse (\q -> runConversion $ convertQuerySet q) expr
        return $ Just (declProv, NonTrivial newExpr)
      else return Nothing
  Trivial _ -> developerError ("property" <+> quotePretty (nameOf declProv) <+> "is trivial")
  where
  runConversion :: TensorBoundContextT (ReaderT LossCtx m) (QuerySet (Expr LossBuiltin)) -> m (QuerySet (Expr LossBuiltin))
  runConversion action = do
    logCompilerSection2 MidDetail ("translation of" <+> quotePretty (nameOf declProv)) $ do
      runMonadLogicT logicID logic declProv action
-}
