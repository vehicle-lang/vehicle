module Vehicle.Backend.LossSearch
  ( convertToSearchLoss,
    SearchTree (..),
  )
where

import Control.Monad.Reader
-- import Data.Map qualified as Map

import Data.List.NonEmpty (fromList, toList)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Vehicle.Backend.Loss.Core
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.LiftQuantifier (LiftedData, compileHardBooleanTree)
import Vehicle.Backend.Loss.LogicCompilation (findAndCompileLogic)
import Vehicle.Backend.Loss.LossCompilation qualified as Loss ()
import Vehicle.Backend.LossTraining (convertDeclType, convertMultiProperty, convertResourceDecl)
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.BooleanExpr (DisjunctAll (..))
import Vehicle.Data.Code.ForcedValue (GenericClosure (..), GenericThunk (..), Thunk, emptyBoundEnv, namedBoundContextToEnv)
import Vehicle.Data.Code.Interface
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Tensor (TensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..), runFreshFreeContextT)
import Vehicle.Verify.Specification (Property, QuerySet (..))

data SearchTree
  = SearchTree Provenance Identifier (Property Name)
  deriving (Show, Generic)

convertToSearchLoss ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Set Name ->
  Prog Builtin ->
  m ([SearchTree], Prog LossBuiltin)
convertToSearchLoss logicID requestedDecls prog@(Main ds) = do
  logic <- logCompilerPass LossLogic $ findAndCompileLogic logicID prog

  runFreshFreeContextT (Proxy @Builtin) $ do
    runFreshFreeContextT (Proxy @LossBuiltin) $
      logCompilerPass Loss $ do
        (searchTrees, ds') <- convertDecls logicID logic requestedDecls ds
        -- logDebug MaxDetail $ prettyFriendly (Main ds')
        return (searchTrees, Main ds')

convertDecls ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadFreeContext LossBuiltin m
  ) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  [Decl Builtin] ->
  m ([SearchTree], [Decl LossBuiltin])
convertDecls logicID logic requestedDecls = \case
  [] -> return ([], [])
  decl : decls -> do
    {-logDebug MaxDetail $ pretty $ identifierOf decl
    logDebugM MaxDetail $ do
      pretty . Map.keys <$> getFreeCtx (Proxy @Builtin)-}
    (maybeSearchTree, maybeLossDecls) <- convertDecl logicID logic requestedDecls decl
    (searchTrees, lossDecls) <- addDeclEntryToContext decl $ convertDecls logicID logic requestedDecls decls
    return (maybeToList maybeSearchTree ++ searchTrees, fromMaybe [] maybeLossDecls ++ lossDecls)

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
  m (Maybe SearchTree, Maybe [Decl LossBuiltin])
convertDecl logicID logic requestedDecls decl = case decl of
  DefAbstract p ident sort typ
    | isAnnotatedAsExternalResource sort -> do
        let normType = Unforced emptyBoundEnv typ
        decl' <- runConversionNonProperty $ convertResourceDecl p ident sort normType
        return (Nothing, Just [decl'])
    | otherwise -> return (Nothing, Nothing)
  DefFunction p ident sort typ expr
    | isAnnotatedAsProperty sort && nameOf decl `Set.member` requestedDecls ->
        logCompilerPass LossQuantifierLifting $ do
          let normType = Unforced emptyBoundEnv typ
          let normExpr = Unforced emptyBoundEnv expr
          let declProv = (ident, p)
          propertyLifted <-
            runFreshNameBoundContextT $
              flip runReaderT declProv $
                compileHardBooleanTree normExpr
          (booleanTreeNames, lossDecls) <-
            runConversionProperty $ do
              lossType <- convertDeclType normType
              convertProperty p ident sort lossType propertyLifted
          return (Just booleanTreeNames, Just lossDecls)
    | otherwise -> return (Nothing, Nothing)
  DefRecord {} -> return (Nothing, Nothing)
  where
    runConversionNonProperty :: TensorBoundContextT (ReaderT LossCtx m) (Decl LossBuiltin) -> m (Decl LossBuiltin)
    runConversionNonProperty action = do
      logCompilerSection2 MidDetail ("translation of" <+> quotePretty (identifierOf decl)) $ do
        runMonadLogicT logicID logic (identifierOf decl, provenanceOf decl) action

    runConversionProperty :: TensorBoundContextT (ReaderT LossCtx m) (SearchTree, [Decl LossBuiltin]) -> m (SearchTree, [Decl LossBuiltin])
    runConversionProperty action = do
      logCompilerSection2 MidDetail ("translation of" <+> quotePretty (identifierOf decl)) $ do
        runMonadLogicT logicID logic (identifierOf decl, provenanceOf decl) action

convertProperty ::
  (MonadLogic m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type LossBuiltin ->
  Property LiftedData ->
  m (SearchTree, [Decl LossBuiltin])
convertProperty p ident sort lossType = \case
  NonTrivial expr -> do
    converted <- runSupplyT [0 :: Int ..] (traverse (convertQuerySet p ident sort lossType) expr)
    let searchTree = SearchTree p ident (NonTrivial (fmap fst converted))
    let lossDecls = foldMap snd converted
    return (searchTree, lossDecls)
  Trivial _ -> developerError "Trivial property"

convertQuerySet ::
  ( MonadLogic m,
    MonadSupply Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type LossBuiltin ->
  QuerySet LiftedData ->
  m (QuerySet Name, [Decl LossBuiltin])
convertQuerySet p ident sort lossType (QuerySet negated (DisjunctAll liftedData)) = do
  expr <- runFreshNameBoundContextT $ traverse reconstructExpr liftedData
  logDebug MaxDetail $ prettyFriendlyEmptyCtx expr
  lossExprs <- traverse convertExpr expr
  let flattenedLossExprs = concat $ toList lossExprs
  -- logDebug MaxDetail $ prettyFriendlyEmptyCtx lossExprs
  (names, lossDecls) <- reconstructLossDecls p ident sort lossType flattenedLossExprs
  return (QuerySet negated (DisjunctAll $ fromList names), lossDecls)

reconstructExpr ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  ) =>
  LiftedData ->
  m (Thunk Builtin)
reconstructExpr (quantifiers, value, ctxSize) = case quantifiers of
  [] -> return value
  (quantifier, dimsOrType, binder) : qs -> do
    newBody <- addNameToContext binder $ do
      reconstructed <- reconstructExpr (qs, value, ctxSize - 1)
      lv <- getBinderDepth
      return $ unnormalise lv reconstructed
    newEnv <- namedBoundContextToEnv <$> getNameContext
    case dimsOrType of
      Left (pDims, bDims) -> return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newBody)))
      Right typ -> return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))

convertExpr ::
  (MonadLogic m) =>
  Thunk Builtin ->
  m [Expr LossBuiltin]
convertExpr expr = do
  forcedValue <- forceThunk expr
  -- Separate VQuantifyRecord case not handled yet
  case toBoolValue forcedValue of
    VQuantifyRatTensor args -> do
      lossThunks <- compileQuantifier args
      let lossExprs = fmap (unnormalise 0) lossThunks -- convert each Thunk to Expr
      return $ toList $ unDisjunctAll lossExprs
    _ -> do
      lossExpr <- convertMultiProperty expr
      return [lossExpr]

reconstructLossDecls ::
  ( MonadCompile m,
    MonadSupply Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type LossBuiltin ->
  [Expr LossBuiltin] ->
  m ([Name], [Decl LossBuiltin])
reconstructLossDecls p ident sort lossType exprs = case exprs of
  [] -> return ([], [])
  e : es -> do
    exprCount <- demand @Int
    let newName = nameOf ident <> T.pack (show exprCount)
    let newIdent = changeName ident newName
    let decl = DefFunction p newIdent sort lossType e
    (newNames, decls) <- reconstructLossDecls p ident sort lossType es
    return (newName : newNames, decl : decls)

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

extractLoss ::
  (MonadCompile m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type LossBuiltin ->
  Property (Expr LossBuiltin) ->
  m (Property Name, [Decl LossBuiltin])
extractLoss p ident sort lossType property = case property of
  NonTrivial expr -> do
    (extracted, _) <- runStateT (traverse (extractLossDisjuncts p ident sort lossType) expr) 0
    return (NonTrivial (fmap fst extracted), foldMap snd extracted)
  Trivial _ -> developerError "Trivial property"

extractLossDisjuncts ::
  ( MonadCompile m,
    MonadState Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type LossBuiltin ->
  QuerySet (Expr LossBuiltin) ->
  m (QuerySet Name, [Decl LossBuiltin])
extractLossDisjuncts p ident sort lossType (QuerySet negated disjuncts) = do
  queryCount <- get
  let lossExprs = toList $ unDisjunctAll disjuncts
  modify (+ length lossExprs)
  (names, lossDecls) <- reconstructLossDecls p ident sort lossType lossExprs queryCount
  return (QuerySet negated (DisjunctAll $ fromList names), lossDecls)
-}
