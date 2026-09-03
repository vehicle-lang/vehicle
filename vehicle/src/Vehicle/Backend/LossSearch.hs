module Vehicle.Backend.LossSearch
  ( convertToSearchTree,
    BooleanTree (..),
  )
where

import Control.Monad.Reader
-- import Data.Map qualified as Map

import Data.List.NonEmpty (fromList, toList)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Vehicle.Backend.Loss.Domain (compileQuantifier)
import Vehicle.Backend.Loss.LiftQuantifier (LiftedData, compileHardBooleanTree)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.BooleanExpr (DisjunctAll (..))
import Vehicle.Data.Code.ForcedValue (GenericClosure (..), GenericThunk (..), Thunk, emptyBoundEnv, namedBoundContextToEnv)
import Vehicle.Data.Code.Interface
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Tensor.Instance (runFreshTensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..), runFreshFreeContextT)
import Vehicle.Verify.Specification (Property, QuerySet (..))

data BooleanTree
  = BooleanTree Provenance Identifier (Property Name)
  deriving (Show, Generic)

type MonadSearch m =
  ( MonadCompile m,
    MonadFreeContext Builtin m
  )

convertToSearchTree ::
  (MonadCompile m) =>
  Prog Builtin ->
  m ([BooleanTree], Prog Builtin)
convertToSearchTree (Main ds) = do
  runFreshFreeContextT (Proxy @Builtin) $ do
    logCompilerPass LossQuantifierLifting $ do
      (searchTrees, ds') <- convertDecls ds
      -- logDebug MaxDetail $ prettyFriendly (Main ds')
      return (searchTrees, Main ds')

convertDecls ::
  (MonadSearch m) =>
  [Decl Builtin] ->
  m ([BooleanTree], [Decl Builtin])
convertDecls = \case
  [] -> return ([], [])
  decl : decls -> do
    {-logDebug MaxDetail $ pretty $ identifierOf decl
    logDebugM MaxDetail $ do
      pretty . Map.keys <$> getFreeCtx (Proxy @Builtin)-}
    (maybeBooleanTree, maybeLossDecls) <- convertDecl decl
    (searchTrees, lossDecls) <- addDeclEntryToContext decl $ convertDecls decls
    return (maybeToList maybeBooleanTree ++ searchTrees, fromMaybe [] maybeLossDecls ++ lossDecls)

convertDecl ::
  forall m.
  (MonadSearch m) =>
  Decl Builtin ->
  m (Maybe BooleanTree, Maybe [Decl Builtin])
convertDecl decl = case decl of
  DefAbstract {} -> return (Nothing, Just [decl])
  DefFunction p ident sort typ expr
    | isAnnotatedAsProperty sort ->
        logCompilerPass LossQuantifierLifting $ do
          let normExpr = Unforced emptyBoundEnv expr
          let declProv = (ident, p)
          booleanTreeLifted <-
            runFreshNameBoundContextT $
              flip runReaderT declProv $
                compileHardBooleanTree normExpr
          (booleanTreeNames@(BooleanTree _ _ tree), lossDecls) <-
            convertProperty p ident sort typ booleanTreeLifted
          logDebug MaxDetail $ prettyFriendlyEmptyCtx tree
          return (Just booleanTreeNames, Just lossDecls)
    | otherwise -> return (Nothing, Just [decl])
  DefRecord {} -> return (Nothing, Just [decl])

convertProperty ::
  (MonadSearch m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  Property LiftedData ->
  m (BooleanTree, [Decl Builtin])
convertProperty p ident sort lossType = \case
  NonTrivial expr -> do
    converted <- runSupplyT [0 :: Int ..] (traverse (convertQuerySet p ident sort lossType) expr)
    let booleanTree = BooleanTree p ident (NonTrivial (fmap fst converted))
    let lossDecls = foldMap snd converted
    return (booleanTree, lossDecls)
  Trivial bool -> do
    let booleanTree = BooleanTree p ident (Trivial bool)
    return (booleanTree, [])

convertQuerySet ::
  ( MonadSearch m,
    MonadSupply Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  QuerySet LiftedData ->
  m (QuerySet Name, [Decl Builtin])
convertQuerySet p ident sort lossType (QuerySet negated (DisjunctAll liftedData)) = do
  exprs <- runFreshNameBoundContextT $ traverse reconstructExpr liftedData
  logDebug MaxDetail $ prettyFriendlyEmptyCtx exprs
  lossExprs <- runReaderT (traverse convertExpr exprs) (ident, p)
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
  ( MonadSearch m,
    MonadReader DeclProvenance m
  ) =>
  Thunk Builtin ->
  m [Expr Builtin]
convertExpr expr = runFreshTensorBoundContextT $ do
  forcedValue <- forceThunk expr
  -- Separate VQuantifyRecord case not handled yet
  case toBoolValue forcedValue of
    VQuantifyRatTensor args -> do
      lossExprs <- compileQuantifier mempty args
      return $ toList $ unDisjunctAll lossExprs
    _ -> return [unnormalise 0 expr]

reconstructLossDecls ::
  ( MonadCompile m,
    MonadSupply Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  [Expr Builtin] ->
  m ([Name], [Decl Builtin])
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
    MonadFreeContext Builtin m
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
    MonadFreeContext Builtin m,
    MonadLogic m
  ) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Set Name ->
  (DeclProvenance, Property LiftedExpr) ->
  m (Maybe (DeclProvenance, Property (Expr Builtin)))
convertProperty logicID logic requestedDecls (declProv, property)= case property of
  NonTrivial expr -> do
    if nameOf declProv `Set.member` requestedDecls
      then do
        newExpr <- traverse (\q -> runConversion $ convertQuerySet q) expr
        return $ Just (declProv, NonTrivial newExpr)
      else return Nothing
  Trivial _ -> developerError ("property" <+> quotePretty (nameOf declProv) <+> "is trivial")
  where
  runConversion :: TensorBoundContextT (ReaderT LossCtx m) (QuerySet (Expr Builtin)) -> m (QuerySet (Expr Builtin))
  runConversion action = do
    logCompilerSection2 MidDetail ("translation of" <+> quotePretty (nameOf declProv)) $ do
      runMonadLogicT logicID logic declProv action

extractLoss ::
  (MonadCompile m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  Property (Expr Builtin) ->
  m (Property Name, [Decl Builtin])
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
  Type Builtin ->
  QuerySet (Expr Builtin) ->
  m (QuerySet Name, [Decl Builtin])
extractLossDisjuncts p ident sort lossType (QuerySet negated disjuncts) = do
  queryCount <- get
  let lossExprs = toList $ unDisjunctAll disjuncts
  modify (+ length lossExprs)
  (names, lossDecls) <- reconstructLossDecls p ident sort lossType lossExprs queryCount
  return (QuerySet negated (DisjunctAll $ fromList names), lossDecls)
-}
