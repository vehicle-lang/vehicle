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
import Vehicle.Data.Code.ForcedValue (GenericClosure (..), GenericForcedValue (..), GenericThunk (..), Thunk, emptyBoundEnv, namedBoundContextToEnv)
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

-- Returns a list of boolean trees, a list of boolean decls, and a Prog of all
-- decls in a specification (with domains extracted if they are properties)
convertToSearchTree ::
  (MonadCompile m) =>
  Prog Builtin ->
  m ([BooleanTree], [Decl Builtin], Prog Builtin)
convertToSearchTree (Main ds) = do
  runFreshFreeContextT (Proxy @Builtin) $ do
    logCompilerPass LossQuantifierLifting $ do
      (booleanTrees, boolDecls, domainExtractedDecls) <- convertDecls ds
      return (booleanTrees, boolDecls, Main domainExtractedDecls)

convertDecls ::
  (MonadSearch m) =>
  [Decl Builtin] ->
  m ([BooleanTree], [Decl Builtin], [Decl Builtin])
convertDecls = \case
  [] -> return ([], [], [])
  decl : decls -> do
    (maybeBooleanTree, maybeBoolDecl, maybeDomainExtractedDecl) <- convertDecl decl
    (booleanTrees, boolDecls, domainExtractedDecls) <- addDeclEntryToContext decl $ convertDecls decls
    return (maybeToList maybeBooleanTree ++ booleanTrees, fromMaybe [] maybeBoolDecl ++ boolDecls, fromMaybe [] maybeDomainExtractedDecl ++ domainExtractedDecls)

convertDecl ::
  forall m.
  (MonadSearch m) =>
  Decl Builtin ->
  m (Maybe BooleanTree, Maybe [Decl Builtin], Maybe [Decl Builtin])
convertDecl decl = case decl of
  DefAbstract {} -> return (Nothing, Nothing, Just [decl])
  DefFunction p ident sort typ expr
    | isAnnotatedAsProperty sort ->
        logCompilerPass LossQuantifierLifting $ do
          let normExpr = Unforced emptyBoundEnv expr
          let declProv = (ident, p)
          booleanTreeLifted <-
            runFreshNameBoundContextT $
              flip runReaderT declProv $
                compileHardBooleanTree normExpr
          (booleanTreeNames@(BooleanTree _ _ tree), boolDecls, domainExtractedDecls) <-
            reconstructProperty p ident sort typ booleanTreeLifted
          logDebug MaxDetail $ prettyFriendlyEmptyCtx tree
          return (Just booleanTreeNames, Just boolDecls, Just domainExtractedDecls)
    | otherwise -> return (Nothing, Nothing, Just [decl])
  DefRecord {} -> return (Nothing, Nothing, Just [decl])

reconstructProperty ::
  (MonadSearch m) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  Property LiftedData ->
  m (BooleanTree, [Decl Builtin], [Decl Builtin])
reconstructProperty p ident sort typ = \case
  NonTrivial expr -> do
    reconstructed <- runSupplyT [0 :: Int ..] (traverse (reconstructQuerySet p ident sort typ) expr)
    let expr' = fmap (\(e, _, _) -> e) reconstructed
    let boolDecls = foldMap (\(_, b, _) -> b) reconstructed
    let domainExtractedDecls = foldMap (\(_, _, d) -> d) reconstructed
    let booleanTree = BooleanTree p ident (NonTrivial expr')
    return (booleanTree, boolDecls, domainExtractedDecls)
  Trivial bool -> do
    let booleanTree = BooleanTree p ident (Trivial bool)
    return (booleanTree, [], [])

reconstructQuerySet ::
  ( MonadSearch m,
    MonadSupply Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  QuerySet LiftedData ->
  m (QuerySet Name, [Decl Builtin], [Decl Builtin])
reconstructQuerySet p ident sort typ (QuerySet negated (DisjunctAll liftedData)) = do
  reconstructedDisjuncts <- runReaderT (traverse (reconstructQueryDisjunct p ident sort typ) liftedData) (ident, p)
  let (domainExtractedNames, boolDecls, domainExtractedDecls) = unzip3 $ toList reconstructedDisjuncts
  return (QuerySet negated (DisjunctAll $ fromList $ concat domainExtractedNames), concat boolDecls, concat domainExtractedDecls)

reconstructQueryDisjunct ::
  ( MonadSearch m,
    MonadSupply Int m,
    MonadReader DeclProvenance m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  LiftedData ->
  m ([Name], [Decl Builtin], [Decl Builtin])
reconstructQueryDisjunct p ident sort typ liftedData = do
  (quantifiedBoolExpr, boolExpr) <- runFreshNameBoundContextT $ reconstructExpr liftedData
  logDebug MaxDetail $ prettyFriendlyEmptyCtx quantifiedBoolExpr
  domainExtractedExprs <- runFreshTensorBoundContextT $ do
    forcedValue <- forceThunk quantifiedBoolExpr
    -- Separate VQuantifyRecord case not handled yet
    case toBoolValue forcedValue of
      VQuantifyRatTensor args -> do
        domainExtracted <- compileQuantifier mempty args
        return $ toList $ unDisjunctAll domainExtracted
      _ -> return [unnormalise 0 quantifiedBoolExpr]
  (domainExtractedNames, boolDecls, domainExtractedDecls) <- reconstructDecls p ident sort typ (unnormalise 0 boolExpr) domainExtractedExprs
  return (domainExtractedNames, boolDecls, domainExtractedDecls)

-- Returns a Thunk representing a quantified expression, and
-- a Thunk representing a non-quantified expression with VLam(s) around it
reconstructExpr ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  ) =>
  LiftedData ->
  m (Thunk Builtin, Thunk Builtin)
reconstructExpr (quantifiers, value) = case quantifiers of
  [] -> return (value, value)
  (quantifier, dimsOrType, binder) : qs -> do
    (newQuantifiedBody, newLamBody) <- addNameToContext binder $ do
      (reconstructedQuantified, reconstructedLam) <- reconstructExpr (qs, value)
      lv <- getBinderDepth
      let quantifiedBody = unnormalise lv reconstructedQuantified
      let lamBody = unnormalise lv reconstructedLam
      return (quantifiedBody, lamBody)
    newEnv <- namedBoundContextToEnv <$> getNameContext
    let newLamExpr = Forced $ VLam binder (Closure newEnv newLamBody)
    newQuantifiedExpr <- do
      case dimsOrType of
        Left (pDims, bDims) -> return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newQuantifiedBody)))
        Right typ -> return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newQuantifiedBody)))
    return (newQuantifiedExpr, newLamExpr)

-- This takes a single non-quantified expression, and
-- a list of the domain extracted expressions associated with it
reconstructDecls ::
  ( MonadCompile m,
    MonadSupply Int m
  ) =>
  Provenance ->
  Identifier ->
  DefFunctionSort ->
  Type Builtin ->
  Expr Builtin ->
  [Expr Builtin] ->
  m ([Name], [Decl Builtin], [Decl Builtin])
reconstructDecls p ident sort typ boolExpr domainExtractedExprs = case domainExtractedExprs of
  [] -> return ([], [], [])
  e : es -> do
    exprCount <- demand @Int
    let newNameDomainExtracted = nameOf ident <> T.pack (show exprCount)
    let newIdentDomainExtracted = changeName ident newNameDomainExtracted
    let domainExtractedDecl = DefFunction p newIdentDomainExtracted sort typ e
    let newNameBool = newNameDomainExtracted <> "-bool"
    let newIdentBool = changeName ident newNameBool
    let boolDecl = DefFunction p newIdentBool sort typ boolExpr
    (newDomainExtractedNames, boolDecls, domainExtractedDecls) <- reconstructDecls p ident sort typ boolExpr es
    return (newNameDomainExtracted : newDomainExtractedNames, boolDecl : boolDecls, domainExtractedDecl : domainExtractedDecls)
