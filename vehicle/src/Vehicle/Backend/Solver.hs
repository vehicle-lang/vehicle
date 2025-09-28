module Vehicle.Backend.Solver
  ( compileToQueries,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (StateT (..))
import Data.Maybe (isNothing, maybeToList)
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Solver.QueryCompilation (compilePartitionsToQueries)
import Vehicle.Backend.Solver.UserVariableElimination (eliminateExistless, eliminateExists)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Backend.Solver.UserVariableElimination.Error
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, notClosure)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx)
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, isZeroDimensional)
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameContextT)
import Vehicle.Data.Variable.Free.Context
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Syntax.Tensor (unstack)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO

--------------------------------------------------------------------------------
-- Compilation to individual queries

-- | Compiles the provided program to individual queries suitable for a
-- verifier and outputs them. We need to output them as they are generated as
-- otherwise storing all the queries can result in an out-of-memory errors.
compileToQueries ::
  (MonadStdIO m, MonadCompile m) =>
  QueryFormat ->
  Prog Builtin ->
  Resources ->
  Maybe FilePath ->
  m ()
compileToQueries queryFormat typedProg resources maybeVerificationFolder = do
  -- Create the verification folder if required.
  case maybeVerificationFolder of
    Nothing -> return ()
    Just folder -> liftIO $ createDirectoryIfMissing True folder

  -- Expand out the external resources in the specification (datasets, networks etc.)
  (Main resourceFreeDecls, networkCtx, freeCtx, integrityInfo) <-
    expandResources resources typedProg

  -- Perform the actual compilation to queries
  properties <-
    runFreeContextT freeCtx $
      compileDecls typedProg queryFormat networkCtx 0 resourceFreeDecls maybeVerificationFolder

  -- Check that there were actually properties in the specification.
  when (null properties) $ do
    throwError NoPropertiesFound

  case maybeVerificationFolder of
    Nothing -> return ()
    Just folder -> do
      let verificationPlan = SpecificationCacheIndex integrityInfo properties
      writeSpecificationCache folder verificationPlan

--------------------------------------------------------------------------------
-- Getting properties

compileDecls ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Builtin ->
  QueryFormat ->
  NetworkContext ->
  PropertyID ->
  [Decl Builtin] ->
  Maybe FilePath ->
  m [(Name, MultiProperty ())]
compileDecls _ _ _ _ [] _ = return []
compileDecls prog queryFormat networkCtx propertyID (d : ds) outputLocation = do
  property <- case d of
    DefFunction p ident anns _ body
      | isAnnotatedAsProperty anns -> do
          let propertyData = (queryFormat, networkCtx, (ident, p), propertyID, outputLocation)
          Just <$> compilePropertyDecl prog propertyData body
    _ -> return Nothing

  addDeclToContext d $ do
    let newPropertyID = if isNothing property then propertyID else propertyID + 1
    properties <- compileDecls prog queryFormat networkCtx newPropertyID ds outputLocation
    return $ maybeToList property ++ properties

type MultiPropertyMetaData =
  ( QueryFormat,
    NetworkContext,
    DeclProvenance,
    Int,
    Maybe FilePath
  )

updateMetaData :: MultiPropertyMetaData -> TensorIndices -> PropertyMetaData
updateMetaData (queryFormat, networkCtx, declProvenance, propertyID, outputLocation) indices =
  PropertyMetaData
    { networkCtx = networkCtx,
      queryFormat = queryFormat,
      propertyProvenance = declProvenance,
      propertyAddress = PropertyAddress propertyID (nameOf $ fst declProvenance) indices,
      outputLocation = outputLocation
    }

compilePropertyDecl ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Builtin ->
  MultiPropertyMetaData ->
  Expr Builtin ->
  m (Name, MultiProperty ())
compilePropertyDecl prog propertyData@(_, _, declProv@(ident, _), _, _) expr = do
  logCompilerSection2 MinDetail ("property" <+> quotePretty ident) $ do
    normalisedExpr <- normaliseInEmptyEnv expr
    multiProperty <-
      compileMultiProperty propertyData normalisedExpr
        `catchError` handlePropertyCompileError prog propertyData
    return (nameOf (fst declProv), multiProperty)

handlePropertyCompileError :: (MonadCompile m) => Prog Builtin -> MultiPropertyMetaData -> CompileError -> m a
handlePropertyCompileError prog (queryFormat, _, declProv, _, _) e = do
  let formatID = queryFormatID queryFormat
  case e of
    UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity formatID prog declProv
    UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers formatID prog declProv
    _ -> throwError e

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileMultiProperty ::
  forall m.
  (MonadStdIO m, MonadFreeContext Builtin m, MonadCompile m) =>
  MultiPropertyMetaData ->
  Value Builtin ->
  m (MultiProperty ())
compileMultiProperty multiPropertyMetaData = go []
  where
    go :: TensorIndices -> Value Builtin -> m (MultiProperty ())
    go indices expr = case expr of
      (getExpr accessStackTensor -> Just args) -> do
        let es' = zip [0 :: Int ..] $ stackElements args
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      (getExpr accessBoolTensorLiteral -> Just bs) | not (isZeroDimensional bs) -> do
        -- Important to test for non-zero dimensionality otherwise we don't display the correct
        -- warnings for trivial tensors nor generate .vcl-plan file.
        let es' = zip [0 :: Int ..] (fromBoolTensorValue . VBoolTensorLiteral <$> unstack bs)
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      (getExpr accessVecLit -> Just args) -> do
        let es' = zip [0 :: Int ..] $ vecLitElements args
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      _ -> do
        let propertyMetaData@PropertyMetaData {..} = updateMetaData multiPropertyMetaData indices
        flip runReaderT propertyMetaData $ do
          logCompilerSection2 MinDetail ("property" <+> quotePretty propertyAddress) $ do
            compileSingleProperty expr
            return $ SingleProperty propertyAddress ()

-- Compiles an individual property
compileSingleProperty ::
  (MonadPropertyStructure m, MonadStdIO m) =>
  Value Builtin ->
  m ()
compileSingleProperty expr = do
  queries <- runSupplyT [1 :: QueryID ..] $ eliminateUserVariables expr

  PropertyMetaData {..} <- ask

  -- Warn if trivial.
  case queries of
    Trivial status -> logWarning (TrivialProperty propertyAddress status)
    _ -> return ()

  case outputLocation of
    Nothing -> return ()
    Just folder -> writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan queries)

-- | Compiles the top-level structure of a property until it hits the first quantifier.
-- Assumptions - expression is well-typed in the empty context and of type Bool.
eliminateUserVariables ::
  forall m.
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  Value Builtin ->
  m (Property QueryMetaData)
eliminateUserVariables expr = do
  showTopLevelEntry expr
  showTopLevelExit =<< case toBoolValue expr of
    ----------------
    -- Base cases --
    ----------------
    VBoolLiteral b -> return $ Trivial b
    VQuantifyRatTensor Exists dims binder closure -> compileQuantifiedQuerySet False dims binder closure
    VQuantifyRatTensor Forall dims binder closure -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      let negatedClosure = notClosure 0 dims closure
      compileQuantifiedQuerySet True dims binder negatedClosure
    ---------------------
    -- Recursive cases --
    ---------------------
    VAnd (TensorOp2Args _dims e1 e2) -> andTrivial andBoolExpr <$> eliminateUserVariables e1 <*> eliminateUserVariables e2
    VOr (TensorOp2Args _dims e1 e2) -> orTrivial orBoolExpr <$> eliminateUserVariables e1 <*> eliminateUserVariables e2
    VBoolIf args -> eliminateUserVariables =<< runFreshNameContextT (unfoldIf args)
    -------------------------
    -- Blocked expressions --
    -------------------------
    VReduceAndTensor {} -> eliminateUserVariables =<< unblock expr
    VReduceOrTensor {} -> eliminateUserVariables =<< unblock expr
    VBoolAt {} -> eliminateUserVariables =<< unblock expr
    VCompareIndex {} -> eliminateUserVariables =<< unblock expr
    VCompareNat {} -> eliminateUserVariables =<< unblock expr
    VNot args -> eliminateUserVariables =<< lowerNot mempty unblock args
    -----------------
    -- Mixed cases --
    -----------------
    -- We can only fail to unblock these cases because we can't evaluate networks
    -- applied to constant arguments or because of if statements.
    --
    -- (if (forall x . f x > 0) then x else 0) > 0
    --
    -- When we have the ability to evaluate networks then this case can be turned to a
    -- call to purify.
    VCompareRatTensor {} -> compileUnquantifiedQuerySet expr
  where
    unblock e = runFreshNameContextT (unblockBoolExpr topLevelUnblockingActions e)

compileQuantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  Bool ->
  VArg Builtin ->
  VBinder Builtin ->
  Closure Builtin ->
  m (Property QueryMetaData)
compileQuantifiedQuerySet isPropertyNegated _dims binder closure = logCompilerSection2 MaxDetail "compilation of query set" $ do
  (maybePartitions, globalCtx) <- runStateT (eliminateExists binder closure) emptyGlobalCtx
  compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions

-- | We only need this because we can't evaluate networks in the compiler.
compileUnquantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  Value Builtin ->
  m (Property QueryMetaData)
compileUnquantifiedQuerySet value = do
  let subsectionDoc = "compilation of set of unquantified queries:" <+> prettyFriendlyEmptyCtx value
  logCompilerSection2 MaxDetail subsectionDoc $ do
    (maybePartitions, globalCtx) <- runStateT (eliminateExistless value) emptyGlobalCtx
    compileQuerySetPartitions globalCtx False maybePartitions

compileQuerySetPartitions ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  GlobalCtx ->
  QuerySetNegationStatus ->
  MaybeTrivial Partitions ->
  m (Property QueryMetaData)
compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions = case maybePartitions of
  Trivial b -> return $ Trivial (b `xor` isPropertyNegated)
  NonTrivial partitions -> do
    propertyMetaData <- ask
    maybeQueries <- compilePartitionsToQueries globalCtx propertyMetaData partitions
    case maybeQueries of
      Trivial b -> return $ Trivial b
      NonTrivial queries -> return $ NonTrivial $ Query $ QuerySet isPropertyNegated queries

topLevelUnblockingActions :: (MonadCompile m) => UnblockingActions m
topLevelUnblockingActions =
  UnblockingActions
    (developerError "Should not be unblocking variables at top-level")
    (developerError "Unblocking of constant network functions at top-level not yet supported")

showTopLevelEntry :: (MonadCompile m) => Value Builtin -> m ()
showTopLevelEntry v = do
  logDebugM MaxDetail $ do
    let vDoc = prettyFriendly (WithContext v emptyNamedCtx)
    return $ "top-elim-enter" <+> vDoc
  incrCallDepth

showTopLevelExit :: (MonadCompile m) => MaybeTrivial a -> m (MaybeTrivial a)
showTopLevelExit v = do
  decrCallDepth
  logDebugM MaxDetail $ do
    -- vDoc <- prettyExternalInCtx v
    return "top-elim-exit" -- vDoc
  return v
