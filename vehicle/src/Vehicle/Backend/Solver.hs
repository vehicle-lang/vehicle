module Vehicle.Backend.Solver
  ( compileToQueries,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (StateT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Solver.QueryCompilation (compilePartitionsToQueries)
import Vehicle.Backend.Solver.UserVariableElimination (eliminateExistless, eliminateExists, eliminateExistsRecord)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Backend.Solver.UserVariableElimination.Error
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot
import Vehicle.Compile.Normalise.Builtin (elimImplies, evalAnd, evalOr, getDim, getDims)
import Vehicle.Compile.Normalise.Core (BuiltinEvaluationResult (..))
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx)
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..), andTrivial, orTrivial)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Free.Context
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO
import Vehicle.Data.Tensor (TensorShape, foldMapTensor)
import Vehicle.Compile.Normalise.Force (forceThunk)

--------------------------------------------------------------------------------
-- Compilation to individual queries

-- | Compiles the provided program to individual queries suitable for a
-- solver and outputs them. We need to output them as they are generated as
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
  (resourceFreeProg, networkCtx, integrityInfo, missingResources, uninferableParameters) <-
    expandResources resources typedProg
  case (missingResources, uninferableParameters) of
    (r : rs, _) -> throwError $ ResourcesNotProvided (r :| rs)
    (_, r : rs) -> throwError $ InferableParametersUninferrable (r :| rs)
    ([], []) -> return ()

  -- Create the global settings object
  let settings =
        CompilationSettings
          { originalProg = typedProg,
            queryFormat = queryFormat,
            networkCtx = networkCtx,
            outputLocation = maybeVerificationFolder
          }

  -- Perform the actual compilation
  properties <- compileProg settings resourceFreeProg

  -- Check that there were actually properties in the specification.
  when (null properties) $ throwError NoPropertiesFound

  -- Write out the folder
  case maybeVerificationFolder of
    Nothing -> return ()
    Just folder -> do
      let verificationPlan = SpecificationCacheIndex integrityInfo properties
      writeSpecificationCache folder verificationPlan

--------------------------------------------------------------------------------
-- Getting properties

data CompilationSettings = CompilationSettings
  { originalProg :: Prog Builtin,
    queryFormat :: QueryFormat,
    networkCtx :: NetworkContext,
    outputLocation :: Maybe FilePath
  }

compileProg ::
  (MonadStdIO m, MonadCompile m) =>
  CompilationSettings ->
  Prog Builtin ->
  m [(Name, MultiProperty PropertyAddress)]
compileProg settings (Main decls) =
  runFreshFreeContextT (Proxy @Builtin) $
    compileDecls settings decls

compileDecls ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  CompilationSettings ->
  [Decl Builtin] ->
  m [(Name, MultiProperty PropertyAddress)]
compileDecls settings = \case
  [] -> return []
  (d : ds) -> do
    property <- case d of
      DefFunction p ident anns typ body
        | isAnnotatedAsProperty anns ->
            Just <$> do
              let name = nameOf ident
              let prov = (ident, p)
              logCompilerSection2 MinDetail ("property" <+> quotePretty name) $ do
                multiProperty <- compilePropertyDecl settings prov typ body `catchError` handlePropertyCompileError settings prov
                return (name, multiProperty)
      _ -> return Nothing

    addDeclEntryToContext d $ do
      properties <- compileDecls settings ds
      return $ maybeToList property ++ properties

compilePropertyDecl ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  CompilationSettings ->
  DeclProvenance ->
  Type Builtin ->
  Expr Builtin ->
  m (MultiProperty PropertyAddress)
compilePropertyDecl CompilationSettings {..} prov typ body = do
  let normType = Unforced emptyBoundEnv typ
  let normBody = Unforced emptyBoundEnv body

  logCompilerSection2 MinDetail ("property" <+> quotePretty propertyAddress) $ do
    let propertyAdd = PropertyAddress {
                propertyName = nameOf prov,
                propertyIndices = [0]
              }
    let propertyMetaData =
          PropertyMetaData
            { propertyProvenance = prov,
              propertyAddress = propertyAdd,
              ..
            }

    let shape = getExprShape normType
    queries <-
      flip runReaderT propertyMetaData $
        runSupplyT [1 :: QueryID ..] $
          -- compileQueries normBody
          -- compileQueries normBody
          compileQueries normBody shape

    -- -- Warn if trivial. -- no longer need to warn if trivial..?
    -- case queries of
    --   Trivial status -> logWarning (TrivialProperty propertyAdd status)
    --   _ -> return ()

    case outputLocation of
      Nothing -> return ()
      Just folder -> writePropertyVerificationPlan folder propertyAdd (PropertyVerificationPlan queries)

    return _
    -- return propertyAdd

type MonadCompileQuery m =
  ( MonadPropertyStructure m,
    MonadSupply QueryID m,
    MonadStdIO m,
    MonadError CompileError m
  )

-- | Compiles the top-level structure of a property until it hits the first quantifier.
-- Assumptions - expression is well-typed in the empty context and of type Bool Tensor.
-- J: this function should now be working over tensors of queries instead of single queries.
compileQueries ::
  forall m.
  (MonadCompileQuery m) =>
  Thunk Builtin ->
  TensorShape ->
  m (MultiProperty (Property QueryMetaData))
compileQueries expr shape = do
  showTopLevelEntry expr
  forcedValue <- runFreshTensorBoundContextT $ forceAndRewriteTensor expr
  showTopLevelExit =<< case toBoolTensorValue forcedValue of
    -- NOTE: once you know shape, create a MultiProperty containing each Trivial from the Tensor (coming from each value) 
    ----------------
    -- Base cases --
    ----------------
    VBoolTensorLiteral b -> return $ foldMapTensor makeProperty foldProperties b
    VBoolConstTensor (ConstTensorArgs _typ value _dims) -> do
      queries <- compileQueries value []
      return $ StackMultiProperty $ replicate (sum shape) queries
    VBoolStackTensor (StackTensorArgs _typ _d _ds elements) -> do
      StackMultiProperty <$> traverse (\e -> compileQueries e (tail shape)) elements
    VBoolTensorQuantifyRat (Exists, args) ->
      compileQuantifiedQuerySet False (Left args) shape
    VBoolTensorQuantifyRecord (Exists, args) ->
      compileQuantifiedQuerySet False (Right args) shape
    VBoolTensorQuantifyRat (Forall, args) -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      let negatedArgs = negateQuantifierBody args
      compileQuantifiedQuerySet True (Left negatedArgs) shape
    VBoolTensorQuantifyRecord (Forall, args) -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      let negatedArgs = negateRecordQuantifierBody args
      compileQuantifiedQuerySet True (Right negatedArgs) shape
    VBoolTensorForeach {} -> unblock forcedValue
    ---------------------
    -- Recursive cases --
    ---------------------
    VBoolTensorNot args -> compileNot args shape
    VBoolTensorAnd args -> compileAnd args shape
    VBoolTensorOr args -> compileOr args shape
    VBoolTensorIf args -> (compileQueries =<< runFreshNameBoundContextT (unfoldIf args)) shape
    VBoolTensorImplies args -> (compileQueries $ elimImplies args) shape
    -------------------------
    -- Blocked expressions --
    -------------------------
    VBoolTensorReduceAnd {} -> unblock forcedValue
    VBoolTensorReduceOr {} -> unblock forcedValue
    VBoolTensorTensorAt {} -> unblock forcedValue
    VBoolTensorVectorAt {} -> unblock forcedValue
    VBoolTensorFoldList {} -> unblock forcedValue
    VBoolTensorCompareIndex {} -> unblock forcedValue
    VBoolTensorCompareNat {} -> unblock forcedValue
    -----------------
    -- Mixed cases --
    -----------------
    -- We can only fail to unblock these cases because we can't evaluate networks
    -- applied to constant arguments or because of if statements.
    --
    -- (if (forall x . f x > 0) then x else 0) > 0l
    --
    -- When we have the ability to evaluate networks then this case can be turned to a
    -- call to purify.
    VBoolTensorCompareRatTensor {} -> compileUnquantifiedQuerySet expr shape
  where
    unblock value = (compileQueries =<< runFreshNameBoundContextT (unblockBoolExpr topLevelUnblockingActions (Forced value))) shape
    makeProperty :: Bool -> MultiProperty (Property QueryMetaData)
    makeProperty b = SingleProperty $ Trivial b
    foldProperties :: TensorShape -> [MultiProperty (Property QueryMetaData)] -> MultiProperty (Property QueryMetaData)
    foldProperties _shape elems = StackMultiProperty elems

compileAnd :: (MonadCompileQuery m) => TensorOp2Args (Thunk Builtin) -> TensorShape -> m (MultiProperty (Property QueryMetaData))
compileAnd args@(TensorOp2Args _dims e1 e2) shape = do
  -- We need to evaluate here otherwise, we may end up compiling queries that are unnecessary
  maybeResult <- runFreshNameBoundContextT $ evalAnd args
  case maybeResult of
    Unevaluable {} -> AndMultiProperty <$> compileQueries e1 shape <*> compileQueries e2 shape
    -- Unevaluable {} -> andTrivial andBoolExpr <$> compileQueries e1 shape <*> compileQueries e2 shape
    Evaluated result -> compileQueries result shape

compileOr :: (MonadCompileQuery m) => TensorOp2Args (Thunk Builtin) -> TensorShape -> m (MultiProperty (Property QueryMetaData))
compileOr args@(TensorOp2Args _dims e1 e2) shape = do
  -- We need to evaluate here otherwise, we may end up compiling queries that are unnecessary
  maybeResult <- runFreshNameBoundContextT $ evalOr args
  case maybeResult of
    Unevaluable {} -> OrMultiProperty <$> compileQueries e1 shape <*> compileQueries e2 shape
    -- Unevaluable {} -> orTrivial orBoolExpr <$> compileQueries e1 <*> compileQueries e2
    Evaluated result -> compileQueries result shape

compileNot ::
  (MonadCompileQuery m) =>
  TensorOp1Args (Thunk Builtin) ->
  TensorShape ->
  m (MultiProperty (Property QueryMetaData))
compileNot args shape = do
  (compileQueries =<< runFreshNameBoundContextT (lowerNot topLevelUnblockingActions args)) shape

compileQuantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Bool ->
  Either (QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) (QuantifyRecordArgs (Thunk Builtin) (Closure Builtin)) ->
  TensorShape ->
  m (MultiProperty (Property QueryMetaData))
compileQuantifiedQuerySet isPropertyNegated args shape =
  runFreshTensorBoundContextT $
    logCompilerSection2 MaxDetail "compilation of query set" $ do
      let action = case args of
            Left tensorArgs -> eliminateExists tensorArgs
            Right recordArgs -> eliminateExistsRecord recordArgs
      (maybePartitions, globalCtx) <- runStateT action emptyGlobalCtx
      compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions shape

-- | We only need this because we can't evaluate networks in the compiler.
compileUnquantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Thunk Builtin ->
  TensorShape ->
  m (MultiProperty (Property QueryMetaData))
compileUnquantifiedQuerySet value shape =
  runFreshTensorBoundContextT $ do
    let subsectionDoc = "compilation of set of unquantified queries:" <+> prettyFriendlyEmptyCtx value
    logCompilerSection2 MaxDetail subsectionDoc $ do
      (maybePartitions, globalCtx) <- runStateT (eliminateExistless value) emptyGlobalCtx
      compileQuerySetPartitions globalCtx False maybePartitions shape

compileQuerySetPartitions ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadTensorBoundContext m, MonadStdIO m, MonadError CompileError m) =>
  GlobalCtx ->
  QuerySetNegationStatus ->
  MaybeTrivial Partitions ->
  TensorShape ->
  m (MultiProperty (Property QueryMetaData))
compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions _shape = case maybePartitions of
  Trivial b -> return $ SingleProperty $ Trivial (b `xor` isPropertyNegated)
  NonTrivial partitions -> do
    propertyMetaData <- ask
    maybeQueries <- runReaderT (compilePartitionsToQueries partitions) (propertyMetaData, globalCtx)
    case maybeQueries of
      Trivial b -> return $ SingleProperty $ Trivial b
      NonTrivial queries -> return $ SingleProperty $ NonTrivial $ Query $ QuerySet isPropertyNegated queries

topLevelUnblockingActions :: (Monad m) => UnblockingActions m
topLevelUnblockingActions =
  UnblockingActions
    { unblockBoundVar = \_ _ -> developerError "No bound variables should exist at top-level",
      unblockNetworkApp = \_ _ ident args -> return $ IfLeaf $ Forced $ VFreeVar ident (mkExpr accessSpine args),
      unblockDatasetOrParameter = \_ _ -> developerError "Should not be unblocking datasets or parameters"
    }

handlePropertyCompileError ::
  (MonadIO m, MonadCompile m) =>
  CompilationSettings ->
  DeclProvenance ->
  CompileError ->
  m a
handlePropertyCompileError CompilationSettings {..} declProv err = do
  let formatID = queryFormatID queryFormat
  throwError =<< case err of
    UnsupportedNonLinearConstraint {} -> diagnoseNonLinearity formatID originalProg declProv
    UnsupportedAlternatingQuantifiers {} -> diagnoseAlternatingQuantifiers formatID originalProg declProv
    _ -> return err

showTopLevelEntry :: (MonadCompile m) => Thunk Builtin -> m ()
showTopLevelEntry v = do
  logDebugM MaxDetail $ do
    let vDoc = prettyFriendly (WithContext v emptyNamedCtx)
    return $ "top-elim-enter" <+> vDoc
  incrCallDepth

-- showTopLevelExit :: (MonadCompile m) => MaybeTrivial a -> m (MaybeTrivial a)
showTopLevelExit :: (MonadCompile m) => MultiProperty a -> m (MultiProperty a)
showTopLevelExit v = do
  decrCallDepth
  logDebugM MaxDetail $ do
    -- vDoc <- prettyExternalInCtx v
    return "top-elim-exit" -- vDoc
  return v

getExprShape :: Thunk Builtin -> TensorShape
-- getExprShape :: (MonadCompile m) => Thunk Builtin -> TensorShape
getExprShape typ = do
  forcedType <- runFreshNameBoundContextT $ forceThunk typ
  case toTypeValue forcedType of
    VVectorType elemType dimValue -> do
      maybeDim <- runFreshNameBoundContextT $ getDim dimValue
      case maybeDim of
        Nothing -> []
        Just dim -> dim : getExprShape elemType
        -- Nothing -> throwError $ UnsupportedVectorDimension dimValue
        -- Just dim -> goVector elemType dim indices body
    VTensorType _elemType dimsValue -> do
      maybeDims <- runFreshNameBoundContextT $ getDims dimsValue
      case maybeDims of
        Nothing -> []
        Just dims -> dims
        -- Nothing -> throwError $ UnsupportedTensorDimensions dimsValue
        -- Just dims -> goTensor dims indices body
    _ -> []
    -- _ -> throwError $ UnreducableType typ

    -- if unreducable type (0-dimensional), return [] (empty list) instead of 
    -- if see a Vector, need to traverse into elemType and continue grabbing shape
    -- if a Tensor, the bottom has reached and can just return shape
    -- shapes will need to be passed back up the recursive calls and appended to the prev calls (to get the full shape)