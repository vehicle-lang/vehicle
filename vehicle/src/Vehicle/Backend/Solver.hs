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
import Vehicle.Compile.Property (traverseMultiProperty)
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
import Vehicle.Data.Tensor (TensorShape)
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
              -- propertyAddress = PropertyAddress {
              --   propertyName = nameOf prov,
              --   propertyIndices = [0]
              -- },
              ..
            }
            
    let shape = getExprShape normType
    queries <-
      flip runReaderT propertyMetaData $
        runSupplyT [1 :: QueryID ..] $
          -- compileQueries normBody
          compileQueries normBody shape

    -- Warn if trivial.
    case queries of
      Trivial status -> logWarning (TrivialProperty propertyAdd status)
      _ -> return ()

    case outputLocation of
      Nothing -> return ()
      Just folder -> writePropertyVerificationPlan folder propertyAdd (PropertyVerificationPlan queries)

    return propertyAdd

  -- errorOrResult <- compileQueries compilePropertyFn (nameOf prov) normType normBody
  -- -- errorOrResult <- traverseMultiProperty compilePropertyFn (nameOf prov) normType normBody
  -- case errorOrResult of
  --   Left err -> throwError $ MultiPropertyTraveralError prov err
  --   Right result -> return result

-- -- Compiles an individual property of type `Bool`
-- compileSingleProperty ::
--   (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
--   CompilationSettings ->
--   DeclProvenance ->
--   PropertyAddress ->
--   Thunk Builtin ->
--   m PropertyAddress
-- compileSingleProperty CompilationSettings {..} prov propertyAddress expr =
--   logCompilerSection2 MinDetail ("property" <+> quotePretty propertyAddress) $ do
--     let propertyMetaData =
--           PropertyMetaData
--             { propertyProvenance = prov,
--               propertyAddress = propertyAddress,
--               ..
--             }

--     queries <-
--       flip runReaderT propertyMetaData $
--         runSupplyT [1 :: QueryID ..] $
--           compileQueries expr

--     -- Warn if trivial.
--     case queries of
--       Trivial status -> logWarning (TrivialProperty propertyAddress status)
--       _ -> return ()

--     case outputLocation of
--       Nothing -> return ()
--       Just folder -> writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan queries)

--     return propertyAddress

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
  -- m (Property QueryMetaData)
  m (MultiProperty (Property QueryMetaData))
compileQueries expr = do
  showTopLevelEntry expr
  forcedValue <- runFreshTensorBoundContextT $ forceAndRewriteTensor expr
  showTopLevelExit =<< case toBoolTensorValue forcedValue of
    ----------------
    -- Base cases --
    ----------------
    VBoolTensorLiteral b -> return $ Trivial b -- once you know shape, create a MultiProperty containing each Trivial from the Tensor (coming from each value) 
      -- need to traverse thru the boolean tensor
      -- look at tensor for helper functions -- foldMapTensor with Trivial and MultiProperty constructor
    VBoolConstTensor args -> _
    VBoolTensorQuantifyRat (Exists, args) ->
      compileQuantifiedQuerySet False (Left args)
    VBoolTensorQuantifyRecord (Exists, args) ->
      compileQuantifiedQuerySet False (Right args)
    VBoolTensorQuantifyRat (Forall, args) -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      let negatedArgs = negateQuantifierBody args
      compileQuantifiedQuerySet True (Left negatedArgs)
    VBoolTensorQuantifyRecord (Forall, args) -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      let negatedArgs = negateRecordQuantifierBody args
      compileQuantifiedQuerySet True (Right negatedArgs)
    VBoolTensorForeach args -> _
    ---------------------
    -- Recursive cases --
    ---------------------
    VBoolTensorNot args -> compileNot args
    VBoolTensorAnd args -> compileAnd args
    VBoolTensorOr args -> compileOr args
    VBoolTensorIf args -> compileQueries =<< runFreshNameBoundContextT (unfoldIf args)
    VBoolTensorImplies args -> compileQueries $ elimImplies args
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
    VBoolStackTensor {} -> unblock forcedValue
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
    VBoolTensorCompareRatTensor {} -> compileUnquantifiedQuerySet expr
    -- _ -> _
  where
    unblock value = compileQueries =<< runFreshNameBoundContextT (unblockBoolExpr topLevelUnblockingActions (Forced value))

compileAnd :: (MonadCompileQuery m) => TensorOp2Args (Thunk Builtin) -> m (Property QueryMetaData)
compileAnd args@(TensorOp2Args _dims e1 e2) = do
  -- We need to evaluate here otherwise, we may end up compiling queries that are unnecessary
  maybeResult <- runFreshNameBoundContextT $ evalAnd args
  case maybeResult of
    Unevaluable {} -> andTrivial andBoolExpr <$> compileQueries e1 <*> compileQueries e2
    Evaluated result -> compileQueries result

compileOr :: (MonadCompileQuery m) => TensorOp2Args (Thunk Builtin) -> m (Property QueryMetaData)
compileOr args@(TensorOp2Args _dims e1 e2) = do
  -- We need to evaluate here otherwise, we may end up compiling queries that are unnecessary
  maybeResult <- runFreshNameBoundContextT $ evalOr args
  case maybeResult of
    Unevaluable {} -> orTrivial orBoolExpr <$> compileQueries e1 <*> compileQueries e2
    Evaluated result -> compileQueries result

compileNot ::
  (MonadCompileQuery m) =>
  TensorOp1Args (Thunk Builtin) ->
  m (Property QueryMetaData)
compileNot args = do
  compileQueries =<< runFreshNameBoundContextT (lowerNot topLevelUnblockingActions args)

compileQuantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Bool ->
  Either (QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin)) (QuantifyRecordArgs (Thunk Builtin) (Closure Builtin)) ->
  m (Property QueryMetaData)
compileQuantifiedQuerySet isPropertyNegated args =
  runFreshTensorBoundContextT $
    logCompilerSection2 MaxDetail "compilation of query set" $ do
      let action = case args of
            Left tensorArgs -> eliminateExists tensorArgs
            Right recordArgs -> eliminateExistsRecord recordArgs
      (maybePartitions, globalCtx) <- runStateT action emptyGlobalCtx
      compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions

-- | We only need this because we can't evaluate networks in the compiler.
compileUnquantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Thunk Builtin ->
  m (Property QueryMetaData)
compileUnquantifiedQuerySet value =
  runFreshTensorBoundContextT $ do
    let subsectionDoc = "compilation of set of unquantified queries:" <+> prettyFriendlyEmptyCtx value
    logCompilerSection2 MaxDetail subsectionDoc $ do
      (maybePartitions, globalCtx) <- runStateT (eliminateExistless value) emptyGlobalCtx
      compileQuerySetPartitions globalCtx False maybePartitions

compileQuerySetPartitions ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadTensorBoundContext m, MonadStdIO m, MonadError CompileError m) =>
  GlobalCtx ->
  QuerySetNegationStatus ->
  MaybeTrivial Partitions ->
  m (Property QueryMetaData)
compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions = case maybePartitions of
  Trivial b -> return $ Trivial (b `xor` isPropertyNegated)
  NonTrivial partitions -> do
    propertyMetaData <- ask
    maybeQueries <- runReaderT (compilePartitionsToQueries partitions) (propertyMetaData, globalCtx)
    case maybeQueries of
      Trivial b -> return $ Trivial b
      NonTrivial queries -> return $ NonTrivial $ Query $ QuerySet isPropertyNegated queries

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

showTopLevelExit :: (MonadCompile m) => MaybeTrivial a -> m (MaybeTrivial a)
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
    VTensorType elemType dimsValue -> do
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