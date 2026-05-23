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
import Data.Maybe (maybeToList, fromMaybe)
import Data.Proxy (Proxy (..))
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Solver.QueryCompilation (compilePartitionsToQueries)
import Vehicle.Backend.Solver.UserVariableElimination (eliminateExistless, eliminateExists)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Backend.Solver.UserVariableElimination.Error
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot, negateRatTensorQuantifierBody)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx)
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Property (traverseMultiProperty)
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Scoping (constructFromTensorFreeVar, constructTensorisableDims)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..), andTrivial, orTrivial)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor
import Vehicle.Data.Variable.Free.Context
import Vehicle.Prelude.Warning (CompileWarning (..))
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
    runSupplyT [(0 :: PropertyID) ..] $
      compileDecls settings decls

compileDecls ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m, MonadSupply PropertyID m) =>
  CompilationSettings ->
  [Decl Builtin] ->
  m [(Name, MultiProperty PropertyAddress)]
compileDecls settings = \case
  [] -> return []
  (d : ds) -> do
    decl <- evalDecl d
    property <- case decl of
      DefFunction p ident anns typ body
        | isAnnotatedAsProperty anns ->
            Just <$> do
              let name = nameOf ident
              let prov = (ident, p)
              logCompilerSection2 MinDetail ("property" <+> quotePretty name) $ do
                multiProperty <- compilePropertyDecl settings prov typ body `catchError` handlePropertyCompileError settings prov
                return (name, multiProperty)
      _ -> return Nothing

    addDeclEntryToContext decl $ do
      properties <- compileDecls settings ds
      return $ maybeToList property ++ properties

compilePropertyDecl ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m, MonadSupply PropertyID m) =>
  CompilationSettings ->
  DeclProvenance ->
  VType Builtin ->
  Value Builtin ->
  m (MultiProperty PropertyAddress)
compilePropertyDecl settings prov typ body = do
  propertyID <- demand
  let compilePropertyFn = compileSingleProperty settings prov
  logDebug MaxDetail $ prettyFriendlyEmptyCtx typ
  logDebug MaxDetail $ prettyFriendlyEmptyCtx body
  errorOrResult <- traverseMultiProperty compilePropertyFn propertyID (nameOf prov) typ body
  case errorOrResult of
    Left err -> throwError $ MultiPropertyTraveralError prov err
    Right result -> return result

-- Compiles an individual property of type `Bool`
compileSingleProperty ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  CompilationSettings ->
  DeclProvenance ->
  PropertyAddress ->
  Value Builtin ->
  m PropertyAddress
compileSingleProperty CompilationSettings {..} prov propertyAddress expr =
  logCompilerSection2 MinDetail ("property" <+> quotePretty propertyAddress) $ do
    let propertyMetaData =
          PropertyMetaData
            { propertyProvenance = prov,
              propertyAddress = propertyAddress,
              ..
            }

    queries <-
      flip runReaderT propertyMetaData $
        runFreshTensorBoundContextT $
          runSupplyT [1 :: QueryID ..] $
            compileQueries expr

    -- Warn if trivial.
    case queries of
      Trivial status -> logWarning (TrivialProperty propertyAddress status)
      _ -> return ()

    case outputLocation of
      Nothing -> return ()
      Just folder -> writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan queries)

    return propertyAddress

-- | Compiles the top-level structure of a property until it hits the first quantifier.
-- Assumptions - expression is well-typed in the empty context and of type Bool.
compileQueries ::
  forall m.
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Value Builtin ->
  m (Property QueryMetaData)
compileQueries expr = do
  showTopLevelEntry expr
  showTopLevelExit =<< case toBoolValue expr of
    ----------------
    -- Base cases --
    ----------------
    VBoolLiteral b -> return $ Trivial b
    VQuantifyRatTensor (Exists, args) -> compileQuantifiedQuerySet False args []
    VQuantifyRatTensor (Forall, args) -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      negatedArgs <- negateRatTensorQuantifierBody args
      compileQuantifiedQuerySet True negatedArgs []
    VQuantifyRecord (Exists, args) -> do
      (wrappedBinderArgs, step) <- wrapQuantifyRecord args
      compileQuantifiedQuerySet False wrappedBinderArgs [step]
    VQuantifyRecord (Forall, args) -> do
      logDebug MaxDetail $ "negate" <+> pretty Forall
      (wrappedBinderArgs, step) <- wrapQuantifyRecord args
      negatedArgs <- negateRatTensorQuantifierBody wrappedBinderArgs
      compileQuantifiedQuerySet True negatedArgs [step]
    ---------------------
    -- Recursive cases --
    ---------------------
    VAnd (TensorOp2Args _dims e1 e2) -> andTrivial andBoolExpr <$> compileQueries e1 <*> compileQueries e2
    VOr (TensorOp2Args _dims e1 e2) -> orTrivial orBoolExpr <$> compileQueries e1 <*> compileQueries e2
    VBoolIf args -> compileQueries =<< unfoldIf args
    -------------------------
    -- Blocked expressions --
    -------------------------
    VReduceAndTensor {} -> compileQueries =<< unblock expr
    VReduceOrTensor {} -> compileQueries =<< unblock expr
    VBoolAt {} -> compileQueries =<< unblock expr
    VCompareIndex {} -> compileQueries =<< unblock expr
    VCompareNat {} -> compileQueries =<< unblock expr
    VNot args -> compileQueries =<< lowerNot args
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
    unblock = unblockBoolExpr topLevelUnblockingActions

compileQuantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Bool ->
  QuantifyRatTensorArgs (Value Builtin) (Closure Builtin) ->
  [CompilationStep] ->
  m (Property QueryMetaData)
compileQuantifiedQuerySet isPropertyNegated args prevSteps =
  logCompilerSection2 MaxDetail "compilation of query set" $ do
    (maybePartitions, globalCtx) <- runStateT (eliminateExists args prevSteps) emptyGlobalCtx
    compileQuerySetPartitions globalCtx isPropertyNegated maybePartitions

-- | Takes a record quantifier and wraps the binder & body in a tensor quantifier
--  e.g. given Pair has fields { a : Real, b : Real }
--  forall (r : Pair) . (body)
--  becomes
--  forall (_t0 : tensor Real [2]) . (body (_PairFromTensor _t0))
wrapQuantifyRecord ::
  ( MonadPropertyStructure m,
    MonadSupply QueryID m,
    MonadStdIO m,
    MonadFreeContext Builtin m
  ) =>
  QuantifyRecordArgs (Value Builtin) (Closure Builtin) ->
  m (QuantifyRatTensorArgs (Value Builtin) (Closure Builtin), CompilationStep)
wrapQuantifyRecord QuantifyRecordArgs {..} = do
  namedCtx <- getNameContext
  recordTypeIdent <- case toTypeValue quantifyRecordType of
    VFreeTypeVar v _spine -> pure v
    _ -> developerError "Record binder is not of expected format."

  -- Construct \r -> body from binder and body in record quantifier args
  recordQLam <- unnormaliseInCtx $ VLam quantifyRecordBinder quantifyRecordBody
  fields <- getRecordFields recordTypeIdent
  let shape = constructTensorisableDims fields
  let dims = mkDims shape

  -- Build tensor binder with appropriate dims and type for record
  let Closure boundEnv _body = quantifyRecordBody
  tensorType <- eval namedCtx boundEnv $ fromDSL mempty $ tTensor tRat (toDSL dims)
  normalisedDims <- eval namedCtx boundEnv dims
  let tensorBinderName = getFreshTensorBinderName namedCtx
  let tensorBinder = mkExplicitBinder tensorType (Just (mempty, tensorBinderName))

  let tensorBoundVar = explicit $ BoundVar mempty 0
  recordTypeProv <- getRecordProvenance recordTypeIdent
  -- Construct _PairFromTensor _t0
  let fromTensorExpr = App (constructFromTensorFreeVar recordTypeIdent recordTypeProv) [tensorBoundVar]

  -- Construct body (_PairFromTensor _t0)
  let nestedBody = App recordQLam [Arg Explicit Relevant fromTensorExpr]
  let ratTensorArgs = QuantifyRatTensorArgs normalisedDims tensorBinder (Closure boundEnv nestedBody)
  
  fieldNames <- getRecordFieldNames recordTypeIdent
  let name = fromMaybe (developerError "Quantified variable binder should have name") (nameOf quantifyRecordBinder)
  return (ratTensorArgs, ConvertQuantifiedTensorLike tensorBinderName name fieldNames)


-- | We only need this because we can't evaluate networks in the compiler.
compileUnquantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
  Value Builtin ->
  m (Property QueryMetaData)
compileUnquantifiedQuerySet value = do
  let subsectionDoc = "compilation of set of unquantified queries:" <+> prettyFriendlyEmptyCtx value
  logCompilerSection2 MaxDetail subsectionDoc $ do
    (maybePartitions, globalCtx) <- runStateT (eliminateExistless value) emptyGlobalCtx
    compileQuerySetPartitions globalCtx False maybePartitions

compileQuerySetPartitions ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m, MonadError CompileError m) =>
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
    { unblockRatTensorBoundVar = developerError "No bound variables should exist at top-level",
      unblockRecordBoundVar = developerError "No bound variables should exist at top-level",
      unblockNetworkApp = \_ _ _ -> developerError "Unblocking of constant network functions at top-level not yet supported",
      unblockDatasetOrParameter = developerError "Should not be unblocking datasets or parameters"
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
