module Vehicle.Backend.Queries.UserVariableElimination
  ( eliminateUserVariables,
    UserVariableReconstruction,
    UserVariableReconstructionStep (..),
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..), evalStateT)
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Vehicle.Backend.Queries.PostProcessing (convertPartitionsToQueries)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.EliminateExists (eliminateExists)
import Vehicle.Compile.Boolean.LiftIf (liftIfSpine, unfoldIf)
import Vehicle.Compile.Boolean.LowerNot (lowerNot)
import Vehicle.Compile.Boolean.Unblock (ReduceVectorVars, UnblockingActions (..))
import Vehicle.Compile.Boolean.Unblock qualified as Unblocking
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Compile.Resource (NetworkTensorType (..), NetworkType (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Boolean
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Linear (LinearExpr, addExprs, constantExpr, isConstant, scaleExpr, singletonVarExpr)
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Tensor (RationalTensor, Tensor (..), zeroTensor)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdEqualsVector, StdNotEqualsVector))
import Vehicle.Verify.Core (NetworkContextInfo (..), QuerySetNegationStatus)
import Vehicle.Verify.QueryFormat (QueryFormat (..), supportsStrictInequalities)
import Vehicle.Verify.Specification
import Vehicle.Verify.Variable
import Prelude hiding (Applicative (..))

--------------------------------------------------------------------------------
-- Algorithm

-- | Compiles the top-level structure of a property until it hits the first quantifier.
-- Assumptions - expression is well-typed in the empty context and of type Bool.
eliminateUserVariables ::
  forall m.
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  WHNFValue Builtin ->
  m (Property QueryMetaData)
eliminateUserVariables = go
  where
    go :: WHNFValue Builtin -> m (Property QueryMetaData)
    go expr = case expr of
      ----------------
      -- Base cases --
      ----------------
      IBoolLiteral _ b -> return $ Trivial b
      ---------------------
      -- Recursive cases --
      ---------------------
      IAnd e1 e2 -> andTrivial andBoolExpr <$> go e1 <*> go e2
      IOr e1 e2 -> orTrivial orBoolExpr <$> go e1 <*> go e2
      IIf _ c x y -> go =<< unfoldIf c x y
      IExists _ (VLam binder (WHNFClosure env body)) ->
        compileQuantifiedQuerySet False binder env body
      IForall _ (VLam binder (WHNFClosure env body)) ->
        compileQuantifiedQuerySet True binder env (INot body)
      -----------------
      -- Mixed cases --
      -----------------
      -- In the next three cases, we can only fail to unblock these cases because
      -- we can't evaluate networks applied to constant arguments.
      --
      -- (if (forall x . f x > 0) then x else 0) > 0
      --
      -- When we have that ability then  case can be turned to an error.
      -- These cases can happen, e.g.
      INot {} -> compileUnquantifiedQuerySet expr
      IEqual {} -> compileUnquantifiedQuerySet expr
      INotEqual {} -> compileUnquantifiedQuerySet expr
      IOrder {} -> compileUnquantifiedQuerySet expr
      IVectorEqual {} -> compileUnquantifiedQuerySet expr
      IVectorNotEqual {} -> compileUnquantifiedQuerySet expr
      -- This final case can only occur at all because
      -- we can't evaluate networks applied to constant arguments.
      -- When we have that ability we can replace it with an error.
      _ -> compileUnquantifiedQuerySet expr

compileQuantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  Bool ->
  WHNFBinder Builtin ->
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (Property QueryMetaData)
compileQuantifiedQuerySet isPropertyNegated binder env body = do
  let subsectionDoc = "compilation of set of quantified queries:" <+> prettyFriendlyEmptyCtx (IExists [] (VLam binder (WHNFClosure env body)))
  logCompilerPass MaxDetail subsectionDoc $ do
    flip evalStateT emptyGlobalCtx $ do
      maybePartitions <- compileExists binder env body
      compileQuerySetPartitions isPropertyNegated maybePartitions

-- | We only need this because we can't evaluate networks in the compiler.
compileUnquantifiedQuerySet ::
  (MonadPropertyStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  WHNFValue Builtin ->
  m (Property QueryMetaData)
compileUnquantifiedQuerySet value = do
  let subsectionDoc = "compilation of set of unquantified queries:" <+> prettyFriendlyEmptyCtx value
  logCompilerPass MaxDetail subsectionDoc $ do
    flip evalStateT emptyGlobalCtx $ do
      (maybePartitions, equalities) <- runWriterT $ compileBoolExpr value
      networkEqPartitions <- networkEqualitiesToPartition equalities
      let allPartitions = andTrivial andPartitions maybePartitions networkEqPartitions
      compileQuerySetPartitions False allPartitions

compileQuerySetPartitions ::
  (MonadQueryStructure m, MonadSupply QueryID m, MonadStdIO m) =>
  QuerySetNegationStatus ->
  MaybeTrivial Partitions ->
  m (Property QueryMetaData)
compileQuerySetPartitions isPropertyNegated maybePartitions = case maybePartitions of
  Trivial b -> return $ Trivial (b `xor` isPropertyNegated)
  NonTrivial partitions -> do
    queries <- convertPartitionsToQueries partitions
    return $ NonTrivial $ Query $ QuerySet isPropertyNegated queries

-- | Attempts to compile an arbitrary expression of type `Bool` down to a tree
-- of assertions implicitly existentially quantified by a set of network
-- input/output variables.
compileBoolExpr ::
  (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m) =>
  WHNFValue QueryBuiltin ->
  m (MaybeTrivial Partitions)
compileBoolExpr expr = case expr of
  ----------------
  -- Base cases --
  ----------------
  IBoolLiteral _ b -> return $ Trivial b
  IOrder OrderRat op _ _ -> tryPurifyAssertion expr (compileRationalAssertion (ordToAssertion op))
  IEqual EqRat _ _ -> tryPurifyAssertion expr (compileRationalAssertion eqToAssertion)
  IVectorEqualFull (IVecEqSpine t1 t2 n s _ _) -> tryPurifyAssertion expr (compileTensorAssertion [t1, t2, n, s])
  IForall {} -> throwError catchableUnsupportedAlternatingQuantifiersError
  ---------------------
  -- Recursive cases --
  ---------------------
  INotEqual EqRat e1 e2 -> compileBoolExpr =<< eliminateNotEqualRat e1 e2
  IVectorNotEqualFull spine -> compileBoolExpr =<< eliminateNotVectorEqual spine
  INot e -> compileBoolExpr =<< eliminateNot e
  IIf _ c x y -> compileBoolExpr =<< unfoldIf c x y
  IAnd x y -> andTrivial andPartitions <$> compileBoolExpr x <*> compileBoolExpr y
  IOr x y -> orTrivial orPartitions <$> compileBoolExpr x <*> compileBoolExpr y
  IExists _ (VLam binder (WHNFClosure env body)) -> compileExists binder env body
  _ -> compileBoolExpr =<< unblockBoolExpr expr

eliminateNot ::
  (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
eliminateNot = lowerNot (eliminateNot <=< unblockBoolExpr)

eliminateNotEqualRat ::
  (MonadQueryStructure m) =>
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
eliminateNotEqualRat x y = do
  PropertyMetaData {..} <- ask
  if supportsStrictInequalities queryFormat
    then return $ IOr (IOrderRat Le x y) (IOrderRat Le y x)
    else throwError $ UnsupportedInequality (queryFormatID queryFormat) propertyProvenance

eliminateNotVectorEqual ::
  (MonadQueryStructure m) =>
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
eliminateNotVectorEqual = appHiddenStdlibDef StdNotEqualsVector

compileRationalAssertion ::
  (MonadQueryStructure m) =>
  (LinearExpr RationalVariable Rational -> LinearExpr RationalVariable Rational -> Assertion) ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (MaybeTrivial Partitions)
compileRationalAssertion mkAssertion x y = do
  e1' <- compileRatLinearExpr x
  e2' <- compileRatLinearExpr y
  return $ mkTrivialPartition (mkAssertion e1' e2')

compileRatLinearExpr ::
  forall m.
  (MonadQueryStructure m) =>
  WHNFValue QueryBuiltin ->
  m (LinearExpr RationalVariable Rational)
compileRatLinearExpr = go
  where
    go :: WHNFValue QueryBuiltin -> m (LinearExpr RationalVariable Rational)
    go e = case e of
      ----------------
      -- Base cases --
      ----------------
      IRatLiteral _ l -> return $ constantExpr l
      VBoundVar lv [] -> singletonVarExpr 0 <$> getRationalVariable lv
      ---------------------
      -- Inductive cases --
      ---------------------
      INeg NegRat v -> scaleExpr (-1) <$> go v
      IAdd AddRat e1 e2 -> addExprs 1 1 <$> go e1 <*> go e2
      ISub SubRat e1 e2 -> addExprs 1 (-1) <$> go e1 <*> go e2
      IMul MulRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case (isConstant e1', isConstant e2') of
          (Just c1, _) -> return $ scaleExpr c1 e2'
          (_, Just c2) -> return $ scaleExpr c2 e1'
          _ -> throwError catchableUnsupportedNonLinearConstraint
      IDiv DivRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case isConstant e2' of
          (Just c2) -> return $ scaleExpr (1 / c2) e1'
          _ -> throwError catchableUnsupportedNonLinearConstraint
      -----------------
      -- Error cases --
      -----------------
      ex -> unexpectedExprError "compile linear rational expression" $ prettyVerbose ex

compileTensorAssertion ::
  (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m) =>
  WHNFSpine QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (MaybeTrivial Partitions)
compileTensorAssertion spinePrefix x y = do
  x' <- compileTensorLinearExpr x
  y' <- compileTensorLinearExpr y
  let maybeAssertion = liftA2 tensorEqToAssertion x' y'
  case maybeAssertion of
    Just assertion -> return $ mkTrivialPartition assertion
    Nothing -> do
      logDebug MaxDetail "Unable to solve tensor equality so reducing to rational equalities"
      compileBoolExpr =<< appHiddenStdlibDef StdEqualsVector (spinePrefix <> (Arg mempty Explicit Relevant <$> [x, y]))

compileTensorLinearExpr ::
  forall m.
  (MonadQueryStructure m) =>
  WHNFValue QueryBuiltin ->
  m (Maybe (LinearExpr TensorVariable RationalTensor))
compileTensorLinearExpr = go
  where
    go :: WHNFValue QueryBuiltin -> m (Maybe (LinearExpr TensorVariable RationalTensor))
    go e = case e of
      ---------------------
      -- Inductive cases --
      ---------------------
      IVectorAdd _ _ _ _ _ e1 e2 -> liftA2 (addExprs 1 1) <$> go e1 <*> go e2
      IVectorSub _ _ _ _ _ e1 e2 -> liftA2 (addExprs 1 (-1)) <$> go e1 <*> go e2
      ----------------
      -- Base cases --
      ----------------
      IVecLiteral {} -> do
        return (constantExpr <$> getRationalTensor e)
      VBoundVar lv [] -> do
        var <- getTensorVariable lv
        return $ Just $ singletonVarExpr (zeroTensor $ tensorVariableDims var) var
      _ -> return Nothing

getRationalTensor :: WHNFValue QueryBuiltin -> Maybe RationalTensor
getRationalTensor expr = uncurry Tensor <$> go expr
  where
    go :: WHNFValue QueryBuiltin -> Maybe (TensorShape, Vector Rational)
    go = \case
      IRatLiteral _ r -> Just ([], Vector.singleton (fromRational r))
      IVecLiteral _ xs -> do
        r <- traverse (go . argExpr) xs
        let (dims, rs) = unzip r
        case dims of
          [] -> Nothing
          (ds : _) -> Just (length xs : ds, mconcat rs)
      _ -> Nothing

tryPurifyAssertion ::
  (MonadQuantifierBody m) =>
  WHNFValue Builtin ->
  (WHNFValue Builtin -> WHNFValue Builtin -> m (MaybeTrivial Partitions)) ->
  m (MaybeTrivial Partitions)
tryPurifyAssertion expr whenPure = do
  ctx <- getGlobalNamedBoundCtx
  Unblocking.tryPurifyAssertion ctx unblockingActions expr compileBoolExpr whenPure

unblockBoolExpr ::
  (MonadQuantifierBody m) =>
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockBoolExpr value = do
  ctx <- getGlobalNamedBoundCtx
  Unblocking.unblockBoolExpr ctx unblockingActions value

--------------------------------------------------------------------------------
-- Infinite quantifier elimination

compileExists ::
  (MonadQueryStructure m) =>
  WHNFBinder QueryBuiltin ->
  WHNFBoundEnv QueryBuiltin ->
  Expr Ix QueryBuiltin ->
  m (MaybeTrivial Partitions)
compileExists binder env body = do
  let varName = getBinderName binder
  let subpassDoc = "compilation of quantified variable" <+> quotePretty varName
  logCompilerPass MidDetail subpassDoc $ do
    -- Create the user variable
    userVar <- createUserVar binder

    -- Update the global context
    globalCtx <- get
    let (userVarExpr, newGlobalCtx) = addUserVarToGlobalContext userVar globalCtx
    put newGlobalCtx

    -- Normalise the expression
    let newEnv = extendEnvWithDefined userVarExpr binder env
    normExpr <- normaliseInEnv newEnv body

    -- Recursively compile the expression.
    (partitions, networkInputEqualities) <- runWriterT (compileBoolExpr normExpr)

    -- Prepend network equalities to the tree (prepending is important for
    -- performance as the search for constraints will find them first.)
    networkEqPartitions <- networkEqualitiesToPartition networkInputEqualities
    let finalPartitions = andTrivial andPartitions partitions networkEqPartitions

    -- Solve for the user variable.
    eliminateExists finalPartitions userVar

createUserVar ::
  (MonadQueryStructure m) =>
  WHNFBinder QueryBuiltin ->
  m OriginalUserVariable
createUserVar binder = do
  let varName = getBinderName binder
  checkUserVariableNameIsUnique varName
  varDimensions <- checkUserVariableType binder
  return $
    OriginalUserVariable
      { userTensorVarName = varName,
        userTensorVarDimensions = varDimensions
      }

checkUserVariableNameIsUnique ::
  (MonadQueryStructure m) =>
  Name ->
  m ()
checkUserVariableNameIsUnique varName = do
  localCtx <- getGlobalBoundCtx
  let isDuplicateName = any (\v -> getBinderName v == varName) localCtx
  when isDuplicateName $ do
    PropertyMetaData {..} <- ask
    throwError $ DuplicateQuantifierNames propertyProvenance varName

checkUserVariableType ::
  forall m.
  (MonadQueryStructure m) =>
  WHNFBinder QueryBuiltin ->
  m TensorShape
checkUserVariableType binder = go (typeOf binder)
  where
    go :: WHNFType QueryBuiltin -> m TensorShape
    go = \case
      IRatType {} -> return []
      IVectorType _ tElem (INatLiteral _ d) -> do
        ds <- go tElem
        return $ d : ds
      tElem -> do
        PropertyMetaData {..} <- ask
        let p = provenanceOf binder
        let baseName = getBinderName binder
        let declIdent = fst propertyProvenance
        throwError $ UnsupportedVariableType (queryFormatID queryFormat) declIdent p baseName tElem (typeOf binder) [BuiltinType Rat]

networkEqualitiesToPartition :: (MonadQueryStructure m) => [WHNFValue Builtin] -> m (MaybeTrivial Partitions)
networkEqualitiesToPartition networkEqualities = do
  logDebugM MaxDetail $ do
    networkEqDocs <- traverse prettyFriendlyInCtx networkEqualities
    return $ line <> "Generated network equalities:" <> line <> indent 2 (vsep networkEqDocs)

  (partitions, newNetworkEqualities) <- runWriterT (compileBoolExpr (foldr IAnd (IBoolLiteral mempty True) networkEqualities))
  if null newNetworkEqualities
    then return partitions
    else andTrivial andPartitions partitions <$> networkEqualitiesToPartition newNetworkEqualities

--------------------------------------------------------------------------------
-- Unblocking

type MonadQuantifierBody m =
  ( MonadQueryStructure m,
    MonadWriter [WHNFValue Builtin] m
  )

unblockingActions :: (MonadQuantifierBody m) => UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockBoundVectorVar = unblockBoundVectorVariable,
      unblockFreeVectorVar = unblockFreeVectorVariable
    }

unblockBoundVectorVariable ::
  (MonadQuantifierBody m) =>
  Lv ->
  m (WHNFValue QueryBuiltin)
unblockBoundVectorVariable lv = do
  maybeReduction <- getReducedVariableExprFor lv
  case maybeReduction of
    Just vectorReduction -> return vectorReduction
    Nothing -> return $ VBoundVar lv []

unblockFreeVectorVariable ::
  (MonadQuantifierBody m) =>
  (ReduceVectorVars -> WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  ReduceVectorVars ->
  Identifier ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockFreeVectorVariable unblockVector reduceVectorVars ident spine = do
  let networkName = nameOf ident
  networkContext <- asks networkCtx
  networkInfo <- case Map.lookup networkName networkContext of
    Nothing -> compilerDeveloperError $ "Expecting" <+> quotePretty ident <+> "to be a @network"
    Just info -> return info

  unblockedSpine <- traverse (traverse (unblockVector False)) spine
  liftIfSpine unblockedSpine $ \unblockedSpine' ->
    if unblockedSpine' /= unblockedSpine
      then return $ VFreeVar ident unblockedSpine'
      else do
        let networkApp = (networkName, unblockedSpine')
        globalCtx <- get

        case LinkedHashMap.lookup networkApp (networkApplications globalCtx) of
          Just existingAppInfo -> return $ outputVarExpr existingAppInfo
          Nothing -> do
            input <- case spine of
              [inputArg] -> return $ argExpr inputArg
              _ -> do
                exprDoc <- prettyFriendlyInCtx (VFreeVar ident spine)
                compilerDeveloperError $
                  "Found network application with multiple arguments:"
                    <> line
                    <> indent 2 exprDoc

            let (appInfo, newGlobalCtx) = addNetworkApplicationToGlobalCtx networkApp networkInfo globalCtx
            let inputDims = dimensions (inputTensor (networkType networkInfo))
            let inputEquality = mkVVectorEquality inputDims (inputVarExpr appInfo) input
            put newGlobalCtx
            tell [inputEquality]
            unblockVector reduceVectorVars (outputVarExpr appInfo)

--------------------------------------------------------------------------------
-- Vector operations preservation

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedAlternatingQuantifiersError :: CompileError
catchableUnsupportedAlternatingQuantifiersError =
  UnsupportedAlternatingQuantifiers x x x
  where
    x = developerError "Evaluating temporary quantifier error"

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedNonLinearConstraint :: CompileError
catchableUnsupportedNonLinearConstraint =
  UnsupportedNonLinearConstraint x x x
  where
    x = developerError "Evaluating temporary quantifier error"
