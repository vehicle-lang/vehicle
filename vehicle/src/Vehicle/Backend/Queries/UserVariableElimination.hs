module Vehicle.Backend.Queries.UserVariableElimination
  ( eliminateUserVariables,
    UserVariableReconstruction,
    UserVariableReconstructionStep (..),
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), StateT (..))
import Control.Monad.Writer (MonadWriter, WriterT (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Vehicle.Backend.Queries.PostProcessing (convertPartitionsToQueries)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.EliminateExists (eliminateExists)
import Vehicle.Backend.Queries.UserVariableElimination.EliminateNot (eliminateNot)
import Vehicle.Backend.Queries.UserVariableElimination.Unblocking (tryUnblockBool)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.LinearExpr (LinearExpr, RationalTensor (..), addExprs, constantExpr, isConstant, scaleExpr, singletonVarExpr, zeroTensor)
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdEqualsVector, StdNotEqualsVector))
import Vehicle.Verify.Core (MetaNetwork, QueryContents)
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
  (MonadPropertyStructure m) =>
  WHNFValue Builtin ->
  m (Property (MetaNetwork, UserVariableReconstruction, QueryContents))
eliminateUserVariables = go
  where
    go :: WHNFValue Builtin -> m (Property (MetaNetwork, UserVariableReconstruction, QueryContents))
    go expr = case expr of
      ----------------
      -- Base cases --
      ----------------
      VBoolLiteral b -> return $ Trivial b
      ---------------------
      -- Recursive cases --
      ---------------------
      VAnd e1 e2 -> andTrivial andBoolExpr <$> go e1 <*> go e2
      VOr e1 e2 -> orTrivial orBoolExpr <$> go e1 <*> go e2
      VIf _ c x y -> go (eliminateIf c x y)
      VExists _ binder env body -> compileQuantifiedQuerySet False binder env body
      VForall _ binder env body -> do
        logDebug MinDetail ("Negating property..." <> line)
        compileQuantifiedQuerySet True binder env (negExpr body)
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
      VNot {} -> compileUnquantifiedQuerySet expr
      VEqual {} -> compileUnquantifiedQuerySet expr
      VNotEqual {} -> compileUnquantifiedQuerySet expr
      VOrder {} -> compileUnquantifiedQuerySet expr
      VVectorEqual {} -> compileUnquantifiedQuerySet expr
      VVectorNotEqual {} -> compileUnquantifiedQuerySet expr
      -- This final case can only occur at all because
      -- we can't evaluate networks applied to constant arguments.
      -- When we have that ability we can replace it with an error.
      _ -> compileUnquantifiedQuerySet expr

compileQuantifiedQuerySet ::
  (MonadPropertyStructure m) =>
  Bool ->
  WHNFBinder Builtin ->
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (Property (MetaNetwork, UserVariableReconstruction, QueryContents))
compileQuantifiedQuerySet isPropertyNegated binder env body = do
  let subsectionDoc = "compilation of set of quantified queries:" <+> prettyFriendlyEmptyCtx (VExists [] binder env body)
  logCompilerPass MaxDetail subsectionDoc $ do
    (maybePartitions, globalCtx) <- runStateT (compileExists binder env body) emptyGlobalCtx
    case maybePartitions of
      Trivial b -> return $ Trivial (b `xor` isPropertyNegated)
      NonTrivial partitions -> do
        queries <- convertPartitionsToQueries globalCtx partitions
        return $ NonTrivial $ Query $ QuerySet isPropertyNegated queries

-- | We only need this because we can't evaluate networks in the compiler.
compileUnquantifiedQuerySet ::
  (MonadPropertyStructure m) =>
  WHNFValue Builtin ->
  m (Property (MetaNetwork, UserVariableReconstruction, QueryContents))
compileUnquantifiedQuerySet value = do
  let subsectionDoc = "compilation of set of unquantified queries:" <+> prettyFriendlyEmptyCtx value
  logCompilerPass MaxDetail subsectionDoc $ do
    ((maybePartitions, globalCtx), equalities) <- runWriterT (runStateT (compileBoolExpr value) emptyGlobalCtx)
    (networkEqPartitions, _) <- runStateT (networkEqualitiesToPartition equalities) globalCtx
    let equalitiesPartition = andTrivial andPartitions maybePartitions networkEqPartitions
    case equalitiesPartition of
      Trivial b -> return $ Trivial b
      NonTrivial partitions -> do
        queries <- convertPartitionsToQueries globalCtx partitions
        return $ NonTrivial $ Query $ QuerySet False queries

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
  VBoolLiteral b -> return $ Trivial b
  VOrder OrderRat op e1 e2 -> tryUnblockBool expr compileBoolExpr (compileRationalAssertion (ordToAssertion op) e1 e2)
  VEqual EqRat e1 e2 -> tryUnblockBool expr compileBoolExpr (compileRationalAssertion eqToAssertion e1 e2)
  VVectorEqualFull spine@(VVecEqArgs e1 e2) -> tryUnblockBool expr compileBoolExpr (compileTensorAssertion spine e1 e2)
  VForall {} -> throwError catchableUnsupportedAlternatingQuantifiersError
  ---------------------
  -- Recursive cases --
  ---------------------
  VNotEqual EqRat e1 e2 -> compileBoolExpr =<< eliminateNotEqualRat e1 e2
  VVectorNotEqualFull spine -> compileBoolExpr =<< eliminateNotVectorEqual spine
  VNot e -> compileBoolExpr =<< eliminateNot e
  VIf _ c x y -> compileBoolExpr (eliminateIf c x y)
  VAnd x y -> andTrivial andPartitions <$> compileBoolExpr x <*> compileBoolExpr y
  VOr x y -> orTrivial orPartitions <$> compileBoolExpr x <*> compileBoolExpr y
  VExists _ binder env body -> compileExists binder env body
  _ -> tryUnblockBool expr compileBoolExpr (compilerDeveloperError "Could not unblock bool expr")

eliminateIf :: WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin
eliminateIf c x y = VOr (VAnd c x) (VAnd (VNot c) y)

eliminateNotEqualRat ::
  (MonadQueryStructure m) =>
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
eliminateNotEqualRat x y = do
  PropertyMetaData {..} <- ask
  if supportsStrictInequalities queryFormat
    then return $ VOr (VOrder OrderRat Le x y) (VOrder OrderRat Le y x)
    else throwError $ UnsupportedInequality (queryFormatID queryFormat) propertyProvenance

eliminateNotVectorEqual ::
  (MonadQueryStructure m) =>
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
eliminateNotVectorEqual = appStdlibDef StdNotEqualsVector

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
      VRatLiteral l -> return $ constantExpr l
      VBoundVar lv [] -> singletonVarExpr 0 <$> getRationalVariable lv
      ---------------------
      -- Inductive cases --
      ---------------------
      VNeg NegRat v -> scaleExpr (-1) <$> go v
      VAdd AddRat e1 e2 -> addExprs 1 1 <$> go e1 <*> go e2
      VSub SubRat e1 e2 -> addExprs 1 (-1) <$> go e1 <*> go e2
      VMul MulRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case (isConstant e1', isConstant e2') of
          (Just c1, _) -> return $ scaleExpr c1 e2'
          (_, Just c2) -> return $ scaleExpr c2 e1'
          _ -> throwError catchableUnsupportedNonLinearConstraint
      VDiv DivRat e1 e2 -> do
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
compileTensorAssertion spine x y = do
  x' <- compileTensorLinearExpr x
  y' <- compileTensorLinearExpr y
  let maybeAssertion = liftA2 tensorEqToAssertion x' y'
  case maybeAssertion of
    Just assertion -> return $ mkTrivialPartition assertion
    Nothing -> compileBoolExpr =<< appStdlibDef StdEqualsVector spine

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
      VVectorAdd e1 e2 -> liftA2 (addExprs 1 1) <$> go e1 <*> go e2
      VVectorSub e1 e2 -> liftA2 (addExprs 1 (-1)) <$> go e1 <*> go e2
      ----------------
      -- Base cases --
      ----------------
      VVecLiteral {} -> do
        return (constantExpr <$> getRationalTensor e)
      VBoundVar lv [] -> do
        var <- getTensorVariable lv
        return $ Just $ singletonVarExpr (zeroTensor $ tensorVariableDims var) var
      _ -> return Nothing

getRationalTensor :: WHNFValue QueryBuiltin -> Maybe RationalTensor
getRationalTensor expr = uncurry RationalTensor <$> go expr
  where
    go :: WHNFValue QueryBuiltin -> Maybe (TensorDimensions, Vector Rational)
    go = \case
      VRatLiteral r -> Just ([], Vector.singleton (fromRational r))
      VVecLiteral xs -> do
        r <- traverse (go . argExpr) xs
        let (dims, rs) = unzip r
        case dims of
          [] -> Nothing
          (ds : _) -> Just (length xs : ds, mconcat rs)
      _ -> Nothing

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
    normExpr <- eval newEnv body

    -- Recursively compile the expression.
    (partitions, networkInputEqualities) <- runWriterT (compileBoolExpr normExpr)

    -- Prepend network equalities to the tree (prepending is important for
    -- performance as the search for constraints will find them first.)
    networkEqPartitions <- networkEqualitiesToPartition networkInputEqualities
    let finalPartitions = andTrivial andPartitions partitions networkEqPartitions
    logDebug MaxDetail $ pretty finalPartitions

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
  m TensorDimensions
checkUserVariableType binder = go (typeOf binder)
  where
    go :: WHNFType QueryBuiltin -> m TensorDimensions
    go = \case
      VRatType -> return []
      VVectorType tElem (VNatLiteral d) -> do
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
  (partitions, newNetworkEqualities) <- runWriterT (compileBoolExpr (foldr VAnd (VBoolLiteral True) networkEqualities))
  unless (null newNetworkEqualities) $
    compilerDeveloperError "New network equalities generated when compiling network equalities."
  return partitions

-- (mkSinglePartition (mempty, conjunct equalities))
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
