module Vehicle.Backend.LossFunction.Domain
  ( -- extractSearchDomain,
  -- Domain (..),
  )
where

{-
import Control.Applicative (Applicative (..))
import Control.Monad (when)
import Control.Monad.Except (MonadError (..), runExceptT, void)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Core (MixedLossBinder, MixedLossValue)
import Vehicle.Compile.Boolean.LiftIf (unfoldIf)
import Vehicle.Compile.Boolean.LowerNot (lowerNot)
import Vehicle.Compile.Boolean.Unblock (ReduceVectorVars, UnblockingActions (..))
import Vehicle.Compile.Boolean.Unblock qualified as Unblocking
import Vehicle.Compile.Context.Bound (MonadBoundContext, getNamedBoundCtx)
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error (CompileError (..), MonadCompile, unexpectedExprError)
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Rational.LinearExpr
import Vehicle.Compile.Variable (createUserVar)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Tensor (TensorBuiltin (..))
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Linear (addExprs, rearrangeExprToSolveFor)
import Vehicle.Data.Expr.Value
import Vehicle.Data.QuantifiedVariable

type MonadDomain m =
  ( MonadCompile m,
    MonadBoundContext MixedLossValue m,
    MonadFreeContext Builtin m
  )

type MonadSearch m =
  ( MonadDomain m,
    MonadReader VariableInfo m
  )

-- | Information for the variable whose domain we are trying to find.
data VariableInfo = VariableInfo
  { variableLv :: Lv,
    vectorExpr :: WHNFValue Builtin,
    reducedVars :: [(Lv, UserRationalVariable)]
  }

extractSearchDomain ::
  (MonadDomain m) =>
  DeclProvenance ->
  MixedLossBinder ->
  Lv ->
  WHNFClosure Builtin ->
  m (Domain, WHNFValue Builtin)
extractSearchDomain propertyProv binder lv (WHNFClosure env expr) = do
  -- Convert the binder
  namedCtx <- getNamedBoundCtx (Proxy @MixedLossValue)
  let originalTypedBinder = fmap _ binder
  userVar <- createUserVar propertyProv namedCtx originalTypedBinder
  let envEntry@(reducedUseVars, _) = reduceVariable userTensorVarDimensions (lv + 1) userVar
  let variableInfo = _

  -- Normalise the body
  let newEnv = extendEnvWithBound lv binder env
  value <- normaliseInEnv newEnv expr

  -- Search for constraints
  (maybeConstraints, remainder) <- flip runReaderT _ $ findConstraints value
  case maybeConstraints of
    Nothing -> throwError $ NoQuantifierDomainFound propertyProv (void binder) Nothing
    Just constraints -> do
      let maybeDomain = extractDomainFromConstraints constraints reducedUseVars
      case maybeDomain of
        Left missingCostraints -> throwError $ NoQuantifierDomainFound propertyProv (void binder) (Just missingCostraints)
        Right domain -> return (domain, remainder)

--------------------------------------------------------------------------------
-- Constraints

data VariableConstraints = VariableConstraints
  { lowerBounds :: Map Lv (NFValue TensorBuiltin),
    upperBounds :: Map Lv (NFValue TensorBuiltin)
  }

instance Semigroup VariableConstraints where
  x <> y =
    VariableConstraints
      { lowerBounds = Map.unionWith (\u v -> VBuiltin MaxRatTensor (explicit <$> [u, v])) (lowerBounds x) (lowerBounds y),
        upperBounds = Map.unionWith (\u v -> VBuiltin MinRatTensor (explicit <$> [u, v])) (upperBounds x) (upperBounds y)
      }

instance Monoid VariableConstraints where
  mempty = VariableConstraints mempty mempty

type ConstrainedValue = (Maybe VariableConstraints, WHNFValue Builtin)

updateConstrainedValue ::
  WHNFValue Builtin ->
  ConstrainedValue ->
  ConstrainedValue
updateConstrainedValue originalExpr = \case
  constr@(Just {}, _) -> constr
  (Nothing, _) -> (Nothing, originalExpr)

--------------------------------------------------------------------------------
-- Domain

data Domain = Domain
  { lowerBound :: NFValue TensorBuiltin,
    upperBound :: NFValue TensorBuiltin
  }

extractDomainFromConstraints ::
  VariableConstraints ->
  [(Lv, UserRationalVariable)] ->
  Either [(UserRationalVariable, UnderConstrainedVariableStatus)] Domain
extractDomainFromConstraints VariableConstraints {..} allVariables = do
  let lowerBoundExprs = flip map allVariables $ \(lv, var) ->
        case (Map.lookup lv lowerBounds, Map.lookup lv upperBounds) of
          (Just x, Just y) -> Right (x, y)
          (Just {}, Nothing) -> Left (var, BoundedBelow)
          (Nothing, Just {}) -> Left (var, BoundedAbove)
          (Nothing, Nothing) -> Left (var, Unconstrained)

  let (missingVars, presentVarBounds) = partitionEithers lowerBoundExprs
  if not $ null missingVars
    then Left missingVars
    else do
      let n = length allVariables
      let (lowerBoundElements, upperBoundElements) = unzip presentVarBounds
      let lowerBoundExpr = VBuiltin (StackRatTensor n) (explicit <$> lowerBoundElements)
      let upperBoundExpr = VBuiltin (StackRatTensor n) (explicit <$> upperBoundElements)
      Right $ Domain lowerBoundExpr upperBoundExpr

--------------------------------------------------------------------------------
-- Constraint search

findConstraints :: (MonadSearch m) => WHNFValue Builtin -> m ConstrainedValue
findConstraints expr = case expr of
  ----------------------------------
  -- No useful domain information --
  ----------------------------------
  IBoolLiteral {} -> return (Nothing, expr)
  IExists {} -> return (Nothing, expr)
  IForall {} -> return (Nothing, expr)
  INotEqual {} -> return (Nothing, expr)
  IOr {} -> return (Nothing, expr) -- Handle disjoint domains?
  ----------------
  -- Base cases --
  ----------------
  IOrder OrderRat op _x _y -> tryPurifyAssertion expr (handleRatInequality op)
  ---------------------
  -- Recursive cases --
  ---------------------
  INot e -> handleNot e
  IIf _ c x y -> updateConstrainedValue expr <$> (findConstraints =<< unfoldIf c x y)
  IEqual EqRat x y -> updateConstrainedValue expr <$> findConstraints (unfoldEquality x y)
  IAnd x y -> do
    (cx, x') <- findConstraints x
    (cy, y') <- findConstraints y
    return (liftA2 (<>) cx cy, IAnd x' y')
  _ -> unblockBoolExpr expr

handleNot ::
  (MonadSearch m) =>
  WHNFValue Builtin ->
  m ConstrainedValue
handleNot expr = do
  loweredExpr <- lowerNot return expr
  case loweredExpr of
    INot {} -> return (Nothing, expr)
    newExpr -> updateConstrainedValue expr <$> findConstraints newExpr

unfoldEquality ::
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin
unfoldEquality x y = IAnd (IOrderRat Le x y) (IOrderRat Ge x y)

--------------------------------------------------------------------------------
-- Unblocking

tryPurifyAssertion ::
  (MonadSearch m) =>
  WHNFValue Builtin ->
  (WHNFValue Builtin -> WHNFValue Builtin -> m ConstrainedValue) ->
  m ConstrainedValue
tryPurifyAssertion value whenPure = do
  ctx <- getNamedBoundCtx (Proxy @MixedLossValue)
  result <- runExceptT (Unblocking.tryPurifyAssertion ctx unblockingActions value)
  case result of
    Left _blocked -> return (Nothing, value)
    Right (Right (x, y)) -> whenPure x y
    Right (Left purified) -> updateConstrainedValue value <$> findConstraints purified

unblockBoolExpr ::
  (MonadSearch m) =>
  WHNFValue Builtin ->
  m ConstrainedValue
unblockBoolExpr value = do
  ctx <- getNamedBoundCtx (Proxy @MixedLossValue)
  result <- runExceptT (Unblocking.unblockBoolExpr ctx unblockingActions value)
  case result of
    Left {} -> return (Nothing, value)
    Right unblockedValue -> do
      constrainedValue <- findConstraints unblockedValue
      return $ updateConstrainedValue value constrainedValue

unblockingActions ::
  (MonadError (WHNFValue Builtin) m, MonadReader VariableInfo m) =>
  UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockFreeVectorVar = unblockFreeVectorVariable,
      unblockBoundVectorVar = unblockBoundVectorVariable
    }

unblockFreeVectorVariable ::
  (MonadError (WHNFValue Builtin) m) =>
  (ReduceVectorVars -> WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  ReduceVectorVars ->
  Identifier ->
  WHNFSpine Builtin ->
  m (WHNFValue Builtin)
unblockFreeVectorVariable _unblockVector _reduceVectorVars ident spine =
  throwError $ VFreeVar ident spine

unblockBoundVectorVariable ::
  (MonadError (WHNFValue Builtin) m, MonadReader VariableInfo m) =>
  Lv ->
  m (WHNFValue Builtin)
unblockBoundVectorVariable lv = do
  VariableInfo {..} <- ask

  when (lv /= variableLv) $
    throwError $
      VBoundVar lv []

  return vectorExpr

--------------------------------------------------------------------------------
-- Compilation of inequalities

handleRatInequality ::
  (MonadSearch m) =>
  OrderOp ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m ConstrainedValue
handleRatInequality op e1 e2 = do
  let e = ISub SubRat e1 e2
  result <- runExceptT (compileRatLinearExpr e)
  case result of
    Right (Linear a b) -> do
      case op of
        Le -> return (_, ITrueExpr mempty)
    _ -> return (Nothing, IOrderRat op e1 e2)

data Bound
  = Constant (NFValue TensorBuiltin)
  | Linear (NFValue TensorBuiltin) (NFValue TensorBuiltin)

compileRatLinearExpr ::
  forall m.
  (MonadLogger m, MonadError NonLinearity m) =>
  WHNFValue Builtin ->
  m Bound
compileRatLinearExpr = go
  where
    go :: WHNFValue Builtin -> m Bound
    go e = case e of
      ----------------
      -- Base cases --
      ----------------
      IRatLiteral _ l -> return $ Constant _
      VBoundVar lv [] -> return $ Linear _ _
      ---------------------
      -- Inductive cases --
      ---------------------
      INeg NegRat v -> scaleExpr (-1) <$> go v
      IAdd AddRat e1 e2 -> addExprs 1 1 <$> go e1 <*> go e2
      ISub SubRat e1 e2 -> addExprs 1 (-1) <$> go e1 <*> go e2
      IMul MulRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case (e1', e2') of
          (Constant c1, Constant c2) -> return $ scaleExpr c1 e2'
          (_, Just c2) -> return $ scaleExpr c2 e1'
          _ -> throwError NonLinearity
      IDiv DivRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case isConstant e2' of
          (Just c2) -> return $ scaleExpr (1 / c2) e1'
          _ -> throwError NonLinearity
      -----------------
      -- Error cases --
      -----------------
      ex -> unexpectedExprError "compile linear rational expression" $ prettyVerbose ex
-}
