module Vehicle.Backend.LossFunction.Domain where

{-
import Vehicle.Backend.LossFunction.Core (MixedLossBinder, MixedLossValue)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (FreeEnv, eval)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Expr.Boolean (BooleanExpr (..), MaybeTrivial (..), orTrivial, andTrivial)
import Vehicle.Compile.Boolean.LiftIf (unfoldIf)
import Vehicle.Compile.Boolean.LowerNot (lowerNot)
import Vehicle.Compile.Boolean.Unblock (ReduceVectorVars, UnblockingActions (..))
import Vehicle.Compile.Boolean.Unblock qualified as Unblocking
import Data.Map (Map)
import qualified Data.Map as Map
import Vehicle.Compile.Context.Bound (MonadBoundContext, getNamedBoundCtx)
import Data.Proxy (Proxy(..))
import Control.Monad.Except (MonadError (..), runExceptT)

data Domain = Domain
  { lowerBound :: WHNFValue Builtin,
    upperBound :: WHNFValue Builtin
  }

type MonadDomain m =
  ( MonadCompile m
  , MonadBoundContext MixedLossValue m
  )

extractQuantifierDomain ::
  (MonadDomain m) =>
  MixedLossBinder ->
  Lv ->
  WHNFClosure Builtin ->
  m (Maybe (Domain, Expr Ix Builtin))
extractQuantifierDomain binder lv (WHNFClosure env expr) = case expr of
  BuiltinFunc Implies [argExpr -> e1, argExpr -> e2] -> do
    maybeDomain <- extractDomain binder _ env e1
    return $ (,e2) <$> maybeDomain
  _ -> do
    logWarning _
    return Nothing

extractType ::
  (MonadDomain m) =>
  WHNFType Builtin ->
  m (WHNFType Builtin)
extractType = _

extractDomain ::
  (MonadDomain m) =>
  MixedLossBinder ->
  FreeEnv (WHNFClosure Builtin) Builtin ->
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (Maybe Domain)
extractDomain binder freeEnv boundEnv expr = do
  let finalEnv = extendEnvWithBound _ _ boundEnv
  value <- eval freeEnv finalEnv expr
  _

data VariableConstraints = VariableConstraints
  { lowerBounds :: Map Lv (WHNFValue Builtin)
  , upperBounds :: Map Lv (WHNFValue Builtin)
  }

instance Semigroup VariableConstraints where
  x <> y = VariableConstraints
    { lowerBounds = Map.unionWith IMax (lowerBounds x) (lowerBounds y)
    , upperBounds = Map.unionWith IMin (upperBounds x) (upperBounds y)
    }

instance Monoid VariableConstraints where
  mempty = VariableConstraints mempty mempty

type ConstrainedValue = (Maybe VariableConstraints, WHNFValue Builtin)

searchForDomain ::
  MonadDomain m =>
  Quantifier ->
  WHNFValue Builtin ->
  m ConstrainedValue
searchForDomain q expr = case expr of
  ----------------------------------
  -- No useful domain information --
  ----------------------------------
  IBoolLiteral {} -> return (Nothing, expr)
  IExists {} -> return (Nothing, expr)
  IForall {} -> return (Nothing, expr)
  INotEqual {} -> return (Nothing, expr)
  IVectorNotEqualFull {} -> return (Nothing, expr)
  ----------------
  -- Base cases --
  ----------------
  IOrder {} -> tryPurifyAssertion expr recurseDomain _ --(compileRationalAssertion (ordToAssertion op))
  IEqual {} -> tryPurifyAssertion expr recurseDomain (compileRationalAssertion eqToAssertion)
  IVectorEqualFull {} -> tryPurifyAssertion expr recurseDomain (compileTensorAssertion [t1, t2, n, s])
  ---------------------
  -- Recursive cases --
  ---------------------
  INot e -> handleNot e
  IIf _ c x y -> handleIf c x y
  IAnd x y
    | q == Exists -> do
      x' <- searchForDomain q x
      y' <- searchForDomain q y
      case (x', y') of
        (Just (c1, rem1), Just (c2, rem2)) -> return $ Just (c1 <> c2, IAnd rem1 rem2)
        (Just (c1, rem1), Nothing) -> return $ Just (c1, IAnd rem1 y)
        (Nothing, Just (c2, rem2)) -> return $ Just (c2, IAnd x rem2)
        (Nothing, Nothing) -> return Nothing
    | otherwise -> return Nothing
  IOr x y -> orTrivial _ <$> recurseDomain x <*> recurseDomain y
  _ -> unblockBoolExpr expr

handleNot ::
  MonadDomain m =>
  WHNFValue Builtin ->
  m (MaybeTrivial (BooleanExpr (Maybe VariableConstraints)))
handleNot expr = do
  loweredExpr <- lowerNot return expr
  case loweredExpr of
    INot {} -> return $ NonTrivial $ Query $ Left expr
    _ -> recurseDomain expr

handleIf ::
  MonadDomain m =>
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (MaybeTrivial (BooleanExpr (Either (WHNFValue Builtin) VariableConstraints)))
handleIf c x y = recurseDomain =<< unfoldIf c x y

--------------------------------------------------------------------------------
-- Unblocking

tryPurifyAssertion ::
  (MonadDomain m) =>
  WHNFValue Builtin ->
  (WHNFValue Builtin -> WHNFValue Builtin -> m ConstrainedValue) ->
  m ConstrainedValue
tryPurifyAssertion expr whenPure = do
  ctx <- getNamedBoundCtx (Proxy @Builtin)
  result <- runExceptT (Unblocking.tryPurifyAssertion ctx unblockingActions expr _ whenPure)
  case result of
    Left {} -> return (Nothing, expr)
    Right _ -> _

unblockBoolExpr ::
  (MonadDomain m) =>
  WHNFValue Builtin ->
  m ConstrainedValue
unblockBoolExpr value = do
  ctx <- getNamedBoundCtx (Proxy @Builtin)
  result <- runExceptT (Unblocking.unblockBoolExpr ctx unblockingActions value)
  case result of
    Left {} -> return (Nothing, expr)
    Right _ -> recurseDomain _

unblockingActions :: MonadError (WHNFValue Builtin) m => UnblockingActions m
unblockingActions = UnblockingActions
  { unblockFreeVectorVar = unblockFreeVectorVariable
  , unblockBoundVectorVar = unblockBoundVectorVariable
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
  MonadError (WHNFValue Builtin) m =>
  Lv ->
  m (WHNFValue Builtin)
unblockBoundVectorVariable lv = _-}
