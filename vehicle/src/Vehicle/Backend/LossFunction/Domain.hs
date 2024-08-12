module Vehicle.Backend.LossFunction.Domain where

{-
import Vehicle.Backend.LossFunction.Core (MixedLossBinder)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (FreeEnv, eval)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Expr.Boolean (BooleanExpr (..), MaybeTrivial (..), orTrivial, andTrivial)
import Vehicle.Compile.Boolean.LiftIf (unfoldIf)
import Vehicle.Compile.Boolean.LowerNot (lowerNot)
import Data.Map (Map)
import qualified Data.Map as Map

data Domain = Domain
  { lowerBound :: WHNFValue Builtin,
    upperBound :: WHNFValue Builtin
  }

type MonadDomain m =
  ( MonadCompile m
  )

extractQuantifierDomain ::
  (MonadDomain m) =>
  MixedLossBinder ->
  Lv ->
  WHNFClosure Builtin ->
  m (Maybe (Domain, Expr Ix Builtin))
extractQuantifierDomain binder lv (WHNFClosure env expr) = case expr of
  BuiltinFunc Implies [argExpr -> e1, argExpr -> e2] -> do
    maybeDomain <- extractDomain _ _ env e1
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
    { lowerBounds = Map.unionWith _ (lowerBounds x) (lowerBounds y)
    , upperBounds = Map.unionWith _ (upperBounds x) (upperBounds y)
    }

instance Monoid VariableConstraints where
  mempty = VariableConstraints mempty mempty

searchForDomain ::
  MonadDomain m =>
  Quantifier ->
  WHNFValue Builtin ->
  m (Maybe (VariableConstraints, WHNFValue Builtin))
searchForDomain q value = case value of
  ----------------------------------
  -- No useful domain information --
  ----------------------------------
  IBoolLiteral {} -> return Nothing
  IExists {} -> return Nothing
  IForall {} -> return Nothing
  INotEqual {} -> return Nothing
  IVectorNotEqualFull {} -> return Nothing
  ----------------
  -- Base cases --
  ----------------
  IOrder {} -> _
  IEqual {} -> _
  IVectorEqualFull {} -> _
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
  _ -> recurseDomain =<< unblockBoolExpr expr

recurseDomain ::
  MonadDomain m =>
  WHNFValue Builtin ->
  m (MaybeTrivial (BooleanExpr (Maybe VariableConstraints)))
recurseDomain expr = case expr of
  ---------------------------
  -- Can't compile further --
  ---------------------------
  IBoolLiteral _ b -> return $ Trivial b
  IExists {} -> return $ NonTrivial $ Query $ Left expr
  IForall {} -> return $ NonTrivial $ Query $ Left expr
  ----------------
  -- Base cases --
  ----------------
  IOrder OrderRat op _ _ -> tryPurifyAssertion expr recurseDomain (compileRationalAssertion (ordToAssertion op))
  IEqual EqRat _ _ -> tryPurifyAssertion expr recurseDomain (compileRationalAssertion eqToAssertion)
  IVectorEqualFull (IVecEqSpine t1 t2 n s _ _) -> tryPurifyAssertion expr recurseDomain (compileTensorAssertion [t1, t2, n, s])
  ---------------------
  -- Recursive cases --
  ---------------------
  INotEqual EqRat e1 e2 -> recurseDomain =<< eliminateNotEqualRat e1 e2
  IVectorNotEqualFull spine -> recurseDomain =<< eliminateNotVectorEqual spine
  INot e -> handleNot e
  IIf _ c x y -> handleIf c x y
  IAnd x y -> andTrivial _ <$> recurseDomain x <*> recurseDomain y
  IOr x y -> orTrivial _ <$> recurseDomain x <*> recurseDomain y
  _ -> recurseDomain =<< unblockBoolExpr expr

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
-}
