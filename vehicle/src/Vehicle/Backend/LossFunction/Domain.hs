module Vehicle.Backend.LossFunction.Domain where

{-
import Vehicle.Backend.LossFunction.Core (MixedLossBinder)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (FreeEnv, eval)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised

data Domain = Domain
  { lowerBound :: WHNFValue Builtin,
    upperBound :: WHNFValue Builtin
  }

extractQuantifierDomain ::
  (MonadCompile m) =>
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
  (MonadCompile m) =>
  WHNFType Builtin ->
  m (WHNFType Builtin)
extractType = _

extractDomain ::
  (MonadCompile m) =>
  MixedLossBinder ->
  FreeEnv (WHNFClosure Builtin) Builtin ->
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m (Maybe Domain)
extractDomain binder freeEnv boundEnv expr = do
  let finalEnv = extendEnvWithBound _ _ boundEnv
  value <- eval freeEnv finalEnv expr
  _
-}
