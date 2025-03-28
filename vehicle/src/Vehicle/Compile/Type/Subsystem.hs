module Vehicle.Compile.Type.Subsystem
  ( polarityTypeCheck,
    linearityTypeCheck,
    decidabilityTypeCheck,
    resolveInstanceArgumentsAndCasts,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (MonomorphisationSettings (..), monomorphise)
import Vehicle.Compile.Normalise.NBE (NormalisableBuiltin, findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Type (typeCheckProg)
import Vehicle.Compile.Type.Core (InstanceDatabase, emptyInstanceDatabase)
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCodeFromProg)
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..))
import Vehicle.Data.Builtin.Decidability.Instances (decidabilityBuiltinInstances)
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface (BuiltinHasListLiterals)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin (..))
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin)
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin)
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface

polarityTypeCheck :: (MonadCompile m) => Prog Builtin -> m (Either CompileError (Prog PolarityBuiltin))
polarityTypeCheck prog = do
  preprocessedProg <- simplifyTypes prog
  typeCheckWithSubsystem PolarityTypes emptyInstanceDatabase preprocessedProg

linearityTypeCheck :: (MonadCompile m) => Prog Builtin -> m (Either CompileError (Prog LinearityBuiltin))
linearityTypeCheck prog = do
  preprocessedProg <- simplifyTypes prog
  typeCheckWithSubsystem LinearityTypes emptyInstanceDatabase preprocessedProg

decidabilityTypeCheck :: (MonadCompile m) => Prog Builtin -> m (Prog DecidabilityBuiltin)
decidabilityTypeCheck prog = do
  instanceFreeProg <- resolveInstanceArgumentsAndCasts prog
  -- For aesthetic reasons evaluate all `appendLists` that result from the `HasTensorTC` type-class resolution mechanism
  appendListFreeProg <- resolveAppendLists instanceFreeProg
  monoProg <-
    monomorphise appendListFreeProg $
      MonoSettings
        { isMonomorphisableBinder = not . isExplicit
        }
  instanceFreeProg2 <- resolveInstanceArgumentsAndCasts monoProg

  -- Type-check with subsystem.
  errorOrDecProg <- typeCheckWithSubsystem DecidabilityTypes decidabilityBuiltinInstances instanceFreeProg2
  decProg <- case errorOrDecProg of
    Left err -> throwError err
    Right decProg -> return decProg

  monoDecProg <-
    monomorphise decProg $
      MonoSettings
        { isMonomorphisableBinder = \binder -> not (isExplicit binder) || isDeclTypeClassBinder binder
        }
  instanceFreeDecProg <- resolveInstanceArgumentsAndCasts monoDecProg
  return instanceFreeDecProg
  where
    isDeclTypeClassBinder :: Binder DecidabilityBuiltin -> Bool
    isDeclTypeClassBinder binder = case typeOf binder of
      App (Builtin _ (DecidabilityBuiltinTypeClass {})) _ -> True
      _ -> False

typeCheckWithSubsystem ::
  forall builtin m.
  (HasTypeSystem builtin, NormalisableBuiltin builtin, BuiltinHasListLiterals builtin, MonadCompile m) =>
  SecondaryTypeSystem ->
  InstanceDatabase builtin ->
  Prog Builtin ->
  m (Either CompileError (Prog builtin))
typeCheckWithSubsystem typingSystem instanceCandidates prog = do
  callDepth <- getCallDepth
  logCompilerPass MinDetail ("typing using" <+> quotePretty typingSystem <+> "type subsystem") $ do
    result <- runExceptT $ typeCheckProg instanceCandidates mempty prog
    -- Need to reset the call depth explicitly as type-checking may have errored.
    setCallDepth (callDepth + 1)
    return result

simplifyTypes ::
  (MonadCompile m) =>
  Prog Builtin ->
  m (Prog Builtin)
simplifyTypes prog = do
  irrelevantFreeProg <- removeIrrelevantCodeFromProg prog
  monomorphisedProg <-
    monomorphise irrelevantFreeProg $
      MonoSettings
        { isMonomorphisableBinder = not . isExplicit
        }
  implicitFreeProg <- removeImplicitArgs monomorphisedProg
  return implicitFreeProg

resolveInstanceArgumentsAndCasts ::
  forall m builtin.
  (MonadCompile m, NormalisableBuiltin builtin, BuiltinHasListLiterals builtin, Show builtin) =>
  Prog builtin ->
  m (Prog builtin)
resolveInstanceArgumentsAndCasts prog =
  logCompilerPass MaxDetail "resolution of instance arguments and casts" $ do
    flip traverseDecls prog $ \decl -> do
      logDebug MaxDetail $ prettyExternal decl
      decl1 <- traverse (traverseBuiltinsM removeBuiltinInstances) decl
      logDebug MaxDetail $ prettyExternal decl1
      decl2 <- traverse (traverseBuiltinsM removeCasts) decl1
      return decl2
  where
    removeBuiltinInstances :: BuiltinUpdate m builtin builtin
    removeBuiltinInstances p b args
      | isTypeClassOp b = do
          (inst, remainingArgs) <- findInstanceArg b args
          -- Replace the provenance of the final solution with the provenance of where the
          -- constraint was generated. This is needed to get the information to propagate
          -- properly for the polarity and linearity types, otherwise the provenance ends
          -- up empty as the candidates are constructed independently.
          let newInst = replaceProvenance p inst
          let result = substArgs newInst remainingArgs
          return result
      | otherwise = return $ normAppList (Builtin p b) args

    removeCasts :: BuiltinUpdate m builtin builtin
    removeCasts p b args = case isCast p b of
      Just f -> f args
      Nothing -> return $ normAppList (Builtin p b) args

    replaceProvenance :: Provenance -> Expr builtin -> Expr builtin
    replaceProvenance p = go
      where
        go :: Expr builtin -> Expr builtin
        go = \case
          Meta _p m -> Meta p m
          App fun args -> App (go fun) (fmap (fmap go) args)
          Universe _ u -> Universe p u
          Hole _ h -> Hole p h
          Builtin _ b -> Builtin p b
          FreeVar _ v -> FreeVar p v
          BoundVar _ v -> BoundVar p v
          Pi _ binder res -> Pi p (fmap go binder) (go res)
          Let _ e1 binder e2 -> Let p (go e1) (fmap go binder) (go e2)
          Lam _ binder e -> Lam p (fmap go binder) (go e)

resolveAppendLists :: forall m. (MonadCompile m) => Prog Builtin -> m (Prog Builtin)
resolveAppendLists = traverse (traverseBuiltinsM evalAppend)
  where
    evalAppend :: Provenance -> Builtin -> [Arg Builtin] -> m (Expr Builtin)
    evalAppend p b args = case b of
      DerivedFunction AppendList -> return $ evalAppendList p args
      _ -> return $ normAppList (Builtin p b) args

    evalAppendList :: Provenance -> [Arg Builtin] -> Expr Builtin
    evalAppendList p = \case
      args@[t, xs, ys] -> case argExpr xs of
        INil _ -> argExpr ys
        ICons _ v vs -> ICons t v (evalAppendList p [t, explicit vs, ys])
        _ -> normAppList (Builtin p (DerivedFunction AppendList)) args
      _ -> developerError "malformed append list!"

removeImplicitArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
removeImplicitArgs prog =
  logCompilerPass MaxDetail "removal of implicit arguments" $ do
    result <- traverse go prog
    logCompilerPassOutput $ prettyExternal result
    return result
  where
    go :: Expr builtin -> m (Expr builtin)
    go expr = case expr of
      App fun args -> do
        fun' <- go fun
        let nonImplicitArgs = NonEmpty.filter (not . isImplicit) args
        nonImplicitArgs' <- traverse (traverse go) nonImplicitArgs
        return $ normAppList fun' nonImplicitArgs'
      BoundVar {} -> return expr
      FreeVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body -> Lam p <$> traverse go binder <*> go body
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body
