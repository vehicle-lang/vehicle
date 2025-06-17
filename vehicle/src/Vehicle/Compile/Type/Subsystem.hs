module Vehicle.Compile.Type.Subsystem
  ( polarityTypeCheck,
    linearityTypeCheck,
    decidabilityTypeCheck,
    resolveInstanceArgumentsAndCasts,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (MonomorphisationSettings (..), monomorphise)
import Vehicle.Compile.Normalise.NBE (NormalisableBuiltin, findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Print.Error (errorInSubsystemMessage)
import Vehicle.Compile.Type (typeCheckProg)
import Vehicle.Compile.Type.Core (InstanceDatabase, emptyInstanceDatabase)
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..))
import Vehicle.Data.Builtin.Decidability.Instances (decidabilityBuiltinInstances)
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface (BuiltinHasListLiterals)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin (..))
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin)
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin)
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard

polarityTypeCheck ::
  (MonadCompile m) =>
  Prog Builtin ->
  m (Either CompileError (Prog PolarityBuiltin))
polarityTypeCheck = typeCheckWithSubsystem PolarityTypes emptyInstanceDatabase

linearityTypeCheck ::
  (MonadCompile m) =>
  Prog Builtin ->
  m (Either CompileError (Prog LinearityBuiltin))
linearityTypeCheck = typeCheckWithSubsystem LinearityTypes emptyInstanceDatabase

decidabilityTypeCheck ::
  (MonadCompile m) =>
  Prog Builtin ->
  m (Prog DecidabilityBuiltin)
decidabilityTypeCheck prog = do
  errorOrDecProg <- typeCheckWithSubsystem DecidabilityTypes decidabilityBuiltinInstances prog
  decProg <- case errorOrDecProg of
    Left err -> throwError $ DevError $ errorInSubsystemMessage "determine the decidability of the program for export to ITP" err
    Right decProg -> return decProg

  monoDecProg <-
    monomorphise decProg $
      MonoSettings
        { isMonomorphisableBinder = \binder -> not (isExplicit binder) || isDeclTypeClassBinder binder,
          keepUnusedDeclaration = isUserCode
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
    result <- runExceptT $ typeCheckProg User instanceCandidates mempty prog
    -- Need to reset the call depth explicitly as type-checking may have errored.
    setCallDepth (callDepth + 1)
    return result

resolveInstanceArgumentsAndCasts ::
  forall m builtin.
  (MonadCompile m, NormalisableBuiltin builtin, BuiltinHasListLiterals builtin, Show builtin) =>
  Prog builtin ->
  m (Prog builtin)
resolveInstanceArgumentsAndCasts prog =
  logCompilerPass MaxDetail "resolution of instance arguments and casts" $ do
    prog' <- flip traverseDecls prog $ \decl -> do
      decl1 <- traverse (traverseBuiltinsM removeBuiltinInstances) decl
      decl2 <- traverse (traverseBuiltinsM removeCasts) decl1
      return decl2
    logDebug MaxDetail $ prettyExternal prog'
    return prog'
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
          Record _ ident fields -> Record p ident (mapRecordFields go fields)
          RecordAcc _ record (ident, FieldName _ name) -> RecordAcc p (go record) (ident, FieldName p name)
