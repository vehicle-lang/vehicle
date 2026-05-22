module Vehicle.Compile.Type.Subsystem
  ( polarityTypeCheck,
    linearityTypeCheck,
    decidabilityTypeCheck,
    resolveInstanceArgumentsAndCasts,
    parseModuleText,
  )
where

import Control.Monad.Except (MonadError (..), runExcept, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency (pruneUnusedDeclarationsKeeping)
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Normalise.NBE (findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Print.Error (errorInSubsystemMessage)
import Vehicle.Compile.Sugar.Desugar (elabModule)
import Vehicle.Compile.Type (typeCheckModuleDecls)
import Vehicle.Compile.Type.Core (InstanceDatabase, emptyInstanceDatabase)
import Vehicle.Compile.Type.Irrelevance
import Vehicle.Compile.Type.System
import Vehicle.Data.AST.Expr.Desugared qualified as S
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..))
import Vehicle.Data.Builtin.Decidability.Instances (decidabilityBuiltinInstances)
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface (BuiltinHasListLiterals)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin (..))
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type (typeBuiltinTypeLevelDeps)
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin)
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin)
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ModuleInterface (ImportedModuleContext, ModuleInterface (..), emptyModuleScopingInterface, emptyModuleTypingInterface)
import Vehicle.Libraries.StandardLibrary (standardLibraryBuiltinModulePath, standardLibraryInstanceOps)
import Vehicle.Syntax.Parse (parseExternalModule)

polarityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  Set Identifier ->
  m (Either CompileError (Prog PolarityBuiltin))
polarityTypeCheck prog declarationsToCompile = do
  let keepUnused = if Set.null declarationsToCompile then isUserCode else (`Set.member` declarationsToCompile)
  monomorphisedProg <- monomorphise prog keepUnused
  irrelevantFreeProg <- removeIrrelevantCodeFromProg monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  instanceFreeProg <- resolveInstanceArgumentsAndCasts implicitFreeProg
  typeCheckWithSubsystem PolarityTypes emptyInstanceDatabase instanceFreeProg

linearityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  Set Identifier ->
  m (Either CompileError (Prog LinearityBuiltin))
linearityTypeCheck prog declarationsToCompile = do
  let keepUnused = if Set.null declarationsToCompile then isUserCode else (`Set.member` declarationsToCompile)
  monomorphisedProg <- monomorphise prog keepUnused
  irrelevantFreeProg <- removeIrrelevantCodeFromProg monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  instanceFreeProg <- resolveInstanceArgumentsAndCasts implicitFreeProg
  typeCheckWithSubsystem LinearityTypes emptyInstanceDatabase instanceFreeProg

decidabilityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  m (Prog DecidabilityBuiltin)
decidabilityTypeCheck prog = do
  prunedProg <- pruneUnusedDeclarationsKeeping (stdlibTypeLevelDepsOf prog) prog
  castFreeProg <- resolveInstanceArgumentsAndCasts prunedProg
  errorOrDecProg <- typeCheckWithSubsystem DecidabilityTypes decidabilityBuiltinInstances castFreeProg
  decProg <- case errorOrDecProg of
    Left err -> developerError $ errorInSubsystemMessage "determine the decidability of the program for export to ITP" err
    Right decProg -> return decProg

  monoDecProg <- monomorphise decProg isUserCode
  resolveInstanceArgumentsAndCasts monoDecProg

-- | All stdlib decls referenced by some `Builtin`'s type signature, for
-- use as extra pruning roots (see `typeBuiltinTypeLevelDeps`).
stdlibTypeLevelDepsOf :: Prog Builtin -> Set Identifier
stdlibTypeLevelDepsOf prog = execWriter (traverse_ go prog)
  where
    go :: Expr Builtin -> Writer (Set Identifier) ()
    go = \case
      Builtin _ (BuiltinFunction f) -> tell (typeBuiltinTypeLevelDeps f)
      App fun args -> do go fun; traverse_ (traverse_ go) args
      Pi _ binder result -> do traverse_ go binder; go result
      Lam _ binder body -> do traverse_ go binder; go body
      Let _ bound binder body -> do go bound; traverse_ go binder; go body
      Record _ _ fields -> traverse_ (go . snd) fields
      RecordProj _ recordType record _ -> do go recordType; go record
      _ -> return ()

typeCheckWithSubsystem ::
  (MonadIO m, MonadCompile m, HasTypeSystem builtin) =>
  SecondaryTypeSystem ->
  InstanceDatabase builtin ->
  Prog Builtin ->
  m (Either CompileError (Prog builtin))
typeCheckWithSubsystem typingSystem instanceCandidates prog = do
  callDepth <- getCallDepth
  logCompilerSection2 MinDetail ("typing using" <+> quotePretty typingSystem <+> "type subsystem") $ do
    logCompilerPass TypingSubsystem $ do
      builtinModuleCtx <- loadTypeSystemBuiltins typingSystem instanceCandidates
      errorOrResult <- runExceptT $ typeCheckModuleDecls userModulePath instanceCandidates builtinModuleCtx (programDeclarations prog)
      -- Need to reset the call depth explicitly as type-checking may have errored.
      setCallDepth (callDepth + 1)
      return $ case errorOrResult of
        Left err -> Left err
        Right (decls, _, _) -> Right $ Main decls

loadTypeSystemBuiltins ::
  (MonadIO m, MonadCompile m, HasTypeSystem builtin) =>
  SecondaryTypeSystem ->
  InstanceDatabase builtin ->
  m (ImportedModuleContext builtin)
loadTypeSystemBuiltins typeSystem _instanceCandidates = do
  -- Locate the builtin module file
  let builtinModulePath = standardLibraryBuiltinModulePath (Just typeSystem)
  -- standardLibraryPath <- getLibraryPath standardLibraryName
  -- let builtinModuleFile = calculateModuleFilePath standardLibraryPath builtinModulePath

  -- Parse the builtin file
  -- builtinModuleText <- readSpecification builtinModuleFile
  -- builtinModule <- parseModuleText (builtinModulePath, builtinModuleFile) builtinModuleText
  -- let builtinModuleDecls = moduleDeclarations builtinModule

  -- Scope and type the builtin file
  -- (scopedDecls, scopingInterface) <- scopeModuleDecls builtinModulePath mempty builtinModuleDecls
  -- (typedDecls, typingInterface, freeEnv) <- typeCheckModuleDecls builtinModulePath instanceCandidates mempty scopedDecls
  let freeEnv = mempty
  let typedDecls = mempty

  -- Add in the builtins
  let finalInterface =
        ModuleInterface
          { scopingInterface = emptyModuleScopingInterface,
            typingInterface = emptyModuleTypingInterface,
            typedModule = Module mempty typedDecls
          }
  return [(builtinModulePath, finalInterface, freeEnv)]

resolveInstanceArgumentsAndCasts ::
  forall m builtin.
  (MonadCompile m, NormalisableBuiltin builtin, BuiltinHasListLiterals builtin, Show builtin) =>
  Prog builtin ->
  m (Prog builtin)
resolveInstanceArgumentsAndCasts prog =
  logCompilerSection2 MaxDetail "resolution of instance arguments and casts" $ do
    prog' <- flip traverseDecls prog $ \decl -> do
      decl1 <- traverse (traverseBuiltinsM removeBuiltinInstances) decl
      decl2 <- traverse (traverseBuiltinsM removeCasts) decl1
      decl3 <- traverse (traverseFreeVarsM (\_b r -> r) removeExternalInstances) decl2
      return decl3
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

    removeExternalInstances :: FreeVarUpdate m builtin
    removeExternalInstances recGo p ident args
      | Set.member ident standardLibraryInstanceOps = do
          args' <- traverseArgs recGo args
          (inst, remainingArgs) <- findInstanceArg ident args'
          case inst of
            Record _ _ fields -> do
              let solution = lookupRecordField fields (FieldName p (nameOf ident))
              -- Replace the provenance of the final solution with the provenance of where the
              -- constraint was generated. This is needed to get the information to propagate
              -- properly for the polarity and linearity types, otherwise the provenance ends
              -- up empty as the candidates are constructed independently.
              let newSolution = replaceProvenance p solution
              let finalValue = normAppList newSolution remainingArgs
              return finalValue
            _ -> developerError "Malformed standard instance argument"
      | otherwise = do
          args' <- traverseArgs recGo args
          return $ normAppList (FreeVar p ident) args'

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
          RecordProj _ recordType record field -> RecordProj p (go recordType) (go record) field

removeImplicitArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
removeImplicitArgs prog =
  logCompilerSection2 MaxDetail "removal of implicit arguments" $ do
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
      Record p ident fields -> Record p ident <$> traverseRecordFields go fields
      RecordProj p recordType record field -> RecordProj p <$> go recordType <*> go record <*> pure field

parseModuleText :: (MonadCompile m) => ParseLocation -> Text -> m (S.Module Builtin)
parseModuleText location txt = do
  case runExcept (readAndParseModule location txt) of
    Left err -> throwError $ ParseError location err
    Right modul -> return modul

readAndParseModule :: (MonadError ParseError m) => ParseLocation -> Text -> m (S.Module Builtin)
readAndParseModule modul txt = castBNFCError (elabModule modul) (parseExternalModule txt)

castBNFCError :: (MonadError ParseError m) => (a -> m b) -> Either String a -> m b
castBNFCError f = \case
  Left err -> throwError $ RawParseError err
  Right value -> f value
