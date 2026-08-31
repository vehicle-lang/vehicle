module Vehicle.Compile.Type.Subsystem
  ( polarityTypeCheck,
    linearityTypeCheck,
    decidabilityTypeCheck,
    gradientTypeCheck,
    resolveInstanceArgumentsAndCasts,
    parseModuleText,
  )
where

import Control.Monad.Except (MonadError (..), runExcept, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency (pruneUnusedDeclarations)
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Normalise.Core qualified as Forced
import Vehicle.Compile.Normalise.Force (findInstanceArg)
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
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin)
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin)
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Loss (LossBuiltin (..), LossBuiltinConstructor (WithGradients, WithoutGradients), LossBuiltinType (GradientType), LossBuiltinTypeClass (..), LossMode)
import Vehicle.Data.Builtin.Loss qualified as Loss
import Vehicle.Data.Builtin.Loss.Instances (lossBuiltinInstances)
import Vehicle.Data.Builtin.Loss.Type ()
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
  let keepUnused = if Set.null declarationsToCompile then isUserCode else (\d -> identifierOf d `Set.member` declarationsToCompile)
  monomorphisedProg <- monomorphise prog keepUnused
  irrelevantFreeProg <- removeIrrelevantCodeFromProg (const True, const True) monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  instanceFreeProg <- resolveInstanceArgumentsAndCasts implicitFreeProg
  typeCheckWithSubsystem PolarityTypes emptyInstanceDatabase instanceFreeProg

linearityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  Set Identifier ->
  m (Either CompileError (Prog LinearityBuiltin))
linearityTypeCheck prog declarationsToCompile = do
  let keepUnused = if Set.null declarationsToCompile then isUserCode else (\d -> identifierOf d `Set.member` declarationsToCompile)
  monomorphisedProg <- monomorphise prog keepUnused
  irrelevantFreeProg <- removeIrrelevantCodeFromProg (const True, const True) monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  instanceFreeProg <- resolveInstanceArgumentsAndCasts implicitFreeProg
  typeCheckWithSubsystem LinearityTypes emptyInstanceDatabase instanceFreeProg

decidabilityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  m (Prog DecidabilityBuiltin)
decidabilityTypeCheck prog = do
  prunedProg <- pruneUnusedDeclarations isUserCode prog
  errorOrDecProg <- typeCheckWithSubsystem DecidabilityTypes decidabilityBuiltinInstances prunedProg
  decProg <- case errorOrDecProg of
    Left err -> developerError $ errorInSubsystemMessage "determine the decidability of the program for export to ITP" err
    Right decProg -> return decProg

  monoDecProg <- monomorphise decProg isUserCode
  resolveInstanceArgumentsAndCasts monoDecProg

gradientTypeCheck ::
  (MonadCompile m, TypableBuiltin (LossBuiltin mode)) =>
  LossMode ->
  Identifier ->
  Prog Builtin ->
  m (Prog (LossBuiltin mode))
gradientTypeCheck lossMode differentiableLogic prog = do
  errorOrGradProg <- typeCheckWithSubsystem GradientCarryingTypes (lossBuiltinInstances lossMode differentiableLogic) prog
  gradProg <- case errorOrGradProg of
    Left err -> developerError $ errorInSubsystemMessage "determining the parts of the program with gradients for export to a loss function" err
    Right gradProg -> return gradProg

  let isGradientType = \case
        Builtin _ b -> b == LossBuiltinType GradientType
        App (Builtin _ b) _ -> b == LossBuiltinTypeClass MaxGradients
        _ -> False
  let isGradientArg = \case
        Builtin _ (LossBuiltinConstructor c) -> c == WithGradients || c == WithoutGradients
        Builtin _ (Loss.StandardBuiltinConstructor c) -> c == UnitLiteral
        _ -> False

  relevantGradProg <- removeIrrelevantCodeFromProg (isGradientType, isGradientArg) gradProg
  monoDecProg <- monomorphise relevantGradProg isUserCode
  resolveInstanceArgumentsAndCasts monoDecProg

typeCheckWithSubsystem ::
  (MonadCompile m, HasTypeSystem builtin) =>
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
  (MonadCompile m, HasTypeSystem builtin) =>
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
  (MonadCompile m, Forced.NormalisableBuiltin builtin, BuiltinHasListLiterals builtin, Show builtin) =>
  Prog builtin ->
  m (Prog builtin)
resolveInstanceArgumentsAndCasts prog =
  logCompilerSection2 MidDetail "resolution of instance arguments and casts" $ do
    prog' <- flip traverseDecls prog $ \decl -> do
      decl1 <- traverse (traverseBuiltinsM removeBuiltinInstances) decl
      decl2 <- traverse (traverseBuiltinsM removeCasts) decl1
      decl3 <- traverse (traverseFreeVarsM (\_b r -> r) removeExternalInstances) decl2
      return decl3
    logDebug MidDetail $ "Result:" <> lineIndent (prettyExternal prog')
    return prog'
  where
    removeBuiltinInstances :: BuiltinUpdate m builtin builtin
    removeBuiltinInstances p b args
      | Forced.isTypeClassOp b = do
          (inst, remainingArgs) <- findInstanceArg b args
          -- Replace the provenance of the final solution with the provenance of where the
          -- constraint was generated. This is needed to get the information to propagate
          -- properly for the polarity and linearity types, otherwise the provenance ends
          -- up empty as the candidates are constructed independently.
          let newInst = replaceProvenance p inst
          let result = substArgs newInst remainingArgs
          return result
      | otherwise = case Forced.isCast p b of
          Just f -> f args
          Nothing -> return $ normAppList (Builtin p b) args

    removeCasts :: BuiltinUpdate m builtin builtin
    removeCasts p b args = case Forced.isCast p b of
      Just f -> f args
      Nothing -> return $ normAppList (Builtin p b) args

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

removeImplicitArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
removeImplicitArgs prog =
  logCompilerSection2 MidDetail "removal of implicit arguments" $ do
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
