module Vehicle.Backend.Loss
  ( convertToLossTensors,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), StateT (..), modify)
import Control.Monad.Writer.Strict
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Loss.Domain (findAndAttachQuantifierBounds)
import Vehicle.Backend.Loss.LogicCompilation (findAndLiftLogic, isLogicDecl)
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Dependency (pruneUnusedDeclarations)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem (gradientTypeCheck)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.ForcedValue (isVTypeUniverse)
import Vehicle.Data.Code.Interface (IsArgs (..), SearchRatTensorArgs (..), accessLambda)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, getDeclEntry, isFunctionWhoseReturnType, runFreshFreeContextT, traverseProgDecls)

convertToLossTensors ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Builtin ->
  m (Prog Builtin)
convertToLossTensors logicID prog = do
  -- We first prune the program to remove any unnecessary code in the standard library.
  let keepDecl d = isPropertyDecl d || nameOf d == nameOf logicID
  prunedProg <- pruneUnusedDeclarations keepDecl prog

  -- Next find and compile the logic
  logicResult <- findAndLiftLogic logicID prunedProg
  (logicDecl, rearrangedProg) <- case logicResult of
    Nothing -> missingLogicError prog logicID
    Just result -> return result

  -- Next we go through the program adding the bounds to all the quantifiers
  progWithQuantBounds <- findAndAttachQuantifierBounds rearrangedProg

  -- We then need to prune the program again as some declarations that were used to
  -- declare bounds for the program may be no longer needed. These are removed both
  -- for efficiency and because they may not be monomorphisable after type-checking.
  reprunedProg <- pruneUnusedDeclarations keepDecl progWithQuantBounds

  -- Next we use the gradient type-system to decide which parts of the program should
  -- be translated to loss functions.
  lossProg <- gradientTypeCheck @_ @'Train Train (identifierOf logicDecl) reprunedProg

  -- We then convert back to the original builtins for further processing.
  convertedProg <- convertBackFromLossBuiltins lossProg

  -- Finally we substitute through any derived builtins:
  expandTypeSynonymsAndDerivedBuiltins convertedProg

convertBackFromLossBuiltins ::
  (MonadCompile m) =>
  Prog (LossBuiltin mode) ->
  m (Prog Builtin)
convertBackFromLossBuiltins = traverse $ traverseBuiltinsM $ \p b args -> do
  let gradientOpErr = developerError $ quotePretty b <+> "should not still exist"
  case b of
    -- Should all have been removed by monomorphisation
    LossBuiltinTypeClass {} -> gradientOpErr
    LossBuiltinTypeClassOp {} -> gradientOpErr
    LossBuiltinType {} -> gradientOpErr
    LossBuiltinConstructor {} -> gradientOpErr
    LossBuiltinCast {} -> gradientOpErr
    LossBuiltinFunction f -> case f of
      IfRatTensorWithGradients -> throwError $ UnsupportedIfLossOperation p
    -- Remaining candidates
    StandardBuiltinConstructor c -> return $ normAppList (Builtin p $ BuiltinConstructor c) args
    StandardBuiltinType t -> return $ normAppList (Builtin p $ BuiltinType t) args
    StandardBuiltinFunction f -> case f of
      QuantifyRatTensor Exists -> handleExistsWithoutGradients p args
      _ -> return $ normAppList (Builtin p $ BuiltinFunction f) args
    StandardDerivedFunction f -> return $ normAppList (Builtin p $ DerivedFunction f) args

handleExistsWithoutGradients ::
  (MonadCompile m) =>
  Provenance ->
  [Arg Builtin] ->
  m (Expr Builtin)
handleExistsWithoutGradients p args =
  case getExpr accessSpine args of
    Just SearchRatTensorArgs {..} -> do
      let (binder, _) = accessLambda searchPredicate
      throwError $ QuantifierWithNoGradients p binder
    Nothing -> developerError "Malformed quantifier produced by loss backend"

missingLogicError :: (MonadCompile m) => Prog Builtin -> DifferentiableLogicID -> m a
missingLogicError prog = \case
  BuiltinLogic name -> developerError $ "No logic record found for builtin logic" <+> quotePretty name
  CustomLogic name -> do
    availableLogics <- execWriterT $ traverseProgDecls prog $ \d -> case d of
      DefFunction _ i _ t _ -> do
        whenM (isLogicDecl t) $ do
          lift $ tell [nameOf i]
        return $ Just d
      _ -> return $ Just d
    throwError $ UnknownDifferentiableLogic name availableLogics

-- | Substitutes through the definition so all derived builtins + type synonyms and
-- removes the corresponding declarations from the program.
expandTypeSynonymsAndDerivedBuiltins ::
  (MonadCompile m) =>
  Prog Builtin ->
  m (Prog Builtin)
expandTypeSynonymsAndDerivedBuiltins (Main decls) = do
  let allDerivedIdentifiers = Set.fromList $ fmap identifierOf $ enumerate @DerivedFunction
  (decls', identsRemoved) <-
    runFreshFreeContextT (Proxy @Builtin) $
      runStateT (goDecls allDerivedIdentifiers decls) mempty
  logDebug MidDetail $ "Removed type synonyms and derived functions:" <> lineIndent (pretty $ Set.toList identsRemoved)
  return $ Main decls'
  where
    goDecls ::
      (MonadState (Set Identifier) m, MonadFreeContext Builtin m) =>
      Set Identifier ->
      [Decl Builtin] ->
      m [Decl Builtin]
    goDecls allDerivedIdentifiers = \case
      [] -> return []
      d : ds -> do
        d' <- goDecl allDerivedIdentifiers d
        ds' <- addDeclToContext d $ goDecls allDerivedIdentifiers ds
        return $ maybe ds' (: ds') d'

    goDecl ::
      (MonadState (Set Identifier) m, MonadFreeContext Builtin m) =>
      Set Identifier ->
      Decl Builtin ->
      m (Maybe (Decl Builtin))
    goDecl allDerivedIdentifiers decl
      | identifierOf decl `Set.member` allDerivedIdentifiers = do
          modify (Set.insert (identifierOf decl))
          return Nothing
      | otherwise = do
          isTypeDecl <- isFunctionWhoseReturnType isVTypeUniverse decl
          if isTypeDecl
            then do
              modify (Set.insert (identifierOf decl))
              return Nothing
            else do
              Just <$> traverse (traverseFreeVarsM (const id) go) decl

    -- We have to traverse the free variables as the derived builtins have already
    -- been converted to free variables by monomorphisation. Unsure whether the latter is
    -- the correct strategy or not but rolling with it for the moment.
    go ::
      (MonadState (Set Identifier) m, MonadFreeContext Builtin m) =>
      FreeVarUpdate m Builtin
    go f p ident args = do
      args' <- traverseArgs f args
      identifiersToRemove <- get
      if ident `Set.member` identifiersToRemove
        then do
          result <- getDeclEntry (Proxy @_) ident
          case result of
            DefFunction _ _ _ _ e -> return $ substArgs e args'
            _ -> developerError $ "Unexpected form of derived builtin" <+> pretty ident
        else return $ normAppList (FreeVar p ident) args'
