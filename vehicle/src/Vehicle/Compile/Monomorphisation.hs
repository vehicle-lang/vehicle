{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Monomorphisation
  ( monomorphise,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State
  ( MonadState (..),
    evalStateT,
    gets,
    modify,
  )
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set (member, unions)
import Data.Text (Text)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin (..))
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Hashing ()
import Vehicle.Libraries.StandardLibrary (standardLibraryInstanceOps)

--------------------------------------------------------------------------------
-- Public interface

-- Example of where you can't expand out literal casts until
-- monomorphisation has been completed.
--
-- n : {{HasNatLiterals t}} -> t
-- n = 1
--
-- t : Rat
-- t = n {{fromNatToRat}}

-- | Tries to monomorphise any polymorphic functions by creating a copy per
-- concrete type each function is used with.
-- Not very sophisticated at the moment, if this needs to be improved perhaps
-- http://mrg.doc.ic.ac.uk/publications/featherweight-go/main.pdf
-- by Wen et al is a good starting point.
monomorphise ::
  forall m builtin.
  (MonadCompile m, Hashable builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  Prog builtin ->
  RootDeclarations ->
  m (Prog builtin)
monomorphise prog rootDecls =
  logCompilerSection2 MinDetail "monomorphisation" $ do
    (prog2, substitutions) <- evalStateT (runWriterT (monomorphiseProg rootDecls prog)) mempty
    result <- runReaderT (replacePreviousApplications prog2) substitutions
    logCompilerPassOutput $ prettyExternal result
    return result

--------------------------------------------------------------------------------
-- Backward pass - collects the sites for monomorphisation

type RootDeclarations = Identifier -> Bool

-- | Applications of monomorphisable functions
type CandidateApplications builtin = Map Identifier (NonEmpty [Arg builtin])

-- | Solution identifier for a candidate monomorphisation application
type SubsitutionSolutions builtin = Map Identifier (Type builtin, HashMap [Arg builtin] Identifier)

type MonadCollect builtin m =
  ( MonadCompile m,
    MonadState (CandidateApplications builtin) m,
    MonadWriter (SubsitutionSolutions builtin) m,
    Hashable builtin,
    PrintableBuiltin builtin,
    NormalisableBuiltin builtin
  )

monomorphiseProg ::
  (MonadCollect builtin m) =>
  RootDeclarations ->
  Prog builtin ->
  m (Prog builtin)
monomorphiseProg rootDecls (Main decls) = do
  logCompilerSection2 MaxDetail "collecting monomorphisation sites" $ do
    monoedDecls <- traverse (monomorphiseDecls rootDecls) (reverse decls)
    return $ Main $ reverse $ concat monoedDecls

monomorphiseDecls ::
  (MonadCollect builtin m) =>
  RootDeclarations ->
  Decl builtin ->
  m [Decl builtin]
monomorphiseDecls rootDecls decl = do
  let ident = identifierOf decl
  logCompilerSection2 MaxDetail (quotePretty ident) $ do
    logDebug MaxDetail $ prettyExternal decl <> line
    newDecls <- monomorphiseDecl decl (rootDecls ident)
    traverse collectReferencesAndResolve newDecls

monomorphiseDecl ::
  (MonadCollect builtin m) =>
  Decl builtin ->
  Bool ->
  m [Decl builtin]
monomorphiseDecl decl isRootDecl =
  logCompilerSection2 MaxDetail "monomorphising based on previous applications" $ do
    let ident = identifierOf decl
    maybeApplications <- gets (Map.lookup ident)
    result <- case maybeApplications of
      Nothing -> handleUnusedDecl decl isRootDecl
      Just apps -> handleUsedDecl apps decl
    modify (Map.delete ident)
    return result

handleUsedDecl ::
  (MonadCollect builtin m) =>
  NonEmpty [Arg builtin] ->
  Decl builtin ->
  m [Decl builtin]
handleUsedDecl applications decl = do
  logDebug MaxDetail $
    "Found applications:"
      <> line
      <> indent 2 (prettyMultiLineList $ NonEmpty.toList (fmap prettyVerbose applications))

  case decl of
    DefFunction p ident anns typ body -> do
      let monomorphisations = calculateMonomorphisations typ applications

      logDebug MaxDetail $
        "Unique monomorphisable applications:"
          <> line
          <> indent 2 (prettyMultiLineList (fmap prettyVerbose monomorphisations))

      let numberOfApplications = length monomorphisations
      let allFreeVarsInArgs = Set.unions (freeVarsIn . argExpr <$> concat monomorphisations)
      let createNewName = numberOfApplications > 1 || ident `Set.member` allFreeVarsInArgs
      traverse (performMonomorphisation (p, ident, anns, typ, body) createNewName) monomorphisations
    _ -> do
      logDebug MaxDetail "Not monomorphising as an abstract declaration"
      return [decl]

handleUnusedDecl ::
  (MonadCollect builtin m) =>
  Decl builtin ->
  Bool ->
  m [Decl builtin]
handleUnusedDecl decl isRootDecl = do
  logDebug MaxDetail $ "No applications of declaration" <+> quotePretty (identifierOf decl) <+> "found."
  if isRootDecl
    then do
      -- Work out if the unused declaration needs to be monomorphised
      let fakeArgs = explicit (Hole mempty "fakeArg") : fakeArgs
      let argsToMono = case decl of
            DefFunction _ _ _ t _ -> fst $ obtainArgsToMonomorphise t fakeArgs
            _ -> mempty
      let needsToBeMonomorphised = not (null argsToMono)

      if needsToBeMonomorphised
        then do
          -- All unused declarations shouldn't have any implicit type-parameters as they
          -- should have been resolved by generalisation at type-checking time (special case).
          developerError $ "Unexpected unused non-monomorphisable decl:" <> lineIndent (prettyExternal decl)
        else do
          logDebug MaxDetail "Keeping declaration"
          return [decl]
    else do
      logDebug MaxDetail "Discarding declaration"
      return []

calculateMonomorphisations ::
  (Eq builtin) =>
  Type builtin ->
  NonEmpty [Arg builtin] ->
  [[Arg builtin]]
calculateMonomorphisations declType allApplications = do
  let calculateMonomorphisation = obtainArgsToMonomorphise declType
  let monomorphisations = fmap (fst . calculateMonomorphisation) allApplications
  -- This is inefficient and not strictly semantically correct.
  -- Semantic equality is difficult however.
  let uniqueMonomorphisations = nub $ NonEmpty.toList monomorphisations
  toList uniqueMonomorphisations

performMonomorphisation ::
  (MonadCollect builtin m) =>
  (Provenance, Identifier, DefFunctionSort, Type builtin, Expr builtin) ->
  Bool ->
  [Arg builtin] ->
  m (Decl builtin)
performMonomorphisation (p, ident, sort, typ, body) createNewName args = do
  newIdent <-
    if createNewName
      then changeName ident <$> getMonomorphisedName (nameOf ident) args
      else return ident
  (newType, newBody) <- substituteArgsThrough (typ, body, args)
  tell (Map.singleton ident (typ, HashMap.singleton args newIdent))
  let newDecl = DefFunction p newIdent sort newType newBody
  logDebug MaxDetail $ "Result:" <> lineIndent (prettyFriendly newDecl)
  return newDecl

substituteArgsThrough ::
  (MonadCollect builtin m) =>
  (Expr builtin, Expr builtin, [Arg builtin]) ->
  m (Expr builtin, Expr builtin)
substituteArgsThrough = \case
  (t, e, []) -> return (t, e)
  (Pi _ _ t, Lam _ _ e, arg : args) -> do
    let expr = argExpr arg
    let t' = expr `substDBInto` t
    let e' = expr `substDBInto` e
    substituteArgsThrough (t', e', args)
  (t, e, args) ->
    developerError $
      "Unexpected type/body of function undergoing monomorphisation"
        <+> line
        <> prettyVerbose t
        <> line
        <> prettyVerbose e
        <> line
        <> prettyVerbose args

collectReferencesAndResolve :: forall builtin m. (MonadCollect builtin m) => Decl builtin -> m (Decl builtin)
collectReferencesAndResolve decl =
  logCompilerSection2 MaxDetail ("collecting internal applications for" <+> quotePretty (identifierOf decl)) $ do
    traverse go decl
  where
    go :: Expr builtin -> m (Expr builtin)
    go expr = logEntryExit expr $ case expr of
      -- Builtins
      Builtin p b -> handleBuiltin p b []
      App (Builtin p b) args -> do
        handleBuiltin p b (NonEmpty.toList args)
      -- Free variables
      FreeVar p ident -> do
        handleFreeVar go p ident mempty
      App (FreeVar p ident) args -> do
        handleFreeVar go p ident (NonEmpty.toList args)
      -- Others
      App fun args -> App <$> go fun <*> traverse (traverse go) args
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body
      Lam p binder body -> Lam p <$> traverse go binder <*> go body
      Record p t fs -> Record p <$> go t <*> traverseRecordFields go fs
      RecordProj p t r field -> RecordProj p <$> go t <*> go r <*> pure field
      Universe p u -> return $ Universe p u
      BoundVar p v -> return $ BoundVar p v
      Hole p n -> return $ Hole p n
      Meta p m -> return $ Meta p m

    handleBuiltin :: BuiltinUpdate m builtin builtin
    handleBuiltin p b args = do
      -- Need to evaluate args before evaluating casts as `stack` won't evaluate otherwise.
      --
      -- Currently need to traverse before expanding type-class ops as previous declarations
      -- may only be used in those arguments, but once forcing is refactored to use expressions
      -- we may be able to remove this.
      args' <- traverse (traverse go) args
      if isTypeClassOp b
        then go =<< expandBuiltinTypeClassOp p b args'
        else do
          case evalCast b args' of
            Just result -> go =<< result
            Nothing -> do
              case isDerivedBuiltin b of
                -- We don't actually want to monorphise derived builtins, but we still want to
                -- keep their definition around. Hence we pass the empty arguments here.
                Just ident -> logFoundApplication ident []
                Nothing -> return ()
              return $ normAppList (Builtin p b) args'

    expandBuiltinTypeClassOp :: Provenance -> builtin -> [Arg builtin] -> m (Expr builtin)
    expandBuiltinTypeClassOp p b args = do
      (inst, remainingArgs) <- findInstanceArg b args
      -- Replace the provenance of the final solution with the provenance of where the
      -- constraint was generated. This is needed to get the information to propagate
      -- properly for the polarity and linearity types, otherwise the provenance ends
      -- up empty as the candidates are constructed independently.
      let newInst = replaceProvenance p inst
      let result = substArgs newInst remainingArgs
      return result

    handleFreeVar :: FreeVarUpdate m builtin
    handleFreeVar recGo p ident args
      | Set.member ident standardLibraryInstanceOps =
          go =<< expandExternalTypeClassOp p ident args
      | otherwise = do
          args' <- traverse (traverse recGo) args
          logFoundApplication ident args'
          return $ normAppList (FreeVar p ident) args'

    expandExternalTypeClassOp :: Provenance -> Identifier -> [Arg builtin] -> m (Expr builtin)
    expandExternalTypeClassOp p ident args = do
      (inst, remainingArgs) <- findInstanceArg ident args
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

    logFoundApplication :: Identifier -> [Arg builtin] -> m ()
    logFoundApplication ident args = do
      logDebug MaxDetail $
        "Found application:"
          <> line
          <> indent
            2
            ( "function: " <+> pretty ident
                <> line
                <> "arguments:" <+> prettyVerbose args
            )
      modify (Map.insert ident [args])

    logEntryExit :: Expr builtin -> m (Expr builtin) -> m (Expr builtin)
    logEntryExit input calcOutput = do
      logDebug MaxDetail $ "collect-enter" <+> prettyVerbose input
      incrCallDepth
      output <- calcOutput
      decrCallDepth
      logDebug MaxDetail $ "collect-exit" <+> prettyVerbose output
      return output

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

--------------------------------------------------------------------------------
-- Forward pass - insert the monorphised identifiers

type MonadInsert builtin m =
  ( MonadCompile m,
    MonadReader (SubsitutionSolutions builtin) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

replacePreviousApplications ::
  forall builtin m.
  (MonadInsert builtin m) =>
  Prog builtin ->
  m (Prog builtin)
replacePreviousApplications prog =
  logCompilerSection2 MaxDetail "applying monomorphisation sites" $ do
    traverse (traverseFreeVarsM (const id) replaceCandidateApplication) prog
  where
    replaceCandidateApplication ::
      (MonadInsert builtin m) =>
      FreeVarUpdate m builtin
    replaceCandidateApplication recGo p ident args = do
      maybeSolution <- asks (Map.lookup ident)
      case maybeSolution of
        Nothing -> do
          args' <- traverseArgs recGo args
          return $ normAppList (FreeVar p ident) args'
        Just (typ, applications) -> do
          logCompilerSection2 MaxDetail "replacing monomorphised application" $ do
            logDebug MaxDetail $ "function: " <+> pretty ident
            logDebug MaxDetail $ "arguments:" <+> prettyVerbose args
            let (argsToMono, remainingArgs) = obtainArgsToMonomorphise typ args
            logDebug MaxDetail $ "arguments-to-mono:" <+> prettyVerbose argsToMono
            logDebug MaxDetail $ "remaining-mono:" <+> prettyVerbose remainingArgs
            case HashMap.lookup argsToMono applications of
              Nothing -> developerError $ "Missing application of" <+> pretty ident
              Just newIdent -> do
                remainingArgs' <- traverse (traverse recGo) remainingArgs
                return $ normAppList (FreeVar p newIdent) remainingArgs'

getMonomorphisedName ::
  (MonadCollect builtin m) =>
  Text ->
  [Arg builtin] ->
  m Text
getMonomorphisedName name args = do
  let nameJoiner = "-"
  let typeJoiner = getTypeJoiner nameJoiner
  let implicits = mapMaybe getImplicitArg args
  let parts = name : fmap getImplicitName implicits
  return $
    Text.replace "\\" "lam" $
      Text.replace " " nameJoiner $
        Text.replace "->" "" $
          Text.intercalate typeJoiner parts

getImplicitName :: (PrintableBuiltin builtin) => Type builtin -> Text
getImplicitName t = layoutAsText $ prettyFriendlyEmptyCtx t

getTypeJoiner :: Text -> Text
getTypeJoiner nameJoiner = nameJoiner <> nameJoiner

--------------------------------------------------------------------------------
-- Utilities

obtainArgsToMonomorphise ::
  forall builtin.
  Type builtin ->
  [Arg builtin] ->
  ([Arg builtin], [Arg builtin])
obtainArgsToMonomorphise typ appArgs =
  fromMaybe ([], appArgs) (go typ appArgs)
  where
    go :: Type builtin -> [Arg builtin] -> Maybe ([Arg builtin], [Arg builtin])
    go t args = case (t, args) of
      (Pi _ binder result, a : as)
        | not (isExplicit binder) ->
            Just $ maybe ([a], as) (first (a :)) (go result as)
      _ -> Nothing
