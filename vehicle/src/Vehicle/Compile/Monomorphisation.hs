{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Monomorphisation
  ( DeclarationFilter,
    monomorphise,
  )
where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State
  ( MonadState (..),
    evalStateT,
    gets,
    modify,
  )
import Control.Monad.Writer.Strict (MonadWriter (..), runWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..), traverse_)
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
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Hashing ()

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
  (MonadCompile m, Hashable builtin, PrintableBuiltin builtin) =>
  Prog builtin ->
  DeclarationFilter builtin ->
  m (Prog builtin)
monomorphise prog rootDecls =
  logCompilerSection2 MinDetail "monomorphisation" $ do
    (prog2, substitutions) <- evalStateT (runWriterT (monomorphiseProg rootDecls prog)) mempty
    result <- runReaderT (replacePreviousApplications prog2) substitutions
    logCompilerPassOutput $ prettyExternal result
    return result

--------------------------------------------------------------------------------
-- Backward pass - collects the sites for monomorphisation

-- | Should a declaration be kept in the program even if it is unused?
type DeclarationFilter builtin = Decl builtin -> Bool

-- | Applications of monomorphisable functions
type CandidateApplications builtin = Map Identifier (NonEmpty [Arg builtin])

-- | Solution identifier for a candidate monomorphisation application
type SubsitutionSolutions builtin = Map Identifier (Type builtin, HashMap [Arg builtin] Identifier)

type MonadCollect builtin m =
  ( MonadCompile m,
    MonadState (CandidateApplications builtin) m,
    MonadWriter (SubsitutionSolutions builtin) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

monomorphiseProg ::
  (MonadCollect builtin m) =>
  DeclarationFilter builtin ->
  Prog builtin ->
  m (Prog builtin)
monomorphiseProg rootDecls (Main decls) = do
  logCompilerSection2 MaxDetail "collecting monomorphisation sites" $ do
    monoedDecls <- traverse (monomorphiseDecls rootDecls) (reverse decls)
    return $ Main $ reverse $ concat monoedDecls

monomorphiseDecls ::
  (MonadCollect builtin m) =>
  DeclarationFilter builtin ->
  Decl builtin ->
  m [Decl builtin]
monomorphiseDecls rootDecls decl = do
  let ident = identifierOf decl
  logCompilerSection2 MaxDetail (quotePretty ident) $ do
    logDebug MaxDetail $ prettyExternal decl <> line
    newDecls <- monomorphiseDecl rootDecls decl
    forM_ newDecls collectReferences
    return newDecls

monomorphiseDecl ::
  (MonadCollect builtin m) =>
  DeclarationFilter builtin ->
  Decl builtin ->
  m [Decl builtin]
monomorphiseDecl rootDecls decl =
  logCompilerSection2 MaxDetail "monomorphising based on previous applications" $ do
    let ident = identifierOf decl
    maybeApplications <- gets (Map.lookup ident)
    result <- case maybeApplications of
      Nothing -> handleUnusedDecl rootDecls decl
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
      monomorphisationResults <- traverse (performMonomorphisation (p, ident, anns, typ, body) createNewName) monomorphisations
      let (newDecls, substitutions) = unzip monomorphisationResults
      tell (Map.singleton ident (typ, HashMap.fromList substitutions))
      return newDecls
    _ -> do
      logDebug MaxDetail "Not monomorphising as an abstract declaration"
      return [decl]

handleUnusedDecl ::
  (MonadCollect builtin m) =>
  DeclarationFilter builtin ->
  Decl builtin ->
  m [Decl builtin]
handleUnusedDecl keepDecl decl = do
  logDebug MaxDetail $ "No applications of declaration" <+> quotePretty (identifierOf decl) <+> "found."

  if keepDecl decl && not (isInstanceDecl decl || isProjectionDecl decl)
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
  m (Decl builtin, ([Arg builtin], Identifier))
performMonomorphisation (p, ident, sort, typ, body) createNewName args = do
  newIdent <-
    if createNewName
      then changeName ident <$> getMonomorphisedName (nameOf ident) args
      else return ident
  (newType, newBody) <- substituteArgsThrough (typ, body, args)
  let newDecl = DefFunction p newIdent sort newType newBody
  logDebug MaxDetail $ "Result:" <> lineIndent (prettyFriendly newDecl)
  return (newDecl, (args, newIdent))

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

collectReferences :: forall builtin m. (MonadCollect builtin m) => Decl builtin -> m ()
collectReferences decl =
  logCompilerSection2 MaxDetail ("collecting internal applications for" <+> quotePretty (identifierOf decl)) $ do
    -- TODO do this in a single traversal
    traverse_ (traverseFreeVarsM (const id) collectReference) decl
    traverse_ (traverseBuiltinsM collectDerivedReference) decl
  where
    collectReference :: FreeVarUpdate m builtin
    collectReference recGo p ident args = do
      args' <- traverse (traverse recGo) args
      foundApplication ident args'
      return $ normAppList (FreeVar p ident) args

    collectDerivedReference :: BuiltinUpdate m builtin builtin
    collectDerivedReference p b args = do
      case isDerivedBuiltin b of
        Just ident -> foundApplication ident args
        Nothing -> return ()
      return $ normAppList (Builtin p b) args

    foundApplication :: Identifier -> [Arg builtin] -> m ()
    foundApplication ident args = do
      logDebug MaxDetail $
        "Found application:"
          <> line
          <> indent
            2
            ( "function: " <+> pretty ident
                <> line
                <> "arguments:" <+> prettyVerbose args
            )
      modify (Map.insertWith (<>) ident (args NonEmpty.:| []))

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
    -- TODO do this in a single traversal
    prog' <- traverse (traverseFreeVarsM (const id) replaceCandidateApplication) prog
    traverse (traverseBuiltinsM replaceDerivedApplication) prog'
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

    replaceDerivedApplication ::
      (MonadInsert builtin m) =>
      BuiltinUpdate m builtin builtin
    replaceDerivedApplication p b args = do
      case isDerivedBuiltin b of
        Nothing -> return $ normAppList (Builtin p b) args
        Just ident -> do
          maybeSolution <- asks (Map.lookup ident)
          case maybeSolution of
            Nothing -> return $ normAppList (Builtin p b) args
            Just (typ, applications) -> do
              logCompilerSection2 MidDetail "replacing monomorphised derived application" $ do
                logDebug MidDetail $ "function: " <+> pretty ident
                logDebug MaxDetail $ "arguments:" <+> prettyVerbose args
                let (argsToMono, remainingArgs) = obtainArgsToMonomorphise typ args
                logDebug MaxDetail $ "arguments-to-mono:" <+> prettyVerbose argsToMono
                logDebug MaxDetail $ "remaining-mono:" <+> prettyVerbose remainingArgs
                case HashMap.lookup argsToMono applications of
                  Nothing -> developerError $ "Missing derived application of" <+> pretty ident
                  Just newIdent -> do
                    return $ normAppList (FreeVar p newIdent) remainingArgs

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
