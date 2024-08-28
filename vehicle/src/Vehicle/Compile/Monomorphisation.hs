{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Monomorphisation
  ( monomorphise,
    hoistInferableParameters,
    removeLiteralCoercions,
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
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
  ( delete,
    insertWith,
    lookup,
    member,
    singleton,
  )
import Data.Hashable (Hashable)
import Data.LinkedHashSet (LinkedHashSet)
import Data.LinkedHashSet qualified as HashSet (singleton, toList, union)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set (member, unions)
import Data.Text (Text)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Hashing ()
import Vehicle.Libraries.StandardLibrary.Definitions

--------------------------------------------------------------------------------
-- Public interface

-- | Tries to monomorphise any polymorphic functions by creating a copy per
-- concrete type each function is used with.
-- Not very sophisticated at the moment, if this needs to be improved perhaps
-- http://mrg.doc.ic.ac.uk/publications/featherweight-go/main.pdf
-- by Wen et al is a good starting point.
--
-- It also gets rid of automatically inserted coercions of literals
-- (e.g. naturals, rationals and tensors)
monomorphise ::
  forall m builtin.
  (MonadCompile m, Hashable builtin, PrintableBuiltin builtin, BuiltinHasStandardData builtin) =>
  (Decl builtin -> Bool) ->
  Text ->
  Prog builtin ->
  m (Prog builtin)
monomorphise keepEvenIfUnused nameJoiner prog =
  logCompilerPass MinDetail "monomorphisation" $ do
    (prog2, substitutions) <- runReaderT (evalStateT (runWriterT (monomorphiseProg prog)) mempty) (keepEvenIfUnused, nameJoiner)
    result <- runReaderT (insert prog2) substitutions
    logCompilerPassOutput $ prettyFriendly result
    return result

--------------------------------------------------------------------------------
-- Utilities

traverseCandidateApplications ::
  (MonadCompile m) =>
  (Binder builtin -> m (Expr builtin) -> m (Expr builtin)) ->
  (Provenance -> Identifier -> [Arg builtin] -> [Arg builtin] -> m (Expr builtin)) ->
  Expr builtin ->
  m (Expr builtin)
traverseCandidateApplications underBinder processApp =
  traverseFreeVarsM underBinder processApp2
  where
    processApp2 recGo p ident args = do
      let (argsToMono, remainingArgs) = break isExplicit args
      remainingArgs' <- traverse (traverse recGo) remainingArgs
      processApp p ident argsToMono remainingArgs'

--------------------------------------------------------------------------------
-- Pass 2 - collects the sites for monomorphisation

-- | Applications of monomorphisable functions
type CandidateApplications builtin = HashMap Identifier (LinkedHashSet [Arg builtin])

-- | Solution identifier for a candidate monomorphisation application
type SubsitutionSolutions builtin = HashMap (Identifier, [Arg builtin]) Identifier

type MonadCollect builtin m =
  ( MonadCompile m,
    MonadState (CandidateApplications builtin) m,
    MonadWriter (SubsitutionSolutions builtin) m,
    MonadReader (Decl builtin -> Bool, Text) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

monomorphiseProg :: (MonadCollect builtin m) => Prog builtin -> m (Prog builtin)
monomorphiseProg (Main decls) =
  Main . reverse . concat <$> traverse (monomorphiseDecls True) (reverse decls)

monomorphiseDecls :: (MonadCollect builtin m) => Bool -> Decl builtin -> m [Decl builtin]
monomorphiseDecls top decl = do
  let ident = identifierOf decl
  logCompilerSection MaxDetail ("Checking" <+> quotePretty ident) $ do
    newDecls <- monomorphiseDecl top decl
    forM_ newDecls (traverse collectReferences)
    recursiveReferences <- gets (Map.member ident)
    resursiveDecls <-
      if recursiveReferences
        then monomorphiseDecls False decl
        else return []
    return (newDecls <> resursiveDecls)

monomorphiseDecl :: (MonadCollect builtin m) => Bool -> Decl builtin -> m [Decl builtin]
monomorphiseDecl top decl = do
  logDebug MaxDetail $ prettyVerbose decl
  let ident = identifierOf decl
  freeVarApplications <- get
  modify (Map.delete ident)
  case decl of
    DefAbstract {} -> return [decl]
    DefFunction p _ anns t e -> do
      (keepEvenIfUnused, _) <- ask
      case Map.lookup ident freeVarApplications of
        Nothing -> do
          logDebug MaxDetail $ "No applications of" <+> quotePretty ident <+> "found."
          if keepEvenIfUnused decl
            then do
              logDebug MaxDetail "Keeping declaration"
              return [decl]
            else do
              logDebug MaxDetail "Discarding declaration"
              return []
        Just applications -> do
          let applicationList = HashSet.toList applications
          let numberOfApplications = length applicationList
          let allFreeVarsInArgs = Set.unions (freeVarsIn . argExpr <$> concat applicationList)
          let createNewName = numberOfApplications > 1 || not top || ident `Set.member` allFreeVarsInArgs
          logDebug MaxDetail $ "Found" <+> pretty numberOfApplications <+> "type-unique application(s):"
          logDebug MaxDetail $ indent 2 $ prettyVerbose applicationList <> line
          traverse (performMonomorphisation (p, ident, anns, t, e) createNewName) applicationList

performMonomorphisation ::
  (MonadCollect builtin m) =>
  (Provenance, Identifier, [Annotation], Type builtin, Expr builtin) ->
  Bool ->
  [Arg builtin] ->
  m (Decl builtin)
performMonomorphisation (p, ident, anns, typ, body) createNewName args = do
  newIdent <-
    if createNewName
      then changeName ident <$> getMonomorphisedName (nameOf ident) args
      else return ident
  (newType, newBody) <- substituteArgsThrough (typ, body, args)
  tell (Map.singleton (ident, args) newIdent)
  let newDecl = DefFunction p newIdent anns newType newBody
  logDebug MaxDetail $ prettyFriendly newDecl <> line
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

collectReferences :: (MonadCollect builtin m) => Expr builtin -> m ()
collectReferences expr = do
  _ <- traverseCandidateApplications (const id) collectApplication expr
  return ()

collectApplication ::
  (MonadCollect builtin m) =>
  Provenance ->
  Identifier ->
  [Arg builtin] ->
  [Arg builtin] ->
  m (Expr builtin)
collectApplication p ident argsToMono remainingArgs = do
  logDebug MaxDetail $ "Found application:" <+> quotePretty ident <+> prettyVerbose argsToMono
  modify (Map.insertWith HashSet.union ident (HashSet.singleton argsToMono))
  return $ normAppList (FreeVar p ident) (argsToMono <> remainingArgs)

--------------------------------------------------------------------------------
-- Pass 3 - insert the monorphised identifiers

type MonadInsert builtin m =
  ( MonadCompile m,
    MonadReader (SubsitutionSolutions builtin) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

insert :: (MonadInsert builtin m) => Prog builtin -> m (Prog builtin)
insert = traverse (traverseCandidateApplications (const id) replaceCandidateApplication)

replaceCandidateApplication ::
  (MonadInsert builtin m) =>
  Provenance ->
  Identifier ->
  [Arg builtin] ->
  [Arg builtin] ->
  m (Expr builtin)
replaceCandidateApplication p ident monoArgs remainingArgs = do
  solution <- asks (Map.lookup (ident, monoArgs))
  case solution of
    Nothing -> return $ normAppList (FreeVar p ident) (monoArgs <> remainingArgs)
    Just replacementIdent -> return $ normAppList (FreeVar p replacementIdent) remainingArgs

getMonomorphisedName ::
  (MonadCollect builtin m) =>
  Text ->
  [Arg builtin] ->
  m Text
getMonomorphisedName name args = do
  (_, nameJoiner) <- ask
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
-- Step 4. Coercions

removeLiteralCoercions ::
  forall m.
  (MonadCompile m) =>
  Text ->
  Prog Builtin ->
  m (Prog Builtin)
removeLiteralCoercions nameJoiner (Main ds) =
  Main . catMaybes <$> traverse goDecl ds
  where
    goDecl :: Decl Builtin -> m (Maybe (Decl Builtin))
    goDecl decl = case getVectorCoercion (identifierOf decl) of
      Just StdVectorToVector -> return Nothing
      Just StdVectorToList -> return Nothing
      _ ->
        Just
          <$> traverse
            ( \e -> do
                e' <- traverseBuiltinsM (updateBuiltin decl) e
                traverseFreeVarsM (const id) (updateFreeVar decl) e'
            )
            decl

    getVectorCoercion :: Identifier -> Maybe StdLibFunction
    getVectorCoercion ident = do
      let typeJoiner = getTypeJoiner nameJoiner
      let shortIdent = changeName ident $ fst $ Text.breakOn typeJoiner (nameOf ident)
      findStdLibFunction shortIdent

    updateBuiltin :: Decl Builtin -> BuiltinUpdate m Builtin Builtin
    updateBuiltin decl p2 b args = case b of
      (getBuiltinFunction -> Just (FromNat dom)) -> case (dom, filter isExplicit args) of
        (FromNatToIndex, [RelevantExplicitArg _ (INatLiteral p n)]) -> return $ IIndexLiteral p n
        (FromNatToNat, [e]) -> return $ argExpr e
        (FromNatToRat, [RelevantExplicitArg _ (INatLiteral p n)]) -> return $ IRatLiteral p (fromIntegral n)
        _ -> do
          partialApplication decl (pretty (FromNat dom)) args
      (getBuiltinFunction -> Just (FromRat dom)) -> case (dom, args) of
        (FromRatToRat, [e]) -> return $ argExpr e
        _ -> partialApplication decl (pretty (FromRat dom)) args
      _ -> return $ normAppList (Builtin p2 b) args

    updateFreeVar :: Decl Builtin -> FreeVarUpdate m Builtin
    updateFreeVar decl recGo p ident args = do
      let vectorCoercion = getVectorCoercion ident
      args' <- traverse (traverse recGo) args
      case vectorCoercion of
        Just StdVectorToVector -> case reverse args' of
          vec : _ -> return $ argExpr vec
          _ -> partialApplication decl (pretty ident) args'
        Just StdVectorToList -> case reverse args' of
          RelevantExplicitArg _ (IVecLiteral l xs) : _ -> return $ mkListExpr (argExpr l) (fmap argExpr xs)
          _ -> partialApplication decl (pretty ident) args'
        _ -> return $ normAppList (FreeVar p ident) args'

    partialApplication :: Decl Builtin -> Doc () -> [Arg Builtin] -> m b
    partialApplication decl v args =
      compilerDeveloperError $
        "Found partially applied"
          <+> squotes v
          <+> "@"
          <+> prettyVerbose args
          <+> "in"
          <> line
          <> indent
            2
            ( prettyFriendly decl
                <> line
                <> line
                <> prettyVerbose decl
                <> line
                <> line
                <> pretty (show $ bodyOf decl)
            )

--------------------------------------------------------------------------------
-- Step 5. Hoisting. Massive hack. Should be done with erasure.

hoistInferableParameters :: (MonadCompile m) => Prog builtin -> m (Prog builtin)
hoistInferableParameters (Main ds) = do
  (otherDecls, inferableParameters) <- runWriterT (goDecls ds)
  return $ Main (inferableParameters <> otherDecls)
  where
    goDecls :: (MonadWriter [Decl builtin] m) => [Decl builtin] -> m [Decl builtin]
    goDecls [] = return []
    goDecls (decl : decls) = do
      decls' <- goDecls decls
      case decl of
        DefAbstract _ _ (ParameterDef Inferable) _ -> do
          tell [decl]
          return decls'
        _ -> return $ decl : decls'
