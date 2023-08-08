{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Monomorphisation (monomorphise) where

import Control.Monad (forM_, (<=<))
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
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Prelude
import Vehicle.Compile.Prelude.MonadContext
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard ()
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Hashing ()
import Vehicle.Libraries.StandardLibrary

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
  (MonadCompile m, Hashable builtin, PrintableBuiltin builtin, HasStandardData builtin) =>
  (Decl Ix builtin -> Bool) ->
  Bool ->
  Text ->
  Prog Ix builtin ->
  m (Prog Ix builtin)
monomorphise keepEvenIfUnused simplifyTypesAndRemoveCoercions nameJoiner prog =
  logCompilerPass MinDetail "monomorphisation" $ do
    progWithNormalisedTypes <-
      if simplifyTypesAndRemoveCoercions
        then runContextT @m @builtin $ normTypeArgsInProg prog
        else return prog
    (prog2, substitutions) <- runReaderT (evalStateT (runWriterT (monomorphiseProg progWithNormalisedTypes)) mempty) (keepEvenIfUnused, nameJoiner)
    prog3 <- runReaderT (insert prog2) substitutions
    result <-
      if simplifyTypesAndRemoveCoercions
        then hoistInferableParameters =<< removeLiteralCoercions nameJoiner prog3
        else return prog3
    logCompilerPassOutput $ prettyFriendly result
    return result

--------------------------------------------------------------------------------
-- Utilities

traverseCandidateApplications ::
  (MonadCompile m) =>
  (Binder Ix builtin -> m (Expr Ix builtin) -> m (Expr Ix builtin)) ->
  (Provenance -> Identifier -> [Arg Ix builtin] -> [Arg Ix builtin] -> m (Expr Ix builtin)) ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
traverseCandidateApplications underBinder processApp =
  traverseFreeVarsM underBinder processApp2
  where
    processApp2 recGo p1 _p2 ident args = do
      let (argsToMono, remainingArgs) = break isExplicit args
      remainingArgs' <- traverse (traverse recGo) remainingArgs
      processApp p1 ident argsToMono remainingArgs'

--------------------------------------------------------------------------------
-- Pass 1 - normalise types in the program

type MonadTypeNormalise builtin m =
  ( MonadContext builtin m,
    NormalisableBuiltin builtin,
    PrintableBuiltin builtin
  )

normTypeArgsPass :: CompilerPass
normTypeArgsPass = "normalisation of type arguments"

normTypeArgsInProg ::
  (MonadTypeNormalise builtin m) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
normTypeArgsInProg (Main decls) =
  logCompilerPass MaxDetail normTypeArgsPass $ do
    Main <$> normTypeArgsInDecls decls

normTypeArgsInDecls ::
  (MonadTypeNormalise builtin m) =>
  [Decl Ix builtin] ->
  m [Decl Ix builtin]
normTypeArgsInDecls [] = return []
normTypeArgsInDecls (decl : decls) = do
  let passDoc = normTypeArgsPass <+> "for" <+> quotePretty (identifierOf decl)
  decl' <- logCompilerPass MaxDetail passDoc $ case decl of
    DefAbstract p s e t -> DefAbstract p s e <$> normTypeArgsInExpr t
    DefFunction p i anns t e -> DefFunction p i anns <$> normTypeArgsInExpr t <*> normTypeArgsInExpr e

  decls' <- addDeclToContext decl' (normTypeArgsInDecls decls)
  return $ decl' : decls'

normTypeArgsInExpr ::
  (MonadTypeNormalise builtin m) =>
  Expr Ix builtin ->
  m (Expr Ix builtin)
normTypeArgsInExpr = traverseCandidateApplications addBinderToContext $
  \p f argsToMono otherArgs -> do
    let normAndQuote arg
          | isInstance arg = return arg
          | otherwise = traverse (unnormalise <=< normalise) arg
    normArgsToMono <- traverse normAndQuote argsToMono
    return $ normAppList p (FreeVar p f) (normArgsToMono <> otherArgs)

--------------------------------------------------------------------------------
-- Pass 2 - collects the sites for monomorphisation

-- | Applications of monomorphisable functions
type CandidateApplications builtin = HashMap Identifier (LinkedHashSet [Arg Ix builtin])

-- | Solution identifier for a candidate monomorphisation application
type SubsitutionSolutions builtin = HashMap (Identifier, [Arg Ix builtin]) Identifier

type MonadCollect builtin m =
  ( MonadCompile m,
    MonadState (CandidateApplications builtin) m,
    MonadWriter (SubsitutionSolutions builtin) m,
    MonadReader (Decl Ix builtin -> Bool, Text) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

monomorphiseProg :: (MonadCollect builtin m) => Prog Ix builtin -> m (Prog Ix builtin)
monomorphiseProg (Main decls) =
  Main . reverse . concat <$> traverse (monomorphiseDecls True) (reverse decls)

monomorphiseDecls :: (MonadCollect builtin m) => Bool -> Decl Ix builtin -> m [Decl Ix builtin]
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

monomorphiseDecl :: (MonadCollect builtin m) => Bool -> Decl Ix builtin -> m [Decl Ix builtin]
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
  (Provenance, Identifier, [Annotation], Type Ix builtin, Expr Ix builtin) ->
  Bool ->
  [Arg Ix builtin] ->
  m (Decl Ix builtin)
performMonomorphisation (p, ident, anns, typ, body) createNewName args = do
  newIdent <-
    if createNewName
      then Identifier (moduleOf ident) <$> getMonomorphisedName (nameOf ident) args
      else return ident
  (newType, newBody) <- substituteArgsThrough (typ, body, args)
  tell (Map.singleton (ident, args) newIdent)
  let newDecl = DefFunction p newIdent anns newType newBody
  logDebug MaxDetail $ prettyFriendly newDecl <> line
  return newDecl

substituteArgsThrough ::
  (MonadCollect builtin m) =>
  (Expr Ix builtin, Expr Ix builtin, [Arg Ix builtin]) ->
  m (Expr Ix builtin, Expr Ix builtin)
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

collectReferences :: (MonadCollect builtin m) => Expr Ix builtin -> m ()
collectReferences expr = do
  _ <- traverseCandidateApplications (const id) collectApplication expr
  return ()

collectApplication ::
  (MonadCollect builtin m) =>
  Provenance ->
  Identifier ->
  [Arg Ix builtin] ->
  [Arg Ix builtin] ->
  m (Expr Ix builtin)
collectApplication p ident argsToMono remainingArgs = do
  logDebug MaxDetail $ "Found application:" <+> quotePretty ident <+> prettyVerbose argsToMono
  modify (Map.insertWith HashSet.union ident (HashSet.singleton argsToMono))
  return $ normAppList p (FreeVar p ident) (argsToMono <> remainingArgs)

--------------------------------------------------------------------------------
-- Pass 3 - insert the monorphised identifiers

type MonadInsert builtin m =
  ( MonadCompile m,
    MonadReader (SubsitutionSolutions builtin) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

insert :: (MonadInsert builtin m) => Prog Ix builtin -> m (Prog Ix builtin)
insert = traverse (traverseCandidateApplications (const id) replaceCandidateApplication)

replaceCandidateApplication ::
  (MonadInsert builtin m) =>
  Provenance ->
  Identifier ->
  [Arg Ix builtin] ->
  [Arg Ix builtin] ->
  m (Expr Ix builtin)
replaceCandidateApplication p ident monoArgs remainingArgs = do
  solution <- asks (Map.lookup (ident, monoArgs))
  case solution of
    Nothing -> return $ normAppList p (FreeVar p ident) (monoArgs <> remainingArgs)
    Just replacementIdent -> return $ normAppList p (FreeVar p replacementIdent) remainingArgs

getMonomorphisedName ::
  (MonadCollect builtin m) =>
  Text ->
  [Arg Ix builtin] ->
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

getImplicitName :: (PrintableBuiltin builtin) => Type Ix builtin -> Text
getImplicitName t = layoutAsText $ prettyFriendly $ WithContext t emptyDBCtx

getTypeJoiner :: Text -> Text
getTypeJoiner nameJoiner = nameJoiner <> nameJoiner

--------------------------------------------------------------------------------
-- Step 4. Coercions

removeLiteralCoercions ::
  forall m builtin.
  (MonadCompile m, HasStandardData builtin, PrintableBuiltin builtin) =>
  Text ->
  Prog Ix builtin ->
  m (Prog Ix builtin)
removeLiteralCoercions nameJoiner (Main ds) =
  Main . catMaybes <$> traverse goDecl ds
  where
    goDecl :: Decl Ix builtin -> m (Maybe (Decl Ix builtin))
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
      let shortIdent = Identifier (moduleOf ident) $ fst $ Text.breakOn typeJoiner (nameOf ident)
      findStdLibFunction shortIdent

    updateBuiltin :: Decl Ix builtin -> BuiltinUpdate m Ix builtin builtin
    updateBuiltin decl p1 p2 b args = case b of
      (getBuiltinFunction -> Just (FromNat dom)) -> case (dom, filter isExplicit args) of
        (FromNatToIndex, [RelevantExplicitArg _ (NatLiteral p n)]) -> return $ IndexLiteral p n
        (FromNatToNat, [e]) -> return $ argExpr e
        (FromNatToInt, [RelevantExplicitArg _ (NatLiteral p n)]) -> return $ IntLiteral p n
        (FromNatToRat, [RelevantExplicitArg _ (NatLiteral p n)]) -> return $ RatLiteral p (fromIntegral n)
        _ -> do
          partialApplication decl (pretty (FromNat dom)) args
      (getBuiltinFunction -> Just (FromRat dom)) -> case (dom, args) of
        (FromRatToRat, [e]) -> return $ argExpr e
        _ -> partialApplication decl (pretty (FromRat dom)) args
      _ -> return $ normAppList p1 (Builtin p2 b) args

    updateFreeVar :: Decl Ix builtin -> FreeVarUpdate m Ix builtin
    updateFreeVar decl recGo p1 p2 ident args = do
      let vectorCoercion = getVectorCoercion ident
      args' <- traverse (traverse recGo) args
      case vectorCoercion of
        Just StdVectorToVector -> case reverse args' of
          vec : _ -> return $ argExpr vec
          _ -> partialApplication decl (pretty ident) args'
        Just StdVectorToList -> case reverse args' of
          RelevantExplicitArg _ (VecLiteral p l xs) : _ -> return $ mkList p l (fmap argExpr xs)
          _ -> partialApplication decl (pretty ident) args'
        _ -> return $ normAppList p1 (FreeVar p2 ident) args'

    partialApplication :: Decl Ix builtin -> Doc () -> [Arg Ix builtin] -> m b
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

hoistInferableParameters :: (MonadCompile m) => Prog Ix builtin -> m (Prog Ix builtin)
hoistInferableParameters (Main ds) = do
  (otherDecls, inferableParameters) <- runWriterT (goDecls ds)
  return $ Main (inferableParameters <> otherDecls)
  where
    goDecls :: (MonadWriter [Decl Ix builtin] m) => [Decl Ix builtin] -> m [Decl Ix builtin]
    goDecls [] = return []
    goDecls (decl : decls) = do
      decls' <- goDecls decls
      case decl of
        DefAbstract _ _ (ParameterDef Inferable) _ -> do
          tell [decl]
          return decls'
        _ -> return $ decl : decls'
