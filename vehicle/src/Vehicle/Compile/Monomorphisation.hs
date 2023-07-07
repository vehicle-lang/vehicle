{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Monomorphisation (monomorphise) where

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
    insert,
    lookup,
    member,
    singleton,
  )
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set (singleton, size, toList)
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NonEmpty (span)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin, prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard ()
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Hashing ()

--------------------------------------------------------------------------------
-- Public interface

-- | Tries to monomorphise any polymorphic functions by creating a copy per
-- concrete type each function is used with.
-- Not very sophisticated at the moment, if this needs to be improved perhaps
-- http://mrg.doc.ic.ac.uk/publications/featherweight-go/main.pdf
-- by Wen et al is a good starting point.
monomorphise ::
  (MonadCompile m, Eq builtin, Hashable builtin, PrintableBuiltin builtin) =>
  (Decl Ix builtin -> Bool) ->
  Prog Ix builtin ->
  m (Prog Ix builtin)
monomorphise keepEvenIfUnused prog = logCompilerPass MinDetail "monomorphisation" $ do
  (prog2, substitutions) <- runReaderT (evalStateT (runWriterT (monomorphiseProg prog)) mempty) keepEvenIfUnused
  result <- runReaderT (insert prog2) substitutions
  logCompilerPassOutput $ prettyFriendly result
  return result

--------------------------------------------------------------------------------
-- Definitions and utilites

-- | Applications of monomorphisable functions
type CandidateApplications builtin = HashMap Identifier (HashSet [Arg Ix builtin])

-- | Solution identifier for a candidate monomorphisation application
type SubsitutionSolutions builtin = HashMap (Identifier, [Arg Ix builtin]) Identifier

traverseCandidateApplications ::
  (MonadCompile m) =>
  (Provenance -> Identifier -> [Arg Ix builtin] -> [Arg Ix builtin] -> m (Expr Ix builtin)) ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
traverseCandidateApplications processApp = go
  where
    go expr = case expr of
      FreeVar p ident ->
        processApp p ident mempty mempty
      App p (FreeVar _ ident) args -> do
        args' <- traverse (traverse go) args
        let (argsToMono, remainingArgs) = NonEmpty.span (not . isExplicit) args'
        processApp p ident argsToMono remainingArgs
      App p fun args -> do
        fun' <- go fun
        args' <- traverse (traverse go) args
        return $ App p fun' args'
      BoundVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Ann p e t -> Ann p <$> go e <*> go t
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body -> Lam p <$> traverse go binder <*> go body
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body

--------------------------------------------------------------------------------
-- Initial pass - collects the sites for monomorphisation

type MonadCollect builtin m =
  ( MonadCompile m,
    MonadState (CandidateApplications builtin) m,
    MonadWriter (SubsitutionSolutions builtin) m,
    MonadReader (Decl Ix builtin -> Bool) m,
    Hashable builtin,
    PrintableBuiltin builtin
  )

monomorphiseProg :: (MonadCollect builtin m) => Prog Ix builtin -> m (Prog Ix builtin)
monomorphiseProg (Main decls) =
  Main . reverse . concat <$> traverse monomorphiseDecls (reverse decls)

monomorphiseDecls :: (MonadCollect builtin m) => Decl Ix builtin -> m [Decl Ix builtin]
monomorphiseDecls decl = do
  let ident = identifierOf decl
  logCompilerSection MaxDetail ("Checking" <+> quotePretty ident) $ do
    newDecls <- monomorphiseDecl decl
    forM_ newDecls (traverse collectReferences)
    recursiveReferences <- gets (Map.member ident)
    resursiveDecls <-
      if recursiveReferences
        then monomorphiseDecls decl
        else return []
    return (newDecls <> resursiveDecls)

monomorphiseDecl :: (MonadCollect builtin m) => Decl Ix builtin -> m [Decl Ix builtin]
monomorphiseDecl decl = do
  let ident = identifierOf decl
  freeVarApplications <- get
  modify (Map.delete ident)
  case decl of
    DefAbstract {} -> return [decl]
    DefFunction p _ anns t e -> do
      keepEvenIfUnused <- ask
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
          let numberOfApplications = Set.size applications
          let createNewName = numberOfApplications > 1
          logDebug MaxDetail $ "Found" <+> pretty numberOfApplications <+> "type-unique applications:"
          logDebug MaxDetail $ indent 2 $ prettyVerbose (Set.toList applications)
          traverse (performMonomorphisation (p, ident, anns, t, e) createNewName) (Set.toList applications)

performMonomorphisation ::
  (MonadCollect builtin m) =>
  (Provenance, Identifier, [Annotation], Type Ix builtin, Expr Ix builtin) ->
  Bool ->
  [Arg Ix builtin] ->
  m (Decl Ix builtin)
performMonomorphisation (p, ident, anns, typ, body) createNewName args = do
  let newIdent
        | createNewName = Identifier (moduleOf ident) $ nameOf ident <> getMonomorphisedSuffix args
        | otherwise = ident
  (newType, newBody) <- substituteArgsThrough (typ, body, args)
  tell (Map.singleton (ident, args) newIdent)
  let newDecl = DefFunction p newIdent anns newType newBody
  logDebug MaxDetail $ prettyFriendly newDecl
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
  _ -> compilerDeveloperError "Unexpected type/body of function undergoing monomorphisation"

collectReferences :: (MonadCollect builtin m) => Expr Ix builtin -> m ()
collectReferences expr = do
  _ <- traverseCandidateApplications collectApplication expr
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
  modify (Map.insert ident (Set.singleton argsToMono))
  return $ normAppList p (FreeVar p ident) (argsToMono <> remainingArgs)

--------------------------------------------------------------------------------
-- Insertion pass

type MonadInsert builtin m =
  ( MonadCompile m,
    MonadReader (SubsitutionSolutions builtin) m,
    Hashable builtin
  )

insert :: (MonadInsert builtin m) => Prog Ix builtin -> m (Prog Ix builtin)
insert = traverse (traverseCandidateApplications replaceCandidateApplication)

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

getMonomorphisedSuffix :: (PrintableBuiltin builtin) => [Arg Ix builtin] -> Text
getMonomorphisedSuffix args = do
  let implicits = mapMaybe getImplicitArg args
  let typesText = fmap getImplicitName implicits
  let typeNames = fmap (\v -> "[" <> Text.replace " " "-" v <> "]") typesText
  Text.intercalate "-" typeNames

getImplicitName :: (PrintableBuiltin builtin) => Type Ix builtin -> Text
getImplicitName t = layoutAsText $ prettyFriendly $ WithContext t emptyDBCtx
