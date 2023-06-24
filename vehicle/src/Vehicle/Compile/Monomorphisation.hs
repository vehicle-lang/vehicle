{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Monomorphisation where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State
  ( MonadState (..),
    evalStateT,
    modify,
  )
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
  ( insert,
    lookup,
    singleton,
  )
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set (singleton, size, toList)
import Data.List.NonEmpty qualified as NonEmpty (span)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard ()
import Vehicle.Compile.Type.Subsystem.Standard.Core
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
  (MonadCompile m) =>
  Bool ->
  TypeCheckedProg ->
  m TypeCheckedProg
monomorphise keepUnused prog = logCompilerPass MinDetail "monomorphisation" $ do
  (prog2, substitutions) <- runReaderT (evalStateT (runWriterT (monomorphiseProg prog)) mempty) keepUnused
  result <- runReaderT (insert prog2) substitutions
  logCompilerPassOutput $ prettyFriendly result
  return result

--------------------------------------------------------------------------------
-- Definitions and utilites

-- | Candidate monomorphisable functions
type Candidates = HashMap Identifier Int

-- | Applications of monomorphisable functions
type CandidateApplications = HashMap Identifier (HashSet [TypeCheckedArg])

-- | Solution identifier for a candidate monomorphisation application
type SubsitutionSolutions = HashMap (Identifier, [TypeCheckedArg]) Identifier

traverseCandidateApplications ::
  (MonadCompile m) =>
  (Provenance -> Identifier -> [TypeCheckedArg] -> [TypeCheckedArg] -> m TypeCheckedExpr) ->
  TypeCheckedExpr ->
  m TypeCheckedExpr
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

type MonadCollect m =
  ( MonadCompile m,
    MonadState CandidateApplications m,
    MonadWriter SubsitutionSolutions m,
    MonadReader Bool m
  )

monomorphiseProg :: (MonadCollect m) => TypeCheckedProg -> m TypeCheckedProg
monomorphiseProg (Main decls) =
  Main . reverse . concat <$> traverse monomorphiseDecls (reverse decls)

monomorphiseDecls :: (MonadCollect m) => TypeCheckedDecl -> m [TypeCheckedDecl]
monomorphiseDecls decl = do
  logCompilerSection MaxDetail ("Checking" <+> quotePretty (identifierOf decl)) $ do
    newDecls <- monomorphiseDecl decl
    forM_ newDecls (traverse collectReferences)
    return newDecls

monomorphiseDecl :: (MonadCollect m) => TypeCheckedDecl -> m [TypeCheckedDecl]
monomorphiseDecl decl = case decl of
  DefAbstract {} -> return [decl]
  DefFunction p ident anns t e -> do
    freeVarApplications <- get
    keepUnused <- ask
    case Map.lookup ident freeVarApplications of
      Nothing -> do
        logDebug MaxDetail $ "No applications of" <+> quotePretty ident <+> "found."
        if keepUnused || isProperty anns
          then do
            logDebug MaxDetail "Keeping declaration as it is a property"
            return [decl]
          else do
            logDebug MaxDetail "Discarding declaration"
            return []
      Just applications -> do
        let numberOfApplications = Set.size applications
        let createNewName = numberOfApplications > 1
        logDebug MaxDetail $ "Found" <+> pretty numberOfApplications <+> "type-unique applications"
        traverse (performMonomorphisation (p, ident, anns, t, e) createNewName) (Set.toList applications)

performMonomorphisation ::
  (MonadCollect m) =>
  (Provenance, Identifier, [Annotation], TypeCheckedType, TypeCheckedExpr) ->
  Bool ->
  [TypeCheckedArg] ->
  m TypeCheckedDecl
performMonomorphisation (p, ident, anns, typ, body) createNewName args = do
  let newIdent
        | createNewName = Identifier (moduleOf ident) $ nameOf ident <> getMonomorphisedSuffix args
        | otherwise = ident
  let (newType, newBody) = substituteArgsThrough (typ, body, args)
  tell (Map.singleton (ident, args) newIdent)
  let newDecl = DefFunction p newIdent anns newType newBody
  return newDecl

substituteArgsThrough :: (TypeCheckedType, TypeCheckedExpr, [TypeCheckedArg]) -> (TypeCheckedType, TypeCheckedExpr)
substituteArgsThrough = \case
  (t, e, []) -> (t, e)
  (Pi _ _ t, Lam _ _ e, arg : args) -> do
    let expr = argExpr arg
    substituteArgsThrough (expr `substDBInto` t, expr `substDBInto` e, args)
  _ -> developerError "Unexpected type/body of function undergoing monomorphisation"

collectReferences :: (MonadCollect m) => TypeCheckedExpr -> m ()
collectReferences expr = do
  _ <- traverseCandidateApplications collectApplication expr
  return ()

collectApplication ::
  (MonadCollect m) =>
  Provenance ->
  Identifier ->
  [TypeCheckedArg] ->
  [TypeCheckedArg] ->
  m TypeCheckedExpr
collectApplication p ident argsToMono remainingArgs = do
  logDebug MaxDetail $ "Found application:" <+> quotePretty ident <+> prettyVerbose argsToMono
  modify (Map.insert ident (Set.singleton argsToMono))
  return $ normAppList p (FreeVar p ident) (argsToMono <> remainingArgs)

--------------------------------------------------------------------------------
-- Insertion pass

type MonadInsert m =
  ( MonadCompile m,
    MonadReader SubsitutionSolutions m
  )

insert :: (MonadInsert m) => TypeCheckedProg -> m TypeCheckedProg
insert = traverse (traverseCandidateApplications replaceCandidateApplication)

replaceCandidateApplication ::
  (MonadInsert m) =>
  Provenance ->
  Identifier ->
  [TypeCheckedArg] ->
  [TypeCheckedArg] ->
  m TypeCheckedExpr
replaceCandidateApplication p ident monoArgs remainingArgs = do
  solution <- asks (Map.lookup (ident, monoArgs))
  case solution of
    Nothing -> return $ normAppList p (FreeVar p ident) (monoArgs <> remainingArgs)
    Just replacementIdent -> return $ normAppList p (FreeVar p replacementIdent) remainingArgs

getMonomorphisedSuffix :: [TypeCheckedArg] -> Text
getMonomorphisedSuffix args = do
  let implicits = mapMaybe getImplicitArg args
  let typesText = fmap getImplicitName implicits
  let typeNames = fmap (\v -> "[" <> Text.replace " " "-" v <> "]") typesText
  Text.intercalate "-" typeNames

getImplicitName :: TypeCheckedType -> Text
getImplicitName t = layoutAsText $ prettyFriendly $ WithContext t emptyDBCtx
