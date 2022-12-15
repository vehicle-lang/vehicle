{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Vehicle.Compile.Monomorphisation where

import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks, local)
import Control.Monad.State
  ( MonadState (..),
    StateT (runStateT),
    evalStateT,
    gets,
    modify,
  )
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
  ( insert,
    lookup,
    member,
    singleton,
    union,
    unionWith,
    unions,
  )
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set (singleton, size, toList, union)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (length, splitAt)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Expr.AlphaEquivalence ()
import Vehicle.Expr.DeBruijn

--------------------------------------------------------------------------------
-- Public interface

-- | Tries to monomorphise any polymorphic functions by creating a copy per
-- concrete type each function is used with.
-- Not very sophisticated at the moment, if this needs to be improved perhaps
-- http://mrg.doc.ic.ac.uk/publications/featherweight-go/main.pdf
-- by Wen et al is a good starting point.
monomorphise ::
  MonadCompile m =>
  CheckedProg ->
  m CheckedProg
monomorphise prog = logCompilerPass MinDetail "monomorphisation" $ do
  ((prog2, applications), candidates) <- runStateT (runWriterT (collect prog)) mempty
  runReaderT (evalStateT (insert applications prog2) candidates) mempty

--------------------------------------------------------------------------------
-- Definitions and utilites

-- | Candidate monomorphisable functions
type Candidates = HashMap Identifier Int

-- | Applications of monomorphisable functions
newtype CandidateApplications = Applications (HashMap Identifier (HashSet [CheckedArg]))

instance Semigroup CandidateApplications where
  Applications xs <> Applications ys = Applications $ Map.unionWith Set.union xs ys

instance Monoid CandidateApplications where
  mempty = Applications mempty

-- | Solution identifier for a candidate monomorphisation application
type CandidateApplicationSolutions = HashMap (Identifier, [CheckedArg]) Identifier

type MonadMono m =
  ( MonadCompile m,
    MonadState Candidates m
  )

traverseCandidateApplications ::
  MonadMono m =>
  (Provenance -> Identifier -> [CheckedArg] -> [CheckedArg] -> m CheckedExpr) ->
  CheckedExpr ->
  m CheckedExpr
traverseCandidateApplications processApp = go
  where
    go expr = case expr of
      App p fun args -> do
        fun' <- go fun
        args' <- traverse (traverse go) args
        let defaultResult = App p fun' args'
        case getFreeVar fun' of
          Nothing -> return defaultResult
          Just ident -> do
            maybeCandidate <- isCandidateApplication ident args'
            case maybeCandidate of
              Nothing -> return defaultResult
              Just (argsToMono, remainingArgs) -> processApp p ident argsToMono remainingArgs
      Var {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Literal {} -> return expr
      LVec p es -> LVec p <$> traverse go es
      Ann p e t -> Ann p <$> go e <*> go t
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body -> Lam p <$> traverse go binder <*> go body
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body

isCandidateApplication ::
  MonadMono m =>
  Identifier ->
  NonEmpty CheckedArg ->
  m (Maybe ([CheckedArg], [CheckedArg]))
isCandidateApplication ident args = do
  result <- gets (Map.lookup ident)
  case result of
    Nothing -> return Nothing
    Just d ->
      if d > NonEmpty.length args
        then compilerDeveloperError "Monomorphisation does not yet support partially applied polymorphic functions"
        else do
          return $ Just $ NonEmpty.splitAt d args

--------------------------------------------------------------------------------
-- Initial pass - collects the sites for monomorphisation

type MonadCollect m =
  ( MonadMono m,
    MonadWriter CandidateApplications m
  )

addCandidate :: MonadCollect m => Identifier -> Int -> m ()
addCandidate ident numberOfArgs = do
  logDebug MaxDetail $ pretty ident
  modify (Map.insert ident numberOfArgs)
  tell (Applications $ Map.singleton ident mempty)

addCandidateApplication :: MonadCollect m => Identifier -> [CheckedArg] -> m ()
addCandidateApplication ident argsToMono =
  tell (Applications $ Map.singleton ident (Set.singleton argsToMono))

class Monomorphise a where
  collect :: MonadCollect m => a -> m a

instance Monomorphise CheckedProg where
  collect = traverseDecls collect

instance Monomorphise CheckedDecl where
  collect decl = do
    result <- traverse collect decl
    case isMonomorphisationCandidate result of
      Nothing -> return ()
      Just d -> addCandidate (identifierOf result) d
    return result

instance Monomorphise CheckedExpr where
  collect = traverseCandidateApplications collectApplication

collectApplication ::
  MonadCollect m =>
  Provenance ->
  Identifier ->
  [CheckedArg] ->
  [CheckedArg] ->
  m CheckedExpr
collectApplication p ident argsToMono remainingArgs = do
  addCandidateApplication ident argsToMono
  return $ normAppList p (FreeVar p ident) (argsToMono <> remainingArgs)

-- | If monomorphisable then it returns the number of arguments that should be monomorphised.
-- Tracking the number of arguments this way seems more than a little hacky.
isMonomorphisationCandidate :: CheckedDecl -> Maybe Int
isMonomorphisationCandidate = \case
  DefFunction _ _ _ t _ -> getArgs t
  _ -> Nothing
  where
    getArgs :: CheckedExpr -> Maybe Int
    getArgs = \case
      Pi _ binder result
        | isCandidateBinder binder -> maybe (Just 1) (\v -> Just (v + 1)) (getArgs result)
      _ -> Nothing

    isCandidateBinder :: CheckedBinder -> Bool
    isCandidateBinder binder = case (visibilityOf binder, binderType binder) of
      (Implicit, TypeUniverse _ 0) -> True
      (Instance, _) -> True
      _ -> False

--------------------------------------------------------------------------------
-- Insertion pass

type MonadInsert m =
  ( MonadMono m,
    MonadReader CandidateApplicationSolutions m
  )

insert :: MonadInsert m => CandidateApplications -> CheckedProg -> m CheckedProg
insert apps (Main ds) = Main <$> insertDecls apps ds

insertDecls :: MonadInsert m => CandidateApplications -> [CheckedDecl] -> m [CheckedDecl]
insertDecls apps = \case
  [] -> return []
  (d : ds) -> do
    (d', solutions) <- insertDecl apps d
    ds' <- local (Map.union solutions) $ insertDecls apps ds
    return $ d' <> ds'

insertDecl ::
  MonadInsert m =>
  CandidateApplications ->
  CheckedDecl ->
  m ([CheckedDecl], CandidateApplicationSolutions)
insertDecl (Applications apps) decl = do
  result <- traverse insertExpr decl

  case result of
    DefPostulate {} -> return ([decl], mempty)
    DefResource {} -> return ([decl], mempty)
    DefFunction p ident isProperty t e -> do
      isCandidate <- gets (Map.member ident)
      if not isCandidate
        then return ([decl], mempty)
        else case Map.lookup ident apps of
          Nothing -> compilerDeveloperError "No applications entry for monomorphisation candidate"
          Just uniqueArgs -> case Set.size uniqueArgs of
            0 ->
              -- If function is unused then zap it.
              return ([], mempty)
            1 -> do
              let args = head $ Set.toList uniqueArgs
              let (t', e') = substituteArgsThrough (t, e, args)
              return ([DefFunction p ident isProperty t' e'], Map.singleton (ident, args) ident)
            _ -> do
              (decls, solutions) <-
                unzip
                  <$> for
                    (Set.toList uniqueArgs)
                    ( \args -> do
                        let suffix = getMonomorphisedSuffix args
                        let newIdent = Identifier (moduleOf ident) $ nameOf ident <> suffix
                        let (t', e') = substituteArgsThrough (t, e, args)
                        return (DefFunction p newIdent isProperty t' e', Map.singleton (ident, args) newIdent)
                    )
              return (decls, Map.unions solutions)

insertExpr :: MonadInsert m => CheckedExpr -> m CheckedExpr
insertExpr = traverseCandidateApplications replaceCandidateApplication

replaceCandidateApplication ::
  MonadInsert m =>
  Provenance ->
  Identifier ->
  [CheckedArg] ->
  [CheckedArg] ->
  m CheckedExpr
replaceCandidateApplication p ident monoArgs remainingArgs = do
  solution <- asks (Map.lookup (ident, monoArgs))
  case solution of
    Nothing -> compilerDeveloperError "Monomorphisable function application has no resulting monomorphisation."
    Just replacementIdent -> return $ normAppList p (FreeVar p replacementIdent) remainingArgs

substituteArgsThrough :: (CheckedType, CheckedExpr, [CheckedArg]) -> (CheckedType, CheckedExpr)
substituteArgsThrough = \case
  (t, e, []) -> (t, e)
  (Pi _ _ t, Lam _ _ e, arg : args) -> do
    let expr = argExpr arg
    substituteArgsThrough (expr `substDBInto` t, expr `substDBInto` e, args)
  _ -> developerError "Unexpected type/body of function undergoing monomorphisation"

getMonomorphisedSuffix :: [CheckedArg] -> Text
getMonomorphisedSuffix args = do
  let implicits = mapMaybe getImplicitArg args
  let typesText = fmap (layoutAsText . prettyFriendly) implicits
  let typeNames = fmap (\v -> "[" <> Text.replace " " "-" v <> "]") typesText
  Text.intercalate "-" typeNames
