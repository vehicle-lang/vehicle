module Vehicle.Compile.Scope.Generalise
  ( generaliseType,
  )
where

import Control.Monad (foldM, void, when)
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core
import Vehicle.Data.AST.Expr.Desugared qualified as S

--------------------------------------------------------------------------------
-- Expr generalisation

type GeneralisableVariable = (Provenance, Name)

generaliseType :: (MonadScopeExpr builtin m) => S.Expr -> m S.Expr
generaliseType expr = do
  candidates <- execWriterT (findGeneralisableVariables expr)
  generaliseOverVariables (reverse candidates) expr

findGeneralisableVariables :: (MonadScopeExpr builtin m, MonadWriter [GeneralisableVariable] m) => S.Expr -> m ()
findGeneralisableVariables = \case
  S.Var p v -> registerVar p v
  S.Universe {} -> return ()
  S.Hole {} -> return ()
  S.Builtin {} -> return ()
  S.App fun args -> do
    findGeneralisableVariables fun
    traverse_ (traverse_ findGeneralisableVariables) args
  S.Pi _ binder res ->
    findGeneralisableVariablesBinder binder $ findGeneralisableVariables res
  S.Lam _ binder body -> do
    findGeneralisableVariablesBinder binder $ findGeneralisableVariables body
  S.Let _ bound binder body -> do
    findGeneralisableVariables bound
    findGeneralisableVariablesBinder binder $ findGeneralisableVariables body
  S.Record _ fields -> do
    void $ traverseRecordFields findGeneralisableVariables fields
  S.RecordAcc _ record _field -> do
    findGeneralisableVariables record

findGeneralisableVariablesBinder :: (MonadScopeExpr builtin m, MonadWriter [GeneralisableVariable] m) => S.Binder -> m () -> m ()
findGeneralisableVariablesBinder binder update = do
  traverse_ findGeneralisableVariables binder
  addBinder binder update

registerVar :: (MonadScopeExpr builtin m, MonadWriter [GeneralisableVariable] m) => Provenance -> Name -> m ()
registerVar p symbol = do
  maybeVar <- lookupMaybeVariable symbol
  when (isNothing maybeVar) $ tell [(p, symbol)]

generaliseOverVariables ::
  (MonadCompile m) =>
  [GeneralisableVariable] ->
  S.Expr ->
  m S.Expr
generaliseOverVariables vars e = fst <$> foldM generaliseOverVariable (e, mempty) vars

generaliseOverVariable ::
  (MonadCompile m) =>
  (S.Expr, Set Name) ->
  GeneralisableVariable ->
  m (S.Expr, Set Name)
generaliseOverVariable (expr, seenNames) (p, name)
  | name `Set.member` seenNames = return (expr, seenNames)
  | otherwise = do
      logDebug MaxDetail $
        "Generalising over unbound variable" <+> quotePretty name
      let binderType = S.mkHole p ("typeOf[" <> name <> "]")
      let binderDisplayForm = BinderDisplayForm (OnlyName name p) True
      let binder = Binder binderDisplayForm (Implicit True) Relevant binderType
      let newExpr = S.Pi p binder expr
      return (newExpr, Set.insert name seenNames)
