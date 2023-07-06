module Vehicle.Compile.Type.Irrelevance
  ( RemoveIrrelevantCode,
    removeIrrelevantCode,
  )
where

import Data.List.NonEmpty qualified as NonEmpty (toList)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Subsystem.Standard.Core (StandardBuiltin)
import Vehicle.Compile.Type.Subsystem.Standard.Patterns (pattern UnitLiteral)
import Vehicle.Expr.DeBruijn

-- | Removes all irrelevant code from the program/expression.
removeIrrelevantCode ::
  (MonadCompile m) =>
  Prog Ix StandardBuiltin ->
  m (Prog Ix StandardBuiltin)
removeIrrelevantCode x = do
  logCompilerPass MinDetail "removal of irrelevant code" $ do
    result <- remove x
    logDebug MaxDetail $ prettyFriendly result
    return result

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

type MonadRemove m =
  ( MonadCompile m
  )

class RemoveIrrelevantCode m a where
  remove :: (MonadRemove m) => a -> m a

instance (RemoveIrrelevantCode m expr) => RemoveIrrelevantCode m (GenericProg expr) where
  remove = traverse remove

instance (RemoveIrrelevantCode m expr) => RemoveIrrelevantCode m (GenericDecl expr) where
  remove = traverse remove

instance RemoveIrrelevantCode m (Expr Ix StandardBuiltin) where
  remove expr = do
    showRemoveEntry expr
    result <- case expr of
      App p fun args -> do
        normAppList p <$> remove fun <*> removeArgs (NonEmpty.toList args)
      Pi p binder res ->
        if isIrrelevant binder
          then remove $ UnitLiteral p `substDBInto` res
          else Pi p <$> remove binder <*> remove res
      Lam p binder body ->
        if isIrrelevant binder
          then remove $ UnitLiteral p `substDBInto` body
          else Lam p <$> remove binder <*> remove body
      Ann p e t -> Ann p <$> remove e <*> remove t
      Let p bound binder body -> Let p <$> remove bound <*> remove binder <*> remove body
      Universe {} -> return expr
      FreeVar {} -> return expr
      BoundVar {} -> return expr
      Hole {} -> return expr
      Meta {} -> return expr
      Builtin {} -> return expr

    showRemoveExit result
    return result

instance (RemoveIrrelevantCode m expr) => RemoveIrrelevantCode m (GenericArg expr) where
  remove = traverse remove

instance (RemoveIrrelevantCode m expr) => RemoveIrrelevantCode m (GenericBinder expr) where
  remove = traverse remove

removeArgs ::
  (MonadRemove m, RemoveIrrelevantCode m expr) =>
  [GenericArg expr] ->
  m [GenericArg expr]
removeArgs = traverse remove . filter isRelevant

--------------------------------------------------------------------------------
-- Debug functions

showRemoveEntry :: (MonadRemove m) => Expr Ix builtin -> m ()
showRemoveEntry _e = do
  -- logDebug MaxDetail ("remove-entry" <+> prettyVerbose e)
  incrCallDepth

showRemoveExit :: (MonadRemove m) => Expr Ix builtin -> m ()
showRemoveExit _e = do
  -- logDebug MaxDetail ("remove-exit " <+> prettyVerbose e)
  decrCallDepth
