module Vehicle.Compile.Type.Irrelevance
  ( RemoveIrrelevantCode,
    removeIrrelevantCode,
  )
where

import Data.List.NonEmpty qualified as NonEmpty (toList)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (MonadNorm, NormalisableBuiltin, eval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-- | Removes all irrelevant code from the program/expression.
removeIrrelevantCode ::
  (MonadCompile m, RemoveIrrelevantCode m a) =>
  a ->
  m a
removeIrrelevantCode x = do
  -- logCompilerPass MinDetail "removal of irrelevant code" $
  remove x

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

type MonadRemove m =
  ( MonadCompile m
  )

class RemoveIrrelevantCode m a where
  remove :: MonadRemove m => a -> m a

instance RemoveIrrelevantCode m expr => RemoveIrrelevantCode m (GenericProg expr) where
  remove = traverse remove

instance RemoveIrrelevantCode m expr => RemoveIrrelevantCode m (GenericDecl expr) where
  remove = traverse remove

instance RemoveIrrelevantCode m (CheckedExpr builtin) where
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
      LVec p xs -> LVec p <$> traverse remove xs
      Universe {} -> return expr
      Var {} -> return expr
      Hole {} -> return expr
      Meta {} -> return expr
      Literal {} -> return expr
      Builtin {} -> return expr

    showRemoveExit result
    return result

instance MonadNorm builtin m => RemoveIrrelevantCode m (NormExpr builtin) where
  remove expr = case expr of
    VUniverse {} -> return expr
    VLiteral {} -> return expr
    VPi binder res
      -- Don't need to substitute through here as irrelevent
      -- bound variables should only be used in irrelevent positions
      -- which will also be removed.
      | isIrrelevant binder -> remove res
      | otherwise -> VPi <$> remove binder <*> remove res
    VLam binder env body
      -- However, passing in the empty decl context here does feel like a bug...
      -- But don't have access to it here. Tried adding it to the `Env` type, but then
      -- every lambda stores an independent copy.
      | isIrrelevant binder -> do
          newExpr <- eval (VUnitLiteral : env) body
          remove newExpr
      | otherwise -> do
          VLam <$> remove binder <*> remove env <*> remove body
    VLVec xs spine -> VLVec <$> traverse remove xs <*> removeArgs spine
    VFreeVar v spine -> VFreeVar v <$> removeArgs spine
    VBoundVar v spine -> VBoundVar v <$> removeArgs spine
    VMeta m spine -> VMeta m <$> removeArgs spine
    VBuiltin b spine -> VBuiltin b <$> removeArgs spine

instance (MonadNorm builtin m, NormalisableBuiltin builtin) => RemoveIrrelevantCode m (GluedExpr builtin) where
  remove (Glued u n) = Glued <$> remove u <*> remove n

instance RemoveIrrelevantCode m expr => RemoveIrrelevantCode m (GenericArg expr) where
  remove = traverse remove

instance RemoveIrrelevantCode m expr => RemoveIrrelevantCode m (GenericBinder binding expr) where
  remove = traverse remove

instance (MonadNorm builtin m, NormalisableBuiltin builtin) => RemoveIrrelevantCode m (Env builtin) where
  remove = traverse remove

removeArgs ::
  (MonadRemove m, RemoveIrrelevantCode m expr) =>
  [GenericArg expr] ->
  m [GenericArg expr]
removeArgs = traverse remove . filter isRelevant

--------------------------------------------------------------------------------
-- Debug functions

showRemoveEntry :: MonadRemove m => CheckedExpr builtin -> m ()
showRemoveEntry _e = do
  -- logDebug MaxDetail ("remove-entry" <+> prettyVerbose e)
  incrCallDepth

showRemoveExit :: MonadRemove m => CheckedExpr builtin -> m ()
showRemoveExit _e = do
  decrCallDepth

-- logDebug MaxDetail ("remove-exit " <+> prettyVerbose e)
