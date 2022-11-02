module Vehicle.Compile.Type.Irrelevance
  ( RemoveIrrelevantCode
  , removeIrrelevantCode
  ) where

import Data.List.NonEmpty qualified as NonEmpty (filter)

import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Language.Print (prettyVerbose)

-- | Removes all irrelevant code from the program/expression.
removeIrrelevantCode :: (MonadCompile m, RemoveIrrelevantCode a)
                     => a
                     -> m a
removeIrrelevantCode x =
  logCompilerPass MinDetail "removal of irrelevant code" $
    remove x

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

type MonadRemove m =
  ( MonadCompile m
  )

class RemoveIrrelevantCode a where
  remove :: MonadRemove m => a -> m a

instance RemoveIrrelevantCode CheckedProg where
  remove (Main ds) = Main <$> traverse remove ds

instance RemoveIrrelevantCode CheckedDecl where
  remove = traverse remove

instance RemoveIrrelevantCode CheckedExpr where
  remove expr = do
    showRemoveEntry expr
    result <- case expr of
      App p fun args -> do
        let relevantArgs = NonEmpty.filter isRelevant args
        normAppList p <$> remove fun <*> traverse remove relevantArgs

      Pi p binder res -> do
        if isIrrelevant binder
          then remove $ UnitLiteral p `substInto` res
          else Pi p <$> remove binder <*> remove res

      Lam p binder body -> do
        if isIrrelevant binder
          then remove $ UnitLiteral p `substInto` body
          else Lam p <$> remove binder <*> remove body

      Ann  p e t               -> Ann p <$> remove e <*> remove t
      Let  p bound binder body -> Let p <$> remove bound <*> remove binder <*> remove body
      LVec p xs                -> LVec p <$> traverse remove xs

      Universe{} -> return expr
      Var{}      -> return expr
      Hole{}     -> return expr
      Meta{}     -> return expr
      Literal{}  -> return expr
      Builtin{}  -> return expr

    showRemoveExit result
    return result

instance RemoveIrrelevantCode CheckedArg where
  remove = traverse remove

instance RemoveIrrelevantCode CheckedBinder where
  remove = traverse remove

--------------------------------------------------------------------------------
-- Debug functions

showRemoveEntry :: MonadRemove m => CheckedExpr -> m ()
showRemoveEntry e = do
  logDebug MaxDetail ("remove-entry" <+> prettyVerbose e)
  incrCallDepth

showRemoveExit :: MonadRemove m => CheckedExpr -> m ()
showRemoveExit e = do
  decrCallDepth
  logDebug MaxDetail ("remove-exit " <+> prettyVerbose e)
