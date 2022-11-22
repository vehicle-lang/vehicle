module Vehicle.Compile.Type.Irrelevance
  ( RemoveIrrelevantCode
  , removeIrrelevantCode
  ) where

import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Expr.Normalised
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Normalise.Quote (extendEnv)
import Control.Monad.Reader (ReaderT(..))
import Vehicle.Expr.DeBruijn

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

instance RemoveIrrelevantCode expr => RemoveIrrelevantCode (GenericProg expr) where
  remove= traverse remove

instance RemoveIrrelevantCode expr => RemoveIrrelevantCode (GenericDecl expr) where
  remove = traverse remove

instance RemoveIrrelevantCode CheckedExpr where
  remove expr = do
    showRemoveEntry expr
    result <- case expr of
      App p fun args -> do
        normAppList p <$> remove fun <*> removeArgs (NonEmpty.toList args)

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

instance RemoveIrrelevantCode NormExpr where
  remove expr = case expr of
    VUniverse{} -> return expr
    VLiteral{}  -> return expr

    VPi p binder res
        -- Don't need to substitute through here as irrelevent
        -- bound variables should only be used in irrelevent positions
        -- which will also be removed.
      | isIrrelevant binder -> remove res
      | otherwise           -> VPi p <$> remove binder <*> remove res

    VLam p binder env body
      -- However, passing in the empty decl context here does feel like a bug...
      -- But don't have access to it here. Tried adding it to the `Env` type, but then
      -- every lambda stores an independent copy.
      | isIrrelevant binder -> runReaderT (eval (extendEnv (VUnitLiteral p) env) body) mempty
      | otherwise           -> VLam p <$> remove binder <*> remove env <*> remove body

    VLVec    p xs spine -> VLVec p <$> traverse remove xs <*> removeArgs spine
    VVar     p v  spine -> VVar     p v <$> removeArgs spine
    VMeta    p m  spine -> VMeta    p m <$> removeArgs spine
    VBuiltin p b  spine -> VBuiltin p b <$> removeArgs spine

instance RemoveIrrelevantCode GluedExpr where
  remove (Glued u n) = Glued <$> remove u <*> remove n

instance RemoveIrrelevantCode expr => RemoveIrrelevantCode (GenericArg expr) where
  remove = traverse remove

instance RemoveIrrelevantCode expr => RemoveIrrelevantCode (GenericBinder binding expr) where
  remove = traverse remove

instance RemoveIrrelevantCode Env where
  remove = traverse remove

removeArgs :: (MonadRemove m, RemoveIrrelevantCode expr)
           => [GenericArg expr]
           -> m [GenericArg expr]
removeArgs = traverse remove . filter isRelevant

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
