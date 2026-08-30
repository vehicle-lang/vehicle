module Vehicle.Compile.Type.Irrelevance
  ( RemoveIrrelevantCode,
    removeIrrelevantCodeFromProg,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Data.Builtin.Interface.Print

-- | Removes all irrelevant code from the program/expression.
removeIrrelevantCodeFromProg ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  (Type builtin -> Bool, Expr builtin -> Bool) ->
  Prog builtin ->
  m (Prog builtin)
removeIrrelevantCodeFromProg filters prog = do
  logCompilerSection2 MinDetail "removal of irrelevant code" $ do
    result <- runReaderT (remove prog) filters
    logDebug MidDetail $ "Result:" <> lineIndent (prettyExternal result)
    return result

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

type MonadRemove builtin m =
  ( MonadReader (Type builtin -> Bool, Expr builtin -> Bool) m,
    MonadLogger m,
    PrintableBuiltin builtin
  )

class RemoveIrrelevantCode builtin m a where
  remove :: (MonadRemove builtin m) => a -> m a

instance RemoveIrrelevantCode builtin m (Prog builtin) where
  remove = traverseDecls remove

instance RemoveIrrelevantCode builtin m (Decl builtin) where
  remove = traverse remove

instance RemoveIrrelevantCode builtin m (Expr builtin) where
  remove expr = do
    -- showRemoveEntry expr
    result <- case expr of
      App fun args -> do
        normAppList <$> remove fun <*> removeArgs (NonEmpty.toList args)
      Pi p binder res -> do
        (typeFilter, _) <- ask
        if isIrrelevant binder && typeFilter (typeOf binder)
          then remove $ arbitraryExpr `substDBInto` res
          else Pi p <$> remove binder <*> remove res
      Lam p binder body -> do
        (typeFilter, _) <- ask
        if isIrrelevant binder && typeFilter (typeOf binder)
          then remove $ arbitraryExpr `substDBInto` body
          else Lam p <$> remove binder <*> remove body
      Let p bound binder body -> Let p <$> remove bound <*> remove binder <*> remove body
      Record p ident fields -> Record p ident <$> traverseRecordFields remove fields
      RecordProj p recordType record field -> RecordProj p <$> remove recordType <*> remove record <*> pure field
      Universe {} -> return expr
      FreeVar {} -> return expr
      BoundVar {} -> return expr
      Hole {} -> return expr
      Meta {} -> return expr
      Builtin {} -> return expr

    -- showRemoveExit result
    return result

instance (RemoveIrrelevantCode builtin m (Expr builtin)) => RemoveIrrelevantCode builtin m (Arg builtin) where
  remove = traverse remove

instance (RemoveIrrelevantCode builtin m (Expr builtin)) => RemoveIrrelevantCode builtin m (Binder builtin) where
  remove = traverse remove

removeArgs ::
  (MonadRemove builtin m, RemoveIrrelevantCode builtin m (Expr builtin)) =>
  [Arg builtin] ->
  m [Arg builtin]
removeArgs args = do
  (_, argFilter) <- ask
  traverse remove $ filter (\a -> isRelevant a || keepArg argFilter a) args

keepArg :: (Expr builtin -> Bool) -> Arg builtin -> Bool
keepArg argFilter arg = case argExpr arg of
  BoundVar _ (-1) -> False
  expr -> not (argFilter expr)

arbitraryExpr :: Expr builtin
arbitraryExpr = BoundVar mempty (-1)

{-
--------------------------------------------------------------------------------
-- Debug functions

showRemoveEntry :: (MonadRemove m) => Expr builtin -> m ()
showRemoveEntry _e = do
  -- logDebug MaxDetail ("remove-entry" <+> prettyVerbose e)
  incrCallDepth

showRemoveExit :: (MonadRemove m) => Expr builtin -> m ()
showRemoveExit _e = do
  -- logDebug MaxDetail ("remove-exit " <+> prettyVerbose e)
  decrCallDepth
-}
