module Vehicle.Compile.Type.Auxiliary
  ( InsertAuxiliaryAnnotations
  , insertHolesForAuxiliaryAnnotations
  , RemoveAuxiliaryArguments
  , removeAuxiliaryArguments
  ) where

import Control.Monad.Reader (MonadReader(..), asks, ReaderT (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (zip, filter)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet (insert, member, mapMonotonic)

import Vehicle.Compile.Prelude
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Error (MonadCompile)

-- | Inserts holes for all the non-user facing auxilliary annotations, e.g.
-- polarity annotations on the `Bool` type and linearity annotations on the
-- `Rat` type.
insertHolesForAuxiliaryAnnotations :: InsertAuxiliaryAnnotations a => a -> a
insertHolesForAuxiliaryAnnotations = insert

-- | Removes all the non-user facing auxilliary arguments, i.e. arguments of
-- type Polarity/Linearity as well as type-level binders for auxiliary types.
removeAuxiliaryArguments :: (MonadCompile m, RemoveAuxiliaryArguments a)
                         => a -> m a
removeAuxiliaryArguments x =
  logCompilerPass "removal of auxiliary code" $
    runReaderT (remove x) mempty

-------------------------------------------------------------------------------
-- Inserting polarity and linearity annotations

class InsertAuxiliaryAnnotations a where
  insert :: a -> a

instance InsertAuxiliaryAnnotations UncheckedProg where
  insert (Main ds) = Main (insert <$> ds)

instance InsertAuxiliaryAnnotations UncheckedDecl where
  insert = \case
    DefResource ann resourceType ident t ->
      DefResource ann resourceType ident (insert t)

    DefFunction ann propertyInfo ident t e ->
      DefFunction ann propertyInfo ident (insert t) (insert e)

instance InsertAuxiliaryAnnotations UncheckedExpr where
  insert expr = case expr of
    -- Insert a suitable hole for types which have annotations
    Builtin ann Bool -> App ann expr [ImplicitArg ann (mkHole ann "boolAnn")]

    -- Needed to ensure idempotence of type-checking
    App _ (Builtin _ Bool) _ -> expr

    Ann  ann e t               -> Ann ann (insert e) (insert t)
    App  ann fun args          -> App ann (insert fun) (insert <$> args)
    Pi   ann binder res        -> Pi  ann (insert binder) (insert res)
    Let  ann bound binder body -> Let ann (insert bound) (insert binder) (insert body)
    Lam  ann binder body       -> Lam ann (insert binder) (insert body)
    LSeq ann tc xs             -> LSeq ann (insert tc) (insert <$> xs)
    PrimDict ann t             -> PrimDict ann (insert t)

    Type{}    -> expr
    Var{}     -> expr
    Hole{}    -> expr
    Meta{}    -> expr
    Literal{} -> expr
    Builtin{} -> expr

instance InsertAuxiliaryAnnotations UncheckedArg where
  insert = mapArgExpr insert

instance InsertAuxiliaryAnnotations UncheckedBinder where
  insert = mapBinderType insert

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

type MonadRemove m =
  ( MonadCompile m
  , MonadReader IntSet m
  )

class RemoveAuxiliaryArguments a where
  remove :: MonadRemove m => a -> m a

instance RemoveAuxiliaryArguments UncheckedProg where
  remove (Main ds) = Main <$> traverse remove ds

instance RemoveAuxiliaryArguments UncheckedDecl where
  remove = \case
    DefResource ann resourceType ident t ->
      DefResource ann resourceType ident <$> remove t

    DefFunction ann propertyInfo ident t e ->
      DefFunction ann propertyInfo ident <$> remove t <*> remove e

instance RemoveAuxiliaryArguments UncheckedExpr where
  remove expr = do
    showRemoveEntry expr
    result <- case expr of
      App ann fun args ->
        normAppList ann <$> remove fun <*> (traverse remove =<< removeAuxiliaryArgs args)

      Pi ann binder res -> do
        isAux <- isAuxiliary (typeOf binder)
        if isAux
          then local (IntSet.insert 0 . underBinder) (remove res)
          else Pi  ann <$> remove binder <*> local underBinder (remove res)

      Lam ann binder body -> do
        isAux <- isAuxiliary (typeOf binder)
        if isAux
          then local (IntSet.insert 0 . underBinder) (remove body)
          else Lam ann <$> remove binder <*> local underBinder (remove body)

      Ann  ann e t               -> Ann ann <$> remove e <*> remove t
      Let  ann bound binder body -> Let ann <$> remove bound <*> remove binder <*> local underBinder (remove body)
      LSeq ann tc xs             -> LSeq ann <$> remove tc <*> traverse remove xs
      PrimDict ann t             -> PrimDict ann <$> remove t

      Type{}    -> return expr
      Var{}     -> return expr
      Hole{}    -> return expr
      Meta{}    -> return expr
      Literal{} -> return expr
      Builtin{} -> return expr

    showRemoveExit result
    return result

instance RemoveAuxiliaryArguments UncheckedArg where
  remove = traverseArgExpr remove

instance RemoveAuxiliaryArguments UncheckedBinder where
  remove = traverseBinderType remove

isAuxiliary :: MonadRemove m => CheckedExpr -> m Bool
isAuxiliary e = case exprHead e of
  Builtin _ AuxiliaryType       -> return True
  Builtin _ Polarity{}          -> return True
  Builtin _ PolarityTypeClass{} -> return True
  Var     _ (Bound v)           -> asks (IntSet.member v)
  _                             -> return False

removeAuxiliaryArgs :: MonadRemove m => NonEmpty CheckedArg -> m [CheckedArg]
removeAuxiliaryArgs args = do
  decisions <- traverse (isAuxiliary . argExpr) args
  let argsAndDecs = NonEmpty.zip args decisions
  let nonAuxRes = NonEmpty.filter (\(_, d) -> not d) argsAndDecs
  return $ fmap fst nonAuxRes

underBinder :: IntSet -> IntSet
underBinder = IntSet.mapMonotonic (+1)

--------------------------------------------------------------------------------
-- Debug functions

showRemoveEntry :: MonadRemove m => CheckedExpr -> m ()
showRemoveEntry e = do
  auxVars <- ask
  logDebug MaxDetail ("remove-entry" <+> pretty auxVars <+> prettyVerbose e)
  incrCallDepth

showRemoveExit :: MonadRemove m => CheckedExpr -> m ()
showRemoveExit e = do
  decrCallDepth
  logDebug MaxDetail ("remove-exit " <+> prettyVerbose e)
