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
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Meta (freshLinearityMeta, freshPolarityMeta, getDeclType)

-- | Inserts holes for all the non-user facing auxilliary annotations, e.g.
-- polarity annotations on the `Bool` type and linearity annotations on the
-- `Rat` type.
insertHolesForAuxiliaryAnnotations :: (TCM m
                                     , InsertAuxiliaryAnnotations a)
                                   => a
                                   -> m a
insertHolesForAuxiliaryAnnotations = insert

-- | Removes all the non-user facing auxilliary arguments, i.e. arguments of
-- type Polarity/Linearity as well as type-level binders for auxiliary types.
removeAuxiliaryArguments :: (MonadCompile m, RemoveAuxiliaryArguments a)
                         => a -> m a
removeAuxiliaryArguments x =
  logCompilerPass MinDetail "removal of auxiliary code" $
    runReaderT (remove x) mempty

-------------------------------------------------------------------------------
-- Inserting polarity and linearity annotations

class InsertAuxiliaryAnnotations a where
  insert :: TCM m => a -> m a

instance InsertAuxiliaryAnnotations UncheckedProg where
  insert (Main ds) = Main <$> traverse insert ds

instance InsertAuxiliaryAnnotations UncheckedDecl where
  insert = \case
    DefResource p resourceType ident t ->
      DefResource p resourceType ident <$> insert t

    DefFunction p propertyInfo ident t e ->
      DefFunction p propertyInfo ident <$> insert t <*> insert e

instance InsertAuxiliaryAnnotations UncheckedExpr where
  insert expr = case expr of
    -- Insert meta-variables for types which have annotations
    BoolType p -> do
      lin <- freshLinearityMeta p
      pol <- freshPolarityMeta p
      return $ App p expr
        [ ImplicitArg p lin
        , ImplicitArg p pol
        ]

    RatType p -> do
      lin <- freshLinearityMeta p
      return $ App p expr
        [ ImplicitArg p lin
        ]

    Var p (Free ident) -> do
      declType <- getDeclType p ident
      auxArgs <- declAuxArgs p declType
      return $ normAppList p expr auxArgs

    -- Needed to ensure idempotence of type-checking
    App p fun@BoolType{}     args -> App p fun <$> traverse insert args
    App p fun@RatType{}      args -> App p fun <$> traverse insert args
    App p fun@(Var _ Free{}) args -> App p fun <$> traverse insert args

    Ann  p e t               -> Ann p <$> insert e <*> insert t
    App  p fun args          -> App p <$> insert fun <*> traverse insert args
    Pi   p binder res        -> Pi  p <$> insert binder <*> insert res
    Let  p bound binder body -> Let p <$> insert bound <*> insert binder <*> insert body
    Lam  p binder body       -> Lam p <$> insert binder <*> insert body
    LSeq p xs                -> LSeq p <$> traverse insert xs
    PrimDict p t             -> PrimDict p <$> insert t

    Universe{} -> return expr
    Var{}      -> return expr
    Hole{}     -> return expr
    Meta{}     -> return expr
    Literal{}  -> return expr
    Builtin{}  -> return expr

instance InsertAuxiliaryAnnotations UncheckedArg where
  insert = traverseArgExpr insert

instance InsertAuxiliaryAnnotations UncheckedBinder where
  insert = traverseBinderType insert

declAuxArgs :: TCM m => Provenance -> CheckedExpr -> m [CheckedArg]
declAuxArgs p = \case
  Pi _ binder r
    | visibilityOf binder == Implicit && isAuxiliaryUniverse (typeOf binder) -> do
    xs <- declAuxArgs p r
    meta <- case typeOf binder of
      PolarityUniverse{}  -> freshPolarityMeta p
      LinearityUniverse{} -> freshLinearityMeta p
      _                   -> compilerDeveloperError "Mismatch between cases and 'isAuxiliaryUniverse'"
    return $ ImplicitArg p meta : xs
  _ -> return []

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

type MonadRemove m =
  ( MonadCompile m
  , MonadReader IntSet m
  )

class RemoveAuxiliaryArguments a where
  remove :: MonadRemove m => a -> m a

instance RemoveAuxiliaryArguments CheckedProg where
  remove (Main ds) = Main <$> traverse remove ds

instance RemoveAuxiliaryArguments CheckedDecl where
  remove = \case
    DefResource p resourceType ident t ->
      DefResource p resourceType ident <$> remove t

    DefFunction p propertyInfo ident t e ->
      DefFunction p propertyInfo ident <$> remove t <*> remove e

instance RemoveAuxiliaryArguments CheckedExpr where
  remove expr = do
    showRemoveEntry expr
    result <- case expr of
      App p fun args ->
        normAppList p <$> remove fun <*> (traverse remove =<< removeAuxiliaryArgs args)

      Pi p binder res -> do
        isAux <- isAuxiliary (typeOf binder)
        if isAux
          then local (IntSet.insert 0 . underBinder) (remove res)
          else Pi  p <$> remove binder <*> local underBinder (remove res)

      Lam p binder body -> do
        isAux <- isAuxiliary (typeOf binder)
        if isAux
          then local (IntSet.insert 0 . underBinder) (remove body)
          else Lam p <$> remove binder <*> local underBinder (remove body)

      Ann  p e t               -> Ann p <$> remove e <*> remove t
      Let  p bound binder body -> Let p <$> remove bound <*> remove binder <*> local underBinder (remove body)
      LSeq p xs                -> LSeq p <$> traverse remove xs
      PrimDict p t             -> PrimDict p <$> remove t

      Universe{} -> return expr
      Var{}      -> return expr
      Hole{}     -> return expr
      Meta{}     -> return expr
      Literal{}  -> return expr
      Builtin{}  -> return expr

    showRemoveExit result
    return result

instance RemoveAuxiliaryArguments CheckedArg where
  remove = traverseArgExpr remove

instance RemoveAuxiliaryArguments CheckedBinder where
  remove = traverseBinderType remove

isAuxiliary :: MonadRemove m => CheckedExpr -> m Bool
isAuxiliary e = case exprHead e of
  Var     _ (Bound v)           -> asks (IntSet.member v)
  PolarityUniverse{}            -> return True
  LinearityUniverse{}           -> return True
  Builtin _ Polarity{}          -> return True
  Builtin _ Linearity{}         -> return True
  Builtin _ (TypeClass tc)      -> return $ isAuxiliaryTypeClass tc
  PrimDict _ tc                 -> isAuxiliary tc
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