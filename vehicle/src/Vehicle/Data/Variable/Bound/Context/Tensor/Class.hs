module Vehicle.Data.Variable.Bound.Context.Tensor.Class where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict
import Data.Coerce (coerce)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Resource (NetworkModality)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue (UnforcedBinder)
import Vehicle.Data.MaybeTrivial (MaybeTrivialT, mapMaybeTrivialT)
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Data.Variable.Bound.Context.Core
import Vehicle.Data.Variable.Bound.Context.Generic.Core
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Name.Core (CompleteNamedBoundCtx)
import Vehicle.Data.Variable.Bound.Context.Tensor.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude (GenericBinder, Name, Pretty (..), Provenance, SupplyT, developerError, mapSupplyT, (<+>))
import Vehicle.Prelude.Logging.Class

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current bound context at a given point
-- in a program where some of the variables represent tensors, i.e. what
-- bound variables are in scope.
class (MonadReadableNameContext m) => MonadReadableTensorBoundContext m where
  getNestedVariableCtx :: m NestedTensorVariableCtx

instance (Monoid w, MonadReadableTensorBoundContext m) => MonadReadableTensorBoundContext (WriterT w m) where
  getNestedVariableCtx = lift getNestedVariableCtx

instance (MonadReadableTensorBoundContext m) => MonadReadableTensorBoundContext (ReaderT w m) where
  getNestedVariableCtx = lift getNestedVariableCtx

instance (MonadReadableTensorBoundContext m) => MonadReadableTensorBoundContext (ExceptT e m) where
  getNestedVariableCtx = lift getNestedVariableCtx

instance (MonadReadableTensorBoundContext m) => MonadReadableTensorBoundContext (StateT e m) where
  getNestedVariableCtx = lift getNestedVariableCtx

instance (MonadReadableTensorBoundContext m) => MonadReadableTensorBoundContext (SupplyT e m) where
  getNestedVariableCtx = lift getNestedVariableCtx

instance (MonadReadableTensorBoundContext m) => MonadReadableTensorBoundContext (MaybeTrivialT m) where
  getNestedVariableCtx = lift getNestedVariableCtx

getCompleteNamedCtx :: (MonadReadableTensorBoundContext m) => m CompleteNamedBoundCtx
getCompleteNamedCtx = do
  NestedTensorVariableCtx _ ctx <- getNestedVariableCtx
  return ctx

-- | Returns the context as if none of the slice variables that represent
-- sub-tensors existed.
getShrunkenContext :: (MonadReadableTensorBoundContext m) => m (BoundCtx ())
getShrunkenContext = do
  NestedTensorVariableCtx ctx _ <- getNestedVariableCtx
  return $ fmap fst ctx

lookupNestedSliceVariable ::
  (MonadReadableTensorBoundContext m, SliceVariableLike variable) =>
  variable ->
  m NestedSliceVariable
lookupNestedSliceVariable var = do
  parentVar <- lookupParentTensorVariable var
  let shape = findSliceShape parentVar (toSliceVar var)
  return $ NestedSliceVariable shape (toSliceVar var)

lookupChildVariablesCertain ::
  (MonadReadableTensorBoundContext m, MonadLogger m, SliceVariableLike variable) =>
  variable ->
  m [SliceVariable]
lookupChildVariablesCertain var = do
  maybeChildVariables <- childVariablesOf <$> lookupNestedSliceVariable var
  case maybeChildVariables of
    Nothing -> developerError "Expecting a non-zero tensor variable"
    Just childVars -> return $ fmap toSliceVar childVars

lookupNestedTensorVariable ::
  (MonadReadableTensorBoundContext m, TensorVariableLike variable) =>
  variable ->
  m NestedSliceVariable
lookupNestedTensorVariable var = do
  NestedTensorVariableCtx ctx _ <- getNestedVariableCtx
  return $ findVar ctx
  where
    findVar :: GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable) -> NestedSliceVariable
    findVar = \case
      [] -> developerError $ "Missing nested tensor variable" <+> pretty (toLv var)
      (_, Nothing) : xs -> findVar xs
      (_, Just tensorVar) : xs
        | toLv tensorVar == toLv var -> tensorVar
        | otherwise -> findVar xs

-- | Given a set of variables representing the slices of a given set of tensors
-- returns the set of tensor variables that those slices are taken from.
lookupParentTensorVariables ::
  (MonadLogger m, MonadReadableTensorBoundContext m) =>
  Set SliceVariable ->
  m (Set TensorVariable)
lookupParentTensorVariables sliceVars = do
  ctx <- getNestedVariableCtx
  let result = findCorrespondingVariableInOriginalCtx ctx sliceVars
  return $ Set.fromList $ fmap extractTensorVar result
  where
    extractTensorVar :: (OriginalLv, Maybe NestedSliceVariable) -> TensorVariable
    extractTensorVar = \case
      (_, Nothing) -> developerError "was expecting only tensor variables"
      (_, Just var) -> coerce $ nestedStartingVariable var

lookupParentTensorVariable ::
  (MonadReadableTensorBoundContext m, SliceVariableLike variable) =>
  variable ->
  m NestedSliceVariable
lookupParentTensorVariable var = do
  ctx <- getNestedVariableCtx
  -- TODO turn this into a binary search for added efficiency?
  case findCorrespondingVariableInOriginalCtx ctx (Set.singleton $ toLv var) of
    [(_, Just v)] -> return v
    _ -> developerError "Missing variable"

-- | Takes a level and looks up if it represents some slice of a tensor. If it
-- does then it returns the slice variable.
lookupSliceVariableInNestedCtx ::
  (MonadReadableTensorBoundContext m) =>
  Lv ->
  m (Maybe SliceVariable)
lookupSliceVariableInNestedCtx lv = do
  ctx <- getNestedVariableCtx
  return $ fmap snd $ snd $ findOriginalVariableInCtx ctx lv

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current bound context at a given point
-- in a program where some of the variables represent tensors, i.e. what
-- bound variables are in scope.
class (MonadReadableTensorBoundContext m, MonadNameContext m) => MonadTensorBoundContext m where
  addNonTensorBinderToContext :: GenericBinder expr -> m a -> m a
  addTensorBinderToContextLocally :: KnownPrefixOfTensorShape -> UnforcedBinder Builtin -> m a -> m a
  addTensorBinderToContextPermenantly :: Provenance -> Name -> NetworkModality TensorShape -> m NestedSliceVariable

instance (Monoid w, MonadTensorBoundContext m) => MonadTensorBoundContext (WriterT w m) where
  addNonTensorBinderToContext = mapWriterT . addNonTensorBinderToContext
  addTensorBinderToContextLocally dims binder = mapWriterT (addTensorBinderToContextLocally dims binder)
  addTensorBinderToContextPermenantly p dims binder = lift (addTensorBinderToContextPermenantly p dims binder)

instance (MonadTensorBoundContext m) => MonadTensorBoundContext (ReaderT w m) where
  addNonTensorBinderToContext = mapReaderT . addNonTensorBinderToContext
  addTensorBinderToContextLocally dims binder = mapReaderT (addTensorBinderToContextLocally dims binder)
  addTensorBinderToContextPermenantly p dims binder = lift (addTensorBinderToContextPermenantly p dims binder)

instance (MonadTensorBoundContext m) => MonadTensorBoundContext (ExceptT e m) where
  addNonTensorBinderToContext = mapExceptT . addNonTensorBinderToContext
  addTensorBinderToContextLocally dims binder = mapExceptT (addTensorBinderToContextLocally dims binder)
  addTensorBinderToContextPermenantly p dims binder = lift (addTensorBinderToContextPermenantly p dims binder)

instance (MonadTensorBoundContext m) => MonadTensorBoundContext (StateT e m) where
  addNonTensorBinderToContext = mapStateT . addNonTensorBinderToContext
  addTensorBinderToContextLocally dims binder = mapStateT (addTensorBinderToContextLocally dims binder)
  addTensorBinderToContextPermenantly p dims binder = lift (addTensorBinderToContextPermenantly p dims binder)

instance (MonadTensorBoundContext m) => MonadTensorBoundContext (SupplyT e m) where
  addNonTensorBinderToContext = mapSupplyT . addNonTensorBinderToContext
  addTensorBinderToContextLocally dims binder = mapSupplyT (addTensorBinderToContextLocally dims binder)
  addTensorBinderToContextPermenantly p dims binder = lift (addTensorBinderToContextPermenantly p dims binder)

instance (MonadTensorBoundContext m) => MonadTensorBoundContext (MaybeTrivialT m) where
  addNonTensorBinderToContext = mapMaybeTrivialT . addNonTensorBinderToContext
  addTensorBinderToContextLocally dims binder = mapMaybeTrivialT (addTensorBinderToContextLocally dims binder)
  addTensorBinderToContextPermenantly p dims binder = lift (addTensorBinderToContextPermenantly p dims binder)
