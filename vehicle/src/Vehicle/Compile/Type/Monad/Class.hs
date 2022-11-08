module Vehicle.Compile.Type.Monad.Class where

import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT (..), mapWriterT)
import Data.List (partition)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.MetaContext
import Vehicle.Compile.Type.MetaMap qualified as MetaMap
import Vehicle.Compile.Type.MetaSet (MetaSet)
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.MetaSubstitution (MetaSubstitution)
import Vehicle.Compile.Type.VariableContext (TypingDeclCtx)

--------------------------------------------------------------------------------
-- The type-checking monad class

class Monad m => MonadTypeChecker m where
  getDeclContext  :: m TypingDeclCtx
  addDeclContext :: CheckedDecl -> m a -> m a
  getMetaCtx     :: m TypingMetaCtx
  getsMetaCtx    :: (TypingMetaCtx -> a) -> m a
  putMetaCtx     :: TypingMetaCtx -> m ()
  modifyMetaCtx :: (TypingMetaCtx -> TypingMetaCtx) -> m ()

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (WriterT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapWriterT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance (Monoid w, MonadTypeChecker m) => MonadTypeChecker (ReaderT w m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapReaderT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

instance MonadTypeChecker m => MonadTypeChecker (StateT s m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapStateT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

{-

instance MonadTypeChecker m => MonadTypeChecker (LoggerT m) where
  getDeclContext = lift getDeclContext
  addDeclContext d = mapLoggerT (addDeclContext d)
  getMetaCtx = lift getMetaCtx
  getsMetaCtx = lift . getsMetaCtx
  putMetaCtx = lift . putMetaCtx
  modifyMetaCtx = lift . modifyMetaCtx

-}

getUnsolvedConstraints :: MonadTypeChecker m => m [WithContext Constraint]
getUnsolvedConstraints = getsMetaCtx constraints

getNumberOfMetasCreated :: MonadTypeChecker m => m Int
getNumberOfMetasCreated = getsMetaCtx (length . metaInfo)

getMetaSubstitution :: MonadTypeChecker m => m MetaSubstitution
getMetaSubstitution = getsMetaCtx currentSubstitution

getSolvedMetas :: MonadTypeChecker m => m MetaSet
getSolvedMetas = getsMetaCtx solvedMetas

getUnsolvedMetas :: MonadTypeChecker m => m MetaSet
getUnsolvedMetas = do
  metasSolved  <- MetaMap.keys <$> getMetaSubstitution
  numberOfMetasCreated <- getNumberOfMetasCreated
  let metasCreated = MetaSet.fromList $ fmap MetaID [0..numberOfMetasCreated-1]
  return $ MetaSet.difference metasCreated metasSolved

setConstraints :: MonadTypeChecker m => [WithContext Constraint] -> m ()
setConstraints newConstraints = modifyMetaCtx $ \TypingMetaCtx{..} ->
    TypingMetaCtx { constraints = newConstraints, ..}


-- | Returns any constraints that are activated (i.e. worth retrying) based
-- on the set of metas that were solved last pass.
popActivatedConstraints :: MonadTypeChecker m => MetaSet -> m [WithContext Constraint]
popActivatedConstraints metasSolved = do
  allConstraints <- getUnsolvedConstraints
  let (blockedConstraints, unblockedConstraints) = partition (isBlocked metasSolved) allConstraints
  setConstraints blockedConstraints
  return unblockedConstraints
  where
    isBlocked :: MetaSet -> WithContext Constraint -> Bool
    isBlocked solvedMetas constraint =
      let blockingMetas = blockedBy $ constraintContext constraint in
      -- A constraint is blocked if it is blocking on at least one meta
      -- and none of the metas it is blocking on have been solved in the last pass.
      not (MetaSet.null blockingMetas) && MetaSet.disjoint solvedMetas blockingMetas
