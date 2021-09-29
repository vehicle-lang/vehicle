
module Vehicle.Core.Compile.Type.Meta
  ( MetaSet
  , MetaSubstitution
  , MetaSubstitutable(..)
  , MonadMeta
  , freshMetaWith
  , metaSolved
  , makeMetaType
  , addUnificationConstraint
  , addUnificationConstraints
  , getUnificationConstraints
  , setUnificationConstraints
  , getTypeClassConstraints
  , addTypeClassConstraint
  , setTypeClassConstraints
  , getMetaSubstitution
  , modifyMetaSubstitution
  , Progress(..)
  , pattern Solved
  , pattern PartiallySolved
  , solved
  , solvedTrivial
  , partiallySolved
  , isStuck
  ) where

import Control.Monad.Reader (Reader, runReader, ask, local)
import Control.Monad.State (MonadState(..), modify, gets)
import Data.List (foldl')

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Print (prettyVerbose)
import Vehicle.Core.MetaSubstitution ( MetaSubstitution )
import Vehicle.Core.MetaSubstitution qualified as MetaSubst (singleton, map, lookup, insertWith)
import Vehicle.Core.MetaSet (MetaSet)
import Vehicle.Core.MetaSet qualified as MetaSet (singleton)

class MetaSubstitutable a where
  -- TODO change name away from M
  substM :: a -> Reader MetaSubstitution a

  substMetas :: MetaSubstitution -> a -> a
  substMetas s e = runReader (substM e) s

  substMeta :: Meta -> CheckedExpr -> a -> a
  substMeta m e = substMetas (MetaSubst.singleton m e)

  substMetasLiftLocal :: a -> Reader MetaSubstitution a
  substMetasLiftLocal e = local (MetaSubst.map (liftDBIndices 1)) (substM e)

instance MetaSubstitutable a => MetaSubstitutable (a, a) where
  substM (e1, e2) = do
    e1' <- substM e1
    e2' <- substM e2
    return (e1', e2')

instance MetaSubstitutable a => MetaSubstitutable [a] where
  substM = traverse substM

instance MetaSubstitutable CheckedArg where
  substM (Arg p v e) = Arg p v <$> substM e

instance MetaSubstitutable CheckedBinder where
  substM (Binder p v n t) = Binder p v n <$> substM t

instance MetaSubstitutable CheckedExpr where
  substM = \case
    Type l                   -> return (Type l)
    Hole p name              -> return (Hole p name)
    Builtin ann op           -> return (Builtin ann op)
    Literal ann l            -> return (Literal ann l)
    Var     ann v            -> return (Var     ann v)
    Seq     ann es           -> Seq     ann <$> traverse substM es
    Ann     ann term typ     -> Ann     ann <$> substM term   <*> substM typ
    App     ann fun arg      -> App     ann <$> substM fun    <*> substM arg
    Pi      ann binder res   -> Pi      ann <$> substM binder <*> substMetasLiftLocal res
    Let     ann e1 binder e2 -> Let     ann <$> substM e1     <*> substM binder <*> substMetasLiftLocal e2
    Lam     ann binder e     -> Lam     ann <$> substM binder <*> substMetasLiftLocal e
    PrimDict tc              -> PrimDict <$> substM tc

    Meta    ann m -> do
      subst <- ask
      case MetaSubst.lookup m subst of
        Nothing -> return $ Meta ann m
        Just e  -> substM e

instance MetaSubstitutable CheckedDecl where
  substM = \case
    DeclNetw p ident t   -> DeclNetw p ident <$> substM t
    DeclData p ident t   -> DeclData p ident <$> substM t
    DefFun   p ident t e -> DefFun   p ident <$> substM t <*> substM e

instance MetaSubstitutable CheckedProg where
  substM (Main ds) = Main <$> traverse substM ds

instance MetaSubstitutable UnificationConstraint where
  substM (Unify p ctx metas history es) =
    Unify p ctx metas history <$> substM es

instance MetaSubstitutable TypeClassConstraint where
  substM (m `Has` e) = (m `Has`) <$> substM e

--------------------------------------------------------------------------------
-- The meta monad

type MonadMeta m = MonadState MetaCtx m

modifyMetaCtx :: MonadMeta m => (MetaCtx -> MetaCtx) -> m ()
modifyMetaCtx = modify

getUnificationConstraints :: MonadMeta m => m [UnificationConstraint]
getUnificationConstraints = gets unificationConstraints

addUnificationConstraint :: (MonadMeta m, MonadLogger m) => UnificationConstraint -> m ()
addUnificationConstraint c = addUnificationConstraints [c]

addUnificationConstraints :: (MonadMeta m, MonadLogger m) => [UnificationConstraint] -> m ()
addUnificationConstraints constraints = do
  logDebug ("add-unification-constraints " <> pretty constraints)
  modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { unificationConstraints = constraints ++ unificationConstraints, ..}

setUnificationConstraints :: MonadMeta m => [UnificationConstraint] -> m ()
setUnificationConstraints constraints = modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { unificationConstraints = constraints, ..}

getTypeClassConstraints :: MonadMeta m => m [TypeClassConstraint]
getTypeClassConstraints = gets typeClassConstraints

addTypeClassConstraint :: (MonadMeta m, MonadLogger m) => TypeClassConstraint -> m ()
addTypeClassConstraint constraint = do
  logDebug ("add-type-class-constraint " <> pretty constraint)
  modifyMetaCtx $ \ MetaCtx {..} ->
     MetaCtx { typeClassConstraints = constraint : typeClassConstraints, ..}

setTypeClassConstraints :: MonadMeta m => [TypeClassConstraint] -> m ()
setTypeClassConstraints constraints = modifyMetaCtx $ \ MetaCtx {..} ->
    MetaCtx { typeClassConstraints = constraints, ..}

getMetaSubstitution :: MonadMeta m => m MetaSubstitution
getMetaSubstitution = gets currentSubstitution

modifyMetaSubstitution :: MonadMeta m => (MetaSubstitution -> MetaSubstitution) -> m ()
modifyMetaSubstitution f = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { currentSubstitution = f currentSubstitution, ..}

freshMetaName :: MonadMeta m => m Meta
freshMetaName = do
  MetaCtx {..} <- get;
  put $ MetaCtx { nextMeta = succ nextMeta , ..}
  return (MetaVar nextMeta)

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMetaWith :: (MonadMeta m, MonadLogger m)
              => BoundCtx
              -> Provenance
              -> m (Meta, CheckedExpr)
freshMetaWith boundCtx p = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create bound variables for everything in the context
  let boundEnv = reverse [ Var p (Bound varIndex) | varIndex <- [0..length boundCtx - 1] ]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' (\fun arg -> App p fun (Arg p Explicit arg)) (Meta p metaName) boundEnv

  logDebug $ "fresh-meta" <+> pretty metaName
  return (metaName, meta)

-- |Creates a Pi type that abstracts over all bound variables
makeMetaType :: BoundCtx
             -> Provenance
             -> CheckedExpr
             -> CheckedExpr
makeMetaType boundCtx p resultType = foldr entryToPi resultType (reverse boundCtx)
  where
    entryToPi :: (Name, CheckedExpr) -> CheckedExpr -> CheckedExpr
    entryToPi (name, t) resTy = Pi p (Binder p Explicit name t) resTy

metaSolved :: (MonadMeta m, MonadLogger m)
           => Provenance
           -> Meta
           -> CheckedExpr
           -> m ()
metaSolved p m e = do
  logDebug $ "solved" <+> pretty m <+> "as" <+> prettyVerbose e

  -- Insert the new variable throwing an error if the meta-variable is already present
  -- (should have been substituted out)
  modifyMetaSubstitution (MetaSubst.insertWith duplicateMetaError m e)
  where
    duplicateMetaError :: CheckedExpr -> CheckedExpr -> a
    duplicateMetaError new old = developerError $
      "meta-variable" <+> pretty m <+> "already assigned" <+> prettyVerbose old <+>
      "and should have been substituted out but it is still present and" <+>
      "was assigned again to" <+> prettyVerbose new <+>
      pretty p

data Progress
  = Stuck
  | Progress
    { newConstraints :: Bool
    , solvedMetas    :: MetaSet
    }

isStuck :: Progress -> Bool
isStuck Stuck = True
isStuck _     = False

instance Semigroup Progress where
  Stuck <> x = x
  x <> Stuck = x
  Progress n1 ms1 <> Progress n2 ms2 = Progress (n1 || n2) (ms1 <> ms2)

instance Monoid Progress where
  mempty = Stuck

pattern Solved :: MetaSet -> Progress
pattern Solved ms = Progress False ms

solved :: Meta -> Progress
solved m = Solved (MetaSet.singleton m)

solvedTrivial :: Progress
solvedTrivial = Solved mempty

pattern PartiallySolved :: MetaSet -> Progress
pattern PartiallySolved ms = Progress True ms

partiallySolved :: Meta -> Progress
partiallySolved m = PartiallySolved (MetaSet.singleton m)