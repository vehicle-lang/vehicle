module Vehicle.Compile.Lift.Lets
  ( letLift
  ) where

import Data.Maybe (mapMaybe)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List (find)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Hashable (Hashable(..))
import Data.Bifunctor (Bifunctor(..))

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Compile.CoDeBruijnify

letLift :: MonadLogger m => (CoDBExpr () -> Bool) -> CheckedExpr -> m CheckedExpr
letLift exprFilter dbExpr = do
  logDebug "Beginning let-lifting"
  let cdbExpr :: CoDBExpr CheckedAnn = toCodebruijn dbExpr

  logDebug $ line <> "Beginning common subexpression identification"
  incrCallDepth
  let hashableCoDBExpr = mkHashable cdbExpr
  csMap <- runReaderT (findCSs hashableCoDBExpr) exprFilter
  decrCallDepth

  logDebug $ line <> "Beginning common subexpression elimination"
  developerError ("CSMap:" <+> pretty (show csMap))

--------------------------------------------------------------------------------
-- Common subexpression identification

data CSIItem a = CSItem
  { _expr      :: CoDBExpr ()
  , _quantity  :: Int
  , _positions :: a
  }

instance Show a => Show (CSIItem a) where
  show (CSItem _e q ps) = show q <> " " <> show ps

instance Semigroup a => Semigroup (CSIItem a) where
  (CSItem e1 m ps) <> (CSItem e2 n qs)
    | e1 == e2  = CSItem e1 (m + n) (ps <> qs)
    | otherwise = developerError $
      "Merging non-identical subexpressions during let lifting:" <> line <>
      indent 2 (pretty (show e1) <> line <> pretty (show e2))

instance Functor CSIItem where
  fmap f (CSItem e m ps) = CSItem e m (f ps)

-- | Common sub-expression map from hashes to expressions
type CSIMap a = IntMap (CSIItem a)

merge :: CSIMap PositionList -> CSIMap PositionList -> CSIMap PositionList
merge xs ys = IntMap.unionWith (<>) xs (fmap (fmap There) ys)

type MonadCSIdent m = (MonadLogger m, MonadReader (CoDBExpr () -> Bool) m)

class CommonSubexpressionIdentification a where
  findCSs :: MonadCSIdent m => a -> m (CSIMap PositionTree)

instance CommonSubexpressionIdentification (CoDBExpr ()) where
  findCSs expr = do
    showEntry expr
    res <- case (recCoDB expr :: ExprC ()) of
      TypeC{}                       -> mkLeaf
      VarC{}                        -> mkLeaf
      LiteralC{}                    -> mkLeaf
      BuiltinC{}                    -> mkLeaf
      HoleC{}                       -> mkLeaf
      MetaC{}                       -> mkLeaf
      AnnC      _ e t               -> mkNode expr (fmap findCSs [e, t])
      AppC      _ fn args           -> mkNode expr (findCSs fn : fmap findCSs (NonEmpty.toList args))
      PiC       _ binder res        -> mkNode expr [findCSs binder, findCSs res]
      LetC      _ bound binder body -> mkNode expr [findCSs bound, findCSs binder, findCSs body]
      LamC      _ binder body       -> mkNode expr [findCSs binder, findCSs body]
      SeqC      _ xs                -> mkNode expr (fmap findCSs xs)
      PrimDictC _ e                 -> mkNode expr [findCSs e]
    showExit res
    return res

instance CommonSubexpressionIdentification (CoDBBinder ()) where
  findCSs binder = case (recCoDB binder :: BinderC ()) of
    (BinderC _ _ _ t) -> findCSs t

instance CommonSubexpressionIdentification (CoDBArg ()) where
  findCSs arg = case (recCoDB arg :: ArgC ()) of
    (ArgC _ _ e) -> findCSs e

mkLeaf :: MonadCSIdent m => m (CSIMap PositionTree)
mkLeaf = return mempty

mkNode :: MonadCSIdent m => CoDBExpr () -> [m (CSIMap PositionTree)] -> m (CSIMap PositionTree)
mkNode e maps1 = do
  logDebug ("node: " <> prettyFriendly e)

  -- Update the maps
  csMaps' <- sequence maps1
  let maps2 = fmap (fmap (fmap Here)) csMaps'
  let maps3 = foldr1 merge maps2
  let maps4 = fmap (fmap Node) maps3

  -- Check if we should inser the current expression
  exprFilter <- ask
  let maps5 = if exprFilter e
      then IntMap.insertWith (duplicateError e csMaps') (hash e) (CSItem e 1 Leaf) maps4
      else maps4

  -- Return the result
  return maps5

duplicateError :: CoDBExpr ()
               -> [CSIMap PositionTree]
               -> CSIItem PositionTree
               -> CSIItem PositionTree
               -> CSIItem PositionTree
duplicateError e maps _ _ = developerError $
  "During let-lifting found duplicate sub-expression" <+> squotes (pretty (show e)) <+>
  "containing itself..." <> line <>
  "Hash code =" <+> pretty (hash e) <> line <>
  "Maps = " <+> pretty (show maps)

showEntry :: MonadCSIdent m => CoDBExpr () -> m ()
showEntry e = do
  logDebug ("cs-ident-entry " <> pretty (show e))
  incrCallDepth

showExit :: MonadCSIdent m => CSIMap PositionTree -> m ()
showExit _maps = do
  decrCallDepth
  logDebug "cs-ident--exit "
{-
--------------------------------------------------------------------------------
-- Common subexpression lifting

type LiftingMap = [(PositionTree, Maybe DBIndex)]

type MonadCSElim m = (MonadLogger m)

class CommonSubexpressionElimination a where
  elimCSs :: MonadCSElim m => LiftingMap -> a -> m a

instance CommonSubexpressionElimination CheckedExpr where
  elimCSs emap expr = do
    let maybeReplacementIndex = findHere emap
    case maybeReplacementIndex of
      Just i  -> Var (annotationOf expr) (Bound i)
      Nothing -> do
        liftedExprs <- findExpressionsToLift

        result <-
          case expr of
          Type{}                         -> return expr
          Var{}                          -> return expr
          Literal{}                      -> return expr
          Builtin{}                      -> return expr
          Hole{}                         -> return expr
          Meta{}                         -> return expr
          Ann      ann e t               -> do
            Ann ann <$> elimCSs smap rmap e <*> elimCSs smap rmap t

          App ann fn args           -> do
            App ann fn args --(findCSs fn : fmap findCSs (NonEmpty.toList args))

          Pi       ann binder res        -> do
            [findCSs binder, findCSs res]
          Let      ann bound binder body ->  [findCSs bound, findCSs binder, findCSs body]
          Lam      ann binder body       ->  [findCSs binder, findCSs body]
          Seq      ann xs                ->  (fmap findCSs xs)
          PrimDict ann e                 ->  [findCSs e]

        return result

instance CommonSubexpressionElimination CheckedBinder where
  elimCSs smap rmap = do
    let rmap' = fmap (second (+ 1)) rmap
    traverseBinderType (elimCSs smap rmap')

instance CommonSubexpressionElimination CheckedArg where
  elimCSs smap rmap = traverseArgExpr (elimCSs smap rmap)

findHere :: LiftingMap -> Maybe DBIndex
findHere lmap = find (const True) (mapMaybe indexHit lmap)
  where
    indexHit :: (PositionTree, Maybe DBIndex) -> Maybe DBIndex
    indexHit (Leaf, x) = x
    indexHit _         = Nothing

overBinder :: LiftingMap -> LiftingMap
overBinder = fmap $ second $ fmap (+1)
-}