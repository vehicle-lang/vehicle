module Vehicle.Compile.Lift.LetExpr
  ( letLift
  ) where

import Control.Monad (zipWithM, join)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Hashable (Hashable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (mapMaybe)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Compile.CoDeBruijnify

letLift :: MonadLogger m => (CheckedCoDBExpr -> Bool) -> CheckedExpr -> m CheckedExpr
letLift exprFilter dbExpr = do
  logDebug $ line <> "Beginning common subexpression analysis"
  incrCallDepth

  let cdbExpr :: CoDBExpr CheckedAnn = toCoDBExpr dbExpr
  csMap <- runReaderT (findCSs cdbExpr) exprFilter

  logDebug $ line <> "Subexpressions found:" <+> pretty (show csMap)

  let cses = mapMaybe convertCSItem (IntMap.elems csMap)

  logDebug $ line <> "Subexpressions to remove:" <+> prettySimple cses

  result <- elimCSE cses dbExpr

  decrCallDepth
  logDebug $ line <> "Finished common subexpression analysis"
  return result


--------------------------------------------------------------------------------
-- Common subexpression identification

type CheckedCoDBExpr = CoDBExpr CheckedAnn

data CSIItem a = CSItem
  { expr      :: CheckedCoDBExpr
  , quantity  :: Int
  , positions :: a
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

type MonadCSIdent m = (MonadLogger m, MonadReader (CheckedCoDBExpr -> Bool) m)

class CommonSubexpressionIdentification a where
  findCSs :: MonadCSIdent m => a -> m (CSIMap PositionTree)

instance CommonSubexpressionIdentification CheckedCoDBExpr where
  findCSs expr = do
    showEntry expr
    res <- case (recCoDB expr :: ExprC CheckedAnn) of
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

instance CommonSubexpressionIdentification (CoDBBinder CheckedAnn) where
  findCSs binder = case (recCoDB binder :: BinderC CheckedAnn) of
    (BinderC _ _ _ t) -> findCSs t

instance CommonSubexpressionIdentification (CoDBArg CheckedAnn) where
  findCSs arg = case (recCoDB arg :: ArgC CheckedAnn) of
    (ArgC _ _ e) -> findCSs e

mkLeaf :: MonadCSIdent m => m (CSIMap PositionTree)
mkLeaf = return mempty

mkNode :: MonadCSIdent m => CoDBExpr CheckedAnn -> [m (CSIMap PositionTree)] -> m (CSIMap PositionTree)
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
      then IntMap.insertWith (duplicateError e csMaps') (hash (mkHashable e)) (CSItem e 1 Leaf) maps4
      else maps4

  -- Return the result
  return maps5

duplicateError :: CoDBExpr ann
               -> [CSIMap PositionTree]
               -> CSIItem PositionTree
               -> CSIItem PositionTree
               -> CSIItem PositionTree
duplicateError e maps _ _ = let e' = mkHashable e in developerError $
  "During let-lifting found duplicate sub-expression" <+> squotes (pretty (show e')) <+>
  "containing itself..." <> line <>
  "Hash code =" <+> pretty (hash e') <> line <>
  "Maps = " <+> pretty (show maps)

showEntry :: MonadCSIdent m => CheckedCoDBExpr -> m ()
showEntry e = do
  logDebug ("cs-ident-entry " <> prettyVerbose e)
  incrCallDepth

showExit :: MonadCSIdent m => CSIMap PositionTree -> m ()
showExit _maps = do
  decrCallDepth
  logDebug "cs-ident--exit "

--------------------------------------------------------------------------------
-- Common subexpression lifting

type CommonSubExprs = [(PositionTree, CheckedExpr)]

convertCSItem :: CSIItem PositionTree -> Maybe (PositionTree, CheckedExpr)
convertCSItem item
  | quantity item <= 1 = Nothing
  | otherwise          = Just (positions item, fromCoDB (expr item))

removePrefix :: PositionTree -> PositionTree -> PositionTree
removePrefix _remove p = p -- TODO

unnodeCSEs :: CommonSubExprs -> [CommonSubExprs]
unnodeCSEs cses
  | null cses = repeat mempty
  | otherwise = distrib $ fmap (first unnode) cses
  where
    distrib :: [([Maybe PositionTree], CheckedExpr)] -> [CommonSubExprs]
    distrib x = [ mapMaybe (\(xs,e) -> fmap (,e) (join (xs !!? i))) x | i <- [0..]]


-- CSE (x + y) x y

-- ((x + y) / (x + y)) + y

-- let y = ... in let x = ... in let z = x + y in z / z + y

insertLets :: CommonSubExprs -> CommonSubExprs -> CheckedExpr
           -> (CommonSubExprs, CheckedExpr, CheckedExpr -> CheckedExpr)
insertLets acc []              e = (acc, e, id)
insertLets acc ((p, v) : cses) e
  | not (isBoth p) = insertLets ((p, v) : acc) cses e
  | otherwise      =
    let ann   = annotationOf v in
    let e'    = substPos (Var ann (Bound 0)) (Just p) e in
    let acc'  = fmap (first (removePrefix p)) acc in
    let cses' = fmap (first (removePrefix p)) cses in
    let (cses'', e'', appendLets) = insertLets acc' cses' e' in
    -- This is a hack. May have to call the type-checker here?
    let t = Hole ann "?" in
    let appendLets' = appendLets . Let ann v (ExplicitBinder ann Nothing t) in
    (cses'', e'', appendLets')

elimCSE :: MonadLogger m => CommonSubExprs -> CheckedExpr -> m CheckedExpr
elimCSE []   expr = return expr
elimCSE cses expr = do
  let (cses', expr', prependLets) = insertLets [] cses expr

  result <- case (expr', unnodeCSEs cses') of
    (Type{}   , _) -> return expr
    (Var{}    , _) -> return expr
    (Literal{}, _) -> return expr
    (Builtin{}, _) -> return expr
    (Hole{}   , _) -> return expr
    (Meta{}   , _) -> return expr

    (Seq ann xs, cse)          -> Seq ann <$> zipWithM elimCSE cse xs
    (PrimDict ann e, cse1 : _) -> PrimDict ann <$> elimCSE cse1 e

    (Ann ann e t, cse1 : cse2 : _) ->
      Ann ann <$> elimCSE cse1 e <*> elimCSE cse2 t

    (App ann fn args, cse1 : cse2 : cse) -> do
      let args' = NonEmpty.zipWith elimCSEArg (cse2 :| cse) args
      App ann <$> elimCSE cse1 fn <*> sequence args'

    (Pi ann binder res, cse1 : cse2 : _) ->
      Pi ann <$> elimCSEBinder cse1 binder <*> elimCSE (liftOverBinder cse2) res

    (Let ann bound binder body, cse1 : cse2 : cse3 : _) ->
      Let ann <$> elimCSE cse1 bound <*> elimCSEBinder cse2 binder <*> elimCSE (liftOverBinder cse3) body

    (Lam ann binder body, cse1 : cse2 : _) ->
      Lam ann <$> elimCSEBinder cse1 binder <*> elimCSE (liftOverBinder cse2) body

    (_, cse) -> developerError $
      "Expected the same number of CommonSubExprs as args but found" <+> pretty (length cse)

  return $ prependLets result

elimCSEBinder :: MonadLogger m => CommonSubExprs -> CheckedBinder -> m CheckedBinder
elimCSEBinder emap = traverseBinderType (elimCSE emap)

elimCSEArg :: MonadLogger m => CommonSubExprs -> CheckedArg -> m CheckedArg
elimCSEArg emap = traverseArgExpr (elimCSE emap)

liftOverBinder :: CommonSubExprs -> CommonSubExprs
liftOverBinder = fmap $ second $ liftDBIndices 1