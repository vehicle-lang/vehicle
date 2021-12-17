module Vehicle.Compile.LetInsertion
  ( insertLets
  ) where

import Control.Monad (zipWithM, join)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Bifunctor (Bifunctor(..))
import Data.Maybe (mapMaybe)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Compile.CoDeBruijnify
import Vehicle.Compile.AlphaEquivalence

insertLets :: MonadLogger m => (CheckedCoDBExpr -> Bool) -> CheckedExpr -> m CheckedExpr
insertLets exprFilter dbExpr = do
  logDebug $ line <> "Beginning let insertion"
  incrCallDepth

  let cdbExpr :: CoDBExpr CheckedAnn = toCoDBExpr dbExpr
  csMap <- runReaderT (findCSs cdbExpr) exprFilter

  logDebug $ line <> "Subexpressions found:" <+> pretty (show csMap)

  let cses = mapMaybe convertCSItem (IntMap.elems csMap)
  let sortedCSEs = partialSort (\a b -> prefixOrd (snd a) (snd b)) cses

  logDebug $ line <> "Let expressions to insert:" <+> prettySimple cses <> line

  result <- elimCSE sortedCSEs dbExpr

  decrCallDepth
  logDebug $ line <> "Finished let insertion"
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
    | alphaEq e1 e2  = CSItem e1 (m + n) (ps <> qs)
    | otherwise = developerError $
      "Merging non-identical subexpressions during let lifting:" <> line <>
      indent 2 (pretty (show e1) <> line <> pretty (show e2))

instance Functor CSIItem where
  fmap f (CSItem e m ps) = CSItem e m (f ps)

-- | Common sub-expression map from hashes to expressions
type CSIMap = IntMap (CSIItem PositionTree)

type MonadCSIdent m = (MonadLogger m, MonadReader (CheckedCoDBExpr -> Bool) m)

class CommonSubexpressionIdentification a where
  findCSs :: MonadCSIdent m => a -> m CSIMap

instance CommonSubexpressionIdentification CheckedCoDBExpr where
  findCSs expr = do
    showIdentEntry expr
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
      SeqC      _ dict xs           -> mkNode expr (findCSs dict : fmap findCSs xs)
      PrimDictC _ e                 -> mkNode expr [findCSs e]
    showIdentExit res
    return res

instance CommonSubexpressionIdentification (CoDBBinder CheckedAnn) where
  findCSs binder = if visibilityOf (fst binder) /= Explicit
    then return mempty
    else case (recCoDB binder :: BinderC CheckedAnn) of
      (BinderC _ _ _ t) -> findCSs t

instance CommonSubexpressionIdentification (CoDBArg CheckedAnn) where
  findCSs arg = if visibilityOf (fst arg) /= Explicit
    then return mempty
    else case (recCoDB arg :: ArgC CheckedAnn) of
    (ArgC _ _ e) -> findCSs e

mkLeaf :: MonadCSIdent m => m CSIMap
mkLeaf = return mempty

mkNode :: MonadCSIdent m => CoDBExpr CheckedAnn -> [m CSIMap] -> m CSIMap
mkNode e mCSIMaps = do
  logDebug ("node: " <> align (prettyVerbose e))

  -- Update the maps
  csiMaps <- sequence mCSIMaps
  let mergedCSIMap = nodeCSIMap csiMaps

  -- Check if we should insert the current expression
  exprFilter <- ask
  let maps5 = if not (exprFilter e)
      then mergedCSIMap
      else
        let eHash = hashCoDBExpr e in
        let item  = CSItem e 1 Leaf in
        IntMap.insertWith (duplicateError e csiMaps) eHash item mergedCSIMap

  -- Return the result
  return maps5

nodeCSIMap :: [CSIMap] -> CSIMap
nodeCSIMap []   = mempty
nodeCSIMap csis = fmap (fmap Node) (foldr1 merge $ fmap (fmap (fmap Here)) csis)
  where
    merge :: IntMap (CSIItem PositionList)
          -> IntMap (CSIItem PositionList)
          -> IntMap (CSIItem PositionList)
    merge xs ys = IntMap.unionWith (<>) xs (fmap (fmap There) ys)

duplicateError :: CoDBExpr ann
               -> [CSIMap]
               -> CSIItem PositionTree
               -> CSIItem PositionTree
               -> CSIItem PositionTree
duplicateError e maps _ _ = developerError $
  "During let-lifting found duplicate sub-expression" <+> squotes (prettyVerbose e) <+>
  "containing itself..." <> line <>
  "Hash code =" <+> pretty (hashCoDBExpr e) <> line <>
  "Maps = " <+> pretty (show maps)

showIdentEntry :: MonadCSIdent m => CheckedCoDBExpr -> m ()
showIdentEntry e = do
  logDebug ("cs-ident-entry " <> align (prettySimple e))
  incrCallDepth

showIdentExit :: MonadCSIdent m => CSIMap -> m ()
showIdentExit maps = do
  decrCallDepth
  logDebug ("cs-ident-exit " <> align (prettySimple (fmap csItemToPair (IntMap.elems maps))))

--------------------------------------------------------------------------------
-- Common subexpression lifting

type CommonSubExprs = [(CheckedExpr, PositionTree)]

csItemToPair :: CSIItem PositionTree -> (CheckedExpr, PositionTree)
csItemToPair item = (fromCoDB (expr item), positions item)

convertCSItem :: CSIItem PositionTree -> Maybe (CheckedExpr, PositionTree)
convertCSItem item
  | quantity item <= 1 = Nothing
  | otherwise          = Just (csItemToPair item)

-- CSE (x + y) x y

-- ((x + y) / (x + y)) + y

-- let y = ... in (let x = ... in let z = x + y in z / z) + y

insertRelevantLets :: CommonSubExprs -> CommonSubExprs -> CheckedExpr
                   -> (CommonSubExprs, CheckedExpr, CheckedExpr -> CheckedExpr)
insertRelevantLets acc []              e = (acc, e, id)
insertRelevantLets acc ((v, p) : cses) e
  | not (isBoth p) = insertRelevantLets ((v, p) : acc) cses e
  | otherwise      =
    let ann   = annotationOf v in
    let e'    = substPos (Var ann (Bound 0)) (Just p) e in

    -- Find every other subexpression e' and accompanying position tree p'
    -- if p is a prefix of p' then there exists a series of paths p''
    -- which are
    -- Get list of position trees ps' = removePrefix p p'
    -- For element p'' in ps',
    let acc'  = fmap (second (removePrefix p)) acc in
    let cses' = fmap (second (removePrefix p)) cses in

    let (cses'', e'', appendLets) = insertRelevantLets acc' cses' e' in
    -- This is a hack. May have to call the type-checker here?
    let t = Hole ann "?" in
    let appendLets' = appendLets . Let ann v (ExplicitBinder ann Nothing t) in
    (cses'', e'', appendLets')

elimCSE :: MonadLogger m => CommonSubExprs -> CheckedExpr -> m CheckedExpr
elimCSE []   expr = return expr
elimCSE cses expr = do
  showElimEntry cses expr
  let (cses', expr', prependLets) = insertRelevantLets [] cses expr

  expr'' <- case (expr', unnodeCSEs cses') of
    (Type{}   , _) -> return expr
    (Var{}    , _) -> return expr
    (Literal{}, _) -> return expr
    (Builtin{}, _) -> return expr
    (Hole{}   , _) -> return expr
    (Meta{}   , _) -> return expr

    (LSeq ann dict xs, cse1 : cse) -> LSeq ann <$> elimCSE cse1 dict <*> zipWithM elimCSE cse xs
    (PrimDict ann e, cse1 : _)     -> PrimDict ann <$> elimCSE cse1 e

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

  let exprWithLets = prependLets expr''
  showElimExit exprWithLets
  return exprWithLets

elimCSEBinder :: MonadLogger m => CommonSubExprs -> CheckedBinder -> m CheckedBinder
elimCSEBinder emap = traverseBinderType (elimCSE emap)

elimCSEArg :: MonadLogger m => CommonSubExprs -> CheckedArg -> m CheckedArg
elimCSEArg emap = traverseArgExpr (elimCSE emap)

liftOverBinder :: CommonSubExprs -> CommonSubExprs
liftOverBinder = id -- fmap $ second $ liftFreeDBIndices 1

removePrefix :: PositionTree -> PositionTree -> PositionTree
removePrefix _remove p = p -- TODO

unnodeCSEs :: CommonSubExprs -> [CommonSubExprs]
unnodeCSEs cses
  | null cses = repeat mempty
  | otherwise = distrib $ fmap (second unnode) cses
  where
    distrib :: [(CheckedExpr, [Maybe PositionTree])] -> [CommonSubExprs]
    distrib x = [ mapMaybe (\(e,xs) -> fmap (e,) (join (xs !!? i))) x | i <- [0..]]

showElimEntry :: MonadLogger m => CommonSubExprs -> CheckedExpr -> m ()
showElimEntry cses e = do
  logDebug ("cs-elim-entry " <> prettySimple e <+> "||||" <+> prettySimple cses)
  incrCallDepth

showElimExit :: MonadLogger m => CheckedExpr -> m ()
showElimExit e = do
  decrCallDepth
  logDebug ("cs-ident--exit " <> prettySimple e)
