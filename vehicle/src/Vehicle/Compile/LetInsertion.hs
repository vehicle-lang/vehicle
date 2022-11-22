module Vehicle.Compile.LetInsertion
  ( insertLets
  ) where

import Control.Monad.Reader (MonadReader (..), asks, runReaderT)
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap qualified as IntMap
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as Map
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe, mapMaybe)
import Prettyprinter (list)

import Data.Hashable (Hashable (hash))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Expr.AlphaEquivalence
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.Conversion
import Vehicle.Expr.CoDeBruijn.PositionTree

-- | Let-lifts any sub-expressions that matches the provided filter
-- to the highest possible level. Filter takes in the expression
-- and the quantity found.
insertLets :: MonadLogger m
           => (CheckedCoDBExpr -> Int -> Bool)
           -> Bool
           -> CheckedExpr
           -> m CheckedExpr
insertLets subexprFilter liftOverBinders expr =
  logCompilerPass MinDetail "let insertion" $ do
    result <- runReaderT applyInsert (subexprFilter, liftOverBinders)
    logCompilerPassOutput (prettyFriendly result)
    return result
  where
    applyInsert :: MonadLetInsert m => m CheckedExpr
    applyInsert = do
      (result, sm) <- letInsert (toCoDBExpr expr)
      -- Any remaining subexpressions must involve free variables and therefore
      -- we can bind them here at the top level.
      (letBoundResult, _) <- letBindSubexpressions Map.empty (Map.elems sm) result
      return (fromCoDB letBoundResult)

--------------------------------------------------------------------------------
-- Common subexpression identification

-- | Stores information about subexpressions. It is generic in the position
-- parameter purely to make merging them nicer in the `nodeSM` method. Nearly
-- always used as `Subexpression` defined below.
data GenericSubexpression a = CSItem
  { subexpr   :: CheckedCoDBExpr
  , quantity  :: Int
  , positions :: a
  }

instance Show a => Show (GenericSubexpression a) where
  show (CSItem _e q ps) = show q <> " " <> show ps

instance Semigroup a => Semigroup (GenericSubexpression a) where
  (CSItem e1 m ps) <> (CSItem e2 n qs)
    | alphaEq e1 e2  = CSItem e1 (m + n) (ps <> qs)
    | otherwise = developerError $
      "Merging non-identical subexpressions during let lifting:" <> line <>
      indent 2 (pretty (show e1) <> line <> pretty (show e2))

instance Functor GenericSubexpression where
  fmap f (CSItem e m ps) = CSItem e m (f ps)

-- | Stores information about subexpressions.
type Subexpression = GenericSubexpression PositionTree

-- | A partial order over subexpressions based on their position in the AST
subexprPrefixOrder :: Subexpression -> Subexpression -> Maybe Ordering
subexprPrefixOrder e1 e2 = prefixOrd (positions e1) (positions e2)

-- | Common sub-expression map from hashes to expressions.
-- We use a LinkedHashMap rather than a regular map so that expressions are
-- let bound in the order that they occur in the expression.
type SubexpressionMap = LinkedHashMap Int Subexpression

type MonadLetInsert m =
  ( MonadLogger m
  , MonadReader (CheckedCoDBExpr -> Int -> Bool, Bool) m
  )

letInsert :: MonadLetInsert m => CheckedCoDBExpr -> m (CheckedCoDBExpr, SubexpressionMap)
letInsert expr = do
  showIdentEntry expr
  res@(expr', sm) <- case recCoDB expr of
    UniverseC{} -> return (expr, leafSM)
    VarC{}      -> return (expr, leafSM)
    LiteralC{}  -> return (expr, leafSM)
    BuiltinC{}  -> return (expr, leafSM)
    HoleC{}     -> return (expr, leafSM)
    MetaC{}     -> return (expr, leafSM)

    LSeqC ann xs -> do
      ((xs', bvms), sms) <- first unzip <$> (unzip <$> traverse letInsert xs)
      let expr' = (LVec ann xs', nodeBVM bvms)
      return (expr', nodeSM expr' sms)

    AnnC ann e t -> do
      ((e', bvm1), sm1) <- letInsert e
      ((t', bvm2), sm2) <- letInsert t
      let expr' = (Ann ann e' t', nodeBVM [bvm1, bvm2])
      return (expr', nodeSM expr' [sm1, sm2])

    AppC ann fn args -> do
      ((fn',   bvm1), sm1) <- letInsert fn
      ((args', bvms), sms) <- first NonEmpty.unzip <$> (NonEmpty.unzip <$> traverse letInsertArg args)
      let expr' = (App ann fn' args', nodeBVM (bvm1 : NonEmpty.toList bvms))
      return (expr', nodeSM expr' (sm1 : NonEmpty.toList sms))

    PiC ann binder res -> do
      ((res', bvm2), sm2) <- liftOverBinder =<< letInsert res
      let (positionTree, bvm2') = liftBVM bvm2
      ((binder', bvm1), sm1) <- letInsertBinder binder positionTree
      let expr' = (Pi ann binder' res', nodeBVM [bvm1, bvm2'])
      return (expr', nodeSM expr' [sm1, sm2])

    LetC ann bound binder body -> do
      ((body', bvm3), sm3) <- liftOverBinder =<< letInsert body
      let (positionTree, bvm3') = liftBVM bvm3
      ((binder', bvm2), sm2) <- letInsertBinder binder positionTree
      ((bound', bvm1), sm1) <- letInsert bound
      let expr' = (Let ann bound' binder' body', nodeBVM [bvm1, bvm2, bvm3'])
      return (expr', nodeSM expr' [sm1, sm2, sm3])

    LamC ann binder body -> do
      ((body', bvm2), sm2) <- liftOverBinder =<< letInsert body
      let (positionTree, bvm2') = liftBVM bvm2
      ((binder', bvm1), sm1) <- letInsertBinder binder positionTree
      let expr' = (Lam ann binder' body', nodeBVM [bvm1, bvm2'])
      return (expr', nodeSM expr' [sm1, sm2])

  showIdentExit expr' sm
  return res

letInsertBinder :: MonadLetInsert m
                => CheckedCoDBBinder -> Maybe PositionTree
                -> m (CheckedCoDBBinder, SubexpressionMap)
letInsertBinder binder positions = case recCoDB binder of
  (Binder ann v r (CoDBBinding n _) t) ->
    if visibilityOf (fst binder) /= Explicit
        then return (first (Binder ann v r (CoDBBinding n positions)) t, Map.empty)
        else do
          ((t', bvm), sm) <- letInsert t
          return ((Binder ann v r (CoDBBinding n positions) t', bvm), sm)

letInsertArg :: MonadLetInsert m
             => CheckedCoDBArg
             -> m (CheckedCoDBArg, SubexpressionMap)
letInsertArg arg = if visibilityOf (fst arg) /= Explicit
  then return (arg, Map.empty)
  else case recCoDB arg of
    (Arg ann r v e) -> do
      ((e', bvm), sm) <- letInsert e
      return ((Arg ann r v e', bvm), sm)

liftOverBinder :: MonadLetInsert m
               => (CheckedCoDBExpr, SubexpressionMap)
               -> m (CheckedCoDBExpr, SubexpressionMap)
liftOverBinder (body, sm) = do
  liftOverBinders <- asks snd
  -- Obtain the subexpressions that need to be inserted before the binder.
  let (insertSM, remainingSM) =
        if not liftOverBinders
          then (sm, Map.empty)
          else partitionMap shouldInsertHere sm

  let subexprsToInsert = Map.elems insertSM

  -- Let bind those subexpressions.
  (updatedBody, updatedRemainingSM) <- letBindSubexpressions remainingSM subexprsToInsert body

  -- Lift the remaining subexpressions in preparation for going over the binder.
  let liftedSM = fmap liftCommonSubexpr updatedRemainingSM

  -- Return the result
  return (updatedBody, liftedSM)
  where
    shouldInsertHere :: GenericSubexpression PositionTree -> Bool
    shouldInsertHere item = 0 `IntMap.member` snd (subexpr item)

    liftCommonSubexpr :: Subexpression -> Subexpression
    liftCommonSubexpr (CSItem e q p) = CSItem (liftFreeCoDBIndices e) q p

filterItem :: (CheckedCoDBExpr -> Int -> Bool) -> Subexpression -> Bool
filterItem subexprFilter (CSItem subexpr quantity _) = subexprFilter subexpr quantity

letBindSubexpressions :: MonadLetInsert m
                      => SubexpressionMap
                      -> [Subexpression]
                      -> CheckedCoDBExpr
                      -> m (CheckedCoDBExpr, SubexpressionMap)
letBindSubexpressions remainingSM subexprsToInsert expr
  | null subexprsToInsert = return (expr, remainingSM)
  | otherwise             = do
    -- Filter the subexpressions using the provided filter.
    exprFilter <- asks fst
    let filteredSubexprs = filter (filterItem exprFilter) subexprsToInsert

    -- Sort the subexpressions by prefix order so we insert the "larger" ones first.
    let sortedSubexprs = partialSort subexprPrefixOrder filteredSubexprs

    (updatedSM, updatedExpr) <- logCompilerPass MaxDetail "insertion" $ do
      go remainingSM sortedSubexprs expr

    return (updatedExpr, updatedSM)
  where
    go :: MonadLetInsert m
       => SubexpressionMap -> [Subexpression] -> CheckedCoDBExpr
       -> m (SubexpressionMap, CheckedCoDBExpr)
    go sm []                body = return (sm, body)
    go sm (cs : css) body = do
      let ann = provenanceOf (fst (subexpr cs))
      logDebug MaxDetail $ "inserting" <+> prettyEntry body cs
      incrCallDepth

      logDebug MaxDetail $ "body-before:" <+> prettySimple (fromCoDB body)
      -- Everywhere the position tree points to, substitute a variable through
      -- which refers to the let binding that is about to be inserted.
      let coDBVar = (Var ann CoDBBound, leafBVM 0)
      let substBody = substPos coDBVar (Just (positions cs)) (lowerFreeCoDBIndices body)
      -- Wrap the substituted body in a let binding
      let updatedBody = prependLet ann cs substBody
      logDebug MaxDetail $ "body-after: " <+> prettySimple (fromCoDB updatedBody)

      -- Update the remaining subexpressions that are not going to be inserted here,
      -- to take into account the updated form of the body.
      subexprFilter <- asks fst
      logDebug MaxDetail $ "SM-before:" <+> prettySM body subexprFilter sm
      let updatedSM = fmap (updateSubexpression (positions cs)) sm
      logDebug MaxDetail $ "SM-after: " <+> prettySM updatedBody subexprFilter updatedSM

      -- Again update the remaining subexpressions to be inserted here,
      -- to take into account the updated form of the body.
      logDebug MaxDetail $ "S-before:" <+> prettyCommonSubExprs body css
      let updatedCSs = fmap (updateSubexpression (positions cs)) css
      logDebug MaxDetail $ "S-after: " <+> prettyCommonSubExprs updatedBody updatedCSs

      decrCallDepth

      -- Recursively insert the remaining subexpressions.
      go updatedSM updatedCSs updatedBody

    updateSubexpression :: PositionTree -> Subexpression -> Subexpression
    updateSubexpression p1 (CSItem v2 q p2) =
      let (remainder, suffix) = stripPrefix p1 p2 in
      -- The position (if any) in the subexpression bound in the newly inserted let
      let letPosition   = listToMaybe suffix in
      -- The updated positions in body of the newly inserted let
      let bodyPositions = There . Here <$> remainder in
      case mergeBoth letPosition bodyPositions of
        Nothing           -> developerError "Unexpectedly disjoint position trees"
        Just newPositions -> CSItem v2 q (Node newPositions)

    prependLet :: Provenance -> Subexpression -> CheckedCoDBExpr -> CheckedCoDBExpr
    prependLet ann cs (letBody, bvm3) =
      let (pt, bvm3')   = liftBVM bvm3 in
      let (bound, bvm1) = subexpr cs in
      let exprType      = Hole ann "?" in
      let binding       = CoDBBinding Nothing pt in
      let binder        = ExplicitBinder ann binding exprType in
      let bvm2          = mempty in
      (Let ann bound binder letBody, nodeBVM [bvm1, bvm2, bvm3'])

leafSM :: SubexpressionMap
leafSM = Map.empty

nodeSM :: CheckedCoDBExpr -> [SubexpressionMap] -> SubexpressionMap
nodeSM e sms =
  -- Merge the maps together
  let mergedCSIMap = fmap (fmap Node) (foldr merge Map.empty $ fmap (fmap (fmap Here)) sms) in

  -- Add the current node to the map
  let eHash = hash e in
  let item  = CSItem e 1 Leaf in
  Map.insertWith (duplicateError e sms) eHash item mergedCSIMap
  where
    merge :: LinkedHashMap Int (GenericSubexpression PositionList)
          -> LinkedHashMap Int (GenericSubexpression PositionList)
          -> LinkedHashMap Int (GenericSubexpression PositionList)
    merge xs ys = Map.unionWith (<>) xs (fmap (fmap There) ys)

duplicateError :: CoDBExpr
               -> [SubexpressionMap]
               -> Subexpression
               -> Subexpression
               -> Subexpression
duplicateError e maps _ _ = developerError $
  "During let-lifting found duplicate sub-expression" <+> squotes (prettyVerbose e) <+>
  "containing itself..." <> line <>
  "Hash code =" <+> pretty (hash e) <> line <>
  "Maps = " <+> pretty (show maps)

showIdentEntry :: MonadLetInsert m => CheckedCoDBExpr -> m ()
showIdentEntry e = do
  logDebug MaxDetail ("letInsert-entry " <> align (prettySimple (fromCoDB e)))
  incrCallDepth

showIdentExit :: MonadLetInsert m => CheckedCoDBExpr -> SubexpressionMap -> m ()
showIdentExit expr sm = do
  decrCallDepth
  subexprFilter <- asks fst
  logDebug MaxDetail ("letInsert-exit " <+> align (
      prettySimple (fromCoDB expr) <+> " |=" <> softline <>
      prettySM expr subexprFilter sm))

prettyEntry :: CheckedCoDBExpr
            -> Subexpression
            -> Doc a
prettyEntry expr item =
  prettySimple (fromCoDB (subexpr item)) <+> "->" <+> prettySimple (PositionsInExpr expr (positions item))

prettyItem :: CheckedCoDBExpr
           -> (CheckedCoDBExpr -> Int -> Bool)
           -> Subexpression
           -> Maybe (Doc a)
prettyItem expr subexprFilter item = if filterItem subexprFilter item
  then Just $ pretty (quantity item) <+> "of" <+> prettyEntry expr item
  else Nothing

prettySM :: CheckedCoDBExpr -> (CheckedCoDBExpr -> Int -> Bool) -> SubexpressionMap -> Doc a
prettySM expr subexprFilter sm =
  pretty (layoutAsText <$> mapMaybe (prettyItem expr subexprFilter) (Map.elems sm))

prettyCommonSubExprs :: CheckedCoDBExpr -> [Subexpression] -> Doc a
prettyCommonSubExprs expr cses = list (fmap (prettyEntry expr) cses)

partitionMap :: Hashable k => (a -> Bool) -> LinkedHashMap k a -> (LinkedHashMap k a, LinkedHashMap k a)
partitionMap p xs = do
  let (pass, notPass) = List.partition (p . snd) $ Map.toList xs
  (Map.fromList pass, Map.fromList notPass)
