
module Vehicle.Language.AST.Position
  ( PositionTree(..)
  , PositionList(..)
  , BoundVarMap
  , isBoth
  , unnode
  , leafBVM
  , unleafBVM
  , nodeBVM
  , unnodeBVM
  , pop
  , unpop
  , substPos
  , prefixTree
  , prefixOrd
  ) where

import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (zipWith)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.DeBruijn hiding (Free, Bound)
import Vehicle.Language.AST.Utils


--------------------------------------------------------------------------------
-- Core definitions

-- | A position tree points to a series of locations within an expression. A
-- leaf represents the current expression while a node indicates the locations
-- within the sub-expressions of the current expression.
data PositionTree
  = Leaf
  | Node PositionList
  deriving (Show, Eq, Generic)

instance Pretty PositionTree where
  pretty = pretty . show

instance Hashable PositionTree

-- | A position list is used to point at a series of locations within the
-- arguments of a Expr constructor. `Here` points to the first argument,
-- `There` points to the reminder of the arguments, and `Both` points to
-- both.
data PositionList
  = Here  PositionTree
  | There PositionList
  | Both  PositionTree PositionList
  deriving (Show, Eq, Generic)

instance Pretty PositionList where
  pretty = pretty . show

instance Hashable PositionList

instance Semigroup PositionTree where
  Leaf    <> Leaf    = Leaf
  Node l1 <> Node l2 = Node (l1 <> l2)
  _x      <> _y      = developerError "Trying to combine incompatible position trees"

instance Semigroup PositionList where
  Here  t1    <> Here  t2    = Here (t1 <> t2)
  There    l1 <> There    l2 = There (l1 <> l2)
  Both  t1 l1 <> Both  t2 l2 = Both (t1 <> t2) (l1 <> l2)

  Here  t1    <> There    l2 = Both t1 l2
  There    l1 <> Here  t2    = Both t2 l1
  Here  t1    <> Both  t2 l2 = Both (t1 <> t2) l2
  Both  t1 l1 <> Here  t2    = Both (t1 <> t2) l1
  There    l1 <> Both  t2 l2 = Both t2 (l1 <> l2)
  Both  t1 l1 <> There    l2 = Both t1 (l1 <> l2)

isBoth :: PositionTree -> Bool
isBoth Leaf     = False
isBoth (Node l) = go l
  where
    go :: PositionList -> Bool
    go (There l1) = go l1
    go Both{}     = True
    go Here{}     = False

-- | A map from DeBruijn indices to the location that they are used
-- in an expression. If not used, then there is no entry in the map.
type BoundVarMap = IntMap PositionTree

--------------------------------------------------------------------------------
-- Computing prefixes

prefixTree :: PositionTree -> PositionTree -> Maybe PositionTree
prefixTree Leaf      t         = Just t
prefixTree (Node _)  Leaf      = Nothing
prefixTree (Node l1) (Node l2) = prefixList l1 l2

prefixList :: PositionList -> PositionList -> Maybe PositionTree
prefixList (Here  t1)    (Here t2)     = prefixTree t1 t2
prefixList (There l1)    (There l2)    = prefixList l1 l2
prefixList (Here  t1)    (Both t2 _l2) = prefixTree t1 t2
prefixList (There l1)    (Both _t2 l2) = prefixList l1 l2
prefixList (Both  t1 l1) (Both t2 l2)  = prefixTree t1 t2 <> prefixList l1 l2
prefixList _             _             = Nothing

prefixOrd :: PositionTree -> PositionTree -> Maybe Ordering
prefixOrd t1 t2 = case (prefixTree t1 t2, prefixTree t2 t1) of
  (Nothing, Nothing) -> Nothing
  (Just _ , Nothing) -> Just GT
  (Nothing, Just _)  -> Just LT
  (Just _ , Just _)  -> Just EQ

--------------------------------------------------------------------------------
-- Operations

unlist :: PositionList -> [Maybe PositionTree]
unlist (Here t)   = Just t  : repeat Nothing
unlist (There l)  = Nothing : unlist l
unlist (Both t l) = Just t  : unlist l

unnode :: PositionTree -> [Maybe PositionTree]
unnode Leaf     = developerError "Found Leaf when expected Node during traversal of CoDBExpr"
unnode (Node l) = unlist l

pop :: BoundVarMap -> (Maybe PositionTree, BoundVarMap)
pop bvm1 = (mt, bvm3)
  where
    (mt, bvm2) = IntMap.updateLookupWithKey (\_k _v -> Nothing) 0 bvm1
    bvm3       = IntMap.mapKeysMonotonic (\x -> x - 1) bvm2

unpop :: Maybe PositionTree -> BoundVarMap -> BoundVarMap
unpop mt bvm1 = bvm3
  where
    bvm2 = IntMap.mapKeysMonotonic (+ 1) bvm1
    bvm3 = case mt of
      Nothing -> bvm2
      Just t  -> IntMap.insert 0 t bvm2

nodeBVM :: [BoundVarMap] -> BoundVarMap
nodeBVM []   = mempty
nodeBVM bvms = fmap Node (foldr1 merge $ fmap (fmap Here) bvms)
  where
    merge :: IntMap PositionList -> IntMap PositionList -> IntMap PositionList
    merge xs ys = IntMap.unionWith (<>) xs (fmap There ys)

unnodeBVM :: BoundVarMap -> [BoundVarMap]
unnodeBVM bvm
  | null bvm  = repeat mempty
  | otherwise = distrib $ fmap unnode bvm
  where
    -- fromList ((0, [x1, x2]), (2, [y1, y2])
    -- >>> [fromList ((0, x1), (2, y1)), fromList ((0, x2), (2, y2))]
    distrib :: IntMap [Maybe PositionTree] -> [BoundVarMap]
    distrib x = [ IntMap.mapMaybe (\l -> join (l !!? i)) x | i <- [0..]]

leafBVM :: DBIndex -> BoundVarMap
leafBVM i = IntMap.singleton i Leaf

unleafBVM :: BoundVarMap -> DBIndex
unleafBVM bvm = case IntMap.minViewWithKey bvm of
  Just ((i, Leaf), bvm')
    | null bvm' -> i
  _ -> developerError $ "Expecting a singleton BoundVarMap with a leaf node" <+>
                        "for Var but found" <+> pretty (show bvm)

--------------------------------------------------------------------------------
-- Substitution

substPos :: CheckedExpr -> Maybe PositionTree -> CheckedExpr -> CheckedExpr
substPos _ Nothing         expr = expr
substPos v (Just Leaf)     _    = v
substPos v (Just (Node l)) expr = case (expr, unlist l) of
  (Type{}    , _) -> invalidPositionTreeError l
  (Hole{}    , _) -> invalidPositionTreeError l
  (PrimDict{}, _) -> invalidPositionTreeError l
  (Meta{}    , _) -> invalidPositionTreeError l
  (Literal{} , _) -> invalidPositionTreeError l
  (Builtin{} , _) -> invalidPositionTreeError l
  (Var{}     , _) -> invalidPositionTreeError l

  (Ann  ann e t, p1 : p2 : _) -> Ann ann (substPos v p1 e) (substPos v p2 t)
  (LSeq ann dict xs, p : ps)  -> LSeq ann (substPos v p dict) (zipWith (substPos v) ps xs)

  (App ann fun args, p1 : p2 : ps) ->
    App ann (substPos v p1 fun) (NonEmpty.zipWith (substPosArg v) (p2 :| ps) args)

  (Pi  ann binder result, p1 : p2 : _) ->
    Pi ann (substPosBinder v p1 binder) (substPos (liftFreeDBIndices 1 v) p2 result)

  (Let ann bound binder body, p1 : p2 : p3 : _) ->
    Let ann (substPos v p1 bound) (substPosBinder v p2 binder) (substPos (liftFreeDBIndices 1 v) p3 body)

  (Lam ann binder body, p1 : p2 : _) ->
    Lam ann (substPosBinder v p1 binder) (substPos (liftFreeDBIndices 1 v) p2 body)

  (_, ps) -> developerError $
    "Expected the same number of PositionTrees as args but found" <+> pretty (length ps)

substPosArg :: CheckedExpr -> Maybe PositionTree -> CheckedArg -> CheckedArg
substPosArg v p = mapArgExpr (substPos v p)

substPosBinder :: CheckedExpr -> Maybe PositionTree -> CheckedBinder -> CheckedBinder
substPosBinder v p = mapBinderType (substPos v p)

invalidPositionTreeError :: PositionList -> a
invalidPositionTreeError l = developerError $
  "Whilst performing a positional substitution expected a Leaf" <+>
  "but found" <+> squotes (pretty l)
