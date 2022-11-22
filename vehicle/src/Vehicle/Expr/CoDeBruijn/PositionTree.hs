
module Vehicle.Expr.CoDeBruijn.PositionTree
  ( PositionTree(..)
  , PositionList(..)
  , BoundVarMap
  , isBoth
  , mergeBoth
  , unnode
  , unlist
  , leafBVM
  , unleafBVM
  , nodeBVM
  , unnodeBVM
  , liftBVM
  , lowerBVM
  , stripPrefix
  , prefixOrd
  ) where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import GHC.Generics (Generic)

import Vehicle.Expr.DeBruijn (DBIndex)
import Vehicle.Prelude


--------------------------------------------------------------------------------
-- Data

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

mergeBoth :: Maybe PositionTree -> Maybe PositionList -> Maybe PositionList
mergeBoth Nothing  Nothing  = Nothing
mergeBoth (Just t) Nothing  = Just $ Here t
mergeBoth Nothing  (Just l) = Just $ There l
mergeBoth (Just t) (Just l) = Just $ Both t l

--------------------------------------------------------------------------------
-- Computing prefixes

-- | Strips the first PositionTree from the second PositionTree. Returns a
-- pair containing@
--   1) the second tree with the positions in the first removed,
--   2) a list of the suffixes in depth-first order removed from the second
--      after that point.
stripPrefix :: PositionTree -> PositionTree -> (Maybe PositionTree, [PositionTree])
stripPrefix Leaf      t         = (Nothing,   [t])
stripPrefix (Node _)  Leaf      = (Just Leaf, [])
stripPrefix (Node l1) (Node l2) = first (Node <$>) (stripPrefixList l1 l2)

stripPrefixList :: PositionList -> PositionList -> (Maybe PositionList, [PositionTree])
stripPrefixList (Here t1)     (Here t2)     = first (Here <$>) (stripPrefix t1 t2)
stripPrefixList (Here _)      (There l2)    = (Just (There l2), [])
stripPrefixList (Here t1)     (Both t2 l2)  = mergeStrip (stripPrefix t1 t2) (Just l2, [])
stripPrefixList (There _)     (Here t2)     = (Just (Here t2), [])
stripPrefixList (There l1)    (There l2)    = first (There <$>) (stripPrefixList l1 l2)
stripPrefixList (There l1)    (Both t2 l2)  = mergeStrip (Just t2, []) (stripPrefixList l1 l2)
stripPrefixList (Both t1 _)   (Here t2)     = first (Here <$>) (stripPrefix t1 t2)
stripPrefixList (Both _ l1)   (There l2)    = first (There <$>) (stripPrefixList l1 l2)
stripPrefixList (Both  t1 l1) (Both t2 l2)  = mergeStrip (stripPrefix t1 t2) (stripPrefixList l1 l2)

mergeStrip :: (Maybe PositionTree, [PositionTree])
           -> (Maybe PositionList, [PositionTree])
           -> (Maybe PositionList, [PositionTree])
mergeStrip (mt, xs) (ml, ys) = (mergeBoth mt ml, xs <> ys)

prefixOrd :: PositionTree -> PositionTree -> Maybe Ordering
prefixOrd t1 t2 = case (snd (stripPrefix t1 t2), snd (stripPrefix t2 t1)) of
  ([], []) -> Nothing
  (_ , []) -> Just LT
  ([], _)  -> Just GT
  (_ , _)  -> Just EQ
--------------------------------------------------------------------------------
-- Operations

unlist :: PositionList -> [Maybe PositionTree]
unlist (Here t)   = Just t  : repeat Nothing
unlist (There l)  = Nothing : unlist l
unlist (Both t l) = Just t  : unlist l

unnode :: PositionTree -> [Maybe PositionTree]
unnode Leaf     = developerError "Found Leaf when expected Node during traversal of CoDBExpr"
unnode (Node l) = unlist l

-- | Lifts a `BoundVarMap` over a binder, removing the position tree
-- for the bound variable.
liftBVM :: BoundVarMap -> (Maybe PositionTree, BoundVarMap)
liftBVM bvm1 = (mt, bvm3)
  where
    (mt, bvm2) = deleteAndGet 0 bvm1
    bvm3       = IntMap.mapKeysMonotonic (\x -> x - 1) bvm2

-- | Lowers a `BoundVarMap` over a binder, add the position tree for the
-- bound variable.
lowerBVM :: Maybe PositionTree -> BoundVarMap -> BoundVarMap
lowerBVM mt bvm1 = bvm3
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
