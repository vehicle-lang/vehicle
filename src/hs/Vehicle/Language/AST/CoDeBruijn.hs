{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Language.AST.CoDeBruijn
  ( CoDBExpr
  , CoDBArg
  , CoDBBinder
  , CoDBBinding(..)
  , CoDBVar(..)
  , BoundVarMap
  , PositionTree(..)
  , PositionList(..)
  , BinderC(..)
  , ArgC(..)
  , ExprC(..)
  , RecCoDB(..)
  , leaf
  , unleaf
  , node
  , unnode
  , pop
  , unpop
  , mkHashable
  ) where

import Control.Exception (assert)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (length, zip)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Hashable

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.DeBruijn hiding (Free, Bound)
import Vehicle.Language.AST.DeBruijn qualified as DB (DBVar(..))
import Vehicle.Language.AST.Visibility
import Vehicle.Language.AST.Builtin (Builtin)
import Vehicle.Language.AST.Utils ( removeAnnotations )
import GHC.Generics (Generic)
import Data.Maybe (catMaybes)

--------------------------------------------------------------------------------
-- Core definitions

data PositionTree
  = Leaf
  | Node PositionList
  deriving (Show, Eq, Generic)

instance Pretty PositionTree where
  pretty = pretty . show

instance Hashable PositionTree

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

-- |Bound variable maps - maps DeBruijn indices to the location that they are used
-- in an expression. If not used, then there is no entry in the map.
type BoundVarMap = IntMap PositionTree

pop :: BoundVarMap -> (Maybe PositionTree, BoundVarMap)
pop bvm1 = (mt, bvm3)
  where
    (mt, bvm2) = IntMap.updateLookupWithKey (\_k _v -> Nothing) 0 bvm1
    bvm3       = IntMap.mapKeysMonotonic (\x -> x - 1) bvm2

unpop :: (Maybe PositionTree, BoundVarMap) -> BoundVarMap
unpop (mt, bvm1) = bvm3
  where
    bvm2 = IntMap.mapKeysMonotonic (+ 1) bvm1
    bvm3 = case mt of
      Nothing -> bvm2
      Just t  -> IntMap.insert 0 t bvm2

node :: [BoundVarMap] -> BoundVarMap
node []   = mempty
node bvms = fmap Node (foldr1 merge $ fmap (fmap Here) bvms)
  where
    merge :: IntMap PositionList -> IntMap PositionList -> IntMap PositionList
    merge xs ys = IntMap.unionWith (<>) xs (fmap There ys)

unnode :: BoundVarMap -> [BoundVarMap]
unnode bvm = mapM goTree bvm
  where
    goTree :: PositionTree -> [PositionTree]
    goTree Leaf     = developerError "Found Leaf when expected Node during traversal of CoDBExpr"
    goTree (Node l) = goList l

    goList :: PositionList -> [PositionTree]
    goList l = catMaybes (unhere l : traverse goList (unthere l))

    unhere :: PositionList -> Maybe PositionTree
    unhere (Here t)   = Just t
    unhere (There _)  = Nothing
    unhere (Both t _) = Just t

    unthere :: PositionList -> Maybe PositionList
    unthere (Here _)   = Nothing
    unthere (There l)  = Just l
    unthere (Both _ l) = Just l

leaf :: DBIndex -> BoundVarMap
leaf i = IntMap.singleton i Leaf

unleaf :: BoundVarMap -> DBIndex
unleaf bvm = case IntMap.minViewWithKey bvm of
  Just ((i, Leaf), bvm')
    | null bvm' -> i
  _ -> developerError $ "Expecting a singleton BoundVarMap with a leaf node" <+>
                        "for Var but found" <+> pretty (show bvm)

--------------------------------------------------------------------------------
-- AST Definitions

data CoDBBinding
  = CoDBBinding DBBinding (Maybe PositionTree)
  deriving (Show, Eq, Generic)

instance Hashable CoDBBinding where
  -- We deliberately ignore the name stored in the binding
  hashWithSalt d (CoDBBinding _n t) = hashWithSalt d t

instance HasName CoDBBinding DBBinding where
  nameOf (CoDBBinding name _) = name

data CoDBVar
  = CoDBFree Identifier
  | CoDBBound
  deriving (Show, Eq, Generic)

instance Hashable CoDBVar

-- An expression that uses DeBruijn index scheme for both binders and variables.
type PartialCoDBBinder ann = Binder CoDBBinding CoDBVar ann
type PartialCoDBArg    ann = Arg    CoDBBinding CoDBVar ann
type PartialCoDBExpr   ann = Expr   CoDBBinding CoDBVar ann

type CoDBBinder ann = (PartialCoDBBinder ann, BoundVarMap)
type CoDBArg    ann = (PartialCoDBArg    ann, BoundVarMap)
type CoDBExpr   ann = (PartialCoDBExpr   ann, BoundVarMap)

instance Hashable (PartialCoDBExpr   ()) where
instance Hashable (PartialCoDBArg    ()) where
instance Hashable (PartialCoDBBinder ()) where

mkHashable :: CoDBExpr ann -> CoDBExpr ()
mkHashable = first removeAnnotations

--------------------------------------------------------------------------------
-- Intermediate state

-- Recursing over a `CoDBExpr` is very difficult as you have to decompose
-- the BoundVarMap as well, an operation that we can't enforce is safe via the
-- type system. To avoid doing the decomposition and error checking over and
-- over again we define the following intermediate state where the decomposition
-- has already been carried out.

data ArgC ann
  = ArgC ann Visibility (CoDBExpr ann)
  deriving (Show)

data BinderC ann
  = BinderC ann Visibility CoDBBinding (CoDBExpr ann)
  deriving (Show)

data ExprC ann
  = TypeC     UniverseLevel
  | AnnC      ann (CoDBExpr ann) (CoDBExpr ann)
  | AppC      ann (CoDBExpr ann) (NonEmpty (CoDBArg ann))
  | PiC       ann (CoDBBinder ann) (CoDBExpr ann)
  | BuiltinC  ann Builtin
  | VarC      ann DBVar
  | HoleC     ann Symbol
  | MetaC     ann Meta
  | LetC      ann (CoDBExpr ann) (CoDBBinder ann) (CoDBExpr ann)
  | LamC      ann (CoDBBinder ann) (CoDBExpr ann)
  | LiteralC  ann Literal
  | SeqC      ann [CoDBExpr ann]
  | PrimDictC ann (CoDBExpr ann)
  deriving (Show)


class RecCoDB a b where
  recCoDB :: a -> b

instance RecCoDB (CoDBExpr ann) (ExprC ann) where
  recCoDB (expr, bvm) = case expr of
    Type l          -> assert (null bvm) (TypeC         l)
    Hole     ann n  -> assert (null bvm) (HoleC     ann n)
    Meta     ann m  -> assert (null bvm) (MetaC     ann m)
    Builtin  ann op -> assert (null bvm) (BuiltinC  ann op)
    Literal  ann l  -> assert (null bvm) (LiteralC  ann l)

    PrimDict ann e -> PrimDictC ann (e, bvm)

    Seq ann xs ->
      let bvms = unnode bvm in
      let xs'  = assert (length xs == length bvms) (zip xs bvms) in
      SeqC ann xs'

    Var ann v -> case v of
      CoDBFree  ident -> assert (null bvm) (VarC ann (DB.Free ident))
      CoDBBound       -> VarC ann (DB.Bound (unleaf bvm))

    Ann ann e t -> case unnode bvm of
      [bvm1, bvm2] -> AnnC ann (e, bvm1) (t, bvm2)
      bvms         -> lengthError "Ann" 2 bvms

    App ann fun args -> case unnode bvm of
      (bvm1 : bvms) -> AppC ann (fun, bvm1) (zipArgs args bvms)
      bvms          -> lengthError "App" 1 bvms

    Pi ann binder result -> case unnode bvm of
      [bvm1, bvm2] -> PiC ann (binder, bvm1) (result, bvm2)
      bvms         -> lengthError "Pi" 2 bvms

    Let ann bound binder body -> case unnode bvm of
      [bvm1, bvm2, bvm3] -> LetC ann (bound, bvm1) (binder, bvm2) (body, bvm3)
      bvms               -> lengthError "Let" 3 bvms

    Lam ann binder body -> case unnode bvm of
      [bvm1, bvm2] -> LamC ann (binder, bvm1) (body, bvm2)
      bvms         -> lengthError "Lam" 2 bvms

instance RecCoDB (CoDBBinder ann) (BinderC ann) where
  recCoDB (Binder ann v (CoDBBinding n mpt) t, bvm) =
    let bvm' = unpop (mpt, bvm) in
    BinderC ann v (CoDBBinding n mpt) (t, bvm')
instance RecCoDB (CoDBArg ann) (ArgC ann) where
  recCoDB (Arg ann v e, bvm) = ArgC ann v (e, bvm)

lengthError :: Doc b -> Int -> [BoundVarMap] -> a
lengthError constructor numberOfArgs bvms = developerError $
  "Expected the same number of BoundVarMaps as args to" <+> constructor <+>
  "but found" <+> pretty (length bvms) <+> "vs" <+> pretty numberOfArgs

zipArgs :: NonEmpty (PartialCoDBArg ann) -> [BoundVarMap] -> NonEmpty (CoDBArg ann)
zipArgs args bvms@(b : bs)
  | length bvms == NonEmpty.length args = NonEmpty.zip args (b :| bs)
zipArgs args bvms = developerError $
  lengthError "App" (NonEmpty.length args) bvms

