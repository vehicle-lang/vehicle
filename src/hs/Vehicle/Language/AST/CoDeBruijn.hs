{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Language.AST.CoDeBruijn
  ( CodebruijnExpr
  , CodebruijnArg
  , CodebruijnBinder
  , Codebruijn(..)
  ) where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (unionWith, singleton, updateLookupWithKey, mapKeysMonotonic)
import Data.Hashable

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.DeBruijn hiding (Free, Bound)
import Vehicle.Language.AST.DeBruijn qualified as DB (LocallyNamelessVar(..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Definitions

data PositionTree
  = Leaf
  | Node PositionList
  deriving (Eq, Generic)

instance Hashable PositionTree

data PositionList
  = Here  PositionTree
  | There PositionList
  | Both  PositionTree PositionList
  deriving (Eq, Generic)

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



data CodebruijnBinding
  = CodebruijnBinding (Maybe Symbol) (Maybe PositionTree)
  deriving (Eq, Generic)

instance Hashable CodebruijnBinding

instance HasName CodebruijnBinding (Maybe Symbol) where
  nameOf (CodebruijnBinding name _) = name

data CodebruijnVar
  = Free Identifier
  | Bound
  deriving (Eq, Generic)

instance Hashable CodebruijnVar

-- An expression that uses DeBruijn index scheme for both binders and variables.
type CodebruijnBinder ann = Binder CodebruijnBinding CodebruijnVar ann
type CodebruijnArg    ann = Arg    CodebruijnBinding CodebruijnVar ann
type CodebruijnExpr   ann = Expr   CodebruijnBinding CodebruijnVar ann

--------------------------------------------------------------------------------
-- Conversion

type BoundVarMap = IntMap PositionTree

pop :: BoundVarMap -> (Maybe PositionTree, BoundVarMap)
pop bvm1 = (mt, bvm3)
  where
    (mt, bvm2) = IntMap.updateLookupWithKey (\_k _v -> Nothing) 0 bvm1
    bvm3       = IntMap.mapKeysMonotonic (\x -> x - 1) bvm2

node :: [BoundVarMap] -> BoundVarMap
node []   = mempty
node bvms = fmap Node (foldr1 merge $ fmap (fmap Here) bvms)
  where
    merge :: IntMap PositionList -> IntMap PositionList -> IntMap PositionList
    merge xs ys = IntMap.unionWith (<>) xs (fmap There ys)

leaf :: Index -> BoundVarMap
leaf i = IntMap.singleton i Leaf

-- TODO once we have #41 done then collapse these into one function
varmapOf :: HasAnnotation e (BoundVarMap, ann) => e -> BoundVarMap
varmapOf = fst . annotationOf

class Codebruijn f where
  toCodebruijn :: f (Maybe Symbol) LocallyNamelessVar ann
               -> f CodebruijnBinding CodebruijnVar (BoundVarMap, ann)
  --fromCoDeBruijn :: (f CodebruijnBinding CodebruijnVar ann, VarMap) -> f Name LocallyNamelessVar ann

instance Codebruijn Binder where
  toCodebruijn (Binder ann v n t) = Binder (bvm', ann) v (CodebruijnBinding n mt) t'
    where
      t'         = toCodebruijn t
      (mt, bvm') = pop (varmapOf t')

instance Codebruijn Arg where
  toCodebruijn (Arg ann v e) = Arg (varmapOf e', ann) v e'
    where e' = toCodebruijn e

instance Codebruijn Expr where
  toCodebruijn = cata $ \case
    TypeF l         -> Type l
    HoleF    ann n  -> Hole    (mempty, ann) n
    MetaF    ann m  -> Meta    (mempty, ann) m
    BuiltinF ann op -> Builtin (mempty, ann) op
    LiteralF ann l  -> Literal (mempty, ann) l

    PrimDictF e     -> PrimDict e
    SeqF ann xs     -> Seq (node (map varmapOf xs), ann) xs

    VarF ann v -> case v of
      DB.Free  ident -> Var (mempty, ann) (Free ident)
      DB.Bound i     -> Var (leaf i, ann) Bound

    AnnF ann e t -> Ann (node [varmapOf e, varmapOf t], ann) e t

    AppF ann fun' args ->
      let args' = fmap toCodebruijn args in
      App (node (varmapOf fun' : fmap varmapOf (NonEmpty.toList args')), ann) fun' args'

    PiF  ann binder result' ->
      let binder' = toCodebruijn binder in
      Pi (node [varmapOf binder', varmapOf result'], ann) binder' result'

    LetF ann bound' binder body' ->
      let binder' = toCodebruijn binder in
      Let (node [varmapOf bound', varmapOf binder', varmapOf body'], ann) bound' binder' body'

    LamF ann binder body' ->
      let binder' = toCodebruijn binder in
      Lam (node [varmapOf binder', varmapOf body'], ann) binder' body'

--------------------------------------------------------------------------------
-- Hashing

instance Hashable (CodebruijnBinder BoundVarMap)

instance Hashable (CodebruijnArg BoundVarMap)

instance Hashable (CodebruijnExpr BoundVarMap)