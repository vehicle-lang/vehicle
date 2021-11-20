module Vehicle.Language.AST.CoDeBruijn
  ( CodebruijnExpr
  , CodebruijnArg
  , CodebruijnBinder
  , Codebruijn(..)
  ) where

import Data.Functor.Foldable (Recursive(..))
import Data.Bifunctor (first, bimap)
import Data.List.NonEmpty qualified as NonEmpty (unzip, toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (unionWith, singleton, updateLookupWithKey, mapKeysMonotonic)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.DeBruijn hiding (Free, Bound)
import Vehicle.Language.AST.DeBruijn qualified as DB (LocallyNamelessVar(..))

--------------------------------------------------------------------------------
-- Definitions

data PositionTree
  = Leaf
  | Node PositionList

data PositionList
  = Here  PositionTree
  | There PositionList
  | Both  PositionTree PositionList

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



data CodebruijnBinding = CodebruijnBinding (Maybe Symbol) (Maybe PositionTree)

data CodebruijnVar
  = Free Identifier
  | Bound

instance HasName CodebruijnBinding (Maybe Symbol) where
  nameOf (CodebruijnBinding name _) = name

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

class Codebruijn f where
  toCodebruijn   :: f (Maybe Symbol) LocallyNamelessVar ann -> (f CodebruijnBinding CodebruijnVar ann, BoundVarMap)
  --fromCoDeBruijn :: (f CodebruijnBinding CodebruijnVar ann, VarMap) -> f Name LocallyNamelessVar ann

instance Codebruijn Binder where
  toCodebruijn (Binder ann v n t) = (Binder ann v (CodebruijnBinding n mt) t', bvm')
    where
      (t', bvm)  = toCodebruijn t
      (mt, bvm') = pop bvm

instance Codebruijn Arg where
  toCodebruijn (Arg o v e) = first (Arg o v) (toCodebruijn e)

instance Codebruijn Expr where
  toCodebruijn = cata $ \case
    TypeF l         -> (Type l,         mempty)
    MetaF    ann m  -> (Meta ann m,     mempty)
    HoleF    ann n  -> (Hole ann n,     mempty)
    BuiltinF ann op -> (Builtin ann op, mempty)
    LiteralF ann l  -> (Literal ann l,  mempty)

    PrimDictF (e, bvm) -> (PrimDict e, bvm)
    SeqF ann xs        -> bimap (Seq ann) node (unzip xs)

    VarF ann v -> case v of
      DB.Free  ident -> (Var ann (Free ident), mempty)
      DB.Bound i     -> (Var ann Bound, leaf i)

    AnnF ann (e, bvm1) (t, bvm2) ->
      (Ann ann e t, node [bvm1, bvm2])

    AppF ann (fun', bvm1) args ->
      let (args', bvms) = NonEmpty.unzip (fmap toCodebruijn args) in
      (App ann fun' args', node (bvm1 : NonEmpty.toList bvms))

    PiF  ann binder result ->
      let (binder', bvm1) = toCodebruijn binder in
      let (result', bvm2) = result in
      (Pi ann binder' result', node [bvm1, bvm2])

    LetF ann bound binder body ->
      let (bound',  bvm1) = bound in
      let (binder', bvm2) = toCodebruijn binder in
      let (body',   bvm3) = body in
      (Let ann bound' binder' body', node [bvm1, bvm2, bvm3])

    LamF ann binder body ->
      let (binder', bvm1) = toCodebruijn binder in
      let (body', bvm2)   = body in
      (Lam ann binder' body', node [bvm1, bvm2])

--------------------------------------------------------------------------------
-- Hashing