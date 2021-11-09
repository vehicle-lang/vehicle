{-# LANGUAGE PartialTypeSignatures #-}

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
  = Here
  | There [Maybe PositionTree]

instance Semigroup PositionTree where
  Here     <> Here     = Here
  Here     <> There _  = error _
  There _  <> Here     = error _
  There xs <> There ys = There $ zipWith (<>) xs ys


data CodebruijnBinding = CodebruijnBinding Name (Maybe PositionTree)

data CodebruijnVar
  = Free Identifier
  | Bound

instance HasName CodebruijnBinding where
  nameOf (CodebruijnBinding name _) = name

-- An expression that uses DeBruijn index scheme for both binders and variables.
type CodebruijnBinder ann = Binder CodebruijnBinding CodebruijnVar ann
type CodebruijnArg    ann = Arg    CodebruijnBinding CodebruijnVar ann
type CodebruijnExpr   ann = Expr   CodebruijnBinding CodebruijnVar ann


--------------------------------------------------------------------------------
-- Conversion

newtype BoundVarMap = BoundVarMap (IntMap PositionTree)

instance Semigroup BoundVarMap where
  BoundVarMap m1 <> BoundVarMap m2 = BoundVarMap $ IntMap.unionWith (<>) m1 m2

instance Monoid BoundVarMap where
  mempty = BoundVarMap mempty

popBoundVarMap :: BoundVarMap -> (Maybe PositionTree, BoundVarMap)
popBoundVarMap (BoundVarMap m1) = (b, BoundVarMap m3)
  where
    (b, m2) = IntMap.updateLookupWithKey (\_k _v -> Nothing) 0 m1
    m3      = IntMap.mapKeysMonotonic (\x -> x - 1) m2

mergeBoundVarMaps :: [BoundVarMap] -> BoundVarMap
mergeBoundVarMaps ms = BoundVarMap $ mconcat
  [fmap (There . oneHot i l) m | let l = length ms, (i, BoundVarMap m) <- zip [0..] ms]


class Codebruijn f where
  toCodebruijn   :: f Name LocallyNamelessVar ann -> (f CodebruijnBinding CodebruijnVar ann, BoundVarMap)
  --fromCoDeBruijn :: (f CodebruijnBinding CodebruijnVar ann, VarMap) -> f Name LocallyNamelessVar ann

instance Codebruijn Binder where
  toCodebruijn (Binder p o v n t) = (Binder p o v (CodebruijnBinding n b) t', bvm')
    where
      (t', bvm)  = toCodebruijn t
      (b,  bvm') = popBoundVarMap bvm

instance Codebruijn Arg where
  toCodebruijn (Arg o v e) = first (Arg o v) (toCodebruijn e)

instance Codebruijn Expr where
  toCodebruijn = cata $ \case
    TypeF l         -> (Type l,         mempty)
    MetaF    ann m  -> (Meta ann m,     mempty)
    HoleF    ann n  -> (Hole ann n,     mempty)
    BuiltinF ann op -> (Builtin ann op, mempty)
    LiteralF ann l  -> (Literal ann l,  mempty)

    PrimDictF (e, vm) -> (PrimDict e, vm)
    SeqF ann xs       -> bimap (Seq ann) mergeBoundVarMaps (unzip xs)

    VarF ann v -> case v of
      DB.Free  ident -> (Var ann (Free ident), mempty)
      DB.Bound i    -> (Var ann Bound, BoundVarMap (IntMap.singleton i Here))

    AnnF ann (e, vm1) (t, vm2) ->
      (Ann ann e t, mergeBoundVarMaps [vm1, vm2])

    AppF ann (fun', vm1) args      ->
      let (args', vms) = NonEmpty.unzip (fmap toCodebruijn args) in
      (App ann fun' args', mergeBoundVarMaps (vm1 : NonEmpty.toList vms))

    PiF  ann binder result ->
      let (binder', vm1) = toCodebruijn binder in
      let (result', vm2) = result in
      (Pi ann binder' result', mergeBoundVarMaps [vm1, vm2])

    LetF ann bound binder body ->
      let (bound',  vm1) = bound in
      let (binder', vm2) = toCodebruijn binder in
      let (body',   vm3) = body in
      (Let ann bound' binder' body', mergeBoundVarMaps [vm1, vm2, vm3])

    LamF ann binder body ->
      let (binder', vm1) = toCodebruijn binder in
      let (body', vm2) = body in
      (Lam ann binder' body', mergeBoundVarMaps [vm1, vm2])