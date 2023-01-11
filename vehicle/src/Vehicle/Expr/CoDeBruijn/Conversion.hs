{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Expr.CoDeBruijn.Conversion
  ( toCoDBExpr,
    fromCoDB,
  )
where

import Data.Functor.Foldable (Recursive (..))
import Data.List.NonEmpty qualified as NonEmpty (toList, unzip)
import Vehicle.Compile.Prelude
import Vehicle.Expr.CoDeBruijn
import Vehicle.Expr.CoDeBruijn.PositionTree
import Vehicle.Expr.DeBruijn as DB

--------------------------------------------------------------------------------
-- Conversion between DeBruijn and CoDeBruijn expressions
--------------------------------------------------------------------------------
-- Forwards direction

toCoDBExpr :: DBExpr -> CoDBExpr
toCoDBExpr = cata $ \case
  UniverseF ann l -> (Universe ann l, mempty)
  HoleF ann n -> (Hole ann n, mempty)
  MetaF ann m -> (Meta ann m, mempty)
  BuiltinF ann op -> (Builtin ann op, mempty)
  LiteralF ann l -> (Literal ann l, mempty)
  LVecF ann xs ->
    let (xs', bvms) = unzip xs in (LVec ann xs', nodeBVM bvms)
  VarF ann v -> case v of
    DB.Free ident -> (Var ann (CoDBFree ident), mempty)
    DB.Bound i -> (Var ann CoDBBound, leafBVM i)
  AnnF ann (e', bvm1) (t', bvm2) ->
    (Ann ann e' t', nodeBVM [bvm1, bvm2])
  AppF ann (fun', bvm) args ->
    let (args', bvms) = NonEmpty.unzip $ fmap unpairArg args
     in (App ann fun' args', nodeBVM (bvm : NonEmpty.toList bvms))
  PiF ann binder (result', bvm2) ->
    let (positionTree, bvm2') = liftBVM bvm2
     in let (binder', bvm1) = toCoDBBinder binder positionTree
         in (Pi ann binder' result', nodeBVM [bvm1, bvm2'])
  LetF ann (bound', bvm1) binder (body', bvm3) ->
    let (positionTree, bvm3') = liftBVM bvm3
     in let (binder', bvm2) = toCoDBBinder binder positionTree
         in (Let ann bound' binder' body', nodeBVM [bvm1, bvm2, bvm3'])
  LamF ann binder (body', bvm2) ->
    let (positionTree, bvm2') = liftBVM bvm2
     in let (binder', bvm1) = toCoDBBinder binder positionTree
         in (Lam ann binder' body', nodeBVM [bvm1, bvm2'])

toCoDBBinder ::
  GenericBinder DBBinding CoDBExpr ->
  Maybe PositionTree ->
  CoDBBinder
toCoDBBinder binder mpt =
  let (t', bvm) = unpairBinder binder
   in (replaceBinderRep (CoDBBinding mpt) t', bvm)

--------------------------------------------------------------------------------
-- Backwards

fromCoDB :: CoDBExpr -> DBExpr
fromCoDB expr = case recCoDB expr of
  UniverseC p l -> Universe p l
  HoleC p n -> Hole p n
  MetaC p m -> Meta p m
  BuiltinC p op -> Builtin p op
  LiteralC p l -> Literal p l
  LSeqC p xs -> LVec p (fmap fromCoDB xs)
  VarC p v -> Var p v
  AnnC p e t -> Ann p (fromCoDB e) (fromCoDB t)
  AppC p fun args -> App p (fromCoDB fun) (fmap fromCoDBArg args)
  PiC p binder result -> Pi p (fromCoDBBinder binder) (fromCoDB result)
  LetC p bound binder body -> Let p (fromCoDB bound) (fromCoDBBinder binder) (fromCoDB body)
  LamC p binder body -> Lam p (fromCoDBBinder binder) (fromCoDB body)

fromCoDBBinder :: RecCoDB a BinderC => a -> DBBinder
fromCoDBBinder binder = case recCoDB binder of
  Binder p u v r (CoDBBinding _) t -> Binder p u v r () $ fromCoDB t

fromCoDBArg :: RecCoDB a ArgC => a -> DBArg
fromCoDBArg arg = fromCoDB <$> recCoDB arg
