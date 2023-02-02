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

toCoDBExpr :: CheckedExpr -> CoDBExpr
toCoDBExpr = cata $ \case
  UniverseF p l -> (Universe p l, mempty)
  HoleF p n -> (Hole p n, mempty)
  MetaF p m -> (Meta p m, mempty)
  BuiltinF p op -> (Builtin p op, mempty)
  LiteralF p l -> (Literal p l, mempty)
  LVecF p xs ->
    let (xs', bvms) = unzip xs in (LVec p xs', nodeBVM bvms)
  VarF p v -> case v of
    DB.Free ident -> (Var p (CoDBFree ident), mempty)
    DB.Bound i -> (Var p CoDBBound, leafBVM i)
  AnnF p (e', bvm1) (t', bvm2) ->
    (Ann p e' t', nodeBVM [bvm1, bvm2])
  AppF p (fun', bvm) args ->
    let (args', bvms) = NonEmpty.unzip $ fmap unpairArg args
     in (App p fun' args', nodeBVM (bvm : NonEmpty.toList bvms))
  PiF p binder (result', bvm2) ->
    let (positionTree, bvm2') = liftBVM bvm2
     in let (binder', bvm1) = toCoDBBinder binder positionTree
         in (Pi p binder' result', nodeBVM [bvm1, bvm2'])
  LetF p (bound', bvm1) binder (body', bvm3) ->
    let (positionTree, bvm3') = liftBVM bvm3
     in let (binder', bvm2) = toCoDBBinder binder positionTree
         in (Let p bound' binder' body', nodeBVM [bvm1, bvm2, bvm3'])
  LamF p binder (body', bvm2) ->
    let (positionTree, bvm2') = liftBVM bvm2
     in let (binder', bvm1) = toCoDBBinder binder positionTree
         in (Lam p binder' body', nodeBVM [bvm1, bvm2'])

toCoDBBinder ::
  GenericBinder DBBinding CoDBExpr ->
  Maybe PositionTree ->
  CoDBBinder
toCoDBBinder binder mpt =
  let (t', bvm) = unpairBinder binder
   in (replaceBinderRep (CoDBBinding mpt) t', bvm)

--------------------------------------------------------------------------------
-- Backwards

fromCoDB :: CoDBExpr -> CheckedExpr
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

fromCoDBBinder :: RecCoDB a BinderC => a -> CheckedBinder
fromCoDBBinder binder = case recCoDB binder of
  Binder p u v r (CoDBBinding _) t -> Binder p u v r () $ fromCoDB t

fromCoDBArg :: RecCoDB a ArgC => a -> CheckedArg
fromCoDBArg arg = fromCoDB <$> recCoDB arg
