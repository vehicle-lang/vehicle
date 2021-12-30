{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Compile.CoDeBruijnify
  ( ConvertCodebruijn(..)
  , toCoDBExpr
  ) where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty qualified as NonEmpty (toList, unzip)

import Vehicle.Compile.Prelude
import Vehicle.Language.AST.DeBruijn as DB

--------------------------------------------------------------------------------
-- Conversion between DeBruijn and CoDeBruijn expressions
--------------------------------------------------------------------------------
-- Forwards direction

toCoDBExpr :: Expr DBBinding DBVar ann -> (Expr CoDBBinding CoDBVar ann, BoundVarMap)
toCoDBExpr = cata $ \case
  TypeF l                -> (Type l,          mempty)
  HoleF     ann n        -> (Hole     ann n,  mempty)
  MetaF     ann m        -> (Meta     ann m,  mempty)
  BuiltinF  ann op       -> (Builtin  ann op, mempty)
  LiteralF  ann l        -> (Literal  ann l,  mempty)

  PrimDictF ann (e, bvm) -> (PrimDict ann e, bvm)

  LSeqF ann (dict, bvm) xs ->
    let (xs', bvms) = unzip xs in (LSeq ann dict xs', nodeBVM (bvm : bvms))

  VarF ann v -> case v of
    DB.Free  ident -> (Var ann (CoDBFree ident), mempty)
    DB.Bound i     -> (Var ann CoDBBound,        leafBVM i)

  AnnF ann (e', bvm1) (t', bvm2) ->
    (Ann ann e' t', nodeBVM [bvm1, bvm2])

  AppF ann (fun', bvm) args ->
    let (args', bvms) = NonEmpty.unzip $ fmap toCoDBArg args in
    (App ann fun' args', nodeBVM (bvm : NonEmpty.toList bvms))

  PiF  ann binder (result', bvm2) ->
    let (positionTree, bvm2') = liftBVM bvm2 in
    let (binder', bvm1) = toCoDBBinder binder positionTree in
    (Pi ann binder' result', nodeBVM [bvm1, bvm2'])

  LetF ann (bound', bvm1) binder (body', bvm3) ->
    let (positionTree, bvm3') = liftBVM bvm3 in
    let (binder', bvm2) = toCoDBBinder binder positionTree in
    (Let ann bound' binder' body', nodeBVM [bvm1, bvm2, bvm3'])

  LamF ann binder (body', bvm2) ->
    let (positionTree, bvm2') = liftBVM bvm2 in
    let (binder', bvm1) = toCoDBBinder binder positionTree in
    (Lam ann binder' body', nodeBVM [bvm1, bvm2'])

toCoDBBinder :: DBBinder ann -> Maybe PositionTree -> CoDBBinder ann
toCoDBBinder (Binder ann v n t) mpt =
  let (t', bvm) = toCoDBExpr t in
  (Binder ann v (CoDBBinding n mpt) t', bvm)

toCoDBArg :: DBArg ann -> CoDBArg ann
toCoDBArg (Arg ann v e) =
  let (e', bvm) = toCoDBExpr e in
  (Arg ann v e', bvm)

--------------------------------------------------------------------------------
-- Backwards

class ConvertCodebruijn t where
  fromCoDB :: (t CoDBBinding CoDBVar ann, BoundVarMap) -> t DBBinding DBVar ann

instance ConvertCodebruijn Expr where
  fromCoDB expr = case recCoDB expr of
    TypeC l          -> Type l
    HoleC     ann n  -> Hole     ann n
    MetaC     ann m  -> Meta     ann m
    BuiltinC  ann op -> Builtin  ann op
    LiteralC  ann l  -> Literal  ann l

    LSeqC ann dict xs -> LSeq ann (fromCoDB dict) (fmap fromCoDB xs)
    VarC ann v       -> Var ann v

    AnnC ann e t               -> Ann ann (fromCoDB e) (fromCoDB t)
    AppC ann fun args          -> App ann (fromCoDB fun) (fmap fromCoDB args)
    PiC  ann binder result     -> Pi  ann (fromCoDB binder) (fromCoDB result)
    LetC ann bound binder body -> Let ann (fromCoDB bound) (fromCoDB binder) (fromCoDB body)
    LamC ann binder body       -> Lam ann (fromCoDB binder) (fromCoDB body)

    PrimDictC ann e -> PrimDict ann $ fromCoDB e

instance ConvertCodebruijn Binder where
  fromCoDB binder = case recCoDB binder of
    BinderC ann v (CoDBBinding n _) t -> Binder ann v n $ fromCoDB t

instance ConvertCodebruijn Arg where
  fromCoDB arg = case recCoDB arg of
    ArgC ann v e -> Arg ann v $ fromCoDB e
