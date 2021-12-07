{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Compile.CoDeBruijnify
  ( ConvertCodebruijn(..)
  , toHashableCodebruijn
  ) where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty qualified as NonEmpty (toList, unzip)

import Vehicle.Language.AST
import Vehicle.Language.AST.DeBruijn as DB

--------------------------------------------------------------------------------
-- Conversion

class ConvertCodebruijn t where
  toCodebruijn   :: t DBBinding DBVar ann -> (t CoDBBinding CoDBVar ann, BoundVarMap)
  fromCodebruijn :: (t CoDBBinding CoDBVar ann, BoundVarMap) -> t DBBinding DBVar ann

instance ConvertCodebruijn Binder where
  toCodebruijn (Binder ann v n t) =
    let (t', bvm) = toCodebruijn t in
    let (positionTree, bvm') = pop bvm in
    (Binder ann v (CoDBBinding n positionTree) t', bvm')

  fromCodebruijn binder = case recCoDB binder of
    BinderC ann v (CoDBBinding n _) t -> Binder ann v n $ fromCodebruijn t

instance ConvertCodebruijn Arg where
  toCodebruijn (Arg ann v e) =
    let (e', bvm) = toCodebruijn e in
    (Arg ann v e', bvm)

  fromCodebruijn arg = case recCoDB arg of
    ArgC ann v e -> Arg ann v $ fromCodebruijn e

instance ConvertCodebruijn Expr where
  toCodebruijn = cata $ \case
    TypeF l                -> (Type l,          mempty)
    HoleF     ann n        -> (Hole     ann n,  mempty)
    MetaF     ann m        -> (Meta     ann m,  mempty)
    BuiltinF  ann op       -> (Builtin  ann op, mempty)
    LiteralF  ann l        -> (Literal  ann l,  mempty)

    PrimDictF ann (e, bvm) -> (PrimDict ann e, bvm)

    SeqF ann xs -> let (xs', bvms) = unzip xs in (Seq ann xs', node bvms)

    VarF ann v -> case v of
      DB.Free  ident -> (Var ann (CoDBFree ident), mempty)
      DB.Bound i     -> (Var ann CoDBBound,        leaf i)

    AnnF ann (e', bvm1) (t', bvm2) ->
      (Ann ann e' t', node [bvm1, bvm2])

    AppF ann (fun', bvm) args ->
      let (args', bvms) = NonEmpty.unzip $ fmap toCodebruijn args in
      (App ann fun' args', node (bvm : NonEmpty.toList bvms))

    PiF  ann binder (result', bvm2) ->
      let (binder', bvm1) = toCodebruijn binder in
      (Pi ann binder' result', node [bvm1, bvm2])

    LetF ann (bound', bvm1) binder (body', bvm3) ->
      let (binder', bvm2) = toCodebruijn binder in
      (Let ann bound' binder' body', node [bvm1, bvm2, bvm3])

    LamF ann binder (body', bvm2) ->
      let (binder', bvm1) = toCodebruijn binder in
      (Lam ann binder' body', node [bvm1, bvm2])


  fromCodebruijn expr = case recCoDB expr of
    TypeC l          -> Type l
    HoleC     ann n  -> Hole     ann n
    MetaC     ann m  -> Meta     ann m
    BuiltinC  ann op -> Builtin  ann op
    LiteralC  ann l  -> Literal  ann l

    PrimDictC ann e -> PrimDict ann $ fromCodebruijn e
    SeqC ann xs     -> Seq ann (fmap fromCodebruijn xs)
    VarC ann v      -> Var ann v

    AnnC ann e t               -> Ann ann (fromCodebruijn e) (fromCodebruijn t)
    AppC ann fun args          -> App ann (fromCodebruijn fun) (fmap fromCodebruijn args)
    PiC  ann binder result     -> Pi  ann (fromCodebruijn binder) (fromCodebruijn result)
    LetC ann bound binder body -> Let ann (fromCodebruijn bound) (fromCodebruijn binder) (fromCodebruijn body)
    LamC ann binder body       -> Lam ann (fromCodebruijn binder) (fromCodebruijn body)

--------------------------------------------------------------------------------
-- Hashing

toHashableCodebruijn :: forall ann. DBExpr ann -> CoDBExpr ()
toHashableCodebruijn e = mkHashable (toCodebruijn e :: CoDBExpr ann)