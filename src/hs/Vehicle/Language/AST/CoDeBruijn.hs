{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Language.AST.CoDeBruijn
  ( CoDBExpr
  , CoDBArg
  , CoDBBinder
  , CoDBBinding(..)
  , CoDBVar(..)
  , ExtractPositionTrees(..)
  , BinderC(..)
  , ArgC(..)
  , ExprC(..)
  , RecCoDB(..)
  , mkHashable
  ) where

import Control.Exception (assert)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (unzip, zip, toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Hashable (Hashable(..))
import Data.Functor.Foldable (Recursive(..))
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.DeBruijn hiding (Free, Bound)
import Vehicle.Language.AST.DeBruijn qualified as DB (DBVar(..))
import Vehicle.Language.AST.Visibility
import Vehicle.Language.AST.Builtin (Builtin)
import Vehicle.Language.AST.Utils ( removeAnnotations )
import Vehicle.Language.AST.Position

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
-- Extract binder positionTrees

-- This operation is only used to print CoDBExprs in a vaguely reasonable form.

type NamedPTMap = Map NamedBinding (Maybe PositionTree)

class ExtractPositionTrees t where
  extractPTs :: t (Symbol, Maybe PositionTree) CoDBVar ann ->
                (t Symbol CoDBVar ann, NamedPTMap)

instance ExtractPositionTrees Expr where
  extractPTs = cata $ \case
    TypeF l          -> (Type    l,      mempty)
    HoleF     ann n  -> (Hole    ann n,  mempty)
    MetaF     ann m  -> (Meta    ann m,  mempty)
    BuiltinF  ann op -> (Builtin ann op, mempty)
    LiteralF  ann l  -> (Literal ann l,  mempty)
    VarF      ann v  -> (Var ann v,      mempty)

    PrimDictF ann (e, mpts)        -> (PrimDict ann e, mpts)
    AnnF ann (e, mpts1) (t, mpts2) -> (Ann ann e t, mergePTs [mpts1, mpts2])

    SeqF ann xs ->
      let (xs', mpts) = unzip xs in
      (Seq ann xs', mergePTs mpts)

    AppF ann (fun, mpt) args ->
      let (args', mpts) = NonEmpty.unzip (fmap extractPTs args) in
      (App ann fun args', mergePTs (mpt : NonEmpty.toList mpts))

    PiF  ann binder (result', mpt2) ->
      let (binder', mpt1) = extractPTs binder in
      (Pi ann binder' result', mergePTs [mpt1, mpt2])

    LetF ann (bound', mpt1) binder (body', mpt3) ->
      let (binder', mpt2) = extractPTs binder in
      (Let ann bound' binder' body', mergePTs [mpt1, mpt2, mpt3])

    LamF ann binder (body', mpt2) ->
      let (binder', mpt1) = extractPTs binder in
      (Lam ann binder' body', mergePTs [mpt1, mpt2])

instance ExtractPositionTrees Binder where
  extractPTs (Binder ann v (n, mpt) t) =
    let (t', mpts) = extractPTs t in
    let pts' = mergePTs [Map.singleton n mpt, mpts] in
    (Binder ann v n t', pts')

instance ExtractPositionTrees Arg where
  extractPTs (Arg ann v e) =
    let (e', mpts) = extractPTs e in
    (Arg ann v e', mpts)

mergePTPair :: NamedPTMap -> NamedPTMap -> NamedPTMap
mergePTPair = Map.unionWith (\_ _ -> developerError
  "Printing of CoDeBruijn expressions with shadowed variables not yet supported")

mergePTs :: [NamedPTMap] -> NamedPTMap
mergePTs = foldr mergePTPair mempty

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
  recCoDB (expr, bvm) = case (expr, unnodeBVM bvm) of
    (Type l         , _) -> TypeC         l
    (Hole     ann n , _) -> HoleC     ann n
    (Meta     ann m , _) -> MetaC     ann m
    (Builtin  ann op, _) -> BuiltinC  ann op
    (Literal  ann l , _) -> LiteralC  ann l

    (PrimDict ann e, bvm1 : _) -> PrimDictC ann (e, bvm1)

    (Seq ann xs, bvms) -> SeqC ann (zip xs bvms)

    (Var ann v, _) -> case v of
      CoDBFree  ident -> assert (null bvm) (VarC ann (DB.Free ident))
      CoDBBound       -> VarC ann (DB.Bound (unleafBVM bvm))

    (Ann ann e t, bvm1 : bvm2 : _) -> AnnC ann (e, bvm1) (t, bvm2)

    (App ann fun args, bvm1 : bvm2 : bvms) ->
      AppC ann (fun, bvm1) (NonEmpty.zip args (bvm2 :| bvms))

    (Pi ann binder result, bvm1 : bvm2 : _) ->
      PiC ann (binder, bvm1) (result, unpop (positionTreeOf binder) bvm2)

    (Let ann bound binder body, bvm1 : bvm2 : bvm3 : _) ->
      LetC ann (bound, bvm1) (binder, bvm2) (body, unpop (positionTreeOf binder) bvm3)

    (Lam ann binder body, bvm1 : bvm2 : _) ->
      LamC ann (binder, bvm1) (body, unpop (positionTreeOf binder) bvm2)

    (_, bvms) -> developerError $
      "Expected the same number of BoundVarMaps as args but found" <+> pretty (length bvms)

instance RecCoDB (CoDBBinder ann) (BinderC ann) where
  recCoDB (Binder ann v n t, bvm) = BinderC ann v n (t, bvm)

instance RecCoDB (CoDBArg ann) (ArgC ann) where
  recCoDB (Arg ann v e, bvm) = ArgC ann v (e, bvm)

positionTreeOf :: PartialCoDBBinder ann -> Maybe PositionTree
positionTreeOf b = case nameOf b of
  CoDBBinding _ pt -> pt