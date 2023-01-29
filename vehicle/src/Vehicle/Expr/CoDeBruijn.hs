{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Expr.CoDeBruijn
  ( CoDBExpr,
    CoDBArg,
    CoDBBinder,
    CoDBBinding (..),
    CoDBVar (..),
    BinderC,
    ArgC,
    ExprC (..),
    RecCoDB (..),
    substPos,
    liftFreeCoDBIndices,
    lowerFreeCoDBIndices,
  )
where

import Control.Exception (assert)
import Data.Hashable (Hashable (..))
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList, unzip, zip, zipWith)
import GHC.Generics (Generic)
import Vehicle.Expr.CoDeBruijn.PositionTree
import Vehicle.Expr.DeBruijn hiding (Bound, Free)
import Vehicle.Expr.DeBruijn qualified as DB
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- AST Definitions

newtype CoDBBinding = CoDBBinding (Maybe PositionTree)
  deriving (Show, Eq, Generic)

instance Hashable CoDBBinding

data CoDBVar
  = CoDBFree Identifier
  | CoDBBound
  deriving (Show, Eq, Generic)

instance Hashable CoDBVar

-- An expression that uses DeBruijn index scheme for both binders and variables.
type PartialCoDBBinder = Binder CoDBBinding CoDBVar

type PartialCoDBArg = Arg CoDBBinding CoDBVar

type PartialCoDBExpr = Expr CoDBBinding CoDBVar

type CoDBBinder = (PartialCoDBBinder, BoundVarMap)

type CoDBArg = (PartialCoDBArg, BoundVarMap)

type CoDBExpr = (PartialCoDBExpr, BoundVarMap)

instance Hashable PartialCoDBExpr

instance Hashable PartialCoDBArg

instance Hashable PartialCoDBBinder

{-
--------------------------------------------------------------------------------
-- Extract binder positionTrees

-- This operation is only used to print CoDBExprs in a vaguely reasonable form.

type NamedPTMap = Map (Maybe Name) (Maybe PositionTree)

class ExtractPositionTrees t where
  extractPTs ::
    t CoDBBinding CoDBVar ->
    (t Name CoDBVar, NamedPTMap)

instance ExtractPositionTrees Expr where
  extractPTs = cata $ \case
    UniverseF p l -> (Universe p l, mempty)
    HoleF p n -> (Hole p n, mempty)
    MetaF p m -> (Meta p m, mempty)
    BuiltinF p op -> (Builtin p op, mempty)
    LiteralF p l -> (Literal p l, mempty)
    VarF p v -> (Var p v, mempty)
    AnnF p (e, mpts1) (t, mpts2) -> (Ann p e t, mergePTs [mpts1, mpts2])
    LVecF p xs ->
      let (xs', mpts) = unzip xs
       in (LVec p xs', mergePTs mpts)
    AppF p (fun, mpt) args ->
      let (args', mpts) = NonEmpty.unzip (fmap unpairArg args)
       in (App p fun args', mergePTs (mpt : NonEmpty.toList mpts))
    PiF p binder (result', mpt2) ->
      let (binder', mpt1) = extractPTsBinder binder
       in (Pi p binder' result', mergePTs [mpt1, mpt2])
    LetF p (bound', mpt1) binder (body', mpt3) ->
      let (binder', mpt2) = extractPTsBinder binder
       in (Let p bound' binder' body', mergePTs [mpt1, mpt2, mpt3])
    LamF p binder (body', mpt2) ->
      let (binder', mpt1) = extractPTsBinder binder
       in (Lam p binder' body', mergePTs [mpt1, mpt2])

extractPTsBinder ::
  GenericBinder CoDBBinding (Expr Name CoDBVar, NamedPTMap) ->
  (Binder Name CoDBVar, NamedPTMap)
extractPTsBinder binder = do
  let CoDBBinding binderName mpt = binderRepresentation binder
  let (binder', mpts) = unpairBinder binder
  let binder'' = replaceBinderRep binderName binder'
  let pts' = mergePTs [Map.singleton (Just binderName) mpt, mpts]
  (binder'', pts')

mergePTPair :: NamedPTMap -> NamedPTMap -> NamedPTMap
mergePTPair =
  Map.unionWith
    ( \_ _ ->
        developerError
          "Printing of CoDeBruijn expressions with shadowed variables not yet supported"
    )

mergePTs :: [NamedPTMap] -> NamedPTMap
mergePTs = foldr mergePTPair mempty
-}
--------------------------------------------------------------------------------
-- Intermediate state

-- Recursing over a `CoDBExpr` is very difficult as you have to decompose
-- the BoundVarMap as well, an operation that we can't enforce is safe via the
-- type system. To avoid doing the decomposition and error checking over and
-- over again we define the following intermediate state where the decomposition
-- has already been carried out.

type ArgC = GenericArg CoDBExpr

type BinderC = GenericBinder CoDBBinding CoDBExpr

data ExprC
  = UniverseC Provenance Universe
  | AnnC Provenance CoDBExpr CoDBExpr
  | AppC Provenance CoDBExpr (NonEmpty CoDBArg)
  | PiC Provenance CoDBBinder CoDBExpr
  | BuiltinC Provenance Builtin
  | VarC Provenance DBIndexVar
  | HoleC Provenance Name
  | MetaC Provenance MetaID
  | LetC Provenance CoDBExpr CoDBBinder CoDBExpr
  | LamC Provenance CoDBBinder CoDBExpr
  | LiteralC Provenance Literal
  | LSeqC Provenance [CoDBExpr]
  deriving (Show)

class RecCoDB a b where
  recCoDB :: a -> b

instance RecCoDB CoDBExpr ExprC where
  recCoDB (expr, bvm) = case (expr, unnodeBVM bvm) of
    (Universe p u, _) -> UniverseC p u
    (Hole p n, _) -> HoleC p n
    (Meta p m, _) -> MetaC p m
    (Builtin p op, _) -> BuiltinC p op
    (Literal p l, _) -> LiteralC p l
    (LVec p xs, bvms) -> LSeqC p (zip xs bvms)
    (Var p v, _) -> case v of
      CoDBFree ident -> assert (null bvm) (VarC p (DB.Free ident))
      CoDBBound -> VarC p (DB.Bound (unleafBVM bvm))
    (Ann p e t, bvm1 : bvm2 : _) -> AnnC p (e, bvm1) (t, bvm2)
    (App p fun args, bvm1 : bvm2 : bvms) ->
      AppC p (fun, bvm1) (NonEmpty.zip args (bvm2 :| bvms))
    (Pi p binder result, bvm1 : bvm2 : _) ->
      PiC p (binder, bvm1) (result, lowerBVM (positionTreeOf binder) bvm2)
    (Let p bound binder body, bvm1 : bvm2 : bvm3 : _) ->
      LetC p (bound, bvm1) (binder, bvm2) (body, lowerBVM (positionTreeOf binder) bvm3)
    (Lam p binder body, bvm1 : bvm2 : _) ->
      LamC p (binder, bvm1) (body, lowerBVM (positionTreeOf binder) bvm2)
    (_, bvms) ->
      developerError $
        "Expected the same number of BoundVarMaps as args but found" <+> pretty (length bvms)

instance RecCoDB CoDBBinder BinderC where
  recCoDB (Binder p u v r n t, bvm) = Binder p u v r n (t, bvm)

instance RecCoDB CoDBArg ArgC where
  recCoDB (Arg p v r e, bvm) = Arg p v r (e, bvm)

positionTreeOf :: PartialCoDBBinder -> Maybe PositionTree
positionTreeOf b = case binderRepresentation b of
  CoDBBinding pt -> pt

--------------------------------------------------------------------------------
-- Substitution

-- | Position-based substitution of expressions. Note that it needs to be used
-- with care as unlike DeBruijn based substitution it does not only target
-- variables but arbitrary expressions. Assumes that all the variables in the
-- value `v` being substituted are free in the expression being substituted into.
substPos :: CoDBExpr -> Maybe PositionTree -> CoDBExpr -> CoDBExpr
substPos _ Nothing expr = expr
substPos v (Just Leaf) _ = v
substPos v (Just (Node l)) expr = case (recCoDB expr, unlist l) of
  (UniverseC {}, _) -> invalidPositionTreeError l
  (HoleC {}, _) -> invalidPositionTreeError l
  (MetaC {}, _) -> invalidPositionTreeError l
  (LiteralC {}, _) -> invalidPositionTreeError l
  (BuiltinC {}, _) -> invalidPositionTreeError l
  (VarC {}, _) -> invalidPositionTreeError l
  (AnnC p e t, p1 : p2 : _) ->
    let (e', bvm1) = substPos v p1 e
     in let (t', bvm2) = substPos v p2 t
         in (Ann p e' t', nodeBVM [bvm1, bvm2])
  (LSeqC p xs, ps) ->
    let (xs', bvms) = unzip (zipWith (substPos v) ps xs)
     in (LVec p xs', nodeBVM bvms)
  (AppC p fun args, p1 : p2 : ps) ->
    let (fun', bvm1) = substPos v p1 fun
     in let (args', bvms) = NonEmpty.unzip $ NonEmpty.zipWith (substPosArg v) (p2 :| ps) args
         in (App p fun' args', nodeBVM (bvm1 : NonEmpty.toList bvms))
  (PiC p binder result, p1 : p2 : _) ->
    let (result', bvm2) = substPos (lowerValue v) p2 result
     in let (positionTree, bvm2') = liftBVM bvm2
         in let (binder', bvm1) = substPosBinder v p1 binder positionTree
             in (Pi p binder' result', nodeBVM [bvm1, bvm2'])
  (LetC p bound binder body, p1 : p2 : p3 : _) ->
    let (body', bvm3) = substPos (lowerValue v) p3 body
     in let (positionTree, bvm3') = liftBVM bvm3
         in let (binder', bvm2) = substPosBinder v p2 binder positionTree
             in let (bound', bvm1) = substPos v p1 bound
                 in (Let p bound' binder' body', nodeBVM [bvm1, bvm2, bvm3'])
  (LamC p binder body, p1 : p2 : _) ->
    let (body', bvm2) = substPos (lowerValue v) p2 body
     in let (positionTree, bvm2') = liftBVM bvm2
         in let (binder', bvm1) = substPosBinder v p1 binder positionTree
             in (Lam p binder' body', nodeBVM [bvm1, bvm2'])
  (_, ps) ->
    developerError $
      "Expected the same number of PositionTrees as args but found" <+> pretty (length ps)
  where
    lowerValue :: CoDBExpr -> CoDBExpr
    lowerValue (e, bvm) = (e, lowerBVM Nothing bvm)

substPosArg :: CoDBExpr -> Maybe PositionTree -> CoDBArg -> CoDBArg
substPosArg expr pos arg = case recCoDB arg of
  (Arg p v r e) ->
    let (e', bvm) = substPos expr pos e
     in (Arg p v r e', bvm)

substPosBinder ::
  CoDBExpr ->
  Maybe PositionTree ->
  CoDBBinder ->
  Maybe PositionTree ->
  CoDBBinder
substPosBinder expr pos binder boundPositions = case recCoDB binder of
  (Binder p u v r (CoDBBinding _) t) ->
    let (t', bvm) = substPos expr pos t
     in (Binder p u v r (CoDBBinding boundPositions) t', bvm)

invalidPositionTreeError :: PositionList -> a
invalidPositionTreeError l =
  developerError $
    "Whilst performing a positional substitution expected a Leaf"
      <+> "but found"
      <+> squotes (pretty l)

--------------------------------------------------------------------------------
-- Lifting

liftFreeCoDBIndices :: CoDBExpr -> CoDBExpr
liftFreeCoDBIndices (e, bvm) = (e, IntMap.mapKeysMonotonic (\x -> x - 1) bvm)

lowerFreeCoDBIndices :: CoDBExpr -> CoDBExpr
lowerFreeCoDBIndices (e, bvm) = (e, IntMap.mapKeysMonotonic (+ 1) bvm)
