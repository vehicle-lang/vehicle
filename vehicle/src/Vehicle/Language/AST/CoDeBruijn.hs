{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Language.AST.CoDeBruijn
  ( CoDBExpr
  , CoDBArg
  , CoDBBinder
  , CoDBBinding(..)
  , CoDBVar(..)
  , ExtractPositionTrees(..)
  , BinderC
  , ArgC
  , ExprC(..)
  , RecCoDB(..)
  , substPos
  , liftFreeCoDBIndices
  , lowerFreeCoDBIndices
  ) where

import Control.Exception (assert)
import Data.Functor.Foldable (Recursive (..))
import Data.Hashable (Hashable (..))
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList, unzip, zip, zipWith)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)

import Vehicle.Language.AST.Builtin (Builtin)
import Vehicle.Language.AST.Expr
import Vehicle.Language.AST.DeBruijn hiding (Bound, Free)
import Vehicle.Language.AST.DeBruijn qualified as DB (DBVar (..))
import Vehicle.Language.AST.Meta
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.Position
import Vehicle.Language.AST.Provenance
import Vehicle.Prelude
import Vehicle.Language.AST.Binder
import Vehicle.Language.AST.Arg

--------------------------------------------------------------------------------
-- AST Definitions

data CoDBBinding name
  = CoDBBinding name (Maybe PositionTree)
  deriving (Show, Eq, Generic)

instance Eq name => Hashable (CoDBBinding name) where
  -- We deliberately ignore the name stored in the binding
  hashWithSalt d (CoDBBinding _n t) = hashWithSalt d t

instance HasName (CoDBBinding name) name where
  nameOf (CoDBBinding name _) = name

data CoDBVar
  = CoDBFree Identifier
  | CoDBBound
  deriving (Show, Eq, Generic)

instance Hashable CoDBVar

-- An expression that uses DeBruijn index scheme for both binders and variables.
type PartialCoDBBinder = Binder (CoDBBinding DBBinding) CoDBVar
type PartialCoDBArg    = Arg    (CoDBBinding DBBinding) CoDBVar
type PartialCoDBExpr   = Expr   (CoDBBinding DBBinding) CoDBVar

type CoDBBinder = (PartialCoDBBinder, BoundVarMap)
type CoDBArg    = (PartialCoDBArg   , BoundVarMap)
type CoDBExpr   = (PartialCoDBExpr  , BoundVarMap)

instance Hashable PartialCoDBExpr
instance Hashable PartialCoDBArg
instance Hashable PartialCoDBBinder

--------------------------------------------------------------------------------
-- Extract binder positionTrees

-- This operation is only used to print CoDBExprs in a vaguely reasonable form.

type NamedPTMap = Map NamedBinding (Maybe PositionTree)

class ExtractPositionTrees t where
  extractPTs :: t (CoDBBinding Name) CoDBVar ->
                (t Name CoDBVar, NamedPTMap)

instance ExtractPositionTrees Expr where
  extractPTs = cata $ \case
    UniverseF ann l  -> (Universe ann l,  mempty)
    HoleF     ann n  -> (Hole    ann n,  mempty)
    MetaF     ann m  -> (Meta    ann m,  mempty)
    BuiltinF  ann op -> (Builtin ann op, mempty)
    LiteralF  ann l  -> (Literal ann l,  mempty)
    VarF      ann v  -> (Var ann v,      mempty)

    AnnF ann (e, mpts1) (t, mpts2) -> (Ann ann e t, mergePTs [mpts1, mpts2])

    LVecF ann xs ->
      let (xs', mpts) = unzip xs in
      (LVec ann xs', mergePTs mpts)

    AppF ann (fun, mpt) args ->
      let (args', mpts) = NonEmpty.unzip (fmap unpairArg args) in
      (App ann fun args', mergePTs (mpt : NonEmpty.toList mpts))

    PiF  ann binder (result', mpt2) ->
      let (binder', mpt1) = extractPTsBinder binder in
      (Pi ann binder' result', mergePTs [mpt1, mpt2])

    LetF ann (bound', mpt1) binder (body', mpt3) ->
      let (binder', mpt2) = extractPTsBinder binder in
      (Let ann bound' binder' body', mergePTs [mpt1, mpt2, mpt3])

    LamF ann binder (body', mpt2) ->
      let (binder', mpt1) = extractPTsBinder binder in
      (Lam ann binder' body', mergePTs [mpt1, mpt2])

extractPTsBinder :: GenericBinder (CoDBBinding Name) (Expr Name CoDBVar, NamedPTMap)
                 -> (Binder Name CoDBVar, NamedPTMap)
extractPTsBinder binder = do
  let CoDBBinding binderName mpt = binderRepresentation binder
  let (binder', mpts)  = unpairBinder binder
  let binder''         = replaceBinderRep binderName binder'
  let pts'             = mergePTs [Map.singleton binderName mpt, mpts]
  (binder'', pts')

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

type ArgC = GenericArg CoDBExpr

type BinderC = GenericBinder (CoDBBinding DBBinding) CoDBExpr

data ExprC
  = UniverseC Provenance Universe
  | AnnC      Provenance CoDBExpr CoDBExpr
  | AppC      Provenance CoDBExpr (NonEmpty CoDBArg)
  | PiC       Provenance CoDBBinder CoDBExpr
  | BuiltinC  Provenance Builtin
  | VarC      Provenance DBVar
  | HoleC     Provenance Name
  | MetaC     Provenance Meta
  | LetC      Provenance CoDBExpr CoDBBinder CoDBExpr
  | LamC      Provenance CoDBBinder CoDBExpr
  | LiteralC  Provenance Literal
  | LSeqC     Provenance [CoDBExpr]
  deriving (Show)

class RecCoDB a b where
  recCoDB :: a -> b

instance RecCoDB CoDBExpr ExprC where
  recCoDB (expr, bvm) = case (expr, unnodeBVM bvm) of
    (Universe ann u , _) -> UniverseC ann u
    (Hole     ann n , _) -> HoleC     ann n
    (Meta     ann m , _) -> MetaC     ann m
    (Builtin  ann op, _) -> BuiltinC  ann op
    (Literal  ann l , _) -> LiteralC  ann l

    (LVec ann xs, bvms) -> LSeqC ann (zip xs bvms)

    (Var ann v, _) -> case v of
      CoDBFree  ident -> assert (null bvm) (VarC ann (DB.Free ident))
      CoDBBound       -> VarC ann (DB.Bound (unleafBVM bvm))

    (Ann ann e t, bvm1 : bvm2 : _) -> AnnC ann (e, bvm1) (t, bvm2)

    (App ann fun args, bvm1 : bvm2 : bvms) ->
      AppC ann (fun, bvm1) (NonEmpty.zip args (bvm2 :| bvms))

    (Pi ann binder result, bvm1 : bvm2 : _) ->
      PiC ann (binder, bvm1) (result, lowerBVM (positionTreeOf binder) bvm2)

    (Let ann bound binder body, bvm1 : bvm2 : bvm3 : _) ->
      LetC ann (bound, bvm1) (binder, bvm2) (body, lowerBVM (positionTreeOf binder) bvm3)

    (Lam ann binder body, bvm1 : bvm2 : _) ->
      LamC ann (binder, bvm1) (body, lowerBVM (positionTreeOf binder) bvm2)

    (_, bvms) -> developerError $
      "Expected the same number of BoundVarMaps as args but found" <+> pretty (length bvms)

instance RecCoDB CoDBBinder BinderC where
  recCoDB (Binder ann v r n t, bvm) = Binder ann v r n (t, bvm)

instance RecCoDB CoDBArg ArgC where
  recCoDB (Arg ann v r e, bvm) = Arg ann v r (e, bvm)

positionTreeOf :: PartialCoDBBinder -> Maybe PositionTree
positionTreeOf b = case (nameOf b :: CoDBBinding DBBinding) of
  CoDBBinding _ pt -> pt

--------------------------------------------------------------------------------
-- Substitution

-- | Position-based substitution of expressions. Note that it needs to be used
-- with care as unlike DeBruijn based substitution it does not only target
-- variables but arbitrary expressions. Assumes that all the variables in the
-- value `v` being substituted are free in the expression being substituted into.
substPos :: CoDBExpr -> Maybe PositionTree -> CoDBExpr -> CoDBExpr
substPos _ Nothing         expr = expr
substPos v (Just Leaf)     _    = v
substPos v (Just (Node l)) expr = case (recCoDB expr, unlist l) of
  (UniverseC{}, _) -> invalidPositionTreeError l
  (HoleC{}    , _) -> invalidPositionTreeError l
  (MetaC{}    , _) -> invalidPositionTreeError l
  (LiteralC{} , _) -> invalidPositionTreeError l
  (BuiltinC{} , _) -> invalidPositionTreeError l
  (VarC{}     , _) -> invalidPositionTreeError l

  (AnnC ann e t, p1 : p2 : _) ->
    let (e', bvm1) = substPos v p1 e in
    let (t', bvm2) = substPos v p2 t in
    (Ann ann e' t', nodeBVM [bvm1, bvm2])

  (LSeqC ann xs, ps) ->
    let (xs', bvms) = unzip (zipWith (substPos v) ps xs) in
    (LVec ann xs', nodeBVM bvms)

  (AppC ann fun args, p1 : p2 : ps) ->
    let (fun',  bvm1) = substPos v p1 fun in
    let (args', bvms) = NonEmpty.unzip $ NonEmpty.zipWith (substPosArg v) (p2 :| ps) args in
    (App ann fun' args', nodeBVM (bvm1 : NonEmpty.toList bvms))

  (PiC ann binder result, p1 : p2 : _) ->
    let (result', bvm2)       = substPos (lowerValue v) p2 result in
    let (positionTree, bvm2') = liftBVM bvm2 in
    let (binder', bvm1)       = substPosBinder v p1 binder positionTree in
    (Pi ann binder' result', nodeBVM [bvm1, bvm2'])

  (LetC ann bound binder body, p1 : p2 : p3 : _) ->
    let (body', bvm3)         = substPos (lowerValue v) p3 body in
    let (positionTree, bvm3') = liftBVM bvm3 in
    let (binder', bvm2)       = substPosBinder v p2 binder positionTree in
    let (bound', bvm1)        = substPos v p1 bound in
    (Let ann bound' binder' body', nodeBVM [bvm1, bvm2, bvm3'])

  (LamC ann binder body, p1 : p2 : _) ->
    let (body', bvm2)         = substPos (lowerValue v) p2 body in
    let (positionTree, bvm2') = liftBVM bvm2 in
    let (binder', bvm1)       = substPosBinder v p1 binder positionTree in
    (Lam ann binder' body', nodeBVM [bvm1, bvm2'])

  (_, ps) -> developerError $
    "Expected the same number of PositionTrees as args but found" <+> pretty (length ps)
  where
    lowerValue :: CoDBExpr -> CoDBExpr
    lowerValue (e, bvm) = (e, lowerBVM Nothing bvm)

substPosArg :: CoDBExpr -> Maybe PositionTree -> CoDBArg -> CoDBArg
substPosArg v p arg = case recCoDB arg of
  (Arg ann vis r e) ->
    let (e', bvm) = substPos v p e in
    (Arg ann vis r e', bvm)

substPosBinder :: CoDBExpr
               -> Maybe PositionTree
               -> CoDBBinder
               -> Maybe PositionTree
               -> CoDBBinder
substPosBinder v p binder boundPositions = case recCoDB binder of
  (Binder ann vis r (CoDBBinding n _) t) ->
    let (t', bvm) = substPos v p t in
    (Binder ann vis r (CoDBBinding n boundPositions) t', bvm)

invalidPositionTreeError :: PositionList -> a
invalidPositionTreeError l = developerError $
  "Whilst performing a positional substitution expected a Leaf" <+>
  "but found" <+> squotes (pretty l)

--------------------------------------------------------------------------------
-- Lifting

liftFreeCoDBIndices :: CoDBExpr -> CoDBExpr
liftFreeCoDBIndices (e, bvm) = (e, IntMap.mapKeysMonotonic (\x -> x - 1) bvm)

lowerFreeCoDBIndices :: CoDBExpr -> CoDBExpr
lowerFreeCoDBIndices (e, bvm) = (e, IntMap.mapKeysMonotonic (+ 1) bvm)
