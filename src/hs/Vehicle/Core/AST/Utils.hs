{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedLists #-}

module Vehicle.Core.AST.Utils where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn
import Vehicle.Core.AST.Builtin

--------------------------------------------------------------------------------
-- Patterns

pattern Type0 :: Expr var ann
pattern Type0 = Type 0

pattern Type1 :: Expr var ann
pattern Type1 = Type 1

pattern LitNat :: ann -> Int -> Expr var ann
pattern LitNat ann n = Literal ann (LNat n)

pattern LitInt :: ann -> Int -> Expr var ann
pattern LitInt ann n = Literal ann (LInt n)

pattern LitRat :: ann -> Double -> Expr var ann
pattern LitRat ann n = Literal ann (LRat n)

pattern LitBool :: ann -> Bool -> Expr var ann
pattern LitBool ann n = Literal ann (LBool n)

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

type InputVar  = Name
type InputAnn  = Provenance

type InputArg       = Arg    InputVar InputAnn
type InputBinder    = Binder InputVar InputAnn
type InputExpr      = Expr   InputVar InputAnn
type InputDecl      = Decl   InputVar InputAnn
type InputProg      = Prog   InputVar InputAnn

-- * Types pre type-checking

type UncheckedVar    = Var
type UncheckedAnn    = Provenance

type UncheckedBinder = DeBruijnBinder UncheckedAnn
type UncheckedArg    = DeBruijnArg    UncheckedAnn
type UncheckedExpr   = DeBruijnExpr   UncheckedAnn
type UncheckedDecl   = DeBruijnDecl   UncheckedAnn
type UncheckedProg   = DeBruijnProg   UncheckedAnn

-- * Types post type-checking

type CheckedVar    = Var
type CheckedAnn    = Provenance

type CheckedBinder = DeBruijnBinder  CheckedAnn
type CheckedArg    = DeBruijnArg     CheckedAnn
type CheckedExpr   = DeBruijnExpr    CheckedAnn
type CheckedDecl   = DeBruijnDecl    CheckedAnn
type CheckedProg   = DeBruijnProg    CheckedAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputVar  = Name
type OutputAnn  = Provenance

type OutputBinder = Binder OutputVar OutputAnn
type OutputArg    = Arg    OutputVar OutputAnn
type OutputExpr   = Expr   OutputVar OutputAnn
type OutputDecl   = Decl   OutputVar OutputAnn
type OutputProg   = Prog   OutputVar OutputAnn

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance ann => HasProvenance (Arg var ann) where
  prov (Arg Explicit e) = prov e
  prov (Arg Implicit e) = expandProvenance (1, 1) (prov e)
  prov (Arg Instance e) = expandProvenance (2, 2) (prov e)

instance HasProvenance ann => HasProvenance (Expr var ann) where
  prov (Hole p _) = p
  prov e          = prov (annotation e)

instance HasProvenance ann => HasProvenance (Decl var ann) where
  prov (DeclNetw p _ _) = p
  prov (DeclData p _ _) = p
  prov (DefFun p _ _ _) = p

--------------------------------------------------------------------------------
-- Utility functions

-- |Extract a binder's name
binderName :: Binder var ann -> Name
binderName (Binder _ _ name _) = name

binderType :: Binder var ann -> Expr var ann
binderType (Binder _ _ _ t) = t

argExpr :: Arg var ann -> Expr var ann
argExpr (Arg _ e) = e

isHole :: Expr var ann -> Bool
isHole (Hole _ _ ) = True
isHole _           = False

mapArgExpr :: (Expr var ann -> Expr var ann) -> Arg var ann -> Arg var ann
mapArgExpr f (Arg v e) = Arg v $ f e

traverseArgExpr :: Monad m => (Expr var ann -> m (Expr var ann)) -> Arg var ann -> m (Arg var ann)
traverseArgExpr f (Arg v e) = Arg v <$> f e

traverseBinderExpr :: Monad m => (Expr var ann -> m (Expr var ann)) -> Binder var ann -> m (Binder var ann)
traverseBinderExpr f (Binder p v n e) = Binder p v n <$> f e

declIdent :: Decl var ann -> Identifier
declIdent (DeclNetw _ ident _) = ident
declIdent (DeclData _ ident _) = ident
declIdent (DefFun _ ident _ _) = ident

declType :: Decl var ann -> Expr var ann
declType (DeclNetw _ _ t) = t
declType (DeclData _ _ t) = t
declType (DefFun _ _ t _) = t

-- |Extract a term's annotation
annotation :: Expr name ann -> ann
annotation = \case
  Type     _         -> developerError "Should not be requesting an annotation from Type"
  Hole     _   _     -> developerError "Should not be requesting an annotation from Hole"
  PrimDict _         -> developerError "Should not be requesting an annotation from PrimitiveDict"
  Meta     ann _     -> ann
  Ann      ann _ _   -> ann
  App      ann _ _   -> ann
  Pi       ann _ _   -> ann
  Builtin  ann _     -> ann
  Var      ann _     -> ann
  Let      ann _ _ _ -> ann
  Lam      ann _ _   -> ann
  Literal  ann _     -> ann
  Seq      ann _     -> ann

freeNames :: CheckedExpr -> [Identifier]
freeNames = cata $ \case
  TypeF     _                   -> []
  HoleF     _   _               -> []
  PrimDictF _                   -> []
  MetaF     _ _                 -> []
  LiteralF  _ _                 -> []
  BuiltinF  _ _                 -> []
  AnnF      _ e t               -> e <> t
  AppF      _ fun args          -> fun <> concatMap (freeNames . argExpr) args
  PiF       _ binder result     -> freeNames (binderType binder) <> result
  VarF      _ (Free ident)      -> [ident]
  VarF      _ (Bound _)         -> []
  LetF      _ bound binder body -> bound <> freeNames (binderType binder) <> body
  LamF      _ binder body       -> freeNames (binderType binder) <> body
  SeqF      _ xs                -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr var ann -> (Expr var ann, [Arg var ann])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

exprHead :: Expr var ann -> Expr var ann
exprHead = fst . toHead

--------------------------------------------------------------------------------
-- Views

data QuantView var ann = QuantView ann Quantifier Name (Expr var ann) (Expr var ann)

quantView :: Expr var ann -> Maybe (QuantView var ann)
quantView (App ann (Builtin _ (Quant q)) (Arg Explicit
  (Lam _ (Binder _ Explicit n t) e) :| [])) = Just (QuantView ann q n t e)
quantView _ = Nothing

--------------------------------------------------------------------------------
-- Construction functions

-- Primed versions take `Arg` instead of `Expr`

-- Types

mkListType :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkListType ann tElem = App ann (Builtin ann List) [Arg Explicit tElem]

mkTensorType :: CheckedAnn -> CheckedExpr -> [Int] -> CheckedExpr
mkTensorType ann tElem dims =
  let dimExprs = fmap (Literal ann . LNat)  dims in
  let dimList  = mkSeq ann (Builtin ann Nat) (mkListType ann (Builtin ann Nat)) dimExprs in
  App ann (Builtin ann Tensor) [Arg Explicit tElem, Arg Explicit dimList]

mkIsTruth :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkIsTruth ann t = App ann (Builtin ann IsTruth) [Arg Explicit t]

mkIsContainer :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkIsContainer ann tElem tCont = App ann (Builtin ann IsContainer)
  [Arg Explicit tElem, Arg Explicit tCont]

-- Expressions

mkLiteral' :: CheckedAnn -> Literal -> CheckedArg -> CheckedArg -> CheckedExpr
mkLiteral' ann lit t tc = App ann (Literal ann lit) [t, tc]

mkLiteral :: CheckedAnn -> Literal -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkLiteral ann lit t tc = mkLiteral' ann lit
  (Arg Implicit t)
  (Arg Instance (PrimDict tc))

mkBool :: CheckedAnn -> CheckedExpr -> Bool -> CheckedExpr
mkBool ann t b = mkLiteral ann (LBool b) t (PrimDict t)

mkNat :: CheckedAnn -> Int -> CheckedExpr
mkNat ann n = let t = Builtin ann Nat in mkLiteral ann (LNat n) t (PrimDict t)

mkInt :: CheckedAnn -> Int -> CheckedExpr
mkInt ann i = let t = Builtin ann Int in mkLiteral ann (LInt i) t (PrimDict t)

mkReal :: CheckedAnn -> Double -> CheckedExpr
mkReal ann r = let t = Builtin ann Real in mkLiteral ann (LRat r) t (PrimDict t)

mkSeq' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> [CheckedExpr] -> CheckedExpr
mkSeq' ann tElem tCont tc xs = App ann (Seq ann xs) [tElem, tCont, tc]

mkSeq :: CheckedAnn -> CheckedExpr -> CheckedExpr -> [CheckedExpr] -> CheckedExpr
mkSeq ann tElem tCont = mkSeq' ann
  (Arg Implicit tElem)
  (Arg Implicit tCont)
  (Arg Instance (PrimDict (mkIsContainer ann tElem tCont)))

-- Expressions

mkEq' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkEq' ann tElem tRes tc e1 e2 = App ann (Builtin ann Eq)
  [tElem, tRes, tc, Arg Explicit e1, Arg Explicit e2]

mkEq :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkEq ann tElem tRes = mkEq' ann
  (Arg Implicit tElem)
  (Arg Implicit tRes)
  (Arg Instance (PrimDict (mkIsTruth ann tRes)))

mkNot' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr
mkNot' ann t tc e = App ann (Builtin ann Not) [t, tc, Arg Explicit e]

mkNot :: CheckedAnn -> CheckedExpr -> CheckedExpr  -> CheckedExpr
mkNot ann t = mkNot' ann
  (Arg Implicit t)
  (Arg Instance (PrimDict (mkIsTruth ann t)))

mkBoolOp2' :: Builtin -> CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkBoolOp2' op ann t tc e1 e2 = App ann (Builtin ann op)
  [t, tc, Arg Explicit e1, Arg Explicit e2]

mkBoolOp2 :: Builtin -> CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkBoolOp2 op ann t = mkBoolOp2' op ann
  (Arg Implicit t)
  (Arg Instance (PrimDict (mkIsTruth ann t)))

mkImplies' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkImplies' = mkBoolOp2' Impl

mkImplies :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkImplies = mkBoolOp2 Impl

mkAnd' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkAnd' = mkBoolOp2' And

mkAnd :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkAnd = mkBoolOp2 And

mkOr' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkOr' = mkBoolOp2' Or

mkOr :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkOr = mkBoolOp2 Or

mkAt' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkAt' ann tElem tCont tc xs i = App ann (Builtin ann At)
  [tElem, tCont, tc, Arg Explicit xs , Arg Explicit i ]

mkMap' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr
mkMap' ann tFrom tTo f xs = App ann (Builtin ann Map)
  [tFrom, tTo, f, xs]

mkFold' :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkFold' ann tElem tCont tRes tc bop unit xs = App ann (Builtin ann Fold)
  [tElem, tCont, tRes, tc, Arg Explicit bop, Arg Explicit unit, Arg Explicit xs]

mkQuantifier :: CheckedAnn -> Quantifier -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkQuantifier ann q n t e =
  App ann (Builtin ann (Quant q))
    (Arg Explicit (Lam ann (Binder ann Explicit n t) e) :| [])

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Name -> [Int] -> Name
mkNameWithIndices Machine  _       = Machine
mkNameWithIndices (User n) indices = User $
  mconcat (n : ["_" <> pack (show index) | index <- indices])

substContainerType :: CheckedArg -> CheckedExpr -> CheckedExpr
substContainerType newTElem (App ann1 (Builtin ann2 List)   [_tElem]) =
  App ann1 (Builtin ann2 List) [newTElem]
substContainerType newTElem (App ann1 (Builtin ann2 Tensor) [_tElem, tDims])  =
  App ann1 (Builtin ann2 Tensor) [newTElem, tDims]
substContainerType _ _ = developerError "Provided an invalid container type"