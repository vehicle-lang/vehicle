{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedLists #-}

module Vehicle.Core.AST.Utils where

import Data.List.NonEmpty qualified as NonEmpty (toList)

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

type UncheckedBinder  = DeBruijnBinder UncheckedAnn
type UncheckedArg     = DeBruijnArg    UncheckedAnn
type UncheckedExpr    = DeBruijnExpr   UncheckedAnn
type UncheckedDecl    = DeBruijnDecl   UncheckedAnn
type UncheckedProg    = DeBruijnProg   UncheckedAnn

-- * Types post type-checking

type CheckedVar    = Var
type CheckedAnn    = Provenance

type CheckedBinder  = DeBruijnBinder  CheckedAnn
type CheckedArg     = DeBruijnArg     CheckedAnn
type CheckedExpr    = DeBruijnExpr    CheckedAnn
type CheckedDecl    = DeBruijnDecl    CheckedAnn
type CheckedProg    = DeBruijnProg    CheckedAnn

-- * Type of annotations attached to the Core AST that are output by the compiler

type OutputVar  = Name
type OutputAnn  = Provenance

type OutputBinder  = Binder OutputVar OutputAnn
type OutputArg     = Arg    OutputVar OutputAnn
type OutputExpr    = Expr   OutputVar OutputAnn
type OutputDecl    = Decl   OutputVar OutputAnn
type OutputProg    = Prog   OutputVar OutputAnn

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance ann => HasProvenance (Expr var ann) where
  prov (Hole p _) = p
  prov e          = prov (annotation e)

--------------------------------------------------------------------------------
-- Utility functions

-- |Extract a binder's name
binderName :: Binder var ann -> Name
binderName (Binder _ _ name _) = name

binderType :: Binder var ann -> Expr var ann
binderType (Binder _ _ _ t) = t

argExpr :: Arg var ann -> Expr var ann
argExpr (Arg _ _ e) = e

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

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: CheckedExpr -> (CheckedExpr, [CheckedArg])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

--------------------------------------------------------------------------------
-- Construction functions

mkLiteral :: CheckedAnn -> Literal -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkLiteral ann lit t tc = App ann (Literal ann lit)
  [Arg ann Implicit t, Arg ann Instance (PrimDict tc)]

mkBool :: CheckedAnn -> Bool -> CheckedExpr -> CheckedExpr
mkBool ann b t = mkLiteral ann (LBool b) t (PrimDict t)

mkNat :: CheckedAnn -> Int -> CheckedExpr
mkNat ann n = let t = Builtin ann Nat in mkLiteral ann (LNat n) t (PrimDict t)

mkInt :: CheckedAnn -> Int -> CheckedExpr
mkInt ann i = let t = Builtin ann Int in mkLiteral ann (LInt i) t (PrimDict t)

mkReal :: CheckedAnn -> Double -> CheckedExpr
mkReal ann r = let t = Builtin ann Real in mkLiteral ann (LRat r) t (PrimDict t)

mkIsContainer :: CheckedAnn -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkIsContainer ann tElem tCont = App ann (Builtin ann IsContainer)
  [Arg ann Explicit tElem, Arg ann Explicit tCont]

mkSeq :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> [CheckedExpr] -> CheckedExpr
mkSeq ann tElem tCont tc xs = App ann (Seq ann xs) [tElem, tCont, tc]

mkSeq' :: CheckedAnn -> CheckedExpr -> CheckedExpr -> [Int] -> CheckedExpr
mkSeq' ann tElem tCont elems = mkSeq ann
  (Arg ann Implicit tElem)
  (Arg ann Implicit tCont)
  (Arg ann Instance (PrimDict (mkIsContainer ann tElem tCont)))
  (fmap (Literal ann . LNat) elems)

mkAt :: CheckedAnn -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr -> CheckedExpr -> CheckedExpr
mkAt ann tElem tCont tc xs i = App ann (Builtin ann At)
  [tElem, tCont, tc, Arg ann Explicit xs , Arg ann Explicit i ]

mkListType :: CheckedAnn -> CheckedExpr -> CheckedExpr
mkListType ann tElem = App ann (Builtin ann List) [Arg ann Explicit tElem]

mkTensorType :: CheckedAnn -> CheckedExpr -> [Int] -> CheckedExpr
mkTensorType ann tElem dims =
  let eDims = mkSeq' ann (Builtin ann Nat) (mkListType ann (Builtin ann Nat)) dims in
  App ann (Builtin ann Tensor) [Arg ann Explicit tElem, Arg ann Explicit eDims]