{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Frontend.AST.Utils where

import Vehicle.Prelude hiding (Le, Ge, Lt, Gt)
import Vehicle.Frontend.AST.Core

--------------------------------------------------------------------------------
-- Type synonyms for literals

pattern LitNat :: ann -> Int -> Expr ann
pattern LitNat ann n = Literal ann (LNat n)

pattern LitInt :: ann -> Int -> Expr ann
pattern LitInt ann n = Literal ann (LInt n)

pattern LitRat :: ann -> Double -> Expr ann
pattern LitRat ann n = Literal ann (LRat n)

pattern LitBool :: ann -> Bool -> Expr ann
pattern LitBool ann n = Literal ann (LBool n)

--------------------------------------------------------------------------------
-- Type synonyms

-- * Type of annotations attached to the Frontend AST after parsing before being
-- analysed by the compiler

type InputAnn     = Provenance
type InputArg     = Arg     InputAnn
type InputBinder  = Binder  InputAnn
type InputLetDecl = LetDecl InputAnn
type InputExpr    = Expr    InputAnn
type InputDecl    = Decl    InputAnn
type InputProg    = Prog    InputAnn

-- * Type of annotations attached to the Frontend AST that are output by
-- the compiler

type OutputAnn     = Provenance
type OutputArg     = Arg     OutputAnn
type OutputBinder  = Binder  OutputAnn
type OutputLetDecl = LetDecl OutputAnn
type OutputExpr    = Expr    OutputAnn
type OutputDecl    = Decl    OutputAnn
type OutputProg    = Prog    OutputAnn

--------------------------------------------------------------------------------
-- Utility functions

-- |Extract the top-level annotation from a expression
annotation :: Expr ann -> ann
annotation = \case
  -- Core
  Forall      ann _ns _t  -> ann
  Fun         ann _t1 _t2 -> ann
  Ann         ann _e _t   -> ann
  App         ann _e1 _e2 -> ann
  Let         ann _ds _e  -> ann
  Lam         ann _ns _e  -> ann
  Literal     ann _l      -> ann
  Var         ann _n      -> ann
  Hole _ _    -> developerError "Should not be requesting an annotation from Hole"
  PrimDict tc -> annotation tc

  -- Kinds
  Type _     -> developerError "Should not be requesting an annotation from Type"

  -- Types
  Bool        ann         -> ann
  Prop        ann         -> ann
  Real        ann         -> ann
  Int         ann         -> ann
  Nat         ann         -> ann
  List        ann _t      -> ann
  Tensor      ann _t1 _t2 -> ann

  -- Type classes
  HasEq       ann _e1 _e2 -> ann
  HasOrd      ann _e1 _e2 -> ann
  IsContainer ann _e1 _e2 -> ann
  IsTruth     ann _e      -> ann
  IsQuant     ann _e      -> ann
  IsNatural   ann _e      -> ann
  IsIntegral  ann _e      -> ann
  IsRational  ann _e      -> ann
  IsReal      ann _e      -> ann

  -- Other builtins
  If      ann _e1 _e2 _e3 -> ann
  Impl    ann _e1 _e2     -> ann
  And     ann _e1 _e2     -> ann
  Or      ann _e1 _e2     -> ann
  Not     ann _e          -> ann
  Eq      ann _e1 _e2     -> ann
  Neq     ann _e1 _e2     -> ann
  Le      ann _e1 _e2     -> ann
  Lt      ann _e1 _e2     -> ann
  Ge      ann _e1 _e2     -> ann
  Gt      ann _e1 _e2     -> ann
  Mul     ann _e1 _e2     -> ann
  Div     ann _e1 _e2     -> ann
  Add     ann _e1 _e2     -> ann
  Sub     ann _e1 _e2     -> ann
  Neg     ann _e          -> ann
  Cons    ann _e1 _e2     -> ann
  At      ann _e1 _e2     -> ann
  Quant   ann _q _n _e    -> ann
  QuantIn ann _q _n _ _   -> ann
  Seq     ann _es         -> ann

-- | Extracts the type of the term from the term's annotation.
getType :: Expr (RecAnn ann) -> Expr (RecAnn ann)
getType (Type l)   = Type (l + 1)
getType e          = let RecAnn t _ = annotation e in t

--------------------------------------------------------------------------------
-- Instances

instance HasProvenance ann => HasProvenance (Expr ann) where
  prov (Hole p _) = p
  prov e          = prov (annotation e)

instance HasProvenance (Binder ann) where
  prov (Binder p _ _ _) = p

instance HasProvenance (LetDecl ann) where
  prov (LetDecl p _ _) = p

instance HasProvenance ann => HasProvenance (Arg ann) where
  prov (Arg Explicit e) = prov e
  prov (Arg Implicit e) = expandProvenance (1, 1) (prov e)
  prov (Arg Instance e) = expandProvenance (2, 2) (prov e)