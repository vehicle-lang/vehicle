
module Vehicle.Frontend.AST.Utils where

import Numeric.Natural (Natural)

import Vehicle.Prelude
import Vehicle.Frontend.AST.Core

-- |Extract the top-level annotation from a expression
annotation :: Expr ann -> ann
annotation = \case
  -- Kinds
  Kind                    -> error "Should not be requesting an annotation from Kind"
  Type    ann             -> ann

  -- Types
  Forall     ann _ns _t  -> ann
  Fun        ann _t1 _t2 -> ann
  Bool       ann         -> ann
  Prop       ann         -> ann
  Real       ann         -> ann
  Int        ann         -> ann
  List       ann _t      -> ann
  Tensor     ann _t1 _t2 -> ann

  -- Expressions
  Ann     ann _e _t       -> ann
  Literal ann _l          -> ann
  Var     ann _n          -> ann
  Let     ann _ds _e      -> ann
  Lam     ann _ns _e      -> ann
  App     ann _e1 _e2     -> ann
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
  All     ann             -> ann
  Any     ann             -> ann
  Seq     ann _es         -> ann

-- * Type synonyms for literals

pattern LitNat :: ann -> Natural -> Expr ann
pattern LitNat ann n = Literal ann (LNat n)

pattern LitInt :: ann -> Integer -> Expr ann
pattern LitInt ann n = Literal ann (LInt n)

pattern LitReal :: ann -> Double -> Expr ann
pattern LitReal ann n = Literal ann (LReal n)

pattern LitBool :: ann -> Bool -> Expr ann
pattern LitBool ann n = Literal ann (LBool n)


-- * Type of annotations attached to the Frontend AST after parsing
-- before being analysed by the compiler

data MaybeRecAnn ann = MaybeRecAnn (Maybe (Expr (RecAnn ann))) ann

type InputAnn = Provenance

type InputArg     = Arg     InputAnn
type InputLetDecl = LetDecl InputAnn
type InputExpr    = Expr    InputAnn
type InputDecl    = Decl    InputAnn
type InputIdent   = Ident   InputAnn
type InputProg    = Prog    InputAnn

{-
instance KnownSort sort => HasProvenance (InputAnn sort) where
  prov (K p) = p

instance KnownSort sort => HasProvenance (InputTree sort) where
  prov = prov . annotation
-}

-- * Type of annotations attached to the Frontend AST that are output by the compiler

-- | An annotation that stores both the type of the expression and some other arbitrary annotations.
-- Used to avoid unrestricted type-level recursion.
data RecAnn ann = RecAnn (Expr (RecAnn ann)) ann

type OutputAnn = RecAnn Provenance

type OutputArg     = Arg     OutputAnn
type OutputLetDecl = LetDecl OutputAnn
type OutputExpr    = Expr    OutputAnn
type OutputDecl    = Decl    OutputAnn
type OutputIdent   = Ident   OutputAnn
type OutputProg    = Prog    OutputAnn

{-
instance KnownSort sort => HasProvenance (OutputAnn sort) where
  prov (_ :*: K p) = p

instance KnownSort sort => HasProvenance (OutputTree sort) where
  prov = prov . annotation

pattern Let1 :: ann -> ann -> Ident ann -> Expr ann -> Expr ann -> Expr ann
pattern Let1 ann1 ann2 n e1 e2 = Let ann1 (LetDecl ann2 n e1 :| []) e2
-}