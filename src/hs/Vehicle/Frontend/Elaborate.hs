module Vehicle.Frontend.Elaborate
  ( Elab(..)
  ) where

import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty)

import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Frontend.AST qualified as VF

import Vehicle.Prelude
import Vehicle.Error

-- * Elaboration class

--------------------------------------------------------------------------------
-- $sugar
-- The following definitions are tactics for unfolding various bits of syntactic
-- sugar. These are unfolded /before/ type-checking occurs, as Frontend is never
-- type-checked. Therefore, more clever bits have to wait until type-checking.
--
-- The following pieces of syntactic sugar are unfolded here:
--
--   * @forall a b. TYPE@
--     is unfolded to
--     @(forall a ?_ (forall b ?_ TYPE))@
--     (see 'elabTForalls')
--
--   * @\x y -> EXPR@
--     is unfolded to
--     @(lambda ( x ?_ ) (lambda ( y ?_ ) EXPR))@
--     (see 'elabELams')
--
--   * @\{x y} -> EXPR@
--     is unfolded to
--     @(lambda { x ?_ } (lambda { y ?_ } EXPR))@
--     (see 'elabELams')
--
--   * @let { x : Nat ; x = 1 ; y : Nat ; y = 2 } in EXPR@
--     is unfolded to
--     @(let ( x Nat ) 1 (let ( y Nat ) 2 EXPR@
--
--   * infix operators are rewritten to Polish notation, e.g.,
--     @1 + 5@ is rewritten to @(+ 1 5)@
--
--------------------------------------------------------------------------------

-- |Class for the various elaboration functions.
class Elab a b where
  elab :: a -> b

-- |Elaborate a let binding with /multiple/ bindings to a series of let
--  bindings with a single binding each.
elabLetDecl :: VC.InputAnn -> VF.InputLetDecl -> VC.InputExpr -> VC.InputExpr
elabLetDecl ann1 (VF.LetDecl ann2 n e) body = do
  cBody <- body
  VC.Let _ (VC.Binder _ Explicit _ _) (elab e) body

elabForallBinder :: VF.InputExpr -> VF.InputExpr -> VC.InputExpr
elabForallBinder = _

instance Elab VF.InputExpr VC.InputExpr where
  elab = \case
    -- Elaborate kinds.
    VF.Kind              -> VC.Kind
    VF.Type   ann        -> op0 VC.Type ann

    -- Elaborate types.
    VF.Forall ann ns t   -> foldr _ (elab t) (fmap elab ns)
    VF.Fun    ann t1 t2  -> VC.Pi ann (VC.Binder _ Explicit _ (elab t1)) (elab t2)
    VF.Bool   ann        -> op0 (VC.PrimitiveTruth  TBool) ann
    VF.Prop   ann        -> op0 (VC.PrimitiveTruth  TProp) ann
    VF.Real   ann        -> op0 (VC.PrimitiveNumber TReal) ann
    VF.Int    ann        -> op0 (VC.PrimitiveNumber TInt)  ann
    VF.List   ann t      -> op1 VC.List   ann t
    VF.Tensor ann t1 t2  -> op2 VC.Tensor ann t1 t2

    -- Elaborate expressions.
    VF.Ann     ann e t   -> VC.Ann ann (elab e) (elab t)
    VF.Let     ann ds e  -> foldr (elabLetDecl ann) (elab e) ds
    VF.Lam     ann ns e  -> foldr (VC.Lam ann) (elab e) (fmap elab ns)
    VF.App     ann e1 e2 -> VC.App ann (elab e1) (elab e2)
    VF.Var     ann n     -> VC.Bound ann n
    VF.Literal ann l     -> VC.Literal ann l

    -- Conditional expressions.
    VF.If    ann e1 e2 e3 -> op3 VC.If      ann e1 e2 e3
    VF.Impl  ann e1 e2    -> op2 VC.Impl    ann e1 e2
    VF.And   ann e1 e2    -> op2 VC.And     ann e1 e2
    VF.Or    ann e1 e2    -> op2 VC.Or      ann e1 e2
    VF.Not   ann e        -> op1 VC.Not     ann e

    -- Integers and reals.
    VF.Eq      ann e1 e2 -> op2 VC.Eq  ann e1 e2
    VF.Neq     ann e1 e2 -> op2 VC.Neq ann e1 e2
    VF.Le      ann e1 e2 -> op2 VC.Le  ann e1 e2
    VF.Lt      ann e1 e2 -> op2 VC.Lt  ann e1 e2
    VF.Ge      ann e1 e2 -> op2 VC.Ge  ann e1 e2
    VF.Gt      ann e1 e2 -> op2 VC.Gt  ann e1 e2
    VF.Mul     ann e1 e2 -> op2 VC.Mul ann e1 e2
    VF.Div     ann e1 e2 -> op2 VC.Div ann e1 e2
    VF.Add     ann e1 e2 -> op2 VC.Add ann e1 e2
    VF.Sub     ann e1 e2 -> op2 VC.Sub ann e1 e2
    VF.Neg     ann e     -> op1 VC.Neg ann e

    -- Lists and tensors.
    VF.Cons   ann e1 e2 -> op2 VC.Cons ann e1 e2
    VF.At     ann e1 e2 -> op2 VC.At   ann e1 e2
    VF.All    ann       -> op0 VC.All  ann
    VF.Any    ann       -> op0 VC.Any  ann
    VF.Seq    ann es    -> VC.Seq ann (fmap elab es)

-- |Elaborate declarations.
instance Elab VF.InputDecl VC.InputDecl where
  elab (VF.DeclNetw ann n t)      = VC.DeclNetw ann (elab n) (elab t)
  elab (VF.DeclData ann n t)      = VC.DeclData ann (elab n) (elab t)
  elab (VF.DefType  ann n ns t)   = VC.DefType  ann (elab n) (fmap elab ns) (elab t)
  elab (VF.DefFun   ann n t ns e) = VC.DefFun   ann (elab n) (elab t) expr
    where
      expr = foldr (VC.Lam ann) (elab e) (fmap elab ns)

-- |Elaborate programs.
instance Elab VF.InputProg VC.InputProg where
  elab (VF.Main decls) = VC.Main (fmap elab decls)

-- |Elaborate any builtin token to an expression.
op0 :: VC.Builtin -> VF.InputAnn -> VC.InputExpr
op0 b ann = VC.Builtin ann b

-- |Elaborate a unary function symbol with its argument to an expression.
op1 :: VC.Builtin -> VF.InputAnn -> VF.InputExpr -> VC.InputExpr
op1 b ann e1 = VC.App ann (op0 b ann) (elab e1)

-- |Elaborate a binary function symbol with its arguments to an expression.
op2 :: VC.Builtin -> VF.InputAnn -> VF.InputExpr -> VF.InputExpr -> VC.InputExpr
op2 b ann e1 e2 = VC.App ann (op1 b ann e1) (elab e2)

-- |Elaborate a binary function symbol with its arguments to an expression.
op3 :: VC.Builtin -> VF.InputAnn -> VF.InputExpr -> VF.InputExpr -> VF.InputExpr -> VC.InputExpr
op3 b ann e1 e2 e3 = VC.App ann (op2 b ann e1 e2) (elab e3)