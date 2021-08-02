module Vehicle.Frontend.Elaborate
  ( runElab
  ) where

import Control.Monad.Identity (runIdentity)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Prelude
import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Frontend.AST qualified as VF

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

runElab :: VF.InputProg -> VC.InputProg
runElab prog = runIdentity $ elab prog

type MonadElab m = Monad m

-- |Class for the various elaboration functions.
class Elab a b where
  elab :: MonadElab m => a -> m b

hole :: MonadElab m => Provenance -> m VC.InputExpr
hole p = return $ VC.Hole p "_"

-- |Elaborate a let binding with /multiple/ bindings to a series of let
--  bindings with a single binding each.
elabLetDecls :: MonadElab m => VF.InputExpr -> [VF.InputLetDecl] -> m VC.InputExpr
elabLetDecls = foldr elabLetDecl . elab
  where
    elabLetDecl :: MonadElab m => VF.InputLetDecl -> m VC.InputExpr -> m VC.InputExpr
    elabLetDecl (VF.LetDecl ann binder e) body = VC.Let ann <$> elab binder <*> elab e <*> body

elabBinders :: MonadElab m => (VC.InputBinder -> VC.InputExpr -> VC.InputExpr) -> [VF.InputBinder] -> VF.InputExpr -> m VC.InputExpr
elabBinders f bs body = foldr (\b d -> f <$> elab b <*> d) (elab body) bs

elabFunInputType :: MonadElab m => VF.InputExpr -> m VC.InputBinder
elabFunInputType t = VC.Binder (VF.annotation t) Explicit "@funplaceholder" <$> elab t -- TODO really need to come up with a more satisfying way

instance Elab VF.InputBinder VC.InputBinder where
  elab (VF.Binder ann vis name t) = VC.Binder ann vis name <$> maybe (hole (prov ann)) elab t

instance Elab VF.InputArg VC.InputArg where
  elab (VF.Arg ann vis e) = VC.Arg ann vis <$> elab e

instance Elab VF.InputExpr VC.InputExpr where
  elab = \case
    -- Core.
    VF.Forall  ann ns t   -> elabBinders (VC.Pi ann) (NonEmpty.toList ns) t
    VF.Fun     ann t1 t2  -> VC.Pi ann <$> elabFunInputType t1 <*> elab t2
    VF.Ann     ann e t    -> VC.Ann ann <$> elab e <*> elab t
    VF.Let    _ann ds e   -> elabLetDecls e (NonEmpty.toList ds)
    VF.Lam     ann ns e   -> elabBinders (VC.Lam ann) (NonEmpty.toList ns) e
    VF.App     ann e1 e2  -> VC.App ann <$> elab e1 <*> elab e2
    VF.Var     ann n      -> return $ VC.Var ann n
    VF.Literal ann l      -> return $ VC.Literal ann l
    VF.Hole    ann name   -> return $ VC.Hole ann name

    -- Kinds.
    VF.Type l            -> return (VC.Type l)
    VF.Constraint        -> return VC.Constraint

    -- Types.
    VF.Bool    ann        -> op0 VC.Bool   ann
    VF.Prop    ann        -> op0 VC.Prop   ann
    VF.Real    ann        -> op0 VC.Real   ann
    VF.Int     ann        -> op0 VC.Int    ann
    VF.Nat     ann        -> op0 VC.Nat    ann
    VF.List    ann t      -> op1 VC.List   ann t
    VF.Tensor  ann t1 t2  -> op2 VC.Tensor ann t1 t2

    -- Type classes.
    VF.HasEq       ann e1 e2 -> op2 VC.HasEq          ann e1 e2
    VF.HasOrd      ann e1 e2 -> op2 VC.HasOrd         ann e1 e2
    VF.IsContainer ann e1 e2 -> op2 VC.IsContainer    ann e1 e2
    VF.IsTruth     ann e     -> op1 VC.IsTruth        ann e
    VF.IsQuant     ann e     -> op1 VC.IsQuantifiable ann e
    VF.IsNatural   ann e     -> op1 VC.IsNatural      ann e
    VF.IsIntegral  ann e     -> op1 VC.IsIntegral     ann e
    VF.IsRational  ann e     -> op1 VC.IsRational     ann e
    VF.IsReal      ann e     -> op1 VC.IsReal         ann e

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
    VF.Seq    ann es    -> VC.Seq ann <$> traverse elab es

-- |Elaborate declarations.
instance Elab VF.InputDecl VC.InputDecl where
  elab (VF.DeclNetw ann n t)      = VC.DeclNetw ann n <$> elab t
  elab (VF.DeclData ann n t)      = VC.DeclData ann n <$> elab t
  elab (VF.DefType  ann n ns e)   = VC.DefFun   ann n <$> hole (prov ann) <*> elabBinders (VC.Lam ann) ns e
  elab (VF.DefFun   ann n t ns e) = VC.DefFun   ann n <$> elab t               <*> elabBinders (VC.Lam ann) ns e

-- |Elaborate programs.
instance Elab VF.InputProg VC.InputProg where
  elab (VF.Main decls) = VC.Main <$> traverse elab decls

-- |Elaborate a builtin argument to an application Arg
exprToArg :: MonadElab m => VF.InputExpr -> m VC.InputArg
exprToArg e = VC.Arg (VF.annotation e) Explicit <$> elab e

-- |Elaborate any builtin token to an expression.
op0 :: MonadElab m => VC.Builtin -> VF.InputAnn -> m VC.InputExpr
op0 b ann = return $ VC.Builtin ann b

-- |Elaborate a unary function symbol with its argument to an expression.
op1 :: MonadElab m => VC.Builtin -> VF.InputAnn -> VF.InputExpr -> m VC.InputExpr
op1 b ann e1 = VC.App ann <$> op0 b ann <*> exprToArg e1

-- |Elaborate a binary function symbol with its arguments to an expression.
op2 :: MonadElab m => VC.Builtin -> VF.InputAnn -> VF.InputExpr -> VF.InputExpr -> m VC.InputExpr
op2 b ann e1 e2 = VC.App ann <$> op1 b ann e1 <*> exprToArg e2

-- |Elaborate a binary function symbol with its arguments to an expression.
op3 :: MonadElab m => VC.Builtin -> VF.InputAnn -> VF.InputExpr -> VF.InputExpr -> VF.InputExpr -> m VC.InputExpr
op3 b ann e1 e2 e3 = VC.App ann <$> op2 b ann e1 e2 <*> exprToArg e3