module Vehicle.Frontend.Elaborate
  ( runElab
  ) where

import Control.Monad.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Prelude
import Vehicle.Core.AST qualified as VC
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
--   * quantifiers over lists are rewritten to folds
--------------------------------------------------------------------------------

runElab :: VF.InputProg -> VC.InputProg
runElab prog = runIdentity $! elab prog

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
    elabLetDecl (VF.LetDecl ann binder e) body = VC.Let ann <$> elab e <*> elab binder <*> body

elabAndReturnBinders :: MonadElab m =>
                        (VC.InputBinder -> VC.InputExpr -> VC.InputExpr) ->
                        [VF.InputBinder] ->
                        VF.InputExpr ->
                        m (VC.InputExpr, [VC.InputBinder])
elabAndReturnBinders _ []       body = elab body >>= (\v -> return (v , []))
elabAndReturnBinders f (b : bs) body = do
  b' <- elab b
  (e' , bs') <- elabAndReturnBinders f bs body
  return (f b' e' , b' : bs')

elabBinders :: MonadElab m => (VC.InputBinder -> VC.InputExpr -> VC.InputExpr) -> [VF.InputBinder] -> VF.InputExpr -> m VC.InputExpr
elabBinders f bs body = fst <$> elabAndReturnBinders f bs body

elabFunInputType :: MonadElab m => VF.InputExpr -> m VC.InputBinder
elabFunInputType t = VC.Binder (VF.annotation t) Explicit VC.Machine <$> elab t

instance Elab VF.InputBinder VC.InputBinder where
  elab (VF.Binder ann v n t) = VC.Binder ann v (VC.User n) <$> maybe (hole (prov ann)) elab t

instance Elab VF.InputArg VC.InputArg where
  elab (VF.Arg ann v e) = VC.Arg ann v <$> elab e

instance Elab VF.InputExpr VC.InputExpr where
  elab = \case
    VF.PrimDict _e          -> developerError "PrimDict not supported during elaboration"

    -- Core.
    VF.Type l               -> return (VC.Type l)
    VF.Forall  ann ns t     -> elabBinders (VC.Pi ann) (NonEmpty.toList ns) t
    VF.Fun     ann t1 t2    -> VC.Pi ann <$> elabFunInputType t1 <*> elab t2
    VF.Ann     ann e t      -> VC.Ann ann <$> elab e <*> elab t
    VF.Let    _ann ds e     -> elabLetDecls e (NonEmpty.toList ds)
    VF.Lam     ann ns e     -> elabBinders (VC.Lam ann) (NonEmpty.toList ns) e
    VF.Var     ann n        -> return $ VC.Var ann (VC.User n)
    VF.Literal ann l        -> return $ VC.Literal ann l
    VF.Hole    ann name     -> return $ VC.Hole ann name
    VF.App     ann fun arg  -> do
      fun' <- elab fun
      arg' <- elab arg
      return $ VC.normApp ann fun' (arg' :| [])

    -- Types.
    VF.Bool    ann        -> op VC.Bool   ann []
    VF.Prop    ann        -> op VC.Prop   ann []
    VF.Real    ann        -> op VC.Real   ann []
    VF.Int     ann        -> op VC.Int    ann []
    VF.Nat     ann        -> op VC.Nat    ann []
    VF.List    ann t      -> op VC.List   ann [t]
    VF.Tensor  ann t1 t2  -> op VC.Tensor ann [t1, t2]

    -- Type classes.
    VF.HasEq       ann e1 e2 -> op VC.HasEq          ann [e1, e2]
    VF.HasOrd      ann e1 e2 -> op VC.HasOrd         ann [e1, e2]
    VF.IsContainer ann e1 e2 -> op VC.IsContainer    ann [e1, e2]
    VF.IsTruth     ann e     -> op VC.IsTruth        ann [e]
    VF.IsQuant     ann e     -> op VC.IsQuantifiable ann [e]
    VF.IsNatural   ann e     -> op VC.IsNatural      ann [e]
    VF.IsIntegral  ann e     -> op VC.IsIntegral     ann [e]
    VF.IsRational  ann e     -> op VC.IsRational     ann [e]
    VF.IsReal      ann e     -> op VC.IsReal         ann [e]

    -- Conditional expressions.
    VF.If    ann e1 e2 e3 -> op VC.If      ann [e1, e2, e3]
    VF.Impl  ann e1 e2    -> op VC.Impl    ann [e1, e2]
    VF.And   ann e1 e2    -> op VC.And     ann [e1, e2]
    VF.Or    ann e1 e2    -> op VC.Or      ann [e1, e2]
    VF.Not   ann e        -> op VC.Not     ann [e]

    -- Integers and reals.
    VF.Eq      ann e1 e2 -> op VC.Eq  ann [e1, e2]
    VF.Neq     ann e1 e2 -> op VC.Neq ann [e1, e2]
    VF.Le      ann e1 e2 -> op (VC.Order Le) ann [e1, e2]
    VF.Lt      ann e1 e2 -> op (VC.Order Lt) ann [e1, e2]
    VF.Ge      ann e1 e2 -> op (VC.Order Ge) ann [e1, e2]
    VF.Gt      ann e1 e2 -> op (VC.Order Gt) ann [e1, e2]
    VF.Mul     ann e1 e2 -> op VC.Mul ann [e1, e2]
    VF.Div     ann e1 e2 -> op VC.Div ann [e1, e2]
    VF.Add     ann e1 e2 -> op VC.Add ann [e1, e2]
    VF.Sub     ann e1 e2 -> op VC.Sub ann [e1, e2]
    VF.Neg     ann e     -> op VC.Neg ann [e]

    -- Lists and tensors.
    VF.Seq     ann es        -> VC.Seq ann <$> traverse elab es
    VF.Cons    ann e1 e2     -> op VC.Cons ann [e1, e2]
    VF.At      ann e1 e2     -> op VC.At   ann [e1, e2]
    VF.Map     ann e1 e2     -> op VC.Map  ann [e1, e2]
    VF.Fold    ann e1 e2 e3  -> op VC.Fold ann [e1, e2, e3]
    VF.Quant   ann q n e     -> op (VC.Quant q) ann [VF.Lam ann (n :| []) e]
    VF.QuantIn ann q n e1 e2 -> quantIn ann q n e1 e2

-- |Elaborate declarations.
instance Elab VF.InputDecl VC.InputDecl where
  elab (VF.DeclNetw ann n t)      = VC.DeclNetw ann n <$> elab t
  elab (VF.DeclData ann n t)      = VC.DeclData ann n <$> elab t
  elab (VF.DefFun   ann n t ns e) = VC.DefFun   ann n <$> elab t <*> elabBinders (VC.Lam ann) ns e
  elab (VF.DefType  ann n ns e)   = do
    (body, binders) <- elabAndReturnBinders (VC.Lam ann) ns e
    let t = typeDefType binders
    return $ VC.DefFun ann n t body

-- |Elaborate programs.
instance Elab VF.InputProg VC.InputProg where
  elab (VF.Main decls) = VC.Main <$> traverse elab decls

-- |Construct the type for a type definition
typeDefType :: [VC.InputBinder] -> VC.InputExpr
typeDefType = foldr (\b -> VC.Pi (prov b) b) VC.Type0

op :: MonadElab m => VC.Builtin -> VF.InputAnn -> [VF.InputExpr] -> m VC.InputExpr
op b ann args = opC b ann <$> traverse elab args

opC :: VC.Builtin -> VF.InputAnn -> [VC.InputExpr] -> VC.InputExpr
opC b ann args = VC.normAppList ann (VC.Builtin ann b) (fmap (VC.Arg ann Explicit) args)

-- |Elaborate quantification over the members of a container type.
-- Expands e.g. `every x in list . y` to `fold and true (map (\x -> y) list)`
quantIn :: MonadElab m => VC.InputAnn -> Quantifier -> VF.InputBinder -> VF.InputExpr -> VF.InputExpr -> m VC.InputExpr
quantIn ann quantifier n container body = do
  let lam = VF.Lam ann (n :| []) body
  op (VC.QuantIn quantifier) ann [lam, container]