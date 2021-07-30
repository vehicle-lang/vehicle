
module Vehicle.Frontend.Delaborate
  ( runDelab
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState(..), StateT, evalStateT, modify)
import Control.Monad.Except (MonadError(..), Except, runExcept)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Functor.Foldable (Recursive(..))
import Prettyprinter (pretty, (<+>))

import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Delaboration converts a program in the Core language to the Frontend language

runDelab :: VC.OutputProg -> VF.OutputProg
runDelab prog = runExcept $ evalStateT (unSOT $ delab prog) []

data DelabError
  = UnsolvedMeta Provenance VC.Meta

instance MeaningfulError DelabError where
  details (UnsolvedMeta p meta) = UError $ UserError
    { provenance = p
    , problem    = "unsolved meta variable" <+> pretty meta
    }

-- * Delaboration class

--------------------------------------------------------------------------------
-- $sugar
-- The following definitions are tactics for refolding various bits of syntactic
-- sugar
--
-- The following pieces of syntactic sugar are unfolded here:
--
--   * @(forall a ?_ (forall b ?_ TYPE))@
--     is refolded to
--     @forall a b. TYPE@
--     (see 'delabTForalls')
--
--   * @(lambda ( x ?_ ) (lambda ( y ?_ ) EXPR))@
--     is unfolded to
--     @\x y -> EXPR@
--     (see 'delabELams')
--
--   * @(lambda { x ?_ } (lambda { y ?_ } EXPR))@
--     is unfolded to
--     @\{x y} -> EXPR@
--     (see 'delabELams')
--
--   * @(let ( x Nat ) 1 (let ( y Nat ) 2 EXPR@
--     is unfolded to
--     @let { x : Nat ; x = 1 ; y : Nat ; y = 2 } in EXPR@
--
--   * infix operators are rewritten from Polish notation, e.g.,
--     @(+ 1 5)@ is rewritten to @1 + 5@
--
--------------------------------------------------------------------------------


-- | Parameter "a" represents the type of the holes left in the expression.
--   Parameter b represents the type of the whole expression.
type Hole a b = StateT [a] (Except DelabError) b

hole :: Provenance -> Hole a a
hole p = get >>= \case
  []       -> developerError $ "Partially applied operator found at" <+> pretty p
  (x : xs) -> do put xs ; return x

plug :: Hole a b -> b
plug h = evalStateT h []

plugFlow :: Hole a a -> Hole b a
plugFlow h = lift (plug h)

addArg :: a -> Hole a ()
addArg x = modify (x :)

class Delaborate a b where
  delab :: a -> Hole b b

  delabSep :: a -> Hole b c
  delabSep = plugFlow . delab

delabBuiltin :: VF.OutputAnn
             -> VC.Builtin
             -> Hole VF.OutputExpr VF.OutputExpr
delabBuiltin ann = let p = prov ann in \case
  VC.PrimitiveTruth  TBool   -> return $ VF.Bool ann
  VC.PrimitiveTruth  TProp   -> return $ VF.Prop ann
  VC.PrimitiveNumber TInt    -> return $ VF.Int  ann
  VC.PrimitiveNumber TReal   -> return $ VF.Real ann
  VC.PrimitiveNumber TNat    -> return $ VF.Nat  ann
  VC.PrimitiveType   _       -> _
  VC.HasEq          -> VF.HasEq          ann <$> hole p <*> hole p
  VC.HasOrd         -> VF.HasOrd         ann <$> hole p <*> hole p
  VC.IsContainer    -> VF.IsContainer    ann <$> hole p <*> hole p
  VC.IsTruth        -> VF.IsTruth        ann <$> hole p
  VC.IsQuantifiable -> VF.IsQuant        ann <$> hole p
  VC.IsNatural      -> VF.IsNatural      ann <$> hole p
  VC.IsIntegral     -> VF.IsIntegral     ann <$> hole p
  VC.IsRational     -> VF.IsRational     ann <$> hole p
  VC.IsReal         -> VF.IsReal         ann <$> hole p
  VC.List           -> VF.List   ann <$> hole p
  VC.Tensor         -> VF.Tensor ann <$> hole p <*> hole p
  VC.Add            -> VF.Add    ann <$> hole p <*> hole p
  VC.Cons           -> VF.Cons   ann <$> hole p <*> hole p
  VC.If             -> VF.If     ann <$> hole p <*> hole p <*> hole p
  VC.Impl           -> VF.Impl   ann <$> hole p <*> hole p
  VC.And            -> VF.And    ann <$> hole p <*> hole p
  VC.Or             -> VF.Or     ann <$> hole p <*> hole p
  VC.Not            -> VF.Not    ann <$> hole p
  VC.Eq             -> VF.Eq     ann <$> hole p <*> hole p
  VC.Neq            -> VF.Neq    ann <$> hole p <*> hole p
  VC.Le             -> VF.Le     ann <$> hole p <*> hole p
  VC.Lt             -> VF.Lt     ann <$> hole p <*> hole p
  VC.Ge             -> VF.Ge     ann <$> hole p <*> hole p
  VC.Gt             -> VF.Gt     ann <$> hole p <*> hole p
  VC.Mul            -> VF.Mul    ann <$> hole p <*> hole p
  VC.Div            -> VF.Div    ann <$> hole p <*> hole p
  VC.Sub            -> VF.Sub    ann <$> hole p <*> hole p
  VC.Neg            -> VF.Neg    ann <$> hole p
  VC.At             -> VF.At     ann <$> hole p <*> hole p
  VC.All            -> return $ VF.All ann
  VC.Any            -> return $ VF.Any ann

instance Delaborate VC.OutputBinder VF.OutputBinder where
  delab (VC.Binder p v name typ) = VF.Binder p v name <$> _

instance Delaborate VC.OutputExpr VF.OutputExpr where
  delab t = do t <- _ delabAnn t; delabLayer t -- traverseTreeAnn
    where
      delabLayer :: VC.OutputExpr -> Hole VF.OutputExpr VF.OutputExpr
      delabLayer = cata delabF

      delabAnn :: VC.OutputAnn -> Hole VF.OutputExpr VF.OutputAnn
      delabAnn (VC.RecAnn e p) = VF.RecAnn <$> delab e <*> pure p

delabF :: VC.ExprF Symbol Symbol VF.OutputAnn (Hole VF.OutputExpr VF.OutputExpr)
       -> Hole VF.OutputExpr VF.OutputExpr
delabF = \case
  VC.TypeF l     -> return $ VF.Type l
  VC.ConstraintF -> return VF.Constraint

  VC.AnnF ann e t    -> VF.Ann ann <$> e <*> t;
  -- Annotation is not used here, as it was duplicated during elaboration
  VC.AppF _ann k1 k2 -> do k2 <- k2; addArg k2; unSOT k1
  VC.BuiltinF ann op -> delabBuiltin ann op
  VC.HoleF    ann n  -> _
  VC.MetaF    i      -> throwError $ UnsolvedMeta i

  VC.PiF      ann binder t -> do n <- delabSep binder; t <- t; return _ --VF.TForall ann (n :| []) t
  VC.VarF     ann n        -> return $ VF.Var ann n

  -- Expressions
  VC.LetF     ann n e1 e2 -> delabLet ann <$> delabSep n <*> delab e1 <*> delab e2
  VC.LamF     ann n e     -> do n <- delabSep n; VF.Lam ann (n :| []) <$> e;
  VC.LiteralF ann z       -> return $ VF.Literal ann z
  VC.SeqF     ann es      -> VF.Seq ann <$> sequence es

instance Delaborate VC.OutputDecl VF.OutputDecl where
  delab = \case
    VC.DeclNetw p n t    -> VF.DeclNetw p n <$> delabSep t
    VC.DeclData p n t    -> VF.DeclData p n <$> delabSep t
    --VC.DefType  p n ns t -> VF.DefType  p n <$> traverse delabSep ns <*> delabSep t
    VC.DefFun   p n t e  -> _ --delabFun p n <$> delabSep t <*> delabSep e

instance Delaborate VC.OutputProg VF.OutputProg where
  delab = \case
    VC.Main ds -> VF.Main <$> traverse delabSep ds

-- | Collapses let expressions into a sequence of let declarations
decomposeLet :: ([VF.OutputLetDecl], VC.OutputExpr) -> ([VF.OutputLetDecl], VC.OutputExpr)
decomposeLet (args, VC.Let ann binder e body) = decomposeLet (args <> [VF.LetDecl (prov ann) binder e], body)
decomposeLet result                   = result

delabLet :: VF.OutputAnn -> VF.OutputArg -> VF.OutputExpr -> VF.OutputExpr -> VF.OutputExpr
delabLet ann n = VF.Let ann (mempty :*: K (outputProv ann)) n _

-- | Collapses consecutative lambda expressions into a sequence of binders
decomposeLam :: ([VF.OutputBinder], VC.OutputExpr) -> ([VF.OutputBinder], VC.OutputExpr)
decomposeLam (args, VC.Lam _ ns body) = decomposeFun (args <> NonEmpty.toList ns, body)
decomposeLam result                   = result

delabFun :: Provenance -> WithProvenance Identifier -> VF.OutputExpr -> VC.OutputExpr -> VF.OutputDecl
delabFun p n t e = do
  let (args, body) = decomposeLam ([], e)
  body' <- plugFlow $ delab body
  return $ VF.DefFun p n t args body'
