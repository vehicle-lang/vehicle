
module Vehicle.Frontend.Delaborate
  ( DelabError(..)
  , runDelab
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState(..), StateT, evalStateT, modify)
import Control.Monad.Except (MonadError(..), Except, runExcept)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Prettyprinter (pretty, (<+>))

import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Error
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Delaboration converts a program in the Core language to the Frontend language

runDelab :: VC.OutputProg -> Either DelabError VF.OutputProg
runDelab prog = runExcept $ evalStateT (unSOT $ delab prog) []

data DelabError
  = PartiallyAppliedOperator Provenance
  | UnsolvedMeta Provenance VC.Meta

instance MeaningfulError DelabError where
  details (PartiallyAppliedOperator p) = DError $ DeveloperError
    { provenance = p
    , problem = "Partially applied operator found at"
    }

  details (UnsolvedMeta p meta) = DError $ DeveloperError
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
  []       -> throwError $ PartiallyAppliedOperator p
  (x : xs) -> do put xs ; return x

plug :: Hole a b -> Except DelabError b
plug h = evalStateT h []

addArg :: a -> Hole a ()
addArg x = modify (x :)

-- TODO make HasProvenance work
outputProv :: VF.OutputAnn sort -> Provenance
outputProv = unK . isnd

delabBuiltin :: VF.OutputAnn
             -> VC.Builtin
             -> Hole VF.OutputExpr VF.OutputExpr
delabBuiltin ann = let p = outputProv ann in \case
  VC.Type    -> return $ VF.Type ann
  VC.PrimitiveTruth  TBool   -> return $ VF.Bool ann
  VC.PrimitiveTruth  TProp   -> return $ VF.Prop ann
  VC.PrimitiveNumber TInt    -> return $ VF.Int ann
  VC.PrimitiveNumber TReal   -> return $ VF.Real ann
  VC.List    -> VF.List   ann <$> hole p
  VC.Tensor  -> VF.Tensor ann <$> hole p <*> hole p
  VC.Add     -> VF.Add    ann <$> hole p <*> hole p
  VC.Cons    -> VF.Cons   ann <$> hole p <*> hole p
  VC.If      -> VF.If     ann <$> hole p <*> hole p <*> hole p
  VC.Impl    -> VF.Impl   ann <$> hole p <*> hole p
  VC.And     -> VF.And    ann <$> hole p <*> hole p
  VC.Or      -> VF.Or     ann <$> hole p <*> hole p
  VC.Not     -> VF.Not    ann <$> hole p
  VC.Eq      -> VF.Eq     ann <$> hole p <*> hole p
  VC.Neq     -> VF.Neq    ann <$> hole p <*> hole p
  VC.Le      -> VF.Le     ann <$> hole p <*> hole p
  VC.Lt      -> VF.Lt     ann <$> hole p <*> hole p
  VC.Ge      -> VF.Ge     ann <$> hole p <*> hole p
  VC.Gt      -> VF.Gt     ann <$> hole p <*> hole p
  VC.Mul     -> VF.Mul    ann <$> hole p <*> hole p
  VC.Div     -> VF.Div    ann <$> hole p <*> hole p
  VC.Add     -> VF.Add    ann <$> hole p <*> hole p
  VC.Sub     -> VF.Sub    ann <$> hole p <*> hole p
  VC.Neg     -> VF.Neg    ann <$> hole p
  VC.Cons    -> VF.Cons   ann <$> hole p <*> hole p
  VC.At      -> VF.At     ann <$> hole p <*> hole p
  VC.All     -> return $ VF.All ann
  VC.Any     -> return $ VF.Any ann

delabLiteral :: VF.OutputAnn
             -> VC.Literal
             -> Hole VF.OutputExpr VF.OutputExpr
delabLiteral = _

class Delaborate a b where
  delab :: a -> Hole b b

instance Delaborate VC.OutputExpr VF.OutputExpr where
  delab t = do t <- _ delabAnn t; delabLayer t -- traverseTreeAnn
    where
      delabLayer ::
        VC.Expr Symbol Symbol (VF.RecAnn Provenance) ->
        Hole VF.OutputExpr VF.OutputExpr
      delabLayer = cata delabF

      delabAnn ::
        VC.RecAnn Symbol Symbol Provenance ->
        Hole VF.OutputExpr (VF.RecAnn Provenance)
      delabAnn (VC.RecAnn e p) = do v <- plugFlow (delab v); return $ VF.RecAnn v p

delabF :: VC.ExprF (K Symbol) VF.OutputAnn (Hole VF.OutputExpr VF.OutputExpr)
       -> Hole VF.OutputExpr VF.OutputExpr
delabF = \case
  VC.TypeF l    -> _
  VC.Constraint -> _

  VC.AnnF ann e t    -> do e <- unSOT e; t <- plugFlow t; return $ VF.Ann ann e t
  -- Annotation is not used here, as it was duplicated during elaboration
  VC.AppF _ann k1 k2 -> do k2 <- unSOT k2; addArg k2; unSOT k1
  VC.BuiltinF ann op    -> delabBuiltin ann op
  VC.MetaF    ann i     -> throwError $ UnsolvedMeta (outputProv ann) i

  VC.Pi          ann binder n t -> do n <- plugFlow n; t <- unSOT t; return $ _ --VF.TForall ann (n :| []) t
  VC.BoundF      ann n     -> return $ VF.Var ann n
  VC.FreeF       ann n     -> return $ VF.Var ann n
  VC.MetaF       ann i     -> throwError $ UnsolvedMeta (outputProv ann) i

  -- Expressions
  VC.LetF     ann n e1 e2 -> delabLet ann <$> plugFlow n <*> unSOT e1 <*> unSOT e2
  VC.LamF     ann n e     -> do n <- plugFlow n; e <- unSOT e; return $ VF.Lam ann (n :| []) e
  VC.LiteralF ann z       -> return $ VF.Literal ann z
  VC.SeqF     ann es      -> do es <- traverse unSOT es; return $ VF.Seq ann es

instance Delaborate VC.OutputDecl VF.OutputDecl where
  delab = \case
    VC.DeclNetw ann n t    -> do n <- plugFlow n; t <- plugFlow t; return $ VF.DeclNetw ann n t
    VC.DeclData ann n t    -> do n <- plugFlow n; t <- plugFlow t; return $ VF.DeclData ann n t
    VC.DefType  ann n ns t -> do n <- plugFlow n; t <- plugFlow t; ns <- traverse plugFlow ns; return $ VF.DefType ann n ns t
    VC.DefFun   ann n t e  -> delabFun ann <$> plugFlow n <*> plugFlow t <*> plugFlow e

instance Delaborate VC.OutputProg VF.OutputProg where
  delab = \case
    VC.Main ann ds -> do ds <- traverse plugFlow ds; return $ VF.Main ann ds



delabLet :: VF.OutputAnn 'EXPR -> VF.OutputArg -> VF.OutputExpr -> VF.OutputExpr -> VF.OutputExpr
delabLet ann n = VF.ELet1 ann (mempty :*: K (outputProv ann)) n (VF.unInfo $ ifst ann)

decomposeFun :: FunDecomp -> FunDecomp
decomposeFun (VF.ELam   _ ns body, args) = decomposeFun (body, args <> map Right (NonEmpty.toList ns))
decomposeFun (VF.ETyLam _ ns body, args) = decomposeFun (body, args <> map Left  (NonEmpty.toList ns))
decomposeFun result                      = result

delabFun :: VF.OutputAnn 'DECL -> VF.OutputEArg -> VF.OutputType -> VF.OutputExpr -> VF.OutputDecl
delabFun ann n t e = let (body , args) = decomposeFun (e , []) in VF.DefFun ann n t args body