
module Vehicle.Frontend.Delaborate
  ( runDelab
  , DelabError(..)
  ) where

import Control.Monad.State (MonadState(..), evalStateT, modify)
import Control.Monad.Except (MonadError(..), runExcept)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NonEmpty (reverse)
import Data.Text (pack)
import Prettyprinter (pretty, (<+>))
import Debug.Trace (trace)

import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Core.Print.Core (showCore)
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Delaboration converts a program in the Core language to the Frontend language

runDelab :: Delaborate a b => a -> Either DelabError b
runDelab e = runExcept $! delab e

data DelabError
  = UnsolvedMeta Provenance VC.Meta

instance MeaningfulError DelabError where
  details (UnsolvedMeta p meta) = UError $ UserError
    { provenance = p
    , problem    = "unsolved meta variable" <+> pretty meta
    , fix        = "try adding a type annotation"
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

type MonadDelab      m = MonadError DelabError m
type MonadDelabHoles m = (MonadDelab m, MonadState [VF.OutputExpr] m)

hole :: MonadDelabHoles m => Provenance -> m VF.OutputExpr
hole p = get >>= \case
  []       -> developerError $ "Partially applied operator found at" <+> pretty p
  (x : xs) -> do put xs ; return x

addArg :: MonadDelabHoles m => VF.OutputExpr -> m ()
addArg x = modify (x :)

-- | Tests if a binder is from a forall or a function.
isFunBinder :: VC.OutputBinder -> Bool
isFunBinder (VC.Binder _ vis _ _) = vis == Explicit

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: MonadDelabHoles m => VC.OutputAnn -> VC.OutputBinder -> VC.OutputExpr -> m VF.OutputExpr
delabPi ann binder@(VC.Binder _ _ _ arg) body
  | isFunBinder binder = VF.Fun <$> delab ann <*> delabH arg <*> delabH body
  | otherwise                 = do
    binder'       <- delab binder
    ann'          <- delab ann
    (ns' , body') <- decomposeForall (binder' :| [], body)
    return $ VF.Lam ann' (NonEmpty.reverse ns') body'
    where
      decomposeForall :: MonadDelabHoles m => (NonEmpty VF.OutputBinder, VC.OutputExpr) -> m (NonEmpty VF.OutputBinder, VF.OutputExpr)
      decomposeForall (args, VC.Pi _ binder body)
        | not (isFunBinder binder) = do binder' <- delab binder; decomposeForall (binder' <| args, body)
      decomposeForall (args, body) = do body'   <- delab body; return (args , body')

-- | Collapses let expressions into a sequence of let declarations
delabLet :: MonadDelabHoles m => VC.OutputAnn -> VC.OutputExpr -> VC.OutputBinder -> VC.OutputExpr -> m VF.OutputExpr
delabLet ann bound binder body = do
  decl'         <- delabLetDecl ann binder bound
  ann'          <- delab ann
  (ds' , body') <- decomposeLet (decl' :| [], body)
  return $ VF.Let ann' (NonEmpty.reverse ds') body'
  where
    decomposeLet :: MonadDelabHoles m => (NonEmpty VF.OutputLetDecl, VC.OutputExpr) -> m (NonEmpty VF.OutputLetDecl, VF.OutputExpr)
    decomposeLet (args, VC.Let ann bound binder body) = do decl  <- delabLetDecl ann binder bound; decomposeLet (decl <| args, body)
    decomposeLet (args, body)                         = do body' <- delab body; return (args , body')

    delabLetDecl :: MonadDelabHoles m => VC.OutputAnn -> VC.OutputBinder -> VC.OutputExpr -> m VF.OutputLetDecl
    delabLetDecl ann binder bound = VF.LetDecl (prov ann) <$> delab binder <*> delab bound

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: MonadDelabHoles m => VC.OutputAnn -> VC.OutputBinder -> VC.OutputExpr -> m VF.OutputExpr
delabLam ann binder body = do
  binder'       <- delab binder
  ann'          <- delab ann
  (ns' , body') <- decomposeLam (binder' :| [], body)
  return $ VF.Lam ann' (NonEmpty.reverse ns') body'
  where
    decomposeLam :: MonadDelabHoles m => (NonEmpty VF.OutputBinder, VC.OutputExpr) -> m (NonEmpty VF.OutputBinder, VF.OutputExpr)
    decomposeLam (args, VC.Lam _ binder body) = do binder' <- delab binder; decomposeLam (binder' <| args, body)
    decomposeLam (args, body)                 = do body' <- delab body; return (args , body')

-- TODO this should reconstruct whether it's a type synonym or not
delabFun :: MonadDelab m => Provenance -> WithProvenance Identifier -> VC.OutputExpr -> VC.OutputExpr -> m VF.OutputDecl
delabFun p n typ body = do
  typ' <- delab typ
  (args', body') <- decomposeFun ([], body)
  return $ VF.DefFun p n typ' (reverse args') body'
  where
    decomposeFun :: MonadDelab m => ([VF.OutputBinder], VC.OutputExpr) -> m ([VF.OutputBinder], VF.OutputExpr)
    decomposeFun (args, VC.Lam _ binder body) = do binder' <- delab binder; decomposeFun (binder' : args, body)
    decomposeFun (args, body)                 = do body' <- delab body; return (args , body')


delabBuiltin :: MonadDelabHoles m
             => VF.OutputAnn
             -> VC.Builtin
             -> m VF.OutputExpr
delabBuiltin ann = let p = prov ann in \case
  VC.Bool           -> return $ VF.Bool ann
  VC.Prop           -> return $ VF.Prop ann
  VC.Int            -> return $ VF.Int  ann
  VC.Real           -> return $ VF.Real ann
  VC.Nat            -> return $ VF.Nat  ann
  VC.HasEq          -> VF.HasEq        ann <$> hole p <*> hole p
  VC.HasOrd         -> VF.HasOrd       ann <$> hole p <*> hole p
  VC.IsContainer    -> VF.IsContainer  ann <$> hole p <*> hole p
  VC.IsTruth        -> VF.IsTruth      ann <$> hole p
  VC.IsQuantifiable -> VF.IsQuant      ann <$> hole p
  VC.IsNatural      -> VF.IsNatural    ann <$> hole p
  VC.IsIntegral     -> VF.IsIntegral   ann <$> hole p
  VC.IsRational     -> VF.IsRational   ann <$> hole p
  VC.IsReal         -> VF.IsReal       ann <$> hole p
  VC.List           -> VF.List         ann <$> hole p
  VC.Tensor         -> VF.Tensor       ann <$> hole p <*> hole p
  VC.Add            -> VF.Add          ann <$> hole p <*> hole p
  VC.Cons           -> VF.Cons         ann <$> hole p <*> hole p
  VC.If             -> VF.If           ann <$> hole p <*> hole p <*> hole p
  VC.Impl           -> VF.Impl         ann <$> hole p <*> hole p
  VC.And            -> VF.And          ann <$> hole p <*> hole p
  VC.Or             -> VF.Or           ann <$> hole p <*> hole p
  VC.Not            -> VF.Not          ann <$> hole p
  VC.Eq             -> VF.Eq           ann <$> hole p <*> hole p
  VC.Neq            -> VF.Neq          ann <$> hole p <*> hole p
  VC.Le             -> VF.Le           ann <$> hole p <*> hole p
  VC.Lt             -> VF.Lt           ann <$> hole p <*> hole p
  VC.Ge             -> VF.Ge           ann <$> hole p <*> hole p
  VC.Gt             -> VF.Gt           ann <$> hole p <*> hole p
  VC.Mul            -> VF.Mul          ann <$> hole p <*> hole p
  VC.Div            -> VF.Div          ann <$> hole p <*> hole p
  VC.Sub            -> VF.Sub          ann <$> hole p <*> hole p
  VC.Neg            -> VF.Neg          ann <$> hole p
  VC.At             -> VF.At           ann <$> hole p <*> hole p
  VC.All            -> return $ VF.All ann
  VC.Any            -> return $ VF.Any ann


class DelaborateWithHoles a b where
  delabH :: MonadDelabHoles m => a -> m b

instance DelaborateWithHoles VC.OutputExpr VF.OutputExpr where
  delabH e = trace ("delab-exit: " <> showCore e) $ case e of
    VC.Type l                        -> return (VF.Type l)
    VC.Constraint                    -> return VF.Constraint
    VC.Hole    p n                   -> return $ VF.Hole p n
    VC.Meta    ann i                 -> return $ VF.Hole (prov ann) (pack $ "?" <> show i)
    VC.Pi      ann       binder body -> delabPi  ann       binder body
    VC.Let     ann bound binder body -> delabLet ann bound binder body
    VC.Lam     ann       binder body -> delabLam ann       binder body
    VC.Builtin ann op                -> do ann' <- delab ann; delabBuiltin ann' op
    VC.Ann     ann e t               -> VF.Ann     <$> delab ann <*> delabH e   <*> delabH t
    VC.App     ann fun arg           -> VF.App     <$> delab ann <*> delabH fun <*> delabH arg
    VC.Var     ann n                 -> VF.Var     <$> delab ann <*> pure n
    VC.Literal ann z                 -> VF.Literal <$> delab ann <*> pure z
    VC.Seq     ann es                -> VF.Seq     <$> delab ann <*> traverse delabH es

instance DelaborateWithHoles VC.OutputArg VF.OutputArg where
  delabH (VC.Arg p v e) = do
    e' <- delabH e
    addArg e'
    return $ VF.Arg p v e'


class Delaborate a b where
  delab :: MonadDelab m => a -> m b

instance Delaborate VC.OutputAnn VF.OutputAnn where
  delab (VC.RecAnn e ann) = VF.RecAnn <$> delab e <*> pure ann

instance Delaborate VC.OutputBinder VF.OutputBinder where
  delab (VC.Binder p v n t) = VF.Binder p v n . Just <$> delab t

instance Delaborate VC.OutputExpr VF.OutputExpr where
  delab e = evalStateT (delabH e) []

instance Delaborate VC.OutputDecl VF.OutputDecl where
  delab = \case
    VC.DeclNetw p n t    -> VF.DeclNetw p n <$> delab t
    VC.DeclData p n t    -> VF.DeclData p n <$> delab t
    VC.DefFun   p n t e  -> delabFun p n t e

instance Delaborate VC.OutputProg VF.OutputProg where
  delab (VC.Main ds) = VF.Main <$> traverse delab ds

