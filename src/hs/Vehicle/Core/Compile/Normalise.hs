
module Vehicle.Core.Compile.Normalise where

import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.State (MonadState(..))
import Data.IntMap qualified as IntMap

import Vehicle.Prelude
import Vehicle.Core.AST
{-

-- |Run a function in 'MonadNorm'.
runNorm :: a -> ExceptT NormError Logger a
runNorm = nf

--------------------------------------------------------------------------------
-- Setup

-- |Errors thrown during normalisation
newtype NormError
  = EmptyQuantifierDomain Provenance

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = (MonadError NormError m, MonadLogger m)

-- |Pattern synonyms to help matching during normalisation. Perhaps these are useful elsewhere and should be lifted?
pattern Op0 :: Builtin -> ann -> Expr binder var ann
pattern Op0 op ann0 = Builtin ann0 op

pattern Op1 :: Builtin -> Arg binder var ann -> ann -> ann -> ann -> Expr binder var ann
pattern Op1 op e1 ann0 ann1 ann2 vis = App ann1 (Op0 op ann0) (Arg vis ann2 e1)

pattern Op2 :: Builtin
  -> Arg binder var ann
  -> Arg binder var ann
  -> ann -> ann -> ann
  -> Expr binder var ann
pattern Op2 op e1 e2 ann0 ann1 ann2 = App ann2 (Op1 op e1 ann0 ann1) e2

pattern Op3 :: Builtin
  -> Arg binder var ann
  -> Arg binder var ann
  -> Arg binder var ann
  -> ann -> ann -> ann-> ann
  -> Expr binder var ann
pattern Op3 op e1 e2 e3 ann0 ann1 ann2 ann3 = App ann3 (Op2 op e1 e2 ann0 ann1 ann2) e3

pattern ETrue :: ann -> Expr binder var ann
pattern ETrue ann = Literal ann (LBool True)

pattern EFalse :: ann -> Expr binder var ann
pattern EFalse ann = Literal ann (LBool False)

pattern EInt :: Int ->ann ->  Expr binder var ann
pattern EInt i ann = Literal ann (LInt i)

pattern EReal :: Double -> ann -> Expr binder var ann
pattern EReal d ann = Literal ann (LRat d)

--------------------------------------------------------------------------------
-- Normalisation algorithms
-}

-- TODO: move this to elsewhere, we need to normalise types in the
-- typechecker when checking against them too.
whnf :: MonadState MetaSubstitution m => CheckedExpr -> m CheckedExpr
whnf (App ann fun arg@(Arg _ _ argE)) = do
  whnfFun <- whnf fun
  case whnfFun of
    Lam _ _ body -> whnf (argE `substInto` body)
    _            -> return (App ann whnfFun arg)
whnf (Meta p n) = do
  subst <- get
  case IntMap.lookup n subst of
    Nothing -> return (Meta p n)
    Just tm -> whnf tm
-- TODO: expand out declared identifiers once the declCtx stores them
--  whnf (Free nm) = ...
whnf (Let _ bound _ body) =
  whnf (bound `substInto` body)
whnf (Ann _ body _) = whnf body
whnf e = return e

{-
-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm CheckedExpr where
  nf e = case e of
    Type{}     -> return e
    Hole{}     -> return e
    Literal{}  -> return e
    Builtin{}  -> return e
    Var{}      -> return e
    Meta{}     -> return e

    Seq ann exprs     -> Seq ann <$> traverse nf exprs
    Ann ann expr typ  -> Ann ann <$> nf expr <*> nf typ
    Lam ann arg expr  -> Lam ann arg <$> nf expr

    Pi ann arg body -> Pi ann arg <$> nf body

    Let _ letValue _ letBody -> do
      normalisedLetValue <- nf letValue
      let letBodyWithSubstitution = substInto normalisedLetValue letBody
      nf letBodyWithSubstitution

    App ann fn arg -> do
      normalisedArg <- nf arg
      normalisedFn  <- nf fn
      normApp (App ann normalisedFn normalisedArg)

instance Norm CheckedDecl where
  nf = \case
    DeclNetw ann arg typ    -> DeclNetw ann arg <$> nf typ
    DeclData ann arg typ    -> DeclData ann arg <$> nf typ
    DefFun ann arg typ expr -> DefFun   ann arg <$> nf typ <*> nf expr

instance Norm CheckedProg where
  nf (Main decls)= Main <$> traverse nf decls

normApp :: MonadNorm m => CheckedExpr -> m CheckedExpr
normApp = \case
  -- Lambda expressions
  App _ (Lam _ _ funcBody) (Arg _ _ arg) -> nf (substInto arg funcBody)

  -- Equality
  Op2 Eq (Op0 (ETrue  _) _) e2     _    _    _   -> return e2
  Op2 Eq (Op0 (EFalse _) _) e2     ann0 ann1 pos -> normApp $ Op1 ENot e2 ann0 ann1 pos
  Op2 Eq (EInt  _ i) (EInt  _ j) ann0 _      pos -> return $ mkBool (i == j) ann0 pos
  Op2 Eq (EReal _ x) (EReal _ y) ann0 _      pos -> return $ mkBool (x == y) ann0 pos

  -- Not
  Op1 Not (Op0 (ETrue  _) _) ann0 pos -> return $ Op0 EFalse ann0 pos
  Op1 Not (Op0 (EFalse _) _) ann0 pos -> return $ Op0 ETrue ann0 pos

  -- And
  Op2 And (Op0 (ETrue  _) _) e2 _ _ _ -> return e2
  Op2 And (Op0 (EFalse _) _) _ ann0 _ pos -> return $ Op0 EFalse ann0 pos
  Op2 And e1 (Op0 (ETrue _) _) _ _ _ -> return e1
  Op2 And _ (Op0 (EFalse _) _) ann0 _ pos -> return $ Op0 EFalse ann0 pos

  -- Or
  Op2 Or (Op0 (ETrue _) _) _ ann0 _ pos -> return $ Op0 ETrue ann0 pos
  Op2 Or (Op0 (EFalse _) _) e2 _ _ _ -> return e2
  Op2 Or _ (Op0 (ETrue _) _) ann0 _ pos -> return $ Op0 ETrue ann0 pos
  Op2 Or e1 (Op0 (EFalse _) _) _ _ _ -> return e1
  -- See https://github.com/wenkokke/vehicle/issues/2

  -- If
  Op3 If (Op0 (ETrue  _) _) e2 _ _ _ _ _ -> return e2
  Op3 If (Op0 (EFalse _) _) _ e3 _ _ _ _ -> return e3

  -- Natural builtins
  Op2 Le (EInt _ i) (EInt _ j) ann0 _ pos -> return $ mkBool (i <= j) ann0 pos
  Op2 Lt (EInt _ i) (EInt _ j) ann0 _ pos -> return $ mkBool (i < j) ann0 pos
  -- TODO implement associativity rules?
  Op2 Add (EInt _ i) (EInt _ j) ann0 _ _ -> return $ EInt ann0 (i + j)
  Op2 Sub (EInt _ i) (EInt _ j) ann0 _ _ -> return $ EInt ann0 (i - j)
  Op2 Mul (EInt _ i) (EInt _ j) ann0 _ _ -> return $ EInt ann0 (i * j)
  -- Real builtins
  Op2 Le (EReal _ x) (EReal _ y) ann0 _ pos -> return $ mkBool (x <= y) ann0 pos
  Op2 Lt (EReal _ x) (EReal _ y) ann0 _ pos -> return $ mkBool (x < y) ann0 pos
  -- TODO implement associativity rules?
  Op2 Add (EReal _ x) (EReal _ y) ann0 _ _ -> return $ EReal ann0 (x + y)
  Op2 Sub (EReal _ x) (EReal _ y) ann0 _ _ -> return $ EReal ann0 (x - y)
  Op2 Mul (EReal _ x) (EReal _ y) ann0 _ _ -> return $ EReal ann0 (x * y)
  Op2 Div (EReal _ x) (EReal _ y) ann0 _ _ -> return $ EReal ann0 (x / y)
  Op1 Neg (EReal _ x) ann0 _ -> return $ EReal ann0 (- x)
  -- Tensor builtins
  Op2 Cons e (Seq _ es) ann0 _ _ -> return $ ELitSeq ann0 (e : es)
  Op2 At (Seq _ es) (EInt _ i) _ _ _ -> return $ es !! fromIntegral i
  Op2 At (Op2 Cons e1 _ _ _ _) (EInt _ 0) _ _ _ -> return e1
  Op2 At (Op2 Cons _ es _ _ _) (EInt ann3 i) ann0 ann1 pos -> normApp (Op2 EAt es (EInt ann3 (i - 1)) ann0 ann1 ann2 pos)
  -- Quantifier builtins
  Op2 All e1 e2 ann0 ann1 pos -> normQuantifier All e1 e2 ann0 ann1 ann2 pos >>= norm
  Op2 Any e1 e2 ann0 ann1 pos -> normQuantifier Any e1 e2 ann0 ann1 ann2 pos >>= norm
  -- Fall-through case
  expr -> return expr

mkBool :: Bool -> ann -> DeBruijnExpr ann
mkBool b ann = Literal ann (LBool b)
-}